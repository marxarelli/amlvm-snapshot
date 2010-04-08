#!/usr/bin/perl
# Copyright (c) 2010, Daniel Duvall.  All Rights Reserved.
#
# This program is free software; you can redistribute it and/or modify it
# under the terms of the GNU General Public License version 2 as published
# by the Free Software Foundation.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
# or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
# for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
#
# Author: Daniel Duvall <the.liberal.media@gmail.com>

# PROPERTY:
#
#    SNAPSHOT-SIZE
#
#    LVCREATE-PATH
#    LVDISPLAY-PATH
#    LVREMOVE-PATH
#    VGDISPLAY-PATH
#
#    SUDO
#
use lib '/usr/local/share/perl/5.8.4';
use strict;
use Getopt::Long;

package Amanda::Script::Amlvm_snapshot;
use base qw(Amanda::Script);

use Amanda::Config qw( :getconf :init );
use Amanda::Debug qw( :logging );
use Amanda::Util qw( :constants );
use Amanda::Paths;
use Amanda::Constants;

use Config;
use IPC::Open3;
use Symbol;

sub new {
    my $class = shift;
    my ($execute_where, $config, $host, $disk, $device, $level, $index,
        $message, $collection, $record, $snapsize, $lvcreate, $lvdisplay,
        $lvremove, $vgdisplay, $sudo) = @_;
    my $self = $class->SUPER::new($execute_where, $config);

    $self->{execute_where}  = $execute_where;
    $self->{config}         = $config;
    $self->{host}           = $host;
    $self->{device}         = $device;
    $self->{disk}           = $disk;
    $self->{level}          = [ @{$level} ]; # Copy the array
    $self->{index}          = $index;
    $self->{message}        = $message;
    $self->{collection}     = $collection;
    $self->{record}         = $record;

    $self->{snapsize}       = $snapsize;

    $self->{lvcreate}       = $lvcreate;
    $self->{lvdisplay}      = $lvdisplay;
    $self->{lvremove}       = $lvremove;
    $self->{vgdisplay}      = $vgdisplay;

    $self->{sudo}           = $sudo;

    $self->{volume_group}   = undef;

    return $self;
}

sub calculate_snapsize {
    my $self = shift;

    my $size;

    # if a snapshot size isn't already set, use all available extents in the
    # volume group
    if (!defined $self->{snapsize}) {
        foreach ($self->execute("$self->{vgdisplay} -c")) {
            my @parts = split(/:/);
            my $group = $parts[0];
            my $total = $parts[13];
            my $alloc = $parts[14];

            $group =~ s/^\s*//;
            chomp($group);

            if ($group eq $self->{volume_group}) {
                $self->{snapsize} = $total - $alloc;
                last;
            }
        }
    }

    # fallback to just 1 extent (though this might fail anyway)
    $self->{snapsize} = 1 if (!defined $self->{snapsize});
}

sub create_snapshot {
    my $self = shift;

    # calculate default snapshot size
    $self->calculate_snapsize();

    $self->print_to_server("",
        "A snapshot of size `$self->{snapsize}' will be created.",
        $Amanda::Script_App::GOOD
    );

    # create a new snapshot with lvcreate
    $self->execute(
        "$self->{lvcreate} --size $self->{snapsize} --snapshot ".
        "--name amsnapshot $self->{device}"
    );

    $self->print_to_server("",
        "Created snapshot of `$self->{device}'.",
        $Amanda::Script_App::GOOD
    );
}

sub execute {
    my $self = shift;
    my ($cmd) = @_;

    my ($in, $out, $err, $pid);
    $err = Symbol::gensym;

    if ($self->{sudo}) {
        $cmd = "sudo $cmd";
    }

    $pid = open3($in, $out, $err, $cmd);

    close($in);

    my @output = <$out>;
    my @errors = <$err>;

    close($out);
    close($err);

    waitpid($pid, 0);

    if ($? > 0) {
        my $err_str = join("", @errors);
        chomp($err_str);

        $self->print_to_server_and_die("",
            "Failed to execute (status $?) `$cmd': $err_str",
            $Amanda::Script_App::ERROR
        );
    }

    return @output;
}

sub remove_snapshot {
    my $self = shift;

    # remove snapshot device
    $self->execute(
        "$self->{lvremove} -f /dev/$self->{volume_group}/amsnapshot"
    );

    $self->print_to_server("",
        "Removed snapshot of `$self->{device}'.",
        $Amanda::Script_App::GOOD
    );
}

# Returns the device that's mounted at the given directory.
sub resolve_device {
    my $self = shift;

    my $mnt_device;

    # look in the system mtab
    open(MTAB, "/etc/mtab");
    my $line;
    while ($line = <MTAB>) {
        chomp($line);
        my ($device, $directory) = split(/\s+/, $line);

        if ($directory eq $self->{disk}) {
            $mnt_device = $device;
        }
    }
    close MTAB;

    if (!defined $mnt_device) {
        $self->print_to_server_and_die("",
            "Failed to resolve a device from mount point `$self->{disk}'. ".
            "Is the volume mounted?",
            $Amanda::Script_App::ERROR
        );
    }

    # loop through the LVs to find the one that matches
    foreach ($self->execute("$self->{lvdisplay} -c")) {
        my ($device, $group) = split(/:/);

        $device =~ s/^\s*//;
        chomp($device);

        # we don't use perl's readlink here, because it might need to be
        # executed with sudo
        my $real_device = join("", $self->execute("readlink $device"));
        chomp($real_device);

        if ($real_device eq $mnt_device) {
            $self->{device} = $device;
            $self->{volume_group} = $group;

            $self->print_to_server("",
                "Resolved device `$self->{device}' and volume group ".
                "`$self->{volume_group}' from mount point `$self->{disk}'.",
                $Amanda::Script_App::GOOD
            );

            last;
        }
    }
}

sub setup {
    my $self = shift;

    # can only be executed in client context
    if ($self->{execute_where} ne "client") {
        $self->print_to_server_and_die("",
            "Script must be run on the client",
            $Amanda::Script_App::ERROR
        );
    }

    # resolve paths, if not already provided.
    if (!defined $self->{lvcreate}) {
        chomp($self->{lvcreate} = `which lvcreate`);
        $self->print_to_server_and_die("",
            "lvcreate wasn't found.",
            $Amanda::Script_App::ERROR
        ) if $?;
    }

    if (!defined $self->{lvdisplay}) {
        chomp($self->{lvdisplay} = `which lvdisplay`);
        $self->print_to_server_and_die("",
            "lvdisplay wasn't found.",
            $Amanda::Script_App::ERROR
        ) if $?;
    }

    if (!defined $self->{lvremove}) {
        chomp($self->{lvremove} = `which lvremove`);
        $self->print_to_server_and_die("",
            "lvremove wasn't found.",
            $Amanda::Script_App::ERROR
        ) if $?;
    }

    if (!defined $self->{vgdisplay}) {
        chomp($self->{vgdisplay} = `which vgdisplay`);
        $self->print_to_server_and_die("",
            "vgdisplay wasn't found.",
            $Amanda::Script_App::ERROR
        ) if $?;
    }

    # resolve actual lvm device
    $self->resolve_device();

    if (!defined $self->{volume_group}) {
        $self->print_to_server_and_die("",
            "Failed to resolve device path and volume group.",
            $Amanda::Script_App::ERROR
        );
    }
}

sub command_support {
    my $self = shift;

    print "CONFIG YES\n";
    print "HOST YES\n";
    print "DISK YES\n";
    print "MESSAGE-LINE YES\n";
    print "MESSAGE-XML NO\n";
    print "EXECUTE-WHERE YES\n";
}

#define a execute_on_* function for every execute_on you want the script to do
#something
sub command_pre_dle_backup {
    my $self = shift;

    $self->setup();
    $self->create_snapshot();
}

sub command_post_dle_backup {
    my $self = shift;

    $self->setup();
    $self->remove_snapshot();
}

package main;

sub usage {
    print <<EOF;
Usage: amlvm-snapshot <command> --execute-where=client --config=<config> --host=<host> --disk=<disk> --device=<device> --level=<level> --index=<yes|no> --message=<text> --collection=<no> --record=<yes|no> --snapshot-size=<lvm snapshot size> --lvcreate-path=<path> --lvdisplay-path=<path> --lvremove-path=<path> --vgdisplay-path=<path> --sudo=<0|1>.
EOF
    exit(1);
}

my $opt_execute_where;
my $opt_config;
my $opt_host;
my $opt_disk;
my $opt_device;
my @opt_level;
my $opt_index;
my $opt_message;
my $opt_collection;
my $opt_record;

my $opt_snapsize;
my $opt_lvcreate;
my $opt_lvdisplay;
my $opt_lvremove;
my $opt_vgdisplay;
my $opt_sudo;

#print join(" ", @ARGV) . "\n";
#exit 1;

Getopt::Long::Configure(qw{bundling});
GetOptions(
    'execute-where=s'   => \$opt_execute_where,
    'config=s'          => \$opt_config,
    'host=s'            => \$opt_host,
    'disk=s'            => \$opt_disk,
    'device=s'          => \$opt_device,
    'level=s'           => \@opt_level,
    'index=s'           => \$opt_index,
    'message=s'         => \$opt_message,
    'collection=s'      => \$opt_collection,
    'record=s'          => \$opt_record,
    'snapshot-size=s'   => \$opt_snapsize,
    'lvcreate-path=s'   => \$opt_lvcreate,
    'lvdisplay-path=s'  => \$opt_lvdisplay,
    'lvremove-path=s'   => \$opt_lvremove,
    'vgdisplay-path=s'  => \$opt_vgdisplay,
    'sudo=s'            => \$opt_sudo,
) or usage();

# add SBIN to PATH
$ENV{'PATH'} = "/sbin:/usr/sbin:$ENV{'PATH'}:/usr/local/sbin";

my $script = Amanda::Script::Amlvm_snapshot->new($opt_execute_where,
    $opt_config, $opt_host, $opt_disk, $opt_device, \@opt_level, $opt_index,
    $opt_message, $opt_collection, $opt_record, $opt_snapsize, $opt_lvcreate,
    $opt_lvdisplay, $opt_lvremove, $opt_vgdisplay, $opt_sudo);
$script->do($ARGV[0]);

# vim: set et sts=4 sw=4 :
