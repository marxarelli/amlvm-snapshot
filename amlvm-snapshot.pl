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
use File::Temp qw(tempdir);
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
    $self->{fs_type}        = undef;

    return $self;
}

sub calculate_snapsize {
    my $self = shift;

    my $size;

    # if a snapshot size isn't already set, use all available extents in the
    # volume group
    if (!defined $self->{snapsize}) {
        foreach ($self->execute(1, "$self->{vgdisplay} -c")) {
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

    debug("A snapshot of size `$self->{snapsize}' will be created.");

    # create a new snapshot with lvcreate
    $self->execute(1,
        "$self->{lvcreate}", "--extents", $self->{snapsize},
        "--snapshot", "--name", "amsnapshot", $self->{device}
    );
    my $snapshot_device = $self->get_snap_device();

    debug("Created snapshot of `$self->{device}' at `$snapshot_device'.");
}

# Executes (safely) the given command and arguments. Optional execution
# through sudo can be specified, but will only occur if the script was invoked
# with the '--sudo' argument.
sub execute {
    my $self = shift;
    my $sudo = shift;
    my $cmd = shift;

    # escape all given arguments
    my @args = map(quotemeta, @_);

    my ($full_cmd, $in, $out, $err, $pid);
    $err = Symbol::gensym;

    if ($sudo && $self->{sudo}) {
        $full_cmd = "sudo $cmd";
    }

    $full_cmd .= " @args";

    $pid = open3($in, $out, $err, $full_cmd);

    close($in);

    my @output = <$out>;
    my @errors = <$err>;

    close($out);
    close($err);

    waitpid($pid, 0);

    # NOTE There's an exception for readlink, as it's failure isn't critical.
    if ($? > 0 and $cmd ne "readlink") {
        my $err_str = join("", @errors);
        chomp($err_str);

        $self->print_to_server_and_die("",
            "Failed to execute (status $?) `$full_cmd': $err_str",
            $Amanda::Script_App::ERROR
        );
    }

    return @output;
}

# Returns the snapshot device path.
sub get_snap_device {
    my $self = shift;
    return "/dev/$self->{volume_group}/amsnapshot";
}

# Mounts the snapshot device at the configured directory.
sub mount_snapshot {
    my $self = shift;

    # mount options
    my @options = ('ro');

    # special mount options for xfs
    # XXX should this be left up to the user as an argument?
    if ($self->{fs_type} eq 'xfs') {
        push(@options, 'nouuid');
    }

    # create a temporary mount point and mount the snapshot volume
    $self->{directory} = tempdir(CLEANUP => 0);
    my $snapshot_device = $self->get_snap_device();
    $self->execute(1,
        "mount -o ", join(",", @options),
        $snapshot_device, $self->{directory}
    );

    debug("Mounted snapshot `$snapshot_device' at `$self->{directory}'.");
}

# Readlink wrapper.
sub readlink {
    my $self = shift;
    my $path = shift;

    # NOTE: We don't use perl's readlink here, because it might need to be
    # executed with elevated privileges (sudo).
    my $real_path = join("", $self->execute(1, "readlink", $path));
    chomp($real_path);

    return ($real_path ne "") ? $real_path : $path;
}

# Removes the snapshot device.
sub remove_snapshot {
    my $self = shift;

    # remove snapshot device with 'lvremove'
    $self->execute(1, "$self->{lvremove} -f", $self->get_snap_device());

    debug("Removed snapshot of `$self->{device}'.");
}

# Resolves the underlying device on which the configured directory resides.
sub resolve_device {
    my $self = shift;

    # Search mtab for the mount point. Get the device path and filesystem type.
    my $mnt_device = $self->scan_mtab(
        sub { return $_[0] if ($_[1] eq $self->{disk}); }
    );

    my $fs_type = $self->scan_mtab(
        sub { return $_[2] if ($_[1] eq $self->{disk}); }
    );

    if (!defined $mnt_device) {
        $self->print_to_server_and_die("",
            "Failed to resolve a device from directory `$self->{disk}'. ",
            $Amanda::Script_App::ERROR
        );
    }

    # loop through the LVs to find the one that matches
    foreach ($self->execute(1, "$self->{lvdisplay} -c")) {
        my ($device, $group) = split(/:/);

        $device =~ s/^\s*//;
        chomp($device);

        my $real_device = $self->readlink($device);
        chomp($real_device);

        if ($real_device eq $mnt_device) {
            $self->{device} = $device;
            $self->{volume_group} = $group;
            $self->{fs_type} = $fs_type;

            debug(
                "Resolved device `$self->{device}' and volume group ".
                "`$self->{volume_group}' from mount point `$self->{disk}'."
            );

            last;
        }
    }
}

# Iterates over lines in the system mtab and invokes the given anonymous
# subroutine with entries from each record:
#  1. Canonical device path (as resolved from readlink).
#  2. Mount point directory.
#  3. Filesystem type.
sub scan_mtab {
    my $self = shift;
    my $sub = shift;

    open(MTAB, "/etc/mtab");
    my $line;
    my $result;
    while ($line = <MTAB>) {
        chomp($line);
        my ($device, $directory, $type) = split(/\s+/, $line);
        $result = $sub->($self->readlink($device), $directory, $type);
        last if ($result);
    }
    close MTAB;

    return $result;
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

sub umount_snapshot {
    my $self = shift;
    my $device = $self->readlink($self->get_snap_device());

    my $mnt = $self->scan_mtab(sub { return $_[1] if ($_[0] eq $device); });

    if (!$mnt) {
        $self->print_to_server_and_die("",
            "Failed to get mount point for snapshot device `$device'.",
            $Amanda::Script_App::ERROR
        );
    }

    $self->execute(1, "umount", $mnt);

    debug("Un-mounted snapshot device `$device' from `$mnt'.");
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
    $self->mount_snapshot();

    print "PROPERTY directory $self->{directory}\n";
}

sub command_post_dle_backup {
    my $self = shift;

    $self->setup();
    $self->umount_snapshot();
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
