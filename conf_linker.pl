#!/usr/bin/perl
use strict;
use FindBin '$Bin';

die "Nothing to do.\n" unless @ARGV;

my $config_dir = $Bin;
my $files = {'cli' => {'gitconfig' => '.gitconfig',
                       'gitexcludes' => '.gitexcludes',
                       'screenrc' => '.screenrc',
                       'tmux.conf' => '.tmux.conf',
                       'zsh/zshrc' => '.zshrc',
                       'zsh/zshenv' => '.zshenv',
                       'zsh/zsh' => '.zsh',
                       'zsh/oh-my-zsh' => '.oh-my-zsh',
                       'vim/vimrc' => '.vimrc',
                       'vim/vim' => '.vim',
                       'terminfo' => '.terminfo',
             },
             'gui' => {'xbindkeysrc' => '.xbindkeysrc',
                       'xmodmaprc' => '.xmodmaprc',
                       'Xresources' => '.Xresources',
                       'xmonad/xmonad.hs' => '.xmonad/xmonad.hs',
                       'stalonetrayrc' => '.stalonetrayrc',
             },
             'bin' => {'bin' => 'bin',
             },
};

@ARGV = keys %{$files} if $ARGV[0] eq 'all';

foreach my $c (@ARGV) {
    while (my ($s, $d) = each %{$files->{$c}}) {
        my $source = "$config_dir/$s";
        my @di = split(/\//, $d);
        if (scalar(@di) > 1) {
            splice(@di, -1, 1);
            my $p = "$ENV{'HOME'}/" . join(/\//, @di);
            unless (-d $p) {
                print "Creating directory $p...\n";
                system "mkdir -p $p";
            }
        }
        my $dest = "$ENV{'HOME'}/$d";
        if (-l $dest) {
            print "Skipping $d. Already linked.\n";
        } elsif (-e "$dest.moved" && -e $dest) {
            print "Skipping $d. Can't make a backup.\n";
        } else {
            system "mv $dest $dest.moved" if -e "$dest";
            print "Linking $s...\n";
            system "ln -s $source $dest";
        }
    }
}
