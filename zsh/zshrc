# oh-my-zsh setup {{{ ---------------------------------------------------------
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="neh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git vi-mode extract)

source $ZSH/oh-my-zsh.sh

# }}}
# Settings {{{ ----------------------------------------------------------------

HISTSIZE=8000
SAVEHIST=5000
HISTFILE=~/.zshhistory

# make sure ^S and ^Q are not mapped to stop/start so they're freed up for 
# other things (like screen)
stty stop "" start ""

# custom window title truncation for tmux
if [ $TMUX ]
then
    ZSH_THEME_TERM_TAB_TITLE_IDLE="%2(c.…/.)%1c"
fi

# Set up the $DISPLAY var to allow vim (and others) to connect back 
# to X from within remote screen sessions.
# Only stores/sets $DISPLAY when running over ssh -X
if [[ "$TERM" != 'screen-bce' ]] && [[ "$SSH_CONNECTION" != '' ]] && [[ "$DISPLAY" != '' ]]; then
    echo $DISPLAY > $HOME/.displayvar;
elif [[ "$TERM" == 'screen-bce' ]] && [[ "$SSH_CONNECTION" != '' ]]; then
    [[ -f $HOME/.displayvar ]] && export DISPLAY=$(cat $HOME/.displayvar);
fi

# }}}
# Key bindings {{{ ------------------------------------------------------------

bindkey -M vicmd s vi-forward-char
bindkey -M vicmd t down-line-or-history
bindkey -M vicmd n up-line-or-history
bindkey ^r history-incremental-pattern-search-backward
bindkey ^f history-incremental-pattern-search-forward
bindkey -M vicmd v edit-command-line

# }}}
# Aliases {{{ -----------------------------------------------------------------

alias au='sudo apt-get update'
alias adu='sudo apt-get dist-upgrade'
alias ai='sudo apt-get install'
alias air='sudo aptitude -R install'
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'

alias scd='screen -X chdir `pwd`'
alias tcd='tmux set-option default-path `pwd`'
alias gcd='cd $(git rev-parse --show-toplevel)'

alias less='less -Mircaf'
alias ltail='less +F'
alias df='df -h'
alias psg='ps ax|grep'
alias asdf='setxkbmap dvorak'
alias aoeu='setxkbmap us'

alias myip='curl http://whatthefuckismyip.com'

alias vpup='cd ~/myconfig/vim/vim/bundle;for p in `ls`; do cd $p;git checkout master && git pull;cd ..; done'

alias t='tmux'

# }}}
# Completions {{{ -------------------------------------------------------------



# }}}
