# oh-my-zsh setup {{{ ---------------------------------------------------------
# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="neh2"

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
plugins=(vi-mode)
plugins+=git
plugins+=extract
plugins+=command-not-found
plugins+=history
plugins+=rsync
# zsh-syntax-highlighting plugin must be last
plugins+=zsh-syntax-highlighting

source $ZSH/oh-my-zsh.sh

zstyle :omz:plugins:ssh-agent agent-forwarding on

# }}}
# Settings {{{ ----------------------------------------------------------------

export EDITOR=vim

typeset -U path cdpath

path+=($HOME/bin(N-/))
path+=($HOME/.cabal/bin(N-/))

HISTSIZE=8000
SAVEHIST=5000
HISTFILE=~/.zshhistory

# make sure ^S and ^Q are not mapped to stop/start so they're freed up for 
# other things (like screen)
stty stop "" start ""

# shortest possible delay when pressing esc
KEYTIMEOUT=1

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

# load any local settings if they exist
if [[ -f $HOME/.local_zshrc ]]
then
    source $HOME/.local_zshrc
fi

# }}}
# Key bindings {{{ ------------------------------------------------------------

bindkey -M vicmd s vi-forward-char
bindkey -M vicmd t down-line-or-history
bindkey -M vicmd n up-line-or-history
bindkey ^r history-incremental-pattern-search-backward
bindkey ^f history-incremental-pattern-search-forward
bindkey -M vicmd v edit-command-line

# set cursor colour as indicator of vi mode
zle-keymap-select () {
    if [ $KEYMAP = vicmd ]; then
        if [[ $TMUX = '' ]]; then
            echo -ne "\033]12;#fffb17\007"
        else
            #printf '\033Ptmux;\033\033]12;#fffb17\007\033\\'
        fi
    else
        if [[ $TMUX = '' ]]; then
            echo -ne "\033]12;#33b1ff\007"
        else
            #printf '\033Ptmux;\033\033]12;#33b1ff\007\033\\'
        fi
    fi
}
zle-line-init () {
    zle -K viins
    if [[ $TMUX = '' ]]; then
        echo -ne "\033]12;#33b1ff\007"
    else
        #printf '\033Ptmux;\033\033]12;#33b1ff\007\033\\'
    fi
}
zle -N zle-keymap-select
zle -N zle-line-init

# }}}
# Aliases {{{ -----------------------------------------------------------------

alias lrt='ls -lrth'
alias lart='ls -larth'

alias au='sudo apt-get update'
alias adu='sudo apt-get dist-upgrade'
alias ai='sudo apt-get install'
alias air='sudo aptitude -R install'
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'

alias ssha='ssh -A'
alias sshx='ssh -X'
alias sshax='ssh -A -X'

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

alias v='vim'
alias vd='vimdiff'
alias t='tmux'

alias glr='git pull --rebase'
compdef _git glr=git-pull

alias gll='git log'
compdef _git gll=git-log
alias glns='git log --name-status'
compdef _git glns=git-log

alias gdns='git diff --name-status'
compdef _git gdns=git-diff
alias gdd='git diff'
compdef _git gdd=git-diff
alias gddc='git diff --cached'

alias gslr='git stash && git pull --rebase'
alias gsp='git stash pop'

alias gn='git annex'

# }}}
# Functions {{{ ---------------------------------------------------------------

# toggle the fullscreen status of the current focused window
tfs () {
    WID=$(xprop -root | awk '/_NET_ACTIVE_WINDOW\(WINDOW\)/{print $NF}')
    wmctrl -i -r $WID -b toggle,fullscreen
}

# }}}
# Completions {{{ -------------------------------------------------------------



# }}}
