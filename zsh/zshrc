#==============================================================================
# Initial setup {{{

typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

autoload -U colors compinit promptinit zsh/terminfo
colors
compinit -C
promptinit

export __CURRENT_GIT_BRANCH=
export EDITOR="vim"

HISTSIZE=8000
SAVEHIST=5000
HISTFILE=~/.zshhistory

# make sure ^S and ^Q are not mapped to stop/start so they're freed up for 
# other things (like screen)
stty stop "" start ""

# Set up the $DISPLAY var to allow vim (and others) to connect back 
# to X from within remote screen sessions.
# Only stores/sets $DISPLAY when running over ssh -X
if [[ "$TERM" != 'screen-bce' ]] && [[ "$SSH_CONNECTION" != '' ]] && [[ "$DISPLAY" != '' ]]; then
    echo $DISPLAY > $HOME/.displayvar;
elif [[ "$TERM" == 'screen-bce' ]] && [[ "$SSH_CONNECTION" != '' ]]; then
    [[ -f $HOME/.displayvar ]] && export DISPLAY=$(cat $HOME/.displayvar);
fi

# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto -h'
fi

# }}}
#==============================================================================
# Options {{{

# Changing Directories
setopt AUTOCD
setopt AUTO_PUSHD
setopt CDABLEVARS
setopt PUSHD_IGNORE_DUPS
setopt PUSHD_TO_HOME

# Completion

# Expansion and Globbing
setopt NO_CASE_GLOB
#setopt EXTENDEDGLOB
setopt NUMERIC_GLOB_SORT

## History
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_FIND_NO_DUPS
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_NO_STORE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt INC_APPEND_HISTORY

# Input/Output
setopt NO_CLOBBER
setopt CORRECT
setopt NO_FLOW_CONTROL

# Job Control
setopt CHECK_JOBS

# Prompting
setopt PROMPT_SUBST

# Zle
setopt NOBEEP
setopt VI
setopt ZLE

# }}}
#==============================================================================
# Aliases {{{

alias au='sudo apt-get update'
alias adu='sudo apt-get dist-upgrade'
alias ai='sudo apt-get install'
alias air='sudo aptitude -R install'
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'

alias scd='screen -X chdir `pwd`'
alias gcd='cd $(git rev-parse --show-toplevel)'

alias less='less -Mircaf'
alias ltail='less +F'
alias df='df -h'
alias psg='ps ax|grep'
alias asdf='setxkbmap dvorak'
alias aoeu='setxkbmap us'
alias hddtemps='for i in /dev/sd? ; do sudo smartctl -d ata -a $i | grep -i tempera ; done'

# this one doesn't work right in zsh... fix? (prob. turn into function. don't 
# think zsh aliases take $N.
#alias topthreads="find /proc/*/status -exec gawk '/^Pid:/ { p=\$2}; /^Name:/ { n=\$2}; /^Threads:/ { t=\$2}; END{ printf(\"%6d %-30s %5d\n\", p, n, t);}' {} \; | sort -k3 -g -r | head -10"

# }}}
#==============================================================================
# Key bindings {{{

bindkey -v
bindkey -M vicmd "s" vi-forward-char
bindkey -M vicmd "t" down-line-or-history
bindkey -M vicmd "n" up-line-or-history
bindkey "^r" history-incremental-pattern-search-backward
bindkey "^f" history-incremental-pattern-search-forward

# }}}
#==============================================================================
# Completions {{{

## case-insensitive (all),partial-word and then substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
zstyle ':completion:*:*:kill:*:processes' command 'ps -axco pid,user,command'

# }}}
#==============================================================================

## Some functions used to put the current vcs branch name in my prompt 
git_prompt_info() {
    BRANCH=$(git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
    STASH=$(git stash list 2> /dev/null | wc -l)
    if [[ $STASH -gt 0 ]]; then
        chars=(¹ ² ³ ⁴ ⁵ ⁶ ⁷ ⁸ ⁹)
        STASHCOUNT="$STASH:"
    fi
    case $(git status 2> /dev/null | tail -n1) in
        'nothing to commit'*)
        echo "%{${fg_bold[yellow]}%}$STASHCOUNT%b%{${fg[green]}%}$BRANCH%b";;
        *)
        echo "%{${fg_bold[yellow]}%}$STASHCOUNT%b%{${fg_bold[red]}%}$BRANCH%b";;
    esac
}
vcs_prompt_update() {
    if which git &> /dev/null; then
        export VCS_PROMPT="$(git_prompt_info)"
    fi
    export VCS_PROMPT_INVALID=
}
vcs_preexec_check() {
    case "$(history $HISTCMD)" in
        *git*) vcs_prompt_update;;
    esac
}
vcs_chpwd_check() {
    [[ $(basename $(pwd)) != '.git' ]] && export VCS_PROMPT_INVALID=1
}
vcs_prompt() {
    test -n "$VCS_PROMPT_INVALID" && vcs_prompt_update
    echo $VCS_PROMPT
}
preexec_functions+='vcs_preexec_check'
chpwd_functions+='vcs_chpwd_check'


## Window/Tab title setting fun
#
# use the current user as the prefix of the current tab title
#TAB_TITLE_PREFIX='"${USER}:"'
# when at the shell prompt, show a truncated version of the current path (with
# standard ~ replacement) as the rest of the title.
case "$TMUX" in
    '') ELLIPSIS="...";;
    *) ELLIPSIS="…";;
esac
TAB_TITLE_PROMPT='`print -Pn "%~" | sed "s:\([~/][^/]*\)/.*/:\1$ELLIPSIS:"`'
# when running a command, show the name of the command as the rest of the
# title (truncate to drop the path to the command)
# also strip out sudo (replace with *), VAR=val
# FIXME doesn't handle commands with absolute paths run via sudo i.e. */usr/bin/man
TAB_TITLE_EXEC='`echo ${$(echo "$cmd" | sed -r -e "s:sudo :*:" -e "s:[A-Z]+=\S+ ::g")[1]}`'

# use the current path (with standard ~ replacement) in square brackets as the
# prefix of the tab window hardstatus.
TAB_HARDSTATUS_PREFIX='`print -Pn "%~ "`'
# when at the shell prompt, use the shell name (truncated to remove the path to
# the shell) as the rest of the title
TAB_HARDSTATUS_PROMPT='$SHELL:t'
# when running a command, show the command name and arguments as the rest of
# the title
TAB_HARDSTATUS_EXEC='$cmd'

# tell GNU screen what the tab window title ($1) and the hardstatus($2) should be
function screen_tab_title_set {  
    #  set the tab window title (%t) for screen
    print -nR $'\033k'$1$'\033'\\\
    # set hardstatus of tab window (%h) for screen
    print -nR $'\033]0;'$2$'\a'
}
function rxvt_title_set {
    print -Pn "\e]0;%n@%m: $1\a"
}
# called by zsh before executing a command
function title_preexec {
    case $TERM in
        (rxvt*|xterm))
        rxvt_title_set $1
        ;;

        screen*)
        local -a cmd; cmd=(${(z)1}) # the command string
        eval "tab_title=$TAB_TITLE_PREFIX$TAB_TITLE_EXEC"
        eval "tab_hardstatus=$TAB_HARDSTATUS_PREFIX$TAB_HARDSTATUS_EXEC"
        screen_tab_title_set $tab_title $tab_hardstatus
        rxvt_title_set $tab_hardstatus
        ;;
    esac
}
# called by zsh before showing the prompt
function title_precmd {
    case $TERM in
        (rxvt*|xterm))
        rxvt_title_set '%~'
        ;;

        screen*)
        eval "tab_title=$TAB_TITLE_PREFIX$TAB_TITLE_PROMPT"
        eval "tab_hardstatus=$TAB_HARDSTATUS_PREFIX$TAB_HARDSTATUS_PROMPT"
        screen_tab_title_set $tab_title $tab_hardstatus
        rxvt_title_set $tab_hardstatus
        ;;
    esac
}
preexec_functions+='title_preexec'
precmd_functions+='title_precmd'


## Prompt addons

function zle-keymap-select {
    # Notification if prompt is in vi command mode
    local VIMODE="${${KEYMAP/vicmd/${fg_bold[red]}!%b}/(main|viins)/}"

    zle reset-prompt
}
zle -N zle-keymap-select

# Indicate background jobs
function jobs_indicator {
    if [[ $(jobs | wc -l) -gt 0 ]]; then
        JOBS=" %{${fg[yellow]}%}⚑%b"
    else
        JOBS=""
    fi
}
precmd_functions+='jobs_indicator'

function pwd_colour {
    if [[ -w $PWD ]]; then
        PWD_COLOUR="%{${fg[green]}%}"
    else
        PWD_COLOUR="%{${fg[red]}%}"
    fi
}
precmd_functions+='pwd_colour'

# change user@host color based on where I am
case "$SSH_CONNECTION" in
    '') COLOUR="%{${fg[green]}%}";;
    *) COLOUR="%{${fg[yellow]}%}";;
esac

PS1='%{${fg_bold[red]}%}%(?..%?%b%{${fg_no_bold[white]}%}:)$(vcs_prompt)%b%{${fg[green]}%}#%b$VIMODE'
RPS1='$JOBS $COLOUR%n@%m%{${fg[default]}%}:${PWD_COLOUR}%3(c.…/.)%2c%{${fg[default]}%}'
