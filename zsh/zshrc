
autoload colors zsh/terminfo
if [[ "$terminfo[colors]" -ge 8 ]]; then
    colors
fi

export __CURRENT_GIT_BRANCH=
EDITOR='vim -p'

# Nice directory truncation with a proper ellipsis: %30<…<

typeset -ga preexec_functions
typeset -ga precmd_functions
typeset -ga chpwd_functions

setopt nobeep
setopt autocd
#setopt extendedglob
setopt correct
setopt NO_CLOBBER
setopt prompt_subst

## History
setopt HIST_VERIFY
setopt EXTENDED_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_FIND_NO_DUPS
setopt HIST_EXPIRE_DUPS_FIRST
setopt HIST_IGNORE_ALL_DUPS
setopt HIST_REDUCE_BLANKS
setopt HIST_NO_STORE

HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zshhistory


# enable color support of ls and also add handy aliases
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto -h'
fi
alias vim='vim -p'
alias m4a2ogg='for nam in *.m4a; do nice mplayer -vo null -vc null -ao pcm:fast "$nam" -ao pcm:file="$nam.wav" && nice oggenc -q5 "$nam.wav" -o "$(basename "$nam" .m4a).ogg"; rm "$nam.wav"; done'
alias au='sudo aptitude update'
alias afu='sudo aptitude full-upgrade'
alias less='less -Mircaf'
alias ltail='less +F'
alias rm='rm --one-file-system'
alias vless='vim -u /usr/share/vim/vim71/macros/less.vim'
alias df='df -h'
alias acs='apt-cache search'
alias acsn='apt-cache search --names-only'
alias ai='sudo apt-get install'
alias psg='ps ax|grep'
alias asdf='setxkbmap dvorak'
alias aoeu='setxkbmap us'
alias xmb='pushd `pwd`; cd $HOME/xmonad/xmonad; runhaskell Setup.lhs build; popd'
alias xmi='pushd `pwd`; cd $HOME/xmonad/xmonad; rm -f $HOME/bin/xmonad; runhaskell Setup.lhs install --user; popd'
alias xmbi='xmb && xmi'
alias hddtemps='for i in /dev/sd? ; do sudo smartctl -d ata -a $i | grep -i tempera ; done'
alias scd='screen -X chdir `pwd`'

# this one doesn't work right in zsh... fix? (prob. turn into function. don't think zsh aliases take $N.
#alias topthreads="find /proc/*/status -exec gawk '/^Pid:/ { p=\$2}; /^Name:/ { n=\$2}; /^Threads:/ { t=\$2}; END{ printf(\"%6d %-30s %5d\n\", p, n, t);}' {} \; | sort -k3 -g -r | head -10"

# TODO can I make zsh understand .desktop files (for execution and completion of friendly names)

# hash the cwd to a short ~name
setopt CDABLEVARS
hit() { hash -d $1=$PWD }

# make sure ^S and ^Q are not mapped to stop/start so they're freed up for other things (like screen)
stty stop "" start ""

bindkey -v
bindkey -M vicmd "s" vi-forward-char
bindkey -M vicmd "t" down-line-or-history
bindkey -M vicmd "n" up-line-or-history


# TODO completions don't seem to work for the very first cmd in an new shell. things like C-r don't either.

## Completions
autoload -U compinit
compinit -C

## case-insensitive (all),partial-word and then substring completion
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'

## Some functions used to put the current vcs branch name in my prompt
git_prompt() {
    if which git &> /dev/null; then
        BRANCH=$(git-branch --no-color 2> /dev/null \
        | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/')
        case $(git status 2> /dev/null | tail -n1) in
            'nothing to commit'*)
            echo "%{${fg[green]}%}$BRANCH%b";;
            *)
            echo "%{${fg_bold[red]}%}$BRANCH%b";;
        esac
    fi
}
vcs_check() {
    VCS_PROMPT="$(git_prompt)"
}
preexec_functions+='vcs_check'
precmd_functions+='vcs_check'
chpwd_functions+='vcs_check'


## Window/Tab title setting fun
#
# use the current user as the prefix of the current tab title
#TAB_TITLE_PREFIX='"${USER}:"'
# when at the shell prompt, show a truncated version of the current path (with
# standard ~ replacement) as the rest of the title.
TAB_TITLE_PROMPT='`print -Pn "%~" | sed "s:\([~/][^/]*\)/.*/:\1...:"`'
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
        rxvt*)
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
        rxvt*)
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
    #VIMODE="${${KEYMAP/vicmd/${fg_bold[red]}!%b}/(main|viins)/}"
    if [[ $KEYMAP = vicmd ]]; then
        local VIMODE="%{${fg_bold[red]}%}!%b"
    else
        local VIMODE=""
    fi

    zle reset-prompt
}
zle -N zle-keymap-select

# Indicate background jobs
function jobs_indicator {
    if [[ $(jobs | wc -l) -gt 0 ]]; then
        JOBS=" *"
    else
        JOBS=""
    fi
}
precmd_functions+='jobs_indicator'

# change user@host color based on where I am
case "$SSH_CONNECTION" in
    '') COLOUR="%B%{${fg[green]}%}";;
    *) COLOUR="%B%{${fg[yellow]}%}";;
esac

PS1='%{${fg_bold[red]}%}%(?..%?%b%{${fg_no_bold[white]}%}:% )$COLOUR%n@%m%{${fg[default]}%}$JOBS%b $VIMODE'
RPS1='$VCS_PROMPT %{${fg[green]}%}%~%{${fg[default]}%}'
