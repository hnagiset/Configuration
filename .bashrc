# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

############
# Settings #
############

# PS1=" [\u@\h \W]\\$ "
# PS1="\[\e[00;34m\][\u@\h \W]\\$ \[\e[0m\]"
PS1="\[\e[0;34m\][\u@\h \[\e[1;32m\]\W\[\e[0;34m\]]\\$ \[\e[0m\]"

export EDITOR="vim"
umask 027

export HISTFILESIZE=-1
export HISTSIZE=-1
export HISTFILE=~/.bash_history_2
export HISTTIMEFORMAT="[%F %T] "
# prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"

###########
# Aliases #
###########

alias ls='ls --color=auto -F'
alias l=ls
alias ll="ls -l"
alias la="ls -a"

alias hn-copy-pwd="pwd | tr -d '\n' | xclip; pwd"
alias hn-timestamp=" date '+%s.%Y.%b.%d'"
alias hn-git-tree="git log --oneline --graph --decorate --all"
alias hn-git-merge="git merge --no-commit --no-ff"

alias emc="emacsclient -n"
alias vmc="vim --servername prime --remote"
alias bcl="bc -ql"

alias hn-du="env ls -A1 | xargs -d '\n' du -shc | sort -h"

alias ..="cd ..; ls"
alias ....="cd ..; cd ..; ls"

#############
# Functions #
#############

cl () { cd "$1"; ls; }

hn-gen-password () {
    tr -dc A-Za-z0-9 < /dev/urandom | head -c $1 ; echo ''
}

hn-tmpdir () { cd $(mktemp -d); pwd; }

###########
# Private #
###########

if [ -f ~/.bashrc.private ]; then
    source ~/.bashrc.private
fi

#########
# Local #
#########

if [ -f ~/.bashrc.local ]; then
    source ~/.bashrc.local
fi
