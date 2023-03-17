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

export EDITOR="vim"
umask 027

###########
# Aliases #
###########

alias ls='ls --color=auto -F'
alias l=ls
alias ll="ls -o"
alias la="ls -a"

alias hn-copy-pwd="pwd | tr -d '\n' | xclip; pwd"
alias hn-timestamp="date '+%s-%Y_%b_%d'"
alias hn-git-tree="git log --oneline --graph --decorate --all"
alias hn-git-merge="git merge --no-commit --no-ff"

alias emc="emacsclient -n"
alias bcl="bc -ql"

alias hn-du="env ls -A1 | xargs -d '\n' du -shc | sort -h"

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

