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

fcl () {
    local bm_file="$HOME/.bookmarks"
    local dirs=()

    if [[ ! -f "$bm_file" ]]; then
        echo "Error: Bookmark file not found at $bm_file"
        return 1
    fi

    while IFS= read -r line || [[ -n "$line" ]]; do
        # Skip empty lines and comments
        [[ -z "$line" || "$line" =~ ^# ]] && continue
        # Expand tilde to the home directory
        line="${line/#\~/$HOME}"
        if [ -d "$line" ]; then
            dirs+=("$line")
        fi
    done < "$bm_file"

    if [ -n "$1" ] && [ -d "$1" ]; then
        local new_dir
        new_dir=$(cd "$1" && pwd)

        local is_duplicate=false
        for d in "${dirs[@]}"; do
            if [[ "$d" == "$new_dir" ]]; then
                is_duplicate=true
                break
            fi
        done
        if [[ "$is_duplicate" == true ]]; then
            echo "Bookmark already exists."
            cd "$new_dir" && ls
        else
            echo "$new_dir" >> "$bm_file"
            echo "Added bookmark."
            cd "$new_dir" && ls
        fi
        return 0
    fi

    local choices=()
    if [ -n "$1" ]; then
        for d in "${dirs[@]}"; do
            # Case-insensitive substring match
            if [[ "${d,,}" == *${1,,}* ]]; then
                choices+=("$d")
            fi
        done
    else
        choices=("${dirs[@]}")
    fi

    while true; do
        if [ ${#choices[@]} -eq 0 ]; then
            echo "No matching directories found."
            return 1
        elif [ ${#choices[@]} -eq 1 ]; then
            cd "${choices[0]}" && ls
            return 0
        fi

        for i in "${!choices[@]}"; do
            echo "$((i+1))) ${choices[$i]}"
        done

        read -p "Enter a number or string (blank to quit): " input

        if [[ -z "$input" ]]; then
            echo "Cancelled."
            return 1
        fi

        if [[ "$input" =~ ^[0-9]+$ ]] && [ "$input" -ge 1 ] && [ "$input" -le "${#choices[@]}" ]; then
            cd "${choices[$((input-1))]}" && ls
            return 0
        fi

        local new_choices=()
        for d in "${choices[@]}"; do
            if [[ "${d,,}" == *${input,,}* ]]; then
                new_choices+=("$d")
            fi
        done

        choices=("${new_choices[@]}")
    done
}

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
