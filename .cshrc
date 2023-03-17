# .cshrc

set path=($path $HOME/bin)

############
# Settings #
############

setenv EDITOR vim

###########
# Aliases #
###########

alias ls ls --color=auto -F
alias l  ls
alias ll ls -o
alias la ls -a

alias hn-copy-pwd  pwd \| tr -d '\\n' \| xclip \; pwd
alias hn-timestamp date '+%s-%Y_%b_%d'
alias hn-git-tree  git log --oneline --graph --decorate --all
alias hn-git-merge git merge --no-commit --no-ff

alias emc emacsclient -n
alias bcl bc -ql
alias cl cd \!:1 \; ls
alias hn-tmpdir source "$HOME/bin/hn-tmpdir.csh"

###########
# Private #
###########

if ( -f $HOME/.cshrc.private ) then
    source $HOME/.cshrc.private
endif
