#! /usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
FUNCTIONS="$SCRIPT_DIR/functions.sh"
REPO_DIR="$SCRIPT_DIR/Repos"

yell() { echo -e "\nERROR"; echo "$0: $*" >&2; }
die() { yell "$*"; exit 111; }
try() { "$@" || die "cannot $*"; }

try source "$FUNCTIONS"

clone_repo "https://github.com/hnagiset/verilog3-mode.git"   "$REPO_DIR"/verilog3-mode
clone_repo "https://github.com/nashamri/spacemacs-theme.git" "$REPO_DIR"/spacemacs-theme
clone_repo "https://github.com/protesilaos/standard-themes"  "$REPO_DIR"/standard-themes
clone_repo "https://github.com/doomemacs/themes"             "$REPO_DIR"/doom-themes
clone_repo "https://github.com/volipher/vim-rsi.git"         "$REPO_DIR"/vim-rsi
clone_repo "https://github.com/volipher/vim-gnupg.git"       "$REPO_DIR"/vim-gnupg
clone_repo "https://github.com/volipher/verilog_systemverilog.vim.git" \
                                                             "$REPO_DIR"/verilog_systemverilog.vim

# Emacs
link_file "$SCRIPT_DIR"/.emacs.d                   "$HOME"/.emacs.d
link_file "$REPO_DIR"/verilog3-mode                "$HOME"/.emacs.d/elisp/verilog3-mode
link_file "$REPO_DIR"/spacemacs-theme              "$HOME"/.emacs.d/elisp/spacemacs-theme
link_file "$REPO_DIR"/standard-themes              "$HOME"/.emacs.d/elisp/standard-themes
link_file "$REPO_DIR"/doom-themes                  "$HOME"/.emacs.d/elisp/doom-themes

# Vim
link_file "$SCRIPT_DIR"/.vimrc                     "$HOME"/.vimrc
link_file "$SCRIPT_DIR"/.config/nvim/init.vim      "$XDG_CONFIG_HOME"/nvim/init.vim
link_file "$REPO_DIR"/vim-rsi                      "$HOME"/.vim/pack/vim-rsi/start/vim-rsi
link_file "$REPO_DIR"/vim-gnupg                    "$HOME"/.vim/pack/vim-gnupg/start/vim-gnupg
link_file "$REPO_DIR"/verilog_systemverilog.vim    "$HOME"/.vim/pack/verilog_systemverilog.vim/start/verilog_systemverilog.vim

# Command Line
link_file "$SCRIPT_DIR"/.bashrc                    "$HOME"/.bashrc
link_file "$SCRIPT_DIR"/.bash_profile              "$HOME"/.bash_profile
link_file "$SCRIPT_DIR"/.cshrc                     "$HOME"/.cshrc
link_file "$SCRIPT_DIR"/.inputrc                   "$HOME"/.inputrc
link_file "$SCRIPT_DIR"/.tmux.conf                 "$HOME"/.tmux.conf

# Scripts
link_file "$SCRIPT_DIR"/Bash/hn-convert-epoch.sh   "$HOME"/bin/hn-convert-epoch.sh
link_file "$SCRIPT_DIR"/Bash/hn-firefox.sh         "$HOME"/bin/hn-firefox.sh
link_file "$SCRIPT_DIR"/Bash/hn-chromium.sh        "$HOME"/bin/hn-chromium.sh
link_file "$SCRIPT_DIR"/Csh/hn-tmpdir.csh          "$HOME"/bin/hn-tmpdir.csh

# Misc
copy_file "$SCRIPT_DIR"/.gitconfig.global          "$HOME"/.gitconfig
