#! /usr/bin/env bash

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )"
XDG_CONFIG_HOME=${XDG_CONFIG_HOME:-$HOME/.config}
FUNCTIONS="$SCRIPT_DIR/functions.sh"

if [ -f "$FUNCTIONS" ]; then
    source "$FUNCTIONS"
fi

link_file "$SCRIPT_DIR"/.bashrc                    "$HOME"/.bashrc
link_file "$SCRIPT_DIR"/.bash_profile              "$HOME"/.bash_profile
link_file "$SCRIPT_DIR"/.cshrc                     "$HOME"/.cshrc
link_file "$SCRIPT_DIR"/.emacs.d                   "$HOME"/.emacs.d
link_file "$SCRIPT_DIR"/.inputrc                   "$HOME"/.inputrc
link_file "$SCRIPT_DIR"/.tmux.conf                 "$HOME"/.tmux.conf
link_file "$SCRIPT_DIR"/.vimrc                     "$HOME"/.vimrc
link_file "$SCRIPT_DIR"/.config/nvim/init.vim      "$XDG_CONFIG_HOME"/nvim/init.vim
copy_file "$SCRIPT_DIR"/.gitconfig.global          "$HOME"/.gitconfig
link_file "$SCRIPT_DIR"/Bash/hn-convert-epoch.sh   "$HOME"/bin/hn-convert-epoch.sh
link_file "$SCRIPT_DIR"/Bash/hn-firefox.sh         "$HOME"/bin/hn-firefox.sh
link_file "$SCRIPT_DIR"/Bash/hn-chromium.sh        "$HOME"/bin/hn-chromium.sh
link_file "$SCRIPT_DIR"/Csh/hn-tmpdir.csh          "$HOME"/bin/hn-tmpdir.csh

# .emacs.d/
clone_repo "https://github.com/hnagiset/verilog3-mode.git" \
    "$HOME"/.emacs.d/elisp/verilog3-mode/
clone_repo "https://github.com/nashamri/spacemacs-theme.git" \
    "$HOME"/.emacs.d/elisp/spacemacs-theme/
clone_repo "https://github.com/protesilaos/standard-themes" \
    "$HOME"/.emacs.d/elisp/standard-themes/
clone_repo "https://github.com/doomemacs/themes" \
    "$HOME"/.emacs.d/elisp/doom-themes/
# .vim/
clone_repo "https://github.com/volipher/vim-rsi.git" \
    "$HOME"/.vim/pack/vim-rsi/start/vim-rsi/
clone_repo "https://github.com/volipher/vim-gnupg.git" \
    "$HOME"/.vim/pack/vim-gnupg/start/vim-gnupg/
clone_repo "https://github.com/volipher/verilog_systemverilog.vim.git" \
    "$HOME"/.vim/pack/verilog_systemverilog.vim/start/verilog_systemverilog.vim/

