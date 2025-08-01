#! /usr/bin/env bash

VIM_COMMANDS=(
    'cnoremap q<CR> :echo "Use :q! instead."<CR>'
    'cnoremap wq<CR> :echo "Use :wq! instead."<CR>'
    'set autochdir'
)

VIM="vim"

if command -v vimx &> /dev/null; then
    VIM=vimx
fi

vim_args=('--servername' 'prime')
for cmd in "${VIM_COMMANDS[@]}"; do
    vim_args+=(-c "$cmd")
done

# Announce the special session to the user.
echo "Running '$VIM ${vim_args[@]}'"

$VIM "${vim_args[@]}" "$@"
