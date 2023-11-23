" General Settings {{{1

filetype plugin indent on
set autoindent
set scrolloff=0
set foldmethod=marker
set wildmenu
set incsearch
" For the semicolon, see :help file-searching
set tags=./tags;
set ttimeoutlen=50
set tabstop=8
set softtabstop=4
set shiftwidth=4
set expandtab
set nomodeline
set hidden
set path+=**
" set autochdir

runtime macros/matchit.vim

syntax on
set number
set laststatus=1
set ruler
set list
set listchars=tab:\`\ ,trail:•

set background=light
if has('gui_running')
"    colorscheme solarized
     set lines=27 columns=90
endif

" Mappings {{{1

let mapleader = ","
let maplocalleader = "\<Space>"

inoremap jk         <Esc>
" <Tab> is also CTRL-I
"nnoremap <Tab>      za
nnoremap <Leader>   :echo "
            \ b: Show Buffers\n
            \ d: Insert Date\n
            \ o: Open\n
            \ p: Preferences\n
            \ q: Delete Buffer\n
            \ s: Spell Check\n
            \ t: Tags\n
            \ w: Windows" 
            \\| call HNFinishKeyMapping("\<Leader>")<CR>
nnoremap <Leader>b  :ls<CR>:b<Space>
nnoremap <Leader>d  :r! date<CR>
nnoremap <Leader>o  :echo "
            \ m: Open Bookmarks\n
            \ n: Open Notes\n
            \ s: Open Scratch\n
            \ t: Open Tree"
            \\| call HNFinishKeyMapping("\<Leader>o")<CR>
nnoremap <Leader>om :sp ~/.marks<CR>
nnoremap <Leader>on :sp ~/.notes<CR>
nnoremap <Leader>os :HNScratchBuffer<CR>
nnoremap <Leader>ot :Vexplore<CR>
nnoremap <Leader>p  :echo "
            \ t: Theme\n
            \ w: Toggle Line-Wrap"
            \\| call HNFinishKeyMapping("\<Leader>p")<CR>
nnoremap <Leader>pt  :echo "
            \ l: Light\n
            \ d: Dark"
            \\| call HNFinishKeyMapping("\<Leader>pt")<CR>
nnoremap <Leader>ptl :set background=light<CR>
nnoremap <Leader>ptd :set background=dark<CR>
nnoremap <Leader>pw :setlocal invwrap<CR>
nnoremap <Leader>q  :bd<CR>
nnoremap <Leader>s  :echo "
            \ a: Add Spelling\n
            \ i: Ignore Spelling\n
            \ n: Next Error\n
            \ p: Previous Error\n
            \ r: Remove Spelling\n
            \ t: Toggle Spell Check On/Off"
            \\| call HNFinishKeyMapping("\<Leader>s")<CR>
nnoremap <Leader>sa zg
nnoremap <Leader>si zG
nnoremap <Leader>sn ]s
nnoremap <Leader>sp [s
nnoremap <Leader>sr zug
nnoremap <Leader>st :setlocal invspell<CR>
nnoremap <Leader>t  :echo "
            \ f: Follow Tag\n
            \ l: List Tags\n
            \ n: Next (Push) Tag\n
            \ p: Previous (Pop) Tag"
            \\| call HNFinishKeyMapping("\<Leader>t")<CR>
nnoremap <Leader>tf g]
nnoremap <Leader>tl :tags<CR>
nnoremap <Leader>tn :tag<CR>
nnoremap <Leader>tp <C-t>
nnoremap <Leader>w  :echo "
            \ hjkl: Navigate\n
            \ c: Close Window\n
            \ o: Only Window"
            \\| call HNFinishKeyMapping("\<Leader>w")<CR>
nnoremap <Leader>wh <C-w>h
nnoremap <Leader>wj <C-w>j
nnoremap <Leader>wk <C-w>k
nnoremap <Leader>wl <C-w>l
nnoremap <Leader>wc <C-w>c
nnoremap <Leader>wo <C-w>o

" Commands {{{1

command!          HNCopyFileName  let @*=expand("%:p") | echo @*
command!          HNChangeDir     cd %:h
command!          HNWriteBackup   execute "w %:p." . system("date +%s")
command! -range   HNEncryptRegion <line1>,<line2>! gpg -ca
command! -range   HNDecryptRegion <line1>,<line2>! gpg -dq
command! -range=% HNExecutePython <line1>,<line2>call HNExecuteRange("python")
command! -range=% HNExecuteBash   <line1>,<line2>call HNExecuteRange("bash")
command!          HNScratchBuffer call HNScratchBuffer()

" Functions {{{1

" Repeat input key prefixed with previous keys
function! HNFinishKeyMapping(keys)
    let key = input("Enter Key: ")
    redraw
    if len(key) > 0
        call feedkeys(a:keys . key)
    endif
endfunction

" Prompt user and create file at dir
function! HNCreateFile(dir)
    let file = input('Enter File Name: ')
    if len(file) > 0
        execute "!touch " . a:dir . "/" . file
    endif
endfunction

" Backup file if required
function! HNBackupFile()
    let extension = expand("%:e")
    " ==? is Case Insensitive
    if extension ==? "GPG"
        execute "!mkdir -p ~/nobackup/gpg/"
        execute "!cp % ~/nobackup/gpg/%:t." . system("date +%s")
    endif
endfunction

" Filter range with command in new buffer
function! HNExecuteRange(command) range
    " Yank range and restore register
    let savereg = @a
    execute "normal! " . a:firstline . "gg\"ay" . a:lastline . "gg"
    let input = @a
    let @a = l:savereg
    " Open a new split and set it up.
    split *Results*
    normal! ggdG
    setlocal buftype=nofile
    " Paste yanked range
    call append(0, split(input, '\v\n'))
    execute "normal! ggVG:!" . a:command . "\<CR>"
endfunction

" Create scratch buffer
function! HNScratchBuffer()
    edit *Scratch*
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    " if persistent undo is present, disable it for this buffer
    if exists('+undofile')
      setl noundofile
    endif
endfunction

" Terminal Mode {{{1

if exists (":tnoremap")
    tnoremap <Esc> <C-\><C-n>
    tnoremap jk <C-\><C-n>
    " Insert register contents with Ctrl-R
    tnoremap <expr> <C-R> '<C-\><C-N>"'.nr2char(getchar()).'pi'
endif

" Autocommands {{{1

augroup vimrc_all
    au!
    au BufWritePre,FileWritePre * call HNBackupFile()
augroup END

augroup vimrc_text
    au!
    "au FileType text setlocal colorcolumn=80
    au FileType text setlocal textwidth=79
    "au FileType text setlocal spell
augroup END

augroup vimrc_org
    au!
    au BufNewFile,BufRead *.org nnoremap <buffer> <LocalLeader>  :echo "
                \ i: Insert"
                \\| call HNFinishKeyMapping("\<LocalLeader>")<CR>
    au BufNewFile,BufRead *.org nnoremap <LocalLeader>i  :echo "
                \ q: Insert Quote\n
                \ s: Insert Source"
                \\| call HNFinishKeyMapping("\<LocalLeader>i")<CR>
    au BufNewFile,BufRead *.org nnoremap <buffer>
                \<LocalLeader>iq o#+begin_quote<Esc>o#+end_quote<Esc>
    au BufNewFile,BufRead *.org nnoremap <buffer>
                \<LocalLeader>is o#+begin_src sh<Esc>o#+end_src<Esc>
augroup END

augroup vimrc_systemverilog
    au!
    au FileType verilog_systemverilog setlocal foldmethod=manual
    "au FileType verilog_systemverilog setlocal colorcolumn=100
    au FileType verilog_systemverilog,verilog setlocal softtabstop=2
    au FileType verilog_systemverilog,verilog setlocal shiftwidth=2
    " Key Mappings
    au FileType verilog_systemverilog nnoremap <buffer> <LocalLeader>  :echo "
                \ i: Go to start of instance\n
                \ I: Follow instance"
                \\| call HNFinishKeyMapping("\<LocalLeader>")<CR>
    au FileType verilog_systemverilog nnoremap <buffer> <LocalLeader>i :VerilogGotoInstanceStart<CR>
    au FileType verilog_systemverilog nnoremap <buffer> <LocalLeader>I :VerilogFollowInstance<CR>
augroup END

augroup vimrc_netrw
    au!
    au FileType netrw nnoremap <buffer> <LocalLeader>   :echo "
                \ c: Create File/Directory\n
                \ d: Cycle Hide Dotfiles\n
                \ h: Netrw Help\n
                \ u: Go Up"
                \\| call HNFinishKeyMapping("\<LocalLeader>")<CR>
    au FileType netrw nnoremap <buffer> <LocalLeader>c  :echo "
                \ d: Create Directory\n
                \ f: Create File"
                \\| HNFinishKeyMapping("\<LocalLeader>c")<CR>
    au FileType netrw nmap     <buffer> <LocalLeader>cd d
    au FileType netrw nnoremap <buffer> <LocalLeader>cf :call 
                \HNCreateFile(b:netrw_curdir)<CR>
    au FileType netrw nmap     <buffer> <LocalLeader>d  a
    au FileType netrw nnoremap <buffer> <LocalLeader>h  :help netrw-quickmap<CR>
    au FileType netrw nmap     <buffer> <LocalLeader>u  -<Esc>
augroup END

augroup vimrc_notes
    au!
    au BufNewFile,BufRead *.notes set filetype=temporary syntax=conf
augroup END

augroup vimrc_marks
    au!
    au BufNewFile,BufRead *.marks set filetype=temporary syntax=markdown
    " gf (follow file) after first "(" on that line
    au BufNewFile,BufRead *.marks nnoremap <buffer> <CR> 0f(lgf
augroup END

augroup vimrc_temporary
    au!
    au FileType temporary setlocal noswapfile
augroup END

" Gnupg Settings {{{1

let g:GPGPreferSymmetric = 0
let g:GPGPreferArmor = 1

" Verilog-SystemVerilog Settings {{{1

let g:verilog_syntax_fold_lst = "all"

" Netrw Settings {{{1

let g:netrw_banner = 0
let g:netrw_liststyle = 0
"let g:netrw_browse_split = 4
let g:netrw_browse_split = 0
let g:netrw_altv = 1
let g:netrw_winsize = 25

" hide by default
let g:netrw_list_hide = '^\..*'
let g:netrw_hide = 1

" Other Settings {{{1

if filereadable($HOME . "/.vimrc.private")
    source $HOME/.vimrc.private
endif
