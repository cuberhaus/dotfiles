" With a map leader it's possible to do extra key combinations
let mapleader=" "

" Activate folds
set foldmethod=marker

" Showcommands
set showcmd

" Sets how many lines of history VIM has to remember
set history=500

" Sets default yank register to same as linux system
if has('win32')
    set clipboard=unnamed
endif

if system('uname -s') == "Darwin\n"
  set clipboard=unnamed "OSX
else
  set clipboard=unnamedplus "Linux
endif

" Enable filetype plugins
filetype plugin indent on

"Fa que el escape sigui instantani
set timeoutlen=1000 ttimeoutlen=0

" Número relatiu a la teva posició en la barra de l'esquerra
set number relativenumber

"activa suport de mouse
set mouse=a

" Disable auto comments
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Set 4 lines to the cursor - when moving vertically using j/k
set so=4

" Turn on the Wild menu, better autocompletion
set wildmenu

" Ignore compiled files
set wildignore=*.o,*~,*.pyc
if has("win16") || has("win32")
    set wildignore+=.git\*,.hg\*,.svn\*
else
    set wildignore+=*/.git/*,*/.hg/*,*/.svn/*,*/.DS_Store
endif

"Always show current position
set ruler

" Configure backspace so it acts as it should act
"set backspace=eol,start,indent

" Ignore case when searching
set ignorecase

" When searching try to be smart about cases
set smartcase

" Highlight search results
set hlsearch

" Makes search act like search in modern browsers
set incsearch

" Don't redraw while executing macros (good performance config)
set lazyredraw

" For regular expressions turn magic on
set magic

" Show matching brackets when text indicator is over them
set showmatch

"This one is the one that works
set belloff=all

" Change cursor to a vertical thin line while in insert mode and underscore while replacing
if system('uname -s') == "Darwin\n"
    "Mode Settings
    let &t_SI.="\e[5 q" "SI = INSERT mode
    let &t_SR.="\e[4 q" "SR = REPLACE mode
    let &t_EI.="\e[1 q" "EI = NORMAL mode (ELSE)
else
    let &t_SI = "\<Esc>[6 q"
    let &t_SR = "\<Esc>[4 q"
    let &t_EI = "\<Esc>[2 q"
endif

" Changes current directory for new files
set autochdir

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

" Colors and Fonts

" Enable syntax highlighting
syntax enable

"Activar sintàxis de programació
syntax on

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8
set fileencoding=utf-8

" Use Unix as the standard file type
set ffs=unix,dos,mac

" :help updatetime
set updatetime=1500

" Use spaces instead of tabs
set expandtab

" Be smart when using tabs ;)
set smarttab

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4

" Linebreak on 500 characters
set lbr
set tw=500

set ai "Auto indent
set si "Smart indent
set wrap "Wrap lines
set breakindent "Indent after line wrapped

" Keybindings

function! GotoJump()
  jumps
  let j = input("Please select your jump: ")
  if j != ''
    let pattern = '\v\c^\+'
    if j =~ pattern
      let j = substitute(j, pattern, '', 'g')
      execute "normal " . j . "\<c-i>"
    else
      execute "normal " . j . "\<c-o>"
    endif
  endif
endfunction

nmap <Leader>j :call GotoJump()<CR>

" Toggle paste mode on and off
nnoremap <leader>pp :setlocal paste!<cr>

" Fast saving
nnoremap <leader>w :w!<cr>

" :W sudo saves the file (useful for handling the permission-denied error)
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!

map <C-down> <C-E>
map <C-up> <C-Y>

" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Smart way to move between windows
map <C-j> <C-W>j
map <C-k> <C-W>k
map <C-h> <C-W>h
map <C-l> <C-W>l

" Close the current buffer :bd will close a buffer
map <leader>bd :bd<cr>
" Close all the buffers
map <leader>ba :bufdo bd<cr>
" Previous buffer
map <leader>bp :bp <cr>
" Next buffer
map <leader>bn :bn <cr>

" Opens a new tab with the current buffer's path
" Super useful when editing files in the same directory
map <leader>te :tabedit <C-r>=expand("%:p:h")<cr>/
" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>tc :tabclose<cr>
map <leader>t<leader> :tabnext <cr>

" Editing mappings

" Press F5 to eliminate trailing whitespaces
:nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

" Remap VIM 0 to first non-blank character
map 0 ^

" Turn persistent undo on
" means that you can undo even when you close a buffer/VIM
try
    set undodir=~/.vim_runtime/temp_dirs/undodir
    set undofile
catch
endtry

" Spelling 

" Demana per descarregar el fixter corresponent
let g:spellfile_URL = 'http://ftp.vim.org/vim/runtime/spell'
"setlocal spell spelllang=en
map <leader>ss :setlocal spell! spelllang=en<cr>
map <leader>se :setlocal spell! spelllang=es<cr>
map <leader>sc :setlocal spell! spelllang=ca<cr>

map <leader>sn ]s
map <leader>sp [s
" zg accepts a word, zug undoes
"map <leader>sa zg
map <leader>? z=

" Enable italics
highlight Comment cterm=italic gui=italic
