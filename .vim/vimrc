"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Launch without anything: vim --clean
" :messages per veure els errors per pantalla
" Per comentar una linea gcc per comentar gc per fer moviments

" vim repeat cs'" to change surround from ' to "
" " ysiw" to add " to a word
" " ds" to remove surround

"Jump to category under the cursor<C-]>
"Jump back <C-T>

" gv (re-select last visual select)

" g ctrl A (increase i+1)

" Visual select then press S (and whatever you want to surround with)

" :Obsess to start recording vim session
" vim -S to source the session

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" With a map leader it's possible to do extra key combinations
let mapleader=" "

if has('unix') || has('macunix')
    " Clean-up
    set undodir=$XDG_DATA_HOME/vim/undo
    set directory=$XDG_DATA_HOME/vim/swap
    set backupdir=$XDG_DATA_HOME/vim/backup
    set viewdir=$XDG_DATA_HOME/vim/view
    if has('nvim')
        set viminfo+='1000,n$XDG_DATA_HOME/vim/viminfo
    endif
    if !has('nvim')
        set nocompatible
        " if (cannot open viminfo file for reading) create a cache folder and viminfo
        set viminfo=%,<800,'10,/50,:100,h,f0,n~/.vim/cache/viminfo
"                   | |    |   |   |    | |  + viminfo file path
"                   | |    |   |   |    | + file marks 0-9,A-Z 0=NOT stored
"                   | |    |   |   |    + disable 'hlsearch' loading viminfo
"                   | |    |   |   + command-line history saved
"                   | |    |   + search history saved
"                   | |    + files marks saved
"                   | + lines saved each register (old name for <, vi6.2)
"                   + save/restore buffer list
    endif
endif

" Si plug.vim no existeix ho instala
if has('unix') || has('macunix')
  if empty(glob('~/.vim/autoload/plug.vim'))
      silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                  \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
endif
" Specify a directory for plugins
call plug#begin('~/.vim/plugged')
call plug#begin()
Plug 'Chiel92/vim-autoformat'
Plug 'airblade/vim-gitgutter'
Plug 'ap/vim-css-color'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'godlygeek/tabular'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/goyo.vim'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'preservim/nerdtree'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'ryanoasis/vim-devicons'
Plug 'sheerun/vim-polyglot'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/bufexplorer.zip'
Plug 'ycm-core/YouCompleteMe'
call plug#end()
"Acaben els plugins

"Configuracions del plugin YouCompleteMe
let g:ycm_confirm_extra_conf = 0 "Disables prompting for script every time

if has('unix')
    let g:ycm_clangd_binary_path = exepath("clangd")
endif
if has('macunix')
    let g:ycm_clangd_binary_path = exepath("clangd")
endif

let g:ycm_autoclose_preview_window_after_insertion = 1
"let g:ycm_add_preview_to_completeopt = 1

if !has('nvim')
    set completeopt+=popup
endif

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

" Height of the command bar
set cmdheight=1

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

" How many tenths of a second to blink when matching brackets
"set mat=2

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


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Use 24-bit (true-color) mode in Vim/Neovim when outside tmux.
if (empty($TMUX))
  if (has("nvim"))
    let $NVIM_TUI_ENABLE_TRUE_COLOR=1
  endif
  if (has("termguicolors"))
    set termguicolors
  endif
endif

" Enable syntax highlighting
syntax enable

"Activar sintàxis de programació
syntax on

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8
set fileencoding=utf-8

set t_Co=256

" Dark truecolor
set background=dark
colorscheme onedark
" Configuració per a vim-airline
let g:airline#extensions#tabline#enabled = 1 "fica automaticament els buffers a la barreta

" Use Unix as the standard file type
set ffs=unix,dos,mac

" :help updatetime
set updatetime=1500

" Set font for gvim
if has("gui_running")
    set guifont=SauceCodePro\ Nerd\ Font\ 13
endif
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git etc. anyway...
"set nobackup
"set nowb
"set noswapfile

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Text, tab and indent related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Indent Guides
let g:indent_guides_guide_size = 1
let g:indent_guides_enable_on_vim_startup = 1

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

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

nnoremap <leader>r :YcmCompleter RefactorRename<Space>
nnoremap <leader>d :YcmCompleter GoToDefinition<cr>
nnoremap <leader>T :YcmCompleter GetType<cr>

"ctrlp
let g:ctrlp_map = '<c-f>'

" Toggle paste mode on and off
nnoremap <leader>pp :setlocal paste!<cr>

" Press ctrl+m to toggle markdown preview
nmap <C-m> <Plug>MarkdownPreviewToggle

"Autoformat
noremap <F3> :Autoformat<CR>

if system('uname -s') == "Darwin\n"
    "Toggle nerd Tree ctrl-spacebar
    nmap <C-@> :NERDTreeToggle <CR>
else
    if has('nvim')
        nmap <C-Space> :NERDTreeToggle <CR>
    else
        nmap <NUL> :NERDTreeToggle <CR>
    endif
endif

"Open bufexplorer to see and manage the current buffers (<leader>o):
nnoremap <leader>o :BufExplorer<cr>

" Vimroom
nnoremap <silent> <leader>z :Goyo<cr>

" YankStack
nmap <C-p> <Plug>yankstack_substitute_older_paste
nmap <C-n> <Plug>yankstack_substitute_newer_paste

" Git gutter toggle
nnoremap <silent> <leader>g :GitGutterToggle<cr>

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
" Close a window
map <leader>c <C-W>c
" Vertical split
map <leader>v <C-W>v
" Horizontal split
map <leader>s <C-W>s

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

" Git Gutter
nmap <leader>hn <Plug>(GitGutterNextHunk)
nmap <leader>hp <Plug>(GitGutterPrevHunk)
nmap <leader>hv <Plug>(GitGutterPreviewHunk)
nmap <leader>hs <Plug>(GitGutterStageHunk)
nmap <leader>hu <Plug>(GitGutterUndoHunk)

"Press Enter to jump to the subject (topic) under the cursor.
"Press Backspace to return from the last jump.
"This breaks some stuff
"map <buffer> <CR> <C-]>
"map <buffer> <BS> <C-T>

""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" barra, amaga la barra antiga, set laststatus=2 ensenya la barra sempre
set noshowmode

""""""""""""""""""""""""""""""
" => Spelling
""""""""""""""""""""""""""""""
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

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"" => Editing mappings
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

" PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerd Tree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDTreeWinPos = "right"
let NERDTreeShowHidden=0
let g:NERDTreeWinSize=35
""""""""""""""""""""""""""""""
" => bufExplorer plugin
""""""""""""""""""""""""""""""
let g:bufExplorerDefaultHelp=0
let g:bufExplorerShowRelativePath=1
let g:bufExplorerFindActive=1
let g:bufExplorerSortBy='name'
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vimroom
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:goyo_margin_top = 0
let g:goyo_margin_bottom = 0
""""""""""""""""""""""""""""""
" => YankStack
""""""""""""""""""""""""""""""
let g:yankstack_yank_keys = ['y', 'd']
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git gutter (Git diff)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_enabled=0

" Enable italics
highlight Comment cterm=italic gui=italic
