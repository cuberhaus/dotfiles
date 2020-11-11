"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Sections:
"    -> General
"    -> VIM user interface
"    -> Colors and Fonts
"    -> Files and backups
"    -> Text, tab and indent related
"    -> Visual mode related
"    -> Moving around, tabs and buffers
"    -> Status line
"    -> Editing mappings
"    -> vimgrep searching and cope displaying
"    -> Spell checking
"    -> Misc
"    -> Helper functions
"    -> EXTENDED
"    -> FILETYPES
"    -> PLUGINS
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" Launch without anything: vim --clean
" Install vim-gtk
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

if has('unix') || has('macunix')
    "echo 'test'
    "autocmd VimEnter * echo 'test'
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

" With a map leader it's possible to do extra key combinations
" like <leader>w saves the current file
"nnoremap <SPACE> <Nop>
"map <Space> <leader>
let mapleader=" "
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
" Same as this 3 commands together: filetype on | filetype plugin on | filetype indent on

" Set to auto read when a file is changed from the outside
set autoread
au FocusGained,BufEnter * checktime

"Fa que el escape sigui instantani
set timeoutlen=1000 ttimeoutlen=0

" Número relatiu a la teva posició en la barra de l'esquerra
:set number relativenumber

"Activar sintàxis de programació
:syntax on

"activa suport de mouse
:set mouse=a

" Disable auto comments
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Si plug.vim no existeix ho instala
if has('unix') || has('macunix')
  if empty(glob('~/.vim/autoload/plug.vim'))
      silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
                  \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
      autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
  endif
endif
" tabular comes before vim markdown
" Specify a directory for plugins
call plug#begin('~/.vim/plugged')
call plug#begin()
Plug 'Chiel92/vim-autoformat'
Plug 'NLKNguyen/papercolor-theme'
Plug 'airblade/vim-gitgutter'
Plug 'amix/vim-zenroom2'
Plug 'ap/vim-css-color'
Plug 'chriskempson/base16-vim'
Plug 'christoomey/vim-tmux-navigator'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'dracula/vim', { 'as': 'dracula' }
Plug 'easymotion/vim-easymotion'
Plug 'godlygeek/tabular'
Plug 'iamcco/markdown-preview.nvim', { 'do': 'cd app & yarn install'  }
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/goyo.vim'
Plug 'maxbrunsfeld/vim-yankstack'
Plug 'nathanaelkane/vim-indent-guides'
Plug 'plasticboy/vim-markdown'
Plug 'preservim/nerdtree'
Plug 'prettier/vim-prettier', { 'do': 'yarn install' }
Plug 'ryanoasis/vim-devicons'
Plug 'sheerun/vim-polyglot'
Plug 'tmux-plugins/vim-tmux'
Plug 'tmux-plugins/vim-tmux-focus-events'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-obsession'
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
let g:ycm_clangd_uses_ycmd_caching = 0
let g:ycm_clangd_args = ['-log=verbose', '-pretty']

if has('unix')
    let g:ycm_clangd_binary_path = exepath("clangd")
endif
if has('macunix')
    let g:ycm_clangd_binary_path = exepath("clangd")
endif

let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_add_preview_to_completeopt = 1

if !has('nvim')
    set completeopt+=popup
endif

function s:Hover()
    " get the doc string from YCM
    let response = youcompleteme#GetCommandResponse('GetDoc')
    if response == ''
        return
    endif
    " set the width
    let s:width = min([winwidth('%') * 9 / 10, 100])
    " calculate the height to show the whole doc with wrap enabled
    let lines = split(response, '\n')
    let s:height = len(lines) + 1
    for s:line in lines
        let s:height = s:height + len(s:line) / s:width
    endfor
    " nvim floating window interface
    let buf = nvim_create_buf(v:false, v:true)
    call nvim_buf_set_lines(buf, 0, -1, v:true, lines)
    let opts = {'relative': 'cursor', 'width': s:width, 'height': len(lines) + 1, 'col': 1,
                \ 'row': 0, 'anchor': 'SW', 'style': 'minimal'}
    let s:win = nvim_open_win(buf, 0, opts)
    " set the window option
    call nvim_win_set_option(s:win, 'winhl', 'Normal:NormalFloat')
    call nvim_win_set_option(s:win, 'wrap', v:true)
    call nvim_win_set_option(s:win, 'linebreak', v:true)
    " close the window once the cursor moved
    autocmd CursorMoved <buffer> ++once call nvim_win_close(s:win, v:false)
endfunction

command YcmGetDocFloatWin :call <SID>Hover()
autocmd FileType c,cpp,h,hpp nmap <leader>k :YcmGetDocFloatWin<cr>

set guifont="SauceCodePro\ Nerd\ Font\ 13"

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => VIM user interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Set 4 lines to the cursor - when moving vertically using j/k
set so=4

" Avoid garbled characters in Chinese language windows OS
let $LANG='en'
set langmenu=en
source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim

" Turn on the Wild menu
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

" A buffer becomes hidden when it is abandoned
set hid

" Configure backspace so it acts as it should act
set backspace=eol,start,indent
" Move across lines with h and l
"set whichwrap+=<,>,h,l

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
set mat=2

"This one is the one that works
set belloff=all

" Properly disable sound on errors on MacVim
if has("gui_macvim")
    autocmd GUIEnter * set vb t_vb=
endif

" Add a bit extra margin to the left
set foldcolumn=1

" Change cursor to a vertical thin line while in insert mode and underscore while replacing
if has("autocmd")
  au VimEnter,InsertLeave * silent execute '!echo -ne "\e[2 q"' | redraw!
  au InsertEnter,InsertChange *
\ if v:insertmode == 'i' |
\   silent execute '!echo -ne "\e[6 q"' | redraw! |
\ elseif v:insertmode == 'r' |
\   silent execute '!echo -ne "\e[4 q"' | redraw! |
\ endif
au VimLeave * silent execute '!echo -ne "\e[ q"' | redraw!
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

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

" Changes current directory for new files
set autochdir

set encoding=utf-8
set fileencoding=utf-8

" set Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

" Tema
if filereadable(expand("~/.vimrc_background"))
    let base16colorspace=256
    source ~/.vimrc_background
endif
" TRUECOLOR SCHEMES uncheck to enable
"autocmd vimenter * colorscheme gruvbox
set t_Co=256

" Dark truecolor
set background=dark
colorscheme onedark
function! Dark()
    set background=dark
    colorscheme onedark
endfunction
command Dark :call Dark()

" Light truecolor
function! Light()
    set background=light
    colorscheme PaperColor
endfunction

command Light :call Light()
" This is what sets vim to use 24-bit colors. It will also work for any version of neovim
set termguicolors

"Barra
" Configuració per a vim-airline 
let g:airline#extensions#tabline#enabled = 1 "fica automaticament els buffers a la barreta
"let g:airline_theme='dracula'

" Set extra options when running in GUI mode
if has("gui_running")
    set guioptions-=T
    set guioptions-=e
    set t_Co=256
    set guitablabel=%M\ %t
endif

" Set utf8 as standard encoding and en_US as the standard language
set encoding=utf8

" Use Unix as the standard file type
set ffs=unix,dos,mac

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Turn backup off, since most stuff is in SVN, git etc. anyway...
set nobackup
"set nowb
set noswapfile

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
"Si estas en una linea i li dons al enter va a la següent línea amb la mateixa identació
:set autoindent

""""""""""""""""""""""""""""""
" => Visual mode related
""""""""""""""""""""""""""""""

" Visual mode pressing * or # searches for the current selection
" Super useful! From an idea by Michael Naumann
vnoremap <silent> * :<C-u>call VisualSelection('', '')<CR>/<C-R>=@/<CR><CR>
vnoremap <silent> # :<C-u>call VisualSelection('', '')<CR>?<C-R>=@/<CR><CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Per comentar una linea gcc per comentar gc per fer moviments
" vim repeat cs'" to change surround from ' to "
" " ysiw" to add " to a word
" " ds" to remove surround

" :Obsess to start recording vim session
" vim -S to source the session

" Press ctrl+m to toggle markdown preview
nmap <C-m> <Plug>MarkdownPreviewToggle

" Toggle fullscreen
"noremap <M-f> :FullscreenToggle<CR>

"Autoformat
noremap <F3> :Autoformat<CR>

if system('uname -s') == "Darwin\n"
    "Toggle nerd Tree ctrl-spacebar
    nmap <c-@> :NERDTreeToggle <CR>
else
    nmap <c-Space> :NERDTreeToggle <CR>
endif

"Open bufexplorer to see and manage the current buffers (<leader>o):
map <leader>o :BufExplorer<cr>

" Ctrlp Quickly find and open a file in the current working directory
let g:ctrlp_map = '<C-f>'
let g:ctrlp_cmd = 'CtrlP'

" FZF
"map <leader>f :Files ~/<cr>

" Vimroom
nnoremap <silent> <leader>z :Goyo<cr>

" YankStack
nmap <C-p> <Plug>yankstack_substitute_older_paste
nmap <C-n> <Plug>yankstack_substitute_newer_paste

" Git gutter toggle
nnoremap <silent> <leader>d :GitGutterToggle<cr>

" Fast saving
nmap <leader>w :w!<cr>

" :W sudo saves the file (useful for handling the permission-denied error)
command! W execute 'w !sudo tee % > /dev/null' <bar> edit!

map <C-down> <C-E>
map <C-up> <C-Y>
" Disable highlight when <leader><cr> is pressed
map <silent> <leader><cr> :noh<cr>

" Edit file |   DO NOT take out the trailing whitespace on this line
map <leader>e :e

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
map <leader>bd :Bclose<cr>
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
"nmap [c <Plug>(GitGutterNextHunk)
"nmap ]c <Plug>(GitGutterPrevHunk)
nmap <leader>hn <Plug>(GitGutterNextHunk)
nmap <leader>hp <Plug>(GitGutterPrevHunk)
nmap <leader>hv <Plug>(GitGutterPreviewHunk)
nmap <leader>hs <Plug>(GitGutterStageHunk)
nmap <leader>hu <Plug>(GitGutterUndoHunk)

" :help updatetime 
set updatetime=100

"Press Enter to jump to the subject (topic) under the cursor.
"Press Backspace to return from the last jump.
"This breaks some stuff
"map <buffer> <CR> <C-]>
"map <buffer> <BS> <C-T>

""""""""""""""""""""""""""""""
" => Moving around
""""""""""""""""""""""""""""""
try
    set switchbuf=useopen,usetab,newtab
    set stal=2
catch
endtry

" Return to last edit position when opening files (You want this!)
au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif

""""""""""""""""""""""""""""""
" => Status line
""""""""""""""""""""""""""""""
" Always show the status line
set laststatus=2

" barra, amaga la barra antiga, set laststatus=2 ensenya la barra sempre
set noshowmode

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Editing mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Press F5 to eliminate trailing whitespaces
:nnoremap <silent> <F5> :let _s=@/ <Bar> :%s/\s\+$//e <Bar> :let @/=_s <Bar> :nohl <Bar> :unlet _s <CR>

" Remap VIM 0 to first non-blank character
map 0 ^

if has("mac") || has("macunix")
    nmap <D-j> <M-j>
    nmap <D-k> <M-k>
    vmap <D-j> <M-j>
    vmap <D-k> <M-k>
endif

" Delete trailing white space on save, useful for some filetypes ;)
fun! CleanExtraSpaces()
    let save_cursor = getpos(".")
    let old_query = getreg('/')
    silent! %s/\s\+$//e
    call setpos('.', save_cursor)
    call setreg('/', old_query)
endfun

if has("autocmd")
    autocmd BufWritePre *.txt,*.js,*.py,*.wiki,*.sh,*.coffee :call CleanExtraSpaces()
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Misc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Quickly open a buffer for scribble
map <leader>q :e ~/buffer<cr>

" Toggle paste mode on and off
map <leader>pp :setlocal paste!<cr>

" Quickly open a markdown buffer for scribble
"map <leader>pm :e ~/buffer.md<cr>

" Remove the Windows ^M - when the encodings gets messed up
"noremap <Leader>m mmHmt:%s/<C-V><cr>//ge<cr>'tzt'm

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Helper functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Don't close window, when deleting a buffer
command! Bclose call <SID>BufcloseCloseIt()
function! <SID>BufcloseCloseIt()
    let l:currentBufNum = bufnr("%")
    let l:alternateBufNum = bufnr("#")

    if buflisted(l:alternateBufNum)
        buffer #
    else
        bnext
    endif

    if bufnr("%") == l:currentBufNum
        new
    endif

    if buflisted(l:currentBufNum)
        execute("bdelete! ".l:currentBufNum)
    endif
endfunction

function! CmdLine(str)
    call feedkeys(":" . a:str)
endfunction

function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", "\\/.*'$^~[]")
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'gv'
        call CmdLine("Ack '" . l:pattern . "' " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction

" EXTENDED
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => GUI related
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Disable scrollbars (real hackers don't use scrollbars for navigation!)
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Turn persistent undo on
"    means that you can undo even when you close a buffer/VIM
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
try
    set undodir=~/.vim_runtime/temp_dirs/undodir
    set undofile
catch
endtry

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General abbreviations
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"Insert the current date and time (useful for timestamps):
iab xdate <C-r>=strftime("%d/%m/%y %H:%M:%S")<cr>

""""""""""""""""""""""""""""""
" => Shell section
""""""""""""""""""""""""""""""
if exists('$TMUX')
    if has('nvim')
        set termguicolors
    else
        set term=screen-256color
    endif
endif

""""""""""""""""""""""""""""""
" => Markdown
""""""""""""""""""""""""""""""
let vim_markdown_folding_disabled = 1

" PLUGINS
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Nerd Tree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:NERDTreeWinPos = "right"
let NERDTreeShowHidden=0
let NERDTreeIgnore = ['\.pyc$', '__pycache__']
let g:NERDTreeWinSize=35

""""""""""""""""""""""""""""""
" => bufExplorer plugin
""""""""""""""""""""""""""""""
let g:bufExplorerDefaultHelp=0
let g:bufExplorerShowRelativePath=1
let g:bufExplorerFindActive=1
let g:bufExplorerSortBy='name'

""""""""""""""""""""""""""""""
" => CTRL-P
""""""""""""""""""""""""""""""
let g:ctrlp_working_path_mode = 0
let g:ctrlp_show_hidden = 1
let g:ctrlp_max_height = 20
let g:ctrlp_custom_ignore = 'node_modules\|^\.DS_Store\|^\.git\|^\.coffee'

""""""""""""""""""""""""""""""
" => ZenCoding
""""""""""""""""""""""""""""""
" Enable all functions in all modes
let g:user_zen_mode='a'

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Vimroom
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:goyo_width=100
let g:goyo_margin_top = 2
let g:goyo_margin_bottom = 2

""""""""""""""""""""""""""""""
" => YankStack
""""""""""""""""""""""""""""""
let g:yankstack_yank_keys = ['y', 'd']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Git gutter (Git diff)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gitgutter_enabled=1
