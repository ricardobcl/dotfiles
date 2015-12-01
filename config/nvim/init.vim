" original: https://github.com/nicknisi/dotfiles

call plug#begin('~/.config/nvim/plugged')


" colorschemes
Plug 'chriskempson/base16-vim'
Plug 'altercation/vim-colors-solarized'
Plug 'flazz/vim-colorschemes'

" add visual aid for line indentation
Plug 'Yggdroot/indentLine'

Plug 'sjl/gundo.vim'
Plug 'ervandew/supertab'
Plug 'troydm/zoomwintab.vim'
Plug 'honza/vim-snippets'
Plug 'bronson/vim-trailing-whitespace'
Plug 'benmills/vimux'
Plug 'kien/ctrlp.vim'
Plug 'Valloric/YouCompleteMe'
Plug 'bling/vim-airline'
Plug 'edkolev/tmuxline.vim'
Plug 'airblade/vim-gitgutter'
Plug 'myusuf3/numbers.vim'
Plug 'scrooloose/syntastic'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'matchit.zip'
Plug 'sickill/vim-pasta'
Plug 'vimwiki/vimwiki'
Plug 'xolox/vim-session'
Plug 'xolox/vim-misc'

" Erlang plugins
Plug 'vim-erlang/vim-erlang-runtime'
Plug 'vim-erlang/vim-erlang-compiler'
Plug 'vim-erlang/vim-erlang-tags'
Plug 'vim-erlang/erlang-motions.vim'
Plug 'vim-erlang/vim-erlang-omnicomplete'
Plug 'vim-erlang/vim-erlang-skeletons'

call plug#end()

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => General
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Abbreviations
abbr funciton function
abbr teh the

set nocompatible " not compatible with vi
set autoread " detect when a file is changed
filetype plugin on

" make backspace behave in a sane manner
set backspace=indent,eol,start

" set a map leader for more key combos
let mapleader = ','
let g:mapleader = ','

" set history=1000 " change history to 1000
" set textwidth=120

" Tab control
set expandtab " insert spaces rather than tabs for <Tab>
set smarttab " tab respects 'tabstop', 'shiftwidth', and 'softtabstop'
set tabstop=4 " the visible width of tabs
set softtabstop=4 " edit as if the tabs are 4 characters wide
set shiftwidth=4 " number of spaces to use for indent and unindent
set shiftround " round indent to a multiple of 'shiftwidth'
set completeopt+=longest

" This is the mac clipboard; useful to copy stuff to other applications
set clipboard=unnamed

" faster redrawing
set ttyfast

set diffopt+=vertical

" highlight conflicts
" match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'

" remap the escape to cancel highlighted text from searches
nnoremap <esc> :noh<return><esc>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => User Interface
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set so=7 " set 7 lines to the cursors - when moving vertical
set wildmenu " enhanced command line completion
set hidden " current buffer can be put into background
set showcmd " show incomplete commands
set noshowmode " don't show which mode disabled for PowerLine
set wildmode=list:longest " complete files like a shell
set scrolloff=3 " lines of text around cursor
set shell=$SHELL
set cmdheight=1 " command bar height

set title " set terminal title

" Searching
set ignorecase " case insensitive searching
set smartcase " case-sensitive if expresson contains a capital letter
set hlsearch
set incsearch " set incremental search, like modern browsers
set nolazyredraw " don't redraw while executing macros

set magic " Set magic on, for regex

set showmatch " show matching braces
set mat=2 " how many tenths of a second to blink

" error bells
set noerrorbells
set visualbell
set t_vb=
set tm=500

set encoding=utf8
let base16colorspace=256  " Access colors present in 256 colorspace"
set t_Co=256 " Explicitly tell vim that the terminal supports 256 colors"
" execute "set background=".$BACKGROUND
" execute "colorscheme ".$THEME
set background=dark
colorscheme base16-monokai
syntax on "switch syntax highlighting on

" set number " show line numbers
" set relativenumber " show relative line numbers
set number " show the current line number"

set wrap "turn on line wrapping
" set wrapmargin=8 " wrap lines when coming within n characters from side
set linebreak " set soft wrapping
set showbreak=… " show ellipsis at breaking

set autoindent " automatically set indent of new line
set smartindent

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Files, backups, and undo
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

"set nobackup
"set nowritebackup
"set noswapfile
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set undodir=~/.vim-tmp/undo//

set undofile
set history=100
set undolevels=100

nnoremap <leader>u :GundoToggle<cr>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => StatusLine
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

set laststatus=2 " show the status line all the time

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Mappings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General mappings/shortcuts for functionality
" Additional, plugin-specific mappings are located under
" the plugins section

" Close the current buffer
" noremap <C-x> :q<cr>

nnoremap <Tab> :bnext<cr>
nnoremap <S-Tab> :bprevious<cr>

" remap esc
inoremap jk <esc>

" insert newline bellow without entering insert mode
nmap oo o<Esc>k
nmap OO O<Esc>j

" Shortcut to quit vim
nmap <leader>q :q<cr>

" Shortcut to source the current file
nmap <leader>s :source ~/.vimrc<cr>

" markdown to html
nmap <leader>md :%!markdown --html4tags <cr>

" remove extra whitespace
nmap <leader><space> :%s/\s\+$<cr>

" wipout buffer
" nmap <silent> <leader>b :bw<cr>

" shortcut to save
nmap <leader>, :w<cr>

" disable Ex mode
noremap Q <NOP>

" set paste toggle
" set pastetoggle=<F6>

" toggle paste mode
map <leader>v :set paste!<cr>

" edit ~/.config/nvim/init.vim
map <leader>vim :sp! ~/.config/nvim/init.vim<cr>

" clear highlighted search
" noremap <space> :set hlsearch! hlsearch?<cr>

" activate spell-checking alternatives
nmap ;s :set invspell spelllang=en<cr>

" toggle invisible characters
set invlist
" set listchars=tab:▸\ ,eol:¬,trail:⋅,extends:❯,precedes:❮
set listchars=tab:▸\ ,trail:⋅,extends:❯,precedes:❮
highlight SpecialKey ctermbg=none " make the highlighting of tabs less annoying
set showbreak=↪
set list
nmap <leader>l :set list!<cr>

" " Textmate style indentation
" vmap <leader>[ <gv
" vmap <leader>] >gv
" nmap <leader>[ <<
" nmap <leader>] >>

" switch between current and last buffer
nmap <leader>. <c-^>

" enable . command in visual mode
vnoremap . :normal .<cr>

" map <silent> <C-h> :call WinMove('h')<cr>
" map <silent> <C-j> :call WinMove('j')<cr>
" map <silent> <C-k> :call WinMove('k')<cr>
" map <silent> <C-l> :call WinMove('l')<cr>

map <silent> <C-h> :wincmd h<cr>
map <silent> <C-j> :wincmd j<cr>
map <silent> <C-k> :wincmd k<cr>
map <silent> <C-l> :wincmd l<cr>

" map <leader>wc :wincmd q<cr>

" toggle cursor line
set cursorline
" nnoremap <leader>c :set cursorcolumn!<cr>
" set cursorcolumn
set colorcolumn=+1 " red line and over is error
set textwidth=80
set winwidth=80

" scroll the viewport faster
nnoremap <C-e> 3<C-e>
nnoremap <C-y> 3<C-y>

" moving up and down work as you would expect
nnoremap <silent> j gj
nnoremap <silent> k gk
nnoremap <silent> ^ g^
nnoremap <silent> $ g$

nnoremap <leader>n :NumbersToggle<cr>

" This rewires n and N to do the highlighing...
nnoremap <silent> n   n:call HLNext(0.4)<cr>
nnoremap <silent> N   N:call HLNext(0.4)<cr>

highlight WhiteOnRed ctermbg=red ctermfg=white

" OR ELSE just highlight the match in red...
function! HLNext (blinktime)
	let [bufnum, lnum, col, off] = getpos('.')
	let matchlen = strlen(matchstr(strpart(getline('.'),col-1),@/))
	let target_pat = '\c\%#\%('.@/.'\)'
	let ring = matchadd('WhiteOnRed', target_pat, 101)
	redraw
	exec 'sleep ' . float2nr(a:blinktime * 1000) . 'm'
	call matchdelete(ring)
	redraw
endfunction

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Functions
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" " Window movement shortcuts
" " move to the window in the direction shown, or create a new window
" function! WinMove(key)
"     let t:curwin = winnr()
"     exec "wincmd ".a:key
"     if (t:curwin == winnr())
"         if (match(a:key,'[jk]'))
"             wincmd v
"         else
"             wincmd s
"         endif
"         exec "wincmd ".a:key
"     endif
" endfunction


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" => Plugins
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

""" airline options

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#whitespace#enabled = 0
let g:airline_powerline_fonts=1
" let g:airline_left_sep=''
" let g:airline_right_sep=''
let g:airline_theme='bubblegum'


" if (has("gui_running"))
"     set guioptions=egmrt
"     set background=light
"     colorscheme solarized
"     let g:airline_left_sep=''
"     let g:airline_right_sep=''
"     let g:airline_powerline_fonts=0
"     let g:airline_theme='solarized'
" endif

"" syntastic
set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

let g:tmuxline_preset = {
      \'a'    : '#S',
      \'c'    : ['#(whoami)'],
      \'win'  : ['#I', '#W'],
      \'cwin' : ['#I', '#W'],
      \'y'    : '#(osascript ~/.dotfiles/applescripts/spotify.scpt)',
      \'z'    : ['%R', '%d/%m/%Y']}


" Vimux
nmap <leader>c :call VimuxRunCommand("./rebar3 compile")<cr>
nmap <leader>r :call VimuxRunCommand("./rebar3 restart")<cr>

" Run the current file with rspec
" map <Leader>rb :call VimuxRunCommand("clear; rspec " . bufname("%"))<cr>

" Prompt for a command to run
nmap <leader>vp :VimuxPromptCommand<cr>

" Run last command executed by VimuxRunCommand
nmap <leader>vl :VimuxRunLastCommand<cr>

" Inspect runner pane
nmap <leader>vi :VimuxInspectRunner<cr>

" Close vim tmux runner opened by VimuxRunCommand
nmap <leader>vq :VimuxCloseRunner<cr>

" Interrupt any command running in the runner pane
nmap <leader>vx :VimuxInterruptRunner<cr>

" Zoom the runner pane (use <bind-key> z to restore runner pane)
nmap <leader>vz :call VimuxZoomRunner()<cr>

let g:erl_author="Ricardo Gonçalves"

" session-vim: session managment
let g:session_directory = "~/.vim/session"
let g:session_autoload = "no"
let g:session_autosave = "no"
let g:session_command_aliases = 1
let g:session_autosave_periodic = 1

nnoremap <leader>so :OpenSession<cr>
nnoremap <leader>ss :SaveSession<cr>
nnoremap <leader>sd :DeleteSession<cr>
nnoremap <leader>sc :CloseSession<cr>

