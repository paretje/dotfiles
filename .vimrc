if !filereadable($HOME . '/.vim/autoload/plug.vim')
	execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin('~/.vim/bundle')

Plug 'tomtom/tcomment_vim'
Plug 'bling/vim-airline'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'paretje/vim-dotoo', {'branch': 'merged'}
Plug 'tpope/vim-unimpaired'
Plug 'bkad/CamelCaseMotion'
Plug 'tpope/vim-repeat'

call plug#end()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
	hi SpellBad ctermfg=Black
	hi SpecialKey ctermfg=8
else
	" Always show tab-bar in GVim
	set showtabline=2

	" Fix airline in GVim
	if !exists('g:airline_symbols')
		let g:airline_symbols = {}
	endif
	let g:airline_symbols.space = "\u3000"
endif

" Syntax highlighting
syntax on
" Line numbers
set nu
" Spelling checker
set spell spelllang=en
" Auto indent
set autoindent
filetype indent on
" File-type plugins
filetype plugin on
" Search case-insensitive, unless caps are used
" TODO: enable this only for searches, not when replacing?
set ignorecase
set smartcase
" Highlight searches
set hlsearch
" Max number of tabs
set tabpagemax=32
" Enable airline
set laststatus=2
" Always enable mouse. This enables mouse in tmux
set mouse=a
" Don't do full autocompletion in command mode
set wildmenu
set wildmode=longest,list,full
" Set keycode timeout to 0 ms. Reduces lag when pressing Alt-O on terminal and between leaving insert mode and update of airline
set ttimeoutlen=0
" Set default comments format
set commentstring=#%s
" Disable unloading buffer when abandoned, as needed by vim-dotoo
set hidden
" Return to previous position in file when when opening
au BufReadPost * if &ft != "gitcommit" && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" Set mapleader
let mapleader = ';'
" Start searching while typing pattern
set incsearch
" Always show 3 or 5 lines under or above current line
set scrolloff=3
" Substitute all occurrences by default
set gdefault
" Show normal mode commands
set showcmd

" Airline options
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'

" CtrlP options
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
let g:ctrlp_mruf_exclude = '/\.git/.*\|/tmp/.*'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = ''
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_mruf_relative = 1
let g:ctrlp_mruf_exclude_nomod = 1

" vim-dotoo options
let g:dotoo#agenda#files = ['~/vcs/personal/notes/*.org']
let g:dotoo#capture#refile = '~/vcs/personal/notes/refile.org'
let g:dotoo#parser#todo_keywords = ['TODO', 'NEXT', 'WAITING', 'HOLD', 'PHONE', 'MEETING', 'MAIL', '|', 'CANCELLED', 'DONE']
let g:dotoo_todo_keyword_faces = [
	\ ['TODO', [':foreground 160', ':weight bold']],
	\ ['NEXT', [':foreground 27', ':weight bold']],
	\ ['DONE', [':foreground 22', ':weight bold']],
	\ ['WAITING', [':foreground 202', ':weight bold']],
	\ ['HOLD', [':foreground 53', ':weight bold']],
	\ ['CANCELLED', [':foreground 22', ':weight bold']],
	\ ['MEETING', [':foreground 22', ':weight bold']],
	\ ['PHONE', [':foreground 22', ':weight bold']],
	\ ['MAIL', [':foreground 25', ':weight bold']]
	\ ]

" TComments options
call tcomment#DefineType('matlab', '# %s')

" Bulk options
au FileType haskell,prolog,matlab,tmux	setlocal nospell
au FileType dotooagenda,calendar,qf,man	setlocal nospell
au FileType dotoo*,tex,mail,markdown	setlocal spelllang=nl
au FileType tex,text,bbcode,markdown	setlocal linebreak " don't wrap randomly in a word

" Ruby ft options
au FileType ruby	setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType eruby	setlocal softtabstop=2 shiftwidth=2 expandtab

" R ft options
au FileType r		setlocal softtabstop=2 shiftwidth=2 expandtab

" Org ft options
au BufRead,BufNewFile *.org	setf dotoo
au FileType dotoo*		setlocal softtabstop=2 shiftwidth=1 expandtab textwidth=77
au FileType dotoocapture	iabbrev <expr> <buffer> <silent> :date: '['.strftime(g:dotoo#time#date_day_format).']'
au FileType dotoocapture	iabbrev <expr> <buffer> <silent> :time: '['.strftime(g:dotoo#time#datetime_format).']'
au FileType dotoo,dotoocapture	inoremap <buffer> <C-l> <CR><BS><BS><BS><BS><BS><BS>- [ ] 

" Matlab ft options
au FileType matlab	setlocal softtabstop=4 shiftwidth=4 expandtab

" Java ft options
au FileType java	setlocal softtabstop=4 shiftwidth=4 expandtab

" LaTex ft options
let g:tex_flavor = "latex" " Use LaTeX by default
au FileType tex		compiler tex | setlocal makeprg=latexmk\ -pdf\ -cd\ '%'

" Haskell ft options
au FileType haskell	setlocal softtabstop=4 shiftwidth=4 expandtab

" HTML ft options
au FileType html	setlocal softtabstop=2 shiftwidth=2 expandtab

" XML ft options
au FileType xml,xsd	setlocal softtabstop=2 shiftwidth=2 expandtab

" VimL ft options
au FileType vim		setlocal softtabstop=2 shiftwidth=2 expandtab
au BufRead ~/.vimrc	setlocal softtabstop=8 shiftwidth=8 noexpandtab

" SQL ft options
au FileType sql		setlocal softtabstop=2 shiftwidth=2 expandtab

" mail ft options
au FileType mail	setlocal formatoptions+=na

" markdown ft options
au FileType markdown	setlocal softtabstop=2 shiftwidth=1 expandtab

" javascript ft options
au FileType javascript	setlocal softtabstop=4 shiftwidth=4 expandtab

" less ft options
au FileType less	setlocal softtabstop=4 shiftwidth=4 expandtab

" aptconf ft options
au FileType aptconf	setlocal commentstring=//%s

" xsession options
au BufRead ~/.xsession	setfiletype sh

" Custom key mappings
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj
nnoremap <tab> za
inoremap <C-e> <C-O>A
inoremap <C-a> <C-O>I
nnoremap <Leader>n :nohlsearch<CR>
cnoremap <C-a> <C-b>
cnoremap <C-d> <Del>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <Leader>r :redraw!<CR>
nnoremap <C-n> :CtrlPBuffer<CR>
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k
nnoremap <Leader>s :exec '!git -C ~/vcs/personal/notes autocommit'<CR><CR>
nnoremap <Leader>l :call ToggleSpellLang()<CR>
nnoremap <silent> zi :call ToggleFolding()<CR>

" Custom functions
fun ToggleSpellLang()
	if &spelllang == "en"
		setlocal spelllang=nl
	else
		setlocal spelllang=en
	endif
	setlocal spelllang?
endfun

fun ToggleFolding()
	if &l:foldmethod == "manual"
		setlocal foldmethod=syntax
		return
	endif
	normal! zi
endfun
