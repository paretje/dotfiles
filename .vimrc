" Enable Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'vim-scripts/haskell.vim'
Plugin 'craigemery/vim-autotag'
Plugin 'tpope/vim-speeddating'
Plugin 'paretje/vim-orgmode'
Plugin 'scrooloose/syntastic'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Valloric/YouCompleteMe'
Plugin 'tpope/vim-fugitive'
Plugin 'tomtom/tcomment_vim'
Plugin 'godlygeek/tabular'
Plugin 'ciaranm/detectindent'
Plugin 'paretje/javacomplete'
Plugin 'paretje/vim-java-get-set'
Plugin 'paretje/vim-refact'
Plugin 'bling/vim-airline'
Plugin 'Keithbsmiley/tmux.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'fs111/pydoc.vim'
Plugin 'python-rope/ropevim'
Plugin 'plasticboy/vim-markdown'

call vundle#end()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
	hi SpellBad ctermfg=Black
else
	" Always show tab-bar in GVIM
	set showtabline=2
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
" Set ctags options
set tags=./tags;$HOME
" Max number of tabs
set tabpagemax=32
" Use LaTeX by default
let g:tex_flavor="latex"
" Enable airline
set laststatus=2
" Always enable mouse. This enables mouse in tmux
set mouse=a

" Set Syntastic options
let g:syntastic_exit_checks=0
let g:syntastic_java_checkstyle_classpath='~/bin/checkstyle/checkstyle.jar'
let g:syntastic_java_checkstyle_conf_file='~/bin/checkstyle/paretje_checks.xml'
let g:syntastic_java_checkers = ['javac', 'checkstyle']
let g:syntastic_python_python_exec = '/usr/bin/python3'

" Set YouCompleteMe options
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_seed_identifiers_with_syntax=1

" Set javacomplete options
let g:nailgun_port='2113'
let g:javacomplete_ng='ng-nailgun'
let g:javacomplete_methods_paren_close_noargs=1

" Airline options
let g:airline_powerline_fonts = 1

" CtrlP options
let g:ctrlp_cmd='CtrlPMixed'

" Pydoc options
let g:pydoc_cmd = '/usr/bin/pydoc3'

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" UltiSnips options
let g:UltiSnipsExpandTrigger="<c-j>"

" 
au FileType haskell,prolog,matlab,tmux	setlocal nospell
au FileType tex,text,bbcode		setlocal linebreak " don't wrap randomly in a word
au FileType mail,gitcommit		setlocal formatoptions+=a

" Ruby ft options
au FileType ruby	setlocal softtabstop=2 shiftwidth=2 expandtab

" R ft options
au FileType r		setlocal softtabstop=2 shiftwidth=2 expandtab

" Org ft options
au FileType org		setlocal spelllang=nl
au FileType org		setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType org		inoremap <C-L> <Esc>:OrgCheckBoxNewBelow<CR>

" Matlab ft options
au FileType matlab	setlocal softtabstop=4 shiftwidth=4 expandtab

" Java ft options
au FileType java	setlocal softtabstop=4 shiftwidth=4 expandtab
au FileType java	setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java	setlocal omnifunc=javacomplete#Complete
au FileType java	compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml
au FileType java	nnoremap i :JavaCompleteAddImport<CR>

" LaTex ft options
au FileType tex		compiler tex | setlocal makeprg=latexmk\ -pdf\ -cd\ '%'

" It's All Text options
au BufRead ~/.mozilla/firefox/*/itsalltext/blog.online-urbanus.be*	setlocal ft=mkd spelllang=nl
au BufRead ~/.mozilla/firefox/*/itsalltext/github*			setlocal ft=mkd

" Custom key mappings
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj
vnoremap > >gv
vnoremap < <gv

" Custom commands
com TODO tabnew ~/cloud/config/notes/Tasks.org
