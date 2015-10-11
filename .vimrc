" Enable Vundle
set nocompatible
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
" Plugin 'vim-scripts/haskell.vim'
Plugin 'craigemery/vim-autotag'
Plugin 'tpope/vim-speeddating'
Plugin 'scrooloose/syntastic'
Plugin 'SirVer/ultisnips'
Plugin 'honza/vim-snippets'
Plugin 'Shougo/neocomplete.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tomtom/tcomment_vim'
Plugin 'godlygeek/tabular'
Plugin 'ciaranm/detectindent'
Plugin 'paretje/javacomplete'
Plugin 'Dinduks/vim-java-get-set'
Plugin 'taq/vim-refact'
Plugin 'bling/vim-airline'
Plugin 'Keithbsmiley/tmux.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'fs111/pydoc.vim'
Plugin 'python-rope/ropevim'
Plugin 'plasticboy/vim-markdown'
Plugin 'raichoo/haskell-vim'
Plugin 'eagletmt/neco-ghc'
" Plugin 'dhruvasagar/vim-dotoo'
set rtp+=~/vcs/active/vim-dotoo

call vundle#end()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
	hi SpellBad ctermfg=Black
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
" Set ctags options
set tags=./tags;$HOME
" Max number of tabs
set tabpagemax=32
" Enable airline
set laststatus=2
" Always enable mouse. This enables mouse in tmux
set mouse=a
" Don't do full autocompletion in command mode
set wildmenu
set wildmode=longest,list,full
" enable folding
set foldmethod=syntax
let xml_syntax_folding=1
" Set keycode timeout to 0 ms. Reduces lag when pressing Alt-O on terminal and between leaving insert mode and update of airline
set ttimeoutlen=0
" Set default comments format
set commentstring=#%s
" Disable unloading buffer when abandoned, as needed by vim-dotoo
set hidden

" Set Syntastic options
let g:syntastic_exit_checks=0
let g:syntastic_java_checkstyle_classpath='~/bin/checkstyle/checkstyle.jar'
let g:syntastic_java_checkstyle_conf_file='~/bin/checkstyle/paretje_checks.xml'
let g:syntastic_java_checkers=['javac', 'checkstyle']
let g:syntastic_python_python_exec='/usr/bin/python3'
let g:syntastic_check_on_wq=0
let g:syntastic_java_javac_classpath='.'

" Set javacomplete options
let g:nailgun_port='2113'
let g:javacomplete_ng='ng-nailgun'
let g:javacomplete_methods_paren_close_noargs=1

" Airline options
let g:airline_powerline_fonts=1

" CtrlP options
let g:ctrlp_cmd='CtrlPMixed'
let g:ctrlp_user_command='find %s -maxdepth 5 -type f | grep -v "/\.git/\|/tmp/\|~$\|\.swp$\|\.mp4$\|\.mpg$\|\.mkv$\|\.jpg$"'
let g:ctrlp_mruf_exclude='/\.git/.*'
let g:ctrlp_working_path_mode=0

" Pydoc options
let g:pydoc_cmd = '/usr/bin/pydoc3'

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" UltiSnips options
let g:UltiSnipsExpandTrigger="<c-j>"

" Neocomplete options
let g:neocomplete#enable_at_start=1
let g:neocomplete#sources#omni#input_patterns = {}
call neocomplete#initialize()

" neco-ghc options
let g:necoghc_enable_detailed_browse=1

" vim-dotoo options
let g:dotoo#agenda#files=['~/vcs/personal/notes/*.org', '~/vcs/active/vim-dotoo/todo.dotoo']
let g:dotoo#capture#refile='~/vcs/personal/notes/refile.org'

" vim-notes options
let g:notes_directories=['~/vcs/personal/notes/notes']

" vim-speeddating options
au VimEnter * 1SpeedDatingFormat %Y-%m-%d %a %H:%M | 1SpeedDatingFormat %Y-%m-%d %a

" rubycomplete options
let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1
let g:rubycomplete_rails=1

" neocomplete options
if !exists('g:neocomplete#force_omni_input_patterns')
  let g:neocomplete#force_omni_input_patterns = {}
endif
let g:neocomplete#force_omni_input_patterns.ruby = '[^. *\t]\.\w*\|\h\w*::'

" Bulk options
au FileType dotoo*,latex,mail				setlocal spelllang=nl
au FileType haskell,prolog,matlab,tmux,dotooagenda	setlocal nospell
au FileType tex,text,bbcode				setlocal linebreak " don't wrap randomly in a word

" Ruby ft options
au FileType ruby	setlocal softtabstop=2 shiftwidth=2 expandtab

" R ft options
au FileType r		setlocal softtabstop=2 shiftwidth=2 expandtab

" Org ft options
au BufRead,BufNewFile *.org	setf dotoo
au FileType dotoo*		setlocal softtabstop=2 shiftwidth=1 expandtab textwidth=77
au FileType dotoo		nunmap <buffer> <C-A>
au FileType dotoo		nunmap <buffer> <C-X>

" Matlab ft options
au FileType matlab	setlocal softtabstop=4 shiftwidth=4 expandtab

" Java ft options
au FileType java	setlocal softtabstop=4 shiftwidth=4 expandtab
au FileType java	setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java	setlocal omnifunc=javacomplete#Complete
au FileType java	compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml
au FileType java	nnoremap i :JavaCompleteAddImport<CR>
au FileType java	JavaCompleteAddSourcePath .

" LaTex ft options
let g:tex_flavor="latex" " Use LaTeX by default
au FileType tex		compiler tex | setlocal makeprg=latexmk\ -pdf\ -cd\ '%'

" Haskell ft options
au FileType haskell	setlocal omnifunc=necoghc#omnifunc
au FileType haskell	setlocal softtabstop=4 shiftwidth=4 expandtab

" HTML ft options
au FileType html	setlocal softtabstop=2 shiftwidth=2 expandtab

" XML ft options
au FileType xml,xsd	setlocal softtabstop=2 shiftwidth=2 expandtab

" ATL ft options
au BufRead *.atl	setlocal syntax=haskell " Haskell syntax seems to be close to ATL
au BufRead *.atl	setlocal softtabstop=4 shiftwidth=4 expandtab
au BufRead *.atl	setlocal nospell
au BufRead *.atl	setlocal commentstring=--%s

" VimL ft options
au FileType vim		setlocal softtabstop=2 shiftwidth=2 expandtab
au BufRead ~/.vimrc,~/vcs/config/vimrc/.vimrc	setlocal softtabstop=8 shiftwidth=8 noexpandtab

" It's All Text options
au BufRead ~/.mozilla/firefox/*/itsalltext/blog.online-urbanus.be*	setlocal ft=mkd spelllang=nl
au BufRead ~/.mozilla/firefox/*/itsalltext/github*			setlocal ft=mkd

" notes options
au VimLeave *		if exists('g:sync_notes') | exec '!git -C ~/vcs/personal/notes autocommit' | endif
au FileType dotoo*	if !exists('g:sync_notes') | let g:sync_notes=1 | exec '!git -C ~/vcs/personal/notes autocommit' | endif

" Custom key mappings
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj
nnoremap <tab> za
inoremap <A-A> <C-O>A
inoremap <A-O> <C-O>O
" inoremap <A-o> <C-O>o
nnoremap <C-n> :nohlsearch<CR>
cnoremap <C-a> <C-b>
cnoremap <C-d> <Del>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-S-l> <C-l>

" keymappings for Neocomplete
inoremap <C-Space> <C-x><C-o>
inoremap <expr><TAB> pumvisible() ? "\<C-n>" : "\<TAB>"
imap <C-@> <C-Space>

" Custom commands
com -narg=1 -complete=file AddJavaClasspath let g:syntastic_java_javac_classpath=g:syntastic_java_javac_classpath . ':' . <q-args> | JavaCompleteAddClassPath <q-args>
