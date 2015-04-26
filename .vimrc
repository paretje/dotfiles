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
Plugin 'paretje/vim-snippets'
Plugin 'tpope/vim-fugitive'
Plugin 'tomtom/tcomment_vim'
Plugin 'godlygeek/tabular'
Plugin 'ciaranm/detectindent'
Plugin 'paretje/vim-java-get-set'
Plugin 'paretje/vim-refact'
Plugin 'bling/vim-airline'
Plugin 'Keithbsmiley/tmux.vim'
Plugin 'kien/ctrlp.vim'
Plugin 'fs111/pydoc.vim'

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
" Location of org-files
let g:org_agenda_files=['~/cloud/config/notes/*.org']
command TODO tabnew ~/cloud/config/notes/Tasks.org
" Max number of tabs
set tabpagemax=32
" Disable exit code checks
let g:syntastic_exit_checks=0
let g:syntastic_java_checkstyle_classpath='~/bin/checkstyle/checkstyle.jar'
let g:syntastic_java_checkstyle_conf_file='~/bin/checkstyle/paretje_checks.xml'
let g:syntastic_java_checkers = ['javac', 'checkstyle']
" Set YouCompleteMe options
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_seed_identifiers_with_syntax=1
" Set javacomplete options
let g:nailgun_port='2113'
let g:javacomplete_ng='ng-nailgun'
let g:javacomplete_methods_paren_close_noargs=1
" Use LaTeX by default
let g:tex_flavor="latex"
" Enable airline
set laststatus=2
let g:airline_powerline_fonts = 1
" CtrlP settings
let g:ctrlp_cmd='CtrlPMixed'
" Use python3 documentation
let g:pydoc_cmd = '/usr/bin/pydoc3'

" Arrow keys
" https://gist.github.com/hugoroy/5822226
" http://billodom.com/talks/vim-key-mapping.pdf
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj
vnoremap > >gv
vnoremap < <gv
let g:UltiSnipsExpandTrigger="<c-j>"

au FileType haskell,prolog,matlab,tmux	setlocal nospell
au FileType tex,text,bbcode		setlocal linebreak
au FileType mail,gitcommit		setlocal formatoptions+=a
au FileType org				setlocal spelllang=nl

au FileType ruby	setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType r		setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType matlab	setlocal softtabstop=4 shiftwidth=4 expandtab
au FileType java	setlocal softtabstop=4 shiftwidth=4 expandtab

au FileType java	setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java	setlocal omnifunc=javacomplete#Complete

au FileType tex		compiler tex | setlocal makeprg=latexmk\ -pdf\ -cd\ '%'
au FileType java	compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml

au FileType java	nnoremap i :JavaCompleteAddImport<CR>
au FileType org		inoremap <C-L> <Esc>:OrgCheckBoxNewBelow<CR>

au BufReadPost fugitive://*	set bufhidden=delete

" TODO: does this really work?
au BufNewFile,BufRead /run/user/*/gvfs/**	setlocal directory=/tmp backupdir=/tmp

au BufRead ~/.mozilla/firefox/*/itsalltext/blog.online-urbanus.be*	setlocal ft=bbcode
au BufRead ~/.mozilla/firefox/*/itsalltext/github*	setlocal ft=markdown
