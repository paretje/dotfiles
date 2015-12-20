if !filereadable($HOME . '/.vim/autoload/plug.vim')
	execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin('~/.vim/bundle')

Plug 'craigemery/vim-autotag'
Plug 'tpope/vim-speeddating'
Plug 'scrooloose/syntastic'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'Valloric/YouCompleteMe', {'do': './install.py'}
Plug 'tpope/vim-fugitive'
Plug 'tomtom/tcomment_vim'
Plug 'godlygeek/tabular', {'on': 'Tabularize'}
Plug 'ciaranm/detectindent', {'on': 'DetectIndent'}
Plug 'paretje/javacomplete', {'for': 'java', 'do': 'mvn -f java/pom.xml initialize package'}
Plug 'Dinduks/vim-java-get-set', {'for': 'java'}
Plug 'taq/vim-refact', {'for': 'java'}
Plug 'bling/vim-airline'
Plug 'Keithbsmiley/tmux.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'fs111/pydoc.vim', {'for': 'python'}
Plug 'python-rope/ropevim', {'for': 'python'}
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
Plug 'paretje/vim-dotoo', {'branch': 'merged'}
Plug 'Yggdroot/indentLine'
Plug 'Shougo/vimproc.vim', {'do': 'make'}
Plug 'vim-ruby/vim-ruby'
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-rails', {'for': ['ruby', 'eruby']}
Plug 'jaxbot/browserlink.vim', {'for': ['html', 'javascript', 'css']}
Plug 'tpope/vim-markdown'
Plug 'dhruvasagar/vim-table-mode', {'on': 'TableModeToggle'}
Plug 'tpope/vim-endwise'
Plug 'Raimondi/delimitMate'
Plug 'vim-scripts/bbcode', {'for': 'bbcode'}
Plug 'briceburg/vimperator-labs', {'rtp': 'muttator/contrib/vim'}
Plug 'ciaranm/securemodelines'
Plug 'tpope/vim-unimpaired'
Plug 'mattn/calendar-vim', {'on': ['<Plug>CalendarV']}
Plug 'airblade/vim-gitgutter'

call plug#end()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
	hi SpellBad ctermfg=Black
	highlight SpecialKey ctermfg=8
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
" Set keycode timeout to 0 ms. Reduces lag when pressing Alt-O on terminal and between leaving insert mode and update of airline
set ttimeoutlen=0
" Set default comments format
set commentstring=#%s
" Disable unloading buffer when abandoned, as needed by vim-dotoo
set hidden
" Return to previous position in file when when opening
au BufReadPost * if &ft != "gitcommit" && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" Set mapleader
let mapleader=';'
" Start searching while typing pattern
set incsearch
" Use smartcase matching in autocompletion
set infercase
" Show tab indentation levels
set list
set listchars=tab:¦\ 
" Always show 3 or 5 lines under or above current line
set scrolloff=3

" Set Syntastic options
let g:syntastic_exit_checks=0
let g:syntastic_java_checkstyle_classpath='~/bin/checkstyle/checkstyle.jar'
let g:syntastic_java_checkstyle_conf_file='~/bin/checkstyle/paretje_checks.xml'
let g:syntastic_java_checkers=['javac', 'checkstyle']
let g:syntastic_python_python_exec='/usr/bin/python3'
let g:syntastic_check_on_wq=0
let g:syntastic_java_javac_classpath='.'

" Set YouCompleteMe options
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_seed_identifiers_with_syntax=1
let g:ycm_complete_in_comments=1
let g:ycm_collect_identifiers_from_comments_and_strings=1
let g:ycm_semantic_triggers={'haskell': ['.'], 'xml': ['</'], 'xsd': ['</']}
let g:ycm_filetype_blacklist={'help': 1, 'text': 1, 'mail': 1, 'dotoo': 1}

" Set javacomplete options
let g:nailgun_port='2113'
let g:javacomplete_ng='ng-nailgun'
let g:javacomplete_methods_paren_close_noargs=1

" Airline options
let g:airline_powerline_fonts=1
let g:airline_theme='bubblegum'

" CtrlP options
let g:ctrlp_cmd='CtrlPMixed'
let g:ctrlp_user_command='ag %s -l --nocolor --hidden -g ""'
let g:ctrlp_mruf_exclude='/\.git/.*\|/tmp/.*'
let g:ctrlp_working_path_mode=0
let g:ctrlp_switch_buffer=''
let g:ctrlp_follow_symlinks=1

" Pydoc options
let g:pydoc_cmd = '/usr/bin/pydoc3'

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" UltiSnips options
let g:UltiSnipsExpandTrigger="<c-j>"

" neco-ghc options
let g:necoghc_enable_detailed_browse=1

" vim-dotoo options
let g:dotoo#agenda#files=['~/vcs/personal/notes/*.org']
let g:dotoo#capture#refile='~/vcs/personal/notes/refile.org'
let g:dotoo#parser#todo_keywords=['TODO', 'NEXT', 'WAITING', 'HOLD', 'PHONE', 'MEETING', 'MAIL', '|', 'CANCELLED', 'DONE']
let g:dotoo_todo_keyword_faces=[
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

" vim-notes options
let g:notes_directories=['~/vcs/personal/notes/notes']

" vim-speeddating options
au VimEnter * 1SpeedDatingFormat %Y-%m-%d %a %H:%M | 1SpeedDatingFormat %Y-%m-%d %a

" rubycomplete options
let g:rubycomplete_buffer_loading=1
let g:rubycomplete_classes_in_global=1
let g:rubycomplete_rails=1
let g:rubycomplete_use_bundler=1

" NERDTree options
let g:NERDTreeMapActivateNode='l'
let g:NERDTreeMapJumpParent='h'
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" indentLine options
let g:indentLine_fileTypeExclude=['help', 'dotoo', 'dotoocapture', 'dotooagenda', 'markdown', '']
let g:indentLine_faster=1
let g:indentLine_showFirstIndentLevel=1

" vim-markdown options
let g:markdown_folding=1

" TComments options
call tcomment#DefineType('matlab', '# %s')

" xml options
let g:xml_syntax_folding=1

" calendar-vim options
let g:calendar_monday=1

" Bulk options
au FileType haskell,prolog,matlab,tmux	setlocal nospell
au FileType dotooagenda,calendar	setlocal nospell
au FileType dotoo*,tex,mail,markdown	setlocal spelllang=nl
au FileType tex,text,bbcode,markdown	setlocal linebreak " don't wrap randomly in a word
au FileType help,dotoo*			setlocal nolist " disable indentation lines

" Ruby ft options
au FileType ruby	setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType eruby	setlocal softtabstop=2 shiftwidth=2 expandtab

" R ft options
au FileType r		setlocal softtabstop=2 shiftwidth=2 expandtab

" Org ft options
au BufRead,BufNewFile *.org	setf dotoo
au FileType dotoo*		setlocal softtabstop=2 shiftwidth=1 expandtab textwidth=77
au FileType dotoo		nmap <buffer> <C-a> <Plug>SpeedDatingUp
au FileType dotoo		nmap <buffer> <C-x> <Plug>SpeedDatingDown
au FileType dotoocapture	iabbrev <expr> <buffer> <silent> :date: '['.strftime(g:dotoo#time#date_day_format).']'
au FileType dotoocapture	iabbrev <expr> <buffer> <silent> :time: '['.strftime(g:dotoo#time#datetime_format).']'
au FileType dotoo,dotoocapture	inoremap <buffer> <C-l> <CR><BS><BS><BS><BS><BS><BS>- [ ] 

" Matlab ft options
au FileType matlab	setlocal softtabstop=4 shiftwidth=4 expandtab

" Java ft options
au FileType java	setlocal softtabstop=4 shiftwidth=4 expandtab
au FileType java	setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java	setlocal omnifunc=javacomplete#Complete
au FileType java	compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml
au FileType java	nnoremap <Leader>i :JavaCompleteAddImport<CR>
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
au BufRead ~/.vimrc	setlocal softtabstop=8 shiftwidth=8 noexpandtab

" SQL ft options
au FileType sql		setlocal softtabstop=2 shiftwidth=2 expandtab

" mail ft options
au FileType mail	setlocal formatoptions+=na

" markdown ft options
au FileType markdown		setlocal softtabstop=2 shiftwidth=1 expandtab

" javascript ft options
au FileType javascript	setlocal softtabstop=4 shiftwidth=4 expandtab

" less ft options
au FileType less	setlocal softtabstop=4 shiftwidth=4 expandtab

" xmobarrc options
au BufRead ~/.xmobarrc	setlocal expandtab

" xsession options
au BufRead ~/.xsession	setfiletype sh

" terminal options
if exists(':terminal')
	au TermOpen * setlocal nospell
	au TermOpen * set nohlsearch
	au TermClose * set hlsearch
endif

" It's All Text options
au BufRead ~/.mozilla/firefox/*/itsalltext/blog.online-urbanus.be*	setlocal ft=mkd spelllang=nl
au BufRead ~/.mozilla/firefox/*/itsalltext/github*			setlocal ft=mkd

" notes options
au VimLeave *				if exists('g:sync_notes') | exec '!git -C ~/vcs/personal/notes autocommit' | endif
au FileType dotooagenda,dotoocapture	let g:sync_notes=1
au BufRead ~/vcs/personal/notes/*	let g:sync_notes=1

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
nnoremap <C-g> :NERDTreeToggle<CR>
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k
nnoremap <Leader>s :exec '!git -C ~/vcs/personal/notes autocommit'<CR><CR>
nnoremap <Leader>l :call ToggleSpellLang()<CR>
nnoremap <silent> zi :call ToggleFolding()<CR>
nnoremap <Leader>tm :TableModeToggle<CR>
nmap <Leader>cal <Plug>CalendarV

if exists(':tnoremap')
	tnoremap <C-x> <C-\><C-n>
endif

" Custom commands
com -narg=1 -complete=file AddJavaClasspath let g:syntastic_java_javac_classpath=g:syntastic_java_javac_classpath . ':' . <q-args> | JavaCompleteAddClassPath <q-args>

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
