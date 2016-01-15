if !filereadable($HOME . '/.vim/autoload/plug.vim')
	execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
endif

call plug#begin('~/.vim/bundle')

Plug 'craigemery/vim-autotag'
Plug 'tpope/vim-speeddating'
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'tpope/vim-fugitive'
Plug 'tomtom/tcomment_vim'
Plug 'godlygeek/tabular', {'on': 'Tabularize'}
Plug 'ciaranm/detectindent', {'on': 'DetectIndent'}
Plug 'paretje/javacomplete', {'for': 'java', 'do': 'mvn -f java/pom.xml clean install'}
Plug 'Dinduks/vim-java-get-set', {'for': 'java'}
Plug 'taq/vim-refact', {'for': 'java'}
Plug 'vim-airline/vim-airline'
Plug 'Keithbsmiley/tmux.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'python-rope/ropevim', {'for': 'python'}
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
Plug 'paretje/vim-dotoo', {'branch': 'merged'}
Plug 'Yggdroot/indentLine'
Plug 'Shougo/vimproc.vim', {'do': 'make'} " used by neco-ghc
Plug 'vim-ruby/vim-ruby'
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-rails', {'for': ['ruby', 'eruby']}
Plug 'jaxbot/browserlink.vim', {'for': ['html', 'javascript', 'css']}
Plug 'dhruvasagar/vim-table-mode', {'on': 'TableModeToggle'}
Plug 'tpope/vim-endwise'
Plug 'Raimondi/delimitMate'
Plug 'vim-scripts/bbcode', {'for': 'bbcode'}
Plug 'briceburg/vimperator-labs', {'rtp': 'muttator/contrib/vim'}
Plug 'ciaranm/securemodelines'
Plug 'tpope/vim-unimpaired'
Plug 'mattn/calendar-vim', {'on': '<Plug>CalendarV'}
Plug 'airblade/vim-gitgutter'
Plug 'ledger/vim-ledger'
Plug 'simnalamburt/vim-mundo', {'on': 'GundoToggle'}
Plug 'benekastah/neomake'
Plug 'bkad/CamelCaseMotion'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'lucapette/vim-ruby-doc', {'for': ['ruby', 'eruby']}
Plug 'tpope/vim-repeat'
Plug 'Shougo/neopairs.vim'
Plug 'davidhalter/jedi-vim', {'for': 'python'}
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch' " used by vim-rails and vim-fugitive
Plug 'mhinz/vim-grepper'
Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}
Plug 'tpope/vim-scriptease', {'for': 'vim'}

if has('nvim')
	Plug 'Shougo/deoplete.nvim'
	Plug 'radenling/vim-dispatch-neovim'
	Plug 'paretje/nvim-man'
endif

call plug#end()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
	hi SpellBad ctermfg=Black
	hi SpecialKey ctermfg=8
else
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
let mapleader = ';'
" Start searching while typing pattern
set incsearch
" Use smartcase matching in autocompletion
set infercase
" Show tab indentation levels
set list
set listchars=tab:¦\ 
" Always show 3 or 5 lines under or above current line
set scrolloff=3
" Substitute all occurrences by default
set gdefault
" Show normal mode commands
set showcmd
" Open vertical splits at the right
set splitright
" Single spaces when joining (aka French spacing)
set nojoinspaces
" Use X clipboard as default yank register
set clipboard=unnamedplus
" Stay on same column when jumping in file
set nostartofline
" Skip intro
set shortmess+=I
" Toggle paste option to safely paste via tmux (eg. when using ssh)
set pastetoggle=<leader>p

" Set YouCompleteMe options
let g:ycm_autoclose_preview_window_after_insertion = 1
let g:ycm_seed_identifiers_with_syntax = 1
let g:ycm_complete_in_comments = 1
let g:ycm_collect_identifiers_from_comments_and_strings = 1
let g:ycm_semantic_triggers = {'haskell': ['.'], 'xml': ['</'], 'xsd': ['</']}
let g:ycm_filetype_blacklist = {'help': 1, 'text': 1, 'mail': 1, 'dotoo': 1, 'markdown': 1}

" Set javacomplete options
let g:nailgun_port = '2113'
let g:javacomplete_ng = 'ng-nailgun'
let g:javacomplete_methods_paren_close_noargs = 1

" Airline options
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#tagbar#enabled = 0

" CtrlP options
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_user_command = 'sh -c "cd %s; ag -l --nocolor --hidden -g \"\""'
let g:ctrlp_mruf_exclude = '/\.git/.*\|/tmp/.*\|term://.*'
let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = ''
let g:ctrlp_follow_symlinks = 1
let g:ctrlp_mruf_exclude_nomod = 1

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" UltiSnips options
let g:UltiSnipsExpandTrigger = "<c-j>"

" neco-ghc options
let g:necoghc_enable_detailed_browse = 1

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

" vim-speeddating options
au VimEnter * 1SpeedDatingFormat %Y-%m-%d %a %H:%M | 1SpeedDatingFormat %Y-%m-%d %a

" rubycomplete options
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
let g:rubycomplete_rails = 1
let g:rubycomplete_use_bundler = 1

" NERDTree options
let g:NERDTreeMapActivateNode = 'l'
let g:NERDTreeMapJumpParent = 'h'
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

" indentLine options
let g:indentLine_fileTypeExclude = ['help', 'dotoo', 'dotoocapture', 'dotooagenda', 'markdown', '']
let g:indentLine_faster = 1
let g:indentLine_showFirstIndentLevel = 1

" vim-markdown options
let g:markdown_folding = 1

" TComments options
call tcomment#DefineType('matlab', '# %s')

" xml options
let g:xml_syntax_folding = 1

" calendar-vim options
let g:calendar_monday = 1

" ledger-vim options
let g:ledger_bin = 'echo' " disable use of ledger command, as I'm using hledger

" neomake options
au BufWritePost * Neomake
let g:neomake_error_sign = {'text': 'E>', 'texthl': 'Error'}
let g:neomake_warning_sign = {'text': 'W>', 'texthl': 'Todo'}

" CamelCaseMotion options
call camelcasemotion#CreateMotionMappings('<leader>')

" deoplete options
let g:deoplete#enable_at_startup = 1

" jedi options
let g:jedi#completions_enabled = 0
let g:jedi#force_py_version = 3

" tagbar options
let g:tagbar_ctags_bin = 'ctags'

" securemodelines options
let g:secure_modelines_allowed_items = [
	\ "textwidth",		"tw",
	\ "softtabstop",	"sts",
	\ "tabstop",		"ts",
	\ "shiftwidth",		"sw",
	\ "expandtab",		"et",		"noexpandtab",		"noet",
	\ "filetype",		"ft",
	\ "foldmethod",		"fdm",
	\ "formatoptions",	"fo",
	\ "readonly",		"ro",		"noreadonly",		"noro",
	\ "rightleft",		"rl",		"norightleft",		"norl",
	\ "cindent",		"cin",		"nocindent",		"nocin",
	\ "smartindent",	"si",		"nosmartindent",	"nosi",
	\ "autoindent",		"ai",		"noautoindent",		"noai",
	\ "spell",		"nospell",
	\ "spelllang",
	\ "wrap",		"nowrap"
	\ ]

" table-mode options
let g:table_mode_toggle_map = 't'

" Bulk options
au FileType haskell,prolog,matlab,tmux	setlocal nospell
au FileType dotooagenda,calendar,qf,man	setlocal nospell
au FileType dotoo*,tex,mail,markdown	setlocal spelllang=nl
au FileType tex,text,bbcode,markdown	setlocal linebreak " don't wrap randomly in a word
au FileType help,dotoo*			setlocal nolist " disable indentation lines

" Ruby ft options
au FileType ruby	setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType eruby	setlocal softtabstop=2 shiftwidth=2 expandtab
au FileType eruby	inoremap <silent> <buffer> / <C-o>:call CloseTag()<CR>

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
au BufHidden *.org		setlocal nobuflisted

" Matlab ft options
au FileType matlab	setlocal softtabstop=4 shiftwidth=4 expandtab

" Java ft options
au FileType java	setlocal softtabstop=4 shiftwidth=4 expandtab
au FileType java	setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java	setlocal omnifunc=javacomplete#Complete
au FileType java	compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml
au FileType java	nnoremap <leader>i :JavaCompleteAddImport<CR>
au FileType java	JavaCompleteAddSourcePath .

" LaTex ft options
let g:tex_flavor = "latex" " Use LaTeX by default
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
au FileType markdown	setlocal softtabstop=2 shiftwidth=1 expandtab
au FileType markdown	au BufWritePost <buffer> Neomake!

" javascript ft options
au FileType javascript	setlocal softtabstop=4 shiftwidth=4 expandtab

" less ft options
au FileType less	setlocal softtabstop=4 shiftwidth=4 expandtab

" ledger ft options
au BufRead,BufNewFile *.journal	setf ledger
au FileType ledger	setlocal softtabstop=4 shiftwidth=4 expandtab
au FileType ledger	normal! zn

" aptconf ft options
au FileType aptconf	setlocal commentstring=//%s

" python ft options
au FileType python	setlocal omnifunc=jedi#completions

" xmobarrc options
au BufRead ~/.xmobarrc	setfiletype haskell

" xsession options
au BufRead ~/.xsession	setfiletype sh

" terminal options
if has('nvim')
	au TermOpen * setlocal nospell
	au TermOpen * setlocal nobuflisted
endif

" It's All Text options
au BufRead ~/.mozilla/firefox/*/itsalltext/blog.online-urbanus.be*	setlocal ft=mkd spelllang=nl
au BufRead ~/.mozilla/firefox/*/itsalltext/github*			setlocal ft=mkd

" notes options
au VimLeave *				if exists('g:sync_notes') | exec '!git -C ~/vcs/personal/notes autocommit' | endif
au FileType dotooagenda,dotoocapture	let g:sync_notes = 1
au BufRead ~/vcs/personal/notes/*	let g:sync_notes = 1

" Custom key mappings
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj
nmap <tab> za
inoremap <C-e> <C-O>A
inoremap <C-a> <C-O>I
nnoremap <leader>n :nohlsearch<CR>
cnoremap <C-a> <C-b>
cnoremap <C-d> <Del>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <silent> <leader>r :redraw!<CR>
nnoremap <silent> <C-n> :CtrlPBuffer<CR>
nnoremap <silent> <C-g> :NERDTreeToggle<CR>
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k
nnoremap <leader>s :exec '!git -C ~/vcs/personal/notes autocommit'<CR><CR>
nnoremap <leader>l :call ToggleSpellLang()<CR>
nnoremap <silent> zi :call ToggleFolding()<CR>
nnoremap <silent> <leader>tm :call TableModeToggle()<CR>
nmap <leader>cal <Plug>CalendarV
inoremap <expr><tab> pumvisible() ? "\<C-n>" : "\<TAB>"
nnoremap <silent> <leader>tag :TagbarToggle<CR>
nnoremap <leader>tfo :call OrgRecalculateTable(@%)<CR>

if has('nvim')
	tnoremap <C-q> <C-\><C-n>
	nnoremap <C-q> i<C-q>

	au User ManOpen tmap <buffer> <C-h> <C-w>h
	au User ManOpen tmap <buffer> <C-j> <C-w>j
	au User ManOpen tmap <buffer> <C-k> <C-w>k
	au User ManOpen tmap <buffer> <C-l> <C-w>l
	au User ManOpen tmap <buffer> <esc> <C-\><C-n>M
else
	source $VIMRUNTIME/ftplugin/man.vim
	au FileType man nnoremap <silent> <nowait><buffer> q <C-W>c
	au FileType man wincmd L
	nmap K :Man <cword><CR>
endif

" Custom commands
com -narg=1 -complete=file AddJavaClasspath let g:syntastic_java_javac_classpath = g:syntastic_java_javac_classpath . ':' . <q-args> | JavaCompleteAddClassPath <q-args>
com -narg=* Ag call HighlightSearch(<q-args>) | Grepper -tool ag -open -switch -query <args>

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

fun HighlightSearch(args)
	let @/= matchstr(a:args, "\\v(-)\@<!(\<)\@<=\\w+|['\"]\\zs.{-}\\ze['\"]")
	call feedkeys(":let &hlsearch=1 \| echo \<CR>", 'n')
endfun

fun OrgRecalculateTable(file)
	let pos = getcurpos()
	write
	call system('emacs "' . a:file . '" --batch -f org-table-recalculate-buffer-tables --eval "(save-buffer 0)"')
	edit
	call setpos('.', pos)
endfun

fun TableModeToggle()
	TableModeToggle
	AirlineRefresh
endfun

fun AirlineTableMode(...)
	if exists('b:table_mode_active') && b:table_mode_active
		let w:airline_section_a = 'TABLE MODE'
	endif
endfun
call airline#add_statusline_func('AirlineTableMode')

" Close HTML tag, assuming the use of delimitMate
fun CloseTag()
	call feedkeys("/", 'n')
	if matchstr(getline('.'), '\%' . (col('.') - 1) . 'c.') == '<'
		call feedkeys("\<C-x>\<C-o>\<bs>\<right>", 'n')
	endif
endfun
