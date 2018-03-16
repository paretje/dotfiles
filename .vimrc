set encoding=utf-8
scriptencoding utf-8

" vint: -ProhibitAutocmdWithNoGroup
" remove all autocmd's
autocmd!

" download vim-plug if needed
let s:plug_install = 0
if !filereadable($HOME . '/.vim/autoload/plug.vim')
  if executable('curl')
    execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  elseif executable('wget')
    execute '!mkdir -P ~/.vim/autoload'
    execute '!wget -O ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  endif
  let s:plug_install = 1
endif

" load plug and declare all plugins
call plug#begin('~/.vim/bundle')

Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-fugitive'
Plug 'godlygeek/tabular', {'on': 'Tabularize'} " used by vim-table-mode
Plug 'vim-airline/vim-airline', {'commit': '55a9721c22370a47e076e85449897ded6f45386d'}
Plug 'Keithbsmiley/tmux.vim', {'for': 'tmux'}
Plug 'ctrlpvim/ctrlp.vim'
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
Plug 'paretje/vim-dotoo', {'branch': 'merged'}
Plug 'Yggdroot/indentLine'
Plug 'vim-ruby/vim-ruby', {'for': 'ruby'}
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-rails', {'for': ['ruby', 'eruby']}
if has('python')
  Plug 'jaxbot/browserlink.vim', {'for': ['html', 'javascript', 'css']}
else
  Plug 'juanwolf/browserlink.vim', {'for': ['html', 'javascript', 'css']}
endif
Plug 'dhruvasagar/vim-table-mode', {'on': 'TableModeToggle'}
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-endwise'
Plug 'vim-scripts/bbcode', {'for': 'bbcode'}
Plug 'briceburg/vimperator-labs', {'rtp': 'muttator/contrib/vim', 'for': 'muttator'}
Plug 'vimperator/vimperator.vim', {'for': 'vimperator'}
Plug 'paretje/securemodelines'
Plug 'tpope/vim-unimpaired'
Plug 'mattn/calendar-vim', {'on': '<Plug>CalendarV'}
Plug 'airblade/vim-gitgutter', {'commit': '932ffaca092cca246b82c33e23d2d3a05e192e08'}
Plug 'ledger/vim-ledger', {'for': 'ledger'}
Plug 'simnalamburt/vim-mundo', {'on': 'MundoToggle'}
Plug 'neomake/neomake'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'lucapette/vim-ruby-doc', {'for': ['ruby', 'eruby']}
Plug 'tpope/vim-repeat'
Plug 'Shougo/neopairs.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-dispatch' " used by vim-rails and vim-fugitive
Plug 'mhinz/vim-grepper', {'on': 'Grepper'}
Plug 'majutsushi/tagbar', {'on': 'TagbarToggle'}
Plug 'tpope/vim-scriptease', {'for': 'vim'}
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-sleuth'
Plug 'vim-airline/vim-airline-themes'
Plug 'junegunn/gv.vim', {'on': 'GV'}
Plug 'artur-shaik/vim-javacomplete2', {'for': 'java'}
Plug 'vim-pandoc/vim-pandoc-syntax', {'for': 'markdown.pandoc'}
Plug 'nelstrom/vim-markdown-folding', {'for': 'markdown'}
Plug 'tpope/vim-commentary'
Plug 'chaoren/vim-wordmotion'
Plug 'davidhalter/jedi-vim', {'for': 'python'}
Plug 'rhysd/vim-grammarous', {'on': 'GrammarousCheck'}
Plug 'yssl/QFEnter', {'for': 'qf'}
Plug 'junegunn/vader.vim', {'for': ['vim', 'vader']}
Plug 'dhruvasagar/vim-testify', {'for': 'vim'}
Plug 'brookhong/cscope.vim', {'for': ['c', 'cpp']}
Plug 'FelikZ/ctrlp-py-matcher'
Plug 'Konfekt/FastFold'
Plug 'tpope/vim-obsession'
Plug 'leafgarland/typescript-vim', {'for': 'typescript'}
Plug 'ludovicchabant/vim-gutentags'
Plug 'chrisbra/csv.vim', {'for': 'csv'}
Plug 'joonty/vdebug'
Plug 'vim-scripts/a.vim'

if has('python') || has('python3')
  Plug 'SirVer/ultisnips'
  Plug 'honza/vim-snippets'
endif

if has('nvim')
  Plug 'Shougo/deoplete.nvim', {'do': ':UpdateRemotePlugins'}
  Plug 'radenling/vim-dispatch-neovim'
  Plug 'paretje/nvim-man'
  Plug 'kassio/neoterm'
  Plug 'mhartington/nvim-typescript', {'do': ':UpdateRemotePlugins'}  " TODO: vim8?
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
  Plug 'congma/vim-fakeclip'
endif

Plug 'zchee/deoplete-jedi', {'for': 'python'}
Plug 'fishbullet/deoplete-ruby', {'for': 'ruby'}
Plug 'zchee/deoplete-clang', {'for': ['c', 'cpp']}
Plug 'tweekmonster/deoplete-clang2', {'for': ['c', 'cpp']}  " the completions of this plugin don't work properly (they aren't triggered correctly)
Plug 'Shougo/neco-vim', {'for': 'vim'}
Plug 'frbor/deoplete-abook', {'for': 'mail'}

call plug#end()

" run PlugInstall when this is the first time to use vim
if s:plug_install
  PlugInstall
endif

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if has('gui_running')
  " Fix airline in GVim
  let g:airline_symbols = get(g:, 'airline_symbols', {})
  let g:airline_symbols.space = "\u3000"
elseif $TERM !=# ''
  set background=dark
  hi SpellBad ctermfg=Black
  hi SpecialKey ctermfg=8
endif

" Syntax highlighting
syntax on
" Line numbers
set number
" Spelling checker
set spelllang=en
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
set tags=tags,./tags
" Max number of tabs
set tabpagemax=32
" Enable airline
set laststatus=2
" Always enable mouse. This enables mouse in tmux
set mouse=a
" Don't do full autocompletion in command mode
set wildmenu
set wildmode=longest,list,full
" Set keycode timeout to 0 ms. Reduces lag when pressing Alt-O on terminal and
" between leaving insert mode and update of airline
set ttimeoutlen=0
" Set default comments format
set commentstring=#%s
" Disable unloading buffer when abandoned, as needed by vim-dotoo
set hidden
" Return to previous position in file when when opening
au BufReadPost * if &ft != "gitcommit" && line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
" Set mapleader
let g:mapleader = ';'
" Start searching while typing pattern
set incsearch
" Use smartcase matching in autocompletion
" TODO: re-enable when neovim gets fixed
" set infercase
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
set pastetoggle=<Leader>pp
" Show print dialog instead of using the default printer
set printexpr=system(['yad-print',v:fname_in])+v:shell_error
" Don't use tabs unless sleuth detects them
set expandtab
" Disable folding by default
set nofoldenable
" Return to previous window when closing
au WinEnter * if winnr('$') > 1 && exists('t:win') && winnr('$') < t:win | wincmd p | endif | let t:win = winnr('$')
" Close tab when quickfix is only window
au BufEnter * if (winnr("$") == 1 && &filetype ==# 'qf') | quit | endif
" Set window title
set title
" Automatically close preview window
au InsertLeave * if pumvisible() == 0 && &filetype !=# 'dotoo' | pclose | AirlineRefresh | endif
" Disable blinking cursor
if has('nvim')
  let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
  set guicursor=n-v-c:block-Cursor/lCursor-blinkon0,i-ci:ver25-Cursor/lCursor,r-cr:hor20-Cursor/lCursor
endif
" Preview substitutions
if has('nvim')
  " split shows a preview window when doing a substitution on multiple lines
  set inccommand=split
endif
if exists('+signcolumn')
  set signcolumn=yes
else
  let g:gitgutter_sign_column_always = 1
endif

" Airline options
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#tagbar#enabled = 0
let g:airline_theme_patch_func = 'AirlineThemePatch'
let g:airline_detect_spell = 0
let g:airline_symbols = get(g:, 'airline_symbols', {})
let g:airline_symbols.linenr = ''
let g:airline_symbols.maxlinenr = ''
let g:airline_symbols.whitespace = ''
let g:airline_symbols.notexists = ''
let g:airline#extensions#branch#notexists = ''
let g:airline#extensions#whitespace#trailing_format = "\u2219T[%s]"
let g:airline#extensions#whitespace#long_format = "\u2219L[%s]"
let g:airline#extensions#whitespace#mixed_indent_format = "\u2219M[%s]"
let g:airline#extensions#whitespace#mixed_indent_file_format = "\u2219M[%s]"

" CtrlP options
let g:ctrlp_cmd = 'CtrlPMixed'
if executable('ag')
  let g:ctrlp_user_command = 'sh -c "cd %s; ag -l --nocolor --hidden -f -g \"\""'
endif
let g:ctrlp_mruf_exclude = '/\.git/.*\|/tmp/.*\|term://.*'
let g:ctrlp_switch_buffer = ''
let g:ctrlp_mruf_exclude_nomod = 1
let g:ctrlp_match_func = { 'match': 'pymatcher#PyMatch' }

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" UltiSnips options
let g:UltiSnipsExpandTrigger = '<C-J>'

" neco-ghc options
let g:necoghc_enable_detailed_browse = 1

" vim-dotoo options
if $HOST ==# 'parsley'
  let g:dotoo#agenda#files = ['~/vcs/senso2me/notes/*.org']
else
  let g:dotoo#agenda#files = ['~/vcs/personal/notes/*.org', '~/vcs/senso2me/notes/s2m-refile.org']
endif
let g:dotoo#capture#refile = $ORG_REFILE
let g:dotoo#parser#todo_keywords = ['TODO', 'NEXT', 'WAITING', 'HOLD', 'PHONE', 'MEETING', 'MAIL', '|', 'CANCELLED', 'DONE']
let g:dotoo_todo_keyword_faces = [
  \ ['TODO',      [':foreground 160', ':weight bold']],
  \ ['NEXT',      [':foreground 27',  ':weight bold']],
  \ ['DONE',      [':foreground 22',  ':weight bold']],
  \ ['WAITING',   [':foreground 202', ':weight bold']],
  \ ['HOLD',      [':foreground 53',  ':weight bold']],
  \ ['CANCELLED', [':foreground 22',  ':weight bold']],
  \ ['MEETING',   [':foreground 22',  ':weight bold']],
  \ ['PHONE',     [':foreground 22',  ':weight bold']],
  \ ['MAIL',      [':foreground 25',  ':weight bold']]
\ ]

" vim-speeddating options
au VimEnter * 1SpeedDatingFormat %Y-%m-%d %a %H:%M | 1SpeedDatingFormat %Y-%m-%d %a
au VimEnter * SpeedDatingFormat %Y/%m/%d

" rubycomplete options
let g:rubycomplete_buffer_loading = 1
let g:rubycomplete_classes_in_global = 1
let g:rubycomplete_rails = 1
let g:rubycomplete_use_bundler = 1

" NERDTree options
let g:NERDTreeMapActivateNode = 'l'
let g:NERDTreeMapJumpParent = 'h'
let g:NERDTreeIgnore = ['\.class$', '\.pb.\(h\|cc\)$']
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | quit | endif

" indentLine options
let g:indentLine_fileTypeExclude = ['help', 'dotoo', 'dotoocapture', 'dotooagenda', 'markdown.pandoc', 'ledger', '']
let g:indentLine_showFirstIndentLevel = 1

" xml options
let g:xml_syntax_folding = 1

" calendar-vim options
let g:calendar_monday = 1

" ledger-vim options
let g:ledger_fold_blanks = 1
let g:ledger_maxwidth = 120

" jedi-vim options
if !has('python3')
  let g:jedi#force_py_version = 2
else
  let g:jedi#force_py_version = 3
endif
let g:jedi#completions_enabled = 0
let g:jedi#goto_assignments_command = '<C-]>'
let g:jedi#usages_command = ';]'

" neomake options
au BufWritePost,BufReadPost * if !exists('b:fugitive_type') | Neomake | endif
let g:neomake_error_sign = {'texthl': 'GitGutterDelete'}
let g:neomake_warning_sign = {'texthl': 'GitGutterChange'}

hi NeomakeWarning cterm=underline ctermfg=3 gui=undercurl guisp=Blue
hi NeomakeError cterm=underline ctermfg=1 gui=undercurl guisp=Red

let g:neomake_verbose = 0

let g:neomake_java_enabled_makers = ['javac', 'checkstyle']

let g:neomake_sh_bashate_maker = {
  \ 'errorformat': 
    \ '%EE%n: %m,' .
    \ '%Z - %f%\s%\+: L%l,' .
    \ '%-G%.%#'
\ }
let g:neomake_sh_enabled_makers = ['shellcheck', 'checkbashisms', 'sh']

let g:neomake_python_python_exe = 'python3'
let g:neomake_python_enabled_makers = ['python', 'flake8', 'mypy']

" deoplete options
let g:deoplete#enable_at_startup = 1

let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.html = []
let g:deoplete#omni_patterns.markdown = []

" TODO: use sources instead of ignore_sources
let g:deoplete#ignore_sources = {}
let g:deoplete#ignore_sources._ = ['tag', 'buffer']
for s:ft in ['c', 'cpp', 'python', 'vim', 'java']
  let g:deoplete#ignore_sources[s:ft] = ['tag', 'buffer', 'omni', 'around']
endfor

let g:deoplete#member#prefix_patterns = {}
let g:deoplete#member#prefix_patterns['markdown.pandoc'] = ':'
let g:deoplete#member#prefix_patterns['ledger'] = ':'
let g:deoplete#member#prefix_patterns['dotoo'] = ':'

let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.ledger = "[a-zA-Z](?!.*  )[a-zA-Z.' ]*[a-zA-Z.']"
let g:deoplete#keyword_patterns.dotoo = ':\w+'

let g:deoplete#sources#jedi#python_path = 'python' . g:jedi#force_py_version

let g:deoplete#sources#clang#autofill_neomake = 1

" tagbar options
let g:tagbar_ctags_bin = 'ctags'

" securemodelines options
let g:secure_modelines_allowed_items = [
  \ 'textwidth',     'tw',
  \ 'softtabstop',   'sts',
  \ 'tabstop',       'ts',
  \ 'shiftwidth',    'sw',
  \ 'expandtab',     'et',      'noexpandtab',   'noet',
  \ 'filetype',      'ft',
  \ 'foldmethod',    'fdm',
  \ 'formatoptions', 'fo',
  \ 'readonly',      'ro',      'noreadonly',    'noro',
  \ 'rightleft',     'rl',      'norightleft',   'norl',
  \ 'cindent',       'cin',     'nocindent',     'nocin',
  \ 'smartindent',   'si',      'nosmartindent', 'nosi',
  \ 'autoindent',    'ai',      'noautoindent',  'noai',
  \ 'spell',         'nospell',
  \ 'spelllang',
  \ 'wrap',          'nowrap'
\ ]

" table-mode options
let g:table_mode_toggle_map = 't'

" delimitMate options
let g:delimitMate_expand_cr = 1

" clang_complete options
let g:deoplete#sources#clang#libclang_path = '/usr/lib/llvm-3.8/lib/libclang.so.1'
let g:deoplete#sources#clang#clang_header = '/usr/lib/clang'

" neoterm options
let g:neoterm_size = 15
let g:neoterm_shell = 'busybox sh'
let g:neoterm_autoscroll = 1
let g:noeterm_fixedsize = 1
let g:neoterm_repl_python = 'ipython3'

" wordmotion options
let g:wordmotion_prefix = "\<Leader>"

" vim-grammarous options
let g:grammarous#use_vim_spelllang = 1

" QFEnter options
let g:qfenter_keymap = {}
let g:qfenter_keymap.vopen = ['<C-v>']
let g:qfenter_keymap.hopen = ['<C-CR>', '<C-s>', '<C-x>']
let g:qfenter_keymap.topen = ['<C-t>']
let g:qfenter_enable_autoquickfix = 0

" mundo options
if has('python3')
  let g:mundo_prefer_python3 = 1
endif

" vim-grepper options
let g:grepper = {}
let g:grepper.dir = 'repo,cwd'
au User Grepper call GrepperReset()

" cscope.vim options
let g:cscope_silent = 1
let g:cscope_open_location = 0
au FileType c,cpp set cscopequickfix-=g-

" gitautocommit options
let g:gitautocommit_filetypes = ['dotoo', 'ledger']

" vim-gutentags options
let g:gutentags_file_list_command = {
  \ 'markers': {
    \ '.git': 'git ls-files',
  \ },
\ }

" Bulk options
au FileType text,mail,dotoo,markdown    setlocal spell
au FileType ledger,bbcode,vim,python    setlocal spell
au FileType c,cpp,gitcommit             setlocal spell
au FileType tex,mail                    setlocal spelllang=nl
au FileType dotoo*,ledger               setlocal spelllang=nl,en
au FileType tex,text,bbcode,markdown    setlocal linebreak " don't wrap randomly in a word
au FileType help,dotoo*                 setlocal nolist " disable indentation lines

" Ruby ft options
au FileType eruby inoremap <silent> <buffer> / <C-O>:call CloseTag()<CR>

" Org ft options
au BufEnter *.org              if empty(&filetype) | setfiletype dotoo | endif
au FileType dotoo*             setlocal textwidth=77
au FileType dotoo              setlocal foldenable
au FileType dotoo              nmap <buffer> <C-A> <Plug>SpeedDatingUp
au FileType dotoo              nmap <buffer> <C-X> <Plug>SpeedDatingDown
au FileType dotoocapture       iabbrev <expr> <buffer> <silent> :date: '['.strftime(g:dotoo#time#date_day_format).']'
au FileType dotoocapture       iabbrev <expr> <buffer> <silent> :time: '['.strftime(g:dotoo#time#datetime_format).']'
au FileType dotoo,dotoocapture inoremap <buffer> <C-B> <C-O>:call DotooNewItem()<CR>
au FileType dotooagenda        setlocal nowrap
au FileType dotooagenda        nnoremap <buffer> / :call dotoo#agenda#filter_agendas()<CR>tags<CR>
au BufHidden nmbs.org          setlocal nobuflisted
au FileType dotoo              nnoremap <buffer> <silent> gI :call VimDotoo('clock#start')<CR>
au FileType dotoo              nnoremap <buffer> <silent> gO :call VimDotoo('clock#stop')<CR>
au FileType dotoo              nnoremap <buffer> <silent> cit :call VimDotoo('change_todo')<CR>

" Java ft options
au FileType java setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml
au FileType java let $CLASSPATH="/usr/share/java/junit4.jar:src:test:lib/*"
au FileType java setlocal keywordprg=:JavaDoc
au FileType java nnoremap <buffer> <Leader>i :JCimportAdd<CR>
au BufRead *Test.java let b:tagbar_ignore = 1

" LaTex ft options
let g:tex_flavor = 'latex' " Use LaTeX by default
au FileType tex compiler tex | setlocal makeprg=latexmk\ -pdf\ -cd\ '%'
au FileType tex call AutoMake()

" Haskell ft options
au FileType haskell setlocal omnifunc=necoghc#omnifunc

" HTML ft options
au FileType html inoremap <silent> <buffer> / <C-O>:call CloseTag()<CR>
au FileType html setlocal keywordprg=:HtmlDoc

" ATL ft options
au BufRead *.atl setlocal syntax=haskell " Haskell syntax seems to be close to ATL
au BufRead *.atl setlocal nospell
au BufRead *.atl setlocal commentstring=--%s

" mail ft options
au FileType mail      setlocal formatoptions+=naw
au FileType mail      setlocal formatlistpat=^\s*\d\+[\]:.)}\t\ ]\s*\\\|^[A-Z][a-zA-Z-]*:\s*
au BufRead /tmp/*mutt* 1substitute/<\(kevindeprey\|info\|vraagje\)@online-urbanus.be>$/<kevin@paretje.be>/ei
au BufRead /tmp/neomutt* setfiletype mail

" markdown ft options
au FileType markdown call AutoMake()
au FileType markdown setlocal filetype=markdown.pandoc
au FileType markdown setlocal concealcursor=n
au FileType markdown setlocal keywordprg=:Dictionary

" ledger ft options
au BufRead,BufNewFile *.journal setfiletype ledger
au FileType ledger              setlocal foldenable
au FileType ledger              compiler ledger
au FileType ledger              au BufWritePost <buffer> Neomake!
au FileType ledger              inoremap <buffer> <C-J> <C-O>:call LedgerEntry()<CR>

" aptconf ft options
au FileType aptconf setlocal commentstring=//%s

" python ft options
au FileType python setlocal omnifunc=
au FileType python nnoremap <silent> <buffer> <Leader>K :PyDoc<CR>

" xmobarrc ft options
au BufRead ~/.xmobarrc setlocal syntax=haskell nospell

" sh ft options
au BufRead ~/.xsession set filetype=sh

" help ft options
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> d <C-D> | endif
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> u <C-U> | endif
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> q <C-W>c | endif
au FileType help if !&modifiable | setlocal nospell | endif

" C and C++ ft options
au FileType c,cpp setlocal commentstring=//%s
au FileType c,cpp nnoremap <buffer> <Leader>] :call CscopeFind('c', expand('<cword>'))<CR>
if !has('nvim')
  au FileType c,cpp nnoremap <buffer> <C-]> :call CscopeFind('g', expand('<cword>'))<CR>
endif

" gradle ft options
au BufRead,BufNewFile *.gradle setfiletype groovy

" crontab ft options
au BufRead,BufNewFile ~/.crontab setfiletype crontab

" qf ft options
au FileType qf nnoremap <silent> <nowait> <buffer> q <C-W>c

" nerdtree ft options
au FileType nerdtree nmap <buffer> za l

" vimperator ft options
au FileType vimperator setlocal commentstring=\"%s

" sql ft options
au FileType sql setlocal commentstring=--%s

" proto ft options
au FileType proto setlocal commentstring=//%s

" typescript ft options
au FileType typescript nnoremap <silent> <buffer> <C-]> :TSDef<CR>
au FileType typescript setlocal keywordprg=:TSDoc

" terminal options
if has('nvim')
  au TermOpen * setlocal nospell
  au TermOpen * setlocal nobuflisted
  au TermOpen * setlocal nonumber

  au TermOpen *:git* startinsert
endif

" browser editor options
au BufRead /tmp/vimperator-*                       setlocal ft=markdown
au BufRead /tmp/qutebrowser-editor-*               setlocal ft=markdown

" notes options
au VimLeave *      if exists('g:sync_notes') | call GitAutocommit('notes') | endif
au FileType dotoo* let g:sync_notes = 1

" Custom key mappings
nnoremap <Up> gk
nnoremap <Down> gj
inoremap <Up> <C-O>gk
inoremap <Down> <C-O>gj
vnoremap <Up> gk
vnoremap <Down> gj
nmap <Tab> za
inoremap <C-E> <C-O>A
inoremap <C-A> <C-O>I
nnoremap <Leader>n :nohlsearch<CR>
cnoremap <C-A> <C-B>
cnoremap <C-D> <Del>
nnoremap <C-H> <C-W>h
nnoremap <C-J> <C-W>j
nnoremap <C-K> <C-W>k
nnoremap <C-L> <C-W>l
nnoremap <silent> <Leader><C-L> :redraw!<CR>
nnoremap <silent> <C-N> :CtrlPBuffer<CR>
nnoremap <silent> <C-G> :NERDTreeToggle<CR>
nnoremap <Leader>s :call GitAutocommit()<CR><CR>
nnoremap <Leader>l :call ToggleSpellLang()<CR>
nnoremap <silent> zi :call ToggleFolding()<CR>
nnoremap <silent> <Leader>tm :call TableModeToggle()<CR>
nmap <Leader>cal <Plug>CalendarV
inoremap <expr><Tab> pumvisible() ? "\<C-N>" : "\<Tab>"
nnoremap <silent> <Leader>tb :TagbarToggle<CR>
nnoremap <silent> <Leader>tfo :call OrgRecalculateTable(@%)<CR>
nnoremap <silent> <Leader>ut :MundoToggle<CR>
nnoremap , ;
nnoremap \ ,
vnoremap , ;
vnoremap \ ,
vnoremap <silent> <Leader>hs :call StageSelection()<CR>
nnoremap <silent> <Leader>rt :call neoterm#test#run('all')<CR>
nnoremap <silent> <Leader>rf :call neoterm#test#run('file')<CR>
nnoremap <silent> <Leader>rc :call neoterm#test#run('current')<CR>
nnoremap <silent> <Leader>rr :call neoterm#test#rerun()<CR>
nnoremap <silent> <Leader>tc :call neoterm#kill()<CR>
nnoremap <silent> <Leader>tl :call neoterm#clear()<CR>
nnoremap <silent> <Leader>tt :call neoterm#toggle()<CR>
nmap gx <Plug>(neoterm-repl-send)
xmap gx <Plug>(neoterm-repl-send)
nmap gxx <Plug>(neoterm-repl-send-line)
nnoremap <silent> gf :call OpenFile()<CR>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
cnoremap <Up> <C-P>
cnoremap <Down> <C-N>
nmap [g <Plug>(grammarous-move-to-previous-error)
nmap ]g <Plug>(grammarous-move-to-next-error)
imap <C-L> <Plug>delimitMateS-Tab
nnoremap <Leader>cc :cclose<CR>
nnoremap <Leader>cl :lclose<CR>
nnoremap <Leader>cp :pclose<CR>
nnoremap <Leader>pt :CtrlPBufTag<CR>

if has('nvim')
  tnoremap <C-Q> <C-\><C-N>
  nnoremap <C-Q> i<C-Q>

  au User ManOpen tmap <buffer> <C-H> <C-W>h
  au User ManOpen tmap <buffer> <C-J> <C-W>j
  au User ManOpen tmap <buffer> <C-K> <C-W>k
  au User ManOpen tmap <buffer> <C-L> <C-W>l
  au User ManOpen tmap <buffer> <Esc> <C-\><C-N>M
  " TODO: move this to nvim-man
  " TODO: use terminal to render formatting, but use nvim as pager
  au User ManOpen startinsert
else
  source $VIMRUNTIME/ftplugin/man.vim
  au FileType man nnoremap <silent> <nowait> <buffer> q <C-W>c
  au FileType man nnoremap <silent> <nowait> <buffer> d <C-D>
  au FileType man nnoremap <silent> <nowait> <buffer> u <C-U>
  au FileType man wincmd L
  nmap K :Man <cword><CR>
endif

" Custom commands
com! -narg=* Ag call Grepper(<f-args>)
com! BeamerBackground hi Normal ctermbg=233 | set background=dark
com! -narg=1 JavaDoc call system('find /usr/share/doc/openjdk-8-doc/api/ /usr/share/doc/junit4/api/ -name "' . <q-args> . '.html" -a -not -path "*/class-use/*" -a -not -path "*/src-html/*" | xargs sensible-browser')
com! -narg=1 HtmlDoc call system('sensible-browser http://www.w3schools.com/TAGS/tag_' . <q-args> . '.asp')
com! -narg=1 SpellInstall call spellfile#LoadFile(<q-args>)
com! -narg=1 JediPythonVersion call jedi#force_py_version(<q-args>) | JediClearCache
com! -narg=? PyDoc call PyDoc(<f-args>)
com! -narg=1 Dictionary call Dictionary(<f-args>)
com! Gmdiff Gsdiff :1 | Gvdiff

" TODO: documentation
" TODO: abort?
" Custom functions
fun! ToggleSpellLang()
  if &spelllang ==# 'en'
    setlocal spelllang=nl
  else
    setlocal spelllang=en
  endif
  setlocal spelllang?
endfun

fun! ToggleFolding()
  if &l:foldmethod ==# 'manual' && !exists('b:lastfdm') && !exists('w:lastfdm')
    setlocal foldmethod=syntax foldenable
    return
  endif
  normal! zi
endfun

fun! OrgRecalculateTable(file)
  let l:pos = getcurpos()
  write
  call system('emacs "' . a:file . '" --batch -f org-table-recalculate-buffer-tables --eval "(save-buffer 0)"')
  edit
  call setpos('.', l:pos)
endfun

fun! TableModeToggle()
  TableModeToggle
  AirlineRefresh
endfun

fun! AirlineTableMode(...)
  if exists('b:table_mode_active') && b:table_mode_active
    let w:airline_section_a = 'TABLE MODE'
  endif
endfun
call airline#add_statusline_func('AirlineTableMode')

" Close HTML tag, assuming the use of delimitMate
fun! CloseTag()
  call feedkeys('/', 'n')
  if matchstr(getline('.'), '\%' . (col('.') - 1) . 'c.') ==# '<'
    call feedkeys("\<C-X>\<C-O>", 'n')
    if matchstr(getline('.'), '\%' . (col('.')) . 'c.') ==# '>'
      call feedkeys("\<BS>\<Delete>>", 'n')
    else
      call feedkeys("\<BS>>", 'n')
    endif
  endif
endfun

fun! AirlineThemePatch(palette)
  if g:airline_theme ==# 'bubblegum'
    for l:mode in keys(a:palette)
      let a:palette[l:mode]['airline_warning'] = ['Blue', 'Yellow', 0,  11]
      let a:palette[l:mode]['airline_error'] = ['White', 'Red', 15, 9]
    endfor
  endif
endfun

fun! AutoMake()
  if filereadable('Makefile')
    au BufWritePost <buffer> Neomake!
  elseif &filetype ==# 'tex' && len(glob('*.tex', 1, 1)) == 1
    au BufWritePost <buffer> Neomake!
  endif
endfun

fun! StageSelection() range
  Gdiff!
  execute a:firstline . ',' . a:lastline . 'diffput'
  wincmd p
  write
  close
endfun

fun! OpenFile()
  if getline('.') =~? '\(^\|\s\)https\?://\|\.\(epub\|cbz\|pdf\|ps\|mp4\|mkv\|mpg\|avi\|wmv\|mpg\|ts\|mpeg\)\(\s\|$\)'
    let l:isfname = &isfname
    if getline('.') =~? '^\s*- \[ \] '
      set isfname=@,48-57,/,.,-,_,+,,,#,$,%,~,=,32,',&,:,!,?,(,)
    elseif getline('.') =~? '^\s*- '
      set isfname=@,48-57,/,.,_,+,,,#,$,%,~,=,32,',&,:,!,?,(,)
    else
      set isfname=@,48-57,/,.,-,_,+,,,#,$,%,~,=,32,',&,:,!,?,(,),[,]
    endif
    call system('cd ' . expand('%:p:h') . ' ; xdg-open ' . expand('<cfile>:s?^\s*\(.\{-}\)\s*$?\1?:s?.*\s\+\([a-z]\+\)://?\1://?:S') . ' > /dev/null 2> /dev/null &')
    let &isfname = l:isfname
  else
    normal! gf
  endif
endfun

fun! GitRoot()
  if exists('g:orig_root')
    return
  endif
  let g:orig_root = getcwd()
  execute 'cd ' . fnameescape(system('git -C ' . expand('%:p:h:S') . ' rev-parse --show-toplevel 2> /dev/null || pwd')[:-2])
endfun

fun! ResetRoot()
  if exists('g:orig_root')
    execute 'cd ' . fnameescape(g:orig_root)
    unlet g:orig_root
  endif
endfun

fun! LedgerEntry()
  if getline('.') !~? ':'
    call setline('.', shellescape(getline('.')))
  endif
  call ledger#entry()
  call search('EUR ', 'We')
  execute "normal! lv$h\<C-G>"
endfun

fun! GitAutocommit(...)
  if a:0 == 0 && index(g:gitautocommit_filetypes, &filetype) != -1
    Git autocommit --force
  else
    for l:dir in ['personal', 'senso2me']
      if !empty(glob('~/vcs/' . l:dir . '/notes/.git'))
        execute '!git -C ~/vcs/' . l:dir . '/notes autocommit'
      endif
    endfor
  endif
endfun

fun! VimDotoo(func)
  let l:pos = getcurpos()
  if getline('.') !~? '^\*'
    call search('^\*', 'b')
  endif
  if &modified
    write
  endif
  exe 'call dotoo#' . a:func . '()'
  call setpos('.', l:pos)
  normal zuz
  normal! zO
endfun

fun! Grepper(...)
  if &filetype ==# 'nerdtree'
    wincmd p
  endif

  if !exists('g:grepper_dir')
    let g:grepper_dir = haslocaldir() ? getcwd() : ''
  endif

  execute 'Grepper -tool ag -open -switch -highlight -query ' . join(map(copy(a:000), 'shellescape(v:val)'), ' ')
endfun

fun! GrepperReset()
  if !exists('g:grepper_dir')
    return
  endif

  " TODO: wincmd p should be replaced with an actual test on the origin window
  " and the qf window
  call GrepperResetDir()
  wincmd p
  call GrepperResetDir()
  wincmd p
  unlet g:grepper_dir
endfun

fun! GrepperResetDir()
  if g:grepper_dir ==# ''
    let l:tab = getcwd(-1)
    let l:global = getcwd(-1, -1)
    if l:tab != l:global
      execute 'tcd ' . fnameescape(l:tab)
    else
      execute 'cd ' . fnameescape(l:global)
    endif
  else
    execute 'lcd ' . fnameescape(g:grepper_dir)
  endif
endfun

fun! Dictionary(word)
  if &spelllang =~# 'nl'
    call system('sensible-browser ' . shellescape('http://woordenlijst.org/#/?bwc=1&q=' . a:word))
  endif
endfun

fun! PyDoc(...) abort
  if a:0 > 1
    echoerr 'Too many arguments'
    return
  elseif a:0 == 1
    if g:jedi#force_py_version == 2
      let l:pydoc = 'pydoc2.7'
    else
      let l:pydoc = 'pydoc3'
    endif
    execute 'split | terminal ' . l:pydoc . ' ' . shellescape(a:1)
    doau User ManOpen
  else
    " TODO: handle multiple definitions
    " TODO: handle no definitions without silent
    silent! PythonJedi vim.command('PyDoc ' + jedi_vim.get_script().goto_definitions()[0].full_name)
  endif
endfun

fun! DotooNewItem()
  call feedkeys("\<C-O>cc ", 'n')

  let l:pos = getcurpos()
  if getline('.') !~? '^[[:space:]]*-'
    call search('^[[:space:]]*-', 'b')
  endif
  let l:prev = getline('.')
  call setpos('.', l:pos)

  if dotoo#checkbox#is_checkbox(l:prev)
    call feedkeys("\<C-O>c6h- [ ]\<C-O>A", 'n')
  else
    call feedkeys("\<C-O>c2h-\<C-O>A", 'n')
  endif
endfun
