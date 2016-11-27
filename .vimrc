scriptencoding utf-8

" vint: -ProhibitAutocmdWithNoGroup
" remove all autocmd's
autocmd!

" download vim-plug if needed
if !filereadable($HOME . '/.vim/autoload/plug.vim')
  if executable('curl')
    execute '!curl -fLo ~/.vim/autoload/plug.vim --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  elseif executable('wget')
    execute '!mkdir -P ~/.vim/autoload'
    execute '!wget -O ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  endif
endif

" load plug and declare all plugins
call plug#begin('~/.vim/bundle')

Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-fugitive'
Plug 'vim-airline/vim-airline'
Plug 'Keithbsmiley/tmux.vim'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'paretje/vim-dotoo', {'branch': 'merged'}
Plug 'Yggdroot/indentLine'
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-endwise'
Plug 'ciaranm/securemodelines'
Plug 'tpope/vim-unimpaired'
Plug 'airblade/vim-gitgutter'
Plug 'AndrewRadev/splitjoin.vim'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-eunuch'
Plug 'tpope/vim-sleuth'
Plug 'vim-airline/vim-airline-themes'
Plug 'tpope/vim-commentary'
Plug 'chaoren/vim-wordmotion'
Plug 'yssl/QFEnter', {'for': 'qf'}

if has('nvim')
  Plug 'paretje/nvim-man'
  Plug 'kassio/neoterm'
else
  Plug 'congma/vim-fakeclip'
endif

call plug#end()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if has('gui_running')
  " Fix airline in GVim
  if !exists('g:airline_symbols')
    let g:airline_symbols = {}
  endif
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
let g:mapleader = ';'
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
set pastetoggle=<Leader>p
" Show print dialog instead of using the default printer
set printexpr=system(['yad-print',v:fname_in])+v:shell_error
" Don't use tabs unless sleuth detects them
set expandtab
" Disable folding by default
set nofoldenable
" Return to previous window when closing
au WinEnter * if winnr('$') > 1 && exists('t:win') && winnr('$') < t:win | wincmd p | endif | let t:win = winnr('$')

" Airline options
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#tagbar#enabled = 0
let g:airline_theme_patch_func = 'AirlineThemePatch'
let g:airline_detect_spell = 0
let g:airline_symbols = get(g:, 'airline_symbols', {})
let g:airline_symbols.maxlinenr = ''

" CtrlP options
let g:ctrlp_cmd = 'CtrlPMixed'
let g:ctrlp_user_command = 'sh -c "cd %s; ag -l --nocolor --hidden -f -g \"\""'
let g:ctrlp_mruf_exclude = '/\.git/.*\|/tmp/.*\|term://.*'
let g:ctrlp_switch_buffer = ''
let g:ctrlp_mruf_exclude_nomod = 1

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" UltiSnips options
let g:UltiSnipsExpandTrigger = '<C-J>'

" neco-ghc options
let g:necoghc_enable_detailed_browse = 1

" vim-dotoo options
let g:dotoo#agenda#files = ['~/vcs/personal/notes/*.org']
let g:dotoo#capture#refile = '~/vcs/personal/notes/refile.org'
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
let g:NERDTreeIgnore = ['\.class$']
au BufEnter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif

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

" neomake options
let g:neomake_error_sign = {'texthl': 'GitGutterDelete'}
let g:neomake_warning_sign = {'texthl': 'GitGutterChange'}
let g:neomake_verbose = 0

let g:neomake_java_enabled_makers = ['javac', 'checkstyle']

let g:neomake_sh_bashate_maker = {
  \ 'errorformat': 
    \ '%EE%n: %m,' .
    \ '%Z - %f%\s%\+: L%l,' .
    \ '%-G%.%#'
\ }
let g:neomake_sh_enabled_makers = ['shellcheck', 'checkbashisms', 'sh']

" deoplete options
let g:deoplete#enable_at_startup = 1
let g:deoplete#omni_patterns = {}
let g:deoplete#omni_patterns.html = []
let g:deoplete#omni_patterns.markdown = []
let g:deoplete#ignore_sources = {}
let g:deoplete#ignore_sources._ = ['tag', 'buffer']
let g:deoplete#ignore_sources.c = ['tag', 'buffer', 'omni']
let g:deoplete#ignore_sources.java = ['tag', 'buffer', 'member']
let g:deoplete#ignore_sources.ledger = ['tag']
let g:deoplete#member#prefix_patterns = {}
let g:deoplete#member#prefix_patterns['markdown.pandoc'] = ':'
let g:deoplete#member#prefix_patterns['ledger'] = ':'
let g:deoplete#keyword_patterns = {}
let g:deoplete#keyword_patterns.ledger = "[a-zA-Z](?!.*  )[a-zA-Z.' ]*[a-zA-Z.']"

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

" gitgutter options
let g:gitgutter_sign_column_always = 1

" neoterm options
let g:neoterm_size = 15
let g:neoterm_shell = 'busybox sh'

" wordmotion options
let g:wordmotion_prefix = "\<Leader>"

" jedi-vim options
let g:jedi#completions_enabled = 0
let g:jedi#force_py_version = 3

" vim-grammarous options
let g:grammarous#use_vim_spelllang = 1

" QFEnter options
let g:qfenter_vopen_map = ['<C-v>']
let g:qfenter_hopen_map = ['<C-CR>', '<C-s>', '<C-x>']
let g:qfenter_topen_map = ['<C-t>']

" Bulk options
au FileType haskell,prolog,matlab,tmux  setlocal nospell
au FileType dotooagenda,calendar,qf,man setlocal nospell
au FileType vim-plug,git                setlocal nospell
au FileType dotoo*,tex,mail             setlocal spelllang=nl
au FileType tex,text,bbcode,markdown    setlocal linebreak " don't wrap randomly in a word
au FileType help,dotoo*                 setlocal nolist " disable indentation lines

" Ruby ft options
au FileType eruby inoremap <silent> <buffer> / <C-O>:call CloseTag()<CR>

" Org ft options
au BufRead,BufNewFile *.org    setfiletype dotoo
au FileType dotoo*             setlocal textwidth=77
au FileType dotoo              setlocal foldenable
au FileType dotoo              nmap <buffer> <C-A> <Plug>SpeedDatingUp
au FileType dotoo              nmap <buffer> <C-X> <Plug>SpeedDatingDown
au FileType dotoocapture       iabbrev <expr> <buffer> <silent> :date: '['.strftime(g:dotoo#time#date_day_format).']'
au FileType dotoocapture       iabbrev <expr> <buffer> <silent> :time: '['.strftime(g:dotoo#time#datetime_format).']'
au FileType dotoo,dotoocapture inoremap <buffer> <C-B> <Space><C-O>c6h- [ ]<C-O>A
au FileType dotooagenda        setlocal nowrap
au FileType dotooagenda        nnoremap <buffer> / :call dotoo#agenda#filter_agendas()<CR>tags<CR>
au BufHidden nmbs.org          setlocal nobuflisted
au BufEnter ~/vcs/personal/notes/*.org              call GitRoot() | au BufLeave <buffer> call ResetRoot()
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
au BufRead /tmp/mutt* 1substitute/<\(kevindeprey\|info\|vraagje\)@online-urbanus.be>$/<kevin@paretje.be>/ei

" markdown ft options
au FileType markdown call AutoMake()
au FileType markdown setlocal filetype=markdown.pandoc
au FileType markdown setlocal concealcursor=n

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

" xmobarrc ft options
au BufRead ~/.xmobarrc setlocal syntax=haskell nospell

" sh ft options
au BufRead ~/.xsession set filetype=sh

" help ft options
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> d <C-D> | endif
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> u <C-U> | endif
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> q <C-W>c | endif

" C ft options
au FileType c setlocal commentstring=//%s

" gradle ft options
au BufRead,BufNewFile *.gradle setfiletype groovy

" crontab ft options
au BufRead,BufNewFile ~/.crontab setfiletype crontab

" qf ft options
au FileType qf nnoremap <silent> <nowait> <buffer> q <C-W>c

" terminal options
if has('nvim')
  au TermOpen * setlocal nospell
  au TermOpen * setlocal nobuflisted
endif

" It's All Text options
au BufRead ~/.mozilla/firefox/*/itsalltext/github* setlocal ft=markdown
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
nnoremap <silent> <Leader>r :redraw!<CR>
nnoremap <silent> <C-N> :CtrlPBuffer<CR>
nnoremap <silent> <C-G> :NERDTreeToggle<CR>
nnoremap j gj
nnoremap k gk
nnoremap gj j
nnoremap gk k
nnoremap <Leader>s :call GitAutocommit()<CR><CR>
nnoremap <Leader>l :call ToggleSpellLang()<CR>
nnoremap <silent> zi :call ToggleFolding()<CR>
nnoremap <silent> <Leader>tm :call TableModeToggle()<CR>
nmap <Leader>cal <Plug>CalendarV
inoremap <expr><Tab> pumvisible() ? "\<C-N>" : "\<Tab>"
nnoremap <silent> <Leader>tb :TagbarToggle<CR>
nnoremap <Leader>tfo :call OrgRecalculateTable(@%)<CR>
nnoremap <Leader>ut :MundoToggle<CR>
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
nnoremap <silent> gf :call OpenFile()<CR>
cnoremap <C-P> <Up>
cnoremap <C-N> <Down>
cnoremap <Up> <C-P>
cnoremap <Down> <C-N>
nmap [g <Plug>(grammarous-move-to-previous-error)
nmap ]g <Plug>(grammarous-move-to-next-error)

if has('nvim')
  tnoremap <C-Q> <C-\><C-N>
  nnoremap <C-Q> i<C-Q>

  au User ManOpen tmap <buffer> <C-H> <C-W>h
  au User ManOpen tmap <buffer> <C-J> <C-W>j
  au User ManOpen tmap <buffer> <C-K> <C-W>k
  au User ManOpen tmap <buffer> <C-L> <C-W>l
  au User ManOpen tmap <buffer> <Esc> <C-\><C-N>M
else
  source $VIMRUNTIME/ftplugin/man.vim
  au FileType man nnoremap <silent> <nowait><buffer> q <C-W>c
  au FileType man wincmd L
  nmap K :Man <cword><CR>
endif

" Custom commands
com! -narg=* Ag Grepper -tool ag -open -switch -highlight -query <args>
com! BeamerBackground hi Normal ctermbg=233 | set background=dark
com! -narg=1 JavaDoc call system('find /usr/share/doc/openjdk-8-doc/api/ /usr/share/doc/junit4/api/ -name "' . <q-args> . '.html" -a -not -path "*/class-use/*" -a -not -path "*/src-html/*" | xargs qutebrowser')
com! -narg=1 HtmlDoc call system('qutebrowser http://www.w3schools.com/TAGS/tag_' . <q-args> . '.asp')
com! -narg=1 SpellInstall call spellfile#LoadFile(<q-args>)

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
  if &l:foldmethod ==# 'manual'
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
    call feedkeys("\<C-X>\<C-O>\<C-N>\<BS>\<Right>\<BS>>", 'n')
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
    call system('cd ' . expand('%:p:h') . ' ; xdg-open ' . expand('<cfile>:s?^\s*\(.\{-}\)\s*$?\1?:S') . ' > /dev/null 2> /dev/null &')
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
  if a:0 == 0 && &filetype ==# 'ledger'
    Git autocommit
  else
    execute '!git -C ~/vcs/personal/notes autocommit'
  endif
endfun

fun! VimDotoo(func)
  let l:pos = getcurpos()
  call search('^\*', 'b')
  exe 'call dotoo#' . a:func . '()'
  call setpos('.', l:pos)
  normal! zO
endfun
