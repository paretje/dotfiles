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
    execute '!mkdir -p ~/.vim/autoload'
    execute '!wget -O ~/.vim/autoload/plug.vim https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  endif
  let s:plug_install = 1
endif

" load plug and declare all plugins
call plug#begin('~/.vim/bundle')

Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-fugitive'
Plug 'godlygeek/tabular', {'on': 'Tabularize'} " used by vim-table-mode
Plug 'vim-airline/vim-airline', {'tag': '*'}
Plug 'Keithbsmiley/tmux.vim', {'for': 'tmux'}
Plug 'neovimhaskell/haskell-vim', {'for': 'haskell'}
Plug 'eagletmt/neco-ghc', {'for': 'haskell'}
Plug 'nvim-orgmode/orgmode', {'tag': '*'}  " TODO: tag?
Plug 'Yggdroot/indentLine'
Plug 'vim-ruby/vim-ruby', {'for': 'ruby'}
Plug 'scrooloose/nerdtree', {'on': 'NERDTreeToggle'}
Plug 'tpope/vim-rails', {'for': ['ruby', 'eruby']}
Plug 'dhruvasagar/vim-table-mode', {'on': 'TableModeToggle'}
Plug 'Raimondi/delimitMate'
Plug 'tpope/vim-endwise'
Plug 'vim-scripts/bbcode', {'for': 'bbcode'}
Plug 'paretje/securemodelines'
Plug 'tpope/vim-unimpaired'
Plug 'mattn/calendar-vim', {'on': '<Plug>CalendarV'}
Plug 'airblade/vim-gitgutter'
Plug 'ledger/vim-ledger', {'for': 'ledger'}
Plug 'simnalamburt/vim-mundo', {'on': 'MundoToggle'}
Plug 'neomake/neomake'
Plug 'AndrewRadev/splitjoin.vim'  " TODO: use?
Plug 'lucapette/vim-ruby-doc', {'for': ['ruby', 'eruby']}
Plug 'tpope/vim-repeat'
Plug 'Shougo/neopairs.vim'
Plug 'tpope/vim-surround'
Plug 'paretje/async-grepper', {'on': 'Grepper'}
Plug 'majutsushi/tagbar'
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
Plug 'rhysd/vim-grammarous', {'on': 'GrammarousCheck'}
Plug 'yssl/QFEnter', {'for': 'qf'}
Plug 'junegunn/vader.vim', {'for': ['vim', 'vader']}
Plug 'dhruvasagar/vim-testify', {'for': 'vim'}
Plug 'brookhong/cscope.vim', {'for': ['c', 'cpp']}  " TODO: use?
Plug 'Konfekt/FastFold'
Plug 'tpope/vim-obsession', {'on': 'Obsession'}
Plug 'HerringtonDarkholme/yats.vim', {'for': 'typescript'}
Plug 'ludovicchabant/vim-gutentags'
Plug 'chrisbra/csv.vim', {'for': 'csv'}
Plug 'joonty/vdebug'  " TODO: on
Plug 'cespare/vim-toml', {'for': 'toml'}
Plug 'vhdirk/vim-cmake', {'for': ['c', 'cpp', 'cmake']}
Plug 'skywind3000/asyncrun.vim' " used by async-grepper and vim-cmake
Plug 'Shougo/vimproc.vim', {'do' : 'make'} " used by vim-vebugger
Plug 'idanarye/vim-vebugger'  " TODO: on
Plug 'paretje/suda.vim', {'branch': 'feature/disable-no-password-check'}
Plug 'solarnz/thrift.vim', {'for': 'thrift'}
Plug 'pearofducks/ansible-vim', {'for': 'yaml.ansible'}
Plug 'petobens/poet-v', {'for': 'python'}
Plug 'goerz/jupytext.vim'  " TODO: on
Plug 'psf/black', {'for': 'python', 'branch': 'stable'}
Plug 'fisadev/vim-isort', {'for': 'python'}
Plug 'towolf/vim-helm', {'for': 'helm'}
Plug 'martinda/Jenkinsfile-vim-syntax', {'for': 'Jenkinsfile'}
Plug 'vim-scripts/groovyindent-unix', {'for': 'groovy'}
Plug 'posva/vim-vue', {'for': 'vue'}
Plug 'nvim-treesitter/nvim-treesitter', {'tag': '*', 'do': ':TSUpdate'}

if has('nvim')
  Plug 'paretje/nvim-man'
  Plug 'kassio/neoterm'  " TODO: use?

  Plug 'dcampos/nvim-snippy'
  Plug 'honza/vim-snippets'
  Plug 'dcampos/cmp-snippy'

  Plug 'neovim/nvim-lspconfig'
  Plug 'hrsh7th/cmp-nvim-lsp'

  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-cmdline'
  Plug 'hrsh7th/nvim-cmp'

  Plug 'ibhagwan/fzf-lua'
else
  Plug 'congma/vim-fakeclip'
endif

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
  if has('nvim')
    colorscheme vim
  endif
  set notermguicolors
  set background=dark
  hi SpellBad ctermfg=White
  hi SpellCap ctermfg=White
  hi SpellLocal ctermfg=DarkGrey
  hi SpecialKey ctermfg=8
  hi Pmenu        ctermfg=15   ctermbg=8
  hi PmenuSel     ctermfg=8    ctermbg=15

  if has('nvim')
    hi link @variable Normal
    hi link @variable.parameter Normal
    hi link @variable.member Normal
    hi link @variable.builtin Identifier
    " hi link @variable.vim Identifier
    hi link @type.builtin Type
    hi link @constructor Identifier
  endif
endif

" Syntax highlighting
syntax on
" Line numbers
set number
" Spelling checker
set spelllang=en_gb
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
au BufReadPost * if &ft != 'gitcommit' && line("'\"") > 1 && line("'\"") <= line('$') | exe "normal! g'\"" | endif
" Set mapleader
let g:mapleader = ';'
" Start searching while typing pattern
set incsearch
" Use smartcase matching in autocompletion
" TODO: re-enable when neovim gets fixed
" set infercase
" Show tab indentation levels
set list
set listchars=tab:¦\ ,trail:·
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
if !has('nvim')
  set printexpr=system(['yad-print',v:fname_in])+v:shell_error
endif
" Don't use tabs unless sleuth detects them
set expandtab
" Disable folding by default
set nofoldenable
" Return to previous window when closing
au WinEnter * if winnr('$') > 1 && exists('t:win') && winnr('$') < t:win | wincmd p | endif | let t:win = winnr('$')
" Close tab when quickfix is only window
au BufEnter * if (winnr('$') == 1 && &filetype ==# 'qf') | quit | endif
" Set window title
set title
" Automatically close preview window
au InsertLeave * if pumvisible() == 0 | pclose | AirlineRefresh | endif
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
" Enable signcolumn
set signcolumn=yes
au FileType qf,calendar,tagbar,nerdtree setlocal signcolumn=no
highlight! link SignColumn LineNr
" Emit CursorHold event sooner (e.g. used by GitGutter) and write swap file
set updatetime=500

" Airline options
let g:airline_powerline_fonts = 1
let g:airline_theme = 'bubblegum'
let g:airline#extensions#tagbar#enabled = 1
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
let g:airline#extensions#csv#column_display = 'Name'

" Fugitive options
au BufReadPost fugitive://* set bufhidden=delete

" neco-ghc options
let g:necoghc_enable_detailed_browse = 1

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
au BufEnter * if (winnr('$') == 1 && exists('b:NERDTreeType') && b:NERDTreeType == 'primary') | quit | endif

" indentLine options
let g:indentLine_fileTypeExclude = ['help', 'org', 'markdown.pandoc', 'ledger', '']
let g:indentLine_showFirstIndentLevel = 1

" xml options
let g:xml_syntax_folding = 1

" calendar-vim options
let g:calendar_monday = 1

" ledger-vim options
let g:ledger_fold_blanks = 1
let g:ledger_maxwidth = 120
let g:ledger_align_at = 50

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
let g:neomake_python_mypy_args = ['--check-untyped-defs', '--ignore-missing-imports']
let g:neomake_python_enabled_makers = ['python', 'flake8']

let g:neomake_cpp_enabled_makers = ['gcc', 'clang', 'clangtidy', 'clangcheck', 'cppcheck']

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

" neoterm options
let g:neoterm_size = 15
let g:neoterm_shell = 'busybox sh'
let g:neoterm_autoscroll = 1
let g:neoterm_fixedsize = 1
let g:neoterm_repl_python = 'ipython3'
let g:neoterm_default_mod = 'split'

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
let g:grepper.open = 1
let g:grepper.switch = 1
let g:grepper.highlight = 1

" cscope.vim options
let g:cscope_silent = 1
let g:cscope_open_location = 0
au FileType c,cpp set cscopequickfix-=g-

" gitautocommit options
let g:gitautocommit_filetypes = ['org', 'ledger']

" vim-gutentags options
let g:gutentags_file_list_command = {
  \ 'markers': {
    \ '.git': 'git ls-files',
  \ },
\ }

" vim-cmake options
let g:cmake_export_compile_commands = 1

" poet-v options
let g:poetv_auto_activate = 1
let g:poetv_set_environment = 0

" suda.vim options
let g:suda#try_without_password = !has('nvim')

" gitgutter options
let g:gitgutter_preview_win_floating = 0

highlight GitGutterAdd    guifg=#009900 ctermfg=2
highlight GitGutterChange guifg=#bbbb00 ctermfg=3
highlight GitGutterDelete guifg=#ff2222 ctermfg=1

" orgmode options
" TODO: org_archive_location
if has('nvim')
  lua << EOF
  require('orgmode').setup({
    org_agenda_files = {'~/vcs/personal/notes/*.org', '~/vcs/vib/notes/refile.org'},
    org_default_notes_file = os.getenv("ORG_REFILE"),
    org_startup_indented = true,
    mappings = {
      org = {
        org_do_promote = false,
        org_do_demote = false,
        org_toggle_checkbox = 'cic'
      }
    }
  })
EOF
endif

" treesitter options
if has('nvim')
  lua << EOF
      require('nvim-treesitter.configs').setup({
        highlight = {
          enable = true,
        }
      })
EOF
endif

" cmp-nvim options
if has('nvim')
  " cmp-nvim
  lua <<EOF
    -- Set up nvim-cmp.
    local cmp = require'cmp'

    cmp.setup({
      snippet = {
        -- REQUIRED - you must specify a snippet engine
        expand = function(args)
          -- vim.fn["vsnip#anonymous"](args.body) -- For `vsnip` users.
          -- require('luasnip').lsp_expand(args.body) -- For `luasnip` users.
          require('snippy').expand_snippet(args.body) -- For `snippy` users.
          -- vim.fn["UltiSnips#Anon"](args.body) -- For `ultisnips` users.
          -- vim.snippet.expand(args.body) -- For native neovim snippets (Neovim v0.10+)
        end,
      },
      window = {
        -- completion = cmp.config.window.bordered(),
        -- documentation = cmp.config.window.bordered(),
      },
      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<CR>'] = cmp.mapping.confirm({ select = false }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        ['<Tab>'] = cmp.mapping(function(fallback)
          if cmp.visible() then
            cmp.select_next_item()
          else
            fallback() -- The fallback function sends a already mapped key. In this case, it's probably `<Tab>`.
          end
        end)
      }),
      sources = cmp.config.sources({
        { name = 'nvim_lsp' },
        -- { name = 'vsnip' }, -- For vsnip users.
        -- { name = 'luasnip' }, -- For luasnip users.
        -- { name = 'ultisnips' }, -- For ultisnips users.
        { name = 'snippy' }, -- For snippy users.
      }, {
        { name = 'buffer' },
        { name = 'path' },
      })
    })

    -- To use git you need to install the plugin petertriho/cmp-git and uncomment lines below
    -- Set configuration for specific filetype.
    --[[ cmp.setup.filetype('gitcommit', {
      sources = cmp.config.sources({
        { name = 'git' },
      }, {
        { name = 'buffer' },
      })
   })
   require("cmp_git").setup() ]]-- 

    -- Use buffer source for `/` and `?` (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline({
        ['<C-p>'] = cmp.mapping.abort(),
        ['<C-n>'] = cmp.mapping.abort()
      }),
      sources = {
        { name = 'buffer' }
      }
    })

    -- Use cmdline & path source for ':' (if you enabled `native_menu`, this won't work anymore).
    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline({
        ['<C-p>'] = cmp.mapping.abort(),
        ['<C-n>'] = cmp.mapping.abort()
      }),
      sources = cmp.config.sources({
        { name = 'path' }
      }, {
        { name = 'cmdline' }
      }),
      matching = { disallow_symbol_nonprefix_matching = false }
    })

    -- Set up lspconfig.
    local capabilities = require('cmp_nvim_lsp').default_capabilities()
    -- Replace <YOUR_LSP_SERVER> with each lsp server you've enabled.
    require('lspconfig')['jedi_language_server'].setup {
        capabilities = capabilities
      }
    require('lspconfig')['rust_analyzer'].setup {
        capabilities = capabilities
      }
EOF
endif

if has('nvim')
  lua <<EOF
    require('snippy').setup({
        mappings = {
            is = {
                ['<C-J>'] = 'expand_or_advance',
            }
        },
    })
EOF
endif

" fzf-lua options
" TODO: use no_ignore in homedir?
if has('nvim')
  lua require("fzf-lua").setup({'fzf-vim', files={hidden=true}})
endif

" Bulk options
au FileType text,mail,org,markdown      setlocal spell
au FileType ledger,bbcode,vim,python    setlocal spell
au FileType c,cpp,gitcommit             setlocal spell
au FileType tex,mail                    setlocal spelllang=nl
au FileType org,ledger                  setlocal spelllang=nl,en_gb
au FileType tex,text,bbcode,markdown    setlocal linebreak " don't wrap randomly in a word
au FileType help,org                    setlocal nolist " disable indentation lines

" Ruby ft options
au FileType eruby inoremap <silent> <buffer> / <C-O>:call CloseTag()<CR>

" Org ft options
au FileType org setlocal textwidth=77
au FileType org setlocal foldenable
au FileType org inoremap <buffer> <C-CR> <cmd>lua require("orgmode").action("org_mappings.meta_return")<CR>
au FileType org inoreabbrev <silent><buffer> :time: [<C-R>=luaeval("require('orgmode.objects.date').now():to_string()")<CR>]

" Java ft options
au FileType java setlocal tags+=/usr/lib/jvm/openjdk-8/tags
au FileType java compiler ant | setlocal makeprg=ant\ -e\ -s\ build.xml
au FileType java let $CLASSPATH='/usr/share/java/junit4.jar:src:test:lib/*'
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

" xmobarrc ft options
au BufRead ~/.xmobarrc setlocal syntax=haskell nospell

" sh ft options
au BufRead ~/.xsession set filetype=sh
au FileType sh let &l:path = substitute($PATH, ':', ',', '')

" help ft options
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> d <C-D> | endif
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> u <C-U> | endif
au FileType help if !&modifiable | nnoremap <silent> <nowait> <buffer> q <C-W>c | endif
au FileType help if !&modifiable | setlocal nospell | endif

" C and C++ ft options
au FileType c,cpp,arduino setlocal commentstring=//%s
au FileType c,cpp         call ExtractCMakeBuildArgs()
au FileType c,cpp         nnoremap <buffer> <Leader>] :call CscopeFind('c', expand('<cword>'))<CR>
au FileType cpp           setlocal keywordprg=:CppMan

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

" vim ft options
au FileType vim setlocal iskeyword+=:

" ansible ft options
au BufRead,BufNewFile */playbooks/*.yml set filetype=yaml.ansible
au BufRead,BufNewFile */playbooks/*.yaml set filetype=yaml.ansible
au FileType yaml.ansible setlocal keywordprg=:AnsibleDoc

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
au VimLeave *   if exists('g:sync_notes') | call GitAutocommit('notes') | endif
au FileType org let g:sync_notes = 1

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
nnoremap <C-P> :Files<CR>
nnoremap <C-N> :Buffers<CR>
nnoremap <C-N> :Buffers<CR>
nnoremap <Leader>po :FzfLua oldfiles<CR>
nnoremap <Leader>pt :Tags<CR>

if has('nvim')
  tnoremap <C-Q> <C-\><C-N>
  nnoremap <C-Q> i<C-Q>

  au User ManOpen tmap <buffer> <C-H> <C-W>h
  au User ManOpen tmap <buffer> <C-J> <C-W>j
  au User ManOpen tmap <buffer> <C-K> <C-W>k
  au User ManOpen tmap <buffer> <C-L> <C-W>l
  au User ManOpen tmap <buffer> <Esc> <C-\><C-N>M

  " https://github.com/neovim/neovim/issues/11330#issuecomment-723667383
  au VimEnter * :silent exec "!kill -s SIGWINCH $PPID"
else
  source $VIMRUNTIME/ftplugin/man.vim
  au FileType man nnoremap <silent> <nowait> <buffer> q <C-W>c
  au FileType man nnoremap <silent> <nowait> <buffer> d <C-D>
  au FileType man nnoremap <silent> <nowait> <buffer> u <C-U>
  au FileType man wincmd L
  nmap K :Man <cword><CR>
endif

" Custom commands
com! -nargs=+ -complete=file Ag Grepper -noprompt -tool ag -query <args>
com! BeamerBackground hi Normal ctermbg=233 | set background=dark
com! -nargs=1 JavaDoc call system('find /usr/share/doc/openjdk-8-doc/api/ /usr/share/doc/junit4/api/ -name "' . <q-args> . '.html" -a -not -path "*/class-use/*" -a -not -path "*/src-html/*" | xargs sensible-browser')
com! -nargs=1 HtmlDoc call system('sensible-browser http://www.w3schools.com/TAGS/tag_' . <q-args> . '.asp')
com! -nargs=1 SpellInstall call spellfile#LoadFile(<q-args>)
com! -nargs=1 Dictionary call Dictionary(<f-args>)
com! Gmdiffsplit Ghdiffsplit! :1 | Gvdiffsplit!
com! -bang -nargs=* -complete=file Make AsyncRun -program=make @ <args>
com! -nargs=1 CppMan call CppMan(<f-args>)
com! W w
com! SudoRead  edit  suda://%
com! SudoWrite write suda://%
com! -nargs=1 AnsibleDoc call AnsibleDoc(<f-args>)
com! Gstatus Git

" TODO: documentation
" TODO: abort?
" Custom functions
fun! ToggleSpellLang()
  if !&spell
    setlocal spell
  elseif &spelllang ==# 'en_gb'
    setlocal spelllang=nl
  elseif &spelllang ==# 'nl'
    setlocal nospell
    setlocal spelllang=en_gb
  endif
  setlocal spell? spelllang?
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
  if getline('.') =~? '\(^\|\s\)https\?://\|\.\(epub\|cbz\|pdf\|ps\|mp4\|mkv\|mpg\|avi\|wmv\|mpg\|ts\|mpeg\|mov\)\(\s\|$\)'
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
    for l:dir in ['personal', 'vib']
      if !empty(glob('~/vcs/' . l:dir . '/notes/.git'))
        execute '!git -C ~/vcs/' . l:dir . '/notes autocommit'
      endif
    endfor
  endif
endfun

fun! Dictionary(word)
  if &spelllang =~# 'nl'
    call system('sensible-browser ' . shellescape('http://woordenlijst.org/#/?bwc=1&q=' . a:word))
  endif
endfun

fun! ExtractCMakeBuildArgs()
  silent CMakeFindBuildDir
  if !exists('b:build_dir') || b:build_dir ==# ''
    return
  endif

  let b:cmake_compile_db = b:build_dir . '/compile_commands.json'

  if filereadable(b:cmake_compile_db)
    if has('nvim')
      let l:json_db = json_decode(readfile(b:cmake_compile_db))
    else
      let l:json_db = json_decode(join(readfile(b:cmake_compile_db), "\n"))
    endif
    let l:current_file = expand('%:p')
    let b:cmake_compile_args = filter(l:json_db, "v:val['file'] == l:current_file")
    if !empty(b:cmake_compile_args)
      let b:cmake_compile_args = filter(split(b:cmake_compile_args[0]['command'], ' '), "v:val =~# '^-[ID]\\|--std'")
    endif

    let b:neomake_cpp_cppcheck_args = ['--quiet', '--language=c++', '--enable=warning', '--project=' . b:cmake_compile_db]
  else
    let b:cmake_compile_args = []
  endif

  if !empty(b:cmake_compile_args)
    call setbufvar('%', '&path', join(map(filter(copy(b:cmake_compile_args), "v:val =~# '^-I'"), 'v:val[2:]'), ','))
  endif

  " TODO: C
  let b:neomake_cpp_gcc_args = ['-fsyntax-only', '-Wall', '-Wextra'] + b:cmake_compile_args
  let b:neomake_cpp_clang_args = ['-fsyntax-only', '-Wall', '-Wextra'] + b:cmake_compile_args
  let b:neomake_cpp_clangcheck_args = ['%:p', '-p', b:build_dir]
  let b:neomake_cpp_clangtidy_args = ['%:p', '-p', b:build_dir]
endfun

fun! CppMan(page) abort
  let l:shell_term = has('nvim') ? '' : ' ++shell'
  execute 'split | terminal' . l:shell_term . ' LESS="$LESS -+F -c" cppman ' . shellescape(a:page)
  doau User ManOpen
endfun

fun! AnsibleDoc(plugin) abort
  let l:shell_term = has('nvim') ? '' : ' ++shell'
  execute 'split | terminal' . l:shell_term . ' LESS="$LESS -+F -c" ansible-doc ' . shellescape(a:plugin)
  doau User ManOpen
endfun
