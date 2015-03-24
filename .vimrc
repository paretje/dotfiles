" Enable pathogen
execute pathogen#infect()

" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
else
	" Always show tab-bar in GVIM
	set showtabline=2
endif

" Syntax highlighting
syntax on
" Line numbers
set nu
" Spelling checker
set spell spelllang=nl
" Auto indent
set autoindent
filetype indent on
" File-type plugins
" TODO: activate per file-type?
filetype plugin on
" Search case-insensitive, unless caps are used
" TODO: enable this only for searches, not when replacing?
set ignorecase
set smartcase
" Highlight searches
set hlsearch
" Set ctags options
set tags=./tags;$HOME/vcs
" Location of org-files
let g:org_agenda_files=['~/cloud/config/notes/*.org']
command TODO tabnew ~/cloud/config/notes/Tasks.org
" Max number of tabs
set tabpagemax=32
" Disable exit code checks
let g:syntastic_exit_checks=0
" Set YouCompleteMe options
let g:ycm_autoclose_preview_window_after_insertion=1
let g:ycm_seed_identifiers_with_syntax=1

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

autocmd FileType haskell	setlocal nospell
autocmd FileType prolog		setlocal nospell
autocmd FileType matlab		setlocal nospell

autocmd FileType gitcommit	setlocal spelllang=en

autocmd FileType ruby		setlocal softtabstop=2 shiftwidth=2 expandtab
autocmd FileType r		setlocal softtabstop=2 shiftwidth=2 expandtab
autocmd FileType matlab		setlocal softtabstop=4 shiftwidth=4 expandtab
autocmd FileType java		setlocal softtabstop=4 shiftwidth=4 expandtab

autocmd FileType java		setlocal tags+=/usr/lib/jvm/openjdk-8/tags
autocmd FileType java		setlocal omnifunc=javacomplete#Complete

autocmd FileType tex		setlocal makeprg=latexmk\ -pdf\ -cd\ '%'

autocmd FileType java		noremap <F5> :call JavaInsertImport()<CR>
autocmd FileType org		inoremap <C-L> <Esc>:OrgCheckBoxNewBelow<CR>
autocmd FileType python		noremap r :BikeRename<CR>

au FileType tex,text,bbcode	setlocal linebreak
au FileType mail,gitcommit	setlocal formatoptions+=a

" TODO: does this really work?
au BufNewFile,BufRead /run/user/*/gvfs/**	setlocal directory=/tmp backupdir=/tmp
au BufRead ~/.mozilla/firefox/*/itsalltext/blog.online-urbanus.be*	setlocal ft=bbcode

let g:nailgun_port='2113'
let g:javacomplete_ng='ng-nailgun'
call javacomplete#AddClassPath('/home/kevin/vcs/ugent/hersch2/Unification/build/classes')
call javacomplete#AddSourcePath('/home/kevin/vcs/ugent/hersch2/Unification/src')
