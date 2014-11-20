" Vim coloring as default on virtual terminals
" Apparently, Vim uses a white background as basis of the color scheme
" on xterm, although default is dark.
if !has("gui_running")
	set background=dark
endif

" Syntax highlighting
syntax on
" Line numbers
set nu
" Spelling checker
set spell spelllang=nl
" Auto indent
filetype indent on
" File-type plugins
" TODO: activate per file-type?
filetype plugin on
" http://stackoverflow.com/questions/563616/vim-and-ctags-tips-and-tricks
set tags=./.ctags;$HOME
" Location of org-files
let g:org_agenda_files=['~/cloud/config/notes/*.org']

" Arrow keys
" https://gist.github.com/hugoroy/5822226
" http://billodom.com/talks/vim-key-mapping.pdf
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj
noremap <M-Space> <Esc>

" http://jhshi.wordpress.com/2012/11/05/enabledisable-spell-checking-according-to-file-type-in-vim/
" TODO: http://vim.wikia.com/wiki/Keep_your_vimrc_file_clean
au BufNewFile,BufRead,BufEnter *.hs		setlocal nospell
au BufNewFile,BufRead,BufEnter *.pl		setlocal nospell
au BufNewFile,BufRead,BufEnter *.m		setlocal nospell

au BufNewFile,BufRead,BufEnter *.scala.html	setlocal tabstop=4 shiftwidth=4 expandtab
au BufNewFile,BufRead,BufEnter *.java		noremap <F5> :call JavaInsertImport()<CR>
au BufNewFile,BufRead,BufEnter *.java		setlocal tags+=/usr/src/openjdk-7-source/.ctags
au BufNewFile,BufRead,BufEnter *.scala.html	setlocal tags+=/usr/src/openjdk-7-source/.ctags
au BufNewFile,BufRead,BufEnter *.rb		setlocal tabstop=2 shiftwidth=2 expandtab
au BufNewFile,BufRead,BufEnter *.oz		setlocal tabstop=8 shiftwidth=3 expandtab filetype=ruby

autocmd FileType tex				setlocal makeprg=latexmk\ -pdf\ -cd\ '%'

au BufNewFile,BufRead,BufEnter /home/kevin/.gvfs/**	setlocal noswapfile
