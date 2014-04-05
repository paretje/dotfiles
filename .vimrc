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
set spell
" Auto indent
set ai
" http://stackoverflow.com/questions/563616/vim-and-ctags-tips-and-tricks
set tags=./.ctags;$HOME

" Arrow keys
" https://gist.github.com/hugoroy/5822226
" http://billodom.com/talks/vim-key-mapping.pdf
nnoremap <up> gk
nnoremap <down> gj
inoremap <up> <C-O>gk
inoremap <down> <C-O>gj
vnoremap <up> gk
vnoremap <down> gj

" http://jhshi.wordpress.com/2012/11/05/enabledisable-spell-checking-according-to-file-type-in-vim/
au BufNewFile,BufRead,BufEnter *.hs		setlocal nospell
au BufNewFile,BufRead,BufEnter *.pl		setlocal nospell
au BufNewFile,BufRead,BufEnter *.m		setlocal nospell

au BufNewFile,BufRead,BufEnter *.tex		setlocal spell spelllang=nl

au BufNewFile,BufRead,BufEnter *.scala.html	setlocal tabstop=2 shiftwidth=2 expandtab
au BufNewFile,BufRead,BufEnter *.java		noremap <F5> :call JavaInsertImport()<CR>
au BufNewFile,BufRead,BufEnter *.java		setlocal tags+=/usr/src/openjdk-7-source/.ctags
au BufNewFile,BufRead,BufEnter /home/kevin/.gvfs/**	setlocal noswapfile
