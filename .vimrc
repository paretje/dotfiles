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
au BufNewFile,BufRead,BufEnter *.hs	set nospell
