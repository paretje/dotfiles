if exists('g:autoloaded_dotoo_agenda_view_books')
  finish
endif
let g:autoloaded_dotoo_agenda_views_books = 1

let s:notes_deadlines = {} "{{{1
function! s:build_notes(dotoos, ...)
  let force = a:0 ? a:1 : 0
  let filters_header = dotoo#agenda#filters_header()
  if force || empty(s:notes_deadlines)
    let s:notes_deadlines = {}
    for dotoo in values(a:dotoos)
      let headlines = dotoo.filter("v:val.tags =~? 'book'")
      call dotoo#agenda#apply_filters(headlines)
      let s:notes_deadlines[dotoo.file] = headlines
    endfor
  endif
  let notes = []
  call dotoo#agenda#headlines([])
  for file in keys(s:notes_deadlines)
    let headlines = s:notes_deadlines[file]
    call dotoo#agenda#headlines(headlines, 1)
    for headline in headlines
      let note = printf('%s %10s: %-70s %s', '',
            \ fnamemodify(file, ':h:t'),
            \ headline.todo_title(),
            \ headline.tags)
      call add(notes, note)
    endfor
  endfor
  if empty(notes)
    call add(notes, printf('%2s %s', '', 'No books!'))
  endif
  let header = []
  call add(header, 'Books')
  if !empty(filters_header) | call add(header, filters_header) | endif
  call insert(notes, join(header, ', '))
  return notes
endfunction

let s:view_name = 'books'
let s:notes_view = {}
function! s:notes_view.content(dotoos, ...) dict
  let force = a:0 ? a:1 : 0
  return s:build_notes(a:dotoos, force)
endfunction

function! s:notes_view.setup()
  " dirty hack to avoid always loading library
  if index(g:dotoo#agenda#files, '~/vcs/personal/library/**/.metadata.org') == -1
    call add(g:dotoo#agenda#files, '~/vcs/personal/library/**/.metadata.org')
    normal r
  endif
endfunction

call dotoo#agenda#register_view(s:view_name, s:notes_view)
