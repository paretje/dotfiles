fun neoterm#test#junit#run(scope)
  if a:scope ==# 'file'
    return 'ant test-file -Dtest='. expand('%:r:gs?/?.?:s?^test.??')
  elseif a:scope ==# 'current'
    return "echo \"junit doesn't support running a single test\""
  else
    return 'ant test'
  endif
endfun
