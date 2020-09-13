! test\len
  dim x$*80
  x$="asdf               "&chr$(0)&"                "
  pr len(x$)
  x$=trim$(x$)
  pr len(x$)
  pause 
