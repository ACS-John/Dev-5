! <updateable region: ertn>
ERTN: let fnerror(program$,err,line,act$,"xit")
  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
  if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
  print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! </updateable region: ertn>
