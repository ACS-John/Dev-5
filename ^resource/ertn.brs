! r: doNotInsert
library 'S:\Core\Library': fnOpenFile
pr 'this clip is not intended to be compiled directly nor run directly'
pr 'this clip replaces "include: fn_open" when processed with lexi'
end
! /r doNotInsert
! <updateable region: ertn>
ERTN: fnerror(program$,err,line,act$,"xit")
  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
  if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! </updateable region: ertn>