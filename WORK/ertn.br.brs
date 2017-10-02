00001 ! <Updateable Region: ERTN>
00002 ERTN: ! 
00003   library 'Core\Library': fnerror
00004   let fnerror(program$,err,line,act$,"xit")
00005   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00006   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00007   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00008 ERTN_EXEC_ACT: execute act$ : goto ERTN
00009 ! /region
