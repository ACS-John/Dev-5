20000 ! formerly S:\acsGL\RestEdit
20020 ! -- Edit Retained Earnings Statement
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fntop,fnxit,fnerror
20100   library 'S:\Core\Library': fnEditInWordProcessor
20120   on error goto ERTN
20140   fntop(program$)
26000   fnEditInWordProcessor(env$('Q')&'\GLmstr\ACGLStmt.h'&env$('cno'),'atlantis',' -n') ! fntext_editor(os_filename$(env$('Q')&'\GLmstr\ACGLStmt.h'&env$('cno')))
26080   goto XIT
28000 ! 
30000 XIT: fnxit
40000 ! <Updateable Region: ERTN>
40020 ERTN: fnerror(program$,err,line,act$,"xit")
40040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
40060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
40080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
40100 ERTN_EXEC_ACT: execute act$ : goto ERTN
40120 ! /region
40140 ! ______________________________________________________________________
