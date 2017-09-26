20000 ! Replace S:\acsGL\RestEdit
20020 ! -- Edit Retained Earnings Statement
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fntop,fnxit,fnerror
20080   library 'S:\Core\Library': fntext_editor
20100   library 'S:\Core\Library': fnget_atlantis
20120   on error goto ERTN
20140   let fntop(program$,cap$="Edit Retained Earnings Statement")
20160   dim cap$*128
26000   ! fntext_editor(os_filename$(env$('Q')&'\GLmstr\ACGLStmt.h'&env$('cno')))
26020   dim atlantis$*256
26040   let fnget_atlantis(atlantis$)
26060   execute 'SY -w '&atlantis$&' "'&os_filename$(env$('Q')&'\GLmstr\ACGLStmt.h'&env$('cno'))&'" -n'
26080   goto XIT
28000 ! 
30000 XIT: let fnxit
40000 ! <Updateable Region: ERTN>
40020 ERTN: let fnerror(program$,err,line,act$,"xit")
40040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
40060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
40080   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
40100 ERTN_EXEC_ACT: execute act$ : goto ERTN
40120 ! /region
40140 ! ______________________________________________________________________
