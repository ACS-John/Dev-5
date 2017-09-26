00010 ! Replace S:\acsPR\hours.br
00020 ! access breakdown of hours by diffenent classifications
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnhours,fnxit,fnerror,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   let fntop("S:\acsPR\hours","Breakdown of Hours")
00080   let fnhours(eno)
00090   goto XIT
00100 ! ______________________________________________________________________
00110 ! <Updateable Region: ERTN>
00120 ERTN: let fnerror(program$,err,line,act$,"NO")
00130   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 ! ______________________________________________________________________
00190 XIT: let fnxit
00200 ! ______________________________________________________________________
