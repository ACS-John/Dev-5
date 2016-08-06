00010 ! Replace R:\acsCL\Payee
00020 ! Payee File
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnaddpayee,fntop,fncno,fndat,fnxit,fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080 ! ______________________________________________________________________
00090   let fntop(program$, cap$="Maintain Payee Records")
00100   let fnaddpayee
00110   goto XIT
00120 ! ______________________________________________________________________
00130 XIT: let fnxit
00140 ! ______________________________________________________________________
00150 ! <Updateable Region: ERTN>
00160 ERTN: let fnerror(cap$,err,line,act$,"xit")
00170   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00190   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00200 ERTN_EXEC_ACT: execute act$ : goto ERTN
00210 ! /region
00220 ! ______________________________________________________________________
