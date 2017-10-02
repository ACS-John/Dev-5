00010 ! Replace S:\acsCL\Receipt
00020 ! Standard receipt file
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnaddreceipt,fntop,fncno,fndat,fnxit,fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080 ! ______________________________________________________________________
00090   let fntop(program$, cap$="Receipt")
00100   let fnaddreceipt
00110   goto XIT
00120 ! ______________________________________________________________________
00130 XIT: let fnxit
00140 ! ______________________________________________________________________
00150 ! <Updateable Region: ERTN>
00160 ERTN: let fnerror(program$,err,line,act$,"xit")
00170   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00190   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00200 ERTN_EXEC_ACT: execute act$ : goto ERTN
00210 ! /region
00220 ! ______________________________________________________________________
