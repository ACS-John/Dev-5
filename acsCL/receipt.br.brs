00010 ! Replace S:\acsCL\Receipt
00020 ! Standard receipt file
00030 !
00040   library 'S:\Core\Library': fnaddreceipt,fntop,fncno,fndat,fnxit,fnerror
00050   on error goto Ertn
00060 !
00070   dim cap$*128
00080 !
00090   fntop(program$, cap$="Receipt")
00100   fnaddreceipt
00110   goto XIT
00120 !
00130 XIT: fnxit
00140 !
00150 ! <Updateable Region: ERTN>
00160 ERTN: fnerror(program$,err,line,act$,"xit")
00170   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00180   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00190   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00200 ERTN_EXEC_ACT: execute act$ : goto ERTN
00210 ! /region
00220 !
