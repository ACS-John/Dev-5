00010 ! Replace S:\acsGL\acprScr
00020 ! after fact payroll screens
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnacprscr
00050 ! ______________________________________________________________________
00060   fntop(program$,cap$="Payroll Screens")
00070   on error goto ERTN
00080 ! ______________________________________________________________________
00090   fnacprscr
00830   goto XIT
00840 ! ______________________________________________________________________
00850 ! <updateable region: ertn>
00860 ERTN: fnerror(program$,err,line,act$,"xit")
00870   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00890   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00900 ERTN_EXEC_ACT: execute act$ : goto ERTN
00910 ! /region
00920 ! ______________________________________________________________________
00930 XIT: fnxit
