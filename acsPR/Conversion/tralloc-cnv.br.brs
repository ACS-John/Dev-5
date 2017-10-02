00010 ! Replace S:\acsPR\Conversion\TrAlloc-Cnv
00020 ! CONVERT TRANSACTION ALLOCATION FILE
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 L70: pr newpage
00080   close #101: ioerr L90
00090 L90: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT TRANSACTION ALLOCATION FILE",display,outin 
00100   pr fields "10,5,C 60": "ENTER COMPANY NUMBER TO CONVERT OR 0 TO STOP:"
00110   input fields "10,51,Nz 5,UT,N": cno
00120   if cno=0 then goto XIT
00140   execute "Copy "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&" X -79 -n"
00150   execute "Free "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&" -n"
00160   execute "Copy X "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&" -n"
00170   goto L70
00180 ! ______________________________________________________________________
00190 ! <updateable region: ertn>
00200 ERTN: let fnerror(program$,err,line,act$,"xit")
00210   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
00270 XIT: stop 
00280 ! ______________________________________________________________________
