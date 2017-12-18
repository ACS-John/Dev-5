00010 ! Replace S:\acsPR\Conversion\CkHist-Cnv
00020 ! BUILD PAYROLL CHECK HISTORY FILE
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 L70: pr newpage
00080   pr f "08,08,C 34,R,N": " BUILD PAYROLL CHECK HISTORY FILE"
00090   pr f "10,5,C 60": "ENTER COMPANY NUMBER TO BE CONVERTED:"
00100   pr f "12,15,C 16,B,5": "Cancel (F5)"
00110 L110: input fields "10,43,N 5,UE,N": cno conv L110
00120   if cmdkey=5 then goto XIT
00130 ! 
00140   open #4: "Name="&env$('Q')&"\PRmstr\PRCkHist.h"&env$('cno')&",SIZE=0,RecL=150,Replace",internal,output 
00150   close #4: 
00160   execute "Index "&env$('Q')&"\PRmstr\PRCkHist.h"&env$('cno')&' '&env$('Q')&"\PRmstr\PRCKINDX.h"&env$('cno')&" 1 14 Replace DupKeys -n"
00170   goto L70
00180 ! ______________________________________________________________________
00190 ! <Updateable Region: ERTN>
00200 ERTN: fnerror(program$,err,line,act$,"xit")
00210   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
00270 XIT: stop 
00280 ! ______________________________________________________________________
