00010 ! Replace S:\acsPR\Conversion\RPmstr-Cnv
00020 !  CONVERT PAYROLL RPmstr file to RLN=196 !:
        ! from RLN=178 or anything else, i think
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 L70: pr newpage
00080   pr f "8,20,C 30,R,N": "  CONVERT PAYROLL FILES"
00090   pr f "10,15,C 50": "Company Number (0 to stop):"
00100 L100: input fields "10,55,N 5,UE,N": cno conv L100
00110   if cno=0 then goto XIT
00120 ! 
00130   execute "Copy "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&" X -196 -n"
00140   execute "Copy X "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&" -D -n"
00150   execute "Index "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&' '&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&" 1 8 Replace DupKeys -n"
00160   execute "Index "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&' '&env$('Q')&"\PRmstr\RPINDX2.h"&str$(cno)&" 9 30 Replace DupKeys -n"
00170   goto L70
00180 ! ______________________________________________________________________
00190 ! <updateable region: ertn>
00200 ERTN: fnerror(program$,err,line,act$,"xit")
00210   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00230   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00240 ERTN_EXEC_ACT: execute act$ : goto ERTN
00250 ! /region
00260 ! ______________________________________________________________________
00270 XIT: stop 
00280 ! ______________________________________________________________________
