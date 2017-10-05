00010 ! Replace S:\acsPR\Conversion\prAll-Cnv
00011 ! not sure, but with a name like prAll, I bet it does alot.
00015 ! ______________________________________________________________________
00016   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fndate_mmddyy_to_ccyymmdd
00017   on error goto ERTN
00020 ! ______________________________________________________________________
00026 ! ______________________________________________________________________
00032   fncno(cno)
00040 L40: pr newpage
00050   pr f "8,20,C 30,R,N": "  CONVERT PAYROLL FILES"
00060   pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
00070 L70: rinput fields "10,55,N 5,UT,N": cno conv L70
00080   if cno=0 then goto XIT
00090 ! 
00100   execute "Copy "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&" X -196 -n"
00110   execute "Copy X "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&" -D -n"
00120   execute "Index "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&' '&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&" 1 8 Replace DupKeys -n"
00130   execute "Index "&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&' '&env$('Q')&"\PRmstr\RPINDX2.h"&str$(cno)&" 9 30 Replace DupKeys -n"
00140   open #4: "Name="&env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno)&",RecL=150,USE",internal,outin 
00150 L150: read #4,using L160: d1 eof L230,conv L210
00160 L160: form pos 9,n 6
00170   d1=fndate_mmddyy_to_ccyymmdd(d1) ! d1=19000000+FNCD(D1)
00180   rewrite #4,using L190: d1
00190 L190: form pos 9,pd 6
00200   goto L150
00210 L210: read #4,using L190: d1 eof L230
00220   goto L150
00230 L230: close #4: 
00240   execute "Index "&env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno)&' '&env$('Q')&"\PRmstr\PRCKINDX.h"&str$(cno)&" 1 14 Replace DupKeys"
00250   open #1: "Name="&env$('Q')&"\PRmstr\prCode.h"&str$(cno)&",SIZE=0,RecL=512,Replace",internal,output,relative 
00260   write #1,using L270,rec=1: 0,0,0,0
00270 L270: form pos 1,n 1,n 2,n 1,n 5
00280   close #1: 
00290   goto L40
01800 ! ______________________________________________________________________
01801 ! <updateable region: ertn>
01802 ERTN: fnerror(program$,err,line,act$,"xit")
01803   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01804   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01805   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01806 ERTN_EXEC_ACT: execute act$ : goto ERTN
01807 ! /region
01808 ! ______________________________________________________________________
01900 XIT: stop 
01908 ! ______________________________________________________________________
