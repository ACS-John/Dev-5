00010 ! Replace S:\acsPR\Conversion\prCkHist-Cnv
00020 ! CONVERT FOR CC CHG
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00150   pr newpage
00160   pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
00170 L170: input fields "10,55,N 2,UE,N": cno conv L170
00180   if cno=0 then goto XIT
00190 ! 
00200   open #4: "Name="&env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno)&",RecL=150,USE",internal,outin 
00210 L210: read #4,using L220: d1 eof L310,conv L280
00220 L220: form pos 9,n 6
00230   let d1=fndate_mmddyy_to_ccyymmdd(d1) ! LET D1=19000000+FNCD(D1)
00240   rewrite #4,using 'form pos 9,pd 6': d1
00260   goto L210
00270 ! 
00280 L280: read #4,using 'form pos 9,pd 6': d1 eof L310
00290   goto L210
00300 ! ______________________________________________________________________
00310 L310: close #4: 
00320   execute "Index "&env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno)&' '&env$('Q')&"\PRmstr\PRCKINDX.h"&str$(cno)&" 1 14 Replace DupKeys -n"
00330 XIT: stop 
00340 ! ______________________________________________________________________
00350 ! <updateable region: ertn>
00360 ERTN: let fnerror(program$,err,line,act$,"xit")
00370   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00380   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00390   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00400 ERTN_EXEC_ACT: execute act$ : goto ERTN
00410 ! /region
00420 ! ______________________________________________________________________
