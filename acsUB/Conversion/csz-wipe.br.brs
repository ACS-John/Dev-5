00010 ! Replace S:\acsUB\conversion\csz-wipe
00020 ! this program wipes CSZs from the UB customer file and replaces them with whatever you tell it to
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim csz$*30
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100   pr newpage
00110 L110: pr f "8,20,C 30,R,N": "Mask Cisty State Zip"
00120   pr f "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
00130   pr f "11,1,Cr 38": "New City State and Zip:"
00140   io1$(1)="10,40,N 2,UT,N" : io1$(2)="11,40,C 30,UT,N"
00150 L150: rinput fields mat io1$: cno,csz$ conv L150
00160   if cno=0 or cmdkey=5 or cmdkey=99 then goto XIT
00170   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubindex.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   for j=1 to lrec(1)
00190     rewrite #1,using "Form Pos 101,c 30",rec=j: csz$ norec L210
00200     pr f "1,1,N 10,R,N": j
00210 L210: next j
00220   goto DONE
00230 ! ______________________________________________________________________
00240 DONE: close #1: 
00250   pr "company number "&str$(cno)&" completed successfully"
00260   goto L110
00270 XIT: stop 
00280 ! ______________________________________________________________________
00290 ! <Updateable Region: ERTN>
00300 ERTN: fnerror(program$,err,line,act$,"xit")
00310   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00330   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00340 ERTN_EXEC_ACT: execute act$ : goto ERTN
00350 ! /region
00360 ! ______________________________________________________________________
