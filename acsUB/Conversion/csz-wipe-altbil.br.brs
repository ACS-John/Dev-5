00010 ! Replace S:\acsUB\conversion\csz-wipe-altbil
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
00150   csz$="Townsville, AR  55555"
00160 L160: rinput fields mat io1$: cno,csz$ conv L160
00170   if cno=0 or cmdkey=5 or cmdkey=99 then goto XIT
00180   open #1: "Name=[Q]\UBmstr\ubadrbil.h[cno]",internal,outIn,relative 
00190   for j=1 to lrec(1)
00200     rewrite #1,using "Form Pos 101,c 30",rec=j: csz$ noRec L220
00210     pr f "1,1,N 10,R,N": j
00220 L220: next j
00230   goto DONE
00240 ! ______________________________________________________________________
00250 DONE: close #1: 
00260   pr "company number [cno] completed successfully"
00270   goto L110
00280 XIT: stop 
00290 ! ______________________________________________________________________
00300 ! <Updateable Region: ERTN>
00310 ERTN: fnerror(program$,err,line,act$,"xit")
00320   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00330   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00340   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00350 ERTN_EXEC_ACT: execute act$ : goto ERTN
00360 ! /region
00370 ! ______________________________________________________________________
