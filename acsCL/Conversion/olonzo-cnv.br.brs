00010 ! Replace S:\acsCL\Conversion\OLONZO-CNV
00020 ! not sure, but it's old!
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fncno,fnerror,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k$*20,gl(3),ta(2),de$*30
00080 ! ______________________________________________________________________
00090 ! fntop
00100   let io1$(1)="10,51,N 2,U,N"
00110   let io1$(2)="12,52,N 6,U,N"
00120 L120: pr newpage
00130   close #101: ioerr L140
00140 L140: open #101: "SROW=9,SCOL=4,EROW=13,ECOL=65,BORDER=DR,CAPTION=CONVERT UNPAID INVOICE FILE",display,outin 
00150   pr f "10,5,C 60": "COMPANY NUMBER TO CONVERT:"
00160   pr f "12,5,C 60": "PREVIOUS ENDING DATE FOR POST TO GL:"
00170   input fields mat io1$: cno,lpd
00180   if cno=0 or cmdkey=5 then goto XIT
00190 ! 
00200   execute "Copy "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",X -D"
00210   execute "Copy X "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&"  -112"
00220   open #1: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno),internal,outin,relative 
00230   for j=1 to lrec(1)
00240     read #1,using 'Form POS 96,N 1,N 6',rec=j: pcde
00250     if pcde=0 then let pdte=0 else let pdte=lpd
00260     rewrite #1,using 'Form POS 96,N 1,N 6',rec=j: pcde,pdte
00270   next j
00280   close #1: 
00290   execute "Index "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&","&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno)&",1,20,Replace,DupKeys"
00300   execute "Copy "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",X -D -164"
00310   execute "Free "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00320   execute "Rename X "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00330   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&","&env$('Q')&"\CLmstr\PayIndx1.H"&str$(cno)&",1,8,Replace,DupKeys"
00340   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&","&env$('Q')&"\CLmstr\PayIndx2.H"&str$(cno)&",9,28,Replace,DupKeys"
00350   pr f "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: "&str$(cno)
00360   pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00370   input fields "13,40,C 1,IAE,N": pause$
00380   goto L120
00390 ! ______________________________________________________________________
00400 ! <Updateable Region: ERTN>
00410 ERTN: let fnerror(program$,err,line,act$,"xit")
00420   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 ! ______________________________________________________________________
00480 XIT: let fnxit
00490 ! ______________________________________________________________________
