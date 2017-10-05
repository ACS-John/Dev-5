00010 ! Replace S:\acsCL\Conversion\All-cnv
00020 ! don't know, but it is old!
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k$*20,gl(3),ta(2),de$*30
00080 ! ______________________________________________________________________
00090 ! fntop
00100   fncno(cno)
00110   io1$(1)="10,51,N 2,U,N"
00120   io1$(2)="12,52,N 6,U,N"
00130 L130: pr newpage
00140   close #101: ioerr L150
00150 L150: open #101: "SROW=9,SCOL=4,EROW=13,ECOL=65,BORDER=DR,CAPTION=CONVERT UNPAID INVOICE FILE",display,outin 
00160   pr f "10,5,C 60": "COMPANY NUMBER TO CONVERT:"
00170   pr f "12,5,C 60": "PREVIOUS ENDING DATE FOR POST TO GL:"
00180   pr f "14,20,Cc 35,B,5": "F5 to stop"
00190   rinput fields mat io1$: cno,lpd
00200   if cmdkey=5 or cno=0 then goto XIT
00210 ! 
00220   execute "Copy "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",X -D"
00230   execute "COPY X "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&"  -112"
00240   open #1: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno),internal,outin,relative 
00250   for j=1 to lrec(1)
00260     read #1,using 'Form POS 96,N 1,N 6',rec=j: pcde
00270     if pcde=0 then pdte=0 else pdte=lpd
00280     rewrite #1,using 'Form POS 96,N 1,N 6',rec=j: pcde,pdte
00290   next j
00300   close #1: 
00310   execute "Index "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&","&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno)&",1,20,Replace,DupKeys"
00320   execute "Copy "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",X -D -164"
00330   execute "Free "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00340   execute "RENAME X,"&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00350   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&","&env$('Q')&"\CLmstr\PayIndx1.H"&str$(cno)&",1,8,Replace,DupKeys"
00360   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&","&env$('Q')&"\CLmstr\PayIndx2.H"&str$(cno)&",9,28,Replace,DupKeys"
00370   execute "Copy "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&",X -D -80"
00380   execute "Free "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)
00390   execute "RENAME X,"&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)
00400   pr f "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: "&str$(cno)
00410   pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00420   input fields "13,40,C 1,IAE,N": pause$
00430   goto L130
00440 XIT: fnxit
00450 ! <Updateable Region: ERTN>
00460 ERTN: fnerror(program$,err,line,act$,"xit")
00470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00490   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00500 ERTN_EXEC_ACT: execute act$ : goto ERTN
00510 ! /region
00520 ! ______________________________________________________________________
