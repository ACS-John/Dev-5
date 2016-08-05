00010 ! Replace R:\acsCL\Conversion\All-cnv
00020 ! don't know, but it is old!
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnxit,fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k$*20,gl(3),ta(2),de$*30
00080 ! ______________________________________________________________________
00090 ! fntop
00100   let fncno(cno)
00110   let io1$(1)="10,51,N 2,U,N"
00120   let io1$(2)="12,52,N 6,U,N"
00130 L130: print newpage
00140   close #101: ioerr L150
00150 L150: open #101: "SROW=9,SCOL=4,EROW=13,ECOL=65,BORDER=DR,CAPTION=CONVERT UNPAID INVOICE FILE",display,outin 
00160   print fields "10,5,C 60": "COMPANY NUMBER TO CONVERT:"
00170   print fields "12,5,C 60": "PREVIOUS ENDING DATE FOR POST TO GL:"
00180   print fields "14,20,Cc 35,B,5": "F5 to stop"
00190   rinput fields mat io1$: cno,lpd
00200   if cmdkey=5 or cno=0 then goto XIT
00210 ! 
00220   execute "COPY Q:\CLmstr\PayTrans.h"&str$(cno)&",X -D"
00230   execute "COPY X Q:\CLmstr\PayTrans.h"&str$(cno)&"  -112"
00240   open #1: "Name=Q:\CLmstr\PayTrans.h"&str$(cno),internal,outin,relative 
00250   for j=1 to lrec(1)
00260     read #1,using 'Form POS 96,N 1,N 6',rec=j: pcde
00270     if pcde=0 then let pdte=0 else let pdte=lpd
00280     rewrite #1,using 'Form POS 96,N 1,N 6',rec=j: pcde,pdte
00290   next j
00300   close #1: 
00310   execute "INDEX Q:\CLmstr\PayTrans.h"&str$(cno)&",Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",1,20,Replace,DupKeys"
00320   execute "COPY Q:\CLmstr\PayMstr.h"&str$(cno)&",X -D -164"
00330   execute "FREE Q:\CLmstr\PayMstr.h"&str$(cno)
00340   execute "RENAME X,Q:\CLmstr\PayMstr.h"&str$(cno)
00350   execute "INDEX Q:\CLmstr\PayMstr.h"&str$(cno)&",Q:\CLmstr\PayIndx1.H"&str$(cno)&",1,8,Replace,DupKeys"
00360   execute "INDEX Q:\CLmstr\PayMstr.h"&str$(cno)&",Q:\CLmstr\PayIndx2.H"&str$(cno)&",9,28,Replace,DupKeys"
00370   execute "COPY Q:\CLmstr\TRALLOC.h"&str$(cno)&",X -D -80"
00380   execute "FREE Q:\CLmstr\TRALLOC.h"&str$(cno)
00390   execute "RENAME X,Q:\CLmstr\TRALLOC.h"&str$(cno)
00400   print fields "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: "&str$(cno)
00410   print fields "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00420   input fields "13,40,C 1,IAE,N": pause$
00430   goto L130
00440 XIT: let fnxit
00450 ! <Updateable Region: ERTN>
00460 ERTN: let fnerror(cap$,err,line,act$,"xit")
00470   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00480   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00490   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00500 ERTN_EXEC_ACT: execute act$ : goto ERTN
00510 ! /region
00520 ! ______________________________________________________________________
