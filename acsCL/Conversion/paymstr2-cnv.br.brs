00010 ! Replace R:\acsCL\Conversion\PayMstr2-CNV
00020 ! it's old
00030 ! ________________________
00040   library 'R:\Core\Library': fncno,fnxit,fnerror
00050   on error goto ERTN
00060 ! ________________________
00070   dim gl(3),ta(2)
00080 ! ________________________
00090 ! fntop
00100   let fncno(cno)
00110 L110: print newpage
00120   close #101: ioerr L130
00130 L130: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT PAYEE MASTER FILE",display,outin 
00140   print fields "10,5,C 60": "COMPANY NUMBER TO CONVERT:"
00150   rinput fields "10,51,N 2,UE,N": cno
00160   if cno=0 then goto XIT
00170 ! 
00180   execute "COPY Q:\CLmstr\PayMstr.h"&str$(cno)&",X -D -164"
00190   execute "FREE Q:\CLmstr\PayMstr.h"&str$(cno)
00200   execute "RENAME X,Q:\CLmstr\PayMstr.h"&str$(cno)
00210   execute "INDEX Q:\CLmstr\PayMstr.h"&str$(cno)&",Q:\CLmstr\PayIndx1.H"&str$(cno)&",1,8,Replace,DupKeys"
00220   execute "INDEX Q:\CLmstr\PayMstr.h"&str$(cno)&",Q:\CLmstr\PayIndx2.H"&str$(cno)&",9,28,Replace,DupKeys"
00230   print fields "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: "&str$(cno)
00240   print fields "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00250   input fields "13,40,C 1,IAE,N": pause$
00260   goto L110
00270 XIT: let fnxit
00280 ! ______________________________
00290 ! <Updateable Region: ERTN>
00300 ERTN: let fnerror(cap$,err,line,act$,"xit")
00310   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00320   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00330   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00340 ERTN_EXEC_ACT: execute act$ : goto ERTN
00350 ! /region
00360 ! ______________________________
