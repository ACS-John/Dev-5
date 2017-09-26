00010 ! Replace S:\acsPR\Conversion\PayMstr-Cnv
00020 ! CONVERT PAYEE MASTER FILE
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim gl(3),ta(2)
00080 ! ______________________________________________________________________
00090 L90: print newpage
00100   close #101: ioerr L110
00110 L110: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT PAYEE MASTER FILE",display,outin 
00120   print fields "10,5,C 60": "ENTER COMPANY NUMBER TO CONVERT OR 0 TO STOP:"
00130   input fields "10,51,N 2,UE,N": cno
00140   if cno=0 then goto XIT ! CHAIN "RABLDSCR/CCRA1"
00150 ! 
00160   open #1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno),internal,outin,keyed 
00170   open #2: "Name="&env$('Q')&"\CLmstr\PayAlloc.h"&str$(cno)&",SIZE=0,RecL=56,Replace",internal,outin,relative 
00180 L180: read #1,using L190: p$,mat gl eof L290
00190 L190: form pos 1,c 8,pos 147,n 3,n 6,n 3
00200   mat ta=(0)
00210 ! IF SUM(GL)=0 THEN GOTO 190
00220   let lr2=lrec(2)+1
00230   write #2,using L240,rec=lr2: p$,mat gl,100,"",0
00240 L240: form pos 1,c 8,n 3,n 6,n 3,pd 3.2,c 30,pd 3
00250   mat ta=(lr2)
00260   rewrite #1,using L270: mat ta
00270 L270: form pos 147,2*pd 3
00280   goto L180
00290 L290: close #1: 
00300   close #2: 
00310 ! EXECUTE "Copy "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",X -D -152"
00320 ! EXECUTE "Free "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00330 ! EXECUTE "Rename X "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00340 ! EXECUTE "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&","&env$('Q')&"\CLmstr\PayIndx1.H"&str$(cno)&",1,8,Replace,DupKeys"
00350 ! EXECUTE "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&","&env$('Q')&"\CLmstr\PayIndx2.H"&str$(cno)&",9,28,Replace,DupKeys"
00360   print fields "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: "&str$(cno)
00370   print fields "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00380   input fields "13,40,C 1,IAE,N": pause$
00390   goto L90
00400 ! ______________________________________________________________________
00410 ! <updateable region: ertn>
00420 ERTN: let fnerror(program$,err,line,act$,"xit")
00430   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 ! ______________________________________________________________________
00490 XIT: stop 
00500 ! ______________________________________________________________________
