00020 ! 
00030   dim gl(3),ta(2)
00040 L40: pr newpage
00042   close #101: ioerr L44
00044 L44: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT PAYEE MASTER FILE",display,outin 
00050   pr f "10,5,C 60": "ENTER COMPANY NUMBER TO CONVERT OR 0 TO STOP:"
00060   input fields "10,51,N 2,UE,N": cno
00070   if cno=0 then stop  ! CHAIN "RABLDSCR/CCRA1"
00080 ! 
00090   open #1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno'),internal,outin,keyed 
00100   open #2: "Name="&env$('Q')&"\CLmstr\PayAlloc.h"&env$('cno')&",SIZE=0,RecL=56,Replace",internal,outin,relative 
00110 L110: read #1,using L120: p$,mat gl eof L220
00120 L120: form pos 1,c 8,pos 147,n 3,n 6,n 3
00130   mat ta=(0)
00140 ! IF SUM(GL)=0 THEN GOTO 190
00150   lr2=lrec(2)+1
00160   write #2,using L170,rec=lr2: p$,mat gl,100,"",0
00170 L170: form pos 1,c 8,n 3,n 6,n 3,pd 3.2,c 30,pd 3
00180   mat ta=(lr2)
00190   rewrite #1,using L200: mat ta
00200 L200: form pos 147,2*pd 3
00210   goto L110
00220 L220: close #1: 
00230   close #2: 
00240 ! EXECUTE "Copy "&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",X -D -152"
00250 ! EXECUTE "Free "&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')
00260 ! EXECUTE "Rename X "&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')
00270 ! EXECUTE "Index "&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&","&env$('Q')&"\CLmstr\PayIndx1.H"&env$('cno')&",1,8,Replace,DupKeys"
00280 ! EXECUTE "Index "&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&","&env$('Q')&"\CLmstr\PayIndx2.H"&env$('cno')&",9,28,Replace,DupKeys"
00290   pr f "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: "&env$('cno')
00300   pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00310   input fields "13,40,C 1,IAE,N": pause$
00320   goto L40
