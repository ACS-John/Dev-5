! Replace S:\acsPR\Conversion\PayMstr-Cnv
! CONVERT PAYEE MASTER FILE
 
	autoLibrary
	on error goto Ertn
 
	dim gl(3),ta(2)
 
L90: pr newpage
	close #101: ioerr L110
L110: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT PAYEE MASTER FILE",display,outIn
	pr f "10,5,C 60": "ENTER COMPANY NUMBER TO CONVERT OR 0 TO STOP:"
	input fields "10,51,N 2,UE,N": cno
	if cno=0 then goto Xit ! CHAIN "RABLDSCR/CCRA1"
 
	open #1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\PayAlloc.h[cno],SIZE=0,RecL=56,Replace",internal,outIn,relative
L180: read #1,using L190: p$,mat gl eof L290
L190: form pos 1,c 8,pos 147,n 3,n 6,n 3
	mat ta=(0)
! IF SUM(GL)=0 THEN GOTO 190
	lr2=lrec(2)+1
	write #2,using L240,rec=lr2: p$,mat gl,100,"",0
L240: form pos 1,c 8,n 3,n 6,n 3,pd 3.2,c 30,pd 3
	mat ta=(lr2)
	rewrite #1,using L270: mat ta
L270: form pos 147,2*pd 3
	goto L180
L290: close #1:
	close #2:
! EXECUTE "Copy [Q]\CLmstr\PayMstr.h[cno],X -D -152"
! EXECUTE "Free [Q]\CLmstr\PayMstr.h[cno]"
! EXECUTE "Rename X [Q]\CLmstr\PayMstr.h[cno]"
! EXECUTE "Index [Q]\CLmstr\PayMstr.h[cno],[Q]\CLmstr\PayIndx1.H[cno],1,8,Replace,DupKeys"
! EXECUTE "Index [Q]\CLmstr\PayMstr.h[cno],[Q]\CLmstr\PayIndx2.H[cno],9,28,Replace,DupKeys"
	pr f "12,5,C 60": "COMPLETED CONVERTING PAYMSTR FILE FOR COMPANY #: [cno]"
	pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
	input fields "13,40,C 1,IAE,N": pause$
	goto L90
 
include: ertn
 
Xit: stop
 
