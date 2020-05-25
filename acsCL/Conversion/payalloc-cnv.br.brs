! Replace S:\acsCL\Conversion\PayAlloc-Cnv
! convert PayAlloc file - version 0 to version 0 : _
	! not sure exactally what it does yet
! def ___________________________________________________________________
	autoLibrary
	dim k$*20,gl(3),ta(2),de$*30
	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,holdvn$*8,vcode$*8
	dim cnam$*40,dat$*20,gl(3),sf1$*28,sn$*30,de$*30
	dim rn$*12,de$*30,ta(2),tvn$*8,tr$(5)*35,tr(2),de$*30
	io1$(1)="10,51,N 2,U,N"
	fncno(cno)
	open #1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno]",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\X.H[cno],RecL=164,Replace",internal,outIn,relative
	open #3: "Name=[Q]\CLmstr\PayAlloc.h[cno],SIZE=0,RecL=56,Replace",internal,outIn,relative
L140: read #1,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,N 3,N 6,N 3': vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat gl eof L190
	mat ta=(lrec(3)+1)
	write #3,using 'Form POS 1,C 8,N 3,N 6,N 3,PD 3.2,C 30,PD 3': vn$,mat gl,100,de$,0
	write #2,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12': vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$
	goto L140
L190: close #1: : close #2: : close #3:
	execute "Copy [Q]\CLmstr\X.H[cno]"&' '&"[Q]\CLmstr\PayMstr.h[cno]"
	execute "Index [Q]\CLmstr\PayMstr.h[cno]"&' '&"[Q]\CLmstr\PayIdx1.h[cno] 1 8 Replace DupKeys"
	execute "Index [Q]\CLmstr\PayMstr.h[cno]"&' '&"[Q]\CLmstr\PayIdx2.H[cno] 9 30 Replace DupKeys"
	pr "COMPLETED CONVERTING PAYEE FILE FOR COMPANY #: [cno]"
Xit: fnXit
