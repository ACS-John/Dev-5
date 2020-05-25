! (C) COPYRIGHT - 2003 - ADVANCED COMPUTER SERVICES, INC.
	autoLibrary
	open #20: "Name=CNO.H"&wsid$,internal,input,relative
	fnTop(program$)
	read #20,using L70,rec=1: cno,cnam$,dat$,cp,nw,process
L70: form pos 1,n 2,c 40,x 20,c 20,pos 89,2*n 1,pos 141,n 1
	form c 9,skip 0
: _
	! 00100  dim vn$*8,nam$*35,ad1$*20,ad2$*20,csz$*20,ss$*11,holdvn$*8,vcode$*8
	dim cnam$*40,dat$*20,adr(2),id1$*25
	dim rn$*12,de$*30,adr(2),tvn$*8
	open #1: "Name=[Q]\GLmstr\GL1099.h[cno],KFName=[Q]\GLmstr\GL109IDX.h[cno]",internal,outIn,keyed
L140: form pos 1,c 8,c 35,3*c 20,pd 5.2,n 2,c 11,2*pd 3
L150: read #1,using L140: vn$,nam$,ad1$,ad2$,csz$ eof L210
	x=val(vn$) conv L190
	if x<1 or x>99999999 then goto L190
	goto L150
L190: delete #1:
	goto L150
L210: fnXit
