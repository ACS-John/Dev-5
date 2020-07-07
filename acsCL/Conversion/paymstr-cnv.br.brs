! Replace S:\acsCL\conversion\paymstr-cnv
! it's old

	autoLibrary
	on error goto Ertn

! fnTop
	pr newpage
	close #101: ioerr L110
L110: open #101: "SROW=11,SCOL=20,EROW=13,ECOL=63,BORDER=DR,CAPTION=CHANGE PAYEE NUMBERS",display,outIn
	pr f "12,22,C 40": "ENTER COMPANY NUMBER TO BE CHANGE:"
	pr f "14,32,C 16,B,5": "PRESS F5 TO STOP"
L140: rinput fields "12,57,N 2,UE,N": cno conv L140
	if cmdkey=5 then goto Xit
 
	open #1: "Name=[Q]\CLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\PayAlloc.h[cno]",internal,outIn,relative
	open #3: "Name=[Q]\CLmstr\TRMSTR.H[cno],KFName=[Q]\CLmstr\TRIDX2.H[cno]",internal,outIn,keyed
! OPEN #4: "Name=[Q]\CLmstr\TRALLOC.h[cno]",INTERNAL,outIn,RELATIVE
	open #6: "Name=[Q]\CLmstr\IvPaid.H[cno],KFName=[Q]\CLmstr\IVINDEX.H[cno]",internal,outIn,keyed
	pr f "14,32,C 16,RB,N": "  IN PROCESS"
	nk=10
L240: read #1,using L250: k1$,ad1 eof END1
L250: form pos 1,c 8,pos 147,pd 3
	k2$=lpad$(str$(nk),8)
	rewrite #1,using L250: k2$
	nk=nk+5
	ad2=ad1
L300: if ad2=0 then goto END2
	read #2,using L320,rec=ad2: p$,nta
L320: form pos 1,c 8,pos 54,pd 3
	rewrite #2,using L320,rec=ad2: k2$
	ad2=nta : goto L300
END2: restore #3,search>=k1$: nokey END3
L360: read #3,using L370: p$ eof END3
L370: form pos 28,c 8
	if k1$><p$ then goto END3
	rewrite #3,using L370: k2$
	goto L360
END3: restore #6,search>=k1$: nokey END6
L420: read #6,using L430: p$ eof END6
L430: form pos 1,c 8
	if p$><k1$ then goto END6
	rewrite #6,using L430: k2$
	goto L420
END6: goto L240
END1: !
	close #1:
	close #2:
	close #3:
	close #6:
	execute "Index [Q]\CLmstr\PayMstr.h[cno]"&' '&"[Q]\CLmstr\PayIdx1.h[cno] 1 8 Replace DupKeys"
	execute "Index [Q]\CLmstr\PayMstr.h[cno]"&' '&"[Q]\CLmstr\PayIdx2.H[cno] 9 30 Replace DupKeys"
	execute "Index [Q]\CLmstr\IvPaid.H[cno],[Q]\CLmstr\IVINDEX.H[cno],1,20,Replace,DupKeys"
	execute "Index [Q]\CLmstr\TRMSTR.H[cno]"&' '&"[Q]\CLmstr\TRIDX1.H[cno] 1 11 Replace DupKeys"
	execute "Index [Q]\CLmstr\TRMSTR.H[cno]"&' '&"[Q]\CLmstr\TRIDX2.H[cno] 28/1 8/11 Replace DupKeys"
Xit: fnXit
include: Ertn
