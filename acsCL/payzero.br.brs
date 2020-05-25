! ???
 
	autoLibrary
	on error goto Ertn
 
	dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,holdvn$*8,vcode$*8
	dim gl(3),sf1$*28,sn$*30,de$*30,rn$*12,de$*30,ta(2),tvn$*8
	dim flit$(4)*16,scrt$(4)*20,scid$*79,desc$(6)*14
 
	fncno(cno)
 
	open #1: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	restore #1,key>="        ": eof Xit
L150: read #1,using 'Form POS 129,PD 5.2': ytdp eof Xit
	rewrite #1,using 'Form POS 129,PD 5.2': 0
	goto L150
Xit: stop
 
include: Ertn
