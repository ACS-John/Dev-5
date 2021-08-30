
dim dat$*20,cnam$*40,vnam$*30,de$*35,bn$*30

open #5: "Name=[Q]\CLmstr\TRMSTR.h[cno],Shr",i,outi,r 
L90: !
	read #5,using L100: bcde,tcde,checkNumber$,clr eof END1
	L100: form pos 1,n 2,n 1,c 8,pos 72,n 6
	if tcde><1 then goto L90
	ck1=val(checkNumber$) conv L90
	if ck1<40718 or ck1>49000 then goto L90
	if clr><123107 then goto L90
	rewrite #5,using L122: 0
	L122: form pos 72,n 6
goto L90
END1: stop 
