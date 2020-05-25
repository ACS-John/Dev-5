	open #trmstr1=1: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,outIn,keyed 
L30: read #1,using 'form pos 12,n 6': d1 eof L60
	pr d1 : pause 
	rewrite #1,using "form pos 12,n 6": d1
	goto L30
L60: stop 
