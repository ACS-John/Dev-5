! this pgm will search the check history file and delete any records with conversion errors
	autoLibrary
	fncno(cno)
	form c 9,skip 0
 
	dim adr(2),gl(3),sf1$*28,pr$(4)*30,whgl$(5)*12,whgl(5,3)
	dim tr$(5)*35,tr(2),de$*30,bn$*40,ladr(12),sk$*12,sn$*50,flh$(20),ink$(20),ck$*11
	open #1: "Name=[Q]\CLmstr\TRMSTR.H[cno],KFName=[Q]\CLmstr\TRIDX1.H[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\CLmstr\TRMSTR.H[cno],KFName=[Q]\CLmstr\TRIDX2.H[cno],Shr",internal,outIn,keyed
L140: read #1,using L150: ck$,tr$(2),tr$(3),tr$(4),tr$(5),pcde,clr,scd,mat tr eof L200 conv L170
L150: form pos 1,c 11,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
	goto L140
L170: reread #1,using L150: ck$ eof L200
	delete #1,key=ck$:
	goto L140
L200: close #1:
	close #2:
	execute "Index [Q]\CLmstr\TRMSTR.H[cno]"&' '&"[Q]\CLmstr\TRIDX2.H[cno] 28/1 8/11 Replace DupKeys"
	execute "Index [Q]\CLmstr\TRMSTR.H[cno]"&' '&"[Q]\CLmstr\TRIDX1.H[cno] 1 11 Replace DupKeys"
	fnXit
