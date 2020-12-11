! this pgm will search the check history file and delete any records with conversion errors
autoLibrary
dim tr$(5)*35
dim tr(2)
dim checkNumber$*11
open #1: "Name=[Q]\CLmstr\TRMSTR.h[cno],KFName=[Q]\CLmstr\TRIDX1.h[cno],Shr",internal,outIn,keyed
open #2: "Name=[Q]\CLmstr\TRMSTR.h[cno],KFName=[Q]\CLmstr\TRIDX2.h[cno],Shr",internal,outIn,keyed
L140: !
	read #1,using L150: checkNumber$,tr$(2),tr$(3),tr$(4),tr$(5),pcde,clr,scd,mat tr eof L200 conv L170
	L150: form pos 1,c 11,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
goto L140
L170: !
	reread #1,using L150: checkNumber$ eof L200
	delete #1,key=checkNumber$:
goto L140
L200: !
close #1:
close #2:
execute "Index [Q]\CLmstr\TRMSTR.h[cno]"&' '&"[Q]\CLmstr\TRIDX2.h[cno] 28/1 8/11 Replace DupKeys"
execute "Index [Q]\CLmstr\TRMSTR.h[cno]"&' '&"[Q]\CLmstr\TRIDX1.h[cno] 1 11 Replace DupKeys"
fnXit
