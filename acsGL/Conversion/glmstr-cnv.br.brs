dim cnam$*40,revb(13)
autoLibrary
close #101: ioerr L60
fnTop(program$)
L60: open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION=Convert G/L Master for GASB",display,outIn 
	pr f "10,24,C 32": "ENTER COMPANY NUMBER TO CONVERT:"
	pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
L90: input fields "10,57,N 2,UE,N",attr "R": cno conv L90
	if cmdkey=5 then goto L230
! 
	execute "Copy [Q]\GLmstr\GLmstr.h[cno] Work."&session$&" -416" ioerr L220
	execute "COPY  Work."&session$&' '&"[Q]\GLmstr\GLmstr.h[cno]"
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed 
L150: read #1,using L180: mat revb eof END1
	mat revb=(0)
	rewrite #1,using L180: mat revb
L180: form pos 339,13*pd 6.2
	goto L150
END1: close #1: 
	execute "Index [Q]\GLmstr\GLmstr.h[cno]"&' '&"[Q]\GLmstr\GLIndex.h[cno] 1 12 REPLACE DupKeys"
L220: ! Goto 30
L230: chain "S:\acsGL\acglblds"
