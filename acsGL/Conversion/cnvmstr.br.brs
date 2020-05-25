! Replace S:\acsGL\Conversion\CnvMSTR
! some sort of conversion program for [Q]\GLmstr
 
	autoLibrary
	fnTop(program$)
	on error goto Ertn
 
	dim k$*12,d$*50,rf(6),bc(12),bm(12),bp(12),ta(2)
 
	pr newpage
	pr f "10,5,C 60": "COMPANY NUMBER TO CONVERT:"
L120: input fields "10,55,N 2,UE,N": cno conv L120
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed ioerr L120
	open #2: "Name=[Q]\GLmstr\GLmstr.h[cno]",internal,output ioerr L160
	close #2,free:
L160: open #2: "Name=[Q]\GLmstr\GLmstr.h[cno],SIZE=0,RecL=416",internal,output
L170: read #1,using L180: k$,d$,rf(1),rf(3),rf(5),bb,cb,mat bc,mat bp,mat bm,pbp,fr(2),rf(4),rf(6) eof END1
L180: form pos 1,c 12,c 50,3*pd 3,39*pd 6.2,3*pd 3
	write #2,using L200: k$,d$,mat rf,bb,cb,mat bc,0,mat bp,0,mat bm,0,pbp,mat ta
L200: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
	goto L170
 
END1: close #1:
	close #2:
	execute "Index [Q]\GLmstr\GLmstr.h[cno]"&' '&"[Q]\GLmstr\GLIndex.h[cno] 1 12 Replace DupKeys -n"
	goto Xit
 
include: Ertn
 
Xit: fnXit
