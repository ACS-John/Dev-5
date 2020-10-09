! Replace S:\acsCL\Conversion\GLBld-Cnv
 
	autoLibrary
	on error goto Ertn
 
	dim gl$*12,de$*50
 
	fncno(cno)
 
	open #2: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,input,keyed
	open #1: "Name=[Q]\CLmstr\GLmstr.H[cno],Size=0,RecL=62,Replace",internal,output
READ_GLMSTR: !
	read #2,using 'Form POS 1,C 12,C 50': gl$,de$ eof END1
	write #1,using 'Form POS 1,C 12,C 50': gl$,de$
	goto READ_GLMSTR
 
END1: close #1:
	close #2:
	execute "Index [Q]\CLmstr\GLmstr.H[cno]"&' '&"[Q]\CLmstr\GLINDEX.h[cno] 1 12 Replace DupKeys -n"
	goto Xit
 
Xit: fnXit
 
include: ertn
