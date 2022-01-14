autoLibrary
on error goto Ertn


open #2: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr',i,i,k
open #1: 'Name=[Q]\CLmstr\GLmstr.h[cno],RecL=62,Replace',internal,output
do
	dim gl$*12,de$*50
	read #2,using 'form pos 1,C 12,C 50': gl$,de$ eof END1
	write #1,using 'form pos 1,C 12,C 50': gl$,de$
loop
END1: !
	close #1:
	close #2:
	execute 'Index [Q]\CLmstr\GLmstr.h[cno] [Q]\CLmstr\GLINDEX.h[cno] 1 12 Replace DupKeys -n'
goto Xit

Xit: fnXit

include: ertn
