on error goto Ertn
autoLibrary
dim actpd$*6,tb$*32
fnTop(program$)
execute "Copy [Q]\GLmstr\AcGLStmt.h[cno] x -D"
execute "copy x [Q]\GLmstr\AcGLStmt.h[cno]"
open #1: "Name=[Q]\GLmstr\AcGLStmt.h[cno],Shr",internal,outIn,relative 
open #2: "Name=test,size=0,RecL=128,replace",display,output 
dim ln$(10)*78
do
	read #1,using L150: mat ln$ eof L180 noRec L180
	L150: form pos 1,10*c 78
	pr #2,using L150: mat ln$
loop
L180: !
close #1:
close #2: 
execute "copy test [Q]\GLmstr\AcGLStmt.h[cno]"
Xit: fnxit
include: Ertn