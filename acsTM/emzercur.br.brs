on error goto Ertn
autoLibrary
fnTop(program$,"Client")
fnconsole(1)
dim eno$*9,cchrs(10),ncchrs(20),ccamt(10),nccamt(20)
L90: !
pr newpage
pr f "8,25,c 30,r,n": "********  Warning  ********"
pr f "11,10,c 70": "This program zeros the current period totals in"
pr f "12,10,c 70": "the employee master file.  Be sure you have run"
pr f "13,10,c 70": "all of the current period reports before continuing."
pr f "15,10,c 62": "[1] Continue  [2] Exit"
L150: input fields "15,75,n 1,eu,n": a conv L150
on a goto L170,XIT none L90
L170: !
pr newpage
pr f "10,10,c 60,n": "ZERO CURRENT FIELDS IN EMPLOYEE MASTER FILE IN PROCESS"
open #1: "Name=S:\Core\Data\acsllc\EMmstr.H[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,outIn,keyed
L200: read #1,using L210: eno$ eof L250
	L210: form pos 1,c 9
	rewrite #1,using L230: mat cchrs,mat ncchrs,mat ccamt,mat nccamt
L230: form pos 38,30*pd 4.2,pos 278,30*pd 5.2
goto L200
L250: !
close #1: 
Xit: fnxit
include: ertn