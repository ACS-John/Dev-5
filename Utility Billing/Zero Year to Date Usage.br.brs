! formerly S:\acsUB\UBZEROYT
autoLibrary
on error goto Ertn
fnTop(program$)
SCREEN1: !
fnTos(sn$:="ubZeroYt")
fnLbl(1,1,'Type "ZERO" to Zero all Year To Date Usages:',48,1)
fnTxt(1,50,5)
resp$(1)=""
fnCmdSet(2)
fnAcs(mat resp$,ckey)
if ckey=5 then goto Xit
if rtrm$(uprc$(resp$(1)))<>"ZERO" then goto SCREEN1
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
dim z$*10
do
	read #1,using "Form POS 1,C 10": z$ eof DONE
	rewrite #1,using "Form POS 232,PD 5,POS 252,PD 5,POS 272,PD 5": 0,0,0
loop
DONE: !
close #1:
goto Xit
 
Xit: fnXit
include: Ertn
