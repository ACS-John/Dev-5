! formerly S:\acsUB\UBZEROYT
library 'S:\Core\Library': fnLbl
library 'S:\Core\Library': fnTxt
library 'S:\Core\Library': fnAcs
library 'S:\Core\Library': fnTos,fnxit
library 'S:\Core\Library': fnCmdSet,fntop
on error goto Ertn
fntop(program$)
SCREEN1: ! 
fnTos(sn$:="ubZeroYt")
fnLbl(1,1,'Type "ZERO" to Zero all Year To Date Usages:',48,1)
fnTxt(1,50,5)
resp$(1)=""
fnCmdSet(2)
fnAcs(sn$,0,mat resp$,ck)
if ck=5 then goto XIT
if rtrm$(uprc$(resp$(1)))<>"ZERO" then goto SCREEN1
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
dim z$*10
do
	read #1,using "Form POS 1,C 10": z$ eof DONE
	rewrite #1,using "Form POS 232,PD 5,POS 252,PD 5,POS 272,PD 5": 0,0,0
loop
DONE: !
close #1: 
goto XIT

XIT: fnxit
include: ertn