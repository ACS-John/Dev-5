on fkey 5 goto L400
on error goto Ertn

autoLibrary
fnTop(program$,"Employee")
fnopenprn
dim eno$*9,e$*25,r(11),prg$*20
namtab=66-int(len(rtrm$(env$('cnam')))/2)
open #1: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,input,keyed
L120: !
pr newpage
pr f "10,10,c 48,n": "ENTER DATE FOR STAFF DIRECTORY IN MMDDYY FORMAT"
pr f "10,60,n 6,n": dat
L150: !
input fields "10,60,n 6,eu,n": dat conv L150
if dat<10100 or dat>123199 then goto L120
pr newpage
pr f "10,25,c 30,n": "STAFF DIRECTORY IN PROCESS"
pr f "23,2,c 30,n": "Press F5 to stop"
gosub L290
L210: read #1,using L220: eno$,e$,dept,mat r eof L400 
L220: form pos 1,c 9,c 25,pd 2,pos 578,11*pd 3.2
	pr #255,using L240: eno$,e$,dept,mat r pageoflow L260
L240: form pos 1,c 9,pos 12,c 25,n 5,11*n 8.2,skip 1
	goto L210
L260: pr #255: newpage
	gosub L290
	goto L210
L290: pr #255,using L300: env$('cnam')
L300: form skip 3,pos namtab,c 40,skip 1
	pr #255,using L320: "STAFF DIRECTORY"
L320: form pos 58,c 15,skip 1
	pr #255,using L340: dat
L340: form pos 61,pic(zz/zz/zz),skip 3
	pr #255,using L360: "EMPLOYEE","EMPLOYEE","DEPT"
L360: form pos 2,c 8,pos 20,c 8,pos 38,c 4,skip 1
	pr #255,using L380: "NUMBER","NAME","CODE","RATE-1  RATE-2  RATE-3  RATE-4  RATE-5  RATE-6  RATE-7  RATE-8  RATE-9 RATE-10 RATE-11"
L380: form pos 4,c 6,pos 24,c 4,pos 38,c 4,pos 44,c 88,skip 2
return 
L400: !
close #1: ioerr ignore
fncloseprn
if uprc$(rtrm$(prg$))="S:\acsTM\EMAINT" then chain prg$
goto XIT
L440: !
XIT: fnxit
include: ertn