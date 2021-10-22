	on fkey 5 goto L510
	on error goto Ertn
! (C) COPYRIGHT - 1986 - ADVANCED COMPUTER SERVICES, INC.
	autoLibrary
	fnTop(program$)
	dim sc$*4,ds$*30
	dim prg$*20
	open #1: "Name=S:\Core\Data\acsllc\SCMSTR.h[cno],KFName=S:\Core\Data\acsllc\SCIndex.h[cno]",i,i,k
	namtab=43-int(len(rtrm$(env$('cnam')))/2)
! L130: ! pr newpage
	! pr f "7,10,c 60,n": "POSITION PAPER IN PRINTER FOR SERVICE CODE LISTING"
	! pr f "10,10,c 52,n": "ENTER DATE FOR SERVICE CODE LISTING IN MMDDYY FORMAT"
	! pr f "10,66,n 6,n": dat
! L170: input fields "10,66,n 6,eu,n": dat conv L170
! 	if dat<10100 or dat>123199 then goto L130
! 	pr newpage
! 	pr f "10,25,c 48,n": "SERVICE CODE LISTING IN PROCESS"
	! pr f "23,2,c 20,n": "Press F5 to stop"
	fnopenprn
	gosub L410
L260: read #1,using L270: sc$,ds$ eof L520
L270: form pos 1,c 4,c 30
	sc=int(val(sc$)/100)
	if fst=1 then goto L320
	holdsc=sc
	fst=1
L320: if holdsc=sc then goto L350
	pr #255:
	holdsc=sc
L350: pr #255,using L360: sc$,ds$ pageoflow L380
L360: form pos 1,c 4,pos 9,c 30,skip 1
	goto L260
L380: pr #255: newpage
	gosub L410
	goto L260
L410: p1=p1+1
	pr #255,using L430: env$('cnam'),"PAGE",p1
L430: form skip 3,pos namtab,c 40,pos 76,c 5,n 4,skip 1
	pr #255,using L450: "SERVICE CODE LISTING"
L450: form pos 33,c 22,skip 1
	pr #255,using L470: date$
L470: form pos 38,pic(zz/zz/zz),skip 3
	pr #255,using L490: "CODE","DESCRIPTION"
L490: form pos 2,c 4,pos 14,c 11,skip 2
return
L510: close #1: ioerr L520
L520: fncloseprn
	if uprc$(rtrm$(prg$))="S:\Client Billing\Legacy\Service Code" then chain prg$
goto XIT
 
XIT: fnxit
include: ertn
