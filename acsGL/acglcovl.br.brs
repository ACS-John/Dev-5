! Replace S:\acsGL\AcGlCovl
! -- Edit/Print Cover Letter
 
	autoLibrary
	fnTop(program$,cap$="Cover Leter")
	on error goto Ertn
 
	dim tb$*32,cap$*128,p$(20)*50
	dim ln1$*78,ln$*78,shd$*60,fli$(20),cnam$*40,dat$*20,fl2$(2),sc2$(2)*46
 
	fncno(cno,cnam$)
	fndat(dat$)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative  : _
	read #1,using 'Form POS 195,C 30',rec=1: tb$ : _
	close #1: : _
	tb$="("&trim$(tb$)&")"
	tempx=val(fnactpd$) conv L190
	if tempx=1 then actpd$="one" else : _
		if tempx=2 then actpd$="two" else : _
			if tempx=3 then actpd$="three" else : _
				if tempx=4 then actpd$="four" else : _
					if tempx=5 then actpd$="five"
	if tempx=6 then actpd$="six" else : _
		if tempx=7 then actpd$="seven" else : _
			if tempx=8 then actpd$="eight" else : _
				if tempx=9 then actpd$="nine" else : _
					if tempx=10 then actpd$="ten"
	if tempx=11 then actpd$="eleven" else : _
		if tempx=12 then actpd$="twelve" else : _
			if tempx=13 then actpd$="thirteen" else : _
				if tempx=14 then actpd$="fourteen"
L190: sh$="1,10,C 60,H,N"
	for j=1 to 20 : fli$(j)=str$(j+2)&",2,C 78,UT,N" : next j
	if fnprocess=1 then t=2 : goto L320 else goto MENU1
!_____
MENU1: pr newpage
	close #101: ioerr L250
L250: open #101: "SROW=3,SCOL=13,EROW=9,ECOL=63,BORDER=DR,CAPTION=<Cover Letter",display,outIn
	pr f "3,13,Cc 51,R,N": cnam$ : _
	pr f "4,13,Cc 51,R,N": "Company Number [cno]"
	sc2$(1)=" 1. Edit Cover Letter" : _
	sc2$(2)=" 2. pr Cover Letter"
	for j=1 to 2: fl2$(j)=str$(j+5)&",15,C 46": next j
	pr f "10,35,Cc 09,B,5": "Exit (F5)"
L300: rinput select mat fl2$,attr "H": mat sc2$ : _
	t=curfld
	if cmdkey=5 then goto Xit
L320: on t goto L370,L390 none L300
!_____
	close #101: ioerr L350
L350: open #101: "SROW=5,SCOL=13,EROW=15,ECOL=64,BORDER=SR,CAPTION=<Initial Build Cover Letter",display,outIn
	pr #101,fields "1,1,Cc 52,R,N": cnam$
L370: execute "SY -s NotePad "&os_filename$("[Q]\GLmstr\ACGLCovF.h[cno]")
	goto MENU1
L390: open #1: "Name=[Q]\GLmstr\ACGLCovF.h[cno],Shr",display,input ioerr MENU1
	pr newpage : _
	pr f "10,20,Cc 25,H,N": "Cover Letter Printing..." : _
	pr f "12,2,C 18,B,5": " Press F5 to stop"
	on fkey 5 goto L550
	fnopenprn
L430: linput #1: ln$ eof L550
	for j2=1 to len(rtrm$(ln$))
		if ln$(j2:j2)><"@" then goto L520
		if ln$(j2+1:j2+1)="1" then : _
			ln1$=ln$(1:j2-1)&fnpedat$&ln$(j2+2:78-len(fnpedat$)) : _
		else goto L480
		goto L510
L480: if ln$(j2+1:j2+1)="2" then : _
			ln1$=ln$(1:j2-1)&rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) : _
		else goto L500
		goto L510
L500: if ln$(j2+1:j2+1)="3" then : _
			ln1$=ln$(1:j2-1)&rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(actpd$)))
L510: ln$=ln1$
L520: next j2
	pr #255: tab(10);ln$
	goto L430
L550: close #1:
	pr newpage
	fncloseprn
Xit: fnchain("S:\acsGL\acglAuto")
 
include: Ertn
