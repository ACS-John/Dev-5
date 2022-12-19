! Replace S:\acsGL\acglRest
! pr Retained Earnings Statement

autoLibrary
on error goto Ertn

dim ln1$*78,ln$*78,shd$*60,fli$(10),fli1$(2),hdr$*78,foot$*78
dim sc2$(2),dat$*20

fnTop(program$,"Retained Earnings Statement")
fndat(dat$)
sh$="2,10,C 60,H,N"
for j=1 to 10 : fli$(j)=str$(j+2)&",2,C 78,UT,N" : next j
fli1$(1)="5,2,C 78,UT,N" : fli1$(2)="8,2,C 78,UT,N"
if fnProcess=1 then tx=2 : goto L240

MENU1: ! r:
	fnwin3(win=101,env$('program_caption'),6,40,1,1,5)
	sc2$(1)="1. Edit" : sc2$(2)="2. Print"
	for j=1 to 2 : fl2$(j)=str$(j+3)&",02,C 08,N" : next j
	rinput #win,select mat fl2$,attr "H": mat sc2$
	tx=curfld
	close #win:
	if cmdkey=5 or cmdkey=99 then goto Xit
L240: !
	j=0
on tx goto EDITX,L300 none MENU1 ! /r

EDITX: ! r:
	execute 'SY NotePad "'&os_filename$("[Q]\GLmstr\ACGLSTMT.h[cno]")&'"'
goto MENU1 ! /r
L300: ! r:
if fnGlAskFormatPriorCdPeriod=5 then goto MENU1
fnOpenPrn
pr newpage
open #1: "Name=[Q]\GLmstr\AcGLStmt.h[cno],Shr",display,input ioerr EDITX
pr newpage
pr f "10,20,Cc 30,H,N": "R/E Statement Printing..."
pr f "12,34,C 11,B,5": "Cancel (F5)"
on fkey 5 goto L480

L360: !
linput #1: ln$ eof L480
for j2=1 to len(rtrm$(ln$))
	if ln$(j2:j2)><"@" then goto L450
	if ln$(j2+1:j2+1)="1" then ln1$=ln$(1:j2-1)&rtrm$(fnpedat$)&ln$(j2+2:78-len(rtrm$(fnpedat$))) else goto L410
goto L440

L410: !
	if ln$(j2+1:j2+1)="2" then ln1$=ln$(1:j2-1)&rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) else goto L430
goto L440

L430: !
	if ln$(j2+1:j2+1)="3" then ln1$=ln$(1:j2-1)&rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(actpd$)))
L440: !
	ln$=ln1$
L450: !
	next j2
	pr #255: tab(10);ln$
goto L360

L480: !
	close #1:
	fnClosePrn
	on fkey 5 ignore
if fnProcess=1 then goto Xit else goto MENU1 ! /r

include: ertn
Xit: fnXit

