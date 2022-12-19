! Replace S:\acsGL\RestPrint
! -- pr Retained Earnings Statement

	autoLibrary
	on error goto Ertn

	dim cap$*128,ln1$*8800,ln$*8800,dat$*20

	fnTop(program$)
	fndat(dat$)
	tempx=val(fnactpd$) conv L180
	if tempx=1 then 
		actpd$="one" 
	else if tempx=2 then 
		actpd$="two" 
	else if tempx=3 then 
		actpd$="three" 
	else if tempx=4 then 
		actpd$="four" 
	else if tempx=5 then 
		actpd$="five"
	else if tempx=6 then 
		actpd$="six" 
	else if tempx=7 then 
		actpd$="seven"
	else if tempx=8 then 
		actpd$="eight" 
	else if tempx=9 then 
		actpd$="nine"
	else if tempx=10 then 
		actpd$="ten"
	else if tempx=11 then 
		actpd$="eleven" 
	else if tempx=12 then 
		actpd$="twelve" 
	else if tempx=13 then 
		actpd$="thirteen" 
	else if tempx=14 then 
		actpd$="fourteen"
	end if
	L180: !
	open #1: "Name=[Q]\GLmstr\acglstmt.h[cno],Shr",display,input ioerr Xit
	fnOpenPrn
	READ_ACGLREST: ! 
	linput #1: ln$ eof DONE ioerr DONE
	for j2=1 to len(rtrm$(ln$))
		if ln$(j2:j2)><"@" then goto L320
		if ln$(j2+1:j2+1)="1" then 
			ln$(j2:j2+1)=fnpedat$&ln$(j2+2:132-len(fnpedat$)) 
		else 
			goto L280
		end if
		goto L310
		L280: !
		if ln$(j2+1:j2+1)="2" then 
			ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:132-len(rtrm$(dat$))) 
		else 
			goto L300
		end if
		goto L310
		L300: !
		if ln$(j2+1:j2+1)="3" then 
			ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:132-len(rtrm$(actpd$)))
		else 
			goto L320
		end if
		L310: ! lN$=LN1$
		L320: !
	next j2
	pr #255: tab(10);ln$
goto READ_ACGLREST

DONE: !
	close #1: 
	fnClosePrn
goto Xit

Xit: fnXit
include: ertn
