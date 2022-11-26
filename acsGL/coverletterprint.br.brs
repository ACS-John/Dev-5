! Replace S:\acsGL\CoverLetterPrint
! -- pr Cover Letter

	autoLibrary
	on error goto Ertn

	dim tb$*32,ln1$*8800,ln$*8800,dat$*20

	fnTop("S:\acsGL\CoverLetterPrint","Print Cover Leter")
	fndat(dat$)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r
	read #1,using 'form pos 195,C 30',rec=1: tb$
	close #1:
	tb$="("&trim$(tb$)&")"
	tempx=val(fnactpd$) conv L180
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
L180: open #1: "Name=[Q]\GLmstr\ACGLCovF.h[cno],Shr",display,input ioerr Xit
	on fkey 5 goto DONE
	fnopenprn
READ_ACGLCOVF: !
	linput #1: ln$ eof DONE ioerr DONE
	for j2=1 to len(rtrm$(ln$))
		if ln$(j2:j2)><"@" then goto L320
		if ln$(j2+1:j2+1)="1" then : _
			ln$(j2:j2+1)=fnpedat$&ln$(j2+2:132-len(fnpedat$)) : _
		else goto L280
goto L310
L280: !
	if ln$(j2+1:j2+1)="2" then : _
			ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:132-len(rtrm$(dat$))) : _
		else goto L300
goto L310
L300: if ln$(j2+1:j2+1)="3" then : _
			ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:132-len(rtrm$(actpd$))) else goto L320
		L310: ! lN$=LN1$
		L320: !
	next j2
	pr #255: tab(10);ln$
goto READ_ACGLCOVF

DONE: close #1:
	fncloseprn
goto Xit

Xit: fnXit

include: ertn

