! Replace S:\acsGL\AcGlNote
! -- Foot Notes
 
	autoLibrary
	on error goto Ertn
 
	dim tb$*32,fl2$(2),sc2$(2)*46,ln$*8000,cnam$*40,dat$*20,cap$*128
	dim option$(2)*42,resp$(1)*50,atlantis$*80
 
	fnTop(program$,cap$="Financial Statements Notes")
	fncno(cno,cnam$) : _
	fndat(dat$)
	pr newpage
	tempx=val(fnactpd$) conv L170
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
L170: open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  : _
	read #1,using 'Form POS 195,C 30': tb$ : _
	close #1:
	tb$="("&trim$(tb$)&")"
	if fnprocess=1 then t=2 : goto L290
MENU1: pr newpage
	fnTos(sn$="acglnote") : _
	mylen=20: mypos=mylen+3 : right=1
	option$(1)="1 = Edit Notes to Financial Statements" : _
	option$(2)="2 = pr Notes"
	fncomboa("NoteOption",1,mypos,mat option$,"You can edit or pr notes to the financial statements ",40)
	fnCmdKey("&Next",1,1,0,"Allows you to enter information.")
	fnCmdKey("&Cancel",5,0,1,"Return to menu.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)=option$(1) then t=1 else t=2
L290: on t goto L300,L320 none MENU1
L300: !
	fnget_wordprocessor_exe(atlantis$) : _
	execute 'SY -w '&atlantis$&' '&"[Q]\GLmstr\ACGLNote.h[cno] -n"
	goto MENU1
L320: pr newpage
	open #1: "Name=[Q]\GLmstr\AcGLNote.h[cno],Shr",display,input ioerr MENU1
	pr newpage
	pr f "10,20,Cc 25,N": "Foot Notes Printing..." : _
	pr f "12,2,C 11,B,5": "Cancel (F5)" : _
	on fkey 5 goto L460 : _
	fnopenprn
 
L370: linput #1: ln$ eof L460
	for j2=1 to len(rtrm$(ln$))
		if ln$(j2:j2)><"@" then goto L430
		if ln$(j2+1:j2+1)="1" then : _
			ln$(j2:j2+1)=rtrm$(fnpedat$)&ln$(j2+2:78-len(rtrm$(fnpedat$))) : _
			goto L420
		if ln$(j2+1:j2+1)="2" then : _
			ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) : _
		else if ln$(j2+1:j2+1)="3" then : _
			ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(fnactpd$)))
L420: !
L430: next j2
pr #255: tab(10);ln$
goto L370
L460: close #1:
fncloseprn
on fkey 5 ignore
if fnprocess=1 then goto Xit else goto MENU1
goto Xit
 
Xit: fnXit
 
! <updateable region: ertn>
ERTN: fnerror(program$,err,line,act$,"Xit")
if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
if trim$(env$("ACSDeveloper"))<>"" then : _
	execute "list -"&str$(line) : pause : goto ERTN_EXEC_ACT
pr "program pause: type go and press [enter] to continue." : _
pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
 
dim tb$*32,fl2$(2),sc2$(2)*46,ln1$*8000,ln$*8000,cnam$*40,dat$*20,cap$*128
