! Replace S:\acsGL\AcGlNote
! -- Foot Notes

autoLibrary
on error goto Ertn

dim tb$*32,ln$*8000,dat$*20,cap$*128
dim option$(2)*42,resp$(1)*50,atlantis$*80

fnTop(program$,cap$="Financial Statements Notes")
fndat(dat$)
pr newpage
tempx=val(fnactpd$) conv L170
if tempx=1 then actpd$="one"
if tempx=2 then actpd$="two"
if tempx=3 then actpd$="three"
if tempx=4 then actpd$="four"
if tempx=5 then actpd$="five"
if tempx=6 then actpd$="six"
if tempx=7 then actpd$="seven"
if tempx=8 then actpd$="eight"
if tempx=9 then actpd$="nine"
if tempx=10 then actpd$="ten"
if tempx=11 then actpd$="eleven"
if tempx=12 then actpd$="twelve"
if tempx=13 then actpd$="thirteen"
if tempx=14 then actpd$="fourteen"
L170: !
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #1,using 'Form POS 195,C 30': tb$
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
L320: !
open #1: "Name=[Q]\GLmstr\AcGLNote.h[cno],Shr",display,input ioerr MENU1
pr newpage
pr f "10,20,Cc 25,N": "Foot Notes Printing..." : _
pr f "12,2,C 11,B,5": "Cancel (F5)" : _
on fkey 5 goto L460 : _
fnopenprn

do
	linput #1: ln$ eof L460
	for j2=1 to len(rtrm$(ln$))
		if ln$(j2:j2)="@" then 
			if ln$(j2+1:j2+1)="1" then
				ln$(j2:j2+1)=rtrm$(fnpedat$)&ln$(j2+2:78-len(rtrm$(fnpedat$)))
			else if ln$(j2+1:j2+1)="2" then 
				ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:78-len(rtrm$(dat$))) 
			else if ln$(j2+1:j2+1)="3" then 
				ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:78-len(rtrm$(fnactpd$)))
			end if
			L430: !
		end if
	next j2
	pr #255: tab(10);ln$
loop
L460: !
	close #1:
	fncloseprn
	on fkey 5 ignore
	if fnprocess=1 then goto Xit else goto MENU1
goto Xit
Xit: fnXit
include: Ertn