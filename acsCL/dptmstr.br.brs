! Replace S:\acsCL\DptMstr
! ??? department
 
	autoLibrary
	on error goto Ertn
 
	dim hd$(2)*60,cnam$*40,dat$*20,ots$(5),ins$(22),ink$(22)*42
	dim pdate$(2)*20,de$*50,sm1$(6)*37,sf1$(6),d(2)
	dim d$*30,key$*9,nkey$*9,flo$(4),fli$(3),scr$(3)
 
	fncno(cno,cnam$)
	fndat(dat$)
 
	hp1=26-int(len(ltrm$(rtrm$(cnam$)))/2) : _
	hp2=26-int(len(ltrm$(rtrm$(dat$)))/2)
	flo$(1)="8,07,C 64,R,N" : flo$(2)="10,17,C 20" : _
	flo$(3)="10,32,C 20" : flo$(4)="11,08,C 20"
	fli$(1)="10,25,N 3,UT,N"
	fli$(2)="10,40,N 2,UE,N"
	fli$(3)="11,25,C 30,UT,N"
	scr$(1)="Fund #:"
	scr$(2)="Dept #:"
	scr$(3)="Department Name:"
 
	for j=1 to 22 : ins$(j)=str$(j+1)&",2,C 42,H,N" : next j
L240: open #1: "Name=[Q]\CLmstr\DPTMSTR.h[cno],KFName=[Q]\CLmstr\DPTIDX1.h[cno],Shr",internal,outIn,keyed ioerr L1800
	hd$(1)="     **ENTER FUND & DEPT # AS 0 OR BLANK TO STOP**"
	hd$(2)="     **ENTER FUND & DEPT  # AS 0 OR BLANK TO DELETE**"
	for j=1 to 6
		sf1$(j)=str$(j+5)&",11,C 37"
	next j
	sf1$(3)="8,11,C 37,C,N"
	cap$="Department"
MENU1: pr newpage
	if process=1 then ti=4 : goto L450
	cnam$=rtrm$(cnam$)
	close #101: ioerr L360
L360: open #101: "SROW=5,SCOL=10,EROW=12,ECOL=48,BORDER=DR,CAPTION=Department File",display,outIn
	pr f "2,9,Cc 41,R,N": cnam$
	pr f "3,9,Cc 41,R,N": "Company Number [cno]"
	sm1$(1)="1.  Initial File Preparation" : _
	sm1$(2)="2.  Add New Records" : _
	sm1$(3)="3.  Modify or Inquire Records" : _
	sm1$(4)="4.  pr Proof List" : _
	sm1$(5)="5.  Search" : _
	sm1$(6)="6.  Reorganize"
	pr f "13,25,C 09,B,5": "Exit (F5)"
L410: rinput select mat sf1$,attr "H": mat sm1$
	if cmdkey=5 then goto L1270
	ti0=ti=curfld
	mat ta=(0)
L450: on ti goto L460,L790,L800,L1530,L1810,L2070 none L410
L460: pr newpage
	close #101: ioerr L480
L480: open #101: "SROW=5,SCOL=13,EROW=16,ECOL=64,BORDER=DR,CAPTION=INITIAL FILE PREPARATION",display,outIn
	pr f "5,18,Cc 41,R,N": cnam$
	pr f "6,18,C 41,R,N": "            COMPANY NUMBER [cno]"
	pr f "8,13,C 52,R,N": " ******************   WARNING   ******************"
	pr f "10,13,C 52,H,N": "  THIS SELECTION WILL DESTROY ALL EXISTING RECORDS"
	pr f "11,13,C 52,H,N": "  ON THE DEPARTMENT NAME FILE."
	pr f "17,28,C 16,R,N": "PRESS F5 TO STOP"
	pr f "13,15,C 47,R,N": " Enter ERASE to continue"
L560: input fields "15,57,CU 5,UE,N",attr "R": pas$
	if cmdkey=5 then goto MENU1
	if pas$><"COPY " then goto L620
	close #1: ioerr L600
L600: execute "COPY A:DPTMSTR.H[cno],[Q]\CLmstr\*.*"
	goto L770
L620: if ltrm$(rtrm$(pas$))><"BUILD" then goto L730
	close #1: ioerr ignore
	open #2: "Name=DPTMSTR.h[cno]/DPTMSTR,KFName=DPTIDX1.h[cno]/DPTMSTR,Shr",internal,input,keyed
	open #1: "Name=[Q]\CLmstr\DPTMSTR.h[cno],SIZE=0,RecL=35,Replace",internal,output
L660: read #2,using L670: gl$,de$ eof END1
L670: form pos 1,c 12,c 50
	write #1,using L670: gl$,de$
	goto L660
END1: close #1:
	close #2:
	goto L770
L730: if pas$><"ERASE" then goto L560
L740: close #1: ioerr ignore
	open #1: "Name=[Q]\CLmstr\DPTMSTR.h[cno],SIZE=0,RecL=35,Replace",internal,output
	close #1:
L770: execute "Index [Q]\CLmstr\DPTMSTR.h[cno]"&' '&"[Q]\CLmstr\DPTIDX1.h[cno] 1 5 Replace DupKeys"
	goto L240
L790: new1=1
L800: pr newpage
	pr f mat flo$: hd$(1),mat scr$
	pr f "13,20,C 25,R,N": " F1=CONTINUE, F5=STOP"
L830: input fields mat fli$,attr "R": hfun,hdpt conv L830
	if ce>0 then fli$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 or curfld=2 then goto L920 else ce=curfld
L860: ce=ce+1: if ce>udim(fli$) then ce=1
L870: fli$(ce)=rtrm$(uprc$(fli$(ce))) : ce1=pos(fli$(ce),"U",1) : if ce1=0 then goto L860
	ce2=ce1+1 : fli$(ce)(ce1:ce1)="UC" : goto L830
CONV0: if ce>0 then fli$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR0: pr f "24,78,C 1": bell : goto L870
L920: !
	if hfun<0 or hdpt<0 then goto L830
	if hfun+hdpt=0 then goto MENU1
	key$=cnvrt$("N 3",hfun)&cnvrt$("N 2",hdpt)
	read #1,using L1250,key=key$: fun,dpt,d$ nokey L980
	ti=3: goto L990
L980: if ti=2 then goto L1050 else goto L830
L990: pr newpage
	close #101: ioerr L1010
L1010: open #101: "SROW=7,SCOL=07,EROW=12,ECOL=70,BORDER=DR,CAPTION=GL MASTER FILE",display,outIn
	pr f mat flo$: hd$(1),mat scr$
	pr f flo$(1): hd$(2)
	pr f mat fli$: fun,dpt,d$
L1050: if ti><3 then pr f "2,20,C 25,N": ""
	pr f "13,20,C 25,R,N": " F1=CONTINUE, F5=STOP"
	ce=3: goto L1120
L1080: input fields mat fli$,attr "R": fun,dpt,d$ conv CONV1
	if ce>0 then fli$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 or curfld=3 then goto L1170 else ce=curfld+1
L1110: if ce>udim(fli$) then ce=1
L1120: fli$(ce)=rtrm$(uprc$(fli$(ce))) : ce1=pos(fli$(ce),"U",1)
	if ce1=0 then ce=ce+1 : goto L1110 else ce2=ce1+1 : fli$(ce)(ce1:ce1)="UC" : goto L1080
CONV1: if ce>0 then fli$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L1120
L1170: if cmdkey=5 then goto MENU1
! IF TI=2 AND DPT=0 THEN GOTO 470
	if fun<0 then convc=1: goto ERR1
	if dpt<0 then convc=2: goto ERR1
	nkey$=cnvrt$("N 3",fun)&cnvrt$("N 2",dpt)
	if ti=2 then goto L1450
	if nkey$><key$ then goto L1310
L1240: rewrite #1,using L1250,key=nkey$: fun,dpt,d$
L1250: form pos 1,n 3,n 2,c 30
	ti=ti0: goto L450
L1270: close #1:
	if new1=1 then goto L1760
	goto Xit
IGNORE: continue
L1310: pr newpage
	if dpt><0 then goto L1350
	pr f "8,10,c 60,n": "YOU CHOSE TO DELETE ACCOUNT # "&str$(hfun)&" "&str$(hdpt)
	goto L1390
L1350: read #1,using L1360,key=nkey$: fun nokey L1380
L1360: form pos 1,n 3
	goto L800
L1380: pr f "8,4,c 76,n": "YOU CHOSE TO CHANGE ACCOUNT # "&str$(hfun)&" "&str$(hdpt)&" TO "&str$(fun)&" "&str$(dpt)
L1390: pr f "12,10,c 35,n": "ENTER 1 TO CONTINUE; ELSE ENTER 0"
L1400: input fields "12,50,n 1,ue,n": j conv L1400
	if j<0 or j>1 then goto L1400
	if j=0 then goto L450
	delete #1,key=key$:
	if dpt=0 then goto L1510
L1450: if bb=cb then goto L1480
	if bb=0 then bb=cb
	if cb=0 then cb=bb
L1480: read #1,using L1250,key=nkey$: fun nokey L1500
	goto L1240
L1500: write #1,using L1250: fun,dpt,d$
L1510: new1=1
	goto L450
L1530: restore #1,search>="": eof L1540
L1540: pr newpage
	close #101: ioerr ignore
	open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION=GL PROOF LIST",display,outIn
	pr f "08,18,Cc 41,H,N": cnam$
	pr f "09,18,C 41,H,N": "            COMPANY NUMBER [cno]"
	pr f "11,18,C 41,R,N": "              IN PROCESS"
	pr f "13,30,C 16,R,N": "PRESS F5 TO STOP"
	on fkey 5 goto L1720
	fnopenprn
	gosub L1690
L1640: read #1,using L1250: fun,dpt,d$ eof L1720
	pr #255,using L1660: fun,dpt,d$ pageoflow L1680
L1660: form pos 1,n 3,n 7,x 2,c 30,skip 1
	goto L1640
L1680: pr #255: newpage : gosub L1690 : continue
L1690: pr #255,using L1700: date$('mm/dd/yy'),time$,cnam$,"Department File Listing",dat$
L1700: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos hp1,c 40,skip 1,pos 18,c 50,skip 1,pos hp2,c 20,skip 2
return
L1720: fncloseprn
	on fkey 5 ignore
	if process=1 then goto Xit else goto MENU1
	goto Xit
L1760: close #1: ioerr L1770
L1770: close #2: ioerr L1780
L1780: execute "Index [Q]\CLmstr\DPTMSTR.h[cno]"&' '&"[Q]\CLmstr\DPTIDX1.h[cno] 1 5 Replace DupKeys -n"
	goto Xit
L1800: if err=4152 then goto L740
L1810: restore #1,search>="": nokey MENU1
L1820: ln=eof1=0
	pr newpage
	pr f "1,2,C 4,R,N": "FUND"
	pr f "1,7,C 4,R,N": "DEPT"
	pr f "1,13,C 30,R,N": "DESCRIPTION"
L1870: read #1,using L1250: fun,dpt,d$ eof L1950
	ots$(1)=str$(ln+2)&",2,N 3,H,N"
	ots$(2)=str$(ln+2)&",8,N 2,H,N"
	ots$(3)=str$(ln+2)&",13,C 30,H,N"
	pr f mat ots$: fun,dpt,d$
	ln=ln+1
	if ln>21 then goto L1970
	goto L1870
L1950: eof1=1
	if ln=0 then goto MENU1
L1970: pr f "24,2,C 78,R,N": "HIGHLIGHT ACCOUNT # & PRESS ENTER TO SELECT; PRESS F1 TO CONTINUE; F5 TO STOP"
	mat ink$(ln)
L1990: input fields mat ins$,attr "R": mat ink$
	if cmdkey>0 then goto L2040
	key$=ink$(curfld)(1:3)&ink$(curfld)(7:8)
	read #1,using L1250,key=key$: fun,dpt,d$ nokey L1990
	ti=3: goto L990
L2040: if cmdkey=5 or eof1=1 then goto MENU1
	goto L1820
 
L2070: close #1: ioerr L2080
L2080: execute "Copy [Q]\CLmstr\DPTMSTR.h[cno] [Temp]\WORK -D -n"
	fnFree("[Q]\CLmstr\DPTMSTR.h[cno]")
	execute "RENAME [Temp]\WORK [Q]\CLmstr\DPTMSTR.h[cno] -n"
	execute "Index [Q]\CLmstr\DPTMSTR.h[cno]"&' '&"[Q]\CLmstr\DPTIDX1.h[cno] 1 5 Replace DupKeys -n"
	goto L240
 
include: Ertn
Xit: fnXit
 
