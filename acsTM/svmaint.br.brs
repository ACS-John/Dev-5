! (C) COPYRIGHT - 1986 - ADVANCED COMPUTER SERVICES, INC.
	on error goto L2010
	autoLibrary
	fnTop(program$,cap$="Service Code")
	fncno(cno,cnam$)
	dim sc$*4,ds$*30,holdsc$*4,scode$*4
	dim cnam$*40,dat$*20
	dim sb$(4),sd$(4),se$(4)*25,pl$(4,3)*30
	sb$(1)="5,30,c 4,u  ,n"
	sb$(2)="6,30,c 30,CU  ,n"
	sb$(3)="7,30,N 7.2,u  ,n"
	sb$(4)="8,30,n 9.2,u  ,n"
	sd$(1)="5,2,c 25,n"
	sd$(2)="6,2,c 25,n"
	sd$(3)="7,2,c 25,n"
	sd$(4)="8,2,c 25,n"
	se$(1)="SERVICE CODE"
	se$(2)="DESCRIPTION"
	se$(3)="TOTAL HOURS - YTD"
	se$(4)="STANDARD FEES - YTD"
	gosub L1780
	open #1: "Name=S:\Core\Data\acsllc\SCMSTR.H[cno],KFName=S:\Core\Data\acsllc\SCIndex.H[cno],Shr",internal,outIn,keyed ioerr L2000
	goto L260
L260: pr newpage
	pr f "3,9,c 55,N": "SERVICE CODE MASTER FILE"
	pr f "4,6,C 72,N": "COMPANY NUMBER [cno]  "&ltrm$(cnam$)
	pr f "6,9,c 55,n": "1 = INITIAL FILE PREPARATION"
	pr f "7,9,c 55,n": "2 = ADD NEW RECORDS"
	pr f "8,9,c 55,n": "3 = FILE MAINTENANCE / INQUIRY"
	pr f "9,9,c 55,n": "4 = pr PROOF LIST"
	pr f "10,9,c 55,n": "5 = pr SERVICE CODE LISTING"
	pr f "12,9,c 55,n": "0 = COMPLETED (RETURN TO T/M MENU)"
	pr f "14,9,c 55,n": "ENTER SELECTION #:"
L360: input fields "14,28,N 1,EU,N": ti conv L360
	if ti=0 then goto L1700
	restore #1,key>="    ": nokey L390 eof L390
L390: on ti goto L410,L580,L930,L1260,L400 none L360
L400: chain "S:\acsTM\TMSCLIST"
L410: pr newpage
	center=42-int(len(rtrm$(cnam$))/2)
	pr f "5,"&str$(center)&",C 50,N": cnam$
	pr f "8,25,c 50,h,n": "***********   WARNING   ***********"
	pr f "10,20,c 52,n": "THIS SELECTION WILL DESTROY ALL EXISTING SERVICE"
	pr f "11,20,c 60,n": "CODE RECORDS THAT ARE CURRENTLY SET UP ON THIS COMPANY."
	pr f "12,20,c 50,n": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO"
	pr f "13,20,c 23,n": "RETURN TO THE SUB-MENU."
L490: input fields "13,50,C 5,IE,n": a$ conv L490
	if uprc$(a$)="THINK" then goto L510 else goto L260
L510: i2=1
	close #1: ioerr L530
L530: open #1: "Name=S:\Core\Data\acsllc\SCMSTR.H[cno],KFName=S:\Core\Data\acsllc\SCIndex.H[cno]",internal,outIn,keyed ioerr L550
	close #1,free: ioerr L550
L550: open #1: "Name=S:\Core\Data\acsllc\SCMSTR.H[cno],SIZE=0,RecL=43,NoShr",internal,outIn,relative ioerr L2010
	goto L1700
L570: form pos 1,c 4,c 30,pd 4.2,pd 5.2
L580: new1=1
L590: pr newpage
	pr f "2,15,c 40,n": "*** ADD SERVICE CODE RECORDS ***"
	pr f "3,10,c 60,n": "ENTER SERVICE CODE NUMBER AS BLANK WHEN COMPLETED"
L620: pr f mat sd$: mat se$
	if ti=3 or convc>0 then goto L710
L640: input fields "5,30,C 4,eu,n": scode$ conv L640
	if ltrm$(rtrm$(scode$))="0" or rtrm$(scode$)="" then goto L260
	scode$=lpad$(rtrm$(scode$),4)
	read #1,using L570,key=scode$: sc$,ds$,th,sf nokey L730 ioerr L2010
	oldti=2
	ti=3
	holdsc$=sc$
L710: if ti=3 or convc>0 then pr f mat sb$: sc$,ds$,th,sf
	pr f "23,5,C 60,H,N": "F6=HELP"
L730: input fields mat sb$: sc$,ds$,th,sf conv L740
L740: if convc=0 then goto L770
	sb$(convc)(cp1:cp2)="U  "
	convc=0
L770: if cmdkey=6 then goto L1860
	if cnt=4 then goto L840
	convc=cnt+1
	cp1=len(rtrm$(sb$(convc)))-4
	cp2=cp1+2
	sb$(convc)(cp1:cp2)="RC "
	goto L730
L840: sc$=lpad$(rtrm$(sc$),4)
	if ti=3 then goto L1040
	if rtrm$(sc$)="" or ltrm$(rtrm$(sc$))="0" then goto L260
	read #1,using L880,key=sc$: sc$ nokey L910 ioerr L2010
L880: form pos 1,c 4
	pr f "5,35,c 30,h,n": "DUPLICATE SERVICE CODE NUMBER"
	goto L730
L910: write #1,using L570: sc$,ds$,th,sf
	goto L390
L930: pr newpage ! *****  FILE MAINT   *****
	pr f "10,15,c 50,n": "ENTER SERVICE CODE NUMBER, ENTER 0 WHEN COMPLETED"
L950: input fields "10,70,c 4,eu,n": scode$ conv L950
	if ltrm$(rtrm$(scode$))="0" or rtrm$(scode$)="" then goto L260
	scode$=lpad$(rtrm$(scode$),4)
	read #1,using L570,key=scode$: sc$,ds$,th,sf nokey L950 ioerr L2010
	holdsc$=sc$
L1000: pr newpage
	pr f "2,13,c 45,n": "*** REVIEW SERVICE CODE RECORDS ***"
	pr f "3,10,c 60,n": "ENTER SERVICE CODE NUMBER AS BLANK TO DELETE"
	goto L620
L1040: if ltrm$(sc$)="" or rtrm$(ltrm$(sc$))="0" then goto L1050 else goto L1160
L1050: pr newpage
	pr f "10,10,c 60,n": "SERVICE CODE NUMBER "&holdsc$&" WILL BE DELETED"
	pr f "12,10,c 40,n": "ENTER 1 TO DELETE; ENTER 2 TO RE-ENTER"
L1080: input fields "12,55,n 1,eu,n": dcode conv L1080
	if dcode=1 then goto L1130
	if dcode><2 then goto L1080
	sc$=holdsc$
	goto L1000
L1130: delete #1,key=holdsc$:
	new1=1
	goto L930
L1160: if holdsc$=sc$ then goto L1220
	read #1,using L880,key=sc$: sc$ nokey L1190 ioerr L2010
	goto L930
L1190: delete #1,key=holdsc$:
	new1=1
	goto L910
L1220: rewrite #1,using L570,key=sc$: sc$,ds$,th,sf
	if oldti=2 then ti=2
	oldti=0
	goto L390
L1260: pr newpage
	if process=1 then goto L1340
	pr f "8,10,c 50,n": "POSITION PAPER FOR SERVICE CODE PROOF LIST"
	pr f "12,10,c 25,n": "ENTER DATE FOR PROOF LIST"
L1300: rinput fields "12,40,c 20,uE,n": dat$ conv L1300
	namtab=66-int(len(rtrm$(cnam$))/2)
	dattab=66-int(len(rtrm$(dat$))/2)
	pr newpage
L1340: pr f "10,10,c 50,n": "PRINT SERVICE CODE PROOF LIST IN PROCESS"
	pr f "23,2,C 30,N": "Press F5 to stop"
	on fkey 5 goto L1670
	fnopenprn
L1380: j=0
	eofc=0
L1400: read #1,using L570: sc$,ds$,th,sf eof L1640 ioerr L2010
	j=j+1
	pl$(1,j)=sc$
	pl$(2,j)=ds$
	pl$(3,j)=str$(th)
	pl$(4,j)=str$(sf)
	if j=3 then goto L1480
	goto L1400
L1480: if pcnt><0 then goto L1510
	pr #255,using L1500: date$,cnam$,time$,"SERVICE CODE PROOF LIST",dat$
L1500: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 55,c 24,skip 1,pos dattab,c 20,skip 2
L1510: for i=1 to 4
		pr #255,using L1530: se$(i),pl$(i,1),pl$(i,2),pl$(i,3)
L1530: form pos 1,c 25,pos 30,c 30,pos 65,c 30,pos 100,c 30,skip 1
	next i
	pr #255:
	mat pl$=(" ")
	if eofc=1 then goto L1670
	pcnt=pcnt+1
	if pcnt=11 then goto L1610
	goto L1380
L1610: pr #255: newpage
	pcnt=0
	goto L1380
L1640: if j=0 then goto L1670
	eofc=1
	goto L1480
L1670: fncloseprn
	on fkey 5 ignore
	goto L260
L1700: close #1:
	if new1=1 then goto L1730
	if ti=0 and i2=0 then goto Xit
L1730: execute "Index S:\Core\Data\acsllc\SCMSTR.H[cno]"&' '&"S:\Core\Data\acsllc\SCIndex.H[cno] 1 4 REPLACE DupKeys"
	if i2=1 then chain "S:\acsTM\SVMAINT"
	if t1=0 then goto L1770
	chain "S:\acsTM\SVMAINT"
L1770: goto Xit
L1780: dim hlp$(20)*78,flh$(22)*18,a$*5
	open #10: "Name=S:\acsTM\SC.HLP,Shr",internal,outIn,relative ioerr L2010
	for j=1 to 20
		flh$(j)=str$(j+2)&",2,C 78,U,N"
	next j
	flh$(21)="1,25,C 40,H,N"
	flh$(22)="24,5,C 65,H,N"
return
L1860: pr newpage
	convc=currow-4
	if convc<1 or convc>8 then convc=0: goto L1950
	cp1=pos(uprc$(sb$(convc)),"U",1)
	cp2=cp1+2
	sb$(convc)(cp1:cp2)="UC "
	read #10,using L1930,rec=convc: mat hlp$ noRec L1990 ioerr L2010
L1930: form pos 1,20*c 78
	pr f mat flh$: mat hlp$,se$(convc),"ENTER 0 TO CONTINUE OR 1 TO UPDATE HELP SCREEN:"
L1950: input fields "24,69,N 1,EU,N": j2 conv L1950
	if j2<>1 then goto L1990
	input fields mat flh$: mat hlp$
	rewrite #10,using L1930,rec=convc: mat hlp$
L1990: if ti=3 then goto L1000 else goto L590
L2000: if err=4152 then goto L510
L2010: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2030
	goto L2070
L2030: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2060
	goto L2070
L2060: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L2070: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto Xit
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry
Xit: fnXit
