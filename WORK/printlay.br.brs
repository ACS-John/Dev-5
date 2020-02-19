! Replace Work\PrintLay
!
	library 'S:\Core\Library': fnerror,fnopenprn,fncloseprn
	on error goto Ertn
!
	dim a$(200,3)*40,h1$*55,rm$(4)*44,filename$*20,fil$(50)*20,ln$*80
	dim a(200,6),a$*132,prg$*20,mo$(12)
	data JANUARY,FEBRUARY,MARCH,APRIL,MAY,JUNE,JULY,AUGUST,SEPTEMBER,OCTOBER,NOVEMBER,DECEMBER
!
	read mat mo$
	dat$=mo$(val(date$(4:5)))&" "&date$(7:8)&",20"&date$(1:2)
	io1$(1)="10,65,N 1,UT,N"
	io1$(2)="12,65,C 14,UT,N"
	pr newpage
	close #101: ioerr L120
L120: open #101: "SROW=9,SCOL=4,EROW=13,ECOL=79,BORDER=DR,CAPTION=PRINT LAYOUTS",display,outIn 
	pr #101: newpage
	pr f "10,5,C 60": "Enter 0 for Printer only, 1 for Screen only, or 2 for Both:"
	pr f "12,5,C 60": "Enter Ext/VolId to pr all or blank to select:"
L160: input fields mat io1$: pp,ev$ conv L160
	if pp=0 then let fnopenprn
	if pp<0 or pp>2 then goto L160
	ev$=rtrm$(ev$)
	if ev$="" then goto L260
	execute "DROP DIRFILE" ioerr L210
L210: execute "DIR *."&ev$&" >DIRFILE"
	open #2: "Name=DIRFILE",display,input 
	p1=pos(ev$,"/",1)
	ex$=ev$(1:p1-1)
	goto L360
L260: close #101: ioerr L270
L270: open #101: "SROW=2,SCOL=4,EROW=6,ECOL=79,BORDER=DR,CAPTION=PRINT LAYOUTS",display,outIn 
	pr #101: newpage
	if f1>0 then pr f "2,5,C 60,H,N": "LAST FILE NAME ENTER WAS "&fil$(f1)
	pr f "4,5,C 60": "Enter File Name/VolId to pr or Blank to stop"
	rinput fields "4,55,C 20,UE,N": fil$(f1+1)
	fil$(f1+1)=rtrm$(fil$(f1+1))
	if pp>0 or fil$(f1+1)="" then goto L360
	f1=f1+1
	goto L260
L360: f2=f2+1: rl=l=a=b=ino=pg=j3=0: mat a=(0)
	if ev$="" then goto L430
L380: linput #2: a$ eof L1390
	a$=uprc$(a$)
	if rtrm$(a$(11:13))><ex$ then goto L380
	fil$(1)=rtrm$(a$(1:8))&"."&ev$
	f2=1: f1=2
L430: if rtrm$(fil$(f2))="" then goto L1390
	open #1: "Name="&fil$(f2),display,input ioerr L360
	goto L660
L460: p1=pos(filename$,".",1)
	if p1=0 then p1=min(8,len(filename$)) else p1=p1-1
	pr newpage
	pr f "1,1,C 40,R,N": " FIELD DESCRIPTIONS: "&filename$(1:p1)&"/"&volid$
	pr f "1,43,C 8,R,N": " NAME"
	pr f "1,53,C 8,R,N": " FORMAT"
	pr f "1,63,C 5,R,N": " FROM"
	pr f "1,70,C 5,R,N": "  TO"
	sln=1
L550: if pp=0 or j3=0 then goto L660
	if sln=0 then goto L460
	if sln<23 then sln=sln+1: goto L610
	pr f "24,5,C 60,R,N": "  SCREEN FULL  PRESS ENTER TO CONTINUE:"
	input fields "24,50,C 1,RE,N": pause$
	goto L460
L610: pr f str$(sln)&",1,C 40,UT,N": a$(j3,1)
	pr f str$(sln)&",43,C 8,UT,N": a$(j3,2)(1:10)
	pr f str$(sln)&",53,C 8,UT,N": a$(j3,3)(1:8)
	pr f str$(sln)&",63,PIC(ZZZZZ),UT,N": a(j3,5)
	pr f str$(sln)&",70,PIC(ZZZZZ),UT,N": a(j3,6)
L660: linput #1: ln$ eof L1200
	if ln$(7:10)="LET " then goto LETLN
	if ln$(7:10)="DATA" then goto DATALN
	p1=pos(ln$,"Replace",6)
	if p1=0 then goto L740
	p1=p1+8
	p2=pos(ln$,",",p1)
	prg$=ln$(p1:p2-1)
L740: goto L550
LETLN: p2=len(rtrm$(ln$))-1
	p1=pos(ln$,"H1$",1)
	if p1>0 then h1$=ln$(p1+5:p2) : goto L550
	p1=pos(ln$,"FILETYPE$",1)
	if p1>0 then filetype$=ln$(p1+11:p2): goto L550
	p1=pos(ln$,"FILENAME$",1)
	if p1>0 then filename$=ln$(p1+11:p2): goto L550
	p1=pos(ln$,"VOLID$",1)
	if p1>0 then volid$=ln$(p1+8:p2): goto L550
	p1=pos(ln$,"RM$",1)
	if p1=0 then goto L880
	rm=val(ln$(p1+4:p1+4))
	rm$(rm)=ln$(p1+8:p2)
L880: goto L550
DATALN: j3=j3+1
	p1=11
	p2=pos(srep$(ln$,'^','~'),'~',p1+1)
	p3=pos(srep$(ln$,'^','~'),'~',p2+1)
	p4=len(rtrm$(ln$))
	a$(j3,1)=ln$(p1:p2-1)
	a$(j3,2)=ln$(p2+1:p3-1)
	a$(j3,3)=ln$(p3+1:p4)
L970: form c 9,skip 0
	if rtrm$(a$(j3,3))="" then goto L1190
	p1=pos(a$(j3,3)," ",1)+1
	p2=pos(a$(j3,3),".",1)+1
	p3=len(rtrm$(a$(j3,3)))
	p4=pos(a$(j3,3),"*",1)
	if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
	l=int(val(a$(j3,3)(p1:p3))) ! FIELD STORAGE LENGTH
	if p2>1 then dp=val(a$(j3,3)(p2:p3)) else dp=0 ! DECIMAL POSITIONS
	if uprc$(a$(j3,3)(1:p1-2))="PD" then al=l*2-1 else al=l !   ACTUAL FIELD LENGTH
	l=l*m1 ! TOTAL STORAGE LENGTH
	b=a+l
	a=a+1
	ino=ino+1
	a(j3,1)=ino
	a(j3,2)=al
	a(j3,3)=dp
	a(j3,4)=l
	a(j3,5)=a
	a(j3,6)=b
	a=b
	rl=rl+int(val(a$(j3,3)(p1:p3)))*m1
L1190: goto L550
L1200: pgo=ceil(j3/24)
	if pp=1 then goto L1370
	gosub HDR
	for j=1 to j3
		p1=pos(a$(j,3)," ",1)
		p2=len(a$(j,3))
		l=val(a$(j,3)(p1:p2))
		if l>0 then goto L1310
		pr #255,using L1290: a$(j,1) pageoflow NEWPGE
L1290: form pos 13,c 43,skip 2
		goto L1340
L1310: if rtrm$(a$(j,1))="" then goto L1340
		pr #255,using L1330: a(j,1),a$(j,1),a$(j,2),a(j,2),a(j,3),a$(j,3),a(j,4),a(j,5),a(j,6) pageoflow NEWPGE
L1330: form pos 5,n 5,x 3,c 43,c 11,n 7,n 10,x 5,c 11,n 7,2*n 9,skip 2
L1340: next j
	pr #255: newpage
	pr #255,using L970: hex$("1B40")
	fncloseprn
L1370: close #1: 
	if f2<f1 then goto L360
L1390: close #2: ioerr L1400
L1400: pr f "24,1,C 7,UT,N": "Done..."
	stop 
NEWPGE: if j=j3 then goto L1450
	pr #255: newpage
	gosub HDR
L1450: continue 
HDR: ! r:
	pr #255,using L970: hex$("2B0205000A1042")
	pr #255,using L970: hex$("1B471B2D00")
	pg=pg+1
	pr #255,using L970: hex$("1B5701")
	pr #255,using L1520: h1$
	L1520: form pos 3,c 46,skip 2
	pr #255,using L970: hex$("1B57001B53000F")
	pr #255,using L1550: prg$,dat$,pg,pgo
	L1550: form pos 51,c 40,c 20,"PAGE:",n 3,"  OF:",n 3,skip 1
	pr #255,using L970: hex$("1B54")
	pr #255,using L970: hex$("2B0205000A0000")
	pr #255: tab(30);"----------------- REMARKS -------------------"
	pr #255,using L1600: str$(rl),rm$(1)
	L1600: form pos 5,"RECORD LENGTH: ",c 5,pos 30,c 45,skip 1
	pr #255,using L1620: rm$(2)
	L1620: form pos 30,c 45,skip 1
	pr #255,using L1640: rm$(3)
	L1640: form pos 30,c 45,skip 1
	pr #255,using L1660: filetype$,rm$(4)
	L1660: form pos 5,"FILE TYPE: ",c 10,pos 30,c 45,skip 2
	pr #255,using L1680: filename$,volid$
	L1680: form pos 5,"FILE NAME: ",c 20,"VOLID: ",c 10,skip 2
	pr #255,using L970: hex$("1B0F")
	pr #255: "   ITEM #   FIELD DESCRIPTION                           NAME       LENGTH    DECIMALS    FORMAT    STORAGE     FROM      TO  "
	pr #255: "   ------   ----------------------------------------   --------    ------    --------   -------    -------    -----    -----"
	pr #255,using L970: hex$("1B2D01")
return ! /r
include: ertn
