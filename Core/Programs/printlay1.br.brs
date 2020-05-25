! Replace S:\Core\Programs\PrintLay1
! pr Several Layouts
 
	autoLibrary
	on error goto Ertn
 
	dim a$(200,3)*80,h1$*55,rm$(4)*100,filename$*50,fil$(50)*80,ln$*120
	dim ev$*50
	dim a(200,6),a$*132,prg$*30,mo$(12),cap$*128
 
	fnTop("S:\Core\PrintLay",cap$="Print Several Layouts")
	data January,February,March,April,May,June,July,August,September,October,November,December
	read mat mo$
 
	fnconsole(on=1)
	dat$=mo$(val(date$("MM")))&" "&date$("DD")&", "&date$("CCYY")
	fnwin3b(win=101,cap$,4,58,0,2)
! pr #WIN,Fields "2,2,Cr 41,N": "0.Printer; 1.Screen; 2.Both:"
	pr #win,fields "3,2,Cr 41,N": "Ext/VolId to pr all (blank to select):"
	io1$(1)="2,44,N 1,U,N"
	io1$(2)="3,44,14/c 50,U,N"
	ev$="lay/S:\acsTM\Layouts"
L220: rinput #win,fields io1$(2): ev$ conv CONV1 ! pp was on io1$(1)
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L310 else ce=curfld
L250: ce=ce+1: if ce>udim(io1$) then ce=1
L260: io1$(ce)=rtrm$(io1$(ce)) : ce1=pos(io1$(ce),"U",1) : _
	if ce1=0 then goto L250
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L220
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L260
L310: if cmdkey=5 then goto Xit
	if pp<0 or pp>2 then ce=1 : goto ERR1
	if pp=1 then goto L350
	fnopenprn(cp,58,220,process)
L350: ev$=rtrm$(ev$)
	if ev$="" then goto L430
	execute "DROP DirFile" ioerr L380
L380: execute "DIR *."&ev$&" >DirFile"
	open #2: "Name=DirFile",display,input
	p1=pos(ev$,"/",1)
	ex$=ev$(1:p1-1)
	goto L530
L430: close #101: ioerr L440
L440: open #101: "SROW=2,SCOL=4,EROW=6,ECOL=79,BORDER=DR,CAPTION="&cap$,display,outIn
	pr #101: newpage
	if f1>0 then pr f "2,5,C 60,H,N": "LAST FILE NAME ENTER WAS "&fil$(f1)
	pr f "4,5,C 60,N": "Enter File Name/VolId to pr or Blank to stop"
	rinput fields "4,55,C 20,UE,N": fil$(f1+1)
	fil$(f1+1)=rtrm$(fil$(f1+1))
	if pp>0 or fil$(f1+1)="" then goto L530
	f1=f1+1
	goto L430
L530: f2=f2+1: rl=l=a=b=ino=pg=j3=0: mat a=(0)
	if ev$="" then goto L590
L550: linput #2: a$ eof L1600
	if uprc$(rtrm$(a$(11:13)))><uprc$(ex$) then goto L550
	fil$(1)=rtrm$(a$(1:8))&"."&ev$
	f2=1: f1=2
L590: if rtrm$(fil$(f2))="" then goto L1600
	open #1: "Name="&fil$(f2),display,input ioerr L530
	goto L830
 
L630: p1=pos(filename$,".",1)
	if p1=0 then p1=min(8,len(filename$)) else p1=p1-1
	pr newpage
	pr f "1,1,C 40,R,N": " Field Descriptions: "&filename$(1:p1)&"/"&volid$
	pr f "1,43,C 8,R,N": " Name"
	pr f "1,53,C 8,R,N": " Format"
	pr f "1,63,C 5,R,N": " From"
	pr f "1,70,C 5,R,N": "  To"
	sln=1
L720: if pp=0 or j3=0 then goto L830
	if sln=0 then goto L630
	if sln<23 then sln=sln+1: goto L780
	pr f "24,5,C 60,R,N": "  SCREEN FULL  PRESS ENTER TO CONTINUE:"
	input fields "24,50,C 1,RE,N": pause$
	goto L630
L780: pr f str$(sln)&",1,C 40,U,N": a$(j3,1)
	pr f str$(sln)&",43,C 8,U,N": a$(j3,2)(1:10)
	pr f str$(sln)&",53,C 8,U,N": a$(j3,3)(1:8)
	pr f str$(sln)&",63,PIC(ZZZZZ),U,N": a(j3,5)
	pr f str$(sln)&",70,PIC(ZZZZZ),U,N": a(j3,6)
L830: linput #1: ln$ eof L1410
	if uprc$(ln$(7:10))=uprc$("LET ") then goto LETLN
	if uprc$(ln$(7:10))=uprc$("DATA") then goto DATALN
! If UPRC$(LN$(7:7))=UPRC$("!") Then pr #255: LN$(9:LEN(LN$))
	p1=pos(ln$,"REPLACE",6)
	if p1=0 then goto L920
	p1=p1+8
	p2=pos(ln$,",",p1)
	prg$=ln$(p1:p2-1)
L920: goto L720
 
LETLN: p2=len(rtrm$(ln$))-1
	p1=pos(uprc$(ln$),"H1$",1)
	if p1>0 then h1$=ln$(p1+5:p2) : goto L720
	p1=pos(uprc$(ln$),"FILETYPE$",1)
	if p1>0 then filetype$=ln$(p1+11:p2): goto L720
	p1=pos(uprc$(ln$),"FILENAME$",1)
	if p1>0 then filename$=ln$(p1+11:p2): goto L720
	p1=pos(uprc$(ln$),"VOLID$",1)
	if p1>0 then volid$=ln$(p1+8:p2): goto L720
	p1=pos(uprc$(ln$),"RM$",1)
	if p1=0 then goto L1070
	rm=val(ln$(p1+4:p1+4))
	rm$(rm)=ln$(p1+8:p2)
L1070: goto L720
 
DATALN: j3=j3+1
	p1=11
	p2=pos(srep$(ln$,'^','~'),'~',p1+1) ! pos(ln$,"^",p1+1)
	p3=pos(srep$(ln$,'^','~'),'~',p2+1) ! pos(ln$,"^",p2+1)
	p4=len(rtrm$(ln$))
	a$(j3,1)=ln$(p1:p2-1)
	a$(j3,2)=ln$(p2+1:p3-1)
	a$(j3,3)=ln$(p3+1:p4)
	form c 9,skip 0
	if rtrm$(a$(j3,3))="" then goto L1390
	p1=pos(a$(j3,3)," ",1)+1
	p2=pos(a$(j3,3),".",1)+1
	p3=len(rtrm$(a$(j3,3)))
	p4=pos(a$(j3,3),"*",1)
	if p4=0 then m1=1 else m1=val(a$(j3,3)(1:p4-1))
	l=int(val(a$(j3,3)(p1:min(pos(srep$(a$(j3,3),'^','~'),'~')-1,pos(a$(j3,3),chr$(9))-1)))) ! FIELD STORAGE LENGTH : _
	! min(pos(srep$(a$(j3,3),'^','~'),'~')-1,POS(A$(J3,3),chr$(9))-1)         was      P3
	if p2>1 then dp=val(a$(j3,3)(p2:min(pos(srep$(a$(j3,3),'^','~'),'~')-1,pos(a$(j3,3),chr$(9))-1))) else dp=0 ! DECIMAL POS. : _
		! min(pos(srep$(a$(j3,3),'^','~'),'~')-1,POS(A$(J3,3),chr$(9))-1)      was     P3
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
	rl=rl+int(val(a$(j3,3)(p1:min(pos(srep$(a$(j3,3),'^','~'),'~')-1,pos(a$(j3,3),chr$(9))-1))))*m1 : _
	! min(pos(srep$(a$(j3,3),'^','~'),'~')-1,POS(A$(J3,3),chr$(9))-1)      was    P3
L1390: goto L720
 
L1410: pgo=ceil(j3/24)
	if pp=1 then goto L1580
	gosub HDR
	for j=1 to j3
		p1=pos(a$(j,3)," ",1)
		p2=len(a$(j,3))
		l=val(a$(j,3)(p1:min(pos(srep$(a$(j,3),'^','~'),'~')-1,pos(a$(j,3),chr$(9))-1))) ! min(pos(srep$(a$(j,3),'^','~'),'~')-1,POS(A$(J,3),chr$(9))-1)   was    P2
		if l>0 then goto L1520
		pr #255,using L1500: a$(j,1)(1:43) ! Pageoflow NEWPGE
L1500: form pos 13,c 43,skip 2
		goto L1550
L1520: if rtrm$(a$(j,1))="" then goto L1550
		a$(j,3)=a$(j,3)(1:pos(a$(j,3),'^')-1)
		pr #255,using 'Form POS 5,N 5,X 3,C 43,C 21,N 7,N 10,X 5,C 11,N 7,2*N 9': a(j,1),a$(j,1)(1:43),a$(j,2),a(j,2),a(j,3),a$(j,3),a(j,4),a(j,5),a(j,6) ! Pageoflow NEWPGE
L1550: next j
! pr #255: NEWPAGE
! pr #255,Using 1180: HEX$("1B40")
L1580: close #1:
	if f2<f1 then goto L530
L1600: close #2: ioerr L1620
	fncloseprn
L1620: goto Xit
 
Xit: fnXit
 
NEWPGE: if j=j3 then goto L1690
! pr #255: NEWPAGE
	gosub HDR
L1690: continue
 
HDR: !
	pr #255,using 'Form POS 1,C 80': "{\b{\qc{\fs28 "&h1$&"}}}"
! pr #255,Using 'Form POS 5,"Record Length: ",C 5,POS 30,C 45': PRG$,DAT$,PG+=1,PGO
! Form POS 51,C 40,C 20,"Page ",N 3,"  of ",N 3
	pr #255,using 'Form POS 5,"Record Length: ",C 5,POS 30,C 100': str$(rl),rm$(1)
	if trim$(rm$(2))<>'' then pr #255,using 'Form POS 30,C 100': rm$(2)
	if trim$(rm$(3))<>'' then pr #255,using 'Form POS 30,C 100': rm$(3)
	pr #255,using 'Form POS 5,"File Type: ",C 10,POS 30,C 100': filetype$,rm$(4)
	pr #255,using 'Form POS 5,"File Name: ",C 50,"Directory: ",C 10': filename$,volid$
	pr #255: ""
	pr #255: "   Item     Field Description                           Name                 Length    Decimals    Format    Storage     From      To  "
	pr #255: "   ______   ________________________________________   __________________    ______    ________   _______    _______    _____    _____"
return
 
include: Ertn
 
