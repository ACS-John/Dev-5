! formerly S:\acsGL\Employee
! GL Payroll File Menu
 
	autoLibrary
	on error goto Ertn
 
	dim fl2$(5),sc2$(5)*38
	dim sc1$(26),fl1$(26),io1$(41),hd$(2)*78,pr1(21),miscname$(10)*20
	dim l1(4),l2$(4)*25,l3$(4)*25,sc3$(21)*13,fl3$(21),io3$(21),prd(21)
	dim l4$(4)*25,l5$(4)*11,mp(4,36),dat$*20,pr$(21),td$*25
	dim k$(3)*25,ss$*11,m(36),adr(2),d(14),ext(2),en$*4,ta(2)
	dim sr$(16),en$(16),resp$(50)*35,ml$(4)*80
	dim iocr$(3),wrdcr$(3)*23
	dim p$(20)*50,cap$*128,dedcode(10)
 
	fnTop(program$,cap$="Employee")
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input  : _
	read #1,using 'form pos 418,10*C 20,10*N 1': mat miscname$,mat dedcode : _
	close #1:
 
	sc1$(1)="Description:" : _
	sc1$(2)="  Y.T.D." : _
	sc1$(3)="  Q.T.D." : _
	sc1$(4)="Employee #:" : _
	sc1$(5)="Name F/M/L:"
	sc1$(6)="Address:" : _
	sc1$(7)="City St Zip:" : _
	sc1$(8)="Soc-Sec-#:" : _
	sc1$(9)="Gross Wage:" : _
	sc1$(10)="Fed W/H:"
	sc1$(11)="FICA W/H:" : _
	sc1$(12)="State W/H:" : _
	sc1$(13)="Local W/H:"
	for j=1 to 10: sc1$(j+13)=rtrm$(miscname$(j)(1:12))&":" : next j
	sc1$(24)="Tips:" : _
	sc1$(25)="Weeks Worked:" : _
	sc1$(26)="EIC:"
	sc3$(1)="Check Date:" : _
	sc3$(2)="Check #:"
	sc3$(21)="Net Pay:"
	if exists ("[Q]\GLmstr\PRmstr.h[cno]") =0 then goto INITIAL_BUILD
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",i,outIn,k ioerr L2900
	open #2: "Name=[Q]\GLmstr\ACPRCKS.h[cno],Shr",i,outi,r
	goto MAIN
 
DONE: !
	close #1:
	close #2:
	if new1=1 or cont=1 then goto L1410
	goto Xit
 
MAIN: !
	fnTos(sn$="Payroll") : _
	mylen=20: mypos=mylen+3 : right=1
	fnemployee_search(x$,99)
! fnLbl(1,1,"Employee Number:",MYLEN,RIGHT)
! fnCOMBOF("PRmstr",1,MYPOS,27,"[Q]\GLmstr\PRmstr.h[cno]",1,4,5,30,'',0,PAS, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0) : _
	resp$(1)=str$(eno)
	fnCmdKey("E&dit",1,1,0,"")
	fnCmdKey("&Add",2,0,0,"")
	fnCmdKey("&Proof Llist",4,0,0,"")
	fnCmdKey("&Cancel",5,0,1,"")
	ckey=fnAcs(mat resp$)
L480: if ckey=5 then goto Xit
	if ckey=2 then goto ASK_NEW_NUMBER
	if ckey=4 then goto PROOF_LIST
	holden1=en1=val(resp$(1)(1:4))
	goto DISPLAY_RECORD
ASK_NEW_NUMBER: !  add new employee
	eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0)
	fnTos(sn$="Payroll2") : _
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,1,"Employee Number:",mylen,right)
	fnTxt(1,mypos,4,0,0,'30',0,"Enter new employee number.",0 ) : _
	resp$(1)=""
	fnCmdKey("&Next",1,1,0,"")
	fnCmdKey("&Cancel",5,0,1,"")
	ckey=fnAcs(mat resp$)
	holden1=en1=val(resp$(1))
	if en1=0 then goto MAIN
	addemployee=1 ! code for adding new employee
DISPLAY_RECORD: !
	en$=lpad$(str$(en1),4)
	read #1,using 'form pos 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5',key=en$: eno,mat k$,ss$,mat m,mat ta nokey DISPLAY_EMPLOYEE
	disable=1: goto DISPLAY_EMPLOYEE
DISPLAY_EMPLOYEE: !
	fnTos(sn$="Payroll3") : _
	mylen=15: mypos=mylen+3 : right=1
	fnLbl(1,1,"Employee Number:",mylen,right)
	fnTxt(1,mypos,4,0,0,'30',disable,"",0 ) : _
	resp$(1)=str$(en1)
	fnLbl(2,1,"Employee Name:",mylen,right)
	fnTxt(2,mypos,25,0,0,"",0,"",0 ) : _
	resp$(2)=k$(1)
	fnLbl(3,1,"Address:",mylen,right)
	fnTxt(3,mypos,25,0,0,"",0,"",0 ) : _
	resp$(3)=k$(2)
	fnLbl(4,1,"City, St Zip:",mylen,right)
	fnTxt(4,mypos,25,0,0,"",0,"",0 ) : _
	resp$(4)=k$(3)
	fnLbl(5,1,"Social Security:",mylen,right)
	fnTxt(5,mypos,11,0,0,"",0,"",0 ) : _
	resp$(5)=ss$
	mylen2=20 : mypos=mylen+50
	fnLbl(1,70,"Y T D         Q T D ",24,0)
	for j=1 to 18
		fnLbl(j+1,42,sc1$(j+8),mylen2,right)
		fnTxt(j+1,mypos,12,0,0,'10',0,"",0 ) : _
		resp$(j*2-1+5)=str$(m(j*2-1) )
		fnTxt(j+1,mypos+14,12,0,0,'10',0,"",0 ) : _
		resp$(j*2+5)=str$(m(j*2) )
	next j
	fnCmdKey("&Next",1,1,0,"")
	fnCmdKey("W-2 Suplimental",2,0,0,"W-2 Suplimental Information")
	fnCmdKey("&Review Checks",3,0,0,"")
	fnCmdKey("&Add Check",8,0,0,"")
	fnCmdKey("&Change Number",7,0,0,"")
	fnCmdKey("&Delete",6,0,0,"")
	fnCmdKey("&Cancel",5,0,1,"")
	ckey=fnAcs(mat resp$)
	disable=1
	if ckey=5 then goto MAIN
	if ckey=2 then let fnW2supEdit(resp$(1)) : goto DISPLAY_EMPLOYEE
	if ckey=6 then goto DELETEIT
	if ckey=3 then add=0: goto REVIEW_CHECKS
	if ckey=7 then disable=0: goto DISPLAY_EMPLOYEE
	en1=eno=val(resp$(1))
	if ckey=8 then add=1: mat prd=(0): goto L2340
	k$(1)=resp$(2)
	k$(2)=resp$(3)
	k$(3)=resp$(4)
	ss$=resp$(5)
	for j=1 to 36
		m(j)=val(resp$(j+5))
	next j
	if ckey=1 and holden1<>en1 then goto MSGBOX2
	goto L1250
DELETEIT: !
MSGBOX1: !
	mat ml$(3) : _
	ml$(1)="You have chosen to delete employee " : _
	ml$(2)="number "&str$(eno)&".  Click OK to delete" : _
	ml$(3)="this record or Cancel to retain the record." : _
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L1150 else goto MAIN
MSGBOX2: !
	mat ml$(3) : _
	ml$(1)="You are attempting to change the employee" : _
	ml$(2)="number from "&str$(holden1)&" to "&str$(eno)&".  Click OK to change" : _
	ml$(3)="the number or Cancel to retain the old number." : _
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L1170 else goto MAIN
L1150: delete #1,key=lpad$(str$(en1),4): nokey MAIN
! delete or change numbers
L1170: adr=ta(1)
L1180: if adr=0 then goto L1240
	read #2,using L1200,rec=adr: en1,nta
L1200: form pos 1,n 4,pos 108,pd 3
	if ckey=6 then delete #2,rec=adr: else rewrite #2,using L1200,rec=adr: eno,nta
	adr=nta
	goto L1180
L1240: if ckey=6 then eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0) : _
		goto MAIN
L1250: if ckey=1 and addemployee=1 then mat ta=(0): write #1,using 'form pos 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m,mat ta: addemployee=0 else goto L1280
	new1=1
	eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0): goto MAIN
L1280: if ckey=1 then rewrite #1,using 'form pos 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5',key=en$: eno,mat k$,ss$,mat m,mat ta nokey L1290
L1290: eno=0: mat k$=(""): ss$="": mat m=(0): mat ta=(0): goto MAIN
 
INITIAL_BUILD: !
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno]",internal,output ioerr L1330
L1330: close #1,free: ioerr L1340
L1340: open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],SIZE=0,RecL=280,Replace",internal,output
	close #2: ioerr ignore
	fnFree("[Q]\GLmstr\PRIndex.h[cno]")
L1370: open #2: "Name=[Q]\GLmstr\ACPRCKS.h[cno]",internal,output ioerr L1380
L1380: close #2,free: ioerr L1390
L1390: open #2: "Name=[Q]\GLmstr\ACPRCKS.h[cno],SIZE=0,RecL=110,Replace",internal,output,relative
	close #2:
L1410: close #1: ioerr L1420
L1420: execute "Index [Q]\GLmstr\PRmstr.h[cno]"&' '&"[Q]\GLmstr\PRIndex.h[cno] 1 4 Replace DupKeys -n"
	goto MAIN
 
PROOF_LIST: !
	restore #1,key>="    ": eof L1470, nokey L1470
L1470: fnopenprn
	gosub HDR
L1490: read #1,using 'form pos 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m eof L1610
	pl=pl+1
	l1(pl)=eno
	l2$(pl)=k$(1)
	l3$(pl)=k$(2)
	l4$(pl)=k$(3)
	l5$(pl)=ss$
	for j1=1 to 36
		mp(pl,j1)=m(j1)
	next j1
	if pl=4 then gosub L1660
	goto L1490
L1610: if pl>0 then gosub L1660
	on fkey 5 ignore
	fncloseprn
	if fnprocess=1 then goto Xit else goto MAIN
 
L1660: pr #255,using L1670: sc1$(4),mat l1
L1670: form pos 1,c 21,x 7,pic(zzzz),x 24,pic(zzzz),x 24,pic(zzzz),x 24,pic(zzzz),skip 1
	pr #255,using L1690: sc1$(5),mat l2$
L1690: form pos 1,c 21,3*c 28,c 25,skip 1
	pr #255,using L1690: sc1$(6),mat l3$
	pr #255,using L1690: sc1$(7),mat l4$
	pr #255,using L1690: sc1$(8),mat l5$
	for j1=1 to 36
		j2=int((j1-1)/2)+9
		if fp(j1/2)=0 then sc1$=sc1$(j2)&"QTD" else sc1$=sc1$(j2)&"YTD"
		pr #255,using L1770: sc1$,mp(1,j1),mp(2,j1),mp(3,j1),mp(4,j1)
L1770: form pos 1,c 21,pic(---------.##),pic(-------------------------.##),pic(-------------------------.##),pic(------------------------.##),skip 1
	next j1
	mat l1=(0)
	mat l2$=("")
	mat l3$=("")
	mat l4$=("")
	mat l5$=("")
	mat mp=(0)
	pl1=pl1+1
	if pl1=2 then goto L1900
	pr #255,using L1880: " "
L1880: form c 1,skip 4
	goto L1940
L1900: pl1=0
	pr #255: newpage
	if pl><4 then goto L1940
	gosub HDR
L1940: pl=0
return
 
HDR: !
	pr #255,using L1990: date$('mm/dd/yy'),env$('cnam')
L1990: form skip 2,pos 1,c 8,pos 1,cc 108,skip 1
	pr #255,using L2010: time$,"Payroll Proof List",dat$
L2010: form pos 1,c 8,pos 45,c 20,skip 1,pos 1,cc 108,skip 2
return
 
! pr NEWPAGE
! pr f "10,15,Cc 43,N": "Reassigning Transaction Addresses..."
	restore #1,key>="    ": eof L2070
L2070: read #1,using 'form pos 271,2*N 5': mat ta eof L2100
	rewrite #1,using 'form pos 271,2*N 5': 0,0
	goto L2070
L2100: lr2=lrec(2)
! REWRITE #2,USING 2360,REC=1: LR2
	for j=1 to lr2
		read #2,using 'form pos 1,C 4,pos 108,PD 3',rec=j: en$,nta noRec L2210
		read #1,using 'form pos 271,2*N 5',key=en$: mat ta nokey L2210
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #2,using L2200,rec=ta(2): j
		ta(2)=j
		rewrite #1,using 'form pos 271,2*N 5',key=en$: mat ta
		rewrite #2,using L2200,rec=j: 0
L2200: form pos 108,pd 3
L2210: next j
	goto MAIN
 
REVIEW_CHECKS: !
	if ta(1)=0 then goto MSGBOX5 else goto L2290
MSGBOX5: !
	mat ml$(3) : _
	ml$(1)="There are no checks on employee # "&str$(eno)&"." : _
	ml$(2)="Do you wish to add checks?" : _
	fnmsgbox(mat ml$,resp$,cap$,35)
	if resp$="Yes" then add=1: goto L2340 else goto MAIN
L2290: adr=ta(1)
L2300: if adr=0 then goto MAIN
	read #2,using L2320,rec=adr: en2,mat prd,nca noRec L480
L2320: form pos 1,n 4,2*pd 4,19*pd 5.2,pd 3
L2330: mat pr1=prd
L2340: fnTos(sn$="Payroll4") : _
	mylen=15: mypos=mylen+3 : right=1
	fnLbl(1,1,"Check Date:",mylen,right)
	fnTxt(1,mypos,1,0,0,"1",0,"Date of check.",0 ) : _
	resp$(1)=str$(prd(1))
	fnLbl(2,1,"Check Number:",mylen,right)
	fnTxt(2,mypos,8,0,0,'30',0,"",0 ) : _
	resp$(2)=str$(prd(2))
	fnLbl(3,1,"Gross Wage:",mylen,right)
	fnTxt(3,mypos,12,0,0,'10',0,"",0 ) : _
	resp$(3)=str$(prd(3))
	for j=1 to 17
		fnLbl(j+3,1,sc1$(j+9),mylen,right)
		fnTxt(j+3,mypos,12,0,0,'10',0,"",0 ) : _
		resp$(j+3)=str$(prd(j+3))
	next j
	fnLbl(21,1,"Net Pay:",mylen,right)
	fnTxt(21,mypos,12,0,0,'10',0,"",0 ) : _
	resp$(21)=str$(prd(21))
	fnCmdKey("&Next",1,1,0,"")
	fnCmdKey("&Cancel",5,0,1,"")
	ckey=fnAcs(mat resp$)
	if ckey=5 then add=0: goto MAIN
	for j=1 to 21
		prd(j)=val(resp$(j))
	next j
	wh=0
	for j=3 to 21
		if j=3 then goto L2650 ! gross
		if j<8 then wh=wh+prd(j) ! fed,fica,state,local
		if j>7 and j<18 and dedcode(j-7)=2 then wh=wh-prd(j)
		if j>7 and j<18 and dedcode(j-7)<>2 then wh=wh+prd(j)
		if j=18 then wh=wh+prd(18) ! tips
		if j=19 then goto L2650 ! weeks worked
		if j=20 then wh=wh-prd(j) ! eic
L2650: next j
	if prd(3)<>prd(21)+wh then goto MSGBOX4 else goto L2700
MSGBOX4: !
	mat ml$(4) : _
	ml$(1)="Gross pay ("&trim$(cnvrt$("pic(----,---.##)",prd(3)))&") less withholding " : _
	ml$(2)="("&trim$(cnvrt$("pic(----,---.##)",wh))&") does not equal" : _
	ml$(3)="the net check ("&trim$(cnvrt$("pic(----,---.##)",prd(21)))&")" : _
	ml$(4)="Click OK to fix the check." : _
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L2340 else goto MAIN
L2700: lr2=lrec(2)+1
	if add=1 then write #2,using L2320,rec=lr2: eno,mat prd,0 duprec L2700 else rewrite #2,using L2320,rec=adr: eno,mat prd,nca
	if add=0 then goto L2770
	if ta(2)>0 then rewrite #2,using L2740,rec=ta(2): lr2
L2740: form pos 108,pd 3
	if ta(1)=0 then ta(1)=lr2
	ta(2)=lr2
L2770: if add=0 then mat prd=prd-pr1 ! file maintenance
	for j=3 to 20
		if j=3 then m1=1
		if j=4 then m1=3
		if j=5 then m1=5
		if j>5 then m1=(j-5)*2+5
		m(m1)=m(m1)+prd(j)
		m(m1+1)=m(m1+1)+prd(j)
	next j
	rewrite #1,using 'form pos 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5',key=en$: eno,mat k$,ss$,mat m,mat ta
	adr=nca
	if add=1 then mat pr1=(0): mat prd=(0): goto L2330
	if add=0 then goto L2300
 
L2900: if err=4152 then goto INITIAL_BUILD else goto ERTN
Xit: fnXit
 
 
include: ertn
