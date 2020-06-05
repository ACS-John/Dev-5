! formerly S:\acsPR\newprPostGL
! Payroll Post to General Ledger
! r: setup library, on err, dims, fnTop, etc
	autoLibrary
	on error goto Ertn
 
	dim a$*40,em$*30,tgl(3),tcp(32),eno$*8,ttgl(3),oldtgl(3)
	dim tr(7),tr$*12,td$*30,dat$*20,a(100),i$*21,glwk$*30,desc$*50
	dim tgl$*12,oldtgl$*12
	dim t(26),prgl(26,3),prgl$(15)*12,dedcode(20)
	dim message$*40,msgline$(2)*60,ml$(4)*80,resp$(10)*60
	dim fullname$(20)*20,abbrevname$(20)*8,newcalcode(20),newdedfed(20),dedfica(20)
	dim dedst(20),deduc(20),gl$(20)*12
 
	fnTop(program$)
	fnIndex('[Q]\PRmstr\Department.h[cno]','[Q]\PRmstr\DeptId4.h[cno]','12/1/9 12/8/3') ! sort department file in general ledger sequence
	fnStatusClose
	fnopenprn
 
	open #20: "Name=[Q]\GLmstr\GLBucket.h[cno],Shr",internal,input,relative ioerr L260
	read #20,using 'Form POS 1,N 1',rec=1: glb noRec ignore
	close #20:
	if glb=2 then let fn_askaccrue
	L260: !
 
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
	open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,input,keyed
	d1=fnPayPeriodEndingDate
	if fnclient_has('CL') then ! exists("[Q]\CLmstr") then
		mat ml$(3)
		ml$(1)="Normally you would not take this menu option to post"
		ml$(2)="General Ledger if you have the Checkbook system."
		ml$(3)="Click OK to continue or Cancel to stop."
		fnmsgbox(mat ml$,resp$,'',1)
		if resp$="OK" then goto ASK_DATE else goto Xit
	end if
goto ASK_DATE ! /r
ASK_DATE: !
	fnTos(sn$="PostGl")
	respc=0
	fnLbl(1,40,"",1,1) ! bigger screen
	fnLbl(1,1,"Beginning Payroll Date:",25,1)
	fnTxt(1,28,10,0,1,"3",0,"For current payroll, always use the calculation date.  You can post or re-post older payrolls by using the older payroll date.")
	resp$(respc+=1)=str$(d1)
	fnLbl(2,1,"Ending Payroll Date:",25,1)
	fnTxt(2,28,10,0,1,"3",0,"You can post a ranges of payrolls by entering a beginning and ending date.  Use the same date for a single payroll.")
	resp$(respc+=1)=str$(d1)
	fnChk(3,28,"Print Report Only:",1)
	resp$(respc+=1)="False"
	fnCmdKey("&Next",1,1,0,"Proceed with posting." )
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs2(mat resp$,ckey) ! ask payroll date
	if ckey=5 then goto Xit
	dat1=d1=val(resp$(1))
	dat2=d2=val(resp$(2))
	if resp$(3)="True" then skipposting=1
	if glb=2 then glwk$="[Q]\GLmstr\GL"&date$(days(dat1,'ccyymmdd'),'mmddyy')&".h[cno]"
	if glb><2 then glwk$="[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]"
	if glb=2 and accrue$="Yes" then
		open #11: "Name=[Q]\GLmstr\GL"&date$(days(d2,'ccyymmdd'),'mmddyy')&".h[cno],RecL=104,Use",internal,output
	end if
!
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
	read #1,using 'Form POS 1,C 40,POS 437,15*C 12,N 1,POS 618,10*N 1': a$,mat prgl$,glinstal ! need to get from other file
	for j=1 to 4 ! 1=fed 2=fica/med 3=med 4=state
		if j=3 then
			prgl(j,1)=val(prgl$(2)(1:3))
			prgl(j,2)=val(prgl$(2)(4:9))
			prgl(j,3)=val(prgl$(2)(10:12))
		else
			prgl(j,1)=val(prgl$(j)(1:3))
			prgl(j,2)=val(prgl$(j)(4:9))
			prgl(j,3)=val(prgl$(j)(10:12))
		end if
	next j
	for j=5 to 24
		prgl(j,1)=val(gl$(j-4)(1:3))
		prgl(j,2)=val(gl$(j-4)(4:9))
		prgl(j,3)=val(gl$(j-4)(10:12))
	next j
	j=25: prgl(j,1)=val(prgl$(14)(1:3))
	prgl(j,2)=val(prgl$(14)(4:9))
	prgl(j,3)=val(prgl$(14)(10:12)) ! eic
	j=26: prgl(j,1)=val(prgl$(15)(1:3))
	prgl(j,2)=val(prgl$(15)(4:9))
	prgl(j,3)=val(prgl$(15)(10:12)) ! cash
	nametab=36-len(rtrm$(a$))/2
	close #1:
	open #2: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
	open #6: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptId4.h[cno],Shr",internal,outIn,keyed
	fn_pr_hdr
	do
		DEPT_READ: !
		mat oldtgl=tgl
		read #6,using "Form POS 1,N 8,POS 12,n 3,n 6,n 3,pos 42,n 6": teno,mat tgl,paydat eof L1400
		! If fndate_mmddyy_to_ccyymmdd(PAYDAT)<DAT1 OR fndate_mmddyy_to_ccyymmdd(PAYDAT)>DAT2 Then Goto 770 ! payroll date in department record must match
		oldtgl$=tgl$
		mat ttgl=oldtgl
		oldteno=teno
		checkkey$=cnvrt$("pic(zzzzzzzz)",teno)&"         "
		restore #4,key>=checkkey$: nokey DEPT_READ
		L840: !
		read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prdate,ckno,mat tdc,mat tcp eof DEPT_READ
		if heno<>teno then goto DEPT_READ
		if prdate<dat1 or prdate>dat2 then goto L840
		! If SUM(TGL)=0 Then Goto 500
		if mastercd=0 then let fn_l1800
		tgl$=lpad$(str$(tgl(1)),3)&lpad$(str$(tgl(2)),6)&lpad$(str$(tgl(3)),3)
		if oldtgl$=tgl$ or oldtgl$="" then goto L920
		fn_write_gl_trans
		L920: !
		if tgl(1)=0 or tgl(1)=oldtgl then goto L930 else let fn_l1800 ! No Cost Center or same Cost Center
		L930: ! If OLDTENO=TENO Then Goto 830
		eno$=lpad$(str$(teno),8)
		read #2,using L960,key=eno$: em$ nokey DEPT_READ
		L960: form pos 9,c 30
		pr #255,using L980: teno,em$,mat tgl,tcp(31) ! -TCP(29)-TCP(30) Pageoflow PR_HDR kj
		L980: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),n 12.2,skip 1
		for j=1 to 24 ! ACCUMULATE 20 WITHHOLDINGS plus fed,fica,med,state
			if j<=4 then goto L1040
			if dedcode(j-4)=1 then goto L1040
			t(j)=t(j)+tcp(j)
			goto L1050
			L1040: !
			t(j)=t(j)-tcp(j)
			L1050: !
		next j
		t(25)=t(25)+tcp(25) ! EIC
		t(26)=t(26)-tcp(32) ! ACCUMULATE NET
		subtotal=subtotal+tcp(31) ! -TCP(19)-TCP(20)
		! accumulate total by acct to be posted to gl kj
		totaldue=totaldue-tcp(31) ! +TCP(29)+TCP(30) ! DUE TO PAYROLL CLEARING kj
		totaldr=totaldr+tcp(31) ! -TCP(29)-TCP(30)  ! kj
		totalrec=totalrec+tcp(31) ! -TCP(29)-TCP(30) ! TOTAL DUE FROM OTHER FUNDS  kj
	loop
	
L1400: ! r:
	eofcode=1
	mat ttgl=tgl
	oldtgl$=tgl$
	fn_write_gl_trans ! WRITE LAST ENTRY
	if multigl=1 then ! ONLY ONE FUND OR COMPANY
		fn_l1800
		fn_finalscrctrlbookmulitfunds
	end if
	fn_assglnum ! POST WH AND NET
	fn_printtotalsandunderlines
	close #1: ioerr ignore
	close #2: ioerr ignore
	close #4: ioerr ignore
	!
	fncloseprn
	if ~skipposting=1 and glinstal and glb<>2 then
		fnchain("S:\acsGL\ACGLMRGE")
	end if
goto Xit ! /r
PgOf: !  r:
	pr #255: newpage
	fn_pr_hdr
continue ! /r
def fn_write_gl_trans ! SUBTOTAL ROUTINE AND WRITE GL TRANS
	if glinstal then
	if diskin=0 then let fn_L2090
	td$="Payroll summary"
	if accrue$="Yes" then
		accrued=round(subtotal/day*dayslm,2)
		write #11,using L1220: mat ttgl,date(days(d2,'ccyymmdd'),'mmddyy'),accrued,5,0,tr$,"Payroll Accrual",prgl$(15)
		totacc=totacc+accrued
	end if
	write #14,using L1220: mat ttgl,dat,subtotal-accrued,5,0,tr$,"Payroll Summary",prgl$(15) ! gross wages for this department
	L1220: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 52,c 12
	end if
	pr #255,using L1240: "-----------",subtotal
	L1240: form pos 65,c 11,skip 1,pos 64,pic(---------.##),skip 1
	if accrued<>0 then pr #255,using L1260: "Accrued Portion",-accrued else pr #255:
	L1260: form pos 45,c 16,pos 64,pic(---------.##),skip 2
	subtotal=0
fnend
def fn_pr_hdr
	p1=p1+1
	pr #255,using L1320: date$,a$,"PAGE",p1
	L1320: form skip 1,pos 1,c 8,pos nametab,c 40,pos 77,c 5,pic(zzz),skip 1
	pr #255,using L1340: time$, "General Ledger Distribution for Payroll","From "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dat1)&"  To "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dat2)
	L1340: form pos 1,c 8,pos 17,c 40,skip 1,pos 17,cc 40,skip 2
	pr #255: "Employee                                               G/L                 Amount"
	pr #255: " Number        Name                                  Account         Debits     Credits"
	pr #255:
fnend
def fn_assglnum ! assign g/l numbers and post to gl work file
	for j=1 to 26
		if t(j)=0 then goto L1740
		if t(j)<0 then goto L1690
		pr #255,using L980: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),t(j) pageoflow PgOf
		totaldr=totaldr+t(j)
		goto L1720
		L1690: !
		pr #255,using L1700: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),t(j) pageoflow PgOf
		L1700: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),x 12,n 12.2,skip 1
		totalcr=totalcr+t(j)
		L1720: !
		if glinstal=0 then goto L1740
		write #14,using L1220: prgl(j,1),prgl(j,2),prgl(j,3),dat,t(j),5,0,tr$,td$,prgl$(15) ! write summary entries for deductions and net pay
		L1740: !
	next j
	if accrue$<>"Yes" then goto L1780
	write #11,using L1220: g1,g2,g3,date(days(d2,'ccyymmdd'),'mmddyy'),-totacc,5,0,tr$,"Payroll Accrual",prgl$(15)
	write #14,using L1220: g1,g2,g3,dat,totacc,5,0,tr$,"Payroll Accrual",prgl$(15)
	L1780: !
fnend
def fn_l1800 ! OPEN G/L WORK FILES AND CREATE DUE TO AND DUE FROM ENTRIES
	if tgl(1)=0 then goto L2080
	if mastercd=1 then goto L1870
	mat ml$(4)
	ml$(1)="The G/L accounts you are using indicate you have seperate funds or"
	ml$(2)="cost centers on the system.  Click Yes if you do have more than."
	ml$(3)="one set of books that are self balancing in your system."
	ml$(4)="Click NO if you are using the cost center code for other purposes."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then multigl=1 else multigl=2
	if multigl><1 then goto L1870
	mulitidsk=1
	L1870: !
	if multigl=2 then goto L2080
	! CREATE DUE TO PAYROLL FUND ENTRIES
	if mastercd=0 then goto L2050 ! FIRST TIME THRU ROUTINE
	fnTos(sn$="PostGl3")
	respc=0: mypos=45
	fnLbl(1,1,"Due to Payroll Clearing Account on Fund # "&oldtgl$(1:3)&":",mypos,1)
	fnqgl(1,mypos+3,0,2,pas)
	resp$(1)=fnrgl$(bankgl$)
	fnCmdKey("&Next",1,1,0,"Continue posting." )
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs2(mat resp$,ckey) ! ask clearing
	if ckey=5 then goto Xit
	key$=k$=bankgl$=fnagl$(resp$(1))
	ttgl(1)=val(key$(1:3)): ttgl(2)=val(key$(4:9)): ttgl(3)=val(key$(10:12))
	pr #255,using L1700: 0," ",mat ttgl,totaldue
	totalcr=totalcr+totaldue
	fn_printtotalsandunderlines
	fn_pr_hdr
	if glinstal=0 then goto L2050
	write #14,using L1220: mat ttgl,dat,totaldue,5,0," ","Payroll Summary",prgl$(15)
	close #14:
	L2050: !
	totaldue=0
	totalcr=0 : totaldr=0
	i$="for Cost Center # "&str$(tgl(1))
	L2080: !
	mastercd=1
	fn_L2090
fnend
def fn_L2090
	if glinstal=0 then goto L2210
	diskin=1
	open #14: "Name="&glwk$,internal,outIn ioerr L2170
	read #14,using L2130: dat3,trcode eof L2130
	L2130: form pos 13,n 6,pos 25,n 2
	if dat3=dat and trcode=5 then goto L2210
	if glb=2 then goto L2210
	close #14,free:
	L2170: !
	open #14: "Name="&glwk$&",Replace,RecL=104",internal,output
	goto L2210
	close #14:
	open #14: "Name="&glwk$,internal,output ioerr L2210
	L2210: !
	oldtgl=tgl(1)
fnend
def fn_finalscrctrlbookmulitfunds
	! FINAL PAGE FOR CONTROL SET OF BOOKS  (MULTI-FUNDS ONLY)
	fnTos(sn$="PostGl4")
	respc=0: mypos=45
	fnLbl(1,1,"G/L # for Due From Other Funds on Fund # "&oldtgl$(1:3)&":",mypos,1)
	fnqgl(1,mypos+3,0,2,pas)
	resp$(1)=fnrgl$(bankgl$)
	fnCmdKey("&Next",1,1,0,"Continue posting." )
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs2(mat resp$,ckey) ! ask clearing
	if ckey=5 then goto Xit
	key$=fnagl$(resp$(1))
	ttgl(1)=val(key$(1:3)): ttgl(2)=val(key$(4:9)): ttgl(3)=val(key$(10:12))
	pr #255,using L980: 0," ",mat ttgl,totalrec
	totaldr+=totalrec
	if glinstal then
		write #14,using L1220: mat ttgl,dat,totalrec,5,0," ","Payroll Summary",prgl$(15)
	end if
fnend
def fn_printtotalsandunderlines: ! pr TOTALS AND UNDERLINES
	if totacc<>0 then pr #255,using L980: 0," ",g1,g2,g3,totacc
	pr #255,using L2420: "___________","___________",totaldr,totalcr
	L2420: form pos 65,c 11,x 1,c 11,skip 1,pos 64,pic(---------.##),pic(---------.##)
	pr #255,using L2420: "===========","==========="
	pr #255: newpage
fnend
def fn_askaccrue
	open #12: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLindex.h[cno],Shr",internal,input,keyed ioerr L2500
	glthere=1
	L2500: !
	msgline$(1)="Do you wish to accrue part of this Payroll"
	msgline$(2)="in the previous month?"
	fnmsgbox(mat msgline$,resp$,'',4)
	accrue$=resp$
	if accrue$<>"Yes" then goto ASKACCRUE_XIT
 
	ACCRUAL: ! r:
	fnTos(sn$="PostGl5")
	respc=0: mypos=50
	fnLbl(1,1,"Number of Days in this Pay Period:",mypos,1)
	fnTxt(1,mypos+3,10,0,1,"30",0,"In order to know how much to accure, the system needs to know the days to accure.")
	resp$(1)=str$(day)
	fnLbl(2,1,"Number of Days to Expense in Last Month:",mypos,1)
	fnTxt(2,mypos+3,10,0,1,"30",0,"In order to know how much to accure, the system needs to know the days to accure.")
	resp$(2)=str$(dayslm)
	fnLbl(3,1,"G/L # for Due From Other Funds on Fund # "&oldtgl$(1:3)&":",mypos,1)
	fnqgl(3,mypos+3,0,2,pas)
	resp$(3)=fnrgl$(bankgl$)
	fnLbl(4,1,"Last Day of Previous Month:",mypos,1)
	fnTxt(4,mypos+3,10,0,1,"1",0,"Enter the month end date.")
	resp$(4)=str$(d2)
	fnCmdKey("&Next",1,1,0,"Continue posting." )
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs2(mat resp$,ckey) ! ask accrual info
	if ckey=5 then goto Xit
	day=val(resp$(1)) ! days in pay period
	dayslm=val(resp$(2)) ! days last month
	key$=fnagl$(resp$(3))
	g1=val(key$(1:3)): g2=val(key$(4:9)) : g3=val(key$(10:12))
	d2=val(resp$(4)) ! last day previous month
	acgl$=cnvrt$("N 3",g1)&cnvrt$("N 6",g2)&cnvrt$("N 3",g3)
	if glthere=1 then
		read #12,using L2860,key=acgl$: desc$ nokey ASKACCRUE_XIT
	end if
	if day<1 or day>31 then
		mat ml$(2)
		ml$(1)="Invalid number of days in pay period!"
		ml$(2)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',0)
		goto ACCRUAL
	end if
	if d2<10100 or d2>123199 then
		mat ml$(2)
		ml$(1)="Invalid date for last day of month!"
		ml$(2)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',0)
		goto ACCRUAL
	end if
	if dayslm<1 or dayslm>31 then
		mat ml$(2)
		ml$(1)="Invalid number of days for last month!"
		ml$(2)="Click OK to fix."
		fnmsgbox(mat ml$,resp$,'',0)
		goto ACCRUAL
	end if
	if dayslm>day then goto ACCRUAL
	acgl$=cnvrt$("N 3",g1)&cnvrt$("N 6",g2)&cnvrt$("N 3",g3)
	if glthere=1 then read #12,using L2860,key=acgl$: desc$ nokey ASKACCRUE_XIT
	L2860: form pos 13,c 50
	ASKACCRUE_XIT: ! ! /r
fnend
Xit: fnXit
include: Ertn no
