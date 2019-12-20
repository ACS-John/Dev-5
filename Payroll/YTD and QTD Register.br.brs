! formerly S:\acsPR\newprYTDQTD
! Year-To-Date Quarter-To-Date Register
! ______________________________________________________________________
	library 'S:\Core\Library': fntop
	library 'S:\Core\Library': fnxit
	library 'S:\Core\Library': fnwait
	library 'S:\Core\Library': fnopenprn
	library 'S:\Core\Library': fncloseprn
	library 'S:\Core\Library': fnTos
	library 'S:\Core\Library': fnLbl
	library 'S:\Core\Library': fnTxt
	library 'S:\Core\Library': fnCmdKey
	library 'S:\Core\Library': fnFra
	library 'S:\Core\Library': fnAcs
	library 'S:\Core\Library': fnprocess
	library 'S:\Core\Library': fnGetPayrollDates,fnPayPeriodEndingDate
	library 'S:\Core\Library': fnDedNames
	on error goto ERTN
! gosub CHECK_PASSWORD
! ______________________________________________________________________
	dim tdc(10),fullname$(20)*20,abbrevname$(20)*20
	dim rptemp(20),rptot(21,2)
	dim em$*30,em(6),tot(20),message$*40
	dim newdedcode(20),newcalcode(20)
	dim newdedfed(20),dedfica(20),dedst(20),deduc(20)
	dim ytdtotal(32),qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
	dim quartertotals(32),tcp(32),tdc(10),resp$(10)*40
! ______________________________________________________________________
	fntop(program$)
	open #1: "Name=[Q]\PRmstr\prCode.h[cno],Shr",internal,input ioerr L180 
	read #1,using 'Form POS 5,N 5': ckno 
	close #1: 
	L180: !
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input  
	read #1,using 'Form POS 133,PD 6.3,PD 6.2,POS 239,PD 4.2,POS 618,30*N 1,10*C 6': mcr,mcm,ficamaxw 
	close #1: 
	ficamaxw=ficamaxw*10
	fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	open #4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,input,keyed 
	if fnprocess=1 then goto L240
	gosub ASK_DATES
L240: fnopenprn
! r: main loop
	gosub HDR
	open #1: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
	do
		read #1,using L290: eno,em$,mat em eof Finis
		L290: form pos 1,n 8,c 30,pos 112,6*n 2
		a=pos (rtrm$(em$)," ",1)
		b=pos (rtrm$(em$)," ",a+1)
		! if env$('client')="West Rest Haven" then goto L370 ! don't turn name around
		em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
		mat qtr1tcp=(0) : mat qt2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
		fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=medqtr=0
		mat quartertotals=(0): mat ytdtotal=(0): dedfedyr=dedficayr=0 
		dedstateyr=deducyr=wagesyr=0
		mat rptemp=(0) ! L370:
		twy=tty=twq=ttq=tficawy=tficawq=0
		gosub DETERMINE_EARNINGS
		rptemp(1)=rptemp(1)+wagesyr ! total wages ytd
		tx1=tx1+wagesyr-dedfedyr ! taxable ytd
		rptemp(2)=rptemp(2)+fedyr ! federal wh ytd
		rptemp(3)=rptemp(3)+ficayr ! fica wh ytd
		rptemp(4)=rptemp(4)+stateyr ! state wh ytd
		rptemp(5)=rptemp(5)+wagesqtr ! total wages quarter
		rptemp(6)=rptemp(6)+fedqtr ! federal wh quarter
		rptemp(7)=rptemp(7)+ficaqtr ! fica quarter
		rptemp(8)=rptemp(8)+stateqtr ! state wh quarter
		rptemp(9)=rptemp(9)+medyr ! medicare  wh year
		rptemp(10)=rptemp(10)+medqtr ! medicare qtr
		for k2=1 to 20
			rptot(k2,1)=rptot(k2,1)+ytdtotal(k2+4) ! deductions
			rptot(k2,2)=rptot(k2,2)+quartertotals(k2+4) ! deductions qtr
		next k2
		rptot(21,1)=rptot(21,1)+eicytd ! eic  ytd
		rptot(21,2)=rptot(21,2)+eicqtr ! eic qtr
		twy=twy+wagesyr-dedfedyr: twq=twq+wagesqtr-dedfedqtr ! taxable wages year and quarter
		! If EM(6)=9 Then tFICAWY=TFICAWY+wagesyr: tFICAWQ=TFICAWQ+WAGESQTR : Goto 610
		ttips=ttips+ytdtotal(30) ! tips
		! Goto 280
		! mCW1=TFW=0 ! mcw1=total medicare wages for this employee  tfw = totoal ss wage for this employee
		ficawagesyr=wagesyr- dedficayr 
		ficawagesqtr=quartertotals(31)- dedficaqtr
		on em(6)+1 goto L650,L690,L650 none L770
		! determine medicare maximum wages
		L650: !
		mcw1+=ficawagesqtr ! mo maximum mc wage
		mcw2+=ficawagesyr ! total year to date medicare wages
		if em(6)=2 then goto L770
		! determine fica taxable wages
		L690: !
		if ficawagesyr<ficamaxw then 
			tfwy+=ficawagesyr
			tfq+=ficawagesqtr
		else
			tfwy+=ficawagesyr-(ficawagesyr-ficamaxw)
			tfq+=ficawagesqtr-(min(ficawagesyr-ficamaxw,ficawagesqtr))
		end if
		L770: !
		gosub PrintEmployee
	loop
! /r goes to Finis when done.
PrintEmployee: ! r:
	mat tot=tot+rptemp
	tx2=tx2+tx1
	L810: !
	pr #255,using L890: eno,em$(1:19),rptemp(1),tx1,rptemp(3),rptemp(2),rptemp(4),rptemp(5),rptemp(7),rptemp(6),rptemp(8) pageoflow PGOF
	pr #255,using L900: rptemp(9),rptemp(10) pageoflow PGOF
	tx1=0
return ! /r
! ______________________________________________________________________
PGOF: ! r:
	pr #255: newpage
	gosub HDR
	L890: form pos 1,pic(zzzzzzzz),pos 10,c 19,pos 29,n 13.2,n 12.2,n 11.2,n 11.2,n 11.2,n 13.2,n 11.2,n 11.2,n 11.2,skip 1
	L900: form pos 54,n 11.2,x 35,n 11.2,skip 1
continue ! /r
! ______________________________________________________________________
Finis: ! r:
	pr #255: 
	eno=0
	em$="Final Totals"
	mat rptemp=tot
	tx1=tx2
	gosub L810
	pr #255,using L1000: "YTD","QTD"
	L1000: form skip 2,pos 29,c 3,pos 39,c 3,skip 2
	for j=1 to 20
		if trim$(abbrevname$)<>"" then 
			pr #255,using L1040: abbrevname$(j),rptot(j,1),rptot(j,2)
			L1040: form pos 1,c 20,pos 22,2*n 12.2,skip 1
		end if
	next j
	pr #255,using L1040: "EIC",rptot(j,1),rptot(j,2)
	pr #255,using L1040: "Total Soc-Sec. Wages",tfwy,tfq
	pr #255,using L1040: "Total Medicare Wages",mcw2,mcw1
	pr #255,using L1040: "Total Tips-YTD",ttips
	form pos 1,c 33,2* n 10.2,skip 1
	fncloseprn
	close #1: ioerr ignore
	close #2: ioerr ignore
goto Xit ! /.r
! ______________________________________________________________________
HDR: ! r:
	pr #255,using "form pos 1,c 25": 'Page '&str$(pgno+=1)&' '&date$('mm/dd/ccyy')
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&date$(days(prdate,'ccyymmdd'),'month dd, ccyy')&"}"
	! pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	pr #255: "\ql   "
	pr #255: tab(32); "<--------------------Year To Date--------------------->  <-------------Quarter To Date-------------->"
	pr #255: "  Number  Name                   Total      Taxable       SS/Med   Fed W/H    St W/H       Total        SS/Med   Fed W/H    St W/H"
	pr #255: "________  ___________________  __________ ___________ __________ __________ __________  ___________ __________ __________ __________"
return ! /r

XIT: fnxit

ASK_DATES: ! r:
	d1=fnPayPeriodEndingDate
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	fnTos(sn$="YtdQtdReg-1") 
	rc=cf=0 : mylen=27 : mypos=29
	fnFra(1,1,1,44,"Payroll Date","Enter the payroll date.") : frameno=1
	fnLbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Normally the last payroll date, but can beny point in time. ",frameno) 
	resp$(rc+=1)=str$(d1)
	! fnLbl(2,1,"Report Heading Date:",mylen,1,0,frameno)
	! fnTxt(2,mypos,20,0,0," ",0,"Enter the date in alpha format for use in report heading." ,frameno) 
	! resp$(rc+=1)=date$("Month DD, CCYY")
	fnFra(4,1,7,44,"Date Range","In order to Identify earnings and deductions, these answers must be correct.") : frameno=2 
	! mylen=27 : mypos=mylen+2
	fnLbl(1,1,"Starting Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno) 
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
	fnTxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno) 
	resp$(rc+=1)=str$(end_date)
	fnLbl(3,1,"1st Day of 1st quarter:",mylen,1,0,frameno)
	fnTxt(3,mypos,10,0,1,"3",0,"Enter the first day of the first quarter. Could be something other than January 1st if your last payroll of the previous year should be included in this year",frameno) 
	resp$(rc+=1)=str$(qtr1)
	fnLbl(4,1,"1st Day of 2nd quarter:",mylen,1,0,frameno)
	fnTxt(4,mypos,10,0,1,"3",0,"Normally would be April 1st, but could be different if your payroll dates and check dates are not the same.",frameno) 
	resp$(rc+=1)=str$(qtr2)
	fnLbl(5,1,"1st Day of 3rd quarter:",mylen,1,0,frameno)
	fnTxt(5,mypos,10,0,1,"3",0,"Normally would be July 1st",frameno) 
	resp$(rc+=1)=str$(qtr3)
	fnLbl(6,1,"1st Day of 4th quarter:",mylen,1,0,frameno)
	fnTxt(6,mypos,10,0,1,"3",0,"Normally would be October 1st.",frameno) 
	resp$(rc+=1)=str$(qtr4)
	fnCmdKey("Next",1,1,0,"Proceed with calculations.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	dat=prdate=d1=val(resp$(1))
	! dat$*20
	! dat$         =resp$(2)
	beg_date     =val(resp$(2)) 
	end_date     =val(resp$(3)) 
	qtr1         =val(resp$(4)) 
	qtr2         =val(resp$(5)) 
	qtr3         =val(resp$(6)) 
	qtr4         =val(resp$(7))
	qtr5         =val(resp$(8)(1:4))*10000+1231
	begin_year   =val(resp$(8)(1:4))*10000+0101
	end_year     =val(resp$(8)(1:4))*10000+1231
return ! /r
DETERMINE_EARNINGS: ! r:
	tfd=tmd=td14=tdw=0: mat caf=(0)
	mat tcp=(0): mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0) 
	mat qtr4tcp=(0): mat ytdtotal=(0): mat tdc=(0)
	fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0 
	medqtr=eicyr=eicqtr=wagesqtr=0
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey DetermineEarningsXit
	do
		read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof STORE_VARIABLES
		if heno<>eno then goto STORE_VARIABLES
		if prd=>beg_date and prd<=end_date then 
			if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
			if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
			if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
			if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
			mat ytdtotal=ytdtotal+tcp
		end if
	loop
	STORE_VARIABLES: !
	wagesyr=ytdtotal(31) ! total wages
	fedyr  =ytdtotal(1) ! ytdl fed
	ficayr =ytdtotal(2) ! fica year to date
	medyr  =ytdtotal(3) ! medicare year to date
	stateyr=ytdtotal(4) ! total state  quarter
	eicyr  =ytdtotal(25) ! eic
	if prdate>=qtr1 and prdate<qtr2 then mat quartertotals=qtr1tcp
	if prdate>=qtr2 and prdate<qtr3 then mat quartertotals=qtr2tcp
	if prdate>=qtr3 and prdate<qtr4 then mat quartertotals=qtr3tcp
	if prdate>=qtr4 and prdate=<end_date then mat quartertotals=qtr4tcp
	wagesqtr=quartertotals(31) ! total wages quarter
	fedqtr=quartertotals(1) ! total fed  quarter
	ficaqtr=quartertotals(2) ! total fica quarter
	medqtr=quartertotals(3) ! total medicare quarter
	stateqtr=quartertotals(4) ! total state  quarter
	eicqtr=quartertotals(25) ! eic qtr
	for j=1 to 20
		if newdedfed(j)=1 then dedfedyr+=ytdtotal(j+4) ! deduct for federal wh
		if dedfica(j)=1 then dedficayr+=ytdtotal(j+4) 
			dedficaqtr+=quartertotals(j+4) ! deduct for fica
		if dedst(j)=1 then dedstateyr+=ytdtotal(j+4) ! deduct for state
		if deduc(j)=1 then deducyr+=ytdtotal(j+4) ! deduct for unemployment
	next j
	DetermineEarningsXit: !
return ! /r

include: ertn