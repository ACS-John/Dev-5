! formerly S:\acsPR\newprYTDQTD
! Year-To-Date Quarter-To-Date Register
fn_setup
fnTop(program$)
if ~fnprocess=1 then
	gosub Screen1
end if
open #hTrans=fnH: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",i,i,k
fnopenprn
! r: main loop
	gosub PrHdr
	dim emp$(0)*256
	dim empN(0)
	hEmployee=fn_openFio('PR Employee',mat emp$,mat empN, 1)
	! open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
	dim em$*30 ! first column
	do
		read #hEmployee,using form$(hEmployee): mat emp$,mat empN eof Finis
		eno=empN(emp_no)
		ficaCode=empN(emp_ficaCode)
		em$=emp$(emp_name)
		! dim em(6)
		! read #hEmployee,using L290: eno,em$,mat em eof Finis
		! L290: form pos 1,n 8,c 30,pos 112,6*n 2
		! r: last name first
			! if env$('client')="West Rest Haven" then goto L370 ! don't turn name around
			a=pos (rtrm$(em$)," ",1)
			b=pos (rtrm$(em$)," ",a+1)
			em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
			! L370: !
		! /r
		dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
		mat qtr1tcp=(0) : mat qt2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
		fedYr=ficaYr=stateYr=wagesQtr=fedQtr=ficaQtr=stateQtr=medYr=medQtr=0
		dim quarterTotals(32)
		mat quarterTotals=(0)
		dim ytdtotal(32)
		mat ytdtotal=(0): dedfedYr=dedficaYr=0
		dedstateYr=deducYr=wagesYr=0
		
		dim rptemp(20)
		mat rptemp=(0)
		
		dim rptot(21,2)
		
		twy=tty=twq=ttq=tficawy=tficawq=0
 
		gosub DetermineEarnings
 
		rptemp(1) +=wagesYr                  ! total wages ytd
		rptemp(2) +=fedYr                    ! federal wh ytd
		rptemp(3) +=ficaYr                   ! fica wh ytd
		rptemp(4) +=stateYr                  ! state wh ytd
		rptemp(5) +=wagesQtr                 ! total wages quarter
		rptemp(6) +=fedQtr                   ! federal wh quarter
		rptemp(7) +=ficaQtr                  ! fica quarter
		rptemp(8) +=stateQtr                 ! state wh quarter
		rptemp(9) +=medYr                    ! medicare  wh year
		rptemp(10)+=medQtr                   ! medicare Qtr
		tx1+=(wagesYr-dedfedYr)              ! taxable ytd
		for k2=1 to 20
			rptot(k2,1)+=ytdtotal(k2+4)        ! deductions
			rptot(k2,2)+=quarterTotals(k2+4)   ! deductions Qtr
		next k2
		rptot(21,1)+=eicytd                  ! eic  ytd
		rptot(21,2)+=eicQtr                  ! eic Qtr
		twq+=(wagesQtr-dedfedQtr)            ! taxable wages quarter
		twy+=(wagesYr-dedfedYr  )            ! taxable wages year
		ttips=ttips+ytdtotal(30)                      ! tips
		! Goto 280
		! totalMedicareWagesQtr=TFW=0 ! totalMedicareWagesQtr=total medicare wages for this employee  tfw = totoal ss wage for this employee
		ficawagesYr=wagesYr-dedficaYr
		ficaWagesQtr=quarterTotals(31)-dedficaQtr
		
		if ficaCode=0 then      ! 0 - Subject to SS and Med WH
			gosub FicaMedicare
			gosub FicaSS
		else if ficaCode=1 then ! 1 - SS only
			gosub FicaSS
		else if ficaCode=2 then ! 2 - Medicare Only
			gosub FicaMedicare
		end if
		dim tot(20)
		mat tot=tot+rptemp
		tx2+=tx1
		gosub PrEmp
	loop
! /r goes to Finis when done.
FicaMedicare: ! r: determine medicare maximum wages
	totalMedicareWagesQtr+=ficaWagesQtr ! mo maximum mc wage
	totalMedicareWagesYear+=ficawagesYr ! total year to date medicare wages
return ! /r
FicaSS: ! r: determine fica taxable wages
	if ficawagesYr<ficaMaxW then
		tfwy+=ficawagesYr
		tfq+=ficaWagesQtr
	else
		tfwy+=ficawagesYr-(ficawagesYr-ficaMaxW)
		tfq+=ficaWagesQtr-(min(ficawagesYr-ficaMaxW,ficaWagesQtr))
	end if
return ! /r
PrEmp: ! r:
	if ~(tx1=0 and rptemp(3)=0 and rptemp(2)=0 and rptemp(4)=0 and rptemp(5)=0 and rptemp(7)=0 and rptemp(6)=0 and rptemp(8)=0 and rptemp(9)=0 and rptemp(10)=0) then
		pr #255,using F_empLine1: eno,em$(1:19),rptemp(1),tx1,rptemp(3),rptemp(2),rptemp(4),rptemp(5),rptemp(7),rptemp(6),rptemp(8) pageoflow PgOf
		F_empLine1: form pos 1,pic(zzzzzzzz),pos 10,c 19,pos 29,n 13.2,n 12.2,n 11.2,n 11.2,n 11.2,n 13.2,n 11.2,n 11.2,n 11.2
		pr #255,using F_empLine2: rptemp(9),rptemp(10) pageoflow PgOf
		F_empLine2: form pos 54,n 11.2,x 35,n 11.2
		tx1=0
	end if
return ! /r
 
PgOf: ! r:
	pr #255: newpage
	gosub PrHdr
continue ! /r
 
Finis: ! r:
	pr #255:
	eno=0
	em$="Final Totals"
	mat rptemp=tot
	tx1=tx2
	gosub PrEmp
	pr #255: ''
	pr #255: ''
	pr #255,using 'form pos 29,c 3,pos 39,c 3': "YTD","QTD"
	pr #255: ''
	for j=1 to 20
		if trim$(abbrevname$(j))<>"" then
			pr #255,using L1040: abbrevname$(j),rptot(j,1),rptot(j,2)
			L1040: form pos 1,c 20,pos 22,2*n 12.2,skip 1
		end if
	next j
	pr #255,using L1040: "EIC",rptot(j,1),rptot(j,2)
	pr #255,using L1040: "Total Soc-Sec. Wages",tfwy,tfq
	pr #255,using L1040: "Total Medicare Wages",totalMedicareWagesYear,totalMedicareWagesQtr
	pr #255,using L1040: "Total Tips-YTD",ttips
	form pos 1,c 33,2* n 10.2,skip 1
	fncloseprn
	close #hTrans: ioerr ignore
	close #hEmployee: ioerr ignore
goto Xit ! /r
 
PrHdr: ! r:
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
 
Xit: fnXit
 
Screen1: ! r:
	dim resp$(10)*40
	prdate=fnPayPeriodEndingDate
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	fnTos
	rc=0 : mylen=23 : mypos=mylen+2
	fnLbl(1,1,"Pay Period Ending Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Normally the last payroll date, but can beny point in time. ",frameno)
	resp$(rc_prdate=rc+=1)=str$(prdate)
	fnCmdKey("Next"  ,1,1,0)
	fnCmdKey("Cancel",5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	prdate=val(resp$(1))
	payrollYear=val(resp$(rc_prdate)(1:4))
	beg_date=qtr1=payrollYear*10000+ 101
	qtr2         =payrollYear*10000+ 401
	qtr3         =payrollYear*10000+ 701
	qtr4         =payrollYear*10000+1001
	end_date     =payrollYear*10000+1231
return ! /r
DetermineEarnings: ! r:
	tfd=tmd=td14=tdw=0
	mat qtr1tcp=(0)
	mat qtr2tcp=(0)
	mat qtr3tcp=(0)
	mat qtr4tcp=(0)
	mat ytdtotal=(0)
	dedficaQtr=0
	fedYr=ficaYr=stateYr=wagesQtr=fedQtr=ficaQtr=stateQtr=medYr=0
	medQtr=eicYr=eicQtr=wagesQtr=0
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #hTrans,key>=checkkey$: nokey DetermineEarningsXit
	do
		dim tcp(32)
		mat tcp=(0)
		read #hTrans,using "form pos 1,n 8,n 3,pd 6,pos 65,32*pd 5.2": heno,tdn,prd,mat tcp eof EoTransForEmp
		if heno=eno then
			if prd=>beg_date and prd<=end_date then
				if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
				if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
				if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
				if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
				mat ytdtotal=ytdtotal+tcp
			end if
		end if
	loop while heno=eno
	EoTransForEmp: !
	wagesYr=ytdtotal(31) ! total wages
	fedYr  =ytdtotal(1) ! ytdl fed
	ficaYr =ytdtotal(2) ! fica year to date
	medYr  =ytdtotal(3) ! medicare year to date
	stateYr=ytdtotal(4) ! total state  quarter
	eicYr  =ytdtotal(25) ! eic
	if prdate>=qtr1 and prdate<qtr2 then mat quarterTotals=qtr1tcp
	if prdate>=qtr2 and prdate<qtr3 then mat quarterTotals=qtr2tcp
	if prdate>=qtr3 and prdate<qtr4 then mat quarterTotals=qtr3tcp
	if prdate>=qtr4 and prdate=<end_date then mat quarterTotals=qtr4tcp
	wagesQtr=quarterTotals(31) ! total wages quarter
	fedQtr  =quarterTotals(1) ! total fed  quarter
	ficaQtr =quarterTotals(2) ! total fica quarter
	medQtr  =quarterTotals(3) ! total medicare quarter
	stateQtr=quarterTotals(4) ! total state  quarter
	eicQtr  =quarterTotals(25) ! eic Qtr
	for j=1 to 20
		if newdedfed(j)=1 then dedfedYr+=ytdtotal(j+4) ! deduct for federal wh
		if dedfica(j)=1 then dedficaYr+=ytdtotal(j+4)
			dedficaQtr+=quarterTotals(j+4) ! deduct for fica
		if dedst(j)=1 then dedstateYr+=ytdtotal(j+4) ! deduct for state
		if deduc(j)=1 then deducYr+=ytdtotal(j+4) ! deduct for unemployment
	next j
	DetermineEarningsXit: !
return ! /r
def fn_setup
	autoLibrary
	
	on error goto Ertn
 
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
	read #1,using 'Form POS 239,PD 4.2': ficaMaxW
	close #1:
	
	ficaMaxW=ficaMaxW*10
	
	dim fullname$(20)*20
	dim abbrevname$(20)*20
	dim newdedcode(20)  ! unused in this program
	dim newcalcode(20)  ! unused in this program
	dim newdedfed(20)
	dim dedfica(20)
	dim dedst(20)
	dim deduc(20)
	fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	
fnend
include: fn_open
include: ertn
