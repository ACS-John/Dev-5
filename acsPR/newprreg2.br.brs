! Replace S:\acsPR\newprReg2
! Payroll Register (Part 2 of 2)
fn_setup
fn_payroll_register_2( 0)
Xit: fnXit

def library fnPayrollRegister2(; det,include_tips_in_other_wh,append_reg1,ppdOverride)
	fn_setup
	fnPayrollRegister2=fn_payroll_register_2( det,include_tips_in_other_wh,append_reg1,ppdOverride)
fnend
def fn_payroll_register_2(; det,include_tips_in_other_wh,append_reg1,ppdOverride)
	! DET=1 if you dont want details printed but you want department totals.
	! DET=2 if you dont want details or department totals printed.

	dim a$*40,em$*30,cp(32),tcp(32),hc(5),thc(5),whc(10),gcp(32)
	dim tdc(10),fullname$(20)*20,abbrevname$(20)*20
	dim dedcode(10),calcode(10),dedfed(10)
	dim deductionCode(20),newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20)
	dim statewh(10),totaltcp(32),totalthc(5),deptname$*20
	dim stuc1(10),stuc2(10),err$(3)*65

	fnTop(program$,'Payroll Registers')
	! r: read ckno
	open #hPrCode=fnH: 'Name=[Q]\PRmstr\prCode.h[cno],Shr',i,i ioerr GetCkNoEnd
	read #hPrCode,using 'form pos 5,N 5': ckno
	close #hPrCode:
	GetCkNoEnd: ! /r

	if ~append_reg1 then fnOpenPrn(' (Departmental Register)')

	open #hDeptName=fnH: 'Name=[Q]\PRmstr\DeptName.h[cno],KFName=[Q]\PRmstr\DeptNameIdx.h[cno],Shr',i,i,k ioerr P2DeptOpened
	founddept=1
	P2DeptOpened: !
	if ppdOverride then
		ppd=ppdOverride
	else
		d1$=cnvrt$('pic(zzzzzzzz)',fnPayPeriodEndingDate)
		ppd=val(d1$(5:6))*10000+val(d1$(7:8))*100+val(d1$(3:4))
	end if
	fnDedNames(mat fullname$,mat abbrevname$,mat deductionCode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)

	ssr1=fnSsRateEmployee
	ssr2=fnSsEmployer*.01
	open #hCompany=fnH: 'Name=[Q]\PRmstr\Company.h[cno],Shr',i,i
	dim sucrat(10)
	dim statname$(10)*8
	read #hCompany,using F_company: a$,ficar2,feducrat,mat statname$,ficar1,mat sucrat
	F_company: form pos 1,c 40,pos 133,pd 6.3,pos 145,pd 5.2,pos 150,10*c 8,pos 236,pd 3.3,pos 287,10*pd 3.3,pos 618,30*n 1,pos 648,10*c 6
	close #hCompany: ioerr ignore

	ficar1=ficar1*.01
	ficar2=ficar2*.01
	ficarate=ficar1+ficar2
	fnIndex('[Q]\PRmstr\PayrollChecks.h[cno]','[Q]\PRmstr\CheckIdx2.h[cno]','9/12/1 3/6/8')
	! execute 'Index [Q]\PRmstr\PayrollChecks.h[cno],[Q]\PRmstr\CheckIdx2.h[cno] 9/12/1 3/6/8,replace,DupKeys -n'
	open #hTrans=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\CheckIdx2.h[cno]',i,i,k
	open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,i,k
	open #hDepartment=fnH: 'Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]',i,outIn,k
	! Read #hDepartment,Using 370,Rec=adr: eno, dep1,lpd,tcd(1),mat tdet,mat hc,mcwh,mat cp
	P2ReadChecks: !
	read #hTrans,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': eno,dep1,prdate,ckno,mat tdc,mat cp eof EoPayrollChecks
	!  if eno=307 then pr 'eno '&str$(eno) : exe 'break other_wh' : break_is_on=1 else if break_is_on then exe 'break other_wh off' : break_is_on=0
	! mcwh now in cp(3)
	if eno=0 and dep1=0 then goto P2ReadChecks
	if prdate><fndate_mmddyy_to_ccyymmdd(ppd) then goto P2ReadChecks
	read #hDepartment,using 'form pos 48,n 2',key=cnvrt$('pic(ZZZZZZZ#)',eno)&cnvrt$('pic(ZZ#)',dep1): statecode
	read #hEmployee,using 'form pos 9,c 30',key=lpad$(str$(eno),8): em$
	if det=2 then goto L580
	a=pos (rtrm$(em$),' ',1)
	b=pos (rtrm$(em$),' ',a+1)
	em$=rtrm$(em$(max(a,b):30))&' '&em$(1:a)  error ignore
	if dep2=0 then goto L550
	if dep1=dep2 then goto L580
	gosub P2DeartmentTotals
	pr #255: newpage
	L550: !
	deptname$=''
	if founddept=1 then
		read #hDeptName,using 'form pos 4,c 20',key=rpad$(ltrm$(str$(dep1)),3): deptname$ nokey ignore
	end if
	gosub P2Header
	dep2=dep1
	L580: !
	oi=cp(27)+cp(28)+cp(29)+cp(30)
	t3=t4=0
	for j=5 to 24
		if deductionCode(j-4)<>3 then

			if deductionCode(j-4)=2 then
				other_wh=other_wh-cp(j) ! if break_is_on and cp(j)<>0 then pr 'cp('&str$(j)&') deducts '&str$(cp(j))
			else
				other_wh=other_wh+cp(j) ! if break_is_on and cp(j)<>0 then pr 'cp('&str$(j)&')    adds '&str$(cp(j))
			end if

			if deductionCode(j-4)=1 and newdedfed(j-4)=2 then
				t3=t3+cp(j) : tt3=tt3+cp(j): gtt3=gtt3+cp(j) ! cafiteria
				!     if env$('client')='Washington Parrish' and j=5 then totaldef=totaldef+cp(5) ! add deferred comp match and to later add to medicare wages
			else if newdedfed(j-4)=1 and deductionCode(j-4)=1 then
				t4=t4+cp(j) ! retirement only
			end if

		end if
	next j
	other_wh=other_wh-cp(24)
	!   if include_tips_in_other_wh then other_wh+=tcp(30) ! include tips in Other Withholdings added for West Accounting on 1/18/2016
	taxwg1=taxwg1+cp(31)-t3-t4
	taxwg2=taxwg2+cp(31)-t3
	tothc=0
	for j=1 to 5
		hc(j)=tdc(j)
		tothc=tothc+hc(j)
	next j
	for j=1 to 5
		tothrs=tothrs+tdc(j)
		grandtothrs=grandtothrs+tdc(j)
	next j
	ntc=ntc+1
	! if det=1 or det=2 then goto L760
	if det<>1 and det<>2 then
		pr #255,using F_PR_LINE: dep1,eno,em$(1:11),mat hc,tothc,cp(31),cp(3),cp(2),cp(1),cp(4),other_wh,cp(32) pageoflow P2NewPage
		F_PR_LINE: form pos 1,n 4,n 8,x 2,c 12,6*n 7.2,7*n 9.2,skip 1
	end if
	! L760: !
	sswg=ficawag=tdet(1)
	ficawage=ficawage+ficawag
	totalfi=totalfi+ficawag
	sswh1=sswh1+cp(2)
	sswh2=sswh2+cp(2)
	mcwh1=mcwh1+cp(3)
	mcwh2=mcwh2+cp(3)
	mcwg=tdc(8)
	sswg=tdc(7)
	!   if env$('client')='Washington Parrish' and mcwh>0 then mcwg=cp(31)-t3+cp(15) ! add deferred comp match to medicare wages  (always must be misc 2 deduction)
	sswg1=sswg1+sswg
	sswg2=sswg2+sswg
	mcwg1=mcwg1+mcwg
	mcwg2=mcwg2+mcwg
	stateuc+=round(tdc(10)*sucrat(statecode)*.01,2)
	feduc+=round(tdc(9)*feducrat*.01,2)
	!   if env$('client')='Washington Parrish' then feducwg=tdc(9)+tcp(5): goto L870
	feducwg=tdc(9)
	! L870: !
	fedwages=fedwages+feducwg
	totalfuc=totalfuc+feducwg
	!   if env$('client')='Washington Parrish' then stucwg=tdc(10)+tcp(5): goto L900
	stucwg=tdc(10)
	! L900: !
	stuc1(statecode)=stuc1(statecode)+stucwg
	stuc2(statecode)=stuc2(statecode)+stucwg
	mat tcp=tcp+cp
	mat thc=thc+hc
	statewh(statecode)=statewh(statecode)+cp(4) ! accululate state w/h by dept
	mat totaltcp=totaltcp+cp
	mat totalthc=totalthc+hc
	other_wh=0
goto P2ReadChecks

P2DeartmentTotals: ! r:
	lp+=1
	oi=tcp(17)+tcp(18)+tcp(20)+tcp(19)
	for j=5 to 24
		if deductionCode(j-4)=3 then goto L1030
		if deductionCode(j-4)=2 then other_wh=other_wh-tcp(j) else other_wh=other_wh+tcp(j)
		L1030: !
	next j
	other_wh=other_wh-tcp(25)
	pr #255: '                   ________________________________________________________________________________________________________________' pageoflow P2NewPage
	pr #255,using FprDtotals1: ' Department Totals:',thc(2),thc(4),tothrs,tcp(3),tcp(1),other_wh pageoflow P2NewPage
	tothrs=0
	pr #255,using FprDtotals2: thc(1),thc(3),thc(5),tcp(31),tcp(2),tcp(4),tcp(32) pageoflow P2NewPage
	pr #255: '                   ================================================================================================================' pageoflow P2NewPage
	FprDtotals1: form pos 1,c 26,n 14.2,n 14.2,n 14.2,n 18.2,n 18.2,n 18.2,n 10.2,skip 1
	FprDtotals2: form pos 1,n 33.2,n 14.2,n 14.2,n 16.2,n 18.2,n 18.2,n 18.2,skip 1
	if 66-lp<26 then pr #255: newpage
	pr #255: ''
	pr #255: ''
	pr #255,using 'form pos 10,c 50': 'Department Totals'
	pr #255: ''
	pr #255,using 'form pos 12,c 30': 'Tax Expense'
	pr #255,using F_prDtotals3: 'Medicare ',tcp(3)
	if env$('client')='Thomas Richardson' or env$('client')='Kincaid' then
		pr #255,using F_prDtotals3: 'SS  ',round(tcp(2)/ssr1*ssr2,2) ! show only employeer expense here
	else
		pr #255,using F_prDtotals3: 'SS  ',tcp(2)+round(tcp(2)/ssr1*ssr2,2) ! show total AND employeer matching
	end if
	F_prDtotals3: form pos 20,c 10,pic(-------.##),skip 1
	pr #255,using F_prDtotals3: 'Fed U/C',feduc
	pr #255,using F_prDtotals3: 'State U/C',stateuc
	pr #255,using F_prDtotals3: '     Total',round(tcp(2)/ssr1*ssr2,2)+feduc+stateuc+tcp(3) ! 2013
	pr #255: ''
	pr #255,using F_prDtotals4: 'Net Pay',tcp(32)
	pr #255: ''
	pr #255,using F_prDtotals4: 'Taxable Wages',taxwg1
	F_prDtotals4: form pos 10,c 20,n 10.2,skip 1
	tfw=ficawage
	if tfw>=tcp(31)-tt3-.1 and tfw<=tcp(31)-tt3+.1 then 
		tfw=tcp(31)
	end if
	! pr #255,using 1100: 'FICA Wages',tfw ! use same form as 'Taxable wages'
	tucw=fedwages
	if tucw>=tcp(31)-tt3-.1 and tucw<=tcp(31)-tt3+.1 then 
		tucw=tcp(31)-tt3
	end if
	pr #255,using F_prDtotals4: 'SS Wages',sswg1
	pr #255,using F_prDtotals4: 'MC Wages',mcwg1
	pr #255,using F_prDtotals4: 'Fed U/C Wages',tucw
	for j=1 to 10
		if stuc1(j) then
			pr #255,using F_prDtotals4: statname$(j)&' UC Wages',stuc1(j)
		end if
	next j
	pr #255,using F_prDtotals4: 'Payroll Tax Deposit',tcp(1)+tcp(2)+round(tcp(2)/ssr1*ssr2,2)-tcp(25)+tcp(3)*2 ! 2013
	pr #255,using 'form skip 1,pos 12,c 30': 'Payroll Deductions'
	pr #255,using F_prDtotals3: 'SS-Wh',sswh1
	pr #255,using F_prDtotals3: 'MC-Wh',mcwh1
	pr #255,using F_prDtotals3: 'Fed',tcp(1)
	pr #255,using F_prDtotals3: 'State',tcp(4)
	for j=1 to 20

		if tcp(j+4) and deductionCode(j)=1 then
			pr #255,using F_prDtotals3: abbrevname$(j),tcp(j+4)
		end if
		! if tcp(j+4)=0 then goto L1530
		! if deductionCode(j)<>1 then goto L1530
		! pr #255,using F_prDtotals3: abbrevname$(j),tcp(j+4)
		! L1530: !

	next j
	pr #255,using 'form skip 1,pos 12,c 30': 'Additions to Net'
	for j=1 to 20

		if tcp(j+4) and deductionCode(j)=2 then
			pr #255,using F_prDtotals3: abbrevname$(j),tcp(j+4)
		end if
		! if tcp(j+4)=0 or deductionCode(j)<>2 then goto L1580
		! pr #255,using F_prDtotals3: abbrevname$(j),tcp(j+4)
		! L1580: !

	next j
	if tcp(25) then
		pr #255,using F_prDtotals3: 'EIC',tcp(25)
	end if
	! if tcp(25)=0 then goto L1610
	! pr #255,using F_prDtotals3: 'EIC',tcp(25)
	! L1610: !

	if final<>1 then
		other_wh=0
		o1=0
		mat gcp=gcp+tcp
		! IF NTC=0 THEN GOTO asdf
		! eMPMATCH=EMPMATCH+TCP(3) ! add medicare into employer match
		! form pos 1,n 6
		! form pos 1,n 6,n 3,35*pd 5.2,n 4
		! ASDF: !
		!  goto L1700
		! L1700: !
		ntc=0 : mat tcp=(0) : tt3=0
		mat thc=(0) : ficawage=0 : fedwages=0
		mat stuc1=(0) : taxwg1=sswh1=mcwh1=sswg1=mcwg1=stateuc=feduc=0
		dep+=1 ! count # of departments used
	end if
return  ! /r
P2NewPage: ! r:
	pr #255: newpage
	ct1=ct1+1: pr f '12,45,CL 5,N': str$(ct1)
	gosub P2Header
continue  ! /r
P2Header: ! r:
		pr #255,using 'form pos 1,c 25': 'Page '&str$(pgno+=1)&' '&date$
		pr #255: '\qc  {\f221 \fs22 \b '&env$('cnam')&'}'
		pr #255: '\qc  {\f201 \fs20 \b Payroll Departmental Register}'
		pr #255: '\qc  {\f181 \fs16 \b Payroll Date: '&cnvrt$('pic(zz/zz/zz)',ppd)&'}'
		pr #255: '\qc  {\f181 \fs16 \b '&trim$(deptname$)&'}'
		pr #255: '\ql   '
		! pr #255,Using 1860: A$,'Payroll Register',D$,TRIM$(DEPTNAME$)
		! form skip 2,pos 1,CC 132,skip 1,pos 58,C 18,skip 1,pos 1,CC 132,skip 1,pos 1,CC 132,skip 2
		L1860: pr #255: tab(29);'<----------------Hours----------------->';
		pr #255: tab(71);'<-Pay->';
		pr #255: tab(79);'<-----------------Deductions---------------->';
		pr #255: tab(129);'Net'
		if eofcode=1 then goto L1940
		pr #255: 'Dept';
		if det=1 or det=2 then goto L1940
		pr #255: tab(8);'Emp #  Name';
L1940: pr #255: tab(29);' Reg    O/T   Sick    Vac    Hol   Total';
		pr #255: tab(71);'  Total   Med WH    SS WH  Federal    State    Other      Pay'
		lp=6
		return  ! /r

EoPayrollChecks: !
		eofcode=1
		if det=2 then goto L2040
		gosub P2DeartmentTotals
		! if dep=1 then goto L2200 ! only 1 dept printed-no summary
		if dep<>1 then  ! only 1 dept printed-no summary
			pr #255: newpage ! pr final departmental summary
			L2040: ! pr #255,Using 1840: A$,'Payroll Register',D$
			pr #255: '\qc  {\f221 \fs22 \b '&env$('cnam')&'}'
			pr #255: '\qc  {\f201 \fs20 \b Payroll Register - Departmental Totals}'
			pr #255: '\qc  {\f181 \fs16 \b Payroll Date: '&cnvrt$('pic(zz/zz/zz)',ppd)&'}'
			pr #255: '\ql   '
			mat tcp=totaltcp : mat thc=totalthc : tt3=gtt3
			ficawage=totalfi : fedwages=totalfuc : mat stuc1=stuc2
			sswh1=sswh2 : mcwh1=mcwh2 : sswg1=sswg2
			mcwg1=mcwg2 : taxwg1=taxwg2 : tothrs=grandtothrs
			pr #255: ''
			pr #255,using 'form pos 52,c 40': 'Summary for all Departments'
			gosub L1860
			final=1
			gosub P2DeartmentTotals
			pr #255,using 'form skip 1,pos 12,c 30': 'State W/H Breakdown'
			for j=1 to 10
				if statewh(j)=0 then goto L2190
				pr #255,using F_prDtotals4: statname$(j),statewh(j)
				L2190: !
			next j
			! L2200: !
		end if
		close #hEmployee: ioerr ignore
		close #hDepartment: ioerr ignore
		close #hTrans: ioerr ignore
		! fnStatusClose
		fnClosePrn
		fnIndex('[Q]\PRmstr\prTot.h[cno]','[Q]\PRmstr\PRTotIdx.h[cno]','1 9')
		! fnStatusClose
fnend
include: fn_setup
