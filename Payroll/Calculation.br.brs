! r: Test The Tables!
	if env$('ACSdeveloper')<>'' then 
	  fn_setup 
	  fn_setupOpenFiles
	  fn_test_state_calk
		! pause
	  end
	end if
! /r
! Payroll\Calculation   (formerly   S:\Payroll\Calc  S:\acsPR\newprCalk   )

! Each year list 'every year'

! sswh - social security withholding
! mcwh - medicare withholding
! MinHourlyWage -  minumum hourly wage
! payPeriodsPerYear used to be t6
! totalWagesYtd - total wages yearToDate
! gdp - gross pay for department
! totalGrossPay - total gross pay
! stdWhFed - Standard Federal Withholding
! stdWhFed - Standard Federal Withholding
fn_setup
fntop(program$)
gosub Screen1
if ckey=5 then goto XIT

dat$=lpad$(str$(d1),6)
mo1=val(dat$(5:6)) : da=val(dat$(7:8)) : yr=val(dat$(3:4))
ppd=round(yr*365+int(yr/4)+motab(mo1)+da,2)
d1=mo1*10000+da*100+yr
fnAutomatedSavePoint('before')
fn_setupOpenFiles
open #hRpWork:=fngethandle: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],KFName=[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno]",internal,outIn,keyed  ! was 3
F_RPWORK: form pos 1,c 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
goto ReadRpWork
ReadRpWork: ! r:  read rpwork, read employee, call calc deduction etc  basically beginning of main loop
	read #hRpWork,using F_RPWORK: x$,dep,mat _inp,gpd,mat hr eof Finis
	! r: force test or check an entry from rpWork
	! fnpause
	! x$    ='      27'
	! dep   =  2
	! gpd   =637.5
	! hr(1) = 12.75
	! hr(2) = 19.13
	! _inp(1)=50 ! regular hours
	! d1= 10120
	! /r
	
	if env$('client')='Payroll Done Right' then gosub West_Acc_WorkmansComp ! env$('client')='West Accounting' or 
	newdeptkey$=cnvrt$("pic(zzzzzzz#)",val(x$))&cnvrt$("pic(zz#)",dep)
	eno=val(x$)
	if eno=0 then goto ReadRpWork
	if n$<>x$ then 
		twc=totalWagesYtd=tfy=cafy=eicytd=deducy=0
		if rtrm$(n$)<>"" then gosub ReallocateStateByDept
		dim em(16)
		dim hr(2)
		read #hEmployee,using F_employee,key=x$: mat em,lpd,totalGrossPay,w4step2,w4Year$,w4Step3,w4step4a,w4step4b,w4step4c nokey EmployeeNotFound
		gosub EmployeeRecordToLocal
		
		F_employee: form pos 112,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,n 1,C 4,pos 197,4*n 12.2
		gosub CalculateAllDeductionsAllDept
		n$=x$
		! r: Accrue Sick and Vacation
		if accrueVacaAndSick=1 then 
			if sickCode=-1 then ! Check for elgibility
				if hireDate=>10100 and hireDate<=123199 then 
					dat$=lpad$(str$(hireDate),6)
					mo  =val(dat$(1:2))
					da  =val(dat$(3:4))
					yr  =val(dat$(5:6))
					dh  =round(yr*365+int(yr/4)+motab(mo)+da,2)
					if ppd-dh=>sck(1) then 
						sickCode=sck(3)
						hrsSick =sck(2)
					end if
				end if
			end if
			if sickCode>0 then hrsSick+=sickCode ! Accrue Sick
			if sickCode>0 then  ! and env$('client')<>'Battlefield'
				write #hBreakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Sick",prd,sickCode,0 ioerr ignore
			end if
			if vaca>0 then hrsVaca+=vaca ! Accrue Vacation
			if vaca>0 then ! and env$('client')<>'Battlefield'
				write #hBreakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Vac",prd,vaca,0 ioerr ignore
			end if
		end if
		! /r
		payPeriodsPerYear=fn_payPeriodsPerYear(payCode)
		if payPeriodsPerYear<=0 then
			goto ReadRpWork
		end if
		if ~enableSkipWithholdingN(esw_federal) then 
			fed_wh=fn_federalTax(taxYear,fedpct,totalGrossPay,ded,stdWhFed,fedExempt,payPeriodsPerYear,maritial,w4Year$,w4Step3,w4step4a,w4step4b,w4step4c)
		end if
		! cafeteria plan - maybe???
		totalWagesYtd=0
		mat stuc=(0)
		read #hDepartment,using "form pos 48,n 2",key=newdeptkey$: tcd(1) ! get state code
		dim ytdTotal(32)
		fn_determineEarnings(hPrChecks,eno, dep,beg_date,end_date,mat ytdTotal,ytdFICA,ytdMedicare,ytdEic,ytdWages,mat caf)
		for j=1 to 20
			if newdedfed(j)=2 and newdedcode(j)=newdedcode_Deduct then 
				cafy+=caf(j)
				cafd+=caf(j)
			end if 
		next j
		totalWagesYtd+=ytdWages : tfy+=(ytdFICA+ytdMedicare) : ficatfy=tfy
		oldsswg=totalWagesYtd-cafy : eicytd+=ytdEic : stuc(tcd(1))+=ytdWages-cafd
		cafd=0
	end if
	!   Where Federal Withholdings are divided out into each department.
	! gpd = gross pay per department
	! pog = percent of gross
	read #hDepartment,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=newdeptkey$: teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet ! Nokey X
	if totalGrossPay=0 then pog=1: goto L1620 ! Allow checks to calculate with no gross pay
	if totalGrossPay=gpd then pog=1 : goto L1620
	if totalGrossPay=0 then
		mat ml$(1)
		ml$(1)="Employee Number "&trim$(x$)&" skipped Total Gross Pay = 0, Must be Re-entered"
		fnmsgbox(mat ml$,resp$,'',0)
		goto ReadRpWork
	end if
	pog=gpd/totalGrossPay
	L1620: ! 
	for j=1 to 20
		if env$('client')="Franklinton" then 
			if j=1 and empStatus=3 then ! retirement of firemen  ! franklinton
				_inp(j+7)=round(_inp(j+7)*gpd/100,2)
				goto L1710 ! franklinton
			else 
				if j=2 then ! retirement of police !franklinton
					_inp(j+7)=round(_inp(j+7)*((hr(1)*(_inp(1)+_inp(3)+_inp(4))+_inp(6)+_inp(17))/100),2)
					goto L1710 ! franklinton
				end if 
			end if 
			!   else if env$('client')="Washington Parrish" and j=3 and newcalcode(j)=2 then
			!     _inp(j+7)=round(_inp(j+9)*(gpd+defcompmatch)/100,2)
			!     goto L1700
			!   else if env$('client')="West Accounting" and j=10 and _inp(17)<>0 then
			!     gosub West_Acc_WorkmansComp
		else

		end if 
		if newcalcode(j)=2 then _inp(j+9)=round(_inp(j+9)*gpd/100,2)
		! L1700: ! 
		if enableSkipWithholdingN(4) then _inp(j+9)=0
		L1710: ! 
	next j
	hrsSick-=_inp(3) : hrsVaca-=_inp(4)
	! if env$('client')='Battlefield' then goto L1760
	if _inp(3)>0 then ! write sick hours taken to breakdown file
		write #hBreakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Sick",prd,0,_inp(3) ioerr ignore
	end if 
	if _inp(4)>0 then ! write vacation hours taken to breakdown file
		write #hBreakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Vac",prd,0,_inp(4) ioerr ignore
	end if 
	! L1760: ! 
	if _inp(5)>0 then ! write holiday hours taken to breakdown file
		write #hBreakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Hol",prd,0,_inp(5) ioerr ignore
	end if 
	if sck(4)=999 then sck(4)=1000 ! system will only hold 999 maximum accrued sick hours.  If maximum is set at 999, assume no maximum
	if sck(4)<>0 and hrsSick>sck(4) then hrsSick=sck(4)
	if vacm<>0 and hrsVaca>vacm then hrsVaca=vacm
	ext=0 ! Excess Tips
	goto NO_EXCESS_TIPS

	if _inp(9) then 
		tr=round(_inp(1)*MinHourlyWage+_inp(2)*MinHourlyWage*1.5,2)
		g1=gpd-_inp(9)
		ext=0
		if g1>=tr then 
			g2=_inp(9)
		else 
			g2=gpd-tr
		end if 
	end if 
	NO_EXCESS_TIPS: ! 
	gosub FicaUnEmp
goto FEDWH_DEPT ! /r
EmployeeNotFound: ! r:
	n$=" "
	mat ml$(1)
	ml$(1)="Employee Number "&x$&" is not on file. No check calculated."
	fnmsgbox(mat ml$,resp$,'',0)
goto ReadRpWork ! /r
FEDWH_DEPT: ! r: Fed WH for Dept ! Federal Withholding for Department
	if debug then fnStatus('federal  withholding for department calculating')
	f4=round(fed_wh*pog,2)
	stwh(tcd(1),1)+=gpd : eic4=0 ! Calculate EIC
	if eicCode then 
		g2=totalGrossPay
		eic1=round(8970/eicCode/payPeriodsPerYear,2)                ! this is one of the lines that change every year (formerly line 1800)
		eic2=round(16450/eicCode/payPeriodsPerYear,2)               ! this is one of the lines that change every year (formerly line 1810)
		eic3=round(1830/eicCode/payPeriodsPerYear,2)                ! this is one of the lines that change every year (formerly line 1820)
		if g2<=eic1 then eic4=round(totalGrossPay*.2040,2)
		if g2>eic1 and g2<=eic2 then eic4=eic3
		if g2>eic2 then eic4=eic3-(totalGrossPay-eic2)*.09588
		if ytdTotal(25)+eic4<0 then eic4=-ytdTotal(25)
		eic4=round(eic4*pog,2)
	end if
	tcp(1)=f4 : tcp(2)=sswh : tcp(3)=mcwh : tcp(4)=tcp4
	for j=5 to 24
		tcp(j)=_inp(j+5)
	next j
	tcp(25)=min(eic4,tcp(1))
	tcp(27)=round(_inp(2)*hr(2),2)
	tcp(28)=_inp(7)
	tcp(29)=_inp(8)
	tcp(30)=_inp(9)
	tcp(26)=gpd-tcp(27)-tcp(28)-tcp(29)-tcp(30)
	tcp(31)=gpd
	tcp(32)=gpd-tcp(1)-tcp(2)-tcp(3) ! -TCP(4)
	for j=5 to 24
		if newdedcode(j-4)=newdedcode_Benefit then 
			! do nothing
		else if newdedcode(j-4)=newdedcode_Add then 
			tcp(32)+=tcp(j) 
		else if newdedcode(j-4)=newdedcode_Deduct then
			tcp(32)-=tcp(j)
		end if
	next j
	for j=1 to 31 : tcp(j)=round(tcp(j),2) : next j
	tcp(32)+=tcp(25)-tcp(29)-tcp(30)
	! if env$('client')="Washington Parrish" then tcp(32)=tcp(32)+tcp(30) ! add tips which is really an other compensation back to net
	! the following commented lines may have to be put back in and the tdet array extended to hold them  ???  kj
	! SS_WAGE: !
	if ficaCode=9 then 
		tdc(7)=0
	else 
		tdc(7)=round(sswg*ficapog,2)
	end if 
	! MEDICARE_WAGE: !
	tdc(8)=round((totalGrossPay-t3)*ficapog,2)
	tdc(10)=0 ! State U/C Wage
	! if stuc(tcd(1))>=sucw(tcd(1)) then goto L2300
	! L2300: !
	if stuc(tcd(1))+(gpd-ext-deduc)>sucw(tcd(1)) then 
		tdc(10)=sucw(tcd(1))-stuc(tcd(1))
	else 
		tdc(10)=gpd-ext-deduc
	end if 
	if tdc(10)<0 then tdc(10)=0 ! if don't have maximum uc wage in company it will come up with negatives
	tdc(9)=0 ! Fed U/C Wage
	if feducmax and totalWagesYtd-deducy>=feducmax then 
		tdc(9)=min(max(feducmax-(totalWagesYtd-deducy),0),gpd-ext-deduc)
	else 
		tdc(9)=gpd-ext-deduc
	end if
	for j=1 to 5 : tdc(j)=_inp(j) : next j ! Hours
	! pause ! WORKMANS_COMP: !
	! mat wcm is workman's comp maximum
	! trp is (temp variable only used here)
	! tcp(26) is Regular Earnings
	! tcp(27) is OT Earnings
	! wc is (temp variable only used here)
	! tdc(6) is Workman's Comp Wages
	! if env$('client')="West Accounting" then ! perhaps everyone should be doing it this way -prd 01/06/2016
	!   tcp(14)=tdc(1)*_inp(19)*.01 ! base on regular hours times w/c rate
	!   fnStatus('tcp(14) was set to '&str$(tcp(14))&' by tcp(14) = tdc(1)('&str$(tdc(1))&' * _inp(19)('&str$(_inp(19))&') * .01')
	!   _inp(19)=0   ! <-- nice idea but it does not make a difference
	!   fnStatusPause
	! end if  ! else 
	trp=tcp(26)+tcp(27) ! base on wages
	! end if
	wc=0
	if wcm(payCode)=0 or twc+trp<wcm(payCode) then 
		wc=trp
	else 
		wc=wcm(payCode)-twc
	end if 
	twc+=wc : tdc(6)=wc
	rewrite #hDepartment,using "form pos 42,n 6,pos 58,23*pd 4.2",key=newdeptkey$: d1,mat tdet
	tcp(4)=0
	write #hPrChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,tdn,prd,0,mat tdc,mat tcp
	! fnStatus('WRITING payroll check with tcp(4)='&str$(tcp(4))&' and tcp(32)='&str$(tcp(32)))
	! fnStatusPause
	totalWagesYtd+=gpd : cafy+=ficat3 : eicytd+=ytdTotal(25)
	if tdet(16)<>0 then stuc(tcd(1))+=tdet(16) ! ??? kj
goto ReadRpWork
! /r

Screen1: ! r:
	fnGetPayrollDates(beg_date,end_date)
	d1=fnPayPeriodEndingDate
	dim d1$*20
	d1$=date$("Month DD, CCYY")

	fnTos
	rc=lc=0: mylen=42: mypos=45
	lc+=1
	fnLbl(lc+=1,1,"Pay Period Ending Date:",mylen,1)
	fnTxt(lc   ,mypos,10,0,1,"1003",0,"Enter the date which you want used for your earnings records. ")
	resp$(resp_d1N=rc+=1)=str$(d1)
	lc+=1
	fnLbl(lc+=1,1,"Report Heading Date:",mylen,1)
	fnTxt(lc   ,mypos,20,0,0," ",1,'Enter the date in alpha format for use in report headings, etc.') ! disabled on 1/12/20, doesn't seem like something people should be changing.
	resp$(resp_d1S=rc+=1)=d1$
	lc+=1
	fnChk(lc+=1,46,"Accrue Vacation and Sick Leave this period:",1)
	resp$(rc+=1)="False"
	lc+=1
	fnChk(lc+=1,46,"Skip Federal Withholdings:",1)  :	resp$(resp_skipWh1=rc+=1)="False" 
	fnChk(lc+=1,46,"Skip State Withholdings:",1)    :	resp$(resp_skipWh2=rc+=1)="False" 
	fnChk(lc+=1,46,"Skip Fica Withholdings:",1)     :	resp$(resp_skipWh3=rc+=1)="False" 
	fnChk(lc+=1,46,"Skip Standard Withholdings:",1) :	resp$(resp_skipWh4=rc+=1)="False" 
	lc+=1
	fnLbl(lc+=1,1,"Standard Federal % Override:",mylen,1,0)
	fnTxt(lc   ,mypos,4,0,1,"32",0,"Normally zero. The government allows you to use a standard percent on bonuses, etc. See Circular E for allowable %.")
	resp$(resp_stdFedOverride=rc+=1)=""
	
	fnCmdKey("Calculate",1,1,0,"Proceed with calculations.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
	fnAcs2(mat resp$,ckey)
	if ckey<>5 then 
		prd=d1=val(resp$(resp_d1N))
		d1$=resp$(resp_d1S)
		if resp$(3)(1:1)="T" then accrueVacaAndSick=1 else accrueVacaAndSick=0

		taxYear=val(str$(d1)(1:4)) ! =2019

		fnPayPeriodEndingDate(d1)
		fnSetPayrollDatesForYear(taxYear)
		fnGetPayrollDates(beg_date,end_date)

		dim enableSkipWithholdingN(4)
		mat enableSkipWithholdingN=(0)

		if resp$(resp_skipWh1)(1:1)="T" then enableSkipWithholdingN(esw_federal)=1
		if resp$(resp_skipWh2)(1:1)="T" then enableSkipWithholdingN(esw_state)=1
		if resp$(resp_skipWh3)(1:1)="T" then enableSkipWithholdingN(esw_fica)=1
		if resp$(resp_skipWh4)(1:1)="T" then enableSkipWithholdingN(4)=1
		fedpct=val(resp$(resp_stdFedOverride)) ! federal wh percent
		
	end if
return  ! /r

Finis: ! r:
	if rtrm$(n$)<>"" then gosub ReallocateStateByDept
	fn_setupCloseFiles
	close #hRpWork:
	fnFree("[Q]\PRmstr\jcprh1.h[cno]") ! get rid of jobcost time entry file if exists
goto XIT ! /r
XIT: fnxit

CalculateAllDeductionsAllDept: ! r:  returns totalGrossPay,ded,t3 (and probably other stuff, i.e. a % for each dept)
	! Calculate all deduct for federal for all departments
	totalGrossPay=t3=ded=0
	do
		for j=1 to 20
			if (j+9)=17 and (env$('client')='Payroll Done Right') then goto L3090 ! if processing _inp(17) SKIP IT do not process it.   ! env$('client')='West Accounting' or 
			if newdedfed(j)>=1 and newdedcode(j)=newdedcode_Deduct then 
				! r:  department.tdc1  State Code
					sc1=1
					read #hDepartment,using 'form pos 48,n 2',key=newdeptkey$: sc1 nokey ignore
					if sc1=0 then sc1=1
					! If env$('client')="Washington Parrish" AND J=3 Then sD3=_inp(J+9)*(GPD+DEFCOMPMATCH)/100 : Goto 3150 ! add deferred comp to gross for calculating pension deduction
					if newcalcode(j)=1 then 
						sd3=_inp(j+9) 
					else 
						sd3=_inp(j+9)*gpd/100
					end if
					stwh(sc1,1)=stwh(sc1,1)-sd3
					! returnN=sc1    !    THIS IS WRONG  returnN is not valid here.
				! /r
				if newcalcode(j)=1 then 
					ded+=_inp(j+9)
				else 
					ded+=_inp(j+9)*gpd/100
				end if 
			end if 
			if newdedfed(j)=2 then
				if newcalcode(j)=1 then 
					t3+=_inp(j+9)
				else 
					t3+=_inp(j+9)*gpd/100
				end if 
			end if 
			L3090: ! 
		next j
		totalGrossPay+=gpd
		read #hRpWork,using F_RPWORK: newx$,newdep,mat _inp,gpd,mat hr eof L3150
		if env$('client')='Payroll Done Right' then gosub West_Acc_WorkmansComp ! env$('client')='West Accounting' or 
		! pr 'A right after read rpwork _inp(6)=';_inp(6) : pause
	loop while newx$=x$
	L3150: ! 
	workkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",dep)
	restore #hRpWork,key>=workkey$: 
	read #hRpWork,using F_RPWORK: x$,dep,mat _inp,gpd,mat hr eof Finis
	if env$('client')='Payroll Done Right' then gosub West_Acc_WorkmansComp !  11/14/2017 - env$('client')='Payroll Done Right'  Does not want any special processing for deduction 8        ! env$('client')='West Accounting' or 
	! pr 'B right after read rpwork  _inp(6)=';_inp(6) : pause
return  ! /r
def fn_federalTax(taxYear,fedpct,totalGrossPay,ded,stdWhFed,fedExempt,payPeriodsPerYear,maritial,w4Year$,w4Step3,w4step4a,w4step4b,w4step4c; ___,returnN,t2,j2,previousBreak,withholdingPercentage,atLeast,baseAmt)
	! https://www.irs.gov/pub/irs-pdf/p15t.pdf
	! https://www.irs.gov/pub/irs-pdf/fw4.pdf
	! https://www.irs.gov/pub/irs-pdf/p15.pdf

	! retains: setupFederalTables,fed_annual_wh_allowance,mat fjs,mat fss,mat fhs,mat fjc,mat fsc,mat fhc,mat ft
	! ded = federal deduction addition for all departments (deduct before calculating federal taxes)
	if ~setupFederalTables then 
		setupFederalTables=1
		! r: (2017-2019) Federal - SINGLE person (mat ft(1-8,1-3))
		dim ft(8,6)
		if taxYear<=2017 then !  (includes head of household)
			fed_annual_wh_allowance=4050 ! (was 4000)   Withholding allowance. The 2016 amount for one withholding allowance on an annual basis is $4,050
			! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
			ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
			ft(2,1)=  2300 : ft(2,2)=     0    : ft(2,3)=0.1  
			ft(3,1)= 11625 : ft(3,2)=   932.5  : ft(3,3)=0.15 
			ft(4,1)= 40250 : ft(4,2)=  5226.25 : ft(4,3)=0.25 
			ft(5,1)= 94200 : ft(5,2)= 18713.75 : ft(5,3)=0.28 
			ft(6,1)=193950 : ft(6,2)= 46643.75 : ft(6,3)=0.33 
			ft(7,1)=419000 : ft(7,2)=120910.25 : ft(7,3)=0.35 
			ft(8,1)=420700 : ft(8,2)=121505.25 : ft(8,3)=0.396
		else if taxYear=2018 then !  (includes head of household)
			fed_annual_wh_allowance=4150
			ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
			ft(2,1)=  3700 : ft(2,2)=     0    : ft(2,3)=0.1  
			ft(3,1)= 13225 : ft(3,2)=   952.5  : ft(3,3)=0.12 
			ft(4,1)= 42400 : ft(4,2)=  4453.5  : ft(4,3)=0.22 
			ft(5,1)= 86200 : ft(5,2)= 14089.5  : ft(5,3)=0.24 
			ft(6,1)=161200 : ft(6,2)= 32089.5  : ft(6,3)=0.32 
			ft(7,1)=203700 : ft(7,2)= 45689.5  : ft(7,3)=0.35 
			ft(8,1)=503700 : ft(8,2)=150689.5  : ft(8,3)=0.37 
		else if taxYear=2019 then !  (includes head of household)
			fed_annual_wh_allowance=4200
			ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
			ft(2,1)=  3800 : ft(2,2)=     0    : ft(2,3)=0.1  
			ft(3,1)= 13500 : ft(3,2)=   970    : ft(3,3)=0.12 
			ft(4,1)= 43275 : ft(4,2)=  4543    : ft(4,3)=0.22 
			ft(5,1)= 88000 : ft(5,2)= 14382.5  : ft(5,3)=0.24 
			ft(6,1)=164525 : ft(6,2)= 32748.5  : ft(6,3)=0.32 
			ft(7,1)=207900 : ft(7,2)= 46628.5  : ft(7,3)=0.35 
			ft(8,1)=514100 : ft(8,2)=153798.5  : ft(8,3)=0.37 
		end if
		! /r
		! r: (2017-2019) Federal - MARRIED person (mat ft(1-8,4,6))
		if taxYear<=2017 then
			! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
			fed_annual_wh_allowance=4050
			ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
			ft(2,4)=  8650  : ft(2,5)=     0    : ft(2,6)=0.1
			ft(3,4)= 27300  : ft(3,5)=  1865    : ft(3,6)=0.15
			ft(4,4)= 84550  : ft(4,5)= 10452.5  : ft(4,6)=0.25
			ft(5,4)=161750  : ft(5,5)= 29752.5  : ft(5,6)=0.28
			ft(6,4)=242000  : ft(6,5)= 52222.5  : ft(6,6)=0.33
			ft(7,4)=425350  : ft(7,5)=112728    : ft(7,6)=0.35
			ft(8,4)=479350  : ft(8,5)=131628    : ft(8,6)=0.396
		else if taxYear=2018 then
			fed_annual_wh_allowance=4150
			ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
			ft(2,4)= 11550  : ft(2,5)=     0    : ft(2,6)=0.1
			ft(3,4)= 30600  : ft(3,5)=  1905    : ft(3,6)=0.12
			ft(4,4)= 88950  : ft(4,5)=  8907    : ft(4,6)=0.22
			ft(5,4)=176550  : ft(5,5)= 28179    : ft(5,6)=0.24
			ft(6,4)=326550  : ft(6,5)= 64179    : ft(6,6)=0.32
			ft(7,4)=411550  : ft(7,5)= 91379    : ft(7,6)=0.35
			ft(8,4)=611550  : ft(8,5)=161379    : ft(8,6)=0.37
		else if taxYear=2019 then
			fed_annual_wh_allowance=4200
			ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
			ft(2,4)= 11800  : ft(2,5)=     0    : ft(2,6)=0.1
			ft(3,4)= 31200  : ft(3,5)=  1940    : ft(3,6)=0.12
			ft(4,4)= 90750  : ft(4,5)=  9086    : ft(4,6)=0.22
			ft(5,4)=180200  : ft(5,5)= 28765    : ft(5,6)=0.24
			ft(6,4)=333250  : ft(6,5)= 65497    : ft(6,6)=0.32
			ft(7,4)=420000  : ft(7,5)= 93257    : ft(7,6)=0.35
			ft(8,4)=624150  : ft(8,5)=164709.5  : ft(8,6)=0.37
		end if
		! /r
		! r: 2020 Federal Tables
		! Page 6 from   https://www.irs.gov/pub/irs-pdf/p15t.pdf
		! fjs=federal  joint              standard     fjc=federal  joint              W-4 Step 2 checked
		! fss=federal  single             standard     fsc=federal  single             W-4 Step 2 checked
		! fhs=federal  head of household  standard     fhc=federal  head of household  W-4 Step 2 checked
		if taxYear=2020 then
			fed_annual_wh_allowance=2000
			! r: fjs=federal  joint              standard
			dim fjs(8,3)
			fjs(1,1)=     0 : fjs(1,2)=     0     : fjs(1,3)=0     !      0    11900         0    0         0
			fjs(2,1)= 11900 : fjs(2,2)=     0     : fjs(2,3)=0.1   !  11900    31650         0    0.10    11900
			fjs(3,1)= 31650 : fjs(3,2)=  1975     : fjs(3,3)=0.12  !  31650    92150      1975    0.12    31650
			fjs(4,1)= 92150 : fjs(4,2)=  9235     : fjs(4,3)=0.22  !  92150   182950      9235    0.22    92150
			fjs(5,1)=182950 : fjs(5,2)= 29211     : fjs(5,3)=0.24  ! 182950   338500     29211    0.24   182950
			fjs(6,1)=338500 : fjs(6,2)= 66543     : fjs(6,3)=0.32  ! 338500   426600     66543    0.32   338500
			fjs(7,1)=426600 : fjs(7,2)= 94735     : fjs(7,3)=0.35  ! 426600   633950     94735    0.35   426600
			fjs(8,1)=633950 : fjs(8,2)=167307.5   : fjs(8,3)=0.37  ! 633950             167307.5  0.37   633950
			! /r
			! r: fjc=federal  joint              W-4 Step 2 checked
			dim fjc(8,3)
			fjc(1,1)=     0 : fjc(1,2)=    0      : fjc(1,3)=0     !      0   12400      0     0           0
			fjc(2,1)= 12400 : fjc(2,2)=    0      : fjc(2,3)=0.1   !  12400   22275      0     0.1     12400
			fjc(3,1)= 22275 : fjc(3,2)=  987.5    : fjc(3,3)=0.12  !  22275   52525    987.5   0.12    22275
			fjc(4,1)= 52525 : fjc(4,2)= 4617.5    : fjc(4,3)=0.22  !  52525   97925   4617.5   0.22    52525
			fjc(5,1)= 97925 : fjc(5,2)=14605.5    : fjc(5,3)=0.24  !  97925  175700  14605.5   0.24    97925
			fjc(6,1)=175700 : fjc(6,2)=33271.5    : fjc(6,3)=0.32  ! 175700  219750  33271.5   0.32   175700
			fjc(7,1)=219750 : fjc(7,2)=47367.5    : fjc(7,3)=0.35  ! 219750  323425  47367.5   0.35   219750
			fjc(8,1)=323425 : fjc(8,2)=83653.75   : fjc(8,3)=0.37  ! 323425          83653.75  0.37   323425
			! /r
			! r: fss=federal  single             standard
			dim fss(8,3)
			fss(1,1)=     0 : fss(1,2)=     0   : fss(1,3)=0     !      0    3800       0    0           0
			fss(2,1)=  3800 : fss(2,2)=     0   : fss(2,3)=0.10  !   3800   13675       0    0.10     3800
			fss(3,1)= 13675 : fss(3,2)=   987.5 : fss(3,3)=0.12  !  13675   43925     987.5  0.12    13675
			fss(4,1)= 43925 : fss(4,2)=  4617.5 : fss(4,3)=0.22  !  43925   89325    4617.5  0.22    43925
			fss(5,1)= 89325 : fss(5,2)= 14605.5 : fss(5,3)=0.24  !  89325  167100   14605.5  0.24    89325
			fss(6,1)=167100 : fss(6,2)= 33271.5 : fss(6,3)=0.32  ! 167100  211150   33271.5  0.32   167100
			fss(7,1)=211150 : fss(7,2)= 47367.5 : fss(7,3)=0.35  ! 211150  522200   47367.5  0.35   211150
			fss(8,1)=522200 : fss(8,2)=156235   : fss(8,3)=0.37  ! 522200          156235    0.37   522200
			! /r
			! r: fsc=federal  single             W-4 Step 2 checked
			dim fsc(8,3)
			fsc(1,1)=     0 : fsc(1,2)=    0    : fsc(1,3)=0     !      0    6200      0     0          0
			fsc(2,1)=  6200 : fsc(2,2)=    0    : fsc(2,3)=0.1   !   6200   11138      0     0.1     6200
			fsc(3,1)= 11138 : fsc(3,2)=  493.75 : fsc(3,3)=0.12  !  11138   26263    493.75  0.12   11138
			fsc(4,1)= 26263 : fsc(4,2)= 2308.75 : fsc(4,3)=0.22  !  26263   48963   2308.75  0.22   26263
			fsc(5,1)= 48963 : fsc(5,2)= 7302.75 : fsc(5,3)=0.24  !  48963   87850   7302.75  0.24   48963
			fsc(6,1)= 87850 : fsc(6,2)=16635.75 : fsc(6,3)=0.32  !  87850  109875  16635.75  0.32   87850
			fsc(7,1)=109875 : fsc(7,2)=23683.75 : fsc(7,3)=0.35  ! 109875  265400  23683.75  0.35  109875
			fsc(8,1)=265400 : fsc(8,2)=78117.5  : fsc(8,3)=0.37  ! 265400          78117.50  0.37  265400
			! /r
			! r: fhs=federal  head of household  standard
			dim fhs(8,3)
			fhs(1,1)=     0 : fhs(1,2)=     0   : fhs(1,3)=0     !      0   10050       0    0          0
			fhs(2,1)= 10050 : fhs(2,2)=     0   : fhs(2,3)=0.10  !  10050   24150       0    0.10   10050
			fhs(3,1)= 24150 : fhs(3,2)=  1410   : fhs(3,3)=0.12  !  24150   63750    1410    0.12   24150
			fhs(4,1)= 63750 : fhs(4,2)=  6162   : fhs(4,3)=0.22  !  63750   95550    6162    0.22   63750
			fhs(5,1)= 95550 : fhs(5,2)= 13158   : fhs(5,3)=0.24  !  95550  173350   13158    0.24   95550
			fhs(6,1)=173350 : fhs(6,2)= 31830   : fhs(6,3)=0.32  ! 173350  217400   31830    0.32  173350
			fhs(7,1)=217400 : fhs(7,2)= 45926   : fhs(7,3)=0.35  ! 217400  528450   45926    0.35  217400
			fhs(8,1)=528450 : fhs(8,2)=154793.5 : fhs(8,3)=0.37  ! 528450          154793.5  0.37  528450
			! /r
			! r: fhc=federal  head of household  W-4 Step 2 checked
			dim fhc(8,3)
			fhc(1,1)=     0 : fhc(1,2)=    0    : fhc(1,3)=0     !      0    9325      0    0          0
			fhc(2,1)=  9325 : fhc(2,2)=    0    : fhc(2,3)=0.10  !   9325   16375      0    0.10    9325
			fhc(3,1)= 16375 : fhc(3,2)=  705    : fhc(3,3)=0.12  !  16375   36175    705    0.12   16375
			fhc(4,1)= 36175 : fhc(4,2)= 3081    : fhc(4,3)=0.22  !  36175   52075   3081    0.22   36175
			fhc(5,1)= 52075 : fhc(5,2)= 6579    : fhc(5,3)=0.24  !  52075   90975   6579    0.24   52075
			fhc(6,1)= 90975 : fhc(6,2)=15915    : fhc(6,3)=0.32  !  90975  113000  15915    0.32   90975
			fhc(7,1)=113000 : fhc(7,2)=22963    : fhc(7,3)=0.35  ! 113000  268525  22963    0.35  113000
			fhc(8,1)=268525 : fhc(8,2)=77396.75 : fhc(8,3)=0.37  ! 268525          77396.75 0.37  268525
			! /r
		end if
		! /r
	end if

	! r: set mat fedTable
		dim fedTable(8,3)
		if taxyear<=2019 then
			mat fedTable(8,6)
			mat fedTable=ft
			if maritial=0 then ! 0 - Single
				j2=1
			else if maritial=1 or maritial=2 or maritial=3 or maritial=4 or maritial=5 then 
				! 1 - Married
				! 2 - Single - Head of Household
				! 3 - Married - filing joint return - only one working
				! 4 - Married - filing joint - both working
				! 5 - Married - filing seperate - both working
				j2=4
			end if
		else ! taxyear=>2020
			j2=0 !  not used
			mat fedTable(8,3)
			mat fedTable=(0)
			if maritial=1 or maritial=3 or maritial=4 then 
				! 1 - Married - filing jointly
				! 3 - Married - filing joint return - only one working
				! 4 - Married - filing joint - both working
				if w4step2 then mat fedTable=fjc else mat fedTable=fjs
			else if maritial=0 or maritial=5 then
				! 0 - Single
				! 5 - Married - filing seperate - both working
				if w4step2 then mat fedTable=fsc else mat fedTable=fss
			else if maritial=2 then
				! 2 - Single - Head of Household
				if w4step2 then mat fedTable=fhc else mat fedTable=fhs
			else 
				pr bell;'invalid maritial=';maritial : pause
			end if
		end if
	! /r

	if fedpct>0 then 
		returnN=round((totalGrossPay-ded)*fedpct,2)
	else if stdWhFed=-1 or enableSkipWithholdingN(esw_federal) then ! no federal withholding
		returnN=0
	else if stdWhFed then
		returnN=stdWhFed
	else
		estPayPeriodNetPay=totalGrossPay-ded-(w4step4b/payPeriodsPerYear)
		if estPayPeriodNetPay<=0 then 
			returnN=estAnnualNetPay=withholdingPercentage=fedTaxesAnnualEstimate=0
		else
			! tableRow was j1
			estAnnualNetPay=round(estPayPeriodNetPay*payPeriodsPerYear,2)+w4step4a ! estAnnualNetPay (previously g2) - estimated annual net pay
			if w4Year$='2019' then
				estAnnualNetPay-=fedExempt*fed_annual_wh_allowance
			else ! w4Year$='2020' or w4Year$='none' then
				estAnnualNetPay-=w4step3
			end if
			tableRow=fn_table_line(mat fedTable,estAnnualNetPay)
			atLeast=fedTable(tableRow,1)
			baseAmt=fedTable(tableRow,2)
			withholdingPercentage=fedTable(tableRow,3)
			
			returnN=baseAmt+(estAnnualNetPay-atLeast)*withholdingPercentage
			returnN=returnN/payPeriodsPerYear
			returnN=round(returnN,2)

			returnN+=addOnFed+w4step4c
		end if 
	end if
	FwhFinis: !
	! pr 'federal withholding is ';returnN : pause
	if returnN<=0 then returnN=0 ! we should never have negative federal withholding.
	fn_federalTax=returnN
fnend
FicaUnEmp: ! r: FICA
	deduc=ficat3=f3=0 ! FICA
	sswg=sswh=mcwh=0
	for j=1 to 20
		if dedfica(j)=1 and newdedcode(j)=newdedcode_Deduct then 
			ficat3+=_inp(j+9)
		end if
		if deduc(j)=1 then ! total deductions for unemployment for current period and year to date
			deduc+=_inp(j+9)
			deducy+=caf(j)
		end if
	next j
	if totalGrossPay=0 then  ! calculate checks w/ no gross pay
		f3=0
		goto FicaUnEmpFinis
	else
	end if
	if ~enableSkipWithholdingN(esw_fica) then
		if ficaCode=0 then      !           0 - Subject to SS and Med WH
			! r: FicaSsTaxAndMedicare
			! if env$('client')="Washington Parrish" then
			!   tf0=totalGrossPay-t3+totaldef ! add deferred in taxable wages for washington parrish
			!   goto L1950
			! end if
			tf0=totalGrossPay-t3 ! if over both max else if over both max this time else if over max-1
			! L1950: !
			if ficatfy>=ficamxr+ficamx2 then 
				goto FicaEnd
			else if (ficatfy-ficamxr)+(tf0*ficar2)>=ficamx2 then 
				mcwh=ficamxr+ficamx2-ficatfy
				goto FicaEnd
			else if ficatfy>=ficamxr then 
				mcwh=tf0*ficar2 : sswg=0
				goto FicaEnd
			end if 
			! if went over first max this time else Under 1st Max 
			if ficatfy+(tf0*ficarate)>=ficamxr then 
				tf1=ficamax-ficatfy/ficarate : tf2=totalGrossPay-t3 
				sswh=(tf1*ficar1) 
				mcwh=(tf2*ficar2) 
				sswg=tf1 
			else 
				sswh=tf0*ficar1 
				mcwh=tf0*ficar2
				sswg=tf0
			end if
			! /r
		else if ficaCode=1 then !           1 - SS only
			! r: FicaSsTaxOnly  SOC-SEC-TAX ONLY
			tf0=totalGrossPay-t3
			if ficatfy>=ssmax then  ! OVER MAX
				sswg=0
				goto FicaEnd
			else if ficatfy+tf0>=ssmax then ! WENT OVER MAX THIS TIME
				sswh=(ssmax-ficatfy)*ficar1
				sswg=ssmax-ficatfy
				goto FicaEnd
			else ! UNDER MAX 
				sswh=tf0*ficar1
				sswg=tf0
			end if 
			! /r
		else if ficaCode=2 then !           2 - Medicare Only
			! r: FicaMedicareOnly  MEDICARE-TAX ONLY
			tf0=totalGrossPay-t3
			! if env$('client')="Washington Parrish" then ! (add deferred comp match to medicare wages)
			!   tf0+=totaldef
			! end if
			if ficatfy>=mcmax then  ! OVER MAX
				goto FicaEnd
			else if ficatfy+tf0>=mcmax then ! Went over max this time
				mcwh=(mcmax-ficatfy)*ficar2
				goto FicaEnd
			else 
				! UNDER MAX 
				mcwh=tf0*ficar2
			end if 
			! /r
		else                    !           9 - Neither SS nor Medicare
			goto FicaUnEmpFinis
		end if
		FicaEnd: ! 

		if sswg>ficamax-oldsswg-.10 and sswg<ficamax-oldsswg+.10 then 
			sswg=ficamax-oldsswg
		end if
		if totalGrossPay-t3>0 then 
			ficapog=((gpd-ficat3)/(totalGrossPay-t3)) 
		else 
			ficapog=1
		end if
		sswh=round(sswh*ficapog,2) 
		mcwh=round(mcwh*ficapog,2) 
		f3=sswh+mcwh : oldsswg+=sswg
		! CALC_NO_GROSS: !
		tfy+=f3
	end if ! ~enableSkipWithholdingN(esw_fica)
	FicaUnEmpFinis: !
return ! /r
def fn_determineEarnings(hPrChecks,eno,dep,beg_date,end_date,mat ytdTotal,&ytdFICA,&ytdMedicare,&ytdEic,&ytdWages,mat caf; ___,heno)
	ytdFICA=ytdMedicare=ytdEic=0: mat caf=(0)
	mat tcp=(0)
	mat ytdTotal=(0) : mat tdc=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",dep)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #hPrChecks,key>=checkkey$: nokey dePrCkNokey
	do
		read #hPrChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prdate,ckno,mat tdc,mat tcp eof dePrCkEof
		if heno=eno and prdate=>beg_date and prdate<=end_date then 
			mat ytdTotal=ytdTotal+tcp
		end if
	loop while heno=eno
	dePrCkEof: ! 
	ytdFICA    =ytdTotal(2) ! fica year to date
	ytdMedicare=ytdTotal(3) ! medicare year to date
	ytdEic     =ytdTotal(25) ! eic
	ytdWages   =ytdTotal(31) ! total wages
	for j=1 to 20
		caf(j)=ytdTotal(j+4) ! total miscellaneous deductions for year
	next j
	dePrCkNokey:!
fnend

def fn_table_line(mat tl_table,tl_seek_amount; tl_second_dimension)
	! this function finds where [tl_seek_amount] falls within a range in a singe row (1st element) of a 2 dimensional array)
	! this function identifies which column (2nd element) of the 2d array to search with [tl_second_dimension] which defaults to the first 
	if tl_second_dimension=0 then tl_second_dimension=1
	for tl_item=1 to udim(mat tl_table,1)-1
		if tl_seek_amount>tl_table(tl_item,tl_second_dimension) and tl_seek_amount<=tl_table(tl_item+1,tl_second_dimension) then 
			goto TL_XIT
		end if 
	next tl_item
	tl_item=udim(mat tl_table,1)
	TL_XIT: ! 
	fn_table_line=tl_item
fnend 

def fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear; clientState$*2)
	
	if clientState$='' th clientState$=fnpayroll_client_state$
	
	if clientState$='AR' then 
		returnN=fn_wh_arkansas(wages,pppy,allowances,marital,eicCode)         
	else if clientState$='AZ' then 
		returnN=fn_wh_arizona(wages,allowances)                                   
	else if clientState$='GA' then 
		returnN=fn_wh_georgia(wages,pppy,allowances,marital,eicCode)              
	else if clientState$='IL' then 
		returnN=fn_wh_illinois(wages,pppy,allowances)  
	else if clientState$='IN' then 
		returnN=fn_wh_indiana(wages,pppy,allowances)                              
	else if clientState$='KY' then ! added 10/03/2016 for R R Crawford Engineering
		returnN=fn_wh_kentuky(wages,pppy,allowances)                              
	else if clientState$='LA' then 
		returnN=fn_wh_louisiana(wages,pppy,allowances) 
	else if clientState$='MO' then 
		returnN=fn_wh_missouri(wages,maritial,fedWh,allowances,pppy)              
	else if clientState$='MS' then 
		returnN=fn_wh_mississippi(wages,pppy,addOnSt) 
	else if clientState$='OK' then 
		returnN=fn_wh_oklahoma(wages,pppy,allowances,maritial) 
	else if clientState$='OR' then 
		returnN=fn_wh_oregon(wages,fedWh,pppy,allowances,marital,w4year$,taxYear)
	else if clientState$='TN' then 
		returnN=0 ! no Tenn wh
	else if clientState$='TX' then 
		returnN=0 ! no Texas wh
	end if 
	
	fn_stateTax=returnN
	
fnend
! r: fn_stateTax subordionate functions
def fn_wh_arkansas(war_wages_taxable_current,payPeriodsPerYear,allowances,wga_is_married,wga_eicCode; ___,s1,s2,returnN)
	if ~setup_arwh then ! r: setup AR Arkansas
		dim ar(6,3) ! ar(7,3)
		setup_arwh=1
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=5: mat ar ! Arkansas
		! Page 1 of http://www.dfa.arkansas.gov/offices/incomeTax/withholding/Documents/whformula.pdf
		! over                              Percentage
		ar(1,1)=    0 : ar(1,2)=   0    :  ar(1,3)=0.009
		ar(2,1)= 4300 : ar(2,2)=  38.7  :  ar(2,3)=0.024
		ar(3,1)= 8400 : ar(3,2)= 137.1  :  ar(3,3)=0.034
		ar(4,1)=12600 : ar(4,2)= 279.9  :  ar(4,3)=0.044
		ar(5,1)=21000 : ar(5,2)= 649.5  :  ar(5,3)=0.059
		ar(6,1)=35100 : ar(6,2)=1481.4  :  ar(6,3)=0.069
		arStandardDeduction=2200
	end if ! /r
	t1=round(war_wages_taxable_current*payPeriodsPerYear,2)
	! t2=2000
	t3=t1-arStandardDeduction
	tableRow=fn_table_line(mat ar,t3)
	s1=round(ar(tableRow,2)+(t3-ar(tableRow,1))*ar(tableRow,3),2)
	s2=stAllowances*20
	returnN=round((s1-s2)/payPeriodsPerYear,2)
	if returnN<.1 then returnN=0
	fn_wh_arkansas=returnN
fnend
def fn_wh_arizona(taxableWagesCurrent,allowances; ___,returnN,stp)
	! no table  revised 1/01/10, functionalized 3/24/20
	! effective june 30, 2010 the rates changed and also 
	! the base change from a percent of federal wh to a percent of total taxable wages
	if allowances=1 then stp=.013
	if allowances=2 then stp=.018
	if allowances=3 then stp=.027
	if allowances=4 then stp=.036
	if allowances=5 then stp=.042
	if allowances=6 then stp=.0510
	returnN=round(taxableWagesCurrent*stp,2)
	! h3=min(h3,1200)
	fn_wh_arizona=returnN
fnend
def fn_wh_georgia(taxableWagesCurrent,payPeriodsPerYear,allowances,wga_is_married,wga_eicCode; ___,returnN)
	! created 06/29/2017
	! taxableWagesCurrent - formerly b8
	! payPeriodsPerYear - formerly stwh(tcd1,1)
	if ~wga_setup then 
		wga_setup=1
		gaAnnualDependantAllowance=3000
		! r: single Table F Page 43 of Employeer's Tax Guide dated 1/16/2017
		dim gawhTableF(6,3)
		gawhTableF(1,1)=    0 : gawhTableF(1,2)=   0.00 : gawhTableF(1,3)=0.01
		gawhTableF(2,1)= 1000 : gawhTableF(2,2)=  10.00 : gawhTableF(2,3)=0.02
		gawhTableF(3,1)= 3000 : gawhTableF(3,2)=  50.00 : gawhTableF(3,3)=0.03
		gawhTableF(4,1)= 5000 : gawhTableF(4,2)= 110.00 : gawhTableF(4,3)=0.04
		gawhTableF(5,1)= 7000 : gawhTableF(5,2)= 190.00 : gawhTableF(5,3)=0.05
		gawhTableF(6,1)=10000 : gawhTableF(6,2)= 340.00 : gawhTableF(6,3)=0.06
		! /r
		! r: single Table G Page 44 of Employeer's Tax Guide dated 1/16/2017
		dim gawhTableG(6,3)
		gawhTableG(1,1)=    0 : gawhTableG(1,2)=   0.00 : gawhTableG(1,3)=0.01
		gawhTableG(2,1)=  500 : gawhTableG(2,2)=   5.00 : gawhTableG(2,3)=0.02
		gawhTableG(3,1)= 1500 : gawhTableG(3,2)=  25.00 : gawhTableG(3,3)=0.03
		gawhTableG(4,1)= 2500 : gawhTableG(4,2)=  55.00 : gawhTableG(4,3)=0.04
		gawhTableG(5,1)= 3500 : gawhTableG(5,2)=  95.00 : gawhTableG(5,3)=0.05
		gawhTableG(6,1)= 5000 : gawhTableG(6,2)= 170.00 : gawhTableG(6,3)=0.06
		! /r
		! r: single Table H Page 45 of Employeer's Tax Guide dated 1/16/2017
		dim gawhTableH(6,3)
		gawhTableH(1,1)=    0 : gawhTableH(1,2)=   0.00 : gawhTableH(1,3)=0.01
		gawhTableH(2,1)=  750 : gawhTableH(2,2)=   7.50 : gawhTableH(2,3)=0.02
		gawhTableH(3,1)= 2250 : gawhTableH(3,2)=  37.50 : gawhTableH(3,3)=0.03
		gawhTableH(4,1)= 3750 : gawhTableH(4,2)=  82.50 : gawhTableH(4,3)=0.04
		gawhTableH(5,1)= 5250 : gawhTableH(5,2)= 142.50 : gawhTableH(5,3)=0.05
		gawhTableH(6,1)= 7000 : gawhTableH(6,2)= 230.00 : gawhTableH(6,3)=0.06
		! /r
	end if 
	Ga_StateDeduction=fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
	Ga_StatePersonalAllowance=fn_statePersonalAllowance('GA',wga_is_married,wga_eicCode)
	if env$('acsDeveloper')<>'' then dev=1 else dev=0
	! if dev then pr 'taxableWagesCurrent=';taxableWagesCurrent
	! if dev then pr '   payPeriodsPerYear=';payPeriodsPerYear
	! if dev then pr '   Ga_StatePersonalAllowance=';Ga_StatePersonalAllowance
	! if dev then pr '   fn_standardStateDeduction=';Ga_StateDeduction
	! if dev then pr '   allowances=';allowances
	ga_WagesAnnual=(taxableWagesCurrent)*payPeriodsPerYear
	! if dev then pr '   Ga_WagesAnnual=';Ga_WagesAnnual
	ga_WagesAnnualTaxable=Ga_WagesAnnual-Ga_StateDeduction ! fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
	ga_WagesAnnualTaxable-=Ga_StatePersonalAllowance ! fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
	! if dev then pr '   Annual Wages (less state deduction and personal allowance)=';Ga_WagesAnnualTaxable
	if wga_is_married=0 then ! SINGLE INDIVIDUAL 
		mat gawh(udim(mat gawhTableH,1),udim(mat gawhTableH,2))
		mat gawh=gawhTableH
		! if dev then pr '   using Table H'
	else if wga_is_married=4 or wga_is_married=5 then ! MARRIED FILING JOINT RETURN (both spouses having income) OR MARRIED FILING SEPARATE RETURN
		mat gawh(udim(mat gawhTableG,1),udim(mat gawhTableG,2))
		mat gawh=gawhTableG
		! if dev then pr '   using Table G'
	else if wga_is_married=3 or wga_is_married=2 or wga_is_married=1 then ! MARRIED FILING JOINT RETURN (one spouse having income) OR HEAD OF HOUSEHOLD or 1-Married (nothing else known)
		mat gawh(udim(mat gawhTableF,1),udim(mat gawhTableF,2))
		mat gawh=gawhTableF
		! if dev then pr '   using Table F'
	else 
		pr 'unrecognized wga_is_married';wga_is_married : pause
	end if
	tableRow=fn_table_line(mat gawh,Ga_WagesAnnualTaxable)
	! if dev then pr '   table line ';tableRow
	! if dev then pause
	ga_AnnualWagesSubjToWithhold=Ga_WagesAnnualTaxable-gaAnnualDependantAllowance*allowances
	returnN=gawh(tableRow,2)+(ga_AnnualWagesSubjToWithhold-gawh(tableRow,1))*gawh(tableRow,3)
	returnN=returnN
	returnN=returnN/payPeriodsPerYear
	returnN=round(returnN,2) ! round to the nearest whole dollar
	if returnN<.1 then returnN=0 ! do not withhold less than 10 cents.
	fn_wh_georgia=returnN
fnend 
def fn_wh_illinois(taxableWagesCurrent,payPeriodsPerYear,stAllowances; ___,g2,returnN)
	! no table
	! line 1 allowances = +1 for claiming self, +1 for claiming spouse
	! line 2 allowances = +1 for each other (not you nor spouse) dependent
	g2=round(taxableWagesCurrent*payPeriodsPerYear,2)
	!  new way needs awesome function !    allowances_line_1=fn_allowances_spouse_and_self
	!  new way needs awesome function !    allowances_line_2=stAllowances-allowances_line_1
	!  new way needs awesome function !    g2=g2-(allowances_line_1*2175+allowances_line_2*1000)
	g2=g2-1000*stAllowances
	returnN=g2*.0495 ! changed from .0375 on 7/10/17  ! changed from .03 to .05 1/1/11, changed from .05 to .0375 1/1/15, ok as of 1/6/16
	returnN=round(returnN/payPeriodsPerYear,2)
	if returnN<.1 then returnN=0 ! do not withhold less than 10 cents.
	fn_wh_illinois=returnN
fnend
def fn_wh_indiana(taxableWagesCurrent,payPeriodsPerYear,stAllowances; ___,returnN,h3)
	! no table   07/01/2000  ! still in effect 71508, changed on 1/1/2016, but I didn't bother to update it because no one is using it.
	! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
	h3=round(taxableWagesCurrent*payPeriodsPerYear,2)-(stAllowances*1000)
	if h3>0 then 
		returnN=h3*.034 ! +H3*.003  SOME COUNTIES HAVE WH
		returnN=round(returnN/payPeriodsPerYear,2)
		if returnN<.1 then returnN=0
	end if 
	fn_wh_indiana=returnN
fnend
def fn_wh_kentuky(taxableWagesCurrent,payPeriodsPerYear,allowances; ___,returnN,h1,h2,tableRow)
	! revised 12/31/2005
	! taxableWagesCurrent - formerly b8
	! payPeriodsPerYear - formerly stwh(tcd1,1)
	if ~wky_setup then 
		wky_setup=1
		! r: Pull the withholding routines from new\acswrk
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=20: mat ky ! Kentucky
		dim ky(6,3)
		ky(1,1)=    0 : ky(1,2)=   0 : ky(1,3)=0.02
		ky(2,1)= 3000 : ky(2,2)=  60 : ky(2,3)=0.03
		ky(3,1)= 4000 : ky(3,2)=  90 : ky(3,3)=0.04
		ky(4,1)= 5000 : ky(4,2)= 130 : ky(4,3)=0.05
		ky(5,1)= 8000 : ky(5,2)= 280 : ky(5,3)=0.058
		ky(6,1)=75000 : ky(6,2)=4166 : ky(6,3)=0.06
		! /r
	end if 
	h1=(taxableWagesCurrent)*payPeriodsPerYear
	h2=h1-1970
	tableRow=fn_table_line(mat ky,h2)
	returnN=ky(tableRow,2)+(h2-ky(tableRow,1))*ky(tableRow,3)
	returnN=returnN-20*allowances
	returnN=returnN/payPeriodsPerYear
	returnN=round(returnN,2)
	if returnN<.1 then returnN=0 ! do not withhold less than 10 cents.
	fn_wh_kentuky=returnN
fnend 
def fn_wh_louisiana(taxableWagesCurrent,payPeriodsPerYear,allowances; _
		___,returnN,h1,h2,h3,stateEarnings,x,y,m1,m2,a,bb,cc,dd,ee)
	! no table: revised 1/01/03

	stateEarnings=round(taxableWagesCurrent,2) ! stateEarnings
	if maritial=0 or maritial=2 then 
		y=stAllowances-1
		x=1
		if y>=0 then goto L3800
		x=0
		y=0
		goto L3800
	end if
	if stAllowances=0 then y=0 : x=0
	if stAllowances=1 then y=0 : x=1
	if stAllowances>=2 then y=stAllowances-2 : x=2
	L3800: ! married formula or >1 exemptions
	if x<2 then m1=12500 : m2=25000
	if x>=2 then m1=25000 : m2=50000
	n=payPeriodsPerYear
	if stateEarnings>0 then a=(stateEarnings*.021) else a=0
	if stateEarnings>(m1/n) then bb=.0135*(stateEarnings-(m1/n)) else bb=0
	if stateEarnings>(m2/n) then cc=.0135*(stateEarnings-(m2/n)) else cc=0
	dd=.021*(((x*4500)+(y*1000))/n)
	if ((x*4500)+(y*1000))>m1 then 
		ee=.0135*(((x*4500)+(y*1000)-m1)/n)
	else 
		ee=0
	end if 
	returnN=max(0,((a+bb+cc)-(dd+ee)))
	returnN=round(returnN,2)
	if returnN<.1 then returnN=0
	fn_wh_louisiana=returnN
fnend
def fn_wh_missouri(taxableWagesCurrent,maritial,fed_wh,allowances,payPeriodsPerYear; _
										___,returnN,tableRow,numb6,h1,h2,numb4,h3)
	! revised 1/1/2002
	if ~setup_mowh then  ! r: MO Missouri
		setup_mowh=1
		dim mo(9,3)
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=28: mat mo ! Missouri
		mo( 1,1)=   0 : mo( 1,2)=  0  : mo( 1,3)=0.015
		mo( 2,1)=1053 : mo( 2,2)= 16  : mo( 2,3)=0.02
		mo( 3,1)=2106 : mo( 3,2)= 37  : mo( 3,3)=0.025
		mo( 4,1)=3159 : mo( 4,2)= 63  : mo( 4,3)=0.03
		mo( 5,1)=4212 : mo( 5,2)= 95  : mo( 5,3)=0.035
		mo( 6,1)=5265 : mo( 6,2)=132  : mo( 6,3)=0.04
		mo( 7,1)=6318 : mo( 7,2)=174  : mo( 7,3)=0.045
		mo( 8,1)=7371 : mo( 8,2)=221  : mo( 8,3)=0.05
		mo( 9,1)=8424 : mo( 9,2)=274  : mo( 9,3)=0.054
	end if ! /r
	! MARITAL STATUS =2 IF HEAD OF HOUSEHOLD
	
	if maritial=0 or maritial=1 or maritial=4 or maritial=5 then 
		numb6=min(12200,fed_wh) ! FEDERAL DEDUTIONS LIMITED TO 12200 FOR SINGLE or married, filing seperately
	else if maritial=2 then 
		numb6=min(18350,fed_wh) ! FEDERAL DEDUCTIONS LIMIT FOR SINGLE HEAD OF HOUSEHOLD
	else if maritial=3 then 
		numb6=min(24400,fed_wh) ! Married filing joint, spouse doesn't work
	end if
	if maritial=1 or maritial=3 or maritial=4 or maritial=5 then 
		h1=3925
	else if maritial=2 then 
		h1=7850
	else
		h1=4700
	end if
	h2=0
	if allowances then 
		if maritial=0 then  ! single
			h2=1200+(allowances-1)*1200
		else if maritial=1 or maritial=3 or maritial=4 or maritial=5 then 
			h2=min(allowances,2)*1200+max(allowances-2,0)*1200 ! married
		else if maritial=2 then  ! head of house hold
			h2=3500+max(allowances-4,0)*1200
		end if
	end if
	numb4=round(taxableWagesCurrent*payPeriodsPerYear,2)
	h3=max(0,numb4-h1-h2-numb6)
	tableRow=fn_table_line(mat mo,h3)
	returnN=(mo(tableRow,2)+(h3-mo(tableRow,1))*mo(tableRow,3))/payPeriodsPerYear
	returnN=round(returnN,0)
	fn_wh_missouri=returnN
fnend
def fn_wh_mississippi(taxableWagesCurrent,payPeriodsPerYear,addOnSt; ___,returnN,h1,h3)
  ! no table
	! **********  REMOVE THE addOnSt FROM LINE 740 **********
	! substitute the exemptions into the field now called state tax add-on
	! the exemptions must be entered in dollars and the standard deduction
	! must be added to the exemptions.
	! SINGLE =2300, MARRIED=3400, MARRIED BOTH WORKING=1700
	h1=round(taxableWagesCurrent*payPeriodsPerYear,2)
	h3=h1-addOnSt
	if h3<=0 then 
		returnN=0
	else if h3<10000 then 
		if h3>0 and h3<=5000 then 
			returnN=.03*h3
			if returnN<.1 then returnN=0
		else
			returnN=150+.04*(h3-5000)
		end if
	else 
		returnN=350+.05*(h3-10000)
	end if
	returnN=returnN/payPeriodsPerYear
	returnN=round(returnN,2)
	if returnN<.1 then returnN=0
	fn_wh_mississippi=returnN
fnend
def fn_wh_oklahoma(taxableWagesCurrent,payPeriodsPerYear,allowances,maritial; _
		___,g2,j2,tableRow,returnN)
	!  REV. 1/01/07
	if ~setup_okwh then ! r:
		setup_okwh=1
		dim ok(7,6)
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=39: mat ok ! Oklahoma
		! r: single 
		ok(1,1)=    0 : ok(1,2)=   0   : ok(1,3)=0
		ok(2,1)= 6350 : ok(2,2)=   0   : ok(2,3)=0.005
		ok(3,1)= 7350 : ok(3,2)=   5   : ok(3,3)=0.01
		ok(4,1)= 8850 : ok(4,2)=  20   : ok(4,3)=0.02
		ok(5,1)=10100 : ok(5,2)=  45   : ok(5,3)=0.03
		ok(6,1)=11250 : ok(6,2)=  79.5 : ok(6,3)=0.04
		ok(7,1)=13550 : ok(7,2)= 171.5 : ok(7,3)=0.05
		! ok(8,1)=15000 : ok(8,2)= 246.5 : ok(8,3)=0.0525
		! /r
		! r: married 
		ok(1,4)=    0  : ok(1,5)=  0 : ok(1,6)=0
		ok(2,4)=12700  : ok(2,5)=  0 : ok(2,6)=0.005
		ok(3,4)=14700  : ok(3,5)= 10 : ok(3,6)=0.01
		ok(4,4)=17700  : ok(4,5)= 40 : ok(4,6)=0.02
		ok(5,4)=20200  : ok(5,5)= 90 : ok(5,6)=0.03
		ok(6,4)=22500  : ok(6,5)=159 : ok(6,6)=0.04
		ok(7,4)=24900  : ok(7,5)=255 : ok(7,6)=0.05
		! ok(8,4)=27600  : ok(8,5)=395 : ok(8,6)=0.0525
		! /r
	end if ! /r
	g2=taxableWagesCurrent*payPeriodsPerYear
	g2=g2-allowances*1000
	if maritial=0 or maritial=2 then j2=1 else j2=4 ! single of married
	tableRow=fn_table_line(mat ok,g2)
	returnN=ok(tableRow,j2+1)+(g2-ok(tableRow,j2))*ok(tableRow,j2+2)
	returnN=returnN/payPeriodsPerYear
	returnN=round(returnN,0)
	fn_wh_oklahoma=returnN
fnend
def fn_wh_oregon( taxableWagesCurrent,fedWhAnnualEstimate, _
									payPeriodsPerYear,allowances,isMarried, _
									w4year$,taxYear; ___, _
									returnN,allowancesEffective,theyAreSingle, _
									theyAreMarried,standardDeduction,whichTable, _
									perAllowance	)
	! requires retaining variables: wor_setup, mat or1, mat or2
	! if env$('ACSdeveloper')<>'' then print taxableWagesCurrent,fedWhAnnualEstimate,payPeriodsPerYear,allowances,isMarried : pause
	if ~wor_setup then ! r:
		wor_setup=1
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=40: mat or1,mat or2
		!  r: Withholding Table for Single with fewer than 3 allowances
		dim or1(0,0)
		if taxYear=2019 then
			mat or1(5,3)
			or1(1,1)=     0 : or1(1,2)=  206    : or1(1,3)=0.05
			or1(2,1)=  3550 : or1(2,2)=  383.5  : or1(2,3)=0.07
			or1(3,1)=  8900 : or1(3,2)=  758    : or1(3,3)=0.09
			or1(4,1)= 50000 : or1(4,2)=  540    : or1(4,3)=0.09
			or1(5,1)=250000 : or1(5,2)=11007    : or1(5,3)=0.099
		else 
			mat or1(4,3)
			or1(1,1)=     0 : or1(1,2)= 210   : or1(1,3)=0.0475
			or1(2,1)=  3600 : or1(2,2)= 381   : or1(2,3)=0.0675
			or1(3,1)=  9050 : or1(3,2)= 749   : or1(3,3)=0.0875
			
			or1(4,1)= 50000 : or1(4,2)=   0   : or1(4,3)=0.099
		end if
		! /r
		dim or2(0,0) ! r: Single with 3 or more allowances, or married
		if taxYear=2019 then
			mat or2(5,3)
			or2(1,1)=     0 : or2(1,2)=  206 : or2(1,3)=0.05
			or2(2,1)=  7100 : or2(2,2)=  561 : or2(2,3)=0.07
			or2(3,1)= 17800 : or2(3,2)= 1310 : or2(3,3)=0.09
			or2(4,1)= 50000 : or2(4,2)= 1080 : or2(4,3)=0.09
			or2(5,1)=250000 : or2(5,2)=22014 : or2(5,3)=0.099
		else
			mat or2(4,3)
			or2(1,1)=     0 : or2(1,2)=  210 : or2(1,3)=0.0475
			or2(2,1)=  7200 : or2(2,2)=  552 : or2(2,3)=0.0675
			or2(3,1)= 18100 : or2(3,2)= 1310 : or2(3,3)=0.0875
			
			or2(4,1)= 50000 : or2(4,2)= 1080 : or2(4,3)=0.09
			! or2(5,1)=250000 : or2(5,2)=22014 : or2(5,3)=0.099
		end if
		! /r
	end if ! /r
	! returns Oregon State Withholding
	theyAreSingle=theyAreMarried=0
	if isMarried=0 or isMarried=2 then theyAreSingle=1
	if isMarried=1 or isMarried=3 or isMarried=4 or isMarried=5 then theyAreMarried=1

	if w4year$='none' then
		returnN=round(taxableWagesCurrent*.08,2) ! flat 8% if no w-4
		fnStatus('flat 8% tax override triggered by W-4 Year setting of "none".')
	else
		allowancesEffective=allowances
		if taxableWagesCurrent>100000 and theyAreSingle then allowancesEffective=0
		if taxableWagesCurrent>200000 and theyAreMarried then allowancesEffective=0
	
		if theyAreMarried or (theyAreSingle and allowancesEffective>=3) then ! (married or more than 3 allowances)
			whichTable=2
		else ! (single and less than 3 allowances)
			whichTable=1
		end if 
	
		if whichTable=2 then ! theyAreMarried then
			if taxYear=2019 then
				standardDeduction=4545 ! 2019
			else
				standardDeduction=4630 ! 2020
			end if
		else ! if whichTable=1 then ! if theyAreSingle then
			if taxYear=2019 then
				standardDeduction=2270 ! 2019
				perAllowance=206
			else
				standardDeduction=2315 ! 2020
				perAllowance=210
			end if
		end if 
			! 
		wor_wagesAnnualEstimate=taxableWagesCurrent*payPeriodsPerYear
		fedWhAnnualEstimate=fedWhAnnualEstimate*payPeriodsPerYear ! needed to be annual, not pay period LS 12/31/18
		wor_phaseOut=fn_oregonPhaseOut(wor_wagesAnnualEstimate,fedWhAnnualEstimate,whichTable,theyAreSingle,theyAreMarried)
		! 
		! wor_base=taxableWagesCurrent*payPeriodsPerYear-min(fedWhAnnualEstimate,8550)-(allowancesEffective*2250)
		wor_base=wor_wagesAnnualEstimate-wor_phaseOut-standardDeduction
			! 
		if whichTable=2 then 
			wor_table_line=fn_table_line(mat or2,wor_base)
			wor_removePrev=or2(wor_table_line,1)
			wor_preBase=or2(wor_table_line,2)
			wor_taxRate=or2(wor_table_line,3)
		else ! whichTable=1
			wor_table_line=fn_table_line(mat or1,wor_base)
			wor_removePrev=or1(wor_table_line,1)
			wor_preBase=or1(wor_table_line,2)
			wor_taxRate=or1(wor_table_line,3)
		end if 
		if debug then 
			fnStatus('-------------------------------------------')
			if theyAreSingle then 
				fnStatus('  Single with '&str$(allowancesEffective)&' allowances')
			else if theyAreMarried then 
				fnStatus('  Married with '&str$(allowancesEffective)&' allowances')
			else 
				fnStatus('  Maridal Status is undetermined!!!  ')
			end if 
			fnStatus('    Current Wage (Gross)    = '&str$(taxableWagesCurrent))
			fnStatus('    Pay Periods Per Year    = '&str$(payPeriodsPerYear))
			fnStatus('    Annual wage (estimate)  = '&str$(wor_wagesAnnualEstimate))
			fnStatus('    standard deduction      = '&str$(standardDeduction))
			fnStatus('    table '&str$(whichTable)&' line '&str$(wor_table_line))
			fnStatus('    phase out               = '&str$(wor_phaseOut))
			fnStatus('    fedWhAnnualEstimate = '&str$(fedWhAnnualEstimate))
			fnStatus('.')
			fnStatus('    BASE = '&str$(wor_wagesAnnualEstimate)&' (an..wages) - '&str$(wor_phaseOut)&' (phase out/fed wh) - '&str$(standardDeduction)&' (std ded)')
			fnStatus('    base                   = '&str$(wor_base))
			! fn  status('    pre base               = '&str$(wor_preBase))
			! fn  status('    tax rate               = '&str$(wor_taxRate))
			! fn  status('    remove_prev            = '&str$(wor_removePrev))
			fnStatus('.')
			fnStatus('                                   WH = '&str$(wor_preBase)&' + [('&str$(wor_base)&' - '&str$(wor_removePrev)&')] x '&str$(wor_taxRate)&'] - (195 x '&str$(allowancesEffective)&')')
			fnStatus('-------------------------------------------')
		end if
		! 
		!      WH = 1,244 + [(BASE – 16,900        ) * 0.09] – (206 * allowances) ! 2019
		!      WH = $749 + [(BASE – $9,050) x 0.0875] – ($210 x allowances) ! 2020
		! returnN=or2(wor_table_line,2)+(wor_base-or2(wor_table_line,1))*or2(wor_table_line,3)
		returnN = wor_preBase +(( wor_base - wor_removePrev) * wor_taxRate) - (perAllowance * allowancesEffective)
	end if
	! fnStatus(' ** annual withholding estimate = '&str$(returnN))
	returnN=returnN/payPeriodsPerYear
	returnN=round(returnN,2)
	if returnN<.1 then returnN=0
	! fnStatus(' *** calculated withholding:  '&str$(returnN))
	! if debug then fnStatusPause ! pause
	fn_wh_oregon=returnN
fnend 
def fn_oregonPhaseOut(opo_wages,opo_fed_wh,opo_table,isSingle,isMarried)
	if opo_wages<50000 then 
		opo_return=min(opo_fed_wh,6800)
	else if opo_table=1 then 
		if opo_wages => 50000 and opo_wages<125000 th opo_return= 6950 : goto OPO_XIT
		if opo_wages =>125000 and opo_wages<130000 th opo_return= 5550 : goto OPO_XIT
		if opo_wages =>130000 and opo_wages<135000 th opo_return= 4150 : goto OPO_XIT
		if opo_wages =>135000 and opo_wages<140000 th opo_return= 2750 : goto OPO_XIT
		if opo_wages =>140000 and opo_wages<145000 th opo_return= 1350 : goto OPO_XIT
		if opo_wages =>145000 then opo_return=0
	else ! if opo_table=2 then
		if isMarried then 
			if opo_wages => 50000 and opo_wages<250000 then opo_return= 6950 : goto OPO_XIT
			if opo_wages =>250000 and opo_wages<260000 then opo_return= 5550 : goto OPO_XIT
			if opo_wages =>260000 and opo_wages<270000 then opo_return= 4150 : goto OPO_XIT
			if opo_wages =>270000 and opo_wages<280000 then opo_return= 2750 : goto OPO_XIT
			if opo_wages =>280000 and opo_wages<290000 then opo_return= 1350 : goto OPO_XIT
			if opo_wages =>290000 then opo_return=0
		else ! if isSingle then
			if opo_wages => 50000 and opo_wages<125000 then opo_return= 6950 : goto OPO_XIT
			if opo_wages =>125000 and opo_wages<130000 then opo_return= 5550 : goto OPO_XIT
			if opo_wages =>130000 and opo_wages<135000 then opo_return= 4150 : goto OPO_XIT
			if opo_wages =>135000 and opo_wages<140000 then opo_return= 2750 : goto OPO_XIT
			if opo_wages =>140000 and opo_wages<145000 then opo_return= 1350 : goto OPO_XIT
			if opo_wages =>145000 then opo_return=0
		end if 
	end if 
	OPO_XIT: ! 
	fn_oregonPhaseOut=opo_return
fnend 
def fn_standardStateDeduction(state$,wga_is_married,wga_eicCode)
	if state$='GA' then
		if wga_is_married=0 or wga_is_married=2 then ! Single (or Single - Head of Household)
			standardStateDeduction=2300
		else if wga_is_married=5 then ! Married - filing seperate - both working
			standardStateDeduction=1500
		else if wga_is_married=4 then ! Married - filing joint - both working
			standardStateDeduction=3000
		else if wga_is_married=3 then ! Married - filing joint return - only one working
			standardStateDeduction=3000
		else  ! 1 - Married - (filing status unknown) - just use lowest deduction
			standardStateDeduction=1500
		end if
	else
		pr 'fn_standardStateDeduction not yet configured for state: "'&state$&'"'
		pause
	end if
	fn_standardStateDeduction=standardStateDeduction
fnend
def fn_statePersonalAllowance(state$,wga_is_married,wga_eicCode)
	if state$='GA' then
		if wga_is_married=0 or wga_is_married=2 then ! Single (or Single - Head of Household)
			statePersonalAllowance=2700
		else if wga_is_married=3 then ! Married - filing joint - only one working
			statePersonalAllowance=3700
		else if wga_is_married=4 then ! Married - filing joint - both working
			statePersonalAllowance=7400
		else if wga_is_married=5 then ! Married - filing seperate - both working
			statePersonalAllowance=3700
		else  ! 1 - Married - (filing status unknown) - just use lowest married deduction
			statePersonalAllowance=3700
		end if
	else
		pr 'fn_statePersonalAllowance not yet configured for state: "'&state$&'"'
		pause
	end if
	fn_statePersonalAllowance=statePersonalAllowance
fnend
def fn_test_state_calk
	fn_setup
	! show the state you are assined  to and you change it if you like.
	!  is_married=0	! is_married = 0 - Single
									! is_married = 1 - Married
									! is_married = 2 - Single - Head of Household
									! is_married = 3 - Married - filing joint return - only one working
									! is_married = 4 - Married - filing seperate or joint return both working
	!   eicCode=0		! eicCode = 0 - Not qualified for EIC
									! eicCode = 1 - Single or Spouse not file
									! eicCode = 2 - Married both filing
	!		w4year$			! "2019", "2020", or "none"

	! r: example 1 OR 2020
		pay_periods_per_year=1
		wages_taxable_current=15000
		fed_wh=1166
		allowances=0
		is_married=0
		eicCode=0
		w4year$='2020'
		gosub TscDisplayStats
		tmp=fn_wh_oregon(wages_taxable_current,fed_wh,pay_periods_per_year,allowances,is_married,w4year$,taxYear)
		fnStatus('Oregon Example 1 returns '&str$(tmp))
		fnStatusPause
	! /r
	! r: example 2 OR 2020
		pay_periods_per_year=12
		wages_taxable_current=1250
		fed_wh=1166/12
		allowances=0
		is_married=0
		eicCode=0
		w4year$='2020'
		gosub TscDisplayStats
		fnStatus('Oregon Example 2 returns '&str$(fn_wh_oregon(wages_taxable_current,fed_wh,pay_periods_per_year,allowances,is_married,w4year$,taxYear)))
		fnStatusPause
	! /r
	! r: example 3 OR 2020
		pay_periods_per_year=12
		wages_taxable_current=132000/12
		fed_wh=21098
		allowances=0 ! 4
		is_married=0
		eicCode=0
		w4year$='none'
		gosub TscDisplayStats
		fnStatus('Oregon Example 3 returns '&str$(fn_wh_oregon(wages_taxable_current,fed_wh,pay_periods_per_year,allowances,is_married,w4year$,taxYear)))
		fnStatusPause
	! /r
	! pr 'Kentuky Function returns ';fn_wh_kentuky(wages_taxable_current,pay_periods_per_year,allowances)
	! pr 'Georgia Function returns ';fn_wh_georgia(wages_taxable_current,pay_periods_per_year,allowances,is_married,eicCode)
	! pr 'Oregon Function returns ';fn_wh_oregon(wages_taxable_current,fed_wh,pay_periods_per_year,allowances,is_married,w4year$,taxYear)
	! if env$('ACSdeveloper')<>'' then pause 
fnend 
TscDisplayStats: ! r: pr stats
	fnStatus('wages_taxable_current: '&str$(wages_taxable_current))
	fnStatus(' pay_periods_per_year: '&str$(pay_periods_per_year ))
	fnStatus('               fed_wh: '&str$(fed_wh               ))
	fnStatus('           allowances: '&str$(allowances           ))
	fnStatus('           is_married: '&str$(is_married           ))
	fnStatus('              eicCode: '&str$(eicCode              ))
	fnStatus('               w4year: '&w4year$                    )
return ! /r
! /r

West_Acc_WorkmansComp: ! r:
	! _inp(6) Other Compensation
	! if other compensation > 0 then
	!   if pay code is 1 hours = 173.33
	!   if pay code is 2 hours = 86.66
	!   if pay code is 3 hours = 80
	!   if pay code is 4 hours = 40
	! else 
	!   hours = regular hours + overtime hours
	! end if
	tmphrs=_inp(1)+_inp(2) ! if _inp(6)>0 then tmphrs=saif(payCode) else tmphrs=_inp(1)+_inp(2)
	!     fnStatus('_inp(17) changed to '&str$(round(tmphrs*_inp(17)*.01,2))&' round('&str$(tmphrs)&' * _inp(17)('&str$(_inp(17))&' * .01)',2)
	!     fnStatusPause
	_inp(17)=round(tmphrs*_inp(17)*.01,2) ! _inp(17)=round(tmphrs*_inp(17)*.01,2)
return  ! /r
ReallocateStateByDept: ! r: (reallocate state taxes based on earnings by dept and state
	s3=0 : tcp(4)=0 : tcp4=0
	oldeno=val(n$)
	restore #hDepartment,key>=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",0): 
	if stdWhSt=-1 then goto L960
	! Read #hDepartment,Using 610,Rec=TRA: tdt(4),TCD(1),ty4,tqm4,tcp4,tcp31,TCP22,NTA,MAT DST
	do
		L670: ! 
		read #hDepartment,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L960
		if teno=oldeno then 
			if debug then fnStatus('department read employee '&str$(eno)&' department '&str$(tdn))
			if d1><tdt(4) then goto L670
			holdtdn=tdn
			olddeptkey$=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",holdtdn)
			read #hPrChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",key=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",tdn)&cnvrt$("pd 6",prd): heno,tdn,prdate,ckno,mat tdc,mat tcp nokey L670
			if debug then fnStatus('read check history: heno='&str$(heno)&',tdn='&str$(tdn)&',prdate='&str$(prdate)&',ckno='&str$(ckno)&'...')
			dst3=0
			for j=1 to 20
				if dedst(j)>0 then dst3=dst3+tcp(j+4)
			next j
			! sTWH(tcd(1),1)=STWH(tcd(1),1)-DST3
			if stwh(tcd(1),1)=0 then 
				goto L670
			else if stwh(tcd(1),2)><0 then 
				goto L870
			else if stdWhSt then
				if enableSkipWithholdingN(4)<>1 then stwh(tcd(1),2)=stdWhSt
				goto L870
			end if
			if enableSkipWithholdingN(esw_state) then s3=0: goto L860
				! tcd(1) = state code
				! payPeriodsPerYear     = number of pay periods per year (formerly b8)
				! stAllowances  = allowances
				! maritial  = married (1=yes and more )
				! stAllowances - number of allowances
				! payPeriodsPerYear = number of pay periods (formerly b8)
			if tcd(1)=1 then
				s3=fn_stateTax(stwh(tcd(1),1),payPeriodsPerYear,stAllowances,marital,eicCode,fed_wh,addOnSt,w4year$,taxYear) ! ST01:
			else if tcd(1)=2  then
				s3=0 ! ST02:
			else if tcd(1)=3  then
				s3=0 ! ST03:
			else if tcd(1)=4  then
				s3=0 ! ST04:
			else if tcd(1)=5  then
				s3=0 ! ST05:
			else if tcd(1)=6  then
				s3=0 ! ST06:
			else if tcd(1)=7  then
				s3=0 ! ST07:
			else if tcd(1)=8  then
				s3=0 ! ST08:
			else if tcd(1)=9  then
				s3=0 ! ST09:
			else if tcd(1)=10 then
				s3=0 ! ST10:
			end if

			L860: ! 
			stwh(tcd(1),2)=s3
			L870: ! 
			if env$('client')="Lamar" then 
				tcp4=(stwh(tcd(1),2))*((tcp(31)-dst3)/stwh(tcd(1),1))
			else 
				tcp4=(stwh(tcd(1),2)+addOnSt)*((tcp(31)-dst3)/stwh(tcd(1),1))
			end if 
			tcp4=round(tcp4,2)
			if enableSkipWithholdingN(esw_state) then tcp4=0
			tcp(32)-=tcp4: tcp(4)=tcp4
			rewritekey$=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",holdtdn)&cnvrt$("pd 6",prd) ! index employee#,department# and payroll date
			rewrite #hPrChecks,using 'Form pos 80,pd 5.2,poS 220,pd 5.2',key=rewritekey$: tcp(4),tcp(32)
			fn_report_stuff
			rewrite #hDepartment,using 'Form pos 42,n 6',key=olddeptkey$: tdt(4)
		else
			goto L960
		end if
	loop while teno=oldeno
	L960: ! 
	gosub EmployeeLocalToRecord
	rewrite #hEmployee,using F_employee,key=n$: mat em,d1,totalGrossPay
	if fp(d1*.01)>.9 then hd1=19000000+fncd(d1) else hd1=20000000+fncd(d1)
	mat stwh=(0)
return ! /r
def fn_report_stuff
	! fnStatus('check completely calcualated for eno '&str$(eno))
	! fnStatus('tcp(1)='&str$(tcp(1)))
	! fnStatus('tcp(2)='&str$(tcp(2)))
	! fnStatus('tcp(3)='&str$(tcp(3)))
	! fnStatus('tcp(4)='&str$(tcp(4)))
	! fnStatus('tcp(13)='&str$(tcp(13)))
	! fnStatus('tcp(14)='&str$(tcp(14)))
	! fnStatus('tcp(32)='&str$(tcp(32)))
	! fnStatusPause
fnend 
EmployeeRecordToLocal: ! r:
	maritial     =em(1)
	fedExempt    =em(2)
	stAllowances =em(3)
	empStatus    =em(4)
	payCode      =em(5)
	ficaCode     =em(6)
	eicCode      =em(7)
	sickCode     =em(8)
	vaca         =em(9)
	hrsSick      =em(10)
	hrsVaca      =em(11)
	stdWhFed     =em(12)
	addOnFed     =em(13)
	stdWhSt      =em(14)
	addOnSt      =em(15)
	hireDate     =em(16)
return ! /r
EmployeeLocalToRecord: ! r:
	em(1)  = maritial    
	em(2)  = fedExempt   
	em(3)  = stAllowances
	em(4)  = empStatus   
	em(5)  = payCode     
	em(6)  = ficaCode    
	em(7)  = eicCode     
	em(8)  = sickCode    
	em(9)  = vaca        
	em(10) = hrsSick
	em(11) = hrsVaca
	em(12) = stdWhFed    
	em(13) = addOnFed    
	em(14) = stdWhSt     
	em(15) = addOnSt     
return ! /r
def fn_payPeriodsPerYear(payCode; ___,returnN)
	if payCode=1 then
		returnN=12
	else if payCode=2 then
		returnN=24
	else if payCode=3 then
		returnN=26
	else if payCode=4 then
		returnN=52
	else if payCode=5 then
		returnN=1
	else
		mat ml$(1)
		ml$(1)="Incorrect Pay Code "&str$(payCode)&" on Employee Number "&trim$(x$)&". Did not calculate pay on this Employee"
		fnmsgbox(mat ml$,resp$,'',0)
		returnN=-1
	end if
	fn_payPeriodsPerYear=returnN
fnend
def library fnCheckStateCalculation(; ___, _
	returnN, _
	lc,respc,col1_len, _
	marital,payCode,allowances,stateAddOn,w4Year$,wages,pppy,fedWh, _
	col1_pos,col1_len,col2_pos,col2_len	)
	! mat resp$,resp_*
	if ~setup_checkCalculation then ! r:
		setup_checkCalculation=1
		if ~setup then fn_setup
		dim marriedOption$(0)*58
		dim eicOption$(0)*29
		dim w4yearOption$(0)*4
		dim payPeriodOption$(0)*16
		fnGetEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)
		
		dim fed_exemption_option$(22)
		for j=1 to 21
			fed_exemption_option$(j)=str$(j-1)
		next j
		fed_exemption_option$(22)="99"
		
	end if ! /r
	fn_setupOpenFiles
	do
		! r: set default answers ;)
		pppy=1 				!	pay_periods_per_year=1
		wages=15000 	! wages_taxable_current=15000
		fedWh=1166    ! fed_wh=1166
		payCode=5
		allowances=0
		is_married=0
		eicCode=0
		w4year$='2020'
		taxYear=2020
		! /r
		fnTos ! r: Test State Calculation Ask Criteria Screen
		lc=respc=0
		col1_pos=1 : col1_len=22 : col2_pos=col1_pos+col1_len+1 : col2_len=25
		
		fnLbl(lc+=1,col1_pos,"Current Taxable Wages:",col1_len,1)
		fnTxt(lc   ,col2_pos,10,10,0,"32",0,"If you wish for the system to add additional Federal withholdings, enter that amount here.")
		resp$(resp_wages=respc+=1)=str$(wages)
	
		fnLbl(lc+=1,col1_pos,"Federal Withholding:",col1_len,1)
		fnTxt(lc   ,col2_pos,10,10,0,"32",0,"")
		resp$(resp_fedWh=respc+=1)=str$(fedWh)
		
		lc+=1
		
		fnLbl(             lc+=1,col1_pos,'Marital Status:',col2_len,1)
		fncomboa('Marital',lc   ,col2_pos,mat marriedOption$,'',col2_len)
		resp$(resp_married=respc+=1)=fnSetForCombo$(mat marriedOption$,str$(marital))
		
		fnLbl(              lc+=1,col1_pos,"Pay Code:",col1_len,1)
		fncomboa("PayCode", lc    ,col2_pos,mat payPeriodOption$,"",16)
		resp$(resp_payCode=respc+=1)=fnSetForCombo$(mat payPeriodOption$,str$(PayCode))
	
		lc+=1
	
		fnLbl(             lc+=1,col1_pos,"State Exemptions:",col1_len,1)
		fncomboa("StateEx",lc   ,col2_pos,mat fed_exemption_option$,"",3)
		resp$(resp_stExeptions=respc+=1)=fnSetForCombo$(mat fed_exemption_option$,str$(allowances),1,2)
		
		fnLbl(lc+=1,col1_pos,"State Tax Add-On:",col1_len,1)
		fnTxt(lc   ,col2_pos,10,10,0,"32",0,"If you wish for the system to add additional state withholdings, enter that amount here.")
		resp$(resp_StateAddOn=respc+=1)=str$(stateAddOn)
		
		lc+=1
		
		fnLbl(             lc+=1,col1_pos,"W-4 Year:",col1_len,1)
		fncomboa("w4Year", lc   ,col2_pos,mat w4yearOption$,'Only used if W-4 Year is set to 2020 or later.',5)
		resp$(resp_w4year=respc+=1)=w4Year$

		fnLbl(lc+=1,col1_pos,"Tax Year:",col1_len,1)
		fnTxt(lc   ,col2_pos,4,4,0,"30")
		resp$(resp_taxYear=respc+=1)=str$(taxYear)

		lc+=1
		
		fnLbl(              lc+=1,col1_pos,"EIC Code:",col1_len,1)
		fncomboa("EICCode", lc   ,col2_pos,mat eicOption$,'',25)
		resp$(resp_EicCode=respc+=1)=eicOption$(eicCode+1)

		fnCmdSet(2)
		fnAcs2(mat resp$,ckey) ! /r
		if ckey=5 then
			goto XitCheckStateCalculation
		else 
			! r: do test the calc.
			marital       =val(resp$(resp_married        )(1:1)) ! marital status
			payCode       =val(resp$(resp_payCode        )(1:2)) ! pay code
			allowances    =val(resp$(resp_stExeptions    )(1:2)) ! state ex
			stateAddOn    =val(resp$(resp_StateAddOn     )     ) ! state addon
			w4year$       =    resp$(resp_w4year         )       ! W-4 Year
			wages         =val(resp$(resp_wages          )     ) ! Current Wages Taxable
			fedWh         =val(resp$(resp_fedWh          )     ) ! Federal Withholding
			taxYear       =val(resp$(resp_taxYear        )     ) ! taxYear
			eicCode       =val(resp$(resp_EicCode        )(1:2)) ! eic code
			
			
			
			pppy=fn_payPeriodsPerYear(payCode)

			fnStatus('              taxYear: '&str$(taxYear            ))
			fnStatus('Wages Taxable Current: '&str$(wages              ))
			fnStatus(' Pay Periods Per Year: '&str$(pppy               ))
			fnStatus('  Federal WithHolding: '&str$(fedWh              ))
			fnStatus('           Allowances: '&str$(allowances         ))
			fnStatus('              Married: '&marriedOption$(marital+1))
			fnStatus('              eicCode: '&str$(eicCode            ))
			fnStatus('               w4year: '&w4year$                  )


			
			fnStatus('Calculated Federal WithHolding: '&str$( fn_federalTax(taxYear,fedpct,wages,ded,stdWhFed,fedExempt,pppy,maritial,w4Year$,w4Step3,w4step4a,w4step4b,w4step4c) ))
			fnStatus('Arkansas     State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'AR') ))
			fnStatus('Arizona      State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'AZ') ))
			fnStatus('Georgia      State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'GA') ))
			fnStatus('Illinois     State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'IL') ))
			fnStatus('Indiana      State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'IN') ))
			fnStatus('Kentuky      State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'KY') ))
			fnStatus('Louisiana    State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'LA') ))
			fnStatus('Missouri     State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'MO') ))
			fnStatus('Mississippi  State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'MS') ))
			fnStatus('Oklahoma     State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'OK') ))
			fnStatus('Oregon       State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear, 'OR') ))
			fnStatus('your State WithHolding: '&str$( fn_stateTax(wages,pppy,allowances,marital,eicCode,fedWh,addOnSt,w4year$,taxYear)               ))
			fnStatusPause
			fnStatusClose
			! /r
		end if
	loop

	! if env$('ACSdeveloper')<>'' then pause 
	
	
	XitCheckStateCalculation: !
	fn_setupCloseFiles
	returnN=0 
	fnCheckStateCalculation=returnN
fnend

def fn_setup
	autoLibrary
	on error goto Ertn
	if env$('ACSDeveloper')<>'' then debug=1 else debug=0

	dim stwh(10,2)
	dim _inp(29)
	dim dat$*20
	dim caf(20)
	dim resp$(10)*40
	dim tdt(4),tcd(3)
	dim tdet(17)
	dim tdc(10)
	dim tcp(32)
	dim x$*8
	dim n$*8
	dim stuc(10)
	dim newx$*8
	dim ml$(1)*256
	
	
	dim motab(12)
	mtc=0 ! motab counter
	motab(mtc+=1)=0   : motab(mtc+=1)=31  : motab(mtc+=1)=59
	motab(mtc+=1)=90  : motab(mtc+=1)=120 : motab(mtc+=1)=151
	motab(mtc+=1)=181 : motab(mtc+=1)=212 : motab(mtc+=1)=243
	motab(mtc+=1)=273 : motab(mtc+=1)=304 : motab(mtc+=1)=334
	
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	dim wcm(4) ! mat wcm is workman's comp maximum
	dim sck(4)
	dim sucw(10)
	! dim sucr(10)
	! dim dedcode(10),calcode(10),dedfed(10)
	! read #20,using 'Form POS 145,PD 5.2,POS 230,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,POS 618,30*N 1,POS 708,3*PD 4.3,3*PD 3.2,4*PD 4.2,POS 133,PD 6.3,PD 6.2': fucr,loccode,feducmax,ficarate,ficamax,ficamxr,mat sucw,mat sucr,mat dedcode,mat calcode,mat dedfed,mat sck,vacm,MinHourlyWage,mat wcm,ficar2,ficamx2
	! REMOVED:UNUSED POS 145       PD 5.2  fucr
	! REMOVED:UNUSED POS 230        N  2   loccode
	!                              PD 4.2  feducmax
	!                              PD 3.3  ficarate
	!                          12*	PD 4.2 ficamax,ficamxr,mat sucw(1-10)
	! REMOVED:UNUSED           10*	PD 3.3 mat sucr(1-10)
	! REMOVED:UNUSED POS 618   30*	N  1   mat dedcode(1-10),mat calcode(1-10),mat dedfed(1-10)
	!                POS 708    3* PD 4.3  mat sck(1-3)
	!                           3* PD 3.2  sck(4),vacm,MinHourlyWage
	!                           4* PD 4.2  mat wcm(1-4)
	!                POS 133       PD 6.3  ficar2
	!                              PD 6.2  ficamx2
	read #20,using 'Form POS 230,x 2,PD 4.2,PD 3.3,12*PD 4.2,POS 708,3*PD 4.3,3*PD 3.2,4*PD 4.2,POS 133,PD 6.3,PD 6.2': feducmax,ficarate,ficamax,ficamxr,mat sucw,mat sck,vacm,MinHourlyWage,mat wcm,ficar2,ficamx2
	close #20: 
	ficamax=ficamax*10
	dim fullname$(20)*20
	dim abrevname$(20)*8
	dim newdedcode(20)
	dim newcalcode(20)
	dim newdedfed(20)
	dim dedfica(20),dedst(20),deduc(20)
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	ssmax=ficamax : mcmax=ficamx2 : ficar1=ficarate*.01
	ficar2=ficar2*.01 : ficarate=ficar1+ficar2
	ficamxr=ficamax*ficarate : ficamx2=(ficamx2-ficamax)*ficar2
	! 
	! if env$('client')="West Accounting" then 
	!   saif(1)=173.33
	!   saif(2)=86.66
	!   saif(3)=80
	!   saif(4)=40
	! end if 

	newdedcode_Deduct =1
	newdedcode_Add    =2
	newdedcode_Benefit=3

	esw_federal =1 ! enums for enableSkipWithholdingN
	esw_state   =2 ! enums for enableSkipWithholdingN
	esw_fica    =3 ! enums for enableSkipWithholdingN
	esw_standard=4 ! enums for enableSkipWithholdingN

fnend 
def fn_setupOpenFiles
	open #hBreakdown:=fngethandle: "Name=[Q]\PRmstr\HourBreakdown.H[cno],KFName=[Q]\PRmstr\HourBreakdown-idx.H[cno],Shr",internal,outIn,keyed ioerr ignore ! formerly file #31
	open #hEmployee:=fngethandle: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno]',internal,outIn,keyed  ! formerly file #1
	open #hDepartment:=fngethandle: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed  ! was #2
	open #hPrChecks:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr,Use,RecL=224,KPs=1,KLn=17",internal,outIn,keyed  ! was 4
	open #hPayrollCheckIdx3_unused:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr",internal,outIn,keyed ! was 44
fnend
def fn_setupCloseFiles
	close #hBreakdown:
	close #hEmployee: 
	close #hDepartment: 
	close #hPrChecks:
	close #hPayrollCheckIdx3_unused:
	! close #hRpWork:
fnend


include: Ertn no
