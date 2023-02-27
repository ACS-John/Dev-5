! r: notes
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
! /r
fn_setup
fnTop(program$)
! r: screen1
	fnGetPayrollDates(begDate,endDate)
	d1=fnPayPeriodEndingDate
	dim d1$*20
	d1$=date$('Month DD, CCYY')

	fnTos
	rc=lc=0: mylen=42: mypos=45
	lc+=1
	fnLbl(lc+=1,1,'Pay Period Ending Date:',mylen,1)
	fnTxt(lc   ,mypos,10,0,1,'1003',0,'Enter the date which you want used for your earnings records. ')
	resp$(resp_d1N=rc+=1)=str$(d1)
	lc+=1
	fnLbl(lc+=1,1,'Report Heading Date:',mylen,1)
	fnTxt(lc   ,mypos,20,0,0,' ',1,'the date for use in report headings') ! disabled on 1/12/20, doesn't seem like something people should be changing.
	resp$(resp_d1S=rc+=1)=d1$
	lc+=1
	fnChk(lc+=1,46,'Accrue Vacation and Sick Leave this period:',1)
	resp$(rc+=1)='False'
	lc+=1
	fnChk(lc+=1,46,'Skip Federal Withholdings:',1)  :	resp$(resp_skipWh1=rc+=1)='False'
	fnChk(lc+=1,46,'Skip State Withholdings:',1)    :	resp$(resp_skipWh2=rc+=1)='False'
	fnChk(lc+=1,46,'Skip Fica Withholdings:',1)     :	resp$(resp_skipWh3=rc+=1)='False'
	fnChk(lc+=1,46,'Skip Standard Withholdings:',1) :	resp$(resp_skipWh4=rc+=1)='False'
	lc+=1
	fnLbl(lc+=1,1,'Standard Federal % Override:',mylen,1,0)
	fnTxt(lc   ,mypos,4,0,1,'32',0,'Normally zero. The government allows you to use a standard percent on bonuses, etc. See Circular E for allowable %.')
	resp$(resp_stdFedOverride=rc+=1)=''
	if fn_justTesting then
		fnCmdKey('Test Calculation',1,1,0,'Proceed with testing the calculation.')
	else
		fnCmdKey('Calculate',1,1,0,'Proceed with calculations.')
	end if
		fnCmdKey('Cancel',5,0,1,'Returns to menu without calculating')
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto Xit
	else
		prd=d1=val(resp$(resp_d1N))
		d1$=resp$(resp_d1S)
		if resp$(3)(1:1)='T' then accrueVacaAndSick=1 else accrueVacaAndSick=0

		taxYear=val(str$(d1)(1:4)) ! =2019

		fnPayPeriodEndingDate(d1)
		fnSetPayrollDatesForYear(taxYear)
		fnGetPayrollDates(begDate,endDate)

		dim disableWithholdingN(4)
		mat disableWithholdingN=(0)

		if resp$(resp_skipWh1)(1:1)='T' then disableWithholdingN(esw_federal)=1
		if resp$(resp_skipWh2)(1:1)='T' then disableWithholdingN(esw_state)=1
		if resp$(resp_skipWh3)(1:1)='T' then disableWithholdingN(esw_fica)=1
		if resp$(resp_skipWh4)(1:1)='T' then disableWithholdingN(esw_standard)=1
		fedpct=val(resp$(resp_stdFedOverride)) ! federal wh percent
		dim dat$*20
		dat$=lpad$(str$(d1),6)
		mo1=val(dat$(5:6)) : da=val(dat$(7:8)) : yr=val(dat$(3:4))
		ppd=round(yr*365+int(yr/4)+motab(mo1)+da,2)
		d1=mo1*10000+da*100+yr
	end if
! /r
if ~fn_justTesting then fnAutomatedSavePoint('before')
fn_setupOpenFiles
open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno]',i,outIn,k  ! was 3
goto MainLoop

def fn_readTimesheet(&empNo$,&dep,mat xInp,&gpd,mat hr; ___,returnN,x)
	! if triggerEOF=1 then returnN=-4270 : goto ReadtimesheetFinis
	read #hTimesheet,using Ftimesheet: empNo$,dep,mat xInp,gpd,mat hr ioerr ReadTimesheetErr
	Ftimesheet: form pos 1,c 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
	if env$('client')='Payroll Done Right' then gosub PrdrWorkmansComp ! env$('client')='West Accounting' or

	fn_detail('___timsheet read___') ! r:
	fn_detail('employee='&empNo$,val(empNo$),'department',dep)
	fn_detail('gpd (gross)',gpd)
	fn_detail('hourly Regular',hr(1))
	fn_detail('hourly Overtime',hr(2))
	for x=1 to udim(mat inpX)
		if inpx(x) then fn_detail('inpX('&str$(x)&')',inpx(x))
	nex x
	fn_detail('___________________') ! /r

	goto ReadtimesheetFinis
	ReadTimesheetErr: returnN=-err : goto ReadtimesheetFinis
	ReadtimesheetFinis: fn_readTimesheet=returnN
fnend
	PrdrWorkmansComp: ! r:
		! xInp(6) Other Compensation
		! if other compensation > 0 then
		!   if pay code is 1 hours = 173.33
		!   if pay code is 2 hours = 86.66
		!   if pay code is 3 hours = 80
		!   if pay code is 4 hours = 40
		! else
		!   hours = regular hours + overtime hours
		! end if
		tmphrs=xInp(1)+xInp(2) ! if xInp(6)>0 then tmphrs=saif(payCode) else tmphrs=xInp(1)+xInp(2)
		!     fnStatus('xInp(17) changed to '&str$(round(tmphrs*xInp(17)*.01,2))&' round('&str$(tmphrs)&' * xInp(17)('&str$(xInp(17))&' * .01)',2)
		!     fnStatusPause
		xInp(17)=round(tmphrs*xInp(17)*.01,2) ! inp(17)=round(tmphrs*xInp(17)*.01,2)
	return  ! /r
MainLoop: ! r:  read timesheet, read employee, call calc deduction etc  basically beginning of main loop
	dim empNo$*8
	dim xInp(29)
	if fn_readTimesheet(empNo$,dep,mat xInp,gpd,mat hr)=-4270 then goto Finis ! else  : pr 'readPointA'

	deptKey$=cnvrt$('pic(zzzzzzz#)',val(empNo$))&cnvrt$('pic(zz#)',dep)
	eno=val(empNo$)
	if eno=0 then goto MainLoop
	dim emp$*8
	if emp$<>empNo$ then
		twc=totalWagesYtd=tfy=cafy=deducy=0 ! eicytd=0
		if rtrm$(emp$)<>'' then gosub ReallocateStateByDept
		dim em(16)
		dim hr(2)
		read #hEmployee,using Femployee,key=empNo$: mat em,lpd,totalGrossPay,w4step2,w4Year$,w4Step3,w4step4a,w4step4b,w4step4c nokey EmployeeNoKey
		! if trim$(empNo$)='25' then pr 'wolfe' : pause
		Femployee: form pos 112,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,n 1,C 4,pos 197,4*n 12.2
		! gosub EmployeeRecordToLocal
		! EmployeeRecordToLocal: ! r:
			marital       	=em(1)
			fedExempt     	=em(2)
			stAllowances 	=em(3)
			empStatus     	=em(4)
			payCode       	=em(5)
			ficaCode      	=em(6)
			! eicCode      	=em(7)
			sickCode      	=em(8)
			vaca          	=em(9)
			hrsSick       	=em(10)
			hrsVaca       	=em(11)
			stdWhFed      	=em(12)
			addOnFed      	=em(13)
			stdWhSt       	=em(14)
			addOnSt       	=em(15)
			hireDate      	=em(16)
		! return ! /r
		! gosub CalculateAllDeductionsAllDept
		! CalculateAllDeductionsAllDept: ! r:  returns totalGrossPay,ded,t3 (and probably other stuff, i.e. a % for each dept)
			! Calculate all deduct for federal for all departments
			totalGrossPay=t3=ded=0
							fn_detail('___________________________________________________________________')
							fn_detail('Employee   '&empNo$)
			do
				for j=1 to 20
					if (j+9)=17 and env$('client')='Payroll Done Right' then goto L3090 ! if processing xInp(17) skip IT do not process it.   ! env$('client')='West Accounting' or

					if newdedfed(j)>=1 and deductionCode(j)=dc_deduct then
						! r:  department.tdc1  State Code
							sc1=1
							read #hDepartment,using 'form pos 48,n 2',key=deptKey$: sc1 nokey ignore
							if sc1=0 then sc1=1
							! If env$('client')='Washington Parrish' AND J=3 Then sD3=xInp(J+9)*(gpd+defcompmatch)/100 : Goto L3150 ! add deferred comp to gross for calculating pension deduction
							if newcalcode(j)=1 then sd3=xInp(j+9) 	else  sd3=xInp(j+9)*gpd/100
							stwh(sc1,1)=stwh(sc1,1)-sd3
							if sd3 then fn_detail('reducing stateWh by',sd3,'to',stwh(sc1,1))
						! /r
						if newcalcode(j)=1 then  ded+=xInp(j+9) 	else  ded+=xInp(j+9)*gpd/100
						if ded then fn_detail('ded adjusted ',ded,'newcalcode('&str$(j)&')',newcalcode(j))
					end if
					if newdedfed(j)=2 then
						if newcalcode(j)=1 then   t3+=xInp(j+9) 	else   t3+=xInp(j+9)*gpd/100
						if t3 then fn_detail('t3',t3)
					end if
					L3090: !
				next j


				fn_detail('totalGrossPay+=gpd',totalGrossPay,'before')
				totalGrossPay+=gpd ! hmmm...   doesnt seem to be necessary - maybe it is for multi department paychecks or something i dunno.  not changing
				fn_detail('totalGrossPay+=gpd',totalGrossPay,'after ')


				dim newx$*8
				! read #hTimesheet,using Ftimesheet: newx$,newdep,mat xInp,gpd,mat hr eof L3150
				if fn_readTimesheet(newx$,newdep,mat xInp,gpd,mat hr)=-4270 then goto L3150 ! else  : pr 'readPointB'
			loop while newx$=empNo$
			L3150: !


			workkey$=cnvrt$('pic(zzzzzzz#)',eno)&cnvrt$('pic(zz#)',dep)
			restore #hTimesheet,key>=workkey$:
			if fn_readTimesheet(empNo$,dep,mat xInp,gpd,mat hr)=-4270 then goto Finis ! else  : pr 'readPointC'
		! return  ! /r

		emp$=empNo$
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
			if sickCode>0 then  ! and env$('client')<>'Battlefield'
				hrsSick+=sickCode ! Accrue Sick
				if fn_justTesting then
					fn_detail('did not write breakdown for sick')
				else
					write #hBreakdown,using 'form pos 1,n 8,c 5,n 8,2*n 9.2': eno,'Sick',prd,sickCode,0 ioerr ignore
				end if
			end if
			if vaca>0 then hrsVaca+=vaca ! Accrue Vacation
			if vaca>0  then ! and env$('client')<>'Battlefield'
				if fn_justTesting then
					fn_detail('did not write breakdown for vaca')
				else
					write #hBreakdown,using 'form pos 1,n 8,c 5,n 8,2*n 9.2': eno,'Vac',prd,vaca,0 ioerr ignore
				end if
			end if
		end if
		! /r
		payPeriodsPerYear=fn_payPeriodsPerYear(payCode)
		if payPeriodsPerYear<=0 then
			goto MainLoop
		end if
		if ~disableWithholdingN(esw_federal) then
			fed_wh=fn_federalTax(taxYear,fedpct,totalGrossPay,ded,stdWhFed,fedExempt,payPeriodsPerYear,marital,w4Year$,w4step2,w4Step3,w4step4a,w4step4b,w4step4c)
		end if
		! cafeteria plan - maybe???
		totalWagesYtd=0
		dim stuc(10)
		mat stuc=(0)
		dim tcd(3)
		read #hDepartment,using 'form pos 48,n 2',key=deptKey$: tcd(1) ! get state code
		dim ytdTotal(32)
		dim caf(20)
		fn_determineEarnings(hCheckHistory,eno, dep,begDate,endDate,mat ytdTotal,ytdFICA,ytdMedicare,ytdWages,mat caf)
		for j=1 to 20
			if newdedfed(j)=2 and deductionCode(j)=dc_deduct then
				cafy+=caf(j)
				cafd+=caf(j)
			end if
		next j
		totalWagesYtd+=ytdWages
		tfy+=(ytdFICA+ytdMedicare)
		ficatfy=tfy
		oldsswg=totalWagesYtd-cafy
		! eicytd+=ytdEic
		stuc(tcd(1))+=ytdWages-cafd
		cafd=0
	end if
	!   Where Federal Withholdings are divided out into each department.
	! gpd = gross pay per department
	! pog = percent of gross
	dim tdt(4)
	dim tdet(23)
	read #hDepartment,using 'form pos 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=deptKey$: teno,empDept,gl$,mat tdt,mat tcd,tli,mat tdet ! Nokey X
	if totalGrossPay=0 then pog=1 : goto L1620 ! Allow checks to calculate with 0 gross pay
	if totalGrossPay=gpd then pog=1 : goto L1620
	if totalGrossPay=0 then
		mat ml$(1)
		ml$(1)='Employee Number '&trim$(empNo$)&' skipped Total Gross Pay = 0, Must be Re-entered'
		fnMsgBox(mat ml$,resp$,'',0)
		goto MainLoop
	end if
	pog=gpd/totalGrossPay

	L1620: !
	for j=1 to 20
		if env$('client')='Franklinton' then
			if j=1 and empStatus=3 then ! retirement of firemen  ! franklinton
				xInp(j+7)=round(xInp(j+7)*gpd/100,2)
				goto L1710 ! franklinton
			else
				if j=2 then ! retirement of police !franklinton
					xInp(j+7)=round(xInp(j+7)*((hr(1)*(xInp(1)+xInp(3)+xInp(4))+xInp(6)+xInp(17))/100),2)
					goto L1710 ! franklinton
				end if
			end if
			!   else if env$('client')='Washington Parrish' and j=3 and newcalcode(j)=2 then
			!     xInp(j+7)=round(xInp(j+9)*(gpd+defcompmatch)/100,2)
			!     goto L1700
			!   else if env$('client')='West Accounting' and j=10 and xInp(17)<>0 then
			!     gosub PrdrWorkmansComp
		else

		end if
		if newcalcode(j)=2 then xInp(j+9)=round(xInp(j+9)*gpd/100,2)
		! L1700: !
		if disableWithholdingN(esw_standard) then xInp(j+9)=0
		L1710: !
	next j
	hrsSick-=xInp(3) : hrsVaca-=xInp(4)
	if fn_justTesting then
		if xInp(3)+xInp(4)+xInp(5)>0 then fn_detail('did not write breakdown for sick, vac or hol')
	else
		if xInp(3)>0 then ! write sick hours taken to breakdown file
			write #hBreakdown,using 'form pos 1,n 8,c 5,n 8,2*n 9.2': eno,'Sick',prd,0,xInp(3) ioerr ignore
		end if
		if xInp(4)>0 then ! write vacation hours taken to breakdown file
			write #hBreakdown,using 'form pos 1,n 8,c 5,n 8,2*n 9.2': eno,'Vac',prd,0,xInp(4) ioerr ignore
		end if
		if xInp(5)>0 then ! write holiday hours taken to breakdown file
			write #hBreakdown,using 'form pos 1,n 8,c 5,n 8,2*n 9.2': eno,'Hol',prd,0,xInp(5) ioerr ignore
		end if
	end if
	if sck(4)=999 then sck(4)=1000 ! system will only hold 999 maximum accrued sick hours.  If maximum is set at 999, assume no maximum
	if sck(4)<>0 and hrsSick>sck(4) then hrsSick=sck(4)
	if vacm<>0 and hrsVaca>vacm then hrsVaca=vacm
	ext=0 ! Excess Tips
	! unreferenced lines              goto NoExcessTips
	! unreferenced lines
	! unreferenced lines              if xInp(9) then
	! unreferenced lines              	tr=round(xInp(1)*MinHourlyWage+xInp(2)*MinHourlyWage*1.5,2)
	! unreferenced lines              	g1=gpd-xInp(9)
	! unreferenced lines              	ext=0
	! unreferenced lines              	if g1>=tr then
	! unreferenced lines              		g2=xInp(9)
	! unreferenced lines              	else
	! unreferenced lines              		g2=gpd-tr
	! unreferenced lines              	end if
	! unreferenced lines              end if
	! unreferenced lines              NoExcessTips: !
	! r: FicaUnEmp
		deduc=ficat3=f3=0 ! FICA
		sswg=sswh=mcwh=0
		for j=1 to 20
			if dedfica(j)=1 and deductionCode(j)=dc_deduct then
				ficat3+=xInp(j+9)
			end if
			if deduc(j)=1 then ! total deductions for unemployment for current period and year to date
				deduc+=xInp(j+9)
				deducy+=caf(j)
			end if
		next j
		if totalGrossPay=0 then  ! calculate checks w/ no gross pay
			f3=0
			goto FicaUnEmpFinis
		else
		end if
		if ~disableWithholdingN(esw_fica) then
			if ficaCode=0 then      !           0 - Subject to SS and Med WH
				! r: FicaSsTaxAndMedicare
				! if env$('client')='Washington Parrish' then
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
				! if env$('client')='Washington Parrish' then ! (add deferred comp match to medicare wages)
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
		end if ! ~disableWithholdingN(esw_fica)
		FicaUnEmpFinis: !
	! /r
	! r: FedWh_Dept        Fed WH for Dept ! Federal Withholding for Department
		if showDetails then fn_detail('federal  withholding for department calculating')
		stwh(tcd(1),1)+=gpd
		tcp(tcp_fedWh)       	=round(fed_wh*pog,2)
		tcp(tcp_ssWh)        	=sswh
		tcp(tcp_medicareWh)  	=mcwh
		tcp(tcp_stateWh)     	=tcp4
		for j=5 to 24
			tcp(j)=xInp(j+5)
		next j
		! tcp(tcp_earnedIncomeCredit)=min(0,tcp(tcp_fedWh))
		tcp(tcp_payOvertime)	=round(xInp(2)*hr(2),2)
		tcp(tcp_otherCompensation)=xInp(7)
		tcp(tcp_meals)       	=xInp(8)
		tcp(tcp_tips)        	=xInp(9)
		tcp(tcp_payRegular) 	=gpd-tcp(tcp_payOvertime)-tcp(tcp_otherCompensation)-tcp(tcp_meals)-tcp(tcp_tips)
		tcp(tcp_gross)       	=gpd
		fn_detail('gross is initialized as GDP',tcp(tcp_gross))
		tcp(tcp_net)         	=gpd-tcp(tcp_fedWh)-tcp(tcp_ssWh)-tcp(tcp_medicareWh) ! -tcp(tcp_stateWh)
		fn_detail('net is initialized as GDP-fedWh-ssWh-medicareWh',tcp(tcp_net))
		for j=5 to 24
			dc=deductionCode(j-4)
			if tcp(j) then
				if deductionCode(j-4)=dc_benefit then
					! do nothing
				else if deductionCode(j-4)=dc_add then
					tcp(tcp_net)+=tcp(j)
					fn_detail('Net Pay increased by '&rtrm$(dedname$(j-4)),tcp(j),'to',tcp(tcp_net))
				else if deductionCode(j-4)=dc_deduct then
					tcp(tcp_net)-=tcp(j)
					fn_detail('Net Pay decreased by '&rtrm$(dedname$(j-4)),tcp(j),'to',tcp(tcp_net))
				else if deductionCode(j-4)=dc_addGross then
					tcp(tcp_gross)+=tcp(j)
					fn_detail('Gross Pay increased by '&rtrm$(dedname$(j-4)),tcp(j),'to',tcp(tcp_gross))
					tcp(tcp_net)+=tcp(j)
					fn_detail('Net Pay increased by '&rtrm$(dedname$(j-4)),tcp(j),'to',tcp(tcp_net))
				end if
			end if
		next j
		for j=1 to 31 : tcp(j)=round(tcp(j),2) : next j

		tcp(tcp_net)=tcp(tcp_net)-tcp(tcp_meals)-tcp(tcp_tips) ! tcp(tcp_net)+=tcp(tcp_earnedIncomeCredit)-tcp(tcp_meals)-tcp(tcp_tips)
		if tcp(tcp_meals) or tcp(tcp_tips) then
			fn_detail('net is reduced by meals and tips to',tcp(tcp_net))
		end if
		! if env$('client')='Washington Parrish' then tcp(tcp_net)+=tcp(tcp_tips) ! add tips which is really an other compensation back to net

		! SS_WAGE: !

		if ficaCode=9 then
			tdc(tdc_wagesSs)=0
		else
			tdc(tdc_wagesSs)=round(sswg*ficapog,2)
		end if
		! MEDICARE_WAGE: !
		tdc(tdc_wagesMedicare)=round((totalGrossPay-t3)*ficapog,2)
		tdc(tdc_wagesUcState)=0 ! State U/C Wage
		! if stuc(tcd(1))>=sucw(tcd(1)) then goto L2300
		! L2300: !
		if stuc(tcd(1))+(gpd-ext-deduc)>sucw(tcd(1)) then
			tdc(tdc_wagesUcState)=sucw(tcd(1))-stuc(tcd(1))
		else
			tdc(tdc_wagesUcState)=gpd-ext-deduc
		end if
		if tdc(tdc_wagesUcState)<0 then tdc(tdc_wagesUcState)=0 ! if don't have maximum uc wage in company it will come up with negatives
		tdc(tdc_wagesUcFed)=0 ! Fed U/C Wage
		if feducmax and totalWagesYtd-deducy>=feducmax then
			tdc(tdc_wagesUcFed)=min(max(feducmax-(totalWagesYtd-deducy),0),gpd-ext-deduc)
		else
			tdc(tdc_wagesUcFed)=gpd-ext-deduc
		end if
		tdc(tdc_hoursRegular  	)=xInp(1) ! Hours
		tdc(tdc_hoursOvertime 	)=xInp(2) ! Hours
		tdc(tdc_hoursSick      	)=xInp(3) ! Hours
		tdc(tdc_hoursVacation  	)=xInp(4) ! Hours
		tdc(tdc_hoursHoliday   	)=xInp(5) ! Hours
		! pause ! WORKMANS_COMP: !
		! mat wcm is workman's comp maximum
		! trp is (temp variable only used here)
		! tcp(tcp_payRegular) is Regular Earnings
		! tcp(tcp_payOvertime) is OT Earnings
		! wc is (temp variable only used here)
		! tdc(tdc_wagsWorkmansCompensation) is Workman's Comp Wages

		! if env$('client')='West Accounting' then ! perhaps everyone should be doing it this way -prd 01/06/2016
		!   tcp(14)=tdc(tdc_hoursRegular)*xInp(19)*.01 ! base on regular hours times w/c rate
		!   fn_detail('tcp(14) was set to '&str$(tcp(14))&' by tcp(14) = tdc(tdc_hoursRegular)('&str$(tdc(tdc_hoursRegular))&' * xInp(19)('&str$(xInp(19))&') * .01')
		!   xInp(19)=0   ! <-- nice idea but it does not make a difference
		!   fnStatusPause
		! end if  ! else
		trp=tcp(tcp_payRegular)+tcp(tcp_payOvertime) ! base on wages
		! end if

		wc=0
		if wcm(payCode)=0 or twc+trp<wcm(payCode) then
			wc=trp
		else
			wc=wcm(payCode)-twc
		end if
		twc+=wc : tdc(tdc_wagsWorkmansCompensation)=wc

		if fn_justTesting then
			fn_detail('did not write Department')
		else
			rewrite #hDepartment,using 'form pos 42,n 6,pos 58,23*pd 4.2',key=deptKey$: d1,mat tdet
		end if

		tcp(tcp_stateWh)=0

		! if fn_justTesting then
			fn_detail('***** CHECK RESULTS *****')
			for deductionX=1 to 20
				if dedName$(deductionX)<>'' and tcp(deductionX+4) then
					fn_detail(rtrm$(dedName$(deductionX))&':'&str$(tcp(deductionX+4)))
				end if
			next deductionX
			fn_detail('WH -  Federal',tcp(tcp_fedWh))
			fn_detail('WH -       SS',tcp(tcp_ssWh))
			fn_detail('WH - Medicare',tcp(tcp_medicareWh))
			fn_detail('WH -    State',tcp(tcp_stateWh))
			fn_detail('Pay -     Net',tcp(tcp_net))
			fn_detail('Pay -   Total',tcp(tcp_gross))
		! else
			write #hCheckHistory,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': eno,empDept,prd,0,mat tdc,mat tcp
			fn_detail('***writing check history eno',eno,'dept',empDept)
			fn_detail('***                      net',tcp(tcp_net),'gross',tcp(tcp_gross))
			fn_detail('***                 state WH',tcp(tcp_stateWh),'fed wh',tcp(tcp_fedWh))
		! end if
		! fn_detail('WRITING payroll check with tcp(tcp_stateWh)='&str$(tcp(tcp_stateWh))&' and tcp(tcp_net)='&str$(tcp(tcp_net)))
		! fnStatusPause
		totalWagesYtd+=gpd : cafy+=ficat3 ! eicytd+=ytdTotal(25)
		if tdet(16)<>0 then stuc(tcd(1))+=tdet(16) ! ??? kj
	! /r FedWh_Dept
	if fn_justTesting then triggerEOF=1
goto MainLoop
! /r
Finis: ! r:
	if rtrm$(emp$)<>'' then gosub ReallocateStateByDept
	fn_setupCloseFiles
	close #hTimesheet:
	fnFree('[Q]\PRmstr\jcprh1.h[cno]') ! get rid of jobcost time entry file if exists
	if showDetails then
			fnClosePrn
		showDetails=0
	end if
goto Xit ! /r
	EmployeeNoKey: ! r:
		emp$=''
		mat ml$(1)
		ml$(1)='Employee Number '&empNo$&' is not on file. No check calculated.'
		fnMsgBox(mat ml$,resp$,'',0)
	goto MainLoop ! /r
	def fn_determineEarnings(hCheckHistory,eno,dep,begDate,endDate,mat ytdTotal,&ytdFICA,&ytdMedicare,&ytdWages,mat caf; ___,heno,checkkey$)
		ytdFICA=ytdMedicare=0 ! ytdEic=0
		mat caf=(0)
		mat ytdTotal=(0)
		mat tcp=(0)
		mat tdc=(0)
		checkkey$=cnvrt$('pic(zzzzzzz#)',eno)&cnvrt$('pic(zz#)',dep)&cnvrt$('pd 6',0) ! index employee#,department# and payroll date
		restore #hCheckHistory,key>=checkkey$: nokey DePrCkFinis
		do
			read #hCheckHistory,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': heno,empDept,prDate,ckno,mat tdc,mat tcp eof DePrCkEof
			if heno=eno and prDate=>begDate and prDate<=endDate then
				mat ytdTotal=ytdTotal+tcp
			end if
		loop while heno=eno
		DePrCkEof: !
		ytdFICA     	=ytdTotal( 2) ! fica year to date
		ytdMedicare	=ytdTotal( 3) ! medicare year to date
		ytdWages    	=ytdTotal(31) ! total wages
		for j=1 to 20
			caf(j)=ytdTotal(j+4) ! total miscellaneous deductions for year
		next j
		DePrCkFinis:!
		fn_detail('YTD Earnings Determined:   FICA='&str$(ytdFICA)&'  Medicare='&str$(Medicare)&'  Wages='&str$(ytdWages)&' (from Check History)')
	fnend

def fn_getFederalTable(taxYear,marital,w4Year$,w4step2,mat fedTable,&fed_annual_wh_allowance)
	! retains: setupFederalTables,mat fjs,mat fss,mat fhs,mat fjc,mat fsc,mat fhc,mat ft
	! returns: mat fedTable,fed_annual_wh_allowance
	! https://www.irs.gov/pub/irs-pdf/p15t.pdf
	! https://www.irs.gov/pub/irs-pdf/fw4.pdf
	! https://www.irs.gov/pub/irs-pdf/p15.pdf
	if setupFederalTables<>val(str$(taxYear)&w4Year$) then ! or taxYear<=2019 then
		setupFederalTables=val(str$(taxYear)&w4Year$)
		! if taxYear<=2017 then ! r:
			! dim ft(8,6)
			! if marital=1 or marital=3 or marital=4 then
				! ! 1 - Married - filing jointly
				! ! 3 - Married - filing joint return - only one working
				! ! 4 - Married - filing joint - both working
				! fed_annual_wh_allowance=4050 ! (was 4000)   Withholding allowance. The 2016 amount for one withholding allowance on an annual basis is $4,050
				! ! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
				! ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0
				! ft(2,1)=  2300 : ft(2,2)=     0    : ft(2,3)=0.1
				! ft(3,1)= 11625 : ft(3,2)=   932.5  : ft(3,3)=0.15
				! ft(4,1)= 40250 : ft(4,2)=  5226.25 : ft(4,3)=0.25
				! ft(5,1)= 94200 : ft(5,2)= 18713.75 : ft(5,3)=0.28
				! ft(6,1)=193950 : ft(6,2)= 46643.75 : ft(6,3)=0.33
				! ft(7,1)=419000 : ft(7,2)=120910.25 : ft(7,3)=0.35
				! ft(8,1)=420700 : ft(8,2)=121505.25 : ft(8,3)=0.396
			! else if marital=0 or marital=5 or marital=2 then
				! ! 0 - Single
				! ! 5 - Married - filing seperate - both working
				! ! 2 - Single - Head of Household
				! ! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
				! fed_annual_wh_allowance=4050
				! ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
				! ft(2,4)=  8650  : ft(2,5)=     0    : ft(2,6)=0.1
				! ft(3,4)= 27300  : ft(3,5)=  1865    : ft(3,6)=0.15
				! ft(4,4)= 84550  : ft(4,5)= 10452.5  : ft(4,6)=0.25
				! ft(5,4)=161750  : ft(5,5)= 29752.5  : ft(5,6)=0.28
				! ft(6,4)=242000  : ft(6,5)= 52222.5  : ft(6,6)=0.33
				! ft(7,4)=425350  : ft(7,5)=112728    : ft(7,6)=0.35
				! ft(8,4)=479350  : ft(8,5)=131628    : ft(8,6)=0.396
			! else
				! pr bell;'invalid marital=';marital : pause
			! end if
		! end if ! /r
		! if taxYear=2018 then ! r:
			! if marital=1 or marital=3 or marital=4 then
				! ! 1 - Married - filing jointly
				! ! 3 - Married - filing joint return - only one working
				! ! 4 - Married - filing joint - both working
				! fed_annual_wh_allowance=4150
				! ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0
				! ft(2,1)=  3700 : ft(2,2)=     0    : ft(2,3)=0.1
				! ft(3,1)= 13225 : ft(3,2)=   952.5  : ft(3,3)=0.12
				! ft(4,1)= 42400 : ft(4,2)=  4453.5  : ft(4,3)=0.22
				! ft(5,1)= 86200 : ft(5,2)= 14089.5  : ft(5,3)=0.24
				! ft(6,1)=161200 : ft(6,2)= 32089.5  : ft(6,3)=0.32
				! ft(7,1)=203700 : ft(7,2)= 45689.5  : ft(7,3)=0.35
				! ft(8,1)=503700 : ft(8,2)=150689.5  : ft(8,3)=0.37
			! else if marital=0 or marital=5 or marital=2 then
				! ! 0 - Single
				! ! 5 - Married - filing seperate - both working
				! ! 2 - Single - Head of Household
				! fed_annual_wh_allowance=4150
				! ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
				! ft(2,4)= 11550  : ft(2,5)=     0    : ft(2,6)=0.1
				! ft(3,4)= 30600  : ft(3,5)=  1905    : ft(3,6)=0.12
				! ft(4,4)= 88950  : ft(4,5)=  8907    : ft(4,6)=0.22
				! ft(5,4)=176550  : ft(5,5)= 28179    : ft(5,6)=0.24
				! ft(6,4)=326550  : ft(6,5)= 64179    : ft(6,6)=0.32
				! ft(7,4)=411550  : ft(7,5)= 91379    : ft(7,6)=0.35
				! ft(8,4)=611550  : ft(8,5)=161379    : ft(8,6)=0.37
			! else
				! pr bell;'invalid marital=';marital : pause
			! end if
		! end if ! /r
		! if taxYear=2019 then ! r:
			! if marital=1 or marital=3 or marital=4 then
				! ! 1 - Married - filing jointly
				! ! 3 - Married - filing joint return - only one working
				! ! 4 - Married - filing joint - both working
				! fed_annual_wh_allowance=4200
				! ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0
				! ft(2,1)=  3800 : ft(2,2)=     0    : ft(2,3)=0.1
				! ft(3,1)= 13500 : ft(3,2)=   970    : ft(3,3)=0.12
				! ft(4,1)= 43275 : ft(4,2)=  4543    : ft(4,3)=0.22
				! ft(5,1)= 88000 : ft(5,2)= 14382.5  : ft(5,3)=0.24
				! ft(6,1)=164525 : ft(6,2)= 32748.5  : ft(6,3)=0.32
				! ft(7,1)=207900 : ft(7,2)= 46628.5  : ft(7,3)=0.35
				! ft(8,1)=514100 : ft(8,2)=153798.5  : ft(8,3)=0.37
			! else if marital=0 or marital=5 or marital=2 then
				! ! 0 - Single
				! ! 5 - Married - filing seperate - both working
				! ! 2 - Single - Head of Household
				! fed_annual_wh_allowance=4200
				! ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
				! ft(2,4)= 11800  : ft(2,5)=     0    : ft(2,6)=0.1
				! ft(3,4)= 31200  : ft(3,5)=  1940    : ft(3,6)=0.12
				! ft(4,4)= 90750  : ft(4,5)=  9086    : ft(4,6)=0.22
				! ft(5,4)=180200  : ft(5,5)= 28765    : ft(5,6)=0.24
				! ft(6,4)=333250  : ft(6,5)= 65497    : ft(6,6)=0.32
				! ft(7,4)=420000  : ft(7,5)= 93257    : ft(7,6)=0.35
				! ft(8,4)=624150  : ft(8,5)=164709.5  : ft(8,6)=0.37
			! else
				! pr bell;'invalid marital=';marital : pause
			! end if
		! end if ! /r
		if taxYear=2020 then ! r: 2020 Federal Tables
		! Page 6 from   https://www.irs.gov/pub/irs-pdf/p15t.pdf
		! fjs=federal  joint              standard     fjc=federal  joint              W-4 Step 2 checked
		! fss=federal  single             standard     fsc=federal  single             W-4 Step 2 checked
		! fhs=federal  head of household  standard     fhc=federal  head of household  W-4 Step 2 checked
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
		end if ! /r
		if taxYear=2021 then ! r: 2021 Federal Tables
		! Page 6 from   https://www.irs.gov/pub/irs-pdf/p15t.pdf
		! fjs=federal  joint              standard     fjc=federal  joint              W-4 Step 2 checked
		! fss=federal  single             standard     fsc=federal  single             W-4 Step 2 checked
		! fhs=federal  head of household  standard     fhc=federal  head of household  W-4 Step 2 checked
			fed_annual_wh_allowance=0
			! r: fjs=federal  joint              standard
			dim fjs(8,3)
			fjs(1,1)=     0 : fjs(1,2)=     0     : fjs(1,3)=0     !      0    11900         0    0         0
			fjs(2,1)= 12200 : fjs(2,2)=     0     : fjs(2,3)=0.1   !  11900    31650         0    0.10    11900
			fjs(3,1)= 32100 : fjs(3,2)=  1990     : fjs(3,3)=0.12  !  31650    92150      1975    0.12    31650
			fjs(4,1)= 93250 : fjs(4,2)=  9328     : fjs(4,3)=0.22  !  92150   182950      9235    0.22    92150
			fjs(5,1)=184950 : fjs(5,2)= 29502     : fjs(5,3)=0.24  ! 182950   338500     29211    0.24   182950
			fjs(6,1)=342050 : fjs(6,2)= 67206     : fjs(6,3)=0.32  ! 338500   426600     66543    0.32   338500
			fjs(7,1)=431050 : fjs(7,2)= 95686     : fjs(7,3)=0.35  ! 426600   633950     94735    0.35   426600
			fjs(8,1)=640500 : fjs(8,2)=168993.5   : fjs(8,3)=0.37  ! 633950             167307.5  0.37   633950
			! /r
			! r: fjc=federal  joint              W-4 Step 2 checked
			dim fjc(8,3)
			fjc(1,1)=     0 : fjc(1,2)=    0      : fjc(1,3)=0     !      0   12400      0     0           0
			fjc(2,1)= 12550 : fjc(2,2)=    0      : fjc(2,3)=0.1   !  12400   22275      0     0.1     12400
			fjc(3,1)= 22500 : fjc(3,2)=  995      : fjc(3,3)=0.12  !  22275   52525    987.5   0.12    22275
			fjc(4,1)= 53075 : fjc(4,2)= 4664      : fjc(4,3)=0.22  !  52525   97925   4617.5   0.22    52525
			fjc(5,1)= 98925 : fjc(5,2)=14751      : fjc(5,3)=0.24  !  97925  175700  14605.5   0.24    97925
			fjc(6,1)=177475 : fjc(6,2)=33603      : fjc(6,3)=0.32  ! 175700  219750  33271.5   0.32   175700
			fjc(7,1)=221975 : fjc(7,2)=47843      : fjc(7,3)=0.35  ! 219750  323425  47367.5   0.35   219750
			fjc(8,1)=326700 : fjc(8,2)=87796.75   : fjc(8,3)=0.37  ! 323425          83653.75  0.37   323425
			! /r
			! r: fss=federal  single             standard
			dim fss(8,3)
			fss(1,1)=     0 : fss(1,2)=     0    : fss(1,3)=0     !      0    3800       0    0           0
			fss(2,1)=  3950 : fss(2,2)=     0    : fss(2,3)=0.10  !   3800   13675       0    0.10     3800
			fss(3,1)= 13900 : fss(3,2)=   995    : fss(3,3)=0.12  !  13675   43925     987.5  0.12    13675
			fss(4,1)= 44475 : fss(4,2)=  4664    : fss(4,3)=0.22  !  43925   89325    4617.5  0.22    43925
			fss(5,1)= 90325 : fss(5,2)= 14751    : fss(5,3)=0.24  !  89325  167100   14605.5  0.24    89325
			fss(6,1)=168875 : fss(6,2)= 33603    : fss(6,3)=0.32  ! 167100  211150   33271.5  0.32   167100
			fss(7,1)=213375 : fss(7,2)= 47843    : fss(7,3)=0.35  ! 211150  522200   47367.5  0.35   211150
			fss(8,1)=527550 : fss(8,2)=157804.25 : fss(8,3)=0.37  ! 522200          156235    0.37   522200
			! /r
			! r: fsc=federal  single             W-4 Step 2 checked
			dim fsc(8,3)
			fsc(1,1)=     0 : fsc(1,2)=    0    : fsc(1,3)=0     !      0    6200      0     0          0
			fsc(2,1)=  6275 : fsc(2,2)=    0    : fsc(2,3)=0.1   !   6200   11138      0     0.1     6200
			fsc(3,1)= 11250 : fsc(3,2)=  497.50 : fsc(3,3)=0.12  !  11138   26263    493.75  0.12   11138
			fsc(4,1)= 26538 : fsc(4,2)= 2332.00 : fsc(4,3)=0.22  !  26263   48963   2308.75  0.22   26263
			fsc(5,1)= 49463 : fsc(5,2)= 7375.50 : fsc(5,3)=0.24  !  48963   87850   7302.75  0.24   48963
			fsc(6,1)= 87738 : fsc(6,2)=16801.50 : fsc(6,3)=0.32  !  87850  109875  16635.75  0.32   87850
			fsc(7,1)=110988 : fsc(7,2)=23921.50 : fsc(7,3)=0.35  ! 109875  265400  23683.75  0.35  109875
			fsc(8,1)=268075 : fsc(8,2)=78902.13 : fsc(8,3)=0.37  ! 265400          78117.50  0.37  265400
			! /r
			! r: fhs=federal  head of household  standard
			dim fhs(8,3)
			fhs(1,1)=     0 : fhs(1,2)=     0   : fhs(1,3)=0     !      0   10050       0    0          0
			fhs(2,1)= 10200 : fhs(2,2)=     0   : fhs(2,3)=0.10  !  10050   24150       0    0.10   10050
			fhs(3,1)= 24400 : fhs(3,2)=  1420   : fhs(3,3)=0.12  !  24150   63750    1410    0.12   24150
			fhs(4,1)= 64400 : fhs(4,2)=  6220   : fhs(4,3)=0.22  !  63750   95550    6162    0.22   63750
			fhs(5,1)= 96550 : fhs(5,2)= 13293   : fhs(5,3)=0.24  !  95550  173350   13158    0.24   95550
			fhs(6,1)=175100 : fhs(6,2)= 32145   : fhs(6,3)=0.32  ! 173350  217400   31830    0.32  173350
			fhs(7,1)=219600 : fhs(7,2)= 46385   : fhs(7,3)=0.35  ! 217400  528450   45926    0.35  217400
			fhs(8,1)=533800 : fhs(8,2)=156355   : fhs(8,3)=0.37  ! 528450          154793.5  0.37  528450
			! /r
			! r: fhc=federal  head of household  W-4 Step 2 checked
			dim fhc(8,3)
			fhc(1,1)=     0 : fhc(1,2)=    0     : fhc(1,3)=0     !      0    9325      0    0          0
			fhc(2,1)=  9400 : fhc(2,2)=    0     : fhc(2,3)=0.10  !   9325   16375      0    0.10    9325
			fhc(3,1)= 16500 : fhc(3,2)=  710     : fhc(3,3)=0.12  !  16375   36175    705    0.12   16375
			fhc(4,1)= 36500 : fhc(4,2)= 3110     : fhc(4,3)=0.22  !  36175   52075   3081    0.22   36175
			fhc(5,1)= 52575 : fhc(5,2)= 6646.50  : fhc(5,3)=0.24  !  52075   90975   6579    0.24   52075
			fhc(6,1)= 91850 : fhc(6,2)=16072.50  : fhc(6,3)=0.32  !  90975  113000  15915    0.32   90975
			fhc(7,1)=114100 : fhc(7,2)=23192.50  : fhc(7,3)=0.35  ! 113000  268525  22963    0.35  113000
			fhc(8,1)=271200 : fhc(8,2)=78177.50  : fhc(8,3)=0.37  ! 268525          77396.75 0.37  268525
			! /r
		end if ! /r
		if taxYear=2022 then ! r: 2022 Federal Tables
			! https://www.irs.gov/pub/irs-pdf/p15t.pdf  page 10
			fed_annual_wh_allowance=0
			! r: fjs=federal  joint              standard
			dim fjs(8,3)
			fjs(1,1)=     0 : fjs(1,2)=     0     : fjs(1,3)=0
			fjs(2,1)= 13000 : fjs(2,2)=     0     : fjs(2,3)=0.1
			fjs(3,1)= 33550 : fjs(3,2)=  2055     : fjs(3,3)=0.12
			fjs(4,1)= 96550 : fjs(4,2)=  9615     : fjs(4,3)=0.22
			fjs(5,1)=191150 : fjs(5,2)= 30427     : fjs(5,3)=0.24
			fjs(6,1)=353100 : fjs(6,2)= 69295     : fjs(6,3)=0.32
			fjs(7,1)=444900 : fjs(7,2)= 98671     : fjs(7,3)=0.35
			fjs(8,1)=660850 : fjs(8,2)=174253.5   : fjs(8,3)=0.37
			! /r
			! r: fjc=federal  joint              W-4 Step 2 checked
			dim fjc(8,3)
			fjc(1,1)=     0 : fjc(1,2)=    0      : fjc(1,3)=0
			fjc(2,1)= 12950 : fjc(2,2)=    0      : fjc(2,3)=0.1
			fjc(3,1)= 23225 : fjc(3,2)= 1027.5    : fjc(3,3)=0.12
			fjc(4,1)= 54725 : fjc(4,2)= 4807.5    : fjc(4,3)=0.22
			fjc(5,1)=102025 : fjc(5,2)=15213.5    : fjc(5,3)=0.24
			fjc(6,1)=183000 : fjc(6,2)=34647.5    : fjc(6,3)=0.32
			fjc(7,1)=228900 : fjc(7,2)=49335.5    : fjc(7,3)=0.35
			fjc(8,1)=336875 : fjc(8,2)=87126.75   : fjc(8,3)=0.37
			! /r
			! r: fss=federal  single             standard
			dim fss(8,3)
			fss(1,1)=     0 : fss(1,2)=     0    : fss(1,3)=0
			fss(2,1)=  4350 : fss(2,2)=     0    : fss(2,3)=0.10
			fss(3,1)= 14625 : fss(3,2)=  1027.5  : fss(3,3)=0.12
			fss(4,1)= 46125 : fss(4,2)=  4807.5  : fss(4,3)=0.22
			fss(5,1)= 93425 : fss(5,2)= 15213.5  : fss(5,3)=0.24
			fss(6,1)=174400 : fss(6,2)= 34647.5  : fss(6,3)=0.32
			fss(7,1)=220300 : fss(7,2)= 49335.5  : fss(7,3)=0.35
			fss(8,1)=544250 : fss(8,2)=162718    : fss(8,3)=0.37
			! /r
			! r: fsc=federal  single             W-4 Step 2 checked
			dim fsc(8,3)
			fsc(1,1)=     0 : fsc(1,2)=    0    : fsc(1,3)=0     !      0    6200      0     0          0
			fsc(2,1)=  6475 : fsc(2,2)=    0    : fsc(2,3)=0.1   !   6200   11138      0     0.1     6200
			fsc(3,1)= 11613 : fsc(3,2)=  513.75 : fsc(3,3)=0.12  !  11138   26263    493.75  0.12   11138
			fsc(4,1)= 27363 : fsc(4,2)= 2403.75 : fsc(4,3)=0.22  !  26263   48963   2308.75  0.22   26263
			fsc(5,1)= 51013 : fsc(5,2)= 7606.75 : fsc(5,3)=0.24  !  48963   87850   7302.75  0.24   48963
			fsc(6,1)= 91500 : fsc(6,2)=17323.75 : fsc(6,3)=0.32  !  87850  109875  16635.75  0.32   87850
			fsc(7,1)=114450 : fsc(7,2)=24667.75 : fsc(7,3)=0.35  ! 109875  265400  23683.75  0.35  109875
			fsc(8,1)=276425 : fsc(8,2)=81359    : fsc(8,3)=0.37  ! 265400          78117.50  0.37  265400
			! /r
			! r: fhs=federal  head of household  standard
			dim fhs(8,3)
			fhs(1,1)=     0 : fhs(1,2)=     0   : fhs(1,3)=0
			fhs(2,1)= 10800 : fhs(2,2)=     0   : fhs(2,3)=0.10
			fhs(3,1)= 25450 : fhs(3,2)=  1465   : fhs(3,3)=0.12
			fhs(4,1)= 66700 : fhs(4,2)=  6415   : fhs(4,3)=0.22
			fhs(5,1)= 99850 : fhs(5,2)= 13708   : fhs(5,3)=0.24
			fhs(6,1)=180850 : fhs(6,2)= 33148   : fhs(6,3)=0.32
			fhs(7,1)=226750 : fhs(7,2)= 47836   : fhs(7,3)=0.35
			fhs(8,1)=550700 : fhs(8,2)=161218.5 : fhs(8,3)=0.37
			! /r
			! r: fhc=federal  head of household  W-4 Step 2 checked
			dim fhc(8,3)
			fhc(1,1)=     0 : fhc(1,2)=    0     : fhc(1,3)=0
			fhc(2,1)=  9700 : fhc(2,2)=    0     : fhc(2,3)=0.10
			fhc(3,1)= 17025 : fhc(3,2)=  732.5   : fhc(3,3)=0.12
			fhc(4,1)= 37650 : fhc(4,2)= 3207.5   : fhc(4,3)=0.22
			fhc(5,1)= 54225 : fhc(5,2)= 6854     : fhc(5,3)=0.24
			fhc(6,1)= 94725 : fhc(6,2)=16574     : fhc(6,3)=0.32
			fhc(7,1)=117675 : fhc(7,2)=23918     : fhc(7,3)=0.35
			fhc(8,1)=279650 : fhc(8,2)=80609.25  : fhc(8,3)=0.37
			! /r
		end if ! /r
		if taxYear=>2023 then ! r: 2022 Federal Tables
			! https://www.irs.gov/pub/irs-pdf/p15t.pdf
			if w4year$='2019' then
				fed_annual_wh_allowance=4300 ! every year
				! r: f2m=federal  2019  married
				dim f2m(8,3)
				f2m(1,1)=     0 : f2m(1,2)=     0     : f2m(1,3)=0
				f2m(2,1)= 14800 : f2m(2,2)=     0     : f2m(2,3)=0.1
				f2m(3,1)= 36800 : f2m(3,2)=  2200     : f2m(3,3)=0.12
				f2m(4,1)=104250 : f2m(4,2)= 10294     : f2m(4,3)=0.22
				f2m(5,1)=205550 : f2m(5,2)= 32580     : f2m(5,3)=0.24
				f2m(6,1)=379000 : f2m(6,2)= 74208     : f2m(6,3)=0.32
				f2m(7,1)=477300 : f2m(7,2)=105664     : f2m(7,3)=0.35
				f2m(8,1)=708550 : f2m(8,2)=186601.5   : f2m(8,3)=0.37
				! /r
				! r: f2s=federal  2019  single
				dim f2s(8,3)
				f2s(1,1)=     0 : f2s(1,2)=     0     : f2s(1,3)=0
				f2s(2,1)=  5250 : f2s(2,2)=     0     : f2s(2,3)=0.1
				f2s(3,1)= 16250 : f2s(3,2)=  1100     : f2s(3,3)=0.12
				f2s(4,1)= 49975 : f2s(4,2)=  5147     : f2s(4,3)=0.22
				f2s(5,1)=100625 : f2s(5,2)= 16290     : f2s(5,3)=0.24
				f2s(6,1)=187350 : f2s(6,2)= 37104     : f2s(6,3)=0.32
				f2s(7,1)=236500 : f2s(7,2)= 52832     : f2s(7,3)=0.35
				f2s(8,1)=583375 : f2s(8,2)=174238.25  : f2s(8,3)=0.37
				! /r
			else ! w4year$='2020'
				! r: fss=federal  joint             standard             Top Left
				dim fjs(8,3)
				fjs(1,1)=12*     0 : fjs(1,2)=12*     0    : fjs(1,3)=0
				fjs(2,1)=12*  2308 : fjs(2,2)=12*     0    : fjs(2,3)=0.10
				fjs(3,1)=12*  4142 : fjs(3,2)=12*   183.4  : fjs(3,3)=0.12
				fjs(4,1)=12*  9763 : fjs(4,2)=12*   857.92 : fjs(4,3)=0.22
				fjs(5,1)=12* 18204 : fjs(5,2)=12*  2714.94 : fjs(5,3)=0.24
				fjs(6,1)=12* 32658 : fjs(6,2)=12*  6183.9  : fjs(6,3)=0.32
				fjs(7,1)=12* 40850 : fjs(7,2)=12*  8805.34 : fjs(7,3)=0.35
				fjs(8,1)=12* 60121 : fjs(8,2)=12* 15550.19 : fjs(8,3)=0.37
				! /r
				! r: fjc=federal  joint              W-4 Step 2 checked   Top Right (Married Filing Jointly)
				dim fjc(8,3)
				fjc(1,1)=12*     0 : fjc(1,2)=12*    0      : fjc(1,3)=0
				fjc(2,1)=12*  1154 : fjc(2,2)=12*    0      : fjc(2,3)=0.1
				fjc(3,1)=12*  2071 : fjc(3,2)=12*   91.7    : fjc(3,3)=0.12
				fjc(4,1)=12*  4881 : fjc(4,2)=12*  428.9    : fjc(4,3)=0.22
				fjc(5,1)=12*  9102 : fjc(5,2)=12* 1357.52   : fjc(5,3)=0.24
				fjc(6,1)=12* 16329 : fjc(6,2)=12* 3092      : fjc(6,3)=0.32
				fjc(7,1)=12* 20425 : fjc(7,2)=12* 4402.72   : fjc(7,3)=0.35
				fjc(8,1)=12* 30060 : fjc(8,2)=12* 7774.97   : fjc(8,3)=0.37
				! fjc(1,1)=     0 : fjc(1,2)=    0      : fjc(1,3)=0
				! fjc(2,1)= 13850 : fjc(2,2)=    0      : fjc(2,3)=0.1
				! fjc(3,1)= 24850 : fjc(3,2)= 1100      : fjc(3,3)=0.12
				! fjc(4,1)= 58575 : fjc(4,2)= 5147      : fjc(4,3)=0.22
				! fjc(5,1)=109225 : fjc(5,2)=16290      : fjc(5,3)=0.24
				! fjc(6,1)=195950 : fjc(6,2)=37104      : fjc(6,3)=0.32
				! fjc(7,1)=245100 : fjc(7,2)=52832      : fjc(7,3)=0.35
				! fjc(8,1)=360725 : fjc(8,2)=93300.75   : fjc(8,3)=0.37
				! /r
				! r: fss=federal  single             standard             Middle Left (single or Married Filing Seperately)
							! same as 2019-W4 Single Annual table (pg 64 right side)
				dim fss(8,3)
				fss(1,1)=12*     0 : fss(1,2)=12*     0    : fss(1,3)=0
				fss(2,1)=12*  1154 : fss(2,2)=12*     0    : fss(2,3)=0.10
				fss(3,1)=12*  2071 : fss(3,2)=12*    91.7  : fss(3,3)=0.12
				fss(4,1)=12*  4881 : fss(4,2)=12*   428.9  : fss(4,3)=0.22
				fss(5,1)=12*  9102 : fss(5,2)=12*  1357.52 : fss(5,3)=0.24
				fss(6,1)=12* 16329 : fss(6,2)=12*  3092    : fss(6,3)=0.32
				fss(7,1)=12* 20425 : fss(7,2)=12*  4402.72 : fss(7,3)=0.35
				fss(8,1)=12* 49331 : fss(8,2)=12* 14519.82 : fss(8,3)=0.37
				! /r
				! r: fsc=federal  single             W-4 Step 2 checked   Middle Right (single or Married Filing Seperately)
				dim fsc(8,3)
				fsc(1,1)=12*     0 : fsc(1,2)=12*    0    : fsc(1,3)=0
				fsc(2,1)=12*   577 : fsc(2,2)=12*    0    : fsc(2,3)=0.1
				fsc(3,1)=12*  1035 : fsc(3,2)=12*   45.8  : fsc(3,3)=0.12
				fsc(5,1)=12*  2441 : fsc(5,2)=12*  214.52 : fsc(5,3)=0.24
				fsc(4,1)=12*  4551 : fsc(4,2)=12*  678.72 : fsc(4,3)=0.22
				fsc(6,1)=12*  8165 : fsc(6,2)=12* 1546.08 : fsc(6,3)=0.32
				fsc(7,1)=12* 10213 : fsc(7,2)=12* 2201.44 : fsc(7,3)=0.35
				fsc(8,1)=12* 24666 : fsc(8,2)=12* 7259.99 : fsc(8,3)=0.37
				! /r
				! r: fhs=federal  head of household  standard             Bottom Left (Head of Household)
				dim fhs(8,3)
				fhs(1,1)=12*     0 : fhs(1,2)=12*     0    : fhs(1,3)=0
				fhs(2,1)=12*  1733 : fhs(2,2)=12*     0    : fhs(2,3)=0.1
				fhs(3,1)=12*  3042 : fhs(3,2)=12*   130.9  : fhs(3,3)=0.12
				fhs(4,1)=12*  6721 : fhs(4,2)=12*   572.38 : fhs(4,3)=0.22
				fhs(5,1)=12*  9679 : fhs(5,2)=12*  1223.14 : fhs(5,3)=0.24
				fhs(6,1)=12* 16908 : fhs(6,2)=12*  2958.1  : fhs(6,3)=0.32
				fhs(7,1)=12* 21004 : fhs(7,2)=12*  4268.82 : fhs(7,3)=0.35
				fhs(8,1)=12* 49908 : fhs(8,2)=12* 14385.22 : fhs(8,3)=0.37
				! fhs(1,1)=     0 : fhs(1,2)=     0   : fhs(1,3)=0
				! fhs(2,1)= 12200 : fhs(2,2)=     0   : fhs(2,3)=0.1
				! fhs(3,1)= 27900 : fhs(3,2)=  1570   : fhs(3,3)=0.12
				! fhs(4,1)= 72050 : fhs(4,2)=  6868   : fhs(4,3)=0.22
				! fhs(5,1)=107550 : fhs(5,2)= 14678   : fhs(5,3)=0.24
				! fhs(6,1)=194300 : fhs(6,2)= 35498   : fhs(6,3)=0.32
				! fhs(7,1)=243450 : fhs(7,2)= 51226   : fhs(7,3)=0.35
				! fhs(8,1)=590300 : fhs(8,2)=172623.5 : fhs(8,3)=0.37
				! /r
				! r: fhc=federal  head of household  W-4 Step 2 checked   Bottom Right (Head of Household)
			dim fhc(8,3)
			fhc(1,1)=12*     0 : fhc(1,2)=12*    0     : fhc(1,3)=0
			fhc(2,1)=12*   867 : fhc(2,2)=12*    0     : fhc(2,3)=0.1
			fhc(3,1)=12*  1521 : fhc(3,2)=12*   65.4   : fhc(3,3)=0.12
			fhc(4,1)=12*  3360 : fhc(4,2)=12*  286.08  : fhc(4,3)=0.22
			fhc(5,1)=12*  4840 : fhc(5,2)=12*  611.68  : fhc(5,3)=0.24
			fhc(6,1)=12*  8454 : fhc(6,2)=12* 1479.04  : fhc(6,3)=0.32
			fhc(7,1)=12* 10502 : fhc(7,2)=12* 2134.4   : fhc(7,3)=0.35
			fhc(8,1)=12* 24954 : fhc(8,2)=12* 7192.6   : fhc(8,3)=0.37
			! fhc(1,1)=     0 : fhc(1,2)=    0     : fhc(1,3)=0
			! fhc(2,1)= 10400 : fhc(2,2)=    0     : fhc(2,3)=0.1
			! fhc(3,1)= 18250 : fhc(3,2)=  785     : fhc(3,3)=0.12
			! fhc(4,1)= 40325 : fhc(4,2)= 3434     : fhc(4,3)=0.22
			! fhc(5,1)= 58075 : fhc(5,2)= 7339     : fhc(5,3)=0.24
			! fhc(6,1)=101450 : fhc(6,2)=17749     : fhc(6,3)=0.32
			! fhc(7,1)=126025 : fhc(7,2)=25613     : fhc(7,3)=0.35
			! fhc(8,1)=299450 : fhc(8,2)=86311.75  : fhc(8,3)=0.37
			! /r
			end if
		end if 	! every year /r
	end if
	! r: set mat fedTable
		dim fedTable(8,3)
		! if taxYear<=2019 then ! r:
		! 	mat fedTable(8,6)
		! 	mat fedTable=ft
		! 	if marital=0 then ! 0 - Single
		! 		j2=1
		! 	else if marital=1 or marital=2 or marital=3 or marital=4 or marital=5 then
		! 		! 1 - Married
		! 		! 2 - Single - Head of Household
		! 		! 3 - Married - filing joint return - only one working
		! 		! 4 - Married - filing joint - both working
		! 		! 5 - Married - filing seperate - both working
		! 		j2=4
		! 	end if
		! end if ! /r
		if taxYear=>2020 then ! r:
			j2=0 !  not usedas
			mat fedTable(8,3)
			mat fedTable=(0)
			if w4year$='2019' and (marital=1 or marital=3 or marital=4) then
				mat fedTable=f2m
				fn_detail('Federal Table: 2019 Married')
			else if  w4year$='2019' then
				mat fedTable=f2s
				fn_detail('Federal Table: 2019 Single')
			else if marital=1 or marital=3 or marital=4 then
				! 1 - Married - filing jointly
				! 3 - Married - filing joint return - only one working
				! 4 - Married - filing joint - both working
				if w4step2 then mat fedTable=fjc else mat fedTable=fjs
				fn_detail('Federal Table: Married Filing Jointly')
			else if marital=0 or marital=5 then
				! 0 - Single
				! 5 - Married - filing seperate - both working
				if w4step2 then mat fedTable=fsc else mat fedTable=fss
				fn_detail('Federal Table: Single of Married Filing Seperately')
			else if marital=2 then
				! 2 - Single - Head of Household
				if w4step2 then mat fedTable=fhc else mat fedTable=fhs
				fn_detail('Federal Table: Head of Household')
			else
				pr bell;'invalid marital=';marital : pause
			end if
			if w4year$='2020' then
				if w4step2 then fn_detail('Federal Table: W-4 Step 2 Enabled') else fn_detail('Federal Table: STANDARD')
			end if
		end if ! /r
	! /r
! pause
fnend
def fn_federalTax(taxYear,fedpct,totalGrossPay,ded,stdWhFed,fedExempt,payPeriodsPerYear, _
marital,w4Year$,w4step2,w4Step3,w4step4a,w4step4b,w4step4c; ___,returnN,t2,j2,previousBreak, _
withholdingPercentage,atLeast,baseAmt,estPayPeriodNetPay,estPayPeriodNetPay,adjustedWageAmount)
	! retains: fed_annual_wh_allowance
	! ded = federal deduction addition for all departments (deduct before calculating federal taxes)

	! adjustedWageAmount is local for estAnnualNetPay
	fn_detail('_______federal tax calc__________')

	fn_getFederalTable(taxYear,marital,w4Year$,w4step2,mat fedTable,fed_annual_wh_allowance)

	fn_detail('       totalGrossPay',totalGrossPay)
	fn_detail('                 ded',ded)
	fn_detail('            w4step4b',w4step4b)
	fn_detail('   payPeriodsPerYear',payPeriodsPerYear)
	estPayPeriodNetPay=totalGrossPay-ded-(w4step4b/payPeriodsPerYear)
	fn_detail('   estPayPeriodNetPay=totalGrossPay-ded-(w4step4b/payPeriodsPerYear)',estPayPeriodNetPay)
	if fedpct>0 then
		returnN=round((totalGrossPay-ded)*fedpct,2)
	else if stdWhFed=-1 or disableWithholdingN(esw_federal) then ! disable federal withholding
		returnN=0
	else if stdWhFed then
		returnN=stdWhFed
	else if estPayPeriodNetPay<=0 then
		returnN=withholdingPercentage=0
	else
		fn_detailStep('1.')
		fn_detailStep('1a.',estPayPeriodNetPay)
		adjustedWageAmount=round(estPayPeriodNetPay*payPeriodsPerYear,2)+w4step4a ! adjustedWageAmount (previously g2) - estimated annual net pay
		fn_detail('   adjustedWageAmount=round(estPayPeriodNetPay*payPeriodsPerYear,2)+w4step4a',adjustedWageAmount)
		fn_detail('   estimated annual net pay: '&str$(adjustedWageAmount))
		fn_detailStep('2.')
		if w4Year$='2019' then
			adjustedWageAmount-=fedExempt*fed_annual_wh_allowance
			fn_detail('   (2019) adjustedWageAmount-=fedExempt*fed_annual_wh_allowance',adjustedWageAmount)
		else ! w4Year$='2020' or w4Year$='none' then
			fn_detail('   (2020+) adjustedWageAmount-=w4step3',adjustedWageAmount)
			adjustedWageAmount-=w4step3
		end if
		fn_detail('1h. Adjusted Wage Amount: '&str$(adjustedWageAmount))
		tableRow=fn_tableLine(mat fedTable,adjustedWageAmount)
		atLeast=fedTable(tableRow,1)
		baseAmt=fedTable(tableRow,2)
		fn_detail('  federal table Row='&str$(tableRow))
		fn_detail('            atLeast='&str$(atLeast))
		fn_detail('            baseAmt='&str$(baseAmt))
		withholdingPercentage=fedTable(tableRow,3)
		fn_detailStep('2a. Find the row...',atLeast,'/ppy',atLeast/payPeriodsPerYear)
		fn_detailStep('2b. column C of that row ',baseAmt,'/ppy',baseAmt/payPeriodsPerYear)
		fn_detailStep('2c. % from that row',withholdingPercentage)
		fn_detailStep('2d. Subtract line 2a from line 1h',adjustedWageAmount-atLeast)
		fn_detailStep('2e.  Multiply 2d by 2c percentage', _
			(adjustedWageAmount-atLeast)*withholdingPercentage)
		returnN=baseAmt+(adjustedWageAmount-atLeast)*withholdingPercentage
		fn_detail('   return('&str$(returnN)&')=base('&str$(baseAmt)&')+(adjWage('&str$(adjustedWageAmount)&')-atLeast('&str$(atLeast)&'))*whPercent('&str$(withholdingPercentage)&')')
		returnN=returnN/payPeriodsPerYear
		returnN=round(returnN,2)
		fn_detailStep('2f. Tentative Withholding Amount',returnN)
		fn_detailStep('3.')
		fn_detailStep('3a. Enter the amount from Step 3 of the employee’s Form W-4',w4Step3)
		fn_detailStep('3b. Divide  3a by the number of pay periods on 1b', _
			w4Step3=w4Step3/payPeriodsPerYear)
		fn_detailStep('3c. Subtract 3b from 2f. If zero or less, enter -0- ', _
			returnN=max(returnN-w4Step3,0))
		fn_detailStep('4.')
		fn_detail('   W-4 Step 4C',w4step4c)
		fn_detail('   Federal Tax Add-On',addOnFed)
		fn_detailStep('4a. W-4 Step 4C AND Federal Tax Add-On',w4step4c+addOnFed)
		fn_detailStep('4b. 4a+3d', _
			returnN+=w4step4c+addOnFed)

		fn_detail(' Fed WH Tax Returning '&str$(returnN))
	end if
	FtFinis: !
	! pr 'federal withholding is ';returnN : pause
	fn_detail('_________________________________')
	if returnN<=0 then returnN=0 ! we should never have negative federal withholding.
	fn_federalTax=returnN
fnend

def fn_stateTax(eno,wages,pppy,allowances,marital,fedWh,addOnSt,w4year$,taxYear; clientState$*2)
	if clientState$='' th clientState$=fnpayroll_client_state$

	if clientState$='AR' then
		returnN=fn_wh_arkansas(eno,wages,pppy,allowances,marital)
	else if clientState$='AL' then
		returnN=0 ! no state income tax
	else if clientState$='AZ' then
		returnN=fn_wh_arizona(wages,allowances)
	else if clientState$='FL' then
		returnN=0 ! no state income tax
	else if clientState$='GA' then
		returnN=fn_wh_georgia(taxYear,eno,wages,pppy,marital)
	else if clientState$='IL' then
		returnN=fn_wh_illinois(taxYear,eno,wages,pppy)
	else if clientState$='IN' then
		returnN=fn_wh_indiana(wages,pppy,allowances)
	else if clientState$='KY' then ! added 10/03/2016 for R R Crawford Engineering
		returnN=fn_wh_kentuky(wages,pppy,allowances)
	else if clientState$='LA' then
		returnN=fn_wh_louisiana(eno,marital,wages,pppy,allowances)
	else if clientState$='MO' then
		returnN=fn_wh_missouri(taxYear,wages,marital,fedWh,allowances,pppy)
	else if clientState$='MS' then
		returnN=fn_wh_mississippi(wages,pppy,addOnSt)
	else if clientState$='NV' then
		returnN=0 ! no state income tax
	else if clientState$='OK' then
		returnN=fn_wh_oklahoma(wages,pppy,allowances,marital)
	else if clientState$='OR' then
		returnN=fn_wh_oregon(wages,fedWh,pppy,allowances,marital,w4year$,taxYear)
	else if clientState$='SD' then
		returnN=0 ! no state income tax
	else if clientState$='TN' then
		returnN=0 ! no state income tax
	else if clientState$='TX' then
		returnN=0 ! no state income tax
	else if clientState$='WA' then
		returnN=0 ! no state income tax
	else if clientState$='WY' then
		returnN=0 ! no state income tax
	end if
	returnN=max(0,returnN)
	fn_detail('State Tax Add-On)',addOnSt)
	returnN+=addOnSt
	fn_detail('stateTax (including addOn)',returnN)
	fn_stateTax=returnN
	! pr 'statetax is returning ';returnN : pause
fnend
	! r: fn_stateTax subordionate functions
	def fn_n2(mat n2,n2Index; ___,x,returnN,multiplier)
		! this is working for MO, but not OR so I made fn_n2b
		! n2 stands for middle number - not a great name, but it is goo enough

		! pr 'n2Index='&str$(n2Index)
		for x=2 to n2Index
			multiplier=n2(x,1)
			if x>1 then multiplier-=n2(x-1,1)
			! pr    '   multiplier='&str$(multiplier)
			returnN+=round(multiplier*n2(x-1,3),0)
			! pr '   +=[multiplier='&str$(multiplier)&']+[n2('&str$(x-1)&',3)='&str$(n2(x-1,3))&']   return='&str$(returnN)
		nex x
		! pr 'fn_n2=(mat n2,'&str$(n2Index)&') returns '&str$(returnN) ! pause
		fn_n2=returnN
	fnend
	def fn_debugPrint2d(mat sw; ___,x)
		if env$('acsDeveloper')<>'' then
			for x=1 to udim(mat sw,1)
				pr 'sw('&str$(x)&',1)='&str$(sw(x,1));
				pr ' : sw('&str$(x)&',2)='&str$(sw(x,2));
				if x=1 then
					pr ' : sw('&str$(x)&',2) diff=n/a';
				else
					pr ' : sw('&str$(x)&',2) diff='&str$(sw(x,2)-sw(x-1,2));
				end if
				pr ' : sw('&str$(x)&',3)='&str$(sw(x,3))
			nex x
			pause
		end if
	fnend
	def fn_n2b(mat n2,n2Index; ___,x,returnN,multiplier)
		! n2 stands for middle number - not a great name, but it is goo enough

		pr 'n2Index='&str$(n2Index)
		for x=2 to n2Index
			multiplier=n2(x,1)
			if x>1 then multiplier-=n2(x-1,1)
			pr    '   multiplier='&str$(multiplier)
			returnN+=round(multiplier*n2(x-1,3),0)
			pr '   +=[multiplier='&str$(multiplier)&']+[n2('&str$(x-1)&',3)='&str$(n2(x-1,3))&']   return='&str$(returnN)
		nex x
		pr 'fn_n2=(mat n2,'&str$(n2Index)&') returns '&str$(returnN) : pause
		fn_n2b=returnN
	fnend
	! Tax Table resource(s):
	! https://www.tax-tables.com   <---excellent place to find links to current tax tables for all states
	!                                                                 (click tables in top right of page)
	def fn_wh_arkansas(eno,war_wages_taxable_current,payPeriodsPerYear,allowances, _
		married; ___, _
		s1,annualPersonalCredit,returnN,stAllowances,adjEstAnnWages)
		if setup_arwh<>taxYear then ! r: setup AR Arkansas
			setup_arwh=taxYear
			dim ar(0,0)
			if taxYear<=2020 then ! r:
				mat ar(6,3)
				! Page 1 of http://www.dfa.arkansas.gov/offices/incomeTax/withholding/Documents/whformula.pdf
				! over                              Percentage
				ar(1,1)=    0 : ar(1,2)=   0    :  ar(1,3)=0.009
				ar(2,1)= 4300 : ar(2,2)=  38.7  :  ar(2,3)=0.024
				ar(3,1)= 8400 : ar(3,2)= 137.1  :  ar(3,3)=0.034
				ar(4,1)=12600 : ar(4,2)= 279.9  :  ar(4,3)=0.044
				ar(5,1)=21000 : ar(5,2)= 649.5  :  ar(5,3)=0.059
				ar(6,1)=35100 : ar(6,2)=1481.4  :  ar(6,3)=0.069
				arStandardDeduction=2200
				arPerExemption=20
				! /r
			else if taxYear=2021 then ! r:
				mat ar(12,3)
				! Page 2 of https://www.dfa.arkansas.gov/images/uploads/incomeTaxOffice/whformula.pdf
				! Over         : Minus Adjustment :    Percentage
				ar(1 ,1)=    0 : ar(1 ,2)=   0    :  ar(1 ,3)=0.00
				ar(2 ,1)= 4700 : ar(2 ,2)=  93.98 :  ar(2 ,3)=0.02
				ar(3 ,1)= 9200 : ar(3 ,2)= 185.97 :  ar(3 ,3)=0.03
				ar(4 ,1)=13900 : ar(4 ,2)= 241.57 :  ar(4 ,3)=0.034
				ar(5 ,1)=22900 : ar(5 ,2)= 427.71 :  ar(5 ,3)=0.05
				ar(6 ,1)=38500 : ar(6 ,2)= 774.2  :  ar(6 ,3)=0.059
				ar(7 ,1)=82001 : ar(7 ,2)= 681.7  :  ar(7 ,3)=0.059
				ar(8 ,1)=83001 : ar(8 ,2)= 581.7  :  ar(8 ,3)=0.059
				ar(9 ,1)=84001 : ar(9 ,2)= 481.7  :  ar(9 ,3)=0.059
				ar(10,1)=85301 : ar(10,2)= 381.7  :  ar(10,3)=0.059
				ar(11,1)=86401 : ar(11,2)= 281.7  :  ar(11,3)=0.059
				ar(12,1)=87501 : ar(12,2)= 241.7  :  ar(12,3)=0.059
				arStandardDeduction=2200
				arPerExemption=29
				! /r
			else if taxYear=>2022 then ! r:
				mat ar(52,3)
				! Page 2-3 of https://www.dfa.arkansas.gov/images/uploads/incomeTaxOffice/whformula.pdf
				! "Effective 10/01/2022"
				! Over         : Minus Adjustment :    Percentage
				arStandardDeduction=2270
				arPerExemption=29
				!						Over 	       Percentage  	  Minus Adjustment
				ar( 1,1)=      0 	: ar( 1,3)= 0.000 	: ar( 1,2)=   0
				ar( 2,1)=   5100 	: ar( 2,3)= 0.020 	: ar( 2,2)= 101.98
				ar( 3,1)=  10300 	: ar( 3,3)= 0.030 	: ar( 3,2)= 204.97
				ar( 4,1)=  14700 	: ar( 4,3)= 0.034 	: ar( 4,2)= 263.77
				ar( 5,1)=  24300 	: ar( 5,3)= 0.049 	: ar( 5,2)= 628.25
				ar( 6,1)=  87001 	: ar( 6,3)= 0.049 	: ar( 6,2)= 627.20
				ar( 7,1)=  87101 	: ar( 7,3)= 0.049 	: ar( 7,2)= 617.20
				ar( 8,1)=  87201 	: ar( 8,3)= 0.049 	: ar( 8,2)= 607.20
				ar( 9,1)=  87301 	: ar( 9,3)= 0.049 	: ar( 9,2)= 597.20
				ar(10,1)=  87401 	: ar(10,3)= 0.049 	: ar(10,2)= 587.20
				ar(11,1)=  87601 	: ar(11,3)= 0.049 	: ar(11,2)= 577.20
				ar(12,1)=  87701 	: ar(12,3)= 0.049 	: ar(12,2)= 567.20
				ar(13,1)=  87801 	: ar(13,3)= 0.049 	: ar(13,2)= 557.20
				ar(14,1)=  87901 	: ar(14,3)= 0.049 	: ar(14,2)= 547.20
				ar(15,1)=  88001 	: ar(15,3)= 0.049 	: ar(15,2)= 537.20
				ar(16,1)=  88101 	: ar(16,3)= 0.049 	: ar(16,2)= 527.20
				ar(17,1)=  88201 	: ar(17,3)= 0.049 	: ar(17,2)= 517.20
				ar(18,1)=  88301 	: ar(18,3)= 0.049 	: ar(18,2)= 507.20
				ar(19,1)=  88401 	: ar(19,3)= 0.049 	: ar(19,2)= 497.20
				ar(20,1)=  88501 	: ar(20,3)= 0.049 	: ar(20,2)= 487.20
				ar(21,1)=  88601 	: ar(21,3)= 0.049 	: ar(21,2)= 477.20
				ar(22,1)=  88701 	: ar(22,3)= 0.049 	: ar(22,2)= 467.20
				ar(23,1)=  88801 	: ar(23,3)= 0.049 	: ar(23,2)= 457.20
				ar(24,1)=  88901 	: ar(24,3)= 0.049 	: ar(24,2)= 447.20
				ar(25,1)=  89001 	: ar(25,3)= 0.049 	: ar(25,2)= 437.20
				ar(26,1)=  89101 	: ar(26,3)= 0.049 	: ar(26,2)= 427.20
				ar(27,1)=  89201 	: ar(27,3)= 0.049 	: ar(27,2)= 417.20
				ar(28,1)=  89301 	: ar(28,3)= 0.049 	: ar(28,2)= 407.20
				ar(29,1)=  89401 	: ar(29,3)= 0.049 	: ar(29,2)= 397.20
				ar(30,1)=  89501 	: ar(30,3)= 0.049 	: ar(30,2)= 387.20
				ar(31,1)=  89601 	: ar(31,3)= 0.049 	: ar(31,2)= 377.20
				ar(32,1)=  89701 	: ar(32,3)= 0.049 	: ar(32,2)= 367.20
				ar(33,1)=  89801 	: ar(33,3)= 0.049 	: ar(33,2)= 357.20
				ar(34,1)=  89901 	: ar(34,3)= 0.049 	: ar(34,2)= 347.20
				ar(35,1)=  90001 	: ar(35,3)= 0.049 	: ar(35,2)= 337.20
				ar(36,1)=  90101 	: ar(36,3)= 0.049 	: ar(36,2)= 327.20
				ar(37,1)=  90201 	: ar(37,3)= 0.049 	: ar(37,2)= 317.20
				ar(38,1)=  90301 	: ar(38,3)= 0.049 	: ar(38,2)= 307.20
				ar(39,1)=  90401 	: ar(39,3)= 0.049 	: ar(39,2)= 297.20
				ar(40,1)=  90501 	: ar(40,3)= 0.049 	: ar(40,2)= 287.20
				ar(41,1)=  90601 	: ar(41,3)= 0.049 	: ar(41,2)= 277.20
				ar(42,1)=  90701 	: ar(42,3)= 0.049 	: ar(42,2)= 267.20
				ar(43,1)=  90801 	: ar(43,3)= 0.049 	: ar(43,2)= 257.20
				ar(44,1)=  90901 	: ar(44,3)= 0.049 	: ar(44,2)= 247.20
				ar(45,1)=  91101 	: ar(45,3)= 0.049 	: ar(45,2)= 237.20
				ar(46,1)=  91201 	: ar(46,3)= 0.049 	: ar(46,2)= 227.20
				ar(47,1)=  91301 	: ar(47,3)= 0.049 	: ar(47,2)= 217.20
				ar(48,1)=  91401 	: ar(48,3)= 0.049 	: ar(48,2)= 207.20
				ar(49,1)=  91501 	: ar(49,3)= 0.049 	: ar(49,2)= 197.20
				ar(50,1)=  91601 	: ar(50,3)= 0.049 	: ar(50,2)= 187.20
				ar(51,1)=  91701 	: ar(51,3)= 0.049 	: ar(51,2)= 177.20
				ar(52,1)=  91801  : ar(52,3)= 0.049 	: ar(52,2)= 167.20
				! /r
			end if ! /r
		end if
		adjEstAnnWages=round(war_wages_taxable_current*payPeriodsPerYear,2)
		fn_detail('1. Compute the Annual Gross Pay = '&str$(adjEstAnnWages))
		adjEstAnnWages-=arStandardDeduction
		fn_detail('2. Subtract Standard Deduction = '&str$(adjEstAnnWages))
		if adjEstAnnWages<=88001 and taxYear=>2021 then
			! fn_detail('Adjusted Estimated Annual Wages  before 50 midway:'&str$(adjEstAnnWages))
			adjEstAnnWages=int(adjEstAnnWages/100)*100+50 ! round(adjEstAnnWages+49/50,0)*50
		end if
		fn_detail('3. $50 Midrange Income Lookup = '&str$(adjEstAnnWages))
		tableRow=fn_tableLine(mat ar,adjEstAnnWages)

		if taxYear<=2020 then
			s1=round(ar(tableRow,2)+(adjEstAnnWages-ar(tableRow,1))*ar(tableRow,3),2)
		else if taxYear=>2021 then
			s1=adjEstAnnWages*ar(tableRow,3)-ar(tableRow,2)
		end if
		s1=round(s1,0)
		fn_detail('4. Compute the Annual Gross Tax = '&str$(s1))
		fn_detail('...Multiply by (Percent) = '&str$(ar(tableRow,3)))
		fn_detail('...less bracket adjustment amount = '&str$(ar(tableRow,2)))
		stAllowances=val(fnEmployeeData$(eno,'AR4EC Exemptions'))
		annualPersonalCredit=stAllowances*arPerExemption
		returnN=s1-annualPersonalCredit
		fn_detail('5. Compute the Annual Net Tax = '&str$(returnN))
		returnN=round((returnN)/payPeriodsPerYear,2)
		fn_detail('6. Compute the State Withholding per Pay Period = '&str$(returnN))
		if returnN<.1 then returnN=0
		fn_detail('Adjusted Estimated Annual Wages:'&str$(adjEstAnnWages))
		fn_detail('Standard Deduction:'&str$(arStandardDeduction))
		fn_detail('Per Exemption Amt:'&str$(arPerExemption))
		fn_detail('State Allowances:'&str$(stAllowances))
		fn_detail('annualPersonalCredit (Allowances*PerExemption):'&str$(annualPersonalCredit))
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
	def fn_wh_georgia(taxYear,eno,taxableWagesCurrent,payPeriodsPerYear,married; ___,returnN, _
		dependents,exemptions,tableName$,annualPersonalAllowance,stDeduction, _
		tmp,tmp1,tmp2,tmp4,wagesAnnualTaxable,dependentAllowance, _
		personalAllowance)
		! OLD: wagesAnnual,
		! updated 2/1/2021
		! taxableWagesCurrent - formerly b8
		if ~wga_setup<>taxYear then
			wga_setup=taxYear
			if taxYear and taxYear<=2021 then ! r:
				gaAnnualDependantAllowance=3000
				! r: single Table F Page 45 of Employeer's Tax Guide - Rev Dec 2020
				dim gawhTableF(6,3)
				gawhTableF(1,1)=    0 : gawhTableF(1,2)=   0.00 : gawhTableF(1,3)=0.01
				gawhTableF(2,1)= 1000 : gawhTableF(2,2)=  10.00 : gawhTableF(2,3)=0.02
				gawhTableF(3,1)= 3000 : gawhTableF(3,2)=  50.00 : gawhTableF(3,3)=0.03
				gawhTableF(4,1)= 5000 : gawhTableF(4,2)= 110.00 : gawhTableF(4,3)=0.04
				gawhTableF(5,1)= 7000 : gawhTableF(5,2)= 190.00 : gawhTableF(5,3)=0.05
				gawhTableF(6,1)=10000 : gawhTableF(6,2)= 340.00 : gawhTableF(6,3)=0.0575
				! /r
				! r: single Table G Page 46 of Employeer's Tax Guide - Rev Dec 2020
				dim gawhTableG(6,3)
				gawhTableG(1,1)=    0 : gawhTableG(1,2)=   0.00 : gawhTableG(1,3)=0.01
				gawhTableG(2,1)=  500 : gawhTableG(2,2)=   5.00 : gawhTableG(2,3)=0.02
				gawhTableG(3,1)= 1500 : gawhTableG(3,2)=  25.00 : gawhTableG(3,3)=0.03
				gawhTableG(4,1)= 2500 : gawhTableG(4,2)=  55.00 : gawhTableG(4,3)=0.04
				gawhTableG(5,1)= 3500 : gawhTableG(5,2)=  95.00 : gawhTableG(5,3)=0.05
				gawhTableG(6,1)= 5000 : gawhTableG(6,2)= 170.00 : gawhTableG(6,3)=0.0575
				! /r
				! r: single Table H Page 47 of Employeer's Tax Guide - Rev Dec 2020
				dim gawhTableH(6,3)
				gawhTableH(1,1)=    0 : gawhTableH(1,2)=   0.00 : gawhTableH(1,3)=0.01
				gawhTableH(2,1)=  750 : gawhTableH(2,2)=   7.50 : gawhTableH(2,3)=0.02
				gawhTableH(3,1)= 2250 : gawhTableH(3,2)=  37.50 : gawhTableH(3,3)=0.03
				gawhTableH(4,1)= 3750 : gawhTableH(4,2)=  82.50 : gawhTableH(4,3)=0.04
				gawhTableH(5,1)= 5250 : gawhTableH(5,2)= 142.50 : gawhTableH(5,3)=0.05
				gawhTableH(6,1)= 7000 : gawhTableH(6,2)= 230.00 : gawhTableH(6,3)=0.0575
				! /r
				! /r
			else ! if ~taxYear or taxYear=>2022 then ! r:
				gaAnnualDependantAllowance=3000
				! r: single Table F Page 45 of Employeer's Tax Guide - Rev Dec 2021
				dim gawhTableF(6,3)
				gawhTableF(1,1)=    0 : gawhTableF(1,2)=   0.00 : gawhTableF(1,3)=0.01
				gawhTableF(2,1)= 1000 : gawhTableF(2,2)=  10.00 : gawhTableF(2,3)=0.02
				gawhTableF(3,1)= 3000 : gawhTableF(3,2)=  50.00 : gawhTableF(3,3)=0.03
				gawhTableF(4,1)= 5000 : gawhTableF(4,2)= 110.00 : gawhTableF(4,3)=0.04
				gawhTableF(5,1)= 7000 : gawhTableF(5,2)= 190.00 : gawhTableF(5,3)=0.05
				gawhTableF(6,1)=10000 : gawhTableF(6,2)= 340.00 : gawhTableF(6,3)=0.0575
				! /r
				! r: single Table G Page 46 of Employeer's Tax Guide - Rev Dec 2021
				dim gawhTableG(6,3)
				gawhTableG(1,1)=    0 : gawhTableG(1,2)=   0.00 : gawhTableG(1,3)=0.01
				gawhTableG(2,1)=  500 : gawhTableG(2,2)=   5.00 : gawhTableG(2,3)=0.02
				gawhTableG(3,1)= 1500 : gawhTableG(3,2)=  25.00 : gawhTableG(3,3)=0.03
				gawhTableG(4,1)= 2500 : gawhTableG(4,2)=  55.00 : gawhTableG(4,3)=0.04
				gawhTableG(5,1)= 3500 : gawhTableG(5,2)=  95.00 : gawhTableG(5,3)=0.05
				gawhTableG(6,1)= 5000 : gawhTableG(6,2)= 170.00 : gawhTableG(6,3)=0.0575
				! /r
				! r: single Table H Page 47 of Employeer's Tax Guide - Rev Dec 2021
				dim gawhTableH(6,3)
				gawhTableH(1,1)=    0 : gawhTableH(1,2)=   0.00 : gawhTableH(1,3)=0.01
				gawhTableH(2,1)=  750 : gawhTableH(2,2)=   7.50 : gawhTableH(2,3)=0.02
				gawhTableH(3,1)= 2250 : gawhTableH(3,2)=  37.50 : gawhTableH(3,3)=0.03
				gawhTableH(4,1)= 3750 : gawhTableH(4,2)=  82.50 : gawhTableH(4,3)=0.04
				gawhTableH(5,1)= 5250 : gawhTableH(5,2)= 142.50 : gawhTableH(5,3)=0.05
				gawhTableH(6,1)= 7000 : gawhTableH(6,2)= 230.00 : gawhTableH(6,3)=0.0575
				! /r
				! /r
			end if
		end if
		stDeduction=fn_gaStandardDeduction(married)
		annualPersonalAllowance=fn_gaPersonalAllowance(married)
		if env$('acsDeveloper')<>'' then dev=1 else dev=0
		! wagesAnnual=(taxableWagesCurrent)*payPeriodsPerYear

		wagesAnnualTaxable=wagesAnnual-stDeduction
		wagesAnnualTaxable-=annualPersonalAllowance
		! r: determine table
		if married=0 then ! SINGLE INDIVIDUAL
			mat gawh(udim(mat gawhTableH,1),udim(mat gawhTableH,2))
			mat gawh=gawhTableH
			tableName$='H'
		else if married=4 or married=5 then ! MARRIED FILING JOINT RETURN (both spouses having income) OR MARRIED FILING SEPARATE RETURN
			mat gawh(udim(mat gawhTableG,1),udim(mat gawhTableG,2))
			mat gawh=gawhTableG
			tableName$='G'
		else if married=3 or married=2 or married=1 then ! MARRIED FILING JOINT RETURN (one spouse having income) OR HEAD OF HOUSEHOLD or 1-Married (nothing else known)
			mat gawh(udim(mat gawhTableF,1),udim(mat gawhTableF,2))
			mat gawh=gawhTableF
			tableName$='F'
		else
			pr 'unrecognized married';married : pause
		end if
		fn_detail('   using Table '&tableName$)
		! /r

		exemptions=val(fnEmployeeData$(eno,'Exepmtions'))
		dependents=val(fnEmployeeData$(eno,'Dependents'))
		dependentAllowance=gaAnnualDependantAllowance/payPeriodsPerYear

		fn_detail('Step 1: Total Taxable Wages = '&str$(taxableWagesCurrent))
		fn_detail('    Less Standard Deduction = '&str$(stDeduction/payPeriodsPerYear))
		tmp=taxableWagesCurrent-stDeduction/payPeriodsPerYear
		! fn_detail('                            = '&str$(tmp))

		personalAllowance=round(annualPersonalAllowance/payPeriodsPerYear,2)
		fn_detail( 'Step 2: (a) Less Personal Allowance per Table E = '&str$(personalAllowance))
		fn_detail('        (b) Less Dependent Allowance per Table E $'&str$(dependentAllowance)&' x '&str$(dependents)&' = '&str$(dependentAllowance*dependents))
		totalAllowances=personalAllowance+dependentAllowance*dependents
		fn_detail('            Total allowances                   '&str$(totalAllowances))
		tmp-=totalAllowances
		fn_detail('            Wages subject to withholding       '&str$(tmp))
		tableRow=fn_tableLine(mat gawh,tmp*payPeriodsPerYear)
		tmp1=round(gawh(tableRow,1)/payperiodsperyear,2)
		tmp2=round(gawh(tableRow,2)/payperiodsperyear,2)
		taxRate=gawh(tableRow,3)
		fn_detail('Step 3: Tax on '&str$(tmp1)&' per Table '&tableName$&' = '&str$(tmp2))
		tmp4=round(((tmp-tmp1)*taxRate),2)
		fn_detail('        Tax on excess ('&str$(tmp-tmp1)&' at '&str$(taxRate)&'%)  ='&str$(tmp4))
		returnN=tmp2+tmp4
		fn_detail('        Total tax to be withheld = '&str$(returnN))
		returnN=round(returnN,2) ! round to the nearest whole dollar
		if returnN<.1 then returnN=0 ! do not withhold less than 10 cents.
		fn_wh_georgia=returnN
	fnend
		def fn_gaStandardDeduction(married; ___,returnN)
			if taxYear<=2020 then ! r:
				if married=0 or married=2 then ! Single (or Single - Head of Household)
					standardStateDeduction=2300
				else if married=5 then ! Married - filing seperate - both working
					standardStateDeduction=1500
				else if married=4 then ! Married - filing joint - both working
					standardStateDeduction=3000
				else if married=3 then ! Married - filing joint return - only one working
					standardStateDeduction=3000
				else  ! 1 - Married - (filing status unknown) - just use lowest deduction
					standardStateDeduction=1500
				end if ! /r
			else if taxYear=>2021 then ! r:
				if married=1 or married=3  or married=4 then
					! 1 - Married - filing joint return
					! 3 - Married - filing joint return - only one working
					! 4 - Married - filing joint - both working
					returnN=6000
				else if married=0 or married=2 then
					! 0 - Single
					! 2 - Single - Head of Household
					returnN=4600
				else
					! 5 - Married - filing seperate - both working
					! unknown - just use the lowest deduction
					returnN=3000
				end if ! /r
			end if
			fn_gaStandardDeduction=returnN
		fnend
		def fn_gaPersonalAllowance(married; ___,returnN)
			if taxYear<=2020 then ! r:
				if married=0 or married=2 then
					! Single (or Single - Head of Household)
					returnN=2700
				else if married=3 then ! Married - filing joint - only one working
					returnN=3700
				else if married=4 then ! Married - filing joint - both working
					returnN=7400
				else if married=5 then ! Married - filing seperate - both working
					returnN=3700
				else  ! 1 - Married - (filing status unknown) - just use lowest married deduction
					returnN=3700
				end if ! /r
			else if taxYear=>2021 then ! r:

				if married=1 or married=3  or married=4 then
					! 1 - Married - filing joint return
					! 3 - Married - filing joint return - only one working
					! 4 - Married - filing joint - both working
					returnN=7400
				else if married=0 or married=2 then
					! 0 - Single
					! 2 - Single - Head of Household
					returnN=2700
				else if married=5 then
					! 5 - Married - filing seperate - both working
					returnN=3700
				else
					! unknown - just use the lowest
					returnN=2700
				end if ! /r
			end if
			fn_gaPersonalAllowance=returnN
		fnend
	def fn_wh_illinois(taxYear,eno,taxableWagesCurrent,payPeriodsPerYear; ___,g2,returnN, _
		line1allowances,line2allowances,estAnnualNetPay,exemptionAllowance,incomeTaxRate, _
		exemptions,taxableAmount)
		! 			! no table - updated 1/4/2022 from  https://www2.illinois.gov/rev/forms/withholding/Documents/currentyear/IL-700-T.pdf (Effective January 1, 2022)
		fn_detail('Employee Number',eno                          	)
		fn_detail('Pay Periods Per Year',payPeriodsPerYear     	)
		incomeTaxRate=.0495
		fn_detail('Income Tax Rate' ,incomeTaxRate  	)
		line1allowances=val(fnEmployeeData$(eno,'IL W-4 Line 1 Allowances'))
		line2allowances=val(fnEmployeeData$(eno,'IL W-4 Line 2 Allowances'))
		fn_detail('IL W-4 Line 1 Allowances',line1allowances   	)
		fn_detail('IL W-4 Line 2 Allowances',line2allowances   	)
		if taxYear and taxYear<=2021 then
			exemptionAllowance=2375
		else if taxYear=2022 then
			exemptionAllowance=2425
		else if taxYear=>2023 then
			exemptionAllowance=2625
		end if
		fn_detail('Exemption Allowance',exemptionAllowance)
		estAnnualNetPay=taxableWagesCurrent*payPeriodsPerYear
		fn_detail('Step 1'  ,taxableWagesCurrent,'Annual',estAnnualNetPay)
		fn_detail('Step 2 a',line1allowances*exemptionAllowance)
		fn_detail('       b',line2allowances*1000)
		fn_detail('       c',exemptions    =line1allowances*exemptionAllowance+line2allowances*1000)
		fn_detail('       d',exemptions/payPeriodsPerYear)
		fn_detail('Step 3'  ,-exemptions/payPeriodsPerYear)
		fn_detail('     3'  ,taxableAmount=taxableWagesCurrent-exemptions/payPeriodsPerYear)
		returnN=incomeTaxRate*(estAnnualNetPay-exemptions)
		fn_detai4('Step 4'  ,incomeTaxRate)
		fn_detail('     4'  ,incomeTaxRate*taxableAmount,'Annual',returnN)
		! fn_detail('incomeTaxRate*(estAnnualNetPay-exemptions)',returnN)
		! fn_detail('Annual Tax Estimate',returnN)
		returnN=round(returnN/payPeriodsPerYear,2)
		if returnN<.1 then returnN=0 ! do not withhold less than 10 cents.
		fn_detail('** Current IL State Tax',returnN)
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
	def fn_wh_kentuky(taxableWagesCurrent,payPeriodsPerYear; ___, _
		returnN)
		! revised 2/1/21
		if ~wky_setup then
			wky_setup=1
			stStandardDeduction=2690
			stTaxRate=.05
		end if
		returnN=taxableWagesCurrent*payPeriodsPerYear
		fn_detail('1. Compute annual wages: '&str$(returnN))
		returnN-=stStandardDeduction
		fn_detail('2. Compute Kentucky taxable wages: '&str$(returnN))
		returnN=returnN*stTaxRate
		fn_detail('3. Compute gross annual Kentucky tax: '&str$(returnN))
		returnN=returnN/payPeriodsPerYear
		returnN=round(returnN,2)
		fn_detail('4. Compute Kentucky withholding tax for tax period: '&str$(returnN))
		! if returnN<.1 then returnN=0 ! do not withhold less than 10 cents.
		fn_wh_kentuky=returnN
	fnend
	def fn_wh_louisiana(eno,marital,taxableWagesCurrent,payPeriodsPerYear,stAllowances; _
			___,returnN,h1,h2,h3,stateEarnings,x,y,m1,m2,a,bb,cc,dd,ee, _
			vW,vS,vX,vY,vN,vA,vB,estStTaxableAnnualWages)
		! last updated: 2/3/2023
		if taxYear<=2020 then ! r:
			stateEarnings=round(taxableWagesCurrent,2) ! stateEarnings
			if marital=0 or marital=2 then
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
			returnN=max(0,((a+bb+cc)-(dd+ee))) ! /r
		else if taxYear=2021 or taxYear=2022 then ! r:
			if (marital=0 or marital=2) and val(fnEmployeeData$(eno,'R-1300 Exepmtions'))<2 then
				! they are single and claiming less than 2 exemptions
				fn_detail('1. Single Taxpayer Withholding Formulas') ! r:
				fn_detail('   vW is the withholding tax per pay period.')
				vS=taxableWagesCurrent
				fn_detail('   vS is employee''s salary per pay period for each bracket.',vS)
				vX=val(fnEmployeeData$(eno,'R-1300 Exepmtions'))
				fn_detail('   vX is the number of personal exemptions; X must be 0 or 1.',vX)
				vY=val(fnEmployeeData$(eno,'R-1300 Dependencies'))
				fn_detail('   vY is the number of dependency credits; Y must be a whole number that is 0 or greater.',vY)
				vN=payPeriodsPerYear
				fn_detail('   vN is the number of pay periods.',vN)
						vA=.021*(((vX*4500)+(vY*1000))/vN)
						vA=max(0,round(vA,2))
				fn_detail('   vA is the effect of the personal exemptions and dependency credits equal to or less than $12,500;',vA)
				fn_detail('   vA=.021(((vX*4,500)+(vY*1,000))÷vN).',vA)
						vB=.016*((((vX*4500)+(vY*1000))-12500 )/vN)
						vB=max(0,round(vB,2))
				fn_detail('   vB is the effect of the personal exemptions and dependency credits in excess of $12,500;',vB)
				fn_detail('   vB=.016((((vX*4,500)+(vY*1,000))-12,500)÷vN).',vB)
				estStTaxableAnnualWages=taxableWagesCurrent*payPeriodsPerYear
				if estStTaxableAnnualWages<=12500 then
						vW=.021*(vS)-(vA+vB)
					fn_detail('   If annual wages are less than or equal to $12,500, then',vW)
					fn_detail('   vW=.021(vS)-(vA+vB).',vW)
				else if estStTaxableAnnualWages>12500 and estStTaxableAnnualWages<=50000 then
						vW=.021*(vS)+.0160*(vS-(12500/vN))-(vA+vB)
				fn_detail('   If annual wages are greater $12,500 but less than or equal to $50,000, then',vW)
				fn_detail('   vW=.021(S)+.0160(S-(12,500÷vN))-(vA+vB).',vW)
				else if estStTaxableAnnualWages>50000 then
						vW=.021*(vS)+.0160*(vS-(12500/vN))+.0135*(vS-(50000/vN))-(vA + vB)
				fn_detail('   If annual wages are greater than $50,000, then',vW)
				fn_detail('   vW=.021(S)+.0160(vS-(12,500÷vN))+.0135(vS-(50,000÷vN))-(vA + vB)',vW)
				end if
				! /r
			else
				fn_detail('2. Married Taxpayer Withholding Formulas') ! r:
				fn_detail('   vW is the withholding tax per pay period.')
				vS=taxableWagesCurrent
				fn_detail('   vS is the employee''s salary per pay period for each bracket.',vS)
				vX=val(fnEmployeeData$(eno,'R-1300 Exepmtions'))
				fn_detail('   vX is the number of personal exemptions. vX must 2.',vX)
				vY=val(fnEmployeeData$(eno,'R-1300 Dependencies'))
				fn_detail('   vY is the number of dependency credits. vY must be 0 or greater.',vY)
				vN=payPeriodsPerYear
				fn_detail('   vN is the number of pay periods.',vN)
						vA=.021*(((vX*4500)+(vY*1000))/vN)
						vA=max(0,round(vA,2))
				fn_detail('   vA is the effect of the personal exemptions and dependency credits equal to or less than $25,000;',vA)
				fn_detail('   vA=.021(((vX*4,500)+(vY*1,000))÷vN)',vA)
						vB=.0165*((((vX*4500)+(vY*1000))-25000)/vN)
						vB=max(0,round(vB,2))
				fn_detail('   vB is the effect of the personal exemptions and dependency credits in excess of $25000;',vB)
				fn_detail('   vB=.0165((((X*4500)+(vY*1000))-25000)÷vN).',vB)
				estStTaxableAnnualWages=taxableWagesCurrent*payPeriodsPerYear
				if estStTaxableAnnualWages<=25000 then
					fn_detail('   If annual wages are less than or equal to $25000, then')
							vW=.021*(vS)-(vA+vB)
					fn_detail('   vW=.021(S)-(A+B).',vW)
				else if estStTaxableAnnualWages>25000 and estStTaxableAnnualWages<=100000 then
							vW=.021*(vS)+.0165*(vS-(25000/vN))-(vA+vB)
					fn_detail('   If annual wages are greater $25000 but less than or equal to $100000, then',vW)
					fn_detail('   vW=.021(S)+.0165(vS-(25000÷vN))-(vA+vB).',vW)
				else if estStTaxableAnnualWages>100000 then
							vW=.021*(vS)+.0165*(vS-(25000/vN))+.0135*(vS-(100000/vN))-(vA+vB)
					fn_detail('   If annual wages are greater than $100000, then',vW)
					fn_detail('   vW=.021(S)+.0165(vS-(25000÷vN))+.0135(vS-(100000÷vN))-(vA+vB).',vW)
					! /r
				end if
			end if
			vW=max(0,round(vW,2))
			fn_detail('W',vW)
			fn_detail('S',vS)
			fn_detail('X',vX)
			fn_detail('Y',vY)
			fn_detail('N',vN)
			fn_detail('A',vA)
			fn_detail('B',vB)
			fn_detail('estStTaxableAnnualWages='&str$(estStTaxableAnnualWages))
			returnN=vW
			! /r
		else if taxYear=>2023 then ! r:
			! https://revenue.louisiana.gov/TaxForms/1210(1_22).pdf
			if (marital=0 or marital=2) and val(fnEmployeeData$(eno,'R-1300 Exepmtions'))<2 then
				! they are single OR married claiming less than 2 (0 or 1) exemptions
				fn_detail('A. Withholding Formulas for Single or Married Taxpayers Claiming 0 or 1 Personal Exemption:') ! r:
				fn_detail('   W is the withholding tax per pay period.')
				vS=taxableWagesCurrent
				fn_detail('   S is employee''s salary per pay period for each bracket.', vS)
				vX=val(fnEmployeeData$(eno,'R-1300 Exepmtions'))
				fn_detail('   X is the number of personal exemptions; X must be 0 or 1.', vX)
				vY=val(fnEmployeeData$(eno,'R-1300 Dependencies'))
				fn_detail('   Y is the number of dependency credits; Y must be a whole number that is 0 or greater.', vY)
				vN=payPeriodsPerYear
				fn_detail('   N is the number of pay periods.', vN)

				fn_detail('   A is the effect of the personal exemptions and dependency credits equal to or less than $12,500;', vA)
						vA=.0185*(((vX*4500)+(vY*1000))/vN)
						vA=max(0,round(vA,2))
				fn_detail('   A = .0185(((X * 4500) + (Y * 1000)) ÷ N).', vA)

				fn_detail('   B is the effect of the personal exemptions and dependency credits in excess of $12,500;')
						vB=.0165*((((vX*4500)+(vY*1000))-12500)/vN)
				fn_detail('   B = .0165((((X * 4500) + (Y * 1000)) - 12,500) ÷ N).', vB)
						vB=max(0,round(vB,2))

				estStTaxableAnnualWages=taxableWagesCurrent*payPeriodsPerYear
				if estStTaxableAnnualWages<=12500 then
					fn_detail('   If annual wages are less than or equal to $12,500, then')
						vW=.0185*(vS)-(vA+vB)
					fn_detail('   W = .0185(S) - (A + B).', vW)

				else if estStTaxableAnnualWages>12500 and estStTaxableAnnualWages<=50000 then
					fn_detail('   If annual wages are greater $12,500 but less than or equal to $50,000, then')
						vW=.0185*(vS)+.0165*(vS-(12500/vN))-(vA+vB)
					fn_detail('   W = .0185(S) + .0165( S - (12,500 ÷ N)) - (A + B).', vW)

				else if estStTaxableAnnualWages>50000 then
					fn_detail('   If annual wages are greater than $50,000, then')
						vW=.0185*(vS)+.0165*(vS-(12500/vN))+.0075*(vS-(50000/vN))-(vA+vB)
					fn_detail('   W = .0185(S) + .0165( S - (12,500 ÷ N)) + .0075( S - (50,000 ÷ N)) - (A + B).)', vW)

				end if
				! /r
			else

				fn_detail('B. Withholding Formulas for Married Taxpayers Claiming 2 Personal Exemptions:') ! r:
				fn_detail('   vW is the withholding tax per pay period.')
				vS=taxableWagesCurrent
				fn_detail('   S is the employee''s salary per pay period for each bracket.', vS)
				vX=val(fnEmployeeData$(eno,'R-1300 Exepmtions'))
				fn_detail('   X is the number of personal exemptions. vX must 2.', vX)
				vY=val(fnEmployeeData$(eno,'R-1300 Dependencies'))
				fn_detail('   Y is the number of dependency credits. vY must be 0 or greater.', vY)
				vN=payPeriodsPerYear
				fn_detail('   N is the number of pay periods.', vN)

				fn_detail('   A is the effect of the personal exemptions and dependency credits equal to or less than $25,000;')
						vA=.0185*(((vX*4500)+(vY*1000))/vN)
						vA=max(0,round(vA,2))
				fn_detail('   A = .0185(((X * 4500) + (Y * 1000)) ÷ N)', vA)

				fn_detail('   B is the effect of the personal exemptions and dependency credits in excess of $25000;')
				fn_detail('   B = .0165((((X * 4500) + (Y * 1000)) - 25,000) ÷ N)')
						vB=.0165*((((vX*4500)+(vY*1000))-25000)/vN)
						vB=max(0,round(vB,2))

				estStTaxableAnnualWages=taxableWagesCurrent*payPeriodsPerYear
				if estStTaxableAnnualWages<=25000 then
					fn_detail('   If annual wages are less than or equal to $25000, then')
					fn_detail('   W = .0185(S) - (A + B).')
							vW=.0185*(vS)-(vA+vB)

				else if estStTaxableAnnualWages>25000 and estStTaxableAnnualWages<=100000 then
					fn_detail('   If annual wages are greater $25000 but less than or equal to $100000, then')
							vW=.0185*(vS)+.0165*(vS-(25000/vN))-(vA+vB)
					fn_detail('   W = .0185(S) + .0165(S - (25,000 ÷ N)) - (A + B).',vW)

				else if estStTaxableAnnualWages>100000 then
					fn_detail('   If annual wages are greater than $100000, then')
							vW=.0185*(vS)+.0165*(vS-(25000/vN))+.0075*(vS-(100000/vN))-(vA+vB)
					fn_detail('   W = .0185(S) + .0165(S - (25,000 ÷ N)) + .0075(S - (100,000 ÷ N)) - (A + B)',vW)

					! /r
				end if
			end if
			vW=max(0,round(vW,2))
			fn_detail('W',vW)
			! fn_detail('S='&str$(vS))
			! fn_detail('X='&str$(vX))
			! fn_detail('Y='&str$(vY))
			! fn_detail('N='&str$(vN))
			! fn_detail('A='&str$(vA))
			! fn_detail('B='&str$(vB))
			fn_detail('estStTaxableAnnualWages='&str$(estStTaxableAnnualWages))
			returnN=vW
			! /r
		end if
		returnN=round(returnN,2)
		if returnN<.1 then returnN=0
		fn_wh_louisiana=returnN
	fnend
	def fn_wh_missouri(taxYear,taxableWagesCurrent,marital,fed_wh,allowances,payPeriodsPerYear; _
											___,returnN,tableRow,stStandardDeduction,stAnnualGrossTaxableIncome,h3)
		! revised 1/30/2023
		if setup_mowh<>taxYear then  ! r: MO Missouri
			setup_mowh=taxYear
			if taxYear=2020 then ! r:
				dim mo(9,3)
				! read #h_tables,using 'form pos 31,102*PD 6.4',rec=28: mat mo ! Missouri
				mo( 1,1)=   0 : mo( 1,2)=  0  : mo( 1,3)=0.015
				mo( 2,1)=1053 : mo( 2,2)= 16  : mo( 2,3)=0.02
				mo( 3,1)=2106 : mo( 3,2)= 37  : mo( 3,3)=0.025
				mo( 4,1)=3159 : mo( 4,2)= 63  : mo( 4,3)=0.03
				mo( 5,1)=4212 : mo( 5,2)= 95  : mo( 5,3)=0.035
				mo( 6,1)=5265 : mo( 6,2)=132  : mo( 6,3)=0.04
				mo( 7,1)=6318 : mo( 7,2)=174  : mo( 7,3)=0.045
				mo( 8,1)=7371 : mo( 8,2)=221  : mo( 8,3)=0.05
				mo( 9,1)=8424 : mo( 9,2)=274  : mo( 9,3)=0.054
				stStdDed_single              	=12200
				stStdDed_spouseWorks         	=12200
				stStdDed_filingSeperate      	=12200
				stStdDed_spouseDoesNotWork  	=24400
				stStdDed_headOfHousehold    	=18350
				! /r
			else if taxYear=2021 then ! r:
				dim mo(9,3)
				mo( 1,1)=   0 : mo( 1,2)=fn_n2(mat mo,1)  : mo( 1,3)=0.015
				mo( 2,1)=1088 : mo( 2,2)=fn_n2(mat mo,2)  : mo( 2,3)=0.02
				mo( 3,1)=2176 : mo( 3,2)=fn_n2(mat mo,3)  : mo( 3,3)=0.025
				mo( 4,1)=3264 : mo( 4,2)=fn_n2(mat mo,4)  : mo( 4,3)=0.03
				mo( 5,1)=4352 : mo( 5,2)=fn_n2(mat mo,5)  : mo( 5,3)=0.035
				mo( 6,1)=5440 : mo( 6,2)=fn_n2(mat mo,6)  : mo( 6,3)=0.04
				mo( 7,1)=6528 : mo( 7,2)=fn_n2(mat mo,7)  : mo( 7,3)=0.045
				mo( 8,1)=7616 : mo( 8,2)=fn_n2(mat mo,8)  : mo( 8,3)=0.05
				mo( 9,1)=8704 : mo( 9,2)=fn_n2(mat mo,9)  : mo( 9,3)=0.054
				stStdDed_single              	=12550
				stStdDed_spouseWorks         	=12550
				stStdDed_filingSeperate      	=12550
				stStdDed_spouseDoesNotWork  	=25100
				stStdDed_headOfHousehold    	=18800
				! /r
			else if taxYear=2022 then ! r:
				dim mo(9,3)
				mo( 1,1)=   0 : mo( 1,2)=fn_n2(mat mo,1)  : mo( 1,3)=0.015
				mo( 2,1)=1121 : mo( 2,2)=fn_n2(mat mo,2)  : mo( 2,3)=0.02
				mo( 3,1)=2242 : mo( 3,2)=fn_n2(mat mo,3)  : mo( 3,3)=0.025
				mo( 4,1)=3363 : mo( 4,2)=fn_n2(mat mo,4)  : mo( 4,3)=0.03
				mo( 5,1)=4484 : mo( 5,2)=fn_n2(mat mo,5)  : mo( 5,3)=0.035
				mo( 6,1)=5605 : mo( 6,2)=fn_n2(mat mo,6)  : mo( 6,3)=0.04
				mo( 7,1)=6726 : mo( 7,2)=fn_n2(mat mo,7)  : mo( 7,3)=0.045
				mo( 8,1)=7847 : mo( 8,2)=fn_n2(mat mo,8)  : mo( 8,3)=0.05
				mo( 9,1)=8968 : mo( 9,2)=fn_n2(mat mo,9)  : mo( 9,3)=0.053
				stStdDed_single              	=12950
				stStdDed_spouseWorks         	=12950
				stStdDed_filingSeperate      	=12950
				stStdDed_spouseDoesNotWork  	=25900
				stStdDed_headOfHousehold    	=19400
				! /r
			else if taxYear=>2023 then ! r:
				! https://dor.mo.gov/forms/4282_2023.pdf
				dim mo(8,3)
				mo( 1,1)=   0 : mo( 1,2)=fn_n2(mat mo,1)  : mo( 1,3)=0
				mo( 2,1)=1207 : mo( 2,2)=fn_n2(mat mo,2)  : mo( 2,3)=0.02
				mo( 3,1)=2414 : mo( 3,2)=fn_n2(mat mo,3)  : mo( 3,3)=0.025
				mo( 4,1)=3621 : mo( 4,2)=fn_n2(mat mo,4)  : mo( 4,3)=0.03
				mo( 5,1)=4828 : mo( 5,2)=fn_n2(mat mo,5)  : mo( 5,3)=0.035
				mo( 6,1)=6035 : mo( 6,2)=fn_n2(mat mo,6)  : mo( 6,3)=0.04
				mo( 7,1)=7242 : mo( 7,2)=fn_n2(mat mo,7)  : mo( 7,3)=0.045
				mo( 8,1)=8449 : mo( 8,2)=fn_n2(mat mo,8)  : mo( 8,3)=0.0495
				stStdDed_single              	=13850
				stStdDed_spouseWorks         	=13850
				stStdDed_filingSeperate      	=13850
				stStdDed_spouseDoesNotWork  	=27700
				stStdDed_headOfHousehold    	=20800
				! /r
			end if
			! fn_debugPrzint2d(mat mo)

		end if ! /r
		! r: set stStandardDeduction
		if marital=1 then ! 1 - Married - filing jointly
			stStandardDeduction=stStdDed_spouseWorks
		else if marital=2 then ! 2 - Single - Head of Household
			stStandardDeduction=stStdDed_headOfHousehold
		else if marital=3 then ! 3 - Married - filing joint - only one working
			stStandardDeduction=stStdDed_spouseDoesNotWork
		else if marital=4 then ! 4 - Married - filing joint - both working
			stStandardDeduction=stStdDed_filingSeperate
		else if marital=5 then ! 5 - Married - filing seperate - both working
			stStandardDeduction=stStdDed_filingSeperate
		end if
		fn_detail('State Standard Deduction is '&str$(stStandardDeduction))
		! /r
		stAnnualGrossTaxableIncome=round(taxableWagesCurrent*payPeriodsPerYear,2)
		h3=max(0,stAnnualGrossTaxableIncome-stStandardDeduction)
		tableRow=fn_tableLine(mat mo,h3)
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
	def fn_wh_oklahoma(taxableWagesCurrent,payPeriodsPerYear,allowances,marital; _
			___,estAnnualNetPay,j2,tableRow,returnN)
		!  revised 1/4/2022
		if ~setup_okwh then ! r:
			setup_okwh=1
			dim ok(7,6)
			! read #h_tables,using 'form pos 31,102*PD 6.4',rec=39: mat ok ! Oklahoma
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
		estAnnualNetPay=taxableWagesCurrent*payPeriodsPerYear
		estAnnualNetPay-=allowances*1000
		if marital=0 or marital=2 then j2=1 else j2=4 ! single of married
		tableRow=fn_tableLine(mat ok,estAnnualNetPay)
		returnN=ok(tableRow,j2+1)+(estAnnualNetPay-ok(tableRow,j2))*ok(tableRow,j2+2)
		returnN=returnN/payPeriodsPerYear
		returnN=round(returnN,0)
		fn_wh_oklahoma=returnN
	fnend
	def fn_wh_oregon( taxableWagesCurrent,fedWhEstimate,payPeriodsPerYear, _
										allowances,isMarried, _
										w4year$,taxYear; ___, _
										returnN,allowancesEffective,theyAreSingle,theyAreMarried, _
										standardDeduction,whichTable, _
										perAllowance,wagesAnnualEstimate,phaseOut,tableLine, _
										fedWhAnnualEstimate, _
										orBase,orDebug,preBase,removePrev,taxRate	)
		! requires retaining variables: wor_setup, mat or1, mat or2

		if ~wor_setup then ! r:
			wor_setup=1
			! read #h_tables,using 'form pos 31,102*PD 6.4',rec=40: mat or1,mat or2
			!  r: Withholding Table for Single with fewer than 3 allowances
			dim or1(0,0)
			if taxYear=2019 then ! r:
				mat or1(5,3)
				or1(1,1)=     0 : or1(1,2)=  206    : or1(1,3)=0.05
				or1(2,1)=  3550 : or1(2,2)=  383.5  : or1(2,3)=0.07
				or1(3,1)=  8900 : or1(3,2)=  758    : or1(3,3)=0.09
				or1(4,1)= 50000 : or1(4,2)=  540    : or1(4,3)=0.09
				or1(5,1)=250000 : or1(5,2)=11007    : or1(5,3)=0.099 ! /r
			else if taxYear=2020 then ! r:
				mat or1(4,3)
				or1(1,1)=     0 : or1(1,2)= 210   : or1(1,3)=0.0475
				or1(2,1)=  3600 : or1(2,2)= 381   : or1(2,3)=0.0675
				or1(3,1)=  9050 : or1(3,2)= 749   : or1(3,3)=0.0875
				or1(4,1)= 50000 : or1(4,2)= 4332   : or1(4,3)=0.099 ! /r
			else ! 2021
				mat or1(4,3)
				or1(1,1)=     0 : or1(1,2)=213     : or1(1,3)=0.0475     !!
													!         213												! 0+3650*.0475+213=386.375  (round to 386)
				or1(2,1)=  3650 : or1(2,2)= 386   : or1(2,3)=0.0675     !!
													! (9050-3600)*.0675+381= 748.875  (round to 749)     !!
				! pr fn_n2(mat or1,2) : pause                            !!
				or1(3,1)=  9200 : or1(3,2)= 761   : or1(3,3)=0.0875     !!
													! (50000-9200)*.0875+761=  4331 (round to 4332)  !!
				or1(4,1)= 50000 : or1(4,2)= 4331   : or1(4,3)=0.099     !!

				! fn_debugPrint2d(mat or1)

			end if
			! /r
			dim or2(0,0) ! r: Single with 3 or more allowances, or married
			if taxYear=2019 then ! r:
				mat or2(5,3)
				or2(1,1)=     0 : or2(1,2)=  206 : or2(1,3)=0.05
				or2(2,1)=  7100 : or2(2,2)=  561 : or2(2,3)=0.07
				or2(3,1)= 17800 : or2(3,2)= 1310 : or2(3,3)=0.09
				or2(4,1)= 50000 : or2(4,2)= 1080 : or2(4,3)=0.09
				or2(5,1)=250000 : or2(5,2)=22014 : or2(5,3)=0.099 ! /r
			else if taxYear=2020 then ! r:
				mat or2(4,3)
				or2(1,1)=     0 : or2(1,2)=  210 : or2(1,3)=0.0475
				or2(2,1)=  7200 : or2(2,2)=  552 : or2(2,3)=0.0675
				or2(3,1)= 18100 : or2(3,2)= 1310 : or2(3,3)=0.0875
				or2(4,1)= 50000 : or2(4,2)= 1080 : or2(4,3)=0.09
				! or2(5,1)=250000 : or2(5,2)=22014 : or2(5,3)=0.099 ! /r
			else ! 2021
				mat or2(4,3)
				or2(1,1)=     0 : or2(1,2)=  213 : or2(1,3)=0.0475 !!
				or2(2,1)=  7300 : or2(2,2)=  560 : or2(2,3)=0.0675 !!

				or2(3,1)= 18400 : or2(3,2)= 1309 : or2(3,3)=0.0875 !!

				or2(4,1)= 50000 : or2(4,2)= 4074 : or2(4,3)=0.09   !!
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
			if taxableWagesCurrent>100000 and theyAreSingle then allowancesEffective=0  !!
			if taxableWagesCurrent>200000 and theyAreMarried then allowancesEffective=0 !!

			if theyAreMarried or (theyAreSingle and allowancesEffective>=3) then ! (married or more than 3 allowances)
				whichTable=2
			else ! (single and less than 3 allowances)
				whichTable=1
			end if

			! r: determine:  standardDeduction,perAllowance  provided:  whichTable,taxYear
			if whichTable=2 then ! theyAreMarried then
				if taxYear=2019 then
					standardDeduction=4545
				else if taxYear=2020 then
					standardDeduction=4630
				else ! 2021
					standardDeduction=4700 !!
				end if
			else ! if whichTable=1 then ! if theyAreSingle then
				if taxYear=2019 then
					standardDeduction=2270
					perAllowance=206
				else if taxYear=2020 then
					standardDeduction=2315
					perAllowance=210
				else ! 2021
					standardDeduction=2350 !!
					perAllowance=213  !!
				end if
			end if
			! /r
			! if env$('ACSdeveloper')<>'' then ! r: set orDebug and pr some entry values
			! 	orDebug=1
			! 	pr 'eno=';eno
			! 	pr '  taxableWagesCurrent=';taxableWagesCurrent
			! 	pr '  fedWhEstimate=';fedWhEstimate
			! 	pr '    payPeriodsPerYear=';payPeriodsPerYear
			! 	pr '           allowances=';allowances
			! 	pr '            isMarried=';isMarried
			! 	pr '              w4year$=';w4year$
			! 	pr '              taxYear=';taxYear
			! en if ! /r

			wagesAnnualEstimate=taxableWagesCurrent*payPeriodsPerYear

			fedWhAnnualEstimate=fedWhEstimate*payPeriodsPerYear ! needed to be annual, not pay period LS 12/31/18

			phaseOut=fn_oregonPhaseOut(wagesAnnualEstimate,fedWhAnnualEstimate,whichTable,theyAreSingle,theyAreMarried)

			if orDebug then
				pr '  fed Wh Estimate=';fedWhEstimate
				pr '  fed Wh Annual Estimate=';fedWhAnnualEstimate
				pr '  phaseOut=';phaseOut
			end if


			! orBase=taxableWagesCurrent*payPeriodsPerYear-min(fedWhAnnualEstimate,8550)-(allowancesEffective*2250)
			! fnStatus('BASE=wagesAnnualEstimate-phaseOut-standardDeduction')
			! fnStatus('BASE='&str$(wagesAnnualEstimate)&'-'&str$(phaseOut)&'-'&str$(standardDeduction))
			orBase=wagesAnnualEstimate-phaseOut-standardDeduction
			! fnStatus('BASE='&str$(orBase))

				!
			if whichTable=2 then
				tableLine=fn_tableLine(mat or2,orBase)
				removePrev=or2(tableLine,1)
				preBase=or2(tableLine,2)
				taxRate=or2(tableLine,3)
			else ! whichTable=1
				tableLine=fn_tableLine(mat or1,orBase)
				removePrev=or1(tableLine,1)
				preBase=or1(tableLine,2)
				taxRate=or1(tableLine,3)
			end if


			!      WH = 1,244 + [(BASE – 16,900        ) * 0.09] – (206 * allowances) ! 2019
			!      WH = $749 + [(BASE – $9,050) x 0.0875] – ($210 x allowances) ! 2020


			!      WH =  $761 + [(BASE ­ $9,200) x 0.0875] ­ ($213 x allowances) ! 2021

			! returnN=or2(tableLine,2)+(orBase-or2(tableLine,1))*or2(tableLine,3)
			returnN = preBase +(( orBase - removePrev) * taxRate) - (perAllowance * allowancesEffective)



		end if
		! fnStatus(' ** annual withholding estimate = '&str$(returnN))
		returnN=returnN/payPeriodsPerYear
		returnN=round(returnN,2)
		if returnN<.1 then returnN=0


			if showDetails then ! r: display all the details
				fnStatus('-------------------------------------------')
				if theyAreSingle then
					fnStatus('  Single with '&str$(allowancesEffective)&' allowances')
				else if theyAreMarried then
					fnStatus('  Married with '&str$(allowancesEffective)&' allowances')
				else
					fnStatus('  Maridal Status is undetermined!!!  ')
				end if
				fnStatus('    Current Wage (Gross)      = '&str$(taxableWagesCurrent))
				fnStatus('    Pay Periods Per Year      = '&str$(payPeriodsPerYear))
				fnStatus('    Annual wage (estimate)    = '&str$(wagesAnnualEstimate))
				fnStatus('    standard deduction        = '&str$(standardDeduction))
				fnStatus('    phase out                 = '&str$(phaseOut))
				fnStatus('    fed WH Estimate PayPeriod = '&str$(fedWhEstimate))
				fnStatus('    fed WH Estimate Annual    = '&str$(fedWhAnnualEstimate))
				fnStatus('.')
				fnStatus('    BASE = '&str$(wagesAnnualEstimate)&' (wagesAnnualEstimate) - '&str$(phaseOut)&' (phaseOut) - '&str$(standardDeduction)&' (standardDeduction)')
				fnStatus('    BASE = '&str$(orBase))
				fnStatus('.')
				fnStatus('    table '&str$(whichTable)&' line '&str$(tableLine))
				fnStatus('    removePrev            = '&str$(removePrev))
				fnStatus('    preBase               = '&str$(preBase))
				fnStatus('    taxRate               = '&str$(taxRate))
				fnStatus('.')
				fnStatus('         WH = '&str$(preBase)&' (preBase) + (('&str$(orBase)&' (Base) - '&str$(removePrev)&' (removePrev) )) x '&str$(taxRate)&' (taxRate) ) - ('&str$(perAllowance)&' x '&str$(allowancesEffective)&')')
				fnStatus('         WH = '&str$(returnN))
				fnStatus('-------------------------------------------')
			end if ! /r


		! if showDetails then fnStatusPause ! pause
		fn_wh_oregon=returnN
	fnend
		def fn_oregonPhaseOut(opo_wages,opo_fed_wh,opo_table,isSingle,isMarried; ___,returnN)
			if opo_wages<50000 then
				if taxYear=2020 then
					returnN=min(opo_fed_wh,6800)
				else ! taxYear=2021
					returnN=min(opo_fed_wh,7050)
				end if
			else if opo_table=1 then
				if opo_wages => 50000 and opo_wages<125000 th returnN= 6950 : goto Opo_Finis
				if opo_wages =>125000 and opo_wages<130000 th returnN= 5550 : goto Opo_Finis
				if opo_wages =>130000 and opo_wages<135000 th returnN= 4150 : goto Opo_Finis
				if opo_wages =>135000 and opo_wages<140000 th returnN= 2750 : goto Opo_Finis
				if opo_wages =>140000 and opo_wages<145000 th returnN= 1350 : goto Opo_Finis
				if opo_wages =>145000 then returnN=0

			else ! if opo_table=2 then
				if opo_wages => 50000 and opo_wages<250000 then returnN= 6950 : goto Opo_Finis
				if opo_wages =>250000 and opo_wages<260000 then returnN= 5550 : goto Opo_Finis
				if opo_wages =>260000 and opo_wages<270000 then returnN= 4150 : goto Opo_Finis
				if opo_wages =>270000 and opo_wages<280000 then returnN= 2750 : goto Opo_Finis
				if opo_wages =>280000 and opo_wages<290000 then returnN= 1350 : goto Opo_Finis
				if opo_wages =>290000 then returnN=0
			end if
			Opo_Finis: !
			fn_oregonPhaseOut=returnN
		fnend
	! /r


ReallocateStateByDept: ! r:   (...;___,s3,tcp4,oldeno...) (reallocate state taxes based on earnings by dept and state
	fn_detail('*ReallocateStateByDept*')
	s3=tcp(tcp_stateWh)=tcp4=0
	! tcp4 - is the state withholding for the transaction record
	oldeno=val(emp$)
	restore #hDepartment,key>=cnvrt$('pic(zzzzzzz#)',oldeno)&cnvrt$('pic(zz#)',0):
	if stdWhSt=-1 then goto EoStDeptLoop
	! Read #hDepartment,Using 610,Rec=TRA: tdt(4),TCD(1),ty4,tqm4,tcp4,tcp31,TCP22,NTA,MAT DST
	do
		TopStDeptLoop: !
		read #hDepartment,using 'form pos 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,empDept,gl$,mat tdt,mat tcd,tli,mat tdet eof EoStDeptLoop
		if teno=oldeno then
			if showDetails then fnStatus('department read employee '&str$(eno)&' department '&str$(empDept))
			if d1><tdt(4) then goto TopStDeptLoop
			holdEmpDept=empDept
			olddeptkey$=cnvrt$('pic(zzzzzzz#)',oldeno)&cnvrt$('pic(zz#)',holdEmpDept)
			read #hCheckHistory,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2',key=cnvrt$('pic(zzzzzzz#)',oldeno)&cnvrt$('pic(zz#)',empDept)&cnvrt$('pd 6',prd): heno,empDept,prDate,ckno,mat tdc,mat tcp nokey TopStDeptLoop
			if showDetails then fnStatus('read check history: emp$='&emp$&',empDept='&str$(empDept)&',prDate='&str$(prDate)&',ckno='&str$(ckno)&'...')
			dst3=0
			for j=1 to 20
				if dedst(j)>0 then dst3=dst3+tcp(j+4)
			next j
			! sTWH(tcd(1),1)=STWH(tcd(1),1)-DST3
			if stwh(tcd(1),1)=0 then
				goto TopStDeptLoop
			else if stwh(tcd(1),2)><0 then
				goto L870
			else if stdWhSt then
				if ~disableWithholdingN(esw_standard) then stwh(tcd(1),2)=stdWhSt
				goto L870
			end if
			if disableWithholdingN(esw_state) then s3=0: goto L860
				! tcd(1) = state code
				! payPeriodsPerYear     = number of pay periods per year (formerly b8)
				! stAllowances  = allowances
				! marital  = married (1=yes and more )
				! stAllowances - number of allowances
				! payPeriodsPerYear = number of pay periods (formerly b8)
			if tcd(1)=1 then
				s3=fn_stateTax(eno,stwh(tcd(1),1),payPeriodsPerYear,stAllowances,marital,fed_wh,addOnSt,w4year$,taxYear) ! ST01:
			else if tcd(1)=2  then
				s3=0
			else if tcd(1)=3  then
				s3=0
			else if tcd(1)=4  then
				s3=0
			else if tcd(1)=5  then
				s3=0
			else if tcd(1)=6  then
				s3=0
			else if tcd(1)=7  then
				s3=0
			else if tcd(1)=8  then
				s3=0
			else if tcd(1)=9  then
				s3=0
			else if tcd(1)=10 then
				s3=0
			end if

			L860: !
			stwh(tcd(1),2)=s3
			L870: !
			tcp4=(stwh(tcd(1),2))*((tcp(tcp_gross)-dst3)/stwh(tcd(1),1))
			tcp4=round(tcp4,2)
			if disableWithholdingN(esw_state) then tcp4=0
			tcp(tcp_net)-=tcp4
			fn_detail('net adjusted in ReallocateStateByDept',tcp(tcp_net))
			tcp(tcp_stateWh)=tcp4
			rewritekey$=cnvrt$('pic(zzzzzzz#)',oldeno)&cnvrt$('pic(zz#)',holdEmpDept)&cnvrt$('pd 6',prd) ! index employee#,department# and payroll date
			rewrite #hCheckHistory,using 'form pos 80,pd 5.2,pos 220,pd 5.2',key=rewritekey$: tcp(tcp_stateWh),tcp(tcp_net)
			fn_detail('rewriting check history stateWh',tcp(tcp_stateWh),'net',tcp(tcp_net))
			! fn_report_stuff
			! def fn_report_stuff
				! fnStatus('check completely calcualated for eno '&str$(eno))
				! fnStatus('tcp(tcp_fedWh)='&str$(tcp(tcp_fedWh)))
				! fnStatus('tcp(tcp_ssWh)='&str$(tcp(tcp_ssWh)))
				! fnStatus('tcp(tcp_medicareWh)='&str$(tcp(tcp_medicareWh)))
				! fnStatus('tcp(tcp_stateWh)='&str$(tcp(tcp_stateWh)))
				! fnStatus('tcp(13)='&str$(tcp(13)))
				! fnStatus('tcp(14)='&str$(tcp(14)))
				! fnStatus('tcp(tcp_net)='&str$(tcp(tcp_net)))
				! fnStatusPause
			! fnend
			rewrite #hDepartment,using 'form pos 42,n 6',key=olddeptkey$: tdt(4)
		end if
	loop while teno=oldeno
	EoStDeptLoop: !
	! gosub EmployeeLocalToRecord
	! EmployeeLocalToRecord: ! r:
		em(1)  = marital
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
	! return ! /r
	rewrite #hEmployee,using Femployee,key=emp$: mat em,d1,totalGrossPay
	if fp(d1*.01)>.9 then hd1=19000000+fncd(d1) else hd1=20000000+fncd(d1)
	mat stwh=(0)
return ! /r

def library fnCheckPayrollCalculation(; ___, _
	returnN, _
	lc,respc,col1_len, _
	marital,payCode,allowances,stateAddOn,w4Year$,wages,pppy,fedWh, _
	col1_pos,col1_len,col2_pos,col2_len,testStateTax,testFederalTax,w4step2$,w4step2,x,pass)
	! mat resp$,resp_*
	if ~setup_checkCalculation then ! r:
		setup_checkCalculation=1
		if ~setup then fn_setup
		dim marriedOption$(0)*58
		dim w4yearOption$(0)*4
		dim payPeriodOption$(0)*16
		fnGetEmpOptions(mat marriedOption$,mat w4yearOption$,mat payPeriodOption$)
		dim exemptionOption$(22)
		for j=1 to 21
			exemptionOption$(j)=str$(j-1)
		next j
		exemptionOption$(22)='99'
		clientState$=fnpayroll_client_state$
	end if ! /r
	fn_setupOpenFiles
	do
		CpcTopOfScreen: !
		fnTos ! r: Test State Calculation Ask Criteria Screen
		lc=respc=0
		col1_pos=1 : col1_len=25 : col2_pos=col1_pos+col1_len+1 : col2_len=25

		fnLbl(lc+=1,col1_pos,'Current Taxable Wages:',col1_len,1)
		fnTxt(lc   ,col2_pos,10,10,0,'32',0,'If you wish for the system to add additional Federal withholdings, enter that amount here.')
		fnPcReg_read('current taxable wages',resp$(resp_wages=respc+=1),'35000')

		lc+=1

		fnLbl(             lc+=1,col1_pos,'Marital Status:',col1_len,1)
		fnComboA('Marital',lc   ,col2_pos,mat marriedOption$,'',col2_len)
		fnPcReg_read('Marital Status',resp$(resp_married=respc+=1)=fnSetForCombo$(mat marriedOption$,str$(marital)), marriedOption$(4))

		fnLbl(              lc+=1,col1_pos,'Pay Code:',col1_len,1)
		fnComboA('payCode', lc    ,col2_pos,mat payPeriodOption$,'',16)
		payCode=fnPcReg_read('Pay Code',resp$(resp_payCode=respc+=1)=fnSetForCombo$(mat payPeriodOption$,str$(payCode)), payPeriodOption$(5))
		lc+=1
		fnLbl(lc+=1,col1_pos,'Tax Year:',col1_len,1)
		fnTxt(lc   ,col2_pos,4,4,0,'30')
		tmp$=date$('ccyy')
		fnPcReg_read('Tax Year',resp$(resp_taxYear=respc+=1), date$('ccyy'))
		lc+=1
		fnLbl(             lc+=1,col1_pos,'W-4 Year:',col1_len,1)
		fnComboA('w4Year', lc   ,col2_pos,mat w4yearOption$,'Only used if W-4 Year is set to 2020 or later.',5)
		fnPcReg_read('W-4 Year',resp$(resp_w4year=respc+=1), w4yearOption$(1))
		fnLbl(             lc,col1_pos+35,'Federal Exemptions:',col1_len,1)
		fnComboA('StateEx',lc   ,col2_pos+35,mat exemptionOption$,'',3)
		fnPcReg_read('Federal Exemptions',resp$(resp_fedExeptions=respc+=1)=fnSetForCombo$(mat exemptionOption$,str$(allowances)), exemptionOption$(1))
		lc+=1
		fnChk(lc+=1,46,'W-4 Step 2',1)
		fnPcReg_read('W-4 Step 2',resp$(resp_w4step2=respc+=1), w4step2$)


		lc+=1
		fnLbl(lc+=1,col1_pos,'Federal Withholding:',col1_len,1)
		fnLbl(lc,col2_pos+10+2,'(Use to Override for State Calculation)')
		fnTxt(lc   ,col2_pos,10,10,0,'32',0,'')
		fnPcReg_read('Federal Withholding',resp$(resp_fedWh=respc+=1),'1530.71')

		lc+=1
		fnLbl(lc+=1,col1_pos,'State Exemptions:',col1_len,1)
		fnComboA('StateEx',lc,col2_pos,mat exemptionOption$,'',3)
		fnPcReg_read('State Exemptions',resp$(resp_stExeptions=respc+=1)=fnSetForCombo$(mat exemptionOption$,str$(allowances)), exemptionOption$(1))

		fnLbl(lc+=1,col1_pos,'State Tax Add-On:',col1_len,1)
		fnTxt(lc   ,col2_pos,10,10,0,'32',0,'If you wish for the system to add additional state withholdings, enter that amount here.')
		fnPcReg_read('State Tax Add-On',resp$(resp_StateAddOn=respc+=1), '0')

		if clientState$='IL' then ! r:
			lc+=1
			fnLbl(lc+=1,1,'IL W-4 Line 1 Allowances:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_stateExemptions1=respc+=1)=fnEmployeeData$(0,'IL W-4 Line 1 Allowances')
			fnLbl(lc+=1,1,'IL W-4 Line 2 Allowances:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_ilw4line2=respc+=1)=fnEmployeeData$(0,'IL W-4 Line 2 Allowances')
		else if clientState$='AR' then
			lc+=1
			fnLbl(lc+=1,1,'AR4EC Exemptions:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_stateExemptions1=respc+=1)=fnEmployeeData$(0,'AR4EC Exemptions')
		else if clientState$='LA' then
			lc+=1
			fnLbl(lc+=1,1,'R-1300 Exepmtions:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_stateExemptions1=respc+=1)=fnEmployeeData$(0,'R-1300 Exepmtions')
			fnLbl(lc+=1,1,'R-1300 Dependencies:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_stDependents=respc+=1)=fnEmployeeData$(0,'R-1300 Dependencies')
		else if clientState$='GA' then
			lc+=1
			fnLbl(lc+=1,1,'Exepmtions:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_stExemptions=respc+=1)=fnEmployeeData$(0,'Exepmtions')
			fnLbl(lc+=1,1,'Dependents:',col1_len,1)
			fnTxt(lc,col2_pos,5, 0,1,'30',0,'')
			resp$(rc_stDependents=respc+=1)=fnEmployeeData$(0,'Dependents')
		end if ! /r

		if env$('acsDeveloper')<>'' then ! not ready for prime time yet
			fnCmdKey('Test Check' 	,ck_check      	=4,0,0,'Rerun the calculation from a past check')
		end if

		fnCmdKey('Test Federal' 	,ck_federal    	=3,0,0,'Print Federal Table for selected Pay Code')
		fnCmdKey('Test State'   	,ck_state      	=2,0,0,'Print Federal Table for selected Pay Code')
		fnCmdKey('Test Both'    	,ck_both       	=1,1,0,'')
		fnCmdKey('Federal Table'	,ck_federalTable	=7,0,0,'Print Federal Tables')
		fnCmdKey('Cancel'        	,5,0,1,'')
		ckey=fnAcs(mat resp$) ! /r
		if ckey=5 then
			goto XitCheckStateCalculation
		else
			! r: do test the calc. resp_f
			fnPcReg_write('current taxable wages'  	,resp$(resp_wages))
			fnPcReg_write('Federal Withholding'    	,resp$(resp_fedWh))
			fnPcReg_write('Marital Status'          	,resp$(resp_married))
			fnPcReg_write('Pay Code'                	,resp$(resp_payCode))
			fnPcReg_write('State Exemptions'       	,resp$(resp_stExeptions))
			fnPcReg_write('Federal Exemptions'     	,resp$(resp_fedExeptions))
			fnPcReg_write('State Tax Add-On'       	,resp$(resp_StateAddOn))
			fnPcReg_write('W-4 Year'                	,resp$(resp_w4year))
			fnPcReg_write('Tax Year'                	,resp$(resp_taxYear))
			fnPcReg_write('W-4 Step 2'             	,resp$(resp_w4step2))
			if clientState$='IL' then
				fnEmployeeData$(0,'IL W-4 Line 1 Allowances',resp$(rc_stateExemptions1))
				fnEmployeeData$(0,'IL W-4 Line 2 Allowances',resp$(rc_ilw4line2))
			else if clientState$='AR' then
				fnEmployeeData$(0,'AR4EC Exemptions',resp$(rc_stateExemptions1))
			else if clientState$='LA' then
				fnEmployeeData$(0,'R-1300 Exepmtions',resp$(rc_stateExemptions1))
				fnEmployeeData$(0,'R-1300 Dependencies',resp$(rc_stDependents))
			else if clientState$='GA' then
				fnEmployeeData$(0,'Exepmtions',resp$(rc_stExemptions))
				fnEmployeeData$(0,'Dependents',resp$(rc_stDependents))
			end if
			marital       	=val(resp$(resp_married      	)(1:1)) ! marital status
			payCode       	=val(resp$(resp_payCode      	)(1:2)) ! pay code
			allowances   	=val(resp$(resp_stExeptions 	)(1:2)) ! state ex
			fedExempt    	=val(resp$(resp_fedExeptions	)(1:2)) ! fed ex
			stateAddOn   	=val(resp$(resp_StateAddOn  	)     ) ! state addon
			w4year$       	=    resp$(resp_w4year       	)       ! W-4 Year
			wages         	=val(resp$(resp_wages        	)     ) ! Current Wages Taxable
			fedWh         	=val(resp$(resp_fedWh        	)     ) ! Federal Withholding
			taxYear       	=val(resp$(resp_taxYear      	)     ) ! taxYear
			w4step2$      	=    resp$(resp_w4step2      	)
			if w4step2$='True' then w4step2=1 else w4step2=0
			showDetails=1 ! a global variable that tells routines to show more info about calcuatlions for users when Checking Payroll Calculations

			pppy=fn_payPeriodsPerYear(payCode)

			if ckey=ck_federalTable then ! r: print the federal tables
				! dim checkedOption$(2)
				! checkedOption$(1)='NOT checked'
				! checkedOption$(2)='Checked'
				fnOpenPrn
				for payCodeItem=1 to udim(mat payPeriodOption$)
					pr #255,using 'form pos 1,cc 67': 'Tax Year '&str$(taxYear)
					pr #255,using 'form pos 1,cc 67': payPeriodOption$(payCodeItem)(5:inf)&' Payroll Period'
					pr #255: ''
					for pass=1 to 3
						if pass=1 then marital=1
						if pass=2 then marital=0
						if pass=3 then marital=2
						dim ft1(8,3)
						dim ft2(8,3)
						fn_getFederalTable(taxYear,marital,w4Year$,0,mat ft1,fed_annual_wh_allowance)
						fn_getFederalTable(taxYear,marital,w4Year$,1,mat ft2,fed_annual_wh_allowance)
						pr #255:'___________________________________________________________________'
						pr #255,using 'form pos 1,cc 33,x 1,cc 33': marriedOption$(marital+1)(5:inf),marriedOption$(marital+1)(5:inf)
						pr #255: '      W-4 Step 2 NOT Checked             W-4 Step 2 Checked        '
						pr #255: '     A            C           D         A            C           D '
						for x=1 to udim(mat fedTable,1)
							pr #255: cnvrt$('pic(zzz,zzz,zz#)',round(ft1(x,1)/pppy,0))&' '&cnvrt$('pic(zzz,zzz,zz#.##)',round(ft1(x,2)/pppy,2))&'   '&cnvrt$('pic(z#)',ft1(x,3)*100)&'%   ';
							pr #255: cnvrt$('pic(zzz,zzz,zz#)',round(ft2(x,1)/pppy,0))&' '&cnvrt$('pic(zzz,zzz,zz#.##)',round(ft2(x,2)/pppy,2))&'   '&cnvrt$('pic(z#)',ft2(x,3)*100)&'%'
						nex x
						pr #255: ''
						pr #255: ''
					next pass
					! pr #255:'___________________________________________________________________'
					if payCodeItem<>udim(mat payPeriodOption$) the pr #255: newpage
				next payCodeItem
				fnClosePrn
				! /r
			else ! if ckey=ck_both then r: test the calcualtion
				fn_detail('              taxYear: '&str$(taxYear            	))
				fn_detail('Wages Taxable Current: '&str$(wages              	))
				fn_detail(' Pay Periods Per Year: '&str$(pppy               	))
				if fedWh then fn_detail('  Federal WithHolding: '&str$(fedWh              	))
				if allowances then fn_detail('           Allowances: '&str$(allowances         	))
				fn_detail('              Married: '&marriedOption$(marital+1	))
				! fnStatus('              fedExempt: '&str$(fedExempt            	))
				fn_detail('               w4year: '&w4year$                  	 )
				if w4step2 then fn_detail('              w4step2: '&str$(w4step2            	))
				fn_detail('__')

				if ckey=ck_check then ! r: Test Check
					if ~fnSelectCheck(eno$,checkRec) then goto CpcTopOfScreen
					! pr 'selectCheckReply=';selectCheckReply
					! pr '            eno$=';eno$
					read #hCheckHistory,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2',rec=checkRec,release: heno,empDept,prDate,ckno,mat tdc,mat tcp
					read #hEmployee,using Femployee,key=eno$: mat em,lpd,totalGrossPay,w4step2,w4Year$,w4Step3,w4step4a,w4step4b,w4step4c
					deptKey$=eno$&cnvrt$('pic(zz#)',empDept)
					read #hDepartment,using 'form pos 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=deptKey$: teno,empDept,gl$,mat tdt,mat tcd,tli,mat tdet
					! open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],KFName=[Q]\PRmstr\timesheet[acsUserId]idx.h[cno]',i,outIn,k
					! open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],RecL=167,Replace',internal,output
					if exists('[Q]\PRmstr\timesheet[acsUserId].h[cno]') then
						fnCopy('[Q]\PRmstr\timesheet[acsUserId].h[cno]','[Q]\PRmstr\timesheet[acsUserId]backup.h[cno]')
					end if
					open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],Version=0,KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno],Replace,RecL=167,KPs=1,KLn=11,Shr',i,outIn,k
					!  r: make timesheet record from read stuff
						empNo$=eno$ ! heno
						dep=empDept
						xInp(1 )=tdc(tdc_hoursRegular)  	! Regular Hours
						xInp(2 )=tdc(tdc_hoursOvertime)  	! Overtime Hours
						xInp(3 )=tdc(tdc_hoursSick)  	! Sick Hours
						xInp(4 )=tdc(tdc_hoursVacation)  	! Vacation Hours
						xInp(5 )=tdc(tdc_hoursHoliday)  	! Holiday Hours
						xInp(6 )=tdet(1) 	! Salary
						xInp(7 )=tcp(tcp_otherCompensation) 	! other compensation
						xInp(8 )=tcp(tcp_meals) 	! meals
						xInp(9 )=tcp(tcp_tips) 	! tips
						xInp(10)=tdet(4 ) ! tcp(5)  	! standard deduction 1
						xInp(11)=tdet(5 ) ! tcp(6)  	! standard deduction 2
						xInp(12)=tdet(6 ) ! tcp(7)  	! standard deduction 3
						xInp(13)=tdet(7 ) ! tcp(8)  	! standard deduction 4
						xInp(14)=tdet(8 ) ! tcp(9)  	! standard deduction 5
						xInp(15)=tdet(9 ) ! tcp(10) 	! standard deduction 6
						xInp(16)=tdet(10) ! tcp(11) 	! standard deduction 7
						xInp(17)=tdet(11) ! tcp(12) 	! standard deduction 8
						xInp(18)=tdet(12) ! tcp(13) 	! standard deduction 9
						xInp(19)=tdet(13) ! tcp(14) 	! standard deduction 10
						xInp(20)=tdet(14) ! tcp(15) 	! standard deduction 11
						xInp(21)=tdet(15) ! tcp(16) 	! standard deduction 12
						xInp(22)=tdet(16) ! tcp(17) 	! standard deduction 13
						xInp(23)=tdet(17) ! tcp(18) 	! standard deduction 14
						xInp(24)=tdet(18) ! tcp(19) 	! standard deduction 15
						xInp(25)=tdet(19) ! tcp(20) 	! standard deduction 16
						xInp(26)=tdet(20) ! tcp(21) 	! standard deduction 17
						xInp(27)=tdet(21) ! tcp(22) 	! standard deduction 18
						xInp(28)=tdet(22) ! tcp(23) 	! standard deduction 19
						xInp(29)=tdet(23) ! tcp(24) 	! standard deduction 20
						gdp     =tcp(tcp_gross)  	! Pay - Net
						hr(1)   =tdet(2)  	! hourly rate - regular
						hr(2)   =tdet(3)  	! hourly rate - overtime
					! /r
					! r: fn_detail the timesheet
					showDetails=1
					fn_detail('***** Generated Timesheet Entry *****')
					fn_detail('Regular Hours        :',xInp(1 ))
					fn_detail('Overtime Hours       :',xInp(2 ))
					fn_detail('Sick Hours           :',xInp(3 ))
					fn_detail('Vacation Hours       :',xInp(4 ))
					fn_detail('Holiday Hours        :',xInp(5 ))
					fn_detail('Salary               :',xInp(6 ))
					fn_detail('Other compensation   :',xInp(7 ))
					fn_detail('Meals                :',xInp(8 ))
					fn_detail('Tips                 :',xInp(9 ))
					fn_detail('Standard Deduction 1 :',xInp(10))
					fn_detail('Standard Deduction 2 :',xInp(11))
					fn_detail('Standard Deduction 3 :',xInp(12))
					fn_detail('Standard Deduction 4 :',xInp(13))
					fn_detail('Standard Deduction 5 :',xInp(14))
					fn_detail('Standard Deduction 6 :',xInp(15))
					fn_detail('Standard Deduction 7 :',xInp(16))
					fn_detail('Standard Deduction 8 :',xInp(17))
					fn_detail('Standard Deduction 9 :',xInp(18))
					fn_detail('Standard Deduction 10:',xInp(19))
					fn_detail('Standard Deduction 11:',xInp(20))
					fn_detail('Standard Deduction 12:',xInp(21))
					fn_detail('Standard Deduction 13:',xInp(22))
					fn_detail('Standard Deduction 14:',xInp(23))
					fn_detail('Standard Deduction 15:',xInp(24))
					fn_detail('Standard Deduction 16:',xInp(25))
					fn_detail('Standard Deduction 17:',xInp(26))
					fn_detail('Standard Deduction 18:',xInp(27))
					fn_detail('Standard Deduction 19:',xInp(28))
					fn_detail('Standard Deduction 20:',xInp(29))
					fn_detail('Pay - Net            :',gdp     )
					fn_detail('Hourly Rate Regular  :',hr(1)   )
					fn_detail('Hourly Rate Overtime :',hr(2)   )
					fncloseprn
					! /r
					write #hTimesheet,using Ftimesheet: empNo$,dep,mat xInp,gpd,mat hr
					close #hTimesheet:
					fn_setupCloseFiles
					setenv('PR-justTesting','Yes')
					fnChain('S:\Payroll\Calculation')
					! /r
				else
					if ckey=ck_state or ckey=ck_both then
						testStateTax=fn_stateTax(0,wages,pppy,allowances,marital,fedWh,stateAddOn,w4year$,taxYear)
					end if
					if ckey=ck_federal or ckey=ck_both then
						testFederalTax=fn_federalTax(taxYear,fedpct,wages,ded,stdWhFed,fedExempt,pppy,marital,w4Year$,w4step2,w4Step3,w4step4a,w4step4b,w4step4c)
					end if
					if ckey=ck_state or ckey=ck_both then
						fn_detail('Calculated '&fnpayroll_client_state$&' State WithHolding: '&str$( testStateTax ))
					end if
					if ckey=ck_federal or ckey=ck_both then
						fn_detail('Calculated Federal WithHolding: '&str$( testFederalTax ))
					end if
				end if
				! fn_detail('_____________________________')
				! fn_detail('_____________________________')
				! fn_detail('Federal: '&str$(testFederalTax))
				! fn_detail(fnpayroll_client_state$&': '&str$(testStateTax))
				! fn_detail('_____________________________')
				! fn_detail('_____________________________')
				! if detailReport then
					fnClosePrn
				! else
				! 	fnStatusPause
				! 	fnStatusClose
				! end if
				showDetails=0
				! /r
			end if
			! /r
		end if
	loop

	XitCheckStateCalculation: !
	fn_setupCloseFiles
	returnN=0
	fnCheckPayrollCalculation=returnN
fnend
def fn_justTesting(; ___,returnN)
	if lwrc$(env$('PR-justTesting'))='yes' then returnN=1
	fn_justTesting=returnN
fnend

def fn_tableLine(mat tl_table,tl_seekAmount; tl_secondDimension,___,returnN)
	! this function finds where [tl_seekAmount] falls within a range in a singe row (1st element) of a 2 dimensional array)
	! this function identifies which column (2nd element) of the 2d array to search with [tl_secondDimension] which defaults to the first
	if tl_secondDimension=0 then tl_secondDimension=1
	for returnN=1 to udim(mat tl_table,1)-1
		if tl_seekAmount>tl_table(returnN,tl_secondDimension) and tl_seekAmount<=tl_table(returnN+1,tl_secondDimension) then
			goto Tl_finis
		end if
	next returnN
	returnN=udim(mat tl_table,1)
	Tl_finis: !
	fn_tableLine=returnN
fnend
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
		ml$(1)='Incorrect Pay Code '&str$(payCode)&' on Employee Number '&trim$(empNo$)&'. Did not calculate pay on this Employee'
		fnMsgBox(mat ml$,resp$,'',0)
		returnN=-1
	end if
	fn_payPeriodsPerYear=returnN
fnend
def fn_detai4(c1$*128,n1)
		if file(255)=-1 then fnOpenPrn
		pr #255: lpad$(c1$,45)&':  '&cnvrt$('n 10.4',n1)
fnend
def fn_detail(c1$*128; n1,c2$*128,n2)
	if showDetails then ! and (n1 or n2) then
		if file(255)=-1 then fnOpenPrn
		if n2 then
			pr #255: lpad$(c1$,45)&':  '&cnvrt$('n 10.2',n1)&'   '&c2$&':  '&cnvrt$('n 10.2',n2)
		else if c2$<>'' then
			pr #255: lpad$(c1$,45)&':  '&cnvrt$('n 10.2',n1)&'   '&c2$
		else if n1 then
			pr #255: lpad$(c1$,45)&':  '&cnvrt$('n 10.2',n1)
		else
			pr #255: c1$
		end if
	end if

fnend


def fn_detailStep(text$*64; number,extra$*64,extraNumber)
	fn_detailStep=fn_detail('Step '&text$,number,extra$,extraNumber)
fnend

def fn_setup(; ___,mtc)
	autoLibrary
	on error goto Ertn

	showDetails=0
	if env$('acsDeveloper')<>''  or  env$('showDetails')<>'' then showDetails=1
	if fn_justTesting then showDetails=1

	detailReport=1

	dim stwh(10,2)
	dim resp$(64)*256
	dim ml$(0)*256

	dim motab(12)
	mtc=0 ! motab counter
	motab(mtc+=1)=  0 : motab(mtc+=1)= 31 : motab(mtc+=1)= 59
	motab(mtc+=1)= 90 : motab(mtc+=1)=120 : motab(mtc+=1)=151
	motab(mtc+=1)=181 : motab(mtc+=1)=212 : motab(mtc+=1)=243
	motab(mtc+=1)=273 : motab(mtc+=1)=304 : motab(mtc+=1)=334

	open #20: 'Name=[Q]\PRmstr\Company.h[cno],Shr',i,i
	dim wcm(4) ! mat wcm is workman's comp maximum
	dim sck(4)
	dim sucw(10)
	! dim sucr(10)
	! dim dedcode(10),calcode(10),dedfed(10)
	! read #20,using 'form pos 145,PD 5.2,pos 230,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,pos 618,30*N 1,pos 708,3*PD 4.3,3*PD 3.2,4*PD 4.2,pos 133,PD 6.3,PD 6.2': fucr,loccode,feducmax,ficarate,ficamax,ficamxr,mat sucw,mat sucr,mat dedcode,mat calcode,mat dedfed,mat sck,vacm,MinHourlyWage,mat wcm,ficar2,ficamx2
	! REMOVED:UNUSED pos 145       PD 5.2  fucr
	! REMOVED:UNUSED pos 230        N  2   loccode
	!                              PD 4.2  feducmax
	!                              PD 3.3  ficarate
	!                          12*	PD 4.2 ficamax,ficamxr,mat sucw(1-10)
	! REMOVED:UNUSED           10*	PD 3.3 mat sucr(1-10)
	! REMOVED:UNUSED pos 618   30*	N  1   mat dedcode(1-10),mat calcode(1-10),mat dedfed(1-10)
	!                pos 708    3* PD 4.3  mat sck(1-3)
	!                           3* PD 3.2  sck(4),vacm,MinHourlyWage
	!                           4* PD 4.2  mat wcm(1-4)
	!                pos 133       PD 6.3  ficar2
	!                              PD 6.2  ficamx2
	read #20,using 'form pos 230,x 2,PD 4.2,PD 3.3,12*PD 4.2,pos 708,3*PD 4.3,3*PD 3.2,4*PD 4.2,pos 133,PD 6.3,PD 6.2': feducmax,ficarate,ficamax,ficamxr,mat sucw,mat sck,vacm,MinHourlyWage,mat wcm,ficar2,ficamx2
	close #20:
	mat wcm(5)
	wcm(5)=wcm(1)*12 ! make an annual wcm and set it equal to monthly*12
	ficamax=ficamax*10
	dim dedName$(20)*20
	dim abrevname$(20)*8
	dim deductionCode(20)
	dim newcalcode(20)
	dim newdedfed(20)
	dim dedfica(20)
	dim dedst(20)
	dim deduc(20)
	fnDedNames(mat dedName$,mat abrevname$,mat deductionCode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	ssmax=ficamax : mcmax=ficamx2 : ficar1=ficarate*.01
	ficar2=ficar2*.01 : ficarate=ficar1+ficar2
	ficamxr=ficamax*ficarate : ficamx2=(ficamx2-ficamax)*ficar2
	! if env$('client')='West Accounting' then
	!   saif(1)=173.33
	!   saif(2)=86.66
	!   saif(3)=80
	!   saif(4)=40
	! end if

	dc_deduct  	=1
	dc_add      	=2
	dc_benefit 	=3
	dc_addGross	=4

	esw_federal	=1 ! enums for disableWithholdingN
	esw_state  	=2 ! enums for disableWithholdingN
	esw_fica   	=3 ! enums for disableWithholdingN
	esw_standard	=4 ! enums for disableWithholdingN


	dim tcp(32)
	tcp_fedWh=1
	tcp_ssWh=2
	tcp_medicareWh=3
	tcp_stateWh=4
	! ...
	tcp_standarddeduction10=14
	! tcp_earnedIncomeCredit=25
	tcp_payRegular=26
	tcp_payOvertime=27
	tcp_otherCompensation=28
	tcp_meals=29
	tcp_tips=30
	tcp_gross=31
	tcp_net=32

	dim tdc(10)
	tdc_hoursRegular  	=1
	tdc_hoursOvertime 	=2
	tdc_hoursSick      	=3
	tdc_hoursVacation  	=4
	tdc_hoursHoliday   	=5
	tdc_wagsWorkmansCompensation=6
	tdc_wagesSs=7
	tdc_wagesMedicare=8
	tdc_wagesUcFed=9
	tdc_wagesUcState=10


fnend
def fn_setupOpenFiles
	open #hBreakdown=fnH: 'Name=[Q]\PRmstr\HourBreakdown.h[cno],KFName=[Q]\PRmstr\HourBreakdown-idx.h[cno],Shr',i,outIn,k ioerr ignore
	open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,outIn,k
	open #hDepartment=fnH: 'Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr',i,outIn,k
	open #hCheckHistory=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr,Use,RecL=224,KPs=1,KLn=17',i,outIn,k
	open #hPayrollCheckIdx2_unused=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx2.h[cno],Shr',i,outIn,k
	open #hPayrollCheckIdx3_unused=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr',i,outIn,k
fnend
def fn_setupCloseFiles
	close #hBreakdown:
	close #hEmployee:
	close #hDepartment:
	close #hCheckHistory:
	close #hPayrollCheckIdx2_unused:
	close #hPayrollCheckIdx3_unused:
	! close #hTimesheet:
fnend

Xit: setenv('PR-justTesting','') : fnXit
include: fn_open
include: ertn
