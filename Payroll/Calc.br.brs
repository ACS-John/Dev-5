! r: Test The Tables!
	  if env$('ACSdeveloper')<>'' then 
	    fn_setup 
	    fn_setupOpenFiles
	    fn_test_state_calk
	    end
	  end if
! /r
! S:\Payroll\Calc (formerly) S:\acsPR\newprCalk
! Payroll Calculation
! if you accidentally renumber it, be sure and list 'every year' and make sure the line numbers still match up
! sswh - social security withholding
! mcwh - medicare withholding
! MinHourlyWage -  minumum hourly wage
! g_pay_periods_per_year used to be t6
! twy - total wages yearToDate
! gdp - gross pay for department
	fn_setup
	fntop(program$,cap$="Payroll Calculation")
	gosub ASKDATES
	if ckey=5 then goto XIT
	dat$=lpad$(str$(d1),6)
	mo1=val(dat$(5:6)) : da=val(dat$(7:8)) : yr=val(dat$(3:4))
	ppd=round(yr*365+int(yr/4)+motab(mo1)+da,2)
	d1=mo1*10000+da*100+yr
	gosub ASKSKIPWH
	if ckey=5 then goto XIT
	fnAutomatedSavePoint('before')
	fn_setupOpenFiles
	ReadRpWork: ! 
	read #h_rpwork,using F_RPWORK: x$,dep,mat inp,gpd,mat hr eof EO_RPWORK
	if env$('client')='Payroll Done Right' then gosub WEST_ACC_WORKMANSCOMP ! env$('client')='West Accounting' or 
	! pr 'FIRST READ OF RPWORK right after read rpwork inp(6)=';inp(6) : pause
	newdeptkey$=cnvrt$("pic(zzzzzzz#)",val(x$))&cnvrt$("pic(zz#)",dep)
	! totaldef=0
	! Form POS 1,C 8,N 3,5*PD 4.2,15*PD 5.2,2*PD 4.2,PD 3
	eno=val(x$)
	if eno=0 then goto ReadRpWork
	if n$=x$ then goto L1540
	twc=twy=tfy=cafy=eicytd=deducy=0
	if rtrm$(n$)<>"" then gosub SUBROUTINE2
goto L1010
! ______________________________________________________________________
SUBROUTINE2: ! r: (reallocate state taxes based on earnings by dept and state
	s3=0 : tcp(4)=0 : tcp4=0
	oldeno=val(n$)
	restore #h_department,key>=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",0): 
	if em(14)=-1 then goto L960
	! Read #h_department,Using 610,Rec=TRA: tdt(4),TCD(1),ty4,tqm4,tcp4,tcp31,TCP22,NTA,MAT DST
	L670: ! 
	read #h_department,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L960
	if debug then let fnStatus('department read employee '&str$(eno)&' department '&str$(tdn))
	if teno<>oldeno then goto L960
	if d1><tdt(4) then goto L670
	holdtdn=tdn
	olddeptkey$=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",holdtdn)
	read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",key=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",tdn)&cnvrt$("pd 6",prd): heno,tdn,prdate,ckno,mat tdc,mat tcp nokey L670
	if debug then let fnStatus('read check history: heno='&str$(heno)&',tdn='&str$(tdn)&',prdate='&str$(prdate)&',ckno='&str$(ckno)&'...')
	dst3=0
	for j=1 to 20
		if dedst(j)>0 then dst3=dst3+tcp(j+4)
	next j
	! sTWH(tcd(1),1)=STWH(tcd(1),1)-DST3
	if stwh(tcd(1),1)=0 then goto L670
	if stwh(tcd(1),2)><0 then goto L870
	if em(14)=0 then goto L840
	if in2$(4)<>"Y" then stwh(tcd(1),2)=em(14)
	goto L870
	L840: ! 
	if in2$(2)="Y" then s3=0: goto L860
	on tcd(1) gosub ST01,ST02,ST03,ST04,ST05,ST06,ST07,ST08,ST09,ST10
	L860: ! 
	stwh(tcd(1),2)=s3
	L870: ! 
	if env$('client')="Lamar" then 
		tcp4=(stwh(tcd(1),2))*((tcp(31)-dst3)/stwh(tcd(1),1))
	else 
		tcp4=(stwh(tcd(1),2)+em(15))*((tcp(31)-dst3)/stwh(tcd(1),1))
	end if 
	tcp4=round(tcp4,2)
	if in2$(2)="Y" then tcp4=0
	tcp(32)-=tcp4: tcp(4)=tcp4
	rewritekey$=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",holdtdn)&cnvrt$("pd 6",prd) ! index employee#,department# and payroll date
	rewrite #h_payrollchecks,using 'Form pos 80,pd 5.2,poS 220,pd 5.2',key=rewritekey$: tcp(4),tcp(32)
	fn_report_stuff
	rewrite #h_department,using 'Form pos 42,n 6',key=olddeptkey$: tdt(4)
	goto L670
	L960: ! 
	rewrite #hEmployee,using F_RPMSTR,key=n$: mat em,d1,tgp
	if fp(d1*.01)>.9 then hd1=19000000+fncd(d1) else hd1=20000000+fncd(d1)
	mat stwh=(0)
return ! /r
L1010: ! r: read employee, call calc deduction etc  basically beginning of main loop i think
	read #hEmployee,using F_RPMSTR,key=x$: mat em,lpd,tgp nokey EMPLOYEE_NOT_FOUND
	F_RPMSTR: form pos 112,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2
	gosub CALK_ALL_DEDUCTIONS_ALL_DEPT
	n$=x$
	if d3$><"Y" then goto L1170 ! Accrue Sick and Vacation
	if em(8)><-1 then goto L1110 ! Check for elgibility
	if em(16)<10100 or em(16)>123199 then goto L1110
	dat$=lpad$(str$(em(16)),6) : mo=val(dat$(1:2))
	da=val(dat$(3:4)) : yr=val(dat$(5:6))
	dh=round(yr*365+int(yr/4)+motab(mo)+da,2)
	if ppd-dh<sck(1) then goto L1110
	em(8)=sck(3) : em(10)=sck(2)
	L1110: ! 
	if em(8)>0 then em(10)+=em(8) ! Accrue Sick
	! if env$('client')='Battlefield' then goto L1140
	if em(8)>0 then write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Sick",prd,em(8),0 ioerr ignore
	! L1140: ! 
	if em(9)>0 then em(11)+=em(9) ! Accrue Vacation
	! if env$('client')='Battlefield' then goto L1170
	if em(9)>0 then write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Vac",prd,em(9),0 ioerr ignore
	L1170: ! 
	twy=0
	tf4_a=0 ! Calculate Total Federal WithHoldings
	fed_wh_annual_estimate=0
! IF in2$(1)="Y" THEN GOTO 1420
	! if em(1)=2 then j2=4 else j2=round(1+em(1)*3,2) ! 2=HEAD OF HOUSEHOLD
	if em(1)=0 then ! 0 - Single
		j2=1
	else if em(1)=1 then ! 1 - Married
		j2=4
	else if em(1)=2 then ! 2 - Single - Head of Household
		j2=4
	else if em(1)=3 then ! 3 - Married - filing joint return - only one working
		j2=4
	else if em(1)=4 then ! 4 - Married - filing joint - both working
		j2=4
	else if em(1)=5 then ! 5 - Married - filing seperate - both working
		j2=4
	end if
	on em(5) goto PAYCODE_1,PAYCODE_2,PAYCODE_3,PAYCODE_4 none BAD_PAY_CODE
! /r
BAD_PAY_CODE: ! r:
	mat ml$(1)
	ml$(1)="Incorrect Pay Code "&str$(em(5))&" on Employee Number "&trim$(x$)&". Did not calculate pay on this Employee"
	fnmsgbox(mat ml$,resp$,cap$,0)
goto ReadRpWork ! /r
! ______________________________________________________________________
PAYCODE_1: t6=12 : g_pay_periods_per_year=12 : goto PAST_PAYCODE
PAYCODE_2: t6=24 : g_pay_periods_per_year=24 : goto PAST_PAYCODE
PAYCODE_3: t6=26 : g_pay_periods_per_year=26 : goto PAST_PAYCODE
PAYCODE_4: t6=52 : g_pay_periods_per_year=52
PAST_PAYCODE: ! r: continues here
! pr '@ PAST_PAYCODE' : pause
	if in2$(1)="Y" then goto L1470
	if fedpct>0 then 
		tf4_a=round((tgp-ded)*fedpct,2) : fed_wh_annual_estimate=tf4_a
		goto L1470
	end if 
	if em(12)=0 then goto L1370
	tf4_a=0
	if em(12)=-1 then goto L1470
	tf4_a=em(12) : goto L1470
	L1370: ! 
	t2=round(em(2)*(fed_annual_wh_allowance/g_pay_periods_per_year),2) ! this is one of the lines that change every year (line 1240)
	g2=tgp-t2-ded
	if g2>0 then 
		g2=round(g2*g_pay_periods_per_year,2) ! g2 - becomes estimated annual net pay
		j1=fn_table_line(mat ft,g2, j2)
		fed_wh_annual_estimate=tf4_a=round(ft(j1,j2+1)+(g2-ft(j1,j2))*ft(j1,j2+2),2)
		! if env$('acsDeveloper')<>'' then pause ! table total federal w/h used in some state routines
		tf4_a=round(tf4_a/g_pay_periods_per_year,2)
	else 
		g2=0
	end if 
	if in2$(1)><"Y" then tf4_a=tf4_a+em(13)
	L1470: ! 
	mat stuc=(0)
	read #h_department,using "form pos 48,n 2",key=newdeptkey$: tcd(1) ! get state code
	gosub DETERMINE_EARNINGS
	for j=1 to 20
		if newdedfed(j)=2 and newdedcode(j)=newdedcode_Deduct then 
			cafy+=caf(j)
			cafd+=caf(j)
		end if 
	next j
	twy+=twd : tfy+=(ytdFICA+tmd) : ficatfy=tfy
	oldsswg=twy-cafy : eicytd+=td14 : stuc(tcd(1))+=twd-cafd
	cafd=0
	L1540: ! 
	read #h_department,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=newdeptkey$: teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet ! Nokey X
	if tgp=0 then pog=1: goto L1620 ! Allow checks to calculate with no gross pay
	if tgp=gpd then pog=1 : goto L1620
	if tgp<>0 then goto L1610
	mat ml$(1)
	ml$(1)="Employee Number "&trim$(x$)&" skipped Total Gross Pay = 0, Must be Re-entered"
	fnmsgbox(mat ml$,resp$,cap$,0)
	goto ReadRpWork
	L1610: !
	pog=gpd/tgp
	L1620: ! 
	for j=1 to 20
		if env$('client')="Franklinton" then 
			if j=1 and em(4)=3 then ! retirement of firemen  ! franklinton
				inp(j+7)=round(inp(j+7)*gpd/100,2)
				goto L1710 ! franklinton
			else 
				if j=2 then ! retirement of police !franklinton
					inp(j+7)=round(inp(j+7)*((hr(1)*(inp(1)+inp(3)+inp(4))+inp(6)+inp(17))/100),2)
					goto L1710 ! franklinton
				end if 
			end if 
			!   else if env$('client')="Washington Parrish" and j=3 and newcalcode(j)=2 then
			!     inp(j+7)=round(inp(j+9)*(gpd+defcompmatch)/100,2)
			!     goto L1700
			!   else if env$('client')="West Accounting" and j=10 and inp(17)<>0 then
			!     gosub WEST_ACC_WORKMANSCOMP
		end if 
		if newcalcode(j)=2 then inp(j+9)=round(inp(j+9)*gpd/100,2)
	! L1700: ! 
		if in2$(4)="Y" then inp(j+9)=0
	L1710: ! 
	next j
	em(10)-=inp(3) : em(11)-=inp(4)
	! if env$('client')='Battlefield' then goto L1760
	if inp(3)>0 then ! write sick hours taken to breakdown file
		write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Sick",prd,0,inp(3) ioerr ignore
	end if 
	if inp(4)>0 then ! write vacation hours taken to breakdown file
		write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Vac",prd,0,inp(4) ioerr ignore
	end if 
	! L1760: ! 
	if inp(5)>0 then ! write holiday hours taken to breakdown file
		write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Hol",prd,0,inp(5) ioerr ignore
	end if 
	if sck(4)=999 then sck(4)=1000 ! system will only hold 999 maximum accrued sick hours.  If maximum is set at 999, assume no maximum
	if sck(4)<>0 and em(10)>sck(4) then em(10)=sck(4)
	if vacm<>0 and em(11)>vacm then em(11)=vacm
	ext=0 ! Excess Tips
	goto NO_EXCESS_TIPS
! ______________________________________________________________________
	if inp(9)=0 then 
		goto NO_EXCESS_TIPS
	else 
		tr=round(inp(1)*MinHourlyWage+inp(2)*MinHourlyWage*1.5,2)
		g1=gpd-inp(9)
		ext=0
		if g1>=tr then 
			g2=inp(9)
		else 
			g2=gpd-tr
		end if 
	end if 
NO_EXCESS_TIPS: ! 
	deduc=ficat3=f3=0 ! FICA
	for j=1 to 20
		if dedfica(j)=1 and newdedcode(j)=newdedcode_Deduct then ficat3+=inp(j+9)
		if deduc(j)=1 then deduc+=inp(j+9): deducy+=caf(j) ! total deductions for unemployment for current period and year to date
	next j
	sswg=sswh=mcwh=0
	if tgp=0 then f3=0: goto CALC_NO_GROSS ! calculate checks w/ no gross pay
	if in2$(3)="Y" then goto FEDWH_DEPT
	on em(6)+1 goto L1930,SS_TAX_ONLY,L3240 none FEDWH_DEPT
! ______________________________________________________________________
L1930: ! 
! if env$('client')="Washington Parrish" then
!   tf0=tgp-t3+totaldef ! add deferred in taxable wages for washington parrish
!   goto L1950
! end if
	tf0=tgp-t3 ! if over both max else if over both max this time else if over max-1
! L1950: !
	if ficatfy>=ficamxr+ficamx2 then 
		goto FICAEND
	else if (ficatfy-ficamxr)+(tf0*ficar2)>=ficamx2 then 
		mcwh=ficamxr+ficamx2-ficatfy
		goto FICAEND
	else if ficatfy>=ficamxr then 
		mcwh=tf0*ficar2 : sswg=0
		goto FICAEND
	end if 
! if went over first max this time else Under 1st Max 
	if ficatfy+(tf0*ficarate)>=ficamxr then 
		tf1=ficamax-ficatfy/ficarate : tf2=tgp-t3 
		sswh=(tf1*ficar1) 
		mcwh=(tf2*ficar2) 
		sswg=tf1 
	else 
		sswh=tf0*ficar1 
		mcwh=tf0*ficar2
		sswg=tf0
	end if
FICAEND: ! 
	if sswg>ficamax-oldsswg-.10 and sswg<ficamax-oldsswg+.10 then 
		sswg=ficamax-oldsswg
	end if
	if tgp-t3>0 then 
		ficapog=((gpd-ficat3)/(tgp-t3)) 
	else 
		ficapog=1
	end if
	sswh=round(sswh*ficapog,2) 
	mcwh=round(mcwh*ficapog,2) 
	f3=sswh+mcwh : oldsswg+=sswg
CALC_NO_GROSS: tfy+=f3
FEDWH_DEPT: ! Fed WH for Dept ! Federal Withholding for Department
	if debug then let fnStatus('federal  withholding for department calculating')
	f4=round(tf4_a*pog,2)
	stwh(tcd(1),1)+=gpd : eic4=0 ! Calculate EIC
	if em(7)=0 then goto CURRENT_PERIOD else g2=tgp
	eic1=round(8970/em(7)/g_pay_periods_per_year,2)                ! this is one of the lines that change every year (formerly line 1800)
	eic2=round(16450/em(7)/g_pay_periods_per_year,2)               ! this is one of the lines that change every year (formerly line 1810)
	eic3=round(1830/em(7)/g_pay_periods_per_year,2)                ! this is one of the lines that change every year (formerly line 1820)
	if g2<=eic1 then eic4=round(tgp*.2040,2)
	if g2>eic1 and g2<=eic2 then eic4=eic3
	if g2>eic2 then eic4=eic3-(tgp-eic2)*.09588
	if ytdTotal(25)+eic4<0 then eic4=-ytdTotal(25)
	eic4=round(eic4*pog,2)
CURRENT_PERIOD: ! 
	tcp(1)=f4 : tcp(2)=sswh : tcp(3)=mcwh : tcp(4)=tcp4
	for j=5 to 24
		tcp(j)=inp(j+5)
	next j
	tcp(25)=min(eic4,tcp(1))
	tcp(27)=round(inp(2)*hr(2),2)
	tcp(28)=inp(7)
	tcp(29)=inp(8)
	tcp(30)=inp(9)
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
	if em(6)=9 then 
		tdc(7)=0
	else 
		tdc(7)=round(sswg*ficapog,2)
	end if 
! MEDICARE_WAGE: !
	tdc(8)=round((tgp-t3)*ficapog,2)
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
	if feducmax=0 then goto FEDERAL_UC_WAGE
	if twy-deducy>=feducmax then goto L2370
	if twy-deducy+(gpd-ext-deduc)>feducmax then goto FEDERAL_UC_WAGE
FEDERAL_UC_WAGE: ! 
	tdc(9)=gpd-ext-deduc
	goto L2370
L2370: ! 
	tdc(9)=min(max(feducmax-(twy-deducy),0),gpd-ext-deduc)
	for j=1 to 5 : tdc(j)=inp(j) : next j ! Hours
! pause ! WORKMANS_COMP: !
! em(5) is paycode
! mat wcm is workman's comp maximum
! trp is (temp variable only used here)
! tcp(26) is Regular Earnings
! tcp(27) is OT Earnings
! wc is (temp variable only used here)
! tdc(6) is Workman's Comp Wages
! if env$('client')="West Accounting" then ! perhaps everyone should be doing it this way -prd 01/06/2016
!   tcp(14)=tdc(1)*inp(19)*.01 ! base on regular hours times w/c rate
!   fnStatus('tcp(14) was set to '&str$(tcp(14))&' by tcp(14) = tdc(1)('&str$(tdc(1))&' * inp(19)('&str$(inp(19))&') * .01')
!   inp(19)=0   ! <-- nice idea but it does not make a difference
!   fnStatusPause
! end if  ! else 
	trp=tcp(26)+tcp(27) ! base on wages
! end if
	wc=0
	if wcm(em(5))=0 or twc+trp<wcm(em(5)) then 
		wc=trp
	else 
		wc=wcm(em(5))-twc
	end if 
	twc=twc+wc : tdc(6)=wc
	rewrite #h_department,using "form pos 42,n 6,pos 58,23*pd 4.2",key=newdeptkey$: d1,mat tdet
	tcp(4)=0
	write #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,tdn,prd,0,mat tdc,mat tcp
! fnStatus('WRITING payroll check with tcp(4)='&str$(tcp(4))&' and tcp(32)='&str$(tcp(32)))
! fnStatusPause
	twy+=gpd : cafy+=ficat3 : eicytd+=ytdTotal(25)
	if tdet(16)<>0 then stuc(tcd(1))+=tdet(16) ! ??? kj
	goto ReadRpWork
! /r
EMPLOYEE_NOT_FOUND: ! r:
	n$=" "
	mat ml$(1)
	ml$(1)="Employee Number "&x$&" is not on file. No check calculated."
	fnmsgbox(mat ml$,resp$,cap$,0)
	goto ReadRpWork
! /r
EO_RPWORK: ! r:
	if rtrm$(n$)<>"" then gosub SUBROUTINE2
	close #hEmployee: 
	close #h_department: 
	close #h_rpwork: ! ,Free:
	fnFree("[Q]\PRmstr\jcprh1.h[cno]") ! get rid of jobcost time entry file if exists
	goto XIT ! /r
XIT: fnxit
IGNORE: continue 
CALK_ALL_DEDUCTIONS_ALL_DEPT: ! r:
	! Calculate all deduct for federal for all departments
	tgp=t3=ded=0
	L3020: ! 
	for j=1 to 20
		if (j+9)=17 and (env$('client')='Payroll Done Right') then goto L3090 ! if processing inp(17) SKIP IT do not process it.   ! env$('client')='West Accounting' or 
		if newdedfed(j)>=1 and newdedcode(j)=newdedcode_Deduct then 
			gosub SUBROUTINE6
		else 
			goto L3060
		end if 
		if newcalcode(j)=1 then 
			ded=ded+inp(j+9)
		else 
			ded=ded+inp(j+9)*gpd/100
		end if 
		L3060: ! 
		if newdedfed(j)><2 then goto L3090
		if newcalcode(j)=1 then 
			t3=t3+inp(j+9)
		else 
			t3=t3+inp(j+9)*gpd/100
		end if 
		L3090: ! 
	next j
	tgp=tgp+gpd
	read #h_rpwork,using F_RPWORK: newx$,newdep,mat inp,gpd,mat hr eof L3150
	if env$('client')='Payroll Done Right' then gosub WEST_ACC_WORKMANSCOMP ! env$('client')='West Accounting' or 
	! pr 'A right after read rpwork inp(6)=';inp(6) : pause
	if newx$=x$ then goto L3020
	L3150: ! 
	workkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",dep)
	restore #h_rpwork,key>=workkey$: 
	read #h_rpwork,using F_RPWORK: x$,dep,mat inp,gpd,mat hr eof EO_RPWORK
	if env$('client')='Payroll Done Right' then gosub WEST_ACC_WORKMANSCOMP !  11/14/2017 - env$('client')='Payroll Done Right'  Does not want any special processing for deduction 8        ! env$('client')='West Accounting' or 
	! pr 'B right after read rpwork  inp(6)=';inp(6) : pause
return  ! /r
SS_TAX_ONLY: ! r: SOC-SEC-TAX ONLY
	tf0=tgp-t3
	if ficatfy>=ssmax then 
		sswg=0
		goto FICAEND ! OVER MAX
	end if 
	if ficatfy+tf0>=ssmax then ! WENT OVER MAX THIS TIME
		sswh=(ssmax-ficatfy)*ficar1
		sswg=ssmax-ficatfy
		goto FICAEND
	end if 
	sswh=tf0*ficar1
	sswg=tf0
goto FICAEND ! UNDER MAX /r
L3240: ! r: MEDICARE-TAX ONLY??
	! if env$('client')="Washington Parrish" then ! MEDICARE-TAX ONLY  (add deferred comp match to medicare wages)
	!   tf0=tgp-t3+totaldef
	!   goto L3260
	! end if
	tf0=tgp-t3 ! MEDICARE-TAX ONLY
	! L3260: ! 
	if ficatfy>=mcmax then goto FICAEND ! OVER MAX
	if ficatfy+tf0>=mcmax then ! Went over max this time
		mcwh=(mcmax-ficatfy)*ficar2
		goto FICAEND
	end if 
	mcwh=tf0*ficar2
goto FICAEND ! UNDER MAX  /r
SUBROUTINE6: ! r:
	sc1=1
	read #h_department,using 'form pos 48,n 2',key=newdeptkey$: sc1 nokey ignore
	if sc1=0 then sc1=1
	! If env$('client')="Washington Parrish" AND J=3 Then sD3=INP(J+9)*(GPD+DEFCOMPMATCH)/100 : Goto 3150 ! add deferred comp to gross for calculating pension deduction
	if newcalcode(j)=1 then sd3=inp(j+9) else sd3=inp(j+9)*gpd/100
	stwh(sc1,1)=stwh(sc1,1)-sd3
return  ! /r
! ______________________________________________________________________
ASKSKIPWH: ! r:
	fnTos(sn$="Skipdeductions")
	rc=cf=0: mylen=42: mypos=45
	fnChk(1,46,"Skip Federal Withholdings:",1)
	resp$(rc+=1)="False"
	fnChk(2,46,"Skip State Withholdings:",1)
	resp$(rc+=1)="False"
	fnChk(3,46,"Skip Fica Withholdings:",1)
	resp$(rc+=1)="False"
	fnChk(4,46,"Skip Standard Withholdings:",1)
	resp$(rc+=1)="False"
	fnLbl(6,1,"Standard Federal % Override:",mylen,1,0)
	fnTxt(6,mypos,4,0,1,"32",0,"Normally zero. The government allows you to use a standard percent on bonuses, etc. See Circular E for allowable %.")
	resp$(rc+=1)=""
	fnCmdKey("Next",1,1,0,"Proceed with calculations.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
	fnAcs(sn$,0,mat resp$,ckey) ! skip deductions & std %
	if ckey<>5 then 
		if resp$(1)(1:1)="T" then in2$(1)="Y" else in2$(1)="N"
		if resp$(2)(1:1)="T" then in2$(2)="Y" else in2$(2)="N"
		if resp$(3)(1:1)="T" then in2$(3)="Y" else in2$(3)="N"
		if resp$(4)(1:1)="T" then in2$(4)="Y" else in2$(4)="N"
		fedpct=val(resp$(5)) ! federal wh percent
	end if
return  ! /r
ERTN: fnerror(program$,err,line,act$,"NO") ! r:
	if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN ! /r
ASKDATES: ! r:
	open #h_dates:=11: "Name=[Q]\PRmstr\Dates.h[cno],USE,RecL=76,shr",internal,outIn,relative 
	read #h_dates,using "form pos 1,2*n 8,x 32,n 8,c 20",rec=1,release: beg_date,end_date,d1,d1$ noRec ASKDATES_WRITE_DATE
	goto ASKDATES_SCREEN
	ASKDATES_WRITE_DATE: ! 
	write #h_dates,using "form pos 1,2*n 8,x 32,n 8,c 20",rec=1: beg_date,end_date,d1,d1$
	ASKDATES_SCREEN: ! 
	fnTos(sn$="Calculation-1")
	rc=cf=0: mylen=42: mypos=45: frameno=1
	gosub GET_ALPHA_DATE ! get alpha date
	fnFra(1,1,4,66,"Payroll Date","Enter the payroll date.")
	fnLbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Enter the date which you want used for your earnings records. ",frameno)
	resp$(rc+=1)=str$(d1)
	fnLbl(2,1,"Report Heading Date:",mylen,1,0,frameno)
	fnTxt(2,mypos,20,0,0," ",0,"Enter the date in alpha format for use in report headings, etc." ,frameno)
	resp$(rc+=1)= d1$
	fnChk(3,46,"Accrue Vacation and Sick Leave this period:",1,frameno)
	resp$(rc+=1)="False"
	fnFra(7,25,2,42,"Date Range","In order to Identify earnings and deductions, these answers must be correct.")
	frameno=2 : mylen=31 : mypos=mylen+2
	fnLbl(1,1,"Year Starting Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Year Ending Date:",mylen,1,0,frameno)
	fnTxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno)
	resp$(rc+=1)=str$(end_date)
	! fnchk(13,65,'Enable 2018 Federal Withholdings (FOR TESTING ONLY)', 1,0) 
	! rc_taxYear=rc+=1 
	! if env$('taxYear')='2018' then resp$(rc_taxYear)='True' else resp$(rc_taxYear)='False'
	fnCmdKey("Next",1,1,0,"Proceed with calculations.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey<>5 then 
		prd=d1=val(resp$(1))
		d1$=resp$(2)
		if resp$(3)(1:1)="T" then d3$="Y" else d3$="N"
		beg_date=val(resp$(4))
		end_date=val(resp$(5))
		! if resp$(rc_taxYear)='True' then 
				taxYear=2018
		! else 
		!   taxYear=2017
		! end if
		fn_setupFederalTables(taxYear,mat ft,fed_annual_wh_allowance)
		rewrite #h_dates,using "form pos 1,2*n 8,x 32,n 8,c 20",rec=1: beg_date,end_date,d1,d1$
		close #h_dates: 
	end if
return  ! /r
DETERMINE_EARNINGS: ! r: passed eno, dep,beg_date, end_date, returns mat ytdTotal,ytdFICA,tmd,td14,twd,mat caf
	ytdFICA=tmd=td14=0: mat caf=(0)
	mat tcp=(0)
	mat ytdTotal=(0) : mat tdc=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",dep)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #h_payrollchecks,key>=checkkey$: nokey dePrCkNokey
	do
		read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prdate,ckno,mat tdc,mat tcp eof dePrCkEof
		if heno=eno and prdate=>beg_date and prdate<=end_date then 
			mat ytdTotal=ytdTotal+tcp
		end if
	loop while heno=eno
	dePrCkEof: ! 
	ytdFICA=ytdTotal(2) ! fica year to date
	tmd=ytdTotal(3) ! medicare year to date
	td14=ytdTotal(25) ! eic
	twd=ytdTotal(31) ! total wages
	for j=1 to 20
		caf(j)=ytdTotal(j+4) ! total miscellaneous deductions for year
	next j
	dePrCkNokey:!
return  ! /r
def fn_setup
	library 'S:\Core\Library': fntop, fnerror, fnxit,fnTos,fnFra,fnChk,fnLbl,fnTxt,fnCmdKey,fnAcs,fncd,fnpayroll_client_state$,fnmsgbox,fnStatus,fngethandle,fnStatusPause,fnDedNames,fnAutomatedSavePoint
	library 'S:\Core\Library': fnFree
	on error goto ERTN
	debug=0 ! if env$('ACSDeveloper')<>'' then debug=1 else debug=0
	! ______________________________________________________________________
	dim sck(4),motab(12),stwh(10,2),sucw(10),sucr(10)
	dim inp(29),dat$*20,cap$*128,caf(20)
	dim fullname$(20)*20,abrevname$(20)*8,resp$(10)*40
	dim tdt(4),tcd(3),tdet(17),tdc(10),tcp(32)
	dim ytdTotal(32)
	dim x$*8,em(16),hr(2),n$*8,in2$(4),stuc(10)
	dim dedcode(10),calcode(10),dedfed(10),d1$*20,wcm(4) ,newx$*8
	dim newdedcode(20),newcalcode(20),newdedfed(20)
	dim dedfica(20),dedst(20),deduc(20)
	dim ml$(1)*256
	! fn_setupFederalTables(2017,mat ft,fed_annual_wh_allowance)
	mtc=0 ! motab counter
	motab(mtc+=1)=0   : motab(mtc+=1)=31  : motab(mtc+=1)=59
	motab(mtc+=1)=90  : motab(mtc+=1)=120 : motab(mtc+=1)=151
	motab(mtc+=1)=181 : motab(mtc+=1)=212 : motab(mtc+=1)=243
	motab(mtc+=1)=273 : motab(mtc+=1)=304 : motab(mtc+=1)=334
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #20,using 'Form POS 145,PD 5.2,POS 230,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,POS 618,30*N 1,POS 708,3*PD 4.3,3*PD 3.2,4*PD 4.2,POS 133,PD 6.3,PD 6.2': fucr,loccode,feducmax,ficarate,ficamax,ficamxr,mat sucw,mat sucr,mat dedcode,mat calcode,mat dedfed,mat sck,vacm,MinHourlyWage,mat wcm,ficar2,ficamx2
	close #20: 
	ficamax=ficamax*10
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

fnend 
def fn_setupOpenFiles
	open #breakdown=fngethandle: "Name=[Q]\PRmstr\HourBreakdown.H[cno],KFName=[Q]\PRmstr\HourBreakdown-idx.H[cno],Shr",internal,outIn,keyed ioerr ignore ! formerly file #31
	open #hEmployee:=fngethandle: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno]",internal,outIn,keyed  ! formerly file #1
	open #h_department:=2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed 
	open #h_payrollchecks:=4: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr,Use,RecL=224,KPs=1,KLn=17",internal,outIn,keyed 
	open #44: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr",internal,outIn,keyed 
	open #h_rpwork:=3: "Name=[Q]\PRmstr\rpwork"&wsid$&".h[cno],KFName=[Q]\PRmstr\rpwork"&wsid$&"Idx.h[cno]",internal,outIn,keyed 
	F_RPWORK: form pos 1,c 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
fnend
GET_ALPHA_DATE: ! r:
	dim month$(12),payrolldate$*20
	payrolldate$=cnvrt$("pic(########)",d1)
	year=val(payrolldate$(1:4))
	month=val(payrolldate$(5:6))
	day=val(payrolldate$(7:8))
	month$(1)="January"
	month$(2)="February"
	month$(3)="March"
	month$(4)="April"
	month$(5)="May"
	month$(6)="June"
	month$(7)="July"
	month$(8)="August"
	month$(9)="September"
	month$(10)="October"
	month$(11)="November"
	month$(12)="December"
	d1$=month$(month)&" "&str$(day)&", "&str$(year)
return  ! /r
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
ST01: ! r:
! tcd(1) = state code
! g_pay_periods_per_year     = number of pay periods per year (formerly b8)
! em(3)  = allowances
! em(1)  = married (1=yes and more )
	s3=0
	if fnpayroll_client_state$='AR' then 
		gosub ARWH
	else if fnpayroll_client_state$='AZ' then 
		gosub AZWH
	else if fnpayroll_client_state$='GA' then 
		! if env$('acsDeveloper')<>'' then 
		s3=fn_wh_georgia(stwh(tcd(1),1),g_pay_periods_per_year,em(3),em(1),em(7))
		! else
		!   s3=0 ! fn_wh_georgia(stwh(tcd(1),1),g_pay_periods_per_year,em(3),em(1),em(7))
		! end if
	else if fnpayroll_client_state$='IL' then 
		gosub ILWH
	else if fnpayroll_client_state$='IN' then 
		gosub INWH
	else if fnpayroll_client_state$='KY' then ! added 10/03/2016 for R R Crawford Engineering
		s3=fn_wh_kentuky(stwh(tcd(1),1),g_pay_periods_per_year,em(3))
	else if fnpayroll_client_state$='LA' then 
		gosub LAWH
	else if fnpayroll_client_state$='MO' then 
		gosub MOWH
	else if fnpayroll_client_state$='MS' then 
		gosub MSWH
	else if fnpayroll_client_state$='OK' then 
		gosub OKWH
	else if fnpayroll_client_state$='OR' then 
		s3=fn_wh_oregon(stwh(tcd(1),1),fed_wh_annual_estimate,g_pay_periods_per_year,em(3),em(1))
	else if fnpayroll_client_state$='TN' then 
		goto ST1_XIT ! no Tenn wh
	else if fnpayroll_client_state$='TX' then 
		goto ST1_XIT ! no Texas wh
	end if 
	ST1_XIT: ! 
return  ! /r
ST02: s3=0 : return
ST03: s3=0 : return
ST04: s3=0 : return
ST05: s3=0 : return
ST06: s3=0 : return
ST07: s3=0 : return
ST08: s3=0 : return
ST09: s3=0 : return
ST10: s3=0 : return
def fn_setupFederalTables(taxYear,mat ft,&fed_annual_wh_allowance)
	dim ft(8,6)
	! r: Federal - SINGLE person (including head of household)
	if taxYear<=2017 then
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
	else if taxYear=2018 then
		fed_annual_wh_allowance=4150
		ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
		ft(2,1)=  3700 : ft(2,2)=     0    : ft(2,3)=0.1  
		ft(3,1)= 13225 : ft(3,2)=   952.5  : ft(3,3)=0.12 
		ft(4,1)= 42400 : ft(4,2)=  4453.5  : ft(4,3)=0.22 
		ft(5,1)= 86200 : ft(5,2)= 14089.5  : ft(5,3)=0.24 
		ft(6,1)=161200 : ft(6,2)= 32089.5  : ft(6,3)=0.32 
		ft(7,1)=203700 : ft(7,2)= 45689.5  : ft(7,3)=0.35 
		ft(8,1)=503700 : ft(8,2)=150689.5  : ft(8,3)=0.37 
	else if taxYear=2019 then
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
	! r: Federal - MARRIED person
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
fnend
ARWH: ! r: REPLACE ACSWRK\ARKANSAS.WH,SOURCE ! Arkansas #5 ar(7,3)  REVISED 7/01/91
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
	end if ! /r
	t1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
	t2=2000
	t3=t1-t2
	j1=fn_table_line(mat ar,t3)
	s1=round(ar(j1,2)+(t3-ar(j1,1))*ar(j1,3),2)
	s2=em(3)*20
	s3=round((s1-s2)/g_pay_periods_per_year,2)
	if s3<.1 then s3=0
return  ! /r
AZWH: ! r: REPLACE ACSWRK\ARIZONA.WH,SOURCE ! ARIZONA:  NO TABLE  REVISED 1/01/10
	! effective june 30, 2010 the rates changed and also the base change from a percent of federal wh to a percent of total taxable wages
	stp=0
	if em(3)=1 then stp=.013
	if em(3)=2 then stp=.018
	if em(3)=3 then stp=.027
	if em(3)=4 then stp=.036
	if em(3)=5 then stp=.042
	if em(3)=6 then stp=.0510
	s3=round(stwh(tcd(1),1)*stp,2)
	h3=min(h3,1200)
return  ! /r
def fn_wh_georgia(wga_wages_taxable_current,g_pay_periods_per_year,wga_allowances,wga_is_married,wga_eicCode)
	! created 06/29/2017
	! wga_wages_taxable_current - formerly b8
	! g_pay_periods_per_year - formerly stwh(tcd1,1)
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
	! if dev then pr 'wga_wages_taxable_current=';wga_wages_taxable_current
	! if dev then pr '   g_pay_periods_per_year=';g_pay_periods_per_year
	! if dev then pr '   Ga_StatePersonalAllowance=';Ga_StatePersonalAllowance
	! if dev then pr '   fn_standardStateDeduction=';Ga_StateDeduction
	! if dev then pr '   wga_allowances=';wga_allowances
	ga_WagesAnnual=(wga_wages_taxable_current)*g_pay_periods_per_year
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
	j1=fn_table_line(mat gawh,Ga_WagesAnnualTaxable)
	! if dev then pr '   table line ';j1
	! if dev then pause
	ga_AnnualWagesSubjToWithhold=Ga_WagesAnnualTaxable-gaAnnualDependantAllowance*wga_allowances
	s3=gawh(j1,2)+(ga_AnnualWagesSubjToWithhold-gawh(j1,1))*gawh(j1,3)
	s3=s3
	s3=s3/g_pay_periods_per_year
	s3=round(s3,2) ! round to the nearest whole dollar
	if s3<.1 then s3=0 ! do not withhold less than 10 cents.
	fn_wh_georgia=s3
fnend 
ILWH: ! r: REPLACE ACSWRK\ILLINOIS.WH,SOURCE ! ILLINOIS   NO TABLE
	! line 1 allowances = +1 for claiming self, +1 for claiming spouse
	! line 2 allowances = +1 for each other (not you nor spouse) dependent
	! em(3) - number of allowances
	! g_pay_periods_per_year = number of pay periods (formerly b8)
	g2=round((stwh(tcd(1),1))*g_pay_periods_per_year,2)
	!  new way needs awesome function !    allowances_line_1=fn_allowances_spouse_and_self
	!  new way needs awesome function !    allowances_line_2=em(3)-allowances_line_1
	!  new way needs awesome function !    g2=g2-(allowances_line_1*2175+allowances_line_2*1000)
	g2=g2-1000*em(3)
	s3=g2*.0495 ! changed from .0375 on 7/10/17  ! changed from .03 to .05 1/1/11, changed from .05 to .0375 1/1/15, ok as of 1/6/16
	s3=round(s3/g_pay_periods_per_year,2)
	if s3<.1 then s3=0 ! do not withhold less than 10 cents.
return  ! /r
INWH: ! r: INDIANA    NO TABLE   07/01/2000  ! still in effect 71508, changed on 1/1/2016, but I didn't bother to update it because no one is using it.
	! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
	h1=h2=h3=0
	h1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
	h2=em(3)*1000
	h3=h1-h2
	if h3>0 then 
		s3=h3*.034 ! +H3*.003  SOME COUNTIES HAVE WH
		s3=round(s3/g_pay_periods_per_year,2)
		if s3<.1 then s3=0
	end if 
return  ! /r
def fn_wh_kentuky(wky_wages_taxable_current,g_pay_periods_per_year,wky_allowances)
	! KYWH: ! REPLACE kentucky.wh/acswrk,source ! kentucky:  rec=20  ky(6,3) ! revised 12/31/2005
	! wky_wages_taxable_current - formerly b8
	! g_pay_periods_per_year - formerly stwh(tcd1,1)
	if ~wky_setup then 
		wky_setup=1
		! r: Pull the withholding routines from new\acswrk
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=20: mat ky ! Kentucky
		dim ky(6,3)
		ky(1,1)=0     : ky(1,2)=0    : ky(1,3)=0.02
		ky(2,1)=3000  : ky(2,2)=60   : ky(2,3)=0.03
		ky(3,1)=4000  : ky(3,2)=90   : ky(3,3)=0.04
		ky(4,1)=5000  : ky(4,2)=130  : ky(4,3)=0.05
		ky(5,1)=8000  : ky(5,2)=280  : ky(5,3)=0.058
		ky(6,1)=75000 : ky(6,2)=4166 : ky(6,3)=0.06
		! /r
	end if 
	h1=(wky_wages_taxable_current)*g_pay_periods_per_year
	h2=h1-1970
	j1=fn_table_line(mat ky,h2)
	s3=ky(j1,2)+(h2-ky(j1,1))*ky(j1,3)
	s3=s3-20*wky_allowances
	s3=s3/g_pay_periods_per_year
	s3=round(s3,2)
	if s3<.1 then s3=0 ! do not withhold less than 10 cents.
	fn_wh_kentuky=s3
fnend 
LAWH: ! r: REPLACE ACSWRK\LOUSIANA.WH,SOURCE ! LOUISANA: NO TABLE: LA(5): revised 1/01/03
	h1=0
	h2=0
	h3=0
	mat la=(0)
	s=round(stwh(tcd(1),1),2)
	if em(1)=0 or em(1)=2 then 
		y=em(3)-1
		x=1
		if y>=0 then goto L3800
		x=0
		y=0
		goto L3800
	end if
	if em(3)=0 then y=0 : x=0
	if em(3)=1 then y=0 : x=1
	if em(3)>=2 then y=em(3)-2 : x=2
	L3800: ! 
	if x<2 then m1=12500 : m2=25000
	if x>=2 then m1=25000 : m2=50000
	n=g_pay_periods_per_year
	if s>0 then a=(s*.021) else a=0
	if s>(m1/n) then b=.0135*(s-(m1/n)) else b=0
	if s>(m2/n) then c=.0135*(s-(m2/n)) else c=0
	d=.021*(((x*4500)+(y*1000))/n)
	if ((x*4500)+(y*1000))>m1 then 
		e=.0135*(((x*4500)+(y*1000)-m1)/n)
	else 
		e=0
	end if 
	if (a+b+c)-(d+e)>0 then 
		s3=(a+b+c)-(d+e)
	else 
		s3=0
	end if 
	s3=round(s3,2)
	if s3<.1 then s3=0
return  ! /r
MOWH: ! r: REPLACE ACSWRK\MISSOURI.WH,SOURCE ! MISSOURI MO(10,3) REC # 28  REVISED 1/1/2002
	if ~setup_mowh then  ! r: MO Missouri
		setup_mowh=1
		dim mo(10,3)
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=28: mat mo ! Missouri
		mo( 1,1)=   0 : mo( 1,2)=  0  : mo( 1,3)=0.015
		mo( 2,1)=1000 : mo( 2,2)= 15  : mo( 2,3)=0.02
		mo( 3,1)=2000 : mo( 3,2)= 35  : mo( 3,3)=0.025
		mo( 4,1)=3000 : mo( 4,2)= 60  : mo( 4,3)=0.03
		mo( 5,1)=4000 : mo( 5,2)= 90  : mo( 5,3)=0.035
		mo( 6,1)=5000 : mo( 6,2)=125  : mo( 6,3)=0.04
		mo( 7,1)=6000 : mo( 7,2)=165  : mo( 7,3)=0.045
		mo( 8,1)=7000 : mo( 8,2)=210  : mo( 8,3)=0.05
		mo( 9,1)=8000 : mo( 9,2)=260  : mo( 9,3)=0.055
		mo(10,1)=9000 : mo(10,2)=315  : mo(10,3)=0.06
	end if ! /r
	! MARITAL STATUS =2 IF HEAD OF HOUSEHOLD
	numb4=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
	if em(1)=0 or em(1)=2 then numb6=min(5000,fed_wh_annual_estimate) ! FEDERAL DED LIMITED TO 5000 FOR SINGLE
	if em(1)<>0 then numb6=min(10000,fed_wh_annual_estimate) ! FEDERAL DED LIMITED TO 10000 FOR MARRIED OR HEAD OF HOUSEHOLD
	if em(1)=1 or em(1)=3 or em(1)=4 or em(1)=5 then h1=3925 : goto L4110
	if em(1)=2 then h1=7850 : goto L4110
	h1=4700
	goto L4110
	L4110: ! 
	h2=0
	! on em(1)+1 goto L4160,L4140,L4180 none L4190
	if em(3)<>0 then 
		!
		if em(1)=0 then 
			h2=1200+(em(3)-1)*1200 ! SINGLE
		else if em(1)=1 or em(1)=3 or em(1)=4 or em(1)=5 then 
			h2=min(em(3),2)*1200+max(em(3)-2,0)*1200 ! MARRIED
		else if em(1)=2 then 
			h2=3500+max(em(3)-4,0)*1200 ! HEAD OF HOUSE HOLD
		end if
	end if
	h3=numb4-h1-h2-numb6
	if h3<0 then h3=0
	j1=fn_table_line(mat mo,h3)
	s3=(mo(j1,2)+(h3-mo(j1,1))*mo(j1,3))/g_pay_periods_per_year
	s3=round(s3,0)
	if s3<.1 then s3=0
return  ! /r
MSWH: ! r: REPLACE ACSWRK\MISISIPI.WH,SOURCE ! MISSISSIPPI  NO TABLE
	! **********  REMOVE THE EM(15) FROM LINE 740 **********
	! SUBSTITUTE THE EXEMPTIONS INTO THE FIELD NOW CALLED STATE TAX ADD-ON
	! THE EXEMPTIONS MUST BE ENTERED IN DOLLARS AND THE STANDARD DEDUCTION
	! MUST BE ADDED TO THE EXEMPTIONS.
	! SINGLE =2300, MARRIED=3400, MARRIED BOTH WORKING=1700
	h1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
	h3=h1-em(15)
	if h3<=0 then s3=0 : goto L4481
	if h3<10000 then goto L4474
	s3=350+.05*(h3-10000)
	goto L4481
	L4474: if h3>0 and h3<=5000 then goto L4477
	s3=150+.04*(h3-5000)
	goto L4481
	L4477: s3=.03*h3
	if s3<.1 then s3=0
	goto L4481
	L4481: s3=s3/g_pay_periods_per_year
	s3=round(s3,2)
	if s3<.1 then s3=0
return  ! /r
OKWH: ! r:  ACSWRK\OKLAHOMA.WH,SOURCE ! rec=39 ok(8,6) REV. 1/01/07 (table change also!)
	if ~setup_okwh then ! r: OK Oklahoma
		setup_okwh=1
		! dim ok(8,6)
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
	g2=stwh(tcd(1),1)*g_pay_periods_per_year
	g2=g2-em(3)*1000
	if em(1)=0 or em(1)=2 then j2=1 else j2=4 ! single of married
	j1=fn_table_line(mat ok,g2)
	s3=ok(j1,j2+1)+(g2-ok(j1,j2))*ok(j1,j2+2)
	s3=s3/g_pay_periods_per_year
	s3=round(s3,2)
	s3=round(s3,0)
	if s3<.1 then s3=0
return  ! /r
def fn_wh_oregon(wor_wages_taxable_current,wor_fed_wh_annual_estimate,wor_pay_periods_per_year,wor_allowances,wor_is_married)
	if ~wor_setup then 
		wor_setup=1
		! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=40: mat or1,mat or2
		dim or1(4,3) !  r: Withholding Table for Single with fewer than 3 allowances
		or1(1,1)=    0 : or1(1,2)= 197   : or1(1,3)=0.05
		or1(2,1)= 3350 : or1(2,2)= 367   : or1(2,3)=0.07
		or1(3,1)= 8450 : or1(3,2)= 724   : or1(3,3)=0.09
		or1(4,1)=50000 : or1(4,2)=4459.5 : or1(4,3)=0.09
		! /r
		dim or2(4,3) ! r: Single with 3 or more allowances, or married
		or2(1,1)=    0 : or2(1,2)= 197 : or2(1,3)=0.05
		or2(2,1)= 6700 : or2(2,2)= 537 : or2(2,3)=0.07
		or2(3,1)=16900 : or2(3,2)=1251 : or2(3,3)=0.09
		or2(4,1)=50000 : or2(4,2)=4223 : or2(4,3)=0.09
		! /r
	end if 
		! requires locally populated variables Mat OR1 and Mat OR2
		! returns Oregon State Withholding
		! Oregon  !  rec=40
		! 
		! RECALK: ! used only for debugging purposes
	wor_allowances_effective=wor_allowances
		! 
	wor_they_are_single=wor_they_are_married=0
	if wor_is_married=0 or wor_is_married=2 then wor_they_are_single=1
	if wor_is_married=1 or wor_is_married=3 or wor_is_married=4 or wor_is_married=5 then wor_they_are_married=1
		! 
	if wor_wages_taxable_current>100000 and wor_they_are_single then wor_allowances_effective=0
	if wor_wages_taxable_current>200000 and wor_they_are_married then wor_allowances_effective=0
		! 
	if wor_they_are_married or (wor_they_are_single and wor_allowances_effective>=3) then ! (married or more than 3 allowances)
		wor_table=2
	else ! (single and less than 3 allowances)
		wor_table=1
	end if 
		! 
	if wor_table=2 then ! wor_they_are_married then
		wor_standard_deduction=4350
	else ! if wor_table=1 then ! if wor_they_are_single then
		wor_standard_deduction=2175
	end if 
		! 
	wor_wages_annual_estimate=wor_wages_taxable_current*wor_pay_periods_per_year
		! 
	wor_phase_out=fn_oregonPhaseOut(wor_wages_annual_estimate,wor_fed_wh_annual_estimate,wor_table,wor_they_are_single,wor_they_are_married)
		! 
		! wor_base=wor_wages_taxable_current*wor_pay_periods_per_year-min(wor_fed_wh_annual_estimate,8550)-(wor_allowances_effective*2250)
	wor_base=wor_wages_annual_estimate-wor_phase_out-wor_standard_deduction
		! 
	if wor_table=2 then 
		wor_table_line=fn_table_line(mat or2,wor_base)
		wor_pre_base=or2(wor_table_line,2)
		wor_tax_rate=or2(wor_table_line,3)
		wor_remove_prev=or2(wor_table_line,1)
	else ! wor_table=1
		wor_table_line=fn_table_line(mat or1,wor_base)
		wor_pre_base=or1(wor_table_line,2)
		wor_tax_rate=or1(wor_table_line,3)
		wor_remove_prev=or1(wor_table_line,1)
	end if 
	if debug then let fnStatus('-------------------------------------------')
	if wor_they_are_single then 
		if debug then let fnStatus('  Single with '&str$(wor_allowances_effective)&' allowances')
	else if wor_they_are_married then 
		if debug then let fnStatus('  Married with '&str$(wor_allowances_effective)&' allowances')
	else 
		if debug then let fnStatus('  Maridal Status is undetermined!!!  ')
	end if 
	if debug then let fnStatus('    Current Wage (Gross)    = '&str$(wor_wages_taxable_current))
	if debug then let fnStatus('    Pay Periods Per Year    = '&str$(wor_pay_periods_per_year))
	if debug then let fnStatus('    Annual wage (estimate)  = '&str$(wor_wages_annual_estimate))
	if debug then let fnStatus('    standard deduction     = '&str$(wor_standard_deduction))
	if debug then let fnStatus('    table '&str$(wor_table)&' line '&str$(wor_table_line))
	if debug then let fnStatus('    phase out              = '&str$(wor_phase_out))
	if debug then let fnStatus('    fed_wh_annual_estimate = '&str$(wor_fed_wh_annual_estimate))
	if debug then let fnStatus('.')
	if debug then let fnStatus('    BASE = '&str$(wor_wages_annual_estimate)&' (an..wages) - '&str$(wor_phase_out)&' (phase out/fed wh) - '&str$(wor_standard_deduction)&' (std ded)')
	if debug then let fnStatus('    base                   = '&str$(wor_base))
		! fn  status('    pre_base               = '&str$(wor_pre_base))
		! fn  status('    tax rate               = '&str$(wor_tax_rate))
		! fn  status('    remove_prev            = '&str$(wor_remove_prev))
	if debug then let fnStatus('.')
	if debug then let fnStatus('                                   WH = '&str$(wor_pre_base)&' + [('&str$(wor_base)&' - '&str$(wor_remove_prev)&')] x '&str$(wor_tax_rate)&'] - (195 x '&str$(wor_allowances_effective)&')')
		! 
		! WH = 1,244 + [(BASE – 16,900        ) * 0.09] – (195 * allowances)
		! wor_return=or2(wor_table_line,2)+(wor_base-or2(wor_table_line,1))*or2(wor_table_line,3)
	wor_return = wor_pre_base +(( wor_base - wor_remove_prev) * wor_tax_rate) - (195 * wor_allowances_effective)
	fnStatus('withholding before dividing by pay periods = '&str$(wor_return))
	wor_return=wor_return/wor_pay_periods_per_year
	wor_return=round(wor_return,2)
	if wor_return<.1 then wor_return=0
	fnStatus('calculated withholding ='&str$(wor_return))
	if debug then let fnStatusPause ! pause
	fn_wh_oregon=wor_return
fnend 
def fn_oregonPhaseOut(opo_wages,opo_fed_wh,opo_table,opo_is_single,opo_is_married)
	if opo_wages<50000 then 
		opo_return=min(opo_fed_wh,6500)
	else if opo_table=1 then 
		if opo_wages => 50000 and opo_wages<125000 then opo_return= 6550 : goto OPO_XIT
		if opo_wages =>125000 and opo_wages<130000 then opo_return= 5200 : goto OPO_XIT
		if opo_wages =>130000 and opo_wages<135000 then opo_return= 3900 : goto OPO_XIT
		if opo_wages =>135000 and opo_wages<140000 then opo_return= 2600 : goto OPO_XIT
		if opo_wages =>140000 and opo_wages<145000 then opo_return= 1300 : goto OPO_XIT
		if opo_wages =>145000 then opo_return=0
	else ! if opo_table=2 then
		if opo_is_married then 
			if opo_wages => 50000 and opo_wages<250000 then opo_return= 6550 : goto OPO_XIT
			if opo_wages =>250000 and opo_wages<260000 then opo_return= 5200 : goto OPO_XIT
			if opo_wages =>260000 and opo_wages<270000 then opo_return= 3900 : goto OPO_XIT
			if opo_wages =>270000 and opo_wages<280000 then opo_return= 2600 : goto OPO_XIT
			if opo_wages =>280000 and opo_wages<290000 then opo_return= 1300 : goto OPO_XIT
			if opo_wages =>290000 then opo_return=0
		else ! if opo_is_single then
			if opo_wages => 50000 and opo_wages<125000 then opo_return= 6550 : goto OPO_XIT
			if opo_wages =>125000 and opo_wages<130000 then opo_return= 5200 : goto OPO_XIT
			if opo_wages =>130000 and opo_wages<135000 then opo_return= 3900 : goto OPO_XIT
			if opo_wages =>135000 and opo_wages<140000 then opo_return= 2600 : goto OPO_XIT
			if opo_wages =>140000 and opo_wages<145000 then opo_return= 1300 : goto OPO_XIT
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
	pay_periods_per_year=52
	wages_taxable_current=399.60
	fed_wh=7.75
	allowances=2
	is_married=4  ! is_married = 0 - Single
										! is_married = 1 - Married
										! is_married = 2 - Single - Head of Household
										! is_married = 3 - Married - filing joint return - only one working
										! is_married = 4 - Married - filing seperate or joint return both working
	eicCode=0    ! eicCode = 0 - Not qualified for EIC
									 ! eicCode = 1 - Single or Spouse not file
									 ! eicCode = 2 - Married both filing
	pr 'wages_taxable_current: ';wages_taxable_current
	pr ' pay_periods_per_year: ';pay_periods_per_year
	pr '               fed_wh: ';fed_wh
	pr '           allowances: ';allowances
	pr '           is_married: ';is_married
	pr '              eicCode: ';eicCode
	pr 'Kentuky Function returns ';fn_wh_kentuky(wages_taxable_current,pay_periods_per_year,allowances)
	pr 'Georgia Function returns ';fn_wh_georgia(wages_taxable_current,pay_periods_per_year,allowances,is_married,eicCode)
	pr 'Oregon Function returns ';fn_wh_oregon(wages_taxable_current,fed_wh,pay_periods_per_year,allowances,is_married)
	if env$('ACSdeveloper')<>'' then pause 
fnend 
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
WEST_ACC_WORKMANSCOMP: ! r:
	! inp(6) Other Compensation
	! em(5)  Pay Code
	! if other compensation > 0 then
	!   if pay code is 1 hours = 173.33
	!   if pay code is 2 hours = 86.66
	!   if pay code is 3 hours = 80
	!   if pay code is 4 hours = 40
	! else 
	!   hours = regular hours + overtime hours
	! end if
	tmphrs=inp(1)+inp(2) ! if inp(6)>0 then tmphrs=saif(em(5)) else tmphrs=inp(1)+inp(2)
	!     fnStatus('inp(17) changed to '&str$(round(tmphrs*inp(17)*.01,2))&' round('&str$(tmphrs)&' * inp(17)('&str$(inp(17))&' * .01)',2)
	!     fnStatusPause
	inp(17)=round(tmphrs*inp(17)*.01,2) ! inp(17)=round(tmphrs*inp(17)*.01,2)
return  ! /r
