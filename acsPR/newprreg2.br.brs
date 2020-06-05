! Replace S:\acsPR\newprReg2
! Payroll Register (Part 2 of 2)
	fn_setup
	det=0
	fn_payroll_register_2(det)
Xit: fnXit
def fn_setup
	if ~setup then 
		setup=1
		autoLibrary
		on error goto Ertn
	end if 
fnend 
def library fnpayroll_register_2(; det,include_tips_in_other_wh,append_reg1,ppdOverride)
	fn_setup
	fnpayroll_register_2=fn_payroll_register_2( det,include_tips_in_other_wh,append_reg1,ppdOverride)
fnend 
def fn_payroll_register_2(; det,include_tips_in_other_wh,append_reg1,ppdOverride)
	! DET=1 if you dont want details printed but you want department totals.
	! DET=2 if you dont want details or department totals printed.

	dim a$*40,em$*30,cp(32),tcp(32),hc(5),thc(5),whc(10),gcp(32)
	dim tdc(10),fullname$(20)*20,abbrevname$(20)*20,client$*30
	dim dedcode(10),calcode(10),dedfed(10),statname$(10)*8
	dim newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20)
	dim statewh(10),totaltcp(32),totalthc(5),deptname$*20
	dim sucrat(10),stuc1(10),stuc2(10),err$(3)*65,cap$*128

	fnTop(program$,cap$="Payroll Registers")
	open #20: "Name=[Q]\PRmstr\prCode.h[cno],Shr",internal,input ioerr L180
	read #20,using 'Form POS 5,N 5': ckno
	close #20: 
	L180: ! 
	if ~append_reg1 then let fnopenprn( 0,0,0,fnprocess,' (Departmental Register)')
	
	open #9: "Name=[Q]\PRmstr\DeptName.h[cno],KFName=[Q]\PRmstr\DeptNameIdx.h[cno],Shr",internal,input,keyed ioerr L220X
	founddept=1
	L220X: !
	if ppdOverride then
		ppd=ppdOverride
	else
		d1$=cnvrt$("pic(zzzzzzzz)",fnPayPeriodEndingDate)
		ppd=val(d1$(5:6))*10000+val(d1$(7:8))*100+val(d1$(3:4))
	end if
	fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	
	ssr1=fnss_employee*.01
	ssr2=fnss_employer*.01
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #1,using F_COMPANY: a$,ficar2,feducrat,mat statname$,ficar1,mat sucrat
	F_COMPANY: form pos 1,c 40,pos 133,pd 6.3,pos 145,pd 5.2,pos 150,10*c 8,pos 236,pd 3.3,pos 287,10*pd 3.3,pos 618,30*n 1,pos 648,10*c 6
	close #1: ioerr ignore
	! 
	ficar1=ficar1*.01
	ficar2=ficar2*.01
	ficarate=ficar1+ficar2
	fnIndex("[Q]\PRmstr\PayrollChecks.h[cno]","[Q]\PRmstr\CheckIdx2.h[cno]","9/12/1 3/6/8")
	! execute "Index [Q]\PRmstr\PayrollChecks.h[cno],[Q]\PRmstr\CheckIdx2.h[cno] 9/12/1 3/6/8,replace,DupKeys -n"
	open #h_payrollchecks:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\CheckIdx2.h[cno]",internal,input,keyed 
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed 
	open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed 
	! Read #2,Using 370,Rec=ADR: ENO, DEP1,LPD,TCD(1),MAT TDET,MAT HC,MCWH,MAT CP
	READ_CHECKS: ! 
	read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,dep1,prdate,ckno,mat tdc,mat cp eof L1990
	!  if eno=307 then pr 'eno '&str$(eno) : exe 'break other_wh' : break_is_on=1 else if break_is_on then exe 'break other_wh off' : break_is_on=0
	! mcwh now in cp(3)
	if eno=0 and dep1=0 then goto READ_CHECKS
	if prdate><fndate_mmddyy_to_ccyymmdd(ppd) then goto READ_CHECKS
	read #2,using 'form pos 48,n 2',key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",dep1): statecode
	read #1,using 'form pos 9,c 30',key=lpad$(str$(eno),8): em$
	if det=2 then goto L580
	a=pos (rtrm$(em$)," ",1)
	b=pos (rtrm$(em$)," ",a+1)
	em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a)  error ignore
	if dep2=0 then goto L550
	if dep1=dep2 then goto L580
	gosub PrintDepartmentTotals
	pr #255: newpage
	L550: ! 
	deptname$=""
	if founddept=1 then 
		read #9,using "form pos 4,c 20",key=rpad$(ltrm$(str$(dep1)),3): deptname$ nokey ignore
	end if 
	gosub HDR
	dep2=dep1
	L580: ! 
	oi=cp(27)+cp(28)+cp(29)+cp(30)
	t3=t4=0
	for j=5 to 24
		if newdedcode(j-4)=3 then goto L680
		if newdedcode(j-4)=2 then 
			other_wh=other_wh-cp(j) ! if break_is_on and cp(j)<>0 then pr 'cp('&str$(j)&') deducts '&str$(cp(j))
		else 
			other_wh=other_wh+cp(j) ! if break_is_on and cp(j)<>0 then pr 'cp('&str$(j)&')    adds '&str$(cp(j))
		end if 
		if newdedfed(j-4)=2 and newdedcode(j-4)=1 then goto L630 else goto L660
		L630: !
		t3=t3+cp(j) : tt3=tt3+cp(j): gtt3=gtt3+cp(j) ! cafiteria
		!     if client$="Washington Parrish" and j=5 then totaldef=totaldef+cp(5) ! add deferred comp match and to later add to medicare wages
		goto L680
		L660: if newdedfed(j-4)=1 and newdedcode(j-4)=1 then goto L670 else goto L680
		L670: t4=t4+cp(j) ! retirement only
		L680: next j
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
		if det=1 or det=2 then goto L760
		pr #255,using F_PR_LINE: dep1,eno,em$(1:11),mat hc,tothc,cp(31),cp(3),cp(2),cp(1),cp(4),other_wh,cp(32) pageoflow NWPG
F_PR_LINE: form pos 1,n 4,n 8,x 2,c 12,6*n 7.2,7*n 9.2,skip 1
L760: ! 
		sswg=ficawag=tdet(1)
		ficawage=ficawage+ficawag
		totalfi=totalfi+ficawag
		sswh1=sswh1+cp(2)
		sswh2=sswh2+cp(2)
		mcwh1=mcwh1+cp(3)
		mcwh2=mcwh2+cp(3)
		mcwg=tdc(8)
		sswg=tdc(7)
!   if client$="Washington Parrish" and mcwh>0 then mcwg=cp(31)-t3+cp(15) ! add deferred comp match to medicare wages  (always must be misc 2 deduction)
		sswg1=sswg1+sswg
		sswg2=sswg2+sswg
		mcwg1=mcwg1+mcwg
		mcwg2=mcwg2+mcwg
		stateuc+=round(tdc(10)*sucrat(statecode)*.01,2)
		feduc+=round(tdc(9)*feducrat*.01,2)
!   if client$="Washington Parrish" then feducwg=tdc(9)+tcp(5): goto L870
		feducwg=tdc(9)
! L870: ! 
		fedwages=fedwages+feducwg
		totalfuc=totalfuc+feducwg
!   if client$="Washington Parrish" then stucwg=tdc(10)+tcp(5): goto L900
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
		goto READ_CHECKS

PrintDepartmentTotals: ! r:
	lp=lp+1
	oi=tcp(17)+tcp(18)+tcp(20)+tcp(19)
	for j=5 to 24
		if newdedcode(j-4)=3 then goto L1030
		if newdedcode(j-4)=2 then other_wh=other_wh-tcp(j) else other_wh=other_wh+tcp(j)
		L1030: ! 
	next j
	other_wh=other_wh-tcp(25)
	pr #255: "                   ________________________________________________________________________________________________________________" pageoflow NWPG
	pr #255,using F_PR_DTOTALS_1: " Department Totals:",thc(2),thc(4),tothrs,tcp(3),tcp(1),other_wh pageoflow NWPG
	tothrs=0
	pr #255,using F_PR_DTOTALS_2: thc(1),thc(3),thc(5),tcp(31),tcp(2),tcp(4),tcp(32) pageoflow NWPG
	pr #255: "                   ================================================================================================================" pageoflow NWPG
	F_PR_DTOTALS_1: form pos 1,c 26,n 14.2,n 14.2,n 14.2,n 18.2,n 18.2,n 18.2,n 10.2,skip 1
	F_PR_DTOTALS_2: form pos 1,n 33.2,n 14.2,n 14.2,n 16.2,n 18.2,n 18.2,n 18.2,skip 1
	if 66-lp<26 then pr #255: newpage
	pr #255: ""
	pr #255: ""
	pr #255,using 'form pos 10,c 50': "Department Totals"
	pr #255: ""
	pr #255,using 'form pos 12,c 30': "Tax Expense"
	pr #255,using F_PR_DTOTALS_3: "Medicare ",tcp(3)
	if env$('client')="Thomas Richardson" or env$('client')="Kincaid" then 
		pr #255,using F_PR_DTOTALS_3: "SS  ",round(tcp(2)/ssr1*ssr2,2) ! show only employeer expense here
	else 
		pr #255,using F_PR_DTOTALS_3: "SS  ",tcp(2)+round(tcp(2)/ssr1*ssr2,2) ! show total AND employeer matching
	end if 
	F_PR_DTOTALS_3: form pos 20,c 10,pic(-------.##),skip 1
	pr #255,using F_PR_DTOTALS_3: "Fed U/C",feduc
	pr #255,using F_PR_DTOTALS_3: "State U/C",stateuc
	pr #255,using F_PR_DTOTALS_3: "     Total",round(tcp(2)/ssr1*ssr2,2)+feduc+stateuc+tcp(3) ! 2013
	pr #255: ""
	pr #255,using F_PR_DTOTALS_4: "Net Pay",tcp(32)
	pr #255: ""
	pr #255,using F_PR_DTOTALS_4: "Taxable Wages",taxwg1
	F_PR_DTOTALS_4: form pos 10,c 20,n 10.2,skip 1
	tfw=ficawage
	if tfw>=tcp(31)-tt3-.1 and tfw<=tcp(31)-tt3+.1 then goto L1310
	goto L1320
	L1310: ! 
	tfw=tcp(31)
	L1320: ! 
	! pr #255,using 1100: "FICA Wages",tfw ! use same form as "Taxable wages"
	tucw=fedwages
	if tucw>=tcp(31)-tt3-.1 and tucw<=tcp(31)-tt3+.1 then goto L1350 else goto L1360
	L1350: ! 
	tucw=tcp(31)-tt3
	L1360: ! 
	pr #255,using F_PR_DTOTALS_4: "SS Wages",sswg1
	pr #255,using F_PR_DTOTALS_4: "MC Wages",mcwg1
	pr #255,using F_PR_DTOTALS_4: "Fed U/C Wages",tucw
	for j=1 to 10
		if stuc1(j)=0 then goto L1420
		pr #255,using F_PR_DTOTALS_4: statname$(j)&" UC Wages",stuc1(j)
	L1420: ! 
	next j
	pr #255,using F_PR_DTOTALS_4: "Payroll Tax Deposit",tcp(1)+tcp(2)+round(tcp(2)/ssr1*ssr2,2)-tcp(25)+tcp(3)*2 ! 2013
	pr #255,using 'form skip 1,pos 12,c 30': "Payroll Deductions"
	pr #255,using F_PR_DTOTALS_3: "SS-Wh",sswh1
	pr #255,using F_PR_DTOTALS_3: "MC-Wh",mcwh1
	pr #255,using F_PR_DTOTALS_3: "Fed",tcp(1)
	pr #255,using F_PR_DTOTALS_3: "State",tcp(4)
	for j=1 to 20
		if tcp(j+4)=0 then goto L1530
		if newdedcode(j)<>1 then goto L1530
		pr #255,using F_PR_DTOTALS_3: abbrevname$(j),tcp(j+4)
	L1530: ! 
	next j
	pr #255,using 'form skip 1,pos 12,c 30': "Additions to Net"
	for j=1 to 20
		if tcp(j+4)=0 or newdedcode(j)<>2 then goto L1580
		pr #255,using F_PR_DTOTALS_3: abbrevname$(j),tcp(j+4)
	L1580: ! 
	next j
	if tcp(25)=0 then goto L1610
	pr #255,using F_PR_DTOTALS_3: "EIC",tcp(25)
	L1610: ! 
	if final=1 then goto PRINTDEPARTMENTTOTALS_XIT
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
	dep=dep+1 ! count # of departments used
	PRINTDEPARTMENTTOTALS_XIT: ! 
return  ! /r
NWPG: ! r:
		pr #255: newpage
		ct1=ct1+1: pr f "12,45,CL 5,N": str$(ct1)
		gosub HDR
		continue  ! /r
HDR: ! r:
		pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
		pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
		pr #255: "\qc  {\f201 \fs20 \b Payroll Departmental Register}"
		pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zz/zz/zz)",ppd)&"}"
		pr #255: "\qc  {\f181 \fs16 \b "&trim$(deptname$)&"}"
		pr #255: "\ql   "
! pr #255,Using 1860: A$,"Payroll Register",D$,TRIM$(DEPTNAME$)
! Form SKIP 2,POS 1,CC 132,SKIP 1,POS 58,C 18,SKIP 1,POS 1,CC 132,SKIP 1,POS 1,CC 132,SKIP 2
L1860: pr #255: tab(29);"<----------------Hours----------------->";
		pr #255: tab(71);"<-Pay->";
		pr #255: tab(79);"<-----------------Deductions---------------->";
		pr #255: tab(129);"Net"
		if eofcode=1 then goto L1940
		pr #255: "Dept";
		if det=1 or det=2 then goto L1940
		pr #255: tab(8);"Emp #  Name";
L1940: pr #255: tab(29);" Reg    O/T   Sick    Vac    Hol   Total";
		pr #255: tab(71);"  Total   Med WH    SS WH  Federal    State    Other      Pay"
		lp=6
		return  ! /r

L1990: ! 
		eofcode=1
		if det=2 then goto L2040
		gosub PrintDepartmentTotals
		if dep=1 then goto L2200 ! only 1 dept printed-no summary
		pr #255: newpage ! pr final departmental summary
		L2040: ! pr #255,Using 1840: A$,"Payroll Register",D$
		pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
		pr #255: "\qc  {\f201 \fs20 \b Payroll Register - Departmental Totals}"
		pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zz/zz/zz)",ppd)&"}"
		pr #255: "\ql   "
		mat tcp=totaltcp : mat thc=totalthc : tt3=gtt3
		ficawage=totalfi : fedwages=totalfuc : mat stuc1=stuc2
		sswh1=sswh2 : mcwh1=mcwh2 : sswg1=sswg2
		mcwg1=mcwg2 : taxwg1=taxwg2 : tothrs=grandtothrs
		pr #255: ""
		pr #255,using 'form pos 52,c 40': "Summary for all Departments"
		gosub L1860
		final=1
		gosub PrintDepartmentTotals
		pr #255,using 'form skip 1,pos 12,c 30': "State W/H Breakdown"
		for j=1 to 10
			if statewh(j)=0 then goto L2190
			pr #255,using F_PR_DTOTALS_4: statname$(j),statewh(j)
			L2190: ! 
		next j
		L2200: ! 
		close #1: ioerr ignore
		close #2: ioerr ignore
		close #h_payrollchecks: ioerr ignore
		! fnStatusClose
		fncloseprn
		fnIndex("[Q]\PRmstr\prTot.h[cno]","[Q]\PRmstr\PRTotIdx.h[cno]","1 9")
		! fnStatusClose
fnend 
include: Ertn
