! formerly S:\acsPR\newprFM
! Payroll Employee File
fn_setup
fnTop(program$)
fn_openFiles
goto Menu1

Menu1: ! r:
	fnTos : screen=0
	respc=0
	fnLbl(1,1,'Employee Number:',16,right)
	fnCmbEmp(1,18)
	if hact$='' then
		resp$(respc+=1)=''
	else
		resp$(respc+=1)=hact$
	end if
	fnCmdKey('&Add',1,0,0,'Add a new employee' )
	fnCmdKey('E&dit',2,1,0,'Access the highlighted record')
	fnCmdKey('&Search',8,0,0,'Search for employee record')
	fnCmdKey('E&xit',6,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$) ! ask employee #
	hact$=resp$(1)(1:8)
	eno=ent=val(resp$(1)(1:8))
	if ckey=1 then
		goto EmployeeAdd
	else if ckey=2 then
		fn_employeeEdit(ent)
		goto Menu1
	else if ckey=8 then
		fnEmployeeSrch(x$,fixgrid)
		ent=val(x$)
		fn_employeeEdit(ent)
		goto Menu1
	else if ckey=6 or env$('ExitNow')='yes' then ! Added ExitNow env$ by GSB to ensure program recursively exits when they click the Windows X in a Subwindow
		goto Finis
	end if
goto EmployeeAdd ! /r
EmployeeAdd: ! r:
	fnTos : screen=scrEmployee
	respc=0 : frac=0
	mylen=25 : mypos=mylen+2
	fnLbl(1,1,'Employee Number:',mylen,1)
	fnTxt(1,mylen+3,8,8,1,'30',0,'Employee numbers must be numeric.')
	resp$(respc+=1)='' ! str$(eno)
	fnCmdKey('&Next',1,1,0,'Process employee information.')
	fnCmdKey('&Cancel',5,0,1,'Returns to maintenance screen.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto EmployeeAddXit
	add1=1
	ent=val(resp$(1))
	ent$=lpad$(str$(ent),8)
	read #hEmployee,using F_employee,key=ent$: tempeno nokey DoEmployeeAdd
		! r: only happens if employee number already exists
		mat ml$(2)
		ml$(1)='A record with this number already exists!'
		ml$(2)='Select a different employee number.'
		fnMsgBox(mat ml$,resp$,'',48)
	goto Menu1 ! /r
	DoEmployeeAdd: !
	dim em$(3)*30
	mat em$=('')
	dim ph$*12
	dim ss$*11
	ph$=ss$=''
	bd=0
	dim rs(2)
	mat rs=(0)
	dim em(16)
	mat em=(0)
	lpd=tgp=0
	dim ty(21)
	mat ty=(0)
	dim tqm(17)
	mat tqm=(0)
	dim tcp(22)
	mat tcp=(0)
	mat sc=(0)
	eno=teno=holdeno=ent
	! holdem$=em$(1)
	dd$='N'
	! clear bank draft
	rtn=0
	acc=0
	acn$=''
	dim w4Step2
	w4Step2        =0
	w4Step3        =0
	w4Step4a       =0
	w4Step4b       =0
	w4Step4c       =0
	disableStTax=0
	disableFedTax=0
	dim w4Year$*4
	w4Year$='none'
	fn_employeeEdit(ent, 1)
	EmployeeAddXit: !
goto Menu1 ! /r

def library fnEmployeeEdit(eno)
	if ~setup then fn_setup
	fn_openFiles
	fnEmployeeEdit=fn_employeeEdit(eno)
	fn_closeFiles
fnend
def fn_employeeEdit(ent; employeeAdding)
	if employeeAdding then goto ScrEmployee

	EmployeeEdit: ! r:
		if ent=0 then goto EmployeeEditXit
		teno=eno=ent ! hdar=0
		ent$=lpad$(str$(ent),8)
		read #hEmployee,using F_employee,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c nokey EmpNotFound
		holdeno=eno
	goto ScrEmployee ! /r
	EmpNotFound: ! r:
		mat ml$(2)
		ml$(1)='A record with this number does not exist!'
		ml$(2)='Select a different employee number.'
		fnMsgBox(mat ml$,resp$,'',48)
	goto EmployeeEditXit ! /r

	ScrEmployee: ! r:
		fnTos : screen=scrEmployee ! r: build the screen
		respc=0 : frac=0 : mat lc=(0)
		mylen=28 : mypos=mylen+2
		col1_pos=1 : col1_len=mylen
		col2_pos=col1_pos+col1_len+2

		col3_pos=58 : col3_len=21 ! 20
		col4_pos=col3_pos+col3_len+2 ! 73
		fn_navButtons(lc(1),deptCount)
		lc(1)+=1 : lc(2)=lc(1)
		fnLbl(lc(1)+=1,1,'Employee Number:',mylen,1)
		fnTxt(lc(1)   ,mylen+3,8,8,1,'30',0,'Employee numbers must be numeric.')
		resp$(resp_eno=respc+=1)=str$(eno)
		! r: col 1 top section
		lc(1)+=1
		fnLbl(lc(1)+=1,1,'Name:',mylen,1)
		fnTxt(lc(1)   ,mylen+3,col2_len,30,0,'',0,'Name can be entered first name first or last name first.')
		resp$(resp_name=respc+=1)=em$(1)

		fnLbl(lc(1)+=1,1,'Address:',mylen,1)
		fnTxt(lc(1)   ,mylen+3,col2_len,30,0,'',0,'')
		resp$(resp_addr=respc+=1)=em$(2)
		fnLbl(lc(1)+=1,1,'City, State Zip:',mylen,1)
		fnTxt(lc(1)   ,mylen+3,col2_len,30,0,'',0,'')
		resp$(resp_csz=respc+=1)=em$(3)
		lc(1)+=1

		fnLbl(lc(1)+=1,1,'Social Security Number:',mylen,1)
		fnTxt(lc(1)   ,mylen+3,11,11,0,'',0,'')
		resp$(resp_ssn=respc+=1)=ss$
		! /r
		! r: col 2 top section
		lc(2)=4

		fnLbl(lc(2)+=1,col3_pos,'Birth Date:',col3_len,1)
		fnTxt(lc(2)   ,col4_pos,10,10,0,'1',0,'The birth date is not required.')
		resp$(resp_birthDate=respc+=1)=str$(bd)

		fnLbl(lc(2)+=1,col3_pos,'Phone Number:',col3_len,1)
		fnTxt(lc(2)   ,col4_pos,12)
		resp$(resp_phone=respc+=1)=ph$

		fnLbl(          lc(2)+=1,col3_pos,'Race:',col3_len,1)
		fnComboA('Race',lc(2)   ,col4_pos,mat race_option$,'',16)
		resp$(resp_race=respc+=1)=fnSetForCombo$(mat race_option$,str$(rs(1)))

		fnLbl(         lc(2)+=1,col3_pos,'Sex:',col3_len,1)
		fnComboA('Sex',lc(2)   ,col4_pos,mat gender_option$,'',10)
		resp$(resp_sex=respc+=1)=fnSetForCombo$(mat gender_option$,str$(rs(2)))

		fnLbl(             lc(2)+=1,col3_pos,'Marital Status:',col3_len,1)
		fnComboA('Marital',lc(2)   ,col4_pos,mat marriedOption$,'',25) ! ,'',11)
		resp$(resp_married=respc+=1)=fnSetForCombo$(mat marriedOption$,str$(em(1)))

		! lc(2)+=1
		! fnLbl(             lc(2)+=1,col3_pos,'Dependants - Under 17:',col3_len,1)
		! fnTxt(             lc(2)   ,col4_pos,2,0,1,'30',0,'Employee numbers must be numeric.')
		! resp$(resp_dependentsMinor=respc+=1)=str$(dependentsMinor)
		! fnLbl(             lc(2)+=1,col3_pos,'Dependants - Other:',col3_len,1)
		! fnTxt(             lc(2)   ,col4_pos,2,0,1,'30',0,'Employee numbers must be numeric.')
		! resp$(resp_dependentsOther=respc+=1)=str$(dependentsOther)

		! /r
		! r: col 2 - state and federal section
		lcTmp=lc(2)+=2

		fnChk(lc(2)+=1,col4_pos+1,'Disable Federal Taxes',1) ! , align,contain,tabcon,chk_disable)
		resp_disableFedTax=respc+=1 : if em(12)=-1 then resp$(resp_disableFedTax)='True' else resp$(resp_disableFedTax)='False'

		fnLbl(             lc(2)+=1,col3_pos,'Federal Exemptions:',col3_len,1)
		fnComboA('FedEx',  lc(2)   ,col4_pos,mat fed_exemption_option$,'',3)
		resp$(resp_fedExepmtions=respc+=1)=fnSetForCombo$(mat fed_exemption_option$,str$(em(2)),1,2)
		fnLbl(lc(2)+=1,col3_pos,'Federal Tax Add-On:',col3_len,1)
		fnTxt(lc(2)   ,col4_pos,10,10,0,'32',0,'If you wish for the system to add additional Federal withholdings, enter that amount here.')
		resp$(resp_fedAddOn=respc+=1)=str$(em(13))
		fnLbl(lc(2)+=1,col3_pos,'Standard Federal W/H:',col3_len,1)
		fnTxt(lc(2)   ,col4_pos,10,10,0,'32',0,'If you wish for the system to withhold a fixed amount of Federal withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip Federal withholdings on this employee.')
		resp$(resp_stdFed=respc+=1)=str$(em(12))
		lc(2)+=1
		fnChk(lc(2)+=1,col4_pos+1,'Disable State Taxes',1) ! , align,contain,tabcon,chk_disable)
		resp_disableStTax=respc+=1 : if em(14)=-1 then resp$(resp_disableStTax)='True' else resp$(resp_disableStTax)='False'
		fnLbl(             lc(2)+=1,col3_pos,'State Exemptions:',col3_len,1)
		fnComboA('StateEx',lc(2)   ,col4_pos,mat fed_exemption_option$,'',3)
		resp$(resp_stExeptions=respc+=1)=fnSetForCombo$(mat fed_exemption_option$,str$(em(3)),1,2)
		fnLbl(lc(2)+=1,col3_pos,'State Tax Add-On:',col3_len,1)
		fnTxt(lc(2)   ,col4_pos,10,10,0,'32',0,'If you wish for the system to add additional state withholdings, enter that amount here.')
		resp$(resp_StateAddOn=respc+=1)=str$(em(15))
		fnLbl(lc(2)+=1,col3_pos,'Standard State W/H:',col3_len,1)
		fnTxt(lc(2)   ,col4_pos,10,10,0,'32',0,'If you wish for the system to withhold a fixed amount of State withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip state withholdings on this employee.')
		resp$(resp_stdState=respc+=1)=str$(em(14))
		! /r

		! lc(1)=lcTmp
		fnLbl(lc(1)+=1,col1_pos,'Date Hired:',col1_len,1)
		fnTxt(lc(1)   ,col2_pos,10,10,0,'1',0,'The date hired is only used for information purposes only.')
		resp$(resp_hireDate=respc+=1)=str$(em(16))
		fnLbl(lc(1)+=1,col1_pos,'Last Payroll Date:',col1_len,1)
		fnTxt(lc(1)   ,col2_pos,10,10,0,'1',0,'This will always be the last time pay was calculated on this employee.')
		resp$(resp_lastPayrollDate=respc+=1)=str$(lpd)

		fnLbl(               lc(1)+=1,1,'Employment Status:',mylen,1)
		fnComboF('EmpStatus',lc(1)   ,col2_pos,20,'[Q]\PRmstr\EmpStatus.dat',1,2,3,15,'[Q]\PRmstr\EmpStatus.idx',0,0, ' ',fracustinfo,0)
		resp$(resp_empStatus=respc+=1)=str$(em(4))
		fnLbl(              lc(1)+=1,col1_pos,'Pay Code:',col1_len,1)
		fnComboA('PayCode', lc(1)    ,col2_pos,mat payPeriodOption$,'',16)
		resp$(resp_payCode=respc+=1)=fnSetForCombo$(mat payPeriodOption$,str$(em(5)))
		lc(1)+=1
		fnLbl(lc(1)+=1,col1_pos,'Vacation Pay Code:',col1_len,1)
		fnTxt(lc(1)   ,col2_pos,6,6,0,'33',0,'Normally is number of vacation hours you want accrued each pay period.')
		resp$(resp_vacationPay=respc+=1)=str$(em(9))
		fnLbl(lc(1)+=1,col1_pos,'Vacation Hours Accrued:',col1_len,1)
		fnTxt(lc(1)   ,col2_pos,10,10,0,'32',0,'This should be the balance of vacation hours available at this time.')
		resp$(resp_vacationAccrued=respc+=1)=str$(em(11))


		lc(1)+=2
		fnLbl(lc(1)+=1,1,'Sick Pay Code:',col1_len,1)
		fnTxt(lc(1)   ,col2_pos,6,6,0,'33',0,'Normally is number of sick hours you want accrued each pay period.')
		resp$(resp_sickPay=respc+=1)=str$(em(8))
		fnLbl(lc(1)+=1,1,'Sick Hours Accrued:',col1_len,1)
		fnTxt(lc(1)   ,col2_pos,10,10,0,'32',0,'This should be the balance of sick hours available at this time.')
		resp$(resp_sickAccrued=respc+=1)=str$(em(10))

		! lc(2)-=2   !  go back up two lines and do the right side

		fnLbl(              lc(2)+=1,col3_pos,'FICA Code:',col3_len,1)
		fnComboA('FICACode',lc(2)    ,col4_pos,mat code6$,'',25)
		resp$(resp_ficaCode=respc+=1)=fnSetForCombo$(mat code6$,str$(em(6)))
		! fnLbl(              lc(2)+=1,col3_pos,'EIC Code:',col3_len,1)
		! fnComboA('EICCode', lc(2)    ,col4_pos,mat eicOption$,'',25)
		! resp$(resp_EicCode=respc+=1)=eicOption$(em(7)+1)


		lc(1)+=1

		fnLbl(             lc(1)+=1,col1_pos,'W-4 Year:',col1_len,1)
		fnComboA('w4Year', lc(1)   ,col2_pos,mat w4yearOption$,'Only used if W-4 Year is set to 2020 or later.',5)
		resp$(resp_w4year=respc+=1)=w4Year$

		fnChk(lc(1)+=1,col2_pos+1,'2020 W-4 Step 2',1) ! , align,contain,tabcon,chk_disable)
		resp_w4Step2=respc+=1 : if w4Step2 then resp$(resp_w4Step2)='True' else resp$(resp_w4Step2)='False'

		fnLbl(             lc(1)+=1,col1_pos,'2020 W-4 Step 3:',col1_len,1)
		fnTxt(             lc(1)   ,col2_pos,10,10,0,'32',0,'Only used if W-4 Year is set to 2020 or later.')
		resp$(resp_w4Step3=respc+=1)=str$(w4Step3)
		fnLbl(             lc(1)+=1,col1_pos,'2020 W-4 Step 4a:',col1_len,1)
		fnTxt(             lc(1)   ,col2_pos,10,10,0,'32',0,'Only used if W-4 Year is set to 2020 or later.')
		resp$(resp_w4Step4a=respc+=1)=str$(w4Step4a)
		fnLbl(             lc(1)+=1,col1_pos,'2020 W-4 Step 4b:',col1_len,1)
		fnTxt(             lc(1)   ,col2_pos,10,10,0,'32',0,'Only used if W-4 Year is set to 2020 or later.')
		resp$(resp_w4Step4b=respc+=1)=str$(w4Step4b)
		fnLbl(             lc(1)+=1,col1_pos,'2020 W-4 Step 4c:',col1_len,1)
		fnTxt(             lc(1)   ,col2_pos,10,10,0,'32',0,'Only used if W-4 Year is set to 2020 or later.')
		resp$(resp_w4Step4c=respc+=1)=str$(w4Step4c)



		! picture=0
		! fnCmdKey('&Departments ('&str$(deptCount)&')',2,0,0,'Review this employee''s departmental information.')
		fnCmdKey('Direct D&eposit',7,0,0,'Review direct deposit information.')
		fnCmdKey('Special Hrs',8,0,0,'Review miscellaneous breakdown of hours.')
		fnCmdKey('&Check History',10,0,0,'Review check information.')
		! fnCmdKey('&Picture',6,0,0,'Place any picture in share\images.')
		if ~employeeAdding then
			fnCmdKey('De&lete Employee',4,0,0,'Deletes this record')
		end if
		fnCmdKey('&Save',1,1,0,'Saves all changes.')
		fnCmdKey('&Cancel',5,0,1,'Stops without applying any changes.')
		! /r
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto EmployeeEditXit
		! r: get local from mat resp$
		eno             =val(resp$(resp_eno            )(1:8))
		em$(1)          =    resp$(resp_name           )       ! name
		em$(2)          =    resp$(resp_addr           )
		em$(3)          =    resp$(resp_csz            )
		ss$             =    resp$(resp_ssn            )
		rs(1)           =val(resp$(resp_race           )(1:1))
		rs(2)           =val(resp$(resp_sex            )(1:1)) ! sex
		em(1)           =val(resp$(resp_married        )(1:1)) ! marital status
		em(2)           =val(resp$(resp_fedExepmtions  )(1:2)) ! fed ex
		em(3)           =val(resp$(resp_stExeptions    )(1:2)) ! state ex
		em(4)           =val(resp$(resp_empStatus      )(1:2)) ! emp status
		em(5)           =val(resp$(resp_payCode        )(1:2)) ! pay code
		em(6)           =val(resp$(resp_ficaCode       )(1:2)) ! fica code
		! em(7)           =val(resp$(resp_EicCode        )(1:2)) ! eic code
		em(8)           =val(resp$(resp_sickPay        )(1:5)) ! sick pay
		em(9)           =val(resp$(resp_vacationPay    )     ) ! vacation Pay code
		em(10)          =val(resp$(resp_sickAccrued    )     ) ! sick accrued
		em(11)          =val(resp$(resp_vacationAccrued)     ) ! vac accrued
		em(12)          =val(resp$(resp_stdFed         )     ) ! std fed
		em(13)          =val(resp$(resp_fedAddOn       )     ) ! fed addon
		em(14)          =val(resp$(resp_stdState       )     ) ! std state
		em(15)          =val(resp$(resp_StateAddOn     )     ) ! state addon
		em(16)          =val(resp$(resp_hireDate       )     ) ! date hired
		lpd             =val(resp$(resp_lastPayrollDate)     ) ! last payroll date
		bd              =val(resp$(resp_birthDate      )     ) ! birth date
		ph$             =    resp$(resp_phone          )       ! phone
		w4Year$         =    resp$(resp_w4year          )
		if resp$(resp_w4Step2)='True' then w4Step2=1 else w4Step2=0
		w4Step3        =val(resp$(resp_w4Step3         )     )
		w4Step4a       =val(resp$(resp_w4Step4a        )     )
		w4Step4b       =val(resp$(resp_w4Step4b        )     )
		w4Step4c       =val(resp$(resp_w4Step4c        )     )
		if resp$(resp_disableStTax)='True' then disableStTax=-1 else disableStTax=0
		if resp$(resp_disableFedTax)='True' then disableFedTax=-1 else disableFedTax=0
		if disableFedTax then em(12)=-1
		if disableStTax then em(14)=-1
		! /r
		if ckey=8 then
			fnHours(eno)
			goto ScrEmployee
		else if ckey=7 then
			gosub DD
			goto EmployeeEdit
		else if ckey=10 then
			fnCheckHistory(hact$:=str$(eno),hCheckIdx3,hCheckIdx1,hEmployee)
			goto EmployeeEdit
		else if ckey=1 then ! or ckey=2
			if em(5)=0 then ! pay code not selected
				mat ml$(1) : ml$(1)='Pay Code is required.'
				fnMsgBox(mat ml$,resp$)
				goto ScrEmployee
			end if
			gosub EmployeeSave
			goto EmployeeEditXit
		else if ckey=4 then
			goto EmployeeDelete
		else if ckey=>5200 and ckey<=ckey_high then
			if employeeAdding then gosub EmployeeSave
			goto Nav
		end if
	goto ScrEmployee ! /r
	EmployeeEditXit: !
fnend


ScrDepartment: ! r:
	fnTos ! r:
	respc=0
	mylen=21 : mypos=mylen+2 : mat resp$=('')
	dim departmentCap$*128
	lc=0
	if ~deptNew then
		screen=scrDept(whichDepartment)
	end if
	fn_navButtons(lc,deptCount)
	dim tdet(23)
	dim tcd(3)
	dim tdt(4)
	read #hDepartment,using Fdept,rec=empDeptRec(whichDepartment): teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet
	if departmentAddMode then
		departmentCap$='Adding Department '&str$(deptCount+1)&' for '&trim$(em$(1))
		departmentAddMode=0
	else
		departmentCap$='Department '&str$(whichDepartment)&' of '&str$(deptCount)&' for '&trim$(em$(1))
	end if
	! lc=3
	! fnLbl(1,50,'whichDepartment='&str$(whichDepartment)&' scree='&str$(screen))
	fram1=1 : fnFra(3,1,6,97,departmentCap$)
	fnLbl(1,1,'Employee Number:',mylen,1,0,fram1)
	fnTxt(1,mylen+3,8,8,1,'1030',1,'Employee numbers must be numeric.',fram1)
	resp$(respc+=1)=str$(eno)
	fnLbl(2,1,'Department Number:',mylen,1,0,fram1)
	fnTxt(2,mylen+3,3,3,1,'30',0,'Department numbers must be numeric and no department number can be used twice on the same employee.',fram1)
	resp$(respc+=1)=str$(tdn)
	fnLbl(2,35,'General Ledger Acct:',mylen,1,0,fram1)
	fnQgl(2,58,fram1)
	resp$(respc+=1)=fnrgl$(gl$)
	fnLbl(3,1,'Last Review Date:',mylen,1,0,fram1)
	fnTxt(3,mylen+3,8,8,1,'1',0,'Last review is only used for information purposes.  Use MMDDYY format.',fram1)
	resp$(respc+=1)=str$(tdt(1))
	fnLbl(3,35,'Next Review Date:',mylen,1,0,fram1)
	fnTxt(3,58,8,8,1,'1',0,'Next review date is only used for information purposes.  Use MMDDYY format.',fram1)
	resp$(respc+=1)=str$(tdt(2))
	fnLbl(4,1,'Last Increase Date:',mylen,1,0,fram1)
	fnTxt(4,mylen+3,8,8,1,'1',0,'Last increase date is only used for information purposes.  Use MMDDYY format.',fram1)
	resp$(respc+=1)=str$(tdt(3))
	fnLbl(4,35,'Last Increase Amt:',mylen,1,0,fram1)
	fnTxt(4,58,12,12,1,'10',0,'Just a method of storing the amount of the last pay increase.  You must enter by hand.',fram1)
	resp$(respc+=1)=str$(tli)
	fnLbl(5,1,'Last Payroll Date:',mylen,1,0,fram1)
	fnTxt(5,mylen+3,8,8,1,'1',0,'Last payroll date is updated each time pay is calculated.  Use MMDDYY format.  Do not change this date.',fram1)
	resp$(respc+=1)=str$(tdt(4))
	fnLbl(5,35,'State Code:',mylen,1,0,fram1)
	! fnTxt(5,58,2,2,1,'30',0,'You must enter a state code, even if you have no state withholdings.',FRAM1)
	fnComboA('StateCode',5,58,mat state_option$,'',11,fram1)
	if tcd(1)=0 or tcd(1)>10 then tcd(1)=1 ! default state code to 1
	resp$(respc+=1)=state_option$(tcd(1))
	fnLbl(6,1,'Workmans Code:',mylen,1,0,fram1)
	fnTxt(6,mylen+3,2,2,1,'30',0,'You workmans comp code is used for grouping certain types of work on the workmans comp report.',fram1)
	resp$(respc+=1)=str$(tcd(2))
	fnLbl(6,35,'Union Code:',mylen,1,0,fram1)
	fnTxt(6,58,2,2,1,'30',0,'You union code is used for grouping employees for the union report.',fram1)
	resp$(respc+=1)=str$(tcd(3))
	! fram1=1 : fnFra(3,1,6,97,departmentCap$)
	fram2=2 : fnFra(12,1,3,97,'Salary and Pay Rates')

	fnLbl(1,1,'Salary:',mylen,1,0,fram2)
	fnTxt(1,mylen+3,12,12,1,'10',0,'Enter the salary for the pay period.',fram2)
	resp$(respc+=1)=str$(tdet(1))
	fnLbl(2,1,'Regular Hourly Rate:',mylen,1,0,fram2)
	fnTxt(2,mylen+3,12,12,1,'10',0,'Enter the regular hourly rate.',fram2)
	resp$(respc+=1)=str$(tdet(2))
	fnLbl(2,35,'O/T Hourly Rate:',mylen,1,0,fram2)
	fnTxt(2,58,12,12,1,'10',0,'Enter the overtime hourly rate.',fram2)
	resp$(respc+=1)=str$(tdet(3))
	fram3=3 : fnFra(14+3,1,10,97,'Deductions and Additions')
	for j=1 to 10
		fnLbl(j,1,dednames$(j*2-1),mylen,1,0,fram3)
		fnTxt(j,mylen+3,12,12,1,'10',0,'Enter the standard amount or the percent.',fram3)
		resp$(respc+=1)=str$(tdet(j*2-1+3))
		fnLbl(j,35+2,dednames$(j*2),mylen,1,0,fram3)
		fnTxt(j,58+2,12,12,1,'10',0,'Enter the standard amount or the percent.',fram3)
		resp$(respc+=1)=str$(tdet(j*2+3))
	next j

	! fnCmdKey('&Add Department',4,0,0,'Add an additional department record.')
	fnCmdKey('Check &History',10,0,0,'Review check information.')
	fnCmdKey('&Delete Dept',9,0,0,'Deletes the department record.')
	fnCmdKey('&Save Dept',1,1,0,'Save changes and returns to Employee.')
	fnCmdKey('&Cancel',5,0,1,'Exit departmental record without saving changes.')
	ckey=fnAcs(mat resp$) ! /r
	if ckey=5 then goto EmployeeEditXit
	teno=val(resp$(1)) ! employee # in dept record
	tdn=val(resp$(2)) ! department #
	if ckey=9 then
		delete #hDepartment,key=cnvrt$('pic(zzzzzzz#)',eno)&cnvrt$('pic(zz#)',tdn):
		goto EmployeeEdit
	end if
	if resp$(3)='combos' then resp$(3)=''
	gl$=fnagl$(resp$(3))
	tdt(1)=val(resp$(4)) ! last review date
	tdt(2)=val(resp$(5)) ! next review date
	tdt(3)=val(resp$(6)) ! last increase date
	tli=val(resp$(7)) ! last increase amount
	tdt(4)=val(resp$(8)) ! last payroll date
	for j=1 to 10
		if uprc$(trim$(resp$(9)(1:2)))=uprc$(trim$(state_option$(j)(1:2))) then tcd(1)=j ! state code
	next j
	tcd(2)=val(resp$(10)) ! w/c code
	tcd(3)=val(resp$(11)) ! union code
	tdet(1)=val(resp$(12)) ! salary
	tdet(2)=val(resp$(13)) ! hourly rate
	tdet(3)=val(resp$(14)) ! overtime rate
	for j=4 to 23
		tdet(j)=val(resp$(14+j-3)) ! standard deductions
	next j

	if tdn=0 then
		mat ml$(2)
		ml$(1)='The department number can not be 0 (zero).'
		ml$(2)='Enter a valid department number!'
		fnMsgBox(mat ml$,resp$)
		goto ScrDepartment
	end if
	if eno<>ent then goto EmployeeChangeKey
	rewrite #hDepartment,using Fdept,rec=	empDeptRec(whichDepartment)	: teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet
	! if ckey=4 then ! add new department
	! 	goto DepartmentAdd
	! else
	if ckey=1 then
		goto EmployeeEdit
	else if ckey=10 then
		fnCheckHistory(hact$:=str$(eno),hCheckIdx3,hCheckIdx1,hEmployee)
		goto EmployeeEdit
	else if ckey=>5200 and ckey<=ckey_high then
		goto Nav
	end if
goto EmployeeEditXit ! /r
def fn_departmentAdd(eno,&deptNew; ___,returnN)
	ScrDepartmentAdd: !
	fnTos
	respc=0 : frac=0
	mylen=25 : mypos=mylen+2
	fnLbl(1,1,'Department Number:',mylen,1)
	fnTxt(1,mylen+3,3,3,1,'30',0,'Department numbers must be numeric.')
	resp$(respc+=1)=''
	fnCmdKey('&Next',1,1,0,'Process department information.')
	fnCmdKey('&Cancel',5,0,1,'Returns to maintenance screen.')
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		deptNew=val(resp$(1))
		ent$=cnvrt$('n 8',eno)&cnvrt$('n 3',deptNew) ! lpad$(str$(ent),8)
		read #hDepartment,using Fdept,key=ent$: tempEno,tempDept nokey DoDepartmentAdd
		! r: only happens if not nokey on department above
			mat ml$(2)
			ml$(1)='This department number is already attached to this employee.'
			ml$(2)='Select a different department number.'
			fnMsgBox(mat ml$,resp$,'',48)
			goto ScrDepartmentAdd
		DoDepartmentAdd: ! /r
		departmentAddMode=1
		tdn=deptNew
		gl$=''
		mat tdt=(0)
		mat tcd=(0)
		tli=0
		mat tdet=(0)
		write #hDepartment,using Fdept:eno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet
		returnN=1
	end if
	fn_departmentAdd=returnN
fnend
ScrState: ! r:
	screen=scrState
	dim ids$(0)*64
	mat ids$(0)
	dim fieldLen(0)
	mat fieldLen(0)   ! default is 5
	dim fieldMask$(0)*64
	mat fieldMask$(0) ! default is '30'
	if fnpayroll_client_state$='AR' then
		fnAddOneC(mat ids$,'AR4EC Exemptions')
		ckey=fn_askMat(eno,mat ids$)
	else if fnpayroll_client_state$='IL' then
		fnAddOneC(mat ids$,'IL W-4 Line 1 Allowances')
		fnAddOneC(mat ids$,'IL W-4 Line 2 Allowances')
		ckey=fn_askMat(eno,mat ids$)
	else if fnpayroll_client_state$='OR' then
		fnAddOneC(mat ids$,'OR W-4 Line 1 Select One')         	: fnAddOneN(mat fieldLen,52) : fnAddOneC(mat fieldMask$,'')
			dim selectOneOption$(0)*64
			str2mat(' _
				0 Single^ _
				1 Married^ _
				2 Married, but withholding at the higher single rate _
				',mat selectOneOption$,'^' _
			)
		! use State Exemptions instead    	fnAddOneC(mat ids$,'OR W-4 Line 2 Allowances')         	: fnAddOneN(mat fieldLen,14) : fnAddOneC(mat fieldMask$,'32')
		! use State Tax Add-On instead    	fnAddOneC(mat ids$,'OR W-4 Line 3 Additional amount') 	: fnAddOneN(mat fieldLen,10) : fnAddOneC(mat fieldMask$,'32')
		! use disable State Taxes instead 	fnAddOneC(mat ids$,'OR W-4 Line 4a Exemption Code')   	: fnAddOneN(mat fieldLen,30) : fnAddOneC(mat fieldMask$,'')
		! use disable State Taxes instead 	fnAddOneC(mat ids$,'OR W-4 Line 4b Exempt')            	: fnAddOneN(mat fieldLen, 6) : fnAddOneC(mat fieldMask$,'')

		ckey=fn_askMat(eno,mat ids$,mat fieldLen,mat fieldMask$,mat selectOneOption$)
	else if fnpayroll_client_state$='LA' then !  R-1300 (L-4)
		fnAddOneC(mat ids$,'R-1300 Exepmtions')
		fnAddOneC(mat ids$,'R-1300 Dependencies')
		ckey=fn_askMat(eno,mat ids$)
	else if fnpayroll_client_state$='GA' then !  R-1300 (L-4)
		fnAddOneC(mat ids$,'Exepmtions')
		fnAddOneC(mat ids$,'Dependents')
		ckey=fn_askMat(eno,mat ids$)
	else
		pr 'state not yet setup yet'
		pause
	end if
	if ckey=>5200 and ckey<=ckey_high then goto Nav
goto ScrEmployee ! /r
def fn_askMat(eno,mat id$; mat fieldLen,mat fieldMask$,mat askmat1option$, _
	___,lc,respc,tmp,txtLen,col1_len,col2_pos,txtMask$*32) ! also references em(1)
	fnTos
	for tmp=1 to udim(mat id$) : col1_len=max(len(id$(tmp))+1,16) : next tmp
	if fnpayroll_client_state$='OR' then col1_len+=9
	col2_pos=1+col1_len+2
	fn_navButtons(lc,deptCount)
	lc+=1
	fnLbl(lc+=1,1,'Employee Number:',col1_len,1)
	fnTxt(lc,col2_pos,8,8,1,'1030',1,'Employee numbers must be numeric.')
	resp$(respc+=1)=str$(eno)
	lc+=1
	for tmp=1 to udim(mat id$)
		fnLbl(lc+=1,1,id$(tmp)&':',col1_len,1)
		txtLen=5 : if tmp<=udim(mat fieldLen) and fieldLen(tmp)>0 then txtLen=fieldlen(tmp)
		txtMask$='30' : if tmp<=udim(mat fieldMask$) and fieldMask$(tmp)<>'' then txtMask$=fieldMask$(tmp)
		if tmp=1 and udim(mat askmat1option$) then
			fnComboA('select one',lc,col2_pos,mat askmat1option$)
			resp$(respc+=1)=fnEmployeeData$(eno,id$(tmp))
			if resp$(respc)='' and em(1)=0 then resp$(respc)=askmat1option$(1)
			if resp$(respc)='' and em(1)=1 then resp$(respc)=askmat1option$(2)
			if resp$(respc)='' and em(1)=2 then resp$(respc)=askmat1option$(3)
			if resp$(respc)='' and em(1)>2 then resp$(respc)=askmat1option$(2)
		else
			fnTxt(lc,col2_pos,txtLen, 0,1,txtMask$,0,'')
			resp$(respc+=1)=fnEmployeeData$(eno,id$(tmp))
		end if
		lc+=1
	next tmp
	fnCmdKey('Complete',1,1,0,'Saves any changes and returns to Employee screen.')
	fnCmdKey('Cancel'  ,5,0,1,'Exit record without saving changes.')
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		respc=1
		for tmp=1 to udim(mat id$)
			fnEmployeeData$(eno,id$(tmp),resp$(respc+=1))
		nex tmp
		! handled after function ! if ckey=>5200 and ckey<=ckey_high then goto Nav
	end if
	ScrAskMatXit: !
	fn_askMat=ckey
fnend
def fn_navButtons(&lc,&deptCount; ___,deptItem)
	! many other locals too
	! uses local deptNew, screen, mat scrDept, eno, etc
	! returns local mat empDept,mat empDeptRec, etc
	deptCount=fn_EmployeeDepartments(eno,mat empDept,mat empDeptRec)

	mat scrDept(deptCount)
	for deptItem=1 to deptCount
		scrDept(deptItem)=(deptItem+100)
	nex deptItem

	if deptNew then
		whichDepartment=srch(mat empDept,deptNew)
		screen=scrDept(whichDepartment)
		deptNew=0
	end if

	lc+=1
	fnButtonOrDisabled(screen<>scrEmployee,lc,1,'Employee',ckey_scrEmployee)
	if fnpayroll_client_state$='AR' then
		fnButtonOrDisabled(screen<>scrState,lc,11,'AR4EC',ckey_ScrState,'',10)
	else if fnpayroll_client_state$='IL' then
		fnButtonOrDisabled(screen<>scrState,lc,11,'IL W-4',ckey_ScrState,'',10)
	else if fnpayroll_client_state$='LA' then
		fnButtonOrDisabled(screen<>scrState,lc,11,'R-1300 (L-4)',ckey_ScrState,'',12)
	else if fnpayroll_client_state$='GA' then
		fnButtonOrDisabled(screen<>scrState,lc,11,'Withholding Exemptions',ckey_ScrState,'',22)
	else if fnpayroll_client_state$='OR' then
		fnButtonOrDisabled(screen<>scrState,lc,11,'OR W-4',ckey_ScrState,'',10)
	end if
	lc+=1
	dptPos=1
	for deptItem=1 to deptCount
		dim deptTxt$*128
		deptTxt$='Dept '&str$(empDept(deptItem)) ! &' '&fnDeptName$(empDept(deptItem))
		! deptTxt$&=' [F'&str$(5201+deptItem)&']'
		dim deptToolTip$*256
		deptToolTip$=fnDeptName$(empDept(deptItem))&'\n(Department '&str$(empDept(deptItem))&')'
		fnButtonOrDisabled(screen<>scrDept(deptItem),lc,dptPos,deptTxt$,5201+deptItem, deptToolTip$)
		dptPos+=(len(deptTxt$)+2)
	nex deptItem
	fnbutton(lc,dptPos,'Add Department',ckey_DepartmentAdd, 'Add a new department')

	! fnlbl(36,10
	fnlbl(28,105,' ') ! invisible space to keep a minimum size, otherwise tab buttons jump while you're using them
fnend
Nav: ! r:
	if ckey=ckey_scrEmployee then
		goto ScrEmployee
	else if ckey=ckey_ScrState then
		goto ScrState
	else if ckey=>5201 and ckey<=5201+deptCount then
		whichDepartment=ckey-5201
		goto ScrDepartment
	else if ckey=ckey_DepartmentAdd then
		if fn_departmentAdd(eno,deptNew) then
			deptCount=fn_EmployeeDepartments(eno,mat empDept,mat empDeptRec)
			ckey=5202
			goto Nav
		else
			goto ScrEmployee
		end if
	else
		pr 'unknown ckey=';ckey
		pause
		goto xit
	end if
! /r
EmployeeSave: ! r:
	if add1=1 then
		write #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c
		add1=0
	else if holdeno<>eno then
		goto EmployeeChangeKey
	else
		rewrite #hEmployee,using F_employee,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c
	end if
return ! /r
EmployeeDelete: ! r:
	mat ml$(2)
	ml$(1)='Employee Number '&ltrm$(ent$)&' will be Deleted.'
	ml$(2)='Do you wish to continue?'
	fnMsgBox(mat ml$,resp$,'',52)
	if resp$='Yes' then ! delete direct deposit
		fn_dDdelete(eno)
		delete #hEmployee,key=ent$:
		heno$=cnvrt$('pic(zzzzzzz#)',eno)
		fnKeyDelete(hDepartment,'form pos 1,n 8',heno$)
		fnKeyDelete(hCheckIdx1,'form pos 1,n 8',heno$)

		dim ed$(0)*128
		dim edN(0)
		hEmpData=fn_openFio('PR Employee Data',mat ed$,mat edN)
		fnKeyDelete(hEmpData,'form pos 1,n 8',heno$)
		fnCloseFile(hEmpData,'PR Employee Data')
	else
		goto EmployeeDeleteFinis
	end if
	EmployeeDeleteFinis: !
goto EmployeeEditXit ! /r
EmployeeChangeKey: ! r:
	mat ml$(3)
	ml$(1)='You have chosen to change the employee number'
	ml$(2)='from '&str$(holdeno)&' to '&str$(eno)&'.'
	ml$(3)='Do you wish to continue?'
	fnMsgBox(mat ml$,resp$,'',52)
	if resp$<>'Yes' then
		goto CHGENO_XIT
	end if
	read #hEmployee,using 'form pos 1,n 8',key=lpad$(str$(eno),8): teno nokey ChangeEmpNo
	mat ml$(2)
	ml$(1)='Employee Number '&ltrm$(ent$)&' already exists.'
	ml$(2)='You cannot change to this number.'
	fnMsgBox(mat ml$,resp$)
	goto CHGENO_XIT
	ChangeEmpNo: !
	fn_DirectDepositKeyChange(rpad$(trim$(ent$),10),key$:=fn_dDkey$(eno))
	heno$=lpad$(str$(holdeno),8)
	fnKeyChange(hDepartment,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
	fnKeyChange(hCheckIdx1,'form pos 1,n 8',heno$,lpad$(str$(eno),8)) ! change employee number in check history

	hEmpData=fn_openFio('PR Employee Data',mat ed$,mat edN)
	fnKeyChange(hEmpData,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
	fnCloseFile(hEmpData,'PR Employee Data')


	! r: change employee number in any and all timesheet files.
	dim filename$(0)*256
	dim kfname$*256
	fnGetDir2('[Q]\PRmstr\',mat filename$, '','timesheet*.h[cno]')
	for fileItem=1 to udim(mat filename$)
		if pos(lwrc$(filename$(fileItem)),'idx.')<=0 and pos(lwrc$(filename$(fileItem)),'idx2.')<=0 then
			kfname$=srep$(filename$(fileItem),'.','Idx.')
			open #h_timesheet=fnH: 'Name=[Q]\PRmstr\'&filename$(fileItem)&',KFName=[Q]\PRmstr\'&kfname$&',shr',i,outIn,k ioerr TimesheetOpenErr
			fnKeyChange(h_timesheet,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
			close #h_timesheet:
		end if
		TimesheetOpenErr: !
	nex fileItem
	! /r
	! change main employee record
	delete #hEmployee,key=ent$:
	write #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c
	ent$=lpad$(str$(eno),8)
	hact$=ent$
	CHGENO_XIT: !
goto EmployeeEditXit ! /r
def fn_openFiles
	open #hEmployee=fnH: 'name=[Q]\PRmstr\Employee.h[cno],version=1,kfName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,outIn,k
	F_employee: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,n 1,c 4,x 1,c 12,n 6,4*n 12.2
	open #hEmployeeIdx2=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr',i,outIn,k
	open #hCheckIdx1=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr',i,outIn,k
	open #hCheckIdx3=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr',i,outIn,k
	open #hDepartment=fnH: 'Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr',i,outIn,k
		Fdept: form pos 1,n 8,n 3,c 12,4*n 6,3*n 2,pd 4.2,23*pd 4.2
		!  pos 1   n  8   teno
		!          n  3   tdn
		!          c 12   gl$
		!        4*n  6   mat tdt(1-4)
		!        3*n  2   mat tcd(1-3)
		!          pd 4.2 tli
		!       23*pd 4.2 mat tdet(1-23)
fnend
def fn_closeFiles
	close #hEmployee: ioerr ignore
	close #hEmployeeIdx2: ioerr ignore
	close #hCheckIdx1: ioerr ignore
	close #hCheckIdx3: ioerr ignore
	close #hDepartment: ioerr ignore
	fnEmployeeDataClose
	close #hEdcDept: ioerr ignore
	hEdcDept=0
fnend
def fn_EmployeeDepartments(eno,mat empDept,mat empDeptRec; ___,teno,deptNumber)
	if ~hEdcDept then
		open #hEdcDept=fnH: 'Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr',i,i,k
	end if
	mat empDept(0)
	mat empDeptRec(0)
	restore #hEdcDept,key>=cnvrt$('pic(zzzzzzz#)',eno)&'   ': nokey EdcFinis
	do
		read #hEdcDept,using 'form pos 1,N 8,N 3': teno,deptNumber eof EdcFinis
		if teno=eno then
			fnAddOneN(mat empDept,deptNumber)
			fnAddOneN(mat empDeptRec,rec(hEdcDept))
		end if
	loop while teno=eno
	EdcFinis: !
	fn_EmployeeDepartments=udim(mat empDept)
fnend
def fn_setup
	autoLibrary
	on error goto Ertn

	dim resp$(50)*128
	dim ml$(0)*128

	dim race_option$(7)*15
	race_option$(1)='0 - Unknown'
	race_option$(2)='1 - Caucasian'
	race_option$(3)='2 - Hispanic'
	race_option$(4)='3 - Black'
	race_option$(5)='4 - Oriental'
	race_option$(6)='5 - AmIndian'
	race_option$(7)='6 - Indochines'

	dim gender_option$(3)*11
	gender_option$(1)='0 - Unknown'
	gender_option$(2)='1 - Male'
	gender_option$(3)='2 - Female'

	dim fed_exemption_option$(22)
	for j=1 to 21
		fed_exemption_option$(j)=str$(j-1)
	next j
	fed_exemption_option$(22)='99'

	dim code6$(4)*28
	code6$(1)='0 - Subject to SS and Med WH'
	code6$(2)='1 - SS only'
	code6$(3)='2 - Medicare Only'
	code6$(4)='9 - Neither SS nor Medicare'

	dim statenames$(10)*8
	open #1: 'Name=[Q]\PRmstr\Company.h[cno]',i,outi,r
	read #1,using 'form pos 150,10*c 8',rec=1: mat statenames$
	close #1:
	dim state_option$(10)*11
	for j=1 to 10: state_option$(j)=cnvrt$('Pic(z#)',j)&' '&statenames$(j): next j

	dim dednames$(20)*21
	fnDedNames(mat dednames$)
	for j=1 to 20
		if trim$(dednames$(j))<>'' then
			dednames$(j)=trim$(dednames$(j))&':'
		end if
	next j
	scrState=1        	: ckey_ScrState=5200
	scrEmployee=2     	: ckey_scrEmployee=5201
	scrDeptAdd=3      	: ckey_DepartmentAdd=5301
	ckey_high=ckey_DepartmentAdd ! should be the highest of these universal ckeys
	dim marriedOption$(0)*58
	dim w4yearOption$(0)*4
	dim payPeriodOption$(0)*16
	fn_getEmpOptions(mat marriedOption$,mat w4yearOption$,mat payPeriodOption$)

fnend
def library fnGetEmpOptions(mat marriedOption$,mat w4yearOption$,mat payPeriodOption$)
	if ~setup then fn_setup
	fnGetEmpOptions=fn_getEmpOptions(mat marriedOption$,mat w4yearOption$,mat payPeriodOption$)
fnend
def fn_getEmpOptions(mat marriedOption$,mat w4yearOption$,mat payPeriodOption$)
	mat marriedOption$(0)
	fnAddOneC(mat marriedOption$,'0 - Single')
	fnAddOneC(mat marriedOption$,'1 - Married - filing jointly')
	fnAddOneC(mat marriedOption$,'2 - Single - Head of Household')
	fnAddOneC(mat marriedOption$,'3 - Married - filing joint - only one working')
	fnAddOneC(mat marriedOption$,'4 - Married - filing joint - both working')
	fnAddOneC(mat marriedOption$,'5 - Married - filing seperate - both working')

	! mat eicOption$(0)
	! fnAddOneC(mat eicOption$,'0 - Not qualified for EIC'    )  !  em(7)=1
	! fnAddOneC(mat eicOption$,'1 - Single or Spouse not file')  !  em(7)=2
	! fnAddOneC(mat eicOption$,'2 - Married both filing'      )  !  em(7)=3

	mat w4yearOption$(0)
	fnAddOneC(mat w4yearOption$,'2019')
	fnAddOneC(mat w4yearOption$,'2020')
	fnAddOneC(mat w4yearOption$,'none')


	mat payPeriodOption$(0)
	fnAddOneC(mat payPeriodOption$,'1 - Monthly'     )
	fnAddOneC(mat payPeriodOption$,'2 - Semi-monthly')
	fnAddOneC(mat payPeriodOption$,'3 - Bi-weekly'   )
	fnAddOneC(mat payPeriodOption$,'4 - Weekly'      )
	fnAddOneC(mat payPeriodOption$,'5 - Annually'    ) ! added 3/26/20, for testing state tax withholdings

fnend

Finis: ! ! r:
	fn_closeFiles
goto Xit ! /r
Xit: fnXit

DD: ! r:
	key$=fn_dDkey$(eno)
	hDd=fn_dDopen
	read #hDd,using 'form pos 11,C 1,N 9,N 2,C 17',key=key$: dd$,rtn,acc,acn$ nokey DdReadNoKey
AskDd: !
	if ~setup_askdd then
		setup_askdd=1
		dim optDirectDepositAccountType$(2)*22
		optDirectDepositAccountType$(1)='27 = Regular Checking'
		optDirectDepositAccountType$(2)='37 = Savings Account'
		dim optEnableDirectDeposit$(2)*33
		optEnableDirectDeposit$(1)='Y = Activate Direct Deposit'
		optEnableDirectDeposit$(2)='N = Direct Deposit not activated.'
	end if

	fnTos
	respc=0: mylen=35 : right=1
	fnLbl(1,1,'Employee Number:',mylen,right)
	fnTxt(1,mylen+3,8,8,1,'',1,'')
	resp$(respc+=1)=str$(eno)
	fnLbl(2,1,'Direct Deposit:',mylen,right)
	fnComboA('Directd',2,mylen+3,mat optEnableDirectDeposit$,'',35)
	respc+=1
	for j=1 to udim(optEnableDirectDeposit$)
		if dd$=optEnableDirectDeposit$(j)(1:1) then resp$(respc)=optEnableDirectDeposit$(j)
	next j
	fnLbl(3,1,'Employee Bank Routing Number:',mylen,right)
	fnTxt(3,mylen+3,9,9,1,'',0,'Employee''s bank''s routing #. The bank account and the routing # can be found at the bottom of the employees personal check.')
	resp$(respc+=1)=str$(rtn)
	fnLbl(4,1,'Account Type:',mylen,right)
	fnComboA('AccType',4,mylen+3,mat optDirectDepositAccountType$,'',35)
	respc+=1
	resp$(respc)=''
	for j=1 to udim(mat optDirectDepositAccountType$)
		if acc=val(optDirectDepositAccountType$(j)(1:2)) then resp$(respc)=optDirectDepositAccountType$(j)
	next j
	if resp$(respc)='' then resp$(respc)=optDirectDepositAccountType$(1)
	fnLbl(5,1,'Employee Bank Account Number:',mylen,right)
	! fnTxt(5,mylen+3,17,17,1,'30',0,'Enter the employee''s bank account number. ')
	fnTxt(5,mylen+3,17,17,1,'30',0,'Enter the employee''s bank account number. ')
	resp$(respc+=1)=acn$
	fnCmdKey('&Save',1,1,0,'Saves the information on the screen.' )
	fnCmdKey('&Delete',4,0,0,'Deletes the direct deposit information on this employee.You can stop direct deposits simply by changing the direct deposit question to no.')
	fnCmdKey('&Cancel',5,0,1,'Cancels without recording any chnages to the screen.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto DdFinis
	key$ =    resp$(1)
	dd$  =    resp$(2)(1:1)
	rtn  =val(resp$(3)     ) !  banks routing #
	acc  =val(resp$(4)(1:2)) ! checking or savings
	acn$ =    resp$(5)       ! employee bank acct #
	if ckey=4 then
		dd$='N'
		rtn=acc=0 : acn$=''
		key$=rpad$(key$,10)
		delete #hDd,key=key$: nokey ignore
		goto DdFinis
	end if
	if dd$='Y' and (rtn=0 or acc=0 or acn$='') then
		mat ml$(2)
		ml$(1)='You must have valid answers in the routing #, account'
		ml$(2)='type, and bank account before you can answer yes.'
		fnMsgBox(mat ml$,resp$)
		goto AskDd
	else if ckey=1 or ckey=4 then
		key$=rpad$(str$(eno),10)
		rewrite #hDd,using 'form pos 11,C 1,N 9,N 2,C 17',key=key$: dd$,rtn,acc,acn$
		goto DdFinis
	end if
goto AskDd ! /r
DdReadNoKey: ! r:
	dd$='N' : rtn=acc=0 : acn$='' ! defaults
	write #hDd,using 'form pos 1,C 10,C 1,N 9,N 2,C 17': key$,dd$,rtn,acc,acn$ nokey DdReadNoKey
goto AskDd ! /r
DdFinis: ! r:
	fn_dDclose
return  ! /r
def fn_dDkey$*10(eno)
	fn_dDkey$=rpad$(str$(eno),10)
fnend
def fn_dDopen
	open #hDd=fnH: 'Name=[Q]\PRmstr\dd.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],kps=1,kln=10,Use',i,outIn,k
	fn_dDopen=hDd
fnend
def fn_dDclose
	close #hDd:
	hDd=0
fnend
def fn_dDdelete(eno)
	! delete the record
	hDd=fn_dDopen
	fnKeyDelete(hDd,'form pos 1,N 8',cnvrt$('pic(zzzzzzz#)',eno))
	fn_dDclose
fnend
def fn_DirectDepositKeyChange(from$,to$)
	hDd=fn_dDopen
	fnKeyChange(hDd,'form pos 1,C 10',from$,to$)
	fn_dDclose
fnend
include: ertn
include: fn_open
