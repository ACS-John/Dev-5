enableEasyDeptNavigation=0 ! if env$('acsDeveloper')<>'' then enableEasyDeptNavigation=1

! formerly S:\acsPR\newprFM
! Payroll Employee File
! r: setup and open files
fn_setup
fnTop(program$)
fn_openFiles
goto Menu1 ! /r


Menu1: !
goto AskEmployee
AskEmployee: ! r:
	departmentAddMode=0  !  this used to be in Menu1, but I moved it here because it shouldn't matter and I'd like to remove the Menu1 in favor of AskEmployee
	ad1=0 ! add code - used to tell other parts of the program, that I am currently adding an employee record.
	fnTos
	respc=0
	fnLbl(1,1,"Employee Number:",16,right)
	fncmbemp(1,18)
	if hact$="" then
		resp$(respc+=1)=""
	else
		resp$(respc+=1)=hact$
	end if
	fnCmdKey("&Add",1,0,0,"Add a new employee" )
	fnCmdKey("E&dit",2,1,0,"Access the highlighted record")
	fnCmdKey("&Next Sequential",3,0,0,"Access next record in employee # order")
	fnCmdKey("&Search",8,0,0,"Search for employee record")
	! fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new employee information")
	fnCmdKey("E&xit",6,0,1,"Returns to menu")
	fnAcs2(mat resp$,ckey) ! ask employee #
	hact$=resp$(1)(1:8)
	eno=ent=val(resp$(1)(1:8))
	if ckey=1 then
		ad1=1 ! ti1=1
		goto AddEmployee
	else if ckey=2 then
		goto EditEmployee
	else if ckey=3 then
		read #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c  eof EmpNotFound
		holdeno=eno
		ent$=lpad$(str$(eno),8)
		goto ScrEmployee
	else if ckey=4 then
		ent=eno
		goto EditEmployee
	else if ckey=8 then
		fnemployee_srch(x$,fixgrid)
		ent=val(x$)
		goto EditEmployee
	else if ckey=6 or env$('ExitNow')='yes' then ! Added ExitNow env$ by GSB to ensure program recursively exits when they click the Windows X in a Subwindow
		goto Finis
	else if ckey=7 then
		goto AskEmployee
	end if
goto AddEmployee ! /r
AddEmployee: ! r:
	fnTos
	respc=0 : frac=0
	mylen=25 : mypos=mylen+2
	fnLbl(1,1,"Employee Number:",mylen,1)
	fnTxt(1,mylen+3,8,8,1,"30",0,"Employee numbers must be numeric.")
	resp$(respc+=1)=str$(eno)
	fnCmdKey("&Next",1,1,0,"Process employee information.")
	fnCmdKey("&Cancel",5,0,1,"Returns to maintenance screem.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto AskEmployee
	add1=1
	ent=val(resp$(1))
	ent$=lpad$(str$(ent),8)
	read #hEmployee,using F_employee,key=ent$: tempeno nokey DoEmployeeAdd
	! r: only happens if employee number already exists
	mat ml$(2)
	ml$(1)="A record with this number already exists!"
	ml$(2)="Select a different employee number."
	fnmsgbox(mat ml$,resp$,'',48)
	goto AddEmployee ! /r
	DoEmployeeAdd: !
	mat em$=("")
	ph$=ss$=""
	bd=0
	mat rs=(0)
	mat em=(0)
	lpd=tgp=0
	mat ty=(0)
	mat tqm=(0)
	mat tcp=(0)
	mat sc=(0)
	eno=teno=holdeno=ent
	! holdem$=em$(1)
	dd$="N"
	! clear bank draft
	rtn=0
	acc=0
	acn$=''
	w4Step2        =0
	w4Step3        =0
	w4Step4a       =0
	w4Step4b       =0
	w4Step4c       =0
	disableStTax=0
	disableFedTax=0
	w4Year$='none'
	
	
	
goto ScrEmployee ! /r
EditEmployee: ! r:
	if ent=0 then goto AskEmployee
	teno=eno=ent ! hdar=0
	ent$=lpad$(str$(ent),8)
	read #hEmployee,using F_employee,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c nokey EmpNotFound
	holdeno=eno
goto ScrEmployee ! /r
EmpNotFound: ! r:
	mat ml$(2)
	ml$(1)="A record with this number does not exist!"
	ml$(2)="Select a different employee number."
	fnmsgbox(mat ml$,resp$,'',48)
goto AskEmployee ! /r




def fn_deptButtons(&lc,&deptCount; ___,deptItem)
	if ~setup_deptButtons then ! r:
		setup_deptButtons=1

	end if ! /r
	deptCount=fn_EmployeeDepartments(eno,mat empDept)
	if enableEasyDeptNavigation then
		fnbutton_or_disabled(screen<>scrEmployee,lc+=1,dptPos=5,'Employee',fkey_scrEmployee)  
		dptPos+=(8+2)
		dptPos+=2  ! extra spaces after employee
		for deptItem=1 to deptCount
			dim deptTxt$*128
			deptTxt$='- '&str$(empDept(deptItem))&' -' ! &' '&fnDeptName$(empDept(deptItem))
			dim deptToolTip$*256
			deptToolTip$='Department '&str$(empDept(deptItem))&' - '&fnDeptName$(empDept(deptItem))
			fnbutton_or_disabled(screen<>scrDept(deptItem),lc,dptPos,deptTxt$,fkey_scrDept(deptItem), deptToolTip$)
			dptPos+=(len(deptTxt$)+2)
		nex deptItem
	end if
fnend

ScrEmployee: ! r:
	fnTos : screen=scrEmployee
	respc=0 : frac=0 : mat lc=(0)
	mylen=28 : mypos=mylen+2
	col1_pos=1 : col1_len=mylen
	col2_pos=col1_pos+col1_len+2
	
	col3_pos=58 : col3_len=21 ! 20
	col4_pos=col3_pos+col3_len+2 ! 73
	fn_deptButtons(lc(1),deptCount)
	lc(1)+=1 : lc(2)=lc(1)
	fnLbl(lc(1)+=1,1,'Employee Number:',mylen,1)
	fnTxt(lc(1)   ,mylen+3,8,8,1,"30",0,"Employee numbers must be numeric.")
	resp$(resp_eno=respc+=1)=str$(eno)
	! r: col 1 top section
	lc(1)+=1
	fnLbl(lc(1)+=1,1,'Name:',mylen,1)
	fnTxt(lc(1)   ,mylen+3,col2_len,30,0,'',0,'Name can be entered first name first or last name first.')
	resp$(resp_name=respc+=1)=em$(1)

	fnLbl(lc(1)+=1,1,"Address:",mylen,1)
	fnTxt(lc(1)   ,mylen+3,col2_len,30,0,"",0,"")
	resp$(resp_addr=respc+=1)=em$(2)
	fnLbl(lc(1)+=1,1,"City, State Zip:",mylen,1)
	fnTxt(lc(1)   ,mylen+3,col2_len,30,0,"",0,"")
	resp$(resp_csz=respc+=1)=em$(3)
	lc(1)+=1

	fnLbl(lc(1)+=1,1,"Social Security Number:",mylen,1)
	fnTxt(lc(1)   ,mylen+3,11,11,0,"",0,"")
	resp$(resp_ssn=respc+=1)=ss$
	! /r
	! r: col 2 top section
	lc(2)=2
	
	fnLbl(lc(2)+=1,col3_pos,'Birth Date:',col3_len,1)
	fnTxt(lc(2)   ,col4_pos,10,10,0,"1",0,"The birth date is not required.")
	resp$(resp_birthDate=respc+=1)=str$(bd)
	
	fnLbl(lc(2)+=1,col3_pos,'Phone Number:',col3_len,1)
	fnTxt(lc(2)   ,col4_pos,12)
	resp$(resp_phone=respc+=1)=ph$

	fnLbl(          lc(2)+=1,col3_pos,"Race:",col3_len,1)
	fncomboa("Race",lc(2)   ,col4_pos,mat race_option$,"",16)
	resp$(resp_race=respc+=1)=fnSetForCombo$(mat race_option$,str$(rs(1)))

	fnLbl(         lc(2)+=1,col3_pos,'Sex:',col3_len,1)
	fncomboa('Sex',lc(2)   ,col4_pos,mat gender_option$,'',10)
	resp$(resp_sex=respc+=1)=fnSetForCombo$(mat gender_option$,str$(rs(2)))
	
	fnLbl(             lc(2)+=1,col3_pos,'Marital Status:',col3_len,1)
	fncomboa('Marital',lc(2)   ,col4_pos,mat marriedOption$,'',25) ! ,'',11)
	resp$(resp_married=respc+=1)=fnSetForCombo$(mat marriedOption$,str$(em(1)))

	! lc(2)+=1
	! fnLbl(             lc(2)+=1,col3_pos,'Dependants - Under 17:',col3_len,1)
	! fnTxt(             lc(2)   ,col4_pos,2,0,1,"30",0,"Employee numbers must be numeric.")
	! resp$(resp_dependentsMinor=respc+=1)=str$(dependentsMinor)
	! fnLbl(             lc(2)+=1,col3_pos,'Dependants - Other:',col3_len,1)
	! fnTxt(             lc(2)   ,col4_pos,2,0,1,"30",0,"Employee numbers must be numeric.")
	! resp$(resp_dependentsOther=respc+=1)=str$(dependentsOther)

	! /r
	! r: col 2 - state and federal section
	lcTmp=lc(2)+=2
	
	fnChk(lc(2)+=1,col4_pos+1,'Disable Federal Taxes',1) ! , align,contain,tabcon,chk_disable)
	resp_disableFedTax=respc+=1 : if em(12)=-1 then resp$(resp_disableFedTax)='True' else resp$(resp_disableFedTax)='False'
	
	fnLbl(             lc(2)+=1,col3_pos,"Federal Exemptions:",col3_len,1)
	fncomboa("FedEx",  lc(2)   ,col4_pos,mat fed_exemption_option$,"",3)
	resp$(resp_fedExepmtions=respc+=1)=fnSetForCombo$(mat fed_exemption_option$,str$(em(2)),1,2)
	fnLbl(lc(2)+=1,col3_pos,"Federal Tax Add-On:",col3_len,1)
	fnTxt(lc(2)   ,col4_pos,10,10,0,"32",0,"If you wish for the system to add additional Federal withholdings, enter that amount here.")
	resp$(resp_fedAddOn=respc+=1)=str$(em(13))
	fnLbl(lc(2)+=1,col3_pos,"Standard Federal W/H:",col3_len,1)
	fnTxt(lc(2)   ,col4_pos,10,10,0,"32",0,"If you wish for the system to withhold a fixed amount of Federal withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip Federal withholdings on this employee.")
	resp$(resp_stdFed=respc+=1)=str$(em(12))
	lc(2)+=1
	fnChk(lc(2)+=1,col4_pos+1,'Disable State Taxes',1) ! , align,contain,tabcon,chk_disable)
	resp_disableStTax=respc+=1 : if em(14)=-1 then resp$(resp_disableStTax)='True' else resp$(resp_disableStTax)='False'
	fnLbl(             lc(2)+=1,col3_pos,"State Exemptions:",col3_len,1)
	fncomboa("StateEx",lc(2)   ,col4_pos,mat fed_exemption_option$,"",3)
	resp$(resp_stExeptions=respc+=1)=fnSetForCombo$(mat fed_exemption_option$,str$(em(3)),1,2)
	fnLbl(lc(2)+=1,col3_pos,"State Tax Add-On:",col3_len,1)
	fnTxt(lc(2)   ,col4_pos,10,10,0,"32",0,"If you wish for the system to add additional state withholdings, enter that amount here.")
	resp$(resp_StateAddOn=respc+=1)=str$(em(15))
	fnLbl(lc(2)+=1,col3_pos,"Standard State W/H:",col3_len,1)
	fnTxt(lc(2)   ,col4_pos,10,10,0,"32",0,"If you wish for the system to withhold a fixed amount of State withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip state withholdings on this employee.")
	resp$(resp_stdState=respc+=1)=str$(em(14))
	! /r
	
	! lc(1)=lcTmp
	fnLbl(lc(1)+=1,col1_pos,"Date Hired:",col1_len,1)
	fnTxt(lc(1)   ,col2_pos,10,10,0,"1",0,"The date hired is only used for information purposes only.")
	resp$(resp_hireDate=respc+=1)=str$(em(16))
	fnLbl(lc(1)+=1,col1_pos,"Last Payroll Date:",col1_len,1)
	fnTxt(lc(1)   ,col2_pos,10,10,0,"1",0,"This will always be the last time pay was calculated on this employee.")
	resp$(resp_lastPayrollDate=respc+=1)=str$(lpd)
	! 
	fnLbl(               lc(1)+=1,1,"Employment Status:",mylen,1)
	fncombof("EmpStatus",lc(1)   ,col2_pos,20,"[Q]\PRmstr\EmpStatus.dat",1,2,3,15,"[Q]\PRmstr\EmpStatus.idx",0,0, " ",fracustinfo,0)
	resp$(resp_empStatus=respc+=1)=str$(em(4))
	fnLbl(              lc(1)+=1,col1_pos,"Pay Code:",col1_len,1)
	fncomboa("PayCode", lc(1)    ,col2_pos,mat payPeriodOption$,"",16)
	resp$(resp_payCode=respc+=1)=fnSetForCombo$(mat payPeriodOption$,str$(em(5)))
	lc(1)+=1
	fnLbl(lc(1)+=1,col1_pos,"Vacation Pay Code:",col1_len,1)
	fnTxt(lc(1)   ,col2_pos,6,6,0,"33",0,"Normally is number of vacation hours you want accrued each pay period.")
	resp$(resp_vacationPay=respc+=1)=str$(em(9))
	fnLbl(lc(1)+=1,col1_pos,"Vacation Hours Accrued:",col1_len,1)
	fnTxt(lc(1)   ,col2_pos,10,10,0,"32",0,"This should be the balance of vacation hours available at this time.")
	resp$(resp_vacationAccrued=respc+=1)=str$(em(11))
	
	
	lc(1)+=2
	fnLbl(lc(1)+=1,1,"Sick Pay Code:",col1_len,1)
	fnTxt(lc(1)   ,col2_pos,6,6,0,"33",0,"Normally is number of sick hours you want accrued each pay period.")
	resp$(resp_sickPay=respc+=1)=str$(em(8))
	fnLbl(lc(1)+=1,1,"Sick Hours Accrued:",col1_len,1)
	fnTxt(lc(1)   ,col2_pos,10,10,0,"32",0,"This should be the balance of sick hours available at this time.")
	resp$(resp_sickAccrued=respc+=1)=str$(em(10))
	
	! lc(2)-=2   !  go back up two lines and do the right side
	
	fnLbl(              lc(2)+=1,col3_pos,"FICA Code:",col3_len,1)
	fncomboa("FICACode",lc(2)    ,col4_pos,mat code6$,"",25)
	resp$(resp_ficaCode=respc+=1)=fnSetForCombo$(mat code6$,str$(em(6)))
	fnLbl(              lc(2)+=1,col3_pos,"EIC Code:",col3_len,1)
	fncomboa("EICCode", lc(2)    ,col4_pos,mat eicOption$,"",25)
	resp$(resp_EicCode=respc+=1)=eicOption$(em(7)+1)
	
	
	lc(1)+=1
	
	fnLbl(             lc(1)+=1,col1_pos,"W-4 Year:",col1_len,1)
	fncomboa("w4Year", lc(1)   ,col2_pos,mat w4yearOption$,'Only used if W-4 Year is set to 2020 or later.',5)
	resp$(resp_w4year=respc+=1)=w4Year$
	
	fnChk(lc(1)+=1,col2_pos+1,'2020 W-4 Step 2',1) ! , align,contain,tabcon,chk_disable)
	resp_w4Step2=respc+=1 : if w4Step2 then resp$(resp_w4Step2)='True' else resp$(resp_w4Step2)='False'
	
	fnLbl(             lc(1)+=1,col1_pos,"2020 W-4 Step 3:",col1_len,1)
	fnTxt(             lc(1)   ,col2_pos,10,10,0,"32",0,"Only used if W-4 Year is set to 2020 or later.")
	resp$(resp_w4Step3=respc+=1)=str$(w4Step3)
	fnLbl(             lc(1)+=1,col1_pos,"2020 W-4 Step 4a:",col1_len,1)
	fnTxt(             lc(1)   ,col2_pos,10,10,0,"32",0,"Only used if W-4 Year is set to 2020 or later.")
	resp$(resp_w4Step4a=respc+=1)=str$(w4Step4a)
	fnLbl(             lc(1)+=1,col1_pos,"2020 W-4 Step 4b:",col1_len,1)
	fnTxt(             lc(1)   ,col2_pos,10,10,0,"32",0,"Only used if W-4 Year is set to 2020 or later.")
	resp$(resp_w4Step4b=respc+=1)=str$(w4Step4b)
	fnLbl(             lc(1)+=1,col1_pos,"2020 W-4 Step 4c:",col1_len,1)
	fnTxt(             lc(1)   ,col2_pos,10,10,0,"32",0,"Only used if W-4 Year is set to 2020 or later.")
	resp$(resp_w4Step4c=respc+=1)=str$(w4Step4c)



	! picture=0
	fnCmdKey('&Departments ('&str$(deptCount)&')',2,0,0,"Review this employee's departmental information.")
	fnCmdKey("Direct D&eposit",7,0,0,"Review direct deposit information.")
	fnCmdKey("Re&view Special Hrs",8,0,0,"Review miscellaneous breakdown of hours.")
	fnCmdKey("&Review Checks",10,0,0,"Review check information.")
	! fnCmdKey("&Picture",6,0,0,"Place any picture in share\images.")
	if ad1=0 then
		fnCmdKey("De&lete",4,0,0,"Deletes this record")
	end if
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto AskEmployee
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
	em(7)           =val(resp$(resp_EicCode        )(1:2)) ! eic code
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
	! if ckey=6 then goto PICTURE
	if ckey=8 then
		fnhours(eno)
		goto ScrEmployee
	else if ckey=7 then
		gosub DD
		goto EditEmployee
	else if ckey=10 then
		fncheckfile(hact$:=str$(eno),hCheckIdx3,hCheckIdx1,hEmployee)
		goto EditEmployee
	else if ckey=1 or ckey=2 then
		if em(5)=0 then ! pay code not selected
			mat ml$(1) : ml$(1)='Pay Code is required.'
			fnmsgbox(mat ml$,resp$)
			goto ScrEmployee
		end if
		gosub SaveEmployee
		if ckey=2 then 
			goto FirstDepartment
		else 
			goto Menu1
		end if
	else if ckey=4 then
		goto DeleteEmployee
	end if
goto FirstDepartment ! /r
EndOfDepartments: !
goto ScrEmployee
FirstDepartment: ! r:
	firstread=1
	restore #hDepartment,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey DepartmentAdd
	whichDepartment=0
goto NextDepartment ! /r
NextDepartment: ! r:
	! POS 1   N  8   teno
	!         n  3   tdn
	!         c 12   gl$
	!       4*N  6   mat tdt(1-4)
	!       3*N  2   mat tcd(1-3)
	!         pd 4.2 tli
	!      23*PD 4.2 mat tdet(1-23)
	read #hDepartment,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof EndOfDepartments
	whichDepartment+=1
	if firstread=1 and teno<>eno then goto DepartmentAdd
	if teno<>eno then goto EndOfDepartments
ScrDepartment: !
	fnTos
	respc=0 : fram1=1
	mylen=20 : mypos=mylen+2 : mat resp$=("")
	dim departmentCap$*128
	departmentCount=fn_EmployeeDepartments(eno,mat empDept)
	if departmentAddMode then
		departmentCap$='Adding Department '&str$(departmentCount+1)&' for '&trim$(em$(1))
	else
		departmentCap$='Department '&str$(whichDepartment)&' of '&str$(departmentCount)&' for '&trim$(em$(1))
	end if
	fnFra(1,1,6,97,departmentCap$)
	fnLbl(1,1,"Employee Number:",mylen,1,0,fram1)
	fnTxt(1,mylen+3,8,8,1,"1030",1,"Employee numbers must be numeric.",fram1)
	resp$(respc+=1)=str$(eno)
	fnLbl(2,1,"Department Number:",mylen,1,0,fram1)
	fnTxt(2,mylen+3,3,3,1,"30",0,"Department numbers must be numeric and no department # can be used twice on the same employee.",fram1)
	resp$(respc+=1)=str$(tdn)
	fnLbl(2,35,"General Ledger #:",mylen,1,0,fram1)
	fnqgl(2,58,fram1)
	resp$(respc+=1)=fnrgl$(gl$)
	fnLbl(3,1,"Last Review Date:",mylen,1,0,fram1)
	fnTxt(3,mylen+3,8,8,1,"1",0,"Last review is only used for information purposes.  Use MMDDYY format.",fram1)
	resp$(respc+=1)=str$(tdt(1))
	fnLbl(3,35,"Next Review Date:",mylen,1,0,fram1)
	fnTxt(3,58,8,8,1,"1",0,"Next review date is only used for information purposes.  Use MMDDYY format.",fram1)
	resp$(respc+=1)=str$(tdt(2))
	fnLbl(4,1,"Last Increase Date:",mylen,1,0,fram1)
	fnTxt(4,mylen+3,8,8,1,"1",0,"Last increase date is only used for information purposes.  Use MMDDYY format.",fram1)
	resp$(respc+=1)=str$(tdt(3))
	fnLbl(4,35,"Last Increase Amt:",mylen,1,0,fram1)
	fnTxt(4,58,12,12,1,"10",0,"Just a method of storing the amount of the last pay increase.  You must enter by hand.",fram1)
	resp$(respc+=1)=str$(tli)
	fnLbl(5,1,"Last Payroll Date:",mylen,1,0,fram1)
	fnTxt(5,mylen+3,8,8,1,"1",0,"Last payroll date is updated each time pay is calculated.  Use MMDDYY format.  Do not change this date.",fram1)
	resp$(respc+=1)=str$(tdt(4))
	fnLbl(5,35,"State Code:",mylen,1,0,fram1)
	! fnTxt(5,58,2,2,1,"30",0,"You must enter a state code, even if you have no state withholdings.",FRAM1)
	fncomboa("StateCode",5,58,mat state_option$,"",11,fram1)
	if tcd(1)=0 or tcd(1)>10 then tcd(1)=1 ! default state code to 1
	resp$(respc+=1)=state_option$(tcd(1))
	fnLbl(6,1,"Workmans Code:",mylen,1,0,fram1)
	fnTxt(6,mylen+3,2,2,1,"30",0,"You workmans comp code is used for grouping certain types of work on the workmans comp report.",fram1)
	resp$(respc+=1)=str$(tcd(2))
	fnLbl(6,35,"Union Code:",mylen,1,0,fram1)
	fnTxt(6,58,2,2,1,"30",0,"You union code is used for grouping employees for the union report.",fram1)
	resp$(respc+=1)=str$(tcd(3))
	fram2=2: fnFra(9,1,3,97,"Salary and Pay Rates")
	fnLbl(1,1,"Salary:",mylen,1,0,fram2)
	fnTxt(1,mylen+3,12,12,1,"10",0,"Enter the salary for the pay period.",fram2)
	resp$(respc+=1)=str$(tdet(1))
	fnLbl(2,1,"Regular Hourly Rate:",mylen,1,0,fram2)
	fnTxt(2,mylen+3,12,12,1,"10",0,"Enter the regular hourly rate.",fram2)
	resp$(respc+=1)=str$(tdet(2))
	fnLbl(2,35,"O/T Hourly Rate:",mylen,1,0,fram2)
	fnTxt(2,58,12,12,1,"10",0,"Enter the overtime hourly rate.",fram2)
	resp$(respc+=1)=str$(tdet(3))
	fram3=3: fnFra(14,1,10,97,"Deductions and Additions")
	for j=1 to 10
		fnLbl(j,1,dednames$(j*2-1),mylen,1,0,fram3)
		fnTxt(j,mylen+3,12,12,1,"10",0,"Enter the standard amount or the percent.",fram3)
		resp$(respc+=1)=str$(tdet(j*2-1+3))
		fnLbl(j,35,dednames$(j*2),mylen,1,0,fram3)
		fnTxt(j,58,12,12,1,"10",0,"Enter the standard amount or the percent.",fram3)
		resp$(respc+=1)=str$(tdet(j*2+3))
	next j
	if departmentCount=whichDepartment then
	fnCmdKey("Retur&n to Employee",3,1,0,"Save any changes and access next departmental record.")
	else
	fnCmdKey("&Next Department",3,1,0,"Save any changes and access next departmental record.")
	end if
	fnCmdKey("&Add Department",4,0,0,"Add an additional department record.")
	fnCmdKey("&Review Checks",10,0,0,"Review check information.")
	fnCmdKey("&Delete",9,0,0,"Deletes the department record.")
	fnCmdKey("C&omplete",1,0,0,"Saves any changes and returns to main screen.")
	fnCmdKey("&Cancel",5,0,1,"Exit departmental record without saving changes.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto AskEmployee
	teno=val(resp$(1)) ! employee # in dept record
	tdn=val(resp$(2)) ! department #
	if ckey=9 then 
		delete #hDepartment,key=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn):
		goto EditEmployee
	end if
	if resp$(3)="combos" then resp$(3)=""
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
		ml$(1)="The department number can not be 0 (zero)."
		ml$(2)="Enter a valid department number!"
		fnmsgbox(mat ml$,resp$)
		goto ScrDepartment
	end if
	if departmentAddMode then
		write #hDepartment,using 'Form POS 1,N 8,N 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',reserve: eno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet ! Duprec 3140
	else
		if eno<>ent then goto ChangeEmployeeNo
		rewrite #hDepartment,using "Form POS 1,N 8,N 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2": teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet
	end if
	firstread=0
	departmentAddMode=0
	if ckey=4 then ! add new department
		goto DepartmentAdd
	else if ckey=3 then  ! move to next departmental record
		goto NextDepartment
	else if ckey=1 then
		goto EditEmployee
	else if ckey=10 then
		fncheckfile(hact$:=str$(eno),hCheckIdx3,hCheckIdx1,hEmployee)
		goto EditEmployee
	end if
goto AskEmployee ! /r
DepartmentAdd: ! r: new department
	departmentAddMode=1
	tdn=0
	gl$=""
	mat tdt=(0)
	mat tcd=(0)
	tli=0
	mat tdet=(0)
	firstread=0
goto ScrDepartment ! /r

SaveEmployee: ! r:
	if add1=1 then 
		write #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c 
		
		add1=0
	else if holdeno<>eno then 
		goto ChangeEmployeeNo
	else
		rewrite #hEmployee,using F_employee,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c 
	end if
return ! /r

DeleteEmployee: ! r:
	mat ml$(2)
	ml$(1)="Employee Number "&ltrm$(ent$)&" will be Deleted."
	ml$(2)="Do you wish to continue?"
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then ! delete direct deposit
		fn_dDdelete(eno)
		delete #hEmployee,key=ent$:
		fnKeyDelete(hDepartment,'Form POS 1,N 8',cnvrt$("pic(zzzzzzz#)",eno))
		! delete departmental records
		fnKeyDelete(hDepartment,'Form POS 1,N 8',cnvrt$("pic(zzzzzzz#)",eno))
		! delete check transactions
		heno$=lpad$(str$(eno),8)
		fnKeyDelete(hCheckIdx1,'Form POS 1,N 8',heno$)
	else
		goto DeleteEmployeeFinis
	end if
	DeleteEmployeeFinis: !
goto Menu1 ! /r
ChangeEmployeeNo: ! r:
	mat ml$(3)
	ml$(1)="You have chosen to change the employee number"
	ml$(2)="from "&str$(holdeno)&" to "&str$(eno)&"."
	ml$(3)="Do you wish to continue?"
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$<>"Yes" then
		goto CHGENO_XIT
	end if
	read #hEmployee,using 'form pos 1,n 8',key=lpad$(str$(eno),8): teno nokey L3790
	mat ml$(2)
	ml$(1)="Employee Number "&ltrm$(ent$)&" already exists."
	ml$(2)="You cannot change to this number."
	fnmsgbox(mat ml$,resp$)
	goto CHGENO_XIT
	L3790: ! change direct deposit
	fn_DirectDepositKeyChange(rpad$(trim$(ent$),10),key$:=fn_dDkey$(eno))
	! CHANGE DEPARTMENTS NUMBERS
	heno$=lpad$(str$(holdeno),8) ! &"   "
	restore #hDepartment,key>=rpad$(heno$,kln(hDepartment)): nokey L3890
	do
		read #hDepartment,using 'Form POS 1,N 8': deno eof L3890
		if deno<>holdeno then goto L3890
		rewrite #hDepartment,using 'form pos 1,n 8',rec=rec(hDepartment): eno
	loop
	L3890: ! pause
	fnKeyChange(hCheckIdx1,'form pos 1,n 8',heno$,lpad$(str$(eno),8)) ! change employee number in check history
	! r: change employee number in any and all rpwork files.

		! the old way 1/9/20   ! for wsid_item=1 to 99
		! the old way 1/9/20   ! 	wsid_item$=cnvrt$('pic(##)',wsid_item)
		! the old way 1/9/20   ! 	if exists('[Q]\PRmstr\rpwork'&wsid_item$&'.h[cno]') then
		! the old way 1/9/20   ! 		open #h_rpwork:=fngethandle: "Name=[Q]\PRmstr\rpwork"&wsid_item$&".h[cno],KFName=[Q]\PRmstr\rpwork"&wsid_item$&"idx.H[cno]"&',shr',internal,outIn,keyed ioerr RPWORK_OPEN_ERR
		! the old way 1/9/20   ! 		fnKeyChange(h_rpwork,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
		! the old way 1/9/20   ! 		close #h_rpwork:
		! the old way 1/9/20   ! 		RPWORK_OPEN_ERR: !
		! the old way 1/9/20   ! 	end if
		! the old way 1/9/20   ! next wsid_item
	dim filename$(0)*256
	dim kfname$*256
	fnGetDir2('[Q]\PRmstr\',mat filename$, '','rpwork*.h[cno]')
	for fileItem=1 to udim(mat filename$)
		if pos(lwrc$(filename$(fileItem)),'idx.')<=0 and pos(lwrc$(filename$(fileItem)),'idx2.')<=0 then
			kfname$=srep$(filename$(fileItem),'.','Idx.')
			open #h_rpwork:=fngethandle: 'Name=[Q]\PRmstr\'&filename$(fileItem)&',KFName=[Q]\PRmstr\'&kfname$&',shr',internal,outIn,keyed ioerr RpworkOpenErr
			fnKeyChange(h_rpwork,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
			close #h_rpwork:
		end if
		RpworkOpenErr: !
	nex fileItem
	! /r
	! change main employee record
	delete #hEmployee,key=ent$:
	write #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,w4Step2,w4Year$,ph$,bd,w4Step3,w4Step4a,w4Step4b,w4Step4c 
	ent$=lpad$(str$(eno),8)
	hact$=ent$
	CHGENO_XIT: !
goto Menu1 ! /r
def fn_openFiles
	open #hEmployee:=fngethandle: "name=[Q]\PRmstr\Employee.h[cno],version=1,kfName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,outIn,keyed
	F_employee: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,n 1,c 4,x 1,c 12,n 6,4*n 12.2
	open #hEmployeeIdx2:=fngethandle: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr",internal,outIn,keyed
	open #hCheckIdx1:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr",internal,outIn,keyed
	open #hCheckIdx3:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr",internal,outIn,keyed
	open #hDepartment:=fngethandle: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed
fnend
def fn_EmployeeDepartments(eno,mat empDept)
	if ~edc_setup or ~edc_hDepartment then
		edc_setup=1
		open #edc_hDepartment:=fngethandle: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,input,keyed
	end if
	mat empDept(0)
	restore #edc_hDepartment,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey edcFinis
	do
		read #edc_hDepartment,using 'Form POS 1,N 8,N 3': teno,deptNumber eof edcFinis
		if teno=eno then 
			fnAddOneN(mat empDept,deptNumber)
		end if
	loop while teno=eno
	edcFinis: !
	fn_EmployeeDepartments=udim(mat empDept)
fnend
def fn_setup
	autoLibrary
	on error goto Ertn

	dim ph$*12
	dim resp$(50)*128
	dim ty(21)
	dim tqm(17)
	dim tcp(22)
	dim em(16)
	dim w4Step2
	dim w4Year$*4
	dim tdt(4),tcd(3)
	dim tdet(23)
	dim ss$*11
	dim rs(2)
	dim em$(3)*30
	dim ml$(2)*80
	
	dim race_option$(7)*15
	race_option$(1)="0 - Unknown"
	race_option$(2)="1 - Caucasian"
	race_option$(3)="2 - Hispanic"
	race_option$(4)="3 - Black"
	race_option$(5)="4 - Oriental"
	race_option$(6)="5 - AmIndian"
	race_option$(7)="6 - Indochines"
	
	dim gender_option$(3)*11
	gender_option$(1)="0 - Unknown"
	gender_option$(2)="1 - Male"
	gender_option$(3)="2 - Female"
	


	dim fed_exemption_option$(22)
	for j=1 to 21
		fed_exemption_option$(j)=str$(j-1)
	next j
	fed_exemption_option$(22)="99"



	dim code6$(4)*28
	code6$(1)="0 - Subject to SS and Med WH"
	code6$(2)="1 - SS only"
	code6$(3)="2 - Medicare Only"
	code6$(4)="9 - Neither SS nor Medicare"


	dim statenames$(10)*8
	open #1: "Name=[Q]\PRmstr\Company.h[cno]",internal,outIn,relative
	read #1,using "form pos 150,10*c 8",rec=1: mat statenames$
	close #1:
	dim state_option$(10)*11
	for j=1 to 10: state_option$(j)=cnvrt$("Pic(z#)",j)&" "&statenames$(j): next j

	dim dednames$(20)*20
	fnDedNames(mat dednames$)
	for j=1 to 20
		if trim$(dednames$(j))<>"" then
			dednames$(j)=trim$(dednames$(j))&":"
		end if
	next j

	scrEmployee=1     :      fkey_scrEmployee=5201
	mat scrDept(99)
	dim fkey_scrDept(99)
	for deptItem=1 to udim(mat scrDept)
		scrDept(deptItem)=(deptItem+1)
		fkey_scrDept(deptItem)=5201+deptItem
	nex deptItem



	dim marriedOption$(0)*58
	dim eicOption$(0)*29
	dim w4yearOption$(0)*4
	dim payPeriodOption$(0)*16
	fn_getEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)



fnend
def library fnGetEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)
	if ~setup then fn_setup
	fnGetEmpOptions=fn_getEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)
fnend

def fn_getEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)
	mat marriedOption$(0)
	fnAddOneC(mat marriedOption$,"0 - Single")
	fnAddOneC(mat marriedOption$,"1 - Married - filing jointly")
	fnAddOneC(mat marriedOption$,'2 - Single - Head of Household')
	fnAddOneC(mat marriedOption$,'3 - Married - filing joint - only one working')
	fnAddOneC(mat marriedOption$,'4 - Married - filing joint - both working')
	fnAddOneC(mat marriedOption$,'5 - Married - filing seperate - both working')

	mat eicOption$(0)
	fnAddOneC(mat eicOption$,"0 - Not qualified for EIC"    )  !  em(7)=1
	fnAddOneC(mat eicOption$,"1 - Single or Spouse not file")  !  em(7)=2
	fnAddOneC(mat eicOption$,"2 - Married both filing"      )  !  em(7)=3
	
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
	close #hEmployee:
	close #hEmployeeIdx2:
	close #hDepartment:
goto Xit ! /r
Xit: fnXit


DD: ! r:
	key$=fn_dDkey$(eno)
	hDd=fn_dDopen
	read #hDd,using "Form pos 11,C 1,N 9,N 2,C 17",key=key$: dd$,rtn,acc,acn$ nokey DdReadNoKey
AskDd: !
	if ~setup_askdd then
		setup_askdd=1
		dim optDirectDepositAccountType$(2)*22
		optDirectDepositAccountType$(1)="27 = Regular Checking"
		optDirectDepositAccountType$(2)="37 = Savings Account"
		dim optEnableDirectDeposit$(2)*33
		optEnableDirectDeposit$(1)="Y = Activate Direct Deposit"
		optEnableDirectDeposit$(2)="N = Direct Deposit not activated."
	end if

	fnTos
	respc=0: mylen=35 : right=1
	fnLbl(1,1,"Employee Number:",mylen,right)
	fnTxt(1,mylen+3,8,8,1,"",1,"")
	resp$(respc+=1)=str$(eno)
	fnLbl(2,1,"Direct Deposit:",mylen,right)
	fncomboa("Directd",2,mylen+3,mat optEnableDirectDeposit$,"",35)
	respc+=1
	for j=1 to udim(optEnableDirectDeposit$)
		if dd$=optEnableDirectDeposit$(j)(1:1) then resp$(respc)=optEnableDirectDeposit$(j)
	next j
	fnLbl(3,1,"Employee Bank Routing Number:",mylen,right)
	fnTxt(3,mylen+3,9,9,1,"",0,"Employee's bank's routing #. The bank account and the routing # can be found at the bottom of the employees personal check.")
	resp$(respc+=1)=str$(rtn)
	fnLbl(4,1,"Account Type:",mylen,right)
	fncomboa("AccType",4,mylen+3,mat optDirectDepositAccountType$,"",35)
	respc+=1
	resp$(respc)=''
	for j=1 to udim(mat optDirectDepositAccountType$)
		if acc=val(optDirectDepositAccountType$(j)(1:2)) then resp$(respc)=optDirectDepositAccountType$(j)
	next j
	if resp$(respc)='' then let resp$(respc)=optDirectDepositAccountType$(1)
	fnLbl(5,1,"Employee Bank Account Number:",mylen,right)
	! fnTxt(5,mylen+3,17,17,1,"30",0,"Enter the employee's bank account number. ")
	fnTxt(5,mylen+3,17,17,1,'30',0,"Enter the employee's bank account number. ")
	resp$(respc+=1)=acn$
	fnCmdKey("&Save",1,1,0,"Saves the information on the screen." )
	fnCmdKey("&Delete",4,0,0,"Deletes the direct deposit information on this employee.You can stop direct deposits simply by changing the direct deposit question to no.")
	fnCmdKey("&Cancel",5,0,1,"Cancels without recording any chnages to the screen.")
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto DdFinis
	key$ =    resp$(1)
	dd$  =    resp$(2)(1:1)
	rtn  =val(resp$(3)     ) !  banks routing #
	acc  =val(resp$(4)(1:2)) ! checking or savings
	acn$ =    resp$(5)       ! employee bank acct #
	if ckey=4 then
		dd$="N"
		rtn=acc=0 : acn$=''
		key$=rpad$(key$,10)
		delete #hDd,key=key$: nokey ignore
		goto DdFinis
	end if
	if dd$="Y" and (rtn=0 or acc=0 or acn$='') then
		mat ml$(2)
		ml$(1)="You must have valid answers in the routing #, account"
		ml$(2)="type, and bank account before you can answer yes."
		fnmsgbox(mat ml$,resp$)
		goto AskDd
	else if ckey=1 or ckey=4 then
		key$=rpad$(str$(eno),10)
		rewrite #hDd,using "Form pos 11,C 1,N 9,N 2,C 17",key=key$: dd$,rtn,acc,acn$
		goto DdFinis
	end if
goto AskDd ! /r

DdReadNoKey: ! r:
	dd$='N' : rtn=acc=0 : acn$='' ! defaults
	write #hDd,using "Form pos 1,C 10,C 1,N 9,N 2,C 17": key$,dd$,rtn,acc,acn$ nokey DdReadNoKey
goto AskDd ! /r
DdFinis: ! r:
	fn_dDclose
return  ! /r
def fn_dDkey$*10(eno)
	fn_dDkey$=rpad$(str$(eno),10)
fnend
def fn_dDopen
	open #hDd:=fngethandle: "Name=[Q]\PRmstr\dd.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],kps=1,kln=10,Use",internal,outIn,keyed
	fn_dDopen=hDd
fnend
def fn_dDclose
	close #hDd:
	hDd=0
fnend

def fn_dDdelete(eno)
	! delete the record
	hDd=fn_dDopen
	fnKeyDelete(hDd,'Form POS 1,N 8',cnvrt$("pic(zzzzzzz#)",eno))
	fn_dDclose
fnend
def fn_DirectDepositKeyChange(from$,to$)
	hDd=fn_dDopen
	fnKeyChange(hDd,'form pos 1,C 10',from$,to$)
	fn_dDclose
fnend
include: Ertn
