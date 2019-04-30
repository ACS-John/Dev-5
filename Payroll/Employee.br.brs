! formerly S:\acsPR\newprFM
! Payroll Employee File
! r: setup and open files
	fn_setup
	fntop(program$)
fn_openFiles
goto MENU1 ! /r


MENU1: !
goto AskEmployee
AskEmployee: ! r:
	departmentAddMode=0  !  this used to be in MENU1, but I moved it here because it shouldn't matter and I'd like to remove the MENU1 in favor of AskEmployee
	ad1=0 ! add code - used to tell other parts of the program, that I am currently adding an employee record.
	fnTos(sn$="Employee-ask")
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
	fnAcs(sn$,0,mat resp$,ckey) ! ask employee #
	hact$=resp$(1)(1:8)
	eno=ent=val(resp$(1)(1:8))
	if ckey=1 then
		ad1=1 ! ti1=1
		goto AddEmployee
	else if ckey=2 then
		goto EditEmployee
	else if ckey=3 then
		read #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd eof EmpNotFound
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
	fnTos(sn$="Employeefm")
	respc=0 : frac=0
	mylen=25 : mypos=mylen+2
	fnLbl(1,1,"Employee Number:",mylen,1)
	fnTxt(1,mylen+3,8,8,1,"30",0,"Employee numbers must be numeric.")
	resp$(respc+=1)=str$(eno)
	fnCmdKey("&Next",1,1,0,"Process employee information.")
	fnCmdKey("&Cancel",5,0,1,"Returns to maintenance screem.")
	fnAcs(sn$,0,mat resp$,ckey)
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
	mat ta=(0)
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
goto ScrEmployee ! /r
EditEmployee: ! r:
	if ent=0 then goto AskEmployee
	teno=eno=ent ! hdar=0
	ent$=lpad$(str$(ent),8)
	read #hEmployee,using F_employee,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd nokey EmpNotFound
	holdeno=eno
goto ScrEmployee ! /r
EmpNotFound: ! r:
	mat ml$(2)
	ml$(1)="A record with this number does not exist!"
	ml$(2)="Select a different employee number."
	fnmsgbox(mat ml$,resp$,'',48)
goto AskEmployee ! /r
ScrEmployee: ! r:
	fnTos(sn$="Employeeedit")
	respc=0 : frac=0 !
	mylen=28 : mypos=mylen+2
	fnLbl(1,1,"Employee Number:",mylen,1)
	fnTxt(1,mylen+3,8,8,1,"30",0,"Employee numbers must be numeric.")
	resp$(respc+=1)=str$(eno)
	fnLbl(2,1,"Name:",mylen,1)
	fnTxt(2,mylen+3,30,30,0,"",0,"Name can be entered first name first or last name first.")
	resp$(respc+=1)=em$(1)
	fnLbl(3,1,"Address:",mylen,1)
	fnTxt(3,mylen+3,30,30,0,"",0,"")
	resp$(respc+=1)=em$(2)
	fnLbl(4,1,"City, State Zip:",mylen,1)
	fnTxt(4,mylen+3,30,30,0,"",0,"")
	resp$(respc+=1)=em$(3)
	fnLbl(5,1,"Social Security #:",mylen,1)
	fnTxt(5,mylen+3,11,11,0,"",0,"")
	resp$(respc+=1)=ss$
	fnLbl(6,1,"Race:",mylen,1)
	respc+=1: for j=1 to udim(race_option$)
		if rs(1)=val(race_option$(j)(1:1)) then resp$(respc)=race_option$(j)
	next j
	fncomboa("Race",6,mylen+3,mat race_option$,"",16)
	fnLbl(7,1,"Sex:",mylen,1)
	respc+=1: for j=1 to udim(gender_option$)
		if rs(2)=val(gender_option$(j)(1:1)) then resp$(respc)=gender_option$(j)
	next j
	fncomboa("Sex",7,mylen+3,mat gender_option$,"",10)
	fnLbl(8,1,"Marital Status:",mylen,1)
	respc+=1: for j=1 to udim(married_option$)
		if em(1)=val(married_option$(j)(1:1)) then resp$(respc)=married_option$(j)
	next j
	fncomboa("Marital",8,mylen+3,mat married_option$) ! ,"",11)
	fnLbl(9,1,"Federal Exemptions:",mylen,1)
	respc+=1
	for j=1 to udim(fed_exemption_option$)
		if em(2)=val(fed_exemption_option$(j)(1:2)) then resp$(respc)=fed_exemption_option$(j)
	next j
	fncomboa("FedEx",9,mylen+3,mat fed_exemption_option$,"",3)
	fnLbl(10,1,"State Exemptions:",mylen,1)
	respc+=1
	for j=1 to udim(fed_exemption_option$)
		if em(3)=val(fed_exemption_option$(j)(1:2)) then resp$(respc)=fed_exemption_option$(j)
	next j
	fncomboa("StateEx",10,mylen+3,mat fed_exemption_option$,"",3)
	fnLbl(11,1,"Employment Status:",mylen,1)
	fncombof("EmpStatus",11,mylen+3,25,"[Q]\PRmstr\EmpStatus.dat",1,2,3,25,"[Q]\PRmstr\EmpStatus.idx",0,0, " ",fracustinfo,0)
	resp$(respc+=1)=str$(em(4))
	fnLbl(12,1,"Pay Code:",mylen,1)
	respc+=1
	for j=1 to udim(payperiod_option$)
		if em(5)=val(payperiod_option$(j)(1:1)) then resp$(respc)=payperiod_option$(j)
	next j
	fncomboa("PayCode",12,mylen+3,mat payperiod_option$,"",16)
	fnLbl(13,1,"FICA Code:",mylen,1)
	respc+=1: for j=1 to udim(code6$)
		if em(6)=val(code6$(j)(1:1)) then resp$(respc)=code6$(j)
	next j
	fncomboa("FICACode",13,mylen+3,mat code6$,"",32)
	fnLbl(14,1,"EIC Code:",mylen,1)
	fncomboa("EICCode",14,mylen+3,mat code7$,"",31)
	resp$(respc+=1)=code7$(em(7)+1)
	fnLbl(15,1,"Sick Pay Code:",mylen,1)
	fnTxt(15,mylen+3,6,6,0,"33",0,"Normally is number of sick hours you want accrued each pay period.")
	resp$(respc+=1)=str$(em(8))
	fnLbl(16,1,"Vacation Pay Code:",mylen,1)
	fnTxt(16,mylen+3,6,6,0,"33",0,"Normally is number of vacation hours you want accrued each pay period.")
	resp$(respc+=1)=str$(em(9))
	fnLbl(17,1,"Sick Hours Accrued:",mylen,1)
	fnTxt(17,mylen+3,10,10,0,"32",0,"This should be the balance of sick hours available at this time.")
	resp$(respc+=1)=str$(em(10))
	fnLbl(18,1,"Vacation Hours Accrued:",mylen,1)
	fnTxt(18,mylen+3,10,10,0,"32",0,"This should be the balance of vacation hours available at this time.")
	resp$(respc+=1)=str$(em(11))
	fnLbl(19,1,"Standard Federal W/H:",mylen,1)
	fnTxt(19,mylen+3,10,10,0,"32",0,"If you wish for the system to withhold a fixed amount of Federal withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip Federal withholdings on this employee.")
	resp$(respc+=1)=str$(em(12))
	col3_pos=51 : col3_len=20
	fnLbl(19,col3_pos,"Federal Tax Add-On:",col3_len,1)
	fnTxt(19,73,10,10,0,"32",0,"If you wish for the system to add additional Federal withholdings, enter that amount here.")
	resp$(respc+=1)=str$(em(13))
	fnLbl(20,1,"Standard State W/H:",mylen,1)
	fnTxt(20,mylen+3,10,10,0,"32",0,"If you wish for the system to withhold a fixed amount of State withholdings, enter that amount here. You can use a negative one dollar (-1.00) to skip state withholdings on this employee.")
	resp$(respc+=1)=str$(em(14))
	fnLbl(20,col3_pos,"State Tax Add-On:",col3_len,1)
	fnTxt(20,73,10,10,0,"32",0,"If you wish for the system to add additional state withholdings, enter that amount here.")
	resp$(respc+=1)=str$(em(15))
	fnLbl(21,1,"Date Hired:",mylen,1)
	fnTxt(21,mylen+3,10,10,0,"1",0,"The date hired is only used for information purposes only.")
	resp$(respc+=1)=str$(em(16))
	fnLbl(21,col3_pos,"Last Payroll Date:",col3_len,1)
	fnTxt(21,73,10,10,0,"1",0,"This will always be the last time pay was calculated on this employee.")
	resp$(respc+=1)=str$(lpd)
	fnLbl(22,1,"Birth Date:",mylen,1)
	fnTxt(22,mylen+3,10,10,0,"1",0,"The birth date is not required.")
	resp$(respc+=1)=str$(bd)
	fnLbl(22,col3_pos,"Phone Number:",col3_len,1)
	fnTxt(22,73,12,12,0,"",0,"")
	resp$(respc+=1)=ph$
	! picture=0
	fnCmdKey('&Departments ('&str$(fn_EmployeeDepartmentCount(eno))&')',2,0,0,"Review this employee's departmental information.")
	fnCmdKey("Direct D&eposit",7,0,0,"Review direct deposit information.")
	fnCmdKey("Re&view Special Hrs",8,0,0,"Review miscellaneous breakdown of hours.")
	fnCmdKey("&Review Checks",10,0,0,"Review check information.")
	! fnCmdKey("&Picture",6,0,0,"Place any picture in share\images.")
	if ad1=0 then
		fnCmdKey("De&lete",4,0,0,"Deletes this record")
	end if
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto AskEmployee
	eno=val(resp$(1)(1:8))
	em$(1)=resp$(2) ! name
	em$(2)=resp$(3)
	em$(3)=resp$(4)
	ss$=resp$(5)
	rs(1)=val(resp$(6)(1:1))
	rs(2)=val(resp$(7)(1:1)) ! sex
	em(1)=val(resp$(8)(1:1)) ! marital status
	em(2)=val(resp$(9)(1:2)) ! fed ex
	em(3)=val(resp$(10)(1:2)) ! state ex
	em(4)=val(resp$(11)(1:2)) ! emp status
	em(5)=val(resp$(12)(1:2)) ! pay code
	em(6)=val(resp$(13)(1:2)) ! fica code
	em(7)=val(resp$(14)(1:2)) ! eic code
	em(8)=val(resp$(15)(1:5)) ! sick pay
	em(9)=val(resp$(16)) ! vacation Pay code
	em(10)=val(resp$(17)) ! sick accrued
	em(11)=val(resp$(18)) ! vac accrued
	em(12)=val(resp$(19)) ! std fed
	em(13)=val(resp$(20)) ! fed addon
	em(14)=val(resp$(21)) ! std state
	em(15)=val(resp$(22)) ! state addon
	em(16)=val(resp$(23)) ! date hired
	lpd=val(resp$(24)) ! last payroll date
	bd=val(resp$(25)) ! birth date
	ph$=resp$(26) ! phone
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
			goto MENU1
		end if
	else if ckey=4 then
		goto DeleteEmployee
	end if
goto FirstDepartment
! /r
EndOfDepartments: !
goto ScrEmployee
FirstDepartment: ! r:
	firstread=1
	restore #hDepartment,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey DepartmentAdd
	whichDepartment=0
goto NextDepartment ! /r
NextDepartment: ! r:
	read #hDepartment,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof EndOfDepartments
	whichDepartment+=1
	if firstread=1 and teno<>eno then goto DepartmentAdd
	if teno<>eno then goto EndOfDepartments
ScrDepartment: !
	fnTos(sn$="EmployeeDep")
	respc=0 : fram1=1
	mylen=20 : mypos=mylen+2 : mat resp$=("")
	dim departmentCap$*128
	departmentCount=fn_EmployeeDepartmentCount(eno)
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
	fnAcs(sn$,0,mat resp$,ckey)
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
		if eno<>ent then goto CHGENO
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
	goto AskEmployee
! /r
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
		write #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
		add1=0
	else if holdeno<>eno then 
		goto CHGENO
	else
		rewrite #hEmployee,using F_employee,key=ent$: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
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
		! restore #hCheckIdx1,key>=heno$&"         ": nokey DeleteEmployeeFinis
		! do
		! 	read #hCheckIdx1,using 'form pos 1,n 8': histeno eof DeleteEmployeeFinis
		! 	if histeno<>eno then goto DeleteEmployeeFinis
		! 	delete #hCheckIdx1:
		! loop
	else
		goto DeleteEmployeeFinis
	end if
	DeleteEmployeeFinis: !
goto MENU1 ! /r
CHGENO: ! r:
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
	for wsid_item=1 to 99
		wsid_item$=cnvrt$('pic(##)',wsid_item)
		if exists('[Q]\PRmstr\rpwork'&wsid_item$&'.h[cno]') then
			open #h_rpwork:=fngethandle: "Name=[Q]\PRmstr\rpwork"&wsid_item$&".h[cno],KFName=[Q]\PRmstr\rpwork"&wsid_item$&"idx.H[cno]"&',shr',internal,outIn,keyed ioerr RPWORK_OPEN_ERR
			fnKeyChange(h_rpwork,'form pos 1,n 8',heno$,lpad$(str$(eno),8))
			close #h_rpwork:
			RPWORK_OPEN_ERR: !
		end if
	next wsid_item
	! /r
	! L3980: ! change main employee record
	delete #hEmployee,key=ent$:
	write #hEmployee,using F_employee: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
	ent$=lpad$(str$(eno),8)
	hact$=ent$
	CHGENO_XIT: !
goto MENU1 ! /r
def fn_openFiles
	open #hEmployee:=fngethandle: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,outIn,keyed
	F_employee: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,2*pd 3,c 12,n 6
	open #hEmployeeIdx2:=fngethandle: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPIndx2.h[cno],Shr",internal,outIn,keyed
	open #hCheckIdx1:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr",internal,outIn,keyed
	open #hCheckIdx3:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr",internal,outIn,keyed
	open #hDepartment:=fngethandle: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed
fnend
def fn_EmployeeDepartmentCount(eno; ___,returnN)
	if ~edc_setup or ~edc_hDepartment then
		edc_setup=1
		open #edc_hDepartment:=fngethandle: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,input,keyed
	end if
	restore #edc_hDepartment,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey edcFinis
	do
		read #edc_hDepartment,using 'Form POS 1,N 8': teno eof edcFinis
		if teno=eno then returnN+=1
	loop while teno=eno
	edcFinis: !
	fn_EmployeeDepartmentCount=returnN
fnend
def fn_setup
	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fnhours
	library 'S:\Core\Library': fnTos,fnLbl,fnCmdKey,fnAcs,fncombof,fnTxt
	library 'S:\Core\Library': fncmbemp
	library 'S:\Core\Library': fncomboa,fnpic
	library 'S:\Core\Library': fnFra
	library 'S:\Core\Library': fnrgl$,fnqgl,fnagl$
	library 'S:\Core\Library': fncheckfile
	library 'S:\Core\Library': fnemployee_srch
	library 'S:\Core\Library': fngethandle
	library 'S:\Core\Library': fnKeyChange,fnKeyDelete
	library 'S:\Core\Library': fnDedNames
	library 'S:\Core\Library': fnaddonec
	library 'S:\Core\Library': fnmsgbox
	on error goto ERTN
	! ______________________________________________________________________
	dim ph$*12
	dim resp$(50)*128
	dim ty(21)
	dim tqm(17)
	dim tcp(22)
	dim em(16)
	dim ta(2)
	dim tdt(4),tcd(3)
	dim tdet(23)
	dim ss$*11
	dim rs(2)
	dim em$(3)*30
	dim ml$(2)*80
	!
	dim race_option$(7)*15
	race_option$(1)="0 - Unknown"
	race_option$(2)="1 - Caucasian"
	race_option$(3)="2 - Hispanic"
	race_option$(4)="3 - Black"
	race_option$(5)="4 - Oriental"
	race_option$(6)="5 - AmIndian"
	race_option$(7)="6 - Indochines"
	!
	dim gender_option$(3)*11
	gender_option$(1)="0 - Unknown"
	gender_option$(2)="1 - Male"
	gender_option$(3)="2 - Female"
	!
	dim married_option$(0)*58
	mat married_option$(0)
	fnaddonec(mat married_option$,"0 - Single")
	fnaddonec(mat married_option$,"1 - Married")
	fnaddonec(mat married_option$,'2 - Single - Head of Household')
	fnaddonec(mat married_option$,'3 - Married - filing joint - only one working')
	fnaddonec(mat married_option$,'4 - Married - filing joint - both working')
	fnaddonec(mat married_option$,'5 - Married - filing seperate - both working')

	dim fed_exemption_option$(22)
	for j=1 to 21
		fed_exemption_option$(j)=str$(j-1)
	next j
	fed_exemption_option$(22)="99"

	dim payperiod_option$(4)
	payperiod_option$(1)="1 - Monthly"
	payperiod_option$(2)="2 - Semi-monthly"
	payperiod_option$(3)="3 - Bi-weekly"
	payperiod_option$(4)="4 - Weekly"

	dim code6$(4)*28
	code6$(1)="0 - Subject to SS and Med WH"
	code6$(2)="1 - SS only"
	code6$(3)="2 - Medicare Only"
	code6$(4)="9 - Neither SS nor Medicare"

	dim code7$(3)*29
	code7$(1)="0 - Not qualified for EIC"       !  em(7)=1
	code7$(2)="1 - Single or Spouse not file"   !  em(7)=2
	code7$(3)="2 - Married both filing"         !  em(7)=3

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
fnend
Finis: ! ! r:
	close #hEmployee:
	close #hEmployeeIdx2:
	close #hDepartment:
goto XIT ! /r
XIT: fnxit
IGNORE: continue


DD: ! r:
	key$=fn_dDkey$(eno)
	hDd=fn_dDopen
	read #hDd,using "Form pos 11,C 1,N 9,N 2,C 17",key=key$: dd$,rtn,acc,acn$ nokey DdReadNoKey
ASKDD: !
	if ~setup_askdd then
		setup_askdd=1
		dim optDirectDepositAccountType$(2)*22
		optDirectDepositAccountType$(1)="27 = Regular Checking"
		optDirectDepositAccountType$(2)="37 = Savings Account"
		dim optEnableDirectDeposit$(2)*33
		optEnableDirectDeposit$(1)="Y = Activate Direct Deposit"
		optEnableDirectDeposit$(2)="N = Direct Deposit not activated."
	end if

	fnTos(sn$="DirectDeposit")
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
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto DdFinis
	key$=resp$(1)
	dd$=resp$(2)(1:1)
	rtn=val(resp$(3)) !  banks routing #
	acc=val(resp$(4)(1:2)) ! checking or savings
	acn$=resp$(5) ! employee bank acct #
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
		goto ASKDD
	else if ckey=1 or ckey=4 then
		key$=rpad$(str$(eno),10)
		rewrite #hDd,using "Form pos 11,C 1,N 9,N 2,C 17",key=key$: dd$,rtn,acc,acn$
		goto DdFinis
	end if
goto ASKDD ! /r

DdReadNoKey: ! r:
	dd$='N' : rtn=acc=0 : acn$='' ! defaults
	write #hDd,using "Form pos 1,C 10,C 1,N 9,N 2,C 17": key$,dd$,rtn,acc,acn$ nokey DdReadNoKey
goto ASKDD ! /r
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
include: ertn
