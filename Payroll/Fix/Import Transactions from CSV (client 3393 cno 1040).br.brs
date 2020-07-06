fn_setup
fnTop(program$)
	dim dednames$(20)*20
	dim abrevname$(20)*8
	dim newdedcode(20)
	fnDedNames(mat dednames$,mat abrevname$,mat newdedcode)
	dim tran$(0)*128,tranN(0),csz$*30
	hTran:=fn_open('PR Transaction',mat tran$,mat tranN,mat form$)
		
dim emplRegisterFile$*1024
dim delim$*1
delim$=','
 
 
SCREEN1: ! r:
! fncreg_read(cap$&'.department$',department$, '1')
fncreg_read(cap$&'.emplRegisterFile$',emplRegisterFile$, 'D:\ACS\(Client_Files)\Payroll Done Right\CAMEO CAFE 2019 TRANSACTION REBUILD - employee.csv')
fnTos
col1_width=24 : col2_pos=col1_width+2
fnLbl(2,1,"Employee Register CSV:",col1_width,1)
fnTxt(2,col2_pos,30,256,0,'70',0,'')
resp$(1)=emplRegisterFile$
fnCmdSet(2)
fnAcs(mat resp$,ckey)
if ckey=5 then goto Xit
emplRegisterFile$=resp$(1)
fncreg_write(cap$&'.emplRegisterFile$',emplRegisterFile$)
! /r
! r: Employee File - gather into local variables
	open #hInEmpl:=fngethandle: 'name='&emplRegisterFile$,d,i
	emplEnumCount=0
	empl_Date          	=emplEnumCount+=1
	empl_Check_No      	=emplEnumCount+=1
	empl_Emp_No        	=emplEnumCount+=1
	empl_Emp_Name      	=emplEnumCount+=1
	empl_Reg           	=emplEnumCount+=1
	empl_OT             	=emplEnumCount+=1
	empl_Sick          	=emplEnumCount+=1
	empl_Vac           	=emplEnumCount+=1
	empl_Hol           	=emplEnumCount+=1
	empl_Total_Hours  	=emplEnumCount+=1
	empl_Total_Pay    	=emplEnumCount+=1
	empl_Med_WH       	=emplEnumCount+=1
	empl_FICA         	=emplEnumCount+=1
	empl_Federal      	=emplEnumCount+=1
	empl_State        	=emplEnumCount+=1
	empl_Other        	=emplEnumCount+=1
	empl_Net_Pay      	=emplEnumCount+=1
	empl_WC           	=emplEnumCount+=1
	empl_TRANSIT      	=emplEnumCount+=1
	empl_DRAW         	=emplEnumCount+=1
	empl_GARN         	=emplEnumCount+=1
	empl_TIPS         	=emplEnumCount+=1
	empl_DEPT         	=emplEnumCount+=1
	dim emplList_Emp_Name$(0)*40
	emplRecordCount=fn_readEmplIntoArrays
	close #hInEmpl:
! /r
for empItem=1 to emplRecordCount
	mat tranN=(0)
	mat tran$=('')
	empNo             	=emplList_Emp_No(empItem)
	dept              	=emplList_DEPT(empItem)
	prDate            	=emplList_Date(empItem)
	ckno              	=emplList_Check_No(empItem)
	hoursReg          	=emplList_Total_Hours(empItem)
	hoursOt           	=emplList_OT(empItem)
	hoursSick         	=emplList_Sick(empItem)
	hoursVac          	=emplList_Vac(empItem)
	hoursHoli         	=emplList_Hol(empItem)
	wagesWorkmansComp 	=round(emplList_Total_Pay(empItem),0)
	wagesSS           	=round(emplList_Total_Pay(empItem),0)
	wagesMedicare     	=round(emplList_Total_Pay(empItem),0)
	fedUc             	=round(emplList_Total_Pay(empItem),0)
	StateUc           	=round(emplList_Total_Pay(empItem),0)
	fn_buildTran1(empNo,dept,prDate,ckno,hoursReg,hoursOt,hoursSick,hoursVac,hoursHoli,wagesWorkmansComp,wagesSS,wagesMedicare,fedUc,StateUc)
	tcp1 =emplList_Federal(empItem)     				! ,WH - Federal
	tcp2 =emplList_FICA(empItem)        				! ,WH - SS
	tcp3 =emplList_Med_WH(empItem)      				! ,WH - Medicare
	tcp4 =emplList_State(empItem)       				! ,WH - State
	tcp5 =emplList_DRAW(empItem)        				! ,DRAW standard deduction 1
	tcp6 =0                             				! ,standard deduction 2
	tcp7 =emplList_GARN(empItem)         				! ,GARN standard deduction 3
	tcp8 =0				! ,standard deduction 4
	tcp9 =0				! ,standard deduction 5
	tcp10=0			! ,+ TIPS standard deduction 6
	tcp11=0				! ,standard deduction 7
	tcp12=emplList_WC(empItem) ! deptList_WC(deptIndex) ! round(deptList_WC(deptIndex),0)				! ,W/C standard deduction 8
	tcp13=emplList_TRANSIT(empItem) ! deptList_TrimetTax(deptIndex)				! ,standard deduction 9
	tcp14=0				! ,NO USE standard deduction 10
	tcp15=0				! ,standard deduction 11
	tcp16=0				! ,standard deduction 12
	tcp17=0				! ,standard deduction 13
	tcp18=0				! ,standard deduction 14
	tcp19=0				! ,standard deduction 15
	tcp20=0				! ,standard deduction 16
	tcp21=0				! ,standard deduction 17
	tcp22=0				! ,standard deduction 18
	tcp23=0				! ,standard deduction 19
	tcp24=0				! ,standard deduction 20
	tcp25=0				! ,tcp25
	tcp26=emplList_Total_Pay(empItem) !   deptList_TotalPay(deptIndex)				! ,Pay - Regular
	tcp27=0				! ,Pay - OverTime
	tcp28=0 ! deptList_Other(deptIndex)			! ,Other Compensation
	tcp29=0				! ,Meals
	tcp30=emplList_TIPS(empItem) ! deptList_Tips(deptIndex)				! ,Tips
	tcp31=emplList_Total_Pay(empItem)  !   deptList_TotalPay(deptIndex)				! ,Pay - Total
	tcp32=emplList_Net_Pay(empItem)    ! deptList_Pay(deptIndex)				! ,Pay - Net
	fn_buildTran2(tcp1,tcp2,tcp3,tcp4,tcp5,tcp6,tcp7,tcp8,tcp9,tcp10,tcp11,tcp12,tcp13,tcp14,tcp15,tcp16,tcp17,tcp18,tcp19,tcp20,tcp21,tcp22,tcp23,tcp24,tcp25,tcp26,tcp27,tcp28,tcp29,tcp30,tcp31,tcp32)
	write #hTran,using form$(hTran): mat tran$,mat tranN
nex empItem
goto Xit
def fn_buildTran1(empNo,dept,prDate,ckno,hoursReg,hoursOt,hoursSick,hoursVac,hoursHoli,wagesWorkmansComp,wagesSS,wagesMedicare,fedUc,StateUc)
	tranN(check_emp          )=empNo              	! ,Employee
	tranN(check_dept         )=dept               	! ,Department
	tranN(check_payrollDate )= prDate            	! ,Payroll Date  date(ccyymmdd)
	tranN(check_ckno         )=ckno               	! ,Check Number
	tranN(check_tdc1         )=hoursReg           	! ,Hours - Regular
	tranN(check_tdc2         )=hoursOt            	! ,Hours - OverTime
	tranN(check_tdc3         )=hoursSick          	! ,Hours - Sick
	tranN(check_tdc4         )=hoursVac           	! ,Hours - Vacation
	tranN(check_tdc5         )=hoursHoli          	! ,Hours - Holiday
	tranN(check_tdc6         )=wagesWorkmansComp 	! ,Workman Compensation Wages
	tranN(check_tdc7         )=wagesSS            	! ,SS Wages
	tranN(check_tdc8         )=wagesMedicare     	! ,Wages - Medicare
	tranN(check_tdc9         )=fedUc              	! ,Federal UC
	tranN(check_tdc10        )=StateUc	          	! ,State UC
fnend
def fn_buildTran2(tcp1,tcp2,tcp3,tcp4,tcp5,tcp6,tcp7,tcp8,tcp9,tcp10,tcp11,tcp12,tcp13,tcp14,tcp15,tcp16,tcp17,tcp18,tcp19,tcp20,tcp21,tcp22,tcp23,tcp24,tcp25,tcp26,tcp27,tcp28,tcp29,tcp30,tcp31,tcp32)
	tranN(check_tcp1 )=tcp1 				! ,WH - Federal
	tranN(check_tcp2 )=tcp2 				! ,WH - SS
	tranN(check_tcp3 )=tcp3 				! ,WH - Medicare
	tranN(check_tcp4 )=tcp4 				! ,WH - State
	tranN(check_tcp5 )=tcp5 				! ,DRAW standard deduction 1       , PD 5.2,                ,! SPos=83  aka=tcp(5)
	tranN(check_tcp6 )=tcp6 				! ,standard deduction 2
	tranN(check_tcp7 )=tcp7 				! ,GARN standard deduction 3       , PD 5.2,                ,! SPos=93  aka=tcp(7)
	tranN(check_tcp8 )=tcp8 				! ,standard deduction 4
	tranN(check_tcp9 )=tcp9 				! ,standard deduction 5
	tranN(check_tcp10)=tcp10				! ,+ TIPS standard deduction 6       , PD 5.2,                ,! SPos=108 aka=tcp(10)
	tranN(check_tcp11)=tcp11				! ,standard deduction 7
	tranN(check_tcp12)=tcp12				! ,W/C standard deduction 8
	tranN(check_tcp13)=tcp13				! ,standard deduction 9
	tranN(check_tcp14)=tcp14				! ,NO USE standard deduction 10      , PD 5.2,                ,! SPos=128 aka=tcp(14)
	tranN(check_tcp15)=tcp15				! ,standard deduction 11
	tranN(check_tcp16)=tcp16				! ,standard deduction 12
	tranN(check_tcp17)=tcp17				! ,standard deduction 13
	tranN(check_tcp18)=tcp18				! ,standard deduction 14
	tranN(check_tcp19)=tcp19				! ,standard deduction 15
	tranN(check_tcp20)=tcp20				! ,standard deduction 16
	tranN(check_tcp21)=tcp21				! ,standard deduction 17
	tranN(check_tcp22)=tcp22				! ,standard deduction 18
	tranN(check_tcp23)=tcp23				! ,standard deduction 19
	tranN(check_tcp24)=tcp24				! ,standard deduction 20
	tranN(check_tcp25)=tcp25				! ,tcp25
	tranN(check_tcp26)=tcp26				! ,Pay - Regular
	tranN(check_tcp27)=tcp27				! ,Pay - OverTime
	tranN(check_tcp28)=tcp28				! ,Other Compensation
	tranN(check_tcp29)=tcp29				! ,Meals
	tranN(check_tcp30)=tcp30				! ,Tips
	tranN(check_tcp31)=tcp31				! ,Pay - Total
	tranN(check_tcp32)=tcp32				! ,Pay - Net
fnend
 
def fn_emptyline(line$*2048; ___,returnN)
	line$=srep$(line$,',','')
	line$=srep$(line$,chr$(9),'')
	line$=trim$(line$)
	if line$='' then returnN=1
	fn_emptyline=returnN
fnend
 
def fn_readEmplIntoArrays
	linput #hInempl: line$ ! simply skip the headers for now
	emplRecordCount=0
	do
		linput #hInempl: line$ eof ReadEmplEof
		if fn_emptyline(line$) then goto ReadEmplEof
		str2mat(line$&' ',mat item$, delim$)
		fn_mat_emplList(emplRecordCount+=1)
		if trim$(item$(empl_Date))='' then ! use the last date
			emplList_Date        	(emplRecordCount)	=emplList_Date(emplRecordCount-1)
		else
			emplList_Date        	(emplRecordCount)	=date(days(item$(empl_Date),'mm/dd/ccyy'),'ccyymmdd')
		end if
		emplList_Check_No       	(emplRecordCount)	=val(item$(empl_Check_No      	))
		emplList_Emp_No         	(emplRecordCount)	=val(item$(empl_Emp_No        	))
		emplList_Emp_Name$      	(emplRecordCount)	=    item$(empl_Emp_Name      	)
		if trim$(emplList_Emp_Name$(emplRecordCount))='' then pause
		emplList_Reg            	(emplRecordCount)	=val(item$(empl_Reg           	))
		emplList_OT              	(emplRecordCount)	=val(item$(empl_OT            	))
		emplList_Sick           	(emplRecordCount)	=val(item$(empl_Sick          	))
		emplList_Vac            	(emplRecordCount)	=val(item$(empl_Vac           	))
		emplList_Hol            	(emplRecordCount)	=val(item$(empl_Hol           	))
		emplList_Total_Hours    	(emplRecordCount)	=val(item$(empl_Total_Hours   	))
		emplList_Total_Pay      	(emplRecordCount)	=val(item$(empl_Total_Pay     	))
		emplList_Med_WH         	(emplRecordCount)	=val(item$(empl_Med_WH        	))
		emplList_FICA           	(emplRecordCount)	=val(item$(empl_FICA          	))
		emplList_Federal        	(emplRecordCount)	=val(item$(empl_Federal       	))
		emplList_State          	(emplRecordCount)	=val(item$(empl_State         	))
		emplList_Other          	(emplRecordCount)	=val(item$(empl_Other         	))
		emplList_Net_Pay        	(emplRecordCount)	=val(item$(empl_Net_Pay       	))
		emplList_WC             	(emplRecordCount)	=val(item$(empl_WC            	))
		emplList_TRANSIT        	(emplRecordCount)	=val(item$(empl_TRANSIT       	))
		emplList_DRAW           	(emplRecordCount)	=val(item$(empl_DRAW          	))
		emplList_GARN           	(emplRecordCount)	=val(item$(empl_GARN          	))
		emplList_TIPS           	(emplRecordCount)	=val(item$(empl_TIPS          	))
		emplList_DEPT           	(emplRecordCount)	=val(item$(empl_DEPT          	))
 
	loop
	ReadEmplEof: !
	fn_readEmplIntoArrays=emplRecordCount
fnend
def fn_mat_emplList(newArraySize)
	mat emplList_Date          	(newArraySize)
	mat emplList_Check_No      	(newArraySize)
	mat emplList_Emp_No        	(newArraySize)
	mat emplList_Emp_Name$     	(newArraySize)
	mat emplList_Reg           	(newArraySize)
	mat emplList_OT            	(newArraySize)
	mat emplList_Sick          	(newArraySize)
	mat emplList_Vac           	(newArraySize)
	mat emplList_Hol           	(newArraySize)
	mat emplList_Total_Hours   	(newArraySize)
	mat emplList_Total_Pay     	(newArraySize)
	mat emplList_Med_WH        	(newArraySize)
	mat emplList_FICA          	(newArraySize)
	mat emplList_Federal       	(newArraySize)
	mat emplList_State         	(newArraySize)
	mat emplList_Other         	(newArraySize)
	mat emplList_Net_Pay       	(newArraySize)
	mat emplList_WC            	(newArraySize)
	mat emplList_TRANSIT       	(newArraySize)
	mat emplList_DRAW          	(newArraySize)
	mat emplList_GARN          	(newArraySize)
	mat emplList_TIPS          	(newArraySize)
	mat emplList_DEPT          	(newArraySize)
fnend
 
 
 
 
 
def fn_setup
	if ~setup then
		setup=1
		on error goto Ertn
 
		dim line$*1024
		dim item$(0)*256
 
 
		autoLibrary
 
		dim resp$(64)*1024
 
	end if
fnend
Xit: fnXit
include: fn_open
include: Ertn
