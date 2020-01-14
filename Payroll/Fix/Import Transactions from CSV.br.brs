fn_setup
fntop(program$)
	dim dednames$(20)*20
	dim abrevname$(20)*8
	dim newdedcode(20)
	fnDedNames(mat dednames$,mat abrevname$,mat newdedcode)
	dim tran$(0)*128,tranN(0),csz$*30
	hTran:=fn_open('PR Transaction',mat tran$,mat tranN,mat form$)
		
dim deptRegisterFile$*1024
dim emplRegisterFile$*1024
dim delim$*1
delim$=','
		
SCREEN1: ! r:
fncreg_read(cap$&'.deptRegisterFile$',deptRegisterFile$, 'D:\ACS\(Client_Files)\Payroll Done Right\Excape 2019 Transaction Rebuild with deductions v2 - Department.csv')
fncreg_read(cap$&'.emplRegisterFile$',emplRegisterFile$, 'D:\ACS\(Client_Files)\Payroll Done Right\Excape 2019 Transaction Rebuild with deductions v2 - Employee.csv')
fnTos
col1_width=24 : col2_pos=col1_width+2
fnLbl(1,1,"Department Register CSV:",col1_width,1)
fnTxt(1,col2_pos,30,256,0,'70',0,'')
resp$(1)=deptRegisterFile$
fnLbl(2,1,"Employee Register CSV:",col1_width,1)
fnTxt(2,col2_pos,30,256,0,'70',0,'')
resp$(2)=emplRegisterFile$
fnCmdSet(2)
fnAcs(sn$,0,mat resp$,ckey)
if ckey=5 then goto XIT
deptRegisterFile$=resp$(1)
emplRegisterFile$=resp$(2)
fncreg_write(cap$&'.deptRegisterFile$',deptRegisterFile$)
fncreg_write(cap$&'.emplRegisterFile$',emplRegisterFile$)
! /r
! r: Department File - gather into local variables
	open #hInDept:=fngethandle: 'name='&deptRegisterFile$,d,i
	deptEnumCount=0
	dept_Date                    	=deptEnumCount+=1
	dept_Dept                    	=deptEnumCount+=1
	dept_Emp_No                  	=deptEnumCount+=1
	dept_Emp_Name                	=deptEnumCount+=1
	dept_HoursReg                	=deptEnumCount+=1
	dept_HoursOT                 	=deptEnumCount+=1
	dept_HoursSick               	=deptEnumCount+=1
	dept_HoursVac                	=deptEnumCount+=1
	dept_HoursHol                	=deptEnumCount+=1
	dept_TotalHours              	=deptEnumCount+=1
	dept_Tips                    	=deptEnumCount+=1
	dept_TotalPay                	=deptEnumCount+=1
	dept_MedWh                   	=deptEnumCount+=1
	dept_SSWh                    	=deptEnumCount+=1
	dept_FederalWh               	=deptEnumCount+=1
	dept_StateWh                 	=deptEnumCount+=1
	dept_Other               			=deptEnumCount+=1
	dept_Pay               				=deptEnumCount+=1
	dept_Salary                  	=deptEnumCount+=1
	dept_TipsAddedToPaycheck    	=deptEnumCount+=1
	dept_WC_and_TrimetTax       	=deptEnumCount+=1
	dept_TrimetTax               	=deptEnumCount+=1
	dept_WC                       	=deptEnumCount+=1
	dim deptList_Emp_Name$(0)*40
	deptRecordCount=fn_readDeptIntoArrays(hInDept,mat deptList_Date,mat deptList_Dept,mat deptList_Emp_No,mat deptList_Emp_Name$,mat deptList_HoursReg,mat deptList_HoursOT,mat deptList_HoursSick,mat deptList_HoursVac,mat deptList_HoursHol,mat deptList_TotalHours,mat deptList_Tips,mat deptList_TotalPay,mat deptList_MedWh,mat deptList_SSWh,mat deptList_FederalWh,mat deptList_StateWh,mat deptList_Other,mat deptList_Pay,mat deptList_Salary,mat deptList_TipsAddedToPaycheck,mat deptList_WC_and_TrimetTax,mat deptList_TrimetTax,mat deptList_WC)
	close #hInDept:
! /r
! r: Employee File - gather into local variables
	open #hInEmpl:=fngethandle: 'name='&emplRegisterFile$,d,i
	emplEnumCount=0
	empl_Date            =emplEnumCount+=1
	empl_Check_No        =emplEnumCount+=1
	empl_Emp_No          =emplEnumCount+=1
	empl_Emp_Name        =emplEnumCount+=1
	empl_Reg             =emplEnumCount+=1
	empl_OT              =emplEnumCount+=1
	empl_Sick            =emplEnumCount+=1
	empl_Vac             =emplEnumCount+=1
	empl_Hol             =emplEnumCount+=1
	empl_Total_Hours    =emplEnumCount+=1
	empl_Total_Pay       =emplEnumCount+=1
	empl_Med_WH          =emplEnumCount+=1
	empl_FICA            =emplEnumCount+=1
	empl_Federal         =emplEnumCount+=1
	empl_State           =emplEnumCount+=1
	empl_Other           =emplEnumCount+=1
	empl_Net_Pay         =emplEnumCount+=1
	dim emplList_Emp_Name$(0)*40
	emplRecordCount=fn_readEmplIntoArrays(hInEmpl,mat emplList_Date,mat emplList_Check_No,mat emplList_Emp_No,mat emplList_Emp_Name$,mat emplList_Reg,mat emplList_OT,mat emplList_Sick,mat emplList_Vac,mat emplList_Hol,mat emplList_Total_Hours,mat emplList_Total_Pay,mat emplList_Med_WH,mat emplList_FICA,mat emplList_Federal,mat emplList_State,mat emplList_Other,mat emplList_Net_Pay)
	close #hInEmpl:
! /r
for empItem=1 to emplRecordCount
	deptMatchCount=fn_getMatchingDeptIndexes(emplList_Date(empItem),emplList_Emp_No(empItem),mat deptMatch)
	if deptMatchCount<0 then
		pr '***deptMatchCount=';deptMatchCount;'***';bell
		pr emplList_Date(empItem),emplList_Emp_No(empItem)
		pause
	else
		for deptItem=1 to deptMatchCount
			deptIndex=deptMatch(deptItem)
			mat tranN=(0)
			mat tran$=('')
			empNo             	=emplList_Emp_No(empItem)
			dept              	=deptList_Dept(deptIndex)
			prDate            	=emplList_Date(empItem)
			ckno              	=emplList_Check_No(empItem)
			hoursReg          	=deptList_HoursReg(deptIndex)
			hoursOt           	=deptList_HoursOt(deptIndex)
			hoursSick         	=deptList_HoursSick(deptIndex)
			hoursVac          	=deptList_HoursVac(deptIndex)
			hoursHoli         	=deptList_HoursHol(deptIndex)
			wagesWorkmansComp	=round(deptList_TotalPay(deptIndex),0)
			wagesSS           	=round(deptList_TotalPay(deptIndex),0)
			wagesMedicare    	=round(deptList_TotalPay(deptIndex),0)
			fedUc             	=round(deptList_TotalPay(deptIndex),0)
			StateUc           	=round(deptList_TotalPay(deptIndex),0)
			fn_buildTran1(empNo,dept,prDate,ckno,hoursReg,hoursOt,hoursSick,hoursVac,hoursHoli,wagesWorkmansComp,wagesSS,wagesMedicare,fedUc,StateUc)
			tcp1 =deptList_FederalWh(deptIndex) 				! ,WH - Federal             
			tcp2 =deptList_SSWh(deptIndex)      				! ,WH - SS                  
			tcp3 =deptList_MedWh(deptIndex)     				! ,WH - Medicare            
			tcp4 =deptList_StateWh(deptIndex)				! ,WH - State               
			tcp5 =0				! ,DRAW standard deduction 1   
			tcp6 =0				! ,standard deduction 2       
			tcp7 =0				! ,GARN standard deduction 3   
			tcp8 =0				! ,standard deduction 4       
			tcp9 =0				! ,standard deduction 5       
			tcp10=deptList_Tips(deptIndex)       				! ,+ TIPS standard deduction 6 
			tcp11=0				! ,standard deduction 7      
			tcp12=deptList_WC(deptIndex) ! round(deptList_WC(deptIndex),0)				! ,W/C standard deduction 8  
			tcp13=deptList_TrimetTax(deptIndex)				! ,standard deduction 9      
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
			tcp26=deptList_TotalPay(deptIndex)				! ,Pay - Regular            
			tcp27=0				! ,Pay - OverTime           
			tcp28=0 ! deptList_Other(deptIndex)			! ,Other Compensation      
			tcp29=0				! ,Meals                    
			tcp30=deptList_Tips(deptIndex)				! ,Tips                     
			tcp31=deptList_TotalPay(deptIndex)				! ,Pay - Total              
			tcp32=deptList_Pay(deptIndex)				! ,Pay - Net                
			fn_buildTran2(tcp1,tcp2,tcp3,tcp4,tcp5,tcp6,tcp7,tcp8,tcp9,tcp10,tcp11,tcp12,tcp13,tcp14,tcp15,tcp16,tcp17,tcp18,tcp19,tcp20,tcp21,tcp22,tcp23,tcp24,tcp25,tcp26,tcp27,tcp28,tcp29,tcp30,tcp31,tcp32)
			write #hTran,using form$(hTran): mat tran$,mat tranN
		nex deptItem
	end if
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
def fn_getMatchingDeptIndexes(dateCcyymmdd,empNo,mat matchingIndexes; ___,x)
	if ~setup_gmdi then
		setup_gmdi=1
		dim gmdiKey$(0)*128
		mat gmdiKey$(deptRecordCount)
		for x=1 to deptRecordCount
			gmdiKey$(x)=str$(deptList_Date(x))&'-'&str$(deptList_Emp_No(x))
		nex x
	end if
	mat matchingIndexes(0)
	gmdiSearch$=str$(dateCcyymmdd)&'-'&str$(empNo)
	match=-1
	do
		match=srch(mat gmdiKey$,gmdiSearch$, match+1)
		fnAddOneN(mat matchingIndexes,match, 1)
	loop until match<=0
	fn_getMatchingDeptIndexes=udim(mat matchingIndexes)
fnend
def fn_readDeptIntoArrays(hInDept,mat deptList_Date,mat deptList_Dept,mat deptList_Emp_No,mat deptList_Emp_Name$,mat deptList_HoursReg,mat deptList_HoursOT,mat deptList_HoursSick,mat deptList_HoursVac,mat deptList_HoursHol,mat deptList_TotalHours,mat deptList_Tips,mat deptList_TotalPay,mat deptList_MedWh,mat deptList_SSWh,mat deptList_FederalWh,mat deptList_StateWh,mat deptList_Other,mat deptList_Pay,mat deptList_Salary,mat deptList_TipsAddedToPaycheck,mat deptList_WC_and_TrimetTax,mat deptList_TrimetTax,mat deptList_WC)
	linput #hInDept: line$ ! simply skip the headers for now
	deptRecordCount=0
	do
		linput #hInDept: line$ eof ReadDeptEof
		str2mat(line$,mat item$, delim$)
		fn_mat_deptList(deptRecordCount+=1)
		deptList_Date          						(deptRecordCount)	=	date(days(item$(dept_Date),'mm/dd/yy'),'ccyymmdd')
		if deptList_Date(deptRecordCount)=0 then pr item$(dept_Date) : pause
		deptList_Dept           						(deptRecordCount)	=	val(item$(dept_Dept          							))
		deptList_Emp_No         						(deptRecordCount)	=	val(item$(dept_Emp_No        							))
		deptList_Emp_Name$     						(deptRecordCount)	=	    item$(dept_Emp_Name      							)
		deptList_HoursReg      						(deptRecordCount)	=	val(item$(dept_HoursReg     							))
		deptList_HoursOT       						(deptRecordCount)	=	val(item$(dept_HoursOT      							))
		deptList_HoursSick     						(deptRecordCount)	=	val(item$(dept_HoursSick    							))
		deptList_HoursVac      						(deptRecordCount)	=	val(item$(dept_HoursVac     							))
		deptList_HoursHol      						(deptRecordCount)	=	val(item$(dept_HoursHol     							))
		deptList_TotalHours    						(deptRecordCount)	=	val(item$(dept_TotalHours   							))
		deptList_Tips          						(deptRecordCount)	=	val(item$(dept_Tips          							))
		deptList_TotalPay      						(deptRecordCount)	=	val(item$(dept_TotalPay     							))
		deptList_MedWh         						(deptRecordCount)	=	val(item$(dept_MedWh        							))
		deptList_SSWh          						(deptRecordCount)	=	val(item$(dept_SSWh          							))
		deptList_FederalWh    						(deptRecordCount)	=	val(item$(dept_FederalWh     							))
		deptList_StateWh       						(deptRecordCount)	=	val(item$(dept_StateWh       							))
		deptList_Other    									(deptRecordCount)	=	val(item$(dept_Other     									))
		deptList_Pay     									(deptRecordCount)	=	val(item$(dept_Pay     										))
		deptList_Salary                		(deptRecordCount)	=	val(item$(dept_Salary        							))
		deptList_TipsAddedToPaycheck  		(deptRecordCount)	=	val(item$(dept_TipsAddedToPaycheck     	))
		deptList_WC_and_TrimetTax     		(deptRecordCount)	=	val(item$(dept_WC_and_TrimetTax     			))
		deptList_TrimetTax             		(deptRecordCount)	=	val(item$(dept_TrimetTax     							))
		deptList_WC                     		(deptRecordCount)	=	val(item$(dept_WC     											))
	loop
	ReadDeptEof: !
	fn_readDeptIntoArrays=deptRecordCount
fnend
def fn_mat_deptList(newArraySize)
	mat deptList_Date          						(newArraySize)
	mat deptList_Dept          						(newArraySize)
	mat deptList_Emp_No       						(newArraySize)
	mat deptList_Emp_Name$    						(newArraySize)
	mat deptList_HoursReg     						(newArraySize)
	mat deptList_HoursOT      						(newArraySize)
	mat deptList_HoursSick    						(newArraySize)
	mat deptList_HoursVac     						(newArraySize)
	mat deptList_HoursHol     						(newArraySize)
	mat deptList_TotalHours   						(newArraySize)
	mat deptList_Tips          						(newArraySize)
	mat deptList_TotalPay     						(newArraySize)
	mat deptList_MedWh         						(newArraySize)
	mat deptList_SSWh          						(newArraySize)
	mat deptList_FederalWh    						(newArraySize)
	mat deptList_StateWh      						(newArraySize)
	mat deptList_Other         						(newArraySize)
	mat deptList_Pay    										(newArraySize)
	mat deptList_Salary                		(newArraySize)
	mat deptList_TipsAddedToPaycheck  		(newArraySize)
	mat deptList_WC_and_TrimetTax     		(newArraySize)
	mat deptList_TrimetTax             		(newArraySize)
	mat deptList_WC                     		(newArraySize)

fnend

def fn_readEmplIntoArrays(hInempl,mat emplList_Date,mat emplList_Check_No,mat emplList_Emp_No,mat emplList_Emp_Name$,mat emplList_Reg,mat emplList_OT,mat emplList_Sick,mat emplList_Vac,mat emplList_Hol,mat emplList_Total_Hours,mat emplList_Total_Pay,mat emplList_Med_WH,mat emplList_FICA,mat emplList_Federal,mat emplList_State,mat emplList_Other,mat emplList_Net_Pay)
	linput #hInempl: line$ ! simply skip the headers for now
	emplRecordCount=0
	do
		linput #hInempl: line$ eof ReadEmplEof
		str2mat(line$,mat item$, delim$)
		fn_mat_emplList(emplRecordCount+=1)
		if trim$(item$(empl_Date))='' then ! use the last date
			emplList_Date           (emplRecordCount)	=emplList_Date(emplRecordCount-1)
		else 
			emplList_Date         (emplRecordCount)	=date(days(item$(empl_Date),'mm/dd/ccyy'),'ccyymmdd')
		end if
		
		
		emplList_Check_No       (emplRecordCount)	=val(item$(empl_Check_No       ))
		emplList_Emp_No         (emplRecordCount)	=val(item$(empl_Emp_No         ))
		emplList_Emp_Name$      (emplRecordCount)	=item$(empl_Emp_Name       )
		emplList_Reg            (emplRecordCount)		=val(item$(empl_Reg            ))
		emplList_OT             (emplRecordCount)		=val(item$(empl_OT              ))
		emplList_Sick           (emplRecordCount)	=val(item$(empl_Sick           ))
		emplList_Vac            (emplRecordCount)		=val(item$(empl_Vac            ))
		emplList_Hol            (emplRecordCount)		=val(item$(empl_Hol            ))
		emplList_Total_Hours   (emplRecordCount)		=val(item$(empl_Total_Hours    ))
		emplList_Total_Pay      (emplRecordCount)	=val(item$(empl_Total_Pay      ))
		emplList_Med_WH         (emplRecordCount)	=val(item$(empl_Med_WH         ))
		emplList_FICA           (emplRecordCount)	=val(item$(empl_FICA           ))
		emplList_Federal        (emplRecordCount)	=val(item$(empl_Federal        ))
		emplList_State          (emplRecordCount)	=val(item$(empl_State          ))
		emplList_Other          (emplRecordCount)	=val(item$(empl_Other          ))
		emplList_Net_Pay        (emplRecordCount)	=val(item$(empl_Net_Pay        ))
	loop
	ReadEmplEof: !
	fn_readEmplIntoArrays=emplRecordCount
fnend
def fn_mat_emplList(newArraySize)
	mat emplList_Date           (newArraySize)
	mat emplList_Check_No       (newArraySize)
	mat emplList_Emp_No         (newArraySize)
	mat emplList_Emp_Name$      (newArraySize)
	mat emplList_Reg            (newArraySize)
	mat emplList_OT             (newArraySize)
	mat emplList_Sick           (newArraySize)
	mat emplList_Vac            (newArraySize)
	mat emplList_Hol            (newArraySize)
	mat emplList_Total_Hours   (newArraySize)
	mat emplList_Total_Pay      (newArraySize)
	mat emplList_Med_WH         (newArraySize)
	mat emplList_FICA           (newArraySize)
	mat emplList_Federal        (newArraySize)
	mat emplList_State          (newArraySize)
	mat emplList_Other          (newArraySize)
	mat emplList_Net_Pay        (newArraySize)
fnend





def fn_setup
	if ~setup then
		setup=1
		on error goto Ertn

		dim line$*1024
		dim item$(0)*256


		library 'S:\Core\Library': fntop
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnTos,fnAcs,fnCmdKey
		library 'S:\Core\Library': fnxit
		library 'S:\Core\Library': fnLbl,fnTxt
		library 'S:\Core\Library': fnCmdSet
		library 'S:\Core\Library': fncombof,fnButton
		library 'S:\Core\Library': fnopenprn,fncloseprn
		library 'S:\Core\Library': fnDedNames
		library 'S:\Core\Library': fnAddOneC,fnAddOneN
		library 'S:\Core\Library': fncreg_read,fncreg_write

		dim resp$(64)*1024

	end if
fnend
Xit: fnXit
include: fn_open
include: ertn
