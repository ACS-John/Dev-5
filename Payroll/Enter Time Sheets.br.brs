fn_setup
fnTop(program$)

d1=fnPayPeriodEndingDate
if days(d1,'ccyymmdd')=>days(date)-5 and exists('[Q]\PRmstr\timesheet[acsUserId].h[cno]') then
	payrollDate=date(days(d1,'ccyymmdd'),'mmddyy')
	addDefault$='True'
	newDefault$='False'
else
	payrollDate=0
	addDefault$='False'
	newDefault$='True'
end if

Scr1: ! r:
	fnTos
	rc=fraNum=0
	fnFra(1,1,4,50,'Pay Period Ending Date','You must enter the pay perod ending date.  You can not have more than one payroll with the same date.')
	fraNum+=1 : mylen=23 : mypos=mylen+2
	fnLbl(1,1,'Pay Period Ending Date:',mylen,1,0,fraNum)
	fnTxt(1,mypos,10,0,1,'1',0,'Use mmddyy.',fraNum)   : resp$(resp_payrollDate=rc+=1)=str$(payrollDate)
	! fnFra(4,1,3,50,'Payroll Time Sheet Entry','You would only add to previous entries if the last batch was not calculated.',0)
	! fraNum+=1
	fnOpt(3,20,'Additions to Previous Input'	,0,fraNum) : resp$(resp_addbatch=rc+=1)= addDefault$ ! 'False'
	fnOpt(4,20,'New Batch'                    	,0,fraNum) : resp$(resp_newbatch=rc+=1)= newDefault$ ! 'True'
	fnFra(10-3,1,6,50,'Method of Entry','You can select specific employees to pay; you can automatically calculate salaried persons; or you can pull from a another system.')
	fraNum+=1 : mylen=18 : mypos=mylen+2
	fnOpt(1,3,'Select employees to pay',0,fraNum)              	: resp$(resp_add1=rc+=1)='True'
	fnOpt(2,3,'Automatically pay salaried employees',0,fraNum)	: resp$(resp_add2=rc+=1)='False'
	fnOpt(3,3,'Pull time from time card system',0,fraNum)      	: resp$(resp_add3=rc+=1)='False'
	fnOpt(4,3,'Pull time from job cost system',0,fraNum)       	: resp$(resp_add4=rc+=1)='False'
	fnLbl(6,1,'Employment Status:',mylen,1,0,fraNum)
	fnComboF('EmpStatus',6,mypos,25,'[Q]\PRmstr\EmpStatus.dat',1,2,3,25,'[Q]\PRmstr\EmpStatus.idx',0,0, 'Only necessary if automatically paying salaried people. ',fraNum,0)
	resp$(resp_empStatus=rc+=1)=''
	if ~fnArrayEmpty(mat dedName$) then

		fnFra(18-3,1,10,50,'Skip Deductions This Pay Period','You can skip any deduction this pay period by checking the deduction below.')
		fraNum+=1
		linecnt=0
		resp_skipDedAdd=rc
		for j=1 to 19 step 2
			if trim$(dedName$(j))<>'' then x$=':' else x$=''
			fnChk(linecnt+=1,20,trim$(dedName$(j))&x$,1,fraNum) : resp$(rc+=1)='False'
			if trim$(dedName$(j+1))<>'' then x$=':' else x$=''
			fnChk(linecnt,45,trim$(dedName$(j+1))&x$,1,fraNum)  : resp$(rc+=1)='False'
		next j

	end if
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	fnCmdKey('&Cancel',5,0,1,'Returns to customer record')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(resp_newbatch)='True' then ! Regular Time Sheet Entry
		noauto=ti1=1
		additional=1
	else if resp$(resp_addbatch)='True' then ! Additions to Previous Input
		noauto=ti1=2
		additional=2
	end if
	payrollDate=val(resp$(resp_payrollDate))

	if resp$(resp_add1)='True' then
		noauto=ti1=1
	else if resp$(resp_add2)='True' then
		noauto=ti1=2
	else if resp$(resp_add3)='True' then
		pullFromTimeCardSystem=99
	else if resp$(resp_add4)='True' then
		jobcost=1
	end if
	empStatus=val(resp$(resp_empStatus)(1:2))
	if fnArrayEmpty(mat dedName$) then
		mat resp$(1+resp_skipDedAdd:20+resp_skipDedAdd)=('False')
		mat skipit$(1:20)=('N')
	else
		for j=1 to 20
			if resp$(j+resp_skipDedAdd)='True' then skipit$(j)='Y' else skipit$(j)='N'
		next j
	end if

	if payrollDate=0 then
		mat ml$(0)
		fnAddOneC(mat ml$,'You must enter a valid payroll date!'   )
		fnAddOneC(mat ml$,'Click OK to return to previous screen. ')
		fnMsgBox(mat ml$,resp$)
		goto Scr1
	else if days(payrollDate,'mmddyy')<=days(date)-45 then
		mat ml$(0)
		fnAddOneC(mat ml$,'You have choosen a Pay Period Ending Date that is')
		fnAddOneC(mat ml$,'more than 45 days prior to today.')
		fnAddOneC(mat ml$,'Do you wish to continue?')
		fnMsgBox(mat ml$,resp$,'',mb_question+mb_yesno+mb_button2_default)
		if resp$='No' then goto Scr1
	end if
goto PastSkipDeductions ! /r
	! SKIPDEDUCTIONS: ! r:
	! if fnArrayEmpty(mat dedName$) then
	! 	mat resp$(1:20)=('False')
	! 	mat skipit$(1:20)=('N')
	! else
	! 	fnTos
	! 	rc=fraNum=linecnt=0
	! 	fnFra(1,1,10,50,'skip Deductions This Pay Period','You can skip any deduction this pay period by checking the deduction below.')
	! 	fraNum+=1
	! 	resp_skipDedAdd=rc
	! 	for j=1 to 19 step 2
	! 		if trim$(dedName$(j))<>'' then x$=':' else x$=''
	! 		fnChk(linecnt+=1,20,trim$(dedName$(j))&x$,1,fraNum) : resp$(rc+=1)='False'
	! 		if trim$(dedName$(j+1))<>'' then x$=':' else x$=''
	! 		fnChk(linecnt,45,trim$(dedName$(j+1))&x$,1,fraNum)  : resp$(rc+=1)='False'
	! 	next j
	! 	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	! 	fnCmdKey('&Cancel',5,0,1,'Returns to customer record')
	! 	ckey=fnAcs(mat resp$)
	! 	if ckey<>5 then
	! 		for j=1 to 20
	! 			if resp$(j+resp_skipDedAdd)='True' then skipit$(j)='Y' else skipit$(j)='N'
	! 		next j
	! 	end if
	! end if
	! /r
PastSkipDeductions: ! r: fall into this logic
	if noauto<>2 then empStatus=0 ! do not allow any employment status code if not selecting to automatically pay salaried
	if (~exists('[Q]\PRmstr\timesheet[acsUserId].h[cno]') and additional=2) or additional<>2 then
		open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],RecL=167,Replace',internal,output
		close #hTimesheet:
		hTimesheet=0
	end if
	fnIndex('[Q]\PRmstr\timesheet[acsUserId].h[cno]','[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno]','1,11')
	fnStatusClose
	gosub OpenFiles
	if additional=2 then
		! gosub OpenFiles
		gosub PrintListing
		goto ScrProofTotals
	end if
	! gosub OpenFiles
	if jobcost=1 then goto PullFromJobCost
goto TheNextOne ! /r
TheNextOne: ! r:
	if pullFromTimeCardSystem then
		goto ProcessTimeCard
	end if
goto ScrAskEmployee ! /r
OpenFiles: ! r:

	open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno],shr',i,outIn,k ! #3
	Ftimesheet: form pos 1,n 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2

	open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,outIn,k
	! open #11: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr',i,outIn,k
	Femployee1: form pos 9,c 30,pos 118,n 2,pos 126,2*pd 3.3,pos 162,n 6,pd 5.2
	Femployee2: form pos 162,n 6,pd 5.2
	Femployee3: form pos 1,n 8,c 30,pos 118,n 2,pos 126,2*pd 3.3,pos 162,n 6,pd 5.2

	open #hDept=fnH: 'Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr',i,outIn,k ! #2

	open #hTimeCard=fnH: 'Name='&pathToTimeCard$&'timecard\simplesummary,KFName='&pathToTimeCard$&'timecard\ssindex,Shr',i,outIn,k ioerr L4630 ! timecard #4
	timecard=1 ! timecard files exist
	L4630: !

return  ! /r

ScrEnterTime: ! r:
	read #hEmployee,using Femployee1,key=lpad$(str$(eno),8): em$,empStatus,em8,em9,lpd,tgp nokey TheNextOne
	if editMode then goto ReadDepartment
	if payrollDate=lpd then  ! r: EmpPrevEnteredWarn
		mat ml$(2)
		ml$(1)='Employee number '&str$(eno)&' has been previously entered.'
		ml$(2)='Do you wish to continue anyway? '
		fnMsgBox(mat ml$,resp$,'',52)
		if resp$(1:1)='Y' then goto L1290 ! IN1=2
		if resp$(1:1)='N' then goto TheNextOne ! in1=1
		! /r
	end if
	L1290: !
	! goto DUPLICATE_DATE_TEST
	! DUPLICATE_DATE_TEST: ! r: ! dont allow to calculate if reversing calculation needs to be run
	restore #hDept,key>=cnvrt$('pic(zzzzzzz#)',eno)&'   ': nokey DUPLICATE_DATE_TEST_XIT
	do
		read #hDept,using 'form pos 1,n 8,pos 42,n 6': depeno,tdt4 eof DUPLICATE_DATE_TEST_XIT
		if depeno<>eno then goto DUPLICATE_DATE_TEST_XIT
		if tdt4=payrollDate then
			mat ml$(0)
			fnAddOneC(mat ml$,'You have previously calculated pay using this same payroll date on employee # '&x$)
			fnAddOneC(mat ml$,'You must either use a different date or reverse the previous calculation. '       )
			fnAddOneC(mat ml$,'Click OK to return to previous screen. '                                          )
			fnMsgBox(mat ml$,resp$)
			goto TheNextOne
		end if
	loop
	DUPLICATE_DATE_TEST_XIT: !
	! goto ReadDepartment ! /r

	ReadDepartment: !
	tgp=0
	restore #hDept,key>=cnvrt$('pic(zzzzzzz#)',eno)&'   ':
	L1340: !
	if goprev=0 then
		read #hDept,using 'form pos 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,dep,gl$,mat tdt,mat tcd,tli,mat tdet eof ScrAskEmployee
	else if goprev=1 then
		semp-=1
		goprev=0
		read #hDept,using 'form pos 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',prior: teno,dep,gl$,mat tdt,mat tcd,tli,mat tdet eof ScrAskEmployee
	end if
	if teno=eno and goprev=0 then semp+=1
	if teno<>eno then semp=0
	if teno<>eno then goto ScrAskEmployee
	hr(1)=tdet(2) : hr(2)=tdet(3) ! set hourly rates from dept rec
	regHrs=otHrs=vacHrs=sickHrs=holHrs=otherComp=0 ! timecard
	if timecard=1 then
		tcKey$=lpad$(str$(eno),8)&cnvrt$('n 3',dep)&cnvrt$('n 5',cno) ! timecard
		read #hTimeCard,using 'form pos 17,6*pd 5.2',key=tcKey$: regHrs,otHrs,vacHrs,sickHrs,holHrs,otherComp nokey ignore ! timecard
	end if
	! dim shd$*78
	! shd$='Employee # '&str$(eno)&'   Name '&rtrm$(em$)&'    Department # '&str$(dep)
	if editMode then goto L1450
	goto L1490
	L1450: !
	tdet(2)=hr(1)
	tdet(3)=hr(2)
	tgp=tgp-gpd
goto ScrAskTime ! /r
L1490: ! r: build mat inpX and goto ScrAskTime
	mat inpX=(0)
	inpX(1)=regHrs
	inpX(2)=otHrs
	inpX(3)=vacHrs
	inpX(4)=sickHrs
	inpX(5)=holHrs
	inpX(7)=otherComp ! timecard
	inpX(6)=tdet(1)
	for j=1 to 20
		inpX(j+9)=tdet(j+3)
		if skipIt(j)=1 then inpX(j+9)=0
	next j
goto ScrAskTime ! /r
ScrAskTime: ! r:
	fnTos
	respc=0: mylen=20: fraNum=0: rc=0
	fnLbl(1,1,'Employee Number: '&str$(eno),60,2,0,fraNum)
	fnLbl(2,1,'Employee Name: '&rtrm$(em$),60,2,0,fraNum)
	fnLbl(3,1,'Department: '&str$(dep)&' '&fnDeptName$(dep),60,2,0,fraNum)
	fnLbl(5,1,'Regular Hours:',mylen,1,0,fraNum)
	fnTxt(5,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(1))
	fnLbl(6,1,'Overtime Hours:',mylen,1,0,fraNum)
	fnTxt(6,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(2))
	fnLbl(7,1,'Sick Hours:',mylen,1,0,fraNum)
	fnTxt(7,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(3))
	fnLbl(8,1,'Vacation Hours:',mylen,1,0,fraNum)
	fnTxt(8,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(4))
	fnLbl(9,1,'Holiday Hours:',mylen,1,0,fraNum)
	fnTxt(9,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(5))
	fnLbl(10,1,'Salary:',mylen,1,0,fraNum)
	fnTxt(10,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(6))
	fnLbl(11,1,'Other Compensation:',mylen,1,0,fraNum)
	fnTxt(11,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(7))
	fnLbl(12,1,'Meals:',mylen,1,0,fraNum)
	fnTxt(12,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(8))
	fnLbl(13,1,'Tips:',mylen,1,0,fraNum)
	fnTxt(13,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(inpX(9))
	fnLbl(15,1,'Reg Hourly Rate:',mylen,1,0,fraNum)
	fnTxt(15,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(hr(1))
	fnLbl(16,1,'O/T Hourly Rate:',mylen,1,0,fraNum)
	fnTxt(16,mylen+2,12,0,1,'10',0,'.',fraNum)
	resp$(rc+=1)=str$(hr(2))
	dim name$(20)*21
	for j=1 to 20
		if trim$(dedName$(j))='' then name$(j)='' else name$(j)=trim$(dedName$(j))&':'
		if skipit$(j)='Y' then inpX(j+9)=0
		disable_deduction=0 : if trim$(name$(j))='' then disable_deduction=1
		fnLbl(j+4,25,trim$(name$(j)),mylen,1,0,fraNum)
		fnTxt(j+4,47,12,0,1,'10',disable_deduction,'.',fraNum)
		resp$(rc+=1)=str$(inpX(j+9))
	next j
	fnCmdKey('&Next',1,1,0,'Record this time' )
	if ~editMode then fnCmdKey('&skip Department F2',2,0,0,'Skips this department.')
	if ~editMode and semp>=1 then fnCmdKey('&Prev Department',12,0,0,'Go back to last department.')
	if editMode then fnCmdKey('&Delete Department',10,0,0,'Deletes the hours, etc for this department.')
	if ~editMode then fnCmdKey('&Track Hours',8,0,0,'Track hours other than those entered above.')
	fnCmdKey('&Make Changes Permanent',3,0,0,'Makes any rate changes or other deductions changes permanent in the employee record.')
	if ~editMode then fnCmdKey('E&xit',5,0,1,'Returns to menu')
	if editMode then fnCmdKey('&Finish',7,0,1,'Finished making ScrCorrections')
	ckey=fnAcs(mat resp$) ! ask time
	if ckey=5 and ~editMode then goto FinishAndConfirm
	for j=1 to 9
		inpX(j)=val(resp$(j))
	next j
	hr(1)=val(resp$(10))
	hr(2)=val(resp$(11))
	for j=12 to 31
		inpX(j-2)=val(resp$(j))
	next j
	if ckey=8 then fnHours(eno) : goto ScrAskTime !  breakdown=1 : goto ScrAskTime
	if ckey=5 and editMode then goto L2290
	if ckey=10 and editMode then goto DeleteTimesheetEntry
	if ckey=2 then goto SkipDepartment
	if ckey=12 then goprev=1 : goto L1340
	if ckey<>3 then goto PostDeptRewrite
	tdet(1)=inpX(6)
	for j=1 to 20
		tdet(j+3)=inpX(j+9)
	next j
	tdet(2)=hr(1)
	tdet(3)=hr(2)
	rewrite #hDept,using 'form pos 9,n 3,pos 58,23*pd 4.2',key=cnvrt$('pic(ZZZZZZZZ)',eno)&cnvrt$('pic(ZZZ)',dep): dep,mat tdet
	PostDeptRewrite: !
	gpd=0
	if em8=-2 and inpX(3) then
		mat ml$(0)
		fnAddOneC(mat ml$,'This employee is not eligible for Sick Leave!')
		fnAddOneC(mat ml$,'Click OK to return to previous screen. '      )
		fnMsgBox(mat ml$,resp$)
		goto ScrAskTime
	else if em9=-2 and inpX(4) then
		mat ml$(0)
		fnAddOneC(mat ml$,'This employee is not eligible for Vacation!')
		fnAddOneC(mat ml$,'Click OK to return to previous screen. '    )
		fnMsgBox(mat ml$,resp$)
		goto ScrAskTime
	end if
	L2290: !
	for j=1 to 5
		! if env$('client')='West Rest Haven' and sickHrs>0 then inpX(4)=0 ! if sickHrs come from time clock, set the sick hours in entry as 0
		! if env$('client')='West Rest Haven' and j=5 then
		!   gpd+=inpX(j)*(hr(1)*1.50) : goto L2330 ! pay time and 1/2 on holiday pay
		! end if
		if j=2 then gpd+=inpX(j)*hr(2) else gpd+=inpX(j)*hr(1)
		L2330: !
	next j
	! if env$('client')='West Rest Haven' then
	!   inpX(7)=inpX(7)+round(sickHrs*(hr(1)*.50),2)
	!   sickHrs=0 ! place  double time portion of holiday overtime hours in other compensation, then clear the sick hours
	! end if
	if inpX(9)>0 and gpd+inpX(6)+inpX(7)+inpX(8)+inpX(9)<round((inpX(1)*mhw+inpX(2)*mhw*1.5),2) then inpX(7)=inpX(7)+round((inpX(1)*mhw+inpX(2)*mhw*1.5),2)-(gpd+inpX(6)+inpX(7)+inpX(8)+inpX(9))
	gpd+=inpX(6)+inpX(7) +inpX(8)+inpX(9) ! inpX(8) (meals) and inpX(9) tips both need to be added in for taxing purposes  they will be taken back out in S:\Payroll\Calc
	if ckey=5 and editMode then
		goto Accumulators ! just add proof totals back in
	else if editMode then
		rewrite #hTimesheet,using Ftimesheet,rec=rec(hTimesheet): eno,dep,mat inpX,gpd,mat hr norec WriteAndAccum
		goto Accumulators

	end if
		write #hTimesheet,using Ftimesheet: eno,dep,mat inpX,gpd,mat hr
		mat tinp=tinp+inpX
		tgp+=gpd

SkipDepartment: !
	if pullFromTimeCardSystem then goto L4300 ! pulling from time card system
	if tgp=0 then employee_lastPayrollDate=0 else employee_lastPayrollDate=payrollDate
	rewrite #hEmployee,using Femployee2,key=lpad$(str$(eno),8): employee_lastPayrollDate,tgp
	if heno=eno then goto L2490
	if tgp>0 then ent1=ent1+1
	L2490: !
	heno=eno
goto L1340 ! If ADR>0 Then Goto 1050 Else Goto 820
! /r
WriteAndAccum: !
	write #hTimesheet,using Ftimesheet: eno,dep,mat inpX,gpd,mat hr
goto Accumulators
FinishAndConfirm: ! r: and then ScrProofTotals
	close #hEmployee: ioerr ignore
	hEmployee=0
	close #hDept: ioerr ignore
	hDept=0
	close #hTimesheet: ioerr ignore
	hTimesheet=0
	! close #11: ioerr ignore
goto ScrProofTotals ! /r

ScrProofTotals: ! r:
	fn_addProofTotals(teno,count_employees_entered,mat tinp)
	fnTos
	respc=0 : mylen=20 : fraNum=0 : rc=0
	fnLbl(1,1,'P R O O F  T O T A L S',val(env$('SESSION_COLS')),2)
	! r: proof totals
	fnLbl(2,1,'Total Employees/Departments Entered: '&str$(count_employees_entered),60,2,0,fraNum)
	fnLbl(3,1,'Total Employee Numbers Entered: '&str$(teno),60,2,0,fraNum)
	fnLbl(5,1,'Regular Hours:',mylen,1,0,fraNum)
	fnTxt(5,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(1))
	fnLbl(6,1,'Overtime Hours:',mylen,1,0,fraNum)
	fnTxt(6,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(2))
	fnLbl(7,1,'Sick Hours:',mylen,1,0,fraNum)
	fnTxt(7,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(3))
	fnLbl(8,1,'Vacation Hours:',mylen,1,0,fraNum)
	fnTxt(8,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(4))
	fnLbl(9,1,'Holiday Hours:',mylen,1,0,fraNum)
	fnTxt(9,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(5))
	fnLbl(10,1,'Salary:',mylen,1,0,fraNum)
	fnTxt(10,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(6))
	fnLbl(11,1,'Other Compensation:',mylen,1,0,fraNum)
	fnTxt(11,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(7))
	fnLbl(12,1,'Meals:',mylen,1,0,fraNum)
	fnTxt(12,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(8))
	fnLbl(13,1,'Tips:',mylen,1,0,fraNum)
	fnTxt(13,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(tinp(9))
	fnLbl(15,1,'Reg Hourly Rate:',mylen,1,0,fraNum)
	fnTxt(15,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(hr(1))
	fnLbl(16,1,'O/T Hourly Rate:',mylen,1,0,fraNum)
	fnTxt(16,mylen+2,12,0,1,'10',1,'.',fraNum)
	resp$(rc+=1)=str$(hr(2))
	for j=1 to 10
		if trim$(dedName$(j))='' then name$(j)='' else name$(j)=trim$(dedName$(j))&':'
		fnLbl(j+4,25,trim$(name$(j)),mylen,1,0,fraNum)
		fnTxt(j+4,47,12,0,1,'10',1,'.',fraNum)
		resp$(rc+=1)=str$(tinp(j+9))
	next j
	for j=11 to 20
		if trim$(dedName$(j))='' then name$(j)='' else name$(j)=trim$(dedName$(j))&':'
		fnLbl(j-10+4,25+30,trim$(name$(j)),mylen,1,0,fraNum)
		fnTxt(j-10+4,47+30,12,0,1,'10',1,'.',fraNum)
		resp$(rc+=1)=str$(tinp(j+9))
	next j
	! /r
	! r: add timesheet listview
		dim dh_ch$(0)*18
		dim dh_cm$(0)
		mat dh_ch$(10)
		mat dh_cm$(10)
		dim item$(10)*64
		dh_ch$(1 )='Rec'         	: dh_cm$(1 )='30'
		dh_ch$(2 )='Emp'         	: dh_cm$(2 )=''
		dh_ch$(3 )='Dept'        	: dh_cm$(3 )=''
		dh_ch$(4 )='Hours'       	: dh_cm$(4 )='32'
		dh_ch$(5 )='OT'          	: dh_cm$(5 )='32'
		dh_ch$(6 )='Sick'        	: dh_cm$(6 )='32'
		dh_ch$(7 )='Vac'         	: dh_cm$(7 )='32'
		dh_ch$(8 )='Holiday'     	: dh_cm$(8 )='32'
		dh_ch$(9 )='Salary'      	: dh_cm$(9 )='10'
		dh_ch$(10)='Other Comp'  	: dh_cm$(10)='10'
		fnFlexInit1('timesheet',18,1,5,70,mat dh_ch$,mat dh_cm$,1)
		open #hSptTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],shr',i,i
		! restore #hTimesheet:
		do
			read #hSptTimesheet,using Ftimesheet: eno,dep,mat inpX,gpd,mat hr eof SaoeFlexFinis2
			item$(1 )=lpad$(str$(rec(hSptTimesheet)),6)
			item$(2 )=rtrm$(lpad$(str$(eno),6)&'  '&fnEmployeeName$(eno)     	)
			item$(3 )=rtrm$(lpad$(str$(dep),3)&'  '&fnDepartmentName$(dep)   	)
			item$(4 )=str$(inpX(1))
			item$(5 )=str$(inpX(2))
			item$(6 )=str$(inpX(3))
			item$(7 )=str$(inpX(4))
			item$(8 )=str$(inpX(5))
			item$(9 )=str$(inpX(6))
			item$(10)=str$(inpX(7))
			fnFlexAdd1(mat item$)
		loop
		SaoeFlexFinis2: !
		close #hSptTimesheet:
		fnEmployeeDataClose
		resp$(resp_flex=rc+=1)=''
	! /r
	! fnCmdKey('Co&rrections',ck_corrections=1,0,0,'Correct any entries.')
	! fnCmdKey('Add or Edit',ck_addOrEdit=6,0,1,'Add or Edit Time Sheet records')
	fnCmdKey('Add'       	,ck_addEmployee	=2,0,0,'Enter time for each department of an employee')
	fnCmdKey('Edit'      	,ck_edit        	=1,1,0,'Enter time on this employee' )
	fnCmdKey('Delete'    	,ck_delete     	=3,0,0,'Enter time on this employee' )
	fnCmdKey('&Listing'  	,ck_prListing  	=6,0,0,'Prints a listing of the entries you have made.')
	fnCmdKey('&Calculate'	,ck_calculate  	=8,0,0,'Calculates the pay.')
	! fnCmdKey('&Add',ck_add=4,0,0,'Add additional time. (If you missed a department, you should delete the original entries on that employee and completely re-enter the employee time.')
	fnCmdKey('E&xit',5,0,1,'Exit without calculating')
	ckey=fnAcs(mat resp$) ! proof totals
	pullFromTimeCardSystem=0
	if ckey=5 then goto XitWOCAL
	tsRec=val(resp$(resp_flex))
	if ckey=ck_edit then
		gosub OpenFiles
		read #hTimesheet,using Ftimesheet,rec=tsRec: eno,dep,mat inpX,gpd,mat hr
		editMode=1
		goto ScrAskTime ! ScrEnterTime ! from ScrAskEmployee [Next]
	else if ckey=ck_addEmployee then
		gosub OpenFiles
		editMode=0
		goto ScrAskEmployee
	else if ckey=ck_delete then
		if fnConfirmDelete('Record '&str$(tsRec), 'timeSheetRecDel'&date$('ccyymmdd')) then
			open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno],shr',i,outIn,k
			delete #hTimesheet,rec=tsRec:
			close #hTimesheet:
		end if
		! goto ScrProofTotals
	else if ckey and ckey=ck_prListing then
		gosub OpenFiles
		goto PrintListing
	! else if ckey and ckey=ck_addOrEdit then
	! 	gosub OpenFiles
	! 	goto ScrAddOrEdit
	! else if ckey=ck_corrections then
	! 	goto ScrCorrections
	else if ckey=ck_calculate then
		goto GoCalculate
	else if ckey=ck_add then
		gosub OpenFiles
		goto ScrAskEmployee
	end if
goto ScrProofTotals ! /r
DeleteTimesheetEntry: ! r:
	delete #hTimesheet,rec=rec(hTimesheet): noRec AfterAccumulators
goto RewriteEmployeeTGP ! /r

Accumulators: ! r:
	tgp+=gpd
	teno+=eno
	mat tinp=tinp+inpX
goto AfterAccumulators ! /r
AfterAccumulators: ! r:
	if tgp then employee_lastPayrollDate=payrollDate else employee_lastPayrollDate=0
goto RewriteEmployeeTGP: ! /r
RewriteEmployeeTGP: ! r:
	rewrite #hEmployee,using Femployee2,key=lpad$(str$(eno),8): employee_lastPayrollDate,tgp
	editMode=0
goto READ_NEXT_DEPARTMENT ! /r

GoCalculate: ! r:
	fnPayPeriodEndingDate(fndate_mmddyy_to_ccyymmdd(payrollDate))
	if jobcost=1 then close #hJcPrh1,free:
fnChain('S:\Payroll\Calculation') ! /r

ProcessTimeCard: ! r:
	open #hTimeCard=4: 'Name='&pathToTimeCard$&'timecard\simplesummary,KFName='&pathToTimeCard$&'timecard\ssindex,Shr',i,outIn,k ioerr L4200 ! timecard
	timecard=1 ! timecard files exist
	L4200: !
	dim en$*8
	read #hEmployee,using Femployee3: eno,em$,empStatus,em8,em9,lpd,tgp eof FinishAndConfirm
	if empStatus=9 then goto L4200 ! must use employment status code = 9 for terminated
	! if env$('client')='West Rest Haven' and empStatus=2 then goto L4200 ! wrh uses code 2 for terminated
	tgp=0
! If pullFromTimeCardSystem AND empStatus=1 Then Goto 3590 ! employment status on salaries people must be 1
! If empStatus><pullFromTimeCardSystem Then Goto 3552
	! pr f '16,20,C 60': str$(eno)&'  '&em$
L4290: !
	restore #hDept,key>=cnvrt$('pic(zzzzzzz#)',eno)&'   ':
L4300: !
	read #hDept,using 'form pos 1,n 8,n 3,pos 58,23*PD 4.2': depeno,dep,mat tdet
	if depeno<>eno then goto TheNextOne
	regHrs=otHrs=vacHrs=sickHrs=holHrs=otherComp=0 ! timecard
	if timecard=1 then
		tcKey$=lpad$(str$(eno),8)&cnvrt$('n 3',dep)&cnvrt$('n 5',cno) ! timecard
		read #hTimeCard,using 'form pos 17,6*pd 5.2',key=tcKey$: regHrs,otHrs,vacHrs,sickHrs,holHrs,otherComp nokey TimeCardNoKey ! timecard
	end if
	! if env$('client')='West Rest Haven' then gosub WRH_SIMPLE_OFFSET_HOLIDAY
goto L4400
TimeCardNoKey: !
	! tdet(1) Department Salary
	if empStatus=1 and tdet(1)>0 then goto L4400 ! calculate salaries even if no time in time card file; skip any hourly people who do not have any entries from the time card system
goto L4290 ! If NTA>0 Then aDR=NTA: Goto 3870 Else Goto 2010 ! circle if more than one department on hourly people; else skip if no time and no more departments
L4400: ! r: create Timesheet record from
	hr(1)=tdet(2)
	hr(2)=tdet(3)
	mat inpX=(0)
	inpX(1)=regHrs
	inpX(2)=otHrs
	inpX(3)=vacHrs
	inpX(4)=sickHrs
	inpX(5)=holHrs
	inpX(7)=otherComp ! timecard
	inpX(6)=tdet(1)
	for j=1 to 20
		inpX(j+7)=tdet(j+3)
		if skipIt(j)=1 then inpX(j+7)=0
	next j
goto PostDeptRewrite ! /r
ScrCorrections: ! r:
	editMode=1
	fnTos
	respc=0
	fnLbl(1,1,'Employee to Correct:',22,right)
	fnCmbEmp(1,24)
	resp$(respc+=1)=''
	fnCmdKey('&Next',1,1,0,'Make ScrCorrections to this employee''s time.' )
	fnCmdKey('&Finish',6,0,1,'Finished making ScrCorrections')
	ckey=fnAcs(mat resp$) ! ask employee #
	if ckey=6 then editMode=0 : goto ScrProofTotals ! finished corretions
	eno=ent=val(resp$(1)(1:8))
	read #hTimesheet,using Ftimesheet,key>=cnvrt$('pic(ZZZZZZZZ)',eno)&cnvrt$('pic(ZZZ)',0),release: depeno,dep2,mat inpX,gpd,mat hr nokey L4790
	if eno<>depeno then goto L4790
	if eno=0 then goto ScrCorrections
	goto L4840
L4790: !
	mat ml$(2)
	ml$(1)='No time has been entered on employee number '&str$(eno)&'.'
	ml$(2)='Do you wish to enter new time on this employee? '
	fnMsgBox(mat ml$,resp$,'',52)
	if resp$(1:1)='Y' then
		goto ScrEnterTime ! from ScrCorrections
	else
		goto ScrCorrections
	end if
READ_NEXT_DEPARTMENT: !
	read #hTimesheet,using Ftimesheet,release: depeno,dep2,mat inpX,gpd,mat hr nokey ScrCorrections eof ScrCorrections
L4840: !
	if depeno<>eno then goto ScrCorrections
	em$=''
	read #hEmployee,using Femployee1,key=lpad$(str$(eno),8): em$,empStatus,em8,em9,lpd,tgp nokey ignore
	teno=teno-eno ! remove from proof totals
	mat tinp=tinp-inpX
	dep=dep2 ! fix dept # on correction screen
	tgp=tgp-gpd
goto ScrAskTime ! /r


Xit: fnXit
XITWOCAL: ! r:
	mat ml$(2)
	ml$(1)='To save your changes, next time you choose to Enter Time Sheets'
	ml$(2)='you must select Additions to Previous Input.'
	fnMsgBox(mat ml$,resp$)
	goto Xit
! /r
! DUPLICATE_DEPARTMENTS: ! r:
! L5250: !
!  read #hTimesheet,using L5260,key=cnvrt$('pic(ZZZZZZZZ)',eno)&cnvrt$('pic(ZZZ)',dep),release: transeno,transdep nokey L5280
! L5260: form pos 1,n 8,n 3
!  goto L5250
! L5280: mat ml$(4)
!  ml$(1)='You can not enter time to the same department'
!  ml$(2)='on the same employee. Choose a different department '
!  ml$(3)='or choose to make ScrCorrections to fix the previous entry. '
!  ml$(4)='Click OK to return to previous screen. '
!  fnMsgBox(mat ml$,resp$)
! /r goto TheNextOne
! WRH_SIMPLE_OFFSET_HOLIDAY: ! r: offset holiday hours for West Rest Haven
!   if sickHrs>0 then
!     otHrs=otHrs-sickHrs
!     regHrs=regHrs-(max(0,holHrs-sickHrs)) ! wrh places any holiday hours considered overtime in the sick hours column.  The holiday hours are duplicated either in the reg hours or the ot hours.  this is how we decide which
!   end if
!   if sickHrs=0 then regHrs=regHrs-holHrs ! their timeclock puts holiday hours in regHrs column or otHrs as well holiday column  (if no part belongs to otHrs, then take all from the regular hrs
! return  ! /r
PullFromJobCost: ! r:
	! h(1)=emp#,h(2)=method,h(3)=dept#,h(4)=regHrs,h(5)=ot hrs,h(6)=salary,h(7)=ded #
	gosub SortIt
	open #hJcPrh1=fnH: 'Name=[Q]\PRmstr\JCPRH1.h[cno]',i,i,r ! #5
	open #hAddr=fnH: 'Name=[Temp]\Addr.'&session$,i,i ! #6
	open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],RecL=167,Replace',internal,output ! #3
	close #hTimesheet:
	fnIndex('[Q]\PRmstr\timesheet[acsUserId].h[cno]','[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno]','1,11')
	open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno]',i,outIn,k
	holdeno=eno=holddep=dep=0
	dim h(7)
	mat h=(0)
L5520: !
	holdeno=h(1): holddep=h(3)
	read #hAddr,using 'form pos 1,pd 3': jci eof EoAddr
	read #hJcPrh1,using 'form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6',rec=jci: mat h,dt2,jn$ noRec L5520
	if h(1)=0 then goto L5520 ! don't allow entry without employee #
	if rec(6)>1 and (h(1)><eno or holddep><h(3)) then goto L5820 ! first record or not same emp # or not same dept#
L5590: !
	h2=h(2)
	if h2=1 then goto L5630 ! salary only
	inpX(1)=inpX(1)+h(4) ! accumulate hours
	inpX(2)=inpX(2)+h(5)
	L5630: !
	eno=h(1)
	dep=h(3)
	if h(7)=0 then goto L5660 else inpX(h(7)+7)=inpX(h(7)+7)+tdet(h(7)-3)
	L5660: !
	if h2=1 or h2=3 then inpX(6)=inpX(6)+h(6)
	if h(7)=21 and h(6)>0 then inpX(7)=inpX(7)+h(6)
	if eno=0 then goto L5520
	read #hEmployee,using 'form pos 9,c 30,pos 126,2*pd 3.3,pos 168,pd 5.2',key=lpad$(str$(eno),8): em$,em8,em9,tgp nokey L5710
	goto L5720
	L5710: ! r:
		mat ml$(2)
		ml$(1)='Can''t find an employee record for employee # '&trim$(em$)&'!'
		ml$(2)='Time was entered on '&cnvrt$('pic(zz/zz/zz',dte)
		ml$(3)='Time for this employee will be skipped.'
		fnMsgBox(mat ml$,resp$)
	goto L5520 ! /r
	L5720: !
	if eno<>holdeno then tgp=0
	read #hDept,using 'form pos 58,24*pd 4.2',key=cnvrt$('pic(ZZZZZZZ#)',eno)&cnvrt$('pic(ZZ#)',dep): mat tdet
	if h2=1 or h2=3 then inpX(6)=tdet(1)
	for j=1 to 20
		inpX(j+9)=inpX(j+9)+tdet(j+3)
	next j
	if (holdeno=0 and eno>0) or ( holdeno=h(1) and holddep=h(3)) then goto L5520 ! read another record to see if same employee and department or to handle first record

	L5820: !
	for j=1 to 5
		if j=2 then
			gpd+=(inpX(j)*tdet(3))
		else
			gpd+=(inpX(j)*tdet(2))
		end if
	next j
	gpd+=inpX(6)+inpX(7)+inpX(8)+inpX(9)
	hr(1)=tdet(2)
	hr(2)=tdet(3)
	write #hTimesheet,using Ftimesheet: holdeno,holddep,mat inpX,gpd,mat hr
	mat tinp=tinp+inpX
	tgp+=gpd
	gpd=0
	if tgp=0 then employee_lastPayrollDate=0 else employee_lastPayrollDate=payrollDate
	rewrite #hEmployee,using 'form pos 162,n 6,pd 5.2',key=lpad$(str$(eno),8): employee_lastPayrollDate,tgp
	if holdeno=eno then goto L5960
	if tgp>0 then ent1+=1
	L5960: !
	holdeno=h(1) : holddep=h(3) : mat inpX=(0)
	if eofCode then eofCode=0 : goto L6000
goto L5590
EoAddr: eofCode=1 : goto L5820 ! allow last entry to post
L6000: !
goto ScrProofTotals
! /r
SortIt: ! r:
	open #15: 'Name=[Temp]\Sort[Session].tmp,RecL=128,Replace',internal,output
	write #15,using 'form pos 1,c 128': 'FILE [Q]\PRmstr\JCPRH1.h[cno],,,[Temp]\Addr.[Session],,,acsPR,,A,N'
	write #15,using 'form pos 1,c 128': 'MASK 1,8,N,A,10,2,PD,A'
	close #15:
	close #hAddr: ioerr ignore
	hAddr=0
	execute 'FREE [Temp]\Addr.[Session] -n' ioerr ignore
	execute 'SORT [Temp]\Sort[Session].tmp -n'
return  ! /r
ScrAskEmployee: ! r:
	editMode=0
	fnTos
	respc=0
	fnLbl(1,1,'Employee:',11,right)
	fnCmbEmp(1,13)
	resp$(respc+=1)=''
	fnCmdKey('&Next',1,1,0,'Enter time on this employee' )
	fnCmdKey('&Search',2,0,0,'Search for employee record')
	fnCmdKey('&Finish',6,0,1,'Finished entering hours')
!                     fnCmdKey('E&xit',5,0,1,'Returns to menu') !   fix kj
	ckey=fnAcs(mat resp$) ! ask employee #
	eno=ent=val(resp$(1)(1:8))
	if ckey=1 then
		goto ScrEnterTime ! from ScrAskEmployee [Next]
	else if ckey=2 then
		fnEmployeeSrch(x$,fixgrid)
		eno=val(x$)
		goto ScrEnterTime ! from ScrAskEmployee [Search]
	else if ckey=5 or ckey=6 then
		goto FinishAndConfirm
	else
		goto FinishAndConfirm
	end if
! /r
! /r
! ScrAddOrEdit: ! r:
! 	editMode=0
! 	fnTos
! 	respc=0
! 	fnLbl(1,1,'Select record to edit or add a new employee')
! 	! r: add timesheet listview
! 		dim dh_ch$(0)*18
! 		dim dh_cm$(0)
! 		mat dh_ch$(10)
! 		mat dh_cm$(10)
! 		dh_ch$(1 )='Rec'                 	: dh_cm$(1 )=''
! 		dh_ch$(2 )='Employee'            	: dh_cm$(2 )=''
! 		dh_ch$(3 )='Department'          	: dh_cm$(3 )=''
! 		dh_ch$(4 )='Regular Hours'      	: dh_cm$(4 )='32'
! 		dh_ch$(5 )='Overtime Hours'     	: dh_cm$(5 )='32'
! 		dh_ch$(6 )='Sick Hours'          	: dh_cm$(6 )='32'
! 		dh_ch$(7 )='Vacation Hours'     	: dh_cm$(7 )='32'
! 		dh_ch$(8 )='Holiday Hours'      	: dh_cm$(8 )='32'
! 		dh_ch$(9 )='Salary'              	: dh_cm$(9 )='10'
! 		dh_ch$(10)='Other Compensation' 	: dh_cm$(10)='10'
!
! 		fnFlexInit1('timesheet',5,1,10,70,mat dh_ch$,mat dh_cm$,1)
! 		restore #hTimesheet:
! 		do
! 			read #hTimesheet,using Ftimesheet: eno,dep,mat inpX,gpd,mat hr eof SaoeFlexFinis
! 			item$(1 )=str$(rec(hTimesheet))
! 			item$(2 )=str$(eno)
! 			item$(3 )=str$(dep)
! 			item$(4 )=str$(inpX(1))
! 			item$(5 )=str$(inpX(2))
! 			item$(6 )=str$(inpX(3))
! 			item$(7 )=str$(inpX(4))
! 			item$(8 )=str$(inpX(5))
! 			item$(9 )=str$(inpX(6))
! 			item$(10)=str$(inpX(7))
! 			fnFlexAdd1(mat item$)
! 		loop
! 		SaoeFlexFinis: !
! 		resp$(1)=''
! 	! /r
! 	! fnCmbEmp(1,13)
! 	! resp$(respc+=1)=''
! 	fnCmdKey('Add'    	,ck_addEmployee=2,0,0,'Enter time for each department of an employee')
! 	fnCmdKey('Edit'   	,ck_edit=1,1,0,'Enter time on this employee' )
! 	fnCmdKey('Delete' 	,ck_delete=3,0,0,'Enter time on this employee' )
! 	fnCmdKey('&Finish'	,6,0,1,'Finished entering hours')
! !                     fnCmdKey('E&xit',5,0,1,'Returns to menu') !   fix kj
! 	ckey=fnAcs(mat resp$) ! ask employee #
! 	tsRec=val(resp$(1))
! 	if ckey=ck_edit then
! 		read #hTimesheet,using Ftimesheet,rec=tsRec: eno,dep,mat inpX,gpd,mat hr
! 		goto ScrAskTime ! ScrEnterTime ! from ScrAskEmployee [Next]
! 	else if ckey=ck_addEmployee then
! 		goto ScrAskEmployee
! 	else if ckey=ck_delete then
! 		pr 'delete chosen, no logic yet' : pause
! 		if fnConfirmDelete('time sheet record', 'timeSheetRecordDelete'&date$) then
! 			delete #hTimesheet,rec=tsRec:
! 		end if
! 		goto ScrAddOrEdit
! 	else if ckey=5 or ckey=6 then
! 		goto FinishAndConfirm
! 	else
! 		goto FinishAndConfirm
! 	end if
! ! /r
	def fn_addProofTotals(&apt_total_employee_numbers,&apt_count_employees_entered,mat tinp; ___,hTimesheet)
		open #hTimesheet=fnH: 'Name=[Q]\PRmstr\timesheet[acsUserId].h[cno],KFName=[Q]\PRmstr\timesheet[acsUserId]Idx.h[cno]',i,i,k ioerr APT_FINIS
		apt_heno=0 ! temp variable for internal comparison
		apt_total_employee_numbers=0 ! total of all (unique) employee numbers entered
		apt_count_employees_entered=0 ! total unique employees entered
		mat tinp=(0)
! restore #hTimesheet:
		do
			read #hTimesheet,using Ftimesheet: eno,dep,mat inpX,gpd,mat hr eof APT_FINIS
			if apt_heno<>eno then
				apt_total_employee_numbers=apt_total_employee_numbers+eno
				apt_count_employees_entered+=1
			end if
			mat tinp=tinp+inpX
			apt_heno=eno
		loop
APT_FINIS: !
		close #hTimesheet: ioerr ignore
	fnend
PrintListing: ! r:
	heno=r=pc=teno=ent1=0
	mat tinp=(0)
	if additional=2 then goto L3160
	fncreg_read('enter time sheets proof sequence',printorder$,'1') : printorder=val(printorder$) conv ignore
	PrList_Screen: !
	fnTos
	fnLbl(1,1,'Sequence:',11,right)
	fnOpt(1,13,'Account Order',0) : if printorder<>2 then resp$(1)='True' else resp$(1)='False'
	fnOpt(2,13,'Order Entered',0) : if printorder=2 then resp$(2)='True' else resp$(2)='False'
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	ckey=fnAcs(mat resp$)
	if resp$(1)='False' and resp$(2)='False' then goto PrList_Screen
	if resp$(1)='True' then printorder=1 else printorder=2
	fncreg_write('enter time sheets proof sequence',str$(printorder))
	fnOpenPrn
	L3160: !
	restore #hTimesheet: : record=0
	pc2=0
	do
		PL_Read: !
		if printorder=2 then
		L3200: !
			record+=1 : if record>lrec(hTimesheet) then goto PL_Finis
			read #hTimesheet,using Ftimesheet,rec=record,release: eno,dep,mat inpX,gpd,mat hr eof PL_Finis noRec L3200
		else
			read #hTimesheet,using Ftimesheet,release: eno,dep,mat inpX,gpd,mat hr eof PL_Finis
		end if
		if heno<>eno then
			read #hDept,using 'form pos 42,n 6',key=cnvrt$('pic(ZZZZZZZ#)',eno)&cnvrt$('pic(ZZ#)',dep),release: lastprdate
			if lastprdate=payrollDate then ! make sure pay hasn't been calculated on this person on this date
				mat ml$(4)
				ml$(1)='You have previously calculated pay using this same payroll date on employee '&str$(eno)
				ml$(2)='You must delete this person''s time for now and either reverse the previous calculation '
				ml$(3)='or enter the time using a differen payroll date. '
				ml$(4)='                         Click OK to continue. '
				fnMsgBox(mat ml$,resp$)
				if additional=2 then
					delete #hTimesheet,rec=rec(hTimesheet): noRec L3270
					goto PL_Read
				end if
			end if
			L3270: !
			teno+=eno
			ent1+=1
		end if
		mat tinp=tinp+inpX
		heno=eno
		if additional=2 then goto PL_Read
		if pc=9 then gosub PL_PrintEmpBlock
		pc=pc+1
		read #hEmployee,using Femployee1,key=lpad$(str$(eno),8),release: em$ nokey L3440
		em$=rtrm$(em$)
		for j1=len(em$) to 1 step -1
			if em$(j1:j1)=' ' then goto L3410
		next j1
		n1$(pc)=em$(1:10) : n2$=em$(12:22) : goto L3440
		L3410: !
		j2=min(j1,10)
		n1$(pc)=lpad$(rtrm$(em$(1:j2)),12)
		n2$(pc)=lpad$(rtrm$(em$(j1+1:j1+10)),12)
		L3440: !
		prX(pc,1)=eno
		prX(pc,2)=dep
		prX(pc,32)=gpd
		prX(pc,33)=r
		prX(pc,34)=hr(1)
		prX(pc,35)=hr(2)
		for j=1 to 29
			prX(pc,j+2)=inpX(j)
		next j
	loop

	PL_Finis: !
	if additional=2 then
		additional=1
		close #hTimesheet:
	else
		gosub PL_PrintEmpBlock
		fnClosePrn
	end if

goto ScrProofTotals ! /r
	PL_PrintEmpBlock: ! r:
		if pc2=3 then pc2=0
		pc2=pc2+1
		if pc2=1 then
			pr #255: ''
			pr #255,using 'form pos 1,c 25,cc 82,skip 1,c 25': date$,env$('cnam'),time$
		end if
		pr #255,using L3610: mat n1$
		pr #255,using L3610: mat n2$
		L3610: form pos 21,9*c 12,skip 1
		! pr #255,Using F1$: 'Record #     ',prX(1,23),prX(2,23),prX(3,23),prX(4,23),prX(5,23),prX(6,23),prX(7,23),prX(8,23),prX(9,23)
		pr #255,using f1$: 'Employee  ',prX(1,1),prX(2,1),prX(3,1),prX(4,1),prX(5,1),prX(6,1),prX(7,1),prX(8,1),prX(9,1)
		pr #255,using f1$: 'Department  ',prX(1,2),prX(2,2),prX(3,2),prX(4,2),prX(5,2),prX(6,2),prX(7,2),prX(8,2),prX(9,2)
		for j=1 to 29
			if trim$(sc1$(j))<>'' then
				pr #255,using f2$: sc1$(j),prX(1,j+2),prX(2,j+2),prX(3,j+2),prX(4,j+2),prX(5,j+2),prX(6,j+2),prX(7,j+2),prX(8,j+2),prX(9,j+2)
			end if
		next j
		pr #255,using f2$: 'Dept Gross Pay ',prX(1,32),prX(2,32),prX(3,32),prX(4,32),prX(5,32),prX(6,32),prX(7,32),prX(8,32),prX(9,32)
		pr #255,using f2$: 'Reg Hourly Rate',prX(1,34),prX(2,34),prX(3,34),prX(4,34),prX(5,34),prX(6,34),prX(7,34),prX(8,34),prX(9,34)
		! pr #255,Using F2$: 'O/T Hourly Rate',prX(1,35),prX(2,35),prX(3,35),prX(4,35),prX(5,35),prX(6,35),prX(7,35),prX(8,35),prX(9,35)
		if pc2=2 then pr #255: newpage else pr #255,using L3730: ' '
		L3730: form c 1,skip 2
		if pc2>1 then pc2=0
		mat prX=(0)
		mat n1$=('')
		mat n2$=('')
		pc=0
	return  ! /r


def fn_setup
	autoLibrary

	on error goto Ertn
	gosub Enum

	dim inpX(29)
	dim em$*30
	dim hr(2)
	dim n1$(9)
	dim n2$(9)
	dim tdet(23)
	dim tinp(29)

	dim prX(9,35)
	dim resp$(64)*128
	dim ml$(0)*128
	dim skipit$(20)*1,skipIt(20)


	dim tdt(4),tcd(3)

	dim dedName$(20)*20
	fnDedNames(mat dedName$)

	dim sc1$(31)*20
	sc1$(1)='Regular Hours '
	sc1$(2)='Overtime Hours'
	sc1$(3)='Sick Hours    '
	sc1$(4)='Vacation Hours'
	sc1$(5)='Holiday Hours '
	sc1$(6)='Salary        '
	sc1$(7)='Other Compensation'
	sc1$(8)='Meals'
	sc1$(9)='Tips '
	for j=1 to 20
		sc1$(j+9)=dedName$(j)
	next j
	sc1$(30)='Reg Hourly Rate'
	sc1$(31)='O/T Hourly Rate'
	dim f1$*400
	f1$='form pos 1,C 20' ! need this for edit list
	dim f2$*400
	f2$=f1$
	for j=1 to 9
		f1$=rtrm$(f1$)&',PIC(------------)'
		f2$=rtrm$(f2$)&',PIC(---------.--)'
	next j

	pathToTimeCard$='C:\progra~1\acs\'

	open #hCompany=fnH: 'Name=[Q]\PRmstr\Company.h[cno],Shr',i,i
	read #hCompany,using 'form pos 726,pd 3.2': mhw
	close #hCompany:
	hCompany=0

fnend

include: Enum
include: ertn
