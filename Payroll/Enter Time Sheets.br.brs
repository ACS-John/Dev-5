fn_setup
fnTop(program$)
 
d1=fnPayPeriodEndingDate
if days(d1,'ccyymmdd')=>days(date)-5 then prd=d1
 
open #h_rpwork=fnH: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],KFName=[Q]\PRmstr\prwork"&wsid$&"idx.h[cno]",internal,outIn,keyed ioerr ignore
F_rpwork: form pos 1,n 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
 
SCREEN_1: !
	fnTos
	rc=franum=0
	fnFra(1,1,3,50,"Payroll Time Sheet Entry","You would only add to previous entries if the last batch was not calculated.",0)
	franum+=1
	fnOpt(1,3,"Regular Time Sheet Entry",0,franum)     : resp$(rc+=1)="True"
	fnOpt(2,3,"Additions to Previous Input",0,franum)  : resp$(rc+=1)="False"
	fnFra(6,1,2,50,"Pay Period Ending Date","You must enter the pay perod ending date.  You can not have more than one payroll with the same date.")
	franum+=1 : mylen=23 : mypos=mylen+2
	fnLbl(1,1,"Pay Period Ending Date:",mylen,1,0,franum)
	fnTxt(1,mypos,10,0,1,"1",0,"Use mmddyy.",franum)   : resp$(rc+=1)=str$(prd)
	fnFra(10,1,6,50,"Method of Entry","You can select specific employees to pay; you can automatically calculate salaried persons; or you can pull from a another system.")
	franum+=1 : mylen=18 : mypos=mylen+2
	fnOpt(1,3,"Select employees to pay",0,franum)              : resp$(rc+=1)="True"
	fnOpt(2,3,"Automatically pay salaried employees",0,franum) : resp$(rc+=1)="False"
	fnOpt(3,3,"Pull time from time card system",0,franum)      : resp$(rc+=1)="False"
	fnOpt(4,3,"Pull time from job cost system",0,franum)       : resp$(rc+=1)="False"
	fnLbl(6,1,"Employment Status:",mylen,1,0,franum)
	fncombof("EmpStatus",6,mypos,25,"[Q]\PRmstr\EmpStatus.dat",1,2,3,25,"[Q]\PRmstr\EmpStatus.idx",0,0, "Only necessary if automatically paying salaried people. ",franum,0)
	resp$(rc+=1)=""
	if ~fnArrayEmpty(mat dednames$) then
		
		fnFra(18,1,10,50,"Skip Deductions This Pay Period","You can skip any deduction this pay period by checking the deduction below.")
		franum+=1
		linecnt=0
		resp_skipDedAdd=rc
		for j=1 to 19 step 2
			if trim$(dednames$(j))<>"" then x$=":" else x$=""
			fnChk(linecnt+=1,20,trim$(dednames$(j))&x$,1,franum) : resp$(rc+=1)="False"
			if trim$(dednames$(j+1))<>"" then x$=":" else x$=""
			fnChk(linecnt,45,trim$(dednames$(j+1))&x$,1,franum)  : resp$(rc+=1)="False"
		next j
		
	end if
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to customer record")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)="True" then ! Regular Time Sheet Entry
		noauto=ti1=1
		additional=1
	else if resp$(2)="True" then ! Additions to Previous Input
		noauto=ti1=2
		additional=2
	end if
	prd=val(resp$(3))
 
	if resp$(4)="True" then
		noauto=ti1=1
	else if resp$(5)="True" then
		noauto=ti1=2
	else if resp$(6)="True" then
		pullFromTimeCardSystem=99
	else if resp$(7)="True" then
		jobcost=1
	end if
	em4=val(resp$(8)(1:2))
	if fnArrayEmpty(mat dednames$) then
		mat resp$(1+resp_skipDedAdd:20+resp_skipDedAdd)=('False')
		mat skipit$(1:20)=('N')
	else
		for j=1 to 20
			if resp$(j+resp_skipDedAdd)="True" then skipit$(j)="Y" else skipit$(j)="N"
		next j
	end if
 
	if prd=0 then
		mat ml$(0)
		fnAddOneC(mat ml$,"You must enter a valid payroll date!"   )
		fnAddOneC(mat ml$,"Click OK to return to previous screen. ")
		fnmsgbox(mat ml$,resp$)
		goto SCREEN_1
	else if days(prd,'mmddyy')<=days(date)-45 then
		mat ml$(0)
		fnAddOneC(mat ml$,'You have choosen a Pay Period Ending Date that is')
		fnAddOneC(mat ml$,'more than 45 days prior to today.')
		fnAddOneC(mat ml$,'Do you wish to continue?')
		fnmsgbox(mat ml$,resp$,'',mb_question+mb_yesno+mb_button2_default)
		if resp$='No' then goto SCREEN_1
	end if
 
! SKIPDEDUCTIONS: !
	! if fnArrayEmpty(mat dednames$) then
	! 	mat resp$(1:20)=('False')
	! 	mat skipit$(1:20)=('N')
	! else
	! 	fnTos
	! 	rc=franum=linecnt=0
	! 	fnFra(1,1,10,50,"Skip Deductions This Pay Period","You can skip any deduction this pay period by checking the deduction below.")
	! 	franum+=1
	! 	resp_skipDedAdd=rc
	! 	for j=1 to 19 step 2
	! 		if trim$(dednames$(j))<>"" then x$=":" else x$=""
	! 		fnChk(linecnt+=1,20,trim$(dednames$(j))&x$,1,franum) : resp$(rc+=1)="False"
	! 		if trim$(dednames$(j+1))<>"" then x$=":" else x$=""
	! 		fnChk(linecnt,45,trim$(dednames$(j+1))&x$,1,franum)  : resp$(rc+=1)="False"
	! 	next j
	! 	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	! 	fnCmdKey("&Cancel",5,0,1,"Returns to customer record")
	! 	fnAcs(mat resp$,ckey)
	! 	if ckey<>5 then
	! 		for j=1 to 20
	! 			if resp$(j+resp_skipDedAdd)="True" then skipit$(j)="Y" else skipit$(j)="N"
	! 		next j
	! 	end if
	! end if
	if noauto<>2 then em4=0 ! don't allow any employment status code if not selecting to automatically pay salaried
	if (~exists("[Q]\PRmstr\rpwork[unique_computer_id].h[cno]") and additional=2) or additional<>2 then
		open #h_rpwork=fnH: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],RecL=167,Replace",internal,output
		close #h_rpwork:
	end if
	fnIndex("[Q]\PRmstr\rpwork[unique_computer_id].h[cno]","[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno]","1,11")
	fnStatusClose
	if additional=2 then
!   if exists("[Q]\PRmstr\rpwork[unique_computer_id].h[cno]") then
		gosub OpenFiles
		gosub PRINT_LISTING
!    end if
		goto PROOF_TOTALS
	end if
! close #h_rpwork,free: ioerr ignore
! open #h_rpwork=fnH: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],RecL=167,Replace",internal,output
! close #h_rpwork:
! execute "Index [Q]\PRmstr\rpwork[unique_computer_id].h[cno]"&' '&"[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno] 1,11 replace,DupKeys -N"
	gosub OpenFiles
	if jobcost=1 then goto PullFromJobCost
TheNextOne: !
	if pullFromTimeCardSystem then
		goto L4180
	end if
goto ASK_EMPLOYEE
 
ENTER_TIME: !
	en$=lpad$(str$(eno),8)
	read #hEmployee,using F_employee_1,key=en$: em$,em4,em8,em9,lpd,tgp nokey TheNextOne
	if editmode=1 then goto READ_DEPARTMENTS
	if prd=lpd then goto EMP_PREV_ENTERED_WARN
	L1290: !
	! goto DUPLICATE_DATE_TEST
	! DUPLICATE_DATE_TEST: ! r: ! dont allow to calculate if reversing calculation needs to be run
	restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey DUPLICATE_DATE_TEST_XIT
	do
		read #2,using 'FORM POS 1,n 8,POS 42,n 6': depeno,tdt4 eof DUPLICATE_DATE_TEST_XIT
		if depeno<>eno then goto DUPLICATE_DATE_TEST_XIT
		if tdt4=prd then
			mat ml$(0)
			fnAddOneC(mat ml$,"You have previously calculated pay using this same payroll date on employee # "&x$)
			fnAddOneC(mat ml$,"You must either use a different date or reverse the previous calculation. "       )
			fnAddOneC(mat ml$,"Click OK to return to previous screen. "                                          )
			fnmsgbox(mat ml$,resp$)
			goto TheNextOne
		end if
	loop
	DUPLICATE_DATE_TEST_XIT: !
	! goto READ_DEPARTMENTS ! /r
 
	READ_DEPARTMENTS: !
	tgp=0
	restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ":
	L1340: !
	if goprev=0 then
		read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,dep,gl$,mat tdt,mat tcd,tli,mat tdet eof ASK_EMPLOYEE
	else if goprev=1 then
		semp-=1
		goprev=0
		read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',prior: teno,dep,gl$,mat tdt,mat tcd,tli,mat tdet eof ASK_EMPLOYEE
	end if
	if teno=eno and goprev=0 then semp+=1
	if teno<>eno then semp=0
	if teno<>eno then goto ASK_EMPLOYEE
	hr(1)=tdet(2) : hr(2)=tdet(3) ! set hourly rates from dept rec
	simplekey$=en$&cnvrt$("n 3",dep)&cnvrt$("n 5",cno) ! timecard
	reghrs=othrs=vachrs=sickhrs=holhrs=othercomp=0 ! timecard
	if timecard=1 then
		read #4,using 'form pos 17,6*pd 5.2',key=simplekey$: reghrs,othrs,vachrs,sickhrs,holhrs,othercomp nokey ignore ! timecard
	end if
	! dim shd$*78
	! shd$="Employee # "&ltrm$(en$)&"   Name "&rtrm$(em$)&"    Department # "&str$(dep)
	if editmode=1 then goto L1450
	goto L1490
	L1450: !
	tdet(2)=hr(1)
	tdet(3)=hr(2)
	tgp=tgp-gpd
	goto ASK_TIME
	L1490: !
	mat inpX=(0)
	inpX(1)=reghrs
	inpX(2)=othrs
	inpX(3)=vachrs
	inpX(4)=sickhrs
	inpX(5)=holhrs
	inpX(7)=othercomp ! timecard
	inpX(6)=tdet(1)
	for j=1 to 20
		inpX(j+9)=tdet(j+3)
		if skipit(j)=1 then inpX(j+9)=0
	next j
	ASK_TIME: !
	fnTos
	respc=0: mylen=20: franum=0: rc=0
	fnLbl(1,1,"Employee Number: "&str$(eno),60,2,0,franum)
	fnLbl(2,1,"Employee Name: "&rtrm$(em$),60,2,0,franum)
	fnLbl(3,1,"Department Number: "&str$(dep)&" "&fnDeptName$(dep),60,2,0,franum)
	fnLbl(5,1,"Regular Hours:",mylen,1,0,franum)
	fnTxt(5,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(1))
	fnLbl(6,1,"Overtime Hours:",mylen,1,0,franum)
	fnTxt(6,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(2))
	fnLbl(7,1,"Sick Hours:",mylen,1,0,franum)
	fnTxt(7,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(3))
	fnLbl(8,1,"Vacation Hours:",mylen,1,0,franum)
	fnTxt(8,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(4))
	fnLbl(9,1,"Holiday Hours:",mylen,1,0,franum)
	fnTxt(9,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(5))
	fnLbl(10,1,"Salary:",mylen,1,0,franum)
	fnTxt(10,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(6))
	fnLbl(11,1,"Other Compensation:",mylen,1,0,franum)
	fnTxt(11,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(7))
	fnLbl(12,1,"Meals:",mylen,1,0,franum)
	fnTxt(12,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(8))
	fnLbl(13,1,"Tips:",mylen,1,0,franum)
	fnTxt(13,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(inpX(9))
	fnLbl(15,1,"Reg Hourly Rate:",mylen,1,0,franum)
	fnTxt(15,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(hr(1))
	fnLbl(16,1,"O/T Hourly Rate:",mylen,1,0,franum)
	fnTxt(16,mylen+2,12,0,1,"10",0,".",franum)
	resp$(rc+=1)=str$(hr(2))
	for j=1 to 20
		if trim$(dednames$(j))="" then name$(j)="" else name$(j)=trim$(dednames$(j))&":"
		if skipit$(j)="Y" then inpX(j+9)=0
		disable_deduction=0 : if trim$(name$(j))='' then disable_deduction=1
		fnLbl(j+4,25,trim$(name$(j)),mylen,1,0,franum)
		fnTxt(j+4,47,12,0,1,"10",disable_deduction,".",franum)
		resp$(rc+=1)=str$(inpX(j+9))
	next j
	fnCmdKey("&Next",1,1,0,"Record this time" )
	if editmode=0 then fnCmdKey("&Skip Department F2",2,0,0,"Skips this department.")
	if editmode=0 and semp>=1 then fnCmdKey("&Prev Department",12,0,0,"Go back to last department.")
	if editmode=1 then fnCmdKey("&Delete Department",10,0,0,"Deletes the hours, etc for this department.")
	if editmode=0 then fnCmdKey("&Track Hours",8,0,0,"Track hours other than those entered above.")
	fnCmdKey("&Make Changes Permanent",3,0,0,"Makes any rate changes or other deductions changes permanent in the employee record.")
	if editmode=0 then fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	if editmode=1 then fnCmdKey("&Finish",7,0,1,"Finished making corrections")
	fnAcs(mat resp$,ckey) ! ask time
	if ckey=5 and editmode=0 then goto FINISH
	for j=1 to 9
		inpX(j)=val(resp$(j))
	next j
	hr(1)=val(resp$(10))
	hr(2)=val(resp$(11))
	for j=12 to 31
		inpX(j-2)=val(resp$(j))
	next j
	if ckey=8 then fnhours(eno) : goto ASK_TIME !  breakdown=1 : goto ASK_TIME
	if ckey=5 and editmode=1 then goto L2290
	if ckey=10 and editmode=1 then goto DELETE_IT
	if ckey=2 then goto L2430
	if ckey=12 then goprev=1 : goto L1340
	if ckey<>3 then goto L2220
	tdet(1)=inpX(6)
	for j=1 to 20
		tdet(j+3)=inpX(j+9)
	next j
	tdet(2)=hr(1)
	tdet(3)=hr(2)
	rewrite #2,using 'form pos 9,n 3,pos 58,23*pd 4.2',key=cnvrt$("pic(ZZZZZZZZ)",eno)&cnvrt$("pic(ZZZ)",dep): dep,mat tdet
	L2220: !
	gpd=0
	if em8><-2 then goto L2260
	if inpX(3)=0 then goto L2260
	
	mat ml$(0)
	fnAddOneC(mat ml$,"This employee is not eligible for Sick Leave!")
	fnAddOneC(mat ml$,"Click OK to return to previous screen. "      )
	fnmsgbox(mat ml$,resp$)
	goto ASK_TIME
	L2260: !
	if em9><-2 then goto L2290
	if inpX(4)=0 then goto L2290
	mat ml$(0)
	fnAddOneC(mat ml$,"This employee is not eligible for Vacation!")
	fnAddOneC(mat ml$,"Click OK to return to previous screen. "    )
	fnmsgbox(mat ml$,resp$)
	goto ASK_TIME
L2290: !
	for j=1 to 5
		! if env$('client')="West Rest Haven" and sickhrs>0 then inpX(4)=0 ! if sickhrs come from time clock, set the sick hours in entry as 0
		! if env$('client')="West Rest Haven" and j=5 then
		!   gpd=gpd+inpX(j)*(hr(1)*1.50) : goto L2330 ! pay time and 1/2 on holiday pay
		! end if
		if j=2 then gpd=gpd+inpX(j)*hr(2) else gpd=gpd+inpX(j)*hr(1)
L2330: !
	next j
	! if env$('client')="West Rest Haven" then
	!   inpX(7)=inpX(7)+round(sickhrs*(hr(1)*.50),2)
	!   sickhrs=0 ! place  double time portion of holiday overtime hours in other compensation, then clear the sick hours
	! end if
	if inpX(9)>0 and gpd+inpX(6)+inpX(7)+inpX(8)+inpX(9)<round((inpX(1)*mhw+inpX(2)*mhw*1.5),2) then inpX(7)=inpX(7)+round((inpX(1)*mhw+inpX(2)*mhw*1.5),2)-(gpd+inpX(6)+inpX(7)+inpX(8)+inpX(9))
	gpd=gpd+inpX(6)+inpX(7) +inpX(8)+inpX(9) ! inpX(8) (meals) and inpX(9) tips both need to be added in for taxing purposes  they will be taken back out in S:\Payroll\Calc
	if ckey=5 and editmode=1 then goto L3960 ! just add proof totals back in
	if editmode=1 then goto REWRITE_WORK
	write #h_rpwork,using F_rpwork: eno,dep,mat inpX,gpd,mat hr
	mat tinp=tinp+inpX
	tgp=tgp+gpd
	L2430: !
	if pullFromTimeCardSystem then goto L4300 ! pulling from time card system
	if tgp=0 then ped=0 else ped=prd
	rewrite #hEmployee,using F_employee_2,key=en$: ped,tgp
	if heno=eno then goto L2490
	if tgp>0 then ent1=ent1+1
	L2490: !
	heno=eno
goto L1340 ! If ADR>0 Then Goto 1050 Else Goto 820
!
FINISH: !
	close #hEmployee: ioerr ignore
	close #2: ioerr ignore
	close #h_rpwork: ioerr ignore
	close #11: ioerr ignore
	close #109: ioerr ignore
	close #108: ioerr ignore
PROOF_TOTALS: !
	fn_add_proof_totals(teno,count_employees_entered,mat tinp)
	fnTos
	respc=0 : mylen=20 : franum=0 : rc=0
	fnLbl(1,1,"P R O O F  T O T A L S",60,2,0,franum)
	fnLbl(2,1,"Total Employees/Departments Entered: "&str$(count_employees_entered),60,2,0,franum)
	fnLbl(3,1,"Total Employee Numbers Entered: "&str$(teno),60,2,0,franum)
	fnLbl(5,1,"Regular Hours:",mylen,1,0,franum)
	fnTxt(5,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(1))
	fnLbl(6,1,"Overtime Hours:",mylen,1,0,franum)
	fnTxt(6,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(2))
	fnLbl(7,1,"Sick Hours:",mylen,1,0,franum)
	fnTxt(7,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(3))
	fnLbl(8,1,"Vacation Hours:",mylen,1,0,franum)
	fnTxt(8,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(4))
	fnLbl(9,1,"Holiday Hours:",mylen,1,0,franum)
	fnTxt(9,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(5))
	fnLbl(10,1,"Salary:",mylen,1,0,franum)
	fnTxt(10,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(6))
	fnLbl(11,1,"Other Compensation:",mylen,1,0,franum)
	fnTxt(11,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(7))
	fnLbl(12,1,"Meals:",mylen,1,0,franum)
	fnTxt(12,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(8))
	fnLbl(13,1,"Tips:",mylen,1,0,franum)
	fnTxt(13,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(tinp(9))
	fnLbl(15,1,"Reg Hourly Rate:",mylen,1,0,franum)
	fnTxt(15,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(hr(1))
	fnLbl(16,1,"O/T Hourly Rate:",mylen,1,0,franum)
	fnTxt(16,mylen+2,12,0,1,"10",1,".",franum)
	resp$(rc+=1)=str$(hr(2))
	for j=1 to 20
		if trim$(dednames$(j))="" then name$(j)="" else name$(j)=trim$(dednames$(j))&":"
		fnLbl(j+4,25,trim$(name$(j)),mylen,1,0,franum)
		fnTxt(j+4,47,12,0,1,"10",1,".",franum)
		resp$(rc+=1)=str$(tinp(j+9))
	next j
	fnCmdKey("Co&rrections",1,0,0,"Correct any entries.")
	fnCmdKey("&Listing",2,0,0,"Prints a listing of the entries you have made.")
	fnCmdKey("&Calculate",3,1,0,"Calculates the pay.")
	fnCmdKey("&Add",4,0,0,"Add additional time. (If you missed a department, you should delete the original entries on that employee and completely re-enter the employee time.")
	fnCmdKey("E&Xit",5,0,1,"Exit without calculating")
	fnAcs(mat resp$,ckey) ! proof totals
	pullFromTimeCardSystem=0
	cor=ckey
	if ckey=5 then goto XitWOCAL
	if ckey=1 or ckey=2 or ckey=4 then gosub OpenFiles
	on ckey goto CORRECTIONS,PRINT_LISTING,GOCALK,ASK_EMPLOYEE none PROOF_TOTALS
!
DELETE_IT: !
! eNO=0
! dEP=0
! Mat inpX=(0)
! gPD=0
	delete #h_rpwork,rec=rec(h_rpwork): noRec L3990
	goto L4000
REWRITE_WORK: !
	rewrite #h_rpwork,using F_rpwork,rec=rec(h_rpwork): eno,dep,mat inpX,gpd,mat hr
L3960: !
	tgp=tgp+gpd
	teno=teno+eno
	mat tinp=tinp+inpX
L3990: !
	if tgp=0 then ped=0 else ped=prd
L4000: !
	rewrite #hEmployee,using F_employee_2,key=en$: ped,tgp
	goto READ_NEXT_DEPARTMENT
! rp1=1
	cor=editmode=0
	goto READ_NEXT_DEPARTMENT
 
GOCALK: ! r:
	fnPayPeriodEndingDate(fndate_mmddyy_to_ccyymmdd(prd))
	if jobcost=1 then close #5,free:
fnchain("S:\Payroll\Calculation") ! /r
 
EMP_PREV_ENTERED_WARN: ! r:
	mat ml$(2)
	ml$(1)="Employee number "&str$(eno)&" has been previously entered."
	ml$(2)="Do you wish to continue anyway? "
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$(1:1)="Y" then goto L1290 ! IN1=2
	if resp$(1:1)="N" then goto TheNextOne ! in1=1
! /r
L4180: !
	open #4: "Name="&pathtotimecard$&"timecard\simplesummary,KFName="&pathtotimecard$&"timecard\ssindex,Shr",internal,outIn,keyed ioerr L4200 ! timecard
	timecard=1 ! timecard files exist
L4200: !
	read #hEmployee,using F_employee_3: en$,em$,em4,em8,em9,lpd,tgp eof FINISH
	if em4=9 then goto L4200 ! must use employment status code = 9 for terminated
	! if env$('client')="West Rest Haven" and em4=2 then goto L4200 ! wrh uses code 2 for terminated
	tgp=0
! If pullFromTimeCardSystem AND EM4=1 Then Goto 3590 ! employment status on salaries people must be 1
! If EM4><pullFromTimeCardSystem Then Goto 3552
	eno=val(en$)
	! pr f "16,20,C 60": en$&"  "&em$
L4290: !
	restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ":
L4300: !
	read #2,using 'FORM POS 1,n 8,n 3,POS 58,23*PD 4.2': depeno,dep,mat tdet
	if depeno<>eno then goto TheNextOne
	simplekey$=en$&cnvrt$("n 3",dep)&cnvrt$("n 5",cno) ! timecard
	reghrs=othrs=vachrs=sickhrs=holhrs=othercomp=0 ! timecard
	if timecard=1 then
		read #4,using 'form pos 17,6*pd 5.2',key=simplekey$: reghrs,othrs,vachrs,sickhrs,holhrs,othercomp nokey L4380 ! timecard
	end if
	! if env$('client')="West Rest Haven" then gosub WRH_SIMPLE_OFFSET_HOLIDAY
	goto L4400
L4380: !
	if em4=1 and tdet(1)>0 then goto L4400 ! calculate salaries even if no time in time card file; skip any hourly people who do not have any entries from the time card system
	goto L4290 ! If NTA>0 Then aDR=NTA: Goto 3870 Else Goto 2010 ! circle if more than one department on hourly people; else skip if no time and no more departments
L4400: !
	hr(1)=tdet(2)
	hr(2)=tdet(3)
	mat inpX=(0)
	inpX(1)=reghrs
	inpX(2)=othrs
	inpX(3)=vachrs
	inpX(4)=sickhrs
	inpX(5)=holhrs
	inpX(7)=othercomp ! timecard
	inpX(6)=tdet(1)
	for j=1 to 20
		inpX(j+7)=tdet(j+3)
		if skipit(j)=1 then inpX(j+7)=0
	next j
	goto L2220
CORRECTIONS: ! r:
	editmode=1
	fnTos
	respc=0
	fnLbl(1,1,"Employee to Correct:",22,right)
	fncmbemp(1,24)
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Make corrections to this employee's time." )
	fnCmdKey("&Finish",6,0,1,"Finished making corrections")
	fnAcs(mat resp$,ckey) ! ask employee #
	if ckey=6 then editmode=0: goto PROOF_TOTALS ! finished corretions
	eno=ent=val(resp$(1)(1:8))
	read #h_rpwork,using F_rpwork,key>=cnvrt$("pic(ZZZZZZZZ)",eno)&cnvrt$("pic(ZZZ)",0),release: depeno,dep2,mat inpX,gpd,mat hr nokey L4790
	if eno<>depeno then goto L4790
	if eno=0 then goto CORRECTIONS
	goto L4840
L4790: !
	mat ml$(2)
	ml$(1)="No time has been entered on employee number "&str$(eno)&'.'
	ml$(2)="Do you wish to enter new time on this employee? "
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$(1:1)="Y" then
		goto ENTER_TIME ! ASK_EMPLOYEE
	else
		goto CORRECTIONS
	end if
READ_NEXT_DEPARTMENT: !
	read #h_rpwork,using F_rpwork,release: depeno,dep2,mat inpX,gpd,mat hr nokey CORRECTIONS eof CORRECTIONS
L4840: !
	if depeno<>eno then goto CORRECTIONS
	em$=""
	en$=lpad$(str$(eno),8)
	read #hEmployee,using F_employee_1,key=en$: em$,em4,em8,em9,lpd,tgp nokey ignore
	teno=teno-eno ! remove from proof totals
	mat tinp=tinp-inpX
	dep=dep2 ! fix dept # on correction screen
	tgp=tgp-gpd
	goto ASK_TIME
! /r
OpenFiles: ! r:
	open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,outIn,keyed
	F_employee_1: form pos 9,c 30,pos 118,n 2,pos 126,2*pd 3.3,pos 162,n 6,pd 5.2
	F_employee_2: form pos 162,n 6,pd 5.2
	F_employee_3: form pos 1,c 8,c 30,pos 118,n 2,pos 126,2*pd 3.3,pos 162,n 6,pd 5.2
	close #11: ioerr ignore
	open #11: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr",internal,outIn,keyed
	close #2: ioerr ignore
	open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed
	close #h_rpwork:=3: ioerr ignore
	open #h_rpwork:=3: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],KFName=[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno]"&',shr',internal,outIn,keyed
	close #4: ioerr ignore
	open #4: "Name="&pathtotimecard$&"timecard\simplesummary,KFName="&pathtotimecard$&"timecard\ssindex,Shr",internal,outIn,keyed ioerr L4630 ! timecard
	timecard=1 ! timecard files exist
	L4630: !
return  ! /r
 
Xit: fnXit
XITWOCAL: ! r:
	mat ml$(2)
	ml$(1)="To save your changes, next time you choose to 'Enter Time Sheets'"
	ml$(2)="you must select 'Additions to Previous Input'"
	fnmsgbox(mat ml$,resp$)
	goto Xit
! /r
! DUPLICATE_DEPARTMENTS: ! r:
! L5250: !
!  read #h_rpwork,using L5260,key=cnvrt$("pic(ZZZZZZZZ)",eno)&cnvrt$("pic(ZZZ)",dep),release: transeno,transdep nokey L5280
! L5260: form pos 1,n 8,n 3
!  goto L5250
! L5280: mat ml$(4)
!  ml$(1)="You can not enter time to the same department"
!  ml$(2)="on the same employee. Choose a different department "
!  ml$(3)="or choose to make corrections to fix the previous entry. "
!  ml$(4)="Click OK to return to previous screen. "
!  fnmsgbox(mat ml$,resp$)
! /r goto TheNextOne
! WRH_SIMPLE_OFFSET_HOLIDAY: ! r: offset holiday hours for West Rest Haven
!   if sickhrs>0 then
!     othrs=othrs-sickhrs
!     reghrs=reghrs-(max(0,holhrs-sickhrs)) ! wrh places any holiday hours considered overtime in the sick hours column.  The holiday hours are duplicated either in the reg hours or the ot hours.  this is how we decide which
!   end if
!   if sickhrs=0 then reghrs=reghrs-holhrs ! their timeclock puts holiday hours in reghrs column or othrs as well holiday column  (if no part belongs to othrs, then take all from the regular hrs
! return  ! /r
PullFromJobCost: ! r:
! h(1)=emp#,h(2)=method,h(3)=dept#,h(4)=reghrs,h(5)=ot hrs,h(6)=salary,h(7)=ded #
	gosub SORTIT
	open #5: "Name=[Q]\PRmstr\JCPRH1.h[cno]",internal,input,relative
	open #6: "Name=[Temp]\Addr."&session$,internal,input
	close #h_rpwork:=3: ioerr ignore
	open #h_rpwork:=3: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],RecL=167,Replace",internal,output
	close #h_rpwork:
	execute "Index [Q]\PRmstr\rpwork[unique_computer_id].h[cno]"&' '&"[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno] 1,11 replace,DupKeys -N"
	open #h_rpwork:=3: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],KFName=[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno]",internal,outIn,keyed
! Restore #hEmployee:
! Read #hEmployee,Using 5480: EN$ Eof 5520
! Form POS 1,C 8
! Rewrite #hEmployee,Using 5500,Key=EN$: 0
! Form POS 168,PD 5.2
! Goto 5470
	holdeno=eno=holddep=dep=0
	mat h=(0)
L5520: !
	holdeno=h(1): holddep=h(3)
	read #6,using 'form pos 1,pd 3': jci eof L5990
	read #5,using 'form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6',rec=jci: mat h,dt2,jn$ noRec L5520
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
	en$=lpad$(str$(eno),8)
	read #hEmployee,using 'form pos 9,c 30,pos 126,2*pd 3.3,pos 168,pd 5.2',key=en$: em$,em8,em9,tgp nokey L5710
	goto L5720
L5710: !
	mat ml$(2)
	ml$(1)="Can't find an employee record for employee # "&trim$(em$)&"!"
	ml$(2)="Time was entered on "&cnvrt$("pic(zz/zz/zz",dte)
	ml$(3)="Time for this employee will be skipped."
	fnmsgbox(mat ml$,resp$)
	goto L5520
L5720: !
	if eno=holdeno then goto L5750
	tgp=0
L5750: !
	read #2,using "form pos 58,24*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",dep): mat tdet
	if h2=1 or h2=3 then inpX(6)=tdet(1)
	for j=1 to 20
		inpX(j+9)=inpX(j+9)+tdet(j+3)
	next j
	if (holdeno=0 and eno>0) or ( holdeno=h(1) and holddep=h(3)) then goto L5520 ! read another record to see if same employee and department or to handle first record
L5820: !
	for j=1 to 5
		if j=2 then
			gpd=gpd+(inpX(j)*tdet(3))
		else
			gpd=gpd+(inpX(j)*tdet(2))
		end if
	next j
	gpd=gpd+inpX(6)+inpX(7)+inpX(8)+inpX(9)
	hr(1)=tdet(2)
	hr(2)=tdet(3)
	write #h_rpwork,using F_rpwork: holdeno,holddep,mat inpX,gpd,mat hr
	mat tinp=tinp+inpX
	tgp=tgp+gpd
	gpd=0
	if tgp=0 then ped=0 else ped=prd
	rewrite #hEmployee,using 'form pos 162,n 6,pd 5.2',key=en$: ped,tgp
	if holdeno=eno then goto L5960
	if tgp>0 then ent1=ent1+1
L5960: !
	holdeno=h(1) : holddep=h(3) : mat inpX=(0)
	if eofcode=1 then goto L6000
	goto L5590
L5990: !
	eofcode=1: goto L5820 ! allow last entry to post
L6000: !
	goto PROOF_TOTALS
! /r
SORTIT: ! r:
	open #15: "Name=[Temp]\Sort"&session$&".tmp,RecL=128,Replace",internal,output
	write #15,using 'form pos 1,c 128': "FILE [Q]\PRmstr\JCPRH1.h[cno],,,[Temp]\Addr."&session$&",,,acsPR,,A,N"
	write #15,using 'form pos 1,c 128': "MASK 1,8,N,A,10,2,PD,A"
	close #15:
	close #6: ioerr ignore
	execute "FREE [Temp]\Addr."&session$&" -n" ioerr ignore
	execute "SORT [Temp]\Sort"&session$&".tmp -n"
	return  ! /r
ASK_EMPLOYEE: ! r:
	editmode=0
	fnTos
	respc=0
	fnLbl(1,1,"Employee:",11,right)
	fncmbemp(1,13)
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Enter time on this employee" )
	fnCmdKey("&Search",2,0,0,"Search for employee record")
	fnCmdKey("&Finish",6,0,1,"Finished entering hours")
!                     fnCmdKey("E&Xit",5,0,1,"Returns to menu") !   fix kj
	fnAcs(mat resp$,ckey) ! ask employee #
	eno=ent=val(resp$(1)(1:8))
	if ckey=1 then
		goto ENTER_TIME
	else if ckey=2 then
		fnemployee_srch(x$,fixgrid)
		eno=val(x$)
		goto ENTER_TIME
	else if ckey=5 or ckey=6 then
		goto FINISH
	else
		goto FINISH
	end if
! /r
	def fn_add_proof_totals(&apt_total_employee_numbers,&apt_count_employees_entered,mat tinp)
		open #apt_h_rpwork=fnH: "Name=[Q]\PRmstr\rpwork[unique_computer_id].h[cno],KFName=[Q]\PRmstr\rpwork[unique_computer_id]Idx.h[cno]",internal,input,keyed ioerr APT_FINIS
		apt_heno=0 ! temp variable for internal comparison
		apt_total_employee_numbers=0 ! total of all (unique) employee numbers entered
		apt_count_employees_entered=0 ! total unique employees entered
		mat tinp=(0)
! restore #apt_h_rpwork:
		do
			read #apt_h_rpwork,using F_rpwork: eno,dep,mat inpX,gpd,mat hr eof APT_FINIS
			if apt_heno<>eno then
				apt_total_employee_numbers=apt_total_employee_numbers+eno
				apt_count_employees_entered+=1
			end if
			mat tinp=tinp+inpX
			apt_heno=eno
		loop
APT_FINIS: !
		close #apt_h_rpwork: ioerr ignore
	fnend
PRINT_LISTING: !
	heno=r=pc=teno=ent1=0
	mat tinp=(0)
	if additional=2 then goto L3160
	fncreg_read('enter time sheets proof sequence',printorder$,'1') : printorder=val(printorder$) conv ignore
	PrList_Screen: !
	fnTos
	fnLbl(1,1,"Sequence:",11,right)
	fnOpt(1,13,"Account Order",0) : if printorder<>2 then resp$(1)="True" else resp$(1)='False'
	fnOpt(2,13,"Order Entered",0) : if printorder=2 then resp$(2)='True' else resp$(2)="False"
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnAcs(mat resp$,ckey)
	if resp$(1)="False" and resp$(2)="False" then goto PrList_Screen
	if resp$(1)="True" then printorder=1 else printorder=2
	fncreg_write('enter time sheets proof sequence',str$(printorder))
	fnopenprn
	L3160: !
	restore #h_rpwork: : record=0
	pc2=0
	PL_READ: !
	if printorder=2 then
	L3200: !
		record+=1 : if record>lrec(h_rpwork) then goto PL_FINIS
		read #h_rpwork,using F_rpwork,rec=record,release: eno,dep,mat inpX,gpd,mat hr eof PL_FINIS noRec L3200
	else
		read #h_rpwork,using F_rpwork,release: eno,dep,mat inpX,gpd,mat hr eof PL_FINIS
	end if
	if heno<>eno then
		read #2,using "form pos 42,n 6",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",dep): lastprdate
		if lastprdate=prd then ! make sure pay hasn't been calculated on this person on this date
			mat ml$(4)
			ml$(1)="You have previously calculated pay using this same payroll date on employee "&str$(eno)
			ml$(2)="You must delete this person's time for now and either reverse the previous calculation "
			ml$(3)="or enter the time using a differen payroll date. "
			ml$(4)="                         Click OK to continue. "
			fnmsgbox(mat ml$,resp$)
			if additional=2 then
				delete #h_rpwork,rec=rec(h_rpwork): noRec L3270
				goto PL_READ
			end if
		end if
		L3270: !
		teno+=eno
		ent1+=1
	end if
	mat tinp=tinp+inpX
	heno=eno
	if additional=2 then goto PL_READ
	if pc=9 then gosub PL_PRINT_EMP_BLOCK
	pc=pc+1
	read #hEmployee,using F_employee_1,key=lpad$(str$(eno),8),release: em$ nokey L3440
	em$=rtrm$(em$)
	for j1=len(em$) to 1 step -1
		if em$(j1:j1)=" " then goto L3410
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
goto PL_READ
 
PL_PRINT_EMP_BLOCK: ! r:
	if pc2=3 then pc2=0
	pc2=pc2+1
	if pc2=1 then
		pr #255: ''
		pr #255,using 'form pos 1,c 25,cc 82,skip 1,c 25': date$,env$('cnam'),time$
	end if
	pr #255,using L3610: mat n1$
	pr #255,using L3610: mat n2$
	L3610: form pos 21,9*c 12,skip 1
	! pr #255,Using F1$: "Record #     ",prX(1,23),prX(2,23),prX(3,23),prX(4,23),prX(5,23),prX(6,23),prX(7,23),prX(8,23),prX(9,23)
	pr #255,using f1$: "Employee  ",prX(1,1),prX(2,1),prX(3,1),prX(4,1),prX(5,1),prX(6,1),prX(7,1),prX(8,1),prX(9,1)
	pr #255,using f1$: "Department  ",prX(1,2),prX(2,2),prX(3,2),prX(4,2),prX(5,2),prX(6,2),prX(7,2),prX(8,2),prX(9,2)
	for j=1 to 29
		if trim$(sc1$(j))<>"" then
			pr #255,using f2$: sc1$(j),prX(1,j+2),prX(2,j+2),prX(3,j+2),prX(4,j+2),prX(5,j+2),prX(6,j+2),prX(7,j+2),prX(8,j+2),prX(9,j+2)
		end if
	next j
	pr #255,using f2$: "Dept Gross Pay ",prX(1,32),prX(2,32),prX(3,32),prX(4,32),prX(5,32),prX(6,32),prX(7,32),prX(8,32),prX(9,32)
	pr #255,using f2$: "Reg Hourly Rate",prX(1,34),prX(2,34),prX(3,34),prX(4,34),prX(5,34),prX(6,34),prX(7,34),prX(8,34),prX(9,34)
	! pr #255,Using F2$: "O/T Hourly Rate",prX(1,35),prX(2,35),prX(3,35),prX(4,35),prX(5,35),prX(6,35),prX(7,35),prX(8,35),prX(9,35)
	if pc2=2 then pr #255: newpage else pr #255,using L3730: " "
	L3730: form c 1,skip 2
	if pc2>1 then pc2=0
	mat prX=(0)
	mat n1$=("")
	mat n2$=("")
	pc=0
return  ! /r
 
PL_FINIS: ! r:
	if additional=2 then
		additional=1
		close #h_rpwork:
	else
		gosub PL_PRINT_EMP_BLOCK
		fncloseprn
	end if
goto PROOF_TOTALS ! /r
def fn_setup
	autoLibrary
 
	on error goto Ertn
	gosub Enum
 
	dim inpX(29)
	dim em$*30
	dim hr(2)
	dim n1$(9)
	dim n2$(9)
	dim en$*8,tdet(23)
	dim tinp(29)
	dim f1$*400
	dim f2$*400
	dim prX(9,35)
	dim resp$(64)*128
	dim ml$(0)*128
	dim skipit$(20)*1,skipit(20)
	dim name$(20)*21
	dim h(7)
	dim tdt(4),tcd(3)
 
	dim dednames$(20)*20
	fnDedNames(mat dednames$)
 
	dim sc1$(31)*20
	sc1$(1)="Regular Hours "
	sc1$(2)="Overtime Hours"
	sc1$(3)="Sick Hours    "
	sc1$(4)="Vacation Hours"
	sc1$(5)="Holiday Hours "
	sc1$(6)="Salary        "
	sc1$(7)="Other Compensation"
	sc1$(8)="Meals"
	sc1$(9)="Tips "
	for j=1 to 20
		sc1$(j+9)=dednames$(j)
	next j
	sc1$(30)="Reg Hourly Rate"
	sc1$(31)="O/T Hourly Rate"
 
	f1$="Form POS 1,C 20" ! need this for edit list
	f2$=f1$
	for j=1 to 9
		f1$=rtrm$(f1$)&",PIC(------------)"
		f2$=rtrm$(f2$)&",PIC(---------.--)"
	next j
	pathtotimecard$="C:\progra~1\acs\"
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
	read #1,using 'form pos 726,pd 3.2': mhw
	close #1:
fnend
 
include: Enum
include: ertn
