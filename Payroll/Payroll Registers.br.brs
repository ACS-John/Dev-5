
! r: general setup: libraries, dims, fnTop,fncno,read defaults, open files, etc
	autoLibrary
	on error goto Ertn
 
	fnTop(program$)
	fncreg_read('prreg2.include_tips_in_other_wh',include_tips_in_other_wh$,'True')
	fnreg_read('prreg2.append_reg1',append_reg1$,'True')
	! pr 'before fndednames' : pause
	dim fullname$(20)*20
	dim abrevname$(20)
	dim newdedcode(20)
	dim newcalcode(20)
	dim newdedfed(20)
	dim dedfica(20)
	dim dedst(20)
	dim deduc(20)
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	fncreg_read('CL Bank Code',bankcode$) : bankcode=val(bankcode$) : if bankcode=0 then bankcode=1
 
	newdedcode_Deduct =1
	newdedcode_Add    =2
	newdedcode_Benefit=3
	open #12: 'Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr',i,i,k ioerr BankReadFinis
	read #12,using 'form pos 57,G 8',key=lpad$(str$(bankcode),2),release: cl_bank_last_check$ nokey ignore
	close #12: ioerr ignore
	BankReadFinis: !
	
	open #1: 'Name=[Q]\PRmstr\prCode.h[cno],Shr',i,i ioerr CkNoReadFinis
	read #1,using 'form pos 5,N 8': ckno
	close #1:
	CkNoReadFinis: !
	
	ckno+=1
	d1$=cnvrt$('pic(zzzzzzzz)',fnPayPeriodEndingDate)
	ppd=val(d1$(5:6))*10000+val(d1$(7:8))*100+val(d1$(3:4))
 
	if trim$(cl_bank_last_check$)<>'' then ckno=val(cl_bank_last_check$)+1 conv ignore
 
 
	open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,i,k
	open #hDd=fnH: 'Name=[Q]\PRmstr\DD.h[cno],KFName=[Q]\PRmstr\DDidx1.h[cno],Shr',i,i,k
	open #hCheck1=fnH: 'Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr',i,i,k
	open #hCheck3=fnH: 'Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr',i,i,k
! /r
 
if fnprocess=1 then goto StartReport else goto AskCheckNo
 
AskCheckNo: ! r:
	fnTos
	respc=0
	fnLbl(1,1,'Beginning Check Number:',20,1)
	fnTxt(1,23,8,0,1,'30',0,' ')
	resp$(respc+=1)=str$(ckno)
	fnLbl(1,1,'',34,1) ! bigger screen
	fnLbl(2,1,'Payroll Date:',20,1)
	fnTxt(2,23,10,0,1,'1',0,'For current payroll, always use the calculation date.  You can reprint older payroll registers by using that date.')
	resp$(respc+=1)=str$(ppd)
	fnChk(4,2,'Combine both registers into one multi-page report',50)
	resp$(resp_append_reg1:=respc+=1)=append_reg1$
	if env$('ACSDeveloper')<>'' then ! option under development for West Accounting... held until they decide if they actually want/nedd this - currently causes mismatch (in their cno 18) in other_wh in 1st and 2nd PR Registers
		fnChk(6,2,'Include Tips in Other Withholdings',50)
		resp$(resp_include_tips_in_other_wh:=respc+=1)=include_tips_in_other_wh$
	else
		resp$(resp_include_tips_in_other_wh:=respc+=1)='False'
	end if
	fnCmdKey('&Next',1,1,0,'Proceed with pr the payroll register.' )
	fnCmdKey('E&xit',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	ckno=val(resp$(1))
	ppd=val(resp$(2))
	append_reg1$=resp$(resp_append_reg1)
	include_tips_in_other_wh$=resp$(resp_include_tips_in_other_wh)
	fncreg_write('prreg2.include_tips_in_other_wh',include_tips_in_other_wh$) : if include_tips_in_other_wh$='True' then include_tips_in_other_wh=1 else include_tips_in_other_wh=0
	fnreg_write('prreg2.append_reg1',append_reg1$) : if append_reg1$='True' then append_reg1=1 else append_reg1=0
goto StartReport ! /r
StartReport: ! r:
	if append_reg1 then
		fnopenprn(' (Check and Departmental Registers)')
	else
		fnopenprn(' (Check Register)')
	end if
	gosub PrHeader
goto LoopTop ! /r
 
def fn_employeeHasCheckOnDate(eno,theDate; ___,ehcKey$*17,returnN)
	theDate=date(days(theDate,'mmddyy'),'ccyymmdd')
	ehcKey$=cnvrt$('N 8',eno)
	ehcKey$=ehcKey$&cnvrt$('PD 6',theDate)
	ehcKey$=ehcKey$&cnvrt$('N 3',0)
	restore #hCheck3,key=>ehcKey$: nokey EhcFinis
	read #hCheck3, using 'form pos 1,n 8, pos 12,pd 6': ehcEno,ehcDate
	if ehcEno=eno and theDate=ehcDate then returnN=1
	EhcFinis: !
	fn_employeeHasCheckOnDate=returnN
fnend
 
LoopTop: ! r: main loop
	dim em$*30
	read #hEmployee,using 'form pos 1,n 8,c 30,pos 162,n 6': eno,em$,lpd eof Finis
	! if eno=307 then pr 'eno '&str$(eno) : exe 'break other_wh' : break_is_on=1 else if break_is_on then exe 'break other_wh off' : break_is_on=0
	if lpd=ppd then goto PastEmpCheckTest
	if ~fn_employeeHasCheckOnDate(eno,ppd) then goto LoopTop ! if lpd><ppd then goto LoopTop
	PastEmpCheckTest: !
	dim thc(5)
	dim ttdc(10)
	dim tcp(32)
	mat thc=(0) : mat tcp=(0) : mat ttdc=(0) : holdrealckno=realckno=0
	checkkey$=cnvrt$('pic(ZZZZZZZ#)',eno)&'         '
	dirdep$=rpad$(str$(eno),10)
	dd$=''
	read #hDd,using 'form pos 1,C 10,C 1,N 9,N 2,N 17',key=dirdep$: key$,dd$,rtn,acc,acn nokey ignore
	! if env$('client')='West Rest Haven' then goto L690
	a=pos(rtrm$(em$),' ',1) : b=pos(rtrm$(em$),' ',a+1)
	em$=rtrm$(em$(max(a,b):30))&' '&em$(1:a) error ignore
	! L690: !
	restore #hCheck1,key>=checkkey$: nokey LoopTop
	do
		dim tdc(10)
		dim cp(32)
		read #hCheck1,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': heno,tdn,prd,realckno,mat tdc,mat cp eof EoCheck1
		if heno=eno and prd=fndate_mmddyy_to_ccyymmdd(ppd) then 
			if prd=fndate_mmddyy_to_ccyymmdd(ppd) then holdrealckno=realckno
			mat tcp=tcp+cp : mat ttdc=ttdc+tdc
		end if
	loop while heno=eno
	EoCheck1: !
	other_wh=-tcp(25)
	for j=5 to 24
		if newdedcode(j-4)=newdedcode_Benefit then
			! do nothing
		else if newdedcode(j-4)=newdedcode_Add then
			other_wh=other_wh-tcp(j) ! if break_is_on and tcp(j)<>0 then pr 'tcp('&str$(j)&') deducts '&str$(tcp(j))
		else if newdedcode(j-4)=newdedcode_Deduct then
			other_wh=other_wh+tcp(j) ! if break_is_on and tcp(j)<>0 then pr 'tcp('&str$(j)&')    adds '&str$(tcp(j))
		else ! default to behave like a deduction
			other_wh=other_wh+tcp(j)
			! pr 'newdedcode('&str$(j-4)&')='&str$(newdedcode(j-4))&' which is invalid.'
			! pr bell
			! pause
		end if
	next j
 
	if include_tips_in_other_wh then ! include tips in Other Withholdings added for West Accounting on 1/18/2016
		other_wh+=tcp(30) ! if break_is_on and tcp(30)<>0 then pr 'tcp('&str$(30)&') TIPS    adds '&str$(tcp(30))
	end if
	tothrs=0
	for j=1 to 5
		tothrs=tothrs+ttdc(j)
		thc(j)=ttdc(j)
	next j
	if holdrealckno=0 then ckn2=ckno : ckno+=1 else ckn2=holdrealckno
	if uprc$(dd$)<>'Y' then
		pr #255,using L900: ckn2,eno,em$(1:11),mat thc,tothrs,tcp(31),tcp(3),tcp(2),tcp(1),tcp(4),other_wh,tcp(32) pageoflow PgOf
		L900: form pos 1,n 5,n 8,x 1,c 12,6*n 7.2,7*n 9.2,skip 1
	else
		if tcp(22)=0 then tcp(22)=tcp(32)
		pr #255,using L930: 'DD',eno,em$(1:11),mat thc,tothrs,tcp(31),tcp(3),tcp(2),tcp(1),tcp(4),other_wh,tcp(22) pageoflow PgOf
		L930: form pos 1,c 5,n 8,x 1,c 12,6*n 7.2,7*n 9.2,skip 1
	end if
	total_hours+=sum(mat thc)
	total_net_pay+=tcp(32)
	total_gross_pay+=tcp(31)
	other_wh=0
goto LoopTop ! /r
 
PrTotals: ! r:
	pr #255: ''
	pr #255: '    Total Hours: '&lpad$(str$(total_hours),26)
	pr #255: 'Total Gross Pay: '&cnvrt$('pic(-$$,$$$,$$$,$$$,$$$,$$#.##)',total_gross_pay)
	pr #255: '  Total Net Pay: '&rpt$(' ',88)&cnvrt$('pic(-$$,$$$,$$$,$$$,$$$,$$#.##)',total_net_pay)
return  ! /r
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue  ! /r
PrHeader: ! r:
	pr #255,using 'form pos 1,c 25': 'Page '&str$(pgno+=1)&' '&date$
	pr #255: '\qc  {\f221 \fs22 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f201 \fs20 \b Payroll Check Register}' ! pr #255: '\qc  {\f201 \fs20 \b '&env$('program_caption')&'}'
	pr #255: '\qc  {\f181 \fs16 \b Payroll Date: '&cnvrt$('pic(zz/zz/zz)',ppd)&'}'
	pr #255: '\ql   '
	pr #255: tab(29);'<----------------Hours----------------->';
	pr #255: tab(71);'<-Pay->';
	pr #255: tab(79);'<-----------------Deductions---------------->';
	pr #255: tab(129);'Net'
	pr #255: 'CkNo   EmpNo  Name';
	pr #255: tab(29);'  Reg    O/T   Sick    Vac    Hol  Total';
	pr #255: tab(71);'  Total   Med WH     FICA  Federal    State    Other      Pay'
return  ! /r
Finis: ! r:
	gosub PrTotals
	close #hEmployee: ioerr ignore
	! close #2: ioerr ignore
	close #hCheck1: ioerr ignore
	close #hCheck3: ioerr ignore
	close #hDd: ioerr ignore
	if append_reg1 then
		if env$('ACSDeveloper')<>'' then
			pr #255: ''
			pr #255: '----newpage (ACSDeveloper Style)----'
			pr #255: ''
		else
			pr #255: newpage
		end if
	else
		fncloseprn
	end if
	fnpayroll_register_2(0,include_tips_in_other_wh,append_reg1,ppd)
goto Xit ! /r let fnchain('S:\acsPR\newprReg2')
Xit: fnXit
include: ertn
