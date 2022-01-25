! r: setup stuff
	on error goto Ertn
	autoLibrary
	fnTop(program$)
 
	dim em$*30
	dim tcp(32),tdc(10),ttdc(10)
	dim resp$(50)*60
	dim cp(32)
	dim sel_ded(20)
	dim sel_pen(20)
	dim fullname$(20)*20,abbrevname$(20)*8,dedcode(20),calcode(20),dedfed(20),dedfica(20),dedst(20),deduc(20)
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],Shr",i,i,r
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",i,i,k
	open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",i,i,r
! /r
	gosub SCREEN_PENSION1
	fnopenprn !
	gosub HDR
	READ_EMPLOYEE: !
	read #1,using 'form pos 1,n 8,c 30,pos 99,c 11': eno,em$,ss$ eof Finis
	a=pos (rtrm$(em$)," ",1)
	b=pos (rtrm$(em$)," ",a+1)
	em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
	reg_earnings=ded_pension=pension_amount=0
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	mat tcp=(0) : mat ttdc=(0)
	restore #4,key>=checkkey$: nokey READ_EMPLOYEE
	READ_TRANS: !
	read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof PRINT_ONE
	if heno=eno then
		if prd<beg_date or prd>end_date then goto READ_TRANS
		mat tcp=tcp+cp : mat ttdc=ttdc+tdc
		for j=1 to 20
			if sel_ded(j)=1 and dedcode(j)=1 then ded_pension+=cp(j+4) ! PENSION
			if sel_ded(j)=1 and dedcode(j)>1 then ded_pension-=cp(j+4) ! PENSION
			if sel_pen(j)=1 then pension_amount+=cp(j+4) ! PENSION
		next j
		reg_earnings+=cp(31) ! REGULAR EARNINGS
		goto READ_TRANS !
	end if
PRINT_ONE: ! r:
	if pension_amount<>0 then ! skip if no pension wh
		pr #255,using F_LINE_OUT: em$(1:24),ss$,reg_earnings,ded_pension,pension_amount,reg_earnings+ded_pension pageoflow PgOf
		F_LINE_OUT: form pos 1,c 24,c 12,4*n 12.2
		total_salary+=reg_earnings
		total_ded+=ded_pension
		total_pension+=pension_amount
	end if
goto READ_EMPLOYEE ! /r
Finis: ! r:
	close #1: ioerr ignore
	close #2: ioerr ignore
	pr #255: "                                      ----------  ----------  ----------  ---------- "
	pr #255,using F_LINE_OUT: " "," ",total_salary,total_ded,total_pension,total_salary+total_ded
	pr #255: "                                      =========-  ==========  ==========  ========== "
	fncloseprn
	close #25: ioerr ignore
goto Xit ! /r
Xit: fnXit
HDR: ! r:
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\ql   "
	pr #255: "Name                    SS Number     Total Wage     Ded/Add     Pension  Pension Wage"
return  ! /r
PgOf: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r
SCREEN_PENSION1: ! r:
	fnTos
	rc=cf=0
	fnFra(1,1,21,23,"Deductions Effecting Pension Wage","Mark any deduction that either needs to be added to gross wages or deducted from gross wages before calculating the Pension Wage",0)
	cf+=1 : fratype=cf
	for j=1 to 20
		fnChk(j,3,fullname$(j),0,fratype)
		resp$(rc+=1)='False'
	next j
	fnFra(1,30,20,23,"Pension Deduction","Mark the pension deduction that you want printed on the report",0)
	cf+=1 : fratype=cf
	for j=1 to 20
		fnOpt(j,3,fullname$(j),0,fratype)
		resp$(rc+=1)='False'
	next j
	fnFra(1,60,3,42,"Date Range","Enter the beginning and ending date range covered by this report.")
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting Date:",mylen,1,0,fradate)
	fnTxt(1,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,fradate)
	fnTxt(2,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(end_date)
	fnCmdKey("Next",1,1,0,"Prints the report")
	fnCmdKey("Cancel",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	for j=1 to 20
		if resp$(j)='True' then sel_ded(j)=1
	next j
	for j=1 to 20
		if resp$(j+20)='True' then sel_pen(j)=1
	next j
	beg_date=val(resp$(41))
	end_date=val(resp$(42))
return  ! /r
include: ertn
