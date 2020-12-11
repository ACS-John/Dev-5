! Replace S:\acsPR\conversion\v4.cnv
!    medicare_is_seperated     ...         look for "Fica_Combined" if medicare not separated and remove both exclamations
	if ~setup then fn_setup

	fnTop(program$,cap$="Department Conversion")
! r: do every company - loop top
	fngetdir2('[Q]\'&fncursys$&"mstr",mat filename$,'/od /ta',"Company.*")
	filename_item=0
	for filename_item=1 to udim(mat filename$)
		tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
		if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
			cno=tmp_cno

! /r
			fn_pr_conversion_department(cno)
! r: do every company - loop bottom
		end if 
ACNO_CONV: ! 
		close #h_rpmstr: ioerr ignore
	next filename_item
! /r
!  fnStatusPause
chain "S:\acsPR\conversion\v4_part2"
def library fnpr_conversion_department(cno; medicare_is_seperated)
	if ~setup then fn_setup
	fnpr_conversion_department=fn_pr_conversion_department(cno, medicare_is_seperated)
fnend 
def fn_pr_conversion_department(cno; medicare_is_seperated)
	!  pr 'all files should be closed now' : pause
	on error goto Ertn
	fnStatus('Department Conversion(v4_cnv) for Company [cno]')
	payrollcheck_write_count=0
	!      fnStatusPause
	dim fullname$(10)*20
	fnDedNames(mat fullname$)
	RPMSTR_OPEN: ! 
	if ~exists("[Q]\PRmstr\RPMstr.h[cno]") then 
		open #h_rpmstr:=1: "Name=[Q]\PRmstr\RPMstr.h[cno],RecL=196,Use",internal,outIn 
		close #h_rpmstr: 
	end if 
	fnIndex("[Q]\PRmstr\RPMstr.h[cno]","[Q]\PRmstr\RPIndex.h[cno]","1 8")
	fnIndex("[Q]\PRmstr\RPMstr.h[cno]","[Q]\PRmstr\RPIndx2.h[cno]","9 30")
	open #h_rpmstr:=1: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,outIn,keyed 
	if rln(h_rpmstr)<196 then 
		close #h_rpmstr: 
		fnCopy("[Q]\PRmstr\RPMstr.h[cno]","[Q]\PRmstr\RPMstr.h[cno]",196)
		goto RPMSTR_OPEN
	end if 

	if exists("[Q]\PRmstr\RPTrail.h[cno]") then 
		open #h_rptrail:=2: "Name=[Q]\PRmstr\RPTrail.h[cno],Shr",internal,input,relative 
	else 
		open #h_rptrail:=2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],RecL=474,Use,Shr",internal,outIn,relative 
	end if 

	if fnIndex("[Q]\PRmstr\PRCkHist.h[cno]","[Q]\PRmstr\PRCKINDX.h[cno]","1 14") then 
		open #h_prckhist:=fnH: "Name=[Q]\PRmstr\PRCkHist.h[cno],KFName=[Q]\PRmstr\PRCkIndx.h[cno],Shr",internal,outIn,keyed ioerr L320
		foundhistory=1 ! pr 'foundhistory : lrec(h_prckhist)='&str$(lrec(h_prckhist)) : pause
	else 
		foundhistory=0 ! pr 'was not able to index it - setting foundhistory to ZERO - history will not be created' : pause
	end if 
	L320: ! 
	open #12: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno],RecL=149,kps=1,kln=11,replace",internal,outIn,keyed 
	open #h_payrollchecks:=fnH: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],RecL=224,kps=1,kln=17,replace",internal,outIn,keyed 
	if exists("[Q]\PRmstr\dd.h[cno]")=0 then 
		open #30: "Name=[Q]\PRmstr\dd.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],kps=1,kln=10,Use",internal,outIn,keyed 
		close #30: ioerr ignore
	end if 
	fn_prcode_validate
	goto TOPOFLOOP

	TOPOFLOOP: ! r:
		read #h_rpmstr,using 'form pos 1,n 8,c 30,pos 122,n 2,pos 162,n 6,pos 173,2*pd 3': eno,em$,em6,lpd,mat ta eof DONE
		adr=ta(1)
		do 
			read #h_rptrail,using 'Form POS 1,N 8,N 3,N 3,N 6,N 3,4*N 6,3*N 2,24*PD 4.2,5*PD 3.2,POS 471,PD 4.2,POS 165,PD 3.2,60*PD 5.2,PD 3',rec=adr: teno,tdn,gl1,gl2,gl3,mat tdt,mat tcd,tli,mat tdet,mat tdy,mat tdc,mcwh,mat ty,mat tqm,mat tcp,nta
			gosub CONVERSION
			if nta<>0 then adr=nta
		loop until nta=0
		if foundhistory=1 then goto PULLFROMHISTORY
	goto TOPOFLOOP ! /r

	CONVERSION: ! r:
		tdn=tdn ! department #
		prd=fndate_mmddyy_to_ccyymmdd(tdt(4)) ! payroll date to be used
		newtdet(1)=tdet(1) ! salary
		newtdet(2)=tdet(2) ! hourly rate
		newtdet(3)=tdet(3) ! ot rate
		for j=1 to 10
			newtdet(j+3)=tdet(j+3) ! 10 standard deductions
		next j
		newtdc(1)=tdy(1) ! regular hours for year
		newtdc(2)=tdy(2) ! ot hours
		newtdc(3)=tdy(3) ! sick hours
		newtdc(4)=tdy(4) ! vac hrs
		newtdc(5)=tdy(5) ! holiday hrs
		newtdc(6)=ty(21) ! workmans comp wage  (make all wages = total pay
		newtdc(7)=ty(21) ! ss wage  ! needs help What about cafiteria
		newtdc(8)=ty(21) ! medicaid
		newtdc(9)=ty(21) ! federal uc
		newtdc(10)=ty(21) ! state uc wage
		cp(1)=ty(1) ! fed wh year to date
		cp(2)=ty(2) ! ss wh year to date
		cp(3)=ty(15) ! medicaid ytd
		if medicare_is_seperated then gosub FICA_COMBINED
		cp(4)=ty(3) ! state wh ytd
		for j=1 to 10
			cp(j+4)=ty(j+3) ! 10 miscellaneous deductions'
		next j
		cp(25)=ty(14) ! eic
		cp(26)=ty(16) ! reg pay
		cp(27)=ty(17) ! ot pay
		cp(28)=ty(18) ! other comp
		cp(29)=ty(19) ! meals
		cp(30)=ty(20) ! tips
		cp(31)=ty(21) ! total wage
		cp(32)=0 ! don't have net
		gl$=cnvrt$("pic(zz#)",gl1)&cnvrt$("pic(zzzzz#)",gl2)&cnvrt$("pic(zz#)",gl3)
		write #12,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': eno,tdn,gl$,mat tdt,mat tcd,tli,mat newtdet ! department record
		if ~foundhistory then ! else  write check file later
			fn_payrollchecks_write
		end if 
	return ! /r

	PULLFROMHISTORY: ! r:
		hsk$=lpad$(str$(eno),8)
		restore #h_prckhist,search>=hsk$: nokey TOPOFLOOP
	PFH_READ_PRCKHIST: ! 
	! If env$('client')="Franklinton" Then
	!    read #h_prckhist,using 'form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2': heno,prd,ckno,mat tdc,mat tcp eof TOPOFLOOP conv ignore
	! pRD=fndate_mmddyy_to_ccyymmdd(PRD): Goto PFH_POSTREAD_PRCKHIST
		read #h_prckhist,using 'form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2': heno,prd,ckno,mat tdc,mat tcp eof TOPOFLOOP conv CKHIST_READ_CONV
	PFH_POSTREAD_PRCKHIST: ! 
		if heno<>eno then goto TOPOFLOOP
		if prd<20000101 then goto PFH_READ_PRCKHIST ! don't allow any checks before 2000
		tdn=tdn ! use last department since old check history is not by dept
		ckno=ckno
		for j=1 to 5
			newtdc(j)=tdc(j) ! hours etc
		next j
		newtdc(6)=newtdc(7)=newtdc(8)=newtdc(9)=newtdc(10)=0 ! set these wage figures to zero
		cp(1)=tcp(1) ! fed wh year to date
		cp(2)=tcp(2) ! ss wh year to date
		cp(3)=tcp(15) ! medicaid ytd
		if medicare_is_seperated then gosub FICA_COMBINED
		cp(4)=tcp(3) ! state wh ytd
		for j=1 to 10
			cp(j+4)=tcp(j+3) ! 10 miscellaneous deductions'
		next j
		cp(25)=tcp(14) ! eic
		cp(26)=tcp(16) ! reg pay
		cp(27)=tcp(17) ! ot pay

		cp(28)=tcp(18) ! other comp
		cp(29)=tcp(19) ! meals
		cp(30)=tcp(20) ! tips
		cp(31)=tcp(21) ! total wage
		cp(32)=tcp(22) ! net
		fn_payrollchecks_write
	goto PFH_READ_PRCKHIST ! /r

	DONE: ! 
		gosub CREATENAMES
		open #h_deptname:=fnH: "Name=[Q]\PRmstr\DeptName.h[cno],KFName=[Q]\PRmstr\DeptNameIdx.h[cno],replace,RecL=32,kps=1,kln=3,Shr",internal,outIn,keyed 
		close #h_deptname: 
		close #12: ioerr ignore
		fnIndex("[Q]\PRmstr\Department.h[cno]","[Q]\PRmstr\DeptIdx.h[cno]","1 11")
		close #h_payrollchecks: 
		fnIndex("[Q]\PRmstr\PayrollChecks.h[cno]","[Q]\PRmstr\checkidx.h[cno]","1 17")
		fnIndex("[Q]\PRmstr\dd.h[cno]","[Q]\PRmstr\ddidx1.h[cno]","1,10")
		close #h_rpmstr: ioerr ignore
		fnIndex("[Q]\PRmstr\RPMstr.h[cno]","[Q]\PRmstr\RPIndex.h[cno]","1,8")
		fnIndex("[Q]\PRmstr\RPMstr.h[cno]","[Q]\PRmstr\RPIndx2.h[cno]","9 30")
	!   end if  ! cno_current<>0
	! next company_item
	Xit: ! 
		close #h_prckhist: ioerr ignore
		fnStatus('payrollcheck_write_count='&str$(payrollcheck_write_count))
	! fnStatusPause
fnend 
CREATENAMES: ! r:
	dim a$(3)*40,d$(10)*8,r(10),e$(10)*12,gln$(15)*12,dedcode(10),calcode(10)
	dim dedfed(10),rpnames2$(10)*6
	dim fullname$(20)*20,abrevname$(20)*8,newdedcode(20),newcalcode(20)
	dim newdedfed(20),dedfica(20),dedst(20),deduc(20),gl$(20)*12
	if exists("[Q]\PRmstr\PRCOINFO.h[cno]") and ~exists("[Q]\PRmstr\Company.h[cno]") then 
		execute "Rename [Q]\PRmstr\PRCOINFO.h[cno]"&' '&"[Q]\PRmstr\Company.h[cno]"
	end if 
	open #h_company:=fnH: "Name=[Q]\PRmstr\Company.h[cno]",internal,input 
	read #h_company,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2': mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$ ! eof L370 ioerr L330
	close #h_company: 
	for j=1 to 10
		newdedcode(j)=dedcode(j)
		if dedfed(j)>0 then newdedfed(j)=1
		newcalcode(j)=calcode(j)
		fullname$(j)=rpnames2$(j)
		abrevname$(j)=fullname$(j)(1:6)
		if dedfed(j)=1 then dedst(j)=1 ! if pension type code make deductible for state
		if dedfed(j)=2 then dedfica(j)=1 ! if pension type code =2 then cafiteria
		gl$(j)=gln$(j+3)
	next j
	close #h_rptrail: ioerr ignore
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$,1)
return  ! /r
CKHIST_READ_CONV: ! r:
	reread #h_prckhist,using 'form pos 1,n 8,n 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2': heno,prd,ckno,mat tdc,mat tcp eof TOPOFLOOP conv L1820
	! pause
	prd=fndate_mmddyy_to_ccyymmdd(prd)
goto PFH_POSTREAD_PRCKHIST ! /r
L1820: ! r:
	reread #h_prckhist,using 'form pos 1,c 5': heno$ eof TOPOFLOOP
goto PFH_READ_PRCKHIST ! /r
FICA_COMBINED: ! r:
	med$="N": ss=cp(2)
	cp(2)=ss*.810458: cp(3)=ss-cp(2) ! just use a ratio to calculate breakdwon
	if uprc$(med$)="N" and em6=1 then cp(3)=0 : cp(2)=ss ! NO MC ALL SS ! change to seperate medicare
	if uprc$(med$)="N" and em6=2 then cp(2)=0 : cp(3)=ss ! NO SS ALL MC ! change to seperate medicare
	if em6=9 then cp(2)=cp(3)=0 ! NO SS OR MC
return  ! /r
def fn_prcode_validate !  PRCODE - verify it exist, if not create it with one blank record
	open #20: "Name=[Q]\PRmstr\prCode.h[cno]",internal,output ioerr PRCODE_CREATE_NEW
	close #20: 
	goto L378
	PRCODE_CREATE_NEW: ! 
	open #20: "Name=[Q]\PRmstr\prCode.h[cno],RecL=512,new",internal,output  ! ioerr PRCODE_CREATE_NEW
	write #20,using 'form pos 1,c 512': ""
	close #20: 
	L378: ! 
fnend 
def fn_payrollchecks_write
	if newtdc(1)>999 then newtdc(1)=999
	write #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,tdn,prd,ckno,mat newtdc,mat cp ! payroll check history
	payrollcheck_write_count+=1
fnend 
dim a$*40,em$*30,ta(2),cp(32),tcp(22) ! ,hc(5),thc(5),d$*20,whc(10),message$*40
dim dedcode(10),calcode(10),dedfed(10),cap$*128
dim tcp(32),newtdc(10),newtdet(23),tdt(4),tcd(3),tdet(17),tdy(6),tdc(6)
dim ty(21),tqm(17),tcp(22),tdet(17),dednames$(20)*20,d1$*20
include: fn_setup
