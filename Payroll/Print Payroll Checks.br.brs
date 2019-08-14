! formerly S:\acsPR\newprCkPrt
! pr Payroll Checks ! Nebs 9039t: Standard Check Format (Laser Stub-Check-Stub)
! r: library and on error
	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fnGetPayrollDates
	library 'S:\Core\Library': fnPayPeriodEndingDate
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fnchain
	library 'S:\Core\Library': fnTos,fnLbl,fncomboa,fnTxt,fncombof,fnCmdSet,fnAcs,fnOpt
	library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd
	library 'S:\Core\Library': fnCmdKey
	library 'S:\Core\Library': fnButton
	library 'S:\Core\Library': fnagl$,fnqgl,fnrgl$
	library 'S:\Core\Library': fnmsgbox
	library 'S:\Core\Library': fnss_employee,fnss_employer
	library 'S:\Core\Library': fncd
	library 'S:\Core\Library': fnreg_read,fnreg_write
	library 'S:\Core\Library': fnclient_has
	library 'S:\Core\Library': fncreg_read,fncreg_write
	library 'S:\Core\Library': fngethandle
	library 'S:\Core\Library': fnDedNames
	library 'S:\Core\Library': fnPrPrintNetZeroDefault$
	on error goto ERTN
! /r
	fntop(program$)
	fnreg_read('Print Payroll Checks - Print checks which net zero',pr_prNetZeroChecks$,fnPrPrintNetZeroDefault$)
	fnreg_read('Post to Checkbook - Populate Checkbook Payee from Payroll Employee',pr_prEmpToClPayee$,'True')
	! fnreg_read('Post to Checkbook - Prefix (optional)',pr_clPayeePrefix$)
! r: dims and constants
	dim em$(3)*30 ! payee name and address
	dim tdet(17)
	dim tdc(10)
	dim tcp(32)
	dim tdep(20,26)
	dim ttc(32)
	dim d$(10)*8
	dim gln$(15)*12
	dim tty(32)
	dim mgl$(11)
	dim ded$(29)
	dim de$*30 ! description (from CL (for transaction allocations, i think)
	dim lcn$*8
	dim tr(2)
	dim tr$(5)*35
	dim resp$(25)*128,ttc(32),ttdc(10)
	dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),ytdTotal(32)
	dim quartertotals(32)
	dim dedfed(20)
	dim calcode(20)
	dim dedcode(20)
	dim fullname$(20)*20
	dim abrevName$(20)*8
	dim ml$(2)*128 ! temp variable for messagebox message lines
	dim dedfica(20),dedSt(20),dedUc(20),gl$(20)*12
	dim dtr$(5)*35,key$*21
	dim dept(6),bankgl$*12,gl$*12,bn$*30,text$*90,v(7,8),deptsum(6)
	dim hnames$(20)*8
	dim eng$*128,wording$(27)*9,amount(11)
! 
	dim opt_check_format$(6)*20, scc$(6)
	opt_check_format$(1)="Check, Stub"        : scc$(1)="CS"
	opt_check_format$(2)="Stub, Check"        : scc$(2)="SC"
	opt_check_format$(3)="Stub, Check, Stub"  : scc$(3)="SCS"
	opt_check_format$(4)="Check, Stub, Stub"  : scc$(4)="CSS"
	opt_check_format$(5)="Stub, Stub, Check"  : scc$(5)="SSC"
	opt_check_format$(6)="Stub, Check, Check" : scc$(6)="SCC"
! 
	dim opt_check_type$(3)*14
	opt_check_type$(1)="Regular Check"
	opt_check_type$(2)="Direct Deposit"
	opt_check_type$(3)="All"
! r: set mat wording$
	wc=0 ! Wording$ counter
	wording$(wc+=1)='One'
	wording$(wc+=1)='Two'
	wording$(wc+=1)='Three'
	wording$(wc+=1)='Four'
	wording$(wc+=1)='Five'
	wording$(wc+=1)='Six'
	wording$(wc+=1)='Seven'
	wording$(wc+=1)='Eight'
	wording$(wc+=1)='Nine'
	wording$(wc+=1)='Ten'
	wording$(wc+=1)='Eleven'
	wording$(wc+=1)='Twelve'
	wording$(wc+=1)='Thirteen'
	wording$(wc+=1)='Fourteen'
	wording$(wc+=1)='Fifteen'
	wording$(wc+=1)='Sixteen'
	wording$(wc+=1)='Seventeen'
	wording$(wc+=1)='Eighteen'
	wording$(wc+=1)='Nineteen'
	wording$(wc+=1)='Twenty'
	wording$(wc+=1)='Thirty'
	wording$(wc+=1)='Forty'
	wording$(wc+=1)='Fifty'
	wording$(wc+=1)='Sixty'
	wording$(wc+=1)='Seventy'
	wording$(wc+=1)='Eighty'
	wording$(wc+=1)='Ninety'
! /r
	dim opt_yn$(2)*4
	opt_yn$(1)="Yes"
	opt_yn$(2)="No"
! 
! /r
! r: set default answers and semi-consants and open some files
	ssr1=fnss_employee*.01
	ssr2=fnss_employer*.01
	open #20: "Name=[Q]\PRmstr\prCode.h[cno],Shr",internal,input 
	read #20,using 'Form POS 2,POS 5,N 5': ckno
	close #20: 
	d1=fnPayPeriodEndingDate
	fnGetPayrollDates(old_beg_date,old_end_date,read_qtr1,read_qtr2,read_qtr3,read_qtr4)
	fncreg_read('Prenumbered Checks',pre$)
	fncreg_read('Post to CL',acsclcv$)
	fncreg_read('Post Employer Portion of FiCA',ficam1$)
	fncreg_read('Check Format',sc1$)
	fncreg_read('Print Vacation and Sick Leave on Check',accr$)
	fncreg_read('CL Bank Code',bankcode$) : bankcode=val(bankcode$) : if bankcode=0 then bankcode=1
	fncreg_read('Comp Time Code',compcode$)
	fnDedNames(mat fullname$,mat abrevName$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedSt,mat dedUc,mat gl$)
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #20,using 'Form POS 1,x 120,POS 150,10*C 8,POS 437,15*C 12,N 1': mat d$,mat gln$,gl_installed
	close #20: 

	mat hnames$=abrevName$ : bankgl$=gln$(15)
	if fnclient_has('CL') then let fn_open_acscl

	if bankcode=0 then bankcode=1
	check_number=ckno
! if env$('client')="West Rest Haven" then sc1$="C"
	if env$('client')="Billings" then sc1$="CSS"
	if env$('client')="Divernon" or env$('client')="Thomasboro" or env$('client')="Edinburg" or env$('client')="Philo" or env$('client')="Hope Welty" or env$('client')="Monticello" then ficam1$="Y"
	ddcode$="R"
	fnreg_read('PR.Check print.skip alignment',skip_alignment$) : if skip_alignment$='' then skip_alignment$='No'
	goto MAIN_QUESTIONS ! /r
MAIN_QUESTIONS: ! r:
	fnTos(sn$="prckprt")
	respc=0
	fnLbl(1,1,"Payroll Date:",38,1)
	fnTxt(1,41,10,0,1,"3",0,"")
	resp$(resp_payroll_date:=1)=str$(d1)
	fnLbl(2,1,"Are Checks Prenumbered?",38,1)
	fncomboa("prckprt-2",2,41,mat opt_yn$,"The system needs to know if the checks are already numbered.",3)
	if pre$="Y" then resp$(2)=opt_yn$(1) else resp$(2)=opt_yn$(2)
	fnLbl(3,1,"Beginning Check Number:",38,1)
	fnTxt(3,41,7,0,1,"30",0,"")
	resp$(3)=str$(check_number)
	fnLbl(4,1,"Date of Checks:",38,1)
	fnTxt(4,41,10,0,1,"3",0,"")
	resp$(resp_date_of_checks:=4)=date$("ccYYMMDD")
	fnLbl(5,1,"Beginning Employee Number:",38,1)
	fnTxt(5,41,8,0,1,"30",0,"")
	resp$(5)=str$(beginningEmployeeNumber)
	fnLbl(6,1,"Post to ACS Checkbook",38,1)
	if fnclient_has('CL') then 
		fncomboa("prckprt-3",6,41,mat opt_yn$)
		if acsclcv$="Y" then resp$(6)=opt_yn$(1) else resp$(6)=opt_yn$(2)
	else 
		fnTxt(6,41,3, 0,0,'',1,'ACS Checkbook license not detected.')
		resp$(6)=opt_yn$(2) : acsclcv$='N'
	end if 
	fnLbl(7,1,"Post Employer's Portion of FiCA?",38,1)
	fncomboa("prckprt-4",7,41,mat opt_yn$,"The system can generate and post the employer's portion of FICA at the time the check is being written.",3)
	if ficam1$="Y" then resp$(7)=opt_yn$(1) else resp$(7)=opt_yn$(2)
	fnLbl(8,1,"Check Format:",38,1)
	fncomboa("ckprt-2",8,41,mat opt_check_format$)
	whichScc=srch(mat scc$,sc1$)
	if whichScc>0 then resp$(8)=opt_check_format$(whichScc) else resp$(8)=opt_check_format$(4)
	fnLbl(9,1,"Check Type (Regular or Direct Deposit):",38,1)
	fncomboa("ckprt-5",9,41,mat opt_check_type$,"If you have direct deposits, you can use this option to pr check on plain paper to give the employees.",15)
	if ddcode$="R" then resp$(9)=opt_check_type$(1)
	if ddcode$="D" then resp$(9)=opt_check_type$(2)
	if ddcode$="A" then resp$(9)=opt_check_type$(3)
	fnLbl(10,1,"Print Vacation and Sick Leave?",38,1)
	fncomboa("prckprt-6",10,41,mat opt_yn$)
	if accr$="Y" then resp$(10)=opt_yn$(1) else resp$(10)=opt_yn$(2)
! 
	respc=10
! 
	if cl_installed=0 and exists("[Q]\CLmstr\bankmstr.h[cno]") then 
		fnLbl(11,1,"Bank Account:",38,1)
		fncombof("Bankmstr",11,41,20,"[Q]\CLmstr\bankmstr.h[cno]",1,2,3,15,"[Q]\CLmstr\Bankidx1.h[cno]",1,0, "Select bank account for printing")
		resp$(resp_cl_bankcode:=respc+=1)=str$(bankcode)
	end if 
	if exists("[Q]\PRmstr\hourclass.h[cno]") then 
		fnLbl(12,1,"Comp Time Code:",38,1)
		fncombof("timeclass",12,41,20,"[Q]\PRmstr\hourclass.h[cno]",1,5,6,25,"[Q]\PRmstr\hourclass-idx.h[cno]",1,0, "Select time classification code for comp time, if applicable.")
		resp$(resp_combcode:=respc+=1)=compcode$
	end if 
	fnLbl(14,1,"Print All Checks (or ask after first):",38,1)
	fncomboa("prckprt-prall",14,41,mat opt_yn$)
	resp$(resp_skip_align=respc+=1)=skip_alignment$
	if gl_installed then 
		fnLbl(16,1,"General Ledger detected.",38,1)
	end if 
	if cl_installed then 
		fnLbl(17,1,"Checkbook detected.",38,1)
	end if 
	fncmdkey('Test Check Format',ck_TestCheck:=21)
	fnCmdSet(2) ! need button to show totals
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto XIT ! /r
	if ck=ck_TestCheck then testCheckFormat=1 else testCheckFormat=0
! r: validate answers (and move to local variables from mat resp$)
	d1                       =val(resp$(resp_payroll_date))            ! payroll date
	pre$                     =uprc$(resp$(2)(1:1))                     ! pre-numbered checks Y or N
	check_number             =val(resp$(3))                            ! check #
	ckdat$                   =resp$(resp_date_of_checks)               ! check date
	dat                      =val(ckdat$(5:6)&ckdat$(7:8)&ckdat$(3:4))
	beginningEmployeeNumber =val(resp$(5))                             ! beginning employee #
	acsclcv$                 =uprc$(resp$(6)(1:1))                     ! post Checkbook system
	ficam1$                  =uprc$(resp$(7)(1:1))                     ! post fica match
	sc1$                     =scc$(srch(mat opt_check_format$,resp$(8)))
	ddcode$                  =uprc$(resp$(9)(1:1))                     ! regular check or direct deposit
	accr$                    =uprc$(resp$(10)(1:1))                    ! pr vac and sick
	if resp_cl_bankcode then 
		bankcode               =val(resp$(resp_cl_bankcode)(1:3))        ! bank code
	end if 
	if resp_combcode then 
		compcode$              =resp$(resp_combcode)(1:5)                ! comp time code
	end if 
! date_of_checks=val(ckdat$)
	prdmmddyy=val(ckdat$(5:6))*10000+val(ckdat$(7:8))*100+val(ckdat$(3:4)) ! convert date back to mmddyy format
	skip_alignment$         =resp$(resp_skip_align)
	if skip_alignment$='Yes' then allign=3
! 
	if acsclcv$="Y" then cl_installed=1 else cl_installed=0
	if ficam1$="Y" then ficam1=1 else ficam1=0
	if pre$="Y" then pre=1 else pre=0
! if env$('client')="Washington Parrish" and (prdate<10100 or prdate>123199) then goto MAIN_QUESTIONS
	beg_date=val(str$(d1)(1:4)&'0101')
	end_date=val(str$(d1)(1:4)&'1231')
	qtr1=val(str$(d1)(1:4)&str$(read_qtr1)(5:8))
	qtr2=val(str$(d1)(1:4)&str$(read_qtr2)(5:8))
	qtr3=val(str$(d1)(1:4)&str$(read_qtr3)(5:8))
	qtr4=val(str$(d1)(1:4)&str$(read_qtr4)(5:8))
	!
	! if d1<old_beg_date or d1>old_end_date then ! not this year
	!   mat ml$(4)
	!   ml$(1)='The Payroll Date you have chosen ('&cnvrt$('pic(zzzz/zz/zz)',d1)&') is is outside your years'
	!   ml$(2)='beginning  and ending date range ('&cnvrt$('pic(zzzz/zz/zz)',old_beg_date)&' - '&cnvrt$('pic(zzzz/zz/zz)',old_end_date)&').'
	!   ml$(3)='Checks with a payroll date outside this date range can not be processed.'
	!   ml$(4)='Would you like to "Change Payroll Dates" now?'
	!   fnmsgbox(mat ml$,resp$,'',16+4)
	!   if resp$='Yes' then 
	!     fnchain('S:\Payroll\Change Payroll Dates')
	!   else ! if resp$='No' then
	!     goto MAIN_QUESTIONS
	!   end if 
	! end if 
! pause
! 
	if check_number<0 then 
		mat ml$(2)
		ml$(1)="You must enter a valid check number!"
		ml$(2)="Click OK to return to previous screen. "
		fnmsgbox(mat ml$,resp$)
		goto MAIN_QUESTIONS
	end if 
! /r
! r: save answers for next time
	fncreg_write('Prenumbered Checks',pre$)
	fncreg_write('Post to CL',acsclcv$)
	fncreg_write('Post Employer Portion of FiCA',ficam1$)
	fncreg_write('Check Format',sc1$)
	fncreg_write('Print Vacation and Sick Leave on Check',accr$)
	fncreg_write('CL Bank Code',str$(bankcode))
	fncreg_write('Comp Time Code',compcode$)
	fnreg_write('PR.Check print.skip alignment',skip_alignment$)
! /r
! r: get and validate bank code if ACSCL is in play
	if testCheckFormat then goto L1300
	if cl_installed=1 then 
		read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi nokey L1280 ioerr L1290
	end if 
	goto L1300
	L1280: ! 
	mat ml$(2)
	ml$(1)="You must enter a valid bank code!"
	ml$(2)="Click OK to return to previous screen. "
	fnmsgbox(mat ml$,resp$)
	goto MAIN_QUESTIONS
	L1290: ! 
	mat ml$(3)
	ml$(1)="You have indicated that you want to post checkbook, "
	ml$(2)="but no checkbook files can be found! "
	ml$(3)="Click OK to return to previous screen. "
	fnmsgbox(mat ml$,resp$)
	goto MAIN_QUESTIONS
L1300: ! /r
! if env$('client')<>"Washington Parrish" then prdate=d1
	if ~testCheckFormat then
		if cl_installed then 
			open #7: "Name=[Q]\PRmstr\MGLMSTR.h[cno],KFName=[Q]\PRmstr\MGLIDX1.h[cno],Shr",internal,input,keyed 
		end if 
		open #praddr:=1: "Name=[Q]\PRmstr\prAddr1.h[cno],Shr",internal,input 
		open #hEmployee:=fngethandle: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\rpindex.h[cno],Shr",internal,input,keyed 
		open #hDepartment:=fngethandle: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed 
		open #3: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
		open #breakdown=31: "Name=[Q]\PRmstr\HourBreakdown.H[cno],KFName=[Q]\PRmstr\HourBreakdown-idx.H[cno]",internal,outIn,keyed 
		open #dd=30: "Name=[Q]\PRmstr\DD.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],Shr,kps=1,kln=10,Use",internal,outIn,keyed 
		if fnclient_has('GL') and gl_installed=1 then 
			gl_installed=0
			open #h_gl_glbrec:=fngethandle: "Name=[Q]\GLmstr\GLBREC.h[cno],KFName=[Q]\GLmstr\GLRECIDX.h[cno],Shr",internal,outIn,keyed ioerr L1440
			gl_installed=1
			L1440: ! 
		end if 
	end if
	MAIN_LOOP_TOP: ! 
	s1=1
	ReadNextEmployee: ! 
		if testCheckFormat then
			fn_getTestValues
			goto L1570
		else
			read #hEmployee,using 'form pos 1,n 8,3*c 30,pos 132,2*pd 4.2,pos 162,n 6': eno,mat em$,em10,em11,lpd eof EO_RPMSTR
		end if
		mat v=(0) : v1=1
		! If env$('client')="WashingtonParrish" Then Goto 1110
		dd$=""
		read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17",key=rpad$(str$(eno),10): key$,dd$,rtn,acc,acn nokey ignore
		if uprc$(dd$)="Y" and ddcode$="D" then goto L1570
		if uprc$(dd$)<>"Y" and ddcode$="R" then goto L1570
		if ddcode$="A" then goto L1570 ! all
	goto ReadNextEmployee
	L1570: !
	if beginningEmployeeNumber>eno then goto ReadNextEmployee ! start with certain employee
	tdepXcount=0
	mat tdep=(0)
	tdc1=tdc2=tdc3=tdc4=tdc5=0
	tpd3=tpd4=tpd5=0
	tdct=0
	rate=0
	s1=1
	! If fndate_mmddyy_to_ccyymmdd(LPD)><D1 Then Goto 1360  ! with comment can reprint any payroll
	if ~testCheckFormat then 
		fn_determine_earnings
	end if
	if pr_prNetZeroChecks$='True' and ttc(32)=0 and fndate_mmddyy_to_ccyymmdd(lpd)=d1 then goto ProduceThatCheck ! pr zero checks
	if ttc(32)=0 then goto ReadNextEmployee ! no earnings
	ProduceThatCheck: ! 
	! Mat TCP=(0)
	! Mat TDC=(0)
	goto PRE_CHECK

PRE_CHECK: ! 
	ttc(26)=ttc(26)-tpd3-tpd4-tpd5
	ttc(28)=ttc(28)+ttc(29)+ttc(30) ! OTHER COMP-CURRENT
	ttc(1)=ttc(1)-ttc(25) : tty(1)=tty(1)-tty(25)
L2070: ! 
	if cl_installed=1 then let fn_cknum
	fnopenprn
	if sc1$="SCS" then let fn_print_stub : fn_print_check : fn_print_stub
	if sc1$="CSS" then let fn_print_check : fn_print_stub : fn_print_stub
	if sc1$="SSC" then let fn_print_stub : fn_print_stub : fn_print_check
	if sc1$="SCC" then let fn_print_stub : fn_print_check : fn_print_check
	if sc1$="CS" then let fn_print_check : fn_print_stub
	if sc1$="SC" then let fn_print_stub : fn_print_check
L2150: ! 
	if fp(d1*.01)>.9 then 
		hd1=19000000+fncd(d1)
	else 
		hd1=20000000+fncd(d1)
	end if 
! hsk$=lpad$(str$(eno),8)&cnvrt$("PD 6",hd1)
	if ~testCheckFormat and (allign=3 or skip_alignment$='Yes') then 
		pr #255: chr$(12) ! NEWPAGE
		goto CHECK_PRINT_TOP
	end if 
	fncloseprn
	if testCheckFormat then let fnChain(program$)
	goto ALLIGNMENT

ALLIGNMENT: ! r:
	fnTos(sn$="prckprt2")
	respc=0 : rc=0
	fnOpt(1,3,"Reprint same check",0,franum)
	resp$(rc+=1)="False"
	fnOpt(2,3,"Print next",0,franum)
	resp$(rc+=1)="False"
	fnOpt(3,3,"Print all remaining",0,franum)
	resp$(rc+=1)="True"
	if env$('client')='Billings' then resp$(2)='True' : resp$(3)='False'
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck) ! allignment
	if resp$(1)="True" then allign=1
	if resp$(2)="True" then allign=2
	if resp$(3)="True" then allign=3
	if ck=5 then getout=1: allign=2: goto CHECK_PRINT_TOP ! write history on last check and quit
	on allign goto REPRINT_SAME_CHECK,CHECK_PRINT_TOP,CHECK_PRINT_TOP none ALLIGNMENT
! /r
REPRINT_SAME_CHECK: ! 
	if pre=0 then goto L3130
	if cl_installed=1 then let fn_build_check_record
	check_number=check_number+1
L3130: ! 
	goto L2070

CHECK_PRINT_TOP: ! 
	if cl_installed=1 then let fn_build_check_record
	tdc1=0
	tdc2=0
	tdc3=tdc4=tdc5=0
	tpd3=tpd4=tpd5=0
	tdct=0
	rate=0
	ttc(32)
	mat dept=(0)
	if gl_installed=1 then ! r: update GL's GLBREC
		read #h_gl_glbrec,using 'form pos 1,c 12',key=bankgl$&lpad$(rtrm$(str$(check_number)),12): gl$ nokey L3300
	else 
		goto L3320
	end if 
	rewrite #h_gl_glbrec,using F_GL_GLBREC: bankgl$,lpad$(rtrm$(str$(check_number)),12),em$(1),"PR",dat,ttc22,0
	goto L3320
! 
L3300: ! 
	write #h_gl_glbrec,using F_GL_GLBREC: bankgl$,lpad$(rtrm$(str$(check_number)),12),em$(1),"PR",dat,ttc22,0
	F_GL_GLBREC: form pos 1,2*c 12,c 30,c 2,n 6,pd 5.2,n 1
L3320: ! /r
	if getout=1 then goto FINIS
	ttc(32)=0
	check_number=check_number+1
goto MAIN_LOOP_TOP
def fn_EnglishAmount(dol,mat eng$; n1)
 ! pass amount in dol, returned in mat eng$
 ! n1 = break point (58 is default)
 ! n2  used to hardcoded to 58, let's try setting it to n1 instead 12/29/2017
 !
	if n1=0 then let n1=58
	n2=n1
	dol=ttc(32)
	if dol<=0 then 
		eng$="*** VOID ***"
		goto L3810
	else if dol=>10**8 then 
		eng$="Value too big for editing"
		goto L3810
	end if 
	eng$="***"
	amount(1)=int(dol*100+.500000001)
	for a0=2 to 10
		amount(a0)=int(amount(a0-1)/10+.000000001)
	next a0
	for a0=1 to 10
		amount(a0)=amount(a0)-amount(a0+1)*10
	next a0
	if amount(11)+amount(10)+amount(9)=0 then goto L3530
	a0=9
	fn_engDol_hundred
	eng$=rtrm$(eng$)&" Million"
	L3530: ! 
	if amount(8)+amount(7)+amount(6)=0 then goto L3570
	a0=6
	fn_engDol_hundred
	eng$=rtrm$(eng$)&" Thousand"
	L3570: if amount(5)+amount(4)+amount(3)=0 then goto L3600
	a0=3
	fn_engDol_hundred
	L3600: ! 
	if dol>=1 then goto L3620
	eng$=rtrm$(eng$)&" Zero"
	L3620: ! 
	eng$=rtrm$(eng$)&" Dollar"
	if dol<2 and dol>=1 then goto L3660
	eng$=rtrm$(eng$)&"s"
	if len(rtrm$(eng$))>n2 then goto L3660
	L3660: ! 
	eng$=rtrm$(eng$)&" and"
	if amount(2)+amount(1)=0 then 
		eng$=rtrm$(eng$)&" Zero" 
	else
		amount(3)=0
		a0=1
		fn_engDol_hundred
	end if
	eng$=rtrm$(eng$)&" Cent"
	if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3760
	eng$=rtrm$(eng$)&"s"
	L3760: ! 
	if len(rtrm$(eng$))<n2 then goto L3810
	for j=1 to 9
		n1=(n2+1)-j
		if eng$(n1:n1)=" " then goto L3810
	next j
	L3810: ! 
	fn_EnglishAmount=n1
fnend
def fn_engDol_hundred
	if amount(a0+2)<>0 then 
		eng$=rtrm$(eng$)&" "&wording$(amount(a0+2))
		eng$=rtrm$(eng$)&" Hundred"
	end if
	if amount(a0+1)=0 and amount(a0)=0 then goto L3920
	if amount(a0+1)<2 then goto L3910
	eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)+18)
	if amount(a0)=0 then goto L3920
	amount(a0+1)=0
	L3910: ! 
	eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)*10+amount(a0))
	L3920: ! 
fnend 
EO_RPMSTR: ! r:
	close #hEmployee: 
	close #3: 
	if gl_installed=1 then close #h_gl_glbrec: 
	fncloseprn
	goto FINIS ! /r
FINIS: ! 
	if cl_installed=1 and allign=4 then let fn_build_check_record
XIT: fnxit
def fn_open_acscl
	open #h_cl_bank:=12: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed ioerr L4220
	cl_installed=1
	! not used - commented out 8/13/19 jb    			open #15: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,outIn,relative 
	open #h_cl_payee:=fngethandle: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed 
	! open #14: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed 
	open #h_cl_trans:=fngethandle: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,outIn,keyed 
	open #22: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed 
	!   if exists("[Q]\CLmstr\Tralloc-Idx.h[cno]") then
	open #h_cl_trans_alloc:=fngethandle: "Name=[Q]\CLmstr\TrAlloc.h[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,outIn,keyed 
	!   else 
	!     open #h_cl_trans_alloc:=f: "Name=[Q]\CLmstr\TrAlloc.h[cno],Shr",internal,outIn,relative
	!   end if
	open #h_cl_glmstr:=fngethandle: "Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLINDEX.h[cno],Shr",internal,outIn,keyed 
	read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,ckno nokey L4200
	L4200: ckno=ckno+1
	! dAT=VAL(DATE$(4:5)&DATE$(7:8)&DATE$(1:2))
	L4220: ! 
fnend 
def fn_build_check_record
	tr$(1)=cnvrt$("N 8",check_number)
	tr$(2)=cnvrt$("N 6",dat)
	tr$(3)=cnvrt$("N 10.2",ttc(32))
	tr$(4)=cnvrt$("N 8",eno)
	tr$(5)=em$(1)
	if allign>1 then 
		ded$(1)=str$(ttc(31))
		for j=2 to 25
			if j=3 then ded$(j)=str$(ttc(2)) : goto L4390
			! ADD MEDICARE TO FICA  no kj
			ded$(j)=str$(ttc(j-1))
		L4390: !
		next j
		! dED$(25)=STR$(TTC(29)) ! meals
		! dED$(26)=STR$(TTC(30)) ! tips
		! dED$(27)=STR$(TDC1) ! reg hourly rate
		! dED$(28)=STR$(TDC2) ! ot rate
		if tdc1=0 then goto L4480
		! dED$(29)=STR$(INT(TDC1/40+.99))
	else
		tr$(3)=tr$(4)=""
		tr$(5)="VOID"
		mat ded$=("")
	end if
	goto WRITE_PAYROLL_TRANS

	L4480: ! If EM(5)=1 Then dED$(29)="4" ! weeks worked
	if em(5)=2 then ded$(29)="2"
	if em(5)=3 then ded$(29)="2"
	if em(5)=4 then ded$(29)="1"

	WRITE_PAYROLL_TRANS: ! 
	if testCheckFormat then goto L5250
	! pr tr$(1),tr$(5) :  pause
	mat tr=(0)
	! r: removed existing CL Check (and it's allocations) first
	clk$=lpad$(str$(bankcode),2)&"1"&tr$(1)
	read #h_cl_trans,using 'form pos 79,2*pd 3',key=clk$: nt1 nokey L4610
	delete #h_cl_trans,key=clk$: 
	key$=lpad$(str$(bankcode),2)&str$(tcde)&rpad$(tr$(1),8)
	restore #h_cl_trans_alloc,key>=key$: nokey L4610
	L4590: ! 
	read #h_cl_trans_alloc,using 'Form Pos 1,C 11': newkey$ eof L4610
	if newkey$=key$ then 
		delete #h_cl_trans_alloc: 
		goto L4590
	end if 
	L4610: ! 
	! /r
	!   if exists("[Q]\CLmstr\Tralloc-Idx.h[cno]") then
	tx3=val(tr$(3))
	tr2=val(tr$(2))
	if pr_prEmpToClPayee$='False' then 
		tr$(4)='' !  do not populate CL Payee Number as PR Employee number, just leave it blank instead. 5/22/19 JB - Do this for everyone.
		! laura and john agree that it seems like this should be this way for everyone...  if we get any more compalints
		! 8/13/19 - we got complaints, not everyone liked it - made it an option for the one person who wanted it.
	end if
	! tr$(4)(0:0)=trim$(pr_clPayeePrefix$)
	write #h_cl_trans,using F_CL_TRANS_V1: bankcode,1,tr$(1),tr2,tx3,tr$(4),tr$(5),0,clr,4
	read #h_cl_payee,using 'form pos 129,pd 5.2',key=lpad$(rtrm$(tr$(4)),8): ytdp nokey L4690 ! UPDATE PAYEE FILE
	ytdp=ytdp+val(tr$(3)) conv ignore
	L4690: ! 
	read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey NoKeyOnClBank
	bn$=rtrm$(bn$)
	bal=bal-val(tr$(3)) conv ignore
	rewrite #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2): bn$,bal,upi,tr$(1) nokey NoKeyOnClBank
	F_CLFILE_12: form pos 3,c 30,pos 45,pd 6.2,pd 6.2,g 8
	NoKeyOnClBank: ! 
	F_CL_TRANS_V1: form pos 1,n 2,n 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
	! WRITE ALLOCATIONS
	if allign=1 then goto L5250
	for j=1 to 29
	 if val(ded$(j))=0 then goto L5230
	 gl$=""
	 on j goto L4840,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L5220_Next_J1,L5220_Next_J1,BCR_GLN_VALIDATED none L5230

	 L4840: ! 
	 for j1=1 to tdepXcount
		 if j<6 or j>25 then goto L4870 ! kj 91707
		 if dedcode(j-5)=3 then goto L5230 ! kj 91707  don't write entries for benefits
		 L4870: ! 
		 alloc=tdep(j1,1)
		 gl$=cnvrt$("N 3",tdep(j1,2))&cnvrt$("N 6",tdep(j1,3))&cnvrt$("N 3",tdep(j1,4))
		 sd5$="Gross Pay"
		 goto L4990
		 L4910: ! 
		 if j=2 then sd5$="Federal WH" : gl$=gln$(1)
		 if j=3 then sd5$="FICA WH" : gl$=gln$(2) : fica0=val(ded$(j))
		 if j=4 then sd5$="Medicare" : gl$=gln$(2) : medi0=val(ded$(j)): goto L4990
		 if j=5 then sd5$="State WH" : gl$=gln$(3)
		 if j>5 and j<26 then sd5$=abrevName$(j-5) : gl$=gl$(j-5)
		 if j=26 then gl$=gln$(1): sd5$="eic" : goto L4990 ! use federal
		 if j=27 then goto L4990 ! skip tips i think
		 ! If J=28 Then gL$=GLN$(1): sD5$="Meals" : Goto 4890 ! use wages
		 L4990: ! 
		 cd1=1
		 read #h_cl_glmstr,using F_CL_GLMSTR,key=rpad$(gl$,kln(h_cl_glmstr)),release: de$ nokey INVALIDGLNUMBER
		 F_CL_GLMSTR: form pos 13,c 30
		 BCR_GLN_VALIDATED: ! 
		 if j>1 then alloc=val(ded$(j))
		 if j=29 then miscode=(alloc*100)+29 else miscode=j
		 ! store # of deduction in the invoice date field;
		 ! if weeks worked store weeks worked and code both
		 if j>1 then alloc=-alloc
		 if j<6 or j>25 then goto L5070 ! all tax w/h = negative
		 if dedcode(j-5)=2 then alloc=-alloc ! reverse on additions to net
		 L5070: ! 
		 if j=30 then alloc=0 ! meals
		 !       if env$('client')="Washington Parrish" and j=16 then alloc=0 ! don't allow tips to post on washington parrish
		 if j=4 and ficam1=1 then alloc=alloc-medic3 ! prb 2012
		 if j=3 and ficam1=1 then alloc=alloc-ficam3 ! prb 2012
		 if alloc<>0 then  ! write non-zero allocations
			 lr3=lrec(h_cl_trans_alloc)+1
			 if j=3 then fica1=alloc : fica_rec=lr3
			 if j=4 then medi1=alloc
			 write #h_cl_trans_alloc,using F_ClTransAlloc,rec=lr3: bankcode,1,val(tr$(1)),gl$,alloc,de$(1:30),miscode,0
			 F_ClTransAlloc:  form pos 1,n 2,n 1,g 8,c 12,pd 5.2,c 30,n 6,pd 3
		 end if
		 L5220_Next_J1: ! 
	 if j=1 then next j1
	 L5230: ! 
	next j
	fn_mgl
	L5250: ! 
fnend 
def fn_cknum ! check for duplicate check numbers
	if testCheckFormat then goto L5720
	L5410: ! 
	dk$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(check_number),8)
	read #h_cl_trans,using L5440,key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey L5720
	dtr$(3)=str$(dtr3)
	L5440: form pos 4,c 8,g 6,pd 10.2,c 8,c 35,pos 79,2*pd 3
	DUPLICATE_CHECK: ! 
	fnTos(sn$="Prckprt4")
	respc=0: mypos=50
	fnLbl(1,1,"Check Number "&str$(check_number)&" has been previously used!",50,1)
	fnLbl(3,1,"Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",val(dtr$(2))),50,0)
	fnLbl(4,1,"Amount: "&dtr$(3),50,0)
	fnLbl(5,1,"To: "&rtrm$(dtr$(5)),50,0)
	fnLbl(7,1,"Click          to Delete the previous entry else" ,50,1)
	fnButton(7,10,"Delete",3,"Press Delete to delete the old check from history and replace it with the new check, else",1,6)
	text$="enter the correct check number for "&trim$(dtr$(5))&":"
	textlenght=len(trim$(text$))
	fnLbl(8,4,text$,textlenght,0)
	fnTxt(8,textlenght+7,7,0,1,"30",0,"")
	resp$(respc+=1)=str$(check_number)
	fnCmdKey("&Next",1,1,0,"Continue with checkprinting." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! dupllicate check number
	if ckey=5 then goto XIT
	if ckey=3 then goto L5670 ! if delete
	ckn2=val(resp$(1))
	if ckn2=0 then goto DUPLICATE_CHECK
	check_number=ckn2
	tr$(1)=lpad$(str$(ckn2),8)
	goto L5410

	L5670: ! 
	bal=bal+val(dtr$(3))
	delete #h_cl_trans,key=dk$: 
	key$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(check_number),8)
	restore #h_cl_trans_alloc,key>=key$: nokey L5720
	do
		read #h_cl_trans_alloc,using 'Form Pos 1,C 11': newkey$ eof L5720
		if newkey$=key$ then 
			delete #h_cl_trans_alloc: 
		end if 
	loop while newkey$=key$
	L5720: ! 
fnend 
def fn_mgl ! WRITE BENEFITS & FICA MATCH
	cd1=2
	for j=1 to tdepXcount
		mat mgl$=("            ")
		read #7,using L5790,key=lpad$(str$(tdep(j,5)),3): mat mgl$ nokey L5800
		L5790: form pos 4,11*c 12
		L5800: ! 
		for j2=6 to 16
			if tdep(j,j2)=0 then goto L5980
			if j2>6 then goto L5870
			if ficam1=0 then goto L5980
			sd5$=de$="FICA Match"
			fica2=fica2+tdep(j,j2)
			j4=3
			goto L5900
			L5870: ! 
			if dedcode(j2-6)><3 then goto L5980
			j4=j2-2
			sd5$=de$=rtrm$(abrevName$(j4-4))&" Match"
			L5900: ! 
			gl$=mgl$(j2-5)
			read #h_cl_glmstr,using F_CL_GLMSTR,key=gl$,release: de$ nokey INVALIDGLNUMBER
			! 
			! 
			EXLNKD_L5920: ! 
			if ~testCheckFormat then
				lr3=lrec(h_cl_trans_alloc)+1
				write #h_cl_trans_alloc,using F_ClTransAlloc,rec=lr3: bankcode,1,val(tr$(1)),gl$,tdep(j,j2),de$(1:30),j4,0
			end if
			L5980: ! 
		next j2
	next j
	! fn_FICA_FIX
fnend 
! r: Check pr routines
def fn_print_check
	if ttc(32)<=0 then 
		ca$="***VOID***"
	else 
		ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",ttc(32)))
	end if 
	!
	if env$('client')="Merriam Woods" then 
		englishAmountBreakPoint=65
	else if env$('client')="Payroll Done Right" and env$('cno')='18' then
		englishAmountBreakPoint=82
	else
		englishAmountBreakPoint=0   !   0=default=58
	end if
	n1=fn_EnglishAmount(ttc(32),mat eng$, englishAmountBreakPoint)
	!
	if uprc$(ddcode$)="D" then eng$="Direct Deposit" : ca$="V O I D"
	!
	if env$('client')="ACS" then 
		fn_check_acs
	else if env$('client')="Ash Grove" then 
		fn_check_legacy(3,3)
	else if env$('client')="Bethany" then 
		fn_check_bethany
	else if env$('client')="Billings" then 
		fn_check_billings
	else if env$('client')="Crockett County" then 
		! if env$('acsDeveloper')<>"" then pause
		fn_check_dynamic(26,7,7,9,12,58,0,6)
		! length,line_date,line_amount,line_amount_english,line_name_and_address; pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,checkNumber,pos_amount_english
	else if env$('client')="Campbell" then ! r: updated 1/17/2018 - uses very few options
		length                 =26
		line_date              =13
		line_amount            =13
		line_amount_english    =9
		line_name_and_address  =15
		pos_date               =57
		pos_amt                =69
		fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt) ! /r
	else if env$('client')="Carr Plumbing" then ! r:
		length                = 26
		line_date             =  4
		line_amount           =  7
		line_amount_english   = 10
		line_name_and_address = 14
		pos_date              = 78
		pos_amt               = 72
		line_nameOnly         =  7
		pos_nameOnly=0 ! default to 12
		fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly) ! /r
	else if env$('client')="Cerro Gordo" then 
		fn_check_cerrogordo
	else if env$('client')="Cerro Gordo T" then ! r:
		length                = 27
		line_date             = 14
		line_amount           = 14
		line_amount_english   =  9
		line_name_and_address = 15
		pos_date              = 55
		pos_amt               = 71
		line_nameOnly         =  0
		pos_nameOnly          =  0
		fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly) ! /r
	else if env$('client')="Divernon" then 
		fn_check_divernon
	else if env$('client')="Edinburg" then 
		fn_check_edinburg
	else if env$('client')="Edison" then ! r: 6/29/2017
		length                = 25
		line_date             =  6
		line_amount           = 11
		line_amount_english   = 9
		line_name_and_address = 13  ! 0/not printing is the default
		pos_date              = 74  ! 65 is default
		pos_amt               = 79 ! pos_date+18 is default
		line_nameOnly         =  0
		pos_nameOnly          =  0
		line_checkNumber      = line_date
		pos_checkNumber       = 83
		fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number)
		! /r
	else if env$('client')="Energy Exchanger" then ! r: 5/9/2017 
		length                = 23
		line_date             =  4
		line_amount           = 12
		line_amount_english   = 9
		line_name_and_address = 13
		pos_date              = 75
		pos_amt               = 80
		line_nameOnly         =  0
		pos_nameOnly          =  0
		line_checkNumber      = line_date
		pos_checkNumber       = 86
		fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number)
		! /r
	else if env$('client')="Hope Welty" then 
		fn_check_hope_welty
	else if env$('client')="Kathys Bookkeeping" then 
		fn_check_dynamic(23,9,9,6,10, 58,72)
	else if env$('client')="Lamar" then 
		fn_check_lamar
	else if env$('client')="Lovington" then 
		fn_check_lovington
	else if env$('client')="Merriam Woods" then 
		fn_check_merriam_woods
	else if env$('client')="Monticello" then 
		fn_check_monticello
	else if env$('client')="Philo" then 
		fn_check_philo
	else if env$('client')="Thomasboro" then 
		fn_check_thomasboro
	else if env$('client')="Thomas Richardson" then 
		fn_check_tom_richardson
	else if env$('client')="Unity" then 
		fn_check_unity
	else if env$('client')="Payroll Done Right" then ! env$('client')="West Accounting" or 
		if env$('cno')='18' then ! r: 12/29/2017  (most recent)
			length                = 25
			line_date             =  3
			line_amount           =  7
			line_amount_english   =  9
			line_name_and_address =  0  ! 0/not printing is the default
			pos_date              = 80  ! 65 is default
			pos_amt               = 75 ! pos_date+18 is default
			line_nameOnly         =  7
			pos_nameOnly          =  8
			line_checkNumber      =  0
			pos_checkNumber       =  0
			pos_amount_english    =  1   ! default is 9
			fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number,pos_amount_english)
			! /r
		else
			fn_check_payrollDoneRight
		end if
	else 
		fn_check_dynamic(21,6,6,7,13) ! fn_check_legacy ! default for everyone without a special routine...
	end if 
fnend 
def fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address; pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,checkNumber,pos_amount_english)
	! 
	if pos_date=0 then pos_date=65
	if pos_amt=0 then pos_amt=pos_date+18
	if pos_nameOnly=0 then pos_nameOnly=12
	if pos_amount_english=0 then pos_amount_english=9
	! 
	for line_item=1 to length
! if line_item=4 the pr line_item : pause
		if line_item=line_date and line_item=line_amount then 
			pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),pos pos_amt,c 18': dat,ca$
		else if line_item=line_date and line_item=line_checkNumber and line_checkNumber<>0 then 
			pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),pos pos_checkNumber,N 8': dat,checkNumber
		else if line_item=line_nameOnly and line_item=line_amount then
			pr #255,using "form Pos pos_nameOnly,C 30,pos pos_amt,c 18": em$(1),ca$
		else if line_item=line_nameOnly and line_item=line_date then
			! pause
			if pos_date<pos_nameOnly then
				pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),Pos pos_nameOnly,C 30': dat,em$(1)
			else
				pr #255,using 'form Pos pos_nameOnly,C '&str$(min(30,pos_date-pos_nameOnly))&',Pos pos_date,pic(ZZ/ZZ/ZZ)': em$(1)(1:pos_date-pos_nameOnly),dat
			end if
		else if line_item=line_date then 
			pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ)': dat
		else if line_item=line_nameOnly then
			pr #255,using "form Pos pos_nameOnly,C 30": em$(1)
		else if line_item=line_amount and line_item=line_name_and_address then
			pr #255,using "form Pos 12,C 30,pos pos_amt,c 18": em$(1),ca$
			pr #255,using "form Pos 12,C 30": em$(2) : line_item+=1
			pr #255,using "form Pos 12,C 30": em$(3) : line_item+=1
		else if line_item=line_amount_english then 
			pr #255,using 'form Pos pos_amount_english,C n1': eng$(1:n1)
			pr #255,using 'form Pos pos_amount_english,C 62': eng$(n1+1:inf) : line_item+=1
		else if line_item=line_name_and_address then 
			pr #255,using "form Pos 12,C 30": em$(1)
			pr #255,using "form Pos 12,C 30": em$(2) : line_item+=1
			pr #255,using "form Pos 12,C 30": em$(3) : line_item+=1
		else if line_item=line_amount then
			pr #255,using "form pos pos_amt,c 18": ca$
		else 
			pr #255: ""
		end if 
	next line_item
fnend 
def fn_check_legacy(; extra_lines_at_top,extra_lines_at_bottom)
	for j=1 to extra_lines_at_top
		pr #255: ""
	next j
	fn_check_dynamic(21+extra_lines_at_bottom,6,6,7,13)
fnend 
def fn_check_acs
	pr #255,using 'Form skip 3,POS 80,PIC(ZZ/ZZ/ZZ)': dat
	pr #255,using 'Form skip 2,POS 10,c 30,pos 74,c 18': em$(1),ca$
	pr #255,using 'Form skip 2,pos 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	for j=1 to 3
		pr #255: ""
	next j
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255,using "form pos 1,c 1,skip 7": ""
fnend 
def fn_check_bethany
	for j=1 to 5
		pr #255: ""
	next j
	if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	datepos=65
	pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	for j=1 to 3
		pr #255: ""
	next j
	pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255: 
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255,using "form pos 1,c 1,skip 9": "" ! changed skip 6 to skip 9 on 10/17/2016
fnend 
def fn_check_billings
	pr #255: ""
	pr #255: ""
	pr #255,using 'form pos 40,c 38,skip 1': "Void After 60 Days"
	pr #255: ""
	pr #255: ""
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255,using 'Form POS 63,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255,using "Form Pos 12,C 60": em$(1)
	pr #255,using "Form Pos 12,C 60": em$(2)
	pr #255,using "Form Pos 12,C 60": em$(3)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_cerrogordo
	pr #255: "" : pr #255: "" : pr #255: ""
	pr #255: "" : pr #255: "" : pr #255: ""
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: ""
	pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
	pr #255: ""
	pr #255: ""
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_divernon
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255,using "Form Pos 12,C 60": em$(1)
	pr #255,using "Form Pos 12,C 60": em$(2)
	pr #255,using "Form Pos 12,C 60": em$(3)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_edinburg
	for j=1 to 5
		pr #255: ""
	next j
	if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	datepos=65
	pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	for j=1 to 3
		pr #255: ""
	next j
	pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255: 
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255,using "form pos 1,c 1,skip 6": ""
	pr #255: : pr #255: 
fnend 
def fn_check_hope_welty
	pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255: : pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: 
	pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
	x=3
	for j=1 to x
		pr #255: ""
	next j
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_lamar
	for j=1 to 9
		pr #255: ""
	next j
	if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	datepos=65
	pr #255,using 'Form POS datepos,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	for j=1 to 3
		pr #255: ""
	next j
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255,using "form pos 1,c 1,skip 6": ""
fnend 
def fn_check_lovington
	pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255,using "form pos 76,n 8,skip 1": check_number
	pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255: ''
	pr #255,using 'Form POS 63,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: ""
	pr #255,using "Form Pos 12,C 60": em$(1)
	pr #255,using "Form Pos 12,C 60": em$(2)
	pr #255,using "Form Pos 12,C 60": em$(3)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_monticello
	pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: : pr #255: 
	pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 2,C 18': dat,ca$
	pr #255: ""
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_merriam_woods
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: ""
	pr #255,using 'Form POS 65,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255: ""
	pr #255: ""
	pr #255,using "Form Pos 12,C 60": em$(1)
	pr #255,using "Form Pos 12,C 60": em$(2)
	pr #255,using "Form Pos 12,C 60": em$(3)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_philo
	pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: : pr #255: 
	pr #255,using 'Form POS 57,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	x=3
	for j=1 to x
		pr #255: ""
	next j
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_tom_richardson
	!   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255:
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255,using 'Form POS 67,PIC(ZZ/ZZ/ZZ)': dat
	pr #255,using 'Form POS 9,C 62,X 4,C 18': eng$(1:n1),ca$
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: ""
	pr #255,using "Form Pos 12,C 60": em$(1)
	pr #255,using "Form Pos 12,C 60": em$(2)
	pr #255,using "Form Pos 12,C 60": em$(3)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_thomasboro
	pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	pr #255: : pr #255: : pr #255: 
	pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
	x=3
	for j=1 to x
		pr #255: ""
	next j
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_check_unity
	for j=1 to 5
		pr #255: ""
	next j
	if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
	datepos=65
	pr #255: 
	pr #255,using 'Form POS 9,C 62': eng$(1:n1)
	pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
	for j=1 to 3
		pr #255: ""
	next j
	pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
	pr #255: 
	for j=1 to 3
		pr #255,using "Form Pos 12,C 60": em$(j)
	next j
	pr #255,using "form pos 1,c 1,skip 6": ""
fnend 
def fn_check_payrollDoneRight
	for j=1 to 8
		pr #255: ""                                                ! line 1-8
	next j
	pr #255,using 'Form Pos 65,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$  ! line 9
	pr #255,using 'Form Pos 9,C 62': eng$(1:n1)                   ! line 10
	pr #255,using 'Form Pos 9,C 62': eng$(n1+1:128)               ! line 11
	pr #255: ""                                                   ! line 12
	pr #255: ""                                                   ! line 13
	pr #255: ""                                                   ! line 14
	pr #255,using "Form Pos 12,C 60": em$(1)                     ! line 15
	pr #255,using "Form Pos 12,C 60": em$(2)                     ! line 16
	pr #255,using "Form Pos 12,C 60": em$(3)                     ! line 17
	for j=1 to 8
		pr #255: ""                                                ! line 18-25
	next j
fnend 
! /r
! r: Stub pr routines
def fn_print_stub
	tdedcp=tdedytd=0
	for j=1 to 23
		if j<5 then 
			goto L2230
		else if dedcode(j-4)=2 then 
			tdedcp=tdedcp-ttc(j)
			tdedytd=tdedytd-tty(j)
			goto L2240
		else if dedcode(j-4)=3 then 
			goto L2240
		end if 
		L2230: ! 
		tdedcp=tdedcp+ttc(j): tdedytd=tdedytd+tty(j)
		L2240: ! 
	next j
	if env$('client')='Billings' then 
		fn_stub_billings
	else if env$('client')="Edison" then 
		fn_stub_standard( 0,2)
	else if env$('client')="Kathys Bookkeeping" then 
		fn_stub_standard( 0,5)
	else if env$('client')='Kincaid' then 
		fn_stub_kincaid
	else if env$('client')='Payroll Done Right' then ! env$('client')='West Accounting' or 
		fn_stub_standard( 1) ! standard, but show tips
	else if env$('client')='Energy Exchanger' then 
		! r: setup mat ltext$ and mat lPos for fn_stub_hitBoxes
		! Page Settings:  Orientation: Landscapt
		!                      Height:  7"
		!                       Width:  8.5"
		!                       Lines Per Page:  54 (default)
		!                       Font Size:  10
		!                      Top Margin: .5
		!                   Bottom Margin: .5
		!                     Left Margin: .2
		!                    Right Margin: .2
		dim ltext$(0,0)*256,lPos(0,0)
		mat ltext$(15,10) : mat lPos(15,10)
		mat ltext$=('')
		! lyne=12 : litem=0
		!   lpos(lyne,litem+=1)= 1   : ltext$(lyne,litem)=rpt$('----+----|',12)
		lyne=5 : litem=0
			lpos(lyne,litem+=1)=  1   : ltext$(lyne,litem)=str$(eno)
			lpos(lyne,litem+=1)= 13   : ltext$(lyne,litem)=date$(days(d1,'ccyymmdd'),'mm/dd/yy')
			lpos(lyne,litem+=1)= 22   : ltext$(lyne,litem)=cnvrt$('N 8.2',rate) ! Hourly Rate
			lpos(lyne,litem+=1)= 32   : ltext$(lyne,litem)=cnvrt$('N 6.2',tdc1) ! Reg Hours
			lpos(lyne,litem+=1)= 46   : ltext$(lyne,litem)=cnvrt$('N 5.2',tdc2) ! OT Hours
			lpos(lyne,litem+=1)= 60   : ltext$(lyne,litem)='' ! daily Rate
			lpos(lyne,litem+=1)= 67   : ltext$(lyne,litem)='' ! No Days
			lpos(lyne,litem+=1)= 80   : ltext$(lyne,litem)='' ! Monthly Rate
			lpos(lyne,litem+=1)= 90  : ltext$(lyne,litem)='' ! Salary Adjustment
			lpos(lyne,litem+=1)=112  : ltext$(lyne,litem)=cnvrt$('pic(## ## ##)',prdmmddyy) ! Check Date MM DD YY
		lyne=11 : litem=0
			lpos(lyne,litem+=1)=  1   : ltext$(lyne,litem)=cnvrt$('N  9.2',ttc(31))! gross salary
			lpos(lyne,litem+=1)= 12   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(1)) ! Fed w/h
			lpos(lyne,litem+=1)= 24   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(4)) ! State w/h
			lpos(lyne,litem+=1)= 36   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(2)) ! FICA
			lpos(lyne,litem+=1)= 50   : ltext$(lyne,litem)=cnvrt$('N  9.2',ttc(5)) ! pre insurance tax
			lpos(lyne,litem+=1)= 60   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(6))
			lpos(lyne,litem+=1)= 72   : ltext$(lyne,litem)=cnvrt$('N 11.2',ttc(7))
			lpos(lyne,litem+=1)= 84   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(8))
			lpos(lyne,litem+=1)= 96   : ltext$(lyne,litem)=cnvrt$('N  8.2',ttc(9))
			lpos(lyne,litem+=1)=106   : ltext$(lyne,litem)=cnvrt$('N 13.2',ttc(32)) ! net pay 
		lyne=14 : litem=0                                   ! YTD of line above
			lpos(lyne,litem+=1)=  1   : ltext$(lyne,litem)=cnvrt$('N  9.2',tty(31)) ! gross ytd
			lpos(lyne,litem+=1)= 12   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(1)) ! fed w/h ytd
			lpos(lyne,litem+=1)= 24   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(4)) ! state w/h ytd
			lpos(lyne,litem+=1)= 36   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(2)) ! FICA YTD
			lpos(lyne,litem+=1)= 50   : ltext$(lyne,litem)=cnvrt$('N  9.2',tty(5))
			lpos(lyne,litem+=1)= 60   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(6))
			lpos(lyne,litem+=1)= 72   : ltext$(lyne,litem)=cnvrt$('N 11.2',tty(7))
			lpos(lyne,litem+=1)= 84   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(8))
			lpos(lyne,litem+=1)= 96   : ltext$(lyne,litem)=cnvrt$('N  8.2',tty(9))
			lpos(lyne,litem+=1)=106   : ltext$(lyne,litem)=''
		! /r
		pr #255: '{\fs16 '; !   <-- set to font size 8 
		fn_stub_hitBoxes(mat ltext$,mat lPos)
		pr #255: "}" ! <-- end the font size of 8
		fn_stub_energyExcnahger_extra(mat v,mat abrevName$,mat deptsum)
	else if env$('client')='Crockett County' then
		stubCount+=1
		if stubCount=1 then
			fn_stub_standard(0)
		else if stubCount=2 then
			stubCount=0
			fn_stub_standard
		end if
	else 
		fn_stub_standard
	end if 
fnend 
F_STUB_01: form pos 3,c 18,2*pic(-------.--),x 4,c 12,pic(-------.--),pic(-----------.--)
F_STUB_02: form pos 3,c 9,pic(------.--),2*pic(-------.--),x 4,c 12,pic(-------.--),pic(-----------.--)
def fn_stub_standard(; stst_show_tips,ststaddlength)
	pr #255: ""
	pr #255,using 'form pos 3,c 30,n 8,x 2,c 13,pic(zz/zz/zz),n 10': em$(1),eno,"",prdmmddyy,check_number
	for j=1 to 20
		if tty(j+4)=0 then abrevName$(j)="" else abrevName$(j)=hnames$(j)
	next j
	pr #255,using 'form pos 60,c 7,pos 77,c 3': "Current","YTD"
	if uprc$(accr$)="N" then 
		pr #255,using 'form pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Hours","Pay","Med W/H",ttc(3),tty(3)
	else 
		pr #255,using 'form pos 14,c 7,pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Accrued","Hours","Pay","Med W/H",ttc(3),tty(3)
	end if 
	pr #255,using F_STUB_01: "Regular",tdc1,ttc(26),"Fed W/H",ttc(1),tty(1)
	pr #255,using F_STUB_01: "Over Time",tdc2,ttc(27),"FICA W/H",ttc(2),tty(2)
	if uprc$(accr$)="N" then 
		pr #255,using F_STUB_01: "Sick",tdc3,tpd3,"State W/H",ttc(4),tty(4)
		pr #255,using F_STUB_01: "Vacation",tdc4,tpd4,abrevName$(1),ttc(5),tty(5)
	else 
		pr #255,using F_STUB_02: "Sick",em10,tdc3,tpd3,"State W/H",ttc(4),tty(4)
		pr #255,using F_STUB_02: "Vacation",em11,tdc4,tpd4,abrevName$(1),ttc(5),tty(5)
	end if 
	pr #255,using F_STUB_01: "Holiday",tdc5,tpd5,abrevName$(2),ttc(6),tty(6)
	pr #255,using F_STUB_01: "Other",0,ttc(28),abrevName$(3),ttc(7),tty(7)
	pr #255,using F_STUB_01: rt$,0,0,abrevName$(4),ttc(8),tty(8)
	if trim$(compcode$)="" then 
		pr #255,using F_STUB_01: "",0,0,abrevName$(5),ttc(9),tty(9)
	else 
		fn_extract_comp_time
		pr #255,using F_STUB_02: "Comp Time",balance,0,0,abrevName$(5),ttc(9),tty(9)
	end if 
	pr #255,using F_STUB_01: "YTD Pay",0,tty(31),abrevName$(6),ttc(10),tty(10)
	pr #255,using F_STUB_01: "YTD Deductions",0,tdedytd,abrevName$(7),ttc(11),tty(11)
	pr #255,using F_STUB_01: "Current Pay",0,ttc(31),abrevName$(8),ttc(12),tty(12)
	pr #255,using F_STUB_01: "Cur Deductions",0,tdedcp,abrevName$(9),ttc(13),tty(13)
	pr #255,using F_STUB_01: "Net Pay",0,ttc(32),"Other",ttc(14)+ttc(15)+ttc(16)+ttc(17)+ttc(18)+ttc(19)+ttc(20)+ttc(21)+ttc(22)+ttc(23)+ttc(24),tty(14)+tty(15)+tty(16)+tty(17)+tty(18)+tty(19)+tty(20)+tty(21)+tty(22)+tty(23)+tty(24)
	pr #255: ""
	if stst_show_tips and ttc(28)<>0 then 
		pr #255,using F_STUB_02: "Tips",ttc(28)
	else 
		pr #255: ""
	end if 
	pr #255: ""
	for j=1 to ststaddlength
		pr #255: ""
	next j
fnend 
def fn_stub_kincaid
	pr #255: ""
	pr #255,using 'form pos 3,c 30,n 8,x 2,c 13,pic(zz/zz/zz),n 10': em$(1),eno,"",prdmmddyy,check_number
	for j=1 to 20
		if tty(j+4)=0 then abrevName$(j)="" else abrevName$(j)=hnames$(j)
	next j
	pr #255,using 'form pos 60,c 7,pos 77,c 3': "Current","YTD"
	if uprc$(accr$)="N" then 
		pr #255,using 'form pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Hours","Pay","Med W/H",ttc(3),tty(3)
	else 
		pr #255,using 'form pos 14,c 7,pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Accrued","Hours","Pay","Med W/H",ttc(3),tty(3)
	end if 
	pr #255,using F_STUB_01: "Regular",tdc1,ttc(26),"Fed W/H",ttc(1),tty(1)
	pr #255,using F_STUB_01: "Over Time",tdc2,ttc(27),"FICA W/H",ttc(2),tty(2)
	if uprc$(accr$)="N" then 
		pr #255,using F_STUB_01: "Sick",tdc3,tpd3,"State W/H",ttc(4),tty(4)
		pr #255,using F_STUB_01: "Vacation",tdc4,tpd4,abrevName$(1),ttc(5),tty(5)
	else 
		pr #255,using F_STUB_02: "Sick",em10,tdc3,tpd3,"State W/H",ttc(4),tty(4)
		pr #255,using F_STUB_02: "Vacation",em11,tdc4,tpd4,abrevName$(1),ttc(5),tty(5)
	end if 
	pr #255,using F_STUB_01: "Holiday",tdc5,tpd5,abrevName$(2),ttc(6),tty(6)
	pr #255,using F_STUB_01: "Other",0,ttc(28),abrevName$(3),ttc(7),tty(7)
	pr #255,using F_STUB_01: rt$,0,0,abrevName$(4),ttc(8),tty(8)
	if trim$(compcode$)="" then 
		pr #255,using F_STUB_01: "",0,0,abrevName$(5),ttc(9),tty(9)
	else 
		fn_extract_comp_time
		pr #255,using F_STUB_02: "Comp Time",balance,0,0,abrevName$(5),ttc(9),tty(9)
	end if 
	pr #255,using F_STUB_01: "YTD Pay",0,tty(31),abrevName$(6),ttc(10),tty(10)
	pr #255,using F_STUB_01: "YTD Deductions",0,tdedytd,abrevName$(7),ttc(11),tty(11)
	pr #255,using F_STUB_01: "Current Pay",0,ttc(31),abrevName$(8),ttc(12),tty(12)
	pr #255,using F_STUB_01: "Cur Deductions",0,tdedcp,abrevName$(9),ttc(13),tty(13)
	pr #255,using F_STUB_01: "Net Pay",0,ttc(32),"Other",ttc(14)+ttc(15)+ttc(16)+ttc(17)+ttc(18)+ttc(19)+ttc(20)+ttc(21)+ttc(22)+ttc(23)+ttc(24),tty(14)+tty(15)+tty(16)+tty(17)+tty(18)+tty(19)+tty(20)+tty(21)+tty(22)+tty(23)+tty(24)
	pr #255: ""
	pr #255: ""
	pr #255: ""
	pr #255: ""
fnend 
def fn_stub_billings
	stub_one_or_two+=1
	if stub_one_or_two=3 then stub_one_or_two=1
	if stub_one_or_two=2 then 
		pr #255: ""
		pr #255: ""
		pr #255: ""
		pr #255: ""
	end if 
	pr #255: ""
	pr #255,using 'form pos 3,c 30,n 8,x 2,c 13,pic(zz/zz/zz),n 10': em$(1),eno,"",prdmmddyy,check_number
	for j=1 to 20
		if tty(j+4)=0 then abrevName$(j)="" else abrevName$(j)=hnames$(j)
	next j
	pr #255,using 'form pos 60,c 7,pos 77,c 3': "Current","YTD"
	if uprc$(accr$)="N" then 
		pr #255,using 'form pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Hours","Pay","Med W/H",ttc(3),tty(3)
	else 
		pr #255,using 'form pos 14,c 7,pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Accrued","Hours","Pay","Med W/H",ttc(3),tty(3)
	end if 
	pr #255,using F_STUB_01: "Regular",tdc1,ttc(26),"Fed W/H",ttc(1),tty(1)
	pr #255,using F_STUB_01: "Over Time",tdc2,ttc(27),"FICA W/H",ttc(2),tty(2)
	if uprc$(accr$)="N" then 
		pr #255,using F_STUB_01: "Sick",tdc3,tpd3,"State W/H",ttc(4),tty(4)
		pr #255,using F_STUB_01: "Vacation",tdc4,tpd4,abrevName$(1),ttc(5),tty(5)
	else 
		pr #255,using F_STUB_02: "Sick",em10,tdc3,tpd3,"State W/H",ttc(4),tty(4)
		pr #255,using F_STUB_02: "Vacation",em11,tdc4,tpd4,abrevName$(1),ttc(5),tty(5)
	end if 
	pr #255,using F_STUB_01: "Holiday",tdc5,tpd5,abrevName$(2),ttc(6),tty(6)
	pr #255,using F_STUB_01: "",0,ttc(28),abrevName$(3),ttc(7),tty(7)
	pr #255,using F_STUB_01: rt$,0,0,abrevName$(4),ttc(8),tty(8)
	if trim$(compcode$)="" then 
		pr #255,using F_STUB_01: "",0,0,abrevName$(5),ttc(9),tty(9)
	else 
		fn_extract_comp_time
		pr #255,using F_STUB_02: "Comp Time",balance,0,0,abrevName$(5),ttc(9),tty(9)
	end if 
	pr #255,using F_STUB_01: "YTD Pay",0,tty(31),abrevName$(6),ttc(10),tty(10)
	pr #255,using F_STUB_01: "YTD Deductions",0,tdedytd,abrevName$(7),ttc(11),tty(11)
	pr #255,using F_STUB_01: "Current Pay",0,ttc(31),abrevName$(8),ttc(12),tty(12)
	pr #255,using F_STUB_01: "Cur Deductions",0,tdedcp,abrevName$(9),ttc(13),tty(13)
	pr #255,using F_STUB_01: "Net Pay",0,ttc(32),"",ttc(14)+ttc(15)+ttc(16)+ttc(17)+ttc(18)+ttc(19)+ttc(20)+ttc(21)+ttc(22)+ttc(23)+ttc(24),tty(14)+tty(15)+tty(16)+tty(17)+tty(18)+tty(19)+tty(20)+tty(21)+tty(22)+tty(23)+tty(24)
	pr #255: ""
	pr #255: ""
	if stub_one_or_two=1 then 
		pr #255: ""
		pr #255: ""
	end if
fnend 
def fn_stub_hitBoxes(mat ltext$,mat lPos)
	! mat ltext$(lineCount,boxNumber)=textForBox$ (must be formatted)
	! mat lPos(luneCount,BoxNumber)=Position of Box
	! udim(mat ltext$,1) defines lineCount/length of stub
	dim hbLine$*256
	for hbLine=1 to udim(mat ltext$,1)
		hbLine$=rpt$(' ',256)
		for lposItem=1 to udim(mat lPos,2)
			hbLine$(lPos(hbLine,lposItem):(lPos(hbLine,lposItem)+len(lText$(hbLine,lposItem))-1))=ltext$(hbLine,lposItem)
		nex lposItem
		pr #255: rtrm$(hbLine$)
	nex hbLine
fnend
def fn_stub_energyExcnahger_extra(mat s,mat rpnames2$,mat dept)
	pr #255: ''
	for x=1 to 6
		! if dept(x)=0 then 
			pr #255,using ees_L1510: rpnames2$(x+4),s(x,7),s(x,8)
			ees_L1510: form pos 54,c 10,pos 64,pic(-------.##),pos 74,pic(-------.##)
		! else
		!   pr #255,using ees_L1480: dept(x),s(x,1),s(x,2),s(x,3),s(x,4),s(x,5),s(x,6),rpnames2$(x+4),s(x,7),s(x,8)
		!   ees_L1480: form pos 2,n 3,pos 6,5*n 7.2,n 11.2,x 2,c 10,2*n 10.2
		! end if
	next x
	! if s(7,6)<>0 then 
	!   pr #255,using ees_L1550: "OTHER",s(7,1),s(7,2),s(7,3),s(7,4),s(7,5),s(7,6),"TOTAL",s(7,7),s(7,8)
	!   ees_L1550: form pos 1,c 5,pos 6,pic(----.##),pos 14,pic(---.##),pos 21,pic(---.##),pos 28,pic(---.##),pos 35,pic(---.##),pos 42,pic(---,---.##),pos 54,c 5,pos 64,pic(-------.##),pos 74,pic(-------.##),skip 1
	! end if
fnend
! /r
def fn_extract_comp_time
	balance=0
	key$=lpad$(str$(eno),8)&"             "
	restore #breakdown,key>=key$: nokey EOBREAKDOWN
	READHOURBREAKDOWN: ! 
	read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",release: empno2,class$,tdate,increase,decrease eof EOBREAKDOWN ! kj 4/18/07
	if empno2<>eno then goto EOBREAKDOWN
	if trim$(class$)<>trim$(compcode$) then goto READHOURBREAKDOWN
	balance+=increase-decrease
	goto READHOURBREAKDOWN
	EOBREAKDOWN: ! 
fnend 
def fn_determine_earnings
	mat caf=(0): mat ttc=(0): mat ttdc=(0)
	mat tcp=(0): mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0)
	mat qtr4tcp=(0): mat ytdTotal=(0): mat tdc=(0): mat tty=(0)
	fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0
	medqtr=eicyr=eicqtr=wagesqtr=0
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! indexed by employee#,department# and payroll date
	restore #3,key>=checkkey$: nokey L6920
	L6580: ! 
	read #3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,oldckno,mat tdc,mat tcp eof STORE_VARIABLES : lastrec=rec(3)
	if heno<>eno then goto STORE_VARIABLES
	if prd<beg_date or prd>end_date then ! not this year
		goto L6580
	end if 
	if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
	if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
	if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
	if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
	mat ytdTotal=ytdTotal+tcp
	mat tty=tty+tcp
	if prd=d1 then mat ttc=ttc+tcp: mat ttdc=ttdc+tdc ! total for this check
	if prd=d1 then let fn_accumulate_dept_totals1(tdepXcount,mat tdep,tdn,rate)
	if env$('client')="Energy Exchanger" then let fn_accumulate_dept_totals2
	if prd=d1 then rewrite #3,using "form pos 18,n 7",rec=lastrec: check_number
	goto L6580
	STORE_VARIABLES: ! 
	!   wagesyr=ytdTotal(31) ! total wages
	fedyr=ytdTotal(1) ! ytdl fed
	ficayr=ytdTotal(2) ! fica year to date
	medyr=ytdTotal(3) ! medicare year to date
	stateyr=ytdTotal(4) ! total state  quarter
	eicyr=ytdTotal(25) ! eic
	if prd>=qtr1 and prd<qtr2     then mat quartertotals=qtr1tcp
	if prd>=qtr2 and prd<qtr3     then mat quartertotals=qtr2tcp
	if prd>=qtr3 and prd<qtr4     then mat quartertotals=qtr3tcp
	if prd>=qtr4 and prd<end_date then mat quartertotals=qtr4tcp
	wagesqtr=quartertotals(31) ! total wages quarter
	fedqtr=quartertotals(1) ! total fed  quarter
	ficaqtr=quartertotals(2) ! total fica quarter
	medqtr=quartertotals(3) ! total medicare quarter
	stateqtr=quartertotals(4) ! total state  quarter
	eicqtr=quartertotals(25) ! EIC qtr
	!   for j=1 to 20
	!     if dedfed(j)=1 then dedfedyr+=ytdTotal(j+4) ! deduct for federal wh
	!   next j
	L6920: ! 
fnend 
def fn_accumulate_dept_totals1(&tdepXcount,mat tdep,tdn,&rate) ! probably others too
	! ACCUMULATE CURRENT INFO FROM EACH DEPARTMENT
	if tdepXcount<>0 then 
		for j2=1 to tdepXcount
			if tdep(j2,5)=tdn then goto adt_L1790
		next j2
	end if
	tdepXcount=tdepXcount+1
	j2=tdepXcount
	adt_L1790: ! 
	tdep(j2,1)=tdep(j2,1)+tcp(31)-tcp(30) ! total wage less tips
	deptgl$=""
	read #hDepartment,using "Form pos 12,c 12,pos 62,2*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",tdn): deptgl$,tdet(2),tdet(3) ! Nokey 1660
	tdep(j2,2)=val(deptgl$(1:3)) ! salary for this department
	tdep(j2,3)=val(deptgl$(4:9))
	tdep(j2,4)=val(deptgl$(10:12))
	tdep(j2,5)=tdn
	fn_fica_matching
	tdep(j2,6)=ficam2+medic2 ! fica+match
	for j3=1 to 20
		tdep(j2,j3+6)=tdep(j2,j3+6)+tcp(j3+4)
	next j3
	if s1=1 then 
		if rate=0 then rate=tdet(2)
		if rate>0 then rt$="PAY RATE"&cnvrt$("N 10.2",rate) else rt$=""
	end if
	tpd3=tpd3+round(tdc(3)*tdet(2),2) ! sick pay
	tpd4=tpd4+round(tdc(4)*tdet(2),2) ! vacation pay
	tpd5=tpd5+round(tdc(5)*tdet(2),2) ! if env$('client')="West Rest Haven" then tpd5=tpd5+round(tdc(5)*(tdet(2)*1.5),2) else tpd5=tpd5+round(tdc(5)*tdet(2),2)
	tdc1=ttdc(1) ! Regular Hours
	tdc2=ttdc(2) ! OverTime Hours
	tdc3=ttdc(3)
	tdc4=ttdc(4)
	tdc5=ttdc(5)
	!   ttdct=ttdc(1)+ttdc(2)+ttdc(3)+ttdc(4)+ttdc(5) ! Total Hours
fnend 
def fn_accumulate_dept_totals2
	for v1=1 to 6
		if tdn=deptsum(v1) then goto L8090 ! determine if dept # used on this employee already
		if deptsum(v1)=0 then deptsum(v1)=tdn: goto L8080
	next v1
	if v1>6 then v1=6 ! summarize any departments over 6 and the seventh row
	L8080: ! 
	if prd<>d1 then goto L8150
	L8090: ! 
	for r=1 to 5
		v(v1,r)+=tdc(r)
		v(7,r)+=tdc(r) ! total line
	next r
	v(v1,6)+=tcp(31) ! pay
	v(7,6)+=tcp(31) ! total pay line
	L8150: ! 
	for r=1 to 6
		if prd=d1 then 
			v(r,7)+=tcp(r+8) ! last five misc deductions
		end if
		! FILL TABLE S WITH YEAR TO DATE DEDUCTIONS
		v(r,8)=v(r,8)+tcp(r+8)
	next r
	! FILL TABLE S WITH CURRENT DEDUCTIONS
	for j=9 to 14
		if dedcode(j-3)=1 then goto L8270
		v(7,8)=v(7,8)-tcp(j)
		if prd=d1 then v(7,7)=+v(7,7)-tcp(j)
		goto L8290
		L8270: ! 
		v(7,8)=v(7,8)+tcp(j)
		if prd=d1 then v(7,7)=v(7,7)+tcp(j)
		L8290: !
	next j
	! ROUTINE TO ACCUMULATE HOURS ETC. FOR SUMMARY
fnend 
def fn_fica_fix ! fix rounding problem on fica
	fica3=fica0+medi0+fica1+medi1+fica2
	if fica3=0 then goto FICA_END
	read #h_cl_trans_alloc,using F_ClTransAlloc,rec=fica_rec: bankcode,a1,tr1,gl$,alloc
	alloc=alloc-fica3
	rewrite #h_cl_trans_alloc,using F_ClTransAlloc,rec=fica_rec: bankcode,a1,tr1,gl$,alloc
	FICA_END: fica3=fica0=medi0=fica1=medi1=fica2=0
fnend 
def fn_fica_matching ! CALCULATE MATCHING FICA
	ficawg=round(tcp(2)/ssr1,2) ! employee's fica rate
	ficam2=round(ficawg*ssr2,2) ! employers fica rate
	mediwg=tcp(3)/.0145 ! employee medicare rate
	medic2=round(mediwg*.0145,2) ! employers medicare rate
	if fmeno=eno then goto SENO
	fmeno=eno
	ficam3=ficam2
	medic3=medic2
	goto MFEND
	SENO: ! same employee
	ficam3=ficam3+ficam2
	medic3=medic3+medic2
	MFEND: ! 
fnend 

INVALIDGLNUMBER: ! r:
	fnTos(sn$="Prckprt3")
	mylen=30 : mypos=mylen+2
	fnLbl(1,1,"Employee Number:",mylen,1)
	fnTxt(1,mypos,10, 0,0,'',1)
	resp$(1)=str$(eno)
! 
	fnLbl(2,1,"Department Number:",mylen,1)
	fnTxt(2,mypos,10, 0,0,'',1)
	resp$(2)=str$(tdn)
! 
	fnLbl(4,1,"Invalid General Ledger Number:",mylen,1)
	fnTxt(4,mypos,12, 0,0,'',1)
	resp$(3)=gl$
! 
	fnLbl(5,1,"Purpose for GL Number:",mylen,1)
	fnTxt(5,mypos,40, 0,0,'',1)
	resp$(4)=sd5$
! 
	fnLbl(7,1,"The General Ledger Number is invalid.",40,0)
	fnLbl(8,1,"Please select the correct one.",40,0)
	fnLbl(3,1,"Correct General Ledger Number:",mylen,1)
	fnqgl(3,mypos,0,2,pas)
	resp$(5)=fnrgl$(goodgl$)
	fnCmdKey("&Next",1,1,0,"Continue with checkprinting." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! bad general ledger numbers
	if ckey=5 then goto XIT
	gl$=fnagl$(resp$(5))
	read #h_cl_glmstr,using F_CL_GLMSTR,key=gl$,release: de$ nokey INVALIDGLNUMBER
	on cd1 goto BCR_GLN_VALIDATED,EXLNKD_L5920 none BCR_GLN_VALIDATED
! /r

def fn_getTestValues
	eno=999
	eno=99999999
	em$(1)='|Name ------VOID-VOID-VOID---|'
	em$(2)='|Address --------------------|'
	em$(3)='|City, State and Zip---------|'
	em10=        12                                  ! sick hours accrued
	em11=        14                                  ! vacation hours accrued
	lpd=   date(days(d1,'ccyymmdd'),'mmddyy')       ! last payroll date
	rate=        10                                  ! Hourly Rate
	rt$=         "PAY RATE"&cnvrt$("N 10.2",rate)   ! text used in some stub routines
	tpd3=         9                                  ! sick pay
	tpd4=         8                                  ! vacation pay
	tpd5=         7                                  
	tdc1=        40                                  ! Regular Hours
	tdc2=        10                                  ! OverTime Hours
	tdc3=         6                                  
	tdc4=         5                                  
	tdc5=         4                                  
	ttc(31)=  34500                                  ! gross salary
	ttc(1) =    250                                  ! Fed w/h
	ttc(4) =     75                                  ! State w/h
	ttc(2) =     15                                  ! FICA
	ttc(5) =      3                                  ! pre insurance tax
	ttc(6) =      2                                  
	ttc(7) =      1                                  
	ttc(8) =      0.5                                
	ttc(9) =      0.4                                
	ttc(32)= 13895.27                                  ! net pay 
	 ! YTD of line above
	tty(31)=  30000                                  ! gross ytd
	tty(1) =    100                                  ! fed w/h ytd
	tty(4) =     50                                  ! state w/h ytd
	tty(2) =     25                                  ! FICA YTD
	tty(5) =      5
	tty(6) =      4
	tty(7) =      3
	tty(8) =      2
	tty(9) =      1
fnend
include: ertn
