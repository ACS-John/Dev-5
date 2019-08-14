! formerly S:\acsPR\newprReg1
! Payroll Register

! r: general setup: libraries, dims, fntop,fncno,read defaults, open files, etc
	library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn
	library 'S:\Core\Library': fnPayPeriodEndingDate,fnprocess,fndate_mmddyy_to_ccyymmdd,fndat,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnpayroll_register_2,fncreg_read,fncreg_write,fnChk,fnreg_read,fnreg_write,fncreg_read,fnDedNames
	on error goto ERTN

	dim em$*30,cp(32),tcp(32)
	dim tdc(10),ttdc(10)
	dim thc(5)

	fntop(program$)
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
	open #12: "Name=[Q]\CLmstr\BankMstr.H[cno],KFName=[Q]\CLmstr\BankIdx1.H[cno],Shr",internal,input,keyed ioerr L240
	read #12,using 'Form POS 57,G 8',key=lpad$(str$(bankcode),2),release: cl_bank_last_check$ nokey ignore
	close #12: ioerr ignore
	L240: !
	open #1: "Name=[Q]\PRmstr\prCode.h[cno],Shr",internal,input ioerr L270
	read #1,using 'Form POS 5,N 8': ckno
	close #1:
	L270: !
	ckno+=1
	d1$=cnvrt$("pic(zzzzzzzz)",fnPayPeriodEndingDate)
	ppd=val(d1$(5:6))*10000+val(d1$(7:8))*100+val(d1$(3:4))
	L320: !
	if trim$(cl_bank_last_check$)<>"" then ckno=val(cl_bank_last_check$)+1 conv ignore


	open #1: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,input,keyed
	open #h_dd=30: "Name=[Q]\PRmstr\DD.h[cno],KFName=[Q]\PRmstr\DDidx1.h[cno],Shr",internal,input,keyed
	open #h_checks:=4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]"&',Shr',internal,input,keyed
! /r
	if fnprocess=1 then goto START_REPORT else goto ASK_CHECK_NO
ASK_CHECK_NO: ! r:
	fnTos(sn$="Payrollreg1")
	respc=0
	fnLbl(1,1,"Beginning Check Number:",20,1)
	fnTxt(1,23,8,0,1,"30",0," ")
	resp$(respc+=1)=str$(ckno)
	fnLbl(1,1,"",34,1) ! bigger screen
	fnLbl(2,1,"Payroll Date:",20,1)
	fnTxt(2,23,10,0,1,"1",0,"For current payroll, always use the calculation date.  You can reprint older payroll registers by using that date.")
	resp$(respc+=1)=str$(ppd)
	fnChk(4,2,'Combine both registers into one multi-page report',50)
	resp$(resp_append_reg1:=respc+=1)=append_reg1$
	if env$('ACSDeveloper')<>'' then ! option under development for West Accounting... held until they decide if they actually want/nedd this - currently causes mismatch (in their cno 18) in other_wh in 1st and 2nd PR Registers
		fnChk(6,2,'Include Tips in Other Withholdings',50)
		resp$(resp_include_tips_in_other_wh:=respc+=1)=include_tips_in_other_wh$
	else
		resp$(resp_include_tips_in_other_wh:=respc+=1)='False'
	end if
	fnCmdKey("&Next",1,1,0,"Proceed with pr the payroll register." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! ask employee #
	if ckey=5 then goto XIT
	ckno=val(resp$(1))
	ppd=val(resp$(2))
	append_reg1$=resp$(resp_append_reg1)
	include_tips_in_other_wh$=resp$(resp_include_tips_in_other_wh)
	fncreg_write('prreg2.include_tips_in_other_wh',include_tips_in_other_wh$) : if include_tips_in_other_wh$='True' then include_tips_in_other_wh=1 else include_tips_in_other_wh=0
	fnreg_write('prreg2.append_reg1',append_reg1$) : if append_reg1$='True' then append_reg1=1 else append_reg1=0
	goto START_REPORT ! /r
START_REPORT: ! r:
	if append_reg1 then
		fnopenprn( 0,0,0,fnprocess,' (Check and Departmental Registers)')
	else
		fnopenprn( 0,0,0,fnprocess,' (Check Register)')
	end if
	gosub HDR
	goto LOOP_TOP

LOOP_TOP: !
	read #1,using 'form pos 1,n 8,c 30,pos 162,n 6,pos 173,2*pd 3': eno,em$,lpd eof FINIS
! if eno=307 then pr 'eno '&str$(eno) : exe 'break other_wh' : break_is_on=1 else if break_is_on then exe 'break other_wh off' : break_is_on=0
	if lpd><ppd then goto LOOP_TOP
	mat thc=(0) : mat tcp=(0) : mat ttdc=(0) : holdrealckno=realckno=0
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	dirdep$=rpad$(str$(eno),10)
	dd$=""
	read #h_dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17",key=dirdep$: key$,dd$,rtn,acc,acn nokey ignore
! if env$('client')="West Rest Haven" then goto L690
	a=pos(rtrm$(em$)," ",1) : b=pos(rtrm$(em$)," ",a+1)
	em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a) error ignore
! L690: !
	restore #h_checks,key>=checkkey$: nokey LOOP_TOP
L700: !
	read #h_checks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp eof L760
	if heno<>eno then goto L760
	if prd><fndate_mmddyy_to_ccyymmdd(ppd) then goto L700
	if prd=fndate_mmddyy_to_ccyymmdd(ppd) then holdrealckno=realckno
	mat tcp=tcp+cp : mat ttdc=ttdc+tdc
	goto L700
L760: !
! oi=tcp(28)+tcp(29)+tcp(30)+tcp(27)
	other_wh=-tcp(25)
	! for j=5 to 24
	!   if newdedcode(j-4)=newdedcode_Benefit then
	!     ! do nothing
	!   else if newdedcode(j-4)=newdedcode_Add then
	!     other_wh=other_wh-tcp(j) ! if break_is_on and tcp(j)<>0 then pr 'tcp('&str$(j)&') deducts '&str$(tcp(j))
	!   else if newdedcode(j-4)=newdedcode_Deduct then
	!     other_wh=other_wh+tcp(j) ! if break_is_on and tcp(j)<>0 then pr 'tcp('&str$(j)&')    adds '&str$(tcp(j))
	!   end if
	! next j
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
	if uprc$(dd$)="Y" then goto L915
	pr #255,using L900: ckn2,eno,em$(1:11),mat thc,tothrs,tcp(31),tcp(3),tcp(2),tcp(1),tcp(4),other_wh,tcp(32) pageoflow PGOF
	L900: form pos 1,n 5,n 8,x 1,c 12,6*n 7.2,7*n 9.2,skip 1
	goto L940
L915: if tcp(22)=0 then tcp(22)=tcp(32)
	pr #255,using L930: "DD",eno,em$(1:11),mat thc,tothrs,tcp(31),tcp(3),tcp(2),tcp(1),tcp(4),other_wh,tcp(22) pageoflow PGOF
L930: form pos 1,c 5,n 8,x 1,c 12,6*n 7.2,7*n 9.2,skip 1
L940: !
	total_hours+=sum(mat thc)
	total_net_pay+=tcp(32)
	total_gross_pay+=tcp(31)
	other_wh=0
	goto LOOP_TOP ! /r

TOTALS: ! r:
	pr #255: ''
	pr #255: '    Total Hours: '&lpad$(str$(total_hours),26)
	pr #255: 'Total Gross Pay: '&cnvrt$('pic(-$$,$$$,$$$,$$$,$$$,$$#.##)',total_gross_pay)
	pr #255: '  Total Net Pay: '&rpt$(' ',88)&cnvrt$('pic(-$$,$$$,$$$,$$$,$$$,$$#.##)',total_net_pay)
	return  ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HDR
	continue  ! /r
HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b Payroll Check Register}" ! pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zz/zz/zz)",ppd)&"}"
	pr #255: "\ql   "
	pr #255: tab(29);"<----------------Hours----------------->";
	pr #255: tab(71);"<-Pay->";
	pr #255: tab(79);"<-----------------Deductions---------------->";
	pr #255: tab(129);"Net"
	pr #255: "CK #   Emp #  Name";
	pr #255: tab(29);"  Reg    O/T   Sick    Vac    Hol  Total";
	pr #255: tab(71);"  Total   Med WH     FICA  Federal    State    Other      Pay"
	return  ! /r
FINIS: ! r:
	gosub TOTALS
	close #1: ioerr ignore
	close #2: ioerr ignore
	close #h_checks: ioerr ignore
	close #h_dd: ioerr ignore
	if append_reg1 then
		if env$('ACSDeveloper')<>'' then
			pr #255: '----newpage (ACSDeveloper Style)----'
		else
			pr #255: newpage
		end if
	else
		fncloseprn
	end if
	fnpayroll_register_2(0,include_tips_in_other_wh,append_reg1)
	goto XIT ! /r let fnchain("S:\acsPR\newprReg2")
XIT: fnxit
IGNORE: continue
include: ertn