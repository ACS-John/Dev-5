! formerly S:\acsPR\newDD
! Create Direct Deposit File
fn_setup

dim ml$(0)*256
dim tcp(32)
! tcp(32) = Net Pay
dim cp(32)
dim tdc(10)
dim path$*256 ! Path to Save File to
! lastPayrollDate  = Last Payroll Date (from first screen of employee record,  not departmental record)
! tdt4  = Last Payroll Date (from Departmental record)
dim resp$(10)*256
dim em$(3)*30 ! (1)=Emp Name, (2)=Emp Addr, (3)=Emp CSZ

fn_readSavedResponses
fnTop(program$)
open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
dim dd$(0)*32, ddN(0)
dd=fn_openFio('PR Direct Deposit',mat dd$,mat ddN)
open #hChecks=fnH: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
goto Screen1

Screen1: ! r:
	fnTos
	respc=0    ! response counter
	mypos=55   ! column2 2 position
	fnLbl(1,90,"",1,1) ! bigger screen
	fnLbl(2,1,"Payroll Date:",mypos,1)
	fnTxt(2,mypos+3,10,0,1,"3",0,"For current payroll, always use the calculation date.  You can transfer older payrolls by using a previous payroll date.")
	resp$(resp_payrollDate:=respc+=1)=str$(payrollDate)
	fnLbl(3,1,"Path to Save File to:",mypos,1)
	fnTxt(3,mypos+3,30,128,0,"72")
	resp$(resp_path:=respc+=1)=path$
	fnLbl(5,1,"Your Bank Account Number:",mypos,1)
	fnTxt(5,mypos+3,8,0,0,"",0,"The right hand set of numbers at the bottom of your checks.")
	resp$(resp_bankAccount:=respc+=1)=bankaccount$(1:8)
	fnLbl(6,1,"Routing Number of your Bank:",mypos,1)
	fnTxt(6,mypos+3,10,0,0,"",0,"The middle set of numbers at the bottom of your checks.")
	resp$(resp_bankRouting:=respc+=1)=bankrouting$(1:10)
	! fnLbl(7,1,"Routing Number of Federal Reserve Used by Your Bank:",mypos,1)
	! fnTxt(7,mypos+3,10,0,0,"",0,"You will have to call your bank for this.  Some times it is build into their software and is not needed.")
	! resp$(respc+=1)=federalrouting$
	fnLbl(8,1,"Your Bank Name:",mypos,1)
	fnTxt(8,mypos+3,23,0,0,"",0,"")
	resp$(resp_bankName:=respc+=1)=bankname$
	fnLbl(9,1,"Federal ID Number:",mypos,1)
	fnTxt(9,mypos+3,9,0,0,"",0,"The Federal ID number can be found on any payroll report.")
	resp$(resp_fedId:=respc+=1)=fedid$(1:9)
	fnCmdKey("&Next",1,1,0,"Create the file." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$) ! ask employee number
	if ckey=5 then
		goto Xit
	else
		payrollDate  =val(resp$(resp_payrollDate))
		path$        =resp$(resp_path)
		bankaccount$=resp$(resp_bankAccount) ! bank account number
		bankrouting$=lpad$(resp$(resp_bankRouting),10) ! your bank routing number
		bnkrtn       =val(resp$(resp_bankRouting)) conv Screen1 ! your bank routing number in numeric
		! federalrouting$=resp$(5)
		bankname$=resp$(resp_bankName)
		fedid$=resp$(resp_fedId)
		fncreg_write('Direct Deposit Save File Path',path$) ! The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'
		fncreg_write('Direct Deposit Source Bank Account',bankaccount$) ! The right hand set of numbers at the bottom of your checks.
		fncreg_write('Direct Deposit Source Bank Routing',bankrouting$) ! The middle set of numbers at the bottom of your checks.
		! fncreg_write('Direct Deposit Federal Reserve Routing',federalrouting$) ! Routing Number of Federal Reserve Used by Your Bank
		fncreg_write('Direct Deposit Source Bank Name',bankname$)
		fncreg_write('Direct Deposit Federal ID Number',fedid$) ! The Federal ID number can be found on any payroll report.
		goto MainLoop
	end if
! /r
! (logic just falls through here)
MainLoop: ! r: main loop
	open #hDdout=fnH: "Name=DDout[wsid].txt,RecL=96,EOL=CRLF,Replace",external,output
	fnopenprn
	gosub ReportHdr ! pr header
	gosub FileHeaderRecord
	gosub BatchHeaderRecord
	do
		READ_DD: !
		! read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17": key$,dd$,rtn,acc,acn eof Finis
		read #dd,using form$(dd): mat dd$,mat ddN eof Finis
		! key$	=dd$(dd_eno)
		! dd$		=dd$(dd_enable)
		! rtn	=ddN(dd_routing)
		! acc	=ddN(dd_accType)
		! acn$	=dd$(dd_account)

		mat tcp=(0): mat ttdc=(0)
		if ddN(dd_routing)=0 and ddN(dd_accType)=0 then goto READ_DD
		if uprc$(dd$(dd_enable))='Y' then  ! Y means Yes Direct Deposit is active for this person
			dd$(dd_eno)=lpad$(rtrm$(ltrm$(dd$(dd_eno))),8)
			read #hEmployee,using 'Form pos 9,3*C 30,Pos 162,N 6,Pos 173',key=dd$(dd_eno): mat em$,lastPayrollDate nokey READ_DD
			if fndate_mmddyy_to_ccyymmdd(lastPayrollDate)<payrollDate then goto READ_DD    ! first screen emp data had a last PR Date lower than the Payroll Date specified in this program.
			checkkey$=dd$(dd_eno)&"         "
			restore #hChecks,key>=checkkey$: nokey READ_DD
			ReadCheck: !
			read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L970
			if heno=val(dd$(dd_eno)) then
				if prd><payrollDate then goto ReadCheck
				mat tcp=tcp+cp
				mat ttdc=ttdc+tdc
				goto ReadCheck
			end if
			L970: !
			if sum(tcp)=0 then goto READ_DD ! no pay on this person for this payroll date
			gosub EntryDetailRecord
		end if
	loop
	Finis: !
	close #hEmployee: ioerr ignore
	close #dd: ioerr ignore
	gosub BatchControlRecord ! (8)
	gosub FileControlRecord ! (9)
	close #hDdout:
	if trim$(path$)<>'' then
		if exists(path$) then fnFree(path$)
		if fnCopy("DDout[wsid].txt",path$)<=0 then
			mat ml$(2)
			ml$(1)='Unable to create file at location:'
			ml$(2)=path$
			fnmsgbox(mat ml$)
			goto Screen1
		end if
	end if
	fnfree("DDout[wsid].txt")
	pr #255,using L1770: "Total",totalCredit/100
	L1770: form pos 22,c 15,n 12.2
	! if env$('client')="West Rest Haven" then pr #255,using L1770: "Total In House",totalin
	fncloseprn
goto Xit ! /r
Xit: fnXit
FileHeaderRecord: ! r: (1) File Header Record
	pcde=01 ! Priority Code
	fcd$=date$("YYMMDD") ! File Creation Date
	fct$=time$(1:2)&time$(4:5) ! File Creation Time
	fidm$="A" ! File ID Modifier
	rsz$="094" ! Record Size
	bf$="10" ! Blocking Factor
	fc$="1" ! Format Code
	dim idn$*23 ! (23) Immediate Destination Name
	idn$="Federal Reserve Bank   " ! (23) Immediate Destination Name
	rc$="" ! Reference Code
	! before 5/5/19  write #hDdout,using F_ddout_1: 1,pcde,federalrouting$,bankrouting$,fcd$,fct$,fidm$,rsz$,bf$,fc$,idn$,bankname$,rc$,"0",crlf$
	write #hDdout,using F_ddout_1: 1,pcde,bankrouting$,bankrouting$,fcd$,fct$,fidm$,rsz$,bf$,fc$,idn$,bankname$,rc$,"0",crlf$
	F_ddout_1: Form POS 1,G 1,PIC(##),C 10,C 10,G 6,G 4,C 1,C 3,C 2,C 1,C 23,C 23,C 7,C 1,c 2
return ! /r
BatchHeaderRecord: ! r: (5) Company/Batch Header Record
	scc=220 ! Service Class Code
	cdd$="" ! Company Discretionary Data
	ecc$="PPD" ! Standard Entry Class Code
	ced$="Payroll  " ! Company Entry Descriptive
	eed$=date$("YYMMDD") ! Effective Entry Date
	if uprc$(trim$(bankname$))="RESOURCE BANK" then let eed$=date$(days(date)+2,"YYMMDD")
	osc$="1" ! Originator Status Code
	bn=1 !  BN=Batch Number
	write #hDdout,using F_ddout_5: 5,scc,env$('cnam')(1:16),cdd$="Payroll",'9'&fedid$,ecc$,ced$,fncd(d2),eed$,"",osc$,bankaccount$,bn,crlf$
	F_ddout_5: Form POS 1,G 1,PIC(###),C 16,C 20,C 10,C 3,C 10,PIC(######),G 6,G 3,G 1,C 8,PIC(#######),c 2
return ! /r

EntryDetailRecord: ! r: (6) entry detail
	t1=t1+tcp(32)
	if ddN(dd_accType)=27 then
		tc=22
	else if ddN(dd_accType)=37 then
		tc=32 ! BC ! Transaction Type used to be was TC=23
	end if
	ari=0 ! Addenda Record Indicator
	tn1+=1
	tn$=cnvrt$("PIC(#######)",tn1) ! Trace Number
	! dr$="081505731"
	! da$="10004147         "
	if testfile=1 then tcp(32)=0
	! pr 'tcp(32)=';tcp(32) : pause
	write #hDdout,using F_ddout_6a: 6,tc,int(ddN(dd_routing)/10),str$(ddN(dd_routing))(len(str$(ddN(dd_routing))):len(str$(ddN(dd_routing)))),dd$(dd_account),tcp(32)*100,z$,em$(1)(1:22),"",ari,lpad$(trim$(bankaccount$),8),tn$,crlf$         ! changed dr$ to str(ddN(dd_routing)) ; also da$ to dd$(dd_account)  ! entry to place money in employees account
	F_ddout_6a: Form POS 1,G 1,G 2,pic(########),C 1,C 17,PIC(##########),C 15,C 22,G 2,N 1,C 8,c 7,c 2
	pr #255: z$&" "&em$(1)&" "&str$(tcp(32)) pageoflow ReportPgof
	totalDebit+=(tcp(32)*100)
	totalCredit+=(tcp(32)*100) ! added this for the batch totals - ??
	! if env$('client')="West Rest Haven" and ddN(dd_routing)=1190515 then totalin=totalin+tcp(32)
	eh=eh+int(ddN(dd_routing)/10) ! Entry Hash should accumulate Routing numbers    dropping the last digit of the routing number
return ! /r

BatchControlRecord: ! r: (8) Company/Batch Control Record
	scc=220 ! Service Class Code
	eac=tn1 ! Entry Addenda Count
	! eH=0 ! Entry Hash
	eh$=str$(eh): x=len(eh$): eh=val(eh$(max(1,x-9):x))
	! totalDebit=Total Debit Amount
	! totalCredit=Total Credit Amount
	! fedid$=Company Identification
	if eh=0 then
		mat ml$(3)
		ml$(1)="It appears you do not have anyone with"
		ml$(2)="direct deposit this pay period."
		ml$(3)="Click OK to continue."
		fnmsgbox(mat ml$,resp$)
		goto Xit
	end if

	! write #hDdout,using F_ddout_8: 8,scc,eac,eh,totalDebit,totalCredit,fedid$,mac$,"",bankaccount$,bn,crlf$ ! removed *100 from totalDebit and from totalCredit
	! F_ddout_8: Form POS 1,G 1,PIC(###),PIC(######),PIC(##########),2*PIC(############),C 10,C 19,C 6,C 8,PIC(#######),c 2
	! File Control Record
	bactr=1 ! Batch Count
	tn2=tn1+4         ! total number Records (all 6 Records plus the 1&5 plus 8&9)
	if fp(tn2/10)>0 then
		blctr=int(tn2/10)+1: bkfactor=blctr*10-tn2  ! block counter and block factor
	end if
	if fp(tn2/10)=0 then blctr=int(tn2/10): bkfactor=0
	eac=tn1 ! entry/adgenda count (number of 6 Records)
	! don't change my entry hash any more ! eH=0008150573 ! EH=Entry Hash
	if eh=0 then
		mat ml$(3)
		ml$(1)="It appears you do not have anyone with"
		ml$(2)="direct deposit this pay period."
		ml$(3)="Click OK to continue."
		fnmsgbox(mat ml$,resp$)
		goto Xit
	end if
	! write #hDdout,using F_ddout_6b: 6,27,int(bnkrtn/10),str$(bnkrtn)(len(str$(bnkrtn)):len(str$(bnkrtn))),bankaccount$,totalDebit,"","","",ari,lpad$(trim$(bankaccount$),8),tn$,crlf$        ! changed dr$ to str(ddN(dd_routing)) ; also da$ to dd$(dd_account)  ! total entry for  debiting customer account
	! F_ddout_6b: Form POS 1,G 1,G 2,pic(########),C 1,C 17,PIC(##########),C 15,C 22,G 2,N 1,C 8,c 7,c 2
	if uprc$(trim$(bankname$))="RESOURCE BANK" then
		write #hDdout,using F_ddout_8: 8,scc,eac,eh,0,totalCredit,'1'&fedid$,mac$,"",bankaccount$,bn,crlf$ ! no total debit for Resource Bank
	else
		write #hDdout,using F_ddout_8: 8,scc,eac,eh,totalDebit,totalCredit,'1'&fedid$,mac$,"",bankaccount$,bn,crlf$ ! removed *100 from totalDebit and from totalCredit
	end if 
	F_ddout_8: Form POS 1,G 1,PIC(###),PIC(######),PIC(##########),2*PIC(############),C 10,C 19,C 6,C 8,PIC(#######),c 2
	! 5/5/19 moved the write out for 8 record to after 6 record write out
return ! /r
FileControlRecord: ! r: (9) requires bkfactor,bactr,blctr,eac,eh,totalDebit,totalCredit,crlf$
	if uprc$(trim$(bankname$))="RESOURCE BANK" then
		write #hDdout,using F_ddout_9a: 9,bactr,blctr,eac,eh,0,totalCredit,rpt$(" ",38)," ",crlf$
	else
		write #hDdout,using F_ddout_9a: 9,bactr,blctr,eac,eh,totalDebit,totalCredit,rpt$(" ",38)," ",crlf$    ! removed *100 from totalDebit and totalCredit
	end if
	F_ddout_9a: Form POS 1,G 1,2*PIC(######),PIC(########),PIC(##########),2*PIC(############),C 38,C 1,c 2
	if bkfactor<>0 then
		for j=1 to bkfactor
			! pr "l22="&STR$(L22+=1)
			write #hDdout,using F_ddout_9b: rpt$("9",94)&crlf$
			F_ddout_9b: Form POS 1,C 96
		next j
	end if
return ! /r

ReportPgof: ! r:
	pr #255: newpage
	gosub ReportHdr
continue ! /r
ReportHdr: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zzzz/zz/zz)",payrollDate)&"}"
	pr #255: "\ql   "
return ! /r



def fn_readSavedResponses ! basicaly all variables are local
	fncreg_read('Direct Deposit Save File Path',path$, fnSpecialFolderPath$('desktop')&'\DirDep.txt',1) ! The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'
	dim bankaccount$*8
	dim bankaccountDefault$*8
	if env$('client')="Billings" then
		bankaccountDefault$=" 0040118"
	!else if env$('client')="Washington Parrish" then
	!	bankaccountDefault$="20428027"
	!else if env$('client')="West Rest Haven" then
	!	bankaccountDefault$=" 1055003"
	else
		bankaccountDefault$='' ! Origination DFI Identification  (your bank account number)
	end if
	fncreg_read('Direct Deposit Source Bank Account',bankaccount$, bankaccountDefault$) ! The right hand set of numbers at the bottom of your checks.

	dim bankrouting$*10 ! Immediage Origin Routing Number
	dim bankrouting$*10
	dim bankrouting$*10
	!  then imo$=
	if env$('client')="Billings" then
		bankRoutingDefault$=" 000017738"
	!else if env$('client')="Washington Parrish"
	!	bankRoutingDefault$=" 065201611"
	!else if env$('client')="West Rest Haven"
	!	bankRoutingDefault$=" 111905159"
	else
		bankRoutingDefault$='' ! Immediate Origin (contains the routing number for your bank)
	end if
	fncreg_read('Direct Deposit Source Bank Routing',bankrouting$, bankRoutingDefault$) ! The middle set of numbers at the bottom of your checks.

	! if env$('client')="Billings" then
	! 	federalroutingDefault$=" 081505964"
	! !else if env$('client')="Washington Parrish" then
	! !	federalroutingDefault$=" 061000146"
	! !else if env$('client')="West Rest Haven" then
	! !	federalroutingDefault$=" 111000038"
	! else
	! 	federalroutingDefault$='' ! Immediate Destination   (routing number for federal reserve bank they use)
	! end if
	! dim federalrouting$*20
	! fncreg_read('Direct Deposit Federal Reserve Routing',federalrouting$, federalroutingDefault$) ! Routing Number of Federal Reserve Used by Your Bank

	dim bankname$*23
	dim bankNameDefault$*23 ! (23) Immediate Origin Name
	if env$('client')='Billings' then
		bankNameDefault$='Bank of Billings'
	!else  if env$('client')="Washington Parrish" then
	!	bankNameDefault$="Parrish National"
	!else if env$('client')="West Rest Haven" then
	!	bankNameDefault$="State National Bank"
	else
		bankNameDefault$='' ! (23) Immediate Origin Name  (your bank name)
	end if
	fncreg_read('Direct Deposit Source Bank Name',bankname$, bankNameDefault$)

	dim fedid$*12
	dim fedidDefault$*12
	if env$('client')='Billings' then
		fedidDefault$='430903099'
	!else if if env$('client')="West Rest Haven" then
	!	fedidDefault$="741551549"
	!else if env$('client')="Washington Parrish" then
	!	fedidDefault$="726001461"
	else
		fedidDefault$=''  ! Company Identification
	end if
	fncreg_read('Direct Deposit Federal ID Number',fedid$, fedidDefault$) ! The Federal ID number can be found on any payroll report.

	payrollDate=fnPayPeriodEndingDate

fnend
include: fn_setup
include: fn_open
