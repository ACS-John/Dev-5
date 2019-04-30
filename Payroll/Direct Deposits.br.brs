! formerly S:\acsPR\newDD
! Direct Deposit file creator

	library 'S:\Core\Library': fntop,fnxit
	library 'S:\Core\Library': fngethandle
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd
	library 'S:\Core\Library': fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnChk,fnCmdSet
	library 'S:\Core\Library': fnmsgbox
	library 'S:\Core\Library': fnGetPayrollDates
	library 'S:\Core\Library': fnCopy
	library 'S:\Core\Library': fnfree
	library 'S:\Core\Library': fnCd
	library 'S:\Core\Library': fncreg_read,fncreg_write
	library 'S:\Core\Library': fnSpecialFolderPath$
	on error goto ERTN

	dim ml$(3)*80,tcp(32),cp(32),tdc(10)
	dim path$*256 ! Path to Save File to
	dim email$*30 ! OR E-Mail Address to send file to
	dim ta(2) ! first and last trailer address
	! em17  = Last Payroll Date (from first screen of employee record,  not departmental record)
	! tdt4  = Last Payroll Date (from Departmental record)
	! tcp(32) = Net Pay
	dim idn$*23,resp$(10)*256,bankname$*23 ! (23) Immediate Destination Name
	dim imo$*10 ! Immediage Origin Routing Number
	dim ion$*23 ! (23) Immediate Origin Name
	dim em$(3)*30 ! (1)=Emp Name, (2)=Emp Addr, (3)=Emp CSZ
	dim bankaccount$*20,bankrouting$*20
	dim federalrouting$*20,fedid$*12,bankname$*23

	fntop(program$)
	crlf$=chr$(13)&chr$(10)
	open #hEmployee=1: "Name=[Q]\PRmstr\RPmstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",internal,input,keyed
	! open #dd=fnGetHandle: "Name=[Q]\PRmstr\DD.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],Shr,kps=1,kln=10,Use",internal,outIn,keyed
	dim dd$(0)*32, ddN(0)
	dd=fn_open('PR Direct Deposit',mat dd$,mat ddN,mat form$)
	open #hChecks:=fngethandle: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
	fncreg_read('Direct Deposit Save File Path',path$, fnSpecialFolderPath$('desktop')&'\DirDep.txt',1) ! The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'
	fncreg_read('Direct Deposit Source Bank Account',bankaccount$) ! The right hand set of numbers at the bottom of your checks.
	fncreg_read('Direct Deposit Source Bank Routing',bankrouting$) ! The middle set of numbers at the bottom of your checks.
	fncreg_read('Direct Deposit Federal Reserve Routing',federalrouting$) ! Routing Number of Federal Reserve Used by Your Bank
	fncreg_read('Direct Deposit Source Bank Name',bankname$)
	fncreg_read('Direct Deposit Federal ID Number',fedid$) ! The Federal ID number can be found on any payroll report.
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1)
	goto SCREEN1

XIT: fnxit

SCREEN1: !
	mypos=55
ASK_INFO: !
	fnTos
	respc=0
	fnLbl(1,90,"",1,1) ! bigger screen
	fnLbl(2,1,"Payroll Date:",mypos,1)
	fnTxt(2,mypos+3,10,0,1,"3",0,"For current payroll, always use the calculation date.  You can transfer older payrolls by using a previous payroll date.")
	resp$(respc+=1)=str$(d1)
	fnLbl(3,1,"Path to Save File to:",mypos,1)
	fnTxt(3,mypos+3,30,128,0,"70",0,"The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'")
	resp$(respc+=1)=path$
	fnLbl(5,1,"Your Bank Account Number:",mypos,1)
	fnTxt(5,mypos+3,8,0,0,"number",0,"The right hand set of numbers at the bottom of your checks.")
	resp$(respc+=1)=bankaccount$(1:8)
	fnLbl(6,1,"Routing Number of your Bank:",mypos,1)
	fnTxt(6,mypos+3,10,0,0,"number",0,"The middle set of numbers at the bottom of your checks.")
	resp$(respc+=1)=bankrouting$
	fnLbl(7,1,"Routing Number of Federal Reserve Used by Your Bank:",mypos,1)
	fnTxt(7,mypos+3,10,0,0,"number",0,"You will have to call your bank for this.  Some times it is build into their software and is not needed.")
	resp$(respc+=1)=federalrouting$
	fnLbl(8,1,"Your Bank Name:",mypos,1)
	fnTxt(8,mypos+3,23,0,0,"",0,"")
	resp$(respc+=1)=bankname$
	fnLbl(9,1,"Federal ID Number:",mypos,1)
	fnTxt(9,mypos+3,9,0,0,"number",0,"The Federal ID number can be found on any payroll report.")
	resp$(respc+=1)=fedid$(1:9)
	fnChk(13,mypos,"Is this a test file?",1)
	resp$(respc+=1)="False"
	fnCmdKey("&Next",1,1,0,"Create the file." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! ask employee number
	if ckey=5 then goto XIT
	ppd=val(resp$(1))
	path$=resp$(2)
	bankaccount$=odi$=resp$(3) ! bank account number
	bankrouting$=imo$=lpad$(resp$(4),10) ! your bank routing number
	bnkrtn=val(resp$(4)) conv ASK_INFO ! your bank routing number in numeric
	federalrouting$=imd$=resp$(5)
	bankname$=ion$=resp$(6)
	fedid$=cid$=resp$(7) : cid$="1"&cid$
	if resp$(8)="True" then testfile=1 else testfile=0

	fncreg_write('Direct Deposit Save File Path',path$) ! The path should contain the drive designation, any folders and a file name. Eg  'A:\DirDep.txt'
	fncreg_write('Direct Deposit Source Bank Account',bankaccount$) ! The right hand set of numbers at the bottom of your checks.
	fncreg_write('Direct Deposit Source Bank Routing',bankrouting$) ! The middle set of numbers at the bottom of your checks.
	fncreg_write('Direct Deposit Federal Reserve Routing',federalrouting$) ! Routing Number of Federal Reserve Used by Your Bank
	fncreg_write('Direct Deposit Source Bank Name',bankname$)
	fncreg_write('Direct Deposit Federal ID Number',fedid$) ! The Federal ID number can be found on any payroll report.

	open #ddout=fngethandle: "Name=DDout"&wsid$&".txt,RecL=96,EOL=CRLF,Replace",external,output
	fnopenprn
	gosub ReportHdr ! pr header
	gosub FileHeaderRecord ! file header
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
		read #hEmployee,using 'Form pos 9,3*C 30,Pos 162,N 6,Pos 173',key=dd$(dd_eno): mat em$,em17 nokey READ_DD
		if fndate_mmddyy_to_ccyymmdd(em17)<d1 then goto READ_DD    ! first screen emp data had a last PR Date lower than the Payroll Date specified in this program.
		checkkey$=dd$(dd_eno)&"         "
		restore #hChecks,key>=checkkey$: nokey READ_DD
		ReadCheck: !
		read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L970
		if heno=val(dd$(dd_eno)) then
			if prd><d1 then goto ReadCheck
			mat tcp=tcp+cp
			mat ttdc=ttdc+tdc
			goto ReadCheck
		end if
		L970: !
		if sum(tcp)=0 then goto READ_DD ! no pay on this person for this payroll date
		gosub EntryDetailRecord
	end if
goto READ_DD

FileHeaderRecord: ! r: File Header Record _____________________________________________
	pcde=01 ! Priority Code
	! if env$('client')="Washington Parrish" then imd$=" 061000146" ! Immediate Destination   (routing number for federal reserve bank they use)
	! if env$('client')="West Rest Haven" then imd$=" 111000038" ! Immediate Destination   (routing number for federal reserve bank they use) ! most likely wrong
	if env$('client')="Billings" then imd$=" 081505964" ! Immediate Destination   (routing number for federal reserve bank they use)
	! if env$('client')="Washington Parrish" then imo$=" 065201611" ! Immediate Origin  (routing number for your bank)
	! if env$('client')="West Rest Haven" then imo$=" 111905159" ! Immediate Origin (contains the routing number for your bank)
	if env$('client')="Billings" then imo$=" 000017738" ! Immediate Origin (contains the routing number for your bank)
	fcd$=date$("YYMMDD") ! File Creation Date
	fct$=time$(1:2)&time$(4:5) ! File Creation Time
	fidm$="A" ! File ID Modifier
	rsz$="094" ! Record Size
	bf$="10" ! Blocking Factor
	fc$="1" ! Format Code
	idn$="Federal Reserve Bank   " ! (23) Immediate Destination Name
	! if env$('client')="Washington Parrish" then ion$="Parrish National       " ! (23) Immediate Origin Name  (your bank name)
	! if env$('client')="West Rest Haven" then ion$="State National Bank    " ! (23) Immediate Origin Name
	if env$('client')="Billings" then ion$="Bank of Billings       " ! (23) Immediate Origin Name
	rc$="" ! Reference Code
	write #ddout,using 'Form POS 1,G 1,PIC(##),C 10,C 10,G 6,G 4,C 1,C 3,C 2,C 1,C 23,C 23,C 7,C 1,c 2': 1,pcde,imd$,imo$,fcd$,fct$,fidm$,rsz$,bf$,fc$,idn$,ion$,rc$,"0",crlf$
	! Company/Batch Header Record __________________________________________
	scc=220 ! Service Class Code
	cdd$="" ! Company Discretionary Data
	ecc$="PPD" ! Standard Entry Class Code
	ced$="Payroll  " ! Company Entry Descriptive
	! if env$('client')="West Rest Haven" then cid$="1741551549" ! Company Identification  ! instructions say it is a 1 digit identification code designator plus a nine digit identification number (assure a 1 means federal id code plus the federal id #)
	! if env$('client')="Washington Parrish" then cid$="1726001461" ! Company Identification
	if env$('client')="Billings" then cid$="1430903099" ! Company Identification
	eed$=date$("YYMMDD") ! Effective Entry Date
	osc$="1" ! Originator Status Code
	bn=1 !  BN=Batch Number
	! if env$('client')="Washington Parrish" then odi$="20428027" ! Origination DFI Identification  (your bank account number)
	! if env$('client')="West Rest Haven" then odi$=" 1055003" ! Origination DFI Identification  (your bank account number)
	if env$('client')="Billings" then odi$=" 0040118" ! Origination DFI Identification  (your bank account number)
	write #ddout,using 'Form POS 1,G 1,PIC(###),C 16,C 20,C 10,C 3,C 10,PIC(######),G 6,G 3,G 1,C 8,PIC(#######),c 2': 5,scc,env$('cnam')(1:16),cdd$="Payroll",cid$,ecc$,ced$,fncd(d2),eed$,"",osc$,odi$,bn,crlf$
return ! /r

EntryDetailRecord: ! r: entry detail
	t1=t1+tcp(32)
	if ddN(dd_accType)=27 then
		tc=22
	else if ddN(dd_accType)=37 then
		tc=32 ! BC ! Transaction Code used to be was TC=23
	end if
	ari=0 ! Addenda Record Indicator
	tn1=tn1+1
	tn$=cnvrt$("PIC(#######)",tn1) ! Trace Number
	dr$="081505731"
	da$="10004147         "
	if testfile=1 then tcp(32)=0
	write #ddout,using 'Form POS 1,G 1,G 2,pic(########),C 1,C 17,PIC(##########),C 15,C 22,G 2,N 1,C 8,c 7,c 2': 6,tc,int(ddN(dd_routing)/10),str$(ddN(dd_routing))(len(str$(ddN(dd_routing))):len(str$(ddN(dd_routing)))),dd$(dd_account),tcp(32)*100,z$,em$(1)(1:22),"",ari,lpad$(trim$(odi$),8),tn$,crlf$         ! changed dr$ to str(ddN(dd_routing)) ; also da$ to dd$(dd_account)  ! entry to place money in employees account
	pr #255: z$&" "&em$(1)&" "&str$(tcp(32)) pageoflow ReportPgof
	td1=td1+(tcp(32)*100)
	tc1+=(tcp(32)*100) ! added this for the batch totals - ??
	! if env$('client')="West Rest Haven" and ddN(dd_routing)=1190515 then totalin=totalin+tcp(32)
	eh=eh+int(ddN(dd_routing)/10) ! Entry Hash should accumulate Routing numbers    dropping the last digit of the routing number
return ! /r

BatchControlRecord: ! r: Company/Batch Control Record
	scc=220 ! Service Class Code
	eac=tn1 ! Entry Addenda Count
	! eH=0 ! Entry Hash
	eh$=str$(eh): x=len(eh$): eh=val(eh$(max(1,x-9):x))
	! TD1=Total Debit Amount
	! TC1=Total Credit Amount
	! CID$=Company Identification
	if eh=0 then
		mat ml$(3)
		ml$(1)="It appears you do not have anyone with"
		ml$(2)="direct deposit this pay period."
		ml$(3)="Click OK to continue."
		fnmsgbox(mat ml$,resp$)
		goto XIT
	end if
	write #ddout,using 'Form POS 1,G 1,PIC(###),PIC(######),PIC(##########),2*PIC(############),C 10,C 19,C 6,C 8,PIC(#######),c 2': 8,scc,eac,eh,td1,tc1,cid$,mac$,"",odi$,bn,crlf$ ! removed *100 from TD1 and from TC1
	!
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
		mat ml$(2)
		ml$(1)="It appears you do not have anyone with"
		ml$(2)="direct deposit this pay period."
		ml$(3)="Click OK to continue."
		fnmsgbox(mat ml$,resp$)
		goto XIT
	end if
	write #ddout,using 'Form POS 1,G 1,G 2,pic(########),C 1,C 17,PIC(##########),C 15,C 22,G 2,N 1,C 8,c 7,c 2': 6,27,int(bnkrtn/10),str$(bnkrtn)(len(str$(bnkrtn)):len(str$(bnkrtn))),bankaccount$,td1,"","","",ari,lpad$(trim$(odi$),8),tn$,crlf$        ! changed dr$ to str(ddN(dd_routing)) ; also da$ to dd$(dd_account)  ! total entry for  debiting customer account
	write #ddout,using 'Form POS 1,G 1,2*PIC(######),PIC(########),PIC(##########),2*PIC(############),C 38,C 1,c 2': 9,bactr,blctr,eac,eh,td1,tc1,rpt$(" ",38)," ",crlf$    ! removed *100 from TD1 and TC1
	if bkfactor<>0 then
		for j=1 to bkfactor
			! pr "l22="&STR$(L22+=1)
			write #ddout,using 'Form POS 1,C 96': rpt$("9",94)&crlf$
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
	pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
	pr #255: "\ql   "
return ! /r

Finis: ! r:
	! pr 'Finis' ! XXX
	close #hEmployee: ioerr ignore
	close #dd: ioerr ignore
	gosub BatchControlRecord
	close #ddout:
	if trim$(path$)<>'' then
		if exists(path$) then let fnFree(path$)
		fnCopy("DDout"&wsid$&".txt",path$)
	end if
	if trim$(email$)<>"" then
		execute "sy Start Mailto:"&trim$(email$)&"?attach=DDout"&wsid$&".txt?subject=Direct_Deposit_Payroll"
		pr newpage
		pr f "10,1,Cc 80,N": "After sending your e-mail,"
		pr f "11,1,Cc 80,N": "Pess ENTER to continue."
		input fields "1,1,C 1,N": pause$
	end if
	fnfree("DDout"&wsid$&".txt")

	pr #255,using L1770: "Total",tc1/100
	L1770: form pos 22,c 15,n 12.2,skip 1
	! if env$('client')="West Rest Haven" then pr #255,using L1770: "Total In House",totalin
	fncloseprn
goto XIT ! /r

include: ertn
include: fn_open
