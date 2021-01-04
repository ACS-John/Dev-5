! formerly S:\acsGL\GLInput
! enter GL transactions
! r: setup library, dims, constants, fnTop
	autoLibrary
	on error goto Ertn
!
	dim cap$*128,resp$(30)*128
	dim tr(7),tr$*12,td$*30,tcde(10,2),dedcode(10),empname$*30
	dim miscname$(10)*20
	dim key$*12,k(30,8),camt(5)
	dim d(2),cgl$(5)*12
	dim pgl(5,3)
	dim pr(19)
! dim contra(6)
	dim miscgl$(10)*12
	dim jv$(3)*8,vn$(4)*30
	dim typeofentry_option$(6)*18,message$*100,glitem$(4)*30
	dim glitem2$(7)*30,ml$(4)*100,text$*80,bankname$*40,k_list$(30)*12
	dim pr(19)
!
	fnTop(program$,cap$="Enter Transactions")
	gltyp=7
	fnreg_read('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$,'False')
! fil$(1)="Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr"
! fil$(2)="Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]"
! fil$(3)="Name=[Q]\GLmstr\GLWK2"&wsid$&".h[cno]"
!
! fil$(5)="Name=[Q]\GLmstr\GLPT"&wsid$&".h[cno]"
! fil$(6)="Name=[Q]\GLmstr\GLBRec.h[cno],KFName=[Q]\GLmstr\GLRecIdx.h[cno],Shr"
!
	typeofentry_option$(1)="1 = Disbursements"
	typeofentry_option$(2)="2 = Receipts"
	typeofentry_option$(3)="3 = Adjustments"
	typeofentry_option$(4)="4 = Payroll Check"
	typeofentry_option$(5)="5 = Sales"
	typeofentry_option$(6)="6 = Purchases"
	dim cmask3$(6),chdr_proof_total$(6),glitem3$(6)
	chdr_proof_total$(1)='G/L Account'
	chdr_proof_total$(2)='Beg Balance'
	chdr_proof_total$(3)='Receipts'
	chdr_proof_total$(4)='Disbursements'
	chdr_proof_total$(5)='Adjustments'
	chdr_proof_total$(6)='End Balance'
	cmask3$(1)=''
	cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(5)="10"
	cmask3$(6)='10'
! General Ledger Breakdown Grid
	mat chdr$(5)
	chdr$(1)='Reference'
	chdr$(2)='Payee Number'
	chdr$(3)='GL Number'
	chdr$(4)='Percent'
	chdr$(5)='Description      '
	mat cmask$(5)
	cmask$(1)=cmask$(2)=cmask$(3)=cmask$(5)=''
	cmask$(4)='32'
	mat glitem$(5)
!
	dim chdr2$(7)*25
	mat chdr2$(7)
	mat cmask2$(7)
	mat glitem2$(7)
	chdr2$(1)='Record '
	chdr2$(2)='Date'
	chdr2$(3)='Reference #'
	chdr2$(4)='Payee/Description'
	chdr2$(5)='G/L #  '
	chdr2$(6)='Amount'
	chdr2$(7)='Allocation Description'
	cmask2$(2)='3'
	cmask2$(3)=cmask2$(4)=''
	cmask2$(5)=''
	cmask2$(6)='10'
	cmask2$(7)=""
!
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #1,using 'Form POS 150,2*N 1,POS 298,15*PD 4,POS 382,N 2,POS 418,10*C 20,10*N 1,POS 668,10*C 12': mat d,mat pgl,jccode,mat miscname$,mat dedcode,mat miscgl$
	close #1:
! /r
	open #h_glmstr=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
!
	if gltyp<>1 then
		open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,outIn,keyed ioerr ignore ! 00340   open #1: fil$(1),internal,outIn,keyed ioerr ignore
		open #3: "Name=[Q]\GLmstr\GLWK2"&wsid$&".h[cno]",internal,outIn,relative ioerr ignore ! PR Work File
	end if
	open #h_gl_work:=2: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]",internal,outIn,relative ioerr ignore ! GL Work File
	F_2A: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
	F_2B: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
	pc=1
	adr=lrec(h_gl_work)
	if adr<1 then goto L460
L400: !
	read #h_gl_work,using 'form pos 27,n 2',rec=adr: pc noRec CHECK_FOR_DELETED conv L460
	if pc=0 then post=1 ! if previous batch not posted, set post=1
	if pc>0 then goto L460 ! PREVIOUS TRANSACTIONS HAVE BEEN POSTED
	open #5: "Name=[Q]\GLmstr\GLPT"&wsid$&".h[cno]",internal,outIn,relative ioerr L460
	goto SCREEN_1
	L460: !
	open #5: "Name=[Q]\GLmstr\GLPT"&wsid$&".h[cno],SIZE=0,RecL=538,Replace",internal,outIn,relative
	write #5,using F_5,rec=1: kn,mat k,td,tc,mat tcde
SCREEN_1: ! r:
	edit=0
	fnTos(sn$="GLInput")
	mylen=20: mypos=mylen+3 : right=1 : rc=0
	fnFra(1,1,4,60,"Method of Entry","Choose the method of transaction entry.")
	if post=0 then text$="Regular Input" else text$="Correction or Addition to previous input"
	fnOpt(1,2,text$,0,1)
	resp$(respc_regularInput:=rc+=1)="True"
	fnOpt(2,2," Erase previous input transactions" ,0,1)
	resp$(respc_erasePrevious:=rc+=1)="False"
	fnOpt(3,2,"Input from Client Checkbook Diskette",0,1)
	resp$(respc_inputClientCL:=rc+=1)="False"
! fnOpt(4,2,"Input from Client A/R  Diskette",0,1) ! this option has errors and seems to be no longer used, so i removed it 12/29/2015
! resp$(4)="False"
	fnLbl(7,1,"Type Of Entry:",mylen,right)
	fncomboa("TypeOfEntry1",7,mypos,mat typeofentry_option$,"You must indicate the type of entry you will be entering.",18)
	if sel=0 then sel=3
	resp$(respc_entryType:=rc+=1)=typeofentry_option$(sel)
	! resp$(4)=typeofentry_option$(max(1,sel)) !  for j=1 to 6
													!    if sel=j then resp$(4)=typeofentry_option$(j)
													!  next j
	fnLbl(8,1,"Bank Account:",mylen,right)
	fnqgl(8,mypos,0,2,pas)
	resp$(respc_bankGl:=rc+=1)=fnrgl$(bankgl$)
	fnLbl(9,1,"Process Ending Date:",mylen,right)
	fnTxt(9,mypos,8,0,right,"1001",0,"Process endings date must always be answered and will be the last day of the month or the last day of the period beding processed.",0 )
	resp$(respc_contraDate:=rc+=1)=str$(contra_entry_date)
	fnCmdKey("&Next",1,1,0,"Allows you to enter transactions.")
	fnCmdKey("&Proof Totals",3,0,0,"Provides proof totals and option to post the entries.")
	fnCmdKey("&Cancel",5,0,1,"Exits.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
! pOST=1 ! code as unposted once leave this screen
	if post=1 and resp$(respc_regularInput)="True" then gosub CHECK_FOR_CONTRAS ! remove any contra entries created in previous run
	if resp$(respc_regularInput) ="True" then pt1=j=1
	if resp$(respc_erasePrevious)="True" then pt1=2
	if resp$(respc_inputClientCL)="True" then pt1=3
! if resp$(4)="True" then pt1=4
	sel=sel1=srch(mat typeofentry_option$,resp$(respc_entryType)) ! val(resp$(respc_entryType)(1:1)) ! type of transaction
! if sel<=1 then pause
! If SEL=4 Then Goto SCREEN_1 ! temporary line do not access payroll checks
	typeofentry$=resp$(4)
	key$=bankgl$=fnagl$(resp$(respc_bankGl)) ! GL number
	contra_entry_date=val(resp$(respc_contraDate)) ! contra entry date
	if ckey<>18 then  ! check bank account if editing transactions
		if (sel=1 or sel=2) and val(bankgl$)=0 then goto BANK_GL_FAIL ! must have GL bank account on receipts or disbursements
		if (sel=1 or sel=2 or sel=4) then read #h_glmstr,using "form pos 13,c 40",key=bankgl$,release: bankname$ nokey BANK_GL_FAIL
	end if
	if pt1=1 and post=0 then goto ERASE_PREVIOUS_INPUT ! regular input and previous batch posted
	post=1
	if pt1=3 or pt1=4 then gosub SCREEN_INSERT_DISKETTE
	if pt1=4 then goto COPY_AR_IN ! Input from client AR
	if pt1=2 then goto ERASE_PREVIOUS_INPUT ! erase any previous entries
	goto L1140 ! /r
BANK_GL_FAIL: ! r:
	mat ml$(2)
	ml$(1)="You must have a Bank Account General Ledger"
	ml$(2)="Number for disbursements or receipts."
	fnmsgbox(mat ml$,resp$,cap$,49)
	goto SCREEN_1 ! /r
ERASE_PREVIOUS_INPUT: ! r:
	post=1
	close #3: ioerr ignore
	close #h_gl_work: ioerr ignore
	close #5: ioerr ignore
	close #glallocations: ioerr ignore
	close #paymstr: ioerr ignore
	close #payeegl: ioerr ignore
	if pt1=3 then
		fnCopy(dv$&"GLWK101.h[cno]","[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]")
		fnCopy(dv$&"GLWK201.h[cno]","[Q]\GLmstr\GLWK2"&wsid$&".h[cno]")
		open #3: "Name=[Q]\GLmstr\GLWK2"&wsid$&".h[cno]",internal,outIn,relative
		open #h_gl_work:=2: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]",internal,outIn,relative
		open #9: "Name="&dv$&"PAYMSTR.h[cno],KFName="&dv$&"PAYIDX1.h[cno]",internal,input,keyed ioerr ignore
	else
		open #3: "Name=[Q]\GLmstr\GLWK2"&wsid$&".h[cno],size=0,RecL=110,Replace",internal,outIn,relative
		open #h_gl_work:=2: "Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno],SIZE=0,RecL=104,Replace",internal,outIn,relative
	end if
	open #5: "Name=[Q]\GLmstr\GLPT"&wsid$&".h[cno],Size=0,RecL=538,Replace",internal,outIn,relative
	write #5,using F_5,rec=1: kn,mat k,td,tc,mat tcde
F_5: form pos 1,pd 6.2,80*pd 6.2,pd 6.2,pd 6.2,20*n 2
	goto L1140 ! /r
L1140: ! r:
	close #6: ioerr ignore
	open #6: "Name=[Q]\GLmstr\GLBRec.h[cno],KFName=[Q]\GLmstr\GLRecIdx.h[cno],Shr",internal,outIn,keyed
	open #paymstr=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	open #payeegl=fnH: "Name=[Q]\GLmstr\PayeeGLBreakdown.h[cno],Version=1,KFName=[Q]\GLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
	close #glallocations: ioerr ignore
	open #glallocations=12: "Name=[Q]\GLmstr\GLALLOCATIONS"&wsid$&env$('cno')&",Version=1,replace,RecL=59",internal,outIn,relative
	if jccode then
		open #8: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed ioerr ignore
	end if
	if ckey=3 then goto PROOF_TOTALS
goto MAIN ! /r
PREPARE_EDIT_SCREEN: ! r:
	transactionamt=0
	editmode=1
	gl$=""
	totalalloc=0
	read #h_gl_work,using fGlWork,rec=rn: mat tr,tr$,td$,vn$,mat jv$,key$ noRec PES_XIT ! get basic information from record clicked to find the complete transaction
	holdtr$=tr$
	close #glallocations: ioerr ignore
	open #glallocations=12: "Name=[Q]\GLmstr\GLALLOCATIONS"&wsid$&env$('cno')&",Version=1,replace,RecL=59",internal,outIn,relative
	restore #h_gl_work:
GLALLOCATIONS_READ: !
	read #h_gl_work,using F_2B: payeegl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof GLALLOCATIONS_EOF
	if trim$(tr$)<>trim$(holdtr$) then goto GLALLOCATIONS_READ
	if tr(6)=2 or tr(6)=7 then tr(5)=-tr(5) ! reverse signs on receipts and sales BEFORE DISPLAYING ON CORRECTION SCREEN
	transactionamt+=tr(5)
	vn$=vn$
	sel=tr(6)
	if tr(6)=1 then sel=1 ! reg disb
	if tr(6)=2 then sel=2 ! reg receipt
	if tr(6)=3 then sel=3 ! reg adj
	if tr(6)=4 then sel=4
	if tr(6)=7 then sel=5 !
	if tr(6)=8 then sel=6 !
	write #glallocations,using "form pos 1,c 12,pd 10.2,c 30,pd 5": payeegl$,tr(5),td$,rec(2)
	totalalloc+=tr(5) ! re-add total allocations
	goto GLALLOCATIONS_READ
GLALLOCATIONS_EOF: !
	read #h_gl_work,using fGlWork,rec=rn: mat tr,tr$,td$,vn$,mat jv$,key$ noRec PES_XIT ! get basic information from record clicked to find the complete transaction
PES_XIT: return  ! /r
POSTING_OPTIONS: ! r:
	fnTos(sn$="GLInput6")
! fnFra(1,1,6,60,"Posting Options","You would only use the After_The_Fact options if you are maintaining the payroll records within the general ledger system.",0)
	fnOpt(1,2,"Post to General Ledger",0,0)
	resp$(1)="True"
	fnOpt(2,2,"Automatic Processing",0,0)
	resp$(2)="False"
	fnOpt(3,2,"Post After-The-Fact Payroll only",0,0)
	resp$(3)="False"
	fnOpt(4,2,"Post both General Ledger and After-The-Fact Payroll",0,0)
	resp$(4)="False"
	fnOpt(5,2,"Return to Menu without posting",0,0)
	resp$(5)="False"
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	fnfscode(0)
	gosub CREATE_CONTRA_ENTRIES
	if resp$(5)="True" or ckey=5 then goto Xit ! return w/o posting
	if resp$(2)="True" then let fnprocess(1) else let fnprocess(0)
	if resp$(4)="True" then let fnprocess(4) ! post both
	if resp$(1)="True" then goto ACGLMRGE
	if resp$(3)="True" then let fnchain("S:\acsGL\PRMerge")
	open #h_process:=30: "Name=[Q]\GLmstr\Process.h[cno],RecL=128,Use",internal,outIn,relative
	if lrec(h_process)=0 then write #h_process,using "form pos 1,n 1": 0
	if resp$(2)="True" then rewrite #h_process,using "form pos 1,n 1",rec=1: 1 else rewrite #h_process,using "form pos 1,n 1",rec=1: 0 ! code for post payroll and gl both
	if resp$(4)="True" then rewrite #h_process,using "form pos 1,n 1",rec=1: 4 ! code for posting pr and gl both
	close #h_process:
	if resp$(4)="True" then goto ACGLMRGE ! post both
	if resp$(2)="True" then let fnchain("S:\acsGL\autoproc")
	goto Xit ! /r
Xit: fnXit
!
RECORD_TRANSACTIONS: ! r:
	if tr(6)=2 or tr(6)=7 then tr(5)=-tr(5) ! reverse signs on receipts and sales
	if transadr>0 then
		rewrite #h_gl_work,using fGlWork,rec=transadr: mat tr,tr$,td$ noRec L3280
		goto L3300
	end if
	lr2=lrec(h_gl_work)+1
	L3280: !
	write #h_gl_work,using fGlWork,rec=lr2: mat tr,tr$,td$,vn$,mat jv$,key$
	fGlWork: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
	L3300: !
return  ! /r
MAIN: ! r:
	if sel=4 and edit=0 then goto PAYROLL
	fnTos(sn$="GLInput2-"&str$(sel))
	mylen=18 : mypos=mylen+3 : right=1
	fnLbl(3,1,"Date:",mylen,right)
	fnTxt(3,mypos,8,0,right,"1",0,"Transaction date must always be answered.",0 )
	resp$(1)=str$(tr(4))
	if sel=3 then
		fnLbl(4,1,"Amount:",mylen,right)
	else
		fnLbl(4,1,"Net Amount:",mylen,right)
	end if
	fnLbl(4,36,message$,50,left)
!
	fnTxt(4,mypos,13,0,right,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	if sel=3 then resp$(2)="" else resp$(2)=str$(transactionamt)
	fnLbl(5,1,"Reference #:",mylen,right)
	fnTxt(5,mypos,12,0,0,"",0,"Enter check number, receipt # or adjusting entry number",0)
	resp$(3)=tr$
	if sel=2 or sel=3 or sel=5 then
		fnLbl(6,1,"Description:",mylen,right) ! for receipts
		fnTxt(6,mypos,30,0,left,"",0,"Brief description of transaction.",0 )
		resp$(4)=td$
	else
		fnLbl(6,1,"Payee #:",mylen,right)
		if disable_payee=1 then
			fnTxt(6,mypos,8,0,right,"",1,"Payee field disabled. Click 'Enable Payee' again to enable.",0 )
			resp$(4)=""
		else
			fncombof("Paymstrcomb",6,mypos,35,"[Q]\GLmstr\paymstr.h[cno]",1,8,9,39,"[Q]\GLmstr\payidx1.h[cno]",0,pas, "If the payee # is known, the general ledger information can be extracted from that record.",0)
			resp$(4)=vn$
		end if
	end if
	fnLbl(7,1,"General Ledger #:",mylen,right)
	fnqgl(7,mypos,0,2,pas)
	resp$(5)=fnrgl$(gl$)
	if sel=3 then
		fnLbl(7,60,"Net Adj:",8,right)
		fnTxt(7,70,13,0,right,"10",1,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
		resp$(6)=str$(totalalloc)
	else
		fnLbl(7,60,"Amount:",8,right)
		fnTxt(7,70,13,0,right,"10",0,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
		resp$(6)=""
	end if
	seltype=tr(6) : if tr(6)>4 then seltype=tr(6)-2
	if edit=1 then typeofentry_selected$=typeofentry_option$(seltype) else typeofentry_selected$=typeofentry$(4:20)
	fnLbl(1,4,"Type of Entry: "&typeofentry_selected$,36,right)
	fnLbl(1,38,"Bank Account: "&bankname$,50,right)
	fnLbl(9,53,"Transaction Breakdown")
! r: General Ledger Transaction Breakdown Grid
	mat chdr$(4)
	chdr$(1)='Reference'
	chdr$(2)='GL Number'
	chdr$(3)='Amount'
	chdr$(4)='Description'
	mat cmask$(4)
	mat cmask$=('')
	cmask$(3)='10'
	fnflexinit1('Glalloc',10,40,6,60,mat chdr$,mat cmask$,1,0,0)
	restore #glallocations:
	mat glitem$(4)
	do  ! READ_GL_ALLOCATIONS: !
		read #glallocations,using 'Form Pos 1,c 12,pd 10.2,c 30': allocgl$,allocamt,td$ eof EO_FLEX1
		glitem$(1)=str$(rec(glallocations))
		glitem$(2)=allocgl$
		glitem$(3)=str$(allocamt)
		glitem$(4)=td$
		fnflexadd1(mat glitem$)
	loop  !  goto READ_GL_ALLOCATIONS
EO_FLEX1: ! /r
	if sel=1 or sel=6 then
		fnButton(6,61,"E&xtract",15,"Extracts general ledger numbers from payee records",1,8)
		if disable_payee=1 then payee_button$ ="Enable &Payee" else payee_button$ ="Disable &Payee"
		fnButton(6,72,payee_button$,16,"Allows you to disable or enable the payee field.",1,12)
		fnButton(6,87,"&Add Payee",17,"Allows you to add a payee record.",1,10)
	end if
	fnButton(9,85,"&Edit",18,"Correct an allocation.",1,5)
	fnButton(9,92,"Edit Al&l",19,"Edit all allocations without returning to this screen.",1,8)
! fnLbl(17,73,"",1,right)
! fnLbl(16,1," ")
! If EDIT=1 Then Let fnCmdKey("C&hange Acct #",9,0,0,"")
	if editmode=1 then
		fnCmdKey("&Complete",30,1,0,"Completed making corrections to this transaction.")
	else
		fnCmdKey("&Next Transaction",1,1,0,"You are completed with this transaction and ready to move to the next transaction.")
	end if
	fnCmdKey("&More Breakdowns",2,0,0,"More breakdowns to the same transaction.")
	fnCmdKey("&Review Transactions",3,0,0,"Prints a list of all transactions entered during the setting and also provides for edit options.")
	fnCmdKey("&Delete",7,0,0,"Deletes the entire transaction as shown on screen.")
	fnCmdKey("&Back",6,0,0,"Return to first screen to change transaction types or bank accounts.")
	if ~edit then let fnCmdKey("&Finish",9,0,1,"")
	fnAcs(mat resp$,ckey)
	allocamt=0
	! pAS=1 ! kj 61107
	message$=""
	if extract=1 and ckey<>1 then extract=0
	if (ckey=9 or ckey=3) and sel1=3 and val(resp$(2))<>0 then ckey=1 ! force the last entry to write   ! KJ 50707
	if ckey=30 and sel1=3 and val(resp$(2))<>0 then ckey=1 ! force the last entry to write   ! KJ 50707
	if (ckey=9 or ckey=3 or ckey=6) and lrec(glallocations)>0 and edit=0 then goto AFP_XIT ! 4430 ! unwritten record on screen
	if ckey=9 and val(resp$(2))=0 then
		goto PROOF_TOTALS
	else if ckey=3 then
		gosub REVIEW_TRANS : goto MAIN
	else if ckey=6 then
		transactionamt=0
		un$=tr$=""
		goto SCREEN_1
	else if ckey=7 then
		goto DELETE_TRANS
	else if ckey=16 then
		if disable_payee=0 then disable_payee=1 : goto MAIN
		if disable_payee=1 then disable_payee=0 : goto MAIN
	end if
 
	tr(4)=val(resp$(1)) ! date
	transactionamt=val(resp$(2)) ! amount
	if ckey=2 and transactionamt=0 then goto MAIN
	tr$=resp$(3) ! ref #
	vn$=vn$=resp$(4)(1:8) ! payee number
	glkey$=lpad$(rtrm$(vn$),8)
	read #paymstr, using "form pos 1,c 8",key=glkey$,release: x$ nokey PAYMSTR_GLNUM_NOKEY
	td$=resp$(4)(9:38) ! transaction description = vendor name when vendor entered
goto ALLOCATE_FROM_PAYEE
PAYMSTR_GLNUM_NOKEY: !
	td$=resp$(4)(1:30)
goto ALLOCATE_FROM_PAYEE ! use full response as description if not a payee name
! /r
ALLOCATE_FROM_PAYEE: ! r:
	if ckey=15 and editmode=0 then
		extract=1
		goto EXTRACT ! pull allocation breakdown from payee record
	end if
	allocgl$=fnagl$(resp$(5))
	allocamt=val(resp$(6))
	if sel=3 and ckey<>30 then allocamt=transactionamt ! create an allocation amount automatically on adjustments
	if allocamt=0 and ckey=1 and lrec(glallocations)=0 then allocamt=transactionamt ! allows them to press enter if only allocation without haveing to key the amount a second time
	if extract=1 then goto AFP_XIT
	if ckey=18 then goto AFP_XIT ! don't require gl # on main screen when editing transaction
	if ckey=30 and edit=1 then goto AFP_XIT ! don't require gl # when editing and take complete
	if sel=3 and (allocamt=0 or ckey=30) then goto AFP_XIT ! don't require gl # on adjustments when amount=0 (actually changing from one adjustment to the next)
	if sel=4 and edit=1 then goto AFP_XIT ! never should have an amount on payroll check. (done from another screen and edits all made from allocation screen
	if ckey=17 then goto AFP_XIT
	x=val(allocgl$) conv AFP_BAD_GL
	if edit=1 then goto AFP_XIT ! KJ 080608   DON'T CHECK FOR GENERAL NUMBER ON EDIT
if val(allocgl$)=0 then goto AFP_BAD_GL else goto AFP_XIT ! /r
AFP_BAD_GL: ! r:
	mat ml$(3)
	ml$(1)="You must have a General Ledger Number"
	ml$(2)="on each allocation."
	ml$(3)="Click OK to enter the general ledger number."
	fnmsgbox(mat ml$,resp$,cap$,49)
goto MAIN ! r
AFP_XIT: ! r:
	if ~edit=1 then ! DON'T CHANGE CODE IF MAKEING CORRECTIONS
		if sel=1 then tr(6)=1 ! reg disb
		if sel=2 then tr(6)=2 ! reg receipt
		if sel=3 then tr(6)=3 ! adj
		if sel=4 then tr(6)=4 ! payroll check
		if sel=5 then tr(6)=7 ! sales
		if sel=6 then tr(6)=8 ! purchases
	end if
	if ckey=17 then
		fnaddglpayee
		pas=0
		goto MAIN
	else if ckey=18 then
		gosub EditAllocations
		goto MAIN
	else if ckey=19 then
		editall=1
		gosub EditAllocations
		goto MAIN
	else if ckey=30 then
		goto L4460
	else if ckey=2 and allocamt<>0 then
		write #glallocations,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocamt,td$
		allocgl$=""
		totalalloc+=allocamt
		allocamt=0
		goto MAIN
	else if ckey=1 and allocamt<>0 then
		write #glallocations,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocamt,td$
		allocgl$=""
		totalalloc+=allocamt
		allocamt=0
	end if
	if ckey=1 and sel <>3 and transactionamt<>totalalloc then message$= "Allocations don't add up!" : goto MAIN
	! If CKEY=1 AND SEL=3 AND TOTALALLOC <>0 Then mESSAGE$= "Allocations don't add up!": Goto MAIN
	if ckey=1 or ckey=2 or (ckey=9 and edit=0 and lrec(glallocations)>0) then  ! ckey 9 thing: allow last transaction to be written if take finish
		L4460: !
		restore #glallocations:
		for j=1 to lrec(glallocations)
			read #glallocations,using "form pos 1,c 12,pd 10.2,c 30,pd 5": allocgl$,allocamt,td$,transadr eof L4520
			tr(1)=val(allocgl$(1:3)) : tr(2)=val(allocgl$(4:9))
			tr(3)=val(allocgl$(10:12)) : tr(5)=allocamt
			gosub RECORD_TRANSACTIONS
		next j
		L4520: !
	else
		goto L4620
	end if
	if ckey=2 then goto L4620
goto CLEAN_MAIN_SCREEN ! /r
CLEAN_MAIN_SCREEN: ! r: clear entry screen before returning
	close #glallocations: ioerr ignore
	open #glallocations=12: "Name=[Q]\GLmstr\GLALLOCATIONS"&wsid$&env$('cno')&",Version=1,replace,RecL=59",internal,outIn,relative
	transactionamt=0
	if gl_retainFieldsDuringAdd$='False' then
		td$=""
	end if
	fn_increment_tr
	vn$=gl$=""
	totalalloc=editmode=0 ! extract=0 kj
	if ckey=3 or ckey=30 then
		gosub REVIEW_TRANS : goto MAIN
	else if ckey=6 then
		transactionamt=0
		un$=tr$=vn$=""
		goto SCREEN_1
	end if
	!
	L4620: !
	if ckey=9 then goto PROOF_TOTALS
goto MAIN ! /r
EXTRACT: ! r:
	glkey$=lpad$(rtrm$(vn$),8)
	restore #payeegl,key>=glkey$: nokey L4780
READPAYEEGL: !
	read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,td$ eof L4740
	if payeekey$<>glkey$ then goto L4740
	if percent=0 and lrec(glallocations)=0 then percent=100
	allocamt=round(transactionamt*(percent*.01),2)
	write #glallocations,using "form pos 1,c 12,pd 10.2,c 30": payeegl$,allocamt,td$: allocgl$="": totalalloc+=allocamt: allocamt=0
	goto READPAYEEGL
L4740: !
	if totalalloc<>transactionamt then
		read #glallocations,using "Form pos 13,pd 10.2",rec=lrec(glallocations): lastallocation noRec L4770
	else
		goto L4770
	end if
	lastallocation+=transactionamt-totalalloc
	rewrite #glallocations,using "Form pos 13,pd 10.2",rec=lrec(glallocations): lastallocation
	L4770: !
	allocamt=0: ! kj  eXTRACT=0
	L4780: !
goto MAIN ! /r
DELETE_TRANS: ! r: deletes entire transaction
	mat ml$(3)
	ml$(1)="You have chosen to delete this entire entry."
	ml$(2)="Click Ok to delete this entry."
	ml$(3)="Click Cancel to return to main entry screen."
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L5120 else goto MAIN
L5120: restore #glallocations:
L5130: read #glallocations,using "Form pos 1,c 12,pd 10.2,c 30,pd 5": gl$,allocation,td$,transadr eof L5170
	delete #glallocations:
	delete #h_gl_work,rec=transadr: ioerr ignore
	goto L5130
L5170: !
	transactionamt=0
	vn$=gl$=tr$=""
	goto MAIN ! /r
REVIEW_TRANS: ! r:
	fnTos(sn$="GLInput4")
	fnflexinit1('GlTrans',1,1,20,90,mat chdr2$,mat cmask2$,1,0,0)
	restore #h_gl_work:
L5230: read #h_gl_work,using F_2B: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof L5310
	if trim$(tr$)<>"" and tr$<>oldtr$ then
		mat glitem2$=("")
		glitem2$(6)=str$(net)
		glitem2$(7)="Net"
		fnflexadd1(mat glitem2$)
		net=0 ! add net subtotals any time reference number changes     ( AND NET<>0)was in there
	end if
! gL$=CNVRT$("pic(zz#)",TR(1))&CNVRT$("pic(zzzzz#)",TR(2))&CNVRT$("pic(zz#)",TR(3))
	glitem2$(1)=str$(rec(2))
	glitem2$(2)=str$(tr(4)) ! str$(date(days(tr(4),'mmddyy'),'ccyymmdd'))
	glitem2$(3)=tr$
	glitem2$(4)=vn$
	glitem2$(5)=gl$
	glitem2$(6)=str$(tr(5))
	glitem2$(7)=td$
	fnflexadd1(mat glitem2$)
	net+=tr(5) ! add net check
	oldtr$=tr$ ! hold reference numbers
	goto L5230
L5310: !
	mat glitem2$=("")
	glitem2$(6)=str$(net): glitem2$(7)="Net"
	fnflexadd1(mat glitem2$)
	net=0 ! add net subtotals at end of listing
	fnCmdKey("&Add",1,0,0,"Add additional transactions or allocations.")
	fnCmdKey("&Edit",2,1,0,"Highlight any allocation and click Edit to change any part of the entire transaction")
	fnCmdKey("&Print Proof List",4,0,0,"Prints a proof list of your entries..")
	fnCmdKey("&Back",5,0,1,"Return to main entry screen.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then edit=0: goto CLEAN_MAIN_SCREEN
	rn=val(resp$(1))
	if ckey=2 then edit=1 else edit=0 ! set edit mode
	if ckey=2 and trim$(resp$(1))="" then gosub REVIEW_TRANS : goto EO_FLEX3
	if ckey=2 then gosub PREPARE_EDIT_SCREEN : goto EO_FLEX3
	if ckey=4 then let fn_pr_proof_list
	goto SCREEN_1 ! (on ckey=1 or anything else)
! General Ledger Breakdown Grid
	fnflexinit1('PayeeGl',16,1,5,70,mat chdr$,mat cmask$,1,0,0)
! r: populate grid from PayeeGL
	if trim$(vn$)="" then goto EO_FLEX3
	restore #payeegl,key>=vn$: nokey EO_FLEX3
	do
		read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,td$ eof EO_FLEX3
		if vn$<>payeekey$ then goto EO_FLEX3
		glitem$(1)=str$(rec(payeegl))
		glitem$(2)=payeekey$
		glitem$(3)=payeegl$
		glitem$(4)=str$(percent)
		glitem$(5)=td$
		fnflexadd1(mat glitem$)
	loop
	EO_FLEX3: ! /r
return  ! /r
COPY_AR_IN: ! r:
	close #h_gl_work:
	execute "COPY "&dv$&"GLWK101.h[cno],"&"Name=[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno] -n"
	open #10: "Name="&dv$&"ARTOGL.DAT",internal,input ioerr L5670
	read #10,using L5650: cgl$(1),camt(1),cgl$(2),camt(2),cgl$(3),camt(3),cgl$(4),camt(4),cgl$(5),camt(5)
L5650: form pos 1,c 12,n 10.2,c 12,n 10.2,c 12,n 10.2,c 12,n 10.2,c 12,n 10.2
	close #10:
L5670: !
	fnstyp(92)
	goto ACGLMRGE ! /r
ACGLMRGE: fnchain("S:\acsGL\acglMrge")
SCREEN_INSERT_DISKETTE: ! r:
	close #101: ioerr ignore
	open #101: "SROW=10,SCOL=19,EROW=12,ECOL=62,BORDeR=DR,CaPTION=<Choose Input Drive",display,outIn
	pr #101: newpage
	pr f "#101,2,1,Cr 40": "Insert Input Diskette in selected drive:"
	if dv$="" then dv$="A"
	rinput fields "#101,2,42,Cu 1,UT,N": dv$
	dv$=dv$&":" ! if dv$="A" or dv$="B" then dv$=dv$&":" else goto L5750
	close #101:
	return  ! /r
PROOF_TOTALS: ! r: add and display proof totals
! r: accumulate proof totals (mat k, tc (total credits), td (total debits))
	mat k_list$=("            "): mat k=(0) : td=tc=0
	restore #h_gl_work:
	do
		read #h_gl_work,using "Form POS 1,c 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8 ,C 6,C 5,C 3,C 12": gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof SCREEN_PROOF_TOTALS
!   pr 'read an entruy from work file:'&gl$&' - '&key$ : pause
!
		for j=1 to 30
			if k_list$(j)<>"" and key$=k_list$(j) then goto L6100 ! found matching contra account
			if trim$(k_list$(j))="" then k_list$(j)=key$: goto L6100 ! found a blank contra and no previous match
		next j
		goto SCREEN_PROOF_TOTALS
L6100: !
!
		if k_list$(j)=key$ then
			if tr(6)=1 or tr(6)=4 or tr(6)=8 then
				k(j,6)+=tr(5)
				td+=tr(5)
				tc=tc-tr(5) ! total debits entered
			else if tr(6)=2 or tr(6)=7 then
				k(j,5)+=tr(5)
				tc+=tr(5)
				td=td-tr(5) ! total credits entered
			else if tr(6)=3 then
				k(j,7)+=tr(5) ! net adjustments
				if tr(5)<0 then tc+=tr(5)
				if tr(5)>0 then td+=tr(5)
			end if
		end if
	loop
! /r
SCREEN_PROOF_TOTALS: !
	fnTos(sn$="proof_totals")
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,1,"Total Debits:",mylen,right)
	fnTxt(1,mypos,15,0,right,"10",1,"This is total debits including adjustments",0 )
	resp$(1)=str$(td)
	fnLbl(2,1,"Total Credits:",mylen,right)
	fnTxt(2,mypos,15,0,right,"10",1,"This is total credits including adjustments",0 )
	resp$(2)=str$(tc)
	fnLbl(4,1,"Type of Entry:",mylen,right)
	fnTxt(4,mypos,15,0,0,"",1)
	resp$(3)=typeofentry$ ! typeofentry_selected$
	fnLbl(5,1,"Bank Account: "&bankname$,mylen,right)
	fnTxt(5,mypos,40,0,0,"",1)
	resp$(4)=fnrgl$(bankgl$)
	fnLbl(6,1,"Process Ending Date:",mylen,right)
	fnTxt(6,mypos,15,0,right,"1001",1,"Process Endings Date must should be the last day of the month or the last day of the period beding processed.",0 )
	resp$(5)=str$(contra_entry_date)
	for j=1 to 30
		read #h_glmstr,using "form pos 87,pd 6.2",key=k_list$(j),release: k(j,4) nokey L6240 ! get last balance
		k(j,8)=k(j,4)-k(j,5)-k(j,6)+k(j,7) ! new balance when posted
L6240: !
	next j
!
	fnflexinit1('Prooftotals',8,1,15,90,mat chdr_proof_total$,mat cmask3$,1,0,0)
	mat glitem3$=("")
	for j=1 to 30
		if trim$(k_list$(j))<>"" then ! skip blanks
			glitem3$(1)=k_list$(j)
			glitem3$(2)=str$(k(j,4))
			glitem3$(3)=str$(-k(j,5))
			glitem3$(4)=str$(k(j,6))
			glitem3$(5)=str$(k(j,7))
			glitem3$(6)=str$(k(j,8))
			fnflexadd1(mat glitem3$)
		end if
	next j
	fnCmdKey("Print Proof Totals",10)
	fnCmdKey("Print Proof List",4)
	fnCmdKey("&Make Corrections",1,1,0,"Allows you to make corrections to any transactions before they are posted.")
	fnCmdKey("&Cancel Without Posting",5,0,1,"Allows you to escape without posting this batch of entries.")
	fnCmdKey("&Post",2,0,0,"Will post this group of entries to the general ledger files.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if ckey=4 then let fn_pr_proof_list : goto SCREEN_PROOF_TOTALS
	if ckey=10 then let fn_pr_proof_totals : goto SCREEN_PROOF_TOTALS
	if ckey=1 then gosub REVIEW_TRANS : goto MAIN
	if ckey=2 and td<>-tc then
		mat ml$(3)
		ml$(1)="Total Debits of "&trim$(cnvrt$("pic(-----,---,---.##)",td))&" to not equal"
		ml$(2)="the total Credits of "&trim$(cnvrt$("Pic(----,---,---.##",tc))
		ml$(3)="Click OK to continue or Cancel to go back."
		fnmsgbox(mat ml$,resp$,cap$,49)
		if resp$="Cancel" then goto SCREEN_PROOF_TOTALS
	end if
	if ckey=2 then goto POSTING_OPTIONS
	goto PROOF_TOTALS ! /r
	def fn_pr_proof_totals
F_PPT_LINE: form pos 1,c 12,5*cr 19
		for j=1 to 30
			read #h_glmstr,using "form pos 87,pd 6.2",key=k_list$(j),release: k(j,4) nokey PPT_L6240 ! get last balance
			k(j,8)=k(j,4)-k(j,5)-k(j,6)+k(j,7) ! new balance when posted
PPT_L6240: !
		next j
!
		fnopenprn
		pr #255: lpad$("Total Debits:",mylen)&' '&cnvrt$("pic(-------,---,---.##)",td)
		pr #255: lpad$("Total Credits:",mylen)&' '&cnvrt$("Pic(-------,---,---.##",tc)
		pr #255: ""
		pr #255: lpad$("Type of Entry:",mylen)&' '&typeofentry$
		pr #255: lpad$("Bank Account:",mylen)&' '&fnrgl$(bankgl$)
		pr #255: lpad$("Process Ending Date:",mylen)&' '&cnvrt$("pic(zz/zz/zz)",contra_entry_date)
		pr #255: ""
		pr #255,using F_PPT_LINE: mat chdr_proof_total$
		mat glitem3$=("")
		for j=1 to 30
			if trim$(k_list$(j))<>"" then ! skip blanks
				glitem3$(1)=k_list$(j)
				glitem3$(2)=cnvrt$("Pic(-------,---,---.##)",k(j,4)) ! cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(5)="10"
				glitem3$(3)=cnvrt$("Pic(-------,---,---.##)",-k(j,5))
				glitem3$(4)=cnvrt$("Pic(-------,---,---.##)",k(j,6))
				glitem3$(5)=cnvrt$("Pic(-------,---,---.##)",k(j,7))
				glitem3$(6)=cnvrt$("Pic(-------,---,---.##)",k(j,8)) ! cmask3$(6)='10'
				pr #255,using F_PPT_LINE: mat glitem3$
			end if
		next j
		fncloseprn
	fnend
CREATE_CONTRA_ENTRIES: ! r:
	restore #h_gl_work:
	tr$="999999999999"
	for j=1 to 30
		if val(k_list$(j))<>0 then
			if k(j,5)<>0 then write #h_gl_work,using F_2A,rec=lrec(h_gl_work)+1: k_list$(j),contra_entry_date,-k(j,5),2,0,tr$,"Contra Entry","","","","" ! debits from receipts or sales
			if k(j,6)<>0 then write #h_gl_work,using F_2A,rec=lrec(h_gl_work)+1: k_list$(j),contra_entry_date,-k(j,6),1,0,tr$,"Contra Entry","","","","" ! credits from checks or purchases
		end if
	next j
	return  ! /r
PAYROLL: ! r:
	if sel=0 then sel=4 ! default to payroll
	fnTos(sn$="GLInput7")
	mylen=18: mypos=mylen+3 : right=1
	fnLbl(3,1,"Date:",mylen,right)
	fnTxt(3,mypos,8,0,right,"1",0,"Transaction date must always be answered.",0 )
	resp$(1)=str$(tr(4))
	if sel=3 then let fnLbl(4,1,"Amount:",mylen,right) else let fnLbl(4,1,"Net Amount:",mylen,right)
	fnLbl(4,36,message$,50,left)
!
	fnTxt(4,mypos,12,0,right,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	if sel=3 then resp$(2)="" else resp$(2)=str$(transactionamt)
	fnLbl(5,1,"Reference #:",mylen,right)
	fnTxt(5,mypos,12,0,0,"",0,"Enter check number.",0)
	resp$(3)=tr$
	fnLbl(6,1,"Employee #:",mylen,right)
	fncombof("PRmstr",6,mypos,35,"[Q]\GLmstr\PRmstr.h[cno]",1,4,5,30,"[Q]\GLmstr\PRINDEX.h[cno]",1,pas, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0)
	resp$(4)=str$(pr(1))
	fnLbl(7,1,"General Ledger #:",mylen,right)
	fnqgl(7,mypos,0,2,pas)
	resp$(5)=fnrgl$(gl$)
	if sel=3 then let fnLbl(7,60,"Net Adj:",mylen,right) else let fnLbl(7,60,"Amount:",mylen,right)
	if sel=3 or sel=4 then disable=1 else disable=0
	fnTxt(7,70,13,0,right,"10",disable,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
	if sel=3 then resp$(6)=str$(totalalloc) else resp$(6)=""
	fnLbl(1,4,"Type of Entry: "&typeofentry$(4:20),36,right)
	fnLbl(1,38,"Bank Account: "&bankname$,50,right)
	fnFra(8,1,10,70,"Payroll Breakdown","Enter the check breakdown.")
	fnLbl(1,1,"Total Wage:",mylen,right,0,1)
	fnTxt(1,22,12,0,right,"10",0,"Total wage before any deductions (gross).",1)
	resp$(7)=str$(pr(2))
	fnLbl(2,1,"Federal W/H:",mylen,right,0,1)
	fnTxt(2,22,12,0,right,"10",0,"Total Federal withholdings entered as a positive figure).",1)
	resp$(8)=str$(pr(3))
	fnLbl(3,1,"Fica W/H:",mylen,right,0,1)
	fnTxt(3,22,12,0,right,"10",0,"Total Fica withholdings entered as a positive figure).",1)
	resp$(9)=str$(pr(4))
	fnLbl(4,1,"State W/H:",mylen,right,0,1)
	fnTxt(4,22,12,0,right,"10",0,"Total state withholdings entered as a positive figure).",1)
	resp$(10)=str$(pr(5))
	fnLbl(5,1,"Local W/H:",mylen,right,0,1)
	fnTxt(5,22,12,0,right,"10",0,"Total local withholdings entered as a positive figure).",1)
	resp$(11)=str$(pr(6))
	for j=1 to 5
		fnLbl(j+5,1,trim$(miscname$(j))&":",mylen,right,0,1)
		fnTxt(j+5,22,12,0,right,"10",0,"Total "&trim$(miscname$(j))&" (enter as a positive figure).",1)
		resp$(j+11)=str$(pr(j+6))
	next j
	for j=6 to 10
		fnLbl(j-5,30,trim$(miscname$(j))&":",mylen,right,0,1)
		fnTxt(j-5,51,12,0,right,"10",0,"Total "&trim$(miscname$(j))&" (enter as a positive figure).",1)
		resp$(j+11)=str$(pr(j+6))
	next j
	fnLbl(6,30,"Tips:",mylen,right,0,1)
	fnTxt(6,51,12,0,right,"10",0,"Total tips entered as a positive figure).",1)
	resp$(22)=str$(pr(17))
	fnLbl(7,30,"Weeks Worked:",mylen,right,0,1)
	fnTxt(7,51,12,0,right,"30",0,"Total weeks worked during pay period.",1)
	resp$(23)=str$(pr(18))
	fnLbl(8,30,"Eic:",mylen,right,0,1)
	fnTxt(8,51,12,0,right,"10",0,"Total Earned Income Credit applied.",1)
	resp$(24)=str$(pr(19))
	if sel=1 or sel=6 then let fnButton(6,53,"E&xtract",15,"Extracts general ledger numbers from payee records",1,8)
	if disable_payee=1 and (sel=1 or sel=6) then payee_button$ ="Enable &Payee" else payee_button$ ="Disable &Payee"
	if sel=1 or sel=6 then let fnButton(6,64,payee_button$,16,"Allows you to disable or enable the payee field.",1,12)
	if sel=1 or sel=6 then let fnButton(6,79,"&Add Payee",17,"Allows you to add a payee record.",1,10)
! fnLbl(17,73,"",1,right)
! fnLbl(16,1," ")
! If EDIT=1 Then Let fnCmdKey("C&hange Acct #",9,0,0,"")
	if editmode=1 then
		fnCmdKey("&Complete",30,1,0,"Completed making corrections to this transaction.")
	else
		fnCmdKey("&Next Transaction",1,1,0,"You are completed with this transaction and ready to move to the next transaction.")
	end if
	fnCmdKey("&Review Transactions",3,0,0,"Prints a list of all transactions entered during the setting and also provides for edit options.")
	fnCmdKey("&Delete",7,0,0,"Deletes the entire transaction as shown on screen.")
	fnCmdKey("&Back",6,0,0,"Allows you to return to screen 1 and change transaction types or bank accounts.")
	fnCmdKey("&Finish",9,0,1,"")
	fnAcs(mat resp$,ckey)
	if ckey=9 then
		goto SCREEN_1
	else if ckey=6 then
		transactionamt=0
		mat pr=(0)
		tr$=""
		vn$=""
		goto SCREEN_1
	else if ckey=3 then
		gosub REVIEW_TRANS : goto MAIN
	end if
	tr(4)=val(resp$(1)) ! date
	tr(5)=transactionamt=val(resp$(2)) ! amount
	tr$=resp$(3) ! ref #
	pr(1)=val(resp$(4)(1:4)): vn$=resp$(4)(1:4) ! employee number
	td$=resp$(4)(5:30) ! transaction description = employee name
	empname$=td$
	gl$=fnagl$(resp$(5))
	for j=2 to 19
		pr(j)=val(resp$(j+5))
	next j
	wh=0
	for j=1 to 19
		if j>2 and j<7 then wh=wh+pr(j)
		if j<7 or j>16 then goto L7300
		if dedcode(j-6)=2 then wh=wh-pr(j) else wh=wh+pr(j)
L7300: if j=17 then wh=wh+pr(j)
		if j=19 then wh=wh-pr(j)
	next j
	if tr(5)=pr(2)-wh then goto WRITE_PAYROLL_TRANS
	mat ml$(3)
	ml$(1)="Total wages less deductions do not equal the net check!"
	ml$(2)=" Net entered:" &ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",transactionamt))&"   Calculated net: "&ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",pr(2)-wh))
	ml$(3)="Click ok to return to the entry screen."
	fnmsgbox(mat ml$,resp$,cap$,49)
	goto PAYROLL ! /r
WRITE_PAYROLL_TRANS: ! r:
	tr(6)=4 ! payroll transaction type  (was converted back to 1 in old system)
	for j=2 to 19
		jv$(2)=str$(j-1) ! carry breakdown code for posting employee record
		if j=2 then allocgl$=gl$: td$="Gross Pay-"&empname$(1:18)
		if j=3 then td$="Federal Withholdings"
		if j=4 then td$="FICA Withholdings"
		if j=5 then td$="State Withholdings"
		if j=6 then td$="Local Withholdings"
		if j=3 or j=4 or j=5 or j=6 then allocgl$=cnvrt$("pic(zz#)",pgl(j-2,1))&cnvrt$("pic(zzzzz#)",pgl(j-2,2))&cnvrt$("pic(zz#)",pgl(j-2,3))
		if j=19 then allocgl$=cnvrt$("pic(zz#)",pgl(5,1))&cnvrt$("pic(zzzzz#)",pgl(5,2))&cnvrt$("pic(zz#)",pgl(5,3)) ! eic
		if j>6 and j<17 then
			allocgl$=miscgl$(j-6)
			td$=miscname$(j-6) ! miscellaneous deductions
		end if
		if j>2 and j<7 then
			pr(j)=-pr(j) ! reverse sign on fica, etc
		else if j>6 and j<17 and dedcode(j-6)=1 then
			pr(j)=-pr(j)
! .     ! turn sign around on any of ten deductions coded as additions
		else if j=17 then
			pr(j)=-pr(j)
			td$="Tips"
			allocgl$=gl$ ! tips
		else if j=19 then
			pr(j)=pr(j)
			td$="Eic" ! eic as positive
		end if
		if pr(j)<>0 then
			write #h_gl_work,using F_2A,rec=lrec(h_gl_work)+1: allocgl$,tr(4),pr(j),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ ! gross wage
		end if
	next j
	transactionamt=0
	mat pr=(0)
	vn$=""
	fn_increment_tr
	goto MAIN ! /r
CHECK_FOR_CONTRAS: ! r:
	for j=1 to lrec(h_gl_work)
		read #h_gl_work,using F_2B,rec=j: gl$,tr(4),tr(5),tr(6),tr(7),tr$ noRec CFC_NEXT_J
		if trim$(tr$)="999999999999" then delete #h_gl_work, rec=j:
CFC_NEXT_J: !
	next j
	gl$="": tr(4)=tr(5)=tr(6)=tr(7)=0: tr$=""
	return  ! /r
CHECK_FOR_DELETED: ! r:
	if adr>1 then adr=adr-1: goto L400
	goto L460 ! /r
def fn_increment_tr
	x=0
	x=val(tr$) conv L4580
	if tr$<>"999999999999" then
		tr$=str$(val(tr$)+1) ! increment if possible
	else
		L4580: !
		if gl_retainFieldsDuringAdd$='False' then
			tr$=""
		end if
	end if
fnend
EditAllocations: ! r:  editing glallocation while still being entered into allocation grid
	editrecord=val(resp$(7))
	if editall=19 then editrecord=1
	EA_READ_GLALLOC: !
	read #glallocations,using "Form pos 1,c 12,pd 10.2,c 30,pd 5",rec=editrecord: gl$,allocation,td$,transadr noRec EA_FINIS
	holdallocation=allocation
	fnTos(sn$="GLInput3")
	mylen=18: mypos=mylen+3 : right=1
	fnLbl(2,1,"Amount:",mylen,right)
	fnTxt(2,mypos,13,0,right,"10",0,"Enter the amount of this breakdown.",0 )
	resp$(1)=str$(allocation)
	fnLbl(1,1,"General Ledger #:",mylen,right)
	fnqgl(1,mypos,0,2,pas)
	resp$(2)=fnrgl$(gl$)
	fnLbl(3,1,"Description:",mylen,right)
	fnTxt(3,mypos,30,0,left,"",0,"Enter description to be carried in the general ledger transaction.",0 )
	resp$(3)=td$
	fnCmdKey("&Next",1,1,0,"Apply any changes and return to main entry screen.")
	fnCmdKey("&Delete",6,0,0,"Deletes this allocation.")
	fnCmdKey("&Cancel",5,0,1,"Return to main entry screen without applying changes.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto EA_FINIS
	if ckey=6 then
		mat ml$(3)
		ml$(1)="You have chosen to delete this allocation."
		ml$(2)="Click OK to delete this entry."
		ml$(3)="Click Cancel to return to previous screen."
		fnmsgbox(mat ml$,resp$,cap$,49)
		if resp$<>"OK" then
			goto EditAllocations
		end if
		if ckey=6 then delete #glallocations,rec=editrecord:
		delete #h_gl_work,rec=transadr: ioerr ignore
		transactionamt+=-holdallocation
	else
		allocgl$=fnagl$(resp$(2))
		allocation=val(resp$(1))
		transactionamt+=allocation-holdallocation ! update net amount of transaction
		td$=resp$(3)
		rewrite #glallocations,using "Form pos 1,c 12,pd 10.2,c 30",rec=editrecord: allocgl$,allocation,td$
		totalalloc+=allocation-holdallocation !
	end if
	if editall=1 then
		editrecord=editrecord+1
		goto EA_READ_GLALLOC
	end if
EA_FINIS: !
	editall=0
return  ! /r
def fn_pr_proof_list
	fnopenprn
	gosub PROOF_LIST_HDR
	holdtr$="" : tr$=""
	restore #h_gl_work:
	do
		holdtr$=tr$
		read #h_gl_work,using F_2B: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof PE_FINIS
		if trim$(holdtr$)<>"" and holdtr$<>tr$ then
			pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount
			netamount=0
		end if
		if tr(6)=3 then prntkey$="" else printkey$=key$
		pr #255,using F_PE_LINE: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,printkey$,mat jv$ pageoflow PROOF_LIST_PGOF
		F_PE_LINE: form pos 1,c 12,x 2,pic(zz/zz/zz),n 11.2,x 4,pic(zz),pic(zz),c 13,c 30,c 10,c 12,c 7,c 7,c 41
		netamount+=tr(5)
	loop
	PE_FINIS: !
	pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount : netamount=0
	fncloseprn
fnend
PROOF_LIST_PGOF: ! r:
	pr #255: newpage
	gosub PROOF_LIST_HDR
continue  ! /r
PROOF_LIST_HDR: ! r:
	pr #255: ""
	pr #255,using 'form pos 1,c 8,pos 29,Cc 40,skip 1,pos 1,c 8,pos 40,c 40': date$,env$('cnam'),time$,"GL Input Proof List"
	pr #255: ""
	pr #255: "   Account #    Date       Amount    TC  Reference #  Payee/Description             Vendor";
	if jccode=1 then pr #255: "Job #   Cat  S-Cat" else pr #255: " "
return  ! /r
include: ertn
