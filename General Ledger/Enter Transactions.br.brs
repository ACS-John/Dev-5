! formerly S:\acsGL\GLInput
enableblankLineAfterNet=1
! enter GL transactions
! r: setup library, dims, constants, fnTop
	autoLibrary
	on error goto Ertn
	dim resp$(30)*128
	dim prx(19)
	dim ml$(0)*100

	gridRowHold$=env$('current_grid_row')

	fnTop(program$)
	gltyp=7
	fnreg_read('Enter Transactions - retain some fields between additions',gl_retainFieldsDuringAdd$,'False')
	contraEntryDateN=fncreg_read('Enter Transaction - Process Ending Date',contraEntryDate$)
	fncreg_read('Enter Transaction - Bank Account',glBank$)

	gridTransactions=1
	gridProofTotals=2
	gridTransactionsNet=3
	gridSelected=fnPcReg_read('gridSelected',gridSelected$, str$(gridTransactions))

	sx_disbursement=1
	sx_receipt=2
	sx_adjustment=3
	sx_payrollCheck=4
	sx_sale=7
	sx_purchase=8
	selx=fnPcReg_read('TransactionType',selx$,str$(sx_adjustment))

	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	dim miscname$(10)*20
	dim dedcode(10)
	dim pgl(5,3)
	dim miscgl$(10)*12
	read #1,using 'Form POS 298,15*PD 4,POS 382,N 2,POS 418,10*C 20,10*N 1,POS 668,10*C 12': mat pgl,jccode,mat miscname$,mat dedcode,mat miscgl$
	for j=1 to udim(mat miscname$) : miscname$(j)=trim$(miscname$(j)) : next j
	close #1:
! /r
fn_openFiles
fn_clearContrasAndPosted(hMerge)

ScreenOne: ! r:
	dim kList$(0)*12
	dim kReceipts(0)
	dim kDisbursements(0)
	dim kAdjustments(0)
	fn_buildMatK(hMerge,mat kList$,mat kReceipts,mat kDisbursements,mat kAdjustments,totalCredits,totalDebits,count)

	fnTos ! r:
	colPos=48 : mylen=20 : mypos=colPos+mylen+3 : rc=lc=0
	fnLbl(lc+=1,colPos,"Type Of Entry:",mylen,1)
	fnComboF("TransactionType",lc,mypos,20,'S:\Core\Data\GL TransactionType.dat',1,1,2,18,'S:\Core\Data\GL TransactionType.idx',1,0,"You must indicate the type of entry you will be entering.")

	resp$(respc_entryType=rc+=1)=str$(selx)
	fnLbl(lc+=1,colPos,"Bank Account:",mylen,1)
	fnqgl(lc,mypos,0,2)
	resp$(respc_bankGl=rc+=1)=fnrgl$(glBank$)
	fnLbl(lc+=1,colPos,"Process Ending Date:",mylen,1)
	fnTxt(lc,mypos,8,0,1,"1001",0,"Process endings date must always be answered and will be the last day of the month or the last day of the period beding processed.",0 )
	resp$(respc_contraDate=rc+=1)=contraEntryDate$
	! lc+=1
	lc=0
	colPos=1 : mylen=20 : mypos=colPos+mylen+3
	! prior input   totals here

	fnLbl(lc+=1,1,"Total Debits:",mylen,1)
	fnTxt(lc,mypos,15,0,1,"10",1,"This is total debits including adjustments",0 )
	resp$(rc+=1)=str$(totalDebits)
	fnLbl(lc+=1,1,"Total Credits:",mylen,1)
	fnTxt(lc,mypos,15,0,1,"10",1,"This is total credits including adjustments",0 )
	resp$(rc+=1)=str$(totalCredits)
	fnLbl(lc+=1,1,"Entry Count:",mylen,1)
	fnTxt(lc,mypos,15,0,1,"number",1,"",0 )
	resp$(rc+=1)=str$(count)
	respc_grid=rc+=1
	lc+=1
	lc+=1
	fnLbl(lc+=1,1,"Display:",8,1)
	fnButtonOrDisabled(gridSelected<>gridTransactionsNet	,lc,10,'Transactions'	,ck_gridTransNet 	=57,'Allocations and Net',13)
	fnButtonOrDisabled(gridSelected<>gridTransactions   	,lc,25,'Allocations' 	,ck_gridTrans    	=55,'Allocations Only',13)
	fnButtonOrDisabled(gridSelected<>gridProofTotals    	,lc,40,'Proof Totals'	,ck_gridTotals   	=56,'Totals by Bank GL with Balances before and after posting',13)
	! fnLbl(lc+=1,1,"Previous Input:",mylen,1)


	fnButton(lc,65,"Clear All",ck_clearAll=52,"Erase entire batch of previously entered transactions",1,10)
	defaultEdit=defaultAdd=0
	if count=0 then defaultAdd=1 else defaultEdit=1
	fnButton(lc-2,84,'Add',ck_Add=30,"(Alt+A)  Add a new transaction",1,8,0,0,defaultAdd,0)
	if gridSelected<>gridProofTotals and count then
		fnButton(lc,84,"Edit",ck_Edit=63,"Modify selected transaction",1,8,0,0,defaultEdit,0)
		fnButton(lc,93,"Delete",ck_deleteOne=67,"Delete selected transaction",1,8)
	end if
	fn_addOneGrid
	fnCmdKey("Print Proof Totals",ck_printProofTotals=58)
	fnCmdKey("Print Proof List",ck_printProofList=54)
	fnCmdKey("Import Client File",ck_importFile=51,0,0,"Import a Client's ACS Checkbook file.")
	fnCmdKey("&Post",ck_post=62,0,0,"Will post this group of entries to the general ledger.")
	fnCmdKey("E&xit",ck_exit=5,0,1,"Exit without Posting - you can always come back and finish later")
	! /r
	ckey=fnAcs(mat resp$) ! ScreenOne
	if ckey=ck_exit then goto Xit

	selx=val(resp$(respc_entryType)(1:1))
	fnPcReg_write('TransactionType',str$(selx))
	! If selx=sx_payrollCheck Then Goto ScreenOne ! temporary line do not access payroll checks
	typeofentry$=resp$(respc_entryType)
	key$=glBank$=fnagl$(resp$(respc_bankGl)) ! GL account
	contraEntryDate$=resp$(respc_contraDate)
	contraEntryDateN=val(contraEntryDate$)
	if gridSelected<>gridProofTotals then
		recordNumber=val(resp$(respc_grid))
		previouslySelected=recordNumber
	end if
	if (selx=sx_disbursement or selx=sx_receipt) and val(glBank$)=0 then goto S1BankGlFail ! must have GL bank account on receipts or disbursements
	dim bankAcctName$*40
	bankAcctName$=''
	if (selx=sx_disbursement or selx=sx_receipt or selx=sx_payrollCheck) then
		read #hAccount,using "form pos 13,c 40",key=glBank$,release: bankAcctName$ nokey S1BankGlFail
	else
		read #hAccount,using "form pos 13,c 40",key=glBank$,release: bankAcctName$ nokey ignore
	end if
	fncreg_write('Enter Transaction - Process Ending Date',contraEntryDate$)
	fncreg_write('Enter Transaction - Bank Account',glBank$)
	if ckey=ck_edit then
		! pr 'before fn_transactionEdit(';recordNumber;') call' : pause
		fn_transactionEdit(hMerge,recordNumber,glBank$)
	else if ckey=ck_gridTotals then
		gridSelected=gridProofTotals
		fnPcReg_write('gridSelected',str$(gridSelected))
		previouslySelected=0
	else if ckey=ck_gridTransNet then
		gridSelected=gridTransactionsNet
		fnPcReg_write('gridSelected',str$(gridSelected))
		previouslySelected=0
	else if ckey=ck_gridTrans then
		gridSelected=gridTransactions
		fnPcReg_write('gridSelected',str$(gridSelected))
		previouslySelected=0
	else if ckey=ck_importFile then
		fn_inputClientCheckbookFile
	else if ckey=ck_clearAll then
		if fnConfirmDeleteHard('(entire batch of transactions)','clearAllEnteredTrans') then
			fn_eraseAllPreviousInput
		end if
	else if ckey=ck_deleteOne then
		if recordNumber then
			fn_transactionDelete(hMerge,recordNumber)
		end if
	else if ckey=ck_printProofList then
		fn_prProofList(hMerge)
	else if ckey=ck_printProofTotals then
		fn_prProofTotals(hAccount,totalDebits,totalCredits,glBank$,contraEntryDateN,mat kList$,mat kReceipts,mat kDisbursements,mat kAdjustments,mat chdr_proof_total$,mat glitem3$)
	else if ckey=ck_Add then
		previouslySelected=fn_transactionAdd(hMerge,selx,glBank$,contraEntryDateN)
	else if ckey=ck_post then
		if totalDebits<>-totalCredits then
			mat ml$(3)
			ml$(1)="Total Debits of "&trim$(cnvrt$("pic(-----,---,---.##)",totalDebits))&" to not equal"
			ml$(2)="the total Credits of "&trim$(cnvrt$("Pic(----,---,---.##",totalCredits))
			ml$(3)="Click OK to continue or Cancel to go back."
			fnmsgbox(mat ml$,resp$,'',49)
			if resp$="Cancel" then goto ScreenOne
		end if
		goto ScrPost
	end if
goto ScreenOne ! /r
	S1BankGlFail: ! r:
		mat ml$(2)
		ml$(1)="You must have a Bank Account General Ledger"
		ml$(2)="Number for disbursements or receipts."
		fnmsgbox(mat ml$,resp$,'',49)
	goto ScreenOne ! /r
ScrPost: ! r:
	fnTos
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
	ckey=fnAcs(mat resp$)
	fnfscode(0)
	fn_createContras(hMerge,mat kList$,mat kReceipts,mat kDisbursements,contraEntryDateN)
	if resp$(5)="True" or ckey=5 then goto Xit ! return w/o posting
	if resp$(2)="True" then fnprocess(1) else fnprocess(0)
	if resp$(4)="True" then fnprocess(4) ! post both
	if resp$(1)="True" then goto DoMerge
	if resp$(3)="True" then goto DoPrMerge

	open #h_process=fnH: "Name=[Q]\GLmstr\Process.h[cno],RecL=128,Use",internal,outIn,relative
	if lrec(h_process)=0 then write #h_process,using "form pos 1,n 1": 0
	if resp$(2)="True" then  ! code for post payroll and gl both
		rewrite #h_process,using "form pos 1,n 1",rec=1: 1
	else
		rewrite #h_process,using "form pos 1,n 1",rec=1: 0
	end if
	if resp$(4)="True" then  ! code for posting pr and gl both
		rewrite #h_process,using "form pos 1,n 1",rec=1: 4
	end if
	close #h_process:

	if resp$(4)="True" then goto DoMerge ! post both
	if resp$(2)="True" then goto DoAutoProc
goto Xit ! /r
def fn_scrMain(hMerge; editMode,heading$*64,glBank$*12,transDate,bankAcctName$*40,message$*50,tr$, _
	___,rc,lc,mylen,mypos,x$*8,ckeySave) ! mostly local stuff still
	if ~sm_setup then ! r: ck_* enums
		sm_setup=1
		! ckey enums for both main add/edit screen and payroll add screen (ScrPayroll)
		ck_extract=15
		ck_allocationAdd=66
		ck_payeeTogle=16
		ck_payeeAdd=17
		ck_breakdownEdit=18
		ck_breakdownEditAll=19
		ck_save1=30
		ck_saveA=68
		ck_cancel=6

	end if ! /r
	ScrMainTop: !


	fnTos ! r: draw Main add/edit screen
	mylen=18 : mypos=mylen+3
	fnLbl(1,20,'______________________________ '&heading$&' ______________________________')
	fnLbl(2,1,"Bank Account:",mylen,1)
	fnLbl(2,mypos,bankAcctName$)
	fnLbl(3,1,"Date:",mylen,1)
	fnTxt(3,mypos,8,0,1,"1",0,"Transaction date must always be answered.",0 )
	resp$(1)=str$(tr4)
	if selx=sx_adjustment then
		fnLbl(4,1,"Amount:",mylen,1)
	else
		fnLbl(4,1,"Net Amount:",mylen,1)
	end if
	dim message$*100
	fnLbl(4,36,message$,50,left)

	fnTxt(4,mypos,13,0,1,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	if selx=sx_adjustment then resp$(2)='' else resp$(2)=str$(transactionAmt)
	fnLbl(5,1,"Reference Number:",mylen,1)
	fnTxt(5,mypos,12,0,0,'',0,"Enter check number, receipt number or adjusting entry number",0)
	resp$(3)=tr$

	if selx=sx_receipt or selx=sx_adjustment or selx=sx_sale then
		fnLbl(6,1,"Description:",mylen,1) ! for receipts
		fnTxt(6,mypos,30,0,left,'',0,"Brief description of transaction.",0 )
		resp$(respc_desc=4)=td$
	else if selx=sx_payrollCheck then
		fnLbl(lc+=1,1,"Employee:",mylen,1)
		fncombof("PRmstr",lc,mypos,35,"[Q]\GLmstr\PRmstr.h[cno]",1,4,5,30,"[Q]\GLmstr\PRINDEX.h[cno]",1,0, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0)
		resp$(respc_payee=4)=vn$
	else
		fnLbl(6,1,"Payee:",mylen,1)
		if disable_payee=1 then
			fnTxt(6,mypos,8,0,1,'',1,"Payee field disabled. Click 'Enable Payee' again to enable.",0 )
			resp$(respc_payee=4)=''
		else
			fncombof("payee",6,mypos,35,'[Q]\GLmstr\PayMstr.h[cno]',1,8,9,30,'[Q]\GLmstr\PayIdx1.h[cno]',0,0, 'If the payee is known, the general ledger information can be extracted from that payee record.',0)
			resp$(respc_payee=4)=vn$
		end if
	end if

	if selx=sx_disbursement or selx=sx_sale then
		fnButtonOrDisabled(~editMode,6,63,'Extract',ck_extract,'Extracts general ledger numbers from payee records')
		if disable_payee=1 then payee_button$="Enable Payee" else payee_button$="Disable Payee"
		fnButton(6,73,payee_button$,ck_payeeTogle=16,"Allows you to disable or enable the payee field.",1,12)
		fnButton(6,87,"Payee File",ck_payeeAdd=17,"Allows you to add a payee record.",1,10)
	end if

	lc=8
	if editMode then
		lc+=1
		fnTxt(lc,mypos,20,0,1,"",1,"Disabled General Ledger selection")
		resp$(respc_AllocGl=5)=''
		! fnLbl(lc,45,"Amount:",mylen,1)
		fnTxt(lc,50,13,0,1,"10",1,"Disabled Allocatin Amount",0 )
		resp$(respc_AllocAmt=6)=''
		lc-=2
	else
		fnLbl(lc+=1,20,"________________________________________ Transaction Allocations ________________________________________")
		lc+=1
		mylen=34 : mypos=mylen+3
		fnLbl(lc+=1,1,"General Ledger:",mylen,1)
		fnLbl(lc,mylen+mypos+5,"If only one allocation necessary you may enter only",mylen,1)
		fnLbl(lc+1,mylen+mypos+5,"the GL Account and Save.",mylen,1)
		fnqgl(lc,mypos,0,2)
		resp$(respc_AllocGl=5)=fnrgl$(gl$)
		if selx=sx_adjustment then
			fnLbl(lc+=1,1,"Net Adjustment:",mylen,1)
		else
			fnLbl(lc+=1,1,"Amount:",mylen,1)
		end if
		fnTxt(lc,mypos,13,0,1,"10",0,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
		resp$(respc_AllocAmt=6)='' : if selx=sx_adjustment then resp$(respc_AllocAmt)=str$(totalalloc)
		fnButton(lc,59,"Add Allocation",ck_allocationAdd,'Enter the GL Account and Amount then click this button to add a new allocation to the breakdown.\nThis option is not available during Edits, only Adds.')
	end if

	fnButton(lc+=2,85,"&Edit",ck_breakdownEdit,"Correct an allocation.",1,5)
	fnButton(lc,92,"Edit All",ck_breakdownEditAll,"Edit all allocations without returning to this screen.",1,8)
	! r: General Ledger Transaction Breakdown Grid
	mat chdr$(4)
	chdr$(1)='Reference'
	chdr$(2)='GL Account'
	chdr$(3)='Amount'
	chdr$(4)='Description'
	mat cmask$(4)
	mat cmask$=('')
	cmask$(3)='10'
	fnflexinit1('Glalloc',lc+=1,1,6,60,mat chdr$,mat cmask$,1,0,0)
	restore #hMtemp:
	dim allocItem$(4)*128
	mat allocItem$(4)
	do
		read #hMtemp,using 'Form Pos 1,c 12,pd 10.2,c 30': allocgl$,allocAmt,td$ eof EO_FLEX1
		allocItem$(1)=str$(rec(hMtemp))
		allocItem$(2)=fnrgl$(allocgl$)
		allocItem$(3)=str$(allocAmt)
		allocItem$(4)=td$
		fnflexadd1(mat allocItem$)
	loop
	EO_FLEX1: ! /r
	fnCmdKey("Save",ck_save1,editMode,0,"Completed making corrections to this transaction.") ! formerly complete
	if ~editMode then
		fnCmdKey("Save and Add Another",ck_saveA,~editMode,0,"Save this Transaction and go directly to adding another of the same type")
	end if
	fnCmdKey("Cancel",ck_cancel,0,1,'')
	! /r
	ckey=fnAcs(mat resp$)
	allocAmt=0
	message$=''

	if ckey=ck_cancel then ! Back (Return to first screen to change transaction types or bank accounts.)
		transactionAmt=0
		tr$=''
		goto SmFinis
	else
		tr4=val(resp$(1))                  	! date
		transactionAmt=val(resp$(2))      	! amount
		tr$=resp$(3)                     	  	! ref #

		if selx=sx_receipt or selx=sx_adjustment or selx=sx_sale then
			td$=resp$(respc_desc)            	! description
		else if selx=sx_payrollCheck then
			vn$=resp$(respc_payee)(1:4)      	! employee
			td$=resp$(respc_payee)(6:inf)    	! description
		else
			vn$=resp$(respc_payee)(1:8)      	! payee
			td$=resp$(respc_payee)(10:inf)  	! description
		end if
		allocgl$=fnagl$(resp$(respc_AllocGl))
		allocAmt=val(resp$(respc_AllocAmt))
		if allocAmt=0 and trim$(allocgl$)<>'' and lrec(hMtemp)=0 then allocAmt=transactionAmt
		! r: Get TD$ from Payee
			! requires vn$
			open #hPaymstr=fnH: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
			read #hPaymstr, using "form pos 1,c 8",key=lpad$(rtrm$(vn$),8),release: x$ nokey GtdNokey
			td$=resp$(4)(9:38) ! transaction description = vendor name when vendor entered
			goto GtdpFinis
			GtdNokey: !
				td$=resp$(4)(1:30)
			goto GtdpFinis ! use full response as description if not a payee name
			GtdpFinis: !
			close #hPaymstr: ioerr ignore
		! /r

		if ckey=ck_save1 or ckey=ck_saveA then ckeySave=1 else ckeySave=0

	end if

	if ckey=ck_allocationAdd or (ckeySave and trim$(allocgl$)<>'') and ( allocAmt<>0 or (transactionAmt<>0 and lrec(hMtemp)=0) ) then
		if allocAmt=0 and lrec(hMtemp)=0 then allocAmt=transactionAmt
		if allocAmt<>0 and trim$(allocgl$)<>'' then
			write #hMtemp,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocAmt,td$
			totalalloc+=allocAmt
		else
			pr bell;
		end if
		allocgl$=''
		allocAmt=0
		goto ScrMainTop
	end if

	if ckey=ck_payeeTogle then
		if disable_payee=0 then disable_payee=1 else disable_payee=0
		goto ScrMainTop
	else if ckey=ck_extract and ~editMode then
		fn_extract(hMtemp,vn$,transactionAmt)
		goto ScrMainTop
	end if


	! ! r: GL Account Test
	! if ckey=ck_breakdownEdit then goto SmGlNoTestFinis ! don't require gl # when editing transaction
	! if ckey=ck_save1 and editMode then goto SmGlNoTestFinis ! don't require gl # when editing and take complete
	! if selx=sx_adjustment and (allocAmt=0 or ckey=ck_save1) then goto SmGlNoTestFinis ! don't require gl # on adjustments when amount=0 (actually changing from one adjustment to the next)
	! if selx=sx_payrollCheck and editMode then goto SmGlNoTestFinis ! never should have an amount on payroll check. (done from another screen and edits all made from allocation screen
	! if ckey=ck_payeeAdd then goto SmGlNoTestFinis
	! x=val(allocgl$) conv SmGlTestFail
	! if val(allocgl$)=0 and ~editMode then
	! 	! KJ 080608   DON'T CHECK FOR GENERAL NUMBER ON EDIT
	! 	SmGlTestFail: !
	! 	mat ml$(3)
	! 	ml$(1)="You must have a General Ledger Number"
	! 	ml$(2)="on each allocation."
	! 	ml$(3)="Click OK to enter the general ledger number."
	! 	fnmsgbox(mat ml$,resp$,'',49)
	! 	goto ScrMainTop
	! end if
	! goto SmGlNoTestFinis
	! SmGlNoTestFinis: !
	! ! /r

	tType=selx

	if ckey=ck_payeeAdd then
		fnaddglpayee
		goto ScrMainTop
	else if ckey=ck_breakdownEdit then
		fn_editAllocation(val(resp$(7)))
		goto ScrMainTop
	else if ckey=ck_breakdownEditAll then
		fn_editAllocation(val(resp$(7)), 1)
		goto ScrMainTop
	end if

	if ckeySave then ! ckey=ck_save1 or ckey=ck_saveA

		if selx=sx_adjustment then allocAmt=transactionAmt ! create an allocation amount automatically on adjustments
		if allocAmt=0 and lrec(hMtemp)=0 then allocAmt=transactionAmt ! allows them to press enter if only allocation without haveing to key the amount a second time
		if selx<>sx_adjustment and transactionAmt<>totalalloc then
			message$="Allocations don't add up!"
			pr bell;
			goto ScrMainTop
		end if
		fn_mTempToMerge(hMtemp,hMerge,tr4,selx,tr7,tr$,vn$,jv2$,glBank$)
		fn_clearVar(hMtemp,transactionAmt,td$,tr$,vn$,gl$,totalalloc,gl_retainFieldsDuringAdd$)
		goto SmFinis
	else
		goto ScrMainTop
	end if

	SmFinis: !
	fn_scrMain=ckey
fnend
def fn_scrPayrollAdd(; ___,lendeditRecordc,lc)
	! selx=sx_payrollCheck ! this section is only for Payroll type selx is 4.
	fnPcReg_read('Payroll Check GL Account',gl$, gl$)
	ScrPayrollTos: !
	fnTos
	mylen=18 : mypos=mylen+3

	fnLbl(lc+=1,1,'______________________________ '&heading$&' ______________________________')
	! fnLbl(1,38,"Bank Account: "&bankAcctName$,50,1)
	fnLbl(lc+=1,1,"Bank Account:",mylen,1)
	fnLbl(lc,mypos,bankAcctName$)

	fnLbl(lc+=1,1,"Date:",mylen,1)
	fnTxt(lc,mypos,8,0,1,"1001",0,"Transaction date is required.",0 )
	resp$(1)=str$(tr4)
	fnLbl(lc+=1,1,"Net Amount:",mylen,1)
	fnLbl(lc,36,message$,50,left)
	fnTxt(lc,mypos,12,0,1,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	resp$(2)=str$(transactionAmt) ! if selx=sx_adjustment then resp$(2)='' else resp$(2)=str$(transactionAmt)
	fnLbl(lc+=1,1,"Reference:",mylen,1)
	fnTxt(lc,mypos,12,0,0,'',0,"Enter check number.",0)
	resp$(3)=tr$
	fnLbl(lc+=1,1,"Employee:",mylen,1)
	fncombof("PRmstr",lc,mypos,35,"[Q]\GLmstr\PRmstr.h[cno]",1,4,5,30,"[Q]\GLmstr\PRINDEX.h[cno]",1,0, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0)
	if prx(1) then resp$(4)=str$(prx(1)) else resp$(4)=''
	lc+=1
	fnLbl(lc+=1,1,"General Ledger:",mylen,1)
	fnqgl(lc,mypos,0,2)
	resp$(5)=fnrgl$(gl$)
	fnTxt(lc+2,50,13,0,1,"10",1,"Amount to allocated to this general ledger number.",0 )
	resp$(6)='' ! disabled and hidden behind frame
	lc+=1
	fnFra(lc+=1,1,10,70,"Payroll Breakdown","Enter the check breakdown.")
	fnLbl(1,1,"Total Wage:",mylen,1,0,1)
	fnTxt(1,22,12,0,1,"10",0,"Total wage before any deductions (gross).",1)
	resp$(7)=str$(prx(2))
	fnLbl(2,1,"Federal W/H:",mylen,1,0,1)
	fnTxt(2,22,12,0,1,"10",0,"Total Federal withholdings entered as a positive figure).",1)
	resp$(8)=str$(prx(3))
	fnLbl(3,1,"Fica W/H:",mylen,1,0,1)
	fnTxt(3,22,12,0,1,"10",0,"Total Fica withholdings entered as a positive figure).",1)
	resp$(9)=str$(prx(4))
	fnLbl(4,1,"State W/H:",mylen,1,0,1)
	fnTxt(4,22,12,0,1,"10",0,"Total state withholdings entered as a positive figure).",1)
	resp$(10)=str$(prx(5))
	fnLbl(5,1,"Local W/H:",mylen,1,0,1)
	fnTxt(5,22,12,0,1,"10",0,"Total local withholdings entered as a positive figure).",1)
	resp$(11)=str$(prx(6))
	for j=1 to 5
		fnLbl(j+5,1,miscname$(j)&":",mylen,1,0,1)
		fnTxt(j+5,22,12,0,1,"10",miscname$(j)=='',"Total "&miscname$(j)&" (enter as a positive figure).",1)
		resp$(j+11)=str$(prx(j+6))
	next j
	for j=6 to 10
		if miscname$(j)<>'' then
			fnLbl(j-5,30,miscname$(j)&":",mylen,1,0,1)
		end if
		fnTxt(j-5,51,12,0,1,"10",miscname$(j)=='',"Total "&miscname$(j)&" (enter as a positive figure).",1)
		resp$(j+11)=str$(prx(j+6))
	next j
	fnLbl(6,30,"Tips:",mylen,1,0,1)
	fnTxt(6,51,12,0,1,"10",0,"Total tips entered as a positive figure).",1)
	resp$(22)=str$(prx(17))
	fnLbl(7,30,"Weeks Worked:",mylen,1,0,1)
	fnTxt(7,51,12,0,1,"30",0,"Total weeks worked during pay period.",1)
	resp$(23)=str$(prx(18))
	fnLbl(8,30,"EIC:",mylen,1,0,1)
	fnTxt(8,51,12,0,1,"10",0,"Total Earned Income Credit applied.",1)
	resp$(24)=str$(prx(19))
	fnCmdKey("&Save",ck_save1,0,0,"Completed making corrections to this transaction.")
	fnCmdKey("Save and Add Another",ck_saveA,1,0,"Save this Transaction and go directly to adding another of the same type")
	fnCmdKey("&Back",ck_cancel,0,0,"Allows you to return to screen 1 and change transaction types or bank accounts.")
	! fnCmdKey("&Finish",ck_finish,0,1,'')
	ckey=fnAcs(mat resp$)
	if ckey=ck_cancel then
		transactionAmt=0
		mat prx=(0)
		tr$=''
		vn$=''
		goto ScrPayrollXit
	else
		tr4=val(resp$(1)) ! date
		tr5=transactionAmt=val(resp$(2)) ! amount
		tr$=resp$(3) ! ref #
		prx(1)=val(resp$(4)(1:4)): vn$=resp$(4)(1:4) ! employee
		td$=resp$(4)(5:30) ! transaction description = employee name
		dim empName$*30
		empName$=td$
		gl$=fnagl$(resp$(5))
		fnPcReg_write('Payroll Check GL Account',gl$)
		for j=2 to 19
			prx(j)=val(resp$(j+5))
		next j
		wh=0
		for j=1 to 19
			if j>2 and j<7 then
				wh+=prx(j)
			else if j=>7 and j<=16 then
				if dedcode(j-6)=2 then wh-=prx(j) else wh+=prx(j)
			else if j=17 then
				wh+=prx(j)
			else if j=19 then
				wh-=prx(j)
			end if
		next j
		if tr5<>prx(2)-wh then
			mat ml$(3)
			ml$(1)="Total wages less deductions do not equal the net check!"
			ml$(2)=" Net Entered:" &ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",transactionAmt))
			ml$(2)=" Calculated Net: "&ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",prx(2)-wh))
			ml$(3)="Click Ok to correct."
			fnmsgbox(mat ml$,resp$,'',49)
			goto ScrPayrollTos
		end if
		fn_payrollSave
	end if
	ScrPayrollXit: !
	fn_scrPayrollAdd=ckey
fnend
def fn_scrAdjustment	(hMerge,editRecord,bankAcctName$*40, _
	message$*50, _
	tr$,gl$,tDate; _
	gl$,tDate,tAmt,tType,postCode,tr$,desc$*30,vn$,jv2$, _
	gl2$,tDate2,tAmt2,tType2,postCode2,tr2$,desc2$*30,vn2$,jv22$, _
	foundMatch,transAdrFrom,transAdrTo, _
	___, _
	mylen,mypos,transactionAmt, _
	lc,rc,resp_tDate,resp_amt,resp_ref,respc_payee,respc_glFrom,respc_glTo _
	)

	if editRecord then ! r: get transAdrFrom and transAdrTo
		read #hMerge,using F_merge,rec=editRecord  : gl$,tDate,tAmt,tType,postCode,tr$,desc$,vn$,jv2$
		read #hMerge,using F_merge,rec=editRecord+1: gl2$,tDate2,tAmt2,tType2,postCode2,tr2$,desc2$,vn2$,jv22$ norec TryPrior

		if tDate=tDate2 and abs(tAmt)=abs(tAmt2) and tType2=sx_adjustment and tr$=tr2$ then
			foundMatch=1
			if tAmt2<tAmt then
				transAdrFrom=editRecord+1
				transAdrTo  =editRecord
				glFrom$=gl2$
				glTo$  =gl$
			else
				transAdrFrom=editRecord
				transAdrTo  =editRecord+1
				glFrom$=gl$
				glTo$  =gl2$
			end if
		end if

		TryPrior: !
		if ~foundMatch then
			read #hMerge,using F_merge,rec=editRecord-1: gl2$,tDate2,tAmt2,tType2,postCode2,tr2$,desc2$,vn2$,jv22$,key2$
			if tDate=tDate2 and abs(tAmt)=abs(tAmt2) and tType2=sx_adjustment and tr$=tr2$ then
				foundMatch=1
				if tAmt2<tAmt then
					transAdrFrom=editRecord
					transAdrTo  =editRecord-1
					glFrom$=gl$
					glTo$  =gl2$
				else
					transAdrFrom=editRecord-1
					transAdrTo  =editRecord
					glFrom$=gl2$
					glTo$  =gl$
				end if
			end if
		end if
		if foundMatch then
			transactionAmt=abs(tAmt)
		else ! ~foundMatch
			pr ' could not find matching adjustment record'
			pause
		end if
		! pr 'transAdrFrom=';transAdrFrom
		! pr 'transAdrTo  =';transAdrTo
		! pause
	end if ! /r

	fnTos
	mylen=18 : mypos=mylen+3
	transAdrFrom
	fnLbl(lc+=1,1,'______________________________ '&heading$&' ______________________________')
	! fnLbl(1,38,"Bank Account: "&bankAcctName$,50,1)
	fnLbl(lc+=1,1,"Bank Account:",mylen,1)
	fnLbl(lc,mypos,bankAcctName$)

	fnLbl(lc+=1,1,"Date:",mylen,1)
	fnTxt(lc,mypos,8,0,1,"1001",0,"Transaction date is required.",0 )
	resp$(resp_tDate=rc+=1)=str$(tDate)
	fnLbl(lc+=1,1,"Net Amount:",mylen,1)
	fnLbl(lc,36,message$,50,left)
	fnTxt(lc,mypos,12,0,1,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	resp$(resp_amt=rc+=1)=str$(transactionAmt) ! if selx=sx_adjustment then resp$(2)='' else resp$(2)=str$(transactionAmt)
	fnLbl(lc+=1,1,"Reference:",mylen,1)
	fnTxt(lc,mypos,12,0,0,'',0,"Enter check number.",0)
	resp$(resp_ref=rc+=1)=tr$
	fnLbl(lc+=1,1,"Description:",mylen,1) ! for receipts
	fnTxt(lc,mypos,30,0,left,'',0,"Brief description of transaction.",0 )
	resp$(resp_desc=rc+=1)=td$

	lc+=1
	fnLbl(lc+=1,1,"General Ledger From:",mylen,1)
	fnqgl(lc,mypos,0,2)
	resp$(resp_glFrom=5)=fnrgl$(glFrom$)
	fnLbl(lc+=1,1,"General Ledger To:",mylen,1)
	fnqgl(lc,mypos,0,2)
	resp$(resp_glTo=6)=fnrgl$(glTo$)

	fnCmdKey("Save",ck_save1,~editRecord,0,"Completed making corrections to this transaction.")
	if editRecord then
		fnCmdKey("Save and Add Another",ck_saveA,1,0,"Save this Transaction and go directly to adding another of the same type")
	end if
	fnCmdKey("Cancel",ck_cancel,0,0,"Allows you to return to screen 1 and change transaction types or bank accounts.")
	! fnCmdKey("&Finish",ck_finish,0,1,'')
	ckey=fnAcs(mat resp$)
	if ckey=ck_cancel then
		transactionAmt=0
		mat prx=(0)
		tr$=''
		goto ScrAdjustmentXit
	else
		tDate=val(resp$(resp_tDate)) ! date
		transactionAmt=val(resp$(resp_amt)) ! amount
		tr$=resp$(resp_ref) ! ref #
		td$=resp$(resp_desc) ! transaction description

		glFrom$=fnagl$(resp$(resp_glFrom))
		glTo$=fnagl$(resp$(resp_glTo))

		! fn_payrollSave
		pr ' write two adjustment records here'
		pause

		fn_transactionSave(hMerge,transAdrFrom,glFrom$,tDate,-transactionAmt,sx_adjustment,0,tr$,td$,'','','')
		fn_transactionSave(hMerge,transAdrTo  ,glTo$  ,tDate, transactionAmt,sx_adjustment,0,tr$,td$,'','','')

	end if

	ScrAdjustmentXit: !

	fn_scrAdjustment=ckey
fnend
def fn_editAllocation(editrecord; editall,___,ckey,mylen,mypos)
	! editing glallocation while still being entered into allocation grid
	! editrecord=val(resp$(7))
	! if editall=19 then editrecord=1
	do
		read #hMtemp,using "Form pos 1,c 12,pd 10.2,c 30,pd 5",rec=editrecord: gl$,allocation,td$,transAdr noRec EA_FINIS
		holdallocation=allocation
		TosEditAllocation: !
		fnTos
		mylen=18: mypos=mylen+3
		fnLbl(2,1,"Amount:",mylen,1)
		fnTxt(2,mypos,13,0,1,"10",0,"Enter the amount of this breakdown.",0 )
		resp$(1)=str$(allocation)
		fnLbl(1,1,"General Ledger #:",mylen,1)
		fnqgl(1,mypos,0,2)
		resp$(2)=fnrgl$(gl$)
		fnLbl(3,1,"Description:",mylen,1)
		fnTxt(3,mypos,30,0,left,'',0,"Enter description to be carried in the general ledger transaction.",0 )
		resp$(3)=td$
		fnCmdKey("&Next",1,1,0,"Apply any changes and return to main entry screen.")
		fnCmdKey("&Delete",6,0,0,"Deletes this allocation.")
		fnCmdKey("&Cancel",5,0,1,"Return to main entry screen without applying changes.")
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto EA_FINIS
		if ckey=6 then
			mat ml$(3)
			ml$(1)="You have chosen to delete this allocation."
			ml$(2)="Click OK to delete this entry."
			ml$(3)="Click Cancel to return to previous screen."
			fnmsgbox(mat ml$,resp$,'',49)
			if resp$<>"OK" then
				goto TosEditAllocation
			end if
			if ckey=6 then delete #hMtemp,rec=editrecord:
			delete #hMerge,rec=transAdr: ioerr ignore
			transactionAmt+=-holdallocation
		else
			allocgl$=fnagl$(resp$(2))
			allocation=val(resp$(1))
			transactionAmt+=allocation-holdallocation ! update net amount of transaction
			td$=resp$(3)
			rewrite #hMtemp,using "Form pos 1,c 12,pd 10.2,c 30",rec=editrecord: allocgl$,allocation,td$
			totalalloc+=allocation-holdallocation
		end if
		editrecord+=1
	loop while editall=1
	EA_FINIS: !
	! editall=0
	fn_editAllocation=ckey
fnend

def fn_addOneGrid
	if gridSelected=gridTransactions then
		fn_transactionGrid(hMerge,lc+=1,2,10,90,previouslySelected)
	else if gridSelected=gridTransactionsNet then
		fn_transactionGrid(hMerge,lc+=1,2,10,90,previouslySelected, 1)
	else if gridSelected=gridProofTotals then
		fn_totalGrid(hAccount,mat kList$,mat kReceipts,mat kDisbursements,mat kAdjustments,lc+=1,2)
	end if
fnend
def fn_transactionGrid(hMerge,row,col,twenty,ninety,previouslySelected; _
	enableNet,___,net,tDate,tAmt,tType,desc$*30,colCount,postCode,lineCount, _
	gl$*12,tDate,tAmt,tType,postCode,tr$,desc$*30,vn$,jv2$,key$*12)
	! r: setup
	colCount=10
	dim cmask2$(0)
	mat cmask2$(colCount)
	mat cmask2$=('')

	dim chdr2$(0)*25
	mat chdr2$(colCount)
	chdr2$(1)='rec'                      : cmask2$(1)='30'
	chdr2$(2)='Date'
	chdr2$(3)='Reference'      :     if ~enableNet then cmask2$(2)='3'
	chdr2$(4)='Payee/Description'
	chdr2$(5)='GL Account'
	chdr2$(6)='Amount'                   :  cmask2$(6)='10'
	chdr2$(7)='Allocation Description'
	chdr2$(8)='Type'
	chdr2$(9)='PC'
	chdr2$(10)='Bank'
	dim glitem2$(0)*128
	mat glitem2$(colCount)
	! /r
	fnflexinit1('GlTrans',row,col,twenty,ninety,mat chdr2$,mat cmask2$,1,0,0)
	restore #hMerge:
	do
		if fn_readMerge(0,gl$,tDate,tAmt,tType,postCode,tr$,desc$,vn$,jv2$,key$)=-4270 then goto TgEoF
		if enableNet then
			if trim$(tr$)<>'' and tr$<>oldtr$ and lineCount then
				mat glitem2$=('')
				glitem2$(6)=str$(net)
				glitem2$(7)='Net'
				fnflexadd1(mat glitem2$)

				if enableblankLineAfterNet then
					mat glitem2$=('')
					fnflexadd1(mat glitem2$)
				end if

				net=0 ! add net subtotals any time reference number changes     ( AND NET<>0)was in there
			end if
		end if

		glitem2$(1)=str$(rec(hMerge))
		if enableNet then
			glitem2$(2)=date$(days(tDate,'mmddyy'),'ccyy/mm/dd')
		else
			glitem2$(2)=str$(tDate) ! str$(date(days(tr4,'mmddyy'),'ccyymmdd'))
		end if
		glitem2$(3)=tr$
		glitem2$(4)=vn$
		glitem2$(5)=trim$(fnrgl$(gl$))
		glitem2$(6)=str$(tAmt)
		glitem2$(7)=desc$
		glitem2$(8)=fn_transType$(tType)&' ('&str$(tType)&')'
		if postCode then
			glitem2$(9)='POSTED'
		else
			glitem2$(9)=''
		end if
		glitem2$(10)=trim$(fnrgl$(key$))
		lineCount+=1
		if glitem2$(1)=str$(previouslySelected) then setenv('current_grid_row',str$(lineCount))
		fnflexadd1(mat glitem2$)
		net+=tAmt ! add net check
		oldtr$=tr$ ! hold reference numbers
	loop
	TgEoF: !
	if enableNet and lineCount then
		mat glitem2$=('')
		glitem2$(6)=str$(net)
		glitem2$(7)="Net"
		lineCount+=1
		fnflexadd1(mat glitem2$)
	end if
fnend
def fn_totalGrid(hAccount,mat kList$,mat kReceipts,mat kDisbursements,mat kAdjustments,lc,ps)
	! r: setup
	dim chdr_proof_total$(6),cmask3$(6),glitem3$(6)*128
	chdr_proof_total$(1)='G/L Account'  	: cmask3$(1)=''
	chdr_proof_total$(2)='Beg Balance'  	: cmask3$(2)='10'
	chdr_proof_total$(3)='Receipts'     	: cmask3$(3)='10'
	chdr_proof_total$(4)='Disbursements'	: cmask3$(4)='10'
	chdr_proof_total$(5)='Adjustments'  	: cmask3$(5)='10'
	chdr_proof_total$(6)='End Balance'  	: cmask3$(6)='10'

	! /r
	dim kBegBalance(0),kEndBalance(0)
	mat kBegBalance(udim(mat kList$))
	mat kEndBalance(udim(mat kList$))
	for j=1 to udim(mat kList$)

		read #hAccount,using "form pos 87,pd 6.2",key=kList$(j),release: kBegBalance(j) nokey L6240 ! get last balance
		kEndBalance(j)=kBegBalance(j)-kReceipts(j)-kDisbursements(j)+kAdjustments(j) ! new balance when posted
		L6240: !
	next j

	fnflexinit1('Prooftotals',lc,ps,15,90,mat chdr_proof_total$,mat cmask3$,1,0,0)
	mat glitem3$=('')
	for j=1 to udim(mat kList$)
		! if trim$(kList$(j))<>'' then ! skip blanks
		glitem3$(1)=trim$(fnrgl$(kList$(j)))
		glitem3$(2)=str$(kBegBalance(j))
		glitem3$(3)=str$(-kReceipts(j))
		glitem3$(4)=str$(kDisbursements(j))
		glitem3$(5)=str$(kAdjustments(j))
		glitem3$(6)=str$(kEndBalance(j))
		fnflexadd1(mat glitem3$)
		! end if
	next j
fnend

def fn_transactionAdd(hMerge,typeOfEntryN,glBank$,transDate; ___,returnN,gl$*12,tr4,tr5,tType,tr7,lrPrior,smResponse)
	do
		gl$=''
		tr5=tType=tr7=0
		tr$=td$=vn$=key$=('')
		jv2$=''
		tr4=transDate
		tType=typeOfEntryN
		fn_clearVar(hMtemp,transactionAmt,td$,tr$,vn$,gl$,totalalloc,gl_retainFieldsDuringAdd$)
		lrPrior=lrec(hMerge)

		if selx=sx_adjustment then
			smResponse=fn_scrAdjustment(hMerge,0,bankAcctName$,message$,tr$,glBank$,transDate)
		else if selx=sx_payrollCheck then
			smResponse=fn_scrPayrollAdd
		else
			smResponse=fn_scrMain(hMerge, 0,'Adding '&fn_transType$(typeOfEntryN),glBank$,transDate,bankAcctName$,message$,tr$)
		end if
		if lrPrior<>lrec(hMerge) then returnN=0 else returnN=lrec(hMerge)
	loop while smResponse=ck_saveA
	fn_transactionAdd=returnN ! returns last record worked for auto selection on ScreenOne
fnend
def fn_transactionEdit(hMerge,recordNumber,glBank$*12; ___,returnN,gl$*12,tr4,tr5,tType,tr7,tr$*12,td$*30,vn$*8,jv2$*5,key$*12)
	if recordNumber then
		! returns:   totalalloc
		! requires:  recordNumber (record number of hMerge to edit)
		!            etc
		fn_readMerge(recordNumber,gl$,tr4,tr5,tType,tr7,tr$,td$,vn$,jv2$,key$)  ! get basic information from record clicked to find the complete transaction
		if trim$(key$)='' then key$=glBank$
		! pr '  fn_transactionEdit   passing Key$='&key$ : pause
		if tType=sx_adjustment then
			returnN=fn_scrAdjustment(hMerge,recordNumber,bankAcctName$,message$,tr$,key$,transDate)
		else
			hMtemp=fn_makeMergeTemp(hMerge,recordNumber,transactionAmt,totalalloc,selx)
			returnN=fn_scrMain(hMerge, 1,'Editing '&fn_transType$(selx),key$,tr4,bankAcctName$,message$,tr$)
			! fn_scrMain(hMerge; editMode,heading$*64,glBank$
		end if
	end if
	TeFinis: !
	close #hMtemp,free: ioerr ignore
	fn_transactionEdit=returnN
fnend

def fn_makeMergeTemp(hMerge,mergeRecordNumber,&tAmt,&totalalloc,&selx; ___, _
		allocAmt,holdtr$*12, _
		gl$*12,tr4,tType,tr7,tr$*12,td$*30,vn$*8,jv2$*5,key$*12)
			tAmt=0
			if fn_readMerge(mergeRecordNumber,gl$,tr4,tr5,tType,tr7,tr$,td$,vn$,jv2$,key$)=57 then goto PES_XIT
			holdtr$=tr$
			close #hMtemp: ioerr ignore
			open #hMtemp=fnH: "Name=[Q]\GLmstr\Allocations[acsUserId].h[cno],Version=1,replace,RecL=59",internal,outIn,relative
			! process the whole file and gather all records that match reference number and gather into MergeTemp file
			restore #hMerge:
			do
				if fn_readMerge(0,payeegl$,tr4,allocAmt,tType,tr7,tr$,td$,vn$,jv2$,key$)=-4270 then goto MtEoM
				if trim$(tr$)=trim$(holdtr$) then
					if tType=sx_receipt or tType=sx_sale then allocAmt=-allocAmt ! reverse signs on receipts and sales BEFORE DISPLAYING ON CORRECTION SCREEN
					tAmt+=allocAmt
					vn$=vn$

					selx=tType
					write #hMtemp,using "form pos 1,c 12,pd 10.2,c 30,pd 5": payeegl$,allocAmt,td$,rec(hMerge)
					totalalloc+=allocAmt ! re-add total allocations
				end if
			loop
			MtEoM: !

			PES_XIT: !

	fn_makeMergeTemp=hMtemp
fnend
def fn_mTempToMerge(hMtemp,hMerge,tr4,selx,tr7,tr$,vn$,jv2$,glBank$*12; _
	___,allocgl$*12,td$*30,allocAmt,transAdr)
	restore #hMtemp:
	do
		read #hMtemp,using "form pos 1,c 12,pd 10.2,c 30,pd 5": allocgl$,allocAmt,td$,transAdr eof EoMtempToMerge
		fn_transactionSave(hMerge,transAdr,allocgl$,tr4,allocAmt,selx,tr7,tr$,td$,vn$,jv2$,glBank$)
	loop
	EoMtempToMerge: !
fnend
def fn_transactionSave(hMerge,transAdr,gl$,tr4,tr5,tType,tr7,tr$,td$*30,vn$*8,jv2$*5,glBank$*12)
	if tType=sx_receipt or tType=sx_sale then tr5=-tr5 ! reverse signs on receipts and sales
	if transAdr>0 then
		rewrite #hMerge,using F_merge,rec=transAdr: gl$,tr4,tr5,tType,tr7,tr$,td$,vn$,jv2$,glBank$ ! noRec L3280
	else
		! L3280: !
		write #hMerge,using F_merge: gl$,tr4,tr5,tType,tr7,tr$,td$,vn$,jv2$,glBank$
	end if
fnend
def fn_payrollSave  ! &tr$,gl$,...; ___,j,allocgl$,td$*30
	! WritePayrollTrans:
	! only routine that uses mat pgl (read from company.h[cno])

	for j=2 to 19
		jv2$=str$(j-1) ! carry breakdown code for posting employee record
		if j=2 then allocgl$=gl$ : td$="Gross Pay-"&empName$(1:18)
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
			prx(j)=-prx(j) ! reverse sign on fica, etc
		else if j>6 and j<17 and dedcode(j-6)=1 then
			prx(j)=-prx(j)
			! turn sign around on any of ten deductions coded as additions
		else if j=17 then
			prx(j)=-prx(j)
			td$="Tips"
			allocgl$=gl$ ! tips
		else if j=19 then
			prx(j)=prx(j)
			td$="Eic" ! eic as positive
		end if
		if prx(j)<>0 then
			write #hMerge,using F_merge: allocgl$,tr4,prx(j),sx_payrollCheck,tr7,tr$,td$,vn$,jv2$,key$ ! gross wage
		end if
	next j
	transactionAmt=0
	mat prx=(0)
	vn$=''
	tr$=fn_nextTr$(tr$,gl_retainFieldsDuringAdd$)
fnend
def fn_transactionDelete(hMerge,mergeRec; hMtemp)
	! omit hMtemp to skip processing that file
	! deletes entire transaction
	if fnConfirmDelete('Record '&str$(mergeRec),'transDeleteOne') then
		if hMtemp then
			restore #hMtemp:
			do
				read #hMtemp,using "Form pos 1,c 12,pd 10.2,c 30,pd 5": gl$,allocation,td$,mergeRec eof TdEoMt
				delete #hMtemp:
				delete #hMerge,rec=mergeRec: ioerr ignore
			loop
			TdEoMt: !
		else
			delete #hMerge,rec=mergeRec: ioerr ignore
		end if
		transactionAmt=0
		vn$=gl$=tr$=''
	end if
fnend

def fn_clearVar(&hMtemp,&transactionAmt,&td$,&tr$,&vn$,&gl$,&totalalloc, _
	&gl_retainFieldsDuringAdd$)
	!  clear entry screen
	close #hMtemp: ioerr ignore
	open #hMtemp=fnH: "Name=[Q]\GLmstr\Allocations[acsUserId].h[cno],Version=1,replace,RecL=59",internal,outIn,relative
	transactionAmt=0
	if gl_retainFieldsDuringAdd$='False' then
		td$=''
	end if
	tr$=fn_nextTr$(tr$,gl_retainFieldsDuringAdd$)
	vn$=gl$=''
	totalalloc=0
fnend
def fn_extract(&hMtemp,vn$,transactionAmt; ___, _
	hPayeeGl,glkey$*8,payeekey$*8,payeegl$,percent,td$*30,allocAmt,lastallocation)
	!  pull allocation breakdown from payee record
	if glkey$<>'' then

		open #hPayeeGl=fnH: "Name=[Q]\GLmstr\PayeeGLBreakdown.h[cno],Version=1,KFName=[Q]\GLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
		glkey$=lpad$(rtrm$(vn$),8)
		restore #hPayeeGl,key>=glkey$: nokey ExtractFinis

		close #hMtemp: ioerr ignore
		open #hMtemp=fnH: "Name=[Q]\GLmstr\Allocations[acsUserId].h[cno],Version=1,replace,RecL=59",internal,outIn,relative

		do
			read #hPayeeGl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,td$ eof ExtractEoPgl
			if payeekey$=glkey$ then
				if percent=0 and lrec(hMtemp)=0 then percent=100
				allocAmt=round(transactionAmt*(percent*.01),2)
				write #hMtemp,using "form pos 1,c 12,pd 10.2,c 30": payeegl$,allocAmt,td$
				allocgl$='' : totalalloc+=allocAmt : allocAmt=0
			end if
		loop while payeekey$=glkey$
		ExtractEoPgl: !

		if totalalloc<>transactionAmt and lrec(hMtemp)>0 then ! r: if it doesn't add up put the difference on the last allocation
			read #hMtemp,using "Form pos 13,pd 10.2",rec=lrec(hMtemp): lastallocation noRec ignore
			lastallocation+=transactionAmt-totalalloc
			rewrite #hMtemp,using "Form pos 13,pd 10.2",rec=lrec(hMtemp): lastallocation
		end if 	! /r

		ExtractFinis: !
		close #hPayeeGl: ioerr ignore

	end if
fnend
def fn_nextTr$(tr$,gl_retainFieldsDuringAdd$; ___,trVal)
	if gl_retainFieldsDuringAdd$='False' then
		tr$=''
	else
		trVal=val(tr$) conv ignore
		if trVal and tr$<>"999999999999" then
			tr$=str$(val(tr$)+1) ! increment if possible
		end if
	end if
	fn_nextTr$=tr$
fnend
def fn_buildMatK(hMerge,mat kList$,mat kReceipts,mat kDisbursements,mat kAdjustments,&totalCredits,&totalDebits,&count; ___,tAmt,key$*12,tranType,kWhich)
	!  accumulate proof totals (mat k, totalCredits, totalDebits)
	mat kList$(0)
	mat kReceipts(0)
	mat kDisbursements(0)
	mat kAdjustments(0)

	totalDebits=totalCredits=count=0
	restore #hMerge:
	do
		read #hMerge,using "Form pos 19,PD 6.2,N 2,pos 93,C 12": tAmt,tranType,key$ eof FinisBuildMatK
		count+=1

		kWhich=srch(mat kList$,key$)
		if kWhich>0 then
			kWhich
		else
			kWhich=fnAddOneC(mat kList$,key$)
			mat kReceipts(kWhich)
			mat kDisbursements(kWhich)
			mat kAdjustments(kWhich)
		end if

		if kList$(kWhich)=key$ then
			if tranType=1 or tranType=4 or tranType=8 then
				! 1 = Disbursements
				! 4 = Payroll Check
				kDisbursements(kWhich)+=tAmt ! total debits entered
				kDisbursements(kWhich)+=tAmt ! total debits entered
				totalDebits+=tAmt
				totalCredits-=tAmt
			else if tranType=2 or tranType=7 then
				! 2 = Receipts
				kReceipts(kWhich)+=tAmt ! total credits entered
				kReceipts(kWhich)+=tAmt ! total credits entered
				totalCredits+=tAmt
				totalDebits-=tAmt
			else if tranType=3 then
				! 3 = Adjustments
				kAdjustments(kWhich)+=tAmt ! net adjustments
				kAdjustments(kWhich)+=tAmt ! net adjustments
				if tAmt<0 then totalCredits+=tAmt
				if tAmt>0 then totalDebits+=tAmt
			else
				pr 'how should i total up this trans type ('&str$(tranType)&')'
				pause
			end if
		end if
	loop
	FinisBuildMatK: !
fnend

def fn_prProofTotals(hAccount,totalDebits,totalCredits, _
		glBank$,contraEntryDateN, _
		mat kList$,mat kReceipts,mat kDisbursements,mat kAdjustments,mat chdr_proof_total$,mat glitem3$ _
		; ___,j,mylen)

	dim kBegBalance(0),kEndBalance(0)
	mat kBegBalance(udim(mat kList$))
	mat kEndBalance(udim(mat kList$))
	for j=1 to udim(mat kList$)
		read #hAccount,using "form pos 87,pd 6.2",key=kList$(j),release: kBegBalance(j) nokey PPT_L6240 ! get last balance
		kEndBalance(j)=kBegBalance(j)-kReceipts(j)-kDisbursements(j)+kAdjustments(j) ! new balance when posted
		PPT_L6240: !
	next j
	fnopenprn
	mylen=20
	pr #255: lpad$("Total Debits:",mylen)&' '&cnvrt$("pic(-------,---,---.##)",totalDebits)
	pr #255: lpad$("Total Credits:",mylen)&' '&cnvrt$("Pic(-------,---,---.##",totalCredits)
	pr #255: ''
	! pr #255: lpad$("Bank Account:",mylen)&' '&fnrgl$(glBank$)
	pr #255: lpad$("Process Ending Date:",mylen)&' '&cnvrt$("pic(zz/zz/zz)",contraEntryDateN)
	pr #255: ''
	pr #255,using F_PPT_LINE: mat chdr_proof_total$
	F_PPT_LINE: form pos 1,c 12,5*cr 19
	mat glitem3$=('')
	for j=1 to udim(mat kList$)
		if trim$(kList$(j))<>'' then ! skip blanks
			glitem3$(1)=kList$(j)
			glitem3$(2)=cnvrt$("Pic(-------,---,---.##)",kBegBalance(j)) ! cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(5)="10"
			glitem3$(3)=cnvrt$("Pic(-------,---,---.##)",-kReceipts(j))
			glitem3$(4)=cnvrt$("Pic(-------,---,---.##)",kDisbursements(j))
			glitem3$(5)=cnvrt$("Pic(-------,---,---.##)",kAdjustments(j))
			glitem3$(6)=cnvrt$("Pic(-------,---,---.##)",kEndBalance(j)) ! cmask3$(6)='10'
			pr #255,using F_PPT_LINE: mat glitem3$
		end if
	next j
	fncloseprn
fnend
def fn_prProofList(hMerge; _
	___,holdtr$,tr$,tr4,tr5,tType,tr7,td$,vn$,jv2$,key$,printkey$, _
	netamount)
	fnopenprn
	gosub PrProofListHeader
	holdtr$=tr$=''
	restore #hMerge:
	do
		holdtr$=tr$
		if fn_readMerge(0,gl$,tr4,tr5,tType,tr7,tr$,td$,vn$,jv2$,key$)=-4270 then goto PE_FINIS
		if trim$(holdtr$)<>'' and holdtr$<>tr$ then
			pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount
			netamount=0
		end if
		if tType=sx_adjustment then prntkey$='' else printkey$=key$
		pr #255,using F_PE_LINE: gl$,tr4,tr5,tType,tr7,tr$,td$,vn$,printkey$,'',jv2$ pageoflow PrProofListPgOf
		F_PE_LINE: form pos 1,c 12,x 2,pic(zz/zz/zz),n 11.2,x 4,pic(zz),pic(zz),c 13,c 30,c 10,c 12,c 7,c 7
		netamount+=tr5
	loop
	PE_FINIS: !
	pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount : netamount=0
	fncloseprn
fnend
PrProofListPgOf: ! r:
	pr #255: newpage
	gosub PrProofListHeader
continue  ! /r
PrProofListHeader: ! r:
	pr #255: ''
	pr #255,using 'form pos 1,c 8,pos 29,Cc 40,skip 1,pos 1,c 8,pos 40,c 40': date$,env$('cnam'),time$,"GL Input Proof List"
	pr #255: ''
	pr #255: "   Account      Date       Amount    TC  Reference    Payee/Description             Vendor";
	if jccode=1 then pr #255: "Job     Cat  S-Cat" else pr #255: " "
return  ! /r

def fn_inputClientCheckbookFile

	! r: screen_insert_diskette
	fntos
	fnlbl(2,1,"Insert Input Diskette in selected drive:",40,1)
	fntxt(2,42,1)
	if dv$='' then dv$="A"
	resp$(1)=dv$
	fnCmdSet(2)
	ckey=fnacs(mat resp$)
	dv$=resp$(1)&":"
	! /r
	fn_closeFiles
	fnCopy(dv$&"GLWK101.h[cno]","[Q]\GLmstr\GL_Work_[acsUserId].h[cno]")
	fn_openFiles(1)
fnend
def fn_eraseAllPreviousInput
	fn_closeFiles
	fnCopy('[Q]\GLmstr\GL_Work_[acsUserId].h[cno]','[temp]\acs_Enter_Transactions_[acsUserId]_batchDelete_[datetime].h[cno]')
	fn_openFiles( 1)
fnend

def fn_openFiles(; reset,___,useOrReplace$)
	if reset then
		useOrReplace$='Replace'
		fnFree('[Q]\GLmstr\Allocations[acsUserId].h[cno]')
	else
		useOrReplace$='Use'
	end if
	dim tr7
	dim tr$*12 ! transaction reference number
	dim td$*30 ! transaction description
	dim jv2$*5
	dim key$*12
	open #hMerge=fnH: 'Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],RecL=104,'&useOrReplace$,internal,outIn,relative
	F_merge: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,x 6,c 5,x 3,c 12
	open #hAccount=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
fnend
def fn_closeFiles ! always local
	close #hMerge: ioerr ignore
	close #hAccount: ioerr ignore
fnend

def fn_createContras(hMerge,mat kList$,mat kReceipts,mat kDisbursements,contraEntryDateN; ___,j,tr$*12)
	restore #hMerge:
	tr$="999999999999"
	for j=1 to udim(mat kList$)
		if val(kList$(j))<>0 then
			if kReceipts(j) then
				write #hMerge,using F_merge: kList$(j),contraEntryDateN,-kReceipts(j),2,0,tr$,"Contra Entry",'','' ! debits from receipts or sales      ! ,rec=lrec(hMerge)+1
			end if
			if kDisbursements(j) then
				write #hMerge,using F_merge: kList$(j),contraEntryDateN,-kDisbursements(j),1,0,tr$,"Contra Entry",'','' ! credits from checks or purchases   ! ,rec=lrec(hMerge)+1
			end if
		end if
	next j
fnend
def fn_clearContrasAndPosted(hMerge; ___,tr$*12,postCode)
	restore #hMerge:
	do
		read #hMerge,using 'form pos 27,n 2,pos 29,c 12': postCode,tr$ eof CfcEoF
		if postCode=9 or trim$(tr$)="999999999999" then
			delete #hMerge:
		end if
	loop
	CfcEoF: !
fnend

Xit: ! r:
	setenv('current_grid_row',gridRowHold$)
fnXit ! /r
DoMerge: ! r:
	setenv('current_grid_row',gridRowHold$)
fnchain("S:\General Ledger\Merge") ! /r
DoPrMerge: ! r:
	setenv('current_grid_row',gridRowHold$)
fnchain("S:\acsGL\PRMerge") ! /r
DoAutoProc: ! r:
	setenv('current_grid_row',gridRowHold$)
fnchain("S:\acsGL\autoproc") ! /r
def fn_readMerge(record,&gl$,&tDate,&tAmt,&tType,&postCode,&tr$,&desc$,&vn$,&jv2$,&key$; ___,returnN)
		gl$=tr$=desc$=vn$=jv2$=key$=''
		tDate=tAmt=tType=postCode=0
		if record then
			read #hMerge,using F_merge,rec=record: gl$,tDate,tAmt,tType,postCode,tr$,desc$,vn$,jv2$,key$ ioerr RmErr
		else
			read #hMerge,using F_merge            : gl$,tDate,tAmt,tType,postCode,tr$,desc$,vn$,jv2$,key$ ioerr RmErr
		end if
		if tType=5 then tType=7
		if tType=6 then tType=8

		returnN=rec(hMerge)
	goto RmFinis
	RmErr: !
		returnN=-err
	goto RmFinis
	RmFinis: !
	fn_readMerge=returnN
fnend
def fn_transType$(key; ___,hTt,which,return$*18)
	if ~setupTransTypeC then ! r:
		setupTransTypeC=1
		dim tt$(0)*128
		dim ttN(0)
		hTt=fn_open('GL TransactionType', mat tt$, mat ttN, mat form$, 1)
		dim ttKeyN(0)
		mat ttKeyN(0)
		dim ttDesc$(0)*18
		mat ttDesc$(0)
		do
			read #hTt,using form$(hTt): mat tt$,mat ttN eof TtEof
			fnAddOneN(mat ttKeyN,ttN(tt_key))
			fnAddOneC(mat ttDesc$,rtrm$(tt$(tt_name)))
		loop
		TtEof: !
		close #hTt: ioerr ignore
	end if ! /r

	which=srch(mat ttKeyN,key)
	if which then
		return$=ttDesc$(which)
	else
		return$=''
	end if
	fn_transType$=return$
fnend
include: fn_open
include: ertn
