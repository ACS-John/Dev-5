! formerly S:\acsGL\GLInput
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
	fncreg_read('Enter Transaction - Bank Account',bankgl$)

	gridTransactions=1
	gridProofTotals=2
	gridTransactionsNet=3
	gridSelected=fnPcReg_read('gridSelected',gridSelected$, str$(gridTransactions))


	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	dim miscname$(10)*20
	dim dedcode(10)
	dim pgl(5,3)
	dim miscgl$(10)*12
	read #1,using 'Form POS 298,15*PD 4,POS 382,N 2,POS 418,10*C 20,10*N 1,POS 668,10*C 12': mat pgl,jccode,mat miscname$,mat dedcode,mat miscgl$
	close #1:
! /r
fn_openFiles

fn_clearContras(hMerge)
post=fn_getPost(hMerge)

ScreenOne: ! r:
	editMode=recordNumber=0
	fnTos ! r:
	colPos=48 : mylen=20 : mypos=colPos+mylen+3 : rc=lc=0
	fnLbl(lc+=1,colPos,"Type Of Entry:",mylen,1)
	fnComboF("TransactionType",lc,mypos,20,'S:\Core\Data\GL TransactionType.dat',1,1,2,18,'S:\Core\Data\GL TransactionType.idx',1,0,"You must indicate the type of entry you will be entering.")
	if selx=0 then selx=3
	resp$(respc_entryType=rc+=1)=str$(selx)
	fnLbl(lc+=1,colPos,"Bank Account:",mylen,1)
	fnqgl(lc,mypos,0,2)
	resp$(respc_bankGl=rc+=1)=fnrgl$(bankgl$)
	fnLbl(lc+=1,colPos,"Process Ending Date:",mylen,1)
	fnTxt(lc,mypos,8,0,1,"1001",0,"Process endings date must always be answered and will be the last day of the month or the last day of the period beding processed.",0 )
	resp$(respc_contraDate=rc+=1)=contraEntryDate$
	! lc+=1
	lc=0
	colPos=1 : mylen=20 : mypos=colPos+mylen+3
	! prior input   totals here
	dim kList$(30)*12
	dim kVal(30,8)
	fn_buildMatK(hMerge,mat kList$,mat kVal,totalCredits,totalDebits,count)
	! pr 'totalCredits,totalDebits=';totalCredits,totalDebits : pause
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
	fnbutton_or_disabled(gridSelected<>gridTransactions   	,lc,10,'Entries'     	,ck_gridTrans    	=55,'',12)
	fnbutton_or_disabled(gridSelected<>gridTransactionsNet	,lc,24,'Entries+Net' 	,ck_gridTransNet 	=57,'',12)
	fnbutton_or_disabled(gridSelected<>gridProofTotals    	,lc,38,'Proof Totals'	,ck_gridTotals   	=56,'',12)
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
	ckey=fnAcs(mat resp$)
	if ckey=ck_exit then goto Xit

	! pOST=1 ! code as unposted once leave this screen
	! if post=1 and resp$(respc_regularInput)="True" then fn_clearContras(hMerge) ! remove any contra entries created in previous run
	selx=sel1=val(resp$(respc_entryType)(1:1))
	! If selx=4 Then Goto ScreenOne ! temporary line do not access payroll checks
	typeofentry$=resp$(respc_entryType)
	key$=bankgl$=fnagl$(resp$(respc_bankGl)) ! GL number
	contraEntryDate$=resp$(respc_contraDate)
	contraEntryDateN=val(contraEntryDate$)
	if gridSelected<>gridProofTotals then 
		recordNumber=val(resp$(respc_grid))
		previouslySelected=recordNumber
	end if
	if (selx=1 or selx=2) and val(bankgl$)=0 then goto S1BankGlFail ! must have GL bank account on receipts or disbursements
	dim bankname$*40
	if (selx=1 or selx=2 or selx=4) then
		read #hAccount,using "form pos 13,c 40",key=bankgl$,release: bankname$ nokey S1BankGlFail
	end if

	post=1
	fncreg_write('Enter Transaction - Process Ending Date',contraEntryDate$)
	fncreg_write('Enter Transaction - Bank Account',bankgl$)
	if ckey=ck_edit then
		! pr 'before fn_tranactionEdit(';recordNumber;') call' : pause
		fn_tranactionEdit(recordNumber)
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
		if fnConfirmDelete('(entire batch of transactions)','clearAllEnteredTrans') then
			fn_eraseAllPreviousInput
		end if
	else if ckey=ck_deleteOne then
		if recordNumber then
			fn_transactionDelete(hMerge,recordNumber)
		end if
	else if ckey=ck_printProofList then
		fn_prProofList(hMerge)
	else if ckey=ck_printProofTotals then
		fn_prProofTotals(hAccount,totalDebits,totalCredits,typeofentry$,bankgl$,contraEntryDateN,mat kList$,mat kVal,mat chdr_proof_total$,mat glitem3$)
	else if ckey=ck_Add then
		previouslySelected=fn_transactionAdd(selx,bankGl$,contraEntryDateN)
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

def fn_transactionSave(hMerge,transadr,mat tr,tr$,td$*30,vn$*8,mat jv$,key$*12)
	if tr(6)=2 or tr(6)=7 then tr(5)=-tr(5) ! reverse signs on receipts and sales
	if transadr>0 then
		rewrite #hMerge,using fGlWork,rec=transadr: mat tr,tr$,td$ noRec L3280
	else
		L3280: !
		write #hMerge,using fGlWork: mat tr,tr$,td$,vn$,mat jv$,key$ ! removed ,rec=lr2   was redundant
		fGlWork: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
	end if
fnend

GetAllocationsFromPayee: ! r:  pull allocation breakdown from payee record
	glkey$=lpad$(rtrm$(vn$),8)
	restore #hPayeeGl,key>=glkey$: nokey L4780
	do
		read #hPayeeGl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30',release: payeekey$,payeegl$,percent,td$ eof L4740
		if payeekey$<>glkey$ then goto L4740
		if percent=0 and lrec(hAllocations)=0 then percent=100
		allocamt=round(transactionAmt*(percent*.01),2)
		write #hAllocations,using "form pos 1,c 12,pd 10.2,c 30": payeegl$,allocamt,td$: allocgl$='': totalalloc+=allocamt: allocamt=0
	loop
	L4740: !
	if totalalloc<>transactionAmt then
		read #hAllocations,using "Form pos 13,pd 10.2",rec=lrec(hAllocations): lastallocation noRec ignore
	else
		lastallocation+=transactionAmt-totalalloc
		rewrite #hAllocations,using "Form pos 13,pd 10.2",rec=lrec(hAllocations): lastallocation
	end if
	allocamt=0 ! kj  eXTRACT=0
	L4780: !
return ! /r
def fn_transactionDelete(hMerge,mergeRec; hAllocations)
	! omit hAllocations to skip processing that file
	! deletes entire transaction
	if fnConfirmDelete('Record '&str$(mergeRec),'transDeleteOne') then
		if hAllocations then
			restore #hAllocations:
			do
				read #hAllocations,using "Form pos 1,c 12,pd 10.2,c 30,pd 5": gl$,allocation,td$,mergeRec eof L5170
				delete #hAllocations:
				delete #hMerge,rec=mergeRec: ioerr ignore
			loop
			L5170: !
		else
			delete #hMerge,rec=mergeRec: ioerr ignore
		end if
		transactionAmt=0
		vn$=gl$=tr$=''
	end if
fnend

def fn_addOneGrid
	if gridSelected=gridTransactions then
		fn_transactionGrid(hMerge,lc+=1,2,10,90,previouslySelected)
	else if gridSelected=gridTransactionsNet then
		fn_transactionGrid(hMerge,lc+=1,2,10,90,previouslySelected, 1)
	else if gridSelected=gridProofTotals then
		fn_totalGrid(hAccount,mat kList$,mat kVal,lc+=1,2)
	end if
fnend
def fn_transactionGrid(hMerge,row,col,twenty,ninety,previouslySelected; _
	enableNet,___,net,tDate,tAmt,tType,desc$*30,colCount,postCode,lineCount)
	! r: setup
	colCount=10
	dim cmask2$(0)
	mat cmask2$(colCount)
	mat cmask2$=('')
	
	dim chdr2$(0)*25
	mat chdr2$(colCount)
	chdr2$(1)='Record '                      : cmask2$(1)='30'
	chdr2$(2)='Date'
	chdr2$(3)='Reference'      :     if ~enableNet then cmask2$(2)='3'
	chdr2$(4)='Payee/Description'
	chdr2$(5)='GL Account'
	chdr2$(6)='Amount'                       :  cmask2$(6)='10'
	chdr2$(7)='Allocation Description'
	chdr2$(8)='Type'
	chdr2$(9)='PC'
	chdr2$(10)='Key'
	dim glitem2$(0)*30
	mat glitem2$(colCount)
	! /r
	fnflexinit1('GlTrans',row,col,twenty,ninety,mat chdr2$,mat cmask2$,1,0,0)
	restore #hMerge:
	do
		read #hMerge,using F_merge: gl$,tDate,tAmt,tType,postCode,tr$,desc$,vn$,mat jv$,key$ eof TgEoF
		if enableNet then
			if trim$(tr$)<>'' and tr$<>oldtr$ then
				mat glitem2$=('')
				glitem2$(6)=str$(net)
				glitem2$(7)="Net"
				fnflexadd1(mat glitem2$)
				net=0 ! add net subtotals any time reference number changes     ( AND NET<>0)was in there
			end if
		end if
		! gL$=CNVRT$("pic(zz#)",TR(1))&CNVRT$("pic(zzzzz#)",TR(2))&CNVRT$("pic(zz#)",TR(3))
		glitem2$(1)=str$(rec(hMerge))
		if enableNet then
			glitem2$(2)=date$(days(tDate,'mmddyy'),'ccyy/mm/dd')
		else
			glitem2$(2)=str$(tDate) ! str$(date(days(tr(4),'mmddyy'),'ccyymmdd'))
		end if
		glitem2$(3)=tr$
		glitem2$(4)=vn$
		glitem2$(5)=gl$
		glitem2$(6)=str$(tAmt)
		glitem2$(7)=desc$
		glitem2$(8)=fn_transType$(tType)&' ('&str$(tType)&')'
		if postCode then
			glitem2$(9)='POSTED'
		else
			glitem2$(9)=''
		end if
		glitem2$(10)=key$
		lineCount+=1
		if glitem2$(1)=str$(previouslySelected) then setenv('current_grid_row',str$(lineCount))
		fnflexadd1(mat glitem2$)
		net+=tr(5) ! add net check
		oldtr$=tr$ ! hold reference numbers
	loop
	TgEoF: !
	if enableNet then
		mat glitem2$=('')
		glitem2$(6)=str$(net) : glitem2$(7)="Net"
		lineCount+=1
		fnflexadd1(mat glitem2$)
	end if
fnend
def fn_totalGrid(hAccount,mat kList$,mat kVal,lc,ps)
	! r: setup
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
	! /r
	for j=1 to 30
		read #hAccount,using "form pos 87,pd 6.2",key=kList$(j),release: kVal(j,4) nokey L6240 ! get last balance
		kVal(j,8)=kVal(j,4)-kVal(j,5)-kVal(j,6)+kVal(j,7) ! new balance when posted
		L6240: !
	next j

	fnflexinit1('Prooftotals',lc,ps,15,90,mat chdr_proof_total$,mat cmask3$,1,0,0)
	mat glitem3$=('')
	for j=1 to 30
		if trim$(kList$(j))<>'' then ! skip blanks
			glitem3$(1)=kList$(j)
			glitem3$(2)=str$(kVal(j,4))
			glitem3$(3)=str$(-kVal(j,5))
			glitem3$(4)=str$(kVal(j,6))
			glitem3$(5)=str$(kVal(j,7))
			glitem3$(6)=str$(kVal(j,8))
			fnflexadd1(mat glitem3$)
		end if
	next j
fnend

def fn_transactionAdd(typeOfEntryN,bankGl$,transDate; ___,returnN)
	mat tr=(0)
	tr$=td$=vn$=key$=('')
	mat jv$=('')
	tr(4)=transDate
	tr(6)=typeOfEntryN
	write #hMerge,using fGlWork: mat tr,tr$,td$,vn$,mat jv$,bankGl$
	recordNumber=lrec(hMerge)
	returnN=recordNumber
	
	fn_clearVar(hAllocations,transactionAmt,td$,tr$,vn$,gl$,totalalloc,gl_retainFieldsDuringAdd$)
	! response=fn_tranactionEdit(recordNumber)
	fn_scrMain(editMode)
	response=ckey
	
	if response=6 then ! canceled     response is ckey from fn_scrMain
		delete #hMerge,rec=recordNumber:
		returnN=0
	end if
	fn_transactionAdd=returnN
fnend
def fn_clearVar(&hAllocations,&transactionAmt,&td$,&tr$,&vn$,&gl$,&totalalloc,&gl_retainFieldsDuringAdd$)
	!  clear entry screen
	close #hAllocations: ioerr ignore
	open #hAllocations=fnH: "Name=[Q]\GLmstr\Allocations[acsUserId].h[cno],Version=1,replace,RecL=59",internal,outIn,relative
	transactionAmt=0
	if gl_retainFieldsDuringAdd$='False' then
		td$=''
	end if
	tr$=fn_nextTr$(tr$,gl_retainFieldsDuringAdd$)
	vn$=gl$=''
	totalalloc=0 
	! extract=0 kj
fnend
def fn_tranactionEdit(recordNumber; ___,returnN)
		if recordNumber then
			! gosub PREPARE_EDIT_SCREEN
			! PREPARE_EDIT_SCREEN: ! r:
			! returns:   totalalloc
			! requires:  recordNumber (record number of hMerge)
			!            hMerge
			!            etc
			editMode=1
			transactionAmt=0
			gl$=''
			totalalloc=0
			read #hMerge,using fGlWork,rec=recordNumber: mat tr,tr$,td$,vn$,mat jv$,key$ noRec PES_XIT ! get basic information from record clicked to find the complete transaction
			holdtr$=tr$
			close #hAllocations: ioerr ignore
			open #hAllocations=fnH: "Name=[Q]\GLmstr\Allocations[acsUserId].h[cno],Version=1,replace,RecL=59",internal,outIn,relative
			restore #hMerge:
			do
				GLALLOCATIONS_READ: !
				read #hMerge,using F_merge: payeegl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof GLALLOCATIONS_EOF
				if trim$(tr$)<>trim$(holdtr$) then goto GLALLOCATIONS_READ
				if tr(6)=2 or tr(6)=7 then tr(5)=-tr(5) ! reverse signs on receipts and sales BEFORE DISPLAYING ON CORRECTION SCREEN
				transactionAmt+=tr(5)
				vn$=vn$
				selx=tr(6)
				if tr(6)=1 then selx=1 ! reg Disbursements
				if tr(6)=2 then selx=2 ! reg Receipts
				if tr(6)=3 then selx=3 ! reg Adjustments
				if tr(6)=4 then selx=4 !     Payroll Check
				if tr(6)=7 then selx=5 !     Sales
				if tr(6)=8 then selx=6 !     Purchases
				write #hAllocations,using "form pos 1,c 12,pd 10.2,c 30,pd 5": payeegl$,tr(5),td$,rec(hMerge)
				totalalloc+=tr(5) ! re-add total allocations
			loop
			GLALLOCATIONS_EOF: !
			read #hMerge,using fGlWork,rec=recordNumber: mat tr,tr$,td$,vn$,mat jv$,key$ noRec ignore ! get basic information from record clicked to find the complete transaction
			PES_XIT: !
			! return  ! /r

			fn_scrMain
			returnN=ckey
			
		end if
	TeFinis: !
	close #hAllocations,free: ioerr ignore
	fn_tranactionEdit=returnN
fnend
def fn_scrMain(; editMode,typeofentry$,bankGl$,transDate)
	! pr 'selx=';selx
	! pr 'editMode=';editMode
	! pause
	ScrMainTop: !
	if selx=4 and ~editMode then gosub ScrPayroll : goto SmFinis
	fnTos ! r:
	mylen=18 : mypos=mylen+3
	fnLbl(3,1,"Date:",mylen,1)
	fnTxt(3,mypos,8,0,1,"1",0,"Transaction date must always be answered.",0 )
	resp$(1)=str$(tr(4))
	if selx=3 then
		fnLbl(4,1,"Amount:",mylen,1)
	else
		fnLbl(4,1,"Net Amount:",mylen,1)
	end if
	dim message$*100
	fnLbl(4,36,message$,50,left)

	fnTxt(4,mypos,13,0,1,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	if selx=3 then resp$(2)='' else resp$(2)=str$(transactionAmt)
	fnLbl(5,1,"Reference Number:",mylen,1)
	fnTxt(5,mypos,12,0,0,'',0,"Enter check number, receipt number or adjusting entry number",0)
	resp$(3)=tr$
	if selx=2 or selx=3 or selx=5 then
		fnLbl(6,1,"Description:",mylen,1) ! for receipts
		fnTxt(6,mypos,30,0,left,'',0,"Brief description of transaction.",0 )
		resp$(4)=td$
	else
		fnLbl(6,1,"Payee Number:",mylen,1)
		! if disable_payee=1 then
		! 	fnTxt(6,mypos,8,0,1,'',1,"Payee field disabled. Click 'Enable Payee' again to enable.",0 )
		! 	resp$(4)=''
		! else
			fncombof("Paymstrcomb",6,mypos,35,"[Q]\GLmstr\PayMstr.h[cno]",1,8,9,39,"[Q]\GLmstr\payidx1.h[cno]",0,0, "If the payee # is known, the general ledger information can be extracted from that record.",0)
			resp$(4)=vn$
		! end if
	end if
	fnLbl(7,1,"General Ledger:",mylen,1)
	fnqgl(7,mypos,0,2)
	resp$(5)=fnrgl$(gl$)
	if selx=3 then
		fnLbl(7,60,"Net Adj:",8,1)
		fnTxt(7,70,13,0,1,"10",1,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
		resp$(6)=str$(totalalloc)
	else
		fnLbl(7,60,"Amount:",8,1)
		fnTxt(7,70,13,0,1,"10",0,"Amount to allocated to this general ledger number. Not applicable to adjustments.",0 )
		resp$(6)=''
	end if
	seltype=tr(6) : if tr(6)>4 then seltype=tr(6)-2
	if editMode=1 then
		typeofentry_selected$=fn_transType$(seltype)
	else
		typeofentry_selected$=typeofentry$
	end if

	fnLbl(1,4,typeofentry_selected$,36,1)
	! fnLbl(1,4,"Type of Entry: "&typeofentry_selected$,36,1)
	fnLbl(1,38,"Bank Account: "&bankname$,50,1)
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
	restore #hAllocations:
	dim allocItem$(4)*30
	mat allocItem$(4)
	do  ! READ_GL_ALLOCATIONS: !
		read #hAllocations,using 'Form Pos 1,c 12,pd 10.2,c 30': allocgl$,allocamt,td$ eof EO_FLEX1
		allocItem$(1)=str$(rec(hAllocations))
		allocItem$(2)=allocgl$
		allocItem$(3)=str$(allocamt)
		allocItem$(4)=td$
		fnflexadd1(mat allocItem$)
	loop  !  goto READ_GL_ALLOCATIONS
	EO_FLEX1: ! /r
	if selx=1 or selx=6 then
		fnButton(6,61,"E&xtract",15,"Extracts general ledger numbers from payee records",1,8)
		if disable_payee=1 then payee_button$ ="Enable &Payee" else payee_button$ ="Disable &Payee"
		fnButton(6,72,payee_button$,16,"Allows you to disable or enable the payee field.",1,12)
		fnButton(6,87,"&Add Payee",17,"Allows you to add a payee record.",1,10)
	end if
	! fnButton(9,78,"Add",ck_breakdownAdd=20,"Add an allocation.",1,5)
	fnButton(9,85,"&Edit",ck_breakdownEdit=18,"Correct an allocation.",1,5)
	fnButton(9,92,"Edit Al&l",19,"Edit all allocations without returning to this screen.",1,8)
	! fnLbl(17,73,'',1,1)
	! fnLbl(16,1," ")
	! If editMode=1 Then fnCmdKey("C&hange Acct #",9,0,0,'')
	if editMode=1 then
		fnCmdKey("&Save",ck_save=30,1,0,"Completed making corrections to this transaction.")
	else
		fnCmdKey("&Next Transaction",ck_nextTransaction=1,1,0,"You are completed with this transaction and ready to move to the next transaction.")
	end if
	! fnCmdKey("more Breakdowns",2,0,0,"More breakdowns to the same transaction.")
	! fnCmdKey("&Review Transactions",3,0,0,"Prints a list of all transactions entered during the setting and also provides for edit options.")
	fnCmdKey("&Delete",ck_delete=7,0,0,"Deletes the entire transaction as shown on screen.")
	! fnCmdKey("&Back",6,0,0,"Return to first screen to change transaction types or bank accounts.")
	! if ~editMode then fnCmdKey("&Finish",ck_finish=9,0,1,'')
	fnCmdKey("&Cancel",ck_cancel=6,0,1,'')
	! /r
	ckey=fnAcs(mat resp$)
	allocamt=0
	message$=''
	if extract=1 and ckey<>ck_nextTransaction then extract=0
	if (ckey=ck_finish or ckey=3) and sel1=3 and val(resp$(2))<>0 then ckey=ck_nextTransaction ! force the last entry to write   ! KJ 50707
	if ckey=ck_save and sel1=3 and val(resp$(2))<>0 then ckey=ck_nextTransaction ! force the last entry to write   ! KJ 50707
	if (ckey=ck_finish or ckey=3 or ckey=6) and lrec(hAllocations)>0 and ~editMode then goto SmGlNoTestFinis ! 4430 ! unwritten record on screen

	if ckey=ck_cancel then ! Back (Return to first screen to change transaction types or bank accounts.)
		transactionAmt=0
		tr$=''
		goto SmFinis
	else
		tr(4)=val(resp$(1))            ! date
		transactionAmt=val(resp$(2))   	! amount
		tr$=resp$(3)                    	! ref #
		vn$=vn$=resp$(4)(1:8)          	! payee number

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
		
		allocgl$=fnagl$(resp$(5))
		allocamt=val(resp$(6))
	end if

	if ckey=ck_finish and val(resp$(2))=0 then
		goto SmFinis
	! else if ckey=ck_breakdownAdd then
	! 	pause
	else if ckey=ck_delete then
		fn_transactionDelete(hMerge,mergeRec, hAllocations)
		goto ScrMainTop
	else if ckey=16 then
		if disable_payee=0 then disable_payee=1 else disable_payee=0
		goto ScrMainTop
	else if ckey=15 and ~editMode then
		extract=1
		gosub GetAllocationsFromPayee
		goto ScrMainTop
	end if

	if selx=3 and ckey<>ck_save then allocamt=transactionAmt ! create an allocation amount automatically on adjustments
	if allocamt=0 and ckey=ck_nextTransaction and lrec(hAllocations)=0 then allocamt=transactionAmt ! allows them to press enter if only allocation without haveing to key the amount a second time

	! r: GL Number Test
	if extract=1 then goto SmGlNoTestFinis
	if ckey=ck_breakdownEdit then goto SmGlNoTestFinis ! don't require gl # when editing transaction
	if ckey=ck_save and editMode=1 then goto SmGlNoTestFinis ! don't require gl # when editing and take complete
	if selx=3 and (allocamt=0 or ckey=ck_save) then goto SmGlNoTestFinis ! don't require gl # on adjustments when amount=0 (actually changing from one adjustment to the next)
	if selx=4 and editMode=1 then goto SmGlNoTestFinis ! never should have an amount on payroll check. (done from another screen and edits all made from allocation screen
	if ckey=17 then goto SmGlNoTestFinis
	x=val(allocgl$) conv SmGlTestFail
	if val(allocgl$)=0 and ~editMode then
		! KJ 080608   DON'T CHECK FOR GENERAL NUMBER ON EDIT
		SmGlTestFail: !
		mat ml$(3)
		ml$(1)="You must have a General Ledger Number"
		ml$(2)="on each allocation."
		ml$(3)="Click OK to enter the general ledger number."
		fnmsgbox(mat ml$,resp$,'',49)
		goto ScrMainTop
	end if
	goto SmGlNoTestFinis
	SmGlNoTestFinis: ! 
	! /r

	if ~editMode=1 then ! DON'T CHANGE CODE IF MAKEING CORRECTIONS
		if selx=1 then tr(6)=1 ! reg disb
		if selx=2 then tr(6)=2 ! reg receipt
		if selx=3 then tr(6)=3 ! adj
		if selx=4 then tr(6)=4 ! payroll check
		if selx=5 then tr(6)=7 ! sales
		if selx=6 then tr(6)=8 ! purchases
	end if
	if ckey=17 then
		fnaddglpayee
		goto ScrMainTop
	else if ckey=ck_breakdownEdit then
		fn_editAllocation(val(resp$(7)))
		goto ScrMainTop
	else if ckey=19 then
		fn_editAllocation(val(resp$(7)), 1)
		goto ScrMainTop
	else if ckey=ck_save then
		goto L4460
	else if ckey=2 and allocamt<>0 then
		write #hAllocations,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocamt,td$
		allocgl$=''
		totalalloc+=allocamt
		allocamt=0
		goto ScrMainTop
	else if ckey=ck_nextTransaction and allocamt<>0 then
		write #hAllocations,using "form pos 1,c 12,pd 10.2,c 30": allocgl$,allocamt,td$
		allocgl$=''
		totalalloc+=allocamt
		allocamt=0
	end if
	if ckey=ck_nextTransaction and selx <>3 and transactionAmt<>totalalloc then message$= "Allocations don't add up!" : goto ScrMainTop
	! if ckey=ck_nextTransaction and selx=3 and totalalloc <>0 then message$= "Allocations don't add up!": Goto ScrMainTop
	if ckey=ck_nextTransaction or ckey=2 or (ckey=ck_finish and ~editMode and lrec(hAllocations)>0) then  ! ckey 9 (ck_finish) thing: allow last transaction to be written if take finish
		L4460: !
		restore #hAllocations:
		for j=1 to lrec(hAllocations)
			read #hAllocations,using "form pos 1,c 12,pd 10.2,c 30,pd 5": allocgl$,allocamt,td$,transadr eof L4520
			tr(1)=val(allocgl$(1:3))
			tr(2)=val(allocgl$(4:9))
			tr(3)=val(allocgl$(10:12))
			tr(5)=allocamt
			fn_transactionSave(hMerge,transadr,mat tr,tr$,td$,vn$,mat jv$,key$)
		next j
		L4520: !
		goto SmFinis ! ScreenOne
	else
		goto ScrMainTop
	end if
	if ckey=ck_finish then 
		goto SmFinis
	else if ckey=2 then 
		goto ScrMainTop
	else
		fn_clearVar(hAllocations,transactionAmt,td$,tr$,vn$,gl$,totalalloc,gl_retainFieldsDuringAdd$)
		if ckey=ck_cancel then ! 6
			transactionAmt=0
			tr$=vn$=''
			goto SmFinis
		end if
		goto ScrMainTop
	end if
	SmFinis: !
fnend
ScrPayroll: ! r:
	! selx=4 ! this section is only for Payroll type selx is already 4.
	! pr 'ScrPayroll here' : pause
	fnTos
	mylen=18 : mypos=mylen+3
	fnLbl(3,1,"Date:",mylen,1)
	fnTxt(3,mypos,8,0,1,"1001",0,"Transaction date is required.",0 )
	resp$(1)=str$(tr(4))
	fnLbl(4,1,"Net Amount:",mylen,1)
	fnLbl(4,36,message$,50,left)
	fnTxt(4,mypos,12,0,1,"10",0,"Enter the net transaction amount. If correcting a transaction, change the allocations and net will be adjusted accordingly.",0 )
	resp$(2)=str$(transactionAmt) ! if selx=3 then resp$(2)='' else resp$(2)=str$(transactionAmt)
	fnLbl(5,1,"Reference:",mylen,1)
	fnTxt(5,mypos,12,0,0,'',0,"Enter check number.",0)
	resp$(3)=tr$
	fnLbl(6,1,"Employee:",mylen,1)
	fncombof("PRmstr",6,mypos,35,"[Q]\GLmstr\PRmstr.h[cno]",1,4,5,30,"[Q]\GLmstr\PRINDEX.h[cno]",1,0, "Choose from the list of employees.  Click Add Employee to add a new employee not shown on list.",0)
	resp$(4)=str$(prx(1))
	fnLbl(7,1,"General Ledger:",mylen,1)
	fnqgl(7,mypos,0,2)
	resp$(5)=fnrgl$(gl$)
	fnLbl(7,60,"Amount:",mylen,1) ! if selx=3 then fnLbl(7,60,"Net Adj:",mylen,1) else fnLbl(7,60,"Amount:",mylen,1)
	fnTxt(7,70,13,0,1,"10",1,"Amount to allocated to this general ledger number.",0 )
	resp$(6)=''
	fnLbl(1,4,typeofentry$,36,1)
	fnLbl(1,38,"Bank Account: "&bankname$,50,1)
	fnFra(8,1,10,70,"Payroll Breakdown","Enter the check breakdown.")
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
		fnLbl(j+5,1,trim$(miscname$(j))&":",mylen,1,0,1)
		fnTxt(j+5,22,12,0,1,"10",0,"Total "&trim$(miscname$(j))&" (enter as a positive figure).",1)
		resp$(j+11)=str$(prx(j+6))
	next j
	for j=6 to 10
		fnLbl(j-5,30,trim$(miscname$(j))&":",mylen,1,0,1)
		fnTxt(j-5,51,12,0,1,"10",0,"Total "&trim$(miscname$(j))&" (enter as a positive figure).",1)
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

	if editMode=1 then
		fnCmdKey("&Complete",30,1,0,"Completed making corrections to this transaction.")
	else
		fnCmdKey("&Next Transaction",1,1,0,"You are completed with this transaction and ready to move to the next transaction.")
	end if
	fnCmdKey("&Delete",7,0,0,"Deletes the entire transaction as shown on screen.")
	fnCmdKey("&Back",6,0,0,"Allows you to return to screen 1 and change transaction types or bank accounts.")
	fnCmdKey("&Finish",9,0,1,'')
	ckey=fnAcs(mat resp$)
	if ckey=9 then
		goto ScrPayrollXit
	else if ckey=6 then
		transactionAmt=0
		mat prx=(0)
		tr$=''
		vn$=''
		goto ScrPayrollXit
	! else if ckey=3 then
	! 	gosub ScrTransactionsCorrections : goto ScrMainTop
	end if
	tr(4)=val(resp$(1)) ! date
	tr(5)=transactionAmt=val(resp$(2)) ! amount
	tr$=resp$(3) ! ref #
	prx(1)=val(resp$(4)(1:4)): vn$=resp$(4)(1:4) ! employee number
	td$=resp$(4)(5:30) ! transaction description = employee name
	dim empName$*30
	empName$=td$
	gl$=fnagl$(resp$(5))
	for j=2 to 19
		prx(j)=val(resp$(j+5))
	next j
	wh=0
	for j=1 to 19
		if j>2 and j<7 then wh+=prx(j)
		if j<7 or j>16 then 
			goto L7300
		else
			if dedcode(j-6)=2 then wh-=prx(j) else wh+=prx(j)
		end if
		L7300: !
		if j=17 then wh+=prx(j)
		if j=19 then wh-=prx(j)
	next j
	if tr(5)=prx(2)-wh then 
		gosub WritePayrollTrans
		goto ScrPayrollXit
	end if
	mat ml$(3)
	ml$(1)="Total wages less deductions do not equal the net check!"
	ml$(2)=" Net entered:" &ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",transactionAmt))&"   Calculated net: "&ltrm$(cnvrt$("PIC($$$$,$$$.##CR)",prx(2)-wh))
	ml$(3)="Click ok to return to the entry screen."
	fnmsgbox(mat ml$,resp$,'',49)
	goto ScrPayroll
	ScrPayrollXit: !
return

 ! /r
WritePayrollTrans: ! r:
	tr(6)=4 ! payroll transaction type  (was converted back to 1 in old system)
	for j=2 to 19
		jv$(2)=str$(j-1) ! carry breakdown code for posting employee record
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
			write #hMerge,using F_merge: allocgl$,tr(4),prx(j),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ ! gross wage
		end if
	next j
	transactionAmt=0
	mat prx=(0)
	vn$=''
	tr$=fn_nextTr$(tr$,gl_retainFieldsDuringAdd$)
return ! /r
def fn_clearContras(hMerge; ___,gl$,tr4,tr5,tr6,tr7,tr$*12)
	do
		read #hMerge,using F_merge: gl$,tr4,tr5,tr6,tr7,tr$ eof CfcEoF
		if trim$(tr$)="999999999999" then delete #hMerge:
	loop
	CfcEoF: !
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
def fn_editAllocation(editrecord; editall,___,ckey,mylen,mypos)
	! editing glallocation while still being entered into allocation grid
	! editrecord=val(resp$(7))
	! if editall=19 then editrecord=1
	do
		read #hAllocations,using "Form pos 1,c 12,pd 10.2,c 30,pd 5",rec=editrecord: gl$,allocation,td$,transadr noRec EA_FINIS
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
			if ckey=6 then delete #hAllocations,rec=editrecord:
			delete #hMerge,rec=transadr: ioerr ignore
			transactionAmt+=-holdallocation
		else
			allocgl$=fnagl$(resp$(2))
			allocation=val(resp$(1))
			transactionAmt+=allocation-holdallocation ! update net amount of transaction
			td$=resp$(3)
			rewrite #hAllocations,using "Form pos 1,c 12,pd 10.2,c 30",rec=editrecord: allocgl$,allocation,td$
			totalalloc+=allocation-holdallocation
		end if
		editrecord+=1
	loop while editall=1
	EA_FINIS: !
	! editall=0
	fn_editAllocation=ckey
fnend

def fn_prProofTotals(hAccount,totalDebits,totalCredits, _
		typeofentry$,bankgl$,contraEntryDateN, _
		mat kList$,mat kVal,mat chdr_proof_total$,mat glitem3$ _
		; ___,j,mylen)

	mylen=20
	for j=1 to 30
		read #hAccount,using "form pos 87,pd 6.2",key=kList$(j),release: kVal(j,4) nokey PPT_L6240 ! get last balance
		kVal(j,8)=kVal(j,4)-kVal(j,5)-kVal(j,6)+kVal(j,7) ! new balance when posted
		PPT_L6240: !
	next j
	fnopenprn
	pr #255: lpad$("Total Debits:",mylen)&' '&cnvrt$("pic(-------,---,---.##)",totalDebits)
	pr #255: lpad$("Total Credits:",mylen)&' '&cnvrt$("Pic(-------,---,---.##",totalCredits)
	pr #255: ''
	pr #255: lpad$("Type of Entry:",mylen)&' '&typeofentry$
	pr #255: lpad$("Bank Account:",mylen)&' '&fnrgl$(bankgl$)
	pr #255: lpad$("Process Ending Date:",mylen)&' '&cnvrt$("pic(zz/zz/zz)",contraEntryDateN)
	pr #255: ''
	pr #255,using F_PPT_LINE: mat chdr_proof_total$
	F_PPT_LINE: form pos 1,c 12,5*cr 19
	mat glitem3$=('')
	for j=1 to 30
		if trim$(kList$(j))<>'' then ! skip blanks
			glitem3$(1)=kList$(j)
			glitem3$(2)=cnvrt$("Pic(-------,---,---.##)",kVal(j,4)) ! cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(5)="10"
			glitem3$(3)=cnvrt$("Pic(-------,---,---.##)",-kVal(j,5))
			glitem3$(4)=cnvrt$("Pic(-------,---,---.##)",kVal(j,6))
			glitem3$(5)=cnvrt$("Pic(-------,---,---.##)",kVal(j,7))
			glitem3$(6)=cnvrt$("Pic(-------,---,---.##)",kVal(j,8)) ! cmask3$(6)='10'
			pr #255,using F_PPT_LINE: mat glitem3$
		end if
	next j
	fncloseprn
fnend
def fn_prProofList(hMerge; ___,holdtr$,tr$)
	fnopenprn
	gosub PrProofListHeader
	holdtr$=tr$=''
	restore #hMerge:
	do
		holdtr$=tr$
		read #hMerge,using F_merge: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,mat jv$,key$ eof PE_FINIS
		if trim$(holdtr$)<>'' and holdtr$<>tr$ then
			pr #255,using "Form pos 10,c 10,n 14.2,skip 1": "Net",netamount
			netamount=0
		end if
		if tr(6)=3 then prntkey$='' else printkey$=key$
		pr #255,using F_PE_LINE: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,vn$,printkey$,mat jv$ pageoflow PrProofListPgOf
		F_PE_LINE: form pos 1,c 12,x 2,pic(zz/zz/zz),n 11.2,x 4,pic(zz),pic(zz),c 13,c 30,c 10,c 12,c 7,c 7,c 41
		netamount+=tr(5)
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

def fn_getPost(hMerge; ___,pc,adr,returnN)
	! reads last record of hMerge
	pc=1
	adr=lrec(hMerge)
	do
		if adr>1 then
			L400: !
			read #hMerge,using 'form pos 27,n 2',rec=adr: pc noRec GpePriorRec ! conv GpeFinis
			if pc=0 then returnN=1 ! if previous batch not posted, set returnN=1
			if pc>0 then goto GpeFinis ! PREVIOUS TRANSACTIONS HAVE BEEN POSTED
			goto GpeFinis
		end if
		goto GpeFinis
		GpePriorRec: !
		adr-=1
	loop while adr>0
	GpeFinis: !
	fn_getPost=returnN
fnend
def fn_buildMatK(hMerge,mat kList$,mat kVal,&totalCredits,&totalDebits,&count; ___,key$*12,tr$*12,td$*30,vn$*8,tranType)
	!  accumulate proof totals (mat kVal, totalCredits, totalDebits)
	mat kList$=("            ")
	mat kVal=(0)
	totalDebits=totalCredits=count=0
	restore #hMerge:
	do
		read #hMerge,using "Form POS 1,c 12,N 6,PD 6.2,N 2,N 2,C 12,C 30,C 8 ,Pos 93,C 12": gl$,tr(4),tr(5),tranType,tr(7),tr$,td$,vn$,key$ eof FinisBuildMatK
		!   pr 'read an entry from work file:'&gl$&' - '&key$ : pause
		count+=1

		for j=1 to 30
			if kList$(j)<>'' and key$=kList$(j) then goto AddIntoMatK ! found matching contra account
			if trim$(kList$(j))='' then kList$(j)=key$: goto AddIntoMatK ! found a blank contra and no previous match
		next j
		goto FinisBuildMatK
		AddIntoMatK: !
		if kList$(j)=key$ then
			if tranType=1 or tranType=4 or tranType=8 then
				! 1 = Disbursements
				! 4 = Payroll Check
				kVal(j,6)+=tr(5) ! total debits entered
				totalDebits+=tr(5)
				totalCredits-=tr(5)
			else if tranType=2 or tranType=7 then
				! 2 = Receipts
				kVal(j,5)+=tr(5) ! total credits entered
				totalCredits+=tr(5)
				totalDebits-=tr(5)
			else if tranType=3 then
				! 3 = Adjustments
				kVal(j,7)+=tr(5) ! net adjustments
				if tr(5)<0 then totalCredits+=tr(5)
				if tr(5)>0 then totalDebits+=tr(5)
			else
				! 5 = Sales
				! 6 = Purchases
				pr 'how should i total up this trans type ('&str$(tranType)&')'
				! pause
			end if
		end if
	loop
	FinisBuildMatK: !
fnend

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
	gosub CloseFiles
	fnCopy(dv$&"GLWK101.h[cno]","[Q]\GLmstr\GL_Work_[acsUserId].h[cno]")
	fn_openFiles(1)
fnend
def fn_eraseAllPreviousInput
	gosub CloseFiles
	fnCopy('[Q]\GLmstr\GL_Work_[acsUserId].h[cno]','[temp]\acs_Enter_Transactions_[acsUserId]_batchDelete_[datetime].h[cno]')
	fn_openFiles( 1)
fnend
def fn_openFiles(; reset,___,useOrReplace$)
	! close #hAllocations: ioerr ignore
	! open #hAllocations=fnH: "Name=[Q]\GLmstr\Allocations[acsUserId].h[cno],Version=1,replace,RecL=59",internal,outIn,relative
	if reset then
		useOrReplace$='Replace'
		fnFree('[Q]\GLmstr\Allocations[acsUserId].h[cno]')
	else
		useOrReplace$='Use'
	end if
	dim tr(7)
	dim tr$*12 ! transaction reference number
	dim td$*30 ! transaction description
	dim jv$(3)*8
	dim key$*12
	F_merge: form pos 1,c 12,n 6,pd 6.2,n 2,n 2,c 12,c 30,c 8,c 6,c 5,c 3,c 12
	open #hMerge=fnH: 'Name=[Q]\GLmstr\GL_Work_[acsUserId].h[cno],RecL=104,'&useOrReplace$,internal,outIn,relative
	open #hPayeeGl=fnH: "Name=[Q]\GLmstr\PayeeGLBreakdown.h[cno],Version=1,KFName=[Q]\GLmstr\Payeeglbkdidx.h[cno],Use,RecL=56,KPs=1,KLn=8,Shr",internal,outIn,keyed
	open #hAccount=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
fnend
CloseFiles: ! r:
	close #hMerge: ioerr ignore
	close #hPayeeGl: ioerr ignore
	close #hAccount: ioerr ignore
return ! /r
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
	fn_createContraEntries(hMerge,mat kList$,mat kVal,contraEntryDateN)
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
def fn_createContraEntries(hMerge,mat kList$,mat kVal,contraEntryDateN; ___,j,tr$*12)
	restore #hMerge:
	tr$="999999999999"
	for j=1 to 30
		if val(kList$(j))<>0 then
			if kVal(j,5)<>0 then write #hMerge,using F_merge: kList$(j),contraEntryDateN,-kVal(j,5),2,0,tr$,"Contra Entry",'','','','' ! debits from receipts or sales      ! ,rec=lrec(hMerge)+1
			if kVal(j,6)<>0 then write #hMerge,using F_merge: kList$(j),contraEntryDateN,-kVal(j,6),1,0,tr$,"Contra Entry",'','','','' ! credits from checks or purchases   ! ,rec=lrec(hMerge)+1
		end if
	next j
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
def fn_transType$(key; ___,hTt,which,return$*18)
	if ~setupTransTypeC then
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
	end if
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
