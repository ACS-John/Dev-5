! r: SETUP: fnTop, dims, open files, etc
	autoLibrary
	fnTop(program$)
	on error goto Ertn

	dim jobdesc$*30,jn$*6,l(11),ta(2),jobname$*25,jobitem$(6)*30
	dim in1$(9),de$*30,ta(2)
	dim pr$(4)*30,t1(5),up$(4),unpaidkey$*20
	dim d(2),sn$*50
	dim jn$*6,cn$*11,l(13)
	dim contact$*30,ph$*12,email$*50,fax$*12,myact$*20
	dim chdr$(16),cmask$(16),item$(16)*21 ! used with flex grid
	dim gldesc$*30,ml$(3)*80
	dim item1$(3)*15,type$*25
	dim holdkey$*20,resp$(256)*128

	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input,relative
	read #20,using 'Form POS 150,2*N 1,C 2',rec=1: mat d,bc$
	if d(1)=0 and d(2)=0 then
		glnMask$='50'
	else if d(1)=1 and d(2)=0 then
		glnMask$='51'
	else if d(1)=0 and d(2)=1 then
		glnMask$='52'
	else if d(1)=1 and d(2)=1 then
		glnMask$='53'
	end if
	close #20:

	bankcode=val(bc$)
	open #bankmstr=fnH: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal, outin, keyed
	read #bankmstr,using 'Form POS 45,PD 6.2,PD 6.2',key=bc$,release: bal,upi nokey ignore
	close #bankmstr:
	open #glmstr=fnH: "Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	open #paymstr1=13: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",internal,outIn,keyed
	open #paymstr2=14: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx2.h[cno],Shr",internal,outIn,keyed
	open #payeegl=17: "Name=[Q]\CLmstr\payeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\Payeeglbkdidx.h[cno],Shr",internal,outIn,keyed
	open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed
	open #unpdaloc=5: "Name=[Q]\CLmstr\UnPdAloc.h[cno],KFName=[Q]\CLmstr\Uaidx2.h[cno],Shr",internal,outIn,keyed
	t1(1)=bal : upi=t1(5) : t1(3)=t1(1)-t1(2)
	if fnregistered_for_job_cost_pr then havejc=1 : gosub JCBLD
goto MENU1 ! /r
MENU1: ! r:
	mat chdr$(16) : mat cmask$(16) : mat item$(16)
	chdr$(1)='Ref': chdr$(2)='Payee': chdr$(3)='Invoice'
	chdr$(4)='Date'
	chdr$(5)='Due Date' : chdr$(6)='P O #'
	chdr$(7)='Description' : chdr$(8)='Amount'
	chdr$(9)='Disc Amt' : chdr$(10)='Disc Date'
	chdr$(11)='Pay Code' : chdr$(12)='Bank'
	chdr$(13)='ChkNum' : chdr$(14)='Date Paid'
	chdr$(15)='Post Code' : chdr$(16)='Post Date'
	cmask$(1)="30"
	cmask$(2)="": cmask$(3)="" : cmask$(4)='1'
	cmask$(5)='1' : cmask$(6)='': cmask$(7)=''
	cmask$(8)='10' : cmask$(9)='10' : cmask$(10)='1'
	cmask$(11)='30': cmask$(12)='30'
	cmask$(13)='30': cmask$(14)='3'
	cmask$(15)='30': cmask$(16)='1'
DISPLAY_INVOICE_GRID: !
	fnTos
	respc=0
	frame=0
	fnflexinit1('UnpaidFile',1,1,20,85,mat chdr$,mat cmask$,1,0)
	restore #paytrans:
READ_INVOICE_GRID: ! r: read unpaid invoice file and populate the grid
	read #paytrans,using 'Form POS 1,C 8,c 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof EO_INVOICE_GRID noRec L970
	item$(1)=str$(rec(paytrans))
	item$(2)=vn$: item$(3)=iv$: item$(4)=up$(1)
	item$(5)=up$(2) : item$(6)=up$(3) : item$(7)=up$(4)
	item$(8)=str$(upa) : item$(9)=str$(disamt) : item$(10)=str$(ddate)
	item$(11)=str$(pcde) : item$(12)=str$(bcde)
	item$(13)=str$(ckn) : item$(14)=str$(dp)
	item$(15)=str$(gde) : item$(16)=str$(pdte)
	fnflexadd1(mat item$)
! transactionstotal+=upa
L970: !
	goto READ_INVOICE_GRID
EO_INVOICE_GRID: ! /r
	if havejc=1 then
		fnCmdKey("&Review Job Cost Entries",9,0,0,"Allows you to review and/or post any job cost allocations you have made.")
	end if
	fnCmdKey("&Add",1,0,0,"Allows you to add new unpaid invoice records.")
	fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing unpaid invoice record.")
	fnCmdKey("&Select to Pay",8,0,0,"Allows you to code invoices for payment")
	fnCmdKey("&Listing",3,0,0,"Prints listings from unpaid file")
	fnCmdKey("E&Xit",5,0,1,"Exits to main menu") ! 320
	fnAcs(mat resp$,ckey)
	displayalljobs=0
	if ckey=5 then goto FINIS
	! screen=0
	if ckey=2 then edit=1 : RecordNumberToEdit=val(resp$(1)) else edit=0 : RecordNumberToEdit=0
	if (ckey=1 or ckey=2) then let fn_addInvoice(vn$,iv$,RecordNumberToEdit) : goto menu1
	if ckey=3 then gosub PRINTLISTING : goto DISPLAY_INVOICE_GRID ! pr listings of unpaid invoice file
	if ckey=8 then goto CODE_FOR_PAYMENT ! select invoices to payment
	if ckey=9 then
		displayalljobs=1
		jn$="" : iv$="" : vn$=""
		subcat=0 : cat=0
		gosub JOBCOST
		goto DISPLAY_INVOICE_GRID
	end if
	pause
! ! /r
PRINTLISTING: ! r: pr listings
! need screen here asking paid/unpaid/all,starting inv #,1st inv to print,show breakdowns(y/n),display last ref in file, 1st ref # used this time
! need ref # to begin pr (blank for all)   rf2    show last ref  lrec(4)
	fnopenprn
	pg=0
	if rf2=0 then rf2=1
	gosub HDR
	t1=0
	for j=rf2 to lrec(4)
		read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,G 1,G 2,G 8,G 6,N 1',rec=j,release: mat in1$,ckn,dp,gde noRec NEXTRECORD
		if rtrm$(in1$(1))="" then goto NEXTRECORD
		t0=val(in1$(7)) conv L1440
		t1=t1+t0
L1440: !
		pr #255,using 'Form POS 1,N 4,X 2,C 10,C 14,2*C 8,C 14,C 20,C 12,C 6,G 4,N 10,N 8': j,mat in1$,ckn,dp pageoflow NEWPGE
		aa2=0
!   r5=aa(1)
		restore #unpdaloc,key>=in1$(1)&"            ": nokey NEXTRECORD
L1480: !
		read #unpdaloc,using 'Form POS 1,c 8,c 12,c 12,PD 5.2,C 30',release: vnkey$,vniv$,gl$,aa,de$ eof NEXTRECORD
		if in1$(1)<>vnkey$ then goto NEXTRECORD ! not same vendor
		if in1$(2)<>vniv$ then goto L1480 ! not same invoice
		aa2=aa2+aa
		pr #255,using 'Form POS 47,c 12,X 2,C 20,N 10.2': gl$,de$(1:20),aa pageoflow NEWPGE
		goto L1480
		pr #255:
NEXTRECORD: !
	next j
	pr #255,using 'Form POS 81,C 10,SKIP 1,POS 81,N 10.2,SKIP 1,POS 81,C 10,SKIP 1': "__________",t1,"=========="
!
	fncloseprn
	on fkey 99 ignore
	return ! /r
NEWPGE: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
HDR: ! r:
	pg+=1
	fnopenprn
	pr #255,using 'Form POS 1,C 8,Cc 82': date$,env$('cnam')
	pr #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg,"Unpaid Invoice File Listing"
	pr #255: ""
	pr #255: "                             Invoice    Due     PO Number                                   Pay   Bank   Check     Date "
	pr #255: "Ref#  Payee #   Invoice Numb   Date    Date     GL Number   Description            Amount   Code  Code   Number    Paid "
	pr #255: "____  ________  ____________  ______  ______  ____________  __________________  __________  ____  ____  ________  ______"
	return  ! /r
!
FINIS: ! r:
	if havejc=1 and lrec(jcbreakdown)>0 then
		mat ml$(3)=("")
		ml$(1)="It appears you have "&str$(lrec(jcbreakdown))&"job cost entries"
		ml$(2)="that have not been posted.  Do you wish to post these"
		ml$(3)="entries before you exit?"
		fnmsgbox(mat ml$,resp$,'',4)
		if resp$="Yes" then gosub POST_TO_JOB
	end if
goto Xit ! /r
Xit: fnXit
!
JCBLD: ! r: Open JC Files
	mat chdr3$(6) : mat cmask3$(6) : mat jobitem$(6)
	chdr3$(1)='Refenence'
	chdr3$(2)='Job #'
	chdr3$(3)='Cat #'
	chdr3$(4)='Sub-Cat #'
	chdr3$(5)='Amount'
	chdr3$(6)='Description'
	cmask3$(5)='10' : cmask3$(1)=cmask3$(2)=cmask3$(3)=cmask3$(4)=cmask3$(6)=''
	open #41: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed ioerr JCBLD_FINIS
	open #category:=2: "Name=[Q]\PRmstr\JCCAT.h[cno], KFName=[Q]\PRmstr\CATIndx.h[cno],Shr", internal,outIn,keyed
	open #43: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,outIn,keyed
	open #45: "Name=[Q]\PRmstr\JCTrans.h[cno],Shr",internal,outIn,relative
	if not exists("[Q]\CLmstr\JCBreakdownS"&wsid$&".h[cno]") then gosub MAKE_JCB
	open #jcbreakdown=46: "Name=[Q]\CLmstr\JCBreakdownS"&wsid$&".h[cno],KFName=[Q]\CLmstr\JcBrkidx"&wsid$&".h[cno],Version=1,Shr",internal,outIn,keyed ioerr MAKE_JCB
JCBLD_FINIS: !
	return  ! /r
!
CODE_FOR_PAYMENT: ! r:
	lastrec=nextrec=total=0
	displayattop$="True"
	close #clearing: ioerr ignore
	open #clearing=fnH: "Name=[Q]\CLmstr\clearing.H"&wsid$&",replace,RecL=114",internal,outIn,relative  ! kj wrong recl
	if displayunpaid=1 then
		type$="Coded for Payment"
	else if displayunpaid=0 then
		type$="Approved and Unapproved"
	else if displayunpaid=2 then
		type$="Not Approved for Payment"
	end if
	close #paytrans: ioerr ignore
	open #paytrans=4: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Shr",internal,outIn,keyed
L4700: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof DISPLAY_GRID
	if displayunpaid=1 and pcde=1 then goto L4760 ! if only choose selected, don't allow others to list
	if displayall=1 then goto L4760
	if displayunpaid=2 and pcde=0 then goto L4760 ! if only choose selected, don't allow others to list
	if displayunpaid<>0 then goto L4700 ! if displayed has an answer,but no match go back
! If PCDE<>0 Then Goto 14820  ! go back to read record in don't want selected invoices to show on grid  (not sure how to default)
L4760: write #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
	if pcde=1 and displayunpaid<>2 then total+=upa ! total paids
	if pcde=0 and displayunpaid=2 then total+=upa ! total unpaids
	goto L4700
DISPLAY_GRID: !
	mat chdr$(16) : mat cmask$(16) : mat flxitm$(16)
	chdr$(1)="Rec" : chdr$(2)="Bank" : chdr$(3)="Pay"
	chdr$(4)="Payee #" : chdr$(5)="Invoice #"
	chdr$(6)="Inv Date" : chdr$(7)="Due Date"
	chdr$(8)="Description": chdr$(9)="Amount"
	chdr$(10)="Dis Amt" : chdr$(11)="Dis Date"
	chdr$(12)="BK Code"
	chdr$(13)="CkNo" : chdr$(14)="D Paid"
	chdr$(15)="P C" : chdr$(16)="P Date"
	cmask$(1)='30' : cmask$(2)='30' : cmask$(3)=""
	cmask$(4)=''
	cmask$(5)='' : cmask$(6)='1': cmask$(7)='1'
	cmask$(8)='': cmask$(9)='10' : cmask$(10)='10'
	cmask$(11)='3' : cmask$(12)='30' : cmask$(13)='30'
	cmask$(14)='1' : cmask$(15)='30' : cmask$(16)="1"
RE_DISPLAY_GRID: ! save a little time
	fnTos
	respc=0 : mat resp$=('')
fnFra(2,1,13,23,"Approval Options"," ")
fnButton(1,2,"&Approve All",62,"Will select to pay all unpaid invoices",1,18,1)
fnButton(3,2,"&Approve by Range",63,"Enter a range of reference numbers to approve.  The reference # is the number to the left assigned by the computer.",1,18,1)
fnLbl(4,4,"From:",5,1,0,1)
fnTxt(4,11,5,0,1,"30",0,"Select the first reference # to be approved",1)
resp$(respc_rangefrom:=respc+=1)=""
fnLbl(5,4,"To:",5,1,0,1)
fnTxt(5,11,5,0,1,"30",0,"Select the last reference # to be approved",1)
resp$(respc_rangeto:=respc+=1)=""
fnButton(7,2,"&Approve by Due Date",64,"Approve all invoices due by a certain date.",1,18,1)
fnLbl(8,2,"Date:",5,1,0,1)
fnTxt(8,8,8,0,1,"1",0,"All invoices with a due by date equal to or less than this date will be approved",1)
resp$(respc_duedate:=respc+=1)=""
fnButton(10,2,"Approve By Payee",66,"Approves all invoices with this payee number in invoice record.",1,18,1)
fnLbl(11,2,"Payee #:",8,1,0,1)
fnTxt(11,11,8,0,1,"",0,"Enter payee # to approve all invoices on that payee",1)
resp$(respc_payee:=respc+=1)=""
if displayunpaid=1 or displayunpaid=0 then
	wording$="Total Selected:"
else
	wording$= "Total Unapproved:"
end if
fnLbl(2,28,wording$,18,1)
fnTxt(2,49,12,0,1,"10",0," ")
resp$(respc_total:=respc+=1)=str$(total)
fnChk(3,47,"Display at Top:",1)
resp$(respc+=1)=displayattop$
	fnLbl(1,1,trim$(env$('cnam')(1:30))&"-"&type$,65,2)
	fnflexinit1('unpaidinv',5,27,15,55,mat chdr$,mat cmask$,1)
	respc_selectedrec=respc+=1
	restore #clearing:
	if nextrec>0 and displayattop$="True" then goto L4890 else goto L5030
L4890: for j=nextrec to lrec(clearing) ! read starting with next record
		read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L4940
		flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
		flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
		flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
		flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
		flxitm$(16)=str$(pdte)
		flxitm$(1)=str$(rec(clearing))
		if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
fnflexadd1(mat flxitm$)
L4940: next j
if nextrec=1 then goto L5020 ! thinks it rereads the 1st record twice
for j=1 to max(nextrec-1,1) ! read records previously coded or skipped
	read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
	flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
	flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
	flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
	flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
	flxitm$(1)=str$(rec(clearing))
	if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
fnflexadd1(mat flxitm$)
next j
L5020: goto L5070
L5030: !
read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': flxitm$(4), flxitm$(5), flxitm$(6),flxitm$(7), flxitm$(8), up$(4),upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5070
flxitm$(9)=str$(upa) : flxitm$(10)=str$(disamt)
flxitm$(11)=str$(ddate) : flxitm$(3)=str$(pcde)
flxitm$(2)= flxitm$(12)=str$(bcde): flxitm$(13)=str$(ckn)
flxitm$(14)=str$(dp) : flxitm$(15)=str$(gde)
flxitm$(16)=str$(pdte)
flxitm$(1)=str$(rec(clearing)) ! assign flxitm$(1) with new record #
if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
fnflexadd1(mat flxitm$) : goto L5030
L5070: !
fnCmdKey("&Approve Highlighted",1,1,0,"Approves or cancels the invoice that is highlighted.")
fnCmdKey("&Display All",9,0,0,"Displays all remaining records in the unpaid file.")
fnCmdKey("&Display Selected",3,0,0,"Displays all invoices selected for payment")
fnCmdKey("&Display UnSelected",2,0,0,"Displays all remaining uncleared invoices")
fnCmdKey("C&omplete",5,0,1,"Return to main unpaid invoice menu")
fnAcs(mat resp$,ckey)
displayunpaid=total=displayall=0
if ckey=5 or ckey=99 then goto MENU1
selectedrec=val(resp$(respc_selectedrec)) ! selected record from grid
rangefrom=val(resp$(respc_rangefrom)) ! if select range of reference numbers
rangeto=val(resp$(respc_rangeto)) ! if select range of reference numbers
duedate =val(resp$(respc_duedate)) ! used in selecting invoices by due date
payeevn$=resp$(respc_payee) ! payee number to select
total=val(resp$(respc_total)) ! total used for display only
displayattop$=resp$(7) ! display at top
if ckey=2 then displayunpaid=2: goto CODE_FOR_PAYMENT !                                                   redisplay on uncoded
if ckey=3 then displayunpaid=1: goto CODE_FOR_PAYMENT ! displays only                                       cleared on this date
if ckey=9 then displayall=1: goto CODE_FOR_PAYMENT ! displays everything                                 in unpaid file
if ckey=62 then goto PAY_ALL
if ckey=63 and rangefrom=0 then goto MSGBOX3
if ckey=69 then goto APPROVE_BY_RANGE
if ckey=64 and duedate=0 then goto MSGBOX4
if ckey=64 then goto CLEAR_BY_DUEDATE
if ckey=65 then goto APPROVE ! approve or unselect an invoice
if ckey=66 then goto APPROVE_BY_PAYEE ! approve all invoices for a specific payee
goto APPROVE ! /r  (used to just fall though to approve here)
APPROVE: ! r: clear or unclear selected invoices
	if selectedrec>0 then
		read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=selectedrec: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
		if pcde=0 then pcde=1 : newbcde=bankcode : goto L5540 ! if no previous payment code, use new one; if it has a payment code, change it
		if pcde=1 and dp=0 then pcde=0 : newbcde=0: goto L5540 ! change from yes to no
		if pcde=0 then pcde=1 : newbcde= bankcode: goto L5540 ! change from no to yes
		if pcde=1 and dp>0 then pcde=1 : newbcde=bcde: goto L5540 ! don't change previously paid
		L5540: !
		if pcde=1 then flxitm$(3)="Yes" else if pcde=0 then flxitm$(3)="No" else if pcde=1 and dp>0 then flxitm$(3)="Paid"
		! pr PCDE,BCDE
		rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=selectedrec: pcde,newbcde
		rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,newbcde ! update the transaction history
		lastrec=selectedrec
		if lastrec+1<=lrec(clearing) then nextrec=lastrec+1 else nextrec=1
	end if
goto RE_DISPLAY_GRID ! /r
MSGBOX3: ! r: need range of reference numbers
mat ml$(2)
ml$(1)="You must enter the 'Range From' and 'Range To'"
ml$(2)="reference numbers to choose this option."
fnmsgbox(mat ml$,resp$,'',16)
goto CODE_FOR_PAYMENT ! /r
MSGBOX4: ! r: need due date for selecting by due date
mat ml$(2)
ml$(1)="You must enter the 'Due Date' if you choose to'"
ml$(2)="approve by due date."
fnmsgbox(mat ml$,resp$,'',16)
goto CODE_FOR_PAYMENT ! /r
PAY_ALL: ! r: pay all unpaid invoices
restore #paytrans:
L5710: !
read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5760
if bcde=0 then bcde=bankcode
if pcde=0 then pcde=1: goto L5740
goto L5710
L5740: !
rewrite #paytrans,using 'Form POS 73,n 1,n 2': pcde,bcde ! update the transaction history
goto L5710
L5760: !
goto CODE_FOR_PAYMENT ! /r
APPROVE_BY_RANGE: ! r: clear by reference # range
for j=rangefrom to rangeto
	read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5860 noRec L5860
	if pcde>0 then goto L5850 ! already coded
	if pcde=0 then pcde=1
	if bcde=0 then bcde=bankcode ! don't change bank # if one                                                      previously entered
	rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,bcde ! update the transaction history
	rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=j: pcde,bcde ! update the transaction history
L5850: !
next j
L5860: !
goto CODE_FOR_PAYMENT ! /r
CLEAR_BY_DUEDATE: ! r: clear any invoices with due date less than or equal the one entered
for j=1 to lrec(clearing)
	read #clearing,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=j: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L5970 noRec L5970
	if val(up$(2))<=duedate then goto L5910 else goto L5960
L5910: !
	if pcde>0 then goto L5960 ! already coded
	if pcde=0 then pcde=1
	if bcde=0 then bcde=bankcode ! don't change bank # if one previously entered
	rewrite #paytrans,using 'Form POS 73,n 1,n 2',key=vn$ & iv$: pcde,bcde ! update the transaction history
	rewrite #clearing,using 'Form POS 73,n 1,n 2',rec=j: pcde,bcde ! update the transaction history
L5960: !
next j
L5970: !
goto CODE_FOR_PAYMENT ! /r
APPROVE_BY_PAYEE: ! r: select payee to pay
restore #paytrans:
L6000: read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate eof L6060 : lastrec=rec(paytrans)
if uprc$(lpad$(rtrm$(payeevn$),8))<>uprc$(vn$) then goto L6000
if pcde<>1 then pcde=1
if bcde=0 then bcde=bankcode ! don't change bank # if one                                                      previously entered
rewrite #paytrans,using 'Form POS 73,n 1,n 2',rec=lastrec: pcde,bcde ! update the transaction history
goto L6000
L6060: goto CODE_FOR_PAYMENT ! /r
JOBCOST: ! r:
dim jn$*6
ENTRY_SCREEN: !
fnTos
respc=0 : mat resp$=(''): lc=0: mylen=20: mypos=mylen+3
fnLbl(lc+=1,1,"Payee # "&trim$(vn$)&" Invoice # "&trim$(iv$),50,0)
fnLbl(lc+=2,1,"Job #:",mylen,1)
! fnTxt(LC,MYPOS,6,0,1,"",0,"Choose from the sub-category list.")
! .  !  rESP$(RESPC+=1)=JN$
fncmbjob(lc,mypos)
resp$(respc+=1)=jn$
fnLbl(lc+=2,1,"Category #:",mylen,1)
fncmbcategory(lc,mypos)
resp$(respc+=1)=str$(cat)
fnLbl(lc+=2,1,"Sub-category #:",mylen,1)
fncmbsubcat(lc,mypos)
resp$(respc+=1)=str$(subcat)
fnLbl(lc+=1,1,"Amount:",mylen,1)
fnTxt(lc,mypos,12,0,1,"10",0,"Enter the amount allocated to this category.")
resp$(respc+=1)=str$(amt)
fnLbl(lc+=1,1,"Description:",mylen,1)
fnTxt(lc,mypos,25,0,0,"",0,"Enter the descritpion for the allocation.")
resp$(respc+=1)=jobdesc$
! Job Cost Invoice Breakdown Grid
fnflexinit1('JobAlloc',11,1,6,60,mat chdr3$,mat cmask3$,1,0,0)
if displayalljobs=1 then restore #jcbreakdown: : goto L6270
restore #jcbreakdown,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey EO_FLEX1
L6270: totalcost=0: mat jobitem$=("")
READ_JOB_ALLOCATIONS: !
read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,holdvn$,holdiv$ eof EO_FLEX1
if displayalljobs=1 then goto L6320 ! allow all entries to print
if holdvn$<>vn$ or holdiv$<>iv$ then goto EO_FLEX1
L6320: !
totalcost+=amt
jobitem$(1)=str$(rec(jcbreakdown)) : jobitem$(2)=jn$
jobitem$(3)=str$(cat) : jobitem$(4)=str$(subcat)
jobitem$(5)=str$(amt) : jobitem$(6)=jobdesc$
fnflexadd1(mat jobitem$)
goto READ_JOB_ALLOCATIONS
EO_FLEX1: !
fnButton(3,70,"&Search",68,"Will search for job numbers",1,9)
fnButton(5,70,"&Search",69,"Will search for available category codes",1,9)
fnLbl(18,18,"Total: "&trim$(cnvrt$("pic($$$,$$$,$$$.##)",totalcost)),22,1,0)
if displayalljobs=0 then let fnLbl(19,18,"Invoice: "&trim$(cnvrt$("pic($$$,$$$,$$$.##)",upa)),22,1,0)
fnButton(18,53,"&Edit",65,"Will allow you to change an allocation",1,5)
fnButton(3,70,"&Search",63,"Will search for available category codes",1,9)
fnCmdKey("&Next",1,1,0,"Accept this transaction)")
fnCmdKey("&Listing",4,0,0,"Print listing of all job cost entries.")
fnCmdKey("&Post To Jobs",3,0,0,"Post this batch ofjob cost entries to job cost records. Normally done once complete with batch.")
fnCmdKey("&Cancel",5,0,1,"Cancels without posting to jub cost)")
fnAcs(mat resp$,ckey)
if ckey=4 then gosub PRINT_JOB_COST_ENTRIES: goto ENTRY_SCREEN
if val(resp$(4))=0 and ckey<>65 then ckey=5 ! exit if no amount on next
if ckey=5 then amt=0: totalcost=0 : goto L6930 ! sCREEN=0: Goto MENU1
if ckey=3 then gosub POST_TO_JOB : goto ENTRY_SCREEN
if ckey=65 then goto L6520 else goto L6530
L6520: !
editrec=val(resp$(6))
read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12",rec=editrec: jn$,cat,subcat,amt,jobdesc$,vn$,iv$
delete #jcbreakdown,rec=editrec: noRec ENTRY_SCREEN
goto ENTRY_SCREEN
L6530: if ckey=68 then goto L6540 else goto L6550
L6540: jn$="": fnjob_srch(jn$,1) : goto ENTRY_SCREEN
L6550: jn$=resp$(1)(1:6)
jn$=lpad$(rtrm$(jn$),6)
read #41,using 'form pos 7,c 25',key=jn$: jobname$ nokey L6590
goto L6600
L6590: !
mat ml$(3)=("")
ml$(1)="Job # "&jn$&" does not exist."
ml$(2)="                                        "
ml$(3)="Take OK to select a different job #."
fnmsgbox(mat ml$,resp$,'',0)
goto ENTRY_SCREEN
L6600: if ckey=69 then goto L6610 else goto L6620
L6610: cn$="": fncategory_srch(cn$,1) : cat=val(cn$): goto ENTRY_SCREEN
L6620: cat=val(resp$(2)(1:5))
subcat=val(resp$(3)(1:3))
amt=val(resp$(4))
jobdesc$=resp$(5)
write #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$
amt=0: goto ENTRY_SCREEN
POST_TO_JOB: !
restore #jcbreakdown:
L6700: read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$ eof L6900
if ltrm$(jn$)="" or rtrm$(ltrm$(jn$))="0" then goto L6700
cn$=jn$&lpad$(str$(cat),5)
read #2,using L6740,key=cn$: mat l,mat ta nokey L6780
L6740: form pos 37,11*pd 7.2,2*pd 2,2*pd 3
l(6)=l(6)+amt
l(9)=l(9)+amt
goto L6780
L6780: read #45,using L6790,rec=1,reserve: ot5
L6790: form pos 86,pd 3
! dim empnum$*12
! empnum$=lpad$(rtrm$(str$(ji1(1))),12)
L6810: ot5=lrec(45)+1
invdate=val(up$(1))
write #45,using L6840,rec=ot5,reserve: "",jn$,cat,subcat,0,invdate,0,0,0,0,amt,jobdesc$,0 duprec L6810
L6840: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
if ta(2)=0 then ta(1)=ot5 else rewrite #45,using L6790,rec=ta(2),reserve: ot5
rewrite #45,using L6790,rec=1,release: ot5
ta(2)=ot5
rewrite #2,using L6740,key=cn$: mat l,mat ta
goto L6700
L6900: jn$="": jobdesc$="": amt=0: cat=subcat=0
close #jcbreakdown:
execute "Drop [Q]\CLmstr\jcbreakdowns"&wsid$&".h[cno]"
L6930: !
return  ! /r
MAKE_JCB: ! r: create [Q]\CLmstr\JCBreakdownS"&wsid$&".h[cno]"
open #jcbreakdown=46: "Name=[Q]\CLmstr\JCBreakdownS"&wsid$&".h[cno],Version=1,replace,RecL=79",internal,outIn,relative
close #jcbreakdown:
execute "Index [Q]\CLmstr\JCBreakdownS"&wsid$&".h[cno],[Q]\CLmstr\jcbrkidx"&wsid$&".h[cno],48,20,Replace,DupKeys -n"
return ! /r
HDR2: ! r: header for jub cost listing
fnopenprn
pr #255,using 'Form POS 1,C 8,Cc 82': date$,env$('cnam')
pr #255,using 'Form POS 1,C 4,N 4,POS 36,C 40': "Page",pg,"Job Cost Entry Listing"
pr #255: ""
pr #255: " Payee #    Invoice #    Job #    Cat #  Sub-Cat   Amount  Descripton"
pr #255: " _______    _________    _____    _____  _______   ______  __________"
return ! /r
!
PRINT_JOB_COST_ENTRIES: ! r:
	letotal_allocations=0
	gosub HDR2
	restore #jcbreakdown:
	do
		read #jcbreakdown,using "form pos 1,c 6,pd 3,pd 3,pd 5.2,c 30,c 8,c 12": jn$,cat,subcat,amt,jobdesc$,vn$,iv$ eof L7140
		total_allocations+=amt
		pr #255,using "form pos 1,c 8,x 2,c 12,x 2,c 6,x 2,n 5,x 2,n 6,x 2,pic(zzz,zzz.##cr),c 30,skip 1": vn$,iv$,jn$,cat,subcat,amt,jobdesc$ pageoflow PGOF2
	loop
	L7140: !
	pr #255,using "form pos 48,c 10,skip 1,pos 48,pic(zzz,zzz.zzcr),skip 1,pos 48,c 10": "__________",total_allocations,"=========="
	fncloseprn
	jn$=jobdesc$="" : cat=subcat=amt=0
return ! /r
PGOF2: ! r:
pr #255: newpage
gosub HDR2
continue ! /r
def fn_test_key(holdkey$*20,vn$,iv$)
	dim newkey$*20
	! uses open files:
	newkey$=rpad$(vn$&iv$,20)
	if newkey$=holdkey$ then goto TEST_KEY_OK

	! TEST1: !
	! pass goes to test2 - fail goes to test_key_fail_on_iv
		close #ivpaid: ioerr ignore
		open #ivpaid=fnH: "Name=[Q]\CLmstr\IvPaid.h[cno],KFName=[Q]\CLmstr\IvIndex.h[cno]",internal,outIn,keyed
		unpaidkey$=rpad$(ltrm$(vn$),8)&rpad$(ltrm$(iv$),12)
		read #ivpaid,using 'Form Pos 1,C 8',key=unpaidkey$,release: x$ nokey TEST2
	goto TEST_KEY_FAIL_ON_IV
	
	TEST2: !
		! pass goes to test_key_pass - fail goes to test_key_fail_on_paytrans
		open #testpaytrans=fnH: "Name=[Q]\CLmstr\PayTrans.h[cno],KFName=[Q]\CLmstr\UnPdIdx1.h[cno],SHR",internal,outIn,keyed
		read #testpaytrans,using 'Form Pos 1,C 8',key=newkey$,release: x$ nokey TEST_KEY_OK
	goto TEST_KEY_FAIL_ON_PAYTRANS
	
	TEST_KEY_FAIL_ON_PAYTRANS: !
		mat ml$(3)=("")
		ml$(1)="The invoice number "&trim$(iv$)&" for Payee "&trim$(vn$)
		ml$(2)="already exists in the Unpaid Invoice file."
		ml$(3)="Please change the Invoice Number or the Payee."
		fnmsgbox(mat ml$,resp$,'',0)
	goto TEST_KEY_FAIL
	
	TEST_KEY_FAIL_ON_IV: !
		mat ml$(3)=("")
		ml$(1)="The invoice number "&trim$(iv$)&" for Payee "&trim$(vn$)
		ml$(2)="already exists in the Paid Invoice file."
		ml$(3)="Please change the Invoice Number or the Payee."
		fnmsgbox(mat ml$,resp$,'',0)
	goto TEST_KEY_FAIL
	
	TEST_KEY_OK: !
		! pr 'fnTest Key PASSED'
		fn_test_key=1
	goto EO_TEST_KEY
	
	TEST_KEY_FAIL: !
	! pr 'fnTest Key FAILED'
		fn_test_key=2
	goto EO_TEST_KEY
	
	EO_TEST_KEY: !
	! If FILE(IVPAID)<>0 Then Close #IVPAID:
	if file(testpaytrans)<>0 then close #testpaytrans: ioerr ignore
fnend
def fn_addInvoice(vn$,iv$,aiRecordNumberToEdit)
	if ~aiSetup then
		aiSetup=1
		dim aiUaColMask$(3)
		aiUaColMask$
		aiUaColMask$(1)='' ! glnMask$ <-- can't use gln mask if i want the description on it too
		aiUaColMask$(2)='' ! '10'
		aiUaColMask$(3)=''
		dim aiUaColHead$(3)*40
		aiUaColHead$(1)='General Ledger'
		aiUaColHead$(2)='Amount'
		aiUaColHead$(3)='Description'
		dim selected_alloc$*50
	end if
	!
	if RecordNumberToEdit then !
		editing=1
		read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=aiRecordNumberToEdit,release: vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
		mat resp$=("")
		holdkey$=vn$&iv$
	else ! add  (ckey=1)
		mat resp$=("")
		editing=0
		holdkey$=''
		vn$=''
		iv$=''
		up$(1)=''
		up$(2)=''
		up$(3) =''
		up$(4)=''
		upa=0
		disamt=0
		ddate=0
		pcde=0
		bcde=0
	end if
	!
ai_ADD_UNPAID_INVOICES_TOS: ! r:
	fnTos
	respc=0 : mat resp$=("") : frame_width=90
	lc=0
	fnFra(1,1,11,frame_width,"Unpaid Invoice")
	lc=0 : mylen=18 : mypos=mylen+2
	frame=1
	fnLbl(lc+=1,1,"Payee:",mylen,1,0,frame)
	fncombof("Paymstr",lc,mypos,0,"[Q]\CLmstr\paymstr.h[cno]",1,8,9,30,"[Q]\CLmstr\Payidx1.h[cno]",1,0, "Enter the payee number (Use the 'Add Payee' option to add a new vendor record",frame)
	resp$(1)=vn$
	fnLbl(lc+=1,1,"Invoice Number:",mylen,1,0,frame)
	fnTxt(lc,mypos,12,0,0,"",0,"",frame)
	resp$(2)=iv$
! had a required answer here; temporarly changed to a message box
	fnLbl(lc+=1,1,"Invoice Date:",mylen,1,0,frame)
	fnTxt(lc,mypos,8,0,1,"mmddyy",0,"",frame)
	resp$(3)=up$(1)
	fnLbl(lc+=1,1,"Due Date:",mylen,1,0,frame)
	fnTxt(lc,mypos,8,0,1,"mmddyy",0,"",frame)
	resp$(4)=up$(2)
	fnLbl(lc+=1,1,"P O Number:",mylen,1,0,frame)
	fnTxt(lc,mypos,12,0,0,"",0,"",frame)
	resp$(5)=up$(3)
	fnLbl(lc+=1,1,"Description:",mylen,1,0,frame)
	fnTxt(lc,mypos,18,0,0,"",0,"",frame)
	resp$(6)=up$(4)(1:18)
	fnLbl(lc+=1,1,"Amount:",mylen,1,0,frame)
	fnTxt(lc,mypos,12,0,1,"10",0,"Enter the total invoice amount.",frame)
	resp$(7)=str$(upa)
	fnLbl(lc+=1,1,"Discount Amount:",mylen,1,0,frame)
	fnTxt(lc,mypos,12,0,1,"10",0,"Enter any discount allowed.",frame)
	resp$(8)=str$(disamt)
	fnLbl(lc+=1,1,"Discount Date:",mylen,1,0,frame)
	fnTxt(lc,mypos,10,0,1,"ccyymmdd",0,"",frame)
	resp$(9)=str$(ddate)
	fnLbl(lc+=1,1,"Payment Code:",mylen,1,0,frame)
	item1$(1)="Pay Later"
	item1$(2)="Pay Now"
	item1$(3)="Paid"
	fncomboa("unpaid-2",lc,mypos,mat item1$,"If you choose pay now, the invoice will be coded for payment and will paid next time checks are printed.",0,1)
	if pcde=0 then resp$(10)=item1$(1) ! Pay Later
	if pcde=1 then resp$(10)=item1$(2) ! Pay Now
	if pcde=2 then resp$(10)=item1$(3) ! Paid
	fnLbl(lc+=1,1,"Bank Code:",mylen,1,0,frame)
	fncombof("bankmstr",lc,mypos,23,"[Q]\CLmstr\bankmstr.h[cno]",1,2,3,20,"[Q]\CLmstr\bankidx1.h[cno]",0,0, "",frame)
	resp$(11)=str$(bcde) ! RESP$(RESPC)
	fnButton(1,80,"Payee",50,"Add or edit a payee",0,0,frame)
	fnLbl(lc=15,3,"Breakdown Information",mylen,center)
	!
	fnLbl(lc+=1,35,"Allocation(s):",20,1)
	fnButton(lc,56,"Auto by Payee [F2]",2,"Reset allocations to Payee's defaults")
	fnButton(lc,76,"Add",52,"Add a new allocation")
	fnButton(lc,81,"Edit",53,"Modify an existing allocation")
	fnButton(lc,87,"Delete",54,"Remove selected allocation")
	fnflexinit1('unpdaloc',lc+=1,2,10,88,mat aiUaColHead$, mat aiUaColMask$,1)
	dim alloc2d$(0,3)*30
	dim alloc2d_setup$*20
	if alloc2d_setup$<>lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12) then
		alloc2d_setup$=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12)
		aiInvoiceAllocationCount=fn_readAlloctaitonsInto2dArray(alloc2d_setup$,mat alloc2d$)
		mat alloc2d$(aiInvoiceAllocationCount,udim(mat alloc2d$,2))
	else
		aiInvoiceAllocationCount=udim(mat alloc2d$,1)
	end if
	for aiAllocItem=1 to aiInvoiceAllocationCount
		dim tmpItem$(3)*50
		tmpItem$(1)=fnrgl$(alloc2d$(aiAllocItem,1))
		tmpItem$(2)=alloc2d$(aiAllocItem,2)
		tmpItem$(3)=alloc2d$(aiAllocItem,3)
		fnflexadd1(mat tmpItem$)
	nex aiAllocItem
	fnCmdKey("Save",1,1)
	! fnCmdKey("&Allocate",2,0,0,"Automatically allocates the general ledger breakdown if payee record contains the breakdown information")
	fnCmdKey("&Delete",3,0,0,"Delete the invoice highlighted above")
	fnCmdKey("&Cancel",5,0,1,"Return to Unpaid Invoice selection (without saving)")
	fnAcs(mat resp$,ckey)
	if ckey=5 then
		alloc2d_setup$=''
		mat alloc2d$=('')
		goto aiFinis
	end if
	vn$=lpad$(rtrm$(resp$(1)(1:8)),8) ! payee number
	iv$=lpad$(rtrm$(resp$(2)),12) ! invoice number
	alloc2d_setup$=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12)
	up$(1)=resp$(3) ! invoice date
	up$(2)=resp$(4) ! due date
	up$(3)=resp$(5) ! po number
	up$(4)=resp$(6)(1:18) ! description
	upa=val(resp$(7)) ! amount
	disamt=val(resp$(8)) ! discount amount
	ddate=val(resp$(9)) ! discount date
	if resp$(10)=item1$(1) then pcde=0 ! pay later
	if resp$(10)=item1$(2) then pcde=1 ! pay now
	if resp$(10)=item1$(3) then pcde=2 ! paid
	bcde=val(resp$(11)(1:3))
	selected_alloc$=fnagl$(resp$(12))
	if ckey=3 then ! delete invoice and breakdowns
		fn_InvoiceDelete(holdkey$)
		goto aiFinis
	else if ckey=2 then
		fn_InvoiceAllocateFromPayee(mat alloc2d$,vn$,upa,paymstr1,payeegl)
	else if ckey=50 then
		!   vn$=ss$=ph$=contact$=email$=fax$=myact$=""
		fnaddpayee
	else if ckey=52 then ! Add
		fn_InvoiceAllocationFM(vn$,iv$)
	else if ckey=53 then ! Edit
		fn_InvoiceAllocationFM(vn$,iv$, selected_alloc$)
	else if ckey=54 then
		fn_InvoiceAllocationDelete(selected_alloc$)
	else if ckey=1 then ! Save
		if fn_InvoiceValid then
			fn_InvoiceSave
			if havejc=1 then gosub JOBCOST
			if editing=1 then
				editing=0
				goto aiFinis
			end if
			goto aiFinis ! jb 11/30/2016   !   goto ai_ADD_UNPAID_INVOICES ! setup up new invoice  kj 11609
		else
			goto ai_ADD_UNPAID_INVOICES_TOS ! must have an invoice number
		end if
	end if
	goto ai_ADD_UNPAID_INVOICES_TOS
! /r


aiFinis: !
fnend
def fn_InvoiceSave  ! write any new invoices and matching allocations
	if editing=0 then
		write #paytrans,using 'Form POS 1,Cr 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8': vn$,iv$,mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
	else if editing=1 then
		rewrite #paytrans,using 'Form POS 1,Cr 8,C 12,2*G 6,C 12,C 18,G 10.2,n 1,n 2,G 8,G 6,N 1,n 6,n 10.2,n 8',rec=rec(paytrans): lpad$(rtrm$(vn$),8),lpad$(rtrm$(iv$),12),mat up$,upa,pcde,bcde,ckn,dp,gde,pdte,disamt,ddate
		do
			delete #unpdaloc,key=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey a_awi_delOldAllocFinis eof a_awi_delOldAllocFinis ! delete any previous allocations because all we know about them is the invoice # and vendor#(can't identify which ones to update. change or add
		loop
		a_awi_delOldAllocFinis: !
	end if
	for aiAllocItem=1 to aiInvoiceAllocationCount
		write #unpdaloc,using f_unpdaloc: vn$,iv$,alloc2d$(aiAllocItem,1),val(alloc2d$(aiAllocItem,2)),alloc2d$(aiAllocItem,3)
		f_unpdaloc: form pos 1,Cr 8,C 12,c 12,pd 5.2,c 30
	next aiAllocItem
fnend
def fn_InvoiceValid ! very local
	ivReturn=1
! x=0
	tac=0
	for iv2dItem=1 to udim(mat alloc2d$,1)
		tac+=val(alloc2d$(iv2dItem,2))
	nex iv2dItem
	if fn_test_key(holdkey$,vn$,iv$)=2 then
		ivReturn=0
	else if trim$(iv$)="" then  ! must have an invoice number
		mat ml$(3)
		ml$(1)="You must enter an invoice number on each unpaid "
		ml$(2)="record.  If you must make up an invoice number,"
		ml$(3)="be careful to use a different number each time!"
		fnmsgbox(mat ml$,resp$,'',16)
		ivReturn=0
	else if tac<>upa then ! allocations don't match total invoice
		mat ml$(3)
		ml$(1)="The allocations of "&trim$(cnvrt$("pic($$$$,$$$.##)",abs(tac)))&" do not agree with"
		ml$(2)="the total invoice of "&trim$(cnvrt$("pic($$$$,$$$.##)",abs(upa)))&"."
		ml$(3)="You must correct the problem before you can continue!"
		fnmsgbox(mat ml$,resp$,'',16)
		ivReturn=0
	end if
	fn_InvoiceValid=ivReturn
fnend
def fn_readAlloctaitonsInto2dArray(key$*20,mat alloc2d$)
	rai2a_return=0
	restore #unpdaloc,key>=lpad$(rtrm$(vn$),8)&lpad$(rtrm$(iv$),12): nokey ai_EO_unpdaloc
	do
		read #unpdaloc,using 'Form POS 1,C 8,C 12,c 12,PD 5.2,C 30': hvn$,hiv$,gl$,allocAmount,gldesc$ eof ai_EO_unpdaloc
		if vn$=hvn$ and iv$=hiv$ and (trim$(gl$&gldesc$)<>'' and allocAmount<>0) then
			mat alloc2d$(rai2a_return+=1,udim(mat alloc2d$,2))
			alloc2d$(rai2a_return,1)=gl$
			alloc2d$(rai2a_return,2)=str$(allocAmount)
			alloc2d$(rai2a_return,3)=gldesc$
		end if
	loop while vn$=hvn$ and iv$=hiv$
	ai_EO_unpdaloc: !
	fn_readAlloctaitonsInto2dArray=rai2a_return
fnend
def fn_InvoiceAllocateFromPayee(mat alloc2d$,vn$,upa,paymstr1,payeegl) ! ai_READ_STANDARD_BREAKDOWNS: !  pull standard gl breakdowns from payee file
	read #paymstr1,using "form pos 1,c 8,4*c 30,pd 5.2,n 2,c 11,x 6,c 12,c 30,c 50,c 12,c 20",key=lpad$(rtrm$(vn$),8),release: vn$,mat pr$,ytdp,typ,ss$,ph$,contact$,email$,fax$,myact$ nokey ai_XIT
	ai_totalalloc=0
	restore #payeegl,key>=vn$: nokey ai_XIT
	iafp_count=0
	do
		read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof ai_EO_READSTGL
		if vn$<>payeekey$ then goto ai_EO_READSTGL
		if trim$(payeegl$)="" or payeegl$="  0     0   0" then goto ai_NEXT
		read #glmstr,using 'Form POS 13,C 30',key=payeegl$,release: de$ nokey ai_NEXT
		mat alloc2d$(iafp_count+=1,udim(mat alloc2d$,2))
		alloc2d$(iafp_count,1)=payeegl$
		alloc2d$(iafp_count,2)=str$(round(upa*percent*.01,2))
		ai_totalalloc+=val(alloc2d$(iafp_count,2))
		alloc2d$(iafp_count,3)=gldesc$
		ai_NEXT: !
	loop
	ai_EO_READSTGL: !
	if ai_totalalloc<>upa and iafp_count>0 then alloc2d$(iafp_count,2)=str$(val(alloc2d$(iafp_count,2))+upa-ai_totalalloc)
	ai_XIT: !
fnend
def fn_InvoiceDelete(holdkey$*20)
	delete #paytrans,key=holdkey$: ioerr ai_L4330
	do
		delete #unpdaloc,key=holdkey$: nokey ai_L4330 eof ai_L4330 ! delete any  allocations
	loop ! delete all allocations
ai_L4330: !
fnend
def fn_InvoiceAllocationDelete(selected_alloc$*50)
	for j=1 to udim(mat alloc2d$,1)
		iadMatch=0
		if alloc2d$(j,1)=selected_alloc$ then
			iadMatch=j
			goto iadGotIt
		end if
	next j
	iadGotIt: !
	if iadMatch then let fn_2dRemove(mat alloc2d$,iadMatch)
fnend
def fn_InvoiceAllocationFM(vn$,iv$; selected_alloc$*50)
	dim iaf_desc$*30
	iaf_gl$=''
	iaf_desc$=''
	iaf_amt=0
	iaf_edit=0
	if selected_alloc$<>'' then ! editing an allocation
		for j=1 to udim(mat alloc2d$,1)
			if alloc2d$(j,1)=selected_alloc$ then
				iaf_edit=j
				goto iafGotIt
			end if
		next j
		iafGotIt: !
		if iaf_edit then
			iaf_gl$=alloc2d$(iaf_edit,1)
			iaf_amt=val(alloc2d$(iaf_edit,2))
			iaf_desc$=alloc2d$(iaf_edit,3)
		end if
! else ! adding an allocation
!   iaf_edit=0
	end if
	fnTos
	respc=0
	mylen=32 : mypos=mylen+2
	lc=0
	fnLbl(lc+=1,1,"Payee:",mylen,1)
	fnTxt(lc,mypos,20, 0,0,'',1)
	resp$(respc+=1)=vn$
	fnLbl(lc+=1,1,"Invoice Number:",mylen,1)
	fnTxt(lc,mypos,12,0,0,"",1)
	resp$(respc+=1)=iv$
	lc+=1
	fnLbl(lc+=1,1,"General Ledger:",mylen,1)
	fnqglbig(lc,mypos,0,2)
	resp$(iaf_respc_gl:=respc+=1)=fnrglbig$(iaf_gl$)
	fnLbl(lc+=1,1,"Amount:",mylen,1)
	fnTxt(lc,mypos,12,0,1,"10")
	resp$(iaf_respc_amt:=respc+=1)=str$(iaf_amt)
	fnLbl(lc+=1,1,"Description:",mylen,1)
	fnTxt(lc,mypos,18)
	resp$(iaf_respc_desc:=respc+=1)=iaf_desc$
	fnCmdSet(4)
	fnAcs(mat resp$,ckey)
	if ckey<>5 then
		iaf_amt=val(resp$(iaf_respc_amt))
		iaf_gl$=fnagl$(resp$(iaf_respc_gl))
		iaf_desc$=resp$(iaf_respc_desc)
		if iaf_edit=0 then
			iaf_edit=udim(mat alloc2d$,1)+1
			mat alloc2d$(iaf_edit,udim(mat alloc2d$,2))
		end if
		alloc2d$(iaf_edit,1)=iaf_gl$
		alloc2d$(iaf_edit,2)=str$(iaf_amt)
		alloc2d$(iaf_edit,3)=iaf_desc$
	end if
fnend
def fn_2dRemove(mat a2d$,iadWhich)
	! removes an array item from the row (1st parameter) from an array
	dim a2dnew$(0,0)*30
	a2d_count_x=udim(mat a2d$,1)
	a2d_count_y=udim(mat a2d$,2)
	if iadWhich>0 and iadWhich<=a2d_count_x then
		mat a2dnew$(a2d_count_x-1,a2d_count_y)
		for a2dx=1 to a2d_count_x
			if a2dx<>iadWhich then
				a2dnewX+=1
				for a2dy=1 to a2d_count_y
					a2dnew$(a2dnewX,a2dy)=a2d$(a2dx,a2dy)
				nex a2dy
			end if
		nex a2dx
		mat a2d$(a2d_count_x-1,a2d_count_y)
		mat a2d$=a2dnew$
	end if
fnend
include: ertn
