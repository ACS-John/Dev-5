! (formerly) S:\acsUB\Collections  and before that acsUB\ubIpColl
	fn_setup ! r:
	fnTop(program$) ! for now use the settings from Enter Collections for page formatting of reports
	open #hCustomer1=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #hCustomer2=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,outIn,keyed
	open #hTrans=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
	open #hTrans2=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed
	! r: open BudMstr and BudTrans, also set bud1 (1=budget files opened, 0=not)
	bud1=0
	open #h_budmstr=fnH: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr BudMstrOpenFail
	open #h_budTrans=fnH: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",i,outi,r
	bud1=1
	BudMstrOpenFail: !
	! /r
	! dim tau$(0)*256,tauN(0)
	open #hTransUnposted=fnH: "Name="&collections_filename$&',RecL=91,Use',i,outi,r
	mat x=(0)
	transType=postingCodeUnused=0
goto ScreenMenu1
! /r
ScreenMenu1: ! r:
	do
		fnTos : rc=0
		fnflexinit1('Collections',6,1,10,80,mat chdr$,mat cm$,1) ! r: add the grid to the screen
		restore #hTransUnposted:
		totalacct=0 : totalCollections=totalDebitMemos=totalCreditMemos=0
		do  ! for j=1 to lrec(hTransUnposted)
			read #hTransUnposted,using F_ubColInp: x$,transAmount,transDate, transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow eof L1080 noRec L1070
			totalacct+=val(x$) conv ignore
			dim m1_item$(20)*80
			m1_item$(1)=str$(rec(hTransUnposted)) ! record
			m1_item$(2)=x$                        ! account
			m1_item$(3)=str$(transAmount)         ! amount
			m1_item$(4)=str$(transDate)           ! date
			m1_item$(5)=fn_collType$(transType)
			! if env$('acsDeveloper')<>'' then m1_item$(5)=m1_item$(5)&' ('&str$(transType)&')'
			m1_item$(6)=rcpt$ ! str$(postingCodeUnused)            !
			fn_totalAdd(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
			! m1_item$(7)=rcpt$
			cHdrItem=cHdrItemFirstService=6
			for j2=1 to possibleServiceCount
				if srvName$(j2)="" or srvName$(j2)(1:5)="Reduc" then
					goto L1030
				else
					m1_item$(cHdrItem+=1)=str$(alloc(cHdrItem-cHdrItemFirstService))
				end if
				L1030: !
			next j2
			if uprc$(escrow$)="Y" then m1_item$(cHdrItem+=1)=str$(escrow)
			mat m1_item$(cHdrItem)
			fnflexadd1(mat m1_item$)
			L1070: !
		loop
		L1080: !
		x$=rcpt$=''
		transAmount=transDate=transType=postingCodeUnused=escrow=0
		mat alloc=(0) : mat bd3=(0)
		mat tgb=(0)   : mat hgb=(0) ! this line does not seem to matter in the least.  added to try to fix weird allocation issue, but it had no effect.
		! /r
		resp_selectedRecordNumber=rc+=1 ! resp$(1) returns the record number of the selected entry
		fnLbl(1,1,"Total Collections:",22,1)
		fnLbl(1,24,cnvrt$('pic($----,---,--#.##)',totalCollections),15,1)
		fnLbl(2,1,"Total Credit Memos:",22,1)
		fnLbl(2,24,cnvrt$('pic($----,---,--#.##)',totalCreditMemos),15,1)
		fnLbl(3,1,"Total Debit Memos:",22,1)
		fnLbl(3,24,cnvrt$('pic($----,---,--#.##)',totalDebitMemos),15,1)
		fnLbl(4,1,"Total Account Numbers:",22,1)
		fnLbl(4,24,str$(totalacct),15,1)
		if lrec(hTransUnposted)<1 then
			fnCmdKey("Add &Transaction",2,1,0,"Allows you to enter transactions.")
			fnCmdKey("&Close",5,0,1,"Returns to menu.")
		else
			fnCmdKey("&Add",2,totalacct==0)
			fnCmdKey("E&dit",1,(totalacct<>0))
			fnCmdKey("Delete",4)
			fnCmdKey("&Print",3)
			fnCmdKey("&Close",5,0,1,'Return to main menu.\nEntries will not be lost nor will they be posted to accounts.')
			fnCmdKey("&Post",10,0,0,'Apply collections to customer records.')
			fnCmdKey("Open Drawer",12,0,0,'Open an attached cash drawer')
		end if
		fnCmdKey("Import CSV",14,0,0,'Import Collections CSV')
		fnAcs(mat resp$,ck1)
		if ck1=5 then
			goto Xit
		else
			edrec=val(resp$(resp_selectedRecordNumber))
			if ck1=1 then
				editmode=1
				goto EDIT_REC
			else if ck1=2 then
				editmode=0
				goto ADD_REC
			else if ck1=4 then
				delete #hTransUnposted,rec=edrec: ioerr ignore
			else if ck1=3 then
				fn_printListings
			else if ck1=10 then
				fn_printListings
				goto Merge
				! goto SCREEN_LAST_CHANCE
			else if ck1=12 then
				fn_open_cash_drawer
			else if ck1=14 then
				fn_csv_import
			end if
		end if
	loop
! /r
ADD_REC: ! r:
	b7=transType=1 !     default to always adding collections
	p$=" "
	rcpt$=""
goto ScreenSelectAccount ! /r
ScreenSelectAccount: ! r:
	if fnask_account('Collections',z$,hCustomer1)=5 then
		goto ScreenMenu1
	end if
	x1$=z$
	! r: read selected account and prepare data for ScreenAdd
	read #hCustomer1,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2',key=x1$,release: nam$,bal,db1,mat gb,escrowbal nokey ScreenSelectAccount
	havebudget=0
	fn_getMatTgb(mat tgb,escrow,mat gb,mat srvName$,escrow$,transType,escrowbal,oldescrowbal)
	! /r
goto ScreenAdd ! /r
def fn_getMatTgb(mat tgb,&escrow,mat gb,mat srvName$,escrow$,transType,escrowbal,&oldescrowbal)
	mat tgb=(0)
	j2=0: escrow=0
	for j=1 to possibleServiceCount
		if fn_serviceValidForCollAlloc(j) then tgb(j2+=1)=gb(j)
	next j
	if uprc$(escrow$)="Y" and transType=3 then oldescrowbal=escrowbal ! add escrow balance into last allocation if have escrow and processing a collection transaction
fnend
ScreenAdd: ! r:

	if x(3)=0 then x(3)=date('mmddyy') ! date should default to today
	x(2)=max(0,bal) ! amount collected should default to balance (or 0 if they have a credit)

	fnTos
	respc=0

	fnLbl(3,1,"Amount:",25,1)
	fnTxt(3,27,8,0,0,"32")
	if ~do_not_blank_rcpt then resp$(respc:=1)=str$(x(2))
	
	fnLbl(4,1,"Date (mmddyy):",25,1)
	fnTxt(4,27,8,0,0,"1001")
	if ~do_not_blank_rcpt then resp$(respc:=2)=str$(x(3))
	
	fnLbl(5,1,"Receipt Number (CA=Cash):",25,1)
	fnButton(5,40,"Cash",7,"(F7) Set Receipt Number to CA (for Cash)")
	fnTxt(5,27,9)
	
	fnLbl(1,1,"Entry Type:",25,1)
	if ~do_not_blank_rcpt then resp$(respc:=3)=""
	fncomboa("coll_type_rdc",1,27,mat coll_type_option$)
	if ~do_not_blank_rcpt then
		for ax=1 to 3
			if hresp1$=coll_type_option$(ax) then resp$(respc:=4)=coll_type_option$(ax): verify=1
		next ax
		if verify=0 then resp$(respc:=4)=coll_type_option$(1)
	end if
	
	fnLbl(2,1,"Account:",25,1)
	fnTxt(2,27,10,10,1,"",1,"Account (Press Cancel to Re-Select)")
	if ~do_not_blank_rcpt then resp$(respc:=5)=z$
	
	col3_pos=50 : col4_pos=76
	
	fnLbl(1,col3_pos,"Name:",25,1)
	fnTxt(1,col4_pos,30,30,0,"",1,"Account Name (Press Cancel to Re-Select)")
	if ~do_not_blank_rcpt then resp$(respc:=6)=trim$(nam$)
	
	fnLbl(2,col3_pos,"Balance:",25,1)
	fnTxt(2,col4_pos,10,10,1,"",1,"Account Balance (Press Cancel to Re-Select)")
	if ~do_not_blank_rcpt then resp$(respc:=7)=cnvrt$("N 10.2",bal)
	
	fnLbl(3,col3_pos,"Billed:",25,1)
	fnTxt(3,col4_pos,8,8,1,"1",1)
	resp$(respc:=8)=str$(db1)
	
	if uprc$(escrow$)="Y" then
		fnLbl(4,col3_pos,"Escrow Balance:",25,1)
		fnTxt(4,col4_pos,10,10,1,"10",1)
		if ~do_not_blank_rcpt then resp$(respc:=9)=str$(escrowbal)
	end if
	fnCmdKey("&Save",1,1,0,"Save with this entry and move to next record.")
	fnCmdKey("&Review Customer Record",8,0,0,"Allows you to review any customer record.")
	fnCmdKey("&Notes",3,0,0,"Customer Notes")
	fnCmdKey("&Back",2,0,0,"Back up one screen. Select a different customer.")
	fnCmdKey("&Cancel",5,0,1,"Return to proof total screen.")
	fnAcs(mat resp$,ckey,1)

	do_not_blank_rcpt=0

	if ckey=2 then ! 2=back
		goto ScreenSelectAccount
	else if ckey=5 then
		goto ScreenMenu1 ! 5=cancel
	end if
	x(2)=val(resp$(1))
	x(3)=val(resp$(2))
	rcpt$=trim$(resp$(3))(1:9)
	if ckey=8 then
		fnCustomer( x$)
		goto ScreenAdd
	else if ckey=3 then
		fnCustomerNotes(z$)
		goto ScreenAdd
	end if
	transType=fn_oSub1(resp$(4))
	hresp1$=fn_collType$(transType)
	x1$=lpad$(trim$(z$),10)

	if ckey=7 then
		resp$(3)=rcpt$='CA'
		do_not_blank_rcpt=1
		goto ScreenAdd
	end if
	! r: validate
		! r: transaction date is within the last 7 days else ask confirmation
		if days(x(3),'mmddyy')>days(date) or days(x(3),'mmddyy')<days(date)-7 then ! warning if collection date greater than to today's date of less that one week ago
			if holdbaddate<>x(3) then ! had warning on same date, don't ask again
				mat mesg$(3)
				mesg$(1)="The collection date of "&resp$(2)&" appears "
				mesg$(2)="to be wrong!  It is perhaps too old or too new."
				mesg$(3)="Enter Yes to correct, else No to proceed."
				fnmsgbox(mat mesg$,resp$,'',52)
				holdbaddate=x(3)
				if resp$="Yes" then goto ScreenAdd
			end if
		end if
		! /r
		! r: negative numbers
		if x(2)<=0 then
			mat mesg$(1)
			mesg$(1)="Negative amounts are not allowed."
			fnmsgbox(mat mesg$)
			goto ScreenAdd
		end if
		! /r
		! r: receipt required?
		if uprc$(receipt$)="Y" and trim$(rcpt$)="" then
			mat mesg$(6)
			mesg$(1)="<<<<<   NO RECEIPT # ENTERED   >>>>>!"
			mesg$(2)="You have indicated in the company information"
			mesg$(3)="file that you require receipt numbers. You must"
			mesg$(4)="either enter a receipt # or change the option to"
			mesg$(5)="prevent getting this message."
			mesg$(6)="Take OK to continue."
			fnmsgbox(mat mesg$)
			goto ScreenAdd
		end if
		! /r
	! /r
	if ckey=1 then
		fn_print_receipt(z$,nam$,rcpt$,bal,x(2),x(3),hresp1$)
	end if
! /r
! r: after ScreenAdd - actually do the adding stuff
	! if ub_collDisableDepositList$='True' then
		if sum(tgb)=x(2) then
			for j=1 to validSrvCount : alloc(j)=tgb(j) : next j
			goto L2040
		end if
		if uprc$(escrow$)="Y" then escrow=fn_checkEscrow(escrowbal,mat gb,x(2),possibleServiceCount) ! check escrow balance
		mat hgb=tgb
		! gosub SeemsWrong1
		gosub Bud2
		gosub Bud1
	! end if
	if ~fn_breakdown(hCustomer1,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baOrder,ckey) then goto ScreenSelectAccount

	if ckey=2 then
		goto ScreenAdd
	end if
	!
	! L2020: !
	! for j=1 to validSrvCount : alloc(j)=tgb(j) : next j
	L2040: !
	transAmount=x(2) : transDate=x(3) : b7=transType
	postingCodeUnused=0
	L2060: !
	if sum(tgb)=x(2) then gosub Bud2 ! kj 10/14/09
	if sum(tgb)=x(2) and bud1=1 then gosub Bud1 ! was commented out; changed to if sum= on 101409 to keep from skipping ubdget update if exact amount paid.
	r6=lrec(hTransUnposted)+1
	if escrow>90000 then escrow=0 ! PREVENT 726 ERROR
	write #hTransUnposted,using F_ubColInp,rec=r6: z$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd2,escrow duprec L2060
	transType=b7
	goto ScreenSelectAccount ! /r
EDIT_REC: ! r:
	read #hTransUnposted,using F_ubColInp,rec=edrec: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow noRec ScreenMenu1
	nam$=""
	read #hCustomer1,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2',key=x$,release: nam$,bal,db1,mat gb,escrowbal nokey ignore
	fnTos
	respc=0

	fnLbl(1,1,"Entry Type:",25,1)
	fncomboa("rdc",1,27,mat coll_type_option$)
	resp$(resp_CollType:=respc+=1)=fn_collType$(transType)
	fnLbl(2,1,"Account:",25,1)
	fncmbact(2,27)
	resp$(resp_account:=respc+=1)=x$&"  "&nam$

	if uprc$(escrow$)="Y" then transAmount+=escrow : escrow=0 !     !      ! add escrow amount back into payment amount before edit

	fnLbl(3,1,"Amount:",25,1)
	fnTxt(3,27,9,0,0,"10")
	resp$(resp_amount:=respc+=1)=str$(transAmount)

	fnLbl(4,1,"Date (mmddyy):",25,1)
	fnTxt(4,27,8,0,0,"1")
	resp$(resp_transDate:=respc+=1)=str$(transDate)

	fnLbl(5,1,"Receipt # (CA=Cash):",25,1)
	fnTxt(5,27,9)
	resp$(resp_receiptNumber:=respc+=1)=rcpt$

	fnLbl(3,40,"Balance:",25,1)
	fnTxt(3,66,12,12,1,"10",1,"Account Balance (Press Cancel to Re-Select)")
	resp$(respc+=1)=cnvrt$("N 12.2",bal)

	fnLbl(4,40,"Billed:",25,1)
	fnTxt(4,66,8,8,1,"1",1)
	resp$(respc+=1)=str$(db1)

	fnCmdKey("&Save",1,1,0,"Saves any changes")
	fnCmdKey("&Edit",2,0,0,"Allows you to change the breakdown")
	fnCmdKey("&Delete",4,0,0,"Deletes this collection record")
	fnCmdKey("&Cancel",5,0,1,"Returns to main collection screen")
	ckey=fnAcs(mat resp$)

	if ckey=5 then goto L2590
	! If CKEY=2 Then Goto X
	if ckey=4 then
		delete #hTransUnposted,rec=edrec:
		goto ScreenMenu1
	end if

	transType  =fn_oSub1(resp$(resp_CollType))
	hresp1$             =resp$(resp_CollType)
	x$=x1$  =lpad$(trim$(resp$(resp_account)(1:10)),10)
	transAmount=x(2)=val(resp$(resp_amount))
	transDate       =val(resp$(resp_transDate))
	rcpt$         =trim$(resp$(resp_receiptNumber))
	if uprc$(escrow$)="Y" then escrow=fn_checkEscrow(escrowbal,mat gb,x(2),possibleServiceCount) ! check escrow balance
	if ~fn_breakdown(hCustomer1,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baOrder,ckey) then goto ScreenSelectAccount
	if uprc$(escrow$)="Y" then transAmount=transAmount-escrow !     !      ! subtract escrow amount from  payment amount before rewriting
	rewrite #hTransUnposted,using F_ubColInp,rec=edrec: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow
	! If BUD1=1 Then Gosub Bud2 : Gosub Bud1
	L2590: !
	! fn_totalAdd(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
	fn_print_receipt(x$,nam$,rcpt$,bal,x(2),x(3),hresp1$)
goto ScreenMenu1 ! /r
Merge: ! r:
	r6=0
	do
		MergeNext: !
		r6+=1
		if r6>lrec(hTransUnposted) then goto Merge_FINIS ! prevent stopping to deleted record and quit when finished
		read #hTransUnposted,using F_ubColInp,rec=r6: p$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow noRec MergeNext
		if p$(1:2)<>"  " or transAmount or escrow then
			read #hCustomer1,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=p$: bal,mat gb,escrowbal nokey MergeNext
			if transType=3 then tcode=3 ! collection
			if transType=4 then tcode=4 ! credit memo
			if transType=5 then tcode=5 ! debit memo
			if transType=5 then bal+=transAmount else bal-=transAmount
			tmp=fndate_mmddyy_to_ccyymmdd(transDate)
			mat tg=(0): x=0
			for j=1 to possibleServiceCount
				if fn_serviceValidForCollAlloc(j) then tg(j)=alloc(x+=1)
			next j
			write #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tmp,tcode,transAmount,mat tg,0,0,0,0,0,0,bal,pcode
			if uprc$(escrow$)="Y" and escrow<>0 then
				transAmount=escrow
				mat tg=(0)
				write #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tmp,tcode,transAmount,mat tg,0,0,0,0,0,0,bal,pcode ! write a history record for escrow amount
			end if
			j2=0
			for j=1 to possibleServiceCount
				if srvName$(j)="" or srvName$(j)(1:5)="Reduc" then goto L4470
				j2=j2+1
				if transType=5 then gb(j)=gb(j)+alloc(j2) else gb(j)=gb(j)-alloc(j2)
				L4470: !
			next j
			rewrite #hCustomer1,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=p$: bal,mat gb,escrowbal+escrow
			! postingCodeUnused=9
			delete #hTransUnposted,rec=r6: ! rewrite #hTransUnposted,using "Form POS 19,2*N 1",rec=r6: transType,postingCodeUnused
		end if
	loop
	Merge_FINIS: !
	close #hTransUnposted,free:
	close #hCustomer1:
	close #hCustomer2:
	close #hTrans:
	close #hTrans2: 
	close #hTransUnposted: ioerr ignore
	goto Xit ! /r
Xit: fnXit
Bud2: ! r: requires x1$
	havebudget=0
	bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
	if bud1=0 then goto L5080
	read #h_budmstr,using 'Form POS 1,C 10,PD 4,12*PD 5.2,2*PD 3',key=x1$: z$,mat ba,mat badr nokey L5080
	ta1=badr(1)
	do until ta1=0
		read #h_budTrans,using 'Form POS 1,C 10,2*PD 4,24*PD 5.2,2*PD 4,PD 3',rec=ta1: z$,mat bt1,nba noRec L4900
		if bt1(14,1)>0 and bt1(14,1)<>transDate then goto L4890
		if bt1(14,1)=transDate then bt1(14,1)=bt1(14,2)=0 : rewrite #h_budTrans,using "Form POS 11,2*PD 4,24*PD 5.2,2*PD 4",rec=ta1: mat bt1
		bd1+=1 ! 7/06/05  KJ
		if bd1=>5 then goto L4900 ! 7/06/05 kj
		bd1(bd1)=bt1(1,2)
		bd2(bd1)=ta1
		L4890: !
		ta1=nba
	loop
	L4900: !
	if bd1=0 then goto L5080
	if bd1(1)>0 and bd1(2)=0 then bd3(1)=bd1(1): goto L5030
	mat bd1(bd1)
	! MATCH_BUDGET_BILLING: !
	fnTos
	fnLbl(2,1,"Check the bills to be paid:",30,0)
	for j=1 to udim(bd1)
		if bd1(j)=0 then goto L4990
		fnChk(j+2,10,cnvrt$("pic(zz/zz/zz)",bd1(j)),0)
		L4990: !
	next j
	fnCmdSet(2)
	fnAcs(mat resp$,ck1)
	if ck1=5 then goto L5080 ! 7/06/05  KJ
	L5030: !
	for j=1 to 5
		if uprc$(resp$(j))=uprc$("True") then bd3(j)=1
		if bd3(j)=0 then bd2(j)=0
	next j
	if sum(bd2)=0 then goto Bud2
	L5080: !
return  ! /r
Bud1: ! r:
	mat tgb=(0): mat pgb=(0): mat bt1=(0)
	for j=1 to 5
		if bd3(j)<>0 then rewrite #h_budTrans,using "Form POS 139,2*PD 4",rec=bd2(j): x(3),x(3)
		if bd3(j)<>0 then read #h_budTrans,using 'Form POS 1,C 10,2*PD 4,24*PD 5.2,2*PD 4,PD 3',rec=bd2(j): z$,mat bt1,nba
		x2=0
		for j3=1 to possibleServiceCount
			if srvName$(j3)<>"" then
				if penalty$(j3)="Y" then  ! add penalties up seperat
					pgb(x2+=1)=bt1(j3+1,1)
				else
					if bt1(j3+1,1)>0 then tgb(x2+=1)=bt1(j3+1,1)
				end if
			end if
		next j3
		if sum(tgb)=x(2) then
			goto L5280
		else if sum(tgb)+sum(pgb)=x(2) then
			goto L5230
		else
			goto L5260
		end if
		L5230: !
		for x=1 to udim(tgb)
			tgb(x)+=pgb(x)
		next x
		L5260: !
	next j
	mat tgb=(0) ! no matches found
	L5280: !
	if sum(tgb)>0 then havebudget=1 else havebudget=0
return  ! /r

def fn_checkEscrow(&escrow,escrowbal,mat gb,&amt,possibleServiceCount; ___,j,a2,a1,tg,returnN)
	for j=1 to possibleServiceCount
		tg+=gb(j)
	next j
	if transType=3 and amt<>tg then
		a1=tg-amt
		if escrowbal>=a1 then a2=a1 else a2=escrowbal
		amt+=a2
		returnN=-a2
	end if
	fn_checkEscrow=returnN
fnend
def fn_printListings
	x$=cnvrt$("pic(######)",transDate)
	if ub_collDisableDepositList$='True' then
	 xti1=1
	else
		xti1=3
	end if
	if ub_collPrintInAccountOrder$='False' then
		srt=1
	else
	 srt=2
	end if

	if xti1=1 then
		fnopenprn("Receipt Listing") : ti1_start=1 : ti1_end=1
	else if xti1=2 then
		fnopenprn("Deposit Listing") : ti1_start=2 : ti1_end=2
	else ! if ub_collDisableDepositList$='False' then
		fnopenprn("Receipt and Deposit Listing") : ti1_start=1 : ti1_end=2
	end if
	dim brk(5,10)
	mat ct=(0)
	mat brk=(0)
	if srt=2 then
		! r: Create Sort for UBIPCOLINP
		close #hTransUnposted: ioerr ignore
		fnFree('[temp]\acs\collections_sort_address_s[session].int')
		open #h_control=fnH: "Name=[Temp]\acs\collections_sort_control_s[Session].int,RecL=128,Replace", internal,output
		write #h_control,using 'Form POS 1,C 128': "File "&collections_filename$&",,,"&env$('temp')&'\acs\collections_sort_address_s'&session$&'.int,,,,,A,N'
		write #h_control,using 'Form POS 1,C 128': "Mask 1,11,C,A"
		close #h_control:
		execute "SORT [Temp]\acs\collections_sort_control_s[Session].int -n"
		open #hTransUnposted=fnH: "Name="&collections_filename$,i,outi,r
		open #h_addr=fnH: "Name="&env$('temp')&'\acs\collections_sort_address_s'&session$&'.int',internal,outIn
		! /r
	end if
	for xti1=ti1_start to ti1_end
		p2=r5=totcheck=totcash=totescrow=totalCollections=totalDebitMemos=totalCreditMemos=0
		mat totalByService=(0)
		if srt=2 then
			restore #h_addr:
		end if
		gosub PrHeader
		do
			PS_LOOP_TOP: ! r:
			if srt=2 then
				read #h_addr,using "Form POS 1,PD 3": r5 eof PS_TOTALS
			else
				r5+=1
			end if
			if r5>lrec(hTransUnposted) then goto PS_TOTALS
			read #hTransUnposted,using F_ubColInp,rec=r5: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow conv PS_TOTALS noRec PS_LOOP_TOP
			nam$=""
			read #hCustomer1,using "Form POS 41,C 28,pos 143,pd 2",key=x$,release: nam$,wcode nokey ignore
			if trim$(x$)="" or (transAmount=0 and escrow=0) then goto PS_LOOP_TOP
			if xti1=2 and uprc$(ltrm$(rtrm$(rcpt$)))(1:2)="CA" then
				totcash+=transAmount
				goto PS_LOOP_TOP ! Skip CASH RECEIPTS ON DEPOSIT LIST
			end if
			if xti1=2 and transType<>3 then goto PS_LOOP_TOP ! Skip all entries except receipts.
			totcheck+=transAmount
			fn_totalAdd(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
			if env$('client')="Ash Grove" and (wcode<1 or wcode>10) then wcode=1
			! pr mat water : pause
			if env$('client')="Ash Grove" and transType=3 then water(wcode)=water(wcode)+alloc(1) ! TOTAL WATER BY WATER CODE
			for j=1 to validSrvCount
				totalByService(j)+=alloc(j)
				brk(transType,j)=brk(transType,j)+alloc(j)
			next j
			if transType=3 or transAmount=0 then c$=" "
			if transType=5 then c$="Dm"
			if transType=4 then c$="Cm"
			!
			if xti1=1 then
				if uprc$(escrow$)="Y" then
					pr #255,using L3090: r5,x$,transAmount,c$,transDate,rcpt$,mat alloc,escrow,nam$ pageoflow PgOf
					L3090:    form pos 1,n 4,x 2,c 10,n 10.2,c 4,pic(zz/zz/zz),x 2,c 9,validSrvCount*n 8.2,n 8.2,x 3,c 30
					totescrow+=escrow
				else
					pr #255,using L3130: r5,x$,transAmount,c$,transDate,rcpt$,mat alloc,nam$ pageoflow PgOf
					L3130:    form pos 1,n 4,x 2,c 10,n 10.2,c 4,pic(zz/zz/zz),x 2,c 9,validSrvCount*n 8.2,x 3,c 30
				end if
			else if xti1=2 then
				pr #255,using 'form pos 1,c 15,n 10.2,x 1,c 15': x$,transAmount,nam$(1:15) pageoflow PgOf
			end if
		loop ! goto PS_LOOP_TOP ! /r
	PS_TOTALS: ! r:
	! print "got to totals",xti1 : pause
	if xti1=2 then
		! r: DEPOSIT_LIST_TOTAL
		pr #255,using F_PR_TOTALS: "__________","Total Checks",totcheck
		pr #255,using F_PR_TOTALS: "          ","Total Cash",totcash
		pr #255,using F_PR_TOTALS: "__________","Total Deposit",totcheck+totcash
		pr #255,using "form pos 16,c 10": "=========="
		F_PR_TOTALS: form pos 16,c 10,skip 1,pos 1,c 15,n 10.2
		if env$('client')="Ash Grove" then mat water=(0)
		! /r
	else ! ub_collDisableDepositList$='True'
		pr #255: ""
		pr #255,using " Form POS 24,C 32": "************ Totals ************"
		pr #255: ""
		pr #255: ""
		pr #255,using "Form POS 34,4*C 12": " Collections","         C/M","         D/M","       Total"
		pr #255,using F_PR_TOTAL_STUFF: "Totals",totalCollections,totalCreditMemos,totalDebitMemos,totalCollections+totalCreditMemos+totalDebitMemos
		pr #255: ""
		for j=1 to validSrvCount
			pr #255,using F_PR_TOTAL_STUFF: validServiceLabel$(j),brk(3,j),brk(4,j),brk(5,j),totalByService(j) pageoflow PgOf
		next j
		if uprc$(escrow$)="Y" then pr #255,using F_PR_TOTAL_STUFF: "Escrow",totescrow
		pr #255: ""
		F_PR_TOTAL_STUFF:  form pos 4,c 30,4*n 12.2
		if env$('client')="Ash Grove" then
			pr #255: ''
			pr #255: ''
			pr #255,using 'form pos 7,c 50': "Water Collection Breakdown by Rate Code"
			for j=1 to possibleServiceCount
				if water(j)<>0 then
					pr #255,using 'form pos 21,c 13,n 11.2,skip 1': "WATER CODE "&str$(j),water(j)
				end if
			next j
			mat water=(0)
		end if
	 end if
	 ! /r
	 if xti1<>ti1_end then pr #255: newpage ! and env$('acsDeveloper')=''
	next xti1
	fncloseprn
	close #h_addr: ioerr ignore
	h_addr=0
fnend  ! if l3=1 then goto Merge else goto ScreenMenu1
PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue  ! /r
PrHeader: ! r:
	if xti1=1 then
		pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
		pr #255: "\qc  {\f181 \fs22 \b Receipt Listing}"
		pr #255: "\qc  {\f181 \fs16 \b "&trim$(date$('month, d, ccyy'))&"}"
		pr #255,using 'form pos 1,c 82,c 5,n 4': "\ql "&date$,"Page ",p2+=1
		pr #255: hd1$
	else if xti1=2 then
		pr #255: "\ql  {\f181 \fs18 \b "&env$('cnam')&"}"
		pr #255: "\ql  {\f181 \fs22 \b Deposit Listing}"
		pr #255: "\ql  {\f181 \fs16 \b "&trim$(date$('month, d, ccyy'))&"}"
		pr #255,using 'form pos 1,c 30,c 5,n 4': "\ql "&date$,"Page ",p2+=1
		pr #255: ''
		pr #255: "{\ul Account   }         {\ul Amount} {\ul Name          }"
	end if
return  ! /r
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn

		possibleServiceCount=10
		dim z$*10

		dim original(10)
		dim ay(7)
		dim water(10)
		dim gb(10)
		dim pgb(10)

		dim tg(11)
		dim x(3)
		dim x$*10
		dim x1$*10
		dim resp$(30)*128
		dim collections_filename$*128
		dim mesg$(10)*128
		dim ml$(1)*256

		dim badr(2)
		dim ba(13)
		dim bt1(14,2)
		dim bd1(5)
		dim bd2(5)
		dim bd3(5) ! bd$(5)*30,n$*30,txt$*80,notuse(10),

		dim coll_type_option$(3)
		coll_type_option$(1)="Regular Collection"
		coll_type_option$(2)="Credit Memo"
		coll_type_option$(3)="Debit Memo"

		tab$=chr$(9)

		collections_filename$="[Q]\UBmstr\Collections-"&env$('acsUserId')&".h[cno]"

		open #20: "Name=[Q]\UBmstr\Company.h[cno],NoShr",internal,input
		read #20,using "Form pos 128,C 1,c 1": receipt$,escrow$
		close #20:

		dim srvName$(10)*20
		dim srv$(10)*2
		dim penalty$(10)*1
		dim apply(10)
		fnGetServices(mat srvName$, mat srv$, mat unused_tax_code$,mat penalty$,mat unused_subjectto,mat apply)
		for srvnameItem=1 to udim(mat srvName$)
			srvName$(srvnameItem)=trim$(srvName$(srvnameItem))
		nex srvnameItem
		if srvName$(1)="Water" then havewater=1
		if srvName$(3)<>"Electric" and srv$(3)="EL" then srvName$(3)=""
		if srvName$(4)<>"Gas" and srv$(4)="GA" then srvName$(4)=""
		if srvName$(2)="Sewer" then havesewer=1
		! if srvName$(3)="Electric" then haveelectric=1
		! if srvName$(4)="Gas" then havegas=1
		dim allocOrder(0)
		applyItem=0
		for j=1 to possibleServiceCount
			original(j)=apply(j)
			if apply(j)>0 then
				mat allocOrder(applyItem+=1)
				allocOrder(applyItem)=apply(j) ! set order of applying     collections
			end if
		next j
		dim baOrder(10)
		for j=1 to possibleServiceCount
			for j1=1 to possibleServiceCount
				if j=original(j1) then baOrder(j)=j1
			next j1
		next j
		if env$('client')="Divernon" then mat baOrder=original ! may need to be made standard for everyone   !!!
		dim hd1$*260
		hd1$="{\ul Rec }  {\ul Account   }  {\ul    Total}    {\ul   Date  }  {\ul ReceiptNo}"
		for j=1 to possibleServiceCount
			if fn_serviceValidForCollAlloc(j) then
				validSrvCount+=1
				hd1$=hd1$&"  {\ul "&rpad$(srvName$(j),6)(1:6)&"}"
				mat validServiceLabel$(validSrvCount)
				validServiceLabel$(validSrvCount)=srvName$(j)(1:28)&":"
			end if
		next j
		if uprc$(escrow$)="Y" then hd1$=rtrm$(hd1$)&"  {\ul Escrow}"
		dim totalByService(10)
		mat totalByService(validSrvCount)
		dim alloc(10)
		mat alloc(validSrvCount)

		mat allocOrder(validSrvCount)

		dim tgb(10)
		mat tgb(validSrvCount)
		dim hgb(10)
		mat hgb(validSrvCount)
		dim validServiceLabel$(0)*30
		mat validServiceLabel$(validSrvCount)
		F_ubColInp: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pos 24,c 9,validSrvCount*pd 4.2,5*pd 3,pd 4.2
	end if
	! r: setup column PrHeaders (mat chdr$) and column masks (mat cm$) for flex grid on ScreenMenu1
		dim chdr$(20)*30
		dim cm$(20)
		mat chdr$(20)
		chdr$(1)="Rec"
		chdr$(2)="Account"
		chdr$(3)="Amount"
		chdr$(4)="Date"
		chdr$(5)="Type"
		! chdr$(6)="PC"
		chdr$(6)="Receipt Number"
		cHdrItem=6
		for j2=1 to possibleServiceCount
			if srvName$(j2)<>"" then
				chdr$(cHdrItem+=1)=rpad$(srvName$(j2),10)(1:10)
				chdr$(cHdrItem)=srep$(chdr$(cHdrItem),':','')
			end if
		next j2
		if uprc$(escrow$)="Y" then chdr$(cHdrItem+=1)="Escrow"
		mat chdr$(cHdrItem)

		mat cm$=("")
		cm$(2)="32"
		cm$(3)="10"
		cm$(4)="1"
		cm$(5)=""
		cm$(6)=""
		! cm$(7)=""
		for j=7 to max(9,udim(chdr$))
			cm$(j)="10"
		next j
		mat cm$(udim(chdr$))
	! /r
	ei_item_account=1
	ei_item_amount=2
	ei_item_date_time=3
	ei_item_collection_type=4

	fnreg_read('Collections pr in Account Order',ub_collPrintInAccountOrder$,'False')
	fnreg_read('Collections Disable Deposit List',ub_collDisableDepositList$,'False')
fnend
def fn_haveMainBudget(h_budmstr,x1$; ___,returnN)
	read #h_budmstr,using 'Form POS 1,C 10,PD 4,12*PD 5.2,2*PD 3',key=x1$: z$,mat ba,mat badr nokey HmbFinis ! get mat ba again
	if ba(2)+ba(3)+ba(4)+ba(5)+ba(6)+ba(7)+ba(8)+ba(9)+ba(10)+ba(11)>0 then returnN=1
	HmbFinis: !
	fn_haveMainBudget=returnN
fnend
def library fnBreakdown(hCustomer1,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baOrder,&ckey)
	fn_setup
	fnBreakdown=fn_breakdown(hCustomer1,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baOrder,ckey)
fnend
def fn_breakdown(hCustomer1,h_budmstr,x1$,havebudget, mat tgb, mat alloc,mat baOrder,&ckey)
	! returns 0 if canceled, else 1  (back returns 2)
	! hCustomer1 - customer file handle with account number key
	! x1$ - customer account number key
	! havebudget - 1=yes customer is on budget billing, 0=no
	! mat tgb -
	! mat alloc -
	! mat baOrder -
	! x(2) is transaction amount
	! if env$('acsDeveloper')<>'' then pause


	dim nam$*30
	read #hCustomer1,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2,pos 143,7*pd 2',key=x1$,release: nam$,bal,db1,mat gb,escrowbal,mat ay ! nokey BD_TOS   ! removed nokey trap on 12/9/2018 - if we can't find the customer than allocation is going to fail anyway.
	dim extra(23)
	fnapply_default_rates(mat extra, mat ay)

	s01rate=ay(1)           !  Service 1 (Water) – Rate Code                      , ! SPos= 143 EPos=144
	s02rate=ay(2)           !  Service 2 (Sewer) – Rate Code                      , ! SPos= 145 EPos=146
	! s03rate=ay(3)           !  Service 3 (Electric or Lawn Meter) – Rate Code     , ! SPos= 147 EPos=148
	s04rate=ay(4)           !  Service 4 (Gas) – Rate Code                        , ! SPos= 149 EPos=150
	! s05rate=ay(5)           !  Service 5 – Rate Code                              , ! SPos= 151 EPos=152
	! s09rate=ay(6)           !  Service 9 (Sales Tax) – Rate Code                  , ! SPos= 153 EPos=154
	! s10rate=ay(7)           !  Service 10 (Penalty) – Rate Code                   , ! SPos= 155 EPos=156


	gosub BuildAllocations
	BD_TOS: !

	ckey=fn_askAllocations(x1$,mat srvName$,mat gb,mat alloc,mat validServiceLabel$, csv_import_in_process,escrow$,oldescrowbal,escrow)
	if ckey=1 then
		goto NEXT_AFTER_BREAKDOWN
	else if ckey=2 then
		goto BD_FINIS
	else if ckey=5 then
		bd_return=0
		goto BD_FINIS
	else
		goto NEXT_AFTER_BREAKDOWN
	end if
	!
	NEXT_AFTER_BREAKDOWN: !
	tal=sum(mat alloc) ! for j=1 to udim(mat alloc) : tal+=alloc(j) : next j
	if tal<>x(2) then
		mat mesg$(6)
		mesg$(1)="Total Allocations must equal Transaction Amount!"
		mesg$(2)=""
		mesg$(3)=cnvrt$("pic(-----------#.##)",x(2))&" (Transaction Amount)"
		mesg$(4)=cnvrt$("pic(-----------#.##)",tal)&" (Total Allocations) "
		mesg$(5)="___________"
		mesg$(6)=cnvrt$("pic(-----------#.##)",x(2)-tal)&" (Difference)"
		fnmsgbox(mat mesg$, resp$,'',48)
		goto BD_TOS ! (skip re-reading of record) ! goto BREAKDOWN
	end if
	bd_return=1
	BD_FINIS: !
	fn_breakdown=bd_return
fnend
def fn_askAllocations(x1$,mat srvName$,mat gb,mat alloc,mat validServiceLabel$; csv_import_in_process,escrow$,oldescrowbal,escrow,___,aaRespC,bd_line_add,ckey)
	fnTos
	fnLbl(1,1,"Account:",30,1)
	fnTxt(1,32,10,0,1,"",1)
	resp$(aaRespC+=1)=x1$
	fnLbl(2,1,"Customer Name:",30,1)
	fnTxt(2,32,30,0,0,"",1)
	resp$(aaRespC+=1)=trim$(nam$)
	fnLbl(3,1,"Transaction Amount:",30,1)
	fnTxt(3,32,10,12,1,"10",1)
	resp$(aaRespC+=1)=str$(x(2))
	fnLbl(5,1,"Enter Allocation Breakdown amounts.",54,2)
	fnLbl(6,31,"Allocation",10,2)
	fnLbl(6,44,"Balance",10,2)
	dim bd_real(11)
	mat bd_real=(0)
	bd_line_add=validServiceItem=0
	for possibleServiceItem=1 to possibleServiceCount
		bdAllocItem=0
		if fn_serviceValidForCollAlloc(possibleServiceItem) then
			bd_line_add+=1
			validServiceItem+=1
			fnLbl(bd_line_add+6,1,validServiceLabel$(validServiceItem),29,1)
			fnTxt(bd_line_add+6,44,12,0,1,"10",1)
			! resp$(aaRespC+=1)=str$(tgb(bd_line_add))
			resp$(aaRespC+=1)=str$(gb(possibleServiceItem))
			fnTxt(bd_line_add+6,32,12,0,1,"10")
			resp$(bd_real(bd_line_add):=aaRespC+=1)=str$(alloc(validServiceItem))
			! bd_real(bd_line_add)=aaRespC
		end if
	next possibleServiceItem

	if uprc$(escrow$)="Y" then
		bd_line_add+=1
		fnLbl(bd_line_add+6,1,"Escrow:",29,1)
		resp$(aaRespC+=1)=str$(oldescrowbal)
		fnTxt(bd_line_add+6,44,12,0,1,"10",1)
		resp$(aaRespC+=1)=str$(escrow)
		fnTxt(bd_line_add+6,32,12,0,1,"10")
		bd_real(bd_line_add)=aaRespC
	end if
	if csv_import_in_process then
		fnCmdKey("&Save",1,1) : fnCmdKey("&Skip",5,0,1)
	else
		fnCmdSet(6) ! fnCmdKey("&Next",1,1) : fnCmdKey("&Back",2) : fnCmdKey("&Cancel",5,0,1)
	end if
	ckey=fnAcs(mat resp$)

	if env$('acsDeveloper')<>'' then debug=1
	for j=1 to udim(mat alloc)
		if bd_real(j)<>0 then
			if debug then
				pr 'setting ('&srvName$(j)&') alloc('&str$(j)&') to '&resp$(bd_real(j))&'. '
			end if
			alloc(j)=val(resp$(bd_real(j)))
		else
			alloc(j)=0
		end if
	next j
	! if debug then
	! 	pr 'dev-pause CC'
	! 	pause
	! end if
	if uprc$(escrow$)="Y" then escrow=val(resp$(bd_real(j)))
	fn_askAllocations=ckey
fnend


def fn_serviceValidForCollAlloc(j; ___,returnN) ! uses local mat srvName$
	if srvName$(j)<>"" and srvName$(j)(1:5)<>"Reduc" then
		returnN=1
	end if
	fn_serviceValidForCollAlloc=returnN
fnend
BuildAllocations: ! r: returns mat alloc, mat tgb,transType,escrow$,oldescrowbal,escrowbal
	if editmode=1 then
		mat tgb=alloc
	else
		j2=0
		if havebudget=1 then
		else ! if havebudget<>1 then
			! pause
			for j=1 to possibleServiceCount
				if fn_serviceValidForCollAlloc(j) then
					tgb(j2+=1)=gb(j)
				end if
			next j
	! 		pr '-a-' : gosub DisplayDebugAlloc
		end if
	end if
	if uprc$(escrow$)="Y" and transType=3 then ! add escrow balance into last allocation if have escrow and processing a collection transaction
		oldescrowbal=escrowbal
	end if

	! pr '-b-' : gosub DisplayDebugAlloc

	bd_tgbj=0 ! amount allocated so far    (i think)
	tn=0  ! total negative breakdowns
	mat ba=(0) : mat badr=(0)
	for j=1 to udim(mat alloc)
		if tgb(j)<0 then tn-=tgb(j) ! Total Negative Breakdowns
	next j

	! pr '-c-' : gosub DisplayDebugAlloc

	havemainbudget=fn_haveMainBudget(h_budmstr,x1$)
	validServiceItem=0
	for j=1 to udiM(allocorder) ! to udim(mat alloc)
		! r: new allocation logic
		! if fn_serviceValidForCollAlloc(j) then
			validServiceItem=allocorder(j) ! +=1 ! if allocOrder(validServiceItem)=6 then pause
			alloc(allocOrder(validServiceItem))=max(0,min(x(2)-bd_tgbj+tn,tgb(allocOrder(validServiceItem))))
			bd_tgbj+=alloc(allocOrder(validServiceItem))
			if tgb(allocOrder(validServiceItem))<0 then tn+=tgb(allocOrder(validServiceItem))
		! end if
		! print j,validserviceitem, alloc(allocOrder(validServiceItem)), sum(alloc)
		! pause
		! /r
		! r: old allocation logic
		!   if ~(havemainbudget=1 and penalty$(baOrder(j))="Y") then ! ELSE don't allow penalty budgets amount to go thru routine
		!
		!   	if havemainbudget=1 then
		!   		alloc(allocOrder(j))=max(0,min(x(2)-bd_tgbj,ba(baOrder(j)+1)))
		!   		pr 'a. setting alloc(allocOrder(j='&str$(j)&')='&str$(allocOrder(j))&')='&str$(alloc(allocOrder(j)))
		!   	else
		!   		! whichTgb=srch(mat apply,allocOrder(j))
		!   		! alloc(allocOrder(j))=max(0,min(x(2)-bd_tgbj+tn,tgb(whichTgb)))
		!   		alloc(allocOrder(j))=max(0,min(x(2)-bd_tgbj+tn,tgb(allocOrder(j))))
		!   		pr 'b. setting alloc(allocOrder(j='&str$(j)&')='&str$(allocOrder(j))&')='&str$(alloc(allocOrder(j)))
		!   	end if
		!
		!   	bd_tgbj+=alloc(allocOrder(j))
		!   	if tgb(allocOrder(j))<0 then tn+=tgb(allocOrder(j))
		!
		!   end if
		! /r
	next j
	! pr '-d-' : gosub DisplayDebugAlloc

	if havemainbudget=1 and sum(alloc)<x(2) then
		for j=1 to udim(mat alloc) ! if have budget and pay more than budget, how to allocate remainder
			if alloc(allocOrder(j))=0 then
				alloc(allocOrder(j))=max(0,min(x(2)-bd_tgbj+tn,tgb(allocOrder(j))))
				bd_tgbj+=alloc(allocOrder(j))
				if tgb(allocOrder(j))<0 then tn+=tgb(allocOrder(j))
			end if
		next j
	end if

	! pr '-e-' : gosub DisplayDebugAlloc

	! r: allocate overpament amount
	if (env$('client')="Billings" or env$('client')='Diamond') and s02rate>0 then !  put excess in service 2 - sewer
		alloc(2)=alloc(2)+x(2)-bd_tgbj
		goto BuildAllocations_FINIS
	else if env$('client')="Findlay" and  s04rate>0 then ! excess in gas
		alloc(3)=alloc(3)+x(2)-bd_tgbj
		goto BuildAllocations_FINIS
	else if (1)>0 then ! if no gas, put excess in water
		alloc(1)+=x(2)-bd_tgbj
		goto BuildAllocations_FINIS
	end if

	! pr '-e-' : gosub DisplayDebugAlloc

	if alloc(1)>0 or (havewater=1 and s01rate>0) then ! excess in water if it is an active service for this customer
		alloc(1)=alloc(1)+x(2)-bd_tgbj
		goto BuildAllocations_FINIS
	else if alloc(2)>0 or (havesewer=1 and s02rate>0) then ! excess in sewer if it is an active service for this customer
		alloc(2)+=x(2)-bd_tgbj
		goto BuildAllocations_FINIS
	else if udim(mat alloc)>=3 then
		if alloc(3)>0 then ! excess in electric if it is an active service for this customer
			alloc(3)+=x(2)-bd_tgbj
			goto BuildAllocations_FINIS
		else if udim(mat alloc)>=4 then
			if alloc(4)>0 then ! excess in gas if it is an active service for this customer
				alloc(4)+=+x(2)-bd_tgbj
				goto BuildAllocations_FINIS
			end if
			alloc(1)+=x(2)-bd_tgbj ! if excess not allocated to any other service, allocate it to water
		end if
	end if
	! /r
	! pr '-f-' : gosub DisplayDebugAlloc

	BuildAllocations_FINIS: !
return  ! /r
! DisplayDebugAlloc: ! r:
! 	if env$('acsDeveloper')<>'' then
! 		pr rpad$('Service',20);'allocation';'    mt gb'
! 		for dda=1 to validSrvCount
! 			pr rpad$(validServiceLabel$(dda),20);cnvrt$('pic(---,--#.--)',alloc(dda));cnvrt$('pic(---,--#.--)',gb(dda))
! 		nex dda
! 		! pr 'dev-pause AA'
! 		! pause
! 	end if
! return ! /r

def fn_open_cash_drawer
	fnopen_receipt_printer
	pr #255,using 'form pos 1,c 9,skip 0': hex$("1B70302828") ioerr ignore ! apg cash drawer hooked to epson t 88 thermal receipt printer
	fnclose_receipt_printer
fnend
def fn_print_receipt(pr_acct_key$,pr_acct_name$*30,rcpt$,bal,pr_trans_amt,pr_trans_date,coll_type$*30)
	if fnopen_receipt_printer(1) then
		receipt_width=32
		pr #255,using 'form pos 1,C 2,Cc '&str$(receipt_width-4)&',C 2': '**',env$('cnam')(1:28),'**'
		pr #255,using 'form pos 1,Cc '&str$(receipt_width): date$('mm/dd/ccyy')
		pr #255,using 'form pos 1,Cc '&str$(receipt_width): time$
		if rcpt$<>'' then
			pr #255: "Receipt Number: "&rcpt$
			pr #255: ''
		end if
		pr #255: ''
		pr #255: 'Account: '&pr_acct_key$
		pr #255: '   Name: '&pr_acct_name$
		pr #255: ''
		pr #255,using 'form pos 1,Cc '&str$(receipt_width): coll_type$
		pr #255: ' Amount: '&cnvrt$('G 10.2',pr_trans_amt)
		!   pr #255: 'Bal Before Payment: '&cnvrt$('G 10.2',bal)
		!   pr #255: '              Date: '&cnvrt$('pic(##/##/##)',pr_trans_date)
		pr #255: ''
		pr #255: '________________________________' ! 32 characters - perfect max width fit for my POs-58 usb receipt printer
		pr #255: ''
		fnclose_receipt_printer
	end if
fnend
def fn_csv_import
	fnureg_read('Collections CSV Import Filename',ecp_filename$)
	fnureg_read('Collections CSV Import Skip Duplicates',enableSkipDuplicates$,'True')
	! if ecp_filename$='' then ecp_filename$=os_filename$(env$('Desktop'))&"\ACS_ECP_Export.txt"
	EI_SCREEN1: !
	fnTos
	fnLbl(1,1,"Import CSV Path and File Name:",33,1)
	fnTxt(1,35,40,256,0,"71")
	resp$(1)=ecp_filename$
	! fnLbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
	fnchk(2,1,'Skip Duplicates')
	resp$(2)=enableSkipDuplicates$

	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		ecp_filename$=resp$(1)
		enableSkipDuplicates$=resp$(2)
		fnureg_write('Collections CSV Import Filename',enableSkipDuplicates$)
		!
		fn_ei_backup(ecp_filename$)
		open #h_csv=fnH: "Name=[at]"&ecp_filename$,display,input ioerr EI_SCREEN1
		ecp_filename$=os_filename$(file$(h_csv))
		fnureg_write('Collections CSV Import Filename',ecp_filename$)
		type=fn_csv_type(h_csv)
		csv_import_in_process=1
		if type=1 then
			fn_ecp_import(h_csv)
		else if type=2 then
		! r: import standardized CSV (column PrHeaders enums already identified in fn_csv_type)
			dim csv_line$*512
			dim csv_item$(0)*256
			do
				linput #h_csv: csv_line$ eof CSV_FINIS
				if trim$(srep$(csv_line$,csv_delim$,''))<>'' then ! if not a blank line then
					csv_line$=fn_remove_quote_encap_commas$(csv_line$)
					csv_line$=fn_remove_quote_encap_commas$(csv_line$)
					str2mat(csv_line$,mat csv_item$,csv_delim$)
					trans_date_mmddyy=date(days(csv_item$(csv_date),'m/d/ccyy'),'mmddyy')
					csv_item$(csv_amount)=srep$(csv_item$(csv_amount),'$','')
					csv_item$(csv_amount)=srep$(csv_item$(csv_amount),',','')
					trans_amount=val(csv_item$(csv_amount))
					if trans_amount<0 then !  debit memos maybe used for bounced checks
						trans_type=5 ! debit memo
						trans_amount=abs(trans_amount)
					else
						trans_type=3 ! regular collection
					end if
					fn_addTransToUnposted(csv_item$(csv_account),trans_date_mmddyy,trans_type,trans_amount, enableSkipDuplicates$)
				end if
			loop
		CSV_FINIS: !
		! /r
		else
			mat ml$(9)
			ml$(1)="Unrecognized CSV or TXT Type."
			ml$(2)="Make sure your CSV (comma seperated values) file has each of the following column headings:"
			ml$(3)=tab$&"Date"
			ml$(4)=tab$&"Account"&tab$&'("Acct" and "Acct #" are acceptable also.)'
			ml$(5)=tab$&"Amount"&tab$&'("Amt" and "Payment" are acceptable also.)'
			ml$(6)=tab$&"Type"&tab$&'"Cash" for cash.' ! '  "Chk[checknumber]" for checks.'
			ml$(7)=tab$&tab$&'("Pmt Type" is acceptable also.)'
			ml$(8)="Negative amounts will be entered as Debit Memos."
			ml$(9)="Tab or Comma delimiters are allowed."
			fnmsgbox(mat ml$, response$, '',64)
		end if
		close #h_csv: ioerr ignore
		csv_import_in_process=0
		fn_taeClose
	end if
fnend
def fn_csv_type(h_csv)
	! returns 1 for ECP_IMPORT type CSV File
	! returns 2 for standardized CSV type file
	! returns 0 for unrecognized
	dim ct_line$*512
	dim ct_item$(0)*256
	linput #h_csv: ct_line$
	if pos(ct_line$,tab$)>0 then csv_delim$=tab$ else csv_delim$=','
	str2mat(ct_line$,mat ct_item$,csv_delim$)
	if udim(mat ct_item$)<4 then ct_return=0 : goto CT_FINIS
	ct_item_2_val=val(ct_item$(2)) conv CT_ITEM_2_CONV
	ct_return=1 ! second item on first line has a numeric value - it's ECP_IMPORT type
	goto CT_FINIS
	CT_ITEM_2_CONV: !
	for ct_item=1 to udim(mat ct_item$)
		ct_item$(ct_item)=trim$(lwrc$(ct_item$(ct_item)))
	next ct_item
	csv_date=max(0,srch(mat ct_item$,'date'))
	csv_account=max(0,srch(mat ct_item$,'acct #'))
	if csv_account<=0 then
		csv_account=max(0,srch(mat ct_item$,'acct'))
		if csv_account<=0 then
			csv_account=max(0,srch(mat ct_item$,'account'))
		end if
	end if
	csv_type=max(0,srch(mat ct_item$,'pmt type'))
	if csv_type<=0 then
		csv_type=max(0,srch(mat ct_item$,'type'))
	end if
	csv_amount=max(0,srch(mat ct_item$,'payment'))
	if csv_amount<=0 then
		csv_amount=max(0,srch(mat ct_item$,'amt'))
		if csv_amount<=0 then
			csv_amount=max(0,srch(mat ct_item$,'amount'))
		end if
	end if
	if csv_account and csv_amount and csv_date and csv_type then
		ct_return=2
	end if
	CT_FINIS: !
	fn_csv_type=ct_return
fnend
def fn_remove_quote_encap_commas$*512(rqec_line$*512)
	for rqec_char=1 to len(rqec_line$)
		tmp_chr$=rqec_line$(rqec_char:rqec_char)
		if tmp_chr$='"' then
			rqec_inside_quote+=1
			if rqec_inside_quote=2 then rqec_inside_quote=0
		else if tmp_chr$=',' then
			if rqec_inside_quote then rqec_line$(rqec_char:rqec_char)=';' ! replace the quote with a semi-colon.
		end if
	next rqec_char
	fn_remove_quote_encap_commas$=rqec_line$
fnend
def fn_ecp_import(h_ecp)
	dim ecp_filename$*256
	dim ei_line$*512
	dim ei_item$(0)*256
	if ~fnClientHas('U5') then
		mat ml$(2)
		ml$(1)="You must purchase the ACS Utility Billing External Collections Processing"
		ml$(2)="module to import this type of CSV."
		fnmsgbox(mat ml$, response$, '',64)
		goto EI_FINIS
	end if
	restore #h_ecp:
	! r: main loop
	do
		!
		linput #h_ecp: ei_line$ eof EI_FINIS
		str2mat(ei_line$,mat ei_item$,csv_delim$)
		ei_item_account=1
		ei_item_amount=2
		ei_item_date_time=3
		ei_item_collection_type=4 ! Check or Credit
		trans_date_mmddyy=date(days(ei_item$(ei_item_date_time)(1:pos(ei_item$(ei_item_date_time),' ')-1),'m/d/ccyy'),'mmddyy')
		trans_amount=val(ei_item$(ei_item_amount))
		if trans_amount<0 then !  debit memos maybe used for bounced checks
			trans_type=5 ! debit memo
			trans_amount=abs(trans_amount)
		else
			trans_type=3 ! regular collection
		end if
		fn_addTransToUnposted(ei_item$(ei_item_account),trans_date_mmddyy,trans_type,trans_amount, enableSkipDuplicates$)
	loop
	! /r
	EI_FINIS: !
fnend
def fn_taeReportAdd(p$*10,tdate,tcode,tamount)
	if ~taeReportSetup then
		taeReportSetup=1
		fnopenprn('Import Transactions - Skipped Duplicates')
		pr #255: '             Import Transactions - Skipped Duplicates             '
		pr #255: 'Account      Date       Type                                Amount'
		pr #255: '__________  __________  ____________________  ____________________'
	end if
	pr #255: p$&'  ';
	pr #255: date$(days(tdate,'mmddyy'),'mm/dd/ccyy')&'  ';
	pr #255: rpad$(fn_collType$(tcode),20)&'  ';
	pr #255: cnvrt$('pic(-,---,---,---,--0.00)',tamount)
fnend
def fn_taeClose
	taeSetup=0
	close #hTauTrans: ioerr ignore
	hTauTrans=0
	taeReportSetup=0
	fncloseprn
fnend
def fn_transAlreadyInUnposted(p$,tdate,tcode,tamount; ___,returnN,tauX$*10,tauAmount,tauDate,tauCode)
	restore #hTransUnposted: ! ,key=>fnbuildkey$('UB Transaction UnPosted',mat tau$,mat tauN, 1): nokey TaeFinis
	do
		read #hTransUnposted,using F_ubColInp: tauX$,tauAmount,tauDate,tauCode eof TaeFinis

		if tauX$=p$ and tauDate=tdate and tauCode=tcode and tauAmount=tamount then
			returnN=1
		end if
	loop until returnN !  or EoF
	TaeFinis: !
	fn_transAlreadyInUnposted=returnN
fnend
def fn_transAlreadyPosted(p$,tdate,tcode,tamount; ___,returnN,tapKeyMatch)
	! tdate is expected in ccyymmdd format here
	if ~tapSetup then
		tapSetup=1
		dim tap$(0)*256,tapN(0)
		hTapTrans=fn_openFio('UB Transaction',mat tap$,mat tapN, 1,1)
	end if
	returnN=0
	mat tap$=('')
	mat tapN=(0)
	tap$(trans_acct)=p$
	tapN(trans_tdate)=tdate
	tapN(trans_tcode)=tcode
	! tapN(trans_tamount)=tamount
	dim tapKey$*19
	tapKey$=fnbuildkey$('UB Transaction',mat tap$,mat tapN, 1)
	restore #hTapTrans,key=>tapKey$: nokey TapFinis
	! pr 'testing PASSED '&p$&' '&str$(tdate)&' '&str$(tcode)&' '&str$(tamount)
	do
		read #hTapTrans,using form$(hTapTrans): mat tap$,mat tapN eof TapFinis
		! pr 'testing v read '&tap$(trans_acct)&' '&str$(tapN(trans_tdate))&' '&str$(tapN(trans_tcode))&' '&str$(tapN(trans_tamount))
		! pau
		if tap$(trans_acct)=p$ and tapN(trans_tdate)=tdate and tapN(trans_tcode)=tcode and tapN(trans_tamount)=tamount then
			returnN=1
		else
			returnN=0
		end if
	loop while ~returnN and tap$(trans_acct)=p$ and tapN(trans_tdate)=tdate and tapN(trans_tcode)=tcode
	TapFinis: !
	fn_transAlreadyPosted=returnN
fnend
def fn_ei_backup(ecp_filename$*256)
	if exists(ecp_filename$) then
		fnCopy(ecp_filename$,fnReportCacheFolderCurrent$&'\Electronic Collections Imported - '&date$('ccyy-mm-dd')&' '&fnSafeFilename$(time$)&'.csv')
	end if  ! exists UBmstr\readings.[bk$]
fnend
def fn_addTransToUnposted(at_customer$*10,at_date_mmddyy,at_trans_type,at_amount; enableSkipDuplicates$)
	b7=transType
	p$=" "
	rcpt$=""
	transType=b7
	at_customer$=rpad$(trim$(at_customer$),10)
	! r: read selected account and prepare data for ScreenAdd
	if fnKeyExists(hCustomer1,at_customer$, 1) then
		AT_ReadCustomer: !
		read #hCustomer1,using 'Form Pos 41,C 28,Pos 292,PD 4.2,PD 4,Pos 388,10*PD 5.2,pos 1859,pd 5.2',key=at_customer$,release: nam$,bal,db1,mat gb,escrowbal
		if enableSkipDuplicates$='True' and ( fn_transAlreadyPosted(at_customer$,date(days(at_date_mmddyy,'mmddyy'),'ccyymmdd'),at_trans_type,at_amount) or fn_transAlreadyInUnposted(at_customer$,at_date_mmddyy,at_trans_type,at_amount) ) then
			fn_taeReportAdd(at_customer$,at_date_mmddyy,at_trans_type,at_amount)
		else
			x1$=at_customer$
			havebudget=0
			fn_getMatTgb(mat tgb,escrow,mat gb,mat srvName$,escrow$,transType,escrowbal,oldescrowbal)
			! /r
			! ScreenAdd: !
			x(3)=at_date_mmddyy
			x(2)=at_amount
			transType=at_trans_type
			!
			do_not_blank_rcpt=0
			! rcpt$=trim$(resp$(3))(1:9)
			at_customer$=lpad$(trim$(at_customer$),10)
			! r: after ScreenAdd - actually do the adding stuff
				if sum(tgb)=x(2) then
					for j=1 to validSrvCount : alloc(j)=tgb(j) : next j
					goto AT_L2040
				end if
				if uprc$(escrow$)="Y" then escrow=fn_checkEscrow(escrowbal,mat gb,x(2),possibleServiceCount) ! check escrow balance
				mat hgb=tgb
				! gosub SeemsWrong2

			gosub Bud2
			gosub Bud1
			if ~fn_breakdown(hCustomer1,h_budmstr,at_customer$,havebudget, mat tgb, mat alloc,mat baOrder,ckey) then
				if csv_import_in_process then
					goto AT_FINIS
				else
					goto ScreenSelectAccount
				end if
			end if
			if ckey=2 then
				! if env$('acsDeveloper')<>'' then
				! 	pr 'dev-pause BB'
				! 	pause
				! 	! goto ScreenSelectAccount
				! end if
				for j=1 to validSrvCount : alloc(j)=tgb(j) : next j
			! else
			! 	goto AT_L2040
			end if
			! AT_L2020: ! r:
			! for j=1 to validSrvCount : alloc(j)=tgb(j) : next j
			AT_L2040: !
			transAmount=x(2) : transDate=x(3) : b7=transType
			postingCodeUnused=0
			if sum(tgb)=x(2) then gosub Bud2 ! kj 10/14/09
			if sum(tgb)=x(2) and bud1=1 then gosub Bud1 ! was commented out; changed to if sum= on 101409 to keep from skipping ubdget update if exact amount paid.
			r6=lrec(hTransUnposted)+1
			if escrow>90000 then escrow=0 ! PREVENT 726 ERROR
			write #hTransUnposted,using F_ubColInp,rec=r6: at_customer$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd2,escrow duprec L2060
			transType=b7
		end if
	else ! the account does not exist.
		at_customer$=trim$(at_customer$)
		x1_len=len(at_customer$)
		if x1_len<=9 and at_customer$(len(at_customer$)-1:len(at_customer$)-1)='.' then ! maybe excel messed it up, try adding a 0, because it has something like .1
			at_customer$=at_customer$&'0'
			if fnKeyExists(hCustomer1,at_customer$, 1) then
				goto AT_ReadCustomer
			end if
		end if
		if x1_len<=7 and pos(at_customer$,'.')<=0 then ! maybe excel messed it up, try adding a .00
			at_customer$=at_customer$&'.00'
			if fnKeyExists(hCustomer1,at_customer$, 1) then
				goto AT_ReadCustomer
			end if
		end if
		mat ml$(6)
		ml$(1)='Customer "'&at_customer$&'" could not be found.'
		! 140308.00
		! 110436.02
		ml$(2)='The following transaction will NOT be added.'
		ml$(3)=tab$&'Customer:'&tab$&at_customer$
		ml$(4)=tab$&'Date:'&tab$&date$(days(at_date_mmddyy,'mmddyy'),'mm/dd/ccyy') ! this date is not formatting on the screen properlly
		ml$(5)=tab$&'Type:'&tab$&str$(at_trans_type)
		ml$(6)=tab$&'Amount:'&tab$&str$(at_amount)
		fnmsgbox(mat ml$,resp$,'',0)
	end if

	goto AT_FINIS ! /r
		! pr 'completed add' : pause ! goto ScreenSelectAccount ! /r
	AT_NO_CUSTOMER: ! r:
	goto AT_FINIS ! /r
	AT_FINIS: !
fnend
def fn_oSub1(coll_type_option$) ! returns appropriate transType based on coll_type_option$
	oSub1Return=0
	! 1="Regular Collection",transType=3
	! 2="Credit Memo",transType=4
	! 3="Debit Memo",transType=5
	if coll_type_option$=coll_type_option$(1) then
		oSub1Return=3
	else if coll_type_option$=coll_type_option$(2) then
		oSub1Return=4
	else if coll_type_option$=coll_type_option$(3) then
		oSub1Return=5
	end if
	fn_oSub1=oSub1Return
fnend
def fn_collType$(transType) ! returns appropriate coll_type_option$ based on 0(1)
	! 1="Regular Collection",transType=3
	! 2="Credit Memo",transType=4
	! 3="Debit Memo",transType=5
	collTypeReturn$=''
	if transType=3 then
		collTypeReturn$=coll_type_option$(1)
	else if transType=4 then
		collTypeReturn$=coll_type_option$(2)
	else if transType=5 then
		collTypeReturn$=coll_type_option$(3)
	end if
	fn_collType$=collTypeReturn$ ! &' ('&str$(transType)&')' ! pr collTypeReturn$ : pause
fnend
! READD: ! r: RE-ADD PROOF TOTALS
!   for j=1 to lrec(hTransUnposted)
!     r5=j
!     read #hTransUnposted,using F_ubColInp,rec=r5: x$,transAmount,transDate,transType,postingCodeUnused,rcpt$,mat alloc,mat bd3,escrow noRec L4690 eof L4700
!     fn_totalAdd(transType,transAmount,totalCollections,totalDebitMemos,totalCreditMemos)
! L4690: next j
! L4700: return  ! /r
def fn_totalAdd(transType,amt,&totalCollections,&totalDebitMemos,&totalCreditMemos)
	if transType=1 then
		totalCollections+=amt
	else if transType=3 then
		totalCollections+=amt
	else if transType=4 then
		totalCreditMemos+=amt
	else if transType=5 then
		totalDebitMemos+=amt
	end if
fnend

include: ertn no
include: fn_open