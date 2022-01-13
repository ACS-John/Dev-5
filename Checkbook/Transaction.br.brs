! replace S:\Checkbook\Transaction
! Checkbook transaction file editor
!
autoLibrary
on error goto Ertn
! r: dim
	dim tr$(5)*35,de$*30,bn$*40,aa(2)
	dim ml$(10)*80 ! message box message lines
	dim resp$(100)*100,gldesc$*30
	dim vn$*8,ph$*12 ! payee file
	dim tcde$*25 ! description of working transaction type
	dim chdr$(11),cmask$(11),item$(11) ! temp variables for flexgrids
	dim item$(10)*50 ! flex grid items
	dim tradesc$*30 ! for tralloc file
! /r
	fnTop(program$)
! r: constants and defaults
! constants
	cancel=99 : right=1 : limit_to_list=1 : center=2
	ccyymmdd$='3' : mmddyy$='1' : number$='30'
	pointtwo$='32' : disable=1 : add_all=2 : false=0
! defaults
	addloop$='True'
	d1=date('mmddyy') ! val(date$(4:5))*10000+val(date$(7:8))*100+val(date$(1:2))
	if d1<19999 then d1=d1+110000 : rollback=1 else d1=d1-10000 : rollback=0
	begd=(int(d1/10000)*10000)+100+val(date$('yy'))-rollback
	endd=begd+3000
	transstartdate=date('mm')*10000+100+date('yy') ! changed default to beginning of month as per billing's suggestion on 2/9/12    ! begd
	transenddate=date('mmddyy') ! val(date$(4:5))*10000+val(date$(7:8))*100+val(date$(1:2))
	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,i,r
	read #20,using 'Form Pos 152,N 2',rec=1,release: wbc : close #20:
	gosub OpenTransactionFiles
	open #h_tralloc=fnH: "Name=[Q]\CLmstr\TrAlloc.h[cno],KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,outIn,keyed
! /r
Screen1: ! r:
	! select limitations for the Menu1's record selection grid
	fnTos
	lc=0 : mylen=25 : mypos=mylen+2 : width=100
	fnLbl(lc+=1,1,'Transaction Grid Selection Criteria',width,center)
	lc+=1
	fnLbl(lc+=1,1,"Working Bank:",mylen,right)
	fncombof('BankAll',lc,mypos,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",add_all)
	if wbc=0 then resp$(1)='[All]' else resp$(1)=str$(wbc)
	fnLbl(lc+=1,1,"Working Transaction Type:",mylen,right)
	fncombof('TransactionTypeall',lc,mypos,0,"S:\Core\Data\TransactionType.dat",1,1,2,25,"S:\Core\Data\TransactionType.idx",add_all)
	if wtt=0 then resp$(2)='[All]' else resp$(2)=str$(wtt)
	fnLbl(lc+=1,1,"Payee:",mylen,right)
	fncombof('Payeeall',lc,mypos,0,"[Q]\CLmstr\PayMstr.h[cno]",1,8,9,30,"[Q]\CLmstr\PayIdx1.h[cno]",add_all)
	if wpayee$='' then resp$(3)='[All]' else resp$(3)=wpayee$
	lc+=1
	fnLbl(lc+=1,1,"Transaction Starting Date:",mylen,right)
	fnTxt(lc,mypos,8,0,left,ccyymmdd$,0,'Blank for All')
	resp$(4)=str$(transstartdate)
	fnLbl(lc+=1,1,"Transaction Ending Date:",mylen,right)
	fnTxt(lc,mypos,8,0,left,ccyymmdd$,0,'Blank for All')
	resp$(5)=str$(transenddate)
	fnLbl(lc+=1,1,"Statement Date Cleared:",mylen,right)
	fnTxt(lc,mypos,8,0,left,ccyymmdd$,0,'Blank for All')
	resp$(6)=''
	lc+=1
	fnLbl(lc+=1,1,"Posting Status:",mylen,right)
	fncombof('PostCodeall',lc,mypos,0,"S:\acsCL\PostingCode.dat",1,1,2,25,"S:\acsCL\PostingCode.idx",add_all)
	resp$(7)='[All]'
	fnLbl(lc+=1,1,"Source:",mylen,right)
	fncombof('SourceAll',lc,mypos,0,"S:\acsCL\SourceCode.dat",1,1,2,25,"S:\acsCL\SourceCode.idx",add_all)
	resp$(8)='[All]'
	fnLbl(lc+=1,1,"Check/Reference #:",mylen,right)
	fnTxt(lc,mypos,8,0,left,"",0,'Enter the check or reference # to access a specific transactin, else blank for all')
	resp$(9)=''
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=cancel then goto Xit
	if resp$(1)='[All]' then
		wbc=0 : bn$='[All]'
	else
		wbc=val(resp$(1)(1:2))
		bn$=resp$(1)(4:len(resp$(1)))
	end if
	if resp$(2)='[All]' then wtt=0 else wtt=tcdekey=val(resp$(2)(1:1))
	if wtt=0 then tcde$='[All]' else tcde$=resp$(2)(3:len(resp$(2)))
	if resp$(3)='[All]' then wpayee$=resp$(3) else wpayee$=resp$(3)(1:8)
	transstartdate=val(resp$(4))
	transenddate=val(resp$(5))
	statementdatecleared=val(resp$(6))
	postingcode$=resp$(7)
	sourcecode$=resp$(8)
	if trim$(resp$(9))<>"" then selectedck=1 else selectedck=0
	if trim$(resp$(9))="" then goto Menu1
	if wbc=0 then wbc=1
	if trim$(tcde$)="" or trim$(tcde$)="[All]" then tcde$="1" : tcdekey=1 ! try defaulting to check
	check_ref$=cnvrt$("pic(ZZ)",wbc)&str$(tcdekey)&lpad$(rtrm$(resp$(9)),8)
	read #h_trmstr(1),using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey TRY_RECEIPT
	editrec=rec(h_trmstr(1)): goto DoTransactionEdit
	TRY_RECEIPT: !
	tcde$="2" ! try as receipt
	check_ref$=cnvrt$("pic(ZZ)",wbc)&str$(tcdekey)&lpad$(rtrm$(resp$(9)),8)
	read #h_trmstr(1),using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1',key=check_ref$: newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd nokey Menu1
	editrec=rec(h_trmstr(1)): goto DoTransactionEdit
goto Menu1 ! /r
OpenTransactionFiles: ! r:
	open #h_trmstr(1)=fnH: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx1.h[cno],Shr",internal,outIn,keyed
	open #h_trmstr(2)=fnH: "Name=[Q]\CLmstr\TrMstr.h[cno],KFName=[Q]\CLmstr\TrIdx2.h[cno],Shr",internal,outIn,keyed
return  ! /r
Menu1: ! r:
	fnTos
	lc=0 : mylen=30 : mypos=mylen+2
	fc=0 ! frame count
	fnFra(1,1,10,100,'Transaction Grid Selection Criteria')
	frame=fc+=1
	lc=0
	fnLbl(lc+=1,1,'Bank:',mylen,right,0,frame)
	fnTxt(lc,mypos,3,0,center,'',disable,'',frame)
	resp$(1)=str$(wbc)
	fnTxt(lc,mypos+5,30,0,center,'',disable,'',frame)
	resp$(2)=bn$
	fnTxt(lc,mypos+38,15,0,right,pointtwo$,disable,'',frame)
	resp$(3)=str$(fnbankbal(wbc))
	fnLbl(lc+=1,1,'Transaction Type:',mylen,right,0,frame)
	fnTxt(lc,mypos,1,0,left,'',disable,'',frame)
	resp$(4)=str$(wtt)
	! fncombof('TransactionTypeall',lc,mypos,0,"S:\Core\Data\TransactionType.dat",1,1,2,25,"S:\Core\Data\TransactionType.idx",add_all)
	fnTxt(lc,mypos+4,25,0,left,'',disable,'',frame)
	resp$(5)=tcde$
	lc+=1
	fnLbl(lc+=1,1,'Transaction Starting Date:',mylen,right,0,frame)
	fnTxt(lc,mypos,0,0,left,ccyymmdd$,disable,'',frame)
	resp$(6)=str$(transstartdate)
 
	fnLbl(lc+=1,1,'Transaction Ending Date:',mylen,right,0,frame)
	fnTxt(lc,mypos,0,0,left,ccyymmdd$,disable,'',frame)
	resp$(7)=str$(transenddate)
 
	fnLbl(lc+=1,1,'Statement Cleared Date:',mylen,right,0,frame)
	fnTxt(lc,mypos,0,0,left,ccyymmdd$,disable,'',frame)
	resp$(8)=str$(statementcleareddate)
 
	lc+=1
 
	fnLbl(lc+=1,1,'Posting Status:',mylen,right,0,frame)
	fnTxt(lc,mypos,30,0,left,'',disable,'',frame)
	resp$(9)=postingcode$
 
	fnLbl(lc+=1,1,'Source:',mylen,right,0,frame)
	fnTxt(lc,mypos,30,0,left,'',disable,'',frame)
	resp$(10)=sourcecode$
	fnButton(1,90,'Change',1,'',1,10,frame)
	! r: Transaction Allocation Grid
	fnLbl(lc=13,1,'Transaction Grid',20)
	mat chdr$(11) : mat cmask$(11) : mat item$(11)
	chdr$(1)='Rec'
	chdr$(2)='ChkRf'
	chdr$(3)='Date'
	chdr$(4)='Amount'
	chdr$(5)='Payee'
	chdr$(6)='Name/Description'
	chdr$(7)='PC'
	chdr$(8)='Stmt Clr Date'
	chdr$(9)='SC'
	chdr$(10)='Bank'
	chdr$(11)='Type'
	cmask$(1)='20'
	cmask$(2)=''
	cmask$(3)='1'
	cmask$(4)='10'
	cmask$(8)='1'
 
	fnflexinit1('ApTrans',14,1,10,100,mat chdr$,mat cmask$,1,pas)
	! if pas=1 then goto 1140  ! for pas to work properly, totals need to be adjusted for adds,corrections, and deletions
	restore #h_trmstr(1):
	transactionsTotal=0
	READ_TRMSTR1_1: !
	read #h_trmstr(1),using 'Form Pos 1,C 3,C 8,G 6,PD 10.2,C 8,C 35,N 1,N 6,N 1': newkey$,tr$(1),tr$(2),tr3,tr$(4),tr$(5),posting_code,clr,scd eof EO_FLEX1
	if wbc<>0 and val(newkey$(1:2))<>wbc then goto READ_TRMSTR1_1
	if wtt<>0 and val(newkey$(3:3))<>wtt then goto READ_TRMSTR1_1
	if wpayee$<>'[All]' and trim$(wpayee$)<>trim$(tr$(4)) then goto READ_TRMSTR1_1
	if transstartdate<>0 and transstartdate>fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR1_1
	if transenddate<>0 and transenddate<fndate_mmddyy_to_ccyymmdd(val(tr$(2))) then goto READ_TRMSTR1_1
	if statementdatecleared<>0 and statementdatecleared<>fndate_mmddyy_to_ccyymmdd(clr) then goto READ_TRMSTR1_1
	if postingcode$<>'[All]' and str$(posting_code)<>postingcode$(1:1) then goto READ_TRMSTR1_1
	if sourcecode$<>'[All]' and str$(scd)<>sourcecode$(1:1) then goto READ_TRMSTR1_1
	item$(1)=str$(rec(h_trmstr(1)))
	item$(2)=tr$(1)
	item$(3)=tr$(2)
	item$(4)=str$(tr3)
	item$(5)=tr$(4)
	item$(6)=tr$(5)
	item$(7)=str$(posting_code)
	item$(8)=str$(clr)
	item$(9)=str$(scd)
	item$(10)=newkey$(1:2)
	item$(11)=newkey$(3:3)
	fnflexadd1(mat item$)
	transactionsTotal+=tr3
	goto READ_TRMSTR1_1
	EO_FLEX1: ! /r
	resp$(11)=''
	!  r:
	!  this uses the transactionsTotal which is calculated when the flex grid is made
	fnLbl(13,31,'Transactions Total:',mylen,right)
	fnTxt(13,mypos+30,15,0,right,pointtwo$,disable,'This is the total of only the transactions shown in the Transaction Grid above.  To update this total click the change button at the top and reselect your Transaction Grid Selection Criteria')
	resp$(12)=str$(transactionsTotal)
	! /r
	fnCmdKey('E&dit',3,1,0,"Highlight any entry and click edit to change or review the complete entry.")
	fnCmdKey('&Add Deposit (Receipt)',2,0,0,"Allows you to enter deposits into the files.")
	fnCmdKey('Add &Check (Disbursment)',8,0,0,"Allows you to add hand written checks to the checkbook files.")
	fnCmdKey('&ReIndex',7,0,0,"Allows you to ReIndex the check history files. Should only be necessary if power failures have corrupted the files.")
	fnCmdKey('&Change Selection Criteria',6,0,0,"Allows you to return to first screen and change date ranges, etc.")
	fnCmdKey('E&xit',5,0,1,"Exits the checkbook system.")
	ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=cancel then goto Xit
	if ckey=2 or ckey=8 then addloopcode=1 else addloopcode=0
	! pas=1
	if ckey=6 then pas=0: goto Screen1
	wbc=val(resp$(1)(1:2)) ! working bank(s) code
	bn$=resp$(2) ! bank name
	bankbalance=val(resp$(3)) ! bank balance
	tcde =val(resp$(4)) ! transaction type
	tcde$=resp$(5) ! transaction description
	transstartdate=val(resp$(6))
	transenddate=val(resp$(7))
	statementcleareddate =val(resp$(8)) ! cleared date
	postingcode$=resp$(9) ! posting code
	sourcecode$=resp$(10) ! source code code
	transactionsTotal=val(resp$(12)) conv TEST_CKEY ! transaction total
	TEST_CKEY: !
	! if ckey=3 then typeofentry=tcde
	if ckey=2 then
		typeofentry=2
		editrec=0
		goto TransactionAdd
	else if ckey=8 then
		typeofentry=1
		editrec=0
		goto TransactionAdd
	else if ckey=3 then
		typeofentry=tcde
		! typeofentry=2
		allocations_messed_with=false
		editrec=val(resp$(11))
		goto DoTransactionEdit
	else if ckey=1 then
		goto Screen1
	else if ckey=6 then
		gosub ReIndex
		goto Menu1
	end if
goto Menu1 ! /r only if somehow got through without a valid ckey
TransactionAdd: ! r:
	editrec=hamt=tr3=posting_code=clr=0
	scd=8 : tcde=typeofentry
	bank_code=wbc
	! tcde set by add button
	tr$(1)=tr$(3)=tr$(4)=tr$(5)='' : tr$(2)=date$("mmddyy")
goto TransactionFm ! /r
StandardBreakdown: ! r:
! pull standard gl breakdowns from payee file
! uses - tr$(4), tralloc (file), bank_code, tcde, tr$(1)
! returns - nothing - it's all in the TrAlloc file additions and removal
! what if allocate 2nd time
! ** first remove all old allocations for this check
	key$=lpad$(str$(bank_code),2)&str$(tcde)&rpad$(tr$(1),8)
	restore #h_tralloc,key>=key$: nokey RemoveTrAllocForKeyEoF
RemoveTrAllocForKeyRead: !
	read #h_tralloc,using 'Form Pos 1,C 11': newkey$ eof RemoveTrAllocForKeyEoF
	if newkey$=key$ then
		delete #h_tralloc:
		goto RemoveTrAllocForKeyRead
	else
		goto RemoveTrAllocForKeyEoF
	end if
	goto RemoveTrAllocForKeyRead
RemoveTrAllocForKeyEoF: ! eo first...


! ** next add new allocations that match what they have in their payee file or receipt file (typeofentry=2=reading from receipt file
	if typeofentry=2 then
		open #payee=fnH: "Name=[Q]\CLmstr\RecMstr.h[cno],KFName=[Q]\CLmstr\recIdx1.h[cno],Shr",i,i,k
		open #payeegl=fnH: "Name=[Q]\CLmstr\ReceiptGLBreakdown.h[cno],KFName=[Q]\CLmstr\ReceiptGLBkdIdx.h[cno],Shr",internal,outIn,keyed
	else
		open #payee=fnH: "Name=[Q]\CLmstr\PayMstr.h[cno],KFName=[Q]\CLmstr\PayIdx1.h[cno],Shr",i,i,k
		open #payeegl=fnH: "Name=[Q]\CLmstr\PayeeGLBreakdown.h[cno],KFName=[Q]\CLmstr\PayeeGLBkdIdx.h[cno],Shr",internal,outIn,keyed
	end if

	read #payee,using "form pos 1,c 8",key=lpad$(rtrm$(tr$(4)),8): vn$ nokey XIT_READSTGL
	restore #payeegl,key>=vn$: nokey EO_READSTGL
	totalalloc=0 : totalamt=0
READ_PAYEEGL: !
	if val(tr$(3))<>0 then goto GET_TOTAL
	mat ml$(3)
	ml$(1)='You must enter the transaction amount before'
	ml$(2)="you can pull the standard general ledger breakdowns."
	fnmsgbox(mat ml$,ok$,'',48)
	goto EO_READSTGL
GET_TOTAL: !
	do until totalamt>=val(tr$(3))
		read #payeegl,using 'Form Pos 1,C 8,c 12,n 6.2,c 30': payeekey$,payeegl$,percent,gldesc$ eof EO_READSTGL
		if vn$<>payeekey$ then goto EO_READSTGL
		allocamt=round(val(tr$(3))*percent*.01,2)
		totalalloc+=percent
		totalamt+=allocamt

		write #h_tralloc,using 'form pos 1,C 11,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': key$,payeegl$,allocamt,gldesc$,0,"",0
		lastrec=rec(h_tralloc)
	loop

EO_READSTGL: !
	if totalamt<>val(tr$(3)) then
		allocamt-=totalamt-val(tr$(3))
		rewrite #h_tralloc,using 'Form Pos 24,Pd 5.2',rec=lastrec: allocamt noRec ASSIGN_IF_EMPTY
! plug any rounding differences into last allocation
	end if
ASSIGN_IF_EMPTY: if trim$(tr$(5))="" then tr$(5)=resp$(6)(9:30) ! assign a name if none entered
XIT_READSTGL: !
	close #payee:
	close #payeegl:
	XIT_STGL: !
return  ! /r
DoTransactionEdit: ! r:
	if editrec=0 then goto Menu1
	read #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',rec=editrec,reserve: bank_code,tcde,tr$(1),tr$(2),tx3,tr$(4),tr$(5),posting_code,clr,scd ! noRec Menu1
	tr$(3)=str$(tx3)
	ad1=0
	! if posting_code>0 then gosub crgl1
	hamt=val(tr$(3)) : hkey$=key$ : tr3=val(tr$(3))
goto TransactionFm ! /r
Save: ! r:
	save_good=false
	if editrec>0 then goto EMPTY_BANK_MSG
	check_key$=cnvrt$("pic(ZZ)",wbc)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
	read #h_trmstr(1),using 'Form Pos 1,C 11',key=check_key$: newkey$ nokey EMPTY_BANK_MSG
	mat ml$(1)
	ml$(1)="You already have a transaction with reference # "&trim$(tr$(1))&"."
	fnmsgbox(mat ml$,resp$,'',0)
	tr$(1)=""
	goto TransactionFm
	EMPTY_BANK_MSG: !
	if bank_code=0 then
		mat ml$(1)
		ml$(1)="You must first select a Bank."
		fnmsgbox(mat ml$,resp$,'',0)
		goto EoSave
	end if
	! if trim$(tr$(4))='' and tcde=1 and trim$(uprc$(tr$(5)))<>"VOID" then mat ml$(1)
	! ml$(1)="You must first select a Payee."
	! fnmsgbox(mat ml$,resp$,'',0)
	! goto EoSave
	! end if
	if allocationstotal=val(tr$(3)) then goto RELEASE_TRMSTR1 ! allow zero checks to go thru as long as the allocations = 0 also
	if trim$(tr$(3))='0.00' and trim$(uprc$(tr$(5)))<>"VOID" then
		mat ml$(1)
		ml$(1)="You must first enter an amount."
		fnmsgbox(mat ml$,resp$,'',0)
		goto EoSave
	end if
RELEASE_TRMSTR1: release #h_trmstr(1):
	if editrec<>0 then
		read #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,Pos 18,PD 10.2,pos 71,n 1',rec=editrec,reserve: oldbank_code,oldtcde,oldtr1$,oldtr3,oldposting_code
	end if
 
	! if allocations changed on a posted transaction tell them to update their general ledger
	if allocations_messed_with=1 and oldposting_code>0 then
		mat ml$(2)
		ml$(1)='Your allocations have changed on a posted transaction.'
		ml$(2)='You will need to update your General Ledger!'
		fnmsgbox(mat ml$,resp$,'',0)
		allocations_messed_with=false
	end if

! save - update bank balance for changed amounts
	if editrec=0 then goto L2040 ! adding new record
	if oldtr3<>val(tr$(3)) or oldbank_code<>bank_code or tcde<>oldtcde then
		if oldtcde=1 or oldtcde=4 then
			fnupdatebankbal(oldbank_code,+oldtr3)
		else
			fnupdatebankbal(oldbank_code,-oldtr3) ! take out the old
		end if
		L2040: !
		if tcde=1 or tcde=4 then
			fnupdatebankbal(bank_code,-val(tr$(3)))
		else
			fnupdatebankbal(bank_code,+val(tr$(3))) ! put in the new
		end if
	end if
! save - update key fields in tralloc
	if (oldbank_code<>bank_code or oldtcde<>tcde or oldtr1$<>tr$(1)) and editrec<>0 then
		newkey$=cnvrt$('pic(zz)',bank_code)&str$(tcde)&tr$(1)
		oldkey$=cnvrt$('pic(zz)',oldbank_code)&str$(oldtcde)&oldtr1$
		restore #h_tralloc,key=oldkey$: nokey EO_UPDATE_TRALLOC_KEYS
		READ_TRALLOC_UPDATE: !
		do
			read #h_tralloc,using 'Form Pos 1,C 11',reserve: readkey$ eof L2130
			if oldkey$=readkey$ then
				rewrite #h_tralloc,using 'Form Pos 1,C 11',release: newkey$
			end if
		loop while oldkey$=readkey$
		release #h_tralloc:
		L2130: !
	end if
	EO_UPDATE_TRALLOC_KEYS: !





! and finaly, actually save the transaction
	tr$(1)=lpad$(trim$(tr$(1)),8)
	tx3=val(tr$(3))
	if editrec<>0 then
		tr2=val(tr$(2))
		rewrite #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',same,reserve: bank_code,tcde,tr$(1),tr2,tx3,tr$(4),tr$(5),posting_code,clr,scd
		goto L2260
	end if
	check_key$=cnvrt$("pic(ZZ)",wbc)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
	read #h_trmstr(1),using 'Form Pos 1,C 11',key=check_key$: newkey$ nokey L2250
	mat ml$(1)
	ml$(1)="You already have a transaction with reference # "&trim$(tr$(1))&"."
	fnmsgbox(mat ml$,resp$,'',0)
	tr$(1)=""
	goto TransactionFm
	L2250: !
	tr2=val(tr$(2)): write #h_trmstr(1),using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1',reserve: bank_code,tcde,tr$(1),tr2,tx3,tr$(4),tr$(5),posting_code,clr,scd
	editrec=rec(h_trmstr(1))
	L2260: !
	if ckey=1 and allocationstotal<>val(tr$(3)) then
		mat ml$(4)
		ml$(1)='Allocations do not equal the Check Amount'
		ml$(2)='Please correct the Check Amount or '
		ml$(3)='the Allocations'
		ml$(4)='You are off by '&str$(val(tr$(3))-allocationstotal)
		fnmsgbox(mat ml$,yn$,'',48)
		goto EoSave
	end if
	save_good=1
	EoSave: !
return  ! /r
TransactionVoid: ! r:
	! uses:    bank_code, tcde, tr$(1), h_tralloc
	! returns: tr$(3) and tr$(5)
	tr$(3)='0'
	tr$(5)='Void'
	key$=cnvrt$('pic(ZZ)',bank_code)&str$(tcde)&lpad$(trim$(tr$(1)),8)
	restore #h_tralloc,key>=key$: nokey VOID_EO_TRALLOC
	VOID_READ_TRALLOC: !
	read #h_tralloc,using 'Form Pos 1,C 11,C 12,pd 5.2,C 30,G 6,X 3,C 12,G 1': newkey$,item$(1),tmp,item$(3),item$(4),item$(5),item$(6) eof VOID_EO_TRALLOC
	if key$=newkey$ then
		rewrite #h_tralloc,using 'Form Pos 24,PD 5.2,C 30',release: 0,'Void'
		goto VOID_READ_TRALLOC
	else
		release #h_tralloc:
		goto VOID_EO_TRALLOC
	end if
	VOID_EO_TRALLOC: !
return  ! /r
TransactionDelete: ! r:
	fnTos
	lc=0 : width=50
	fnLbl(lc+=1,1,'Delete Transaction Options',width,center)
	ln+=1
	fnChk(lc+=1,1,'Update Bank Balance')
	resp$(1)='True'
	fnChk(lc+=1,1,'Delete Transaction Allocations too')
	resp$(2)='True'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 or ckey=cancel then goto TransactionDelete_DONE
	updatebankbalance$=resp$(1)
	deletetransactionallocation$=resp$(2)
	if updatebankbalance$='True' then
		if tcde=1 or tcde=4 then
			fnupdatebankbal(bank_code,+val(tr$(3)))
		else
			fnupdatebankbal(bank_code,-val(tr$(3)))
			! take out the old
		end if
	end if
	if deletetransactionallocation$='True' then
		key$=cnvrt$('pic(ZZ)',bank_code)&str$(tcde)&lpad$(trim$(tr$(1)),8)
		restore #h_tralloc,key>=key$: nokey DONE_DELETETRALLOC
		do
			read #h_tralloc,using 'Form Pos 1,C 11',reserve: oldkey$ eof DONE_DELETETRALLOC
			if oldkey$=key$ then
				delete #h_tralloc,release:
			end if
		loop while oldkey$=key$
		DONE_DELETETRALLOC: !
		release #h_tralloc:
	end if
	delete #h_trmstr(1),release:
	TransactionDelete_DONE: !
return  ! /r
AllocationAdd: ! r:
	adding_allocation=1
	trabank_code=bank_code : tratcde=tcde
	track$=lpad$(trim$(tr$(1)),8) : tradesc$=tr$(5)(1:12)
	tragl$=traivd$=trapo$='' : traamt=0 : tragde=posting_code
	! write #h_tralloc,using 'Form Pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1',reserve: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
	! read #h_tralloc,using 'Form Pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1',same,reserve: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
return  ! /r
AllocationRead: ! r: uses allocrec and returns ???
	if allocrec=0 then
		track$=lpad$(trim$(tr$(1)),8)
	else
		read #h_tralloc,using 'Form Pos 1,N 2,N 1,C 8,C 12,pd 5.2,C 30,G 6,X 3,C 12,N 1',rec=allocrec,reserve: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
	end if
return  ! /r
AllocationFm: ! r:
	allocations_messed_with=1
	fnTos
	lc=0 : mylen=22 : mypos=mylen+2
	fnLbl(lc+=1,1,'Bank:',mylen,right)
	fnTxt(lc,mypos,2,0,left,number$,disable)
	resp$(1)=str$(trabank_code)
	fnLbl(lc+=1,1,'Transaction Type:',mylen,right)
	fnTxt(lc,mypos,1,0,left,number$,disable)
	resp$(2)=str$(tratcde)
	fnLbl(lc+=1,1,'Check/Reference:',mylen,right)
	fnTxt(lc,mypos,8,0,right,'',disable)
	resp$(3)=track$
	fnLbl(lc+=1,1,'General Ledger Number:',mylen,right)
	fnqgl(lc,mypos)
	resp$(4)=fnrgl$(tragl$)
	fnLbl(lc+=1,1,'Amount:',mylen,right)
	fnTxt(lc,mypos,9,0,right,pointtwo$)
	resp$(5)=str$(traamt)
	fnLbl(lc+=1,1,'Description:',mylen,right)
	fnTxt(lc,mypos,30,0,left)
	resp$(6)=tradesc$
	fnLbl(lc+=1,1,'Reference:',mylen,right)
	fnTxt(lc,mypos,6,0,left,"",0)
	resp$(7)=traivd$ ! the last zero above was disabled, why kj
	fnLbl(lc+=1,1,'Purchase Order:',mylen,right)
	fnTxt(lc,mypos,12,0,left)
	resp$(8)=trapo$
	fnLbl(lc+=1,1,'Posting Status:',mylen,right)
	fnTxt(lc,mypos,1,0,left,number$,disable)
	resp$(9)=str$(tragde)
	fnCmdSet(4)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto CANCEL_ALLOC
	trabank_code=val(resp$(1))
	tratcde=val(resp$(2))
	track$=resp$(3)
	tragl$=fnagl$(resp$(4))
	traamt=val(resp$(5))
	tradesc$=resp$(6)
	traivd$=resp$(7)
	trapo$=resp$(8)
	tragde=val(resp$(9))
	if ckey=1 then gosub AllocationSave
	if ckey=1 and adding_allocation=1 then
		tragl$=""
		traamt=0
		goto AllocationFm
! add loop
	end if
	CANCEL_ALLOC: ! r:
		if adding_allocation=1 then
		! delete #h_tralloc,same:
		else
			release #h_tralloc:
		end if
	goto EO_ALLOC ! /r
	EO_ALLOC: !
	adding_allocation=0
return  ! /r
AllocationSave: ! r:
	track$=lpad$(trim$(track$),8)
	if adding_allocation=0 then
		rewrite #h_tralloc,using 'form pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1',release: trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
	else
		write #h_tralloc,using 'form pos 1,N 2,N 1,C 8,C 12,PD 5.2,C 30,G 6,X 3,C 12,N 1': trabank_code,tratcde,track$,tragl$,traamt,tradesc$,traivd$,trapo$,tragde
	end if
return  ! /r
AllocationDelete: ! r: uses allocrec
	allocations_messed_with=1
	delete #h_tralloc,rec=allocrec: noRec AllocationDeleteNoRec
return  ! /r
	AllocationDeleteNoRec: ! r:
		mat ml$(3)
		ml$(1)='Delete Allocation Error'
		ml$(2)="You must select an Allocation Record to delete"
		ml$(3)="before clicking the Delete Allocation button."
		fnmsgbox(mat ml$,ok$,'',48)
	continue  ! /r
TransactionFm: ! r: requires typeofentry, scd, and many more
	fnTos
	lc=0 ! line count
	fc=0 ! frame count
	width=120 ! screen width
	fnFra(1,1,10,width,'Transaction Data')
	frame=fc+=1
	lc=0 : mylen=23 : mypos=mylen+2
	fnLbl(lc+=1,1,'Bank:',mylen,right,0,frame)
	fncombof('Bank',lc,mypos,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list,0,'',frame)
	resp$(1)=str$(bank_code)
	fnLbl(lc+=1,1,'Transaction Type:',mylen,right,0,frame)
! fncombof('TransactionType',lc,mypos,0,'S:\Core\Data\TransactionType.dat',1,1,2,25,'S:\Core\Data\TransactionType.idx',limit_to_list,0,'',frame)
! resp$(2)=str$(tcde)
	fnTxt(lc,mypos,28,0,left,'',disable,'',frame)
	resp$(2)=str$(tcde)
	if wit=1 then
		fnLbl(lc+=1,1,'Check Number:',mylen,right,0,frame)
	else
		fnLbl(lc+=1,1,'Reference Number:',mylen,right,0,frame)
	end if
	fnTxt(lc,mypos,8,0,right,'',0,'',frame)
	resp$(3)=tr$(1)
	fnLbl(lc+=1,1,'Transaction Date:',mylen,right,0,frame)
	fnTxt(lc,mypos,8,0,left,mmddyy$,0,'',frame)
	resp$(4)=tr$(2)
	fnLbl(lc+=1,1,'Transaction Amount:',mylen,right,0,frame)
	fnTxt(lc,mypos,12,0,right,pointtwo$,0,'',frame)
	resp$(5)=cnvrt$("N 10.2",val(tr$(3)))
	if tcde=2 then ! typeofentry=2 then
		fnLbl(lc+=1,1,'Receipt Type:',mylen,right,0,frame)
		fncombof('ReceiptType',lc,mypos,0,"[Q]\CLmstr\RecMstr.h[cno]",1,8,9,30,"[Q]\CLmstr\RecIdx1.h[cno]",limit_to_list,0,'',frame)
		resp$(6)=tr$(4)
		fnLbl(lc+=1,1,'Name/Description:',mylen,right,0,frame)
	else if scd=4 then
		fnLbl(lc+=1,1,'Payroll Employee Number:',mylen,right,0,frame)
		fnTxt(lc,mypos,8,0,left,"",0,'Employee # for payroll checksl',frame)
		resp$(6)=tr$(4)
		fnLbl(lc+=1,1,'Payroll Employee Name:',mylen,right,0,frame)
	else
		fnLbl(lc+=1,1,'Payee:',mylen,right,0,frame)
		fncombof('Payee',lc,mypos,0,"[Q]\CLmstr\PayMstr.h[cno]",1,8,9,30,"[Q]\CLmstr\PayIdx1.h[cno]",limit_to_list,0,'',frame)
		resp$(6)=tr$(4)
		fnLbl(lc+=1,1,'Name/Description:',mylen,right,0,frame)
	end if
	fnTxt(lc,mypos,35,0,left,'',0,'',frame)
	resp$(7)=tr$(5)
	fnLbl(lc+=1,1,'Posting Status:',mylen,right,0,frame)
	fncombof('PostCode',lc,mypos,0,"S:\acsCL\PostingCode.dat",1,1,2,25,"S:\acsCL\PostingCode.idx",limit_to_list,0,'',frame)
	resp$(8)=str$(posting_code)
	fnLbl(lc+=1,1,'Statement Date Cleared:',mylen,right,0,frame)
	fnTxt(lc,mypos,8,0,left,mmddyy$,0,'',frame)
	resp$(9)=str$(clr)
!  r: the transaction allocation grid
	mat chdr$(7) : mat cmask$(7) : mat item$(7)
	chdr$(1)='Rec'
	chdr$(2)='GL Account'
	chdr$(3)='Amount'
	chdr$(4)='Description'
	chdr$(5)='Invoice'
	chdr$(6)='PO Number'
	chdr$(7)='PC'
	mat cmask$=("")
	cmask$(1)='30'
	cmask$(2)=''
	cmask$(3)='10'
	cmask$(5)=''
	fnflexinit1('TrAlloc-tran2',16,1,4,90,mat chdr$,mat cmask$,1)
	allocationstotal=0
! fnflexinit1('TrAlloc-'&str$(bank_code)&'-'&str$(tcde)&'-'&trim$(tr$(1)),13,1,7,80,mat chdr$,mat cmask$,1)
! allocationstotal=0
	key$=cnvrt$('pic(ZZ)',bank_code)&str$(tcde)&lpad$(trim$(tr$(1)),8)
	restore #h_tralloc,key>=key$: nokey EO_FLEX2
READ_TRALLOC_1: !
	read #h_tralloc,using 'Form Pos 1,C 11,C 12,pd 5.2,C 30,G 6,X 3,C 12,G 1': newkey$,item$(2),tmp,item$(4),item$(5),item$(6),item$(7) eof EO_FLEX2
	if key$<>newkey$ then goto EO_FLEX2
	allocationstotal+=tmp
	item$(1)=str$(rec(h_tralloc))
	item$(3)=str$(tmp)
	fnflexadd1(mat item$)
	goto READ_TRALLOC_1
EO_FLEX2: ! /r
	fnLbl(lc=15,1,'Allocation Total: $'&trim$(cnvrt$("N 15.2",allocationstotal)),40,right)
	fnButton(lc=15,(61),'&Add',8,'Add Allocation')
	fnButton(lc,(61+4+2),'&Edit',7,'Edit Allocation')
	fnButton(lc,(61+4+2+5+2),'&Delete',6,'Delete Allocation')
	fnButton(lc,(61+4+2+5+2+7+2),'&Get Standard G/L Breakdowns',9,'Reset Allocations to those associated with the Payee.')
	if typeofentry=2 then
		fnButton(6,72,'&Receipt Type File',11,'Add or Edit different types or classifications of receipts ',0,0,1)
	else if scd<>4 then
		fnButton(6,72,'&Payee File',10,'Add or Edit Payees',0,0,1)
	end if
	lc+=1
	fnCmdKey('&Save',1,1,0,"Saves this record and any changes back to the files.")
	fnCmdKey('&Delete',3,0,0,"Will delete this entry from your files.  You will have an option as to how to effect the bank balance.")
	fnCmdKey('&Void',4,0,0,"Voids the transaction that is on the screen. It will adjust the bank balance. It leaves a voided transaction on file.")
	fnCmdKey('&Cancel',5,0,1,"Returns to previous screen without saving any changes.")
	ckey=fnAcs(mat resp$)
	holdtr1$=tr$(1)
	if ckey=3 then
		if fnConfirmDeleteHard('transaction','Refercence Number '&trim$(tr$(1))) then
			gosub TransactionDelete
		end if
		goto Menu1
	else if ckey=5 or ckey=cancel then
		goto Menu1
	else if ckey=1 or ckey=4 or ckey=6 or ckey=7 or ckey=8 or ckey=9 then
		bank_code=val(resp$(1)(1:2))
		tcde=val(resp$(2)(1:1))
		tr$(1)=lpad$(resp$(3),8)
		tr$(2)=resp$(4)
		tr$(3)=resp$(5)
		tr$(4)=lpad$(trim$(resp$(6)(1:8)),8)
		tr$(5)=resp$(7)
		posting_code=val(resp$(8)(1:1))
		clr=val(resp$(9))
	end if
	if ckey=1 and allocationstotal<>val(resp$(5)) then
		mat ml$(3)
		ml$(1)='Allocations ('&cnvrt$('pic(---,---,--#.##)',allocationstotal)&') do not equal the Check Amount ('&cnvrt$('pic(---,---,--#.##)',val(tr$(3)))&')'
		ml$(2)='Please correct the Check Amount or the Allocations'
		ml$(3)='You are off by '&str$(val(tr$(3))-allocationstotal)
		fnmsgbox(mat ml$,yn$,'',48)
		goto TransactionFm
	end if
	if (ckey=1 or ckey=8) and trim$(tr$(1))="" then
		mat ml$(1)
		ml$(1)="You must first enter a Reference Number."
		fnmsgbox(mat ml$,resp$,'',0)
		goto TransactionFm
	end if
	if ckey=6 then
		allocrec=val(resp$(10))
		gosub AllocationDelete
		goto TransactionFm
	end if
	if holdtr1$=tr$(1) and editrec>0 then goto L3780 ! r: duplicate transaction?
	check_key$=cnvrt$("pic(ZZ)",wbc)&str$(tcde)&lpad$(rtrm$(tr$(1)),8)
	read #h_trmstr(1),using 'Form Pos 1,C 11',key=check_key$: newkey$ nokey L3780
	mat ml$(1)
	ml$(1)="You already have a transaction with reference # "&trim$(tr$(1))&"."
	fnmsgbox(mat ml$,resp$,'',0)
	tr$(1)=""
	goto TransactionFm
L3780: ! /r
	if ckey=8 then
		gosub AllocationAdd
		gosub AllocationFm
		goto TransactionFm
	else if ckey=7 then
		allocrec=val(resp$(10))
		gosub AllocationRead
		gosub AllocationFm
		goto TransactionFm
	else if ckey=4 then
		if fnConfirmHard('void','transaction','Refercence Number '&trim$(tr$(1))) then
			gosub TransactionVoid
			gosub Save
		end if
		goto Menu1
	else if ckey=10 then
		fnaddpayee
		goto TransactionFm
	else if ckey=11 then
		fnaddreceipt
		goto TransactionFm
	end if
	gosub Save
	if save_good=false then goto TransactionFm
	if ckey=9 then gosub StandardBreakdown : goto TransactionFm ! the record must be good save first!!!
	if trim$(check_ref$)<>"" then
		check_ref$=""
		goto Screen1
	else if editrec<>0 then ! return to main screen after edit
		goto Menu1
	end if
goto TransactionAdd ! if resp$(12)='True' then goto TransactionAdd
goto Menu1 ! /r
ReIndex: ! r: drops deleted records and reindexes trmstr
	close #h_trmstr(1):
	close #h_trmstr(2):
	close #h_tralloc:
	fnRemoveDeletedRecords("[Q]\CLmstr\TrMstr.h[cno]")
	fnIndex("[Q]\CLmstr\TrMstr.h[cno]","[Q]\CLmstr\TrIdx1.h[cno]","1 11")
	fnIndex("[Q]\CLmstr\TrMstr.h[cno]","[Q]\CLmstr\TrIdx2.h[cno]","28/1 8/11")
	fnIndex("[Q]\CLmstr\Tralloc.h[cno]","[Q]\CLmstr\Tralloc-idx.h[cno]","1 11")
	gosub OpenTransactionFiles
return  ! /r
Xit: fnXit
include: ertn
