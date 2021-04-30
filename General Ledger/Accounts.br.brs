! Formerly S:\acsGL\glMaint
! General Ledger Master File editor

autoLibrary
! fnrglbig$ and fnqglbig  were added so all of the description could easily be seen in the Main gl screen
on error goto Ertn

dim tr(7),tr$*12,td$*30
dim d$*50,bc(13),bp(13)
dim bm(13)
dim rf(6)
dim key$*12
dim ta(2)
dim revb(13)
dim ack$*20
dim resp$(100)*60
dim ml$(3)*128
dim item$(9)*30

fnTop(program$)
fixgrid=99
open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
close #company:
open #8: "Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed ioerr L310
cl1=1
L310: !
open #hAccount=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
open #hAccountUnused=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.h[cno],Shr",internal,outIn,keyed
Faccount: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
open #hTransCurrent=fnH: "Name=[Q]\GLmstr\GLTRANS.h[cno],Shr",internal,outIn,relative
FtransCurrent: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
open #hTransHistory=fnH: "Name=[Q]\GLmstr\ACTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Version=0,Use,RecL=72,KPs=1/71/17/13,KLn=12/2/2/4,Shr",internal,outIn,keyed
FtransHistory: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
Main: !
	fnTos
	mylen=23 : mypos=mylen+3
	fnLbl(1,1,"Account:",mylen,1)
	if edit_mode=1 then ! attempt to put disabled text box for acct #
		fnTxt(1,mypos,60,0,0,"",1,"",0)
		resp$(1)=fnrglbig$(gl$)
	else
		fnqglbig(1,mypos,0,2)
		resp$(1)=fnrglbig$(gl$)
	end if
	holdgl$=gl$
	if edit_mode=1 then
		fnLbl(2,1,"Beginning Balance:",mylen,1)
		fnTxt(2,mypos,14,0,1,"10",0,"The beginning balance will always be the account balance at the beginning of the period. It gets updated when you close the month.",0 )
		resp$(2)=str$(bb)
		fnLbl(2,40,"Current Balance:",mylen,1)
		fnTxt(2,66,14,0,1,"10",0,"The current balance will be updated any time a transaction is posted. The beginning and current balances should both be the same when you begin.",0 )
		resp$(3)=str$(cb)
		f1Col1Len=21
		f1Col2=1+f1Col1Len+2 : f1Col2Len=36
		f1Col3=f1Col2+f1Col2Len+2 : f1Col3len=21
		f1Col4=f1Col3+f1Col3len+2 : f1Col4Len=36
		fnFra(4,1,4,f1Col4+f1Col4Len+2,"Financial Statement Information"," ",0)
		fnLbl(1,1,"Balance Sheet Ref:",f1Col1Len,1,0,1)
		fncombof("fs-bal",1,f1Col2,f1Col2Len,"[Q]\GLmstr\acglfnsb.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx4.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the balance sheet.",1)
		resp$(4)=str$(rf(1)) ! balance sheet ref #
		fnLbl(1,f1Col3,"2nd Balance Sheet:",f1Col3len,1,0,1)
		fncombof("fs-bal2",1,f1Col4,f1Col4Len,"[Q]\GLmstr\acglfnsc.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx1.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",1)
		resp$(5)=str$(rf(2)) ! balance sheet ref #
		fnLbl(2,1,"Income Statement Ref:",f1Col1len,1,0,1)
		fncombof("fs-inc",2,f1Col2,f1Col2Len,"[Q]\GLmstr\acglfnsi.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx3.h[cno]",0,0, "Select the income statement reference number where this account should appear on the income statement.",1)
		resp$(6)=str$(rf(3)) ! income statement ref #
		fnLbl(2,f1Col3,"2nd Income Statement:",f1Col3len,1,0,1)
		fncombof("fs-inc2",2,f1Col4,f1Col4Len,"[Q]\GLmstr\acglfnsj.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx2.h[cno]",0,0, "Select the income statement reference number where this account should appear on the secondary income statement.",1)
		resp$(7)=str$(rf(4)) ! 2nd income statement
		fnLbl(3,1,"Cash Flow/Fund Ref:",f1Col1len,1,0,1)
		fncombof("fs-cash",3,f1Col2,f1Col2Len,"[Q]\GLmstr\acglfnsf.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx5.h[cno]",0,0, "Select the cash flow reference number where this account should appear on the cash flow statement.",1)
		resp$(8)=str$(rf(5)) ! income statement ref #
		fnLbl(3,f1Col3,"2nd Cash Flow/Fund:",f1Col3len,1,0,1)
		fncombof("fs-cash2",3,f1Col4,f1Col4Len,"[Q]\GLmstr\acglfnsg.h[cno]",1,5,6,30,"[Q]\GLmstr\agfsidx6.h[cno]",0,0, "Select the cash flow reference number where this account should appear on the cash flow statement.",1)
		resp$(9)=str$(rf(6)) ! 2nd cash flow

		fnLbl(10,1,"EOY Balance - 2 Years Ago:",30,1,0,0)
		fnTxt(10,33,12,0,1,"10",0,"In order to pr prior year's cash flow and fund statements, the final balance from two years ago must be retained.",0)
		resp$(10)=str$(pbp)
		fnFra(12,1,14,105,"History and Budget Information"," ",0)
		x=11
		fnLbl(1,14,"Balance This Yr",15,0,0,2)
		fnLbl(1,34,"Balance Last Yr",15,0,0,2)
		fnLbl(1,54,"Original Budget",15,0,0,2)
		fnLbl(1,74,"Revised Budget ",15,0,0,2)
		for j=1 to 13
			fnLbl(j+1,1,"Period "&str$(j),90,0,0,2)
			fnTxt(j+1,14,14,0,1,"10",0,"",2 )
			resp$(x)=str$(bc(j))
			x=x+1
			fnTxt(j+1,34,14,0,1,"10",0,"",2 )
			resp$(x)=str$(bp(j))
			x=x+1
			fnTxt(j+1,54,14,0,1,"10",0,"",2 )
			resp$(x)=str$(bm(j))
			x=x+1
			fnTxt(j+1,74,14,0,1,"10",0,"",2 )
			resp$(x)=str$(revb(j))
			x=x+1
		next j
	end if
	if edit_mode=1 then
		fnCmdKey("&Save",6,1,0,"")
		! fnCmdKey("Review &Transactions",3,0,0,"")
		fnCmdKey("Current Transactions",31,0,0,"")
		fnCmdKey("History Transactions",32,0,0,"")
		fnCmdKey("&Delete",7,0,0,"")
		fnButton(1,mypos+60+2,"C&hange",9,"Change the Account and/or the Description of this General Ledger Account") ! fnCmdKey("C&hange Acct-Desc",9,0,0,"")
	else
		fnCmdKey("&Edit",1,1,0,"") ! if edit_mode=1 then let fnCmdKey("&Edit",1,0,0,"") else let fnCmdKey("&Edit",1,1,0,"")
		fnCmdKey("&Add",2,0,0,"")
		fnCmdKey("Sea&rch",8,0,0,"")
	end if
	fnCmdKey("&Cancel",5,0,1,"")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	gl$=fnagl$(resp$(1)) : key$=gl$
	if ckey=7 then gosub DELETE_ACCT : goto L1780
	if edit_mode=1 and gl$<>holdgl$ then 
		! r: msgbox4
		mat ml$(4)
		ml$(1)="You cannot change an account in this manner!"
		ml$(2)="Take the 'Change #' option to change either the"
		ml$(3)="account or the description."
		ml$(4)="Click OK to access the new account; else Cancel to quit."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$='OK' then goto DO_EDIT
		if resp$='Cancel' then gl$=key$=holdgl$: goto Main ! /r
	end if
	! de$=resp$(1)(13:60)
	if edit_mode=1 then
		bb=val(resp$(2)) ! beginning bal
		cb=val(resp$(3)) ! current balance
		for j=1 to 6
			rf(j)=val(resp$(j+3)(1:5)) ! incomestatement reference numbers
		next j
		pbp=val(resp$(10)) ! prior years 2 years ago
		x=11
		for j=1 to 13
			bc(j)=val(resp$(x)): x+=1
			bp(j)=val(resp$(x)): x+=1
			bm(j)=val(resp$(x)): x+=1
			revb(j)=val(resp$(x)): x+=1
		next j
	end if
	if  ckey<>31 and ckey<>32 then edit_mode=0 ! ckey<>3 and
	if ckey=1 then goto DO_EDIT
	if ckey=2 then goto Add ! Add new accounts
	! if ckey=3 then goto REVIEW_TRANS ! review current or prior transactions

	if ckey=31 then
		currentOrHistory=1 ! current
		fn_transactionGrid(currentOrHistory,gl$)
		goto Main
	else if ckey=32 then
		currentOrHistory=2 ! history
		fn_transactionGrid(currentOrHistory,gl$)
		goto Main
	end if

	if ckey=6 then goto Save
	if ckey=8 then goto SEARCH_GRID
	if ckey=9 then goto ChangeAccountNumber
	goto Main

! BLDRANGE: fnchain("S:\acsGL\BldRange")
Xit: fnXit

Add: ! r:
	fnTos
	mylen=23: mypos=mylen+3 : rc=0
	if use_dept =1 then let fnLbl(1,26,"Fund #",6,2)
	if use_sub =1 then let fnLbl(1,40,"Sub #",6,2)
	fnLbl(2,1,"Account:",mylen,1)
	if use_dept then
		let fnTxt(2,26,3,0,1,"30",0,"Enter the fund portion of the account.",0 )
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,31,6,0,1,"1030",0,"Enter the Main part of the account.",0)
	resp$(rc+=1)=str$(ano)
	if use_sub then
		let fnTxt(2,40,3,0,1,"30",0,"Enter the sub portion of the account.",0 )
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,1)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=""
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Main
	fixgrid=99
	dno=ano=sno=0  : rc=0
	if use_dept then dno=val(resp$(rc+=1))
	ano=val(resp$(rc+=1))
	if use_sub then sno=val(resp$(rc+=1))
	d$=resp$(rc+=1)
	key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
	read #hAccount,using 'Form POS 1,N 3',key=key$: dno nokey ignore
	! NEW_RECORD: !
	! L1540: !
	bb=cb=pbp=0
	mat bc=(0): mat bp=(0): mat bm=(0): mat revb=(0): mat ta=(0): mat rf=(0)
	edit_mode=1
	gl$=key$
	! write_new_record
	write #hAccount,using Faccount: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if cl1=1 then
		write #8,using Faccount: gl$,d$
	end if
goto Main ! /r

DO_EDIT: ! r:
	pr newpage
	read #hAccount,using Faccount,key=key$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey L1650
	fnrglbig$(key$)
	for j=1 to 13
		if revb(j)=-202020202.02 then revb(j)=0
	next j
	edit_mode=1
	L1650: !
goto Main ! /r
Save: ! r:
	if holdgl$<>gl$ then ! attempting to change account
		mat ml$(3)
		ml$(1)="You are attempting to change account "&holdgl$&"!"
		ml$(2)="to "&gl$&".  Take OK to change the account."
		ml$(3)="Take Cancel to return to Main screen."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$<>"OK" then
			goto L1780
		end if
	end if
	! !f BB=0 Then bB=CB  ! need to make current bal & begbalance same if adding new record
	rewrite #hAccount,using Faccount,key=key$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if cl1=1 then
		rewrite #8,using Faccount,key=key$: gl$,d$ nokey L1760
	end if
	L1760: !
	if gl$<>holdkey$ then
		gosub CHG_GLNO_IN_HISTORY ! change accounts in history
		gosub ChangeCurrentTrans
	end if
	L1780: !
	bb=cb=pbp=0
	mat bc=(0) : mat bp=(0) : mat bm=(0) : mat revb=(0) : mat ta=(0) : mat rf=(0)
	edit_mode=0
	gl$=""
goto Main ! /r
DELETE_ACCT: ! r:
	if cb=0 then goto L1880
	mat ml$(3)
	ml$(1)="Account # "&gl$&" has a balance. You should not "
	ml$(2)="delete an account with a balance."
	ml$(3)="Take OK to delete; else Cancel to return to Main screen."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then delete_it=1: goto L1910
	if resp$="Cancel" then goto L2030
L1880: !
	mat ml$(3)
	ml$(1)="You have chosen to delete account # "&gl$&"!"
	ml$(2)="Take OK to delete the account."
	ml$(3)="Take Cancel to return to Main screen."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then delete_it=1: goto L1910
	if resp$="Cancel" then goto L2030
L1910: !
	delete #hAccount,key=gl$: ioerr L2030
	if cl1=1 then delete #8,key=gl$: nokey ignore
	goto ChangeCurrentTrans ! /r
ChangeCurrentTrans: ! r:
	if ta(1) then
		adr=ta(1)
		do
			read #hTransCurrent,using FtransCurrent,rec=adr: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
			if delete_it=1 then
				rewrite #hTransCurrent,using FtransCurrent,rec=adr: "",0,0,0,0,"","",0
			else
				rewrite #hTransCurrent,using FtransCurrent,rec=adr: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
			end if
			if nta then adr=nta
		loop until nta=0
		L2030: !
	end if
	delete_it=0
return  ! /r

! REVIEW_TRANS: ! r:
! 	fnTos
! 	mylen=23: mypos=mylen+3 
! 	fnFra(1,1,3,50,"Review transactions","Transactions are retained in the current files until the month is closed; then they are transferred to history ",0)
! 	fnOpt(1,1,"Current transactions",0,1)
! 	resp$(1)="True"
! 	fnOpt(2,1,"History transactions",0,1)
! 	resp$(2)="False"
! 	fnCmdSet(2)
! 	ckey=fnAcs(mat resp$)
! 	if ckey=5 then goto Main
! 	if resp$(1)="True" then currentOrHistory=1 ! current
! 	if resp$(2)="True" then currentOrHistory=2 ! history
! 	fn_transactionGrid(currentOrHistory,gl$)
! goto Main ! /r
def fn_transactionGrid(currentOrHistory,gl$)
	TransactionGrid:! 
	mat chdr$(9) : mat cmask$(9) : mat item$(9)
	chdr$(1)='Ref'         	: cmask$(1)="30"
	chdr$(2)='G/L #'       	: cmask$(2)=""
	chdr$(3)='Date'        	: cmask$(3)="3"
	chdr$(4)='Amount'      	: cmask$(4)='10'
	chdr$(5)='T Code'      	: cmask$(5)='30'
	chdr$(6)='P Code'      	: cmask$(6)='30'
	chdr$(7)='ChkRec #'    	: cmask$(7)=''
	chdr$(8)='Description' 	: cmask$(8)=''
	chdr$(9)='Period'      	: cmask$(9)='30'

	fnTos
	! r: build the grid
	fnflexinit1('Currentfile',1,1,20,85,mat chdr$,mat cmask$,1,0)
	adr=ta(1): pc2=0
	!  read current or history files
	if currentOrHistory=1 then goto ReadTransCurrent else goto RestoreTransHistory
	ReadTransCurrent: !
	if adr=0 then goto EO_TRANS_GRID
	read #hTransCurrent,using FtransCurrent,rec=adr,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	adr=nta
	goto DISPLAY_TRANS
	RestoreTransHistory: !
	ack$=gl$&cnvrt$("N 2",pc1)&"      "
	restore #hTransHistory,key>=ack$: nokey EO_TRANS_GRID
	ReadTransHistory: !
		read #hTransHistory,using FtransHistory,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof EO_TRANS_GRID
		if key$><trgl$ then goto EO_TRANS_GRID
		if pc1 and pc1><pc2 then goto ReadTransHistory
		DISPLAY_TRANS: !
		item$(1)=str$(rec(hTransHistory))
		item$(2)=trgl$
		item$(3)=str$(tr(4))
		item$(4)=str$(tr(5))
		item$(5)=str$(tr(6))
		item$(6)=str$(tr(7))
		item$(7)=tr$
		item$(8)=td$
		item$(9)=str$(pc2)
		fnflexadd1(mat item$)
	if currentOrHistory=1 then goto ReadTransCurrent else goto ReadTransHistory ! read from current or history
	EO_TRANS_GRID: !
	! /r
	if currentOrHistory=1 then 
		fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any information.")
	end if
	fnCmdKey("E&xit",5,0,1,"Exits to Main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto TransactionGridXit
	if ckey=2 then edit_mode=1 else edit_mode=0
	recordnum=val(resp$(1))
	if recordnum=0 then goto TransactionGridXit
	if currentOrHistory=1 then
		read #hTransCurrent,using FtransCurrent,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	else
		read #hTransHistory,using FtransHistory,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
	end if
	resp$(1)=str$(recordnum): resp$(2)=trgl$: resp$(3)=str$(tr(4))
	resp$(4)=str$(tr(5)): resp$(5)=str$(tr(6)) : resp$(6)=str$(tr(7))
	resp$(7)=tr$: resp$(8)=td$
	resp$(9)=str$(pc2)
	fnTos ! edit transaction screen
	mylen=23: mypos=mylen+3 
	fnLbl(1,1,"Account:",mylen,1)
	fnqglbig(1,mypos,0,2)
	resp$(1)=fnrglbig$(trgl$)
	holdgl$=gl$
	fnLbl(2,1,"Date:",mylen,1)
	fnTxt(2,mypos,10,0,1,"1",0,"",0)
	resp$(2)=str$(tr(4))
	fnLbl(3,1,"Amount:",mylen,1)
	fnTxt(3,mypos,10,0,1,"10",0,"",0)
	resp$(3)=str$(tr(5))
	fnLbl(4,1,"Trans Code:",mylen,1)
	fnTxt(4,mypos,2,0,1,"30",0,"Transaction Code - 1=Disbursment  2= Receipt  3= Adjustment",0)
	resp$(4)=str$(tr(6))
	fnLbl(5,1,"Post Code:",mylen,1)
	fnTxt(5,mypos,2,0,1,"30",0,"",0)
	resp$(5)=str$(tr(7))
	fnLbl(6,1,"ChkRef:",mylen,1)
	fnTxt(6,mypos,12,0,1,"",0,"",0)
	resp$(6)=tr$
	fnLbl(7,1,"Description:",mylen,1)
	fnTxt(7,mypos,30,0,0,"",0,"",0)
	resp$(7)=td$
	fnLbl(8,1,"Period:",mylen,1)
	fnTxt(8,mypos,2,0,0,"30",0,"",0)
	resp$(8)=str$(pc2)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		trgl$=fnagl$(resp$(1)) ! transaction gl #
		tr(4)=val(resp$(2)) ! date
		tr(5)=val(resp$(3)) ! amount
		tr(6)=val(resp$(4)) ! t code
		tr(7)=val(resp$(5)) ! p code
		tr$=resp$(6) ! reference #
		td$=resp$(7) ! reference #
		pc2=val(resp$(8)) ! period code from history; blank when returning from current
		if currentOrHistory=1 then
			rewrite #hTransCurrent,using FtransCurrent,rec=recordnum: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
		else
			rewrite #hTransHistory,using FtransHistory,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
		end if
		adr=ta(1)
		goto TransactionGrid
	end if
	TransactionGridXit: !
fnend

SEARCH_GRID: ! r:
	fnaccount_search(gl$,fixgrid)
	fixgrid=0
	read #hAccount,using Faccount,key=gl$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey Main
goto Main ! /r
CHG_GLNO_IN_HISTORY: ! r: change gl # in history
	ack$=holdgl$&cnvrt$("N 2",0)&"      "
	if trim$(ack$)="" then goto L3850
	restore #hTransHistory,key>=rpad$(ack$,kln(hTransHistory)): nokey L3850
	do
		read #hTransHistory,using FtransHistory: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof L3850
		if holdgl$=trgl$ then
			rewrite #hTransHistory,using FtransHistory: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
		end if
	loop
	L3850: !
return ! /r
ChangeAccountNumber: ! r:
	dno=val(gl$(1:3)): ano=val(gl$(4:9)): sno=val(gl$(10:12))
	mat resp$=("")
	fnTos
	mylen=28: mypos=mylen+3 : rc=0
	if use_dept =1 then let fnLbl(1,31,"Fund #",6,2)
	if use_sub =1 then let fnLbl(1,45,"Sub #",6,2)
	fnLbl(2,1,"New Account:",mylen,1)
	if use_dept then
		fnTxt(2,31,3,0,1,"30",0,"Enter the fund portion of the account.",0 )
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,36,6,0,1,"30",0,"Enter the Main part of the account.",0 )
	resp$(rc+=1)=str$(ano)
	if use_sub then
		fnTxt(2,45,3,0,1,"30",0,"Enter the sub portion of the account.",0 )
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,1)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=d$
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Main
	fixgrid=99
	dno=ano=sno=0
	if use_dept then dno=val(resp$(1)) : ano=val(resp$(2))
	if ~use_dept then ano=val(resp$(1))
	if use_dept and use_sub then sno=val(resp$(3))
	if ~use_dept and use_sub then sno=val(resp$(2))
	if use_dept and use_sub then d$=resp$(4)
	if ~use_dept and use_sub then d$=resp$(3)
	if ~use_dept and ~use_sub then d$=resp$(2)
	if use_dept and ~use_sub then d$=resp$(3)
	key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
	if key$<>gl$ then ! change account number
		read #hAccount,using 'Form POS 1,N 3',key=key$: dno nokey L4190
		! MSGBOX5: !
		mat ml$(3)
		ml$(1)="General ledger account # "&key$&" already "
		ml$(2)="exists. Take OK to review the account."
		ml$(3)="Take Cancel to return to Main screen."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$="OK" then goto DO_EDIT else goto Main
		L4190: !
		gl$=key$
	end if
	rewrite #hAccount,using Faccount,key=holdgl$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if cl1=1 then
		rewrite #8,using Faccount,key=holdgl$: gl$,d$ nokey ignore
	end if

	gosub CHG_GLNO_IN_HISTORY ! change accounts in history
	gosub ChangeCurrentTrans
goto Main ! /r

include: ertn
