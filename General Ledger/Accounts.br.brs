! Formerly S:\acsGL\glMaint
! General Ledger Master File editor
fn_setup
fnTop(program$)
dim ml$(0)*128
! fixgrid=99
open #hCompany=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
read #hCompany,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
close #hCompany:
open #hClAcct=fnH: "Name=[Q]\CLmstr\GLmstr.h[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed ioerr OpenClAcctFinis
checkbookEnabled=1
OpenClAcctFinis: !
open #hAccount=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
open #hAccountUnused=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.h[cno],Shr",internal,outIn,keyed
Faccount: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
open #hTransCurrent=fnH: 'Name=[Q]\GLmstr\GLTrans.h[cno],kfname=[Q]\GLmstr\glTrans-IdxAcct.h[cno],Shr',internal,outIn,keyed
FtransCurrent: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
open #hTransHistory=fnH: "Name=[Q]\GLmstr\ACTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Version=0,Use,RecL=72,KPs=1/71/17/13,KLn=12/2/2/4,Shr",internal,outIn,keyed
FtransHistory: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,n 2

Main: ! r:
	dim resp$(100)*60
	fnTos
	mylen=23 : mypos=mylen+3
	fnLbl(1,1,"Account:",mylen,1)
	if edit_mode=1 then ! attempt to put disabled text box for acct #
		fnTxt(1,mypos,60,0,0,"",1,"",0)
		resp$(1)=fnRglBig$(gl$)
	else
		fnQglBig(1,mypos,0,2,1)
		resp$(1)=fnRglBig$(gl$)
		! if resp$(1)='--' or resp$(1)='-' then resp$(1)=''
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
		dim rf(6)
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
		dim bc(13),bp(13),bm(13),revb(13)
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
		fnCmdKey("&Edit",1,1,0,"") ! if edit_mode=1 then fnCmdKey("&Edit",1,0,0,"") else fnCmdKey("&Edit",1,1,0,"")
		fnCmdKey("&Add",2,0,0,"")
		fnCmdKey("Sea&rch",8,0,0,"")
	end if
	fnCmdKey("&Cancel",5,0,1,"")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dim key$*12
	gl$=fnagl$(resp$(1)) : key$=gl$
	if ckey=7 then gosub AcctDelete : goto SaveFinis
	if edit_mode=1 and gl$<>holdgl$ then 
		! r: msgbox4
		mat ml$(4)
		ml$(1)="You cannot change an account in this manner!"
		ml$(2)="Take the 'Change #' option to change either the"
		ml$(3)="account or the description."
		ml$(4)="Click OK to access the new account; else Cancel to quit."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$='OK' then 
			gosub AccountEditRead
		else if resp$='Cancel' then 
			gl$=key$=holdgl$
		end if
		goto Main ! /r
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
	if ckey=1 then 
		gosub AccountEditRead
		! goto Main
	else if ckey=2 then ! Add new accounts
		goto Add
	else if ckey=31 then
		fn_transactionGrid(1,gl$,hTransCurrent) ! current
		! goto Main
	else if ckey=32 then
		fn_transactionGrid(0,gl$,hTransHistory) ! history
		! goto Main
	else if ckey=6 then 
		goto Save
	else if ckey=8 then
		fnGlAccountSearch(gl$) ! ,fixgrid)
		! fixgrid=0
		read #hAccount,using Faccount,key=gl$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey Main
	else if ckey=9 then 
		goto ChangeAccountNumber
	end if
goto Main ! /r

! BLDRANGE: fnchain("S:\acsGL\BldRange")
Xit: fnXit

Add: ! r:
	fnTos
	mylen=23: mypos=mylen+3 : rc=0
	if use_dept =1 then fnLbl(1,26,"Fund #",6,2)
	if use_sub =1 then fnLbl(1,40,"Sub #",6,2)
	fnLbl(2,1,"Account:",mylen,1)
	if use_dept then
		fnTxt(2,26,3,0,1,"30",0,"Enter the fund portion of the account.",0 )
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,31,6,0,1,"1030",0,"Enter the Main part of the account.",0)
	resp$(rc+=1)=str$(ano)
	if use_sub then
		fnTxt(2,40,3,0,1,"30",0,"Enter the sub portion of the account.",0 )
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,1)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=""
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Main
	! fixgrid=99
	dno=ano=sno=rc=0
	if use_dept then dno=val(resp$(rc+=1))
	ano=val(resp$(rc+=1))
	if use_sub then sno=val(resp$(rc+=1))
	dim d$*50
	d$=resp$(rc+=1)
	key$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
	read #hAccount,using 'Form POS 1,N 3',key=key$: dno nokey ignore
	! NEW_RECORD: !
	! L1540: !
	bb=cb=pbp=0
	dim ta(2)
	mat bc=(0): mat bp=(0): mat bm=(0): mat revb=(0): mat ta=(0): mat rf=(0)
	edit_mode=1
	gl$=key$
	! write_new_record
	write #hAccount,using Faccount: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if checkbookEnabled then
		write #hClAcct,using Faccount: gl$,d$
	end if
goto Main ! /r

AccountEditRead: ! r:
	gl$=d$=''
	mat rf=(0)
	bb=cb=0
	mat bc=(0)
	mat bp=(0)
	mat bm=(0)
	pbp=0
	mat ta=(0)
	mat revb=(0)
	read #hAccount,using Faccount,key=key$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey AcctEditReadFinis
	fnrglbig$(key$)
	for j=1 to 13
		if revb(j)=-202020202.02 then revb(j)=0
	next j
	edit_mode=1
	AcctEditReadFinis: !
return ! /r
Save: ! r:
	if holdgl$<>gl$ then ! attempting to change account
		mat ml$(3)
		ml$(1)="You are attempting to change account "&holdgl$&"!"
		ml$(2)="to "&gl$&".  Take OK to change the account."
		ml$(3)="Take Cancel to return to Main screen."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$<>"OK" then
			goto SaveFinis
		end if
	end if
	! !f BB=0 Then bB=CB  ! need to make current bal & begbalance same if adding new record
	rewrite #hAccount,using Faccount,key=key$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if checkbookEnabled then
		rewrite #hClAcct,using Faccount,key=key$: gl$,d$ nokey ignore
	end if
	if gl$<>holdkey$ then
		gosub ChangeGLNoInHistoryTrans ! change accounts in history
		gosub ChangeGLNoInCurrentTrans
	end if
goto SaveFinis ! /r

SaveFinis: ! r:
	bb=cb=pbp=0
	mat bc=(0) : mat bp=(0) : mat bm=(0) : mat revb=(0) : mat ta=(0) : mat rf=(0)
	edit_mode=0
	gl$=""
goto Main ! /r
AcctDelete: ! r:
	if cb then
		mat ml$(3)
		ml$(1)="Account "&gl$&" has a balance. You may not "
		ml$(2)="delete an account with a balance."
		ml$(3)="Take OK to return to Main screen."
		fnmsgbox(mat ml$,resp$,'',mb_exclamation+mb_okonly)
	else
		mat ml$(3)
		ml$(1)="You have chosen to delete account "&gl$&"!"
		ml$(2)="Take OK to delete the account."
		ml$(3)="Take Cancel to return to Main screen."
		fnmsgbox(mat ml$,resp$,'',mb_exclamation+mb_okcancel)
		if resp$="OK" then 
			delete_it=1
			delete #hAccount,key=gl$: ioerr AcctDeleteFinis
			if checkbookEnabled then delete #hClAcct,key=gl$: nokey ignore
			gosub ChangeGLNoInCurrentTrans
		end if
	end if
	AcctDeleteFinis: !
	delete_it=0
return ! /r
ChangeGLNoInCurrentTrans: ! r: (&delete_it,ta(1))
	if ta(1) then
		adr=ta(1)
		do
			dim tr(7),tr$*12,td$*30
			read #hTransCurrent,using FtransCurrent,rec=adr: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
			if delete_it=1 then
				rewrite #hTransCurrent,using FtransCurrent,rec=adr: "",0,0,0,0,"","",0
			else
				rewrite #hTransCurrent,using FtransCurrent,rec=adr: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
			end if
			if nta then adr=nta
		loop until nta=0
	end if
	delete_it=0
return  ! /r

def fn_transactionGrid(isCurrent,gl$,h; ___,nta,recordnum,pc2,trgl$*12)
	! isCurrent =1 Current - include Edit feature
	! isCurrent<>1 History - no edit button
	TransactionGrid:! 
	fnTos
	! r: build the grid
	dim item$(9)*30
	mat chdr$(9) : mat cmask$(9) : mat item$(9)
	chdr$(1)='Rec'          	: cmask$(1)="30"
	chdr$(2)='Account'     	: cmask$(2)=""
	chdr$(3)='Date'        	: cmask$(3)="3"
	chdr$(4)='Amount'      	: cmask$(4)='10'
	chdr$(5)='TrType'      	: cmask$(5)='30'
	chdr$(6)='Post'        	: cmask$(6)='30'
	chdr$(7)='Chk/Ref '    	: cmask$(7)=''
	chdr$(8)='Description' 	: cmask$(8)=''
	chdr$(9)='Period'      	: cmask$(9)='30'
	fnflexinit1('Currentfile',1,1,20,85,mat chdr$,mat cmask$,1,0)
	restore #h,search>=gl$: nokey EoTransGrid
	do
		if isCurrent=1 then 
			read #h,using FtransCurrent,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$ eof EoTransGrid
		else 
			read #h,using FtransHistory,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$ eof EoTransGrid
		end if
		if gl$=trgl$ then
			item$(1)=str$(rec(h))
			item$(2)=trgl$
			item$(3)=str$(tr(4))
			item$(4)=str$(tr(5))
			item$(5)=str$(tr(6))
			item$(6)=str$(tr(7))
			item$(7)=tr$
			item$(8)=td$
			item$(9)=str$(pc2)
			fnflexadd1(mat item$)
		end if
	loop while gl$=trgl$
	EoTransGrid: !
	! /r
	if isCurrent=1 then 
		fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any information.")
	end if
	fnCmdKey("E&xit",5,0,1,"Exits to Main menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto TransactionGridXit
	if ckey=2 then edit_mode=1 else edit_mode=0
	recordnum=val(resp$(1))
	if ~recordnum then goto TransactionGridXit
	if isCurrent=1 then
		read #h,using FtransCurrent,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	else
		read #h,using FtransHistory,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
	end if
	resp$(1)=str$(recordnum): resp$(2)=trgl$: resp$(3)=str$(tr(4))
	resp$(4)=str$(tr(5)): resp$(5)=str$(tr(6)) : resp$(6)=str$(tr(7))
	resp$(7)=tr$: resp$(8)=td$
	resp$(9)=str$(pc2)
	fnTos ! edit transaction screen
	mylen=23: mypos=mylen+3 
	fnLbl(1,1,"Account:",mylen,1)     	: fnqglbig(1,mypos,0,2,1)            	: resp$(1)=fnrglbig$(trgl$) 	: holdgl$=gl$
	fnLbl(2,1,"Date:",mylen,1)         	: fnTxt(2,mypos,10,0,1,"1",0,"",0)   	: resp$(2)=str$(tr(4))
	fnLbl(3,1,"Amount:",mylen,1)      	: fnTxt(3,mypos,10,0,1,"10",0,"",0)  	: resp$(3)=str$(tr(5))
	fnLbl(4,1,"Trans Code:",mylen,1)  	: fnTxt(4,mypos,2,0,1,"30",0,"Transaction Code - 1=Disbursment  2= Receipt  3= Adjustment",0)  	: resp$(4)=str$(tr(6))
	fnLbl(5,1,"Post Code:",mylen,1)   	: fnTxt(5,mypos,2,0,1,"30",0,"",0)   	: resp$(5)=str$(tr(7))
	fnLbl(6,1,"ChkRef:",mylen,1)      	: fnTxt(6,mypos,12,0,1,"",0,"",0)    	: resp$(6)=tr$
	fnLbl(7,1,"Description:",mylen,1) 	: fnTxt(7,mypos,30,0,0,"",0,"",0)    	: resp$(7)=td$
	fnLbl(8,1,"Period:",mylen,1)      	: fnTxt(8,mypos,2,0,0,"30",0,"",0)   	: resp$(8)=str$(pc2)
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
		if isCurrent=1 then
			rewrite #h,using FtransCurrent,rec=recordnum: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
		else
			rewrite #h,using FtransHistory,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
		end if
		adr=ta(1)
		goto TransactionGrid
	end if
	TransactionGridXit: !
fnend


ChangeGLNoInHistoryTrans: ! r: change gl # in history
	dim ack$*20
	ack$=holdgl$&cnvrt$("N 2",0)&"      "
	if trim$(ack$)<>'' then
		restore #hTransHistory,key>=rpad$(ack$,kln(hTransHistory)): nokey L3850
		do
			read #hTransHistory,using FtransHistory: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof L3850
			if holdgl$=trgl$ then
				rewrite #hTransHistory,using FtransHistory: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
			end if
		loop
		L3850: !
	end if
return ! /r
ChangeAccountNumber: ! r:
	dno=val(gl$(1:3)) : ano=val(gl$(4:9)) : sno=val(gl$(10:12))
	mat resp$=("")
	fnTos
	mylen=28 : mypos=mylen+3 : rc=0
	if use_dept then fnLbl(1,31,"Fund #",6,2)
	if use_sub  then fnLbl(1,45,"Sub #",6,2)
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
	! fixgrid=99
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
		if resp$="OK" then gosub AccountEditRead 
		goto Main
		
		L4190: !
		gl$=key$
	end if
	rewrite #hAccount,using Faccount,key=holdgl$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if checkbookEnabled then
		rewrite #hClAcct,using Faccount,key=holdgl$: gl$,d$ nokey ignore
	end if
	gosub ChangeGLNoInHistoryTrans ! change accounts in history
	gosub ChangeGLNoInCurrentTrans
goto Main ! /r

include: fn_setup
