! Formerly S:\acsGL\glMaint
! General Ledger Master File editor
! ______________________________________________________________________
	library 'S:\Core\Library': fnxit,fntop
	library 'S:\Core\Library': fnTos,fnOpt,fnLbl,fnCmdSet,fnAcs
	library 'S:\Core\Library': fnagl$,fnFra,fnTxt,fncombof,fnCmdKey
	library 'S:\Core\Library': fnflexinit1,fnflexadd1
	library 'S:\Core\Library': fnaccount_search
	library 'S:\Core\Library': fnqglbig,fnrglbig$
	library 'S:\Core\Library': fnButton
	library 'S:\Core\Library': fnmsgbox
	library 'S:\Core\Library': fngethandle
! fnrglbig$ and fnqglbig  were added so all of the description could easily be seen in the main gl screen
	on error goto Ertn
! ______________________________________________________________________
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
! ______________________________________________________________________
	fntop(program$)
	fixgrid=99
	open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
	close #company:
	open #8: "Name=[Q]\CLmstr\GLmstr.H[cno],KFName=[Q]\CLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed ioerr L310
	cl1=1
L310: !
	open #hAccount:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed
	open #hAccountUnused:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.H[cno],Shr",internal,outIn,keyed ! ioerr L350
	open #2: "Name=[Q]\GLmstr\GLTRANS.H[cno],Shr",internal,outIn,relative
	open #hAcTrans:=fngethandle: "Name=[Q]\GLmstr\ACTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Version=0,Use,RecL=72,KPs=1/71/17/13,KLn=12/2/2/4,Shr",internal,outIn,keyed
	Factrans: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
MAIN: !
	fnTos(sn$="GLProb2-"&str$(edit_mode))
	mylen=23 : mypos=mylen+3 : right=1
	fnLbl(1,1,"Account:",mylen,right)
	if edit_mode=1 then ! attempt to put disabled text box for acct #
		fnTxt(1,mypos,60,0,0,"",1,"",0)
		resp$(1)=fnrglbig$(gl$)
	else
		fnqglbig(1,mypos,0,2)
		resp$(1)=fnrglbig$(gl$)
	end if
	holdgl$=gl$
	if edit_mode=1 then
		fnLbl(2,1,"Beginning Balance:",mylen,right)
		fnTxt(2,mypos,14,0,right,"10",0,"The beginning balance will always be the account balance at the beginning of the period. It gets updated when you close the month.",0 )
		resp$(2)=str$(bb)
		fnLbl(2,40,"Current Balance:",mylen,right)
		fnTxt(2,66,14,0,right,"10",0,"The current balance will be updated any time a transaction is posted. The beginning and current balances should both be the same when you begin.",0 )
		resp$(3)=str$(cb)
		f1Col1Len=21
		f1Col2=1+f1Col1Len+2 : f1Col2Len=36
		f1Col3=f1Col2+f1Col2Len+2 : f1Col3len=21
		f1Col4=f1Col3+f1Col3len+2 : f1Col4Len=36
		fnFra(4,1,4,f1Col4+f1Col4Len+2,"Financial Statement Information"," ",0)
		fnLbl(1,1,"Balance Sheet Ref:",f1Col1Len,right,0,1)
		fncombof("fs-bal",1,f1Col2,f1Col2Len,"[Q]\GLmstr\acglfnsb.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnsbindx.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the balance sheet.",1)
		resp$(4)=str$(rf(1)) ! balance sheet ref #
		fnLbl(1,f1Col3,"2nd Balance Sheet:",f1Col3len,right,0,1)
		fncombof("fs-bal2",1,f1Col4,f1Col4Len,"[Q]\GLmstr\acglfnsc.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnscindx.h[cno]",0,0, "Select the balance sheet reference number where this account should appear on the secondary balance sheet.",1)
		resp$(5)=str$(rf(2)) ! balance sheet ref #
		fnLbl(2,1,"Income Statement Ref:",f1Col1len,right,0,1)
		fncombof("fs-inc",2,f1Col2,f1Col2Len,"[Q]\GLmstr\acglfnsi.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnsiindx.h[cno]",0,0, "Select the income statement reference number where this account should appear on the income statement.",1)
		resp$(6)=str$(rf(3)) ! income statement ref #
		fnLbl(2,f1Col3,"2nd Income Statement:",f1Col3len,right,0,1)
		fncombof("fs-inc2",2,f1Col4,f1Col4Len,"[Q]\GLmstr\acglfnsj.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnsjindx.h[cno]",0,0, "Select the income statement reference number where this account should appear on the secondary income statement.",1)
		resp$(7)=str$(rf(4)) ! 2nd income statement
		fnLbl(3,1,"Cash Flow/Fund Ref:",f1Col1len,right,0,1)
		fncombof("fs-cash",3,f1Col2,f1Col2Len,"[Q]\GLmstr\acglfnsf.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnsfindx.h[cno]",0,0, "Select the cash flow reference number where this account should appear on the cash flow statement.",1)
		resp$(8)=str$(rf(5)) ! income statement ref #
		fnLbl(3,f1Col3,"2nd Cash Flow/Fund:",f1Col3len,right,0,1)
		fncombof("fs-cash2",3,f1Col4,f1Col4Len,"[Q]\GLmstr\acglfnsg.h[cno]",1,5,6,30,"[Q]\GLmstr\Fnsgindx.h[cno]",0,0, "Select the cash flow reference number where this account should appear on the cash flow statement.",1)
		resp$(9)=str$(rf(6)) ! 2nd cash flow

		fnLbl(10,1,"EOY Balance - 2 Years Ago:",30,right,0,0)
		fnTxt(10,33,12,0,right,"10",0,"In order to pr prior year's cash flow and fund statements, the final balance from two years ago must be retained.",0)
		resp$(10)=str$(pbp)
		fnFra(12,1,14,105,"History and Budget Information"," ",0)
		x=11
		fnLbl(1,14,"Balance This Yr",15,0,0,2)
		fnLbl(1,34,"Balance Last Yr",15,0,0,2)
		fnLbl(1,54,"Original Budget",15,0,0,2)
		fnLbl(1,74,"Revised Budget ",15,0,0,2)
		for j=1 to 13
			fnLbl(j+1,1,"Period "&str$(j),90,0,0,2)
			fnTxt(j+1,14,14,0,right,"10",0,"",2 )
			resp$(x)=str$(bc(j))
			x=x+1
			fnTxt(j+1,34,14,0,right,"10",0,"",2 )
			resp$(x)=str$(bp(j))
			x=x+1
			fnTxt(j+1,54,14,0,right,"10",0,"",2 )
			resp$(x)=str$(bm(j))
			x=x+1
			fnTxt(j+1,74,14,0,right,"10",0,"",2 )
			resp$(x)=str$(revb(j))
			x=x+1
		next j
	end if
	if edit_mode=1 then
		fnCmdKey("&Save",6,1,0,"")
		fnCmdKey("Review &Transactions",3,0,0,"")
		fnCmdKey("&Delete",7,0,0,"")
		fnButton(1,mypos+60+2,"C&hange",9,"Change the Account and/or the Description of this General Ledger Account") ! fnCmdKey("C&hange Acct-Desc",9,0,0,"")
	else
		fnCmdKey("&Edit",1,1,0,"") ! if edit_mode=1 then let fnCmdKey("&Edit",1,0,0,"") else let fnCmdKey("&Edit",1,1,0,"")
		fnCmdKey("&Add",2,0,0,"")
		fnCmdKey("Sea&rch",8,0,0,"")
	end if
	fnCmdKey("&Cancel",5,0,1,"")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
! If edit_mode=1 Then kEY$=GL$=LPAD$(RESP$(1)(1:12),12): Goto 885
	gl$=fnagl$(resp$(1)) : key$=gl$
	if ckey=7 then gosub DELETE_ACCT : goto L1780
	if edit_mode=1 and gl$<>holdgl$ then goto MSGBOX4 else goto L960
L960: !
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
	if ckey<>3 then edit_mode=0
	if ckey=1 then goto DO_EDIT
	if ckey=2 then goto ADD ! add new accounts
	if ckey=3 then goto REVIEW_TRANS ! review current or prior transactions
	if ckey=6 then goto SAVE
	if ckey=8 then goto SEARCH_GRID
	if ckey=9 then goto CHANGE_ACCT_NUM
	goto MAIN
! ______________________________________________________________________
! BLDRANGE: fnchain("S:\acsGL\BldRange")
XIT: fnxit
! ______________________________________________________________________
ADD: !
	fnTos(sn$="GLAdd")
	mylen=23: mypos=mylen+3 : right=1: rc=0
	if use_dept =1 then let fnLbl(1,26,"Fund #",6,2)
	if use_sub =1 then let fnLbl(1,40,"Sub #",6,2)
	fnLbl(2,1,"Account:",mylen,right)
	if use_dept then
		let fnTxt(2,26,3,0,right,"30",0,"Enter the fund portion of the account.",0 )
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,31,6,0,right,"1030",0,"Enter the main part of the account.",0)
	resp$(rc+=1)=str$(ano)
	if use_sub then
		let fnTxt(2,40,3,0,right,"30",0,"Enter the sub portion of the account.",0 )
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,right)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=""
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto MAIN
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
	write #hAccount,using L1740: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	if cl1=1 then
		write #8,using L1740: gl$,d$
	end if
	goto MAIN
! ______________________________________________________________________
DO_EDIT: ! r:
	pr newpage
	read #hAccount,using L1740,key=key$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey L1650
	fnrglbig$(key$)
	for j=1 to 13
		if revb(j)=-202020202.02 then revb(j)=0
	next j
	edit_mode=1
L1650: !
	goto MAIN
! /r
SAVE: ! r:
	if holdgl$<>gl$ then ! attempting to change account
		mat ml$(3)
		ml$(1)="You are attempting to change account "&holdgl$&"!"
		ml$(2)="to "&gl$&".  Take OK to change the account."
		ml$(3)="Take Cancel to return to main screen."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$<>"OK" then
			goto L1780
		end if
	end if
! !f BB=0 Then bB=CB  ! need to make current bal & begbalance same if adding new record
	rewrite #hAccount,using L1740,key=key$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
L1740: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
	if cl1=1 then
		rewrite #8,using L1740,key=key$: gl$,d$ nokey L1760
	end if
L1760: !
	if gl$<>holdkey$ then
		gosub CHG_GLNO_IN_HISTORY ! change accounts in history
		gosub CHANGE_CURRENT_TRANS
	end if
L1780: !
	bb=cb=pbp=0
	mat bc=(0) : mat bp=(0) : mat bm=(0) : mat revb=(0) : mat ta=(0) : mat rf=(0)
	edit_mode=0
	gl$=""
	goto MAIN
! /r
DELETE_ACCT: ! r:
	if cb=0 then goto L1880
	mat ml$(3)
	ml$(1)="Account # "&gl$&" has a balance. You should not "
	ml$(2)="delete an account with a balance."
	ml$(3)="Take OK to delete; else Cancel to return to main screen."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then delete_it=1: goto L1910
	if resp$="Cancel" then goto L2030
L1880: !
	mat ml$(3)
	ml$(1)="You have chosen to delete account # "&gl$&"!"
	ml$(2)="Take OK to delete the account."
	ml$(3)="Take Cancel to return to main screen."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then delete_it=1: goto L1910
	if resp$="Cancel" then goto L2030
L1910: !
	delete #hAccount,key=gl$: ioerr L2030
	if cl1=1 then delete #8,key=gl$: nokey ignore
	goto CHANGE_CURRENT_TRANS ! /r
CHANGE_CURRENT_TRANS: ! r:
	if ta(1)=0 then goto L2030
	adr=ta(1)
L1960: read #2,using L2800,rec=adr: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	if delete_it=1 then goto L1980 else goto L2000
L1980: rewrite #2,using L2800,rec=adr: "",0,0,0,0,"","",0
	goto L2010
L2000: rewrite #2,using L2800,rec=adr: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
L2010: if nta=0 then goto L2030
	adr=nta : goto L1960
L2030: delete_it=0
	return  ! /r
IGNORE: continue
REVIEW_TRANS: ! r:
	fnTos(sn$="review_trans")
	mylen=23: mypos=mylen+3 : right=1: rc=0
	fnFra(1,1,3,50,"Review transactions","Transactions are retained in the current files until the month is closed; then they are transferred to history ",0)
	fnOpt(1,1,"Current transactions",0,1)
	resp$(1)="True"
	fnOpt(2,1,"History transactions",0,1)
	resp$(2)="False"
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto MAIN
	if resp$(1)="True" then currentOrHistory=1
	if resp$(2)="True" then currentOrHistory=2
! ______________________________________________________________________
TRANSACTION_GRID: !
	mat chdr$(9) : mat cmask$(9) : mat item$(9)
	chdr$(1)='Ref': chdr$(2)='G/L #': chdr$(3)='Date'
	chdr$(4)='Amount'
	chdr$(5)='T Code' : chdr$(6)='P Code'
	chdr$(7)='Ck/Rec #' : chdr$(8)='Description'
	chdr$(9)='Period'
	cmask$(1)="30"
	cmask$(2)="": cmask$(3)="3" : cmask$(4)='10'
	cmask$(5)='30' : cmask$(6)='30': cmask$(7)=''
	cmask$(8)='' : cmask$(9)='30'
	fnTos(sn$="gltrans")
	fnflexinit1('Currentfile',1,1,20,85,mat chdr$,mat cmask$,1,0)
	adr=ta(1): pc2=0
!  read current or history files
	if currentOrHistory=1 then goto READ_FROM_CURRENT else goto READ_FROM_HISTORY
	READ_FROM_CURRENT: !
	transfile=2
	L2780: if adr=0 then goto EO_TRANS_GRID
	read #2,using L2800,rec=adr,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	L2800: form pos 1,c 12,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
	adr=nta
	goto DISPLAY_TRANS
	READ_FROM_HISTORY: !
	transfile=3
	ack$=gl$&cnvrt$("N 2",pc1)&"      "
	restore #hAcTrans,key>=ack$: nokey EO_TRANS_GRID
	ReadAcTrans: !
	read #hAcTrans,using Factrans,release: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof EO_TRANS_GRID
	if key$><trgl$ then goto EO_TRANS_GRID
	if pc1 and pc1><pc2 then goto ReadAcTrans
	DISPLAY_TRANS: !
	item$(1)=str$(rec(hAcTrans))
	item$(2)=trgl$
	item$(3)=str$(tr(4))
	item$(4)=str$(tr(5))
	item$(5)=str$(tr(6))
	item$(6)=str$(tr(7))
	item$(7)=tr$
	item$(8)=td$
	item$(9)=str$(pc2)
	fnflexadd1(mat item$)
	if currentOrHistory=1 then goto L2780 else goto ReadAcTrans ! read from current or history
EO_TRANS_GRID: !
	if currentOrHistory=1 then let fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any information.")
	fnCmdKey("E&xit",5,0,1,"Exits to main menu")
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto MAIN
	if ck=2 then edit_mode=1 else edit_mode=0
	recordnum=val(resp$(1))
	if recordnum=0 then goto MAIN
	if currentOrHistory=1 then
		read #2,using L2800,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	else
		read #hAcTrans,using Factrans,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
	end if
	resp$(1)=str$(recordnum): resp$(2)=trgl$: resp$(3)=str$(tr(4))
	resp$(4)=str$(tr(5)): resp$(5)=str$(tr(6)) : resp$(6)=str$(tr(7))
	resp$(7)=tr$: resp$(8)=td$
	resp$(9)=str$(pc2)
	fnTos(sn$="Tredit")
	mylen=23: mypos=mylen+3 : right=1
	fnLbl(1,1,"Account:",mylen,right)
	fnqglbig(1,mypos,0,2)
	resp$(1)=fnrglbig$(trgl$)
	holdgl$=gl$
	fnLbl(2,1,"Date:",mylen,right)
	fnTxt(2,mypos,10,0,right,"1",0,"",0)
	resp$(2)=str$(tr(4))
	fnLbl(3,1,"Amount:",mylen,right)
	fnTxt(3,mypos,10,0,right,"10",0,"",0)
	resp$(3)=str$(tr(5))
	fnLbl(4,1,"Trans Code:",mylen,right)
	fnTxt(4,mypos,2,0,right,"30",0,"Transaction Code - 1=Disbursment  2= Receipt  3= Adjustment",0)
	resp$(4)=str$(tr(6))
	fnLbl(5,1,"Post Code:",mylen,right)
	fnTxt(5,mypos,2,0,right,"30",0,"",0)
	resp$(5)=str$(tr(7))
	fnLbl(6,1,"Ck/Ref:",mylen,right)
	fnTxt(6,mypos,12,0,right,"",0,"",0)
	resp$(6)=tr$
	fnLbl(7,1,"Description:",mylen,right)
	fnTxt(7,mypos,30,0,0,"",0,"",0)
	resp$(7)=td$
	fnLbl(8,1,"Period:",mylen,right)
	fnTxt(8,mypos,2,0,0,"30",0,"",0)
	resp$(8)=str$(pc2)
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto MAIN
	trgl$=fnagl$(resp$(1)) ! transaction gl #
	tr(4)=val(resp$(2)) ! date
	tr(5)=val(resp$(3)) ! amount
	tr(6)=val(resp$(4)) ! t code
	tr(7)=val(resp$(5)) ! p code
	tr$=resp$(6) ! reference #
	td$=resp$(7) ! reference #
	pc2=val(resp$(8)) ! period code from history; blank when returning from current
	if currentOrHistory=1 then
		rewrite #2,using L2800,rec=recordnum: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,nta
	else
		rewrite #hAcTrans,using Factrans,rec=recordnum: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
	end if
	adr=ta(1)
goto TRANSACTION_GRID ! /r
SEARCH_GRID: ! r:
	fnaccount_search(gl$,fixgrid)
	fixgrid=0
	read #hAccount,using L1740,key=gl$: gl$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb nokey MAIN
	goto MAIN
! /r
CHG_GLNO_IN_HISTORY: ! r: change gl # in history
	ack$=holdgl$&cnvrt$("N 2",0)&"      "
	if trim$(ack$)="" then goto L3850
	restore #hAcTrans,key>=rpad$(ack$,kln(3)): nokey L3850
	do
		read #hAcTrans,using Factrans: trgl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2 eof L3850
		if holdgl$=trgl$ then
			rewrite #hAcTrans,using Factrans: gl$,tr(4),tr(5),tr(6),tr(7),tr$,td$,pc2
		end if
	loop
	L3850: !
return ! /r
CHANGE_ACCT_NUM: ! r:
	dno=val(gl$(1:3)): ano=val(gl$(4:9)): sno=val(gl$(10:12))
	mat resp$=("")
	fnTos(sn$="GLchange")
	mylen=28: mypos=mylen+3 : right=1: rc=0
	if use_dept =1 then let fnLbl(1,31,"Fund #",6,2)
	if use_sub =1 then let fnLbl(1,45,"Sub #",6,2)
	fnLbl(2,1,"New Account:",mylen,right)
	if use_dept then
		fnTxt(2,31,3,0,right,"30",0,"Enter the fund portion of the account.",0 )
		resp$(rc+=1)=str$(dno)
	end if
	fnTxt(2,36,6,0,right,"30",0,"Enter the main part of the account.",0 )
	resp$(rc+=1)=str$(ano)
	if use_sub then
		fnTxt(2,45,3,0,right,"30",0,"Enter the sub portion of the account.",0 )
		resp$(rc+=1)=str$(sno)
	end if
	fnLbl(3,1,"Description:",mylen,right)
	fnTxt(3,mypos,50,0,left,"",0,"Enter the account description.",0 )
	resp$(rc+=1)=d$
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto MAIN
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
		ml$(3)="Take Cancel to return to main screen."
		fnmsgbox(mat ml$,resp$,'',49)
		if resp$="OK" then goto DO_EDIT else goto MAIN
		L4190: !
		gl$=key$
	end if
	rewrite #hAccount,using Faccount,key=holdgl$: key$,d$,mat rf,bb,cb,mat bc,mat bp,mat bm,pbp,mat ta,mat revb
	Faccount: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
	if cl1=1 then
		rewrite #8,using Faccount,key=holdgl$: gl$,d$ nokey L4230
	end if
	L4230: !
	gosub CHG_GLNO_IN_HISTORY ! change accounts in history
	gosub CHANGE_CURRENT_TRANS
goto MAIN ! /r
MSGBOX4: ! r:
	mat ml$(4)
	ml$(1)="You cannot change an account in this manner!"
	ml$(2)="Take the 'Change #' option to change either the"
	ml$(3)="account or the description."
	ml$(4)="Click OK to access the new account; else Cancel to quit."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$='OK' then goto DO_EDIT
if resp$='Cancel' then gl$=key$=holdgl$: goto MAIN ! /r
include: ertn
