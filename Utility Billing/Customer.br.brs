! Customer File Editor
fn_setup
fnTop(program$)
fn_customer
Xit: fnXit
def library fnCustomer(; &editOne$)
	if ~setup then fn_setup
	fnCustomer=fn_customer( editOne$)
fnend
def fn_customer(; &editOne$,___,editOne,ckey)
	disableBalanceEdit=1
	if editOne$<>'' then
		editOne=1
	end if
	! r: open files
	open #h_ubadrbil=fnH: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr,Use,RecL=130,KPs=1,KLn=10",internal,outIn,keyed  ! was :=3
	FadrBil: form pos 1,c 10,4*c 30
	gosub Cass1Open
	fn_setup_depositChange ! INITIALIZE DEPOSIT TRACKING FILES
	! r: BUD1: ! INITILIZE BUDGET FILE
	bud1=0
	open #h_budmstr=fnH: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr,Use,RecL=80,KPs=1,KLn=10",internal,outIn,keyed  ! was 81
	F_BUDMSTR: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	open #h_budtrans=fnH: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr,Use,RecL=149",internal,outIn,relative  ! was 82
	F_BUDTRANS: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	bud1=1
	! /r
	dim customer$(0)*256
	dim customerN(0)
	h_customer_1=fn_open('UB Customer',mat customer$,mat customerN,mat form$)
	h_customer_2=h_customer_1+1
	! open #h_customer_1=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed ! 1
	! open #h_customer_2=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",internal,outIn,keyed  ! 11
	! open #h_customer_3=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",internal,outIn,keyed ! Meter address
	! open #h_customer_4=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",internal,outIn,keyed
	! open #h_customer_5=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed
	F_CUSTOMER_1: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1712,c 1,c 9,c 2,c 17,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	open #h_citystzip=fnH: "Name=[Q]\Data\CityStZip.dat,KFName=[Q]\Data\CityStZip.Idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed
	! /r
	
	goto AskAcct
	
	ACCOUNT_X_NOKEY: ! r:
		mat ml$(2)
		ml$(1)="Account "&x$&' could not be found.'
		ml$(2)="Select a different account."
		fnmsgbox(mat ml$,resp$,'',48)
	goto AskAcct ! /r
	EDIT_CUSTOMER: ! r:
	jbact$=x$ ! ken 80905
	if len(x$)<>10 then goto AskAcct

	read #h_customer_1,using form$(h_customer_1),key=x$: mat customer$,mat customerN nokey ACCOUNT_X_NOKEY
	fn_customer2legacy(mat customer$,mat customerN,z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$)
	! read #h_customer_1,using F_CUSTOMER_1,key=x$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$ nokey ACCOUNT_X_NOKEY

	fnFixPd(mat extra)
	gosub REMOVE_INCORRECT_ALLOCATIONS

	holdz$=z$
	olde3$=e$(3)
	EDIT_LOADED_CUSTOMER: !
		dim meterAddressBeforeEdit$*30
		oldService1DepositAmount=b(8)
		oldService2DepositAmount=b(9)
		oldService3DepositAmount=b(10)
		oldService4DepositAmount=b(11)
		!   old_gas_deposit=b(11)
		mat ab$=('')
		read #h_ubadrbil,using "Form POS 11,4*C 30",key=z$: mat ab$ nokey ignore
		! pb=bal
		odp=b(8)+b(9)+b(11)
	goto NameScreen ! /r
	
	CHECK_BALANCE_BREAKDOWN: ! r:
		gosub TGB_SET
		! Gosub DRAFT1
		if tgb=bal then goto REWRITE_RECORD
	goto BREAKDOWN_NOT_EQUAL ! /r

	REWRITE_RECORD: ! r:
		gosub ALT_ADDRESS_SAVE ! rewrite alternate billing address
		if holdz$<>z$ then goto ASK_CONFIRM_KEY_CHANGE
		release #h_customer_2: ioerr ignore

		fn_legacy2customer(mat customer$,mat customerN,z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$)
		rewrite #h_customer_1,using form$(h_customer_1),key=z$: mat customer$,mat customerN
		! rewrite #h_customer_1,using F_CUSTOMER_1,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$

		if ad1 then let fn_record_previous_update(z$) ! &' '&e$(2))
		if oldService1DepositAmount<>b(8) then
			fn_depositChangeLog(z$,oldService1DepositAmount,b(8),date('mmddyy'),trim$(srvnam$(1))(1:15)&' Deposit Changed')
		end if
		if oldService2DepositAmount<>b(9) then
			fn_depositChangeLog(z$,oldService2DepositAmount,b(9),date('mmddyy'),trim$(srvnam$(2))(1:15)&' Deposit Changed')
		end if
		if oldService3DepositAmount<>b(10) then
			fn_depositChangeLog(z$,oldService3DepositAmount,b(10),date('mmddyy'),trim$(srvnam$(3))(1:15)&' Deposit Changed')
		end if
		if oldService4DepositAmount<>b(11) then
			fn_depositChangeLog(z$,oldService4DepositAmount,b(11),date('mmddyy'),trim$(srvnam$(4))(1:15)&' Deposit Changed')
		end if
		if olde3$=e$(3) then goto PAST_CASS_DELETE ! delete bar code if address changes
		if cassopen=0 then goto PAST_CASS_DELETE
		read #h_cass1,using 'Form POS 1,C 10,POS 96,C 12',key=z$: z2$,bc$ nokey PAST_CASS_DELETE
		delete #h_cass1,key=z$: ioerr ignore
	PAST_CASS_DELETE: !
	! probably change customer in ubtrans-vb here !Gosub 5130

	if ad1=1 then goto ADD_RECORD else goto AskAcct ! /r

	ASK_CONFIRM_KEY_CHANGE: ! r:
		mat ml$(2)
		ml$(1)="Do you wish to change account"
		ml$(2)='From "'&holdz$&'" to "'&z$&'"'
		fnmsgbox(mat ml$,resp$,'',36)
		if uprc$(resp$(1:1))="Y" then
			goto ACC_KEY_CHANGE_TEST_EXIST
		else if uprc$(resp$(1:1))="N" then
			z$=x$
			goto NameScreen
		else
			goto ASK_CONFIRM_KEY_CHANGE
		end if  ! /r
	ACC_KEY_CHANGE_TEST_EXIST: ! r:
		read #h_customer_1,using 'form pos 1,c 10',key=z$: p$ nokey ACC_KC_VALID_ROUTE_TEST
		mat ml$(2)
		ml$(1)="Account "&trim$(z$)&" already exists."
		ml$(2)="You must use a different account."
		fnmsgbox(mat ml$,resp$,'',16)
		goto NameScreen ! /r
	ACC_KC_VALID_ROUTE_TEST: ! r:
		if extra(1)<bkno1 or extra(1)>bkno2 then
			mat ml$(2)
			ml$(1)="You must have a valid route number!"
			ml$(2)="(from "&bkno1$&" to "&bkno2$&")"
			fnmsgbox(mat ml$,resp$,'',48)
			goto NameScreen
		end if

		fn_legacy2customer(mat customer$,mat customerN,z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$)
		rewrite #h_customer_1,using form$(h_customer_1),key=x$: mat customer$,mat customerN ! z$ changed to x$ on 1/2/19
		! rewrite #h_customer_1,using F_CUSTOMER_1: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$

		if z$<>holdz$ or extra(1)<>holdroute or extra(2)><holdseq then fixgrid=1
		open #h_ubtransvb=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr,Use,RecL=102,KPs=1,KLn=19",internal,outIn,keyed
		open #hTrans2=fnH:      "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed
		fnKeyChange(h_ubtransvb,'form pos 1,c 10',holdz$,z$) ! change # in history transactions
		close #h_ubtransvb:
		close #hTrans2:
		open #h_workorder=fnH: "Name=[Q]\UBmstr\WorkOrder.h[cno],KFName=[Q]\UBmstr\wkIndex.h[cno],Shr",internal,outIn,keyed
		fnKeyChange(h_workorder,'form pos 1,c 10',holdz$,z$)
		close #h_workorder:
		fn_accountKeyChange_meter(holdz$,z$)

		fnKeyChange(h_deposit2,'form pos 1,c 10',x$,z$)
		gosub BUD3
		noteFile$=fn_notedir$&"\"&trim$(holdz$)&".txt" ! old notes
		noteFileNew$=fn_notedir$&"\"&trim$(z$)&".txt" ! new notes
		if exists(noteFile$)<>0 then 
			! if exists(noteFileNew$) then fnFree(noteFileNew$)
			fnRename(noteFile$,noteFileNew$)
		end if
	goto AskAcct ! /r

	DeleteCustomer: ! r:
	if bal<>0 then
			mat ml$(3)
			ml$(1)="You can not delete a customer with a non-zero balance."
			ml$(2)="You must first issue a debit memo or credit memo"
			ml$(3)="to bring the balance to zero."
			fnmsgbox(mat ml$,resp$,'',16)
			goto NameScreen
		end if
		if fnConfirmDeleteHard('customer',"Account "&trim$(x$)) then
			gosub BUD4 ! delete budget info
			delete #h_customer_1,key=x$:
			gosub DEL_CASS
			delete #h_ubadrbil,key=x$: nokey ignore
			gosub DEL_HIST
			hact$=""
		end if
	goto AskAcct ! /r

	ALT_ADDRESS_SAVE: ! r: write or rewrite alternate billing address
		rewrite #h_ubadrbil,using FadrBil,key=z$: z$,mat ab$ nokey AAS_WRITE
		if trim$(ab$(1)&ab$(2)&ab$(3)&ab$(4))="" then
			do
				delete #h_ubadrbil,key=z$: ioerr AAS_DEL_ALL_END ! some how on conversion there can be several alternate addresses wtih same customer key (if delete any, delete them all)
			loop
			AAS_DEL_ALL_END: !
		end if
		goto AAS_FINIS
		AAS_WRITE: ! r:
		if trim$(ab$(1)&ab$(2)&ab$(3)&ab$(4))<>"" then
			write #h_ubadrbil,using FadrBil: z$,mat ab$
		end if
		goto AAS_FINIS ! /r
		AAS_FINIS: !
		release #h_customer_2: ioerr ignore
	return  ! /r
	BREAKDOWN_NOT_EQUAL: ! r:
		mat ml$(4)
		ml$(1)="Balance Breakdown does not equal Current Balance!"
		ml$(2)="Balance = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",bal))
		ml$(3)="Breakdown Balance = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",tgb))
		ml$(4)="Difference = "&ltrm$(cnvrt$("pic($$$,$$$.## cr)",bal-tgb))
		fnmsgbox(mat ml$,resp$,'',48)
	goto BILLING_INFO ! /r
	BUD2: ! r:
		if bud1=0 then goto NameScreen
		framelen=0
		for j=1 to 10
			if trim$(srvnam$(j))<>"" then framelen=framelen+1
		next j
		framelen=framelen+5
		mat ba=(0)
		mat badr=(0)
		br1=0 ! NO BUDGET RECORD
		read #h_budmstr,using F_BUDMSTR,key=z$: z$,mat ba,mat badr nokey L3040
		br1=1
		L3040: !
		fnTos
		fnFra(1,1,framelen-1,45,"Budget Billing Information","Enter budget amounts to activate budget billing",0)
		fnLbl(2,16,"Budget Amounts",20,2,3,1)
		fnLbl(3,1,"Date:",20,1,0,1)
		fnTxt(3,22,8,8,1,"1",0,"Date budget billing approved (mmddyy format)",1)
		budgetinfo$(1)=str$(ba(1))
		x=1 : lyne =3
		for j=1 to 10
			if trim$(srvnam$(j))<>"" then  ! have this service
				x+=1 : lyne+=1
				fnLbl(lyne,1,trim$(srvnam$(j))&":",20,1,0,1)
				fnTxt(lyne,22,10,0,1,"10",0,"Enter budget amounts where applicable.  All other services will be calculated as normal.",1)
				budgetinfo$(x)=str$(ba(j+1))
			end if
		next j
		lyne+=1 : fnLbl(lyne,1,"Net Bill:",20,1,0,1)
		fnTxt(lyne,22,10,10,1,"10",0,"Net would never have a value unless all items above were budgeted",1)
		budgetinfo$(x+=1)=str$(ba(12))
		fnCmdKey("&Next",1,1)
		fnCmdKey("Access &Transactions",8,0)
		fnCmdKey("&Delete",3,0)
		fnCmdKey("&Cancel",5,0,1)
		fnAcs(mat budgetinfo$,ckey) ! budget billing master record
		if ckey=5 then goto NameScreen
		if ckey=8 then goto TRANS_ROUTINE
		x=1 : ba(1)=val(budgetinfo$(1)) conv ignore
		for j=2 to 11
			if trim$(srvnam$(j-1))<>"" then
				x+=1 : ba(j)=val(budgetinfo$(x))
			end if
		next j
		x+=1: ba(12)=val(budgetinfo$(x))
		if br1=1 and ckey=3 then goto DEL_BUDGET_HISTORY ! if they choose to delete exiting record
		if br1=0 and ckey=3 then goto NameScreen ! if press delete without writing a record
		if br1=0 then goto L3330 ! create record if none exists
		rewrite #h_budmstr,using F_BUDMSTR: z$,mat ba,mat badr
		if ckey=2 then goto L3350
	goto NameScreen

	L3330: !
	if sum(ba)=0 then goto TRANS_ROUTINE
	write #h_budmstr,using F_BUDMSTR: z$,mat ba,mat badr
	L3350: !
	goto TRANS_ROUTINE
	! /r
	DEL_BUDGET_HISTORY: ! r:
		mat ml$(3)
		ml$(1)="You have chosen to delete all budget history"
		ml$(2)="for this customer."
		ml$(3)="Do you wish to continue?"
		fnmsgbox(mat ml$,resp$,'',35)
		if uprc$(resp$)="YES" then goto DBH_DEL else goto DBH_XIT
		DBH_DEL: !
		if br1=1 then delete #h_budmstr,key=z$: nokey ignore
		DBH_XIT: !
	goto NameScreen ! /r
	TRANS_ROUTINE: ! r:
		ta1=badr(1)
		if ta1=0 and sum(ba)>0 then
			ta1=lrec(h_budtrans)+1
			mat bt1=(0)
			nba=0
			write #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba
			badr(1)=ta1
			badr(2)=ta1
			rewrite #h_budmstr,using F_BUDMSTR,key=z$: z$,mat ba,mat badr
		end if
		do while ta1<>0
			read #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba noRec BUDTR_XIT
			! BUDTRANS: ! budget transactions
			mat budgetinfo$(28)
			fnTos
			fnFra(1,1,framelen+1,50,"Budget Billing Transactions","Actual billing compared to budget billing for any month billed",0)
			fnLbl(2,22,"Budget     Actual",20,2,2,1)
			fnLbl(3,1,"Date:",20,1,0,1)
			budgetinfo$(1)=str$(bt1(1,1))
			fnTxt(3,22,8,8,1,"1",0,'',1)
			budgetinfo$(2)=str$(bt1(1,2))
			fnTxt(3,34,8,8,1,"1",0,empty$,1)
			x=2 : lyne=3
			for j=1 to 10
				if trim$(srvnam$(j))<>"" then ! they have this service
					x+=2
					fnLbl(lyne+=1,1,trim$(srvnam$(j))&":",20,1,0,1)
					fnTxt(lyne,22,10,10,1,"10",0,empty$,1)
					budgetinfo$(x-1)=str$(bt1(j+1,1))
					budgetinfo$(x)=str$(bt1(j+1,2))
					fnTxt(lyne,34,10,10,1,"10",0,empty$,1)
				end if
			next j
			lyne+=1 : fnLbl(lyne,1,"Net Bill:",20,1,0,1)
			budgetinfo$(x+=1)=str$(bt1(12,1))
			fnTxt(lyne,22,10,10,1,"10",0,empty$,1)
			budgetinfo$(x+=1)=str$(bt1(12,2))
			fnTxt(lyne,34,10,10,1,"10",0,empty$,1)
			lyne+=1 : fnLbl(lyne,1,"Gross Bill:",20,1,0,1)
			budgetinfo$(x+=1)=str$(bt1(13,1))
			fnTxt(lyne,22,10,10,1,"10",0,empty$,1)
			budgetinfo$(x+=1)=str$(bt1(13,2))
			fnTxt(lyne,34,10,10,1,"10",0,empty$,1)
			lyne+=1 : fnLbl(lyne,1,"Date paid:",20,1,0,1)
			budgetinfo$(x+=1)=str$(bt1(14,1))
			fnTxt(lyne,22,8,8,1,"1",0,'',1)
			budgetinfo$(x+=1)=str$(bt1(14,2))
			fnTxt(lyne,34,8,8,1,"1",0,'',1)
			fnCmdSet(2)
			fnAcs(mat budgetinfo$,ckey) ! budget billing transactions
			if ckey=5 then goto NameScreen
			x=0
			for j=1 to 14
				if j<2 or j>11 or trim$(srvnam$(j-1))<>"" then
					x+=1
					bt1(j,1)=val(budgetinfo$(x*2-1))
					bt1(j,2)=val(budgetinfo$(x*2))
				end if
			next j
			rewrite #h_budtrans,using F_BUDTRANS,rec=ta1: x$,mat bt1,nba
			ta1=nba
		loop ! goto L3450
		BUDTR_XIT: !
	goto BUD2 ! /r
	BUD3: ! r:
		if bud1=0 then goto L3980 ! Account CHANGED
		read #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3',key=x$: x$,mat badr nokey L3980
		rewrite #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3': z$
		tadr=badr(1)
		do
			if tadr=0 then goto L3980
			read #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: x$,nba
			rewrite #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: z$ ! 11/15/00  ADDED USING
			tadr=nba
		loop
		L3980: !
	return ! /r
	BUD4: ! r:
	if bud1=0 then goto L4100 ! Account DELETED
	 read #h_budmstr,using 'form pos 1,c 10,pos 75,2*pd 3',key=x$: x$,mat badr nokey L4100
	 delete #h_budmstr:
	 tadr=badr(1)
	 do until tadr=0
		 read #h_budtrans,using 'form pos 1,c 10,pos 147,pd 3',rec=tadr: x$,nba
		 delete #h_budtrans,rec=tadr:
		 tadr=nba
	 loop
	 L4100: !
	return ! /r
	Cass1Open: ! r:
		open #h_cass1=fnH: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\CASS1IDX.h[cno],Shr",internal,outIn,keyed ioerr Cass1OpenFinis
		cassopen=1
		Cass1OpenFinis: !
	return  ! /r
	DEL_CASS: ! r:
		if cassopen then
			delete #h_cass1,key=x$: nokey ignore
		end if
	return  ! /r
	DEL_HIST: ! r: Delete History with old account
		open #h_ubtransvb=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed
		open #hTrans2=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed
		restore #h_ubtransvb,key>=x$&"         ": nokey DEL_HIST_FINIS
		do
			read #h_ubtransvb,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate eof DEL_HIST_FINIS
			if p$<>x$ then goto DEL_HIST_FINIS ! not same account
			if p$=x$ then delete #h_ubtransvb:
		loop
		DEL_HIST_FINIS: !
		close #h_ubtransvb:
		close #hTrans2:
	return  ! /r
	NameScreen: ! r: the main customer screen
		fnTos
		respc=0 : frac=0
		mylen=25 : mylen+2
		fnLbl(1,1,"Account:",15,1)               : fnTxt(1,17,10,10,1)                     : custInfo$(respc+=1)=trim$(z$)
		fnLbl(1,26,"Route:",8,1)                 : fncmbrt2(1,36,1)                        : custInfo$(respc+=1)=str$(extra(1))
		fnLbl(1,45,"Sequence:",11,1)             : fnTxt(1,58,7,7,1,"30")                  : custInfo$(respc+=1)=str$(extra(2))
		fnFra(3,1,4,48,"Customer Information")   : fraCustInfo=frac+=1
		fnLbl(1,1,"Name:",13,1,0,fraCustInfo)    : fnTxt(1,15,25,30,0,"",0,"",fraCustInfo) : custInfo$(respc+=1)=e$(2)
		fnLbl(2,1,"Address:",13,1,0,fraCustInfo) : fnTxt(2,15,25,30,0,"",0,"",fraCustInfo) : custInfo$(respc+=1)=e$(3)
		fnLbl(3,1,"Address:",13,1,0,fraCustInfo) : fnTxt(3,15,25,30,0,"",0,"",fraCustInfo) : custInfo$(respc+=1)=extra$(1)
		fnLbl(4,1,"City, St Zip:",13,1,0,fraCustInfo)
		fnComboF("CityStZip",4,15,30,"[Q]\Data\CityStZip.dat",1,30,0,0,"[Q]\Data\CityStZip.idx",0,0, " ",fraCustInfo,0)
		custInfo$(respc+=1)=e$(4)
		fnLbl(9,1,"Meter Address:",mylen,1)
		fnTxt(9,27,20,30) ! ,0,'',1)
		custInfo$(respc+=1)=e$(1)
		fnLbl(10,1,"Alpha Sort Name:",mylen,1)
		fnTxt(10,27,7)
		custInfo$(respc+=1)=alp$
		fnLbl(11,1,"Phone Number:",mylen,1)
		fnTxt(11,27,12)
		custInfo$(respc+=1)=extra$(2)
		fnLbl(12,1,"Cell Phone Number:",mylen,1)
		fnTxt(12,27,12,12)
		custInfo$(respc+=1)=extra$(8)
		fnLbl(13,1,"E-mail Address:",mylen,1)
		fnTxt(13,27,20,30,0,"",0)
		custInfo$(respc+=1)=extra$(9)
		fnLbl(14,1,"Current Balance:",mylen,1)
		fnTxt(14,27,12,12,1,"10",disableBalanceEdit)
		custInfo$(respc+=1)=str$(bal)
		fnLbl(15,1,"Last Billing Date:",mylen,1)
		fnTxt(15,27,8,8,1,"1")
		custInfo$(respc+=1)=str$(lastBillingDate)
		fnLbl(16,1,"Current Reading Date:",mylen,1)
		fnTxt(16,27,8,0,1,"1")
		custInfo$(respc+=1)=str$(extra(3))
		fnLbl(17,1,"Prior Reading Date:",mylen,1)
		fnTxt(17,27,8,0,1,"1")
		custInfo$(respc+=1)=str$(extra(4))
		fnLbl(18,1,"Final Billing Code:",mylen,1)
		code$(1)="0 - Active"
		! if env$('client')="Merriam Woods" then let code$(1)="0 - Active (And Shut-Off/Bill)"
		code$(2)="1 - Inactive / Final Billed"
		code$(3)="2 - Inactive / Deposit Refunded"
		code$(4)="3 - Active / but Do Not Bill"
		code$(5)="4 - Finaled / but Not Billed"
		respc+=1 ! update counter for at least one final billing code
		for j=1 to udim(code$)
			if extra(17)=val(code$(j)(1:1)) then custInfo$(respc)=code$(j)
		next j
		fncomboa("final_bill",18,27,mat code$,"",25)
		fnLbl(19,1,"Bulk Sort Code:",mylen,1)
		fnTxt(19,27,12)
		custInfo$(respc+=1)=extra$(6)
		fnLbl(20,1,"Last Estimation Date:",mylen,1)
		fnTxt(20,27,8,8,1,"1")
		custInfo$(respc+=1)=str$(extra(19))
		if env$('client')="Kincaid" then
			fnLbl(21,1,"1=Wand 2=Manual:",mylen,1)
		else if env$('client')="Findlay" then
			fnLbl(21,1,"Energy Assistance:",mylen,1)
		else
			fnLbl(21,1,"Test Circle Code:",mylen,1)
		end if
		if extra(22)<0 then extra(22)=0
		if extra(22)=0 or extra(22)=2 then do_not_use_alt_addr=1 else do_not_use_alt_addr=0
		!
		fnTxt(21,27,12)
		custInfo$(respc+=1)=extra$(7)
		fnFra(3,52,4,48,"Mailing Information","Mailing information is only necessary if different than the customer information",0)
		fnLbl(1,1,"Name:",13,1,0,2)
		fnTxt(1,15,20,30,0,"",do_not_use_alt_addr,"Mailing information is only necessary if different than the customer information",2)
		custInfo$(respc+=1)=ab$(1)
		fnLbl(2,1,"Address:",13,1,0,2)
		fnTxt(2,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
		custInfo$(respc+=1)=ab$(2)
		fnLbl(3,1,"Address:",13,1,0,2)
		fnTxt(3,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
		custInfo$(respc+=1)=ab$(3)
		fnLbl(4,1,"City, St Zip:",13,1,0,2)
		if do_not_use_alt_addr then
			fnTxt(4,15,20,30,0,"",do_not_use_alt_addr,empty$,2)
		else
			fnComboF("CityStZip",4,15,30,"[Q]\Data\CityStZip.dat",1,30,0,0,"[Q]\Data\CityStZip.idx",0,0, " ",2,0)
		end if
		custInfo$(respc+=1)=ab$(4)
		!
		fnLbl(23,1,"Social Security Number:",24,1)
		fnTxt(23,27,9,0,0,'30',0)
		custInfo$(resp_ssn=respc+=1)=str$(extra(20))
		fnLbl(24,1,"Work Phone Number:",24,1)
		fnTxt(24,27,12,0,0,'',0,"Enter the customers work number.")
		custInfo$(resp_phone_work=respc+=1)=extra$(10)
		fnLbl(25,1,"Business Phone Number:",24,1)
		fnTxt(25,27,12,0,0,'',0,"Enter the customers work number.")
		custInfo$(resp_phone_business=respc+=1)=extra$(11)
		fnButton(27,6,fn_warn_text$(z$,"The last note line that begins with 'warn:' will be displayed here."),23,"The last note line that begins with 'warn:' will be displayed here. Click to edit notes.",0,95)
		if do_not_use_alt_addr then
			fnButton(1,37,"Don't Use",51,"Will use regular address on bills, etc. ",2,10,2)
		else
			fnButton(1,37,"Use",50,"Will use alternate address on bills, etc.",2,8,2)
		end if
		nav_button_pos=76 : nav_button_width=25
		fnLbl(9,nav_button_pos,"Additional Information",nav_button_width,2) !,3)
		fnButton(10,nav_button_pos,"Servi&ces",20,"Service Code Information: Including rates codes, meter numbers, deposits, readings, usages, etc",0,nav_button_width)
		fnButton(11,nav_button_pos,"Current &Bill and Breakdown",21,"Charges, balance, balance breakdown, net and gross bill",0,nav_button_width)
		fnButton(12,nav_button_pos,"Bank &Draft Information",22,"Bank draft codes, routing numbers, bank numbers, etc",0,nav_button_width)
		fnButton(13,nav_button_pos,"&Notes",23,"Add notes or footnotes pertaining to this customer",0,nav_button_width)
		fnButton(14,nav_button_pos,"Deposit &History",25,"A record of all changes to customer deposit amounts",0,nav_button_width)
		fnButton(15,nav_button_pos,"&Transaction History",26,"Transactions for all charges, collections, penalties, memos, etc. that have been processed on the customer.",0,nav_button_width)
		fnButton(16,nav_button_pos,"Budget Billing In&formation",27,"Budget amounts and variances between budget and actual",0,nav_button_width)
		fnButton(17,nav_button_pos,"Work &Orders",28,"Print a work order on this customer",0,nav_button_width/2)
		fnButton(17,nav_button_pos+nav_button_width/2+1,"Print History",29,"Review descriptions of past work orders.",0,nav_button_width/2-1)
		fnCmdKey("&Save",1,1,0,"Saves all changes or new information")
		if ad1=0 then
			fnCmdKey("Delete",4,0,0,"Deletes this record")
		end if
		fnCmdKey("&Cancel",5,0,1,"Stops without recording any changes")
		fnAcs(mat custInfo$,ckey) ! CALL main screen
		if ckey=5 then
			release #h_customer_1: ioerr ignore
			release #h_ubadrbil: ioerr ignore
			if ad1=1 then
				goto ADD_CANCEL
			else
				goto AskAcct
			end if
		end if
		z$=lpad$(trim$(custInfo$(1)),10) : if ckey<>5 then rp_prev$(1)=z$ ! important in case of an account number change
		extra(1)=val(custInfo$(2))
		extra(2)=val(custInfo$(3))
		e$(2)=custInfo$(4)
		e$(3)=custInfo$(5)
		extra$(1)=custInfo$(6)
		e$(4)=custInfo$(7)
		e$(1)=custInfo$(8)
		if trim$(e$(1))="" then e$(1)=e$(3) ! set meter address same as customer address if left blank
		citykey$=rpad$(e$(4),30)
		read #h_citystzip,using 'form pos 1,c 30',key=citykey$,release: citystzip$ nokey L5430
		goto L5440
		L5430: !
		write #h_citystzip,using 'form pos 1,c 30': e$(4)
		L5440: !
		! r: get local variables from mat custInfo$ and mat ab$
		alp$=custInfo$(9)
		extra$(2)=custInfo$(10)
		extra$(8)=custInfo$(11)
		extra$(9)=custInfo$(12)
		bal=val(custInfo$(13))
		lastBillingDate=val(custInfo$(14))
		extra(3)=val(custInfo$(15))
		extra(4)=val(custInfo$(16))
		extra(17)=val(custInfo$(17)(1:1))
		extra$(6)=custInfo$(18)
		extra(19)=val(custInfo$(19))
		extra$(7)=custInfo$(20)
		ab$(1)=custInfo$(21)
		ab$(2)=custInfo$(22)
		ab$(3)=custInfo$(23)
		ab$(4)=custInfo$(24)
		extra(20)=val(custInfo$(resp_ssn))
		extra$(10)=custInfo$(resp_phone_work)
		extra$(11)=custInfo$(resp_phone_business)
		citykey$=rpad$(ab$(4),30)
		! /r
		! r: add city state zip to h_citystzip file (if it does not exist)
		read #h_citystzip,using 'form pos 1,c 30',key=citykey$,release: citystzip$ nokey L5520
		goto L5530
		L5520: !
		write #h_citystzip,using 'form pos 1,c 30': ab$(4)
		L5530: !
		! /r
		if ckey=4 and ad1=0 then goto DeleteCustomer ! delete account
		if extra(2)=0 then
			mat ml$(1)
			ml$(1)="Sequence number is required!"
			fnmsgbox(mat ml$,resp$,'',48)
			goto NameScreen
		else if extra(1)<bkno1 or extra(1)>bkno2  then
			mat ml$(2)
			ml$(1)="You must have a valid route number within the range of "&bkno1$&" and "&bkno2$&"!"
			ml$(2)="You can use Company > Configure to set the route number range.."
			fnmsgbox(mat ml$,resp$,'',48)
			goto NameScreen
		end if
		if sum(mat gb)<>bal then goto CHECK_BALANCE_BREAKDOWN
		if ckey=1 then
			goto REWRITE_RECORD
		else if ckey=2 then
			goto REWRITE_RECORD
		else if ckey=20 then
			if trim$(srvnam$(1))<>'' then
				ckey=21 : goto ServiceScreen
			else if trim$(srvnam$(2))<>'' then
				ckey=22 : goto ServiceScreen
			else if trim$(srvnam$(4))<>'' then
				ckey=24 : goto ServiceScreen
			else if trim$(srvnam$(5))<>'' then
				ckey=25 : goto ServiceScreen
			else if trim$(srvnam$(6))<>'' then
				ckey=26 : goto ServiceScreen
			else if trim$(srvnam$(7))<>'' then
				ckey=27 : goto ServiceScreen
			else if trim$(srvnam$(8))<>'' then
				ckey=28 : goto ServiceScreen
			else if trim$(srvnam$(9))<>'' then
				ckey=29 : goto ServiceScreen
			else if trim$(srvnam$(10))<>'' then
				ckey=30 : goto ServiceScreen
			else
				pr 'no services';bell : goto NameScreen
			end if
		else if ckey=21 then
			goto BILLING_INFO
		else if ckey=22 then
			goto BANK_DRAFT
		else if ckey=23 then
			fn_customerNotes(z$)
			goto NameScreen
		else if ckey=25 then
			goto DEPOSIT_HIST
		else if ckey=26 then
			goto TRANS_HIST
		else if ckey=27 then
			goto BUD2
		else if ckey=28 then
			fnWorkOrderAdd(holdz$)
			goto NameScreen
		else if ckey=29 then
			fnWorkOrderList(z$)
			goto NameScreen
		else if ckey=50 then
			extra(22)=2 : goto NameScreen
		else if ckey=51 then
			extra(22)=1 : goto NameScreen
		end if
	! /r (NameScreen)
	BILLING_INFO: ! r:
		fnTos
		fnLbl(1,14,"Billing Information",30,2,4)
		fnLbl(2,1,"Account:",10,1)
		fnTxt(2,12,10,0,1,'',1)
		bxnf$(1)=z$
		fnLbl(2,24,"Name:",5,1)
		fnTxt(2,31,25,30,0,'',1)
		bxnf$(2)=e$(2)
		fnFra(3,1,16,49,'')
		fnLbl(1,1,"Date of Charge:",14,1,0,1)
		fnTxt(1,16,8,0,0,'1',0,'',1)
		bxnf$(3)=str$(lastBillingDate)
		fnLbl(2,1,"Balance:",8,0,0,1)
		fnTxt(2,10,10, 0,1,'10',disableBalanceEdit,'',1)
		bxnf$(4)=str$(bal)
		if uprc$(escrow$)="Y" then
			fnLbl(2,21,"Escrow Balance:",15,1,0,1)
			fnTxt(2,38,9,0,1,'10',0,'',1)
			bxnf$(5)=str$(extra(23)) ! escrow balance
		end if
		fnLbl(3,19,"Current      Balance",25,2,2,1)
		fnLbl(4,20,"    Bill      Breakdown",25,2,2,1)
		lyne=4
		if uprc$(escrow$)="Y" then billinfo=5 else billinfo=4
		for j=1 to 10
			if rtrm$(srvnam$(j))<>"" then
				lyne+=1
				fnLbl(lyne,1,trim$(srvnam$(j))&":",16,1,0,1)
				fnTxt(lyne,19,10,0,1,'10',0,'',1)
				bxnf$(billinfo+=1)=str$(g(j))
				fnTxt(lyne,33,10,0,1,'10',0,'',1)
				bxnf$(billinfo+=1)=str$(gb(j))
			end if
		next j
		lyne+=1 : fnLbl(lyne,1,"Net Bill:",16,1,0,1)
		fnTxt(lyne,19,10,0,1,'10',0,'',1)
		bxnf$(billinfo+=1)=str$(g(11))
		lyne+=1 : fnLbl(lyne,1,"Gross Bill:",16,1,0,1)
		fnTxt(lyne,19,10,0,1,'10',0,'',1)
		bxnf$(billinfo+=1)=str$(g(12))
		fnCmdSet(2)
		fnAcs(mat bxnf$,ckey) ! billing information
		if ckey=5 then goto NameScreen
		lastBillingDate=val(bxnf$(3))
		bal=val(bxnf$(4))
		if uprc$(escrow$)="Y" then
			extra(23)=val(bxnf$(5))
			billinfo=5
		else
			billinfo=4
		end if
		for j=1 to 10
			if rtrm$(srvnam$(j))<>"" then
				billinfo=billinfo+1 : g(j)=val(bxnf$(billinfo))
				billinfo=billinfo+1 : gb(j)=val(bxnf$(billinfo))
			end if
		next j
		billinfo=billinfo+1 : g(11)=val(bxnf$(billinfo))
		billinfo=billinfo+1 : g(12)=val(bxnf$(billinfo))
	goto NameScreen ! /r
	BANK_DRAFT: ! r:
		fnTos
		fnLbl(1,9,"Bank Draft Information",40,2,4)
		fnLbl(2,1,"Account:",10,1)
		fnTxt(2,12,10,10,1,'',1)
		dri$(1)=z$
		fnLbl(2,24,"Name:",5,1)
		fnTxt(2,31,25,30,0,'',1)
		dri$(2)=e$(2)
		!
		fnLbl(4,3,"Bank Draft (Y/N):",18)
		fnTxt(4,20,1,0,0,"",0,"Use Y to specify the customer has requested a bank draft")
		if uprc$(df$)="1" or uprc$(df$)="Y" then
			dri$(3)="Y"
		else
			dri$(3)="N"
		end if
		fnLbl(5,1,"Routing Number:",18,1)
		fnTxt(5,20,9,0,0,'',0,"Routing number for customer's bank")
		dri$(4)=dr$
		fnLbl(6,1,"Account Code:",18,1,0)
		opt$(1)="27 = Checking"
		opt$(2)= "37 = Savings"
		fncomboa("bankdraft",6,20,mat opt$,empty$,13)
		if dc$="37" then
			dri$(5)="37 = Savings"
		else
			dri$(5)="27 = Checking"
		end if
		fnLbl(7,1,"Bank Account:",18,1,0,0)
		fnTxt(7,20,17,0,0,'',0,"Customer's bank account from which payments should be drafted.")
		dri$(6)=da$
		fnCmdSet(2)
		fnAcs(mat dri$,ckey) ! bank draft information
		if ckey=5 then goto NameScreen ! dont update information
		df$=dri$(3)
		dr$=dri$(4)
		dc$=dri$(5)(1:2)
		da$=dri$(6)
	goto NameScreen ! /r
	DEPOSIT_HIST: ! r:
		read #h_deposit2,using 'form pos 1,c 10,g 8,c 32,2*n 10.2',key=z$: k32$,dt1,dp$,dp1,dp2 nokey DEPOSIT_HIST_NONE
		fnTos
		fnLbl(1,16,"Deposit Change Information",40,2,4)
		fnLbl(2,1,"Account:",16,1)
		fnTxt(2,18,10,0,1,'',1)
		resp$(1)=lpad$(trim$(z$),10)
		fnLbl(3,1,"Name:",16,1)
		fnTxt(3,18,25,30,0,'',1)
		resp$(2)=e$(2)
		fnLbl(4,16,"Meter Address:",16,1)
		fnTxt(4,18,25,30,0,'',1)
		resp$(3)=e$(1)
		dim dh_ch$(4)*20
		mat dh_ch$(4)
		dh_ch$(1)="Date"
		dh_ch$(2)="Description"
		dh_ch$(3)="Before"
		dh_ch$(4)="After"
		dim dh_cm$(4)
		mat dh_cm$(4)
		dh_cm$(1)=("3") : dh_cm$(2)="" : dh_cm$(3)="10" : dh_cm$(4)="10"
		fnflexinit1("deposit",5,1,10,70,mat dh_ch$,mat dh_cm$,1)
		do while k32$=z$
			item$(1)=str$(dt1)
			item$(2)=dp$
			item$(3)=str$(dp1)
			item$(4)=str$(dp2)
			fnflexadd1(mat item$)
			read #h_deposit2,using 'Form POS 1,C 10,G 8,C 32,2*N 10.2,PD 3': k32$,dt1,dp$,dp1,dp2 eof L7160
		loop
		L7160: !
		fnCmdSet(2)
		ckey=fnAcs(mat resp$) ! CALL deposit change grd
		DEPOSIT_HIST_XIT: !
	goto NameScreen ! /r
	DEPOSIT_HIST_NONE: ! r:
		mat ml$(1)
		ml$(1)="There is no deposit history to display!"
		fnmsgbox(mat ml$,resp$,'',48)
	goto DEPOSIT_HIST_XIT ! /r
	TRANS_HIST: ! r:
		release #h_customer_1:
		fntransfile(jbact$,bal,mat gb)
		read #h_customer_1,key=jbact$:
	goto NameScreen ! /r
	AskAcct: ! r:
		release #h_customer_1: ioerr ignore
		ad1=0 ! add code - used to tell other parts of the program, that I am currently adding a customer record.
		if editOne then 
			if ckey=0 then
				x$=editOne$
				ckey=1
				goto EDIT_CUSTOMER
			else
				goto Finis
			end if
		end if
		ckey=fn_ask_account('ubfm',x$,h_customer_1, 'Edit',1)
		jbact$=hact$=x$
		if ckey=2 then ! add
			ad1=1
			goto ADD_RECORD
		else if ckey=1 then ! edit
			goto EDIT_CUSTOMER
		else if ckey=5 then ! Cancel
			goto Finis
		end if
		goto AskAcct
	! /r
	ADD_RECORD: ! r:
		fnTos
		fnLbl(1,5,"Adding Accounts",20,0,2)
		fnLbl(3,1,"Account:",15,1)
		fnTxt(3,17,10,0,1)
		resp$(1)=""
		fnCmdSet(11)
		ckey=fnAcs(mat resp$)
		if ckey=5 then goto AskAcct
		x$=lpad$(trim$(resp$(1)),10)
		if trim$(x$)="" then goto ADD_RECORD
		read #h_customer_1,using F_CUSTOMER_1,key=x$: z$ nokey ADD_CONTINUE
		mat ml$(2)
		ml$(1)="A record with this number already exists!"
		ml$(2)="Select a different account."
		fnmsgbox(mat ml$,resp$,'',48)
	goto ADD_RECORD
		!
	ADD_CANCEL: !
		delete #h_customer_1,key=x$: ioerr ignore
	goto ADD_RECORD
	! /r
	ADD_CONTINUE: ! r:
		z$=x$ : mat e$=("") : e$(4)=newe4$
		mat f$=("") : mat a=(0) : mat b=(0) : mat c=(0) : mat d=(0)
		mat g=(0) : mat adr=(0) : mat gb=(0) : bal=lastBillingDate=0
		alp$="" : df$=dr$=dc$=da$="" : mat extra=(0) : mat extra$=("")
		ad1=1 : holdz$=z$
		fn_apply_default_rates(mat extra, mat a)

		fn_legacy2customer(mat customer$,mat customerN,z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$,mat extra,mat extra$)
		write #h_customer_1,using form$(h_customer_1): mat customer$,mat customerN
		! 		write #h_customer_1,using F_CUSTOMER_1: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,lastBillingDate,mat g,mat adr,alp$,f$(2),f$(3),bra_legacy,mat gb,df$,dr$,dc$,da$

		fixgrid=1
		read #h_customer_1,using 'Form POS 1,C 10',key=z$: z$ ! this line should lock the record and set the SAME paramater for use in add_cancel
	goto EDIT_LOADED_CUSTOMER ! /r

	Finis: ! r: close files and leave
	! close #2: ioerr ignore
	fn_close_file(h_customer_2)
	fnCloseFile(hLocation,'U4 Meter Location')
	fnCloseFile(h_customer_1,'UB Customer')
	setup_MeterLocation=0
	fn_close_file(h_ubadrbil)
	fn_close_file(h_citystzip)
	! fn_close_file(h_deposit1)
	fn_close_file(h_deposit2)
	fn_close_file(h_cass1)
	fn_close_file(h_budtrans)
	fn_close_file(h_budmstr)
	fn_close_file(h_budtrans)
	! fn_close_file(h_customer_1)
	! fn_close_file(h_customer_2)
	! fn_close_file(h_customer_3)
	! fn_close_file(h_customer_4)
	! fn_close_file(h_customer_5) ! /r
fnend
def library fnDepositChangeLog(z$*10,odp,ndp,chgDateMmDdYy,comment$*32)
	if ~setup then fn_setup
	if ~setup_depositChange then let fn_setup_depositChange
	fnDepositChangeLog=fn_depositChangeLog(z$,odp,ndp,chgDateMmDdYy,comment$)
fnend
def fn_setup_depositChange
	open #h_deposit2=fnH: 'Name=[Q]\UBmstr\Deposit2.h[cno],KFName=[Q]\UBmstr\Deposit2Index.h[cno],Shr,Use,RecL=73,KPs=1,KLn=10',internal,outIn,keyed ! "Name=[Q]\UBmstr\Deposit2.h[cno],Shr,Use,RecL=73",internal,outIn,relative  ! was 42
fnend
def fn_depositChangeLog(z$,odp,ndp,chgDateMmDdYy,comment$*32)
	! requires local:  #h_deposit2
	rk$=z$
	if rk$<>"" then
		chgDateCcyyMmDd=date(days(chgDateMmDdYy,'mmddyy'),'ccyymmdd')
		write #h_deposit2,using 'form pos 1,c 10,g 8,c 32,2*n 10.2,pd 3',rec=r32: z$,chgDateCcyyMmDd,comment$,odp,ndp,0
	end if
fnend
def fn_close_file(cf_handle)
	close #cf_handle: ioerr ignore
fnend
ServiceScreen: ! r:
	! if ckey=5 then goto NameScreen
	mat rateInfo$=("")
	if ckey>20 and ckey<=30 then
		! on ckey-20 goto SERVICE1,SERVICE2,SERVICE3,SERVICE4,SERVICE5,SERVICE6,SERVICE7,SERVICE8,SERVICE9,SERVICE10
		! r: set service_code and ratecode
		if ckey=21 then
			service_code=1 : ratecode=a(1)
		else if ckey=22 then
			service_code=2 : ratecode=a(2)
		else if ckey=23 then
			service_code=3 : ratecode=a(3)
		else if ckey=24 then
			service_code=4 : ratecode=a(4)
		else if ckey=25 then
			service_code=5 : ratecode=a(5)
		else if ckey=26 then
			service_code=6 : ratecode=extra(11)
		else if ckey=27 then
			service_code=7 : ratecode=extra(12)
		else if ckey=28 then
			service_code=8 : ratecode=extra(13)
		else if ckey=29 then
			service_code=9 : ratecode=a(6)
		else if ckey=30 then
			service_code=10 : ratecode=a(7)
		end if
		! /r
		! r: ServicePart1
		respc=gFkeyMeterLocationSelect=srvLine=0
		srvCol1len=20 : srvCol2pos=22
		gLocationFirstRespc=0 : gLocationKey$=''
		fnTos
		fnLbl(srvLine+=1,19,srvnam$(service_code),20,2,4)
		fnLbl(srvLine+=1, 1,"Account:"  ,10,1) : fnTxt(srvLine,12,10, 0,1,'',1) : rateInfo$(respc+=1)=z$    ! 1
		fnLbl(srvLine    ,24,"Name:"    , 5,1) : fnTxt(srvLine,31,25,30,0,'',1) : rateInfo$(respc+=1)=e$(2) ! 2
		srvLine+=1
		fnLbl(srvLine+=1,1,"Rate Code:",srvCol1len,1)
		fn_getRateCodeOptions(service_code,ratecode,mat rates$)
		fncomboa("ubfm-rates",srvLine,srvCol2pos,mat rates$,"",30)
		respc+=1                                                                                 ! 3
		if service_code=1 then
			fnLbl(srvLine+=1,1,"MeterNo:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=f$(1)       ! 4
			fnLbl(srvLine+=1,1,'Serial No:'      ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=extra$(3)   ! 5
			fnLbl(srvLine+=1,1,"Deposit:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'10') : rateInfo$(respc+=1)=str$(b(8))  ! 6
			fnLbl(srvLine+=1,1,"Deposit Date:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'1' ) : rateInfo$(respc+=1)=str$(c(1))  ! 7
			fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,'10') : rateInfo$(respc+=1)=str$(b(1))  ! 8
			fnLbl(srvLine+=1,1,"Current Reading:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(1))  ! 9
			fnLbl(srvLine+=1,1,"Prior Reading:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(2))  ! 10
			fnLbl(srvLine+=1,1,"Usage - Current:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(3))  ! 11
			fnLbl(srvLine+=1,1,"Usage - YTD:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,11,0,1,'20') : rateInfo$(respc+=1)=str$(d(4))  ! 12
			fnLbl(srvLine+=1,1,"Unit Count:"     ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 5,0,1,'20') : rateInfo$(respc+=1)=str$(d(13)) ! 13
		else if service_code=2 then
			fnLbl(srvLine+=1,1,"Deposit:"         ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'10') : rateInfo$(respc+=1)=str$(b(9))
			fnLbl(srvLine+=1,1,"Deposit Date:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'1' ) : rateInfo$(respc+=1)=str$(c(2))
			fnLbl(srvLine+=1,1,"Standard Charge:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,'10') : rateInfo$(respc+=1)=str$(b(2))
			fnLbl(srvLine+=1,1,"Sewer Reduction:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(extra(5))
			fnLbl(srvLine+=1,1,"Sewer Average:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(extra(18))
			fnLbl(srvLine+=1,1,"Units Per Meter:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 3,0,1,'20') : rateInfo$(respc+=1)=str$(extra(14))
		else if service_code=3 then
			if srv$(service_code)="EL" or srv$(service_code)="LM" then
				fnLbl(srvLine+=1,1,"Meter Number:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=f$(2)
				fnLbl(srvLine+=1,1,'Serial Number:'  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=extra$(4)
				fnLbl(srvLine+=1,1,"Deposit:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'10') : rateInfo$(respc+=1)=str$(b(10))
				fnLbl(srvLine+=1,1,"Deposit Date:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,'1' ) : rateInfo$(respc+=1)=str$(c(3))
				fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,'10') : rateInfo$(respc+=1)=str$(b(3))
				fnLbl(srvLine+=1,1,"Current Reading:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(d(5))
				fnLbl(srvLine+=1,1,"Prior Reading:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(d(6))
				fnLbl(srvLine+=1,1,"Usage - Current:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,'20') : rateInfo$(respc+=1)=str$(d(7))
				fnLbl(srvLine+=1,1,"Usage - YTD:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,"20") : rateInfo$(respc+=1)=str$(d(8))
				if srv$(service_code)="EL" then  ! .    ! skip rest of information on lawn meters
					fnLbl(srvLine+=1,1,"Demand Reading:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"20") : rateInfo$(respc+=1)=str$(d(15))
					fnLbl(srvLine+=1,1,"Demand Multiplier:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"33") : rateInfo$(respc+=1)=str$(d(14)*.001)
					fnLbl(srvLine+=1,1,"Average Usage:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"20") : rateInfo$(respc+=1)=str$(extra(9))
					fnLbl(srvLine+=1,1,"Usage Multiplier:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"33") : rateInfo$(respc+=1)=str$(extra(8)*.001)
					fnLbl(srvLine+=1,1,"Security Light $:" ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,9,0,1,"10") : rateInfo$(respc+=1)=str$(extra(6))
					fnLbl(srvLine+=1,1,"Num of Lights:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,3,0,1,"20") : rateInfo$(respc+=1)=str$(extra(7))
					fnLbl(srvLine+=1,1,"Units per Meter:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,3,0,1,"20") : rateInfo$(respc+=1)=str$(extra(15))
				end if
			end if
		else if service_code=4 then
			if srv$(service_code)="GA" then ! show all of gas information (unless field is used for something other than gas)
				fnLbl(srvLine+=1,1,"Meter Number:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=f$(3)
				fnLbl(srvLine+=1,1,"Serial Number:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12          ) : rateInfo$(respc+=1)=extra$(5)
				fnLbl(srvLine+=1,1,"Deposit:"        ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,"10") : rateInfo$(respc+=1)=str$(b(11))
				fnLbl(srvLine+=1,1,"Deposit Date:"   ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 8,0,1,"1" ) : rateInfo$(respc+=1)=str$(c(4))
				fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10") : rateInfo$(respc+=1)=str$(b(4))
				fnLbl(srvLine+=1,1,"Current Reading:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(9))
				fnLbl(srvLine+=1,1,"Prior Reading:"  ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(10))
				fnLbl(srvLine+=1,1,"Usage - Current:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(11))
				fnLbl(srvLine+=1,1,"Usage - YTD:"    ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos,12,0,1,"20") : rateInfo$(respc+=1)=str$(d(12))
				fnLbl(srvLine+=1,1,"Multiplier:"     ,srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 9,0,1,"33") : rateInfo$(respc+=1)=str$(extra(10)*.001)
				fnLbl(srvLine+=1,1,"Number of Units:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos, 3,0,1,"20") : rateInfo$(respc+=1)=str$(extra(16))
			end if
		else if service_code=5 then
			fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10",0) : rateInfo$(respc+=1)=str$(b(5))
		else if service_code=6 then
			fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10") : rateInfo$(respc+=1)=str$(b(6))
		else if service_code=7 then
				! (place holder for future developemnt)
		else if service_code=8 then
			fnLbl(srvLine+=1,1,"Standard Charge:",srvCol1len,1) : fnTxt(srvLine,srvCol2pos,10,0,1,"10") : rateInfo$(respc+=1)=str$(b(7))
		else if service_code=9 then
				! (place holder for future developemnt)
		else if service_code=10 then
				! (place holder for future developemnt)
		end if
		! /r
		! r: SERVICE_BUTTONS
		lyne=5
		for j=1 to 10
			if trim$(srv$(j))<>"" and trim$(srvnam$(j))<>"" then
				lyne+=1 : funkeyval=j+20
				fnButtonOrDisabled(trim$(srv$(j))<>searchcode$,lyne,45,srvnam$(j),funkeyval,"Allows you to assign "&lwrc$(trim$(srvnam$(j)))&" codes for this customer (service "&str$(j)&')',20)
			end if
		next j
		fn_ScrAddServiceMeterInfo(srvLine,respc+=1,mat rateInfo$,srv$(service_code),service_code)
		fnCmdKey("&Save",1,1,1) ! fnCmdSet(2)  <---  remove the cancel button
		fnAcs(mat rateInfo$,ckey) ! rate screen 1
		! /r
		if ckey<>5 then ! r: get local values out of mat rateInfo$ and Save the record
			! r: receive ratecode back
			ratecode=val(rateInfo$(3)(1:(pos(rateInfo$(3),"=",1)-1)))
			if service_code=1 then !  r: 1ST SERVICE  -  Water
				a(1)     =ratecode
				f$(1)    =rateInfo$(4)(1:12)
				extra$(3)=rateInfo$(5)(1:12)
				b(8)     =val(rateInfo$(6))
				c(1)     =val(rateInfo$(7))
				b(1)     =val(rateInfo$(8))
				d(1)     =val(rateInfo$(9))
				d(2)     =val(rateInfo$(10))
				d(3)     =val(rateInfo$(11))
				d(4)     =val(rateInfo$(12))
				d(13)    =val(rateInfo$(13))
				 ! /r
			else if service_code=2 then ! r: 2nd SERVICE  -  Sewer
				a(2)     =ratecode
				b(9)     =val(rateInfo$(4))
				c(2)     =val(rateInfo$(5))
				b(2)     =val(rateInfo$(6))
				extra(5) =val(rateInfo$(7))
				extra(18)=val(rateInfo$(8))
				extra(14)=val(rateInfo$(9))
				 ! /r
			else if service_code=3 then ! r: 3RD SERVICE  -  Electric
				a(3)     =ratecode
				f$(2)    =rateInfo$(4)(1:12)
				extra$(4)=rateInfo$(5)(1:12)
				b(10)    =val(rateInfo$(6))
				c(3)     =val(rateInfo$(7))
				b(3)     =val(rateInfo$(8))
				d(5)     =val(rateInfo$(9))
				d(6)     =val(rateInfo$(10))
				d(7)     =val(rateInfo$(11))
				d(8)     =val(rateInfo$(12))
				d(15)    =val(rateInfo$(13))
				d(14)    =val(rateInfo$(14))*1000
				extra(9) =val(rateInfo$(15))
				extra(8) =val(rateInfo$(16))*1000
				extra(6) =val(rateInfo$(17))
				extra(7) =val(rateInfo$(18))
				extra(15)=val(rateInfo$(19))
				! /r
			else if service_code=4 then ! r: 4th SERVICE  -  Gas
				a(4)     =ratecode
				f$(3)    =rateInfo$(4)(1:12)
				extra$(5)=rateInfo$(5)(1:12)
				b(11)    =val(rateInfo$(6))
				c(4)     =val(rateInfo$(7))
				b(4)     =val(rateInfo$(8))
				d(9)     =val(rateInfo$(9))
				d(10)    =val(rateInfo$(10))
				d(11)    =val(rateInfo$(11))
				d(12)    =val(rateInfo$(12))
				extra(10)=val(rateInfo$(13))*1000
				extra(16)=val(rateInfo$(14))
				 ! /r
			else if service_code=5 then ! r: 5th SERVICE
				a(5)     =ratecode
				b(5)     =val(rateInfo$(4))
			! /r
			else if service_code=6 then ! r: 6th SERVICE
				extra(11)=ratecode
				b(6)     =val(rateInfo$(4))
				! /r
			else if service_code=7 then ! r:  7th SERVICE
				extra(12)=ratecode
				! b(7)     =VAL(rateInfo$(4))
				! /r
			else if service_code=8 then ! r:  8th SERVICE
				extra(13)=ratecode
				b(7)     =val(rateInfo$(4))
				! /r
			else if service_code=9 then ! r: 9th service (sales tax)
				a(6)     =ratecode
				! /r
			else if service_code=10 then ! r: 10th SERVICE   (penalty)
				a(7)     =ratecode
				! /r
			end if
			fn_ScrAddSrvMeterLocSave
		end if ! /r
		if ckey=5 then
			goto ServiceScreen
		else if gFkeyMeterLocationSelect<>0 and ckey=gFkeyMeterLocationSelect then
			fnCustomerMeterLocationSelect(x$,srv$(service_code))
			ckey=service_code+20
			goto ServiceScreen
		else if ckey=>21 and ckey<=30 then
			goto ServiceScreen
		end if
		! /r
	end if
goto NameScreen ! /r
 dim gLocationKey$*128
 dim gLocationFirstRespc
def fn_ScrAddServiceMeterInfo(&srvLine,&respc1,mat rateInfo$,serviceCode$*2,service_code)
	if fn_serviceIsMetered(service_code) then
		if ~setup_MeterLocation then
			setup_MeterLocation=1
			hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,4)
		end if
		srvLine+=1
		fnLbl(srvLine+=1,19,trim$(srvnam$(service_code))&' Meter Location',20,2,4)
		fncmdkey('Select Location',gFkeyMeterLocationSelect:=2101)
		srvLine+=1
		mat location$=('')
		mat locationN=(0)
		location$(loc_activeCustomer)=trim$(x$)
		location$(loc_serviceId)=serviceCode$
		gLocationKey$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN,4)
		gLocationFirstRespc=respc1
		read #hLocation,using form$(hLocation),key=gLocationKey$,release: mat location$,mat locationN nokey SasNoLocation
		fnlbl(srvLine+=1,1,'Location ID:'       ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,11, 0,0,'',1) : rateInfo$(respc1   )=str$(locationN(loc_locationID     ))
		fnlbl(srvLine+=1,1,'Meter Address:'     ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,30, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_name           )
		fnlbl(srvLine+=1,1,'Longitude:'         ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,17, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_longitude     )
		fnlbl(srvLine+=1,1,'Latitude:'          ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,17, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_latitude       )
		fnlbl(srvLine+=1,1,'Meter Number:'      ,srvCol1len,1) : fntxt(srvLine,srvCol2pos,12, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_meterNumber   )
		fnlbl(srvLine+=1,1,'Transmitter Number:',srvCol1len,1) : fntxt(srvLine,srvCol2pos,20, 0,0,''  ) : rateInfo$(respc1+=1)=location$(loc_transmitter    )
		fnlbl(srvLine+=1,1,'Meter Type:'        ,srvCol1len,1) : fncombof('',srvLine,srvCol2pos,0,'[Q]\UBmstr\MeterType.h[cno]',1,5,6,40,'[Q]\UBmstr\MeterTypeIdx.h[cno]',1) : rateInfo$(respc1+=1)=location$(loc_meterType     )
		goto SasFinis
	end if
	SasNoLocation: !
		gLocationKey$=''
		fnLbl(srvLine+=1,1,'No Meter Location attached.')
	goto SasFinis
	SasFinis: !
	fnLbl(29,1,'') ! force all service windows to be same size
fnend
def fn_ScrAddSrvMeterLocSave
	if gLocationKey$<>'' then
		sasrRespc=gLocationFirstRespc
		locationN(loc_locationID     )=val(rateInfo$(sasrRespc))
		location$(loc_name           )=rateInfo$(sasrRespc+=1)
		location$(loc_longitude      )=rateInfo$(sasrRespc+=1)
		location$(loc_latitude       )=rateInfo$(sasrRespc+=1)
		location$(loc_meterNumber    )=rateInfo$(sasrRespc+=1)
		location$(loc_transmitter    )=rateInfo$(sasrRespc+=1)
		location$(loc_meterType      )=rateInfo$(sasrRespc+=1)(1:5)
		rewrite #hLocation,using form$(hLocation),key=gLocationKey$: mat location$,mat locationN
		gLocationKey$=''
	end if
fnend
def fn_serviceIsMetered(serviceNumber)=max(0,srch(mat serviceCodeMetered$,srv$(serviceNumber))) ! /r
TGB_SET: ! r:
	tgb=0
	for j=1 to 10 : tgb=tgb+gb(j) : next j
return ! /r
REMOVE_INCORRECT_ALLOCATIONS: ! r:
	gosub TGB_SET
	for j=1 to 10
		if trim$(srvnam$(j))='' and gb(j)<>0 then
			mat ml$(2)
			ml$(1)="A lost allocation of "&str$(gb(j))&" was found!"
			ml$(2)="This amount has been moved into "&srvnam$(first_service)
			fnmsgbox(mat ml$,resp$,'',48)
			gb(first_service)+=gb(j)
			gb(j)=0
		end if  ! trim$(srvnam$(j))='' and gb(j)<>0
	next j
return  ! /r
def fn_record_previous_update(rp_account$*10)
	if ~record_previous_load then let fn_record_previous_load(prev_list_id$)
	!   pr 'before' : for x=1 to 10 : pr x;'.';rp_prev$(x) : next x
	rp_which=srch(mat rp_prev$,rp_account$)
	if rp_which>0 then ! remove the old entry
		for rp_item=rp_which to 2 step -1
			rp_item_from=rp_item
			rp_item_to=rp_item-1
			!     pr 'rp_prev$(';rp_item_from;') inherits from ';rp_item_to;'(';rp_prev$(rp_item_to);')'
			rp_prev$(rp_item_from)=rp_prev$(rp_item_to)
		next rp_item
	else
		!
		for rp_item=udim(mat rp_prev$) to 2 step -1
			rp_item_from=rp_item
			rp_item_to=rp_item-1
			!     pr 'rp_prev$(';rp_item_from;') inherits from ';rp_item_to;'(';rp_prev$(rp_item_to);')'
			rp_prev$(rp_item_from)=rp_prev$(rp_item_to)
		next rp_item
	end if
	rp_prev$(1)=rp_account$ ! &' '&fn_customer_name$(rp_account$)
	!   pr 'after' : for x=1 to 10 : pr x;'.';rp_prev$(x) : next x
fnend
def library fnCustomerNotes(z$)
	if ~setup then fn_setup
	fnCustomerNotes=fn_customerNotes(z$)
fnend
def fn_customerNotes(z$)
		noteFile$=fn_notedir$&"\"&trim$(z$)&".txt"
		if exists(noteFile$)=0 then
			open #hTmp=fnH: "Name="&noteFile$&",Replace",display,output
			close #hTmp:
		end if  ! exists(noteFile$)=0
		fnEditFile('text',noteFile$) ! execute 'SY -w '&atlantis$&' "'&os_filename$(noteFile$)&'" -n' ! ioerr [itself]
fnend
def fn_record_previous_save
	for rp_item=1 to udim(mat rp_prev$)
		fncreg_write(prev_list_id$&'.previous.'&str$(rp_item),rp_prev$(rp_item))
	next rp_item
fnend
def fn_record_previous_load(prev_list_id$)
	if ~record_previous_load then
		record_previous_load=1
		dim rp_prev$(10)*10 ! 1 is the most recent, 10 is the oldest
		dim rp_tmp$*256
		for rp_item=1 to udim(mat rp_prev$)
			fncreg_read(prev_list_id$&'.previous.'&str$(rp_item),rp_tmp$) : rp_prev$(rp_item)=rp_tmp$(1:10)
		next rp_item
	end if
fnend
def fn_record_previous_clear
	mat rp_prev$=('')
fnend

def library fnNoteDir$*256
	if ~setup then fn_setup
	fnNoteDir$=fn_notedir$
fnend
def fn_notedir$*256
	if notedir_setup<>val(env$('cno')) then
		dim notedir$*256
		notedir$="[Q]\UBmstr\notes.h[cno]"
		fnmakesurepathexists(notedir$&'\')
	end if
	fn_notedir$=notedir$
fnend
def fn_customer_name$*30(cn_account$*10)
	dim customer_name_return$*30
	customer_name_return$=''
	read #h_customer_1,using 'form pos 41,C 30',key=cn_account$,release: customer_name_return$ ioerr ignore
	fn_customer_name$=rtrm$(customer_name_return$)
fnend
def fn_accountKeyChange_meter(key_from$*10,key_to$*10)
	hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,3)
	fnKeyChange(hLocation,'form pos 42,c 10',key_from$,key_to$)
	fnclosefile(hLocation,'U4 Meter Location') : setup_MeterLocation=0
fnend
def fn_customer_grid(cg_line,cg_pos)
	dim cg_item$(12)*30,cg_ch$(12),cg_cm$(12)
	open #cg_file_num=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed ioerr ERTN
	restore #file_num:
	mat cg_ch$(12) : mat cg_cm$(12) : mat cg_cm$(12)
	cg_ch$(1)="Account"
	cg_ch$(2)="Status"
	cg_ch$(3)="Name"
	cg_ch$(4)="Address"
	cg_ch$(5)="Address"
	cg_ch$(6)="City, ST Zip"
	cg_ch$(7)="Meter Address"
	cg_ch$(8)="Route"
	cg_ch$(9)="Sequence"
	cg_ch$(10)="Phone"
	cg_ch$(11)="Meter"
	cg_ch$(12)="Alpha"
	mat cg_cm$=("80") : cg_cm$(2)="61" : cg_cm$(8)="61": cg_cm$(9)="61"
	fnflexinit1('Cust2',cg_line,cg_pos,10,72,mat cg_ch$,mat cg_cm$,1)
	do
		CG_READ_FILE: !
		read #cg_file_num,using 'Form POS 1,C 10,pos 1821,c 1,POS 41,C 30,C 30,POS 1864,C 30,POS 101,C 30,POS 11,C 30,POS 1741,C 2,C 7,POS 1894,C 12,POS 131,C 12,pos 354, c 7': mat cg_item$ eof CG_EO_CUSTOMER ioerr CG_ERR_READ
		fnflexadd1(mat cg_item$)
	loop
	!
	CG_ERR_READ: !
	if err<>61 then goto ERTN
	! pr 'Record locked during Customer_Search flexgrid creation - skipped'
	read #file_num,release:
	goto CG_READ_FILE
	!
	CG_EO_CUSTOMER: !
	close #cg_file_num:
fnend
def fn_key_tweak(&kt_key$,h_customer_1)
	! function tweaks the customer account key to make it valid if it is not.  if it succeeds it returns a 1, otherwise it returns a 0
	kt_return=kt_key_addition=0
	kt_read_account$=''
	kt_key_origional$=kt_key$
	do
		kt_key$=rpad$(trim$(kt_key$),10)
		!   pr 'trying Rpadded account key: "'&kt_key$&'"'
		read #h_customer_1,using 'Form POS 1,C 10',key>=kt_key$,release: kt_read_account$ nokey KT_TRY_LPADDED
		if kt_key$=kt_read_account$ then kt_return=1 : goto KT_FINIS
		KT_TRY_LPADDED: !
		kt_key$=lpad$(trim$(kt_key$),10)
		!   pr 'trying Lpadded account key: "'&kt_key$&'"'
		read #h_customer_1,using 'Form POS 1,C 10',key>=kt_key$,release: kt_read_account$ nokey KT_TWEAK
		if kt_key$=kt_read_account$ then kt_return=1 : goto KT_FINIS
		!
		KT_TWEAK: !
		kt_key$=trim$(kt_key$)
		if pos(kt_key$,'.')<=0 and len(kt_key$)<=7 then
			kt_key$=kt_key$&'.00'
		else if kt_key$(len(kt_key$):len(kt_key$))='.' and len(kt_key$)<=6 then
			kt_key$=kt_key$&'00'
		else if kt_key$(len(kt_key$)-1:len(kt_key$)-1)='.' and len(kt_key$)<=9 then ! it ends with like a .? so try a .?0
			kt_key$=kt_key$&'0'
		else if kt_key$(len(kt_key$)-2:len(kt_key$)-2)='.' then
			kt_key_addition=val(kt_key$(len(kt_key$)-1:len(kt_key$))) conv KT_NOT_A_NUMBER_AFTER_DOT
			kt_key_addition+=1
			if kt_key_addition<=98 then
				kt_key$(len(kt_key$)-2:len(kt_key$))='.'&cnvrt$('pic(##)',kt_key_addition)
			else if kt_key_addition>=99 then
				 !       pr 'could not find the account - went all the way to .'&cnvrt$('pic(##)',kt_key_addition)
				 !       pause
				 goto KT_FINIS
			end if
		else
			!       pr 'found nothing to change.' : pause
			goto KT_FINIS
		end if
		!   pr 'about to try: '&kt_key$ : pause
	loop
	KT_NOT_A_NUMBER_AFTER_DOT: !
	!   pr 'could not find the account - there was a dot but it was not a number after it:  '&kt_key$(len(kt_key$)-2:len(kt_key$)) : pause
	goto KT_FINIS
	KT_FINIS: !
	! pr 'fn_key_tweak complete.  returning ';kt_return : pause
	if ~kt_return then kt_key$=kt_key_origional$
	fn_key_tweak=kt_return
fnend

def library fnask_account(prev_list_id$,&x$,h_customer_1; select_button_text$,aas_button_enable_add)
	if ~setup then fn_setup
	fnask_account=fn_ask_account(prev_list_id$,x$,h_customer_1, select_button_text$,aas_button_enable_add)
fnend
def fn_ask_account(prev_list_id$,&x$,h_customer_1; select_button_text$,aas_button_enable_add)
!  function returns:
!    2 = (if AAS_button_enable_add) for Add
!    1 = Edit or Select
!    5 = Exit/Cancel Selected
!  X$ = account number selected.
	if ~aas_setup then
		aas_setup=1
		asm_combo=1
		asm_text=2
		asm_grid=3
		asm_locationId=4
		fnureg_read('ubfm.account_selection_method',account_selection_method$)
		account_selection_method=val(account_selection_method$) conv ignore
		if account_selection_method=0 then account_selection_method=asm_combo
		fn_record_previous_load(prev_list_id$)
	end if
	if select_button_text$='' then select_button_text$='Next'
	col1_width=17
	col2_pos=col1_width+2
	do
	AAS_TOP: !
		fnTos
		respc=0
		AskAcct_line=0
		if rp_prev$(1)<>'' then
			if rp_prev$(1)<>'' then
				fnButton(AskAcct_line+=1,col2_pos,'Clear Recent',1000,'Clear Recent Accounts list')
			end if
			fnLbl(AskAcct_line+=1,1,"Recent:",col1_width,1) : AskAcct_line=AskAcct_line-1
			for rp_item=udim(mat rp_prev$) to 1, step -1
				if rp_prev$(rp_item)<>'' then
					fnButton(AskAcct_line+=1,col2_pos,rp_prev$(rp_item)&' '&fn_customer_name$(rp_prev$(rp_item)),1000+rp_item, 'click to select this previously accessed account',1,43)
				end if
			next rp_item
			AskAcct_line+=2
		end if
		fnLbl(AskAcct_line+=1,1,"Selection Method:",col1_width,1)
		btn_width=10
		fnButtonOrDisabled(account_selection_method<>asm_combo,AskAcct_line,col2_pos,'Combo',2001,'',btn_width)
		fnButtonOrDisabled(account_selection_method<>asm_grid,AskAcct_line,col2_pos+((btn_width+1)*1),'Grid',2002,'',btn_width)
		fnButtonOrDisabled(account_selection_method<>asm_text,AskAcct_line,col2_pos+((btn_width+1)*2),'Text',2003,'',btn_width)
		fnButtonOrDisabled(account_selection_method<>asm_locationId,AskAcct_line,col2_pos+((btn_width+1)*3),'Location ID',2004,'',btn_width)
		AskAcct_line+=1
		if account_selection_method=asm_locationId then
			fnLbl(AskAcct_line+=1,1,"Location ID:",col1_width,1)
		else
			fnLbl(AskAcct_line+=1,1,"Account:",col1_width,1)
		end if
		if account_selection_method=asm_combo then
			fncmbact(AskAcct_line,col2_pos)
		else if account_selection_method=asm_grid then
			fn_customer_grid(AskAcct_line,col2_pos)
		else if account_selection_method=asm_text then
			fnTxt(AskAcct_line,col2_pos,10)
		else if account_selection_method=asm_locationId then
			fnTxt(AskAcct_line,col2_pos,11, 0,0,'30')
		end if
		if account_selection_method=asm_locationId then
			resp$(respc+=1)=str$(tmpLocationId)
		else
			resp$(respc+=1)=hact$
		end if
		if aas_button_enable_add then let fnCmdKey("&Add",2,0,0,"Add a new customer" )
		fnCmdKey(select_button_text$,1,1,0,select_button_text$&" the selected/highlighted record.")
		fnCmdKey("Search",6,0,0,"Search for customer record")
		fnCmdKey('Back',5,0,1,"Returns to previous screen")
		ckey=fnAcs(mat resp$)
		x$=trim$(resp$(1)(1:10))
		if account_selection_method=asm_text and ckey=1 then
			if ~fn_key_tweak(x$,h_customer_1) then
				mat ml$(2)
				ml$(1)="Account "&x$&' could not be found.'
				ml$(2)="Select a different account."
				fnmsgbox(mat ml$,resp$,'',48)
				goto AAS_TOP
			end if
		else if account_selection_method=asm_locationId and ckey=1 then
			tmpLocationId=val(resp$(1)) conv AAS_TOP
			x$=fnAccountFromLocationId$(tmpLocationId)
			if x$='' then goto AAS_TOP
		else
			x$=lpad$(x$,10)
		end if
		if ckey=1 then
			fn_record_previous_update(x$)
			goto AA_FINIS
		else if ckey=2 and aas_button_enable_add then
			goto AA_FINIS
		else if ckey=5 then
			goto AA_FINIS
		else if ckey=6 then
			fnCustomerSearch(x$,fixgrid)
			if trim$(x$)<>'' then ! in case the search was canceled
				hact$=x$
				fn_record_previous_update(x$)
				ckey=1
				goto AA_FINIS
			end if
		else if ckey=1000 then
			fn_record_previous_clear
		else if ckey>1000 and ckey<=1000+udim(mat rp_prev$) then
			x$=lpad$(trim$(rp_prev$(ckey-1000)(1:10)),10)
			fn_record_previous_update(rp_prev$(ckey-1000)(1:10))
			ckey=1
			goto AA_FINIS
		else if ckey=2001 then
			account_selection_method=asm_combo
		else if ckey=2002 then
			account_selection_method=asm_grid
		else if ckey=2003 then
			account_selection_method=asm_text
		else if ckey=2004 then
			account_selection_method=asm_locationId
		end if
	loop
	AA_FINIS: !
	fn_record_previous_save
	fnureg_write('ubfm.account_selection_method',str$(account_selection_method))
	release #h_customer_1: 
	fn_ask_account=ckey
fnend
def library fnapply_default_rates(mat extra, mat a)
	if ~setup then fn_setup
	fnapply_default_rates=fn_apply_default_rates(mat extra, mat a)
fnend
def fn_apply_default_rates(mat extra, mat a)
	if ~adr_setup then
		adr_setup=1
		for service_item=1 to 10
			dim tmp_rate$*256
			fncreg_read('default rate '&str$(service_item),tmp_rate$)
			tmp_rate_val(service_item)=val(tmp_rate$(1:pos(tmp_rate$,'=')-1))
		next service_item
	end if
	for service_item=1 to 10
		if tmp_rate_val(service_item)>0 then
			if service_item=>1 and service_item<=5 then
				if a(service_item)=0 then a(service_item)=tmp_rate_val(service_item)
			else if service_item=6 then
				if extra(11)=0 then extra(11)=tmp_rate_val(service_item)
			else if service_item=7 then
				if extra(12)=0 then extra(12)=tmp_rate_val(service_item)
			else if service_item=8 then
				if extra(13)=0 then extra(13)=tmp_rate_val(service_item)
			else if service_item=9 then
				if a(6)=0 then a(6)=tmp_rate_val(service_item)
			else if service_item=10 then
				if a(7)=0 then a(7)=tmp_rate_val(service_item)
			end if
		end if
	next service_item
fnend
def fn_warn_text$*200(z$*10; wtDefaultText$*256)
	dim wtReturn$*200
	dim wtLine$*2048
	wtMaxLen=200
	wtReturn$=wtDefaultText$
	open #h_notefile=fnH:'name='&fn_notedir$&"\"&trim$(z$)&".txt",d,input ioerr WT_FINIS
	do
		linput #h_notefile: wtLine$ eof WT_FINIS
		if lwrc$(wtLine$(1:5))='warn:' then
			wtLine$(1:5)=''
			wtLine$=trim$(wtLine$)
			if len(wtLine$)>(wtMaxLen) then wtLine$(wtMaxLen-2:wtMaxLen)='...'
			wtReturn$=wtLine$(1:wtMaxLen)
		end if
	loop
	WT_FINIS: !
	close #h_notefile: ioerr ignore
	fn_warn_text$=wtReturn$
fnend
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		! r: dims
		dim z$*10
		dim e$(4)*30
		dim f$(3)*12
		dim a(7)
		dim b(11)
		dim c(4)
		dim d(15)
		dim g(12)
		dim adr(2)
		dim alp$*7
		dim resp$(50)*320
		dim hact$*80
		dim jbact$*81
		dim custInfo$(35)*40
		dim x$*10
		dim p$*10
		dim srvnam$(10)*20
		dim srv$(10)*2
		dim noteFile$*256
		dim noteFileNew$*256
		dim gb(10)
		dim ab$(4)*30
		dim rt$*54
		dim newe4$*30
		dim olde3$*30
		dim code$(5)*32
		dim rates$(50)*30
		dim rateInfo$(128)*128
		dim citykey$*30
		dim citystzip$*30
		dim extra(23)
		dim extra$(11)*30
		dim item$(25)*70
		dim opt$(2)*20
		dim ba(13)
		dim ml$(6)*200
		dim dp$*70
		dim bt1(14,2)
		dim badr(2)
		dim budgetinfo$(28)
		dim bxnf$(30)*30      ! Billing Information Responses
		dim dri$(8)*30        ! Draft Information Responses
		dim df$*1             ! Bank Draft (Y)
		dim form$(0)*512
		dim location$(0)*256,locationN(0)
		! /r
	end if
	! r: CONSTANTS
	fncreg_read('Route Low' ,bkno1$) : bkno1=val(bkno1$)
	fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)
	fnGetServices(mat srvnam$,mat srv$)
	dim serviceCodeMetered$(0)*2
	fnGetServiceCodesMetered(mat serviceCodeMetered$)
	open #20: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input,relative
	read #20,using "Form POS 81,C 30,pos 129,c 1",rec=1: newe4$,escrow$
	close #20:
	j=first_service=0
	do until first_service<>0
		if trim$(srvnam$(j+=1))<>'' then first_service=j
	loop
	! /r
fnend
def fn_getRateCodeOptions(service_code,&ratecode,mat rates$ ) ! get applicable rate codes
	! search routine must be passed code for service (WA for water) in searchcode$
	searchcode$=srv$(service_code)
	open #h_rate1=fnH: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed
	restore #h_rate1:
	mat rates$(99)
	mat rates$=("")
	fncreg_read('default rate '&str$(service_code),tmp_rate$)
	if tmp_rate$<>'' then
		x=0
		if ratecode=0 then
			ratecode=val(tmp_rate$(1:pos(tmp_rate$,'=')-1))
		end if
	else
		x=1
		rates$(1)=" 0=Not applicable"
	end if
	do
		read #h_rate1,using "Form POS 1,C 54",release: rt$ eof GCODE_FINIS
		if trim$(rt$(1:2))=searchcode$ then
			rates$(x+=1)=rt$(3:4)&"="&rt$(5:25)
			if ratecode=val(rt$(3:4)) then rateInfo$(3)=rt$(3:4)&"="&rt$(5:25)
		end if
	loop
	GCODE_FINIS: !
	if x>0 then mat rates$(x) else mat rates$(1)
	if ratecode=0 then rateInfo$(3)=" 0=Not applicable"
	close #h_rate1: ioerr ignore
fnend

def fn_legacy2customer(mat customer$,mat customerN,&z$,mat e$,&f1$,mat a,mat b,mat c,mat d,&bal,&lastBillingDate,mat g,mat adr,&alp$,&f2$,&f3$,&bra_legacy,mat gb,&df$,&dr$,&dc$,&da$,mat extra,mat extra$)
	customer$(c_account           		)			=z$
	customer$(c_meterAddress      		)			=e$(1)
	customer$(c_name              		)			=e$(2)
	customer$(c_addr1             		)			=e$(3)
	customer$(c_csz               		)			=e$(4)
	customer$(c_s1meterNumber     		)			=f1$
	customerN(c_s01rate           		)			=a(1)
	customerN(c_s02rate           		)			=a(2)
	customerN(c_s03rate           		)			=a(3)
	customerN(c_s04rate           		)			=a(4)
	customerN(c_s05rate           		)			=a(5)
	customerN(c_s09rate           		)			=a(6)
	customerN(c_s10rate           		)			=a(7)
	customerN(c_s01stdCharge      		)			=b(1)
	customerN(c_s02stdCharge      		)			=b(2)
	customerN(c_s03stdCharge      		)			=b(3)
	customerN(c_s04stdCharge      		)			=b(4)
	customerN(c_s05stdCharge      		)			=b(5)
	customerN(c_s06stdCharge      		)			=b(6)
	customerN(c_s07stdCharge      		)			=b(7)
	customerN(c_s01depositAmt     		)			=b(8)
	customerN(c_s02depositAmt     		)			=b(9)
	customerN(c_s03depositAmt     		)			=b(10)
	customerN(c_s04depositAmt     		)			=b(11)
	customerN(c_s01depositDate    		)			=c(1)
	customerN(c_s02depositDate    		)			=c(2)
	customerN(c_s03depositDate    		)			=c(3)
	customerN(c_s04depositDate    		)			=c(4)
	customerN(c_s01readingCur     		)			=d(1)
	customerN(c_s01readingPri     		)			=d(2)
	customerN(c_s01UsageCur       		)			=d(3)
	customerN(c_s01UsageYtd       		)			=d(4)
	customerN(c_s03readingCur     		)			=d(5)
	customerN(c_s03ReadingPri     		)			=d(6)
	customerN(c_s03UsageCur       		)			=d(7)
	customerN(c_s03UsageYtd       		)			=d(8)
	customerN(c_s04readingCur     		)			=d(9)
	customerN(c_s04readingPri     		)			=d(10)
	customerN(c_s04usageCur       		)			=d(11)
	customerN(c_s04usageYtd       		)			=d(12)
	customerN(c_s01unitCount      		)			=d(13)
	customerN(c_demandMultiplier  		)			=d(14)
	customerN(c_demandReading     		)			=d(15)
	customerN(c_balance           		)			=bal
	customerN(c_lastBillingDate   		)			=lastBillingDate
	customerN(c_s01bill           		)			=g(1)
	customerN(c_s02bill           		)			=g(2)
	customerN(c_s03bill           		)			=g(3)
	customerN(c_s04bill           		)			=g(4)
	customerN(c_s05bill           		)			=g(5)
	customerN(c_s06bill           		)			=g(6)
	customerN(c_s07bill           		)			=g(7)
	customerN(c_s08bill           		)			=g(8)
	customerN(c_s09bill           		)			=g(9)
	customerN(c_s10bill           		)			=g(10)
	customerN(c_netBill           		)			=g(11)
	customerN(c_grossBill         		)			=g(12)
	customerN(c_adr1              		)			=adr1
	customerN(c_adr2              		)			=adr2
	customer$(c_alphaSort         		)			=alp$
	customer$(c_s03meterNumber    		)			=f2$
	customer$(c_s04meterNumber    		)			=f3$
	customerN(c_bra               		)			=bra_legacy
	customerN(c_s01breakdown      		)			=gb(1)
	customerN(c_s02breakdown      		)			=gb(2)
	customerN(c_s03breakdown      		)			=gb(3)
	customerN(c_s04breakdown      		)			=gb(4)
	customerN(c_s05breakdown      		)			=gb(5)
	customerN(c_s06breakdown      		)			=gb(6)
	customerN(c_s07breakdown      		)			=gb(7)
	customerN(c_s08breakdown      		)			=gb(8)
	customerN(c_s09breakdown      		)			=gb(9)
	customerN(c_s10breakdown      		)			=gb(10)
	customer$(c_bankDraft         		)			=df$
	customer$(c_bankRoutingNumber 		)			=dr$
	customer$(c_bankAccountCode   		)			=dc$
	customer$(c_bankAccountNumber 		)			=da$
	customerN(c_route             		)			=extra(1)
	customerN(c_sequence          		)			=extra(2)
	customerN(c_meterReadDateCur  		)			=extra(3)
	customerN(c_meterReadDatePri  		)			=extra(4)
	customerN(c_sewerReduction    		)			=extra(5)
	customerN(c_s03securityLight  		)			=extra(6)
	customerN(c_s03lightCount     		)			=extra(7)
	customerN(c_s03multiplier     		)			=extra(8)
	customerN(c_extra_09N         		)			=extra(9)
	customerN(c_s04multiplier     		)			=extra(10)
	customerN(c_s06rate           		)			=extra(11)
	customerN(c_s07rate           		)			=extra(12)
	customerN(c_s08rate           		)			=extra(13)
	customerN(c_s02units          		)			=extra(14)
	customerN(c_s03units          		)			=extra(15)
	customerN(c_s04units          		)			=extra(16)
	customerN(c_finalBilling      		)			=extra(17)
	customerN(c_s02averageUsage   		)			=extra(18)
	customerN(c_estimationDate    		)			=extra(19)
	customerN(c_unused09          		)			=extra(20)
	customerN(c_unused10          		)			=extra(21)
	customerN(c_enableAltBillAddr 		)			=extra(22)
	customerN(c_unused11          		)			=extra(23)
	customer$(c_addr2             		)			=extra$(1)
	customer$(c_phoneMain         		)			=extra$(2)
	customer$(c_s01serialNumber   		)			=extra$(3)
	customer$(c_s03serialNumber   		)			=extra$(4)
	customer$(c_s04serialNumber   		)			=extra$(5)
	customer$(c_unused05          		)			=extra$(6)
	customer$(c_unused06          		)			=extra$(7)
	customer$(c_phoneCell         		)			=extra$(8)
	customer$(c_email             		)			=extra$(9)
	customer$(c_unused07          		)			=extra$(10)
	customer$(c_unused08          		)			=extra$(11)
fnend
def fn_customer2legacy(mat customer$,mat customerN,&z$,mat e$,&f1$,mat a,mat b,mat c,mat d,&bal,&lastBillingDate,mat g,mat adr,&alp$,&f2$,&f3$,&bra_legacy,mat gb,&df$,&dr$,&dc$,&da$,mat extra,mat extra$)
	z$                   	=customer$(c_account           		)
	e$(1)                	=customer$(c_meterAddress      		)
	e$(2)                	=customer$(c_name              		)
	e$(3)                	=customer$(c_addr1             		)
	e$(4)                	=customer$(c_csz               		)
	f1$                  	=customer$(c_s1meterNumber     		)
	a(1)                 	=customerN(c_s01rate           		)
	a(2)                 	=customerN(c_s02rate           		)
	a(3)                 	=customerN(c_s03rate           		)
	a(4)                 	=customerN(c_s04rate           		)
	a(5)                 	=customerN(c_s05rate           		)
	a(6)                 	=customerN(c_s09rate           		)
	a(7)                 	=customerN(c_s10rate           		)
	b(1)                 	=customerN(c_s01stdCharge      		)
	b(2)                 	=customerN(c_s02stdCharge      		)
	b(3)                 	=customerN(c_s03stdCharge      		)
	b(4)                 	=customerN(c_s04stdCharge      		)
	b(5)                 	=customerN(c_s05stdCharge      		)
	b(6)                 	=customerN(c_s06stdCharge      		)
	b(7)                 	=customerN(c_s07stdCharge      		)
	b(8)                 	=customerN(c_s01depositAmt     		)
	b(9)                 	=customerN(c_s02depositAmt     		)
	b(10)                	=customerN(c_s03depositAmt     		)
	b(11)                	=customerN(c_s04depositAmt     		)
	c(1)                 	=customerN(c_s01depositDate    		)
	c(2)                 	=customerN(c_s02depositDate    		)
	c(3)                 	=customerN(c_s03depositDate    		)
	c(4)                 	=customerN(c_s04depositDate    		)
	d(1)                 	=customerN(c_s01readingCur     		)
	d(2)                 	=customerN(c_s01readingPri     		)
	d(3)                 	=customerN(c_s01UsageCur       		)
	d(4)                 	=customerN(c_s01UsageYtd       		)
	d(5)                 	=customerN(c_s03readingCur     		)
	d(6)                 	=customerN(c_s03ReadingPri     		)
	d(7)                 	=customerN(c_s03UsageCur       		)
	d(8)                 	=customerN(c_s03UsageYtd       		)
	d(9)                 	=customerN(c_s04readingCur     		)
	d(10)                	=customerN(c_s04readingPri     		)
	d(11)                	=customerN(c_s04usageCur       		)
	d(12)                	=customerN(c_s04usageYtd       		)
	d(13)                	=customerN(c_s01unitCount      		)
	d(14)                	=customerN(c_demandMultiplier  		)
	d(15)                	=customerN(c_demandReading     		)
	bal                  	=customerN(c_balance           		)
	lastBillingDate    	=customerN(c_lastBillingDate   		)
	g(1)                 	=customerN(c_s01bill           		)
	g(2)                 	=customerN(c_s02bill           		)
	g(3)                 	=customerN(c_s03bill           		)
	g(4)                 	=customerN(c_s04bill           		)
	g(5)                 	=customerN(c_s05bill           		)
	g(6)                 	=customerN(c_s06bill           		)
	g(7)                 	=customerN(c_s07bill           		)
	g(8)                 	=customerN(c_s08bill           		)
	g(9)                 	=customerN(c_s09bill           		)
	g(10)                	=customerN(c_s10bill           		)
	g(11)                	=customerN(c_netBill           		)
	g(12)                	=customerN(c_grossBill         		)
	adr1                 	=customerN(c_adr1              		)
	adr2                 	=customerN(c_adr2              		)
	alp$                 	=customer$(c_alphaSort         		)
	f2$                  	=customer$(c_s03meterNumber    		)
	f3$                  	=customer$(c_s04meterNumber    		)
	bra_legacy          	=customerN(c_bra               		)
	gb(1)                	=customerN(c_s01breakdown      		)
	gb(2)                	=customerN(c_s02breakdown      		)
	gb(3)                	=customerN(c_s03breakdown      		)
	gb(4)                	=customerN(c_s04breakdown      		)
	gb(5)                	=customerN(c_s05breakdown      		)
	gb(6)                	=customerN(c_s06breakdown      		)
	gb(7)                	=customerN(c_s07breakdown      		)
	gb(8)                	=customerN(c_s08breakdown      		)
	gb(9)                	=customerN(c_s09breakdown      		)
	gb(10)               	=customerN(c_s10breakdown      		)
	df$                  	=customer$(c_bankDraft         		)
	dr$                  	=customer$(c_bankRoutingNumber 		)
	dc$                  	=customer$(c_bankAccountCode   		)
	da$                  	=customer$(c_bankAccountNumber 		)
	extra(1)             	=customerN(c_route             		)
	extra(2)             	=customerN(c_sequence          		)
	extra(3)            	=customerN(c_meterReadDateCur  		)
	extra(4)            	=customerN(c_meterReadDatePri  		)
	extra(5)            	=customerN(c_sewerReduction    		)
	extra(6)            	=customerN(c_s03securityLight  		)
	extra(7)            	=customerN(c_s03lightCount     		)
	extra(8)            	=customerN(c_s03multiplier     		)
	extra(9)            	=customerN(c_extra_09N         		)
	extra(10)           	=customerN(c_s04multiplier     		)
	extra(11)           	=customerN(c_s06rate           		)
	extra(12)           	=customerN(c_s07rate           		)
	extra(13)           	=customerN(c_s08rate           		)
	extra(14)           	=customerN(c_s02units          		)
	extra(15)           	=customerN(c_s03units          		)
	extra(16)           	=customerN(c_s04units          		)
	extra(17)           	=customerN(c_finalBilling      		)
	extra(18)           	=customerN(c_s02averageUsage   		)
	extra(19)           	=customerN(c_estimationDate    		)
	extra(20)           	=customerN(c_unused09          		)
	extra(21)           	=customerN(c_unused10          		)
	extra(22)           	=customerN(c_enableAltBillAddr 		)
	extra(23)           	=customerN(c_unused11          		)
	extra$(1)           	=customer$(c_addr2             		)
	extra$(2)           	=customer$(c_phoneMain         		)
	extra$(3)           	=customer$(c_s01serialNumber   		)
	extra$(4)           	=customer$(c_s03serialNumber   		)
	extra$(5)           	=customer$(c_s04serialNumber   		)
	extra$(6)           	=customer$(c_unused05          		)
	extra$(7)           	=customer$(c_unused06          		)
	extra$(8)           	=customer$(c_phoneCell         		)
	extra$(9)           	=customer$(c_email             		)
	extra$(10)          	=customer$(c_unused07          		)
	extra$(11)          	=customer$(c_unused08          		)
fnend

def fn_customerBeforeSet(mat customer$,mat customerN,mat customerBefore$,mat customerBeforeN)
	if ~setup_customerBeforeSet then
		setup_customerBeforeSet=1
		dim customerBefore$(0)*256
		dim customerBeforeN(0)
		mat customerBefore$(udim(mat customer$))
		mat customerBeforeN(udim(mat customerN))
	end if
	mat customerBefore$=customer$
	mat customerBeforeN=customerN
fnend
def fn_customerChangesReport(mat customer$,mat customerN,mat customerBefore$,mat customerBeforeN)
	open #h_notefile=fnH:'name='&fn_notedir$&"\"&trim$(z$)&".log",d,output
fnend
include: ertn
include: fn_open
