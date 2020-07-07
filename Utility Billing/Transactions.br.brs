! formerly S:\acsUB\FlexTran

fn_setup
fnTop(program$)
dim unused_gb(10)
fn_transfile( empty$,unused_bal,mat unused_gb)
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		
		tcode_charge      	=1
		tcode_penalty     	=2
		tcode_collection  	=3
		tcode_credit      	=4
		tcode_debit       	=5
		
		dim transType$(5)*11
		transType$(tcode_charge    )="Charge"
		transType$(tcode_penalty   )="Penalty"
		transType$(tcode_collection)="Collection"
		transType$(tcode_credit    )="Credit Memo"
		transType$(tcode_debit     )="Debit Memo"

		dim serviceName$(10)*20
		dim srv$(10)*2
		fnget_services(mat serviceName$, mat srv$)
		
	end if
fnend
def library fntrans_total_as_of(; customer_key$,date_ccyymmdd,trans_type)
	if ~setup then fn_setup
	fntrans_total_as_of=fn_trans_total_as_of(customer_key$,date_ccyymmdd,trans_type)
fnend
def fn_trans_total_as_of(;customer_key$,date_ccyymmdd, trans_type)
	! transaction_type or blank for all
	if ~ttao_setup then 
		ttao_setup=1
		open #ttao_h_trans:=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
		dim ttao_key$*19
	end if
	ttao_return=0
	customer_key$=lpad$(customer_key$,10)
	! ttao_key$=rpad$(customer_key$,kln(ttao_h_trans)) 
	ttao_key$=customer_key$&lpad$(str$(date_ccyymmdd),8)&cnvrt$('pic(z)',trans_type)
	restore #ttao_h_trans,key>=ttao_key$: nokey TTAO_FINIS
	do
		read #ttao_h_trans,using "Form pos 1,c 10,N 8,N 1,pd 4.2": ttao_customer_read$,ttao_date,ttao_code,ttao_amount eof TTAO_FINIS
		if lpad$(ttao_customer_read$,10)=lpad$(customer_key$,10) or trim$(ttao_customer_read$)<>'' then 
			if ~date_ccyymmdd or ttao_date=>date_ccyymmdd then 
				if trans_type=0 or trans_type=ttao_code then
					ttao_return+=ttao_amount
				end if
			end if
		end if
	loop until customer_key$<>'' and ttao_customer_read$<>customer_key$
	TTAO_FINIS: !
	fn_trans_total_as_of=ttao_return
fnend 
def library fntransfile(; hact$*81,&bal,mat gb)
	if ~setup then fn_setup
	fntransfile=fn_transfile( hact$,bal,mat gb)
fnend
def fn_transfile(; hact$*81,&bal,mat gb)

	enableDelete=0 ! if env$('acsDeveloper')<>'' then enableDelete=1

	dim resp$(10)*80
	dim totalalloc(10),totalusage(3),usage(3)

	ScreenAskFilters: ! r: 
	fnTos(sn$="Transaction-1")
	rc=cf=0
	fnFra(1,1,6,23,"Transaction Type","You can review all transactions or any specific type of transaction",0)
	cf+=1 : fratype=cf
	fnOpt(1,3,"[All]",0,fratype)
	if sel_code=1 or sel_code=0 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
	fnOpt(2,3,"Charges",0,fratype)
	if sel_code=2 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
	fnOpt(3,3,"Penalties",0,fratype)
	if sel_code=3 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
	fnOpt(4,3,"Collections",0,fratype)
	if sel_code=4 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
	fnOpt(5,3,"Credit Memos",0,fratype)
	if sel_code=5 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
	fnOpt(6,3,"Debit Memos",0,fratype)
	if sel_code=6 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
	fnFra(1,30,3,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.")
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting Date:",mylen,1,0,fradate)
	fnTxt(1,mypos,10,0,1,"3",0,empty$,fradate)
	if beg_date=0 then beg_date=date('mm')*10000+100+date('yy')-1
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,fradate)
	fnTxt(2,mypos,10,0,1,"3",0,empty$,fradate)
	resp$(rc+=1)=str$(end_date)
	fnFra(6,30,2,60,"Account","You review transactions for all accounts or for an individual.")
	cf+=1 : fraaccount=cf
	fnLbl(1,1,"Account:",8,1,0,fraaccount)
	if trim$(hact$)<>'' then 
		fnTxt(1,10,10,0,1,'',1,'',fraaccount)
		resp$(rc+=1)=hact$
	else
		fncmbact(1,10,1,fraaccount)
		rc+=1
		if resp$(rc)="" then resp$(rc)="[All]"
	end if 
	fnCmdKey("Next",1,1,0,"Displays a list of transactions on the screen")
	fnCmdKey("Print",2,0,0,"Prints a transaction listing. (To get totals, you can only select one type of transaction at a time.")
	fnCmdKey("Cancel",5,0,1,"Returns to customer record")
	fnAcs(mat resp$,ckey)
	if ckey=5 then 
		goto Tf_XIT
	else
		if resp$(1)="True" then 
			sel_code=1
		else if resp$(2)="True" then 
			sel_code=2
		else if resp$(3)="True" then 
			sel_code=3
		else if resp$(4)="True" then 
			sel_code=4
		else if resp$(5)="True" then 
			sel_code=5
		else if resp$(6)="True" then 
			sel_code=6
		end if 
		beg_date=val(resp$(7))
		end_date=val(resp$(8))
		z$=resp$(9)(1:10)
		if ckey=2 then 
			! if trim$(z$)<>"" and trim$(z$)<>"[All]"  then 
				fn_printTrans ! pr report of charges
				goto ScreenAskFilters 
			! else
				! messagebox stating you can't
				! goto ScreenTransGrid
			! end if
		end if
	end if
	goto ScreenTransGrid
	! /r

	ScreenTransGrid: ! r:
	do
		fnTos(sn$="Transaction-2")
		stgFlexLine=0
		fnButton(stgFlexLine+=1,1,'Columns',opt_columns:=6)
		if z$<>'[All]' then
			fnLbl(stgFlexLine+=1,1,'Account:',8,1)
			fnTxt(stgFlexLine,10,10,0,0,'',1)
			resp$(1)=z$
		end if
		fn_flextran(stgFlexLine+=1,1,0,z$,beg_date,end_date,sel_code)
		fnCmdKey('Back',opt_back:=2,0,0,'Return to filter selection')
		if enableDelete then
			fnCmdKey("Delete",optDelete:=8,0,0,"Displays a list of transactions on the screen")
		end if
		fnCmdKey('Edit',opt_edit:=1,1,0)
		fnCmdKey('Print',opt_print:=4,0,0)
		fnCmdKey('Close',5,0,1)
		fnAcs(mat resp$,ckey)
		if ckey=opt_back then 
			goto ScreenAskFilters
		else if ckey=5 then 
			goto Tf_XIT
		else
			if z$='[All]' then editrec=val(resp$(1)) else editrec=val(resp$(2))
			if opt_columns and ckey=opt_columns then 
				fn_columnSelect
			else if ckey=opt_print then 
				fn_printTrans
			else if ckey=opt_edit then 
				fn_TransactionEdit(editrec)
			else if enableDelete and ckey=optDelete then
				fn_transactionDelete(editrec,bal,mat gb)
			end if 
		end if 
	loop ! /r
	Tf_XIT: ! 
fnend 
def fn_transactionDelete(editrec,&bal,mat gb; ___,tranRec,runningBalance)
	dim tran$(0)*128
	dim tranN(0)
	hTran=fn_open('UB Transaction',mat tran$,mat tranN,mat form$)
	dim origTran$(0)*128
	dim origTranN(0)
	mat origTran$(udim(mat tran$))
	mat origTranN(udim(mat tranN))
	open #hTranRelative:=fngethandle: 'name=[Q]\UBmstr\ubTransVB.h[cno],shr',internal,outin,relative
	read #hTranRelative,using form$(hTran),rec=editrec: mat origTran$,mat origTranN noRec TdEoTran
	delete #hTranRelative,rec=editrec:
	if origTranN(trans_tcode)=tcode_charge or origTranN(trans_tcode)=tcode_penalty or origTranN(trans_tcode)=tcode_debit then
		tBalanceModifier=-1*transAmount 
	else if origTranN(trans_tcode)=tcode_collection or origTranN(trans_tcode)=tcode_credit then
		tBalanceModifier=   transAmount 
	else
		pr bell;program$&': origTranN(trans_tcode) '&str$(origTranN(trans_tcode))&' unrecognized.'
		pause
	end if
	! r: fix future trans current balance
	tranRec=editrec
	runningBalance=fn_lastTBalBeforeRec(hTranRelative,origTran$(trans_acct),tranRec)
	do
		tranRec+=1
		read #hTranRelative,using form$(hTran),rec=tranRec,release: mat tran$,mat tranN noRec NextTran
		if tran$(trans_acct)=origTran$(trans_acct) then
			! if tranN(trans_tcode)=tcode_penalty then pause
			if tranN(trans_tcode)=tcode_charge or tranN(trans_tcode)=tcode_penalty or tranN(trans_tcode)=tcode_debit then
				runningBalance +=tranN(trans_tamount)
			else if tranN(trans_tcode)=tcode_collection or tranN(trans_tcode)=tcode_credit then
				runningBalance -=tranN(trans_tamount)
			else
				pr bell;program$&': tranN(trans_tcode) '&str$(tranN(trans_tcode))&' unrecognized.'
				pause
			end if
			tranN(trans_tbal)=runningBalance
			rewrite #hTranRelative,using form$(hTran),rec=tranRec: mat tran$,mat tranN
			! pr 'updated one: '&tran$(trans_acct)&' new tbal: '&str$(tranN(trans_tbal))
			! pause
		end if
		NextTran: !
	loop while tranRec<lrec(hTranRelative)
	! /r
	! r: update customer's balance and balance breakdown
	dim c$(0)*256
	dim cN(0)
	hCustomer=fn_open('UB Customer',mat c$,mat cN,mat form$)
	read #hCustomer,using form$(hCustomer),key=origTran$(trans_acct): mat c$,mat cN ! noKey ignore
	! tranN(trans_tcode)   1=Charge,2=Penalty,3=Collection,4=Credit Memo,5=Debit Memo
	pr 'origional balance '&str$(cN(c_balance))&' for '&origTran$(trans_acct)
	
	if origTranN(trans_tcode)=tcode_charge or origTranN(trans_tcode)=tcode_penalty or origTranN(trans_tcode)=tcode_debit then
		gb( 1)=cN(c_s01breakdown)=cN(c_s01breakdown)-origTranN(trans_tg_1   )
		gb( 2)=cN(c_s02breakdown)=cN(c_s02breakdown)-origTranN(trans_tg_2   )
		gb( 3)=cN(c_s03breakdown)=cN(c_s03breakdown)-origTranN(trans_tG_3   )
		gb( 4)=cN(c_s04breakdown)=cN(c_s04breakdown)-origTranN(trans_tG_4   )
		gb( 5)=cN(c_s05breakdown)=cN(c_s05breakdown)-origTranN(trans_tG_5   )
		gb( 6)=cN(c_s06breakdown)=cN(c_s06breakdown)-origTranN(trans_tG_6   )
		gb( 7)=cN(c_s07breakdown)=cN(c_s07breakdown)-origTranN(trans_tG_7   )
		gb( 8)=cN(c_s08breakdown)=cN(c_s08breakdown)-origTranN(trans_tG_8   )
		gb( 9)=cN(c_s09breakdown)=cN(c_s09breakdown)-origTranN(trans_tG_9   )
		gb(10)=cN(c_s10breakdown)=cN(c_s10breakdown)-origTranN(trans_TG_10  )
		bal   =cN(c_balance     )=cN(c_balance     )-origTranN(trans_tamount)
	else if origTranN(trans_tcode)=tcode_collection or origTranN(trans_tcode)=tcode_credit then
		gb( 1)=cN(c_s01breakdown)=cN(c_s01breakdown)+origTranN(trans_tg_1   )
		gb( 2)=cN(c_s02breakdown)=cN(c_s02breakdown)+origTranN(trans_tg_2   )
		gb( 3)=cN(c_s03breakdown)=cN(c_s03breakdown)+origTranN(trans_tG_3   )
		gb( 4)=cN(c_s04breakdown)=cN(c_s04breakdown)+origTranN(trans_tG_4   )
		gb( 5)=cN(c_s05breakdown)=cN(c_s05breakdown)+origTranN(trans_tG_5   )
		gb( 6)=cN(c_s06breakdown)=cN(c_s06breakdown)+origTranN(trans_tG_6   )
		gb( 7)=cN(c_s07breakdown)=cN(c_s07breakdown)+origTranN(trans_tG_7   )
		gb( 8)=cN(c_s08breakdown)=cN(c_s08breakdown)+origTranN(trans_tG_8   )
		gb( 9)=cN(c_s09breakdown)=cN(c_s09breakdown)+origTranN(trans_tG_9   )
		gb(10)=cN(c_s10breakdown)=cN(c_s10breakdown)+origTranN(trans_TG_10  )
		bal   =cN(c_balance     )=cN(c_balance     )+origTranN(trans_tamount)
	end if
	! pr 'rewriting balance '&str$(cN(c_balance))&' for '&origTran$(trans_acct)
	! pause
	rewrite #hCustomer,using form$(hCustomer),key=origTran$(trans_acct): mat c$,mat cN
	fnclosefile(hCustomer,'UB Customer')
	! /r
	TdEoTran: !
	close #hTranRelative:
	fnclosefile(hTran,'UB Transaction')
fnend
def fn_lastTBalBeforeRec(hTranRelative,z$,recNum; ___,returnN) ! reqires local mat tran$ and mat tranN (to copy sizes from) and local trans_[enum]s
	if ~setupltbr then
		setupltbr=1
		dim xTran$(0)*256
		dim xTranN(0)
		mat xTran$(udim(mat tran$))
		mat xTranN(udim(mat tranN))
	end if
	do
		recNum-=1
		read #hTranRelative,using form$(hTran),rec=recNum,release: mat xTran$,mat xTranN norec LtbrNextRec
		if xTran$(trans_acct)=z$ then
			returnN=xTranN(trans_tbal)
		end if
		LtbrNextRec: !
	loop until xTran$(trans_acct)=z$ or recNum=1
	fn_lastTBalBeforeRec=returnN
fnend
def fn_TransactionEdit(editrec)
	open #trans=fngethandle: "Name=[Q]\UBmstr\ubtransvb.h[cno],Shr",internal,outIn,relative 
	read #trans,using "Form pos 1,c 10,N 8,N 1,pd 4.2",rec=editrec: transAcct$,tdate,tcode,tamount
	fnTos(sn$="Transaction-3")
	lc=rc=0 : mylen=20 : mypos=mylen+2
	fnLbl(lc+=1,1,"Record:",mylen)
	fnTxt(lc,mypos,10,0,0,empty$,1)
	resp$(rc+=1)=str$(editrec)
	fnLbl(lc+=1,1,"Customer:",mylen)
	fnTxt(lc,mypos,10,0,0,empty$,1)
	resp$(rc+=1)=transAcct$
	fnLbl(lc+=1,1,"Date:",mylen)
	fnTxt(lc,mypos,10,0,0,"3")
	resp$(respc_tDate:=rc+=1)=str$(tdate)
	fnLbl(lc+=1,1,"Type:",mylen)
	fnTxt(lc,mypos,12,0,0,empty$,1)
	resp$(rc+=1)=transType$(tcode)
	fnLbl(lc+=1,1,"Amount:",mylen)
	fnTxt(lc,mypos,10,0,0,"10",1)
	resp$(rc+=1)=str$(tamount)
	fnCmdKey('Save',1,1,0)
	fnCmdKey('Cancel',5,0,1)
	fnAcs(mat resp$,ckey)
	if ckey=1 then 
		tdate=val(resp$(respc_tDate))
		rewrite #trans,using "Form pos 11,N 8",rec=editrec: tdate
	end if 
	close #trans: 
fnend
def fn_printTrans ! very local function - lots of inherritance
	! dim scr1$(10)*30
	dim alloc(10)
	dim r(20,4)
	dim hd1$*255
	dim tg(11)
	dim name$(10)*20
	! r: ask print_balance
	if env$('client')="White Hall" then 
		msgbox_default=0
	else 
		msgbox_default=256
	end if 
	dim msgbox$(3)*128
	mat msgbox$(3)
	msgbox$(1)="Include balance column?"
	msgbox$(2)="The balances listed were the account balance at the time the transaction completed"
	msgbox$(3)="and will be misleading if transactions were processed out of date sequence."
	fnmsgbox(mat msgbox$,resp$,cap$,32+3+msgbox_default)
	if resp$='Cancel' then goto PT_XIT
	if resp$='Yes' then 
		print_balance=1
	else 
		print_balance=0
	end if 
	! /r
	fnopenprn
	if trim$(serviceName$(3))<>"Electric" and srv$(3)="EL" then ptShowElecUsed=1 ! electric readings are being used for a reduction meter
	if trim$(serviceName$(4))<>"Gas" and srv$(4)="GA" then ptShowGasUsed=1 ! gas readings are being used for a reduction meter
	if trim$(z$)="[All]" then hd1$="{\ul  Account }     {\ul     Date   }" else hd1$="    {\ul    Date   }"
	sz1=0
	x=0
	for j=1 to 10
		if j=3 and ptShowElecUsed=1 then goto L1010 ! skp heading is electric field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
		if j=4 and ptShowGasUsed=1 then goto L1010 ! skp heading is gas field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
		x2=pos(trim$(serviceName$(j))," ",1)
		if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&"-"&serviceName$(j)(x2+1:len(serviceName$(j))) ! if service name two words long, use part of both
		if trim$(serviceName$(j))<>"" then 
			sz1+=1 ! scr1$(sz1+=1)=serviceName$(j)
			hd1$=hd1$&"  {\ul "&lpad$(rtrm$(serviceName$(j)(1:6)),6)&"}" : name$(x+=1)=serviceName$(j)
		end if  ! trim$(serviceName$(j))<>"" then
		L1010: ! 
	next j
	hd1$=hd1$&"{\ul     Total}"
	if print_balance then 
		hd1$=hd1$&"  {\ul   Balance }" 
	else
		if trim$(serviceName$(1))="Water" then 
			hd1$=hd1$&"  {\ul   Wa Used }" 
			water=1
		end if 
		if trim$(serviceName$(3))="Electric" then 
			hd1$=hd1$&"  {\ul   El Used }" 
			electric=1 
		else if ptShowElecUsed=1 then 
			hd1$=hd1$&"  {\ul      Used }" 
			electric=1
		end if 
		if trim$(serviceName$(4))="Gas" then 
			hd1$=hd1$&"  {\ul   Ga Used }" 
			gas=1 
		else if ptShowGasUsed=1 then 
			hd1$=hd1$&"  {\ul      Used }" 
			gas=1
		end if
	end if
	! mat scr1$(sz1)
	mat alloc(sz1) : mat totalalloc(sz1)
	mat totalalloc=(0) : mat totalusage=(0) : totaltamount=0
	mat totalalloc=(0) : mat totalusage=(0) : totaltamount=0
	close #trans: ioerr ignore
	open #trans=2: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed 
	if trim$(z$)="[All]" then 
		restore #trans: 
	else
		dim nam$*30
		dim metraddr$*30
		account_balance=val(fnCustomerData$(lpad$(rtrm$(z$),10),'balance'      , 1))
		nam$           =    fnCustomerData$(lpad$(rtrm$(z$),10),'name'         , 1)
		metraddr$      =    fnCustomerData$(lpad$(rtrm$(z$),10),'Meter Address'   )
		! open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
		! read #h_customer,using 'Form POS 11,c 30,C 28,pos 292,PD 4.2',key=lpad$(rtrm$(z$),10),release: metraddr$,nam$,account_balance nokey PT_NO_CUSTOMER
		! close #h_customer: ioerr ignore
		restore #trans,key>=lpad$(rtrm$(z$),10)&"         ": nokey PT_FINIS
	end if
	gosub HDR
	do 
		PT_TRANS_READ: ! 
		read #trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': transAcct$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PT_FINIS
		if trim$(z$)<>"[All]" and transAcct$<>lpad$(rtrm$(z$),10) then goto PT_FINIS
		if beg_date<>0 and tdate<beg_date then goto PT_TRANS_READ
		if end_date<>0 and tdate>end_date then goto PT_TRANS_READ
		if tamount=0 then goto PT_TRANS_READ
		if sel_code>1 and tcode<>sel_code-1 then goto PT_TRANS_READ
		if tcode=tcode_collection then ti2=1 ! REG.COLLECTION
		if tcode=tcode_credit     then ti2=2 ! CREDIT MEMO
		if tcode=tcode_debit      then ti2=3 ! DEBIT MEMO
		! if tcode=tcode_penalty then pause
		! if trim$(transAcct$)='100145.00' then pr transAcct$;tg(4) : pause
		if ti2=3 then r(1,1)-=tamount else r(1,1)+=tamount
		r(1,ti2+1)+=tamount
		x=0
		for j=1 to 10
			if trim$(serviceName$(j))<>"" then 
				if j=3 and srv$(3)="EL" and trim$(serviceName$(3))<>"Electric" and trim$(serviceName$(3))<>"Lawn Meter" then ! electic being used for reduction meter
					goto L1370
				end if
				if j=4 and srv$(4)="GA" and trim$(serviceName$(4))<>"Gas" then ! gas being used for reduction meter
					goto L1370
				end if
				alloc(x+=1)=tg(j)
				if ti2=3 then 
					r(x+3,1)-=tg(j) 
				else 
					r(x+3,1)+=tg(j)
				end if
				r(x+3,ti2+1)+=tg(j)
				L1370: ! 
			end if
		next j
		cx$=" "
		if tcode=tcode_charge     then cx$="CHG"
		if tcode=tcode_penalty    then cx$="PN"
		if tcode=tcode_collection then cx$="COL"
		if tcode=tcode_credit     then cx$="CM"
		if tcode=tcode_debit      then cx$="DM"
		service=0
		if water=1      	then service+=1 : usage(service)=wu ! water
		if electric=1   	then service+=1 : usage(service)=eu ! Electric
		if gas=1        	then service+=1 : usage(service)=gu ! Gas
		dim printlineform$*1024
		if cx$="CHG" then
			let printlineform$="c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*c 8,n 10.2,3*pic(--------.--),x 1"
			! build string alloc$ and set penalty to "" 
			mat alloc$(udim(alloc)) : mat alloc$=("")
			for counter=1 to udim(alloc)
				let alloc$(counter)=str$(alloc(counter))
				if pos(alloc$(counter),".")=0 then let alloc$(counter)=alloc$(counter)&".00"
				if len(alloc$(counter))-pos(alloc$(counter),".")<2 then alloc$(counter)=alloc$(counter)&"0"
				if len(alloc$(counter))-pos(alloc$(counter),".")<2 then alloc$(counter)=alloc$(counter)&"0" ! get both decimals if needed 
				alloc$(counter)=lpad$(alloc$(counter),8," ")
				next counter 
			penaltyindex=fn_getPenaltyIndex(mat serviceName$)
			if penaltyindex<>0 then alloc$(penaltyindex)=""
		else
			let printlineform$="c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 8.2,n 10.2,3*pic(--------.--),x 1"
		end if 
		if print_balance then 
			usage(1)=tbal
		end if 
		! if env$('acsDeveloper')<>"" then pause
		if trim$(z$)="[All]" then
			if cx$="CHG" then
				pr #255,using 'Form POS 1,c 10,x 1,'&printlineform$: transAcct$,cx$,tdate,mat alloc$,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
			else 
				pr #255,using 'Form POS 1,c 10,x 1,'&printlineform$: transAcct$,cx$,tdate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
			end if 
		else 
			if cx$="CHG" then
				pr #255,using 'Form POS 1,'&printlineform$: cx$,tdate,mat alloc$,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
			else
				pr #255,using 'Form POS 1,'&printlineform$: cx$,tdate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PGOF
			end if 
		end if  ! trim$(z$)="[All]"   /   else 
		if tcode=tcode_charge     then 
			mat totalalloc=totalalloc+alloc: totaltamount+=tamount  ! charges
			mat totalusage=totalusage+usage
			! reverse penaltyindex charges
			if penaltyindex<>0 then totalalloc(penaltyindex)-=alloc(penaltyindex)
		end if
		if tcode=tcode_penalty    then mat totalalloc=totalalloc+alloc: totaltamount+=tamount ! penalties
		if tcode=tcode_collection then mat totalalloc=totalalloc-alloc: totaltamount-=tamount ! collections
		if tcode=tcode_credit     then mat totalalloc=totalalloc-alloc: totaltamount-=tamount ! credit memos
		if tcode=tcode_debit      then mat totalalloc=totalalloc+alloc: totaltamount+=tamount ! debit memos
		!if env$('acsDeveloper')<>"" then
		!	if lamt2<>totaltamount then 
		!		lamt2=totaltamount
		!		print totaltamount
		!		pause
		!	end if 
		!end if 
	loop 
	PGOF: ! r:
		pr #255: newpage
		gosub HDR
	continue  ! /r
	HDR: ! r:
	! need date$,time$
		pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&" }"
		if nam$<>'' then
			pr #255: "\qc  {\f181 \fs20 \b "&trim$(nam$)&" }"
		end if
		if metraddr$<>'' then
			pr #255: "\qc  {\f181 \fs20 \b "&trim$(metraddr$)&" }"
		end if
		pr #255: "\qc  {\f181 \fs20 \b "&trim$(z$)&" }"
		pr #255: "\qc  {\f181 \fs28 \b Transaction List }"
		if beg_date<>0 and end_date<>0 then 
			pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)& "  To "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
		end if  ! beg_date<>0 and end_date<>0
		pr #255: ""
		pr #255: "\ql "
		pr #255: hd1$
	return  ! /r
	PT_FINIS: ! 
	! r: totals
	pr #255,using "form skip 1,pos 10,c 20": "Totals"
	for j=1 to udim(mat alloc)
		pr #255,using "form pos 1,c 20,pic(---,---,---.##)": name$(j),totalalloc(j)
	next j
	pr #255,using "form pos 1,c 20,pic(---,---,---.##)": "Total Amount",totaltamount
	if water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Water Usage",totalusage(1)
	if electric=1 and water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Electric Usage",totalusage(2) ! electric 2nd metered service
	if electric=1 and water=0 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Electric Usage",totalusage(1) ! electric is 1st metered service
	if gas=1 and electric=1 and water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(3) ! gas is third service
	if gas=1 and electric=0 and water=1 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(2) ! gas is second metered service
	if gas=1 and electric=0 and water=0 then pr #255,using "form pos 1,c 20,pic(---,---,---)": "Gas Usage",totalusage(1) ! gas is first  metered service
	pr #255,using "form skip 1,pos 1,cr 18,pic(-,---,---,--#.##)": "Current Balance:",account_balance
	! /r
	PT_NO_CUSTOMER: ! 
	close #trans: ioerr ignore
	fncloseprn
	PT_XIT: ! 
fnend
def fn_getPenaltyIndex(mat serviceName$;___,pindex,moveone)
	! this function returns the slot for penalty
	for pindex=1 to udim(mat serviceName$)
		if trim$((serviceName$(pindex)))="" then let moveone+=1
		if trim$(uprc$(serviceName$(pindex)))="PENALTY" then
			let fn_getPenaltyIndex=pindex-moveone
		end if 
	next pindex
fnend 
def fn_flextran(myline,mypos; hTrans,z$,begdate,enddate,selcode)

	dim colmask$(30),colhdr$(30)*20,item$(25)*70,tg(11)
	dim srv$(10)*2,serviceName$(10)*20

	if hTrans=0 then 
		close_hTrans=1
		open #hTrans:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,input,keyed 
	end if 
	hTrans_lrec_len=len(str$(lrec(hTrans)))
	fn_columnGet(mat colhdr$,mat colmask$,ftShowElecUsed,ftShowGasUsed)
	fn_columnEnabledGet(mat colEnabled) 
	forceAllColumnsOn=0
	if forceAllColumnsOn then mat colEnabled(25) : mat colEnabled=(1)
	if trim$(z$)="[All]" then 
		z$=""
		colEnabled(2)=1
	else if trim$(z$)<>"" then 
		z$=lpad$(trim$(z$),10)
		colEnabled(2)=0
	end if
	dim colHdr_enabled$(0)*20
	dim colMask_enabled$(0)
	colHeaderEnabledCount=0
	for hdrItem=1 to headerCount
		if colEnabled(hdrItem) then
			colHeaderEnabledCount+=1
			mat colHdr_enabled$(colHeaderEnabledCount)
			mat colMask_enabled$(colHeaderEnabledCount)
			colHdr_enabled$(colHeaderEnabledCount)=colhdr$(hdrItem)
			colMask_enabled$(colHeaderEnabledCount)=colmask$(hdrItem)
		end if
	nex hdrItem
	if trim$(z$)='' then 
		restore #hTrans: 
	else 
		restore #hTrans,key>=lpad$(z$,10)&"         ": nokey FlexTranFinis
	end if 
	fnflexinit1("ubtrans_b",myline,mypos,25,100,mat colHdr_enabled$,mat colMask_enabled$,1)
	do
		READ_UBTRANSVB: ! 
		read #hTrans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': transAcct$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexTranFinis
		if lpad$(transAcct$,10)<>lpad$(z$,10) and trim$(z$)<>'' then goto FlexTranFinis ! .     ! not same account
		if selcode>1 and tcode<>selcode-1 then goto READ_UBTRANSVB
		if begdate>20000000 and tdate<begdate then goto READ_UBTRANSVB
		if enddate>20000000 and tdate>enddate then goto READ_UBTRANSVB
		! if tcode=0 then tcode=1 ! temporary to prevent bad transaction codes
		items=0
		item$(items+=1)=lpad$(str$(rec(hTrans)),hTrans_lrec_len,'0')
		if colEnabled(2) then
			 item$(items+=1)=transAcct$
		end if
		item$(items+=1)=str$(tdate)
		if tcode<1 or tcode>udim(mat transType$) then 
			item$(items+=1)='(invalid)'
		else
			item$(items+=1)=transType$(tcode)
		end if
		item$(items+=1)=str$(tamount)
		colEnabledItem=items ! +1
		for j=1 to 10
			if j=3 and ftShowElecUsed=1 then goto L440
			if j=4 and ftShowGasUsed=1 then goto L440
			if trim$(serviceName$(j))<>"" then 
				if colEnabled(colEnabledItem+=1) then
					! pr colhdr$(colEnabledItem) : pause
					item$(items+=1)=cnvrt$("pic(-------.zz)",tg(j))
				end if 
			end if 
			L440: ! 
		next j
		if colEnabled(colEnabledItem+=1) then
			! pr colhdr$(colEnabledItem) : pause
			item$(items+=1)=cnvrt$("pic(-------.zz)",tg(11)) ! net
		end if
		if trim$(serviceName$(1))<>"" then 
			if colEnabled(colEnabledItem+=1) then
				! pr colhdr$(colEnabledItem) : pause
				item$(items+=1)=str$(wr)
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(wu)
			end if
		end if 
		if trim$(serviceName$(3))="Electric" or trim$(srv$(3))="EL" then 
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(er)
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(eu)
			end if
		end if 
		if trim$(serviceName$(3))="Lawn Meter" then 
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(er)
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(eu)
			end if
		end if 
		if trim$(serviceName$(4))="Gas" or trim$(srv$(4))="GA" then 
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(gr) 
			end if
			if colEnabled(colEnabledItem+=1) then
				item$(items+=1)=str$(gu)
			end if
		end if 
		if colEnabled(colEnabledItem+=1) then
			item$(items+=1)=str$(tbal)
		end if
		fnflexadd1(mat item$) 
	loop
	FlexTranFinis: ! 
	if close_hTrans=1 then close #hTrans: : close_hTrans=0
fnend 
def fn_columnGet(mat colhdr$,mat colmask$,&ftShowElecUsed,&ftShowGasUsed)
	mat colhdr$(30)
	mat colmask$(30)
	colhdr$(1)="Rec"
	colhdr$(2)="Account"
	colhdr$(3)="Date"
	colhdr$(4)="Type"
	colhdr$(5)="Amount"
	colmask$(1)=""
	colmask$(2)=""
	colmask$(3)="3"
	colmask$(4)=""
	colmask$(5)="10"
	headerCount=5
	if trim$(serviceName$(3))<>"Electric" and srv$(3)="EL" then ftShowElecUsed=1
	if trim$(serviceName$(4))<>"Gas" and srv$(4)="GA" then ftShowGasUsed=1
	for j=1 to 10
		if j=3 and ftShowElecUsed=1 then goto L220
		if j=4 and ftShowGasUsed=1 then goto L220
		if trim$(serviceName$(j))<>"" then 
			colhdr$(headerCount+=1)=trim$(serviceName$(j))(1:min(8,len(trim$(serviceName$(j)))))
			colmask$(headerCount)="10"
		end if 
		L220: ! 
	next j
	colhdr$(headerCount+=1)="Net" : colmask$(headerCount)="10"
	for j=1 to 4
		if trim$(serviceName$(j))<>"" and j=1 then 
			colhdr$(headerCount+=1)="Water Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Water Used"
			colmask$(headerCount)="20"
		end if 
		if trim$(serviceName$(j))="Electric" and j=3 then 
			colhdr$(headerCount+=1)="Elec Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Elec Used"
			colmask$(headerCount)="20"
		else if trim$(srv$(j))="EL" and j=3 then 
			colhdr$(headerCount+=1)=" 2nd Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)=" 2nd Used"
			colmask$(headerCount)="20"
		end if 
		if trim$(serviceName$(j))="Lawn Meter" and j=3 then 
			colhdr$(headerCount+=1)="Lawn Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Lawn Used"
			colmask$(headerCount)="20"
		end if 
		if uprc$(trim$(serviceName$(j)))="GAS" and j=4 then 
			colhdr$(headerCount+=1)="Gas Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="Gas Used"
			colmask$(headerCount)="20"
		else if uprc$(trim$(srv$(j)))="GA" and j=4 then 
			colhdr$(headerCount+=1)="3nd Reading"
			colmask$(headerCount)="20"
			colhdr$(headerCount+=1)="3rd Used"
			colmask$(headerCount)="20"
		end if 
	next j
	colhdr$(headerCount+=1)="Balance"
	colmask$(headerCount)="10"
	mat colhdr$(headerCount)
	mat colmask$(headerCount)
fnend
def fn_columnEnabledGet(mat colenabled) ! requires local: headerCount
	mat colenabled(headerCount)
	mat colenabled=(0)
	for hdrItem=1 to 5
		colenabled(hdrItem)=1
	nex hdrItem
	for hdrItem=6 to headerCount
		fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',tmp$,'True')
		! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&tmp$
		if tmp$='True' then 
			colenabled(hdrItem)=1
		end if
	nex hdrItem
! pause
fnend
def fn_columnSelect
	dim csHeader$(30)*20
	fnTos(sn$='ubTrColSel') : respc=0 : csLine=0
	fn_columnGet(mat csHeader$,mat unusedColMask$,unusedShowElecUsed,unusedShowGasUsed)
	for hdrItem=6 to udim(mat csHeader$)
		fnChk(csLine+=1,25,csHeader$(hdrItem), 1)
		fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1),'True')
		! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(respc)
	nex hdrItem
! pause
	fnCmdSet(4)
	fnAcs(mat resp$,ckey)
	if ckey<>5 then
		respc=0
		for hdrItem=6 to udim(mat csHeader$)
			fncreg_write('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1))
			! pr 'wrote: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(hdrItem)
		nex hdrItem
	end if
! pause
fnend
include: Ertn
include: fn_open

