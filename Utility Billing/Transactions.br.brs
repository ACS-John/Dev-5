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
		gosub Enum

		tcode_charge      	=1
		tcode_penalty     	=2
		tcode_collection  	=3
		tcode_credit      	=4
		tcode_debit       	=5

		dim transType$(5)*11
		transType$(tcode_charge    )='Charge'
		transType$(tcode_penalty   )='Penalty'
		transType$(tcode_collection)='Collection'
		transType$(tcode_credit    )='Credit Memo'
		transType$(tcode_debit     )='Debit Memo'

		dim serviceName$(10)*20
		dim srv$(10)*2
		fnGetServices(mat serviceName$, mat srv$)
		for trimItem=1 to udim(mat serviceName$) : serviceName$(trimItem)=trim$(serviceName$(trimItem)) : next trimItem

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
			open #ttao_h_trans=fnH: 'Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr',i,i,k
			dim ttao_key$*19
		end if
		ttao_return=0
		customer_key$=lpad$(customer_key$,10)
		! ttao_key$=rpad$(customer_key$,kln(ttao_h_trans))
		ttao_key$=customer_key$&lpad$(str$(date_ccyymmdd),8)&cnvrt$('pic(z)',trans_type)
		restore #ttao_h_trans,key>=ttao_key$: nokey TTAO_FINIS
		do
			read #ttao_h_trans,using 'form pos 1,c 10,N 8,N 1,pd 4.2': ttao_customer_read$,ttao_date,ttao_code,ttao_amount eof TTAO_FINIS
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
def fn_transfile(; hact$*81,&bal,mat gb,___,rc,cf,opt_back,resp_type1,resp_type2,resp_type3,resp_type4, _
	resp_type5,resp_type6)

	enableDelete=0 !
	if env$('acsDeveloper')<>'' then enableDelete=1

	dim resp$(10)*80
	dim totalalloc(10),totalusage(3),usage(3)
	if ~beg_date then beg_date=date('mm')*10000+100+date('yy')-1

	ScreenAskFilters: ! r:
	fnTos
	rc=cf=0
	fnFra(1,1,6,23,'Transaction Type','You can review all transactions or any specific type of transaction',0)
	cf+=1 : fratype=cf
	dim tf$(2)
	tf$(1)='False'
	tf$(2)='True'
	fnOpt(1,3,'[All]',0,fratype)      	: resp_type1=rc+=1 : if (selectedType=1 or ~selectedType) then resp$(resp_type1)='True' else resp$(resp_type1)='False'
	fnOpt(2,3,'Charges',0,fratype)    	: resp$(resp_type2=rc+=1)=tf$((selectedType==2)+1) ! if selectedType=2 then resp$(rc+=1)='True' else resp$(rc+=1)='False'
	fnOpt(3,3,'Penalties',0,fratype)  	: resp$(resp_type3=rc+=1)=tf$((selectedType==3)+1)  ! if selectedType=3 then resp$(rc+=1)='True' else resp$(rc+=1)='False'
	fnOpt(4,3,'Collections',0,fratype)	: resp$(resp_type4=rc+=1)=tf$((selectedType==4)+1)  ! if selectedType=4 then resp$(rc+=1)='True' else resp$(rc+=1)='False'
	fnOpt(5,3,'Credit Memos',0,fratype)	: resp$(resp_type5=rc+=1)=tf$((selectedType==5)+1)  ! if selectedType=5 then resp$(rc+=1)='True' else resp$(rc+=1)='False'
	fnOpt(6,3,'Debit Memos',0,fratype)	: resp$(resp_type6=rc+=1)=tf$((selectedType==6)+1)  ! if selectedType=6 then resp$(rc+=1)='True' else resp$(rc+=1)='False'
	fnFra(1,30,3,42,'Date Range','You can transactions for any date range or leave these blank to see all transactions.')
	cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
	fnLbl(1,1,'Starting Date:',mylen,1,0,fradate) 	: fnTxt(1,mypos,10,0,1,'3',0,empty$,fradate) : 	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,'Ending Date:',mylen,1,0,fradate)   	: fnTxt(2,mypos,10,0,1,'3',0,empty$,fradate) : 	resp$(rc+=1)=str$(end_date)
	fnFra(6,30,2,60,'Account','You review transactions for all accounts or for an individual.')
	cf+=1 : fraaccount=cf
	fnLbl(1,1,'Account:',8,1,0,fraaccount)
	if trim$(hact$)<>'' then
		fnTxt(1,10,10,0,1,'',1,'',fraaccount)
		resp$(rc+=1)=hact$
	else
		fncmbact(1,10,1,fraaccount)
		rc+=1
		if resp$(rc)='' then resp$(rc)='[All]'
	end if
	fnCmdKey('Next',1,1,0,'Displays a list of transactions on the screen')
	fnCmdKey('Print',2,0,0,'Prints a transaction listing. (To get totals, you can only select one type of transaction at a time.')
	fnCmdKey('Cancel',5,0,1,'Returns to customer record')
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		goto TfXit
	else
		if resp$(resp_type1)='True' then
			selectedType=1
		else if resp$(resp_type2)='True' then
			selectedType=2
		else if resp$(resp_type3)='True' then
			selectedType=3
		else if resp$(resp_type4)='True' then
			selectedType=4
		else if resp$(resp_type5)='True' then
			selectedType=5
		else if resp$(resp_type6)='True' then
			selectedType=6
		end if
		beg_date=val(resp$(7))
		end_date=val(resp$(8))
		z$=resp$(9)(1:10)
		if ckey=2 then
			! if trim$(z$)<>'' and trim$(z$)<>'[All]'  then
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
		fnTos
		stgFlexLine=0
		fnButton(stgFlexLine+=1,1,'Columns',ck_columns:=6)
		if z$<>'[All]' then
			fnLbl(stgFlexLine+=1,1,'Account:',8,1)
			fnTxt(stgFlexLine,10,10,0,0,'',1)
			resp$(1)=z$
		end if
		fn_flextran(stgFlexLine+=1,1,0,z$,beg_date,end_date,selectedType)
		fnCmdKey('Back',opt_back:=2,0,0,'Return to filter selection')

		if enableDelete then
			fnCmdKey('Delete',optDelete:=8,0,0,'Displays a list of transactions on the screen')
		end if

		fnCmdKey('Edit',opt_edit:=1,1,0)
		fnCmdKey('Print',opt_print:=4,0,0)
		fnCmdKey('Back',5,0,1)
		ckey=fnAcs(mat resp$)
		if ckey=opt_back then
			goto ScreenAskFilters
		else if ckey=5 then
			goto TfXit
		else
			if ck_columns and ckey=ck_columns then
				fn_columnSelect
			else if ckey=opt_print then
				fn_printTrans
			end if
			editrec=0
			if z$='[All]' then editrec=val(resp$(1)) else editrec=val(resp$(2)) conv ScreenTransGrid
			if ckey=opt_edit then
				fn_TransactionEdit(editrec)
			else if enableDelete and ckey=optDelete then
				fn_transactionDelete(editrec,bal,mat gb)
			end if
		end if
	loop ! /r
	TfXit: !
fnend
	def fn_transactionDelete(editrec,&bal,mat gb; ___,tranRec,runningBalance)
	
		dim tran$(0)*128
		dim tranN(0)
		hTran=fn_openFio('UB Transaction',mat tran$,mat tranN)
		dim origTran$(0)*128
		dim origTranN(0)
		mat origTran$(udim(mat tran$))
		mat origTranN(udim(mat tranN))
		open #hTranRelative=fnH: 'name=[Q]\UBmstr\ubTransVB.h[cno],shr',i,outi,r
		dim c$(0)*256
		dim cN(0)
		hCustomer=fn_openFio('UB Customer',mat c$,mat cN)
	
		read #hTranRelative,using form$(hTran),rec=editrec: mat origTran$,mat origTranN noRec TdEoTran
		read #hCustomer,using form$(hCustomer),key=origTran$(trans_acct): mat c$,mat cN ! noKey ignore
	
		if origTranN(trans_tcode)=tcode_charge or origTranN(trans_tcode)=tcode_penalty or origTranN(trans_tcode)=tcode_debit then
			tBalanceModifier=-1*transAmount
		else if origTranN(trans_tcode)=tcode_collection or origTranN(trans_tcode)=tcode_credit then
			tBalanceModifier=   transAmount
		else
			pr bell;program$&': origTranN(trans_tcode) '&str$(origTranN(trans_tcode))&' unrecognized.'
			pause
		end if
	
		customerLastBillingDay=days(cN(c_lastBillingDate),'mmddyy')
		transactionDay=days(origTranN(trans_tdate),'ccyymmdd')
		if customerLastBillingDay=transactionDay then
			mat msgbox$(0)
			fnAddOneC(mat msgBox$,'You may not delete a customer''s most recent Charge.')
			fnAddOneC(mat msgBox$,'To accomplish this use Reverse Billing Cycle for the Individual.')
			fnMsgBox(mat msgbox$,resp$,'',mb_exclamation+mb_okonly)
			goto TdEoTran
		end if
	
		delete #hTranRelative,rec=editrec:
	
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
		! /r
		TdEoTran: !
		fnclosefile(hCustomer,'UB Customer')
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
	def fn_TransactionEdit(editrec; ___,transAcct$,tDate,tcode,tamount,serviceItem,s1use,s3use,s4use,lc,mylen,mypos,rc,s1use,s3use,s4use)
		open #trans=fnH: 'Name=[Q]\UBmstr\ubtransvb.h[cno],Shr',i,outi,r
		read #trans,using 'form pos 1,c 10,N 8,N 1,pd 4.2,pos 73,pd 5,pos 83,pd 5,pos 93,pd 5',rec=editrec: transAcct$,tDate,tcode,tamount,s1use,s3use,s4use
		fnTos
		mylen=20 : mypos=mylen+2
		fnLbl(lc+=1,1,'Record:',mylen)
		fnTxt(lc,mypos,10,0,0,empty$,1)
		resp$(rc+=1)=str$(editrec)
		fnLbl(lc+=1,1,'Customer:',mylen)
		fnTxt(lc,mypos,10,0,0,empty$,1)
		resp$(rc+=1)=transAcct$
		fnLbl(lc+=1,1,'Date:',mylen)
		fnTxt(lc,mypos,10,0,0,'3')
		resp$(respc_tDate:=rc+=1)=str$(tDate)
		fnLbl(lc+=1,1,'Type:',mylen)
		fnTxt(lc,mypos,12,0,0,empty$,1)
		resp$(rc+=1)=transType$(tcode)
		fnLbl(lc+=1,1,'Amount:',mylen)
		fnTxt(lc,mypos,10,0,0,'10',1)
		resp$(rc+=1)=str$(tamount)
		if tcode=1 then !  if it is a charge then
			lc+=1
			rcServiceBase=rc
			for serviceItem=1 to 10
				if (serviceItem=1 or serviceItem=3 or serviceItem=4) and fn_serviceIsMetered(serviceItem) then
					fnLbl(lc+=1, 1,serviceName$(serviceItem)&' Usage:',mylen)
					fnTxt(lc    ,22,10,0,1,'number')
					rc+=1
					if serviceItem=1 then resp$(rc)=str$(s1use)
					if serviceItem=3 then resp$(rc)=str$(s3use)
					if serviceItem=4 then resp$(rc)=str$(s4use)
				end if
			next serviceItem
		end if
	
		fnCmdKey('Save',1,1,0)
		fnCmdKey('Cancel',5,0,1)
		ckey=fnAcs(mat resp$)
		if ckey=1 then
			tDate=val(resp$(respc_tDate))
			if tcode=1 then !  if it is a charge then
				rc=rcServiceBase
				for serviceItem=1 to 10
					if (serviceItem=1 or serviceItem=3 or serviceItem=4) and fn_serviceIsMetered(serviceItem) then
					rc+=1
					if serviceItem=1 then s1use=val(resp$(rc))
					if serviceItem=3 then s3use=val(resp$(rc))
					if serviceItem=4 then s4use=val(resp$(rc))
					end if
				nex serviceItem
			end if
			rewrite #trans,using 'form pos 11,N 8,pos 73,pd 5,pos 83,pd 5,pos 93,pd 5',rec=editrec: tDate,s1use,s3use,s4use
		end if
		close #trans:
	fnend
	dim msgbox$(3)*128
	def fn_printTrans(; ___,hPtTrans1,hPtTrans2) ! very local function - lots of inherritance
		! dim scr1$(10)*30
		dim alloc(10)
		dim r(20,4)
		dim hd1$*255
		dim tg(11)
		dim name$(10)*20
		! r: ask printBalance
		msgbox_default=256
		if env$('client')='White Hall' then msgbox_default=0
		mat msgbox$(3)
		msgbox$(1)='Include balance column?'
		msgbox$(2)='The balances listed were the account balance at the time the transaction completed'
		msgbox$(3)='and will be misleading if transactions were processed out of date sequence.'
		fnMsgBox(mat msgbox$,resp$,'',32+3+msgbox_default)
		if resp$='Cancel' then goto PT_XIT
		if resp$='Yes' then
			printBalance=1
		else
			printBalance=0
		end if
		! /r
		fnopenprn('','Transactions')
		if serviceName$(3)<>'Electric' and srv$(3)='EL' then ptShowElecUsed=1 ! electric readings are being used for a reduction meter
		if serviceName$(4)<>'Gas' and srv$(4)='GA' then ptShowGasUsed=1 ! gas readings are being used for a reduction meter
		if trim$(z$)='[All]' then hd1$='{\ul  Account }     {\ul     Date   }' else hd1$='    {\ul    Date   }'
		sz1=0
		x=0
		for j=1 to 10
			if j=3 and ptShowElecUsed=1 then goto L1010 ! skp heading is electric field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
			if j=4 and ptShowGasUsed=1 then goto L1010 ! skp heading is gas field is used to hold other readings w/o matching changes (eg Kimberling City as reduction meters)
			x2=pos(serviceName$(j),' ',1)
			if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&'-'&serviceName$(j)(x2+1:len(serviceName$(j))) ! if service name two words long, use part of both
			if (serviceName$(j))<>'' then
				sz1+=1 ! scr1$(sz1+=1)=serviceName$(j)
				hd1$&='  {\ul '&lpad$(rtrm$(serviceName$(j)(1:6)),6)&'}' : name$(x+=1)=serviceName$(j)
			end if  ! (serviceName$(j))<>'' then
			L1010: !
		next j
		hd1$&='{\ul     Total}'
		if printBalance then
			hd1$&='  {\ul   Balance }'
		else
			if serviceName$(1)='Water' then
				hd1$&='  {\ul   Wa Used }'
				water=1
			end if
			if serviceName$(3)='Electric' then
				hd1$&='  {\ul   El Used }'
				electric=1
			else if ptShowElecUsed=1 then
				hd1$&='  {\ul      Used }'
				electric=1
			end if
			if (serviceName$(4))='Gas' then
				hd1$&='  {\ul   Ga Used }'
				gas=1
			else if ptShowGasUsed=1 then
				hd1$&='  {\ul      Used }'
				gas=1
			end if
		end if
		! mat scr1$(sz1)
		mat alloc(sz1) : mat totalalloc(sz1)
		mat totalalloc=(0) : mat totalusage=(0) : totaltamount=0
		mat totalalloc=(0) : mat totalusage=(0) : totaltamount=0
		! close #trans: ioerr ignore
		open #hPtTrans1=fnH: 'Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr',i,outIn,k
		open #hPtTrans2=fnH: 'Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr',i,outIn,k
		if trim$(z$)='[All]' then
			restore #hPtTrans1:
		else
			dim nam$*30
			dim metraddr$*30
			nam$            	=    	fnCustomerData$(lpad$(rtrm$(z$),10),'name'         	, 1)
			metraddr$       	=    	fnCustomerData$(lpad$(rtrm$(z$),10),'Meter Address'	   )
			account_balance	=val(	fnCustomerData$(lpad$(rtrm$(z$),10),'balance'      	, 1))
			! open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,i,k
			! read #hCustomer,using 'form pos 11,c 30,C 28,pos 292,PD 4.2',key=lpad$(rtrm$(z$),10),release: metraddr$,nam$,account_balance nokey PT_NO_CUSTOMER
			! close #hCustomer: ioerr ignore
			restore #hPtTrans1,key>=lpad$(rtrm$(z$),10)&'         ': nokey PT_FINIS
		end if
		gosub HDR
		do
			PtNextTrans: !
			read #hPtTrans1,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': transAcct$,tDate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PT_FINIS
			if trim$(z$)<>'[All]' and transAcct$<>lpad$(rtrm$(z$),10) then goto PT_FINIS
			if beg_date<>0 and tDate<beg_date then goto PtNextTrans
			if end_date<>0 and tDate>end_date then goto PtNextTrans
			if tamount=0 then goto PtNextTrans
			if selectedType>1 and tcode<>selectedType-1 then goto PtNextTrans
			if tcode=tcode_collection 	then ti2=1 ! REG.COLLECTION
			if tcode=tcode_credit     	then ti2=2 ! CREDIT MEMO
			if tcode=tcode_debit       	then ti2=3 ! DEBIT MEMO
			! if tcode=tcode_penalty then pause
			! if trim$(transAcct$)='100145.00' then pr transAcct$;tg(4) : pause
			if ti2=3 then r(1,1)-=tamount else r(1,1)+=tamount
			r(1,ti2+1)+=tamount
			x=0
			for j=1 to 10
				if (serviceName$(j))<>'' then
					if j=3 and srv$(3)='EL' and serviceName$(3)<>'Electric' and serviceName$(3)<>'Lawn Meter' then ! electic being used for reduction meter
						goto L1370
					else if j=4 and srv$(4)='GA' and serviceName$(4)<>'Gas' then ! gas being used for reduction meter
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
			cx$=' '
			if tcode=tcode_charge     then cx$='CHG'
			if tcode=tcode_penalty    then cx$='PN'
			if tcode=tcode_collection then cx$='COL'
			if tcode=tcode_credit     then cx$='CM'
			if tcode=tcode_debit      then cx$='DM'
			service=0
			if water=1      	then service+=1 : usage(service)=wu ! water
			if electric=1   	then service+=1 : usage(service)=eu ! Electric
			if gas=1        	then service+=1 : usage(service)=gu ! Gas
			dim printlineform$*1024
			if cx$='CHG' then
				printlineform$='c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*c 8,n 10.2,3*pic(--------.--),x 1'
				! build string alloc$ and set penalty to ''
				mat alloc$(udim(alloc)) : mat alloc$=('')
				for counter=1 to udim(alloc)
					alloc$(counter)=str$(alloc(counter))
					if pos(alloc$(counter),'.')=0 then alloc$(counter)=alloc$(counter)&'.00'
					if len(alloc$(counter))-pos(alloc$(counter),'.')<2 then alloc$(counter)=alloc$(counter)&'0'
					if len(alloc$(counter))-pos(alloc$(counter),'.')<2 then alloc$(counter)=alloc$(counter)&'0' ! get both decimals if needed
					alloc$(counter)=lpad$(alloc$(counter),8,' ')
					next counter
				penaltyindex=fn_getPenaltyIndex(mat serviceName$)
				if penaltyindex<>0 then alloc$(penaltyindex)=''
			else
				printlineform$='c 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 8.2,n 10.2,3*pic(--------.--),x 1'
			end if
			if printBalance then
				usage(1)=tbal
			end if
			! if env$('acsDeveloper')<>'' then pause
			if trim$(z$)='[All]' then
				if cx$='CHG' then
					pr #255,using 'form pos 1,c 10,x 1,'&printlineform$: transAcct$,cx$,tDate,mat alloc$,tamount,usage(1),usage(2),usage(3) pageoflow PgOf
				else
					pr #255,using 'form pos 1,c 10,x 1,'&printlineform$: transAcct$,cx$,tDate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PgOf
				end if
			else
				if cx$='CHG' then
					pr #255,using 'form pos 1,'&printlineform$: cx$,tDate,mat alloc$,tamount,usage(1),usage(2),usage(3) pageoflow PgOf
				else
					pr #255,using 'form pos 1,'&printlineform$: cx$,tDate,mat alloc,tamount,usage(1),usage(2),usage(3) pageoflow PgOf
				end if
			end if  ! trim$(z$)='[All]'   /   else
			if tcode=tcode_charge     then
				mat totalalloc=totalalloc+alloc: totaltamount+=tamount  ! charges
				mat totalusage=totalusage+usage
				! reverse penaltyindex charges
				if penaltyindex<>0 then totalalloc(penaltyindex)-=alloc(penaltyindex)
			end if
			if tcode=tcode_penalty    	then mat totalalloc=totalalloc+alloc : totaltamount+=tamount ! penalties
			if tcode=tcode_collection 	then mat totalalloc=totalalloc-alloc : totaltamount-=tamount ! collections
			if tcode=tcode_credit     	then mat totalalloc=totalalloc-alloc : totaltamount-=tamount ! credit memos
			if tcode=tcode_debit       	then mat totalalloc=totalalloc+alloc : totaltamount+=tamount ! debit memos
			!if env$('acsDeveloper')<>'' then
			!	if lamt2<>totaltamount then
			!		lamt2=totaltamount
			!		print totaltamount
			!		pause
			!	end if
			!end if
		loop
		PgOf: ! r:
			pr #255: newpage
			gosub HDR
		continue  ! /r
		HDR: ! r:
		! need date$,time$
			pr #255: '\qc  {\f181 \fs20 \b '&env$('cnam')&' }'
			if nam$<>'' then
				pr #255: '\qc  {\f181 \fs20 \b '&trim$(nam$)&' }'
			end if
			if metraddr$<>'' then
				pr #255: '\qc  {\f181 \fs20 \b '&trim$(metraddr$)&' }'
			end if
			pr #255: '\qc  {\f181 \fs20 \b '&trim$(z$)&' }'
			pr #255: '\qc  {\f181 \fs28 \b Transaction List }'
			if beg_date<>0 and end_date<>0 then
				pr #255: '\qc  {\f181 \fs18 \b From '&cnvrt$('pic(zzzz/zz/zz)',beg_date)& '  To '&cnvrt$('pic(zzzz/zz/zz)',end_date)&'}'
			end if  ! beg_date<>0 and end_date<>0
			pr #255: ''
			pr #255: '\ql '
			pr #255: hd1$
		return  ! /r
		PT_FINIS: !
		! r: totals
		pr #255,using 'form skip 1,pos 10,c 20': 'Totals'
		for j=1 to udim(mat alloc)
			pr #255,using 'form pos 1,c 20,pic(---,---,---.##)': name$(j),totalalloc(j)
		next j
		pr #255,using 'form pos 1,c 20,pic(---,---,---.##)': 'Total Amount',totaltamount
		if water=1 then pr #255,using 'form pos 1,c 20,pic(---,---,---)': 'Water Usage',totalusage(1)
		if electric=1 then
			if water=1 then
				pr #255,using 'form pos 1,c 20,pic(---,---,---)': 'Electric Usage',totalusage(2) ! electric 2nd metered service
			else ! if ~water then
				pr #255,using 'form pos 1,c 20,pic(---,---,---)': 'Electric Usage',totalusage(1) ! electric is 1st metered service
			end if
		end if
		if gas=1 then
			if electric=1 and water=1 then
				pr #255,using 'form pos 1,c 20,pic(---,---,---)': 'Gas Usage',totalusage(3) ! gas is third service
			else if electric=0 and water=1 then
				pr #255,using 'form pos 1,c 20,pic(---,---,---)': 'Gas Usage',totalusage(2) ! gas is second metered service
			else if electric=0 and water=0 then
				pr #255,using 'form pos 1,c 20,pic(---,---,---)': 'Gas Usage',totalusage(1) ! gas is first  metered service
			end if
		end if
		pr #255,using 'form skip 1,pos 1,cr 18,pic(-,---,---,--#.##)': 'Current Balance:',account_balance
		! /r
		PT_NO_CUSTOMER: !
		close #hPtTrans1: ioerr ignore
		close #hPtTrans2: ioerr ignore
		fncloseprn
		PT_XIT: !
	fnend
	def fn_getPenaltyIndex(mat serviceName$;___,pindex,moveone)
		! this function returns the slot for penalty
		for pindex=1 to udim(mat serviceName$)
			if trim$((serviceName$(pindex)))='' then moveone+=1
			if trim$(uprc$(serviceName$(pindex)))='PENALTY' then
				fn_getPenaltyIndex=pindex-moveone
			end if
		next pindex
	fnend
	
	def fn_flextran(myline,mypos; hTrans,z$*10,begDate,endDate,selcode,___,recMaxLen,hTransOpened,forceEnableAccountCol, _
		transAcct$,tDate,tcode,tamount,wr,wu,er,eu,gr,gu,tbal,pcode, _
		items,colEnabledItem,j)
	
		if ~hTrans then
			open #hTrans=fnH: 'Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr',i,i,k
			hTransOpened=1
		end if
		recMaxLen=len(str$(lrec(hTrans)))
	
		if trim$(z$)<>'[All]' and trim$(z$)<>'' then 	! scanning all accounts
			z$=lpad$(trim$(z$),10)
		else                                            	! scanning only one account
			z$=''
			forceEnableAccountCol=1 
		end if
		fn_columnEnabledRead(mat colEnabled,forceEnableAccountCol)
		dim colHdr_enabled$(0)*20
		dim colMask_enabled$(0)
		enavledHeaderCount=fn_getEnabledHeader(mat colHdr_enabled$,mat colMask_enabled$,mat colEnabled)
		fnflexinit1('ubtrans_b',myline,mypos,25,100,mat colHdr_enabled$,mat colMask_enabled$,1)
	
		if trim$(z$)='' then
			restore #hTrans:
		else
			restore #hTrans,key>=lpad$(z$,10)&'         ': nokey FlexTranFinis
		end if
		do
			TrReadTrans: !
			dim tg(11)
			read #hTrans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': transAcct$,tDate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof FlexTranFinis
			if lpad$(transAcct$,10)<>lpad$(z$,10) and trim$(z$)<>'' then goto FlexTranFinis !       ! not same account
			if selcode>1 and tcode<>selcode-1 then goto TrReadTrans
			if (begDate>20000000 and tDate<begDate) or (endDate>20000000 and tDate>endDate) then goto TrReadTrans
			! if tcode=0 then tcode=1 ! temporary to prevent bad transaction codes
			items=colEnabledItem=0
			fn_itemAddC(					lpad$(str$(rec(hTrans)),recMaxLen,'0')     	) ! item$(1)= record number
			fn_itemAddC(					transAcct$                                    	) ! item$(2orNOT) Account 
			fn_itemAddN(					tDate                                          	) ! item$(2or3)= transaction Date
			
			fn_itemAddC(					fn_transTypeDescription$(tcode)            	) ! item$(3or4)= transaction Type
			
			fn_itemAddN(					tamount                                        	) ! item$(4or5)= transaction Amount
			! colEnabledItem=items     must already be
			for j=1 to 10
				if (j=3 or j=4) and ftShowElecUsed=1 then goto Ft_J3or4FtPass
				if (serviceName$(j))<>'' then
					fn_itemAddC(			cnvrt$('pic(-------.zz)',tg(j))   )
				end if
				Ft_J3or4FtPass: !
			next j
			fn_itemAddC(					cnvrt$('pic(-------.zz)',tg(11))	) ! net
			if (serviceName$(1))<>'' then
				fn_itemAddN(				wr	)
				fn_itemAddN(				wu	)
			end if
			if (serviceName$(3))='Electric' or srv$(3)='EL' then
				fn_itemAddN(				er	)
				fn_itemAddN(				eu	)
			end if
			if (serviceName$(3))='Lawn Meter' then
				fn_itemAddN(				er	)
				fn_itemAddN(				eu	)
			end if
			if (serviceName$(4))='Gas' or srv$(4)='GA' then
				fn_itemAddN(				gr	)
				fn_itemAddN(				gu	)
			end if
			fn_itemAddN(					tbal	)
			if enavledHeaderCount<>items then pr 'something is off' : pause
			fnflexadd1(mat item$)
		loop
		FlexTranFinis: !
		if hTransOpened then close #hTrans: ioerr ignore
	fnend
		def fn_transTypeDescription$*11(tcode; ___,return$*11)
			return$='(invalid)'
			if tcode and tcode<=udim(mat transType$) then return$=transType$(tcode)
			fn_transTypeDescription$=return$
		fnend
		def fn_itemAddC(newItem$*64) ! very local to above
			! updates local: colEnabledItem,items,mat items$
			colEnabledItem+=1
			if colEnabled(colEnabledItem) then 
				items+=1
				item$(items)=newItem$
			end if
			fn_itemAddC=items
		fnend
		def fn_itemAddN(newItemN) ! very local to above
			fn_itemAddN=fn_itemAddC(  str$(newItemN)  )
		fnend
		def fn_getEnabledHeader(mat colHdr_enabled$,mat colMask_enabled$,mat colEnabled; ___,returnN,hdrItem,headerCount)
			dim colhdr$(30)*20
			dim colmask$(30)
			headerCount=fn_columnGetAll(mat colhdr$,mat colmask$,ftShowElecUsed,ftShowGasUsed)
			dim item$(25)*70
		
			mat colHdr_enabled$ (0)
			mat colMask_enabled$(0)
			for hdrItem=1 to headerCount
				if colEnabled(hdrItem) then
					returnN+=1
					fnAddOneC(mat colHdr_enabled$ ,colhdr$(hdrItem) )
					fnAddOneC(mat colMask_enabled$,colmask$(hdrItem))
				end if
			nex hdrItem
			fn_getEnabledHeader=returnN
		fnend
		def fn_columnGetAll(mat colhdr$,mat colmask$,&ftShowElecUsed,&ftShowGasUsed; ___,returnN)
			mat colhdr$(30)
			mat colmask$(30)
			colhdr$(1)='Rec'
			colhdr$(2)='Account'
			colhdr$(3)='Date'
			colhdr$(4)='Type'
			colhdr$(5)='Amount'
			colmask$(1)=''
			colmask$(2)=''
			colmask$(3)='3'
			colmask$(4)=''
			colmask$(5)='10'
			returnN=5
			if (serviceName$(3))<>'Electric' and srv$(3)='EL' then ftShowElecUsed=1
			if (serviceName$(4))<>'Gas' and srv$(4)='GA' then ftShowGasUsed=1
			for j=1 to 10
				if (j=3 or j=4) and ftShowElecUsed=1 then goto Cg_J3or4FtPass
				if (serviceName$(j))<>'' then
					colhdr$(returnN+=1)=(serviceName$(j))(1:min(8,len((serviceName$(j)))))  : colmask$(returnN)='10'
				end if
				Cg_J3or4FtPass: !
			next j
			colhdr$(returnN+=1)='Net'              	: colmask$(returnN)='10'
		
			if (serviceName$(1))<>'' then
				colhdr$(returnN+=1)='Water Reading' 	: colmask$(returnN)='20'
				colhdr$(returnN+=1)='Water Used'   	: colmask$(returnN)='20'
			end if
			if (serviceName$(3))='Electric' then
				colhdr$(returnN+=1)='Elec Reading' 	: colmask$(returnN)='20'
				colhdr$(returnN+=1)='Elec Used'    	: colmask$(returnN)='20'
			else if srv$(3)='EL' then
				colhdr$(returnN+=1)=' 2nd Reading' 	: colmask$(returnN)='20'
				colhdr$(returnN+=1)=' 2nd Used'    	: colmask$(returnN)='20'
			end if
			if (serviceName$(3))='Lawn Meter' then
				colhdr$(returnN+=1)='Lawn Reading' 	: colmask$(returnN)='20'
				colhdr$(returnN+=1)='Lawn Used'    	: colmask$(returnN)='20'
			end if
			if uprc$((serviceName$(4)))='GAS' then
				colhdr$(returnN+=1)='Gas Reading'  	: colmask$(returnN)='20'
				colhdr$(returnN+=1)='Gas Used'      	: colmask$(returnN)='20'
			else if uprc$(srv$(4))='GA' then
				colhdr$(returnN+=1)='3nd Reading'  	: colmask$(returnN)='20'
				colhdr$(returnN+=1)='3rd Used'      	: colmask$(returnN)='20'
			end if
			colhdr$(returnN+=1)='Balance'          	: colmask$(returnN)='10'
		
			mat colhdr$(returnN)
			mat colmask$(returnN)
			fn_columnGetAll=returnN
		fnend
		def fn_columnEnabledRead(mat colEnabled,forceEnable2)
			mat colEnabled(40) ! dynamic size was annoying, let's just go with 40 and move it bigger if it is ever too small
			mat colEnabled=(0)
			for hdrItem=1 to 5
				colEnabled(hdrItem)=1
			nex hdrItem
			for hdrItem=6 to 40
				fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',tmp$,'True')
				! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&tmp$
				if tmp$='True' then
					colEnabled(hdrItem)=1
				end if
			nex hdrItem
		
			if forceEnable2 then colEnabled(2)=1 	else colEnabled(2)=0
			
		fnend
		def fn_columnSelect
			dim csHeader$(30)*20
			fnTos : respc=0 : csLine=0
			headerCount=fn_columnGetAll(mat csHeader$,mat unusedColMask$,unusedShowElecUsed,unusedShowGasUsed)
			for hdrItem=6 to udim(mat csHeader$)
				fnChk(csLine+=1,25,csHeader$(hdrItem), 1)
				fncreg_read('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1),'True')
				! pr 'read: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(respc)
			nex hdrItem
			! pause
			fnCmdSet(4)
			ckey=fnAcs(mat resp$)
			if ckey<>5 then
				respc=0
				for hdrItem=6 to udim(mat csHeader$)
					fncreg_write('Transaction Grid Column '&str$(hdrItem)&' Visible',resp$(respc+=1))
					! pr 'wrote: Transaction Grid Column '&str$(hdrItem)&' Visible:'&resp$(hdrItem)
				nex hdrItem
			end if
			! pause
		fnend

def fn_serviceIsMetered(serviceNumber; ___,returnN)
	if ~setupServiceIsMetered then
		setupServiceIsMetered
		dim serviceCodeMetered$(0)*2
		fnGetServiceCodesMetered(mat serviceCodeMetered$)
	end if
	returnN=max(0,srch(mat serviceCodeMetered$,srv$(serviceNumber)))
	fn_serviceIsMetered=returnN
fnend

include: Enum
include: ertn
include: fn_open

