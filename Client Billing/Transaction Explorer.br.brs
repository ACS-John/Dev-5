! transactions explorer
fn_setup
fnTop(program$)
dim tr$(0)*128
dim trN(0)
hTrans=fn_openFio('Client Billing Transaction',mat tr$,mat trN) ! , 1)
dim coProvider$*128
fnPcReg_Read('filter provider'	,coProvider$, '[All]',1)
dim coClient$*40
fnPcReg_Read('filter client'  	,coClient$  , '[All]',1)
dim coDayStart$
fnPcReg_Read('filter day start'	,coDayStart$, str$(fnEndOfMonth(days(date)-45)+1),1) : coDayStartN=val(coDayStart$)
dim coDayEnd$
fnPcReg_Read('filter day end' 	,coDayEnd$  , str$(fnEndOfMonth(days(date)-15)  ),1) : coDayEndN  =val(coDayEnd$)
dim coType$
fnPcReg_Read('filter type'    	,coType$    , '[All]',1)
do
	fntos : rc=ln=0
	dim resp$(16)*256
	col1pos= 2
	col1len=20
	col2pos=col1len+col1pos+2
	! fnComboFio(lyne,ps,layoutName$*128, whichIndex,limitToListorAddAll)
	fnLbl(ln+=1,1,'Provider:'     , col1len,1) : fnComboFio(ln,col2pos,'CO Provider', 2) : resp$(resp_provider=rc+=1)=trim$(coProvider$)
	fnLbl(ln+=1,1,'Client:'       , col1len,1) : fnComboFio(ln,col2pos,'CO Client'  , 2) : resp$(resp_client=rc+=1)=coClient$
	ln+=1
	fnLbl(ln+=1,1,'Starting Date:', col1len,1) : 	fnTxt(ln,col2pos,10, 0,1,'3',0,'') : resp$(resp_dateStart=rc+=1) 	=date$(coDayStartN,'ccyymmdd')
	fnLbl(ln+=1,1,'Ending Date:'  , col1len,1) : 	fnTxt(ln,col2pos,10, 0,1,'3',0,'') : resp$(resp_dateEnd  =rc+=1) 	=date$(coDayEndN  ,'ccyymmdd')
	ln+=1
	fnLbl(ln+=1,1,'Trans Type:'   , col1len,1) : fnComboFio(ln,col2pos,'Client Billing Transaction Type'    , 2) : resp$(resp_type=rc+=1)=coType$
	ln+=1
	ln+=1
	fnButton(ln+=1,col2pos,'Refresh',ck_refresh=19, 'Reapply the filters')

	ln+=1
	! r: add grid
		dim ch$(0)                 	, colMask$(0)
		mat ch$(0)
		mat colMask$(0)
		fnAddOneC(mat ch$,'Invoice'          	) : fnAddOneC(mat colMask$,''  )
		fnAddOneC(mat ch$,'Date'             	) : fnAddOneC(mat colMask$,'1' )
		fnAddOneC(mat ch$,'Amount'           	) : fnAddOneC(mat colMask$,'10')
		if coType$    ='[All]' then fnAddOneC(mat ch$,'Transaction Type') : fnAddOneC(mat colMask$,'')
		fnAddOneC(mat ch$,'Inv Description' 	) : fnAddOneC(mat colMask$,''  )
		if coClient$  ='[All]' then fnAddOneC(mat ch$,'Client'  	) : fnAddOneC(mat colMask$,''  )
		if coProvider$='[All]' then fnAddOneC(mat ch$,'Provider'	) : fnAddOneC(mat colMask$,''  )

		fnFlexInit1('explorer',ln+=1,col1pos,10,10,mat ch$, mat colMask$)
		restore #hTrans:
		do
			read #hTrans,using form$(hTrans): mat tr$,mat trN eof TransGridAddEoF
			! r: test amount vs Origional Amt	
			if transN(tr_amtOrigional)<>transN(tr_amt) then
				pr 'origional amount DOES NOT EQUAL amount'
				pr '          client=';fnClientName$(trans$(tr_clientId))&' ('&trim$(trans$(tr_clientId))&')'
				pr '             rec=';rec(hTrans)
				pr 'origional amount=';transN(tr_amtOrigional)
				pr '          amount=';transN(tr_amt)
				pause
			end if
			! /r
			if coClient$='[All]' or coClient$=trim$(tr$(tr_clientId)) then
				dim provider$*128
				provider$=trim$(fn_clientProvider$(tr$(tr_clientId)))
				! pr 'coProvider$="'&coProvider$&'"'
				! pr 'provider$="'&provider$&'"'
				! pause
				if coProvider$='[All]' or coProvider$=provider$ then
					dim tranDayN
					tranDayN=days(trN(tr_date),'mmddyy')
					! pr 'coDayStartN=';coDayStartN,date$(coDayStartN,'ccyymmdd')
					! pr 'tranDayN=';tranDayN,date$(tranDayN,'mm/dd/ccyy')
					! if coDayStartN then pause
					if (~coDayStartN or coDayStartN=<tranDayN) and (~coDayEndN or coDayEndN=>tranDayN) then
						if coType$='[All]' or val(coType$(1:1))=trN(tr_transCode) then
							! pr tranDayN;' passed.'
							! r: add record to grid
							dim item$(0)*128
							mat item$(0)
							mat item$=('')
							
							fnAddOneC(mat item$,     tr$(tr_inv          	) 	)
							fnAddOneC(mat item$,str$(trN(tr_date         	))	)
							fnAddOneC(mat item$,str$(trN(tr_amt          	))	)
							if coType$    ='[All]' then fnAddOneC(mat item$,fnTransactionTypeDescription$(trN(tr_transCode))&' ('&str$(trN(tr_transCode))&')')
							fnAddOneC(mat item$,     tr$(tr_desc         	) 	)
							if coClient$  ='[All]' then fnAddOneC(mat item$,fnClientName$(tr$(tr_clientId))&' ('&trim$(tr$(tr_clientId))&')')
							if coProvider$='[All]' then fnAddOneC(mat item$,provider$)
							fnFlexAdd1(mat item$)
							! /r
						end if
					end if
				end if
			end if
		loop
		TransGridAddEoF: !
	! /r
	! fncmdset(14)
	fnCmdKey("Add"   	,ck_add   =1)
	fnCmdKey("Edit"  	,ck_edit  =2,1)
	fnCmdKey("Delete"	,ck_delete=4)
	fnCmdKey("Exit"  	,ck_cancel=5,0,1)
	ckey=fnacs(mat resp$)
	if ckey<>ck_cancel then
		coProvider$ 	=trim$(resp$(resp_provider 	)(1:11)) : fnPcReg_write('filter provider'   	,coProvider$ 	)
		coClient$   	=trim$(resp$(resp_client   	)(1:5) ) : fnPcReg_write('filter client'     	,coClient$    	)
		dim coDayStartN
		coDayStart$	= resp$(resp_dateStart	) : coDayStartN=days(coDayStart$,'ccyymmdd') : fnPcReg_write('filter day start'	,str$(coDayStartN)	)
		dim coDayEndN  
		coDayEnd$   	= resp$(resp_dateEnd  	) : coDayEndN  =days(coDayEnd$  ,'ccyymmdd') : fnPcReg_write('filter day end'  	,str$(coDayEndN)  	)
		coType$     	= resp$(resp_type     	) : fnPcReg_write('filter type' ,coType$)
		if ckey=ck_add then
			fn_transactionAdd(hTrans)
		else if ckey=ck_edit then
			pr 'ck_edit' : pause
		else if ckey=ck_delete then
			pr 'ck_delete' : pause
		! else if ckey=ck_refresh then ! nothing needs to be done, just loop
		! 	pr 'ck_refresh' : pause
		end if
	end if
loop until ckey=ck_cancel
close #hTrans: ioerr ignore
goto Xit


def fn_transactionAdd(hTrans; ___,col1pos,col1len,col2pos,rc,ln, _
	resp_client,resp_type,resp_amt,resp_inv,resp_desc, _
	ck_cancel,ck_save, _
	hClient)
	! inherrits local mat form$,mat c$,mat cN, mat tr$,mat tN,mat resp$
	mat tr$=('')
	mat trN=(0)
	if fn_transactionFm(mat tr$,mat trN) then
		hClient=fn_openFio('CO Client',mat c$,mat cN)
		read #hClient,using form$(hClient),key=tr$(tr_clientId): mat c$,mat cN
		! pr 'id     =';c$(client_id)
		! pr 'type   =';resp$(resp_type)(1:1)
		! pr 'balance before=';cN(client_balance)
		if trN(tr_transCode)=1 or trN(tr_transCode)=2 or trN(tr_transCode)=3 or trN(tr_transCode)=5 then ! Invoice, Finance Charge, Standard Charge, Debit Memo
			cN(client_balance)+=trN(tr_amt)
		else if trN(tr_transCode)=4 or trN(tr_transCode)=6 then ! Collection, Credit Memo
			cN(client_balance)-=trN(tr_amt)
		else
			pr 'ERROR: unrecognized transaction type: ';trN(tr_transCode)
			pause
		end if
		! pr 'balance after=';cN(client_balance)
		! pause
		if trN(tr_transCode)=1 and trim$(tr$(tr_desc))='' then tr$(tr_desc)='CHARGE'
		write #hTrans,using form$(hTrans): mat tr$,mat trN
		rewrite #hClient,using form$(hClient),key=tr$(tr_clientId): mat c$,mat cN
		close #hClient:
	end if
fnend
def fn_transactionEdit(hTrans,transRec; ___,col1pos,col1len,col2pos,rc,ln, _
	resp_client,resp_type,resp_amt,resp_inv,resp_desc, _
	ck_cancel,ck_save, _
	hClient)
	! inherrits local mat form$,mat c$,mat cN, mat tr$,mat tN,mat resp$
	read #hTrans,using form$(hTrans),rec=transRec: mat tr$,mat trN norec TeNoRec
	if fn_transactionFm(mat tr$,mat trN) then
		
	end if
	TeNoRec: !
fnend
	def fn_transactionFm(mat tr$,mat trN; ___,returnN)
		fnToS
		col1pos= 2
		col1len=20
		col2pos=col1len+col1pos+2
		fnLbl(ln+=1,1,'Client:'       , col1len,1) : fnComboFio(ln,col2pos,'CO Client'  , 1) : resp$(resp_client=rc+=1)=tr$(tr_clientId)
		ln+=1
		fnLbl(ln+=1,1,'Transaction Date:', col1len,1) : 	fnTxt(ln,col2pos,10, 0,1,'3',0,'') : resp$(resp_date=rc+=1) 	=str$(trN(tr_date))
		ln+=1
		fnLbl(ln+=1,1,'Trans Type:'   , col1len,1) : fnComboFio(ln,col2pos,'Client Billing Transaction Type', 1) : resp$(resp_type=rc+=1)=str$(trN(tr_transCode))
		ln+=1
		fnLbl(ln+=1,1,'Amount:', col1len,1) : fnTxt(ln,col2pos,10, 0,1,'32',0,'') : resp$(resp_amt=rc+=1)=str$(trN(tr_amt))
		fnLbl(ln+=1,1,'Invoice:', col1len,1) : fnTxt(ln,col2pos,12, 0,1,'',0,'') : resp$(resp_inv=rc+=1)=tr$(tr_inv)
		fnLbl(ln+=1,1,'Description:', col1len,1) : fnTxt(ln,col2pos,20) : resp$(resp_desc=rc+=1)=tr$(tr_desc)
		ln+=1
		fnLbl(ln,1,'', 105) ! 100 is too small
		fnCmdKey('Save',ck_save=1,1)
		fnCmdKey('Cancel',ck_cancel=5,0,1)
		ckey=fnAcs(mat resp$)
		if ckey=ck_save then
			! pause
			mat tr$=('')
			mat trN=(0)
	
			tr$(tr_clientId    	)=resp$(resp_client)(1:5)
			tr$(tr_inv         	 	)=lpad$(resp$(resp_inv),11)
			trN(tr_date        	)=date(days(resp$(resp_date),'ccyymmdd'),'mmddyy')
			trN(tr_amtOrigional	)=val(resp$(resp_amt) )
			trN(tr_amt         	 	)=val(resp$(resp_amt) )
			trN(tr_salesmanId  	)=0
			trN(tr_transCode   	)=val(resp$(resp_type)(1:1))
			trN(tr_postCode    	)=0
			tr$(tr_desc        	)=resp$(resp_desc)
			trN(tr_nta          	)=0
			returnN=1
		else
			returnN=0
		end if
		fn_transactionFm=returnN
	fnend
dim c$(0)*256
dim cN(0)
def fn_clientProvider$*128(client$*64; ___,return$*128,which,hClient)
	if ~clientProvider_setup then ! r:
		clientProvider_setup=1
		hClient=fn_openFio('CO Client',mat c$,mat cN, 1)
		dim cpClientId$(0)*5
		dim cpProvider$(0)*11
		do
			read #hClient,using form$(hClient): mat c$,mat cN eof EoProvider
			fnAddOneC(mat cpClientId$,lwrc$(trim$(c$(client_id))))
			fnAddOneC(mat cpProvider$,trim$(c$(client_provider)))
		loop
		EoProvider: !
		close #hClient:
		! /r
	end if
	which=srch(mat cpClientId$,lwrc$(trim$(client$)))
	if which>0 then
		return$=cpProvider$(which)
	else
		return$='(*bad id*)'
	end if
	! if client$='ajj' then  ! American Jiu Jitsu of Maplewood
	! 	return$='John Bowman'
	! else if client$='4132' or client$='3670' or client$='ped' then  ! Stern and Stern, Recoveries Unlimited and Peter Engler Designs
	! 	return$='Commercial Software Solutions LLC'
	! else
	! 	return$='Advanced Computer Services LLC'
	! end if
	fn_clientProvider$=return$
fnend

Xit: fnXit
include: fn_open
include: fn_setup