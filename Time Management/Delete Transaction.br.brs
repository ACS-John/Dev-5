fn_setup
fnTop(program$)
 
do ! main loop
	fnTos
	dim resp$(64)*128
	if ~mainLoopGridSetup then ! r:
		dim heading$(11)*30,mask$(11)
		heading$(1 )='Rec               ' : mask$(1 )=''
		heading$(2 )='Client Id         ' : mask$(2 )=''
		heading$(3 )='Invoice Number    ' : mask$(3 )=''
		heading$(4 )='Date              ' : mask$(4 )='' ! '2'  2 is (m/d/ccyy)
		heading$(5 )='Origional Amount  ' : mask$(5 )='32'
		heading$(6 )='Amount            ' : mask$(6 )='32'
		heading$(7 )='Salesman Id       ' : mask$(7 )='30'
		heading$(8 )='Transaction Code  ' : mask$(8 )=''
		heading$(9 )='Posting Code      ' : mask$(9 )='30'
		heading$(10)='Inv Description   ' : mask$(10)=''
		heading$(11)='Next Trans Address' : mask$(11)='30'
	end if ! /r
	fnflexinit1('tmTran',3,1,10,10,mat heading$,mat mask$) ! r: draw the flexgrid
	dim trans$(0)*80,transN(0)
	hTrans=fn_open('TM Transaction',mat trans$,mat transN,mat form$, 1)
 
	dim item$(11)*128
	do
		read #hTrans,using form$(hTrans): mat trans$,mat transN eof EoFlexDraw
		item$(1 )=str$(rec(hTrans))
		item$(2 )=fn_clientName$(trans$(tr_clientId))&' ('&trim$(trans$(tr_clientId))&')'
		item$(3 )=     trans$(tr_inv          )
		item$(4 )=date$(days(transN(tr_date),'mmddyy'),'mm/dd/ccyy')
		if  srep$(srep$(item$(4),'0',''),'/','')='' then
			item$(4)='('&str$(transN(tr_date))&')'
		end if
		item$(5 )=str$(transN(tr_amtOrigional ))
		item$(6 )=str$(transN(tr_amt          ))
		item$(7 )=str$(transN(tr_salesmanId   ))
		item$(8 )=fn_transDesc$(transN(tr_transCode))&' ('&str$(transN(tr_transCode))&')'
		item$(9 )=str$(transN(tr_postCode     ))
		item$(10)=     trans$(tr_desc         )
		item$(11)=str$(transN(tr_nta          ))
		fnflexAdd1(mat item$)
	loop
	EoFlexDraw: ! /r
	fnCloseFile(hTrans,'TM Transaction')
 
	fnCmdKey('Delete',ck_delete:=4)
	fnCmdKey('Exit',ck_exit:=5,0,1)
	fnCmdKey('Reassign Next Transaction Addresses',ck_rnta:=6)
	ckey=fnAcs(mat resp$)
	if ckey=ck_exit then
		goto Finis
	else
		selectedRecord=val(resp$(1))
		if ckey=ck_delete then
			gosub DeleteTransaction
			needToRnta=1
		else if ckey=ck_rnta then
			fnReassignNTA('S:\Core\Data\acsllc\Transactions.h[cno]','Form pos 1,C 5','Form Pos 58,PD 3')
			needToRnta=0
		end if
	end if
loop
def fn_transDesc$(transCode; ___,return$,x)
	if ~transDesc_setup then
		transDesc_setup=1
		dim tc$(0)*64
		dim tcN(0)
		hTcode=fn_open('TM Transaction Code',mat tc$,mat tcN,mat form$,1)
		mat transCodes(0)
		mat transDesc$(0)
		do
			read #hTcode,using form$(hTcode): mat tc$,mat tcN eof EoTcode
			fnAddOneN(mat transCodes,tcN(tc_code))
			fnAddOneC(mat transDesc$,trim$(tc$(tc_desc)))
		loop
		EoTcode: !
		fnCloseFile(hTcode,'TM Transaction Code')
	end if
	x=srch(mat transCodes, transCode)
	return$=transDesc$(x)
	fn_transDesc$=return$
fnend
def fn_clientName$*30(clientId$; ___,return$*30,x)
	if ~clientName_setup then
		clientName_setup=1
		dim client$(0)*256
		dim clientN(0)
		hClient=fn_open('TM Client 420',mat client$,mat clientN,mat form$,1)
		dim clientIds$(0)*5
		mat clientIds$(0)
		dim clientNames$(0)*30
		mat clientNames$(0)
		do
			read #hClient,using form$(hClient): mat client$,mat clientN eof EoClientName
			fnAddOneC(mat clientIds$,trim$(client$(client_id)))
			fnAddOneC(mat clientNames$,trim$(client$(client_name)))
		loop
		EoClientName: !
		fnCloseFile(hClient,'TM Client 420')
	end if
	x=srch(mat clientIds$,trim$(clientId$))
	if x<=0 then
		return$=''
	else
		return$=clientNames$(x)
	end if
	fn_clientName$=return$
fnend
 
DeleteTransaction: ! r: requires selectedRecord
	dim mg$(0)*128
	mat mg$(0)
	fnAddOneC(mat mg$,'Would you like to update the Client Balance?')
	fnMsgBox(mat mg$,updateClientBalance$,'',mb_question+mb_yesno)
	hTrans=fn_open('TM Transaction',mat trans$,mat transN,mat form$)
	read #hTrans,using form$(hTrans),rec=selectedRecord: mat trans$,mat transN
	if updateClientBalance$='Yes' then
		hClient=fn_open('TM Client 420',mat client$,mat clientN,mat form$)
		read #hClient,using form$(hClient),key=trans$(tr_clientId),reserve: mat client$,mat clientN
		if transN(tr_transCode)=1 then ! invoice
			clientN(client_balance)-=transN(tr_amt)
		else if transN(tr_transCode)=2 then ! Fincnce Charge
			clientN(client_balance)-=transN(tr_amt)
		else if transN(tr_transCode)=3 then ! Standard Charge
			clientN(client_balance)-=transN(tr_amt)
		else if transN(tr_transCode)=4 then ! Collection
			clientN(client_balance)+=transN(tr_amt)
		else if transN(tr_transCode)=5 then ! Debit Memo
			clientN(client_balance)-=transN(tr_amt)
		else if transN(tr_transCode)=6 then ! Credit Memo
			clientN(client_balance)+=transN(tr_amt)
		else
			pr 'i do not know how to handle this transaction code'
			pause
		end if
		rewrite #hClient,using form$(hClient): mat client$,mat clientN
		fnCloseFile(hClient,'TM Client 420')
	end if
	delete #hTrans,rec=selectedRecord:
	fnCloseFile(hTrans,'TM Transaction')
	
return ! /r
Finis: ! r:
	if needToRnta then
		fnReassignNTA('S:\Core\Data\acsllc\Transactions.h[cno]','Form pos 1,C 5','Form Pos 58,PD 3')
		needToRnta=0
	end if
goto Xit ! /r
 
Xit: fnXit
include: fn_open
include: fn_setup