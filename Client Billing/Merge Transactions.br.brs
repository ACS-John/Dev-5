on error goto Ertn
autoLibrary
fnTop(program$)
fn_mergeTrans

goto Xit

def library fnClientBillingMergeTrans
	on error goto Ertn
	autoLibrary
	fnClientBillingMergeTrans=fn_mergeTrans
fnend
def fn_mergeTrans
fnStatus('Merging transactions...')
! pr ' entered '&program$ : pause
dim c$(0)*256
dim cN(0)
hClient=fn_openFio('CO Client',mat c$,mat cN)
! open #hClient=fnH: 'Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr',i,outIn,k
! FclientBal: form pos 283,pd 5.2
! open #hTrans=fnH: 'Name=S:\Core\Data\acsllc\Transactions.h[cno],Shr',i,outi,r
dim to$(0)*64
dim toN(0)
hTrans=fn_openFio('Client Billing Transaction',mat to$,mat toN)
open #hTransBatch=fnH: 'Name=[Temp]\transBatch[acsUserId].h[cno]',i,outi,r
FtransBatch: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
do ! r: main loop
	ReadNext: !
	dim clientId$*5
	read #hTransBatch,using FtransBatch: clientId$,to$(tr_inv),toN(tr_date),toN(tr_amtOrigional),toN(tr_amt),toN(tr_salesmanId),toN(tr_transCode),toN(tr_postCode),to$(tr_desc) eof Finis
	! pr 'read transBatch clientId$='&clientId$ : pause
	if trim$(clientId$)='-1' then
		fnStatus('client=-1 trans detected and skipped')
	else if toN(tr_postCode)=9 then
		fnStatus('Already processed transaction (toN(tr_postCode)=9) detected and skipped')
	else
		read #hClient,using form$(hClient),key=rpad$(trim$(clientId$),5): mat c$,mat cN ! nokey NokeyClient 9/5/21 - i want to get errors if they happen here
		to$(tr_inv)=lpad$(rtrm$(to$(tr_inv)),12)
		
		if toN(tr_transCode)=1 then cN(client_balance)+=toN(tr_amt) ! Invoice
		if toN(tr_transCode)=2 then cN(client_balance)+=toN(tr_amt) ! Finance Charge
		if toN(tr_transCode)=3 then cN(client_balance)+=toN(tr_amt) ! Standard Charge
		if toN(tr_transCode)=4 then cN(client_balance)-=toN(tr_amt) ! Collection
		if toN(tr_transCode)=5 then cN(client_balance)+=toN(tr_amt) ! Debit Memo
		if toN(tr_transCode)=6 then cN(client_balance)-=toN(tr_amt) ! Credit Memo

		if toN(tr_transCode)=3 then toN(tr_amt)=toN(tr_amtOrigional)
		toN(tr_amtOrigional)=toN(tr_amt)

		to$(tr_clientId)=rpad$(trim$(clientId$),5)
		toN(tr_nta)=0
		write #hTrans,using form$(hTrans): mat to$,mat toN
		rewrite #hClient,using form$(hClient),key=to$(tr_clientId): mat c$,mat cN ! nokey NokeyClient

		fnStatus('Updating client '&clientId$&' balance to '&str$(cN(client_balance)))

		rewrite #hTransBatch,using 'form pos 37,n 1': 9
	end if
loop ! /r

NokeyClient: ! r:
	if trim$(clientId$)='' or trim$(clientId$)='0' then
		fnStatus('NoKey on Client Id: '&clientId$)
		fnStatusPause
	else
		clientNokeyCount+=1
		fnopenprn
		pr #255: 'Cannot locate account '&clientId$
		fnStatus('Cannot locate account '&clientId$)
	end if
goto ReadNext ! /r

Finis: ! r:
	fnCloseFile(hClient,'CO Client')
	close #hTrans:
	close #hTransBatch:

	if clientNokeyCount then
		clientNokeyCount=0
		pr #255: ''
		pr #255: 'Set up the '&str$(clientNokeyCount)&' A/R accounts as'
		pr #255: 'indicated on the print-out.  then reenter any '
		pr #255: 'transactions that were rejected.'
		fncloseprn

		fnStatus('Set up the '&str$(clientNokeyCount)&' A/R accounts as' 	)
		fnStatus('indicated on the print-out.  then reenter any '        	)
		fnStatus('transactions that were rejected.'                       	)
		fnStatusPause
	end if
	fnStatusClose
	! /r
fnend


Xit: fnXit
include: fn_open
include: ertn

