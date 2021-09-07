on error goto Ertn
autoLibrary
fnTop(program$)
fnStatus('Merging transactions...')
pr ' entered '&program$ : pause
open #hClient=fnH: 'Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr',internal,outIn,keyed
FclientBal: form pos 283,pd 5.2
open #hTrans=fnH: 'Name=S:\Core\Data\acsllc\Transactions.h[cno],Shr',i,outi,r
Ftrans: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
open #hTransBatch=fnH: 'Name=[Temp]\transBatch.[session]',i,outi,r
FtransBatch: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
do ! r: main loop
	ReadNext: !
	dim clientId$*5
	dim inv$*12
	dim tr(6)
	dim id$*20
	read #hTransBatch,using FtransBatch: clientId$,inv$,mat tr,id$ eof Finis
	pr 'read transBatch clientId$='&clientId$ : pause
	if trim$(clientId$)='-1' then
		fnStatus('client=-1 trans detected and skipped')
	else if tr(6)=9 then
		fnStauts('Already processed transaction (tr(6)=9) detected and skipped')
	else
		read #hClient,using FclientBal,key=rpad$(trim$(clientId$),5): cBalance ! nokey NokeyClient 9/5/21 - i want to get errors if they happen here
		inv$=lpad$(rtrm$(inv$),12)

		if tr(5)>=3 or tr(5)=5 then cBalance+=tr(3) else cBalance-=tr(3) ! SIMPLEST WAY
		! if tr(5)>=3 or tr(5)=5 then
		! 	cBalance+-tr(3)
		! else
		! 	cBalance-=tr(3)
		! end if

		tr2=tr(2)
		if tr(5)=3 then tr(3)+=tr2
		tr(2)=tr(3)
		if tr(5)=4 then tr(5)=6
		if tr(5)=3 then tr(5)=4
		if tr(5)=2 then tr(5)=5

		write #hTrans,using Ftrans,reserve: rpad$(trim$(clientId$),5),inv$,mat tr,id$,0
		rewrite #hClient,using FclientBal,key=rpad$(trim$(clientId$),5): cBalance ! nokey NokeyClient

		fnStatus('Updating client '&clientId$&' balance to '&str$(cBalance))
		! fnStatusPause

		if tr(5)=4 then tr(2)=tr2
		if tr(5)=4 then tr(3)=tr(3)-tr2
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
	close #hClient:
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
goto Xit ! /r

Xit: fnXit
include: ertn

