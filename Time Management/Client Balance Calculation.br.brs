fn_setup
do
	fnToS
	dim resp$(10)*128
	! (sfn$*100,lbuttonYNe,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,urep,ttt$*200,contain,tabcon)
	fnLbl(1,1,'Client:',lenCol1,1)
	fncombof('',1,12,37,'S:\Core\Data\acsllc\Client.h[cno]',1,5,6,30, 'S:\Core\Data\acsllc\Client-Idx.h[cno]',0)
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		dim selectedClient$*128
		selectedClient$=resp$(1)
		clientKey$=trim$(selectedClient$(1:5))
		fn_DoCalculation(clientKey$)
	end if
loop until ckey=5
goto Xit

def fn_DoCalculation(; justOne$)
	
	! justOne$='960' ! 3045' ! client id or blank for all
	
	dim d$(0)*128
	dim dN(0)
	hTran=fn_open('TM Transaction',mat d$,mat dN,mat form$, 1)
	! r: build mat client$ and mat balance from Transaction file
	mat client$(0)
	mat balance(0)
	do
		read #hTran,using form$(hTran): mat d$,mat dN eof EoHtran
		fnProgressBar(recCount+=1/lrec(hTran))
		d$(tr_clientid)=trim$(d$(tr_clientid))
		isValid=0 : if justOne$='' or d$(tr_clientid)=justOne$ then isValid=1
		if isValid then
			which=srch(mat client$,d$(tr_clientid))
			if which<=0 then
				which=fnAddOneC(mat client$,d$(tr_clientid))
				mat balance(which)
			end if
			if dN(tr_transCode)>=3 or dN(tr_transCode)=5 then balance(which)+=dN(tr_amt) else balance(which)-=dN(tr_amt) ! SIMPLEST WAY
			if justOne$<>'' then
				if ~reportInitialized then
					reportInitialized=1
					fnOpenPrn
					pr #255: 'Clnt  TransCode                 Date              Amount          RunningBal'
				end if
	
				if dN(tr_transCode)=1 then
					tCode$='Invoice'
				else if dN(tr_transCode)=2 then
					tCode$='Finance Charge'
				else if dN(tr_transCode)=3 then
					tCode$='Standard Charge'
				else if dN(tr_transCode)=4 then
					tCode$='Collection'
				else if dN(tr_transCode)=5 then
					tCode$='Debit Memo'
				else if dN(tr_transCode)=6 then
					tCode$='Credit Memo'
				else
					tCode$='!!!UNKNOWN!!!'
				end if
				tCode$&=' ('&str$(dN(tr_transCode))&')'
				pr #255,using 'form pos 1,C 6,C 20,x 2,pic(zz/zz/zz),2*(x 2,pic(---,---,---,--#.##))': d$(tr_clientid),tCode$,dN(tr_date),dN(tr_amt),balance(which)
			end if
		end if
	loop
	EoHtran: !
	close #hTran:
	! /r
	! r: report findings
	if reportInitialized then
		pr #255: '________'
		pr #255: ''
	end if
	dim c$(0)*256
	dim cN(0)
	hClient=fn_open('TM Client 420',mat c$,mat cN,mat form$,1)
	fnOpenPrn
	pr #255,using 'form pos 1,C 6,2*(x 2,Cr 18),x 2,C 40': 'Client','Calculated Balance','Current Balance','Client Name'
	for item=1 to udim(mat client$)
		mat c$=('')
		mat cN=(0)
		c$(client_name)='!!!NO SUCH CLIENT!!!'
		read #hClient,using form$(hClient),key=rpad$(trim$(client$(item)),kln(hClient)): mat c$,mat cN nokey ignore
		if c$(client_name)='!!!NO SUCH CLIENT!!!' or balance(item)<>cN(client_balance) then
			pr #255,using 'form pos 1,C 6,2*(x 2,pic(---,---,---,--#.##)),x 2,C 40': client$(item),balance(item),cN(client_balance),c$(client_name)
		end if
	next item
	close #hClient:
	fnClosePrn
	! /r
fnend
xit: end ! fnXit
include: fn_open
include: fn_Setup