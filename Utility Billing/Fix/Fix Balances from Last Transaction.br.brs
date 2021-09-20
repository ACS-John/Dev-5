fn_setup

fntop(program$)

dim c$(0)*256
dim cN(0)
hCust=fn_openFio('UB Customer',mat c$,mat cN)
dim t$(0)*256
dim tN(0)
hTran=fn_open('UB Transaction',mat t$,mat tN,mat form$)
hTran2=hTran+1 ! acct/tdate/tcode
fnopenprn
pr #255,us 'form pos 1,8*cr 10': 'account  ','C Balance','T Balance','Diff','Pen Sum','Note     ','LastBill  ','T Type  '
do
	read #hCust,us form$(hCust): mat c$,mat cN eof EoCust
	account$=trim$(c$(c_account))
	if cN(c_finalBilling)=0 or cN(c_finalBilling)=4 then
		customerCount+=1
		! if account$='100101.01' then pause
		fn_getLastTransaction(account$,mat t$,mat tN)
		if tN(trans_tbal)<>cN(c_balance) then
			lastBillDate$=date$(days(cN(c_lastBillingDate),'mmddyy'),'mm/dd/yy')
			diff=cN(c_balance)-tN(trans_tbal)
			! Penalties are Services 5,6,7,10 for findlay
			! penSum=cN(c_s05bill)+cN(c_s06bill)+cN(c_s07bill)+cN(c_s10bill)
			penSum=cN(c_s05breakdown)+cN(c_s06breakdown)+cN(c_s07breakdown)+cN(c_s10breakdown)
			if diff<>penSum then 
				txt$=' DIFF.' 
			else 
				txt$=''
				cN(c_s05breakdown)=0
				cN(c_s06breakdown)=0
				cN(c_s07breakdown)=0
				cN(c_s10breakdown)=0
				cN(c_balance)=tN(trans_tbal)
			end if
			pr #255,us 'form pos 1,c 10,4*N 10.2,3*C 10': account$,cN(c_balance),tN(trans_tbal),diff,penSum,txt$,lastBillDate$,fn_transType$(tN(trans_tcode))
			rewrite #hCust,us form$(hCust): mat c$,mat cN
		end if
	end if
loop
EoCust: !

pr #255: 'test count=';customerCount
fncloseprn
end
def fn_transType$*12(tcode; ___,return$*12)
	if tcode=1 then
		return$='Charge'
	else if tcode=2 then
		return$='Penalty'
	else if tcode=3 then
		return$='Collection'
	else if tcode=4 then
		return$='Credit Memo'
	else if tcode=5 then
		return$='Debit Memo'
	else
		return$='NONE'
	end if
	fn_transType$=return$
fnend
def fn_getLastTransaction(key$*10,mat t$,mat tN; ___,which)
	! fnBuildKey$('UB Transaction',mat t$,mat tN, 2)
	! restore #hTran,last:
	if ~setupTaTlr then ! r: setup mat tranAccount$,mat tranLastRecord
		setupTaTlr=1
		dim tranAccount$(0)*10
		dim tranLastRecord(0)
		mat tranAccount$(0)
		mat tranLastRecord(0)
		tranRec=lrec(hTran)
		do
			mat t$=('')
			mat tN=(0)
			! pr 'reading rec '&str$(tranRec)
			read #hTran,us form$(hTran),rec=tranRec: mat t$,mat tN noRec SetupTaTlrNoRec
			! pr rec(hTran),lrec(hTran)
			which=srch(mat tranAccount$,trim$(t$(trans_acct)))
			if which then
				! do nothing, already got it
			else
				which=udim(mat tranAccount$)+1
				mat tranAccount$(which)
				mat tranLastRecord(which)
				tranAccount$(which)=trim$(t$(trans_acct))
				tranLastRecord(which)=tranRec
				! pr '* adding '&trim$(t$(trans_acct))
			end if
			
			SetupTaTlrNoRec: ! norec on read below lands here
			tranRec-=1
		loop while tranRec>0
	end if
	! /r
	key$=trim$(key$)
	which=srch(mat tranAccount$,key$)
	if which<=0 then 
		which=0
		pr 'never billed customer '&key$
		! pause
		mat t$=('')
		mat tN=(0)
	else
		read #hTran,us form$(hTran),rec=tranLastRecord(which): mat t$,mat tN
	end if
	fn_getLastTransaction=which
fnend
include: fn_setup
include: fn_open
