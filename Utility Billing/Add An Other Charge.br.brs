! doFix=1
fn_setup
if doFix then 
	! exec 'copy     "c:\acs\temp\mwCustomer.h1"    "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\Customer.h1"   '
	! exec 'copy     "c:\acs\temp\mwUbTransVb.h1"   "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\UbTransVb.h1"  '
	! exec 'copy     "c:\acs\temp\mwubTrIndx.h1"    "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\ubTrIndx.h1"   '
	! exec 'copy     "c:\acs\temp\mwUBTrdt.h1"      "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\UBTrdt.h1"     '
	! exec 'copy     "c:\acs\temp\mwubIndex.h1"     "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\ubIndex.h1"    '
	! exec 'copy     "c:\acs\temp\mwubIndx2.h1"     "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\ubIndx2.h1"    '
	! exec 'copy     "c:\acs\temp\mwubIndx3.h1"     "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\ubIndx3.h1"    '
	! exec 'copy     "c:\acs\temp\mwubIndx4.h1"     "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\ubIndx4.h1"    '
	! exec 'copy     "c:\acs\temp\mwubIndx5.h1"     "C:\Users\John\OneDrive\ACS\DEV-5D~1\MERRIA~1\ubmstr\ubIndx5.h1"    '
	! pause
	fn_fixBadOnes(0) 
	goto Xit
end if
fn_addAnOtherCharge
Xit: fnXit
def library fnAddAnOtherCharge(;z$*10,hCustomer1)
	if ~setup then fn_setup
	fnAddAnOtherCharge=fn_addAnOtherCharge( z$,hCustomer1)
fnend
def fn_addAnOtherCharge(; z$*10,hCustomer1)
	if ~hCustomer1 then
		dim c$(0)*256,cN(0)
		hCustomer1=fn_openFio('UB Customer',mat c$,mat cN)
		needToCloseHcustomer1=1
	else
		needToCloseHcustomer1=0
	end if
	service_other=fnservice_other
	if service_other<1 or service_other>10 then pr 'service_other is unexpected.';bell : pause
	dim resp$(20)*256
	! fnask_account('AddAnOtherCharge',x$,hCustomer1)
	SCR1:! 
	fnTos(sn$='AddAnOtherCharge')
	col1Len=8
	col2Pos=10
	lc=rc=0
	fnLbl(lc+=1,1,'Account:', col1Len,1) : fnCmbAct(lc,col2Pos)                 : resp$(rc+=1)=z$
	fnLbl(lc+=1,1,'Amount:' , col1Len,1) : fntxt(lc,col2Pos,10, 0,0,'currency') : resp$(rc+=1)=amt$
	fnLbl(lc+=1,1,'Note:'   , col1Len,1) : fntxt(lc,col2Pos,40, 128)            : resp$(rc+=1)=note$
	fnCmdset(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		rc=0
		z$   =lpad$(trim$(resp$(rc+=1)(1:10)),10)
		amt$ =resp$(rc+=1)
		dim note$*256
		note$=resp$(rc+=1)
		! pr 'z$="'&z$&'"' : pause
		read #hCustomer1,using form$(hCustomer1),key=z$: mat c$,mat cN nokey CustomerNokey
		amt=val(amt$)
		if amt<=0 then 
			pr bell;
			dim mesg$(1)*128
			mat mesg$(1)
			mesg$(1)='Amount ('&str$(amt)&') is invalid.'
			fnMsgBox(mat mesg$)
			goto SCR1
		end if
		fn_addNote(z$,amt,note$)
		cN(c_balance)+=amt
		if service_other=1  then
			cN(c_s01breakdown)+=amt
		else if service_other=2  then
			cN(c_s02breakdown)+=amt
		else if service_other=3  then
			cN(c_s03breakdown)+=amt
		else if service_other=4  then
			cN(c_s04breakdown)+=amt
		else if service_other=5  then
			cN(c_s05breakdown)+=amt
		else if service_other=6  then
			cN(c_s06breakdown)+=amt
		else if service_other=7  then
			cN(c_s07breakdown)+=amt
		else if service_other=8  then
			cN(c_s08breakdown)+=amt
		else if service_other=9  then
			cN(c_s09breakdown)+=amt
		else if service_other=10 then
			cN(c_s10breakdown)+=amt
		end if
		fn_addTransaction(z$,service_other,date('ccyymmdd'),amt,cN(c_balance))
		rewrite #hCustomer1,using form$(hCustomer1),key=z$: mat c$, mat cN
	end if
	if needToCloseHcustomer1 then
		fnCloseFile(hCustomer1,'UB Customer')
		hCustomer1=0
		needToCloseHcustomer1=0
	end if
fnend
CustomerNokey: ! r:
	mat mesg$(1)
	mesg$(1)='Account ('&z$&') is invalid.'
	fnMsgBox(mat mesg$)
goto SCR1 ! /r

def fn_addTransaction(z$,serviceCode,tDate,amt,newBalance)
	dim tran$(0)*256
	dim tranN(0)
	hTran=fn_openFio('UB Transaction',mat tran$,mat tranN)
	mat tran$=('')
	mat tranN=(0)
	
	tran$(trans_acct)=z$
	tranN(trans_tdate  )=tDate
	tranN(trans_tcode  )=1         ! 1=Charge,2=Penalty,3=Collection,4=Credit Memo,5=Debit Memo
	tranN(trans_tamount)=amt
	
	
	if serviceCode=1 then
		tranN(trans_tg_1   )=amt
	else if serviceCode=2 then
		tranN(trans_tg_2   )=amt
	else if serviceCode=3 then
		tranN(trans_tG_3   )=amt
	else if serviceCode=4 then
		tranN(trans_tG_4   )=amt
	else if serviceCode=5 then
		tranN(trans_tG_5   )=amt
	else if serviceCode=6 then
		tranN(trans_tG_6   )=amt
	else if serviceCode=7 then
		tranN(trans_tG_7   )=amt
	else if serviceCode=8 then
		tranN(trans_tG_8   )=amt
	else if serviceCode=9 then
		tranN(trans_tG_9   )=amt
	else if serviceCode=10 then
		tranN(trans_TG_10  )=amt
	end if
	tranN(trans_TG_11  )=amt
	tranN(trans_s1read )=0
	tranN(trans_s1use  )=0
	tranN(trans_s3read )=0
	tranN(trans_s3use  )=0
	tranN(trans_s4read )=0
	tranN(trans_tbal   )=newBalance
	tranN(trans_pcode  )=0
	write #hTran,using form$(hTran): mat tran$, mat tranN
	fnclosefile(hTran,'UB Transaction')
fnend
def fn_addNote(z$,amt,note$*256)
	open #h_notefile=fnH: "Name="&fnNoteDir$&"\"&trim$(z$)&".txt,Use",d,o
	pr #h_notefile: '** Other Charge added '&date$('mm/dd/ccyy')&' at '&time$&' **'
	pr #h_notefile:   '  Account: '&z$&'  '&customer_name$
	if fn_not_blank(note$) then
		pr #h_notefile: "     Note: "&note$
	end if
	pr #h_notefile: '**'
	close #h_notefile: 
fnend
def fn_not_blank(nbTestText$*256)
	nbReturn=1
	nbTestText$=srep$(nbTestText$,' ','')
	if nbTestText$='' then
		nbReturn=0
	end if
	fn_not_blank=nbReturn
fnend

def fn_fixBadOnes(askFirst; ___,z$*10,whichDid,tranRec,justFixedIt,service_other,priorBal)
	service_other=fnservice_other
	hTran=fn_openFio('UB Transaction',mat tran$,mat tranN)
	open #hTranRelative=fnH: 'name=[Q]\UBmstr\ubTransVB.h[cno],shr',i,outi,r
	hCustomer1=fn_openFio('UB Customer',mat c$,mat cN)
	mat tran$=('')
	mat tranN=(0)
	
	dim tranType$(5)*20
	tranType$(1)='Charge'
	tranType$(2)='Penalty'
	tranType$(3)='Collection'
	tranType$(4)='Credit Memo'
	tranType$(5)='Debit Memo'
	fnopenprn
	pr #255: 'Record    Account    Date    Type                 Amount        Balance'
	tranRec=102505
	do
		tranRec+=1
		read #hTranRelative,using form$(hTran),rec=tranRec,release: mat tran$,mat tranN noRec NextTran eof FboEoTran
		z$=tran$(trans_acct) ! lpad$(trim$(tran$(trans_acct)(1:10)),10)
		! if trim$(z$)='130239.04' then debug=1 else debug=0
		dim didFixAcct$(0)
		whichDid=srch(mat didFixAcct$,z$)
		if tranN(trans_tcode)=8 then
			read #hCustomer1,using form$(hCustomer1),key=z$,release: mat c$,mat cN ! nokey CustomerNokey
			if askFirst then
				dim mg$(0)*256
				mat mg$(5)
				fnaddonec(mat mg$,'Faulty Transaction Encountered ('&str$(ftCount+=1)&')')
				fnaddonec(mat mg$,'            Record: '&str$(tranRec))
				fnaddonec(mat mg$,'           Account: '&chr$(9)&tran$(trans_acct))
				fnaddonec(mat mg$,'  Customer Balance: '&chr$(9)&str$(cN(c_balance)))
				fnaddonec(mat mg$,'Transaction Amount: '&chr$(9)&str$(tranN(trans_tamount)))
				fnaddonec(mat mg$,'Transaction Date: '&chr$(9)&str$(tranN(trans_tdate)))
				fnaddonec(mat mg$,'')
				fnaddonec(mat mg$,'Fix this one?')
				fnMsgBox(mat mg$, resp$,'',32+4)
			else
				pr 'Faulty Transaction Encountered ('&str$(ftCount+=1)&')'
				pr '            Record: '&str$(tranRec)
				pr '                Account: '&chr$(9)&tran$(trans_acct)
				pr '       Customer Balance: '&chr$(9)&str$(cN(c_balance))
				pr '     Transaction Amount: '&chr$(9)&str$(tranN(trans_tamount))
				pr '     Transaction Date: '&chr$(9)&str$(tranN(trans_tdate))
				pr '     '
				pr '     Fixing...'
				resp$='Yes'
			end if
			if resp$='Yes' then
				tranN(trans_tcode)=1 ! Charge
				rewrite #hTranRelative,using form$(hTran),rec=tranRec: mat tran$, mat tranN
				cN(c_balance)+=(tranN(trans_tamount)*2)
				cN(fn_serviceEnum(service_other))+=(tranN(trans_tamount)*2)
		    if whichDid<=0 then
					fnAddOneC(mat didFixAcct$,z$)
					whichDid=udim(mat didFixAcct$)
					! fnAddOneN(mat didFixBalanceForward,tranN(trans_tbal))
				end if
				rewrite #hCustomer1,using form$(hCustomer1),key=z$: mat c$, mat cN
			end if
		end if
		if whichDid>0 then
			! if tranN(trans_tcode)=5 then didFixBalanceForward(whichDid)+=transAmount else didFixBalanceForward(whichDid)-=transAmount
			! didFixBalanceForward(whichDid)
			priorBal=fn_lastTBalBeforeRec(hTranRelative,z$,tranRec)
			! if tranRec=104088 then pr str$(tranRec)&' priorBal='&str$(priorBal) : pause
			if tranN(trans_tcode)=1 or tranN(trans_tcode)=2 or tranN(trans_tcode)=5 then ! tranN(trans_tcode)   1=Charge,2=Penalty,3=Collection,4=Credit Memo,5=Debit Memo
				tranN(trans_tbal)=priorBal+tranN(trans_tamount)
				! didFixBalanceForward(whichDid)+=transAmount 
			else if tranN(trans_tcode)=3 or tranN(trans_tcode)=4 then
				tranN(trans_tbal)=priorBal-tranN(trans_tamount)
				! didFixBalanceForward(whichDid)-=transAmount
			else
				pr bell;'tranN(trans_tcode) '&str$(tranN(trans_tcode))&' unrecognized.'
				pause
			end if
			! tranN(trans_tbal)=didFixBalanceForward(whichDid)
			if tran$(trans_acct)<>priorAcct$ then
				pr #255: ''
				priorAcct$=tran$(trans_acct)
			end if
			pr #255: str$(tranRec)&' '&tran$(trans_acct)&' '&cnvrt$('pic(####/##/##)',tranN(trans_tdate))&' '&rpad$(tranType$(tranN(trans_tcode)),12)&' '&cnvrt$('pic(---,---,--#.##)',tranN(trans_tamount))&' '&cnvrt$('pic(---,---,--#.##)',tranN(trans_tbal))
			rewrite #hTranRelative,using form$(hTran),rec=tranRec: mat tran$, mat tranN
		end if
		NextTran: !
		! if debug then pause
	loop while tranRec<lrec(hTranRelative)
	FboEoTran: !
	fncloseprn
	fnclosefile(hTran,'UB Transaction')
	close #hTranRelative:
	fnCloseFile(hCustomer1,'UB Customer')
fnend
def fn_serviceEnum(service_other; ___,returnN)
	if service_other=1 then
		returnN=c_s01breakdown
	else if service_other=2  then
		returnN=c_s02breakdown
	else if service_other=3  then
		returnN=c_s03breakdown
	else if service_other=4  then
		returnN=c_s04breakdown
	else if service_other=5  then
		returnN=c_s05breakdown
	else if service_other=6  then
		returnN=c_s06breakdown
	else if service_other=7  then
		returnN=c_s07breakdown
	else if service_other=8  then
		returnN=c_s08breakdown
	else if service_other=9  then
		returnN=c_s09breakdown
	else if service_other=10 then
		returnN=c_s10breakdown
	end if
	fn_serviceEnum=returnN
fnend
def fn_lastTBalBeforeRec(hTranRelative,z$,recNum; ___,returnN)
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

include: fn_setup
include: fn_open