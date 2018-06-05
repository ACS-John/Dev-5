fn_setup
fn_addAnOtherCharge
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnerror,fntop,fnask_account,fnCmbAct
		library 'S:\Core\Library': fntos,fnlbl,fntxt,fncmdset,fnacs
		library 'S:\Core\Library': fnservice_other
		library 'S:\Core\Library': fnGetHandle
		library 'S:\Core\Library': fnMsgBox
		library 'S:\Core\Library': fnCloseFile
		library 'S:\Core\Library': fnCustomerData$
		library 'S:\Core\Library': fnNoteDir$
	end if
fnend
def library fnAddAnOtherCharge(;z$*10,hCustomer1)
	if ~setup then let fn_setup
	fnAddAnOtherCharge=fn_addAnOtherCharge( z$,hCustomer1)
fnend
def fn_addAnOtherCharge(; z$*10,hCustomer1)
	if hCustomer1<>0 then
		hCustomer1=fn_open('UB Customer',mat c$,mat cN,mat form$)
		needToCloseHcustomer1=1
	else
		needToCloseHcustomer1=0
	end if
	service_other=fnservice_other
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
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey<>5 then
		rc=0
		z$   =resp$(rc+=1)
		amt$ =resp$(rc+=1)
		note$=resp$(rc+=1)
		read #hCustomer1,using form$(hCustomer1),key=z$: mat c$,mat cN nokey CustomerNokey
		amt=val(amt$)
		if amt<=0 then 
			pr bell;
			dim mesg$(1)*128
			mat mesg$(1)
			mesg$(1)='Amount ('&str$(amt)&') is invalid.'
			fnmsgbox(mat mesg$)
			goto SCR1
		else if z$='' then
		end if
		fn_addNote(z$,amt,note$)
		cN(c_balance)-=amt
		fn_addTransaction(z$,service_other,date('ccyymmdd'),amt,cN(c_balance))
		rewrite #hCustomer1,key=z$: mat c$, mat cN
	end if
	if needToCloseHcustomer1 then
		fnCloseFile(hCustomer1,'UB Customer')
		needToCloseHcustomer1=0
	end if
fnend
CustomerNokey: ! r:
	mat mesg$(1)
	mesg$(1)='Account ('&z$&') is invalid.'
	fnmsgbox(mat mesg$)
goto SCR1 ! /r

def fn_addTransaction(z$,tCode,tDate,amt,newBalance)
	dim tran$(0)*256
	dim tranN(0)
	hTran=fn_open('UB Transaction',mat tran$,mat tranN,mat form$)
	mat tran$=('')
	mat tranN=(0)
	
	tran$(trans_acct)=z$
	tranN(trans_tdate  )=tDate
	tranN(trans_tcode  )=tCode
	tranN(trans_tamount)=amt
	if tCode=1 then
		tranN(trans_tg_1   )=amt
	else if tCode=2 then
		tranN(trans_tg_2   )=amt
	else if tCode=3 then
		tranN(trans_tG_3   )=amt
	else if tCode=4 then
		tranN(trans_tG_4   )=amt
	else if tCode=5 then
		tranN(trans_tG_5   )=amt
	else if tCode=6 then
		tranN(trans_tG_6   )=amt
	else if tCode=7 then
		tranN(trans_tG_7   )=amt
	else if tCode=8 then
		tranN(trans_tG_8   )=amt
	else if tCode=9 then
		tranN(trans_tG_9   )=amt
	else if tCode=10 then
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
	fnclosefile(hTran,'UB Transaction')
fnend
def fn_addNote(z$,amt,note$*256)
	open #h_notefile:=fngethandle: "Name="&fnNoteDir$&"\"&trim$(z$)&".txt,Use",display,output
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

include: ertn
include: fn_open
