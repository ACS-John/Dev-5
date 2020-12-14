if env$('acsdeveloper')<>'' then debug=1
verbose=0
	fn_setup
	fnTop(program$)
	current_accounting_period=fnactpd
 
	process_gltrans=1 ! if =1 than gltrans will be added into the period accumulators as well as actrans
 
	open #company:=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative
	read #company,using 'Form Pos 296,n 2,Pos 384,N 2',rec=1: lmu,nap
	! lmu = Last Accounting Period Closed
	! nap = Number of Accounting Periods
	close #company:
	fnGetFundList(mat fund_list) ! pr 'fund_list:' : pr mat fund_list : pause
	mat last_retained_earnings_acct$(udim(mat fund_list)) : if udim(last_retained_earnings_acct$)=0 then mat last_retained_earnings_acct$(1)
	mat period_accumulator_current(nap)
	mat period_accumulator_prior(nap)
	
	if fn_screen_1(nap,mat period_date_start,mat prior_period_date_start)=5 then goto Xit
	! fn_report(env$('program_caption'))
	! fn_report(date$('mm/dd/ccyy'))
	! fn_report('')
	open #hTransHistory=fnH: "Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno],Shr",internal,outIn,keyed
	FtransHistory: form pos 1,c 12,n 6,pd 6.2,n 2,pos 71,n 2
	if process_gltrans then
	fnIndex("[Q]\GLmstr\GLTrans.h[cno]",env$('Temp')&"\GLIndex.h[cno]","1 12")
	open #hTransCurrent=fnH: "Name=[Q]\GLmstr\GLTrans.h[cno],KFName=[Temp]\GLIndex.h[cno],Shr",internal,outIn,keyed
	end if  ! process_gltrans
	FtransCurrent: form pos 1,c 12,n 6,pd 6.2,n 2
	open #hAcct1=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,outIn,keyed
	open #hAcct2=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.h[cno],Shr",internal,outIn,keyed
	Facct: form pos 1,c 12,x 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
	do
		read #hAcct1,using Facct: gl$,mat rf,bb,cb,mat balance_current_year_month,mat balance_prior_year_month eof EO_GLMSTR
		if verbose then fn_report('*** '&gl$&' ***')
		mat balance_current_year_month=(0)
		mat period_accumulator_current=(0)
		if fn_isRetainedEarningsAcct(gl$) then
			period_accumulator_current(1)=balance_prior_year_month(nap)
			! if gl$='  1   405  0' then pr 'initialize it to  ';period_accumulator_current(1) : pause
			if debugAcct$=gl$ then fn_debugReport('Starting Balance for Account ('&gl$&') set to last period of prior year '&str$(period_accumulator_current(1))&' because it is a retained earnings account.')
		else
			period_accumulator_current(1)=0 ! bb ! bb = Beginning Balance (at the beginning of the fiscal year)
			if debugAcct$=gl$ then fn_debugReport('Starting Balance for Account ('&gl$&') set to 0 because it is NOT a retained earnings account.')
	end if
	mat period_accumulator_prior=(0)
	gln_period_did_change=0
	for period=1 to nap
		if period=nap then
			period_date_end=date(days(period_date_start(1)+1,'mmddyy')-1,'mmddyy')
			prior_period_date_end=date(days(prior_period_date_start(1)+1,'mmddyy')-1,'mmddyy')
		else
			period_date_end=date(days(period_date_start(period+1),'mmddyy')-1,'mmddyy')
			prior_period_date_end=date(days(prior_period_date_start(period+1),'mmddyy')-1,'mmddyy')
		end if  ! period=nap   /   else
		! if gl$='  1   405  0' and period=3 then pr 'before fn_processTrans   period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
		fn_processTrans(hTransHistory, 1)
		! if gl$='  1   405  0' then pr 'after fn_processTrans' : pause
		if process_gltrans then fn_processTrans(hTransCurrent)
		if period>1 and period<=current_accounting_period then
			period_accumulator_current(period)+=period_accumulator_current(period-1)
		! if gl$='  1   405  0' and period=3 then pr 'after adding in the prior period    period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
		end if
		if include_prior_periods then
			if period>1 then period_accumulator_prior(period)+=period_accumulator_prior(period-1)
		end if  ! include_prior_periods
		!   if period>1 then period_accumulator_current(period)<>0 then period_accumulator_current(period)+=period_accumulator_current(period-1)
		if period_accumulator_current(period)<>balance_current_year_month(period) then
			gln_period_did_change+=1
			if verbose then fn_report('changing GLmstr '&gl$&' period '&str$(period)&" from "&str$(balance_current_year_month(period))&' to '&str$(period_accumulator_current(period)))
			balance_current_year_month(period)=period_accumulator_current(period)
		!  if gl$='  1   405  0' then pr ' about to write period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
		end if
		if include_prior_periods and period_accumulator_prior(period)<>balance_prior_year_month(period) then
			gln_period_did_change+=1
			if verbose then fn_report('changing GLmstr '&gl$&' period '&str$(period)&" from "&str$(balance_prior_year_month(period))&' to '&str$(period_accumulator_prior(period)))
			balance_prior_year_month(period)=period_accumulator_prior(period)
		end if  ! period_accumulator_prior(period)<>balance_prior_year_month(period) then
	next period
	if current_accounting_period>1 then
		if cb<>balance_current_year_month(current_accounting_period) then gln_period_did_change+=1
		cb=balance_current_year_month(current_accounting_period)
	end if
	if current_accounting_period>2 then
		if bb<>balance_current_year_month(current_accounting_period-1) then gln_period_did_change+=1
		bb=balance_current_year_month(current_accounting_period-1)
		
		
		if debug and gln_period_did_change=1 and (current_accounting_period-1)=4 then 
			pr 'bb=';bb
				pause
		end if
		
	end if
		!   if trim$(gl$)='1   405  0' then pause
	if gln_period_did_change>0 then
		! fn_report(' change detected to the current month balance column '&gl$)
		rewrite #hAcct1,using Facct,key=gl$: gl$,mat rf,bb,cb,mat balance_current_year_month,mat balance_prior_year_month
	end if  ! gln_period_did_change>0
	loop
	EO_GLMSTR: !
	! fncloseprn : report_open=0
	if openDebugReport then
		fncloseprn
		openDebugReport=0
	end if
Xit: fnXit ! if env$('acsdeveloper')<>'' then stop else fnXit ! XXX
def fn_setup
	autoLibrary
	on error goto Ertn
 
	dim resp$(100)*60
	dim balance_current_year_month(13),balance_prior_year_month(13),rf(6)
	dim actrans_key$*20
fnend
def fn_screen_1(nap,mat period_date_start,mat prior_period_date_start)
	mat period_date_start(nap)
	period_date_start=(0)
	mat prior_period_date_start(nap)
	prior_period_date_start=(0)
	fnTos
	mylen=31
	mypos=mylen+2
	respc=0 : myline=0
	for period=1 to nap
		fnLbl(myline+=1,1,"Period "&str$(period)&" Start Date:",mylen,1)
		fnTxt(myline,mypos,8,0,1,"1")
		respc+=1
		fncreg_read("Period "&str$(period)&" Start Date",resp$(respc))
	next period
	fnChk(myline+=2,mypos,"Correct Prior Year",1)
	respc+=1
	fncreg_read("correct prior year",resp$(respc))
	if resp$(respc)='' then resp$(respc)='False'
 
	myline=1 : col3_pos=mypos+20
	resp_lrea_fund_1=respc+1
	if use_dept then
		col4_pos=col3_pos+10
		fnLbl(1,col3_pos,'Last Retained Earnings Account(s)')
		for fund_item=1 to udim(mat fund_list)
			fnLbl(myline+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",9,1)
			fnqgl(myline,col4_pos)
			respc+=1
			fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(respc)) : resp$(respc)=fnrgl$(resp$(respc))
		next fund_item
	else
		col4_pos=col3_pos+32
		fnLbl(1,col3_pos,'Last Retained Earnings Account:',31,1)
		fnqgl(myline,col4_pos)
		respc+=1
		fncreg_read("last retained earnings account - no fund ",resp$(respc)) : resp$(respc)=fnrgl$(resp$(respc))
	end if
	
	if debug then
		myline+=1
		col4_pos=col3_pos+32
		fnLbl(myline+=1,col3_pos,'Debug Account:',31,1)
		fnqgl(myline,col4_pos)
		resp_debugAcct=respc+=1
		dim debugAcct$*128
		fncreg_read("debug account",debugAcct$)
		resp$(resp_debugAcct)=fnrgl$(debugAcct$)
		

		fnLbl(myline+=1,col3_pos,"Debug Period:",31,1)
		fnTxt(myline,col4_pos,2,0,1,'number')
		dim debugPeriod$*128
		fncreg_read("debug period",debugPeriod$)
		resp$(resp_debugPeriod=respc+=1)=debugPeriod$

		
	end if

	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey<>5 then
		respc=0
		for period=1 to nap
			period_date_start(period)=val(resp$(period))
			prior_period_date_start(period)=period_date_start(period)-1
			fncreg_write("Period "&str$(period)&" Start Date",resp$(period))
		next period
		if resp$(nap+1)='True' then include_prior_periods=1 else include_prior_periods=0
		fncreg_write("correct prior year",resp$(nap+1))
		if debug then
			debugAcct$=fnagl$(resp$(resp_debugAcct))
			fncreg_write("debug account",debugAcct$)
			
			debugPeriod$=resp$(resp_debugPeriod)
			debugPeriod=val(debugPeriod$)
			fncreg_write("debug period",debugPeriod$)
			
		end if
		respc=resp_lrea_fund_1-1
		if use_dept then
			for fund_item=1 to udim(mat fund_list)
			last_retained_earnings_acct$(fund_item)=fnagl$(resp$(respc+=1))
			fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
			next fund_item
		else
			last_retained_earnings_acct$(1)=fnagl$(resp$(respc+=1))
			fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
		end if
	end if  ! ckey<>5
	fn_screen_1=ckey
fnend
def fn_date_mmddyy_is_within_range(dmi_test_date,dmi_date_start,dmi_date_end)
	dmi_return=0
	dmi_test_date=fncd(dmi_test_date)
	dmi_date_start=fncd(dmi_date_start)
	dmi_date_end=fncd(dmi_date_end)
	if dmi_test_date=>dmi_date_start and dmi_test_date<=dmi_date_end then dmi_return=1
	fn_date_mmddyy_is_within_range=dmi_return
fnend  ! fn_date_mmddyy_is_within_range
def fn_processTrans(h_trans; pt_fix_trans_period_code,___,trgl$,tr_date,tr_date,tr_6,pc2)
	! uses inherriteed local:  gl$, period, mat period_accumulator_prior    
	!          probably others,
	actrans_key$=rpad$(gl$,kln(h_trans))
	restore #h_trans,key>=actrans_key$: nokey EoTrans
	do
		if pt_fix_trans_period_code then
			read #h_trans,using FtransHistory: trgl$,tr_date,tr_amt,tr_6,pc2 eof EoTrans
		else
			read #h_trans,using FtransCurrent: trgl$,tr_date,tr_amt,tr_6 eof EoTrans
		end if
		if trgl$<>gl$ then goto EoTrans
		! r: prior month
		if fn_date_mmddyy_is_within_range(tr_date,prior_period_date_start(period),prior_period_date_end) then
			period_accumulator_prior(period)+=tr_amt
			!         fn_report(rpt$(' ',40)&str$(tr_date)&' prior period '&str$(period)&'  + '&cnvrt$('pic(----------.--)',tr_amt))
			if pt_fix_trans_period_code and period<>pc2 then ! the period on the transaction is incorrect - correct it.
			!         fn_report('changing actrans '&trgl$&'/'&str$(tr_date)&" from "&str$(pc2)&' to '&str$(period))
			pc2=period
			rewrite #h_trans,using FtransHistory: trgl$,tr_date,tr_amt,tr_6,pc2
			end if
		end if
		! /r
		! r: current month
		if fn_date_mmddyy_is_within_range(tr_date,period_date_start(period),period_date_end) then
			if period=>1 then
				period_accumulator_current(period)+=tr_amt
				if period=debugPeriod and trgl$=debugAcct$ then
					fn_debugReport('TRANS: add '&cnvrt$('pic(---,--#.##)',tr_amt)&' from '&date$(days(tr_date,'mmddyy'),'mm/dd/ccyy')&' accum= '&cnvrt$('pic(---,--#.##)',period_accumulator_current(period)))
				en if
			end if
			!         fn_report(str$(tr_date)&' period '&str$(period)&'  + '&cnvrt$('pic(----------.--)',tr_amt)&' type '&str$(tr_6))
			if pt_fix_trans_period_code and period<>pc2 then ! the period on the transaction is incorrect - correct it.
				!         fn_report('changing actrans '&trgl$&'/'&str$(tr_date)&" from "&str$(pc2)&' to '&str$(period))
				pc2=period
				rewrite #h_trans,using FtransHistory: trgl$,tr_date,tr_amt,tr_6,pc2
			end if
		end if
		! /r
	loop
	EoTrans: !
	if verbose or (gl$=debugAcct$) then  ! period=debugPeriod and 
		if period_accumulator_current(period)<>0 then
			fn_report('  period '&str$(period)&' totals '&cnvrt$('pic(----------.--)',period_accumulator_current(period)))
		end if
		if period_accumulator_prior(period)<>0 then
			fn_report(rpt$(' ',40)&'prior period '&str$(period)&' totals '&cnvrt$('pic(----------.--)',period_accumulator_prior(period)))
		end if
	end if
fnend
def fn_report(line$*256)
	!   if ~report_open then
	!     report_open=1
	!     fnopenprn
	!   end if  ! ~report_open
	!   pr #255: line$
	! fnStatus(line$) ! pr line$ ! XXX
	if debug then fn_debugReport(line$)
fnend
def fn_isRetainedEarningsAcct(gl$; ___,returnN)
	! pr 'gl number passed is *'&gl$&'*'
	! pr 'gl number last retained earnings *'&last_retained_earnings_acct$&'*'
	
	glAcct=val(gl$(4:10))
	! pr glAcct : pause
	gl$=trim$(fnagl$(gl$))
	if srep$(srep$(gl$,' ',''),'0','')='' then ! if GL account is 0-0-0 than it is not a retained earnings account, because it's invalid.
		retunN=0
		goto IareaFinis
	else if use_dept then
		fund_compare=val(gl$(1:3))
		fund_which=srch(mat fund_list,fund_compare)
	else
		fund_which=1
	end if
	lreaVal=val(last_retained_earnings_acct$(fund_which)(4:10))


	! if gl$<=trim$(last_retained_earnings_acct$(fund_which)) then
	if glAcct<=lreaVal then
		!     pr '"'&gl$&'"<="'&trim$(last_retained_earnings_acct$(fund_which))&'" so it IS a retained earnings account - fund:'&str$(fund_which)
		returnN=1
		!     pause
	else
		!     pr '"'&gl$&'">"'&trim$(last_retained_earnings_acct$(fund_which))&'" so it is NOT a retained earnings account - fund:'&str$(fund_which)
		returnN=0
		!     pause
	end if
	IareaFinis: !
	! if returnN then pr gl$&' IS a retained earnings account'
	! if ~returnN then pr gl$&' is NOT a retained earnings account'
	! pause
	fn_isRetainedEarningsAcct=returnN
fnend
def fn_debugReport(text$*256)
	if ~openDebugReport then
		openDebugReport=1
		fnopenprn
		pr #255: env$('program_caption')
		pr #255: date$&' '&time$
	end if
	pr #255: text$
fnend
include: ertn
