! formerly S:\acsGL\CB
! fixes Current Balance, by taking Beginning Balance (or previous balance 2 yrs ago) and adding current transactions (optional a range of accumulated transactions too) to it.
	autoLibrary
	fnTop(program$)
	on error goto Ertn
	dim bp(13)
	dim resp$(128)*256
	open #company=fnH: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #company,using 'Form Pos 150,2*N 1,Pos 384,n 2': use_dept,use_sub,nap
	close #company:
	fnGetFundList(mat fund_list)
	! r: debug setup
						if env$('acsdeveloper')<>'' then
							debug=1
							debug_gl$=' 51   830  0'
						end if
	! /r
	if ~fn_theScreen then goto Xit
	! r: setup and open files
	if enableProcessAccumulatedTrans$='True' then
		open #hAcTrans=fnH: 'Name=[Q]\GLmstr\AcTrans.h[cno],KFName=[Q]\GLmstr\AcTrIdx.h[cno]',internal,input,keyed
		startWithBalEndOfPriorYear=1
	end if
	open #hGlMstr=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",internal,outIn,keyed
	open #hGlTrans=fnH: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,input,relative
	fTransBoth: form pos 1,c 12,n 6,pd 6.2
	! /r
	do ! r: main loop

		read #hGlMstr,using 'Form POS 1,C 12,Pos 81,2*PD 6.2,pos 327,pd 6.2,pos 171,13*pd 6.2': gln$, bb,cb,pbp,mat bp eof Xit
		cbOrigional=cb
		! debug_gl$=' 10   592  0'
		! if gln$=debug_gl$ then pr ' found it. ' : debug=1 : pause else debug=0
		activityCurrent=fn_currentActivity(hGlTrans,gln$)
		if enableProcessAccumulatedTrans$='True' then
			activityHistory=fn_accumulatedActivity(hAcTrans,gln$,dayStart,dayEnd)
		end if
		! if gln$=debug_gl$ then pr gln$;' activityCurrent=';activityCurrent : pause
		if startWithBalEndOfPriorYear then
			isRetainedEarningsAccount=fn_is_a_retained_earn_account(gln$)
			if isRetainedEarningsAccount then
				cb=bp(nap)+activityCurrent+activityHistory
				! cb=pbp+activityCurrent+activityHistory
			else
				cb= 0 +activityCurrent+activityHistory
			end if
		elseen
			cb=bb+activityCurrent+activityHistory
		end if
		if debug and gln$=debug_gl$ then ! r: display debug information
			pr 'GL Number='&gln$
			pr '       current balance before recalcution: ';cbOrigional
			if startWithBalEndOfPriorYear then
				if isRetainedEarningsAccount then
				pr '       balance end of last period last year=';bp(nap)
!       pr '       beginning balance two years ago=';pbp
				else
				pr '       not a retained earnings account start with 0'
				end if
			else
				pr '       beginning balance =';bb
			end if
			pr '       activity from Current Transactions=';activityCurrent
			if enableProcessAccumulatedTrans$='True' then
				pr '       activity from Accumulated Transactions=';activityHistory
			end if
			pr '       new current balance is=';cb
			pause
		end if ! /r
		rewrite #hGlMstr,using 'Form POS 81,2*PD 6.2': bb,cb
	loop ! /r
Xit: fnXit
	def fn_is_a_retained_earn_account(gl$)
! pr 'gl number passed is *'&gl$&'*'
! pr 'gl number last retained earnings *'&last_retained_earnings_acct$&'*'
		gl$=trim$(fnagl$(gl$))
		if use_dept then
			fund_compare=val(gl$(1:3))
			fund_which=srch(mat fund_list,fund_compare)
		else
			fund_which=1
		end if
		if gl$<=trim$(last_retained_earnings_acct$(fund_which)) then
!     pr '"'&gl$&'"<="'&trim$(last_retained_earnings_acct$(fund_which))&'" so it IS a retained earnings account - fund:'&str$(fund_which)
			iarea_return=1
!     pause
		else
!     pr '"'&gl$&'">"'&trim$(last_retained_earnings_acct$(fund_which))&'" so it is NOT a retained earnings account - fund:'&str$(fund_which)
			iarea_return=0
!     pause
		end if
		fn_is_a_retained_earn_account=iarea_return
	fnend
def fn_theScreen ! lots of local variables
	fncreg_read(cap$&': enableProcessAccumulatedTrans',enableProcessAccumulatedTrans$,'False')
	fncreg_read(cap$&': dayStart',tmp$) : dayStart=val(tmp$)
	fncreg_read(cap$&': dayEnd',tmp$) : dayEnd=val(tmp$)
	fnTos
	rc=0

	fnLbl(lc+=1,1,'WARNING: This program recalculates all the Current Balance files in General Ledger Accounts.')
	fnLbl(lc+=1,1,'Normally this program rebuilds the current balance from current transactions only.')
	fnLbl(lc+=1,1,'However you may choose to process History Transactions too.')
	fnLbl(lc+=1,1,'    If you do process History Transactions you must use a date range also')
	fnLbl(lc+=1,1,'    If you do process History Transactions the base for the date will be ')
	fnLbl(lc+=1,1,'          Previous Balance Two Years ago instead of Beginning Balance.')
	lc+=1 : mylen=14 : mypos=mylen+2
	fnChk(lc+=1,1,'Process History Transactions')
	resp$(resp_enableAcTrans:=rc+=1)=enableProcessAccumulatedTrans$
	lc+=1
	fnLbl(lc+=1,1,'Starting Date:',mylen,1,0,0,0,"Enter a date to filter results or blank for all")
	fnTxt(lc,mypos,10,0,1,"3",0,"Enter a date to filter results or blank for all",0)
	resp$(resp_dateStart=rc+=1)=date$(dayStart,'ccyymmdd')
	fnLbl(lc+=1,1,'Ending Date:',mylen,1,0,0,0,"Enter a date to filter results or blank for all")
	fnTxt(lc,mypos,10,0,1,"3",0,"Enter a date to filter results or blank for all",0)
	resp$(resp_dateEnd=rc+=1)=date$(dayEnd,'ccyymmdd')

		lc+=1 : col3_pos=mypos+20
		resp_lrea_fund_1=rc+1
		if use_dept then
			col4_pos=col3_pos+10
			fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account(s)')
			for fund_item=1 to udim(mat fund_list)
				fnLbl(lc+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",9,1)
				fnqgl(lc,col4_pos)
				rc+=1
				fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
			next fund_item
		else
			col4_pos=col3_pos+32
			fnLbl(lc+=1,col3_pos,'Last Retained Earnings Account:',31,1)
			fnqgl(lc,col4_pos)
			rc+=1
			fncreg_read("last retained earnings account - no fund ",resp$(rc)) : resp$(rc)=fnrgl$(resp$(rc))
		end if
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then
		theScreenReturn=0
	else
		theScreenReturn=1
		dayStart=days(resp$(resp_dateStart),'ccyymmdd')
		dayEnd=days(resp$(resp_dateEnd),'ccyymmdd')
		enableProcessAccumulatedTrans$=resp$(resp_enableAcTrans)
		fncreg_write(cap$&': enableProcessAccumulatedTrans',enableProcessAccumulatedTrans$)
		fncreg_write(cap$&': dayStart',str$(dayStart))
		fncreg_write(cap$&': dayEnd',str$(dayEnd))
		rc=resp_lrea_fund_1-1
		if use_dept then
			mat last_retained_earnings_acct$(udim(mat fund_list))
			for fund_item=1 to udim(mat fund_list)
				last_retained_earnings_acct$(fund_item)=fnagl$(resp$(rc+=1))
				fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
			next fund_item
		else
			last_retained_earnings_acct$(1)=fnagl$(resp$(rc+=1))
			fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
		end if
	end if
	fn_theScreen=theScreenReturn
fnend
def fn_currentActivity(hGlTrans,gln$; ___,returnN)
	restore #hGlTrans:
	do
		read #hGlTrans,using fTransBoth: trgl$,tr_date,tr_amt eof caFinis
		if trgl$(1:3)  ="   " then trgl$(1:3)  ="  0"
		if trgl$(4:9)  ="   " then trgl$(4:9)  ="  0"
		if trgl$(10:12)="   " then trgl$(10:12)="  0"
		if trgl$=gln$ then
				if debug and gln$=debug_gl$ then
					pr 'rec=';rec(hgltrans)
					pr 'adding $'&str$(tr_amt)&' from '&date$(days(tr_date,'mmddyy'),'mm/dd/ccyy')&' from Current Transactions'
					pause
				end if
			returnN+=tr_amt
		end if
	loop
	caFinis: !
	fn_currentActivity=returnN
fnend
def fn_accumulatedActivity(hAcTrans,gln$*12,dayStart,dayEnd)
	aaReturn=0
	restore #hAcTrans,search=>gln$: nokey aaFinis
	do
		read #hAcTrans,using fTransBoth: trgl$,tr_date,tr_amt eof aaFinis
		dayTran=days(tr_Date,'mmddyy')
		if (dayStart=0 or dayTran=>dayStart) and (dayEnd=0 or dayTran<=dayEnd) then
			if trgl$(1:3)  ="   " then trgl$(1:3)  ="  0"
			if trgl$(4:9)  ="   " then trgl$(4:9)  ="  0"
			if trgl$(10:12)="   " then trgl$(10:12)="  0"
			if trgl$=gln$ then
				if debug and gln$=debug_gl$ then
					pr 'adding $'&str$(tr_amt)&' from '&date$(dayTran,'mm/dd/ccyy')&' from Accumulated Transactions'
				end if
				aaReturn+=tr_amt
			end if
		end if
	loop while gln$=trgl$
	aaFinis: !
	fn_accumulatedActivity=aaReturn
fnend

include: ertn
