fn_setup
fnTop(program$)
MENU1: ! r:
	fnTos : chk_align=0
	fnLbl(1,1,"Scan:")
	fnChk(2,5,"Scan Customer Balance Breakdowns",chk_align) : resp$(1)='True'
	fnChk(3,5,"Scan Transaction Breakdowns"     ,chk_align) : resp$(2)='True'
	fnLbl(5,1,"Error Handling:")
	fnChk(6,5,"Report Erroneous Transactions"   ,chk_align) : resp$(3)='True'
	fnChk(7,5,"Fix Erroneous Transactions"      ,chk_align) : resp$(4)='False'
	fnLbl(9,1,"Miscellaneous:")
	fnChk(10,5,"Move Credit Balnces to Other"   ,chk_align) : resp$(5)='False'
	fnChk(11,5,"  and apply credits"            ,chk_align) : resp$(6)='False'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then 
		if resp$(1)='True' then do_fix_balance_breakdowns=1 else do_fix_balance_breakdowns=0
		if resp$(2)='True' then do_fix_trans_breakdowns=1 else do_fix_trans_breakdowns=0
		if resp$(3)='True' then do_report=1 else do_report=0
		if resp$(4)='True' then do_fix=1 else do_fix=0
		if resp$(5)='True' then do_move_credit=1 else do_move_credit=0
		if resp$(6)='True' then do_apply_credit=1 else do_apply_credit=0

		if do_fix_balance_breakdowns then 
			fn_balanceBreakdowns(do_fix,do_report)
		end if
		if do_fix_trans_breakdowns then 
			fn_transBreakdowns(do_fix,do_report)
		end if
		if do_move_credit then let fn_moveCredit(do_move_credit)
		if do_apply_credit then let fn_applyCreditFromOther(do_apply_credit)
	end if
goto Xit ! /r
Xit: fnXit 
def fn_setup
	if ~setup then 
		setup=1
		autoLibrary
		on error goto Ertn

		dim serviceName$(10)*20,srv$(10)*2
		fnGetServices(mat serviceName$,mat srv$)
		
		
	dim serviceName$(10)*20
	dim serviceCode$(10)*2
	dim tax_code$(10)*1
	dim penalty$(10)*1

	fnGetServices(mat serviceName$,mat srv$, mat tax_code$,mat penalty$) ! ,mat subjectto,mat ordertoapply)

		
		
		
	end if 
fnend 
def fn_report_it(z$,mat report_g,bal_breakdown; heading$*80,col_2_heading$*12,col_3_heading$,col_3_value$,col_4_heading$*12,col_4_value$*128)
	if do_report then 
		if ~setup_report_it then 
			setup_report_it=1
			fnopenprn
			pr #255,using F_HDR1: heading$
			if col_4_heading$<>'' then 
				pr #255,using F_HDR2c: 'Account',col_2_heading$,serviceName$(1)(1:12),serviceName$(2)(1:12),serviceName$(3)(1:12),serviceName$(4)(1:12),serviceName$(5)(1:12),serviceName$(6)(1:12),serviceName$(7)(1:12),serviceName$(8)(1:12),serviceName$(9)(1:12),serviceName$(10)(1:12),'*Calculated*',col_3_heading$,col_4_heading$
				F_HDR2c: form pos 1,15*(cc 12,',')
			else if col_3_heading$<>'' then 
				pr #255,using F_HDR2b: 'Account',col_2_heading$,serviceName$(1)(1:12),serviceName$(2)(1:12),serviceName$(3)(1:12),serviceName$(4)(1:12),serviceName$(5)(1:12),serviceName$(6)(1:12),serviceName$(7)(1:12),serviceName$(8)(1:12),serviceName$(9)(1:12),serviceName$(10)(1:12),'*Calculated*',col_3_heading$
				F_HDR2b: form pos 1,14*(cc 12,',')
			else
				pr #255,using F_HDR2a: 'Account',col_2_heading$,serviceName$(1)(1:12),serviceName$(2)(1:12),serviceName$(3)(1:12),serviceName$(4)(1:12),serviceName$(5)(1:12),serviceName$(6)(1:12),serviceName$(7)(1:12),serviceName$(8)(1:12),serviceName$(9)(1:12),serviceName$(10)(1:12),'*Calculated*'
				F_HDR2a: form pos 1,13*(cc 12,',')
			end if
			F_HDR1: form pos 1,cc 156
		end if  ! ~setup_report_it
		print_count+=1
		if col_4_heading$<>'' then 
			pr #255,using F_BODYc: z$,bal,report_g(1),report_g(2),report_g(3),report_g(4),report_g(5),report_g(6),report_g(7),report_g(8),report_g(9),report_g(10),bal_breakdown,col_3_value$,col_4_value$
			F_BODYc: form pos 1,c 12,',',12*(n 12.2,','),c 10,c
		else if col_3_heading$<>'' then 
			pr #255,using F_BODYb: z$,bal,report_g(1),report_g(2),report_g(3),report_g(4),report_g(5),report_g(6),report_g(7),report_g(8),report_g(9),report_g(10),bal_breakdown,col_3_value$
			F_BODYb: form pos 1,c 12,',',12*(n 12.2,','),c 10
		else
			pr #255,using F_BODYa: z$,bal,report_g(1),report_g(2),report_g(3),report_g(4),report_g(5),report_g(6),report_g(7),report_g(8),report_g(9),report_g(10),bal_breakdown
			F_BODYa: form pos 1,c 12,',',12*(n 12.2,',')
		end if
		! pr #255: z$&' has a balance of '&str$(gb(10))&' but the breakdowns add up to '&str$(bal_breakdown)
	end if 
fnend 
def fn_any_gb_negative
	agn_return=0
	for agn_item=1 to 10
		if gb(agn_item)<0 then agn_return=1
	next agn_item
	fn_any_gb_negative=agn_return
fnend  ! fn_any_gb_negative
! def library fnfix_balance_breakdowns(do_fix,do_report)
!   fn_setup
!   fnfix_balance_breakdowns=fn_balanceBreakdowns(do_fix,do_report)
! fnend
def fn_balanceBreakdowns(do_fix,do_report) ! assumes balance is right, puts the difference into other
	print_count=0
	fnStatus('Checking Customer Balance Breakdowns')
	dim customer_g(10)
	dim z$*10
	dim service_rate_code(7)
	dim gb(10)
	gb_other=fnservice_other
	fnopenprn
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k 
	do 
		read #hCustomer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof CUSTOMER_EOF
		F_CUSTOMER: form pos 1,c 10,pos 143,7*pd 2,pos 292,pd 4.2,pos 300,10*pd 4.2,pos 388,10*pd 5.2
		customerReadCount+=1
		for gb_item=1 to udim(mat serviceName$) ! udim(mat gb)
			if trim$(serviceName$(gb_item))='' then gb(gb_item)=0
		next gb_item
		bal_breakdown=sum(gb)
		if bal<>bal_breakdown then 
			fn_report_it(z$,mat customer_g,bal_breakdown,"Customer Balance Breakdowns",'Balance')
			customerReportCount+=1
			if do_fix then 
				gb(gb_other)-=(bal_breakdown-bal)
				rewrite #hCustomer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
			end if 
		end if 
	loop 
	CUSTOMER_EOF: ! 
	close #hCustomer: ioerr ignore
	pr #255: 'customers  scanned:'&str$(customerReadCount)
	pr #255: 'customers reported:'&str$(customerReportCount)
	pr #255: ''
	fn_reportItClose(print_count)
fnend 
def library fnfix_trans_breakdowns(do_fix,do_report)
	fn_setup
	fnfix_trans_breakdowns=fn_transBreakdowns(do_fix,do_report)
fnend 
def fn_transBreakdowns(do_fix,do_report; ___,needsFixed)
		print_count=0
	dim trans_g(11),unused_ru(6)
	gb_other=fnservice_other
	fnStatus('Checking Transaction Breakdowns')
	if do_fix then 
		open #h_trans=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",i,outi,r 
	else 
		open #h_trans=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",i,i,r 
	end if 
	do 
		read #h_trans,using F_TRANS: p$,tdate,transcode,tamt,mat trans_g,mat unused_ru,bal,postcode eof TRANS_EOF
		F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
		transReadCount+=1
		needsFixed=0
		for g_item=1 to udim(mat serviceName$) ! udim(mat trans_g)
			if trim$(serviceName$(g_item))='' and trans_g(g_item)<>0 then
				trans_g(bg_other)+=trans_g(g_item)
				trans_g(g_item)=0
				fn_report_it(p$,mat trans_g,bal_breakdown,"Transaction Breakdowns",'T Amount','T Date',cnvrt$('pic(zzzz/zz/zz)',tdate),'Note','Lost allocation on unlabeled service '&str$(g_item))
				needsFixed=1
			end if
		next g_item
	! r: get bal_breakdown
		bal_breakdown=0
		for sz1_item=1 to 9 ! sz1
			bal_breakdown+=trans_g(sz1_item)
		next sz1_item
	!      bal_breakdown=sum(trans_g)
	! /r
		if tamt<>bal_breakdown then 
 			fn_report_it(p$,mat trans_g,bal_breakdown,"Transaction Breakdowns",'T Amount','T Date',cnvrt$('pic(zzzz/zz/zz)',tdate),'Note','breakdown did not equal transaction amount')
			transReportCount+=1
			needsFixed=1
		end if  ! gb(10)<>FICTIONAL_gb10
		if do_fix and needsFixed then 
			trans_g(gb_other)-=(bal_breakdown-tamt)
			rewrite #h_trans,using F_TRANS: p$,tdate,transcode,tamt,mat trans_g,mat unused_ru,bal,postcode
		end if 

	loop 
	TRANS_EOF: ! 
	pr #255: 'transactions  scanned:'&str$(transReadCount)
	pr #255: 'transactions reported:'&str$(transReportCount)
	pr #255: ''
	fn_reportItClose(print_count)
	close #h_trans: 
fnend  ! fn_balanceBreakdowns
def fn_reportItClose(&print_count)
	if print_count>0 then 
		let fncloseprn
		pr 'print_count=';print_count
	end if
	setup_report_it=0
print_count=0

fnend
def fn_moveCredit(do_move_credit)
	fnStatus('Moving Credit Balances to Other in Customer Balance Breakdowns')
	dim customer_g(10)
	dim z$*10,service_rate_code(7)
	dim gb(10)
	gb_other=fnservice_other
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k 
	do 
		read #hCustomer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof MC_CUSTOMER_EOF
		read_count+=1
		if fn_any_gb_negative then 
			fn_report_it(z$,mat gb,bal,"Customer Balance Breakdowns Before Credits Moved to Other",'Balance')
			for gb_item=1 to 10
				if gb_item<>gb_other then 
					if gb(gb_item)<0 then 
						gb(gb_other)+=gb(gb_item)
						gb(gb_item)=0
					end if 
				end if 
			next gb_item
			rewrite #hCustomer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
			!       if trim$(z$)='205320.70' then pause !
		end if 
	loop 
	MC_CUSTOMER_EOF: ! 
	close #hCustomer: 
fnend 
def fn_applyCreditFromOther(do_apply_credit)
	fnStatus('Applying Credit Balances (from Other) to the rest of the Customer Balance Breakdowns')
	dim customer_g(10)
	dim z$*10,service_rate_code(7)
	dim gb(10)
	gb_other=fnservice_other
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k 
	do 
		read #hCustomer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof ACFO_CUSTOMER_EOF
		read_count+=1
		if gb(gb_other)<0 then 
			fn_report_it(z$,mat gb,bal,"Customer Balance Breakdowns Before Credits Moved to Other",'Balance')
			for gb_item=1 to 10
				if gb_item<>gb_other then ! 
					if gb(gb_item)=>abs(gb(gb_other)) then ! this item has more (or equal) charge than other has credit
						gb(gb_item)+=gb(gb_other)
						gb(gb_other)=0
						goto ACFO_REC_COMPLETE ! all of other is consumed, we are done here
					else if gb(gb_item)<abs(gb(gb_other)) then ! other has more than enough credit to cover this charge
						gb(gb_other)+=gb(gb_item)
						gb(gb_item)=0
					end if 
				end if 
			next gb_item
			ACFO_REC_COMPLETE: ! 
			rewrite #hCustomer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
			!       if trim$(z$)='205320.70' then pause !
		end if 
	loop 
	ACFO_CUSTOMER_EOF: ! 
	close #hCustomer: 
fnend 
include: ertn