
fn_setup
fnTop(program$)
! r: Direct clients to the (basic) PrintBill_Basic routine (below) or their custom bill program
	dim alternate_printbill_program$*256
	alternate_printbill_program$=fnub_printbill_program$
	if alternate_printbill_program$='(basic)' then
		goto PrintBill_Basic
	else
		fnchain(fnub_printbill_program$,1)
	end if
! /r

def fn_setup
	autoLibrary
	on error goto Ertn

	dim resp$(60)*128
	dim mg$(4)*128
	dim mg2$(30)*128
	dim z$*10
	dim e$(4)*30
	dim gTmpCustomerAddress$(4)*30
	dim f$*12
	dim g(12)
	dim d(15)
	dim b(11)
	dim gb(10)
	dim pe$(4)*30
	dim at$(3)*40 ! (1)=company name, (2)=company addr, (3)=company address   ** POPULATED BY: fn_getCompanyAddr(mat at$)
	dim serviceName$(10)*20
	dim serviceCode$(10)*2
	dim tax_code$(10)*1
	dim penalty$(10)*1

	fnGetServices(mat serviceName$, mat serviceCode$, mat tax_code$,mat penalty$) ! ,mat subjectto,mat ordertoapply)
fnend
PrintBill_Basic: ! r: set prefrences for clients
	! PrintBill_Basic - dynamic pr bill program that works for multiple clients
	if env$('client')='French Settlement' then ! completed 4/23/16
		!  margins are top:.5, bottom:.2, left:.3, right:.2
		message1_line_count=1
		message1_max_len=52
		include_zero_bal=include_credit_bal=1
		forceWordProcessor$='atlantis'
		enable_service_from=1
		enable_service_to=1
	else if env$('client')='Campbell' then ! completed 4/23/16
		message1_line_count=0 ! 3
		message1_max_len=30
		enable_service_from=0
		enable_service_to=1
		include_zero_bal=include_credit_bal=1
	else if env$('client')='Raymond' then ! completed 4/23/16
		message1_line_count=3
		pa_enabled=1
		pa_orientation$='Landscape'
		include_zero_bal=include_credit_bal=1
		enable_service_from=1
		enable_service_to=1
	else if env$('client')='Cerro Gordo V' then
		message1_line_count=3
		message1_max_len=30
		enable_BulkSort=1
		enable_service_from=1
		enable_service_to=1
		include_zero_bal=include_credit_bal=1
	! else if env$('client')='Merriam Woods' then ! completed 5/5/16
	! 	message1_line_count=2
	! 	enable_service_from=1
	! 	enable_service_to=1
	! 	pa_enabled=1
	! 	pa_orientation$='Landscape'
	! 	enable_BulkSort=1
	! 	include_zero_bal=include_credit_bal=1
	! 	enable_customMargins=1 ! added 2/17/2020
	else if env$('client')='Omaha' then ! 8/10/2016
		message1_line_count=3
		message1_max_len=30
		enable_service_from=1
		enable_service_to=1
		include_zero_bal=include_credit_bal=1
		message2_line_count=12
		message2_max_len=24
	else if env$('client')='Blucksberg' then
		pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
		enable_BulkSort=2
		message1_line_count=13
		message1_max_len=95
		message_onscreen_alignment=2
		enable_service_from=1
		enable_service_to=1
		include_zero_bal=include_credit_bal=1
		enablePriorBillingDate=1
		enableLastPenaltyDate=1
	else if env$('client')='Pennington' then ! 12/07/2016
		message1_line_count=3
		message1_max_len=40
		enable_service_from=1
		enable_service_to=1
		include_zero_bal=include_credit_bal=1
	else if env$('client')='Edinburg' then ! 12/08/2016
		message1_line_count=3
		message1_max_len=30
		pa_enabled=1
		enable_service_from=1
		enable_service_to=1
		pa_orientation$='Landscape'
		include_zero_bal=include_credit_bal=1
	else if env$('client')='Choctaw' then
		enable_service_from=1
		enable_service_to=1
		! message1_line_count=3
		! message1_max_len=30
		enable_BulkSort=1
		include_zero_bal=include_credit_bal=1
		! pause : pa_enabled=0
		forceWordProcessor$='atlantis'
	else if env$('client')='Exeter' then
		basePenaltyOnCurrentBillOnly=1
		message1_line_count=3
		pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
		pa_orientation$='Landscape'
		include_zero_bal=include_credit_bal=1
		message2_line_count=2
		message2_max_len=30
	!       message1_line_count=3
	!       pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
	!       pa_orientation$='Landscape'
	!       include_zero_bal=include_credit_bal=1
	!       basePenaltyOnCurrentBillOnly=1
	else if env$('client')='Diamond' then ! 04/26/2017 ! three per page RTF
		message1_line_count=3
		include_zero_bal=include_credit_bal=1
		enable_BulkSort=1
	else if env$('client')='Billings' then
		message1_line_count=3
		include_zero_bal=include_credit_bal=1
		enable_BulkSort=3 ! 1   12/14/2021 - sort by alphasort+route+sequence
	else if env$('client')='GreeneCo' then ! 06/05/2018 ! 8.5x11, 4 per page - hit pre-printed form
		! enableNewGreeneCoBill=1
		! if enableNewGreeneCoBill then
			message1_line_count=3
			pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
			pa_orientation$='Landscape'
			include_zero_bal=include_credit_bal=1
			message2_line_count=2
			message2_max_len=30
		! else
		! 	message1_line_count=3
		! 	message2_line_count=0
		! 	message1_max_len=52
		! 	pa_enabled=1 ! PrintAce
		! 	pa_orientation$='Landscape'
		! 	include_zero_bal=include_credit_bal=1
		! end if
	else if env$('client')='Galena' then ! 11/29/2018 Portrait two per page - hit pre-printed form
		message1_line_count=3
		message2_line_count=0
		message1_max_len=40
		pa_enabled=2 ! PDF
		! pa_orientation$='Landscape'
		include_zero_bal=include_credit_bal=1
		enable_cassSort=1

		enable_service_from=0 ! does not print prior reading date anyway.
		enable_service_to=1


	else !  default settings:  Findlay, Edison, Thomasboro
		message1_line_count=3
		pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
		pa_orientation$='Landscape'
		include_zero_bal=include_credit_bal=1
		message2_line_count=2
		message2_max_len=30
		enable_service_from=1
		enable_service_to=1
		! usPostagePermitNumber=0
		enable_customMargins=0
	end if
	! r: use the default settings but add a little extra to it
	if env$('client')='Findlay' then
		poundBeforeAccount$='#' ! only for diff - making sure things match - then take it back out. it's lame
		usPostagePermitNumber=1
		enable_BulkSort=1
		enableIsDueNowAndPayable=-1
		enableReturnServiceRequested=-1
	else if env$('client')='Edison' then
		usPostagePermitNumber=1
		penaltyFlatAmount=5
		relaceBulkSortCodeWithCustName=1
	else if env$('client')='Thomasboro' then ! added 7/25/2020
		poundBeforeAccount$='#' ! only for diff - making sure things match - then take it back out. it's lame
		usPostagePermitNumber=1
		enable_BulkSort=1
	else if env$('client')='Alien Electric' then
		usPostagePermitNumber=555
	end if
	! /r
!   enable_cassSort=1
! /r
! r: post client setup configuration, file opening, etc
	mat mg$(message1_line_count) : mat respc_mg1(message1_line_count)
	mat mg2$(message2_line_count) : mat respc_mg2(message2_line_count)
	fnLastBillingDate(d1)
	fncreg_read('Penalty Due Date',tmp$) : d4=val(tmp$)
	if days(d4,'mmddyy')<days(d1,'mmddyy') then
		if d4<>0 then
			d4$=''
			d4$&=date$(days(d1,'mmddyy'),'mm') ! get MM date from d1
			d4$&=date$(days(d4,'mmddyy'),'dd') ! get DD date from d4
			d4$&=date$(days(d1,'mmddyy'),'yy') ! get YY date from d1
			d4=val(d4$)
		end if
	end if
	! if env$('acsDeveloper')<>'' then pause
	open #hCustomer1=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k  ! open in account order
	open #hCustomer2=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k  ! open in route-sequence
	open #h_ubtransvb=fnH: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,i,k
	dim c$(0)*200
	dim cN(0)
	hCustomer=fn_openFio('UB Customer',mat c$,mat cN, 1)
	if enable_BulkSort then gosub BulkSort
	if enable_cassSort then gosub Sort1
! /r
Screen1: ! r:
	starting_key$="" : route_filter=testMode=0 : respc=0
	fnTos
	pf=27 : lc=0
	fnLbl(lc+=1,1,"Payment Due Date:",25,1) : fnLbl(lc,35,"(Penalty Date)",25,1)
	fnTxt(lc,pf,8,8,1,"1",0,tt$)
	resp$(respc_penalty_due_date:=respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(lc+=1,1,"Date of Billing:",25,1)
	fnTxt(lc,pf,8,8,1,"1")
	resp$(respc_billing_date:=respc+=1)=cnvrt$("pic(zzzzzz)",d1)

	if enablePriorBillingDate then
		fnLbl(lc+=1,1,"Prior Date of Billing:",25,1)
		fnTxt(lc,pf,8,8,1,"1")
		resp$(resp_billing_date_prior:=respc+=1)=cnvrt$("pic(zzzzzz)",billing_date_prior)
	end if
	if enableLastPenaltyDate then
		fnLbl(lc+=1,1,"Last Penalty Date:",25,1)
		fnTxt(lc,pf,8,8,1,"1")
		resp$(resp_lastPenaltyDate:=respc+=1)=cnvrt$("pic(zzzzzz)",lastPenaltyDate)
	end if
	if enable_service_from or enable_service_to then
		lc+=1
		if enable_service_from then
			fnLbl(lc+=1,1,"Service From:",25,1)
			fnTxt(lc,pf,8,8,1,"1",0,"This field can be used to override the Prior Reading Date in the customer's record")
			fnLbl(lc,pf+8+4,"(only use to override the Prior Reading Dates from individual customer records)")
			resp$(respc_service_from:=respc+=1)=cnvrt$("pic(zzzzzz)",serviceFromOverride)
		end if
		if enable_service_to then
			fnLbl(lc+=1,1,"Service To:",25,1)
			fnTxt(lc,pf,8,8,1,"1",0,"This field can be used to override the Current Reading Date in the customer's record")
			fnLbl(lc,pf+8+4,"(only use to override the Current Reading Dates from individual customer records)")
			resp$(respc_service_to:=respc+=1)=cnvrt$("pic(zzzzzz)",serviceToOverride)
		end if
	end if
	lc+=1
	fnLbl(lc+=1,1,"Starting Route/Sequence:",25,1)
	fnComboF("ubm-act-nam",lc,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,1,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	! fnComboF("ubm-act-nam",lc,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	resp$(respc_start_place:=respc+=1)="[All]"
	lc+=1
	fnLbl(lc+=1,1,"Route Number:",25,1)
	fncmbrt2(lc,pf)
	resp$(respc_route:=respc+=1)="[All]"
	lc+=1
	fnLbl(lc+=1,1,"Filter:",25,1)
	fnOpt(lc,pf,"All")
	resp$(respc_filter_none:=respc+=1)='True'
	fnOpt(lc+=1,pf,"Select Individuals")
	resp$(respc_filter_individuals:=respc+=1)='False'
	fnOpt(lc+=1,pf,"Past Due Only")
	resp$(respc_filter_past_due:=respc+=1)='False'
	fnOpt(lc+=1,pf,"All Except Past Due")
	resp$(respc_filter_not_past_due:=respc+=1)='False'

	if message1_line_count then
		lc+=1
		if message2_line_count then
			fnLbl(lc+=1,1,"Message 1:",25,1)
		else
			fnLbl(lc+=1,1,"Message:",25,1)
		end if
	end if
	lc-=1
	if message1_line_count>0 and message1_max_len=0 then message1_max_len=30
	for mg1_item=1 to message1_line_count
		fnTxt(lc+=1,pf,max(30,int(message1_max_len*.75)),message1_max_len,message_onscreen_alignment)
		respc_mg1(mg1_item)=respc+=1
		fncreg_read('bill message '&str$(mg1_item),resp$(respc_mg1(mg1_item)))
		resp$(respc_mg1(mg1_item))=resp$(respc_mg1(mg1_item))(1:message1_max_len)
	next mg1_item
	if message2_line_count then
		lc+=1
		fnLbl(lc+=1,1,"Message 2:",25,1)
	end if
	lc-=1
	for mg2_item=1 to message2_line_count
		fnTxt(lc+=1,pf,min(message2_max_len,30),message2_max_len,message_onscreen_alignment)
		respc_mg2(mg2_item)=respc+=1
		fncreg_read('bill message2 '&str$(mg2_item),resp$(respc_mg2(mg2_item)))
		resp$(respc_mg2(mg2_item))=resp$(respc_mg2(mg2_item))(1:message2_max_len)
	next mg2_item
	fnCmdKey('&Margins',ckey_margins:=1021,0,0,'Manually adjust margins for hitting forms')
	fnCmdKey('&Test'   ,ckey_test:=1023   ,0,0,'Prints 4 Bills and then stops.')
	fnCmdKey('&Print'  ,ckey_print:=1     ,1)
	fnCmdKey('&Cancel' ,5,0,1)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if ckey_test and ckey=ckey_test then
		testMode=1
		ckey=ckey_print
	end if
	d1=val(resp$(respc_billing_date))
	d4=val(resp$(respc_penalty_due_date))
	if enablePriorBillingDate then
		billing_date_prior=val(resp$(resp_billing_date_prior))
		billing_date_prior=date(days(billing_date_prior,'mmddyy'),'ccyymmdd')
	end if
	if enableLastPenaltyDate then
		lastPenaltyDate=val(resp$(resp_lastPenaltyDate))
		lastPenaltyDate=date(days(lastPenaltyDate,'mmddyy'),'ccyymmdd')
	end if

	if enable_service_from then
		serviceFromOverride=val(resp$(respc_service_from))
	end if
	if enable_service_to then
		serviceToOverride=val(resp$(respc_service_to))
	end if
	if resp$(respc_start_place)="[All]" then
		starting_key$=""
	else
		starting_key$=lpad$(trim$(resp$(6)(1:10)),10)
		if trim$(starting_key$)<>"" then
			read #hCustomer1,using 'form pos 1,c 10,pos 1741,n 2,n 7',key=starting_key$,release: z$,route,sequence nokey Screen1
			!    starting_place_enabled=1
		end if
	end if
	if resp$(respc_route)="[All]" then
		route_filter=0
	else
		route_filter=val(resp$(respc_route))
	end if
	if resp$(respc_filter_none        )='True' then filter_none=1          else filter_none=0
	if resp$(respc_filter_individuals )='True' then filter_selected_only=1 else filter_selected_only=0
	if resp$(respc_filter_past_due    )='True' then filter_past_due_only=1 else filter_past_due_only=0
	if resp$(respc_filter_not_past_due)='True' then filter_no_past_due=1   else filter_no_past_due=0
	for mg1_item=1 to message1_line_count
		mg$(mg1_item)=resp$(respc_mg1(mg1_item))
		fncreg_write('bill message '&str$(mg1_item),mg$(mg1_item))
	next mg1_item
	for mg2_item=1 to message2_line_count
		mg2$(mg2_item)=resp$(respc_mg2(mg2_item))
		fncreg_write('bill message2 '&str$(mg2_item),mg2$(mg2_item))
	next mg2_item
	fncreg_write('Penalty Due Date',str$(d4))
	if enable_customMargins and ckey=ckey_margins then
		fn_ask_margins
		goto Screen1
	end if
! /r
! r: initalize and open things

	if enable_BulkSort=1 then
		restore #hAddr:
	else if enable_BulkSort=2 then
		restore #hBulk2:
	else if enable_BulkSort=3 then
		restore #hBulk3:
	else if enable_cassSort then
		restore #hAddr:
	else
		restore #hCustomer2:
	end if

	if trim$(starting_key$)="" and route_filter=0 then ! if no beginning account or starting route #, start at beginning of file
		restore #hCustomer2,key>="         ":
	else if trim$(starting_key$)<>"" then
		restore #hCustomer2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey Screen1
	else if trim$(starting_key$)="" and route_filter>0 then ! selected a route and no beginning Account
		restore #hCustomer2,key>=cnvrt$("pic(zz)",route_filter)&"       ":
	end if



	gosub BUD1
	if pa_enabled=1 then
		fnpa_open(pa_orientation$)
	else if pa_enabled=2 then
		fnpa_open(pa_orientation$,'','PDF')
	else
		fnopenprn
	end if
	! IF filter_selected_only=0 THEN GOSUB Sort1
! /r

MainLoop: ! r: main loop
	if filter_selected_only=1 then goto ScrAskIndividual
	if enable_BulkSort=1 then
		read #hAddr,using 'form pos 1,pd 3': r6 eof Finis
		read #hCustomer1,using F_customerA,rec=r6,release: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceToMmddYy,serviceFromMmddYy,est noRec MainLoop ! READ_BulkSort
	else if enable_BulkSort=2 then
		read #hBulk2,using 'form pos 22,c 10': z$ eof Finis
		read #hCustomer1,using F_customerA,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceToMmddYy,serviceFromMmddYy,est nokey MainLoop ! READ_CASSSORT
	else if enable_BulkSort=3 then
		read #hBulk3,using 'form pos 17,c 10': z$ eof Finis
		read #hCustomer1,using F_customerA,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceToMmddYy,serviceFromMmddYy,est nokey MainLoop ! READ_CASSSORT
	else if enable_cassSort then
		read #hAddr,using 'form pos 1,pd 3': r6 eof Finis
		read #hSort1Sequence,using "form pos 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ noRec MainLoop ! READ_CASSSORT
		read #hCustomer1,using F_customerA,key=z$,release: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceToMmddYy,serviceFromMmddYy,est nokey MainLoop ! READ_CASSSORT
	else
		read #hCustomer2,using F_customerA: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceToMmddYy,serviceFromMmddYy,est eof Finis
	end if
	F_customerA: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1831,n 9

	readCount+=1
	if route_filter and route_filter<>route and ~enable_BulkSort then goto Finis
	if enable_BulkSort and route_filter and route_filter<>route then goto MainLoop

	enableLastBillingDateFilter=0
	if enableLastBillingDateFilter and f<>d1 then goto MainLoop
	if f<>d1 then goto MainLoop

	if bal=0 and ~include_zero_bal then goto MainLoop
	if bal<0 and ~include_credit_bal then goto MainLoop
	gosub BUD2 ! determine if budget customer
	! if ~starting_place_enabled then
	!   if st1$=z$ then
	!     starting_place_enabled=0  ! starting_place_enabled used to be st1
	!   else
	!     goto MainLoop
	!   end if
	! end if

	AfterCustomerRead: !

	! an easy second read to gather the mat c$ and mat cN
	read #hCustomer,using form$(hCustomer),key=z$,release: mat c$,mat cN

	! if trim$(z$)='305230.05' then pause


	pb=bal-g(11)
	if filter_past_due_only and pb<=0 then goto MainLoop
	if filter_no_past_due and pb>0 then goto MainLoop
	fncustomer_address(z$,mat pe$) ! read alternate billing address

	fn_override_service_date(serviceFromMmddYy,serviceToMmddYy,serviceFromOverride,serviceToOverride)
	! r: pr bill routine
	if env$('client')='French Settlement' then
		fn_print_bill_fsg(pb,mat g,mat d,bal,final,mat pe$,d4,mat e$,z$,mat mg$,budgetpb,serviceFromMmddYy,serviceToMmddYy)
	else if env$('client')='Campbell' then
		fn_print_bill_campbell(z$,mat mg$,serviceFromMmddYy,serviceToMmddYy)
	else if env$('client')='Raymond' then
		fn_print_bill_raymond(z$,mat mg$, "Mailing Address: P O Box 87")
	else if env$('client')='Cerro Gordo V' then
		fn_print_bill_cerro(z$,mat mg$,mat penalty$,serviceFromMmddYy,serviceToMmddYy)
	! else if env$('client')='Merriam Woods' then
	! 	fn_print_bill_merriam(z$,mat mg$,serviceFromMmddYy,serviceToMmddYy)
	else if env$('client')='Blucksberg' then
		fn_print_bill_blucksberg(z$,mat mg$,billing_date_prior,serviceFromMmddYy,serviceToMmddYy)
	else if env$('client')='Omaha' then
		fn_print_bill_omaha(z$,mat mg$,mat mg2$,serviceFromMmddYy,serviceToMmddYy,mat penalty$)
	else if env$('client')='Pennington' then
		fn_print_bill_pennington(z$,mat mg$,mat mg2$,serviceFromMmddYy,serviceToMmddYy,d4)
		! fn_print_bill_Exeter(z$,mat mg$,serviceFromMmddYy,serviceToMmddYy,d4)
	else if env$('client')='Edinburg' then
		fn_print_bill_edinburg(z$,mat mg$,d1,serviceFromOverride,serviceToOverride,d4) ! ,serviceTo,penaltyDueDate)  ! ~and)
	else if env$('client')='Billings' or env$('client')='Diamond' then
		fn_print_bill_billings(mat mg$,mat g,mat b,bal,mat penalty$,d1,serviceFromMmddYy,serviceToMmddYy,d4,mat pe$,final$,z$) !
	else if env$('client')='Choctaw' then
		fn_print_bill_choctaw(z$,mat g,mat b,mat penalty$,d1,serviceFromMmddYy,serviceToMmddYy,d4,mat e$,final)
		! fn_print_bill_choctaw(z$,mat g,mat b,mat penalty$,d1,serviceFromMmddYy,serviceToMmddYy,d4,mat pe$,final)
	! else if env$('client')='GreeneCo' and ~enableNewGreeneCoBill then
	! 	fn_print_bill_GreeneCo
	else if env$('client')='Galena' then
		fn_print_bill_galena(serviceToMmddYy)
	else ! GreeneCo, Exeter, Findlay, etc
		if enableReturnServiceRequested=>0 then enableReturnServiceRequested=1
		if enableIsDueNowAndPayable=>0 then enableIsDueNowAndPayable=1
		fn_print_bill_standard_pdf_a(z$,mat mg$,mat mg2$,enableIsDueNowAndPayable,enableReturnServiceRequested)
	end if
	! /r
	billsPrintedCount(2)+=1 ! accumulate totals
	if env$('acsDeveloper')<>'' and sum(mat billsPrintedCount)>val(env$('UB_Limit'))+1 and ~ubLimitExceedAlreadyNotified then
		msgbox('UB_Limit exceeded: One or more bills have not been generated because your license limits you to '&env$('UB_Limit')&' bills.') : ubLimitExceedAlreadyNotified=1
		goto Finis
	end if
	if testMode and billsPrintedCount(2)=>4 then
		fn_closeBillPrinter
		restore #hCustomer1:
		mat billsPrintedCount=(0)
		goto Screen1
	end if
	! pr 'billsPrintedCount(2)=';billsPrintedCount(2) : pause ! if testMode and billsPrintedCount(2)=>4 then goto Screen1
goto MainLoop ! /r

def fn_ask_margins
	dim ub4upBill_data$(4)
	AskMarginsReadBeforeScreen: !
	fnreg_read('ub4upBill Margin Top 1' ,ub4upBill_data$(1),  '5',1) ! bills 1 and 2
	fnreg_read('ub4upBill Margin Top 2' ,ub4upBill_data$(2),'113',1) ! bills 3 and 4      ymargin was 108
	fnreg_read('ub4upBill Margin Left 1',ub4upBill_data$(3),  '0',1) ! bills 1 and 3
	fnreg_read('ub4upBill Margin Left 2',ub4upBill_data$(4),'142',1) ! bills 2 and 4      xmargin was 137
	fnTos : lc=0
	mylen=39 : mypos=mylen+2
	fnLbl(lc+=1,1,"Bill 1 and 2 - Distance from Top (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,"Bill 3 and 4 - Distance from Top (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	lc+=1
	fnLbl(lc+=1,1,"Bill 1 and 3 - Distance from Left (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnLbl(lc+=1,1,"Bill 2 and 4 - Distance from Left (mm):",mylen,1)
	fnTxt(lc,mypos,3,0,1,'30')
	fnCmdKey("&Save",1,1)
	fnCmdKey("Default",ckey_default:=1022)
	fnCmdKey("&Cancel",5,0,1)
	fnAcs(mat ub4upBill_data$,ckey)
	if ckey=ckey_default then
		fnreg_write('ub4upBill Margin Top 1' ,'')
		fnreg_write('ub4upBill Margin Top 2' ,'')
		fnreg_write('ub4upBill Margin Left 1','')
		fnreg_write('ub4upBill Margin Left 2','')
		goto AskMarginsReadBeforeScreen
	else if ckey<>5 then
		fnreg_write('ub4upBill Margin Top 1' ,ub4upBill_data$(1))
		fnreg_write('ub4upBill Margin Top 2' ,ub4upBill_data$(2))
		fnreg_write('ub4upBill Margin Left 1',ub4upBill_data$(3))
		fnreg_write('ub4upBill Margin Left 2',ub4upBill_data$(4))
	end if
fnend
def fn_mg2$*80(; m2forcecnt,return$*80)
	if m2forcecnt then
		m2item=m2forcecnt
	else
		m2item+=1
	end if
	return$=''
	if m2item<=udim(mat mg2$) then
		return$=mg2$(m2item)
	end if
	fn_mg2$=return$
fnend
ScrAskIndividual: ! r: account selection screen
	fnTos
	fnLbl(1,1,"Account (blank to stop)",31,1)
	if trim$(starting_key$)="" then
		if z$<>"" then
			fnLbl(3,1,"Last Account entered was "&z$,44,1)
		else
			fnLbl(3,1,'',44,1)
		end if
	end if
	fncmbact(1,17)
	resp$(1)=starting_key$
	fnCmdSet(11)
	ckey=fnAcs(mat resp$)
	if ckey=5 or trim$(resp$(1))='' then goto Finis
	starting_key$=lpad$(trim$(resp$(1)(1:10)),10)
	read #hCustomer1,using F_customerA,key=starting_key$,release: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceFromMmddYy,serviceToMmddYy,est nokey ScrAskIndividual
goto AfterCustomerRead ! /r

def fn_closeBillPrinter
	if pa_enabled then
		fnpa_finis
	else if sum(billsPrintedCount)>0 then
		fncloseprn( forceWordProcessor$)
	end if
fnend
Finis: ! r:
	close #hCustomer: ioerr ignore
	hCustomer=0
	close #hCustomer1: ioerr ignore
	hCustomer1=0
	close #hCustomer2: ioerr ignore
	hCustomer2=0
	close #h_ubtransvb: ioerr ignore
	h_ubtransvb=0
	close #hAddr: ioerr ignore
	hAddr=0
	close #hBulk2: ioerr ignore
	hBulk2=0
	fn_closeBillPrinter
	! if sum(billsPrintedCount)=0 then pct=0 else pct=billsPrintedCount(2)/sum(billsPrintedCount)*100
	if testMode then
		fnChain(program$)
	else
		fnTos : respc=lc=0
		mylen=23 : mypos=mylen+2
		fnLbl(lc+=1,1,'Total Bills Printed:',mylen,1) : fnTxt(lc,mypos,10,0,1,"",1) : resp$(respc+=1)=cnvrt$('N 10',sum(billsPrintedCount))
		if env$('acsDeveloper')<>'' then
			fnLbl(lc+=1,1,'Total Customers Read:',mylen,1) : fnTxt(lc,mypos,10,0,1,"",1) : resp$(respc+=1)=cnvrt$('N 10',readCount)
		end if
		fnCmdSet(52)
		ckey=fnAcs(mat resp$)
	end if
goto Xit ! /r
Xit: fnXit
Bud1: ! r: Open #81 BudMstr and #82 BudTrans bud1=1
	bud1=0
	dim ba(13),badr(2),bt1(14,2)
	open #81: 'Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr',i,i,k ioerr EoBud1
	open #82: 'Name=[Q]\UBmstr\BudTrans.h[cno],Shr',i,i,r
	bud1=1
	EoBud1: !
return  ! /r
Bud2: ! r: the heart of it...
	totba=bd1=budgetpb=havebudget=0
	if bud1=0 then goto EoBud2
	read #81,using 'form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3',key=z$: z$,mat ba,mat badr nokey EoBud2
	havebudget=1
	for j=2 to 12
		totba=totba+ba(j)
	next j
	if totba=0 then havebudget=0: goto EoBud2
	ta1=badr(1)
	do
		if ta1=0 then goto EoBud2
		read #82,using 'form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3',rec=ta1: z$,mat bt1,nba noRec EoBud2
		if bt1(14,1)<=0 then
			! if bt1(1,2)=f then goto 3350 ! ignore current budget billing record
			budgetpb+=bt1(5,1) ! add up prior balance for budget billing customers (any unpaid not including current bill)
			bd1+=1
			if bd1>5 then
				goto EoBud2
			end if
		end if
		ta1=nba
	loop
	EoBud2: !
return  ! /r
def fn_getCompanyAddr(mat at$)
	open #h_company=fnH: 'Name=[Q]\UBmstr\Company.h[cno],Shr',i,i
	read #h_company,using 'form pos 41,2*C 40': at$(2),at$(3)
	close #h_company:
	h_company=0
	at$(1)=env$('cnam')
	z=21
	at$(1)=trim$(at$(1))(1:z)
	x=len(at$(1)) : y=z-x
	at$(1)=rpt$(' ',int(y/2))&at$(1)
	z=26
	for j=2 to udim(at$)
		at$(j)=trim$(at$(j))(1:z)
		x=len(at$(j))
		y=z-x
		at$(j)=rpt$(' ',int(y/2))&at$(j)
	next j
fnend
def fn_override_service_date(&serviceFrom,&serviceTo,serviceFromOverride,serviceToOverride)
	! serviceFrom / serviceFromOverride= Prior Reading Date (from customer record)
	! serviceTo   / serviceToOverride= Current Reading Date (from customer record)
	! serviceToOverride   / extra(3)=Override Service To   Date (ask on Screen)
	! serviceFromOverride / extra(4)=Override Service From Date (ask on Screen)
	if serviceFromOverride then serviceFrom=serviceFromOverride
	if serviceToOverride   then serviceTo=serviceToOverride
fnend
def fn_pay_after_amt(; returnN)
	if bal<=0 then returnN=round(bal,2) else returnN=bal+g(10)
	fn_pay_after_amt=returnN
fnend


! our gold standard and default print bill routine used by more and more people
def fn_print_bill_standard_pdf_a(z$,mat mg$; mat mg2$,enableIsDueNowAndPayable,enableReturnServiceRequested) ! inherrits all those local customer variables too
! based of fn_print_bill_raymond, but enhanced
	if ~setup_standardA then
		setup_standardA=1
		lyne=3
		fn_getCompanyAddr(mat at$)
	end if
	subtotalAmt=g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9)
	payLateAmount=0
	if penaltyFlatAmount then
		if bal>0 then payLateAmount=bal+penaltyFlatAmount
	else if basePenaltyOnCurrentBillOnly then
		payLateAmount=bal+round(subtotalAmt*.10,2)
	else ! base Penalty on full amount
		payLateAmount=bal+round(bal*.10,2)
	end if
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
	billOnPageCount+=1
	if billOnPageCount=1 then xmargin=0 : ymargin=0
	if billOnPageCount=2 then xmargin=139 : ymargin=0
	if billOnPageCount=3 then xmargin=0 : ymargin=108
	if billOnPageCount=4 then xmargin=139 : ymargin=108 : billOnPageCount=0

	fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
	fnpa_fontbold(1)
	fnpa_fontsize(12)
	fnpa_font
	fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin)
	fnpa_font('Lucida Console')
	fnpa_fontsize
	fnpa_fontbold
	fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
	fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
	if relaceBulkSortCodeWithCustName then
		fnpa_txt(poundBeforeAccount$&trim$(z$),xmargin+4,lyne*5+ymargin)
		fnpa_fontsize(8)
		fnpa_txt(pe$(1)(1:20),xmargin+28,lyne*5+ymargin)
		fnpa_fontsize
	else
		fnpa_txt(poundBeforeAccount$&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
	end if

	fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
	fnpa_txt('From: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceFromOverride)&'  To: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceToOverride),xmargin+2,lyne*7+ymargin)
	if enableIsDueNowAndPayable>0 then
		fnpa_txt('Is due now and payable.',xmargin+2,lyne*8+ymargin)
	end if
	fnpa_txt('Billing Date: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1),xmargin+2,lyne*11+ymargin)
	fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
	fnpa_txt('Reading',xmargin+10,lyne*13+ymargin)
	fnpa_txt('Usage',xmargin+33,lyne*13+ymargin)
	fnpa_txt('Charge',xmargin+50,lyne*13+ymargin)

	! PRINTGRID: !
	meter=14
	fnpa_fontsize(8)
	if g(1)<>0 then
		fnpa_txt(serviceCode$(1),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(1),0,9),xmargin+6 ,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(3),0,9),xmargin+25,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(1),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(2)<>0 then
		fnpa_txt(serviceCode$(2),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(2),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(3)<>0 or d(7)<>0 then
		if d(5)=0 and d(7)=0 then ! there are no readings nor usage - pr the name instead of the code
			fnpa_txt(serviceName$(3),xmargin+1,lyne*(meter+=1)+ymargin)
		else
			fnpa_txt(serviceCode$(3),xmargin+1,lyne*(meter+=1)+ymargin)
			fnpa_txt(fnformnumb$(d(5),0,9),xmargin+6,lyne*meter+ymargin)
			fnpa_txt(fnformnumb$(d(7),0,9),xmargin+25,lyne*meter+ymargin)
		end if
		fnpa_txt(fnformnumb$(g(3),2,9),xmargin+45,lyne*meter+ymargin)
	end if  ! g(3)<>0 or d(7)<>0
	if g(4)<>0 then
		fnpa_txt(serviceCode$(4),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(11),0,9),xmargin+25,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(4),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(5)<>0 then
		fnpa_txt(serviceName$(5),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(5),2,9),xmargin+45,lyne*meter+ymargin)
		! fnpa_txt(fnformnumb$(g(5),2,9),xmargin+91+8,lyne*meter+ymargin)
	end if  ! g(5)<>0
	if g(6)<>0 then
		fnpa_txt(serviceName$(6),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(6),2,9),xmargin+43,lyne*meter+ymargin)
	end if  ! g(6)<>0
	if g(7)<>0 then
		fnpa_txt(serviceName$(7),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(7),2,9),xmargin+43,lyne*meter+ymargin)
	end if  ! g(7)=0
	if g(8)<>0 then
		fnpa_txt(serviceName$(8),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(8),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(9)<>0 then
		fnpa_txt(serviceName$(9),xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(9),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if pb><0 then
		! if trim$(z$)='1900003.10' then pause
		fnpa_line(xmargin+46,lyne*(meter+=1)+ymargin,15,0)
		fnpa_txt('   Subtotal',xmargin+1,lyne*(meter+=.25)+ymargin)
		fnpa_txt(fnformnumb$(subtotalAmt,2,9),xmargin+45,lyne*meter+ymargin)
		fnpa_txt('Previous Balance',xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(pb,2,9),xmargin+45,lyne*meter+ymargin)
	end if
	fnpa_fontsize

	if estimatedate=d1 then fnpa_txt('Bill estimated!',xmargin+1,lyne*21+ymargin)
	fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
	fnpa_txt('   Pay By  '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+1,lyne*24+ymargin)
	! fnpa_txt("Pay By "&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+1,lyne*24+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
	if payLateAmount>0 then
		fnpa_txt('Pay After  '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+1,lyne*25+ymargin)
		! if basePenaltyOnCurrentBillOnly then
			fnpa_txt(fnformnumb$(payLateAmount,2,9),xmargin+42,lyne*25+ymargin)
		! end if
	end if
	fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
	for satItem=1 to udim(mat mg2$)
		fnpa_txt(mg2$(satItem),xmargin+1,lyne*(26+satItem)+ymargin)
	nex satItem

	fnpa_fontsize(7)
	fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
	fnpa_line(xmargin+90,ymargin+0,7,0)
	fnpa_line(xmargin+90,ymargin+2.8,7,0)
	fnpa_line(xmargin+90,ymargin+5.6,7,0)
	fnpa_line(xmargin+90,ymargin+8.4,7,0)
	fnpa_line(xmargin+90,ymargin+11.2,7,0)
	fnpa_line(xmargin+90,ymargin+14,7,0)
	fnpa_line(xmargin+90,ymargin+17,7,0)
	fnpa_txt("   Pre-Sorted",xmargin+100,lyne*1-1+ymargin)
	fnpa_txt("First Class Mail",xmargin+100,lyne*2-1+ymargin)
	fnpa_txt("  U.S. Postage  ",xmargin+100,lyne*3-1+ymargin)
	fnpa_txt("      Paid",xmargin+100,lyne*4-1+ymargin)
	fnpa_txt("  Permit No "&str$(usPostagePermitNumber),xmargin+100,lyne*5-1+ymargin)
	fnpa_fontsize(9)
	if enableReturnServiceRequested>0 then
	fnpa_txt("Return Service Requested",xmargin+68+12,lyne*7.6+.2+ymargin-6)
	end if
	fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
	fnpa_txt("side with payment to:",xmargin+68,lyne*8+ymargin)
	fnpa_txt(env$('cnam'),xmargin+68,lyne*9+ymargin)
	fnpa_fontsize
	fnpa_txt('Pay By '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+68,lyne*11+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
	if payLateAmount>0 then
		fnpa_txt('After '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+68,lyne*12+ymargin)
		fnpa_txt(fnformnumb$(payLateAmount,2,9),xmargin+106,lyne*12+ymargin)
	end if
	fnpa_fontsize(9)
	addy=14
	fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
	addy+=1
	fnpa_fontsize
	if df$="Y" then
		fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	if c4>0 then
		fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	fnpa_txt(poundBeforeAccount$&trim$(z$)&' '&bulk$,(xmargin+68),lyne*(addy+=1)+ymargin)
	if pe$(1)<>"" then
		fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(2)<>"" then
		fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(3)<>"" then
		fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(4)<>"" then
		fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if billOnPageCount=1 then checkx=1.375 : checky=3.6875
	if billOnPageCount=2 then checkx=6.75 : checky=3.6875
	if billOnPageCount=3 then checkx=1.375 : checky=7.9375
	if billOnPageCount=0 then checkx=6.75 : checky=7.9375
	bc$=""
	if trim$(bc$)<>"" then fnpa_barcode(checkx,checky,bc$)
	if billOnPageCount=0 then
		fnpa_newpage
	end if
fnend

def fn_print_bill_fsg(pb,mat g,mat d,bal,final,mat pe$,d4,mat e$,z$,mat mg$,budget,serviceFromDate,serviceToDate) ! french settlement gas
	! pre-print calculations
	if pb<>0 then pb$="Prior Balance" else pb$=""
	!   if g(1)=0 then t1$="" else t1$="WTR"
	!   if g(2)=0 then t2$="" else t2$="SWR"
	if g(3)=0 then t3$="" else t3$="Capital Surcharge"
	if g(4)=0 then t4$="" else t4$="GAS"
	if g(5)=0 then t5$="" else t5$="Purchased Gas Adj."
	if g(6)=0 then t6$="" else t6$="Inspection Fee"
	if g(7)=0 then t7$="" else t7$="Deposit Interest"
	if g(8)=0 then
		t8$=""
	else if g(8)<0 and final>0 then
		t8$="Deposit Ref"
		if g(8) and g(3) then t8$="Dep Ref"
	else
		t8$="Other"
	end if
	if g(9)=0 then t9$="" else t9$="La. Sales Tax"
	! If D(10)=1 Then eST$="Bill Estimated" Else eST$=""
	if final>0 then final$="Final Bill" else final$=""
	if budget>0 then bud$="Budgeted Amount:"&trim$(cnvrt$("Pic($$,$$$.##",budget)) else bud$=""
	if bal<=0 then g(10)=0
	gross=max(bal+g(10),0)
	! actual Bill Printing
	F_PR_TABLE_AND_ADDR_1: form pos 1,nz 6,nz 7,nz 7,x 2,c 3,pos 28,nz 8.2,pos 39,c 30
	F_PR_TABLE_AND_ADDR_2: form pos 1,c 20,pos 28,nz 8.2,pos 39,c 52


	pr #255,using 'form pos 25,c 10': z$
	pr #255,using 'form pos 38,c 10,skip 4': z$
	pr #255,using F_PR_TABLE_AND_ADDR_1: d(9),d(10),d(11),t4$,g(4),pe$(1)
	pr #255,using F_PR_TABLE_AND_ADDR_2: t5$,g(5),pe$(2) ! Purchased Gas Adj.
	pr #255,using F_PR_TABLE_AND_ADDR_2: t6$,g(6),pe$(3) ! Inspection Fee
	pr #255,using F_PR_TABLE_AND_ADDR_2: t7$,g(7),pe$(4) ! Deposit Interest
	if ~g(8) and g(3) then
		pr #255,using F_PR_TABLE_AND_ADDR_2: t3$,g(3),mg$(1)! cap surcharge
	else if g(8) and g(3) then
		pr #255,using F_PR_TABLE_AND_ADDR_2: t8$&" & Cap Sur",g(8)+g(3),mg$(1) ! Deposit Refund or Other and Cap surcharge
	else if g(8) and ~g(3) then
		pr #255,using F_PR_TABLE_AND_ADDR_2: t8$,g(8),mg$(1) ! Deposit Refund or Other
	else if ~g(8) and ~g(3) then
		pr #255,using F_PR_TABLE_AND_ADDR_2: '',0,mg$(1) ! none on this row
	end if
	! pr #255,using F_PR_TABLE_AND_ADDR_2: t8$,g(8),mg$(1) ! Deposit Refund or Other
	! pr #255,using F_PR_TABLE_AND_ADDR_2: t3$,g(3),""
	pr #255,using F_PR_TABLE_AND_ADDR_2: t9$,g(9),bud$(1:30) ! La. Sales Tax
	pr #255,using F_PR_TABLE_AND_ADDR_2: pb$,bal-g(11) ! Prior Balance
	pr #255,using 'form pos 22,c 10': final$
	pr #255: ""
	pr #255: "" ! mg$(1)  <-- messages can not pr there - it hits a lot of preprinted text there
	pr #255: "" ! mg$(2)
	pr #255: "" ! mg$(3)
	pr #255: ""
	count+=1
	pr #255: "" ! if count=2 or count=3 then pr #255: ""
	pr #255,using L340: serviceFromDate,serviceToDate,gross,bal,gross,d4,bal
	L340: form pos 1,pic(## ## ##),x 1,pic(## ## ##),n 9.2,pos 27,pic(-----.--),pos 37,n 9.2,x 3,pic(## ## ##),pic(-----.--)
	if count=1 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! pr #255: ! EXTRA LINE BETWEEN 1ST and 2nd bills
	if count=2 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! EXTRA LINE BETWEEN 2nd & 3rd bill
	if count=3 then count=0 : pr #255: newpage
fnend
def fn_print_bill_campbell(z$,mat mg$,serviceFrom,serviceTo)
	! correct margins are left=.4, top=.35, right=.2, bottom=.2
	! r: any and all necessary setup (except opening the printer) to pr one bill
	if ~pbcampbel_setup then
		pbcampbel_setup=1
		open #h_pbcampbel_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
		F_PBCAMPBEL_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6
		blankbefore=1
		blankafter=4
	end if
	read #h_pbcampbel_customer,using F_PBCAMPBEL_CUSTOMER,key=z$: z$,pbcampbel_meter_address$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceFromMmddYy,serviceToMmddYy
	fn_override_service_date(serviceFrom,serviceTo,serviceFromMmddYy,serviceToMmddYy)
	dim pbcampbel_addr$(4)*30
	dim pbcampbel_meter_address$*30
	fncustomer_address(z$,mat pbcampbel_addr$)
	! /r
	! r: pr that bill
	if blankbefore<>0 then
		for j=1 to blankbefore
			pr #255: ""
		next j
	end if
	pr #255,using 'form pos 1,C 30': e$(1) ! Line 1
	pr #255,using 'form pos 1,C 30': pbcampbel_addr$(1) ! Line 2
	pr #255,using 'form pos 1,C 30': pbcampbel_addr$(2) ! Line 3
	pr #255: "" ! Line 4
	pr #255: "" ! Line 5
	pr #255: "" ! Line 5
	pr #255: "" ! Line 6
	if g(3)=0 then
		e$=""
	else if a3=3 then
		e$="CE"
	else if a3=4 then
		e$="RH"
	else if a3=5 then
		e$="CH"
	else
		e$="RE"
	end if
	pr #255,using PBCAMPBEL_L1020: z$,e$,g(3),d(5),d(6),d(7),e$,g(3),"" ! line 7
	PBCAMPBEL_L1020: form pos 1,c 10,pos 13,c 2,pic(---,---.--),x 4,3*pic(zzzzzzzz),pos 58,c 2,x 1,pic(--,---.--),pos 74,c 21
	if g(1)=0 then e$="" else e$="WA"
	pr #255,using PBCAMPBEL_L1020: "",e$,g(1),d(1),d(2),d(3),e$,g(1),z$ ! Line 8
	if g(2)=0 then e$="" else e$="SW"
	pr #255,using PBCAMPBEL_L1080: e$,g(2),e$,g(2),pbcampbel_addr$(1) ! Line 9
	PBCAMPBEL_L1080: form pos 13,c 2,pic(---,---.--),pos 58,c 2,pic(---,---.--),pos 74,c 30
	PBCAMPBEL_L1082: form pos 13,c 2,pic(---,---.--),x 2,c 30,pos 58,c 2,pic(---,---.--),pos 74,c 30
	if g(5)=0 then e$="" else e$="SA"
	pr #255,using PBCAMPBEL_L1082: e$,g(5),pbcampbel_addr$(1),e$,g(5),pbcampbel_addr$(2) ! line 10
	if g(6)=0 then e$="" else e$="SL"
	pr #255,using PBCAMPBEL_L1082: e$,g(6),pbcampbel_addr$(2),e$,g(6),pbcampbel_addr$(3) ! line 11
	if g(9)=0 then e$="" else e$="TX"
	pr #255,using PBCAMPBEL_L1082: e$,g(9),pbcampbel_addr$(3),e$,g(9),pbcampbel_addr$(4) ! line 12
	if g(4)=0 then e$="" else e$="SC"
	pr #255,using PBCAMPBEL_L1082: e$,g(4),pbcampbel_addr$(4),e$,g(4),"" ! line 13
	if g(8)=0 then e$="" else e$="OC"
	pr #255,using PBCAMPBEL_L1080: e$,g(8),e$,g(8),"" ! line 14
	! If BAL-G(11)=0 Then e$="" Else e$="AR"
	pr #255: "" ! line 15
	pr #255: "" ! line 16
	pr #255: "" ! line 17
	pr #255: "" ! line 18
	!if count=3 then pr #255: "" ! skip down a line on the third one only
	pr #255,using 'form pos 1,pic(zz/zz/zz),pos 9,N 8.2,N 8.2,X 2,PIC(ZZ/ZZ/ZZ),pos 42,3*N 12.2,N 12.2': serviceTo,g(12),g(11),serviceTo,g(12),g(11),g(12),g(11) ! line 19
	if (count+1)/3=int((count+1)/3) then goto PBCAMPBEL_L1250
	if blankafter<>0 then
		for j=1 to blankafter
			pr #255: " "
		next j
	end if
	PBCAMPBEL_L1250: !
	billsPrintedCount(3)+=1
	count=count+1
	if count/3=int(count/3) then ! newpage on every third bill
		pr #255: newpage
	end if
	! /r
fnend
def fn_print_bill_raymond(z$,mat mg$; raymondAdditionalText$*128) ! inherrits all those local customer variables too
	if ~setup_raymond then
		setup_raymond=1
		lyne=3
		fn_getCompanyAddr(mat at$)
	end if
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
	billOnPageCount+=1
	if billOnPageCount=1 then xmargin=0 : ymargin=0
	if billOnPageCount=2 then xmargin=139 : ymargin=0
	if billOnPageCount=3 then xmargin=0 : ymargin=108
	if billOnPageCount=4 then xmargin=139 : ymargin=108 : billOnPageCount=0

	fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
	fnpa_fontbold(1)
	fnpa_fontsize(12)
	fnpa_font
	fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin)
	fnpa_font("Lucida Console")
	fnpa_fontsize
	fnpa_fontbold
	fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
	fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
	fnpa_txt('#'&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
	fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
	fnpa_txt('From: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceFromOverride)&'  To: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceToOverride),xmargin+2,lyne*7+ymargin)
	fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
	fnpa_txt('Billing Date: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1),xmargin+2,lyne*11+ymargin)
	fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
	fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
	fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
	fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)

	! PRINTGRID: !
	meter=14
	fnpa_fontsize(8)
	if g(1)<>0 then
		fnpa_txt("WTR",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(1),0,9),xmargin+6 ,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(3),0,9),xmargin+25,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(1),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(2)<>0 then
		fnpa_txt("SWR",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(2),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(4)<>0 then
		fnpa_txt(s4code$,xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(11),0,9),xmargin+25,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(4),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(5)<>0 then
		fnpa_txt("Trash",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(5),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if g(8)<>0 then
		fnpa_txt("MISC",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(8),2,9),xmargin+45,lyne*meter+ymargin)
	end if
	if pb><0 then
		fnpa_line(xmargin+46,lyne*(meter+=1)+ymargin,15,0)
		fnpa_txt("   Subtotal",xmargin+1,lyne*(meter+=.25)+ymargin)
		fnpa_txt(fnformnumb$(g(1)+g(2)+g(8),2,9),xmargin+45,lyne*meter+ymargin)
		fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(pb,2,9),xmargin+45,lyne*meter+ymargin)
	end if
	fnpa_fontsize
	!
	if estimatedate=d1 then fnpa_txt("Bill estimated!",xmargin+1,lyne*21+ymargin)
	fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
	fnpa_txt("Pay By "&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+1,lyne*24+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
	fnpa_txt("Pay After "&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+1,lyne*25+ymargin)
	fnpa_txt(fnformnumb$(bal+round(bal*.10,2),2,9),xmargin+42,lyne*25+ymargin)
	fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
	fnpa_txt(raymondAdditionalText$,xmargin+1,lyne*27+ymargin)
	!   fnpa_txt("Re-connect fee $??.00",XMARGIN+1,LYNE*28+YMARGIN)
	fnpa_fontsize(7)
	fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
	fnpa_line(xmargin+90,ymargin+0,7,0)
	fnpa_line(xmargin+90,ymargin+2.8,7,0)
	fnpa_line(xmargin+90,ymargin+5.6,7,0)
	fnpa_line(xmargin+90,ymargin+8.4,7,0)
	fnpa_line(xmargin+90,ymargin+11.2,7,0)
	fnpa_line(xmargin+90,ymargin+14,7,0)
	fnpa_line(xmargin+90,ymargin+17,7,0)
	fnpa_txt("   Pre-Sorted",xmargin+100,lyne*1-1+ymargin)
	fnpa_txt("First Class Mail",xmargin+100,lyne*2-1+ymargin)
	fnpa_txt("  U.S. Postage  ",xmargin+100,lyne*3-1+ymargin)
	fnpa_txt("      Paid",xmargin+100,lyne*4-1+ymargin)
	fnpa_txt("  Permit No 7",xmargin+100,lyne*5-1+ymargin)
	fnpa_fontsize(9)
	fnpa_txt("Return Service Requested",xmargin+68,lyne*7.6+ymargin-6)
	fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
	fnpa_txt("side with payment to:",xmargin+68,lyne*8+ymargin)
	fnpa_txt(env$('cnam'),xmargin+68,lyne*9+ymargin)
	fnpa_fontsize
	fnpa_txt('Pay By '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+68,lyne*11+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
	fnpa_txt('After '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':',xmargin+68,lyne*12+ymargin)
	fnpa_txt(fnformnumb$(bal+round(bal*.10,2),2,9),xmargin+106,lyne*12+ymargin)
	fnpa_fontsize(9)
	addy=14
	fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
	addy+=1
	fnpa_fontsize
	if df$="Y" then
		fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	if c4>0 then
		fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	fnpa_txt('#'&trim$(z$)&' '&bulk$,(xmargin+68),lyne*(addy+=1)+ymargin)
	if pe$(1)<>"" then
		fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(2)<>"" then
		fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(3)<>"" then
		fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(4)<>"" then
		fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if billOnPageCount=1 then checkx=1.375 : checky=3.6875
	if billOnPageCount=2 then checkx=6.75 : checky=3.6875
	if billOnPageCount=3 then checkx=1.375 : checky=7.9375
	if billOnPageCount=0 then checkx=6.75 : checky=7.9375
	bc$=""
	if trim$(bc$)<>"" then fnpa_barcode(checkx,checky,bc$)
	if billOnPageCount=0 then
		fnpa_newpage
	end if
fnend
def fn_print_bill_cerro(z$,mat mg$,mat penalty$,serviceFrom,serviceTo)
	! r: any and all necessary setup (except opening the printer) to pr one bill
	if ~pbcerro_setup then
		pbcerro_setup=1
		open #h_pbcerro_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
		F_PBCERRO_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1854,pd 5.2
		blankbefore=1
		blankafter=3
	end if
	read #h_pbcerro_customer,using F_PBCERRO_CUSTOMER,key=z$: z$,pbcerro_meter_address$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceFromMmddYy,serviceToMmddYy
	dim pbcerro_meter_address$*30 ! formerly e$(2)
	fncustomer_address(z$,mat pe$)
	! /r
	! r: pr that bill
	if final=2 then
		g(8)-=b(8): g(11)=g(12)+g(8): bal+=g(8)
	end if
	penalty=0
	for j=1 to 10
		if penalty$(j)="Y" then ! accumulate all penalties and set charge to zero
			penalty+=g(j)
			g(j)=0
		end if
	next j
	pb=bal-g(11)
	pr #255: ""
	pr #255: ""
	pr #255,using L1550: "FROM",int(serviceFrom*.01),"TO",int(serviceTo*.01),d1
	L1550: form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##),skip 4
	if pb<>0 then pb$="   PRIOR BALANCE" else pb$=""
	pr #255: ""
	L1580: form pos 3,c 17,nz 10.2,pos 38,c 10
	if g(1)=0 then t$="" else t$=service$(1)
	pr #255,using PBC_L1610: t$,0,d(1),d(3),g(1)
	PBC_L1610: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,nz 10.2,nz 12.2
	PBC_L1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 5,pic(zz/zz/zz)
	if g(2)=0 then t$="" else t$=service$(2)
	if bal<=0 then pr #255,using PBC_L1610: t$,0,0,0,g(2),0,bal : goto PBC_L1660
	pr #255,using PBC_L1610: t$,0,0,0,g(2),bal+penalty,bal
	PBC_L1660: if g(3)=0 then t$="" else t$=service$(3)
	pr #255,using PBC_L1620: t$,0,0,0,g(3),d4
	if g(4)=0 then t$="" else t$=service$(4)
	pr #255,using PBC_L1610: t$,0,0,0,g(4)
	if g(5)=0 then t$="" else t$=service$(5)
	pr #255,using PBC_L1610: t$,0,0,0,g(5)
	if g(6)=0 then t$="" else t$=service$(6)
	pr #255,using L1580: pb$,pb,z$
	if g(8)=0 then t$="" else t$=service$(8)
	pr #255,using PBC_L1610: t$,0,0,0,g(8)
	if est=1 then est$="BILL ESTIMATED" else est$=""
	if c4>0 then final$="FINAL BILL" else final$=""
	if df$="Y" then final$="DRAFTED"
	if bal<=0 then penalty=0
	if bal<0 then g(5)=0
	pr #255: ""
	pr #255,using 'form pos 7,C 20,pos 38,C 25': est$,pe$(1)(1:25)
	pr #255,using 'form pos 1,CR 7,X 1,PIC(ZZ/ZZ/ZZ),NZ 13.2,pos 38,C 25': 'DUE BY:',d4,bal,pe$(2)(1:25)
	pr #255,using 'form pos 13,C 18,pos 38,C 25': e$(1)(1:18),pe$(3)(1:25)
	pr #255,using 'form pos 2,C 10,X 5,C 10,pos 38,C 25': z$,final$,pe$(4)(1:25)
	bills+=1
	pr #255,using 'form pos 2,c 30': mg$(1)
	pr #255,using 'form pos 2,c 30': mg$(2)
	pr #255,using 'form pos 2,c 30': mg$(3)
	if int(bills/3)<>bills/3 then pr #255,using 'form pos 2,c 30,skip 1': " "," " ! space extra if 1st or 2nd bill
	if int(bills/3)=bills/3 then pr #255: newpage ! BOTTOM OF PAGE
	! /r
fnend
! r: fn_print_bill_merriam(z$,mat mg$,serviceFrom,serviceTo) ! inherrits all those local customer variables too
! 	if ~setup_merriam then ! r:
! 		setup_merriam=1
! 		lyne=3
! 		fn_getCompanyAddr(mat at$)
! 		! r: read custom margins
! 		dim ub4upBill_data$(4)
! 		fnreg_read('ub4upBill Margin Top 1' ,ub4upBill_data$(1),  '5') ! bills 1 and 2
! 		fnreg_read('ub4upBill Margin Top 2' ,ub4upBill_data$(2),'113') ! bills 3 and 4      ymargin was 108
! 		fnreg_read('ub4upBill Margin Left 1',ub4upBill_data$(3),  '0') ! bills 1 and 3
! 		fnreg_read('ub4upBill Margin Left 2',ub4upBill_data$(4),'142') ! bills 2 and 4      xmargin was 137
! 		bill1y=bill2y=val(ub4upBill_data$(1))
! 		bill3y=bill4y=val(ub4upBill_data$(2))
! 		bill1x=bill3x=val(ub4upBill_data$(3))
! 		bill2x=bill4x=val(ub4upBill_data$(4))
! 		! /r
! 	end if  ! /r
! 	pb=bal-g(11)
! 	net_bill=g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9)
! 	fn_override_service_date(serviceFrom,serviceTo,serviceFromMmddYy,serviceToMmddYy)
! 	! -- Standard 4 Per Page Even Perforated Card Stock Bills
! 	billOnPageCount+=1
! 	if billOnPageCount=1 then xmargin=bill1x : ymargin=bill1y                      !   0   5
! 	if billOnPageCount=2 then xmargin=bill2x : ymargin=bill2y                      ! 142   5
! 	if billOnPageCount=3 then xmargin=bill3x : ymargin=bill3y                      !   0 113
! 	if billOnPageCount=4 then xmargin=bill4x : ymargin=bill4y : billOnPageCount=0  ! 142 113
! 	! move page down 1/2 inch
!
!
! 	if billOnPageCount=1 then ! it's about to pr the first bill on the page
! 		fnpa_line(70,1,0,1100) ! line down the middle of the page
! 		fnpa_line(140,1,0,1100) ! line down the middle of the page
! 		fnpa_line(215,1,0,1100) ! line down the middle of the page
! 		fnpa_line(1,108,800,0) ! line across the middle of the page
! 	!   fnpa_line(pl_left_pos,pl_top_pos,pl_width,pl_height,  pl_hollow) ! line across the middle of the page
! 	end if
! 	fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
! 	fnpa_fontbold(1)
! 	fnpa_fontsize(12)
! 	fnpa_font
! 	fnpa_txt(at$(1)(1:20),xmargin+8,lyne*1-1+ymargin)
! 	fnpa_font('Lucida Console')
! 	fnpa_fontsize
! 	fnpa_fontbold
! 	fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
! 	fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
! 	fnpa_txt(trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
! 	fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
! 	fnpa_txt('From: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceFrom)&'  To: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceTo),xmargin+2,lyne*7+ymargin)
! 	fnpa_txt("Due upon receipt",xmargin+2,lyne*8+ymargin)
! 	fnpa_txt(e$(2),xmargin+2,lyne*9+ymargin)
! 	fnpa_txt('Billing Date: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1),xmargin+2,lyne*11+ymargin)
! 	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
! 	fnpa_fontsize(7)
! 	pr #20: 'Call Print.AddText("Current",'&str$(xmargin+12+5)&','&str$(lyne*13+ymargin)&')'
! 	pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+35+5)&','&str$(lyne*13+ymargin)&')'
! 	pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+52+5)&','&str$(lyne*13+ymargin)&')'
!
! 	! PRINTGRID: !
! 	meter=14 ! 02114   meter=20 ! lyne=2 ! 3 ! 2.15 !  started at 20 and 2.1
! 	fnpa_fontsize ! line_top(1)
! 	if g(1)<>0 then
! 		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,7)&'",'&str$(xmargin+24)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(1)<>0
! 	if g(2)<>0 then
! 		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(2)<>0
! 	if g(3)<>0 or d(7)<>0 then
! 		pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("EL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(3)<>0 or d(7)<>0
! 	s4code$="GAS"
! 	if g(4)<>0 then
! 		pr #20: 'Call Print.AddText("'&s4code$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&s4code$&'",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(4)<>0
! 	if g(5)<>0 then
! 		pr #20: 'Call Print.AddText("Trash",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' ! "Trash was "SL"
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("Trash",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')' ! "Trash was "SL"
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(5)<>0
! 	if g(6)<>0 then
! 		pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(6)<>0
! 	if g(7)<>0 then
! 		pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(7)=0
! 	if g(8)<>0 then
! 		pr #20: 'Call Print.AddText("Other",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("Other",'&str$(xmargin+01)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(8)<>0
! 	if g(9)<>0 then
! 		pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! 		pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
! 	end if  ! g(9)<>0
! 	pr #20: 'Call Print.AddLine('&str$(xmargin+49)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
! 	pr #20: 'Call Print.AddLine('&str$(xmargin+91+14)&','&str$(lyne*(meter)+ymargin+2)&',15,0)'
! 	pr #20: 'Call Print.AddText("   Net Bill",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')'
! 	pr #20: 'Call Print.AddText("Net",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin+2)&')'
! 	pr #20: 'Call Print.AddText("'&fnformnumb$(net_bill,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
! 	pr #20: 'Call Print.AddText("'&fnformnumb$(net_bill,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
! 	if pb then
! 		pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
! 		pr #20: 'Call Print.AddText("Prior",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin+2)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
! 	end if  ! pb
! 	fnpa_fontsize : lyne=3
!
! 	if estimatedate=d1 then fnpa_txt("Bill estimated!",xmargin+1,lyne*21+ymargin)
! 	fnpa_line(xmargin+1,lyne*23+1+ymargin+10,63,0)
! 	if budget>0 then
! 		pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
! 		pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
! 	else
! 		pr #20: 'Call Print.AddText("Pay By '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
! 		pr #20: 'Call Print.AddText("Pay After '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
! 	end if
! 	fnpa_line(xmargin+1,lyne*26+1+ymargin+10,63,0)
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin+10)&',63,0)'
! 	! fnpa_fontsize(7)
! 	fnpa_line(xmargin+97,ymargin+0,29, lyne*5+2,1)
! 	fnpa_line(xmargin+90,ymargin+0   , 7,0)
! 	fnpa_line(xmargin+90,ymargin+2.8 , 7,0)
! 	fnpa_line(xmargin+90,ymargin+5.6 , 7,0)
! 	fnpa_line(xmargin+90,ymargin+8.4 , 7,0)
! 	fnpa_line(xmargin+90,ymargin+11.2, 7,0)
! 	fnpa_line(xmargin+90,ymargin+14  , 7,0)
! 	fnpa_line(xmargin+90,ymargin+17  , 7,0)
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
! 	! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
! 	fnpa_fontsize(9)
! 	pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68+8)&','&str$(lyne*7+ymargin)&')'
! 	pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68+8)&','&str$(lyne*8+ymargin)&')'
! 	pr #20: 'Call Print.AddText("'&env$('cnam')(1:27)&'",'&str$(xmargin+68+8)&','&str$(lyne*9+ymargin)&')'
! 	fnpa_fontsize
! 	pr #20: 'Call Print.AddText("Pay By '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':",'&str$(xmargin+68+8)&','&str$(lyne*11+ymargin)&')'
! 	if budget>0 then
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+100+8)&','&str$(lyne*11+ymargin)&')'
! 	else
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+100+8)&','&str$(lyne*11+ymargin)&')'
! 		pr #20: 'Call Print.AddText("After  '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)&':",'&str$(xmargin+68+8)&','&str$(lyne*12+ymargin)&')'
! 		pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+100+8)&','&str$(lyne*12+ymargin)&')'
! 	end if
! 	fnpa_fontsize(9)
! 	addy=11.5
! 	fnpa_txt(mg$(1),xmargin+4,(addy+=1)*lyne+ymargin+30)
! 	fnpa_txt(mg$(2),xmargin+4,(addy+=1)*lyne+ymargin+30)
! 	addy+=1
! 	fnpa_fontsize
! 	if df$="Y" then
! 		fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
! 	end if
! 	if c4>0 then
! 		fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
! 	end if
! 	pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68+8+30)&','&str$(lyne*(addy+=1)+ymargin+20+3)&')'
! 	fnpa_txt(trim$(pe$(1)),xmargin+68+8,lyne*(addy+=1)+ymargin+23)
! 	fnpa_txt(trim$(pe$(2)),xmargin+68+8,lyne*(addy+=1)+ymargin+23)
! 	fnpa_txt(trim$(pe$(3)),xmargin+68+8,lyne*(addy+=1)+ymargin+23)
! 	fnpa_txt(trim$(pe$(4)),xmargin+68+8,lyne*(addy+=1)+ymargin+23)
! 	pr #20: 'Call Print.AddText("Return Service Requested.",'&str$(xmargin+68+8)&','&str$(lyne*(addy+=2)+ymargin+20+3)&')'
! 	if billOnPageCount=1 then checkx=1.375 : checky=3.6875
! 	if billOnPageCount=2 then checkx=6.75 : checky=3.6875
! 	if billOnPageCount=3 then checkx=1.375 : checky=7.9375
! 	if billOnPageCount=0 then checkx=6.75 : checky=7.9375
! 	if billOnPageCount=0 then
! 		fnpa_newpage
! 	end if
! /r
def fn_print_bill_blucksberg(z$,mat mg$,billing_date_prior,serviceFrom,serviceTo) ! inherrits all those local customer variables too
	! -- Printer Program for Laser 1-Per Page Utility Bills
	if ~setup_blucksberg then ! r:
		setup_blucksberg=1

		fn_getCompanyAddr(mat at$)

	end if  ! /r
	! r: pr that bill
	lyne=3
	pb=bal-g(11)
	if bal<=0 then g(9)=0 ! don't show penalty if balance 0 or less
	activity_charge =fntrans_total_as_of(z$,billing_date_prior,1)
	activity_penalty=fntrans_total_as_of(z$,billing_date_prior,2)
	activity_payment=fntrans_total_as_of(z$,billing_date_prior,3)
	activity_credit =fntrans_total_as_of(z$,billing_date_prior,4)
	activity_debit  =fntrans_total_as_of(z$,billing_date_prior,5)
	prior_prior_balance=bal ! -g(11)
	prior_prior_balance-=activity_charge
	prior_prior_balance-=activity_penalty
	prior_prior_balance+=activity_payment
	prior_prior_balance+=activity_credit
	prior_prior_balance-=activity_debit

	! r: Blucksberg's prior usages gathering
	mat usage=(0): mat billdate=(0) : mat reads=(0)
	restore #h_ubtransvb,key>=z$&"         ": nokey PuXit ! no average but active customer (use 0 usage)
	do
		Pu_readTrans: !
		read #h_ubtransvb,using FpuTrans: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PuXit
		FpuTrans: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
		if p$=z$ then
			if tcode<>1 then goto Pu_readTrans ! only charge transactions
			usage(3)=usage(2): billdate(3)=billdate(2) : reads(3)=reads(2)
			usage(2)=usage(1): billdate(2)=billdate(1) : reads(2)=reads(1)
			usage(1)=wu: billdate(1)=tdate : reads(1)=wr
		end if
	loop while p$=z$
	PuXit: !
	! /r

	fnpa_fontsize
	! fnpa_txt("Blucksberg Mtn Water Association",158)
	! fnpa_txt("8077 Blucksberg Drive",15,13)
	! fnpa_txt("Sturgis, SD 57785",15,18)
	fnpa_txt(trim$(pe$(1)),22,49)
	fnpa_txt(trim$(pe$(2)),22,54)
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
	fnpa_txt(trim$(pe$(3)),22,59)
	fnpa_txt(trim$(pe$(4)),22,64)
	fnpa_elipse(147,24,38,.5)
	fnpa_elipse(147,24,37,.5)

	! fnpa_pic('S:\acsub\logo_blucksberg.jpg',124,13)

	fnpa_fontSize(13)
	fnpa_fontbold(1) : fnpa_fontitalic(1)
	fnpa_txt('Blucksberg Mountain',121,14)
	fnpa_txt(' Water Association' ,121,18)
	fnpa_fontbold(0) : fnpa_fontitalic(0)
	fnpa_fontSize(12)
	fnpa_txt('8077 Blucksberg Drive',124,25)
	fnpa_txt('Sturgis, SD 57785',124,29)

	! fnpa_fontbold
	! fnpa_fontitalic(1)
	! fnpa_txt("Blucksberg Mtn",119,14)
	! fnpa_fontsize(34)
	! fnpa_txt("Water",126,20)
	! fnpa_fontsize(14)
	! fnpa_txt("Association",128,31)

	fnpa_fontitalic(0)
	fnpa_fontsize(9)
	tmp_box_top=55
	fnpa_line(tmp_box_left_pos=115,tmp_box_top,70,24, 1)
	fnpa_txt('Billing Date:            '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1),tmp_box_left_pos+5,tmp_box_top+4)
	fnpa_txt("Account:      "&lpad$(trim$(z$),19),tmp_box_left_pos+5,tmp_box_top+8)
	fnpa_txt('Due Date:                '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4),tmp_box_left_pos+5,tmp_box_top+12)
	fnpa_txt("Billing Questions:   605-720-5013",tmp_box_left_pos+5,tmp_box_top+16)
	lyne=65 : adder=4
	fnpa_txt("Meter Location: "&trim$(e$(1)) ,23,lyne+=adder)
	fnpa_txt("Service From: "&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceFrom)&" To: "&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceTo) ,23,lyne+=adder)

	if final>0 then
		fnpa_fontBold(1)
		fnpa_txt('Final Bill',102,82)
		fnpa_fontBold(0)
	end if
	fnpa_line(26,85,157)

	lyne=81
	adder=4.5
	! fnpa_fontitalic
	! fnpa_txt("Activity Since "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),80,lyne+=adder)
	! fnpa_fontbold(0)
	lyne+=adder
	fnpa_txt("Amount",170,lyne)
	fn_blucksAddActivityLine("Prior Balance as of "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),prior_prior_balance)
	fn_blucksAddActivityLine("Charges as of "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),activity_charge-g(11))
	fn_blucksAddActivityLine("Penalties as of "&date$(days(lastPenaltyDate,'ccyymmdd'),'mm/dd/yy'),activity_penalty)
	fn_blucksAddActivityLine("Payments Received as of "&date$(days(d1,'mmddyy')-1,'mm/dd/yy')&" - Thank You",-activity_payment)
	fn_blucksAddActivityLine("Credits",-activity_credit)
	fn_blucksAddActivityLine("Debits",activity_debit)
	fnpa_line(162,lyne+4,22)
	fnpa_fontbold(1) ! on
	fn_blucksAddActivityLine("Balance Forward",pb,1,110)
	lyne+=adder
	! fnpa_fontbold(1) ! on
	fnpa_line(26,lyne+=adder,157)
	fnpa_txt("Current Charges",90,lyne+=1)
	! fnpa_txt("Current Charges",30,lyne+=8)
	fnpa_fontbold
	! adder=5
	fnpa_fontitalic
	adder=4
	lyne+=adder
	fnpa_txt("Current",83,lyne) ! lyne=100
	fnpa_txt("Reading",83,lyne+adder)
	fnpa_txt("Previous",103,lyne)
	fnpa_txt("Reading",103,lyne+adder)
	fnpa_txt("Usage",131,lyne+adder)
	fnpa_txt("Charge",170,lyne+adder)
	adder=4 ! maybe it should be 4.5
	lyne+=adder ! lyne=105
	if g(1)<>0 then
		if g(1)>=14 then
			fnpa_txt("Base Water Service Fee",26,lyne+=adder)
			fn_blucksAddChargeField(14)
		end if
		fnpa_txt("Water",26,lyne+=adder)
		fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(1)), 79,lyne)
		fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(2)),103,lyne)
		fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(3)),123,lyne)
		fn_blucksAddChargeField(g(1)-14)
	end if

	if g(2)<>0 then
		fnpa_txt("Sewer",26,lyne+=adder)
		if seweravg>0 then ! if have sewer average, use it for usage
			fnpa_txt(cnvrt$("pic(zzzzzzzz#)",seweravg),121,lyne)
		else !  use water usage
			fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(2)),121,lyne)
		end if
		fn_blucksAddChargeField(g(2))
	end if

	if g(3)<>0 then
		fnpa_txt("Association Fee *",26,lyne+=adder)
		fn_blucksAddChargeField(g(3))
	end if

	if g(4)<>0 then
		fnpa_txt("Ambulance Fund",26,lyne+=adder)
		! fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(9)) ,078,lyne) ! left over from gas
		! fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(11)) ,121,lyne)
		fn_blucksAddChargeField(g(4))
	end if

	! g(5)
	! g(6)

	if g(7)<>0 then
		fnpa_txt(serviceName$(7),26,lyne+=adder)
		fn_blucksAddChargeField(g(7))
	end if

	! g(8)

	if g(9)<>0 then
		fnpa_txt("Tax",26,lyne+=adder)
		fn_blucksAddChargeField(g(9))
	end if

	fnpa_line(162,lyne+4,22) : lyne+=1
	fnpa_fontbold(1)
	fn_blucksAddActivityLine("Total Current Charges",g(11), 1,110)
	! lyne+=adder ! fnpa_txt("Total Current Charges",110,lyne+=adder)
	! fnpa_txt(cnvrt$("pic(--------.##)",g(11)),160,lyne)
	fnpa_fontbold(0)
	lyne+=adder
	fnpa_line(162,lyne+3,22)
	fnpa_fontsize(14)
	fnpa_fontbold(1)
	fnpa_txt("Total Due",105+17,lyne+=adder)
	fnpa_txt(cnvrt$("pic($$$$$---.## CR)",bal),150,lyne)
	fnpa_line(162,lyne+=adder+1,22)
	fnpa_line(162,lyne+=1,22)
	fnpa_fontsize
	if uprc$(df$)="Y" then fnpa_txt("Your bill has been scheduled for automatic withdrawal",85,lyne+=adder)
	fnpa_fontsize
	fnpa_fontbold
	if g(3)>0 then
		fnpa_txt('* Road maintenance and snow removal from main roads, vehicle and equipment',25,lyne+=adder)
		fnpa_txt('repair and maintenance',25,lyne+=adder)
	end if
	fnpa_fontbold(1)
	fnpa_line(26,lyne+adder,157)
	fnpa_fontsize
	fnpa_fontitalic(1)
	lyne=165
	fnpa_txt("MESSAGE BOARD",92,lyne+=4)
	for j=1 to 13
		fnpa_txt(rpt$(' ',(message1_max_len-len(trim$(mg$(j))))/2)&trim$(mg$(j)),5,lyne+=4)
	next j
	fnpa_fontitalic
	x=0
	for j=1 to 39
		fnpa_line(x+=5,234,3,0) ! pr #20: 'Call Print.AddLine('&str$(x+=5)&','&str$(234)&',3,0)'
	next j
	fnpa_fontsize(7)
	fnpa_txt("Please detach here and return with payment.  Mail to 8077 Blucksberg Dr or deposit in black box at bus stop.",18,236)
	fnpa_fontsize
	fnpa_txt("Account: "&lpad$(trim$(z$),16),40,243)
	fnpa_txt('Due Date:        '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d4),40,247)
	fnpa_txt("Total Due:",40,251)
	fnpa_txt(cnvrt$("pic(--------.##)",bal),70,251)
	if bal>0 then
	 !   fnpa_txt("After "&cnvrt$("pic(##/##/##)",d4)&" pay "&cnvrt$("pic(---.##)",g(12)),40,255)
		fnpa_txt("After "&cnvrt$("pic(##/##/##)",d4)&" Add "&cnvrt$("pic(---.##)",2.50),40,255)
	end if
	fnpa_txt(trim$(pe$(1)),130,243)
	fnpa_txt(trim$(pe$(2)),130,247)
	if trim$(pe$(3))="" then pe$(3)=pe$(4) : pe$(4)=""
	fnpa_txt(trim$(pe$(3)),130,251)
	fnpa_txt(trim$(pe$(4)),130,255)
	fnpa_newpage
	! /r
fnend
def fn_blucksAddActivityLine(aal_text$*80,aal_amt; aal_always_show,aal_desc_left_override)
	if aal_desc_left_override=0 then aal_desc_left_override=30
	if aal_always_show or aal_amt<>0 then
		fnpa_txt(aal_text$,aal_desc_left_override,lyne+=adder)
		fnpa_txt(cnvrt$("pic($-------.## CR)",aal_amt),162,lyne)
	end if
fnend
def fn_blucksAddChargeField(value) ! requires local: lyne
	fnpa_txt(cnvrt$("pic($-------.## CR)",value),162,lyne)
fnend

def fn_print_bill_omaha(z$,mat mg$,mat mg2$,serviceFrom,pbo_service_to,mat penalty$)
! correct margins are top=.2, bottom=.2,left=.4,right=.2
! r: any and all necessary setup (except opening the printer) to pr one bill
	if ~pbomaha_setup then
		pbomaha_setup=1
		open #h_pbomaha_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
		F_PBOMAHA_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1854,pd 5.2
		pboposrightcol=70
	end if
	read #h_pbomaha_customer,using F_PBOMAHA_CUSTOMER,key=z$: z$,pbomaha_meter_address$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,serviceToMmddYy,serviceFromMmddYy
	dim pbomaha_meter_address$*30 ! formerly e$(2)
	fncustomer_address(z$,mat pe$)
! /r
	if final=2 then
		g(8)-=b(8): g(11)=g(12)+g(8): bal+=g(8)
	end if
	penalty=0
	for j=1 to 10
		if penalty$(j)="Y" then
			penalty+=g(j)
			g(j)=0 ! accumulate all penalties and set charge to zero
		end if
	next j
	pb=bal-g(11)
	bills=bills+1
	if bills=1 then
		pr #255:
		pr #255:
		pr #255:
		pr #255:
	else if bills=2 then
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
	else if bills=3 then
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
		pr #255:
	end if
	pr #255,using 'form pos 25,pic(##/##/##),pos pboPosRightCol,c message2_max_len': d1,fn_mg2$(1)
	pr #255,using 'form pos 4,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##)': "From",int(serviceFrom*.01),"To",int(pbo_service_to*.01)
	pr #255: ""
	if pb<>0 then pb$="   Prior Balance" else pb$=""
	pr #255: ""
	if g(1)=0 then t$="" else t$="Wtr"
	pr #255: ""
	pr #255,using PBO_F_ONE: t$,0,d(1),d(3),g(1),0,0,fn_mg2$
	PBO_F_ONE: form pos 4,c 3,nz 1,nz 8,nz 8,nz 9.2,x 4,nz 10.2,nz 12.2,pos pboposrightcol,c message2_max_len
	if g(2)=0 then t$="" else t$="Swr"
	if bal<=0 then
		pr #255,using PBO_F_ONE: t$,0,0,0,g(2),0,bal,fn_mg2$
	else
		pr #255,using PBO_F_ONE: t$,0,0,0,g(2),0,0,fn_mg2$
	end if
	if g(3)=0 then t$="" else t$="Pri"
	pr #255,using PBO_F_ONE: t$,0,0,0,g(3),bal+penalty,bal,fn_mg2$
	if g(5)=0 then t$="" else t$="W/F"
	pr #255,using PBO_L1610: t$,0,0,0,g(5),d4,fn_mg2$
	PBO_L1610: form pos 4,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,pic(zz/zz/zz),pos pboposrightcol,c message2_max_len
	if g(9)=0 then t$="" else t$="Tax"
	pr #255,using PBO_F_ONE: t$,0,0,0,g(9),0,0,fn_mg2$
	pr #255,using PBO_L1620: pb$,pb,z$,fn_mg2$
	PBO_L1620: form pos 6,c 17,nz 10.2,pos 36,c 10,pos pboposrightcol,c message2_max_len
	if g(4)=0 then t$="" else t$="SF "
	pr #255,using PBO_F_ONE: t$,0,0,0,g(4),0,0,fn_mg2$
	if d(10)=1 then est$="Bill Estimated" else est$=""
	if c4>0 then final$="Final Bill" else final$=""
	if df$="Y" then final$="Drafted"
	if bal<=0 then penalty=g(10)=0
	pr #255,using 'form pos 10,c 20,pos 38,c 25,pos pboPosRightCol,c message2_max_len': est$,pe$(1)(1:25),fn_mg2$
	if bal<=0 then
		pr #255,using PBO_L1810: 0,d4,bal,pe$(2)(1:25),fn_mg2$
	else
		pr #255,using PBO_L1810: bal+penalty,d4,bal,pe$(2)(1:25),fn_mg2$
	end if
	PBO_L1810: form pos 4,nz 7.2,x 1,pic(zz/zz/zz),nz 13.2,pos 38,c 25,pos pboposrightcol,c message2_max_len
	pr #255,using 'form pos 16,c 18,pos 38,c 25,pos pboPosRightCol,c 30': e$(1)(1:18),pe$(3)(1:25),mg$(1) ! fn_mg2$
	pr #255,using 'form pos 5,c 10,x 5,c 10,pos 38,c 25,pos pboPosRightCol,c 30': z$,final$,pe$(4)(1:25),mg$(2) ! fn_mg2$
	! pr #255,using "form pos 5,C 30,pos pboPosRightCol,c message2_max_len": mg$(1)
	! pr #255,using "form pos 5,C 30,pos pboPosRightCol,c message2_max_len": mg$(2)
	if bills=3 then ! Bottom of Page
		bills=0
		pr #255: newpage
	else
		if allign<>1 then ! it isn't a REPRINT so skip TOTALS
			billsPrintedCount(3)+=1
		end if
	end if
fnend

def fn_print_bill_pennington(z$,mat mg$,mat mg2$,serviceFrom,serviceTo,penaltyDueDate)
	! correct margins are top:.7, bottom:.25, left:.63, right:.25
	! r: any and all necessary setup (except opening the printer) to pr one bill
		if ~pbpennington_setup then
			pbpennington_setup=1
			open #h_pbpennington_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
		end if
		read #h_pbpennington_customer,using f_pbpennington,key=z$: z$,a4,mat b,mat d,bal,f,mat g,serviceToMmddYy,serviceFromMmddYy
		f_pbpennington: form pos 1,c 10,pos 149,pd 2,pos 157,11*pd 4.2,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 1750,2*n 6
		fncustomer_address(z$,mat gTmpCustomerAddress$)
		fn_override_service_date(serviceFrom,serviceTo,serviceFromMmddYy,serviceToMmddYy)
		pb=bal-g(11)
	! /r
	pr #255,using 'form pos 45,c 9,n 10.2': 'Water',g(1)+g(9)
	pr #255,using 'form pos 45,c 9,pic(zzz,zzz.zz)': '',g(2)
	if serviceTo=0 then tmpServiceFrom$='' else tmpServiceFrom$=date$(days(serviceFrom,'mmddyy'),'mm dd') ! serviceToOverride
	if serviceTo=0 then tmpServiceTo$='' else tmpServiceTo$=date$(days(serviceTo,'mmddyy'),'mm dd') ! d4
	pr #255,using 'form pos 11,c 5,pos 20,c 5,pos 30,pic(zzbzzbzz)': tmpServiceFrom$,tmpServiceTo$,date$(days(penaltyDueDate,'mmddyy'),'mm dd yy')
	pr #255: ""
	pr #255,using 'form pos 48,n 9.2,n 10.2': bal,bal+g(10)+g(7)
	pr #255: ""
	pr #255,using 'form pos 9,n 9,n 9,n 7,n 9.2,pos 45,c 30': d(2),d(1),int(d(3)/100),g(1),z$
	pbpennington_L810: form pos 45,c 30
	pbpennington_L820: form pos 26,c 5,pos 32,n 9.2,pos 45,c 30
	if g(2)=0 then
		pr #255,using pbpennington_L810: gTmpCustomerAddress$(1) ! e$(2)
	else
		pr #255,using pbpennington_L820: "Sewer",g(2),gTmpCustomerAddress$(1) ! e$(2)
	end if
	if g(5)=0 then
		pr #255,using pbpennington_L810: gTmpCustomerAddress$(2) ! e$(3)
	else
		pr #255,using pbpennington_L820: "Sanit",g(5),gTmpCustomerAddress$(2) ! e$(3)
	end if
	if g(6)=0 then
		pr #255,using pbpennington_L810: gTmpCustomerAddress$(3) ! e$(4)
	else
		pr #255,using 'form pos 11,C 20,pos 32,n 9.2,pos 45,c 30': "Business License Fee",g(6),gTmpCustomerAddress$(3) ! e$(4)
	end if
	if g(8)=0 then
		pr #255,using pbpennington_L810: gTmpCustomerAddress$(4) ! csz$
	else
		pr #255,using pbpennington_L820: "Other",g(8),gTmpCustomerAddress$(4) ! csz$
	end if
	if pb=0 then
		pr #255,using pbpennington_L810: ''
	else
		pr #255,using pbpennington_L820: "Prev",pb,''
	end if
	pr #255: ""
	pr #255,using 'form pos 32,n 9.2,pos 45,C 40': g(9),mg$(1)
	pr #255,using 'form pos 45,C 40': mg$(2)
	pr #255,using 'form pos 8,n 10.2,x 3,c 10,pos 32,n 9.2,pos 45,C 40': bal+g(10)+g(7),z$,bal,mg$(3)
	count+=1
	if count=1 or count=2 then
		for j=1 to 8
			pr #255: ''
		next j
	else if count=3 then
		pr #255: newpage
		count=0
	end if
fnend
def fn_print_bill_edinburg(z$,mat mg$,d1,serviceFrom,serviceToOverride,penaltyDueDate) ! ,serviceTo,penaltyDueDate)
! correct margins are ??
! r: any and all necessary setup (except opening the printer) to pr one bill
		if ~pbedinburg_setup then
			pbedinburg_setup=1
			open #h_pbedinburg_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
			F_PBedinburg_CUSTOMER: form pos 1,c 10,pos 147,pd 2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 1741,n 2,pos 1750,2*n 6
			lyne=3
		end if
!   read #1,using L590,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,serviceToOverride,serviceFromOverride,bulk$,extra1$ nokey SCREEN3
!   L590: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30
		read #h_pbedinburg_customer,using F_PBedinburg_CUSTOMER,key=z$: z$,a3,final,mat d,bal,f,mat g,route,serviceFromMmddYy,serviceToMmddYy
		fncustomer_address(z$,mat pe$)
		fn_override_service_date(serviceFrom,serviceTo,serviceFromMmddYy,serviceToMmddYy)
! /r
	billOnPageCount+=1
	if billOnPageCount=1 then xmargin=0 : ymargin=0
	if billOnPageCount=2 then xmargin=139 : ymargin=0
	if billOnPageCount=3 then xmargin=0 : ymargin=108
	if billOnPageCount=4 then xmargin=139 : ymargin=108 : billOnPageCount=0
	fnpa_line(xmargin+5,ymargin+2,55,lyne*3+3,1)
	fnpa_fontbold(1)
	fnpa_fontsize(12)
	fnpa_font
	fnpa_txt("Village of Edinburg",xmargin+8,lyne*1-1+ymargin)
	fnpa_font('Lucida Console')
	fnpa_fontsize
	fnpa_fontbold
	fnpa_txt("     P O Box 350    ",xmargin+6,lyne*2+1+ymargin-.2)
	fnpa_txt("  Edinburg, IL 62531    ",xmargin+6,lyne*3+1+ymargin)
	fnpa_txt('#'&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
	fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
	fnpa_txt('From: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceTo)&'  To: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',serviceFrom),xmargin+2,lyne*7+ymargin)
	fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
	fnpa_txt('Billing Date: '&cnvrt$('PIC(ZZ/ZZ/ZZ)',d1),xmargin+2,lyne*11+ymargin)
	fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
	fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
	fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
	fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)

! PRINTGRID: !
	meter=14
	fnpa_fontsize(8)
	if g(1)<>0 then
		fnpa_txt("WTR",xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(2)<>0 then
		fnpa_txt("SWR",xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(4)<>0 then
		if a4=1 then
			s4code$="RSGS"
		else if a4=2 then
			s4code$="CMGS"
		else if a4=3 then
			s4code$="INGS"
		else
			s4code$="GAS"
		end if
		fnpa_txt(s4code$,xmargin+1,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(3)<>0 then
		fnpa_txt('WF',xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(8)<>0 then
		fnpa_txt("MISC",xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(9)<>0 then
		fnpa_txt("TAX",xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(10)<>0 then
		fnpa_txt("SF",xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if pb<>0 then
		fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	fnpa_fontsize

	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$('PIC(ZZ/ZZ/ZZ)',penaltyDueDate)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
	pr #20: 'Call Print.AddText("Pay After '&cnvrt$('PIC(ZZ/ZZ/ZZ)',penaltyDueDate)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
	if bal>0 and g(5)+g(6)+g(7)>0 then penalty=round(bal*.1,2) else penalty =0
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal+penalty,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
	pr #20: 'Call Print.AddText("Phone: 217-623-5542",'&str$(xmargin+1)&','&str$(lyne*27+ymargin)&')'

! special=28

	fnpa_fontsize(7)
	pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
	pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
	pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
	pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  Permit No 12",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
	fnpa_fontsize(9)
! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
	pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("Village of Edinburg",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
	fnpa_fontsize
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$('PIC(ZZ/ZZ/ZZ)',penaltyDueDate)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddText("After '&cnvrt$('PIC(ZZ/ZZ/ZZ)',penaltyDueDate)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal+penalty,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
	fnpa_fontsize(9)
	addy=14
	fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
	addy+=1
	fnpa_fontsize
	if df$="Y" then
		fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	if final>0 then
		fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	if d(10)=1 then
		fnpa_txt("Bill Estimated",xmargin+1,lyne*(addy+=1)+ymargin)
	end if
	fnpa_txt('#'&trim$(z$)&' '&bulk$,xmargin+68,lyne*(addy+=1)+ymargin)
	if pe$(1)<>"" then
		fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(2)<>"" then
		fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(3)<>"" then
		fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(4)<>"" then
		fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if billOnPageCount=1 then checkx=1.375 : checky=3.6875
	if billOnPageCount=2 then checkx=6.75 : checky=3.6875
	if billOnPageCount=3 then checkx=1.375 : checky=7.9375
	if billOnPageCount=0 then checkx=6.75 : checky=7.9375
	if billOnPageCount=0 then
		fnpa_newpage
	end if
fnend
def fn_print_bill_billings(mat mg$,mat g,mat b,bal,mat penalty$,d1,serviceFrom,serviceTo,datePastDue,mat pe$,final$,z$)  ! three per page RTF Bill
	if final=2 then g(8)-=b(8): g(11)=g(12)+g(8): bal+=g(8)
	penalty=0
	for j=1 to 10
		if penalty$(j)="Y" then penalty+=g(j) : g(j)=0 ! accumulate all penalties and set charge to zero
	next j
	pb=bal-g(11)
	pr #255: '' ! 'alphasort='&c$(c_alphaSort)   
	pr #255,using 'form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##)': "FROM",int(serviceFrom*.01),"TO",int(serviceTo*.01),d1
	pr #255,using 'form pos 1,c 10,pos 13,c 18': trim$(z$),e$(1)(1:18)
	pr #255: ''
	pr #255: ''
	if pb<>0 then pb$="   PRIOR BALANCE" else pb$=""
	if g(1)=0 then t$="" else t$="WTR"
	pr #255,using billings_fL1620: t$,0,d(1),d(3),g(1)
	billings_fL1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,skip 1
	billings_fL1630: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,pos 38,pic(zz/zz/zz),skip 1
	if g(2)=0 then t$="" else t$="SWR"
	pr #255,using billings_fL1620: t$,0,0,0,g(2)
	if g(3)=0 then t$="" else t$="PRI"
	pr #255,using billings_fL1620: t$,0,0,0,g(3)
	if g(4)=0 then t$="" else t$="SF "
	pr #255,using billings_fL1630: t$,0,0,0,g(4),datePastDue
	if g(5)=0 then t$="" else t$="SOL"
	pr #255,using billings_fL1620: t$,0,0,0,g(5)
	if g(9)=0 then t$="" else t$="TAX"
	pr #255,using billings_fL1620: t$,0,0,0,g(9)
	if g(8) and g(6) then
		t$=serviceCode$(8)&"+"&serviceCode$(6)
	else if g(8) then
		t$=serviceCode$(8)
	else if g(6) then
		t$=serviceCode$(6)
	else
		t$=""
	end if
	pr #255,using 'form pos 1,c 20,nz 9.2': t$,g(8)+g(6)
	if bal>0 then
		pr #255,using billings_fL1590: pb$,pb,bal+penalty,bal
		billings_fL1590: form pos 1,c 17,nz 10.2,pos 35,nz 10.2,pos 50,nz 10.2,skip 1
	else
		pr #255,using billings_fL1590: pb$,pb,0,bal
	end if
	if d(10)=1 then est$="BILL ESTIMATED" else est$=""
	if c4>0 then final$="FINAL BILL" else final$=""
	if df$="Y" then final$="DRAFTED"
	if bal>g(11) then final$="DELINQUENT NOTICE"
	if bal<=0 then g(10)=0
	pr #255,using 'form pos 7,c 20,pos 37,c 30': est$,trim$(z$)
	pr #255,using 'form pos 37,c 30': pe$(1)
	pr #255,using 'form pos 37,c 30': pe$(2)
	if bal<=0 then
		pr #255,using 'form pos 1,nz 7.2,x 1,pic(zz/zz/zz),nz 13.2,pos 37,c 30': 0,datePastDue,bal,pe$(3)
	else
		pr #255,using 'form pos 1,nz 7.2,x 1,pic(zz/zz/zz),nz 13.2,pos 37,c 30': bal+penalty,datePastDue,bal,pe$(3)
	end if
	pr #255,using 'form pos 1,C 30,pos 37,c 37': final$,pe$(4)
	pr #255,using 'form pos 1,c 30': mg$(1)
	pr #255,using 'form pos 1,c 30': mg$(2)
	pr #255,using 'form pos 1,c 30': mg$(3)
	bills=bills+1
	if int(bills/3)=bills/3 then  ! BOTTOM OF PAGE
		pr #255: newpage
	else ! in between bills
		pr #255: ''
		pr #255: ''
		pr #255: ''
	end if
	! billsPrintedCount(1)+=1  ! not sure if (1) is right.
fnend
def fn_print_bill_choctaw(z$,mat g,mat b,mat penalty$,d1,serviceFrom,serviceTo,d4,mat e$,final)
!    Good margins in Word (5/2/2017)  Top .4", Bottom, .5", Left 1.5", Right .25"
!    Good margins in Word (5/2/2017)  Top .4", Bottom, .5", Left 1.5", Right .25"
	if final=2 then
		g(8)-=b(8): g(11)=g(12)+g(8): bal+=g(8)
	end if
	penalty=0
	for j=1 to 10
		if penalty$(j)="Y" then
			penalty+=g(j)
			g(j)=0 ! accumulate all penalties and set charge to zero
		end if
	next j
	pb=bal-g(11)
	pr #255: ''
	pr #255,using 'form pos 4,c 10,pos 32,c 10,pos 48,pic(zz/zz/zz)': z$,z$,d4
	pr #255,using 'form pos 1,c 5,pic(zz/zz/zz),c 4,pic(zz/zz/zz)': "From:",serviceFrom," To:",serviceTo
	pr #255,using 'form pos 32,n 8.2,pos 48,n 8.2': g(12),g(11)
	pr #255,using 'form pos 32,n 8.2,pos 48,n 8.2': pb,pb
	pr #255,using 'form pos 32,n 8.2,pos 48,n 8.2': bal+penalty,bal
	pr #255,using 'form pos 1,pic(zzzzzzzz),2*pic(zzzzzzzzz)': d(1),d(2),d(3)
	pr #255: ''
	pr #255: ''
	pr #255: ''
	if g(1)>0 then
		cde=1
		d4$=cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)
	else
		cde=0
		d4$=""
	end if
	pr #255,using ce_L1690: "Water",g(1),d4$,e$(2)
	ce_L1690: form pos 1,c 5,pic(-----.--),pos 18,pic(zz/zz/zz),pos 32,c 30,skip 1
	if g(9)>0 then cde=2: d4$=cnvrt$('PIC(ZZ/ZZ/ZZ)',d4) else cde=0 : d4$=""
	pr #255,using ce_L1690: "Tax",g(9),d4$,e$(3)
	if g(8)>0 then cde=3: d4$=cnvrt$('PIC(ZZ/ZZ/ZZ)',d4) else cde = 0 : d4$=""
	if g(8)<>0 then mis$="Misc" else mis$=""
	pr #255,using ce_L1690: mis$,g(8),d4$,e$(4)
	pr #255,using 'form pos 3,c 30': e$(2)
	pr #255: ""
	pr #255,using 'form pos 1,n 8.2,pos 20,n 8.2': g(12),g(11)
	pr #255,using 'form pos 1,n 8.2,pos 20,n 8.2': pb,pb
	pr #255,using 'form pos 1,n 8.2,pos 20,n 8.2': bal+penalty,bal
	pr #255: ''
	bills+=1
	billOnPage+=1
	if billOnPage=1 or billOnPage=2 then ! int(bills/3)<>bills/3 then ! space extra if 1st or 2nd bill
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		pr #255: ''
		! pr #255: '' ! microsoft word seemed to need an extra line
	! end if
	! ! If billOnPage=1 Then  pr #255,Using 1910: " " ! extra line after 1st bill
	else ! if int(bills/3)=bills/3 then
		pr #255: newpage ! BOTTOM OF PAGE
		billOnPage=0
	end if !
fnend
def fn_print_bill_GreeneCo(serviceTo)
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
	if ~setup_GreeneCo then
		setup_GreeneCo=1
		lyne=3
	end if
	billOnPageCount+=1
	if billOnPageCount=1 then xmargin=  0 : ymargin=  0
	if billOnPageCount=2 then xmargin=139 : ymargin=  0
	if billOnPageCount=3 then xmargin=  0 : ymargin=107                   ! 104
	if billOnPageCount=4 then xmargin=139 : ymargin=107  : billOnPageCount=0 ! 104

	fnpa_font("Lucida Console")
	fnpa_fontsize(9)
	fnpa_fontbold(1)
	fnpa_txt(z$,xmargin+50,lyne+6+ymargin)
	fnpa_txt(cnvrt$('pic(##/##/##)',d1),xmargin+80,lyne+6+ymargin)
	fnpa_txt(e$(1),xmargin+50,lyne+9.5+ymargin)
	meter=9
	fnpa_fontsize(10)
	if g(1) then
		fnpa_txt(fnformnumb$(d(1),0,9),xmargin+52,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(2),0,9),xmargin+75,lyne*meter+ymargin)
		fnpa_txt("Water Charge",xmargin+35,lyne*(meter+=1.6)+ymargin)
		fnpa_txt(fnformnumb$(d(3),0,9),xmargin+75,lyne*meter+ymargin) ! Water Used
		fnpa_txt(fnformnumb$(g(1),2,9),xmargin+100,lyne*meter+ymargin)
	end if
	if g(5) then
		fnpa_txt("Primacy Fee",xmargin+35,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(5),2,9),xmargin+100,lyne*meter+ymargin)
	end if
	if g(8) then
		fnpa_txt("Other",xmargin+35,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(8),2,9),xmargin+100,lyne*meter+ymargin)
	end if
	if g(9) then
		fnpa_txt("Sales Tax",xmargin+35,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(9),2,9),xmargin+100,lyne*meter+ymargin)
	end if
	if pb then
		fnpa_line(xmargin+106,lyne*(meter+=1)+ymargin,15)
		fnpa_line(xmargin+106,lyne*(meter)+ymargin,15)
		fnpa_txt("Current Charges",xmargin+40,lyne*(meter+=.25)+ymargin)
		fnpa_txt(fnformnumb$(g(1)+g(5)+g(7)+g(8)+g(9),2,9),xmargin+100,lyne*meter+ymargin)
		fnpa_txt("Prior Balance",xmargin+35,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(pb,2,9),xmargin+100,lyne*meter+ymargin)
	end if
	fnpa_fontsize(10)
	if estimatedate=d1 then
		fnpa_txt("Bill estimated!",xmargin+1,lyne*21+ymargin)
	end if
	fnpa_txt(e$(2)(1:15),xmargin+5,lyne*12+ymargin)
	fnpa_txt(z$,xmargin+5,52+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+100,58+ymargin)
	fnpa_txt(fnformnumb$(bal,2,9),xmargin+5,66+ymargin)
	! If G(9)=0 AND G(10)=0 Then PENALTY=0: Goto 2520
	! If BAL>14.99 Then PENALTY=ROUND(BAL*.10,2) Else PENALTY=0
	fnpa_fontsize(9)
	addy=14
	fnpa_txt(mg$(1),xmargin+35,63+ymargin)
	fnpa_txt(mg$(2),xmargin+35,66+ymargin)
	fnpa_txt(mg$(3),xmargin+35,69+ymargin)
	addy+=1
	fnpa_fontsize
	if df$="Y" then
		fnpa_txt("Drafted",xmargin+1,71+ymargin)
	end if
	if final>0 then
		fnpa_txt("Final Bill",xmargin+1,71+ymargin)
	end if
	addy+=10
	if pe$(1)<>"" then
		fnpa_txt(pe$(1),xmargin+60,lyne*(addy+=1)+ymargin)
	end if
	if pe$(2)<>"" then
		fnpa_txt(pe$(2),xmargin+60,lyne*(addy+=1)+ymargin)
	end if
	if pe$(3)<>"" then
		fnpa_txt(pe$(3),xmargin+60,lyne*(addy+=1)+ymargin)
	end if
	if pe$(4)<>"" then
		fnpa_txt(pe$(4),xmargin+60,lyne*(addy+=1)+ymargin)
	end if
	if billOnPageCount=1 then checkx=1.375 : checky=3.6875
	if billOnPageCount=2 then checkx=6.75  : checky=3.6875
	if billOnPageCount=3 then checkx=1.375 : checky=7.9375
	if billOnPageCount=0 then checkx=6.75  : checky=7.9375
	! bc$=""
	! if trim$(bc$)<>"" then fnpa_barcode(checkx,checky,bc$)
	if billOnPageCount=0 then
		fnpa_newpage
	end if
fnend

def fn_print_bill_galena(serviceTo)
	if ~setup_print_bill_galena then
		setup_print_bill_galena=1
		lyne=4 ! 3
		character=2 ! 1.5
		fontBig  =14 ! 10
		fontNorm =10 ! 8
		fontSmall= 9
	end if
	fn_override_service_date(unused,serviceTo,alsoUnused,serviceToMmddYy)
	billOnPageCount+=1
	if billOnPageCount=1 then
		xmargin=0
		ymargin=0
		if env$('acsDeveloper')<>'' then
			fnpa_background('S:\Core\pdf\Galena Bill Background.pdf')
		end if
	else if billOnPageCount=2 then
		xmargin=0
		ymargin=140
	end if


	! r: left side
	lsColService =xmargin     + 1
	lsColPresent =xmargin     +14-2
	lsColPrevious=xmargin+24  +11+2
	lsColUsed    =xmargin+44  +18+1
	lsColCharges =xmargin+74  +10+3
	fnpa_fontSize(fontBig)
	fnpa_fontBold
	fnpa_txt(trim$(z$),xmargin+40,lyne*2+ymargin)
	fnpa_fontSize(fontSmall)
	fnpa_txt(e$(1),xmargin+26+40,lyne*2+ymargin)
	PRINTGRID: !
	meter=6
	fnpa_fontSize(fontNorm)
	! r: top table
	if g(1) then
		fnpa_txt("WA"                  ,lsColService ,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(d(1),0,9),lsColPresent  ,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(2),0,9),lsColPrevious ,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(d(3),0,9),lsColUsed     ,lyne*meter+ymargin)
		fnpa_txt(fnformnumb$(g(1),2,9),lsColCharges  ,lyne*meter+ymargin)
	end if
	if g(2) then
		fnpa_txt("SW",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(2),2,9),lsColCharges,lyne*meter+ymargin)
	end if
	if g(4) then
		fnpa_txt("PS",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(4),2,9),lsColCharges,lyne*meter+ymargin)
	end if
	if g(5) then
		fnpa_txt("TR",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(5),2,9),lsColCharges,lyne*meter+ymargin)
	end if
	if g(6) then
		fnpa_txt("PW",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(6),2,9),lsColCharges,lyne*meter+ymargin)
	end if
	if g(8) then
		fnpa_txt("OC",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(8),2,9),lsColCharges,lyne*meter+ymargin)
	end if
	if g(9) then
		fnpa_txt("TX",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(g(9),2,9),lsColCharges,lyne*meter+ymargin)
	end if
	if pb then
		fnpa_txt("PB",lsColService,lyne*(meter+=1)+ymargin)
		fnpa_txt(fnformnumb$(pb,2,9),lsColCharges,lyne*meter+ymargin)
	end if
	! /r
! r: bottom table
	fnpa_txt(date$(days(serviceTo,"mmddyy"),"m")     ,xmargin+ 2    ,lyne*23+ymargin)
	fnpa_txt(date$(days(serviceTo,"mmddyy"),"D")     ,xmargin+11    ,lyne*23+ymargin)
	! pr z$,serviceTo : pause
	if bal>0 then
		fnpa_txt(fnformnumb$(bal-g(9),2,9)      ,lsColPrevious ,lyne*23+ymargin)
		if g(10)>0 then
			! Pay Early Save This
			fnpa_txt(fnformnumb$(g(10),2,9)       ,lsColUsed     ,lyne*23+ymargin)
		end if
		fnpa_txt(fnformnumb$(bal+g(10)-g(9),2,9),lsColCharges  ,lyne*23+ymargin)
	else
		fnpa_txt(fnformnumb$(bal,2,9)           ,lsColPrevious ,ymargin+lyne*23)
		fnpa_txt(fnformnumb$(bal,2,9)           ,lsColCharges  ,ymargin+lyne*23)
	end if
	if bal<>0 then
		if g(9)>0 then
			fnpa_txt(fnformnumb$(g(9),2,9)        ,lsColPrevious ,ymargin+lyne*25) ! *xx   was *25.4
			fnpa_txt(fnformnumb$(g(9),2,9)        ,lsColCharges  ,ymargin+lyne*25) ! *xx   was *25.4
		end if
	fnpa_txt(fnformnumb$(bal,2,9)             ,lsColPrevious ,ymargin+lyne*29.5)  ! *xx was *29.2   note: 29.6 pushes numbers off page on second bill
	if bal>0 then fnpa_txt(fnformnumb$(bal+g(10),2,9)       ,lsColCharges  ,ymargin+lyne*29.5)  ! *xx was *29.2   note: 29.6 pushes numbers off page on second bill
	end if
		! /r
	! /r
	! r: right side
		rightSide=xmargin+125
	fnpa_fontSize(fontNorm)
	fnpa_txt('Please return this side with payment to:',rightSide,lyne*6+ymargin)
	fnpa_txt(env$('cnam')                ,rightSide,lyne*7+ymargin)
	fnpa_txt(e$(2)                          ,rightSide     ,ymargin+lyne*9 )
	fnpa_txt(mg$(1)                         ,rightSide     ,ymargin+lyne*11 )
	fnpa_txt(mg$(2)                         ,rightSide     ,ymargin+lyne*12 )
	fnpa_txt(mg$(3)                         ,rightSide     ,ymargin+lyne*13 )

	fnpa_txt(str$(route)                   ,rightSide+5   ,ymargin+lyne*21  )
	fnpa_txt(z$                             ,rightSide+15  ,ymargin+lyne*21  )
	fnpa_txt(cnvrt$('PIC(ZZ/ZZ/ZZ)',d4)   ,rightSide+57  ,ymargin+lyne*21   )
	if bal>0 then
		fnpa_txt(fnformnumb$(bal+g(10),2,9)  ,rightSide+60  ,ymargin+lyne*25) !  xmargin+106
	else
		fnpa_txt(fnformnumb$(bal,2,9)        ,rightSide+60,  ymargin+lyne*25) ! rightSide+31
	end if
	fnpa_txt(fnformnumb$(bal,2,9)          ,rightSide+60   ,ymargin+lyne*29) ! 25
	! /r
	if billOnPageCount=2 then
		fnpa_newpage
		billOnPageCount=0
	end if
fnend

BulkSort: ! r: sort in bulk sort code sequence
	if enable_BulkSort=1 then !
		open #h_control=fnH: "Name=[temp]\printBillsControl.[session],Size=0,RecL=128,Replace",internal,output
		write #h_control,using 'form pos 1,c 128': "FILE [Q]\UBmstr\customer.h[cno],,,[temp]\Addr.[session],,,,,A,N"
		if route_filter>0 then
			write #h_control,using 'form pos 1,c 128': 'RECORD I,1,2,N,"'&str$(route_filter)&'","'&str$(route_filter)&'"'
		end if
		write #h_control,using 'form pos 1,c 128': "MASK 1942,12,C,A,1,10,C,A"
		close #h_control:
		h_control=0
		execute "Free [temp]\Addr.[session]" ioerr ignore
		execute "Sort [temp]\printBillsControl.[session]"
		open #hAddr=fnH: "Name=[temp]\Addr.[session]",i,i,r ! was #7
	else if enable_BulkSort=2 then
		open #hBs2Customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
		open #hBs2Out=fnH: "Name=[temp]\Temp.[session],Replace,RecL=31",internal,output
		do
			read #hBs2Customer,using "form pos 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof BS2_EO_CUSTOMER
			write #hBs2Out,using "form pos 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
		loop
		BS2_EO_CUSTOMER: !
		close #hBs2Customer: ioerr ignore
		hBs2Customer=0
		close #hBs2Out: ioerr ignore
		hBs2Out=0
		execute "Index [temp]\Temp.[session] [temp]\Tempidx.[session] 1,19,Replace,DupKeys -n"
		open #hBulk2=fnH: "Name=[temp]\Temp.[session],KFName=[temp]\Tempidx.[session]",i,i,k
	else if enable_BulkSort=3 then ! alphasort, route, sequence, account
		restore #hCustomer:
		! open #hBs2Customer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,i,k  ! open in Account order
		open #hBs3Out=fnH: 'Name=[temp]\Temp.[session],Replace,RecL=64',internal,output
		do
			read #hCustomer,using form$(hCustomer): mat c$, mat cN eof BS_EO_CUSTOMER
			write #hBs3Out,using "form pos 1,C 7,n 2,n 7,c 10": c$(c_alphaSort),cN(c_route),cN(c_sequence),c$(c_account)
			! field     C/N Len Spos
			! alphaSort  C    7    1
			! route      N    2    8
			! sequence   N    7   10
			! account    C   10   17
		loop
		BS_EO_CUSTOMER: !
		restore #hCustomer:
		close #hBs3Out: ioerr ignore
		hBs3Out=0
		fnIndex('[temp]\Temp.[session]','[temp]\Tempidx.[session]','1,26')
		open #hBulk3=fnH: 'Name=[temp]\Temp.[session],KFName=[temp]\Tempidx.[session]',i,i,k

	end if
return  ! /r
Sort1: ! r: Select & Sort - sorts Cass1 file    requires: (hCustomer2,&enable_cassSort,&hSort1Sequence,&hAddr,d1,route_filter,... ;  ___,z$*10,customerLastBillingDate,route
	enable_cassSort=0 ! replaces old s5 variable
	open #h_cass1=fnH: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",i,i,k ioerr Xit_Sort1
	open #hSort1Sequence=fnH: "Name=[temp]\Temp.[session],Replace,RecL=19",internal,output
	enable_cassSort=1
	if route_filter=0 then routekey$="" else routekey$=cnvrt$("N 2",route_filter)&"       " ! key off first record in route (route # no longer part of customer #)
	restore #hCustomer2,search>=routekey$:
	do
		read #hCustomer2,using 'form pos 1,c 10,pos 296,pd 4,pos 1741,n 2': z$,customerLastBillingDate,route eof END5
		if route_filter and route_filter><route then goto END5
		if customerLastBillingDate=d1 then
			zip5$=cr$=""
			read #h_cass1,using "form pos 96,C 5,pos 108,C 4",key=z$: zip5$,cr$ nokey ignore
			write #hSort1Sequence,using "form pos 1,C 5,C 4,C 10": zip5$,cr$,z$
		end if
	loop

	END5: !
	close #h_cass1:
	h_cass1=0
	close #hSort1Sequence:
	hSort1Sequence=0
	open #h_Sort1_control=fnH: "Name=[temp]\Control.[session],Size=0,RecL=128,Replace",internal,output
	write #h_Sort1_control,using 'form pos 1,c 128': "File [temp]\Temp.[session],,,[temp]\Addr.[session],,,,,A,N"
	write #h_Sort1_control,using 'form pos 1,c 128': "Mask 1,19,C,A"
	close #h_Sort1_control:
	h_Sort1_control=0
	execute "Free [temp]\Addr.[session]" ioerr ignore
	execute "Sort [temp]\Control.[session]"
	open #hSort1Sequence=fnH: "Name=[temp]\Temp.[session]",i,i,r
	open #hAddr=fnH: "Name=[temp]\Addr.[session]",i,i,r ! was #7
	Xit_Sort1: !
return  ! /r

include: fn_open
include: ertn