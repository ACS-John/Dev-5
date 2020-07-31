pr bell;'do not run this program directly.  it is just for library function calling. ' : end
! fn_setup
! fnTop(program$)
! dim goal$*11
! goal$=lwrc$(env$('program_caption')(1:pos(env$('program_caption'),' ')-1))
! fn_calculateBills(goal$)
! Xit: fnXit
def library fnCalculateBills(goal$*11)
	if ~setup then fn_setup
	fnCalculateBills=fn_calculateBills(goal$)
fnend
def fn_calculateBills(goal$*11)
	if goal$<>'calculate' and goal$<>'recalculate' then
		pr Bell;'Invalid Goal ('&goal$&') passed.'
		pr '     only "calculate" and "recalculate" are valid.'
		pause
	end if

	fnLastBillingDate(d1)
	work$="[Q]\UBmstr\Reads_and_Chgs.h[cno]"
	work_addr$="[Q]\UBmstr\Reads_and_Chgs-Key.h[cno]"
	! if env$('client')="Edinburg" or env$('client')="French Settlement" or env$('client')="Allendale" then enableCostOfGas=1 else enableCostOfGas=btu=0
	enableCostOfGas=fnEnableCostOfGas
	! synchronize this setting with S:\Utility Billing\Enter Readings and Charges (Enter Readings and Charges)

	! get date meter read
	fncreg_read('Meter Reading Date Current',tmp$) : dateread=val(tmp$)

	fn_ask_billing_date
	if ckey=5 then goto Xit_CALCULATE
	! fnwait("Calculating: please wait...",0)
	fnAutomatedSavePoint('before')

	fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
	for j=1 to udim(serviceName$)
		serviceName$(j)=trim$(serviceName$(j))
	next j

	fnopenprn

	open #h_ratemst:=8: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed
	open #h_customer:=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	F_CUSTOMER_W_ACCT: form pos 1,c 10,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
	open #h_ubtrans:=3: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
	FORM_UBTRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if goal$='calculate' then
		open #h_work:=2: "Name="&work$,internal,outIn,relative
	end if
	F_WORK: form pos 1,c 10,pos 11,4*pd 5,pos 31,7*pd 4.2,pos 59,3*pd 5,n 1
	fn_deposit_open
	fn_bud_open

TOP: ! r:
	if goal$='calculate' then
		if r3=>lrec(h_work) then goto FINIS
		read #h_work,using F_WORK,rec=r3+=1: x$,mat x eof FINIS,noRec TOP
		if x$(1:2)="00" or uprc$(x$)=uprc$("   DELETED") then goto TOP
		read #h_customer,using F_CUSTOMER,key=x$: meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra nokey NKT9
	else if goal$='recalculate' then
		read #h_customer,using F_CUSTOMER_W_ACCT: x$,meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra eof FINIS

		if f<>d1 then goto TOP

		mat x=(0)
		x(1)=d(1) ! current water reading
		x(2)=d(9) ! current gas reading
		x(3)=d(5) !  current electric reading
		if d(3)>0 and d(1)-d(2)<> d(3) then x(12)=d(3) ! if usage was override then use
		if d(7)>0 and d(5)-d(6)<> d(7) then x(13)=d(7) ! if usage was override then use
		if d(11)>0 and d(9)-d(10)<> d(11) then x(14)=d(11) ! if usage was override then use for gas
	end if

	! r: set default rate codes
	! if env$('client')="Pennington" then extra(12)=1 ! default to 1
	! if env$('client')="Albany" and (a(1)=1 or a(1)=3 or a(1)=4 or a(1)=6) then a(6)=0 ! set residential sales tax code to zero
	if env$('client')="Raymond" and (a(1)<>0 and a(6)=0) then a(6)=1 ! if any water rate code and no water penalty than default to water penalty of 1.
	if env$('client')="Raymond" and (a(2)<>0 and a(7)=0) then a(7)=1 ! if any sewer rate code and no sewer penalty than default to sewer penalty of 1.
	fnapply_default_rates(mat extra, mat a)
	! /r

	if goal$='calculate' and date(days(f,'mmddyy'),'ccyymmdd')>date(days(d1,'mmddyy'),'ccyymmdd') then
		pr #255: ""
		pr #255: "The Calculation date is less than the last billing date on Account: ";x$
		pr #255: "   Billing Date: "&date$(days(f,'mmddyy'),'ccyy/mm/dd')&"  Calculation Date: "&date$(days(d1,'mmddyy'),'ccyy/mm/dd')
		pr #255: "   Action: RECORD SKIPPED."
		print_count_skip+=1
		fn_cuu_report_usage
		goto TOP
	else if f=d1 then ! else recalculation reduce balances
		for j=1 to 10
			if env$('client')="Divernon" then goto LX760 ! Divernon's penalties are added into gb
			if uprc$(penalty$(j))="Y" then goto LX770 ! don't subtract penalties out on recalculation
			LX760: !
			if env$('client')="White Hall" and (j=6 or j=7 or j=10) then goto LX770
			gb(j)=gb(j)-g(j) !  if j><6 then gb(j)=gb(j)-g(j)
			LX770: !
		next j
	end if  ! f=d1
	w7=g(11)
	mat g=(0)
	usage_srv1=0 ! WATER USAGE
	usage_srv3=0 ! ELECTRIC USAGE / lawn meter usage
	usage_srv4=0 ! GAS USAGE
	r9_usage_is_zero=0
	mat w=(0) ! mat w appears to never be set - never be used, but is passed to fncalk
	gosub CHECK_UNUSUAL_USAGE
	if r9_usage_is_zero=1 then goto TOP
	if env$('client')="Chatom" then
		fncalkChatom(x$,d1,f,usage_srv1,usage_srv3,usage_srv4,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,hDeposit2,btu, calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
	else
		fncalk(x$,d1,f,usage_srv1,usage_srv3,usage_srv4,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,hDeposit2,btu, calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
	end if
	fn_date_meter_read ! update meter reading date
	if g(11)>99999 or g(12)>99999 then
		goto BILL_TOO_LARGE
	else if g(11)<99999 and g(11)>-99999 then
		goto LX1230
	end if
	BILL_TOO_LARGE: !
	pr #255: ""
	pr #255: "Net or Gross Bill too large on Account: ";x$
	pr #255: "   Net Bill: "&str$(g(11))&"  Gross Bill: "&str$(g(12))
	pr #255: "   Action: RECORD SKIPPED."
	print_count_skip+=1
	fn_cuu_report_usage
	goto TOP

	LX1230: !
	fn_bud2
	fn_updtbal
	for j=1 to 10
		if uprc$(penalty$(j))<>"Y" then  ! don't add penalties into mat gb
			gb(j)=gb(j)+g(j)
		end if
	next j
	rewrite #h_customer,using F_CUSTOMER,key=x$: meteradr$,custname$,mat a,mat b,mat c,mat d,bal,f,mat g,mat gb,mat extra conv CONV_CUSTOMER_REWRITE
	fn_write_new_trans
goto TOP ! /r
CONV_CUSTOMER_REWRITE: ! r:
	pr #255: ""
	pr #255: "The bill ("&str$(g(12))&") on account "&x$&" is too large for"
	pr #255: "the system to handle.  This record is being skipped. "
	pr #255: 'You must determine what is wrong and re-enter the reading."'
	pr #255: 'ACTION: RECORD SKIPPED'
	print_count_skip+=1
	!
	txt$(1)="The bill ("&str$(g(12))&") on account "&x$&" is too large for"
	txt$(2)="the system to handle.  This record is being skipped. "
	txt$(3)='You must determine what is wrong and re-enter the reading."'
	fnmsgbox(mat txt$,resp$(1),'',48)
goto TOP ! /r
FINIS: ! r:
	fn_t9notification
	close #h_customer: ioerr ignore
	if goal$='calculate' then
		close #h_work,free: ioerr ignore
		fnFree(work_addr$)
	end if
	close #h_ubtrans: ioerr ignore
	! pr 'print_count_unusual=';print_count_unusual : pr 'print_count_skip=';print_count_skip
	if print_count_unusual or print_count_skip then let fncloseprn
! /r
XIT_CALCULATE: !
fnend
def fn_write_new_trans
	dim transkey$*19
	tamount=g(11)
	tdate=d1
	tdate=fndate_mmddyy_to_ccyymmdd(tdate)
	tcode=1
	wr=d(1): wu=d(3): er=d(5): eu=d(7): gr=d(9)
	gu=d(11)
	for j=1 to 11 : tg(j)=g(j) : next j
	transkey$=x$&cnvrt$("pic(########)",tdate)&cnvrt$("pic(#)",tcode)
	read #h_ubtrans,using FORM_UBTRANS,key=transkey$: y$ nokey UBTRANS_NOKEY ! check for recalk
	rewrite #h_ubtrans,using FORM_UBTRANS,key=transkey$: x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,bal,pcode
	goto WNT_XIT
	UBTRANS_NOKEY: !
		! need to update the balance on any transaction that may have been processed since the original charge transaction was created.
		write #h_ubtrans,using FORM_UBTRANS: x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,bal,pcode
	WNT_XIT: !
fnend

def fn_t9notification
	if t9 then
		txt$(1)="One or more reading(s) were encounterd for an account(s) that could not be located."
		txt$(2)="Set up the UB Accounts indicated on the report."
		txt$(3)='Then re-enter and calculate the readings for those customers.' !  by using "Enter Readings and Charges"'
		fnmsgbox(mat txt$,resp$(1),'',48)
	end if
fnend
def fn_cuu_report_main(unusual_service$*128)
	if d1<>f then
		if unusual_usage_report=1 or unusual_usage_report=3 then
			pr #255: ""
			pr #255: "Unusual "&unusual_service$&" Usage on Customer "&trim$(x$)&".  Bill was calculated."
			pr #255,using 'form pos 1,c 30,x 2,c 30': custname$,meteradr$
			fn_cuu_report_usage
			print_count_unusual+=1
		end if
	end if
fnend
NKT9: ! r: NOKEY ROUTINE CODE T9=9
	t9=9
	pr #255: ""
	pr #255: "Could not locate an account for Account: "&x$
	pr #255: "   Action: RECORD SKIPPED."
	print_count_skip+=1
goto TOP ! /r
CHECK_UNUSUAL_USAGE: ! r:
	unusual_service$=''
	if fn_cuu_water then let fn_cuu_report_main(unusual_service$&'/'&serviceName$(1))
	if fn_cuu_electric then let fn_cuu_report_main(unusual_service$&'/'&serviceName$(3))
	if fn_cuu_gas then let fn_cuu_report_main(unusual_service$&'/'&serviceName$(4))
	unusual_service$=trim$(unusual_service$,'/')

	if r9_usage_is_zero=1 then
		pr #255: ""
		pr #255: "Negative usage on Account: "&x$
		pr #255: "   Action: RECORD SKIPPED."
		print_count_skip+=1
		if d1<>f then
			fn_cuu_report_usage
		end if
		goto CUU_XIT
	end if
CUU_XIT: !
	return  ! /r
def fn_cuu_report_usage
	! CUU_REPORT_USAGE_MAIN: !
	x=0
	mat watuse=(0) : mat watdat=(0)
	mat elecuse=(0) : mat elecdat=(0)
	mat gasuse=(0) : mat gasdat=(0)
	restore #h_ubtrans,key>=x$&"         ": nokey CUU_REPORT_USAGE_PRINT ! find all old usages
	CUU_UBTRANS_READ: !
	read #h_ubtrans,using L2360: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof CUU_REPORT_USAGE_PRINT
	L2360: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>x$ then goto CUU_REPORT_USAGE_PRINT
	if tcode<>1 then goto CUU_UBTRANS_READ ! only charge transactions
	billdate=d1
	if tdate<fndate_mmddyy_to_ccyymmdd(billdate)-10000 then goto CUU_UBTRANS_READ ! only list last 12 months
	x=x+1
	if x>12 then goto CUU_REPORT_USAGE_PRINT
	watuse(x)=wu : watdat(x)=tdate
	elecuse(x)=eu : elecdat(x)=tdate
	gasuse(x)=gu : gasdat(x)=tdate
	goto CUU_UBTRANS_READ
	!
	CUU_REPORT_USAGE_PRINT: !
	pr #255: "Type of Service     Old Reading   Current Reading       Calculated Usage"
	F_PR_SERVICE: form c 22,pic(---------),x 9,pic(---------),x 11,pic(----------),x 2,c 30,x 2,c 30
	F_PR_PRIOR_USAGES: form pos 1,c 13,12*(pic(zzzz/zz/zz),nz 9,x 1)
	if trim$(serviceName$(1))<>"" then ! test vs. water
		pr #255,using F_PR_SERVICE: "Water",d(1),x(1),usage_srv1
		pr #255,using F_PR_PRIOR_USAGES: " Prior Usages",watdat(1),watuse(1),watdat(2),watuse(2),watdat(3),watuse(3),watdat(4),watuse(4),watdat(5),watuse(5),watdat(6),watuse(6),watdat(7),watuse(7),watdat(8),watuse(8),watdat(9),watuse(9),watdat(10),watuse(10),watdat(11),watuse(11),watdat(12),watuse(12)
	end if
	if trim$(serviceName$(3))="Electric" or trim$(serviceName$(3))="Lawn Meter" then ! test vs. Electric/lawn meter
		pr #255,using F_PR_SERVICE: "Electric",d(5),x(3),usage_srv3
		pr #255,using F_PR_PRIOR_USAGES: " Prior Usages",elecdat(1),elecuse(1),elecdat(2),elecuse(2),elecdat(3),elecuse(3),elecdat(4),elecuse(4),elecdat(5),elecuse(5),elecdat(6),elecuse(6),elecdat(7),elecuse(7),elecdat(8),elecuse(8),elecdat(9),elecuse(9),elecdat(10),elecuse(10),elecdat(11),elecuse(11),elecdat(12),elecuse(12)
	end if
	if trim$(serviceName$(4))="Gas" then ! test vs. Gas
		pr #255,using F_PR_SERVICE: "Gas",d(9),x(2),usage_srv4
		pr #255,using F_PR_PRIOR_USAGES: " Prior Usages",elecdat(1),gasuse(1),elecdat(2),gasuse(2),elecdat(3),gasuse(3),elecdat(4),gasuse(4),elecdat(5),gasuse(5),elecdat(6),gasuse(6),elecdat(7),gasuse(7),elecdat(8),gasuse(8),elecdat(9),gasuse(9),elecdat(10),gasuse(10),elecdat(11),gasuse(11),elecdat(12),gasuse(12)
	end if
	pr #255: ""
fnend
def fn_date_meter_read ! update meter reading dates
	! f =  billing date (from customer record)
	! d1 = billing date being processed
	! extra(3) prior reading date (from customer record)
	! extra(4) current reading date (from customer record)
	! if trim$(x$)='101385.00' then pause
	if dateread<>0 then
		if f=d1 then
			extra(3)=dateread
		else
			extra(4)=extra(3)
			extra(3)=dateread
		end if
	end if  ! dateread<>0
fnend
def fn_updtbal
	d2=f : f=d1
	if d1=d2 then bal=bal+g(11)-w7 else bal=bal+g(11)
fnend
def fn_demand
	if env$('client')="Bethany" then
		read #h_ratemst,using 'form pos 55,32*g 10',key="DM"&lpad$(str$(extra(11)),2): mc1,mu1,mat rt nokey DEMAND_XIT
		goto L6360
	end if
	!  Read #h_ratemst,Using 540,Key="DM"&LPAD$(STR$(B(2)),2): MC1,MU1,MAT RT Nokey 6070  ! don't have a demand code any where in record.  wlll have to customize for each client  on Bethany we used service 6 to hold demand
	L6360: !
	if env$('client')="Bethany" then
		g(6)=mc1
		goto DEMAND_FINIS
	end if
	if env$('client')="Lovington" then goto DEMAND_FINIS
	g(6)=round(x(4)*d(14)*.001*rt(1,3),2)
	DEMAND_FINIS: !
	d(15)=x(4)
	DEMAND_XIT: !
fnend
def fn_bud_open
	bud1=0
	open #budmstr:=6: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr BUD1_XIT
	open #budtrans:=7: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative
	bud1=1
	BUD1_XIT: !
fnend
def fn_deposit_open !
	open #hDeposit1:=fngethandle: "Name=[Q]\UBmstr\Deposit1.h[cno],KFName=[Q]\UBmstr\DepIdx1.h[cno],Shr,Use,RecL=16,KPs=1,KLn=10",internal,outIn,keyed
	open #hDeposit2:=fngethandle: "Name=[Q]\UBmstr\Deposit2.h[cno],KFName=[Q]\UBmstr\Deposit2Index.h[cno]"&',Shr,Use,RecL=73,KPs=1,KLn=10',internal,outIn,keyed
fnend
def fn_bud2
	bud2=0
	if bud1=0 then goto L7110
	mat bt2=(0)
	read #budmstr,using FORM_BUDMSTR,key=x$: z$,mat ba,mat badr nokey L7110
	if sum(ba)=0 then goto L7110 ! if first screen of budget is blank, then skip budget information  kj 10/20/09
	FORM_BUDMSTR: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	! TRANS ROUTINE
	ta1=badr(1)
	L6840: if ta1=0 then goto UPDATE_BUDGET_FILE
	read #budtrans,using L6860,rec=ta1: z$,mat bt1,nba noRec UPDATE_BUDGET_FILE
	L6860: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(1,2)=d1 then bud2=1 : goto UPDATE_BUDGET_FILE
	ta1=nba : goto L6840

	UPDATE_BUDGET_FILE: !
	! IF BUD2=1 THEN RE-CALCULATION
	bt2(1,1)=bt2(1,2)=d1
	bn1=totpen=0
	for j=2 to 13
		bt2(j,2)=g(j-1)
		if ba(j)=0 then bt2(j,1)=ba(j)
		if ba(12)>0 then bn1=ba(12) : goto L7020 ! TOTAL BILL BUDGETED
		if ba(j)=0 then bt2(j,1)=bt2(j,2) else bt2(j,1)=ba(j)
		if j>11 then goto L7010 ! only 1st 10 can be charges
		if penalty$(j-1)="Y" then
			totpen+=bt2(j,1)
			goto L7020
		end if
		L7010: !
		if j<11 then bn1=bn1+bt2(j,1)
		L7020: !
	next j
	bt2(12,1)=bn1
	bt2(13,1)=bt2(12,1)+totpen
	if bud2=1 then
		rewrite #budtrans,using L6860,rec=ta1: x$,mat bt2
		goto L7100
	end if
	write #budtrans,using L6860: x$,mat bt2,badr(1)
	r82=lrec(budtrans)
	badr(1)=r82
	if badr(2)=0 then badr(2)=r82
	L7100: !
	rewrite #budmstr,using FORM_BUDMSTR,key=x$: x$,mat ba,mat badr
	L7110: !
fnend

def fn_usage(usage_service_number)
	! requires local variables: d1, f, mat x, mat d
	usage_return=0
	if usage_service_number=1 then
		if x(1)=0 or d1=f then
			usage_return=x(1)-d(2)
		else
			usage_return=x(1)-d(1)
		end if
	else if usage_service_number=3 then
		if d1=f then
			usage_return=x(3)-d(6)
		else
			usage_return=x(3)-d(5)
		end if
	else if usage_service_number=4 then
		if d1=f then
			usage_return=x(2)-d(10)
		else
			usage_return=x(2)-d(9)
		end if
	else
		pr 'usage_service_number not recognized.' : pause
	end if
	fn_usage=usage_return
fnend
def fn_cuu_water
	cuu_water_return=0
	if serviceName$(1)="Water" then
		usage_srv1=fn_usage(1)
		if (a(1)=9 or a(1)=0) and (a(2)=9 or a(2)=0) then goto CUU_WATER_XIT ! skip if no water code and no sewer code
		if usage_srv1>=0 then
			if d(3)=0 then goto CUU_WATER_XIT
			if usage_srv1<d(3)-d(3)*pcent or usage_srv1>d(3)+d(3)*pcent then
				if unusual_usage_report=3 then
					pr #255: '* '&serviceName$(1)&' unusual because '&str$(usage_srv1)&'<'&str$(d(3)-d(3)*pcent)&' or '&str$(usage_srv1)&'>'&str$(d(3)+d(3)*pcent)
				end if
				cuu_water_return=1
			end if
		else
			if x(12)=0 then r9_usage_is_zero=1
		end if
	end if
	CUU_WATER_XIT: !
	fn_cuu_water=cuu_water_return
fnend
def fn_cuu_electric
	cuu_electric_return=0
	if serviceName$(3)="Electric" or serviceName$(3)="Lawn Meter" or service$(3)="EL" then
		if x(3)=0 then goto CUU_ELEC_XIT

		usage_srv3=fn_usage(3)

		if usage_srv3>=0 then
			if d(7)=0 then goto CUU_ELEC_XIT
			if usage_srv3<d(7)-d(7)*pcent or usage_srv3>d(7)+d(7)*pcent then
				if unusual_usage_report=3 then
					pr #255: '* '&serviceName$(3)&' unusual because '&str$(usage_srv3)&'<'&str$(d(7)-d(7)*pcent)&' or '&str$(usage_srv3)&'>'&str$(d(7)+d(7)*pcent)
				end if
				cuu_electric_return=1
			end if
		else
			if x(13)=0 then r9_usage_is_zero=1
		end if
	end if
	CUU_ELEC_XIT: !
	fn_cuu_electric=cuu_electric_return
fnend
def fn_cuu_gas
	cuu_gas_return=0
	if service$(4)="GA" or serviceName$(4)="Gas" then
		if x(2)=0 then goto CUU_GAS_XIT
		usage_srv4=fn_usage(4)
		if usage_srv4>=0 then
			if d(11)=0 then goto CUU_GAS_XIT
			if usage_srv4<d(11)-d(11)*pcent or usage_srv4>d(11)+d(11)*pcent then
				if unusual_usage_report=3 then
					pr #255: '* '&serviceName$(4)&' unusual because '&str$(usage_srv4)&'<'&str$(d(11)-d(11)*pcent)&' or '&str$(usage_srv4)&'>'&str$(d(11)+d(11)*pcent)
				end if
				cuu_gas_return=1
			end if
		else
			if x(14)=0 then r9_usage_is_zero=1
		end if
	end if
	CUU_GAS_XIT: !
	fn_cuu_gas=cuu_gas_return
fnend
def fn_ask_billing_date
	! returns ckey (if ckey=5 upon return then cancel  was selected)
	ASK_BILLING_DATE: !
	fnTos
	mylen=24 : mypos=mylen+2
	respc=0 : linec=0
	fnLbl(linec+=1,1,"Billing Date:",mylen,1)
	! fnLbl(1,1,"",34,1)
	fnTxt(linec,mypos,8,0,1,"1001")
	resp$(respc_billing_date:=respc+=1)=str$(d1)
	if env$('client')='Campbell' then
			linec+=1
			fnLbl(linec+=1,1,"Sewer Cap Date:",mylen,1)
			fnTxt(linec,mypos,8,0,1,"1")
			fncreg_read('ubcalk-sewer_cap_date',sewer_cap_date$)
			resp$(resp_sewer_cap_date:=respc+=1)=sewer_cap_date$
	end if
	if enableCostOfGas=1 then ! ask BTU question on Edinburg, French Settlement, and Allendale
		if env$('client')="Edinburg" then
			fnLbl(linec+=1,1,"Current BTU Factor:",mylen,1)
		else
			fnLbl(linec+=1,1,"Cost of Gas Adjustment:",mylen,1)
		end if
		fnTxt(linec,mypos,10,0,1,"1045")
		resp$(resp_btu_factor:=respc+=1)=str$(btu)
	end if
	if env$('client')='French Settlement' then
		linec+=1
		fnChk(linec+=1,1,"Calculate Interest on Deposit")
		resp$(resp_calc_interest_on_deposit:=respc+=1)='False'
		fnChk(linec+=1,1,"Charge Inspection Fee")
		resp$(resp_charge_inspection_fee:=respc+=1)='False'
		fnLbl(linec-2,35,"Interest Credit Rate:",21,1)
		fnTxt(linec-2,58,10,0,1,"44")
		resp$(resp_interest_credit_rate:=respc+=1)='.0500' ! str$(.05)
	end if
	! r: unusual usage report qusetion
	dim unusual_usage_report_opt$(3)*52,unusual_usage_report$*52
	unusual_usage_report_opt$(1)="Unusual and Skipped (Classic)"
	unusual_usage_report_opt$(2)="Skipped Accounts Only"
	unusual_usage_report_opt$(3)="Unusual, Skipped and Show Calculations"
	fnLbl(linec+=1,1,"Unusual Usage Report:",mylen,1)
	fncomboa('ubcalk-unusal_usage_report',linec,mypos,mat unusual_usage_report_opt$, 'Select the unusual usage report style you prefer') ! ,width,contain,tabcon)
	fncreg_read('ubcalk-unusal_usage_report',unusual_usage_report$,unusual_usage_report_opt$(2))
	unusual_usage_report=srch(mat unusual_usage_report_opt$,unusual_usage_report$)
	if unusual_usage_report=0 then unusual_usage_report=1 : unusual_usage_report$=unusual_usage_report_opt$(unusual_usage_report)
	resp$(resp_unusual_usage_report:=respc+=1)=unusual_usage_report$
	! /r
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey<>5 then
		d1=val(resp$(respc_billing_date))
		if enableCostOfGas then btu=val(resp$(resp_btu_factor)) ! Edinburg requires a monthly BTU factor for calculating taxes
		if resp_calc_interest_on_deposit and resp$(resp_calc_interest_on_deposit)='True' then calc_interest_on_deposit=1 else calc_interest_on_deposit=0
		if resp_charge_inspection_fee and resp$(resp_charge_inspection_fee)='True' then charge_inspection_fee=1 else charge_inspection_fee=0
		if resp_interest_credit_rate then interest_credit_rate=val(resp$(resp_interest_credit_rate))
		unusual_usage_report$=resp$(resp_unusual_usage_report)
		unusual_usage_report=srch(mat unusual_usage_report_opt$,unusual_usage_report$)
		fncreg_write('ubcalk-unusal_usage_report',unusual_usage_report$)
		if d1<10101 then pr bell; : goto ASK_BILLING_DATE
		fnLastBillingDate(d1,1)
		if resp_sewer_cap_date then ! if env$('client')='Campbell'
			sewer_cap_date$=resp$(resp_sewer_cap_date)
			fncreg_write('ubcalk-sewer_cap_date',sewer_cap_date$)
		end if
	end if
fnend

def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
		! r: dims
		dim resp$(10)*128
		dim w(5)            ! only dimmed and reset locally - it is used and set in fncalk
		dim x$*10
		dim x(15)
		dim gb(10)
		dim rt(10,3)
		dim ba(13)
		dim da(2)
		dim txt$(3)*128
		dim a(7)
		dim b(11)
		dim c(4)
		dim d(15)
		dim g(12)
		dim bt1(14,2)
		dim p$*10
		dim bt2(14,2)
		dim badr(2)
		dim tg(11)
		dim meteradr$*30
		dim custname$*30
		dim watuse(12)
		dim watdat(12)
		dim elecuse(12)
		dim elecdat(12)
		dim gasuse(12)
		dim gasdat(12)
		dim serviceName$(10)*20
		dim tax_code$(10)*1
		dim work$*256
		dim work_addr$*256
		dim subjectto(10)
		dim extra(23)
		! /r
	end if
fnend
include: Ertn no