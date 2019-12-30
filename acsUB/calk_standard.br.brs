def fn_setup_calk
	if ~setup_calk then ! 
		setup_calk=1
		library 'S:\Core\Library': fnpause,fncd,fngethandle,fnget_services,fncreg_read
		library 'S:\Core\Library': fnDepositChangeLog
		dim x$*10,gb(10),dp$*60,serviceName$(10)*20,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
		FORM_RATEMSTR: form pos 55,32*g 10
		fnget_services(mat serviceName$, mat service$, mat tax_code$,mat penalty$,mat subjectto)
		for j=1 to udim(serviceName$)
			serviceName$(j)=trim$(serviceName$(j))
		next j
	end if 
	if env$('client')='Campbell' then
			fncreg_read('ubcalk-sewer_cap_date',sewer_cap_date$)
			sewer_cap_date=val(sewer_cap_date$)
	end if
	dim onlyMonth(10)
	for service_item=1 to 10
		fncreg_read('Service '&str$(service_item)&' only month',tmp$) : onlyMonth(service_item)=val(tmp$)
	next service_item
fnend 
Ignore: continue
def library fncalk(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,deposit2,btu; calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
	debug_account=0
	! if trim$(x$)='101200.00' and env$('acsdeveloper')<>'' then debug_account=1 ! pause
	! if trim$(x$)='300485.00' then pause
	! 
	if ~setup_calk then let fn_setup_calk
	! when creating rate routines and assigning service codes,
	! water       service 1 with code "WA"
	! Sewer       service 2 with code "SW"
	! Electric    service 3 with code "EL"
	! Lawn Meters service 3 with code "LM" and name "Lawn Meter"
	! Gas         service 4 with code "GA"
	! Gas Purchase Adjustment (if in it's own field)
	!             service 5 with code "GP"
	! Penalty can be anywhere with any code;
	! Sales Tax can be anywhere, but requires code "TX"
	! Inspection Fee can be anywhere, but requires code "IF" and name "Inspection Fee" and must pass variable charge_inspection_fee when wanted to be calculated
	! Interest on Deposit
	!             service 5+, no code necessary, name "Interest on Deposit" required and must pass variable calc_interest_on_deposit when wanted to be calculated
	! you can use either gas or electric for some other meter reading by giving it the correct service code name, but using EL or GA as the codes.
	! don't ever change an existing routine for any service.  Add a new routine  for any service that cannot go thru an existing routine.  This will allow the calculation program to become standard.
	! 

	!   if service$(6)='SF' and env$('client')="Pennington" and extra(11)=0 then extra(11)=1 ! default all Inspection Fee codes of 0 to a 1
	! 
	first_non_metered_service=5
	if fn_PassesOnlyMonthFilter(1) and serviceName$(1)<>"" and service$(1)="WA" then let fn_calk_water ! always use WA as water code in rate file
	if fn_PassesOnlyMonthFilter(3) and serviceName$(3)="Lawn Meter" and service$(3)="LM" then let fn_calk_lawnmeter ! must always use LM for the rate code for lawn meters
	if fn_PassesOnlyMonthFilter(3) and serviceName$(3)(1:5)="Reduc" then let fn_calk_reduc
	if fn_PassesOnlyMonthFilter(2) and serviceName$(2)<>"" and service$(2)="SW" then let fn_calk_sewer ! always use SW for sewer code in rate file
	if fn_PassesOnlyMonthFilter(3) and serviceName$(3)="Electric" and service$(3)="EL" then let fn_calk_electric ! must always use EL for the rate code for electric
	if fn_PassesOnlyMonthFilter(3) and serviceName$(3)<>"Electric" and trim$(serviceName$(3))<>"" and trim$(service$(3))="EL" then let fn_calk_electric !  allow electric go thru usage calculation if beign as another type of meter other that electric
	if fn_PassesOnlyMonthFilter(3) and serviceName$(3)<>"Electric" and trim$(serviceName$(3))<>"" and trim$(service$(3))<>"" then 
		j=3
		g(3)=fn_calk_non_metered(j) ! go thru non-metered if using electric for something else 
	end if 
	! If serviceName$(3)<>"" AND SERVICE$(3)="AD" Then let fn_calk_administrative_fee ! electric service used of administrative fee
	if fn_PassesOnlyMonthFilter(4) and trim$(serviceName$(4))="Gas" and trim$(service$(4))="GA" then let fn_calk_gas
	if fn_PassesOnlyMonthFilter(4) and serviceName$(4)<>"Gas" and trim$(serviceName$(4))<>"" and trim$(service$(4))="GA" then let fn_calk_gas !  allow gas go thru usage calculation if being usd as another type of meter other that gas
	if fn_PassesOnlyMonthFilter(4) and serviceName$(4)<>"Gas" and trim$(serviceName$(4))<>"" and trim$(service$(4))<>"" then 
		j=4
		g(4)=fn_calk_non_metered(j) ! go thru non-metered if using gas for something else 
	end if 
	if fn_PassesOnlyMonthFilter(5) and btu and trim$(service$(5))="GP" then 
		g(5)=fn_calk_purcahsed_gas_cost_adj(btu,usage_gas)
		first_non_metered_service=6
	end if 
	fn_calk_demand
	for j=first_non_metered_service to 10
		if fn_PassesOnlyMonthFilter(j) then 
			if trim$(serviceName$(j))="Interest on Deposit" and calc_interest_on_deposit then 
				fn_interest_credit(interest_credit_rate)
			else if penalty$(j)<>"Y" and service$(j)<>"TX" and trim$(serviceName$(j))<>"" then ! skip penalty, sales tax and unused services
				if env$('client')="Kimberling" and int(d1*.0001)><2 and (j=5 or j=6) then goto LX1110 ! CALCULATE fees EACH FEB 1
				if trim$(serviceName$(j))="Inspection Fee" and ~charge_inspection_fee then goto LX1110 ! French Settlement Gas only ask that question, but it should only be calculated when selected
				if j=6 and env$('client')='Lovington' then goto SKIP_THIS_NON_METERED_SERVICE
				g(j)=fn_calk_non_metered(j)
				SKIP_THIS_NON_METERED_SERVICE: ! 
			end if 
			LX1110: ! 
		end if
	next j
	fn_calk_for_final_bill
	fn_calk_sales_tax
	fn_calk_penalty
	fn_calk_net ! NET AND GROSS BILL
fnend 
def fn_PassesOnlyMonthFilter(pomfServiceCode)
	if onlyMonth(pomfServiceCode)<=0 then
		pomfReturn=1
	else if onlyMonth(pomfServiceCode)=date(days(d1,'mmddyy'),'mm') then 
		pomfReturn=1
	else
		pomfReturn=0
	end if
	fn_PassesOnlyMonthFilter=pomfReturn
fnend
def fn_calk_water
	! if debug_account then pr x$; 'about to go through water routine' : pause
	if x(9)=0 then goto L2670
	w(1)=x(9)
	if x(12)=0 then goto WATER_COMPLETED
	usage_water=x(12)
	goto WATER_COMPLETED
	L2670: if x(12)=0 then goto L2690
	usage_water=x(12)
	L2690: if usage_water>=0 then goto L2720
	goto STANDARD_WATER_CHARGE
	! ___________________________
	L2720: if b(1)><0 then goto STANDARD_WATER_CHARGE ! 2140 ! CALCULATION
	! WATER
	if a(1)=0 and a(2)=0 then goto WATER_END
	read #h_ratemst,using FORM_RATEMSTR, key="WA"&lpad$(str$(a(1)),2): mc1,mu1,mat rt nokey STANDARD_WATER_CHARGE
	! w(1)   is the water charge
	! mc1    is the minimum charge
	! d(13)  is Service 1 (Water) – Unit Count
	w(1)=mc1*max(1,d(13)) ! set the water charge to minimum charge * Unit Count
	if usage_water<=mu1*max(1,d(13)) then goto WATER_COMPLETED else mu2=mu1*max(1,d(13))
	for j=1 to 10
		if rt(j,1)>usage_water then goto WATER_COMPLETED
		if usage_water<rt(j,2) then w1=usage_water-mu2 else w1=rt(j,2)-mu2
		w(1)=w(1)+round(w1*rt(j,3),2)
		if rt(j,2)>usage_water then goto WATER_COMPLETED
		mu2=rt(j,2)
	next j
	goto WATER_COMPLETED
	! ______________________________________________________________________
	STANDARD_WATER_CHARGE: !
		w(1)=b(1)
	goto WATER_COMPLETED
	WATER_COMPLETED: ! 
	!   if env$('client')="Riverside" and w(1)<mc1 then w(1)=mc1
	!   if env$('client')="Albany" and (a(1)=3 or a(1)=6) then usage_water=usage_water*2 ! correct usage after using 1/2 of it
	if env$('client')="Brier Lake" and usage_water>mu1 then w(1)=w(1)+2
	if d1<>f then d(2)=d(1)
	d(1)=x(1)
	w8=d(3)
	d(3)=usage_water
	d(4)=d(4)+d(3)
	if d1=f then d(4)=d(4)-w8
	WATER_END: g(1)=w(1)
fnend  ! fn_calk_water
def fn_calk_sewer
	! if trim$(x$)='300290.02' then pause
	if env$('client')='Lovington' and a(2)>0 then g(6)=5 ! storm sewer
	if x(5)><0 then w(2)=x(5) : goto SEWER_COMPLETED
	if b(2)><0 then goto STANDARD_SEWER_CHARGE ! was disabled but re-enabled on 12/5/16 - standard charge should work on sewer.  clients like Pennington need it.
	!   if env$('client')="Ashland" and a(2)=1 and g(1)<>0 then w(2)=round(g(1)*3/4,2) : goto SEWER_COMPLETED
	read #h_ratemst,using FORM_RATEMSTR,key="SW"&lpad$(str$(a(2)),2): mc1,mu1,mat rt nokey STANDARD_SEWER_CHARGE
	if env$('client')="Ash Grove" and a(2)=2 then ! do not average commercial sewer
		usage_sewer=usage_water 
	else if extra(18)>0 and env$('client')<>"White Hall" and env$('client')<>"Findlay" then ! most people do not average sewer usage over a number of months and then use that average for a number of months
		usage_sewer=extra(18) ! average sewer usage   as calculated by the "Calculate Sewer Average" program.
	else 
		usage_sewer=usage_water
	end if 
	if extra(5)>0 then usage_sewer=usage_sewer-extra(5) ! sewer reduction
	if env$('client')="Ash Grove" then usage_sewer=int((usage_sewer+50)*.01)*100 ! ROUND TO NEAREST 100 ON SEWER
	if serviceName$(3)="Lawn Meter" then usage_sewer=usage_sewer-x2 ! reduce sewer usage by lawn meter usage
	if serviceName$(3)(1:5)="Reduc" and service$(3)="SW" then usage_sewer=usage_sewer-x2 ! reduce sewer usage by Reduce Sewer usage
	if env$('client')="Kimberling" then usage_sewer=usage_sewer-x2-x3 : eu1=x2
	w(2)=mc1*max(1,extra(14)) ! units per meter - sewer (default to one)
	if usage_sewer<=mu1 then goto L3300 else mu2=mu1
	for j=1 to 10
		if rt(j,1)>usage_sewer then goto L3300
		if usage_sewer<rt(j,2) then w1=usage_sewer-mu2 else w1=rt(j,2)-mu2
		w(2)=w(2)+round(w1*rt(j,3),2)
		if rt(j,2)>usage_sewer then goto L3300
		mu2=rt(j,2)
	next j
	L3300: ! 
	goto SEWER_COMPLETED
	! ______________________________________________________________________
	STANDARD_SEWER_CHARGE: ! 
	w(2)=b(2)
	SEWER_COMPLETED: ! 
	g(2)=w(2)
	if sewer_cap_date>0 and a(2)<=5 then ! if env$('client')='Campbell' and sewer_cap_date>0 then
		! only subject to cap if rate code is a 5 or lower
		sewer_cap_amount=fn_service_chg_from_history(2,sewer_cap_date,x$)
		if sewer_cap_amount>0 and sewer_cap_amount<g(2) then
			g(2)=sewer_cap_amount
		end if
		!   pause ! acct 800370.16 43016 sewer charge was 19.06 water usage was 2040
	end if
	! pause
fnend  ! fn_calk_sewer
def fn_service_chg_from_history(service_number,history_date,scfh_account$)
	if ~scfh_setup then
		scfh_setup=1
		open #scfh_h_trans:=fngethandle: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
	end if
	scfh_return=0
	dim scfh_key$*19,scfh_alloc_amt(10)
	scfh_key$=scfh_account$&date$(days(sewer_cap_date,'mmddyy'),'ccyymmdd')&'1'
	mat scfh_alloc_amt=(0)
	read #scfh_h_trans,using 'form pos 24,10*PD 4.2',key=scfh_key$: mat scfh_alloc_amt nokey ignore
	fn_service_chg_from_history=scfh_alloc_amt(service_number)
fnend
def fn_calk_non_metered(j) ! all non-metered charges but penalty and tax
	!  if trim$(x$)='100150.00' then pause
	calk_non_metered_return=0
	if j=3 then ! electric fields used for a non         -metered service
		entered_amt=x(10)
		standard_amt=b(j)
		rate_code=a(j)
		service_code$=service$(j)
	else if j=4 then ! gas used for non=metered                service
		entered_amt=x(11)
		standard_amt=b(j)
		rate_code=a(j)
		service_code$=service$(j)
	else if j=5 then 
		entered_amt=x(6)
		standard_amt=b(j)
		rate_code=a(j)
		service_code$=service$(j)
	else if j=6 then 
		entered_amt=x(7)
		standard_amt=b(j)
		rate_code=extra(11)
		service_code$=service$(j)
	else if j=7 then 
		entered_amt=0
		standard_amt=0
		rate_code=extra(12)
		service_code$=service$(j)
	else if j=8 then 
		entered_amt=x(8)
		standard_amt=b(7)
		rate_code=extra(13)
		service_code$=service$(j)
	else if j=9 then 
		entered_amt=0
		standard_amt=0
		rate_code=a(6)
		service_code$=service$(j)
	else if j=10 then 
		entered_amt=0
		standard_amt=0
		rate_code=a(7)
		service_code$=service$(j)
	else 
		g(j)=0
		goto NM_XIT ! service not covered
	end if 
	! NM_FINIS: !
	if entered_amt>0 then 
		calk_non_metered_return=entered_amt
	else if standard_amt>0 then 
		calk_non_metered_return=standard_amt
	else 
		read #h_ratemst,using FORM_RATEMSTR,key=service_code$&lpad$(str$(rate_code),2): mc1,mu1,mat rt nokey NM_XIT
		calk_non_metered_return=max(mc1,rt(1,3)) ! g(j)=max(mc1,rt(1,3))
		! if env$('client')="Carrizo" and j=7 then gosub CARRIZO_TRASH_TAX
		if env$('client')="Pennington" and service_code$='SF' then gosub PENNINGTON_SERVICE_FEE
	end if 
	NM_XIT: ! 
	fn_calk_non_metered=calk_non_metered_return
fnend  ! fn_calk_non_metered(j)
! CARRIZO_TRASH_TAX: ! r:  called from fn_calk_non_metered
!   if extra(12)=0 then 
!     calk_non_metered_return=0
!     goto CARRIZO_TRASH_TAX_XIT
!   else 
!     read #h_ratemst,using FORM_RATEMSTR,key="TT"&lpad$(str$(extra(12)),2): mc1,mu1,mat rt nokey CARRIZO_TRASH_TAX_XIT
!     calk_non_metered_return=round((g(3)+g(5)+g(6))*rt(1,3),2) ! calculate on canister pickup and trash and canister rental
!   end if 
!   CARRIZO_TRASH_TAX_XIT: ! 
! return  ! /r CARRIZO_TRASH_TAX
PENNINGTON_SERVICE_FEE: ! r: flat percentage on water and sewer
	calk_non_metered_return=round((g(1)+g(2))*rt(1,3),2)
return ! /r
def fn_calk_reduc
	if service$(3)="SW" then 
		x2=x(13)
		d(7)=x(13)
	end if 
fnend  ! fn_calk_reduc
def fn_calk_lawnmeter
	if x(10)=0 then goto L3440
	w(3)=x(10)
	if x(13)=0 then goto LAWNMETER_COMPLETED
	x2=x(13)
	goto LAWNMETER_COMPLETED
	! ___________________________
	L3440: ! 
	if x(13)=0 then goto LX3460
	x2=x(13)
	LX3460: ! 
	if x2>=0 then goto LX3500
	! 
	goto LAWNMETER_COMPLETED
	! ___________________________
	LX3500: ! 
	if b(3)><0 then goto STANDARD_LAWNMETER_CHARGE
	read #h_ratemst,using FORM_RATEMSTR,key="LM"&lpad$(str$(a(3)),2): mc1,mu1,mat rt nokey STANDARD_LAWNMETER_CHARGE
	! wrong If env$('client')="Findlay" Then x2=X(3) ! findlay actually turns in a usage instead of a reading
	lmu1=x2
	w(3)=mc1*(max(1,extra(15))) !  units per meter
	if lmu1<=mu1 then goto L3640 else mu2=mu1
	for j=1 to 10
		if rt(j,1)>lmu1 then goto L3640
		if lmu1<rt(j,2) then w1=lmu1-mu2 else w1=rt(j,2)-mu2
		! 
		w(3)=w(3)+round(w1*rt(j,3),2)
		if rt(j,2)>lmu1 then goto L3640
		mu2=rt(j,2)
	next j
	L3640: ! 
	w(3)=max(mc1,w(3))
	goto LAWNMETER_COMPLETED
	! ___________________________
	STANDARD_LAWNMETER_CHARGE: ! 
	w(3)=b(3)
	LAWNMETER_COMPLETED: ! 
	if env$('client')="Findlay" then x2=x(3) ! findlay actually turns in a usage instead of a reading
	if d1<>f then 
		d(6)=d(5)
	end if 
	d(5)=x(3)
	w8=d(7)
	d(7)=x2
	if d1=f then 
		d(8)=d(8)+d(7)-w8
	else 
		d(8)=d(8)+d(7)
	end if 
	g(3)=w(3)
fnend  ! fn_calk_lawnmeter
def fn_calk_electric
	! if trim$(x$)='300485.00' then pause
	if env$('client')="Kimberling" then goto ELECTRIC_COMPLETED ! don't have electric
	if service$(3)="EL" and serviceName$(3)<>"Electric" then goto ELECTRIC_COMPLETED ! electric used for some other metered service
	if x(10)<>0 then 
		w(3)=x(10)
		if x(13)=0 then goto ELECTRIC_COMPLETED
		x2=x(13)
		goto ELECTRIC_COMPLETED
	end if 
	if x(13)<>0 then 
		x2=x(13)
	end if 
	if x2<0 then 
		goto ELECTRIC_COMPLETED
	end if 
	if b(3)><0 then goto STANDARD_ELEC_CHARGE ! 2580
	if a(3)=0 then goto L4290 ! if rate code is a zero than goto L4290
	read #h_ratemst,using FORM_RATEMSTR,key="EL"&lpad$(str$(a(3)),2): mc1,mu1,mat rt nokey STANDARD_ELEC_CHARGE
	if extra(8)=0 then eu1=x2 else eu1=x2*(extra(8)*.001) ! electric multiplier
	if env$('client')="Bethany" and extra(15)=0 then extra(15)=1
	if env$('client')="Bethany" then goto L4050 ! minimum not used in calculation
	w(3)=mc1*(max(1,extra(15))) ! electric units per meter
	L4050: ! 
	if eu1<=mu1 then goto L4130 else mu2=mu1
	for j=1 to 10
		if rt(j,1)>eu1 then goto L4130
		if eu1<rt(j,2) then w1=eu1-mu2 else w1=rt(j,2)-mu2
		w(3)=w(3)+round(w1*rt(j,3),2)
		if rt(j,2)>eu1 then goto L4130
		mu2=rt(j,2)
	next j
	L4130: ! 
	w(3)=max(mc1*extra(15),w(3))
	goto ELECTRIC_COMPLETED
	! ___________________________
	STANDARD_ELEC_CHARGE: ! 
	w(3)=b(3)
	ELECTRIC_COMPLETED: ! 
	if d1<>f then 
		d(6)=d(5)
	end if 
	d(5)=x(3)
	w8=d(7)
	d(7)=eu1 ! X2  kj 72109
	if d1<>f then 
		d(8)=d(8)+d(7)
	else 
		d(8)=d(8)+d(7)-w8
	end if 
	L4290: ! 
	g(3)=w(3)
fnend  ! fn_calk_electric
! r: def fn_calk_administrative_fee !  (used on Divernon)
!   if x(10) then 
!     w(3)=x(10)
!   else
!     if b(3)><0 then goto STANDARD_ADM_CHARGE
!     read #h_ratemst,using FORM_RATEMSTR,key="AD"&lpad$(str$(a(3)),2): mc1,mu1,mat rt nokey STANDARD_ADM_CHARGE
!     w(3)=mc1
!   end if
!   goto ADMIN_COMPLETED
!   STANDARD_ADM_CHARGE: ! 
!     w(3)=b(3)
!   goto ADMIN_COMPLETED
!   ADMIN_COMPLETED: ! 
!   g(3)=w(3)
! /r fnend  
def fn_calk_penalty ! penalty calculation
	!   if env$('client')="Divernon" then goto DIVERNON ! Divernon has a unique penalty routine
	if env$('client')="Pennington" and a(7)=0 then a(7)=1 ! default all penalty codes of 0 to a 1
	if env$('client')="Granby" and a(7)=0 then a(7)=1 ! default all penalty codes of 0 to a 1
	if env$('client')="Brier Lake" and a(7)=0 then a(7)=1 ! default all penalty codes of 0 to a 1
	mat basepenalty=(0)
	for j=1 to 10
		if subjectto(j)>0 then  ! accumulate all charges by the penalty they are subject to
			basepenalty(subjectto(j))=basepenalty(subjectto(j))+g(j)
			 !     else if env$('client')="Cerro Gordo" and subjectto(j)>0 then 
			 !       basepenalty(subjectto(j))=basepenalty(subjectto(j))+gb(j) ! Cerro Gordo bases penalties on balance
		end if
	next j
	for j=1 to 10
		if uprc$(penalty$(j))="Y" then penaltycode$=uprc$(service$(j)) else goto CP_NEXT_J
		if j<6 then pencode=a(j) ! rate codes in customer layout are not in           order.  The first 5 a( match the services. The next three services are          pulled from mat extra. 9 and 10 use a(6)&a(7)
		if j=6 then pencode=extra(11)
		if j=7 then pencode=extra(12)
		if j=8 then pencode=extra(13)
		if j=9 then pencode=a(6)
		if j=10 then pencode=a(7)
		! If PENCODE=0 OR PENCODE>99 Then pENCODE=1 ! default to one so codes don't have to be added to old customer records
		read #h_ratemst,using FORM_RATEMSTR,key=penaltycode$&lpad$(str$(pencode),2): mc1,mu1,mat rt nokey CP_NEXT_J
		if mc1>0 and env$('client')<>"Millry" then ! penalty is a fixed amount
			g(j)=mc1
			goto CP_NEXT_J
		else if env$('client')="Franklinton" and j=10 then 
			g(10)=round((g(1)+g(2)+g(3)+g(5)+g(8))*.1+(rt(1,3)*x3),2)
			goto CP_NEXT_J
		else if env$('client')="Colyell" and f=d1 then 
			basepenalty(10)=bal+sum(mat g(1:9))
		else if env$('client')="Colyell" and f<>d1 then 
			basepenalty(10)=bal+sum(mat g(1:9))
		end if 
		if env$('client')="White Hall" and f=d1 then 
			basepenalty(10)=g(1)+g(2)+g(4)+g(9)
		else if env$('client')="White Hall" and f<>d1 then 
			basepenalty(10)=g(1)+g(2)+g(4)+g(9)
		end if 
		if env$('client')="Brier Lake" and d1=f then basepenalty(10)=basepenalty(10)+bal-gb(10)
		if env$('client')="Brier Lake" and d1<>f then basepenalty(10)=basepenalty(10)+bal-gb(10)
		if env$('client')="Granby" and d1=f then basepenalty(10)=basepenalty(10)+bal-gb(10)
		if env$('client')="Granby" and d1<>f then basepenalty(10)=basepenalty(10)+bal-gb(10)
		if env$('client')="Kimberling" and g(2)>0 then basepenalty(10)=basepenalty(10)-g(1) ! no penalty on water if they have sewer
		g(j)=round(basepenalty(j)*rt(1,3),2) ! penalty based on base amount that was accumulated for each penalty field * rate for that penalty code
		if env$('client')="Millry" and g(j)<5 then g(j)=5
		CP_NEXT_J: ! 
	next j
	! If env$('client')="Pennington" Then g(10)=ROUND(G(10)*(1+HOLDTAXRATE),2)
	! g(7)=ROUND(G(7)*(1+HOLDTAXRATE),2) ! charge sales tax on penalty
	!   if env$('client')="Riverside" then 
	!     g(10)=round(rt(1,3)*min(mc1,sum(mat g(1:8))),2) 
	!     g(10)=g(10)+rt(2,3)*round(max(0,sum(mat g(1:8))-mc1),2)
	!   end if
	if env$('client')="Kimberling" then let fn_penalty_kimberling ! calculate interest on prev balance
	g(10)=max(0,g(10))
	goto CALK_PENCAL_XIT
	! DIVERNON: ! r: unique penalty calculation  (5% of balance owed on water,sewer                  and gas before new bill calculated.  These are actual charges                   and get added into the balance, balance breakdown, net, gross
	!     if gb(1)>0 and a(5)>0 then g(5)=round(gb(1)*.05,2) : gb(5)+=g(5) : g(11)+=g(5) : g(12)+=g(5)
	!     if gb(2)>0 and extra(11)>0 then g(6)=round(gb(2)*.05,2) : gb(6)+=g(6) : g(11)+=g(6) : g(12)+=g(6)
	!     if gb(4)>0 and extra(12)>0 then g(7)=round(gb(4)*.05,2) : gb(7)+=g(7) : g(11)+=g(7) : g(12)+=g(7)
	!     goto CALK_PENCAL_XIT ! /r
	CALK_PENCAL_XIT: ! 
fnend 
def fn_penalty_kimberling ! add .75% of previous balance (9% annual)
	g(10)=max(0,round(g(10)+max(0,bal)*.0075,2))
fnend 
def fn_calk_net ! CALCULATE NET AND GROSS BILL
	for j=1 to 10
		!     if uprc$(penalty$(j))="Y" and env$('client')="Divernon" then goto L6020 ! don't add penalties into net nor gross
		if uprc$(penalty$(j))<>"Y" then ! add penalties into net
			g(11)=g(11)+g(j)
		end if 
		g(12)=g(12)+g(j)
		! L6020: ! 
	next j
	 !   if (penalty$(5) ="Y" or penalty$(6) ="Y" or penalty$(7) ="Y") and env$('client')="Divernon" then g(12)=g(12)+5 ! divernon also has a 5.00 penalty that can get added in middle of month.
fnend  ! fn_calk_net
def fn_calk_sales_tax
	if env$('client')="Bethany" then goto BETHANY_TAX
	if env$('client')="Franklinton" then goto FRANKLINTON_TAX
	if env$('client')="White Hall" then goto WHITEHALL_TAX
	! r: normal tax
	for j=1 to 10 ! determine which service is tax   (rate code abbreviation must always be TX
		if service$(j)="TX" then taxservice=j
	next j
	if taxservice=0 then 
		taxcode=0
	else if taxservice<6 then ! note - No one has a TX code in anything except 9 or 10
		pr ' faulty logic here - call ACS' : pause : taxcode=a(j)
	else if taxservice=6 then 
		taxcode=extra(11)
	else if taxservice=7 then 
		taxcode=extra(12)
	else if taxservice=8 then 
		taxcode=extra(13)
	else if taxservice=9 then 
		taxcode=a(6)
	else if taxservice=10 then 
		taxcode=a(7)
	end if 
	! L5300: ! 
	! if debug_account then pr x$; 'taxcode=';taxcode;'    taxservice=';taxservice : pause
	taxable=0
	if env$('client')="Pennington" and taxcode=0 then taxcode=1 ! default to tax code 1 on Pennington
	read #h_ratemst,using FORM_RATEMSTR,key="TX"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
	!   if env$('client')="Divernon" then ! tax is % of usage
	!     taxable=x3
	!   else 
		for j=1 to 8
			if uprc$(tax_code$(j))="Y" then taxable=taxable+g(j) ! determine total      taxable sales
		next j
	!   end if 
	! if debug_account then pr x$; 'taxcode=';taxcode;'    taxservice=';taxservice;' total taxable amount:';taxable : pause
	if taxservice>0 and taxservice <=10 then g(taxservice)=round(taxable*rt(1,3),2) ! holdtaxrate=rt(1,3)
	if env$('client')="Edinburg" and btu<>0 then g(taxservice)=min(g(taxservice),round(x3*btu*.024,2)) ! env$('client')="Edinburg"   !! BUT DEFINATELY  NOT French Settlement
	goto SALES_TAX_XIT ! /r SALES_TAX
	! 
	FRANKLINTON_TAX: ! r:
		taxcode=extra(12) ! water
		read #h_ratemst,using FORM_RATEMSTR,key="TW"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
		g(7)=round(g(1)*rt(1,3),2)
		taxcode=a(6) ! gas
		read #h_ratemst,using FORM_RATEMSTR,key="TG"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
		g(9)=round(g(4)*rt(1,3),2)
	goto SALES_TAX_XIT ! /r
	WHITEHALL_TAX: ! r:
		read #h_ratemst,using FORM_RATEMSTR,key="TX"&lpad$(str$(a(6)),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
		g(9)=round(usage_gas*rt(1,3),2) ! tax on gas usage
	goto SALES_TAX_XIT ! /r
	BETHANY_TAX: ! r:
		taxcode=extra(12) ! electric
		read #h_ratemst,using FORM_RATEMSTR,key="ET"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey L5600
		g(7)=round(eu1*rt(1,3),2)
		L5600: taxcode=a(6) ! gas
		read #h_ratemst,using FORM_RATEMSTR,key="GT"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
		g(9)=round(g(4)*rt(1,3),2)
	goto SALES_TAX_XIT ! /r
	! 
	SALES_TAX_XIT: ! 
fnend 
def fn_calk_gas
	! if debug_account then pr x$; 'about to go through gas routine' : pause
	! w(4) seems to be the Gas Charge that accumulates as it is calculated
	if env$('client')="Kimberling" then goto GAS_COMPLETED ! don't have gas (used for sewer reduction)
	if service$(4)="GA" and serviceName$(4)<>"Gas" then goto GAS_COMPLETED ! gas used for some other metered service
	if x(11)>0 then w(4)=x(11): goto GAS_COMPLETED ! gas charge from input4172 if serviceName$(4)="GA" and service$(4)<>"Gas" then goto gas_completed  !gas used for some other metered service
	if x(14)>0 then x3=x(14) ! gas usage override
	if b(4)><0 then goto STANDARD_GAS_CHARGE ! 2870
	if a(4)=0 then goto L4820
	read #h_ratemst,using FORM_RATEMSTR,key=service$(4)&lpad$(str$(a(4)),2): mc1,mu1,mat rt nokey STANDARD_GAS_CHARGE
	if extra(10)=0 then usage_gas=x3 else usage_gas=x3*(extra(10)*.001) ! GAS MULTIPLIER   ( this was .0001 for awhile but changed 61908 to .001
	! If env$('client')="Carrizo" Then usage_gas=(INT(((X3*100)+900)/1000)*1000)/1000 !  round to nearest 1000
	if env$('client')="Bethany" then goto L4590 ! mimimum not included in price
	w(4)=mc1*(max(1,extra(16))) ! gas units per meter
	L4590: !
	if usage_gas<=mu1 then goto L4670 else mu2=mu1
	for j=1 to 10
		if rt(j,1)>usage_gas then goto L4670
		if usage_gas<rt(j,2) then w1=usage_gas-mu2 else w1=rt(j,2)-mu2
		w(4)=w(4)+round(w1*rt(j,3),2)
		if rt(j,2)>usage_gas then goto L4670
		mu2=rt(j,2)
	next j
	L4670: w(4)=max(mc1*max(1,extra(16)),w(4))
	goto GAS_COMPLETED
	! ___________________________
	STANDARD_GAS_CHARGE: ! 
	w(4)=b(4)
	GAS_COMPLETED: ! 
	if d1=f then goto L4750
	d(10)=d(9)
	L4750: d(9)=x(2)
	w8=d(11)
	d(11)=x3
	if d1=f then goto L4810
	d(12)=d(12)+d(11)
	goto L4820
	L4810: d(12)=d(12)+d(11)-w8
	L4820: g(4)=w(4)
	if env$('client')="Franklinton" and a(4)=3 and g(4)<20 then g(4)=20
fnend  ! fn_calk_gas
def fn_calk_for_final_bill
	! if debug_account then pr x$ : pause
	 serviceOther=fn_service_other
	if x(15)>0 then extra(17)=x(15) ! FINAL BILL
	if extra(17)=4 then extra(17)=1 ! change from finaled, but bill once more to just finaled.
	if extra(17)=2 then 
		! b(8)  is service 1 (water)    deposit
		! b(9)  is service 2 (sewer)    deposit
		! b(10) is service 3 (electric) deposit
		! b(11) is service 4 (gas)      deposit
		g(serviceOther)=g(serviceOther)-b(8)-b(9)-b(10)-b(11) ! REFUND DEPOSITS (takes out of any service titled  "Other"
		if d1=f then 
			fn_depr(x$,d1) ! recalculation and deposit possibly already refunded
		else
			if b(8)<>0 then let fnDepositChangeLog(x$,b(8),0,d1,trim$(serviceName$(1))(1:15)&' Deposit Refunded')
			if b(9)<>0 then let fnDepositChangeLog(x$,b(9),0,d1,trim$(serviceName$(2))(1:15)&' Deposit Refunded')
			if b(10)<>0 then let fnDepositChangeLog(x$,b(10),0,d1,trim$(serviceName$(3))(1:15)&' Deposit Refunded')
			if b(11)<>0 then let fnDepositChangeLog(x$,b(11),0,d1,trim$(serviceName$(4))(1:15)&' Deposit Refunded')
			b(8)=b(9)=b(10)=b(11)=0
		end if
	end if 
	! if debug_account then pr x$&' has a g('&str$(serviceOther)&') of '&str$(g(serviceOther))&' at the end of fn_calk_for_final_bill' : pause
fnend  ! fn_calk_for_final_bill
def fn_depr(rk$*10,d1) ! deposit refund
	! uses a lot of local variables ie: 
	! check to see if recalculation and deposit already refunded on previous calculation
	! dim da(2)
	! if debug_account then pr rk$&' entered fn_depr'
	if rk$<>"" then 
		dt1=fncd(d1)
		if int(dt1*.0001)<97 then dt1=dt1+20000000 else dt1=dt1+19000000
		read #deposit2,using FORM_DEPOSIT2,key=>rk$,release: rkRead$,olddt1,dp$,odp,ndp nokey DEPR_XIT
		FORM_DEPOSIT2: form pos 1,c 10,n 8,c 32,2*n 10.2 ! ,pd 3
		do while rkRead$=rk$
			! if debug_account then 
			!   pr '  rkRead$=rk$ ('&rk$&')' 
			!   if olddt1=dt1 and pos(dp$,' Deposit Refunded')>0 then 
			!     pr 'it will pass accumulation test and remove '&str$(odp)&' from g('&str$(serviceOther)&')'
			!   else 
			!     pr '    it will not pass and accumulation'
			!     pr '        dt1='&str$(dt1)
			!     pr '    olddt1='&str$(olddt1)
			!     pr '       dp$='&dp$
			!     pr '       odp='&str$(odp)
			!   end if
			!   pause
			! end if
			if olddt1=dt1 and pos(dp$,' Deposit Refunded')>0 then 
				! if debug_account then pr '    olddt1=dt1 ('&str$(dt1)&')'
				! if debug_account then pr 'removing '&str$(odp)&' from g('&str$(serviceOther)&') for '&dp$ : pause
				g(serviceOther)=g(serviceOther)-odp
			end if 
			read #deposit2,using FORM_DEPOSIT2,release: rkRead$,olddt1,dp$,odp,ndp eof DEPR_XIT
		loop 
	end if
	DEPR_XIT: ! 
	! if debug_account then pr 'at the end of fn_depr '&rk$&' has a g('&str$(serviceOther)&') of '&str$(g(serviceOther)) : pause 
fnend 
def fn_calk_demand
	if env$('client')="Bethany" then read #h_ratemst,using FORM_RATEMSTR,key="DM"&lpad$(str$(extra(11)),2): mc1,mu1,mat rt nokey L6390 : goto L6360
	!  Read #RATEMST,Using 540,Key="DM"&LPAD$(STR$(B(2)),2): MC1,MU1,MAT RT Nokey 6070  ! don't have a demand code any where in record.  wlll have to customize for each client  on Bethany we used service 6 to hold demand
	L6360: if env$('client')="Bethany" then g(6)=mc1: goto L6380
	if env$('client')="Lovington" then goto L6380
	g(6)=round(x(4)*d(14)*.001*rt(1,3),2)
	L6380: d(15)=x(4)
	L6390: ! 
fnend  ! fn_calk_demand
def fn_calk_purcahsed_gas_cost_adj(btu,usage_gas)
	fn_calk_purcahsed_gas_cost_adj=round((btu*.1)*max(0,usage_gas),2)
fnend 
def fn_interest_credit(interest_credit_rate) ! INTEREST CREDIT
	! requires: d1, c(4)
	! returns: g(7)
	w6=round(-(interest_credit_rate*b(11)),2)
	if c(4)<10100 then goto IC_FINIS
	cd1=date(days(d1,'mmddyy'),'ccyymmdd')
	cd2=date(days(c(4),'mmddyy'),'ccyymmdd')
	cy1=int(cd1*.0001)
	cy2=int(cd2*.0001)
	m1=(cy1-cy2)*12
	if m1>12 then goto IC_FINIS
	m2=int(d1*.0001)-int(c(4)*.0001)
	m1=m1+m2
	w6=round(((w6/12)*m1),2)
	if m1<12 then goto L3140 ! don't allow a credit if less than 12 months
	IC_FINIS: ! 
	g(7)=w6
	if g(7)>0 then g(7)=0
	L3140: ! 
fnend 
def library fnservice_other
	if ~setup_calk then let fn_setup_calk
	fnservice_other=fn_service_other
fnend 
def fn_service_other
	if ~service_other_return then 
		for service_other_servicename_item=1 to udim(mat serviceName$)
			if trim$(serviceName$(service_other_servicename_item))(1:5)="Other" then 
				service_other_return=service_other_servicename_item
				goto SERVICE_OTHER_XIT
			end if 
		next service_other_servicename_item
		if ~service_other_return then service_other_return=8 ! default to service 8
	end if 
	SERVICE_OTHER_XIT: ! 
	fn_service_other=service_other_return
fnend 
