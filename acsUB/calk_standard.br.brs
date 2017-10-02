08000 def fn_setup_calk
08020   if ~setup_calk then ! 
08040     let setup_calk=1
08060     library 'S:\Core\Library': fnpause,fncd,fngethandle,fnget_services,fncreg_read
08062     library 'S:\Core\Library': fnDepositChangeLog
08080     dim x$*10,gb(10),dp$*60,servicename$(10)*20,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
08140     FORM_RATEMSTR: form pos 55,32*g 10
08160     fnget_services(mat servicename$, mat service$, mat tax_code$,mat penalty$,mat subjectto)
08180     for j=1 to udim(servicename$)
08200       let servicename$(j)=trim$(servicename$(j))
08220     next j
08240   end if 
08260   if env$('client')='Campbell' then
08280       let fncreg_read('ubcalk-sewer_cap_date',sewer_cap_date$)
08300       sewer_cap_date=val(sewer_cap_date$)
08320   end if
08340   dim onlyMonth(10)
08360   for service_item=1 to 10
08380     let fncreg_read('Service '&str$(service_item)&' only month',tmp$) : onlyMonth(service_item)=val(tmp$)
08400   next service_item
09990 fnend 
09992 Ignore: continue
10010 def library fncalk(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,deposit2,btu; calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
10011   let debug_account=0
10013   ! if trim$(x$)='405051.00' and env$('acsdeveloper')<>'' then let debug_account=1 ! pause
10015   ! if trim$(x$)='300485.00' then pause
10018   ! 
10020   if ~setup_calk then let fn_setup_calk
12000   ! when creating rate routines and assigning service codes,
12020   ! water       service 1 with code "WA"
12040   ! Sewer       service 2 with code "SW"
12060   ! Electric    service 3 with code "EL"
12080   ! Lawn Meters service 3 with code "LM" and name "Lawn Meter"
12100   ! Gas         service 4 with code "GA"
12120   ! Gas Purchase Adjustment (if in it's own field)
12140   !             service 5 with code "GP"
12160   ! Penalty can be anywhere with any code;
12180   ! Sales Tax can be anywhere, but requires code "TX"
12190   ! Inspection Fee can be anywhere, but requires code "IF" and name "Inspection Fee" and must pass variable charge_inspection_fee when wanted to be calculated
13000   ! Interest on Deposit
13020   !             service 5+, no code necessary, name "Interest on Deposit" required and must pass variable calc_interest_on_deposit when wanted to be calculated
13040   ! you can use either gas or electric for some other meter reading by giving it the correct service code name, but using EL or GA as the codes.
13060   ! don't ever change an existing routine for any service.  Add a new routine  for any service that cannot go thru an existing routine.  This will allow the calculation program to become standard.
14000   ! 
14500   ! (it is set in company configure)    if service$(6)='IF' and env$('client')="French Settlement" and extra(11)=0 then let extra(11)=1 ! default all Inspection Fee codes of 0 to a 1
14520   !   if service$(6)='SF' and env$('client')="Pennington" and extra(11)=0 then let extra(11)=1 ! default all Inspection Fee codes of 0 to a 1
14900   ! 
30000   let first_non_metered_service=5
30020   if fn_PassesOnlyMonthFilter(1) and servicename$(1)<>"" and service$(1)="WA" then let fn_calk_water ! always use WA as water code in rate file
30040   if fn_PassesOnlyMonthFilter(3) and servicename$(3)="Lawn Meter" and service$(3)="LM" then let fn_calk_lawnmeter ! must always use LM for the rate code for lawn meters
30060   if fn_PassesOnlyMonthFilter(3) and servicename$(3)(1:5)="Reduc" then let fn_calk_reduc
30080   if fn_PassesOnlyMonthFilter(2) and servicename$(2)<>"" and service$(2)="SW" then let fn_calk_sewer ! always use SW for sewer code in rate file
30100   if fn_PassesOnlyMonthFilter(3) and servicename$(3)="Electric" and service$(3)="EL" then let fn_calk_electric ! must always use EL for the rate code for electric
30120   if fn_PassesOnlyMonthFilter(3) and servicename$(3)<>"Electric" and trim$(servicename$(3))<>"" and trim$(service$(3))="EL" then let fn_calk_electric !  allow electric go thru usage calculation if beign as another type of meter other that electric
30140   if fn_PassesOnlyMonthFilter(3) and servicename$(3)<>"Electric" and trim$(servicename$(3))<>"" and trim$(service$(3))<>"" then 
30160     let j=3
30180     let g(3)=fn_calk_non_metered(j) ! go thru non-metered if using electric for something else 
30200   end if 
30220   ! If SERVICENAME$(3)<>"" AND SERVICE$(3)="AD" Then let fn_calk_administrative_fee ! electric service used of administrative fee
30240   if fn_PassesOnlyMonthFilter(4) and trim$(servicename$(4))="Gas" and trim$(service$(4))="GA" then let fn_calk_gas
30260   if fn_PassesOnlyMonthFilter(4) and servicename$(4)<>"Gas" and trim$(servicename$(4))<>"" and trim$(service$(4))="GA" then let fn_calk_gas !  allow gas go thru usage calculation if being usd as another type of meter other that gas
30280   if fn_PassesOnlyMonthFilter(4) and servicename$(4)<>"Gas" and trim$(servicename$(4))<>"" and trim$(service$(4))<>"" then 
30300     let j=4
30320     let g(4)=fn_calk_non_metered(j) ! go thru non-metered if using gas for something else 
30340   end if 
30360   if fn_PassesOnlyMonthFilter(5) and btu and trim$(service$(5))="GP" then 
30380     let g(5)=fn_calk_purcahsed_gas_cost_adj(btu,usage_gas)
30400     let first_non_metered_service=6
30420   end if 
32000   let fn_calk_demand
32010   for j=first_non_metered_service to 10
32012     if fn_PassesOnlyMonthFilter(j) then 
32020       if trim$(servicename$(j))="Interest on Deposit" and calc_interest_on_deposit then 
32030         let fn_interest_credit(interest_credit_rate)
32040       else if penalty$(j)<>"Y" and service$(j)<>"TX" and trim$(servicename$(j))<>"" then ! skip penalty, sales tax and unused services
32050         if env$('client')="Kimberling" and int(d1*.0001)><2 and (j=5 or j=6) then goto LX1110 ! CALCULATE fees EACH FEB 1
32060         if trim$(servicename$(j))="Inspection Fee" and ~charge_inspection_fee then goto LX1110 ! French Settlement Gas only ask that question, but it should only be calculated when selected
32070         if j=6 and env$('client')='Lovington' then goto SKIP_THIS_NON_METERED_SERVICE
32080         let g(j)=fn_calk_non_metered(j)
32100         SKIP_THIS_NON_METERED_SERVICE: ! 
32110       end if 
32140       LX1110: ! 
32150     end if
32160   next j
32180   let fn_calk_for_final_bill
32200   let fn_calk_sales_tax
32220   let fn_calk_penalty
32240   let fn_calk_net ! NET AND GROSS BILL
32260 fnend 
33000 def fn_PassesOnlyMonthFilter(pomfServiceCode)
33020   if onlyMonth(pomfServiceCode)<=0 then
33040     pomfReturn=1
33060   else if onlyMonth(pomfServiceCode)=date(days(d1,'mmddyy'),'mm') then 
33080     pomfReturn=1
33100   else
33120     pomfReturn=0
33140   end if
33160   fn_PassesOnlyMonthFilter=pomfReturn
33180 fnend
34000 def fn_calk_water
34020   if x(9)=0 then goto L2670
34040   let w(1)=x(9)
34060   if x(12)=0 then goto WATER_COMPLETED
34080   let usage_water=x(12)
34100   goto WATER_COMPLETED
34120   L2670: if x(12)=0 then goto L2690
34140   let usage_water=x(12)
34160   L2690: if usage_water>=0 then goto L2720
34180   goto STANDARD_WATER_CHARGE
34200   ! ___________________________
34220   L2720: if b(1)><0 then goto STANDARD_WATER_CHARGE ! 2140 ! CALCULATION
34240   ! WATER
34260   if a(1)=0 and a(2)=0 then goto WATER_END
34280   read #h_ratemst,using FORM_RATEMSTR, key="WA"&lpad$(str$(a(1)),2): mc1,mu1,mat rt nokey STANDARD_WATER_CHARGE
34300   !   if env$('client')="Riverside" and usage_water>0 then goto LX2770 ! minimum not in total
34320   let w(1)=mc1*max(1,d(13))
34340   ! LX2770: ! if env$('client')="Albany" and (a(1)=3 or a(1)=6) then let usage_water=usage_water/2
34360   if usage_water<=mu1*max(1,d(13)) then goto WATER_COMPLETED else let mu2=mu1*max(1,d(13))
34380   for j=1 to 10
34400     if rt(j,1)>usage_water then goto WATER_COMPLETED
34420     if usage_water<rt(j,2) then let w1=usage_water-mu2 else let w1=rt(j,2)-mu2
34440     let w(1)=w(1)+round(w1*rt(j,3),2)
34460     if rt(j,2)>usage_water then goto WATER_COMPLETED
34480     let mu2=rt(j,2)
34500   next j
34520   goto WATER_COMPLETED
34540   ! ______________________________________________________________________
34560   goto WATER_COMPLETED
34580   STANDARD_WATER_CHARGE: let w(1)=b(1)
34600   WATER_COMPLETED: ! 
34620   !   if env$('client')="Riverside" and w(1)<mc1 then let w(1)=mc1
34640   !   if env$('client')="Albany" and (a(1)=3 or a(1)=6) then let usage_water=usage_water*2 ! correct usage after using 1/2 of it
34660   if env$('client')="Brier Lake" and usage_water>mu1 then let w(1)=w(1)+2
34680   if d1<>f then let d(2)=d(1)
34700   let d(1)=x(1)
34720   let w8=d(3)
34740   let d(3)=usage_water
34760   let d(4)=d(4)+d(3)
34780   if d1=f then let d(4)=d(4)-w8
34800   WATER_END: let g(1)=w(1)
34820 fnend  ! fn_calk_water
36000 def fn_calk_sewer
36020   ! if trim$(x$)='300290.02' then pause
36040   if env$('client')='Lovington' and a(2)>0 then let g(6)=5 ! storm sewer
36060   if x(5)><0 then let w(2)=x(5) : goto SEWER_COMPLETED
36080   if b(2)><0 then goto STANDARD_SEWER_CHARGE ! was disabled but re-enabled on 12/5/16 - standard charge should work on sewer.  clients like Pennington need it.
36100   !   if env$('client')="Ashland" and a(2)=1 and g(1)<>0 then let w(2)=round(g(1)*3/4,2) : goto SEWER_COMPLETED
36120   read #h_ratemst,using FORM_RATEMSTR,key="SW"&lpad$(str$(a(2)),2): mc1,mu1,mat rt nokey STANDARD_SEWER_CHARGE
36140   if env$('client')="Ash Grove" and a(2)=2 then ! do not average commercial sewer
36150     let usage_sewer=usage_water 
36160   else if extra(18)>0 and env$('client')<>"White Hall" and env$('client')<>"Findlay" then ! most people do not average sewer usage over a number of months and then use that average for a number of months
36180     let usage_sewer=extra(18) ! average sewer usage   as calculated by the "Calculate Sewer Average" program.
36200   else 
36220     let usage_sewer=usage_water
36240   end if 
36300   if extra(5)>0 then let usage_sewer=usage_sewer-extra(5) ! sewer reduction
36320   if env$('client')="Ash Grove" then let usage_sewer=int((usage_sewer+50)*.01)*100 ! ROUND TO NEAREST 100 ON SEWER
36340   if servicename$(3)="Lawn Meter" then let usage_sewer=usage_sewer-x2 ! reduce sewer usage by lawn meter usage
36360   if servicename$(3)(1:5)="Reduc" and service$(3)="SW" then let usage_sewer=usage_sewer-x2 ! reduce sewer usage by Reduce Sewer usage
36380   if env$('client')="Kimberling" then let usage_sewer=usage_sewer-x2-x3 : let eu1=x2
36400   let w(2)=mc1*max(1,extra(14)) ! units per meter - sewer (default to one)
36420   if usage_sewer<=mu1 then goto L3300 else let mu2=mu1
36440   for j=1 to 10
36460     if rt(j,1)>usage_sewer then goto L3300
36480     if usage_sewer<rt(j,2) then let w1=usage_sewer-mu2 else let w1=rt(j,2)-mu2
36500     let w(2)=w(2)+round(w1*rt(j,3),2)
36520     if rt(j,2)>usage_sewer then goto L3300
36540     let mu2=rt(j,2)
36560   next j
36580   L3300: ! 
36600   goto SEWER_COMPLETED
36620   ! ______________________________________________________________________
36640   STANDARD_SEWER_CHARGE: ! 
36660   let w(2)=b(2)
36680   SEWER_COMPLETED: ! 
36700   let g(2)=w(2)
36720   if sewer_cap_date>0 and a(2)<=5 then ! if env$('client')='Campbell' and sewer_cap_date>0 then
36730     ! only subject to cap if rate code is a 5 or lower
36740     sewer_cap_amount=fn_service_chg_from_history(2,sewer_cap_date,x$)
36760     if sewer_cap_amount>0 and sewer_cap_amount<g(2) then
36780       g(2)=sewer_cap_amount
36800     end if
36820     !   pause ! acct 800370.16 43016 sewer charge was 19.06 water usage was 2040
36840   end if
36842   ! pause
36860 fnend  ! fn_calk_sewer
37000 def fn_service_chg_from_history(service_number,history_date,scfh_account$)
37020   if ~scfh_setup then
37040     scfh_setup=1
37060     open #scfh_h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
37080   end if
37100   scfh_return=0
37120   dim scfh_key$*19,scfh_alloc_amt(10)
37140   scfh_key$=scfh_account$&date$(days(sewer_cap_date,'mmddyy'),'ccyymmdd')&'1'
37160   mat scfh_alloc_amt=(0)
37180   read #scfh_h_trans,using 'form pos 24,10*PD 4.2',key=scfh_key$: mat scfh_alloc_amt nokey ignore
37200   fn_service_chg_from_history=scfh_alloc_amt(service_number)
37220 fnend
38000 def fn_calk_non_metered(j) ! all non-metered charges but penalty and tax
38002   !  if trim$(x$)='100150.00' then pause
38010   calk_non_metered_return=0
38020   if j=3 then ! electric fields used for a non         -metered service
38040     let entered_amt=x(10)
38060     let standard_amt=b(j)
38080     let rate_code=a(j)
38100     let service_code$=service$(j)
38120   else if j=4 then ! gas used for non=metered                service
38140     let entered_amt=x(11)
38160     let standard_amt=b(j)
38180     let rate_code=a(j)
38200     let service_code$=service$(j)
38220   else if j=5 then 
38240     let entered_amt=x(6)
38260     let standard_amt=b(j)
38280     let rate_code=a(j)
38300     let service_code$=service$(j)
38320   else if j=6 then 
38340     let entered_amt=x(7)
38360     let standard_amt=b(j)
38380     let rate_code=extra(11)
38400     let service_code$=service$(j)
38420   else if j=7 then 
38440     let entered_amt=0
38460     let standard_amt=0
38480     let rate_code=extra(12)
38500     let service_code$=service$(j)
38520   else if j=8 then 
38540     let entered_amt=x(8)
38560     let standard_amt=b(7)
38580     let rate_code=extra(13)
38600     let service_code$=service$(j)
38620   else if j=9 then 
38640     let entered_amt=0
38660     let standard_amt=0
38680     let rate_code=a(6)
38700     let service_code$=service$(j)
38720   else if j=10 then 
38740     let entered_amt=0
38760     let standard_amt=0
38780     let rate_code=a(7)
38800     let service_code$=service$(j)
38820   else 
38840     let g(j)=0
38860     goto NM_XIT ! service not covered
38880   end if 
38900   ! NM_FINIS: !
38920   if entered_amt>0 then 
38940     calk_non_metered_return=entered_amt
38960   else if standard_amt>0 then 
38980     calk_non_metered_return=standard_amt
39000   else 
39020     read #h_ratemst,using FORM_RATEMSTR,key=service_code$&lpad$(str$(rate_code),2): mc1,mu1,mat rt nokey NM_XIT
39040     calk_non_metered_return=max(mc1,rt(1,3)) ! let g(j)=max(mc1,rt(1,3))
39060     ! if j=6 and env$('client')="Eldorado" then calk_non_metered_return=g(6)*g(5)
39080     ! if env$('client')="Sangamon" and cno=1 and service_code$="SF" then calk_non_metered_return=mc1+max(0,(usage_sewer-mu1))*rt(1,3) ! special for Sangamon to handle rates in sewer facilities rate file
39100     ! if env$('client')="Sangamon" and cno=2 and service_code$="SF" then calk_non_metered_return=mc1+max(0,(usage_water-mu1))*rt(1,3) ! special for Sangamon to handle rates in sewer facilities rate file
39120     ! if env$('client')="Sangamon" and service_code$="WF" then calk_non_metered_return=mc1+max(0,(usage_water-mu1))*rt(1,3) ! special for Sangamon to handle rates in water facilities rate file
39140     ! if env$('client')="Carrizo" and j=7 then gosub CARRIZO_TRASH_TAX
39150     if env$('client')="Pennington" and service_code$='SF' then gosub PENNINGTON_SERVICE_FEE
39160   end if 
39180   NM_XIT: ! 
39200   let fn_calk_non_metered=calk_non_metered_return
39220 fnend  ! fn_calk_non_metered(j)
39300 ! CARRIZO_TRASH_TAX: ! r:  called from fn_calk_non_metered
39310 !   if extra(12)=0 then 
39320 !     calk_non_metered_return=0
39330 !     goto CARRIZO_TRASH_TAX_XIT
39340 !   else 
39350 !     read #h_ratemst,using FORM_RATEMSTR,key="TT"&lpad$(str$(extra(12)),2): mc1,mu1,mat rt nokey CARRIZO_TRASH_TAX_XIT
39360 !     calk_non_metered_return=round((g(3)+g(5)+g(6))*rt(1,3),2) ! calculate on canister pickup and trash and canister rental
39370 !   end if 
39380 !   CARRIZO_TRASH_TAX_XIT: ! 
39390 ! return  ! /r CARRIZO_TRASH_TAX
39600 PENNINGTON_SERVICE_FEE: ! r: flat percentage on water and sewer
39620   calk_non_metered_return=round((g(1)+g(2))*rt(1,3),2)
39660 return ! /r
40000 def fn_calk_reduc
40020   if service$(3)="SW" then 
40040     let x2=x(13)
40060     let d(7)=x(13)
40080   end if 
40100 fnend  ! fn_calk_reduc
42000 def fn_calk_lawnmeter
42020   if x(10)=0 then goto L3440
42040   let w(3)=x(10)
42060   if x(13)=0 then goto LAWNMETER_COMPLETED
42080   let x2=x(13)
42100   goto LAWNMETER_COMPLETED
42120   ! ___________________________
42140   L3440: ! 
42160   if x(13)=0 then goto LX3460
42180   let x2=x(13)
42200   LX3460: ! 
42220   if x2>=0 then goto LX3500
42240   ! 
42260   goto LAWNMETER_COMPLETED
42280   ! ___________________________
42300   LX3500: ! 
42320   if b(3)><0 then goto STANDARD_LAWNMETER_CHARGE
42340   read #h_ratemst,using FORM_RATEMSTR,key="LM"&lpad$(str$(a(3)),2): mc1,mu1,mat rt nokey STANDARD_LAWNMETER_CHARGE
42360   ! wrong If env$('client')="Findlay" Then Let X2=X(3) ! findlay actually turns in a usage instead of a reading
42380   let lmu1=x2
42400   let w(3)=mc1*(max(1,extra(15))) !  units per meter
42420   if lmu1<=mu1 then goto L3640 else let mu2=mu1
42440   for j=1 to 10
42460     if rt(j,1)>lmu1 then goto L3640
42480     if lmu1<rt(j,2) then let w1=lmu1-mu2 else let w1=rt(j,2)-mu2
42500     ! 
42520     let w(3)=w(3)+round(w1*rt(j,3),2)
42540     if rt(j,2)>lmu1 then goto L3640
42560     let mu2=rt(j,2)
42580   next j
42600   L3640: ! 
42620   let w(3)=max(mc1,w(3))
42640   goto LAWNMETER_COMPLETED
42660   ! ___________________________
42680   STANDARD_LAWNMETER_CHARGE: ! 
42700   let w(3)=b(3)
42720   LAWNMETER_COMPLETED: ! 
42740   if env$('client')="Findlay" then let x2=x(3) ! findlay actually turns in a usage instead of a reading
42760   if d1<>f then 
42780     let d(6)=d(5)
42800   end if 
42820   let d(5)=x(3)
42840   let w8=d(7)
42860   let d(7)=x2
42880   if d1=f then 
42900     let d(8)=d(8)+d(7)-w8
42920   else 
42940     let d(8)=d(8)+d(7)
42960   end if 
42980   let g(3)=w(3)
43000 fnend  ! fn_calk_lawnmeter
44000 def fn_calk_electric
44010   ! if trim$(x$)='300485.00' then pause
44020   if env$('client')="Kimberling" then goto ELECTRIC_COMPLETED ! don't have electric
44040   if service$(3)="EL" and servicename$(3)<>"Electric" then goto ELECTRIC_COMPLETED ! electric used for some other metered service
44060   if x(10)<>0 then 
44080     let w(3)=x(10)
44100     if x(13)=0 then goto ELECTRIC_COMPLETED
44120     let x2=x(13)
44140     goto ELECTRIC_COMPLETED
44180   end if 
44200   if x(13)<>0 then 
44220     let x2=x(13)
44240   end if 
44260   if x2<0 then 
44280     goto ELECTRIC_COMPLETED
44320   end if 
44330   if b(3)><0 then goto STANDARD_ELEC_CHARGE ! 2580
44340   if a(3)=0 then goto L4290 ! if rate code is a zero than goto L4290
44360   read #h_ratemst,using FORM_RATEMSTR,key="EL"&lpad$(str$(a(3)),2): mc1,mu1,mat rt nokey STANDARD_ELEC_CHARGE
44380   if extra(8)=0 then let eu1=x2 else let eu1=x2*(extra(8)*.001) ! electric multiplier
44400   if env$('client')="Bethany" and extra(15)=0 then let extra(15)=1
44420   if env$('client')="Bethany" then goto L4050 ! minimum not used in calculation
44440   let w(3)=mc1*(max(1,extra(15))) ! electric units per meter
44460   L4050: ! 
44480   if eu1<=mu1 then goto L4130 else let mu2=mu1
44500   for j=1 to 10
44520     if rt(j,1)>eu1 then goto L4130
44540     if eu1<rt(j,2) then let w1=eu1-mu2 else let w1=rt(j,2)-mu2
44560     let w(3)=w(3)+round(w1*rt(j,3),2)
44580     if rt(j,2)>eu1 then goto L4130
44600     let mu2=rt(j,2)
44620   next j
44640   L4130: ! 
44660   let w(3)=max(mc1*extra(15),w(3))
44680   goto ELECTRIC_COMPLETED
44700   ! ___________________________
44720   STANDARD_ELEC_CHARGE: ! 
44740   let w(3)=b(3)
44760   ELECTRIC_COMPLETED: ! 
44780   if d1<>f then 
44800     let d(6)=d(5)
44820   end if 
44822   let d(5)=x(3)
44840   let w8=d(7)
44860   let d(7)=eu1 ! X2  kj 72109
44880   if d1<>f then 
44900     let d(8)=d(8)+d(7)
44930   else 
44940     let d(8)=d(8)+d(7)-w8
44960   end if 
44980   L4290: ! 
44990   let g(3)=w(3)
45000 fnend  ! fn_calk_electric
46000 ! r: def fn_calk_administrative_fee !  (used on Divernon)
46020 !   if x(10) then 
46040 !     let w(3)=x(10)
46060 !   else
46080 !     if b(3)><0 then goto STANDARD_ADM_CHARGE
46100 !     read #h_ratemst,using FORM_RATEMSTR,key="AD"&lpad$(str$(a(3)),2): mc1,mu1,mat rt nokey STANDARD_ADM_CHARGE
46120 !     let w(3)=mc1
46140 !   end if
46160 !   goto ADMIN_COMPLETED
46180 !   STANDARD_ADM_CHARGE: ! 
46200 !     let w(3)=b(3)
46220 !   goto ADMIN_COMPLETED
46240 !   ADMIN_COMPLETED: ! 
46260 !   let g(3)=w(3)
46280 ! /r fnend  
48000 def fn_calk_penalty ! penalty calculation
48020   !   if env$('client')="Divernon" then goto DIVERNON ! Divernon has a unique penalty routine
48040   if env$('client')="Pennington" and a(7)=0 then a(7)=1 ! default all penalty codes of 0 to a 1
48060   if env$('client')="Granby" and a(7)=0 then a(7)=1 ! default all penalty codes of 0 to a 1
48080   if env$('client')="Brier Lake" and a(7)=0 then a(7)=1 ! default all penalty codes of 0 to a 1
48100   mat basepenalty=(0)
48120   for j=1 to 10
48140     if subjectto(j)>0 then  ! accumulate all charges by the penalty they are subject to
48150       basepenalty(subjectto(j))=basepenalty(subjectto(j))+g(j)
48160        !     else if env$('client')="Cerro Gordo" and subjectto(j)>0 then 
48170        !       basepenalty(subjectto(j))=basepenalty(subjectto(j))+gb(j) ! Cerro Gordo bases penalties on balance
48172     end if
48180   next j
48200   for j=1 to 10
48220     if uprc$(penalty$(j))="Y" then let penaltycode$=uprc$(service$(j)) else goto CP_NEXT_J
48240     if j<6 then let pencode=a(j) ! rate codes in customer layout are not in           order.  The first 5 a( match the services. The next three services are          pulled from mat extra. 9 and 10 use a(6)&a(7)
48260     if j=6 then let pencode=extra(11)
48280     if j=7 then let pencode=extra(12)
48300     if j=8 then let pencode=extra(13)
48320     if j=9 then let pencode=a(6)
48340     if j=10 then let pencode=a(7)
48360     ! If PENCODE=0 OR PENCODE>99 Then Let PENCODE=1 ! default to one so codes don't have to be added to old customer records
48380     read #h_ratemst,using FORM_RATEMSTR,key=penaltycode$&lpad$(str$(pencode),2): mc1,mu1,mat rt nokey CP_NEXT_J
48385     if mc1>0 and env$('client')<>"Millry" then ! penalty is a fixed amount
48390       let g(j)=mc1
48395       goto CP_NEXT_J
48400     else if env$('client')="Franklinton" and j=10 then 
48405       let g(10)=round((g(1)+g(2)+g(3)+g(5)+g(8))*.1+(rt(1,3)*x3),2)
48410       goto CP_NEXT_J
48415     else if env$('client')="Colyell" and f=d1 then 
48420       basepenalty(10)=bal+sum(mat g(1:9))
48425     else if env$('client')="Colyell" and f<>d1 then 
48430       basepenalty(10)=bal+sum(mat g(1:9))
48435     end if 
48440     if env$('client')="White Hall" and f=d1 then 
48445       basepenalty(10)=g(1)+g(2)+g(4)+g(9)
48450     else if env$('client')="White Hall" and f<>d1 then 
48455       basepenalty(10)=g(1)+g(2)+g(4)+g(9)
48460     end if 
48465     ! if env$('client')="Sangamon" and d1=f then 
48470     !   basepenalty(10)=basepenalty(10)+bal
48475     ! end if 
48480     ! if env$('client')="Sangamon" and d1<>f then 
48485     !   basepenalty(10)=basepenalty(10)+bal
48490     ! end if 
48520     !    if env$('client')="Cerro Gordo" and d1=f then basepenalty(10)=basepenalty(10)+bal
48540     !    if env$('client')="Cerro Gordo" and d1<>f then basepenalty(10)=basepenalty(10)+bal
48560     if env$('client')="Brier Lake" and d1=f then basepenalty(10)=basepenalty(10)+bal-gb(10)
48580     if env$('client')="Brier Lake" and d1<>f then basepenalty(10)=basepenalty(10)+bal-gb(10)
48600     if env$('client')="Granby" and d1=f then basepenalty(10)=basepenalty(10)+bal-gb(10)
48620     if env$('client')="Granby" and d1<>f then basepenalty(10)=basepenalty(10)+bal-gb(10)
48640     if env$('client')="Kimberling" and g(2)>0 then basepenalty(10)=basepenalty(10)-g(1) ! no penalty on water if they have sewer
48660     let g(j)=round(basepenalty(j)*rt(1,3),2) ! penalty based on base amount that was accumulated for each penalty field * rate for that penalty code
48680     if env$('client')="Millry" and g(j)<5 then let g(j)=5
48700     ! if env$('client')="Sangamon" and cno=1 and g(10)<.20 then let g(10)=0
48720     ! if env$('client')="Sangamon" and cno=2 and g(10)<.10 then let g(10)=0
48740     CP_NEXT_J: ! 
48750   next j
48760   ! If env$('client')="Pennington" Then Let G(10)=ROUND(G(10)*(1+HOLDTAXRATE),2)
48780   ! Let G(7)=ROUND(G(7)*(1+HOLDTAXRATE),2) ! charge sales tax on penalty
48800   !   if env$('client')="Riverside" then 
48810   !     let g(10)=round(rt(1,3)*min(mc1,sum(mat g(1:8))),2) 
48814   !     let g(10)=g(10)+rt(2,3)*round(max(0,sum(mat g(1:8))-mc1),2)
48816   !   end if
48820   if env$('client')="Kimberling" then let fn_penalty_kimberling ! calculate interest on prev balance
48840   let g(10)=max(0,g(10))
48860   goto CALK_PENCAL_XIT
48880   ! DIVERNON: ! r: unique penalty calculation  (5% of balance owed on water,sewer                  and gas before new bill calculated.  These are actual charges                   and get added into the balance, balance breakdown, net, gross
48900   !     if gb(1)>0 and a(5)>0 then let g(5)=round(gb(1)*.05,2) : let gb(5)+=g(5) : let g(11)+=g(5) : let g(12)+=g(5)
48920   !     if gb(2)>0 and extra(11)>0 then let g(6)=round(gb(2)*.05,2) : let gb(6)+=g(6) : let g(11)+=g(6) : let g(12)+=g(6)
48940   !     if gb(4)>0 and extra(12)>0 then let g(7)=round(gb(4)*.05,2) : let gb(7)+=g(7) : let g(11)+=g(7) : let g(12)+=g(7)
48960   !     goto CALK_PENCAL_XIT ! /r
48980   CALK_PENCAL_XIT: ! 
49000 fnend 
49020 def fn_penalty_kimberling ! add .75% of previous balance (9% annual)
49040   let g(10)=max(0,round(g(10)+max(0,bal)*.0075,2))
49060 fnend 
50000 def fn_calk_net ! CALCULATE NET AND GROSS BILL
50020   for j=1 to 10
50040     !     if uprc$(penalty$(j))="Y" and env$('client')="Divernon" then goto L6020 ! don't add penalties into net nor gross
50060     if uprc$(penalty$(j))<>"Y" then ! add penalties into net
50080       let g(11)=g(11)+g(j)
50100     end if 
50120     let g(12)=g(12)+g(j)
50140     ! L6020: ! 
50160   next j
50180    !   if (penalty$(5) ="Y" or penalty$(6) ="Y" or penalty$(7) ="Y") and env$('client')="Divernon" then let g(12)=g(12)+5 ! divernon also has a 5.00 penalty that can get added in middle of month.
50200 fnend  ! fn_calk_net
52000 def fn_calk_sales_tax
52020   if env$('client')="Bethany" then goto BETHANY_TAX
52040   if env$('client')="Franklinton" then goto FRANKLINTON_TAX
52060   if env$('client')="White Hall" then goto WHITEHALL_TAX
52080   ! r: normal tax
52100   for j=1 to 10 ! determine which service is tax   (rate code abbreviation must always be TX
52120     if service$(j)="TX" then let taxservice=j
52140   next j
52160   if taxservice=0 then 
52180     let taxcode=0
52200   else if taxservice<6 then ! note - No one has a TX code in anything except 9 or 10
52220     pr ' faulty logic here - call ACS' : pause : let taxcode=a(j)
52240   else if taxservice=6 then 
52260     let taxcode=extra(11)
52280   else if taxservice=7 then 
52300     let taxcode=extra(12)
52320   else if taxservice=8 then 
52340     let taxcode=extra(13)
52360   else if taxservice=9 then 
52380     let taxcode=a(6)
52400   else if taxservice=10 then 
52420     let taxcode=a(7)
52440   end if 
52460   ! L5300: ! 
52470   ! if debug_account then pr x$; 'taxcode=';taxcode;'    taxservice=';taxservice : pause
52480   let taxable=0
52500   if env$('client')="Pennington" and taxcode=0 then let taxcode=1 ! default to tax code 1 on Pennington
52520   read #h_ratemst,using FORM_RATEMSTR,key="TX"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
52540   !   if env$('client')="Divernon" then ! tax is % of usage
52560   !     let taxable=x3
52580   !   else 
52600     for j=1 to 8
52620       if uprc$(tax_code$(j))="Y" then let taxable=taxable+g(j) ! determine total      taxable sales
52640     next j
52660   !   end if 
52680   if taxservice>0 and taxservice <=10 then let g(taxservice)=round(taxable*rt(1,3),2) ! let holdtaxrate=rt(1,3)
52700   if env$('client')="Edinburg" and btu<>0 then let g(taxservice)=min(g(taxservice),round(x3*btu*.024,2)) ! env$('client')="Edinburg"   !! BUT DEFINATELY  NOT French Settlement
52720   goto SALES_TAX_XIT ! /r SALES_TAX
52740   ! 
52760   FRANKLINTON_TAX: ! r:
52780     let taxcode=extra(12) ! water
52800     read #h_ratemst,using FORM_RATEMSTR,key="TW"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
52820     let g(7)=round(g(1)*rt(1,3),2)
52840     let taxcode=a(6) ! gas
52860     read #h_ratemst,using FORM_RATEMSTR,key="TG"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
52880     let g(9)=round(g(4)*rt(1,3),2)
52900   goto SALES_TAX_XIT ! /r
52920   WHITEHALL_TAX: ! r:
52940     read #h_ratemst,using FORM_RATEMSTR,key="TX"&lpad$(str$(a(6)),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
52960     let g(9)=round(usage_gas*rt(1,3),2) ! tax on gas usage
52980   goto SALES_TAX_XIT ! /r
53000   BETHANY_TAX: ! r:
53020     let taxcode=extra(12) ! electric
53040     read #h_ratemst,using FORM_RATEMSTR,key="ET"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey L5600
53060     let g(7)=round(eu1*rt(1,3),2)
53080     L5600: let taxcode=a(6) ! gas
53100     read #h_ratemst,using FORM_RATEMSTR,key="GT"&lpad$(str$(taxcode),2): mc1,mu1,mat rt nokey SALES_TAX_XIT
53120     let g(9)=round(g(4)*rt(1,3),2)
53140   goto SALES_TAX_XIT ! /r
53160   ! 
53180   SALES_TAX_XIT: ! 
53200 fnend 
54000 def fn_calk_gas
54010   ! if debug_account then pr x$; 'about to go through gas routine' : pause
54020   ! w(4) seems to be the Gas Charge that accumulates as it is calculated
54040   if env$('client')="Kimberling" then goto GAS_COMPLETED ! don't have gas (used for sewer reduction)
54060   if service$(4)="GA" and servicename$(4)<>"Gas" then goto GAS_COMPLETED ! gas used for some other metered service
54080   if x(11)>0 then let w(4)=x(11): goto GAS_COMPLETED ! gas charge from input4172 if servicename$(4)="GA" and service$(4)<>"Gas" then goto gas_completed  !gas used for some other metered service
54100   if x(14)>0 then let x3=x(14) ! gas usage override
54120   if b(4)><0 then goto STANDARD_GAS_CHARGE ! 2870
54140   if a(4)=0 then goto L4820
54160   read #h_ratemst,using FORM_RATEMSTR,key=service$(4)&lpad$(str$(a(4)),2): mc1,mu1,mat rt nokey STANDARD_GAS_CHARGE
54180   if extra(10)=0 then let usage_gas=x3 else let usage_gas=x3*(extra(10)*.001) ! GAS MULTIPLIER   ( this was .0001 for awhile but changed 61908 to .001
54200   ! If env$('client')="Carrizo" Then Let usage_gas=(INT(((X3*100)+900)/1000)*1000)/1000 !  round to nearest 1000
54220   if env$('client')="Bethany" then goto L4590 ! mimimum not included in price
54240   let w(4)=mc1*(max(1,extra(16))) ! gas units per meter
54260   L4590: !
54261   if usage_gas<=mu1 then goto L4670 else let mu2=mu1
54280   for j=1 to 10
54300     if rt(j,1)>usage_gas then goto L4670
54320     if usage_gas<rt(j,2) then let w1=usage_gas-mu2 else let w1=rt(j,2)-mu2
54340     let w(4)=w(4)+round(w1*rt(j,3),2)
54360     if rt(j,2)>usage_gas then goto L4670
54380     let mu2=rt(j,2)
54400   next j
54420   L4670: let w(4)=max(mc1*max(1,extra(16)),w(4))
54440   ! if env$('client')="Eldorado" and w(4)<((rt(1,3)*10)*2) then let w(4)=((rt(1,3)*10)*2)
54460   goto GAS_COMPLETED
54480   ! ___________________________
54500   STANDARD_GAS_CHARGE: ! 
54520   let w(4)=b(4)
54540   GAS_COMPLETED: ! 
54560   if d1=f then goto L4750
54580   let d(10)=d(9)
54600   L4750: let d(9)=x(2)
54620   let w8=d(11)
54640   let d(11)=x3
54660   if d1=f then goto L4810
54680   let d(12)=d(12)+d(11)
54700   goto L4820
54720   L4810: let d(12)=d(12)+d(11)-w8
54740   L4820: let g(4)=w(4)
54760   if env$('client')="Franklinton" and a(4)=3 and g(4)<20 then let g(4)=20
54780 fnend  ! fn_calk_gas
56000 def fn_calk_for_final_bill
56020   ! if debug_account then pr x$ : pause
56030    serviceOther=fn_service_other
56040   if x(15)>0 then let extra(17)=x(15) ! FINAL BILL
56060   if extra(17)=4 then let extra(17)=1 ! change from finaled, but bill once more to just finaled.
56080   if extra(17)=2 then 
56120     ! b(8)  is service 1 (water)    deposit
56140     ! b(9)  is service 2 (sewer)    deposit
56160     ! b(10) is service 3 (electric) deposit
56180     ! b(11) is service 4 (gas)      deposit
56200     let g(serviceOther)=g(serviceOther)-b(8)-b(9)-b(10)-b(11) ! REFUND DEPOSITS (takes out of any service titled  "Other"
56220     if d1=f then 
56240       let fn_depr(x$,d1) ! recalculation and deposit possibly already refunded
56260     else
56280       if b(8)<>0 then let fnDepositChangeLog(x$,b(8),0,d1,trim$(servicename$(1))(1:15)&' Deposit Refunded')
56300       if b(9)<>0 then let fnDepositChangeLog(x$,b(9),0,d1,trim$(servicename$(2))(1:15)&' Deposit Refunded')
56320       if b(10)<>0 then let fnDepositChangeLog(x$,b(10),0,d1,trim$(servicename$(3))(1:15)&' Deposit Refunded')
56340       if b(11)<>0 then let fnDepositChangeLog(x$,b(11),0,d1,trim$(servicename$(4))(1:15)&' Deposit Refunded')
56360       b(8)=b(9)=b(10)=b(11)=0
56380     end if
56400   end if 
56410   ! if debug_account then pr x$&' has a g('&str$(serviceOther)&') of '&str$(g(serviceOther))&' at the end of fn_calk_for_final_bill' : pause
56420 fnend  ! fn_calk_for_final_bill
58000 def fn_depr(rk$*10,d1)
58020   ! uses a lot of local variables ie: 
58040   ! check to see if recalculation and deposit already refunded on previous calculation
58060   ! dim da(2)
58080   ! if debug_account then pr rk$&' entered fn_depr'
58100   if rk$<>"" then 
58120     let dt1=fncd(d1)
58140     if int(dt1*.0001)<97 then let dt1=dt1+20000000 else let dt1=dt1+19000000
58200     read #deposit2,using FORM_DEPOSIT2,key=>rk$,release: rkRead$,olddt1,dp$,odp,ndp nokey DEPR_XIT
58220     FORM_DEPOSIT2: form pos 1,c 10,n 8,c 32,2*n 10.2 ! ,pd 3
58240     do while rkRead$=rk$
58300       ! if debug_account then 
58320       !   pr '  rkRead$=rk$ ('&rk$&')' 
58340       !   if olddt1=dt1 and pos(dp$,' Deposit Refunded')>0 then 
58360       !     pr 'it will pass accumulation test and remove '&str$(odp)&' from g('&str$(serviceOther)&')'
58361       !   else 
58362       !     pr '    it will not pass and accumulation'
58363       !     pr '        dt1='&str$(dt1)
58364       !     pr '    olddt1='&str$(olddt1)
58365       !     pr '       dp$='&dp$
58366       !     pr '       odp='&str$(odp)
58380       !   end if
58400       !   pause
58420       ! end if
58440       if olddt1=dt1 and pos(dp$,' Deposit Refunded')>0 then 
58460         ! if debug_account then pr '    olddt1=dt1 ('&str$(dt1)&')'
58480         ! if debug_account then pr 'removing '&str$(odp)&' from g('&str$(serviceOther)&') for '&dp$ : pause
58500         let g(serviceOther)=g(serviceOther)-odp
58540       end if 
58560       read #deposit2,using FORM_DEPOSIT2,release: rkRead$,olddt1,dp$,odp,ndp eof DEPR_XIT
58580     loop 
58600   end if
58620   DEPR_XIT: ! 
58640   ! if debug_account then pr 'at the end of fn_depr '&rk$&' has a g('&str$(serviceOther)&') of '&str$(g(serviceOther)) : pause 
58660 fnend 
60000 def fn_calk_demand
60020   if env$('client')="Bethany" then read #h_ratemst,using FORM_RATEMSTR,key="DM"&lpad$(str$(extra(11)),2): mc1,mu1,mat rt nokey L6390 : goto L6360
60040   !  Read #RATEMST,Using 540,Key="DM"&LPAD$(STR$(B(2)),2): MC1,MU1,MAT RT Nokey 6070  ! don't have a demand code any where in record.  wlll have to customize for each client  on Bethany we used service 6 to hold demand
60060   L6360: if env$('client')="Bethany" then let g(6)=mc1: goto L6380
60080   if env$('client')="Lovington" then goto L6380
60100   let g(6)=round(x(4)*d(14)*.001*rt(1,3),2)
60120   L6380: let d(15)=x(4)
60180   L6390: ! 
60200 fnend  ! fn_calk_demand
64000 def fn_calk_purcahsed_gas_cost_adj(btu,usage_gas)
64160   let fn_calk_purcahsed_gas_cost_adj=round((btu*.1)*max(0,usage_gas),2)
64190 fnend 
66000 def fn_interest_credit(interest_credit_rate) ! INTEREST CREDIT
66020   ! requires: d1, c(4)
66040   ! returns: g(7)
66060   let w6=round(-(interest_credit_rate*b(11)),2)
66080   if c(4)<10100 then goto IC_FINIS
66100   cd1=date(days(d1,'mmddyy'),'ccyymmdd')
66120   cd2=date(days(c(4),'mmddyy'),'ccyymmdd')
66140   cy1=int(cd1*.0001)
66160   cy2=int(cd2*.0001)
66180   let m1=(cy1-cy2)*12
66200   if m1>12 then goto IC_FINIS
66220   let m2=int(d1*.0001)-int(c(4)*.0001)
66240   let m1=m1+m2
66260   let w6=round(((w6/12)*m1),2)
66280   if m1<12 then goto L3140 ! don't allow a credit if less than 12 months
66300   IC_FINIS: ! 
66320   let g(7)=w6
66340   if g(7)>0 then let g(7)=0
66360   L3140: ! 
66380 fnend 
68000 def library fnservice_other
68020   if ~setup_calk then let fn_setup_calk
68040   let fnservice_other=fn_service_other
68060 fnend 
70000 def fn_service_other
70020   if ~service_other_return then 
70040     for service_other_servicename_item=1 to udim(mat servicename$)
70060       if trim$(servicename$(service_other_servicename_item))(1:5)="Other" then 
70080         let service_other_return=service_other_servicename_item
70100         goto SERVICE_OTHER_XIT
70120       end if 
70140     next service_other_servicename_item
70160     if ~service_other_return then let service_other_return=8 ! default to service 8
70180   end if 
70200   SERVICE_OTHER_XIT: ! 
70220   let fn_service_other=service_other_return
70240 fnend 
