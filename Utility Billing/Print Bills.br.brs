01000 ! Replace S:\acsUB\printbill
01020   fn_setup
01040   fntop(program$)
01060 ! r: Direct clients to the (basic) PrintBill_Basic routine (below) or their custom bill program
01100   dim alternate_printbill_program$*256
01120   alternate_printbill_program$=fnub_printbill_program$
01122   if env$('client')='Findlay' and env$('acsDeveloper')<>'' then alternate_printbill_program$='(basic)' ! fnub_printbill_program$
01140   if alternate_printbill_program$='(basic)' then
01160     goto PrintBill_Basic
01180   else
01200     fnchain(fnub_printbill_program$,1)
01220   end if
01240 ! /r
06000 ! PrintBill_Basic - dynamic pr bill program that works for multiple clients
08000 def fn_setup
08020   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fncombof,fnerror,fnopt,fntos,fncmbact,fnd1,fnxit,fncmdset,fnopenprn,fncloseprn,fncreg_read,fncreg_write,fngethandle,fncustomer_address
08040   library 'S:\Core\Library': fnformnumb$,fntrans_total_as_of,fnget_services
08060   library 'S:\Core\Library': fnpa_open,fnpa_finis,fnpa_barcode,fnpa_newpage,fnpa_txt,fnpa_fontsize,fnpa_fontbold,fnpa_font,fnpa_line,fnpa_fontitalic,fnpa_pic,fnpa_elipse
08070   library 'S:\Core\Library': fntop,fnchain,fnub_printbill_program$
08080   on error goto ERTN
08100 ! ______________________________________________________________________
08110   dim resp$(60)*128
08120   dim mg$(4)*128
08130   dim mg2$(30)*128
08150   dim z$*10
08160   dim e$(4)*30
08162   dim gTmpCustomerAddress$(4)*30
08170   dim f$*12
08180   dim g(12)
08190   dim d(15)
08200   dim b(11)
08210   dim gb(10)
08220   dim pe$(4)*30
08230   dim cnam$*40 ! company name
08240   dim at$(3)*40 ! (1)=company name, (2)=company addr, (3)=company address   ** POPULATED BY: fn_get_mat_at(mat at$)
08250   dim servicename$(10)*20
08260   dim servicecode$(10)*2
08270   dim tax_code$(10)*1
08280   dim penalty$(10)*1
08320 ! ______________________________________________________________________
08330   cno=val(env$('cno'))
08340   cnam$=env$('cnam')
08350   fnget_services(mat servicename$, mat servicecode$, mat tax_code$,mat penalty$) ! ,mat subjectto,mat ordertoapply)
08360 fnend
17000 PrintBill_Basic: !
18000 ! r: set prefrences for clients
18020   if env$('client')='French Settlement' then ! completed 4/23/16
18022     !  margins are top:.5, bottom:.2, left:.3, right:.2
18030     let message1_line_count=1
18040     let message1_max_len=52
18050     let include_zero_bal=include_credit_bal=1
18052     forceWordProcessor$='atlantis'
18054     enable_service_from=1
18056     enable_service_to=1
18060   else if env$('client')='Campbell' then ! completed 4/23/16
18070     let message1_line_count=0 ! 3
18080     let message1_max_len=30
18090     enable_service_from=0
18100     enable_service_to=1
18110     let include_zero_bal=include_credit_bal=1
18120   else if env$('client')='Raymond' then ! completed 4/23/16
18130     let message1_line_count=3
18140     let pa_enabled=1
18150     let pa_orientation$='Landscape'
18160     let include_zero_bal=include_credit_bal=1
18162     enable_service_from=1
18164     enable_service_to=1
18170   else if env$('client')='Cerro Gordo' then 
18180     let message1_line_count=3
18190     let message1_max_len=30
18200     enable_bulksort=1
18210     enable_service_from=1
18220     enable_service_to=1
18230     let include_zero_bal=include_credit_bal=1
18240   else if env$('client')='Merriam Woods' then ! completed 5/5/16
18250     let message1_line_count=1
18260     enable_service_from=1
18270     enable_service_to=1
18280     let pa_enabled=1
18290     let pa_orientation$='Landscape'
18300     enable_bulksort=1
18310     let include_zero_bal=include_credit_bal=1
18320   else if env$('client')='Omaha' then ! 8/10/2016
18330     let message1_line_count=3
18340     let message1_max_len=30
18350     enable_service_from=1
18360     enable_service_to=1
18370     let include_zero_bal=include_credit_bal=1
18380     let message2_line_count=12
18390     let message2_max_len=24
18400   else if env$('client')='Blucksberg' then 
18410     enable_bulksort=1
18420     let message1_line_count=13
18430     let message1_max_len=95
18440     let message_onscreen_alignment=2
18450     enable_service_from=1
18460     enable_service_to=1
18470   else if env$('client')='Pennington' then ! 12/07/2016
18480     message1_line_count=3
18490     message1_max_len=40
18500     enable_service_from=1
18510     enable_service_to=1
18520     include_zero_bal=include_credit_bal=1
18530   else if env$('client')='Edinburg' then ! 12/08/2016
18540     message1_line_count=3
18550     message1_max_len=30
18560     pa_enabled=1
18570     enable_service_from=1
18580     enable_service_to=1
18590     pa_orientation$='Landscape'
18600     include_zero_bal=include_credit_bal=1
18610   else if env$('client')='Choctaw' then 
18620     enable_service_from=1
18630     enable_service_to=1
18640     ! message1_line_count=3
18650     ! message1_max_len=30
18652     enable_bulksort=1
18654     include_zero_bal=include_credit_bal=1
18660     ! pause : pa_enabled=0
18662     forceWordProcessor$='atlantis'
18664   else if env$('client')='Exeter' then 
18666     basePenaltyOnCurrentBillOnly=1
18668     message1_line_count=3
18670     pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
18672     pa_orientation$='Landscape'
18674     include_zero_bal=include_credit_bal=1
18676     message2_line_count=2
18678     message2_max_len=30
18690   !       let message1_line_count=3
18700   !       let pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
18710   !       let pa_orientation$='Landscape'
18720   !       let include_zero_bal=include_credit_bal=1
18730   !       basePenaltyOnCurrentBillOnly=1
18760   else if env$('client')='Billings' then ! 04/26/2017 ! three per page RTF
18770     message1_line_count=3
18780     include_zero_bal=include_credit_bal=1
18790     enable_bulksort=1
18800   else !  default settings:  Findlay, Edison
18810     message1_line_count=3
18820     pa_enabled=1 ! 2 (hopefully one day, but the line lengths do not work right) ! pa_enabled=2 is for ForceFormat=PDF
18830     pa_orientation$='Landscape'
18840     include_zero_bal=include_credit_bal=1
18850     message2_line_count=2
18860     message2_max_len=30
18862     enable_service_from=1
18864     enable_service_to=1
18870     ! usPostagePermitNumber=0
18880   end if 
20000   ! r: use the default settings but add a little extra to it
20020   if env$('client')='Findlay' then 
20021     poundBeforeAccount$='#' ! only for diff - making sure things match - then take it back out. it's lame
20040     usPostagePermitNumber=1
20041     enable_bulksort=1
20042     enableIsDueNowAndPayable=-1
20043     enableReturnServiceRequested=-1
20044   else if env$('client')='Edison' then 
20045     usPostagePermitNumber=1
20046     penaltyFlatAmount=5
20060   end if
20080   ! /r
20100 !   enable_cass_sort=1
20120 ! /r
22000 ! r: post client setup configuration, file opening, etc
22020   mat mg$(message1_line_count) : mat respc_mg1(message1_line_count)
22040   mat mg2$(message2_line_count) : mat respc_mg2(message2_line_count)
22060   fnd1(d1)
22080   fncreg_read('Penalty Due Date',tmp$) : d4=val(tmp$)
22100   if days(d4,'mmddyy')<days(d1,'mmddyy') then 
22120     if d4<>0 then
22180       d4$=''
22200       d4$(inf:inf)=date$(days(d1,'mmddyy'),'mm') ! get MM date from d1
22220       d4$(inf:inf)=date$(days(d4,'mmddyy'),'dd') ! get DD date from d4
22240       d4$(inf:inf)=date$(days(d1,'mmddyy'),'yy') ! get YY date from d1
22260       d4=val(d4$)
22280     end if
22300   end if
22320   if enable_bulksort then gosub BULKSORT
22340   if enable_cass_sort then gosub SORT1
22360   open #h_customer_1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed  ! open in account order
22380   open #h_customer_2:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed  ! open in route-sequence
22400 ! /r
24000 SCREEN1: ! r:
24020   starting_key$="" : let route_filter=0 : let respc=0
24040   fntos(sn$="UBPrtBl1-1")
24060   let pf=27 : lc=0
24080   fnlbl(lc+=1,1,"Penalty Due Date:",25,1)
24100   fntxt(lc,pf,8,8,1,"1",0,tt$)
24120   let resp$(respc_penalty_due_date:=respc+=1)=cnvrt$("pic(zzzzzz)",d4)
24140   fnlbl(lc+=1,1,"Date of Billing:",25,1)
24160   fntxt(lc,pf,8,8,1,"1")
24180   let resp$(respc_billing_date:=respc+=1)=cnvrt$("pic(zzzzzz)",d1)
24200   if enable_service_from or enable_service_to then 
24220     lc+=1
24240     if enable_service_from then 
24260       fnlbl(lc+=1,1,"Service From:",25,1)
24280       fntxt(lc,pf,8,8,1,"1",0,"This field can be used to override the Prior Reading Date in the customer's record")
24290       fnlbl(lc,pf+8+4,"(only use to Prior Reading Dates from individual customer records)")
24300       let resp$(respc_service_from:=respc+=1)=cnvrt$("pic(zzzzzz)",d2)
24320     end if 
24340     if enable_service_to then 
24360       fnlbl(lc+=1,1,"Service To:",25,1)
24380       fntxt(lc,pf,8,8,1,"1",0,"This field can be used to override the Current Reading Date in the customer's record")
24390       fnlbl(lc,pf+8+4,"(only use to Current Reading Dates from individual customer records)")
24400       let resp$(respc_service_to:=respc+=1)=cnvrt$("pic(zzzzzz)",d3)
24420     end if 
24440   end if 
24460   lc+=1
24480   fnlbl(lc+=1,1,"Starting Route/Sequence:",25,1)
24500   fncombof("ubm-act-nam",lc,pf,40,env$('Q')&"\UBmstr\Customer.h"&env$('cno'),1741,9,41,30,env$('Q')&"\UBmstr\ubindx5.h"&env$('cno'),2)
24520   let resp$(respc_start_place:=respc+=1)="[All]"
24540   lc+=1
24560   fnlbl(lc+=1,1,"Route Number:",25,1)
24580   fncmbrt2(lc,pf)
24600   let resp$(respc_route:=respc+=1)="[All]"
24620   lc+=1
24640   fnlbl(lc+=1,1,"Filter:",25,1)
24660   fnopt(lc,pf,"All")
24680   let resp$(respc_filter_none:=respc+=1)="True"
24700   fnopt(lc+=1,pf,"Select Individuals")
24720   let resp$(respc_filter_individuals:=respc+=1)="False"
24740   fnopt(lc+=1,pf,"Past Due Only")
24760   let resp$(respc_filter_past_due:=respc+=1)="False"
24780   fnopt(lc+=1,pf,"All Except Past Due")
24782   let resp$(respc_filter_not_past_due:=respc+=1)="False"
24790 ! 
24800   if message1_line_count then 
24810     lc+=1
24820     if message2_line_count then 
24830       fnlbl(lc+=1,1,"Message 1:",25,1)
24840     else 
24850       fnlbl(lc+=1,1,"Message:",25,1)
24860     end if 
24880   end if 
24900   lc-=1
24910   if message1_line_count>0 and message1_max_len=0 then let message1_max_len=30
24920   for mg1_item=1 to message1_line_count
24940     fntxt(lc+=1,pf,max(30,int(message1_max_len*.75)),message1_max_len,message_onscreen_alignment)
24960     let respc_mg1(mg1_item)=respc+=1
24980     fncreg_read('bill message '&str$(mg1_item),resp$(respc_mg1(mg1_item))) 
25000     let resp$(respc_mg1(mg1_item))=resp$(respc_mg1(mg1_item))(1:message1_max_len)
25020   next mg1_item
25060   if message2_line_count then 
25080     lc+=1
25100     fnlbl(lc+=1,1,"Message 2:",25,1)
25120   end if 
25140   lc-=1
25160   for mg2_item=1 to message2_line_count
25180     fntxt(lc+=1,pf,min(message2_max_len,30),message2_max_len,message_onscreen_alignment)
25200     let respc_mg2(mg2_item)=respc+=1
25220     fncreg_read('bill message2 '&str$(mg2_item),resp$(respc_mg2(mg2_item)))
25240     let resp$(respc_mg2(mg2_item))=resp$(respc_mg2(mg2_item))(1:message2_max_len)
25260   next mg2_item
25280 ! 
25300   fncmdset(3)
25320   fnacs(sn$,0,mat resp$,ck)
25340   if ck=5 then goto XIT
25360   let d1=val(resp$(respc_billing_date))
25380   let d4=val(resp$(respc_penalty_due_date))
25400   if enable_service_from then 
25420     let d2=val(resp$(respc_service_from))
25440   end if 
25460   if enable_service_to then 
25480     let d3=val(resp$(respc_service_to))
25500   end if 
25520   if resp$(respc_start_place)="[All]" then 
25540     starting_key$=""
25560   else 
25580     starting_key$=lpad$(trim$(resp$(6)(1:10)),10)
25600     if trim$(starting_key$)<>"" then 
25620       read #h_customer_1,using 'form pos 1,c 10,pos 1741,n 2,n 7',key=starting_key$,release: z$,route,sequence nokey SCREEN1
25640 !    starting_place_enabled=1
25660     end if 
25680   end if 
25700   if resp$(respc_route)="[All]" then 
25720     let route_filter=0
25740   else 
25760     let route_filter=val(resp$(respc_route))
25780   end if 
25800   if resp$(respc_filter_none)="True" then let filter_none=1 else let filter_none=0
25820   if resp$(respc_filter_individuals)="True" then let filter_selected_only=1 else let filter_selected_only=0
25840   if resp$(respc_filter_past_due)="True" then let filter_past_due_only=1 else let filter_past_due_only=0
25860   if resp$(respc_filter_not_past_due)="True" then let filter_no_past_due=1 else let filter_no_past_due=0
25880   for mg1_item=1 to message1_line_count
25900     let mg$(mg1_item)=resp$(respc_mg1(mg1_item))
25920     fncreg_write('bill message '&str$(mg1_item),mg$(mg1_item))
25940   next mg1_item
25960   for mg2_item=1 to message2_line_count
25980     let mg2$(mg2_item)=resp$(respc_mg2(mg2_item))
26000     fncreg_write('bill message2 '&str$(mg2_item),mg2$(mg2_item))
26020   next mg2_item
26040   fncreg_write('Penalty Due Date',str$(d4))
26080 ! /r
29000 ! r: initalize and open things
29020   if trim$(starting_key$)="" and route_filter=0 then ! if no beginning account or starting route #, start at beginning of file
29040     restore #h_customer_2,key>="         ": 
29060   else if trim$(starting_key$)<>"" then 
29080     restore #h_customer_2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
29100   else if trim$(starting_key$)="" and route_filter>0 then ! selected a route and no beginning Account
29120     restore #h_customer_2,key>=cnvrt$("pic(zz)",route_filter)&"       ": 
29140   end if 
29160 ! 
29180   gosub BUD1
29200   if pa_enabled=1 then 
29220     fnpa_open(pa_orientation$)
29240   else if pa_enabled=2 then 
29260     fnpa_open(pa_orientation$,'','PDF')
29280   else 
29300     fnopenprn
29320   end if 
29340 ! IF filter_selected_only=0 THEN GOSUB SORT1
29360 ! /r
30000 NEXT_ACCOUNT: ! r: main loop
30020   if filter_selected_only=1 then goto SCR_ASK_ACCOUNT
30040   if enable_bulksort then 
30060 READ_BULKSORT: ! 
30080     read #7,using 'form pos 1,pd 3': r6 eof RELEASE_PRINT
30100     read #h_customer_1,using F_CUSTOMER_A,rec=r6,release: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4,est norec READ_BULKSORT
30120   else if enable_cass_sort then 
30140 READ_CASSSORT: ! 
30160     read #7,using 'form pos 1,pd 3': r6 eof RELEASE_PRINT
30180     read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ norec READ_CASSSORT
30200     read #h_customer_1,using F_CUSTOMER_A,key=z$,release: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4,est nokey READ_CASSSORT
30220   else 
30240     read #h_customer_2,using F_CUSTOMER_A: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4,est eof RELEASE_PRINT
30260   end if 
30280 ! if trim$(z$)='100100.00'  then pr g(8) : pause
30300 F_CUSTOMER_A: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1831,n 9
30340   if route_filter<>0 and route_filter><route then goto RELEASE_PRINT

30360 ! if trim$(z$)='106700.00' then pr z$,f,d1 : pause

30380   if f><d1 then goto NEXT_ACCOUNT
30390   if bal=0 and ~include_zero_bal then goto NEXT_ACCOUNT
30392   if bal<0 and ~include_credit_bal then goto NEXT_ACCOUNT
30400   gosub BUD2 ! determine if budget customer
30420 ! if ~starting_place_enabled then
30440 !   if st1$=z$ then
30460 !     starting_place_enabled=0  ! starting_place_enabled used to be st1
30480 !   else 
30500 !     goto NEXT_ACCOUNT
30520 !   end if
30540 ! end if
32000 HERE: ! 
32010   fncustomer_address(z$,mat pe$) ! read alternate billing address
32020   let pb=bal-g(11)
32030   fn_override_service_date(d2,d3,extra_3,extra_4)
32040   if filter_past_due_only and pb<=0 then goto NEXT_ACCOUNT
32060   if filter_no_past_due and pb>0 then goto NEXT_ACCOUNT
33000 ! r: pr bill routine
33010   if env$('client')='French Settlement' then 
33020     fn_print_bill_fsg(pb,mat g,mat d,bal,final,mat pe$,d4,mat e$,z$,mat mg$,budgetpb,d2,d3)
33030   else if env$('client')='Campbell' then 
33040     fn_print_bill_campbell(z$,mat mg$,d2,d3)
33050   else if env$('client')='Raymond' then 
33060     fn_print_bill_raymond(z$,mat mg$, "Mailing Address: P O Box 87")
33070   else if env$('client')='Cerro Gordo' then 
33080     fn_print_bill_cerro(z$,mat mg$,mat penalty$,d2,d3)
33090   else if env$('client')='Merriam Woods' then 
33100     fn_print_bill_merriam(z$,mat mg$,d2,d3)
33110   else if env$('client')='Blucksberg' then 
33120     fn_print_bill_blucksberg(z$,mat mg$)
33130   else if env$('client')='Omaha' then 
33140     fn_print_bill_omaha(z$,mat mg$,mat mg2$,d2,d3,mat penalty$)
33150   else if env$('client')='Pennington' then
33160     fn_print_bill_pennington(z$,mat mg$,mat mg2$,d2,d3,d4)
33170     ! fn_print_bill_Exeter(z$,mat mg$,d2,d3,d4)
33180   else if env$('client')='Edinburg' then
33190     fn_print_bill_edinburg(z$,mat mg$,d1,d2,d3,d4)
33200   else if env$('client')='Billings' then
33210     fn_print_bill_billings(mat mg$,mat g,mat b,bal,mat penalty$,d1,d2,d3,d4,mat pe$,final$,z$) ! 
33220   else if env$('client')='Choctaw' then
33230     fn_print_bill_choctaw(z$,mat g,mat b,mat penalty$,d1,d2,d3,d4,mat e$,final)
33232 !   fn_print_bill_choctaw(z$,mat g,mat b,mat penalty$,d1,d2,d3,d4,mat pe$,final)
33240   else ! Exeter, Findlay, etc
33242     if enableReturnServiceRequested=>0 then enableReturnServiceRequested=1
33244     if enableIsDueNowAndPayable=>0 then enableIsDueNowAndPayable=1
33250     fn_print_bill_standard_pdf_a(z$,mat mg$,mat mg2$,enableIsDueNowAndPayable,enableReturnServiceRequested)
33260   end if 
33270 ! /r
34000   billsPrintedCount(2)=billsPrintedCount(2)+1 ! accumulate totals
34020   if env$('acsDeveloper')<>'' and sum(mat billsPrintedCount)>val(env$('UB_Limit')) then 
34040     msgbox('UB_Limit exceeded')
34060   end if
34080   goto NEXT_ACCOUNT ! /r
35000 def fn_mg2$*80(; m2forcecnt)
35020   if m2forcecnt then 
35040     let m2item=m2forcecnt
35060   else 
35080     let m2item+=1
35100   end if 
35120   if m2item=>udim(mat mg2$) then 
35140     fn_mg2$=''
35160   else 
35180     fn_mg2$=mg2$(m2item)
35200   end if 
35220 fnend 
36000 RELEASE_PRINT: ! r:
36020   close #h_customer_1: ioerr ignore
36040   close #h_customer_2: ioerr ignore
36060   if pa_enabled then 
36080     fnpa_finis
36100   else if sum(billsPrintedCount)>0 then
36120     fncloseprn( forceWordProcessor$)
36140   end if 
36160   goto ENDSCR
36180 ! /r
38000 SCR_ASK_ACCOUNT: ! r: account selection screen
38020   fntos(sn$:="UBPrtBl1-FS3")
38060   fnlbl(1,1,"Account (blank to stop)",31,1)
38080   if trim$(starting_key$)="" then 
38100     if z$<>"" then 
38140       fnlbl(3,1,"Last Account entered was "&z$,44,1)
38160     else 
38200       fnlbl(3,1,'',44,1)
38220     end if 
38240   end if 
38250   fncmbact(1,17)
38260   let resp$(1)=starting_key$
38280   fncmdset(11)
38290   fnacs(sn$,0,mat resp$,ck)
38300   if ck=5 then goto RELEASE_PRINT
38320   starting_key$=lpad$(trim$(resp$(1)(1:10)),10)
38340   if trim$(starting_key$)="" then goto RELEASE_PRINT
38360   read #h_customer_1,using F_CUSTOMER_A,key=starting_key$,release: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4,est nokey SCR_ASK_ACCOUNT
38380   goto HERE
38400 ! /r
40000 ENDSCR: ! r: pr totals screen
40200   if sum(billsPrintedCount)=0 then let pct=0 else let pct=billsPrintedCount(2)/sum(billsPrintedCount)*100
40400   fntos(sn$="Bills-Total")
40600   let mylen=23 : let mypos=mylen+2
40800   let respc=0
41000   fnlbl(1,1,"Total Bills Printed:",mylen,1)
41200   fntxt(1,mypos,8,0,1,"",1)
41400   let resp$(respc+=1)=cnvrt$("N 8",sum(billsPrintedCount))
42000   fncmdset(52)
42200   fnacs(sn$,0,mat resp$,ck)
42290   goto XIT ! /r
42400 XIT: let fnxit
42500 IGNORE: continue 
43000 ! <updateable region: ertn>
43240 ERTN: let fnerror(program$,err,line,act$,"xit")
43260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
43280   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
43300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
43320 ERTN_EXEC_ACT: execute act$ : goto ERTN
43340 ! </updateable region: ertn>
50090 BUD1: ! r:
50092   bud1=0
50100   dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5)
50110   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,input,keyed ioerr EO_BUD1
50120   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,input,relative 
50130   bud1=1
50140 EO_BUD1: ! 
50142   return  ! /r
53160 BUD2: ! r:
53170   let totba=bd1=bd2=budgetpb=havebudget=00
53180   mat bd1(5) : mat bd1=(0) : mat bd2=(0)
53190   if bud1=0 then goto EO_BUD2
53200   read #81,using L3230,key=z$: z$,mat ba,mat badr nokey EO_BUD2
53210   let havebudget=1
53220   for j=2 to 12: let totba=totba+ba(j): next j
53230 L3230: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
53240   if totba=0 then let havebudget=0: goto EO_BUD2
53250   let ta1=badr(1)
53260 L3260: if ta1=0 then goto EO_BUD2
53270   read #82,using L3280,rec=ta1: z$,mat bt1,nba norec EO_BUD2
53280 L3280: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
53290   if bt1(14,1)>0 then goto L3340
53300 ! IF BT1(1,2)=F THEN GOTO 3350 ! ignore current budget billing record
53310   budgetpb=budgetpb+bt1(5,1) ! add up prior balance for budget billing customers (any unpaid not counting current bill
53320   bd1=bd1+1
53330   if bd1>5 then goto EO_BUD2
53340 L3340: let ta1=nba : goto L3260
53350 EO_BUD2: ! 
53360   return  ! /r
53500 def fn_get_mat_at(mat at$)
53540   open #h_company:=fngethandle: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input 
53560   read #h_company,using "Form POS 41,2*C 40": at$(2),at$(3)
53580   close #h_company: 
53600   at$(1)=env$('cnam')
53620   let z=21
53640   at$(1)=trim$(at$(1))(1:z)
53660   let x=len(at$(1)) : let y=z-x
53680   at$(1)=rpt$(" ",int(y/2))&at$(1)
53700   let z=26
53720   for j=2 to udim(at$)
53740     at$(j)=trim$(at$(j))(1:z)
53760     let x=len(at$(j))
53780     let y=z-x
53800     at$(j)=rpt$(" ",int(y/2))&at$(j)
53820   next j
53840 fnend 
54000 def fn_override_service_date(&osd_service_from,&osd_service_to,osd_extra_3,osd_extra_4)
54020   ! osd_service_from / d2=Service From Date (ask on Screen)
54040   ! osd_service_to / d3=Service To Date (ask on Screen)
54060   ! osd_extra_3 / extra(3)=Current Reading Date (from customer record)
54080   ! osd_extra_4 / extra(4)=Prior Reading Date (from customer record)
54100   if osd_service_from=0 then osd_service_from=osd_extra_4
54120   if osd_service_to=0 then osd_service_to=osd_extra_3
54140 fnend 
55000 def fn_pay_after_amt
55020   if bal<=0 then let fn_pay_after_amt=round(bal,2) else let fn_pay_after_amt=bal+g(10) ! Penalties at Merriam Woods are flat
55040 fnend  ! fn_pay_after_amt
60240 def fn_print_bill_fsg(pb,mat g,mat d,bal,final,mat pe$,d4,mat e$,z$,mat mg$,budget,serviceFromDate,serviceToDate) ! french settlement gas
60260   ! ______________pre-print calculations__________________________________
60280   if pb<>0 then let pb$="Prior Balance" else let pb$=""
60300   !   if g(1)=0 then let t1$="" else let t1$="WTR"
60320   !   if g(2)=0 then let t2$="" else let t2$="SWR"
60340   !   if g(3)=0 then let t3$="" else let t3$="RPR"
60360   if g(4)=0 then let t4$="" else let t4$="GAS"
60380   if g(5)=0 then let t5$="" else let t5$="Purchased Gas Adj."
60400   if g(6)=0 then let t6$="" else let t6$="Inspection Fee"
60420   if g(7)=0 then let t7$="" else let t7$="Deposit Interest"
60440   if g(8)=0 then 
60450     let t8$=""
60452   else if g(8)<0 and final>0 then 
60454     let t8$="Deposit Refund"
60456   else 
60458     let t8$="Other"
60460   end if 
60480   if g(9)=0 then let t9$="" else let t9$="La. Sales Tax"
60500   ! If D(10)=1 Then eST$="Bill Estimated" Else eST$=""
60520   if final>0 then let final$="Final Bill" else let final$=""
60530   if budget>0 then bud$="Budgeted Amount:"&trim$(cnvrt$("Pic($$,$$$.##",budget)) else bud$=""
60540   if bal<=0 then let g(10)=0
60560   let gross=max(bal+g(10),0)
60580   ! ______________actual Bill Printing____________________________________
60640   F_PR_TABLE_AND_ADDR_1: form pos 1,nz 6,nz 7,nz 7,x 2,c 3,pos 28,nz 8.2,pos 39,c 30
60660   F_PR_TABLE_AND_ADDR_2: form pos 1,c 20,pos 28,nz 8.2,pos 39,c 52
60760   ! __
60780   ! 
60800   pr #255,using 'form pos 25,c 10': z$
60840   pr #255,using 'form pos 38,c 10,skip 4': z$
60880   pr #255,using F_PR_TABLE_AND_ADDR_1: d(9),d(10),d(11),t4$,g(4),pe$(1)
60900   pr #255,using F_PR_TABLE_AND_ADDR_2: t5$,g(5),pe$(2) ! Purchased Gas Adj.
60920   pr #255,using F_PR_TABLE_AND_ADDR_2: t6$,g(6),pe$(3) ! Inspection Fee
60940   pr #255,using F_PR_TABLE_AND_ADDR_2: t7$,g(7),pe$(4) ! Deposit Interest
60960   pr #255,using F_PR_TABLE_AND_ADDR_2: t8$,g(8),mg$(1) ! Deposit Refund or Other
61000   pr #255,using F_PR_TABLE_AND_ADDR_2: t9$,g(9),bud$(1:30) ! La. Sales Tax
61020   pr #255,using F_PR_TABLE_AND_ADDR_2: pb$,bal-g(11) ! Prior Balance
61060   pr #255,using 'form pos 22,c 10': final$
61100   pr #255: ""
61120   pr #255: "" ! mg$(1)  <-- messages can not pr there - it hits a lot of preprinted text there
61140   pr #255: "" ! mg$(2)
61160   pr #255: "" ! mg$(3)
61180   pr #255: ""
61200   count+=1
61202   pr #255: "" ! if count=2 or count=3 then pr #255: ""
61240   pr #255,using L340: serviceFromDate,serviceToDate,gross,bal,gross,d4,bal
61250   L340: form pos 1,pic(## ## ##),x 1,pic(## ## ##),n 8.2,pos 27,pic(-----.--),pos 40,n 7.2,x 3,pic(## ## ##),pic(-----.--)
61260   if count=1 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! pr #255: ! EXTRA LINE BETWEEN 1ST and 2nd bills
61280   if count=2 then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: ! EXTRA LINE BETWEEN 2nd & 3rd bill
61300   if count=3 then count=0 : pr #255: newpage
61480 fnend 
68000 def fn_print_bill_campbell(z$,mat mg$,pbcampbell_service_from,pbcampbell_service_to)
68002   ! correct margins are left=.4, top=.35, right=.2, bottom=.2
68010   ! r: any and all necessary setup (except opening the printer) to pr one bill
68020   if ~pbcampbel_setup then 
68040     let pbcampbel_setup=1
68060     open #h_pbcampbel_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
68080     F_PBCAMPBEL_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6
68110     blankbefore=1
68120     blankafter=3
68140   end if 
68160   read #h_pbcampbel_customer,using F_PBCAMPBEL_CUSTOMER,key=z$: z$,pbcampbel_meter_address$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4
68170   fn_override_service_date(pbcampbell_service_from,pbcampbell_service_to,extra_3,extra_4)
68180   dim pbcampbel_addr$(4)*30
68200   dim pbcampbel_meter_address$*30
68220   fncustomer_address(z$,mat pbcampbel_addr$)
68230   ! /r
68232   ! r: pr that bill
68240   if blankbefore<>0 then 
68260     for j=1 to blankbefore
68280       pr #255: ""
68300     next j
68320   end if 
68340   pr #255,using 'Form POS 1,C 30': e$(1) ! Line 1
68360   pr #255,using 'Form POS 1,C 30': pbcampbel_addr$(1) ! Line 2
68380   pr #255,using 'Form POS 1,C 30': pbcampbel_addr$(2) ! Line 3
68400   pr #255: "" ! Line 4
68420   pr #255: "" ! Line 5
68440   pr #255: "" ! Line 5
68460   pr #255: "" ! Line 6
68480   if g(3)=0 then 
68500     e$=""
68520   else if a3=3 then 
68540     e$="CE"
68560   else if a3=4 then 
68580     e$="RH"
68600   else if a3=5 then 
68620     e$="CH"
68640   else 
68660     e$="RE"
68680   end if 
68700   pr #255,using PBCAMPBEL_L1020: z$,e$,g(3),d(5),d(6),d(7),e$,g(3),"" ! line 7
68720   PBCAMPBEL_L1020: form pos 1,c 10,pos 13,c 2,pic(---,---.--),x 4,3*pic(zzzzzzzz),pos 58,c 2,x 1,pic(--,---.--),pos 74,c 21
68760   if g(1)=0 then e$="" else e$="WA"
68780   pr #255,using PBCAMPBEL_L1020: "",e$,g(1),d(1),d(2),d(3),e$,g(1),z$ ! Line 8
68800   if g(2)=0 then e$="" else e$="SW"
68820   pr #255,using PBCAMPBEL_L1080: e$,g(2),e$,g(2),pbcampbel_addr$(1) ! Line 9
68840   PBCAMPBEL_L1080: form pos 13,c 2,pic(---,---.--),pos 58,c 2,pic(---,---.--),pos 74,c 30
68860   PBCAMPBEL_L1082: form pos 13,c 2,pic(---,---.--),x 2,c 30,pos 58,c 2,pic(---,---.--),pos 74,c 30
68880   if g(5)=0 then e$="" else e$="SA"
68900   pr #255,using PBCAMPBEL_L1082: e$,g(5),pbcampbel_addr$(1),e$,g(5),pbcampbel_addr$(2) ! line 10
68920   if g(6)=0 then e$="" else e$="SL"
68940   pr #255,using PBCAMPBEL_L1082: e$,g(6),pbcampbel_addr$(2),e$,g(6),pbcampbel_addr$(3) ! line 11
68960   if g(9)=0 then e$="" else e$="TX"
68980   pr #255,using PBCAMPBEL_L1082: e$,g(9),pbcampbel_addr$(3),e$,g(9),pbcampbel_addr$(4) ! line 12
69000   if g(4)=0 then e$="" else e$="SC"
69020   pr #255,using PBCAMPBEL_L1082: e$,g(4),pbcampbel_addr$(4),e$,g(4),"" ! line 13
69040   if g(8)=0 then e$="" else e$="OC"
69060   pr #255,using PBCAMPBEL_L1080: e$,g(8),e$,g(8),"" ! line 14
69080   ! If BAL-G(11)=0 Then e$="" Else e$="AR"
69100   pr #255: "" ! line 15
69120   pr #255: "" ! line 16
69140   pr #255: "" ! line 17
69160   pr #255: "" ! line 18
69180   pr #255,using 'Form pos 1,pic(zz/zz/zz),Pos 9,N 8.2,N 8.2,X 2,PIC(ZZ/ZZ/ZZ),POS 42,3*N 12.2,N 12.2': pbcampbell_service_to,g(12),g(11),pbcampbell_service_to,g(12),g(11),g(12),g(11) ! line 19
69200   if (count+1)/3=int((count+1)/3) then goto PBCAMPBEL_L1250
69220   if blankafter<>0 then 
69240     for j=1 to blankafter
69260       pr #255: " "
69280     next j
69300   end if 
69320   PBCAMPBEL_L1250: ! 
69340   billsPrintedCount(3)+=1
69360   count=count+1
69380   if count/3=int(count/3) then ! newpage on every third bill
69400     pr #255: newpage
69420   end if 
69430   ! /r
69440 fnend 
71000 def fn_print_bill_raymond(z$,mat mg$; raymondAdditionalText$*128) ! inherrits all those local customer variables too
71020   if ~setup_raymond then 
71040     setup_raymond=1
71060     lyne=3
71070     fn_get_mat_at(mat at$)
71080   end if 
71100   ! -- Standard 4 Per Page Even Perferated Card Stock Bills
71120   checkcounter+=1
71140   if checkcounter=1 then let xmargin=0 : let ymargin=0
71160   if checkcounter=2 then let xmargin=139 : let ymargin=0
71180   if checkcounter=3 then let xmargin=0 : let ymargin=108
71200   if checkcounter=4 then let xmargin=139 : let ymargin=108 : checkcounter=0
71220   ! ______________________________________________________________________
71240   fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
71260   fnpa_fontbold(1)
71280   fnpa_fontsize(12)
71300   fnpa_font
71320   fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin)
71340   fnpa_font("Lucida Console")
71360   fnpa_fontsize
71380   fnpa_fontbold
71400   fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
71420   fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
71440   fnpa_txt('#'&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
71460   fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
71480   fnpa_txt('From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3),xmargin+2,lyne*7+ymargin)
71500   fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
71520   fnpa_txt('Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),xmargin+2,lyne*11+ymargin)
71540   fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
71560   fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
71580   fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
71600   fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)
71620   ! ______________________________________________________________________
71640   ! PRINTGRID: !
71660   let meter=14
71680   fnpa_fontsize(8)
71700   if g(1)<>0 then 
71720     fnpa_txt("WTR",xmargin+1,lyne*(meter+=1)+ymargin)
71740     fnpa_txt(fnformnumb$(d(1),0,9),xmargin+6 ,lyne*meter+ymargin)
71760     fnpa_txt(fnformnumb$(d(3),0,9),xmargin+25,lyne*meter+ymargin)
71780     fnpa_txt(fnformnumb$(g(1),2,9),xmargin+45,lyne*meter+ymargin)
71800   end if 
71820   if g(2)<>0 then 
71840     fnpa_txt("SWR",xmargin+1,lyne*(meter+=1)+ymargin)
71860     fnpa_txt(fnformnumb$(g(2),2,9),xmargin+45,lyne*meter+ymargin)
71880   end if 
71900   if g(4)<>0 then 
71920     fnpa_txt(s4code$,xmargin+1,lyne*(meter+=1)+ymargin)
71940     fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
71960     fnpa_txt(fnformnumb$(d(11),0,9),xmargin+25,lyne*meter+ymargin)
71980     fnpa_txt(fnformnumb$(g(4),2,9),xmargin+45,lyne*meter+ymargin)
72000   end if 
72020   if g(8)<>0 then 
72040     fnpa_txt("MISC",xmargin+1,lyne*(meter+=1)+ymargin)
72060     fnpa_txt(fnformnumb$(g(8),2,9),xmargin+45,lyne*meter+ymargin)
72080   end if 
72100   if pb><0 then 
72120     fnpa_line(xmargin+46,lyne*(meter+=1)+ymargin,15,0)
72140     fnpa_txt("   Subtotal",xmargin+1,lyne*(meter+=.25)+ymargin)
72160     fnpa_txt(fnformnumb$(g(1)+g(2)+g(8),2,9),xmargin+45,lyne*meter+ymargin)
72180     fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
72200     fnpa_txt(fnformnumb$(pb,2,9),xmargin+45,lyne*meter+ymargin)
72220   end if 
72240   fnpa_fontsize
72260   ! ______________________________________________________________________
72280   if estimatedate=d1 then let fnpa_txt("Bill estimated!",xmargin+1,lyne*21+ymargin)
72300   fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
72320   fnpa_txt("Pay By "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*24+ymargin)
72340   fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
72360   fnpa_txt("Pay After "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*25+ymargin)
72380   fnpa_txt(fnformnumb$(bal+round(bal*.10,2),2,9),xmargin+42,lyne*25+ymargin)
72400   fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
72420   fnpa_txt(raymondAdditionalText$,xmargin+1,lyne*27+ymargin)
72440   !   fnpa_txt("Re-connect fee $??.00",XMARGIN+1,LYNE*28+YMARGIN)
72460   ! 
72480   fnpa_fontsize(7)
72500   fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
72520   fnpa_line(xmargin+90,ymargin+0,7,0)
72540   fnpa_line(xmargin+90,ymargin+2.8,7,0)
72560   fnpa_line(xmargin+90,ymargin+5.6,7,0)
72580   fnpa_line(xmargin+90,ymargin+8.4,7,0)
72600   fnpa_line(xmargin+90,ymargin+11.2,7,0)
72620   fnpa_line(xmargin+90,ymargin+14,7,0)
72640   fnpa_line(xmargin+90,ymargin+17,7,0)
72660   fnpa_txt("   Pre-Sorted",xmargin+100,lyne*1-1+ymargin)
72680   fnpa_txt("First Class Mail",xmargin+100,lyne*2-1+ymargin)
72700   fnpa_txt("  U.S. Postage  ",xmargin+100,lyne*3-1+ymargin)
72720   fnpa_txt("      Paid",xmargin+100,lyne*4-1+ymargin)
72740   fnpa_txt("  Permit No 7",xmargin+100,lyne*5-1+ymargin)
72760   fnpa_fontsize(9)
72780   fnpa_txt("Return Service Requested",xmargin+68,lyne*7.6+ymargin-6)
72800   fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
72820   fnpa_txt("side with payment to:",xmargin+68,lyne*8+ymargin)
72840   fnpa_txt(env$('cnam'),xmargin+68,lyne*9+ymargin)
72860   fnpa_fontsize
72880   fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*11+ymargin)
72900   fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
72920   fnpa_txt('After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*12+ymargin)
72940   fnpa_txt(fnformnumb$(bal+round(bal*.10,2),2,9),xmargin+106,lyne*12+ymargin)
72960   fnpa_fontsize(9)
72980   addy=14
73000   fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
73020   fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
73040   fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
73060   addy+=1
73080   fnpa_fontsize
73100   if df$="Y" then 
73120     fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
73140   end if 
73160   if c4>0 then 
73180     fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
73200   end if 
73220   fnpa_txt('#'&trim$(z$)&' '&bulk$,(xmargin+68),lyne*(addy+=1)+ymargin)
73240   if pe$(1)<>"" then 
73260     fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
73280   end if 
73300   if pe$(2)<>"" then 
73320     fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
73340   end if 
73360   if pe$(3)<>"" then 
73380     fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
73400   end if 
73420   if pe$(4)<>"" then 
73440     fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
73460   end if 
73480   if checkcounter=1 then checkx=1.375 : checky=3.6875
73500   if checkcounter=2 then checkx=6.75 : checky=3.6875
73520   if checkcounter=3 then checkx=1.375 : checky=7.9375
73540   if checkcounter=0 then checkx=6.75 : checky=7.9375
73560   bc$=""
73580   if trim$(bc$)<>"" then let fnpa_barcode(checkx,checky,bc$)
73600   if checkcounter=0 then 
73620     fnpa_newpage
73640   end if 
73660 fnend 
74000 BULKSORT: ! r: sort in bulk sort code sequence
74020   open #h_control:=fngethandle: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
74060   write #h_control,using 'form pos 1,c 128': "FILE customer.H"&env$('cno')&","&env$('Q')&"\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
74080   if route_filter>0 then write #h_control,using 'form pos 1,c 128': 'RECORD I,1,2,N,"'&str$(route_filter)&'","'&str$(route_filter)&'"'
74100   write #h_control,using 'form pos 1,c 128': "MASK 1942,12,C,A,1,10,C,A"
74120   close #h_control: 
74140   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr ignore
74160   execute "Sort "&env$('Temp')&"\Control."&session$
74180   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
74200   return  ! /r
76000 SORT1: ! r: SELECT & SORT
76010   enable_cass_sort=0 ! replaces old s5 variable
76020   open #h_cass1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Cass1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Cass1Idx.h"&env$('cno')&",Shr",internal,input,keyed ioerr XIT_SORT1
76040   open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=19",internal,output 
76060   enable_cass_sort=1
76080   if route_filter=0 then let routekey$="" else let routekey$=cnvrt$("N 2",route_filter)&"       " ! key off first record in route (route # no longer part of customer #)
76100   restore #h_customer_2,search>=routekey$: 
76120   do 
76140     read #h_customer_2,using 'form pos 1,c 10,pos 296,pd 4,pos 1741': z$,f,route eof END5
76160     if route_filter and route_filter><route then goto END5
76220     if f=d1 then 
76240       let zip5$=cr$=""
76260       read #h_cass1,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey ignore
76280       write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
76290     end if 
76300   loop 
76320 ! 
76340 END5: ! 
76350   close #h_cass1: 
76360   close #6: 
76380   open #h_sort1_control:=fngethandle: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
76400   write #h_sort1_control,using 'form pos 1,c 128': "File "&env$('Temp')&"\Temp."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
76420   write #h_sort1_control,using 'form pos 1,c 128': "Mask 1,19,C,A"
76440   close #h_sort1_control: 
76460   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr ignore
76480   execute "Sort "&env$('Temp')&"\Control."&session$
76500   open #6: "Name="&env$('Temp')&"\Temp."&session$,internal,input,relative 
76520   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
76540 XIT_SORT1: ! 
76560   return  ! /r
78000 def fn_print_bill_cerro(z$,mat mg$,mat penalty$,d2x,d3x)
78020   ! r: any and all necessary setup (except opening the printer) to pr one bill
78040   if ~pbcerro_setup then 
78060     let pbcerro_setup=1
78080     open #h_pbcerro_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
78100     F_PBCERRO_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1854,pd 5.2
78120     blankbefore=1
78140     blankafter=3
78160   end if 
78180   read #h_pbcerro_customer,using F_PBCERRO_CUSTOMER,key=z$: z$,pbcerro_meter_address$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4
78200   dim pbcerro_meter_address$*30 ! formerly e$(2)
78220   fncustomer_address(z$,mat pe$)
78240   ! /r
78260   ! r: pr that bill
78280   if final=2 then 
78300     let g(8)-=b(8): let g(11)=g(12)+g(8): bal+=g(8)
78320   end if 
78340   let penalty=0
78360   for j=1 to 10
78380     if penalty$(j)="Y" then ! accumulate all penalties and set charge to zero
78400       let penalty+=g(j)
78420       let g(j)=0
78440     end if 
78460   next j
78480   let pb=bal-g(11)
78500   pr #255: ""
78520   pr #255: ""
78540   pr #255,using L1550: "FROM",int(d2x*.01),"TO",int(d3x*.01),d1
78560   L1550: form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##),skip 4
78580   if pb<>0 then let pb$="   PRIOR BALANCE" else let pb$=""
78600   pr #255: ""
78620   L1580: form pos 3,c 17,nz 10.2,pos 38,c 10
78640   if g(1)=0 then let t$="" else let t$=service$(1)
78660   pr #255,using PBC_L1610: t$,0,d(1),d(3),g(1)
78680   PBC_L1610: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,nz 10.2,nz 12.2
78700   PBC_L1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 5,pic(zz/zz/zz)
78720   if g(2)=0 then let t$="" else let t$=service$(2)
78740   if bal<=0 then pr #255,using PBC_L1610: t$,0,0,0,g(2),0,bal : goto PBC_L1660
78760   pr #255,using PBC_L1610: t$,0,0,0,g(2),bal+penalty,bal
78780   PBC_L1660: if g(3)=0 then let t$="" else let t$=service$(3)
78800   pr #255,using PBC_L1620: t$,0,0,0,g(3),d4
78820   if g(4)=0 then let t$="" else let t$=service$(4)
78840   pr #255,using PBC_L1610: t$,0,0,0,g(4)
78860   if g(5)=0 then let t$="" else let t$=service$(5)
78880   pr #255,using PBC_L1610: t$,0,0,0,g(5)
78900   if g(6)=0 then let t$="" else let t$=service$(6)
78920   pr #255,using L1580: pb$,pb,z$
78960   if g(8)=0 then let t$="" else let t$=service$(8)
78980   pr #255,using PBC_L1610: t$,0,0,0,g(8)
79000   if est=1 then est$="BILL ESTIMATED" else est$=""
79020   if c4>0 then let final$="FINAL BILL" else let final$=""
79040   if df$="Y" then let final$="DRAFTED"
79060   if bal<=0 then let penalty=0
79080   if bal<0 then let g(5)=0
79100   pr #255: ""
79120   pr #255,using 'Form POS 7,C 20,POS 38,C 25': est$,pe$(1)(1:25)
79140   pr #255,using 'Form POS 1,CR 7,X 1,PIC(ZZ/ZZ/ZZ),NZ 13.2,POS 38,C 25': 'DUE BY:',d4,bal,pe$(2)(1:25)
79160   pr #255,using 'Form POS 13,C 18,POS 38,C 25': e$(1)(1:18),pe$(3)(1:25)
79180   pr #255,using 'Form POS 2,C 10,X 5,C 10,POS 38,C 25': z$,final$,pe$(4)(1:25)
79200   bills+=1
79220   pr #255,using 'form pos 2,c 30': mg$(1)
79240   pr #255,using 'form pos 2,c 30': mg$(2)
79260   pr #255,using 'form pos 2,c 30': mg$(3)
79280   if int(bills/3)<>bills/3 then pr #255,using 'form pos 2,c 30,skip 1': " "," " ! space extra if 1st or 2nd bill
79300   if int(bills/3)=bills/3 then pr #255: newpage ! BOTTOM OF PAGE
79320   ! /r
79340 fnend 
80000 def fn_print_bill_merriam(z$,mat mg$,service_from,service_to) ! inherrits all those local customer variables too
80020   if ~setup_merriam then ! r:
80040     setup_merriam=1
80060     lyne=3
80080     addr_indent=8
80100     addr_down=3
80120     fn_get_mat_at(mat at$)
80460   ! 
80480   end if  ! /r
80500   ! r: pr that bill
80520   let pb=bal-g(11)
80540   let net_bill=g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9)
80550   fn_override_service_date(service_from,service_to,extra_3,extra_4)
80560   ! -- Standard 4 Per Page Even Perforated Card Stock Bills
80580   checkcounter+=1
80600   if checkcounter=1 then let xmargin=0 : let ymargin=5
80620   if checkcounter=2 then let xmargin=142 : let ymargin=5 ! xmargin was 137
80640   if checkcounter=3 then let xmargin=0 : let ymargin=113 ! ymargin was 108
80660   if checkcounter=4 then let xmargin=142 : let ymargin=113 : checkcounter=0
80680   ! move page down 1/2 inch
80700   ! 
80720   ! ______________________________________________________________________
80740   if checkcounter=1 then ! it's about to pr the first bill on the page
80760     fnpa_line(70,1,0,1100) ! line down the middle of the page
80780     fnpa_line(140,1,0,1100) ! line down the middle of the page
80800     fnpa_line(215,1,0,1100) ! line down the middle of the page
80820     fnpa_line(1,108,800,0) ! line across the middle of the page
80840   !   fnpa_line(pl_left_pos,pl_top_pos,pl_width,pl_height,  pl_hollow) ! line across the middle of the page
80860   end if 
80880   fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
80900   fnpa_fontbold(1)
80920   fnpa_fontsize(12)
80940   fnpa_font
80960   fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin)
80980   fnpa_font('Lucida Console')
81000   fnpa_fontsize
81020   fnpa_fontbold
81040   fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
81060   fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
81080   fnpa_txt(trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
81100   fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
81200   pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_from)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_to)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
81220   pr #20: 'Call Print.AddText("Due upon receipt",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
81240   fnpa_txt(e$(2),xmargin+2,lyne*9+ymargin)
81260   pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
81280   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
81300   fnpa_fontsize(7)
81320   pr #20: 'Call Print.AddText("Current",'&str$(xmargin+12+5)&','&str$(lyne*13+ymargin)&')'
81340   pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+35+5)&','&str$(lyne*13+ymargin)&')'
81360   pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+52+5)&','&str$(lyne*13+ymargin)&')'
81380   ! ______________________________________________________________________
81400   ! PRINTGRID: !
81420   let meter=14 ! 02114   let meter=20 ! lyne=2 ! 3 ! 2.15 !  started at 20 and 2.1
81440   fnpa_fontsize ! line_top(1)
81460   if g(1)<>0 then 
81480     pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
81500     pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
81520     pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,7)&'",'&str$(xmargin+24)&','&str$(lyne*meter+ymargin)&')'
81540     pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
81560     pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
81580     pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
81600   end if  ! g(1)<>0
81620   if g(2)<>0 then 
81640     pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
81660     pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
81680     pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
81700     pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
81720   end if  ! g(2)<>0
81740   if g(3)<>0 or d(7)<>0 then 
81760     pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
81780     pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
81800     pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
81820     pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
81840     pr #20: 'Call Print.AddText("EL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
81860     pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
81880   end if  ! g(3)<>0 or d(7)<>0
81900   s4code$="GAS"
81920   if g(4)<>0 then 
81940     pr #20: 'Call Print.AddText("'&s4code$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
81960     pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
81980     pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
82000     pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
82020     pr #20: 'Call Print.AddText("'&s4code$&'",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
82040     pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
82060   end if  ! g(4)<>0
82080   if g(5)<>0 then 
82100     pr #20: 'Call Print.AddText("Trash",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' ! "Trash was "SL"
82120     pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
82140     pr #20: 'Call Print.AddText("Trash",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')' ! "Trash was "SL"
82160     pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
82180   end if  ! g(5)<>0
82200   if g(6)<>0 then 
82220     pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
82240     pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
82260     pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
82280     pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
82300   end if  ! g(6)<>0
82320   if g(7)<>0 then 
82340     pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
82360     pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
82380     pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
82400     pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
82420   end if  ! g(7)=0
82440   if g(8)<>0 then 
82460     pr #20: 'Call Print.AddText("Other",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
82480     pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
82500     pr #20: 'Call Print.AddText("Other",'&str$(xmargin+01)&','&str$(lyne*(meter)+ymargin)&')'
82520     pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
82540   end if  ! g(8)<>0
82560   if g(9)<>0 then 
82580     pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
82600     pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
82620     pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin)&')'
82640     pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
82660   end if  ! g(9)<>0
82680   pr #20: 'Call Print.AddLine('&str$(xmargin+49)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
82700   pr #20: 'Call Print.AddLine('&str$(xmargin+91+14)&','&str$(lyne*(meter)+ymargin+2)&',15,0)'
82720   pr #20: 'Call Print.AddText("   Net Bill",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')'
82740   pr #20: 'Call Print.AddText("Net",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin+2)&')'
82760   pr #20: 'Call Print.AddText("'&fnformnumb$(net_bill,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
82780   pr #20: 'Call Print.AddText("'&fnformnumb$(net_bill,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
82800   if pb then 
82820     pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')'
82840     pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
82860     pr #20: 'Call Print.AddText("Prior",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin+2)&')'
82880     pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
82900   end if  ! pb
82920   fnpa_fontsize : lyne=3
82940   ! 
82960   if estimatedate=d1 then let fnpa_txt("Bill estimated!",xmargin+1,lyne*21+ymargin)
82980   fnpa_line(xmargin+1,lyne*23+1+ymargin+10,63,0)
83000   if budget>0 then 
83020     pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
83040     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
83060     pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
83080     pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
83100   else 
83120     pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
83140     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
83160     pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
83180     pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
83200   end if 
83220   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin+10)&',63,0)'
83240   fnpa_fontsize(7)
83260   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
83280   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
83300   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
83320   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
83340   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
83360   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
83380   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
83400   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
83520   fnpa_fontsize(9)
83560   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68+addr_indent)&','&str$(lyne*7+ymargin)&')'
83580   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68+addr_indent)&','&str$(lyne*8+ymargin)&')'
83600   pr #20: 'Call Print.AddText("'&env$('cnam')(1:27)&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*9+ymargin)&')'
83620   fnpa_fontsize
83640   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+addr_indent)&','&str$(lyne*11+ymargin)&')'
83660   if budget>0 then 
83680     pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*11+ymargin)&')'
83700   else 
83720     pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*11+ymargin)&')'
83740     pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+addr_indent)&','&str$(lyne*12+ymargin)&')'
83760     pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*12+ymargin)&')'
83780   end if 
83800   fnpa_fontsize(9)
83820   addy=12
83840   fnpa_txt(mg$(1),xmargin+4,(addy+=1)*lyne+ymargin+30)
83900   addy+=1
83920   fnpa_fontsize
83940   if df$="Y" then 
83960     fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
83980   end if 
84000   if c4>0 then 
84020     fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
84040   end if 
84060   pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68+addr_indent+30)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
84080   fnpa_txt(trim$(pe$(1)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
84100   fnpa_txt(trim$(pe$(2)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
84120   fnpa_txt(trim$(pe$(3)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
84140   fnpa_txt(trim$(pe$(4)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
84160   pr #20: 'Call Print.AddText("Return Service Requested.",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=2)+ymargin+20+addr_down)&')'
84180   if checkcounter=1 then checkx=1.375 : checky=3.6875
84200   if checkcounter=2 then checkx=6.75 : checky=3.6875
84220   if checkcounter=3 then checkx=1.375 : checky=7.9375
84240   if checkcounter=0 then checkx=6.75 : checky=7.9375
84260   if checkcounter=0 then 
84280     fnpa_newpage
84300   end if 
84320   ! /r
84340 fnend 
86000 def fn_print_bill_blucksberg(z$,mat mg$) ! inherrits all those local customer variables too
86010   if ~setup_blucksberg then ! r:
86020     setup_blucksberg=1
86030     lyne=3
86040     addr_indent=8
86050     addr_down=3
86060      ! 
86070     fn_get_mat_at(mat at$)
86080     ! 
86090   end if  ! /r
86100   ! r: pr that bill
86110   ! AFTER_READ_CUSTOMER: !
86120   ! gosub READALTADR
86130   let pb=bal-g(11)
86140   if bal<=0 then let g(9)=0 ! don't show penalty if balance 0 or less
86150   activity_charge=fntrans_total_as_of(z$,billing_date_prior,1)
86160   activity_penalty=fntrans_total_as_of(z$,billing_date_prior,2)
86170   activity_payment=fntrans_total_as_of(z$,billing_date_prior,3)
86180   activity_credit=fntrans_total_as_of(z$,billing_date_prior,4)
86190   activity_debit=fntrans_total_as_of(z$,billing_date_prior,5)
86200   let prior_prior_balance=bal ! -g(11)
86210   let prior_prior_balance=prior_prior_balance-activity_charge
86220   let prior_prior_balance=prior_prior_balance-activity_penalty
86230   let prior_prior_balance=prior_prior_balance+activity_payment
86240   let prior_prior_balance=prior_prior_balance+activity_credit
86250   let prior_prior_balance=prior_prior_balance-activity_debit
86260   ! 
86270   ! 
86280   ! 
86290   ! 
86300   ! -- Printer Program for Laser 1-Per Page Utility Bills
86310   gosub PRIOR_USAGES
86320   ! pr #20: 'Call Print.AddPicture("Monticello.jpg",82,1)'
86330   fnpa_fontsize
86340   ! fnpa_txt("Blucksberg Mtn Water Association",158)
86350   ! fnpa_txt("8077 Blucksberg Drive",15,13)
86360   ! fnpa_txt("Sturgis, SD 57785",15,18)
86370   fnpa_txt(trim$(pe$(1)),22,49)
86380   fnpa_txt(trim$(pe$(2)),22,54)
86390   if trim$(pe$(3))="" then let pe$(3)=pe$(4): let pe$(4)=""
86400   fnpa_txt(trim$(pe$(3)),22,59)
86410   fnpa_txt(trim$(pe$(4)),22,64)
86420   ! fnpa_fontsize(18)
86430   ! fnpa_fontbold(1)
86440   fnpa_pic('S:\acsub\logo_blucksberg.jpg',124,13)
86450   fnpa_elipse(147,24,38,.5)
86460   fnpa_elipse(147,24,37,.5)
86470   ! fnpa_fontbold
86480   ! fnpa_fontitalic(1)
86490   ! let fnpa_txt("Blucksberg Mtn",119,14)
86500   ! fnpa_fontsize(34)
86510   ! let fnpa_txt("Water",126,20)
86520   ! fnpa_fontsize(14)
86530   ! let fnpa_txt("Association",128,31)
86540   fnpa_fontitalic(0)
86550   fnpa_fontsize(9)
86560   let tmp_box_top=55
86570   fnpa_line(tmp_box_left_pos=115,tmp_box_top,70,24, 1)
86580   fnpa_txt('Billing Date:            '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),tmp_box_left_pos+5,tmp_box_top+4)
86590   fnpa_txt("Account:      "&lpad$(trim$(z$),19),tmp_box_left_pos+5,tmp_box_top+8)
86600   fnpa_txt('Due Date:                '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),tmp_box_left_pos+5,tmp_box_top+12)
86610   fnpa_txt("Billing Questions:   605-720-5013",tmp_box_left_pos+5,tmp_box_top16)
86620   if final>0 then let fnpa_txt('Final Bill'&cnvrt$("PIC(ZZzZZzZZ)",0),80,tmp_box_top+15)
86630   ! 
86640   lyne=65 : adder=4
86650   fnpa_txt("Meter Location: "&trim$(e$(1)) ,23,lyne+=adder)
86660   fnpa_txt("Service From: "&cnvrt$("pic(zz/zz/zz)",d5)&" To: "&cnvrt$("pic(zz/zz/zz)",d6) ,23,lyne+=adder)
86670   ! 
86680   fnpa_line(26,85,157)
86690   ! 
86700   fnpa_fontsize
86710   fnpa_fontitalic(1)
86720   fnpa_fontbold(1)
86730   ! 
86740   lyne=81
86750   adder=4.5
86760   fnpa_fontbold(1) : let fnpa_fontitalic
86770   fnpa_txt("Activity Since "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),80,lyne+=adder)
86780   fnpa_fontbold(0)
86790   fnpa_txt("Amount",170,lyne)
86800   fn_add_activity_line("Balance as of "&date$(days(billing_date_prior,'ccyymmdd'),'mm/dd/yy'),prior_prior_balance)
86810   fn_add_activity_line("Charges",activity_charge-g(11))
86820   fn_add_activity_line("Penalties",activity_penalty)
86830   fn_add_activity_line("Payments Received - Thank You",-activity_payment)
86840   fn_add_activity_line("Credits",-activity_credit)
86850   fn_add_activity_line("Debits",activity_debit)
86860   fnpa_line(162,lyne+4,22)
86870   fnpa_fontbold(1) ! on
86880   fn_add_activity_line("Balance Forward",pb,1,110)
86890   lyne+=adder
86900   ! let fnpa_fontbold(1) ! on
86910   fnpa_line(26,lyne+=adder,157)
86920   fnpa_txt("Current Charges",90,lyne+=1)
86930   ! let fnpa_txt("Current Charges",30,lyne+=8)
86940   fnpa_fontbold
86950   ! adder=5
86960   fnpa_fontitalic
86970   adder=4
86980   lyne+=adder
86990   fnpa_txt("Current",83,lyne) ! lyne=100
87000   fnpa_txt("Reading",83,lyne+adder)
87010   fnpa_txt("Previous",103,lyne)
87020   fnpa_txt("Reading",103,lyne+adder)
87030   fnpa_txt("Usage",131,lyne+adder)
87040   fnpa_txt("Charge",170,lyne+adder)
87050   adder=4 ! maybe it should be 4.5
87060   lyne+=adder ! lyne=105
87070   if g(1)<>0 then 
87080     if g(1)>=14 then 
87090       fnpa_txt("Base Water Service Fee",26,lyne+=adder)
87100       fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",14),160,lyne)
87110     end if 
87120     fnpa_txt("Water",26,lyne+=adder)
87130     fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(1)), 79,lyne)
87140     fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(2)),103,lyne)
87150     fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(3)),123,lyne)
87160     fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",g(1)-14),160,lyne)
87170   end if 
87180   ! 
87190   if g(2)<>0 then 
87200     fnpa_txt("Sewer",26,lyne+=adder)
87210     if seweravg>0 then ! if have sewer average, use it for usage
87220       fnpa_txt(cnvrt$("pic(zzzzzzzz#)",seweravg),121,lyne)
87230     else !  use water usage
87240       fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(2)),121,lyne)
87250     end if 
87260     fnpa_txt(cnvrt$("pic(--------.##)",g(2)),160,lyne)
87270   end if 
87280   ! 
87290   if g(3)<>0 then 
87300     fnpa_txt("Association Fee *",26,lyne+=adder)
87310     fnpa_txt(cnvrt$("pic($$$$$$$$.##)",g(3)),160,lyne)
87320   end if 
87330   ! 
87340   if g(4)<>0 then 
87350     fnpa_txt("Gas",26,lyne+=adder)
87360     fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(9)) ,078,lyne)
87370     fnpa_txt(cnvrt$("pic(zzzzzzzz#)",d(11)) ,121,lyne)
87380     fnpa_txt(cnvrt$("pic(--------.##)",g(4)),160,lyne)
87390   end if 
87400   ! 
87410   ! g(5)
87420   ! g(6)
87430   ! 
87440   if g(7)<>0 then 
87450     fnpa_txt(servicename$(7),26,lyne+=adder)
87460     fnpa_txt(cnvrt$("pic($$$$$$$$.##)",g(7)),160,lyne)
87470   end if 
87480   ! 
87490   ! g(8)
87500   ! 
87510   if g(9)<>0 then 
87520     fnpa_txt("Tax",26,lyne+=adder)
87530     fnpa_txt(cnvrt$("pic(--------.##)",g(9)),160,lyne)
87540   end if 
87550   ! 
87560   fnpa_line(162,lyne+4,22) : lyne+=1
87570   fnpa_fontbold(1)
87580   fn_add_activity_line("Total Current Charges",g(11), 1,110)
87590   ! lyne+=adder ! let fnpa_txt("Total Current Charges",110,lyne+=adder)
87600   ! let fnpa_txt(cnvrt$("pic(--------.##)",g(11)),160,lyne)
87610   fnpa_fontbold(0)
87620   lyne+=adder
87630   fnpa_line(162,lyne+3,22)
87640   fnpa_fontsize(14)
87650   fnpa_fontbold(1)
87660   fnpa_txt("Total Due",105,lyne+=adder)
87670   fnpa_txt(cnvrt$("pic($$$$$$$$.## CR)",bal),150,lyne)
87680   fnpa_line(162,lyne+=adder+1,22)
87690   fnpa_line(162,lyne+=1,22)
87700   fnpa_fontsize
87710   if uprc$(df$)="Y" then let fnpa_txt("Your bill has been scheduled for automatic withdrawal",85,lyne+=adder)
87720   fnpa_fontsize
87730   fnpa_fontbold
87740   if g(3)>0 then 
87750     fnpa_txt('* Road maintenance and snow removal from main roads, vehicle and equipment',25,lyne+=adder)
87760     fnpa_txt('repair and maintenance',25,lyne+=adder)
87770   end if 
87780   fnpa_fontbold(1)
87790   fnpa_line(26,165,157)
87800   fnpa_fontsize
87810   fnpa_fontitalic(1)
87820   lyne=165
87830   fnpa_txt("MESSAGE BOARD",92,lyne+=4)
87840   for j=1 to 13
87850     fnpa_txt(mg$(j),5,lyne+=4)
87860   next j
87870   fnpa_fontitalic
87880   let x=0
87890   for j=1 to 39
87900     fnpa_line(x+=5,234,3,0) ! pr #20: 'Call Print.AddLine('&str$(x+=5)&','&str$(234)&',3,0)'
87910   next j
87920   pr #20: 'Call Print.MyFontSize(7)'
87930   fnpa_txt("Please detach here and return with payment.  Mail to 8077 Blucksberg Dr or deposit in black box at bus stop.",18,236)
87940   fnpa_fontsize
87950   fnpa_txt("Account: "&lpad$(trim$(z$),16),40,243)
87960   fnpa_txt('Due Date:        '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),40,247)
87970   fnpa_txt("Total Due:",40,251)
87980   fnpa_txt(cnvrt$("pic(--------.##)",bal),70,251)
87990   if bal>0 then 
88000    !   fnpa_txt("After "&cnvrt$("pic(##/##/##)",d4)&" pay "&cnvrt$("pic(---.##)",g(12)),40,255)
88010     fnpa_txt("After "&cnvrt$("pic(##/##/##)",d4)&" Add "&cnvrt$("pic(---.##)",2.50),40,255)
88020   end if 
88030   fnpa_txt(trim$(pe$(1)),130,243)
88040   fnpa_txt(trim$(pe$(2)),130,247)
88050   if trim$(pe$(3))="" then let pe$(3)=pe$(4) : let pe$(4)=""
88060   fnpa_txt(trim$(pe$(3)),130,251)
88070   fnpa_txt(trim$(pe$(4)),130,255)
88080   fnpa_newpage
88090   ! /r
88100 fnend 
89000 def fn_print_bill_omaha(z$,mat mg$,mat mg2$,pbo_service_from,pbo_service_to,mat penalty$)
89010 ! correct margins are top=.2, bottom=.2,left=.4,right=.2
89020 ! r: any and all necessary setup (except opening the printer) to pr one bill
89030   if ~pbomaha_setup then 
89040     let pbomaha_setup=1
89050     open #h_pbomaha_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
89060     F_PBOMAHA_CUSTOMER: form pos 1,c 10,c 30,x 90,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1854,pd 5.2
89062     let pboposrightcol=70
89070   end if 
89080   read #h_pbomaha_customer,using F_PBOMAHA_CUSTOMER,key=z$: z$,pbomaha_meter_address$,f$,a3,mat b,final,mat d,bal,f,mat g,mat gb,route,extra_3,extra_4
89090   dim pbomaha_meter_address$*30 ! formerly e$(2)
89100   fncustomer_address(z$,mat pe$)
89110 ! /r
89130   if final=2 then 
89140     let g(8)-=b(8): let g(11)=g(12)+g(8): bal+=g(8)
89150   end if 
89160   let penalty=0
89170   for j=1 to 10
89180     if penalty$(j)="Y" then 
89190       let penalty+=g(j)
89200       let g(j)=0 ! accumulate all penalties and set charge to zero
89210     end if 
89220   next j
89230   let pb=bal-g(11)
89240   bills=bills+1
89250   if bills=1 then 
89260     pr #255: 
89270     pr #255: 
89280     pr #255: 
89290     pr #255: 
89300   else if bills=2 then 
89310     pr #255: 
89320     pr #255: 
89330     pr #255: 
89340     pr #255: 
89350     pr #255: 
89360     pr #255: 
89370     pr #255: 
89380     pr #255: 
89390   else if bills=3 then 
89400     pr #255: 
89410     pr #255: 
89420     pr #255: 
89430     pr #255: 
89440     pr #255: 
89450     pr #255: 
89460     pr #255: 
89470     pr #255: 
89480   end if 
89490   pr #255,using 'form pos 25,pic(##/##/##),pos pboPosRightCol,c message2_max_len': d1,fn_mg2$(1)
89500   pr #255,using 'form pos 4,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##)': "From",int(pbo_service_from*.01),"To",int(pbo_service_to*.01)
89510   pr #255: ""
89520   if pb<>0 then let pb$="   Prior Balance" else let pb$=""
89530   pr #255: ""
89540   if g(1)=0 then let t$="" else let t$="Wtr"
89550   pr #255,using PBO_F_ONE: t$,0,d(1),d(3),g(1),0,0,fn_mg2$
89560   PBO_F_ONE: form pos 4,c 3,nz 1,nz 8,nz 8,nz 9.2,x 4,nz 10.2,nz 12.2,pos pboposrightcol,c message2_max_len
89570   if g(2)=0 then let t$="" else let t$="Swr"
89580   if bal<=0 then 
89590     pr #255,using PBO_F_ONE: t$,0,0,0,g(2),0,bal,fn_mg2$
89600   else 
89610     pr #255,using PBO_F_ONE: t$,0,0,0,g(2),0,0,fn_mg2$
89620   end if 
89630   if g(3)=0 then let t$="" else let t$="Pri"
89640   pr #255,using PBO_F_ONE: t$,0,0,0,g(3),bal+penalty,bal,fn_mg2$
89650   if g(5)=0 then let t$="" else let t$="W/F"
89660   pr #255,using PBO_L1610: t$,0,0,0,g(5),d4,fn_mg2$
89670   PBO_L1610: form pos 4,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,pic(zz/zz/zz),pos pboposrightcol,c message2_max_len
89680   if g(9)=0 then let t$="" else let t$="Tax"
89690   pr #255,using PBO_F_ONE: t$,0,0,0,g(9),0,0,fn_mg2$
89700   pr #255,using PBO_L1620: pb$,pb,z$,fn_mg2$
89710   PBO_L1620: form pos 6,c 17,nz 10.2,pos 36,c 10,pos pboposrightcol,c message2_max_len
89720   if g(4)=0 then let t$="" else let t$="SF "
89730   pr #255,using PBO_F_ONE: t$,0,0,0,g(4),0,0,fn_mg2$
89740   if d(10)=1 then est$="Bill Estimated" else est$=""
89750   if c4>0 then let final$="Final Bill" else let final$=""
89760   if df$="Y" then let final$="Drafted"
89770   if bal<=0 then let penalty=g(10)=0
89790   pr #255,using 'form pos 10,c 20,pos 38,c 25,pos pboPosRightCol,c message2_max_len': est$,pe$(1)(1:25),fn_mg2$
89800   if bal<=0 then 
89810     pr #255,using PBO_L1810: 0,d4,bal,pe$(2)(1:25),fn_mg2$
89820   else 
89830     pr #255,using PBO_L1810: bal+penalty,d4,bal,pe$(2)(1:25),fn_mg2$
89840   end if 
89850   PBO_L1810: form pos 4,nz 7.2,x 1,pic(zz/zz/zz),nz 13.2,pos 38,c 25,pos pboposrightcol,c message2_max_len
89860   pr #255,using 'Form pos 16,c 18,pos 38,c 25,pos pboPosRightCol,c message2_max_len': e$(1)(1:18),pe$(3)(1:25),fn_mg2$
89870   pr #255,using 'Form pos 5,c 10,x 5,c 10,pos 38,c 25,pos pboPosRightCol,c message2_max_len': z$,final$,pe$(4)(1:25),fn_mg2$
89880   pr #255,using "Form POS 5,C 30,pos pboPosRightCol,c message2_max_len": mg$(1),fn_mg2$
89890   pr #255,using "Form POS 5,C 30,pos pboPosRightCol,c message2_max_len": mg$(2),fn_mg2$
89910   if bills=3 then ! Bottom of Page
89920     bills=0
89930     pr #255: newpage
89940   else 
89950     if allign<>1 then ! it isn't a REPRINT so SKIP TOTALS
89960       billsPrintedCount(3)=billsPrintedCount(3)+1
89970     end if 
89980   end if 
89990 fnend 
90520 def fn_add_activity_line(aal_text$*80,aal_amt; aal_always_show,aal_desc_left_override)
90540   if aal_desc_left_override=0 then aal_desc_left_override=30
90560   if aal_always_show or aal_amt<>0 then 
90580     fnpa_txt(aal_text$,aal_desc_left_override,lyne+=adder)
90600     fnpa_txt(cnvrt$("pic(-------#.## CR)",aal_amt),160,lyne)
90620   end if 
90640 fnend 
91000 PRIOR_USAGES: ! r:
91020   mat usage=(0): mat billdate=(0) : mat reads=(0)
91040   restore #h_ubtransvb,key>=z$&"         ": nokey PU_XIT ! no average but active customer (use 0 usage)
91060 L3160: read #h_ubtransvb,using L3170: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PU_XIT
91080 L3170: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
91100   if p$<>z$ then goto PU_XIT
91120   if tcode<>1 then goto L3160 ! only charge transactions
91140   let usage(3)=usage(2): billdate(3)=billdate(2) : let reads(3)=reads(2)
91160   let usage(2)=usage(1): billdate(2)=billdate(1) : let reads(2)=reads(1)
91180   let usage(1)=wu: billdate(1)=tdate : let reads(1)=wr
91200   goto L3160
91220 PU_XIT: ! 
91240   return  ! /r
92000 def fn_print_bill_pennington(z$,mat mg$,mat mg2$,service_from,service_to,penaltyDueDate)
92010   ! correct margins are top:.7, bottom:.25, left:.63, right:.25
92020   ! r: any and all necessary setup (except opening the printer) to pr one bill
92030     if ~pbpennington_setup then 
92040       let pbpennington_setup=1
92050       open #h_pbpennington_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
92060     end if 
92090     read #h_pbpennington_customer,using f_pbpennington,key=z$: z$,a4,mat b,mat d,bal,f,mat g,extra_3,extra_4
92100     f_pbpennington: form pos 1,c 10,pos 149,pd 2,pos 157,11*pd 4.2,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 1750,2*n 6
92120     fncustomer_address(z$,mat gTmpCustomerAddress$)
92125     fn_override_service_date(service_from,service_to,extra_3,extra_4)
92130     let pb=bal-g(11)
92180   ! /r
92190   pr #255,using 'form pos 45,c 9,n 10.2': 'Water',g(1)+g(9)
92200   pr #255,using 'form pos 45,c 9,pic(zzz,zzz.zz)': '',g(2)
92202   if service_to=0 then let tmpServiceFrom$='' else tmpServiceFrom$=date$(days(service_from,'mmddyy'),'mm dd') ! d3
92204   if service_to=0 then let tmpServiceTo$='' else tmpServiceTo$=date$(days(service_to,'mmddyy'),'mm dd') ! d4
92210   pr #255,using 'form pos 11,c 5,pos 20,c 5,pos 30,pic(zzbzzbzz)': tmpServiceFrom$,tmpServiceTo$,date$(days(penaltyDueDate,'mmddyy'),'mm dd yy')
92220   pr #255: ""
92230   pr #255,using 'form pos 48,n 9.2,n 10.2': bal,bal+g(10)+g(7)
92240   pr #255: ""
92250   pr #255,using 'form pos 9,n 8,n 8,n 7,n 9.2,pos 45,c 30': d(2),d(1),int(d(3)/100),g(1),z$
92260   pbpennington_L810: form pos 45,c 30
92270   pbpennington_L820: form pos 26,c 5,pos 32,n 9.2,pos 45,c 30
92280   if g(2)=0 then 
92290     pr #255,using pbpennington_L810: gTmpCustomerAddress$(1) ! e$(2)
92300   else 
92310     pr #255,using pbpennington_L820: "Sewer",g(2),gTmpCustomerAddress$(1) ! e$(2)
92320   end if 
92330   if g(5)=0 then 
92340     pr #255,using pbpennington_L810: gTmpCustomerAddress$(2) ! e$(3)
92350   else 
92360     pr #255,using pbpennington_L820: "Sanit",g(5),gTmpCustomerAddress$(2) ! e$(3)
92370   end if 
92380   if g(6)=0 then 
92390     pr #255,using pbpennington_L810: gTmpCustomerAddress$(3) ! e$(4)
92400   else 
92410     pr #255,using 'form pos 11,C 20,Pos 32,n 9.2,pos 45,c 30': "Business License Fee",g(6),gTmpCustomerAddress$(3) ! e$(4)
92420   end if 
92430   if g(8)=0 then 
92440     pr #255,using pbpennington_L810: gTmpCustomerAddress$(4) ! csz$
92450   else 
92460     pr #255,using pbpennington_L820: "Other",g(8),gTmpCustomerAddress$(4) ! csz$
92470   end if 
92480   if pb=0 then 
92490     pr #255,using pbpennington_L810: ''
92500   else 
92510     pr #255,using pbpennington_L820: "Prev",pb,''
92520   end if 
92530   pr #255: ""
92540   pr #255,using 'form pos 32,n 9.2,pos 45,C 40': g(9),mg$(1)
92550   pr #255,using 'form pos 45,C 40': mg$(2)
92560   pr #255,using 'form pos 8,n 10.2,x 3,c 10,pos 32,n 9.2,pos 45,C 40': bal+g(10)+g(7),z$,bal,mg$(3)
92570   count+=1
92580   if count=1 or count=2 then 
92590     for j=1 to 8
92600       pr #255: ''
92610     next j
92620   else if count=3 then 
92630     pr #255: newpage
92640     count=0
92650   end if 
92660 fnend 
94000 def fn_print_bill_edinburg(z$,mat mg$,d1,service_from,service_to,penaltyDueDate)
94005 ! correct margins are ??
94010 ! r: any and all necessary setup (except opening the printer) to pr one bill
94015     if ~pbedinburg_setup then 
94020       let pbedinburg_setup=1
94025       open #h_pbedinburg_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
94030       F_PBedinburg_CUSTOMER: form pos 1,c 10,pos 147,pd 2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 1741,n 2,pos 1750,2*n 6
94032       lyne=3
94035     end if 
94036 !   read #1,using L590,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$ nokey SCREEN3
94037 !   L590: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30
94040     read #h_pbedinburg_customer,using F_PBedinburg_CUSTOMER,key=z$: z$,a3,final,mat d,bal,f,mat g,route,extra_3,extra_4
94050     fncustomer_address(z$,mat pe$)
94052     fn_override_service_date(service_from,service_to,extra_3,extra_4)
94055 ! /r
94060   checkcounter+=1
94065   if checkcounter=1 then let xmargin=0 : let ymargin=0
94070   if checkcounter=2 then let xmargin=139 : let ymargin=0
94075   if checkcounter=3 then let xmargin=0 : let ymargin=108
94080   if checkcounter=4 then let xmargin=139 : let ymargin=108 : checkcounter=0
94085   fnpa_line(xmargin+5,ymargin+2,55,lyne*3+3,1)
94090   fnpa_fontbold(1)
94095   fnpa_fontsize(12)
94100   fnpa_font
94105   fnpa_txt("Village of Edinburg",xmargin+8,lyne*1-1+ymargin)
94110   fnpa_font('Lucida Console')
94115   fnpa_fontsize
94120   fnpa_fontbold
94125   fnpa_txt("     P O Box 350    ",xmargin+6,lyne*2+1+ymargin-.2)
94130   fnpa_txt("  Edinburg, IL 62531    ",xmargin+6,lyne*3+1+ymargin)
94135   fnpa_txt('#'&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
94140   fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
94145   fnpa_txt('From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_from)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_to),xmargin+2,lyne*7+ymargin)
94150   fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
94155   fnpa_txt('Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),xmargin+2,lyne*11+ymargin)
94160   fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
94165   fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
94170   fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
94175   fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)
94180 ! ______________________________________________________________________
94185 ! PRINTGRID: !
94190   let meter=14 
94195   fnpa_fontsize(8)
94200   if g(1)<>0 then 
94205     fnpa_txt("WTR",xmargin+1,lyne*(meter+=1)+ymargin)
94210     pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' 
94215     pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' 
94220     pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94225   end if
94230   if g(2)<>0 then 
94235     fnpa_txt("SWR",xmargin+1,lyne*(meter+=1)+ymargin)
94240     pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94245   end if
94246   if g(4)<>0 then 
94250     if a4=1 then 
94255       s4code$="RSGS" 
94260     else if a4=2 then 
94265       s4code$="CMGS" 
94270     else if a4=3 then 
94275       s4code$="INGS" 
94280     else 
94285       s4code$="GAS"
94290     end if
94300     fnpa_txt(s4code$,xmargin+1,lyne*(meter+=1)+ymargin)
94305     pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' 
94310     pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' 
94315     pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94320   end if
94325   if g(3)<>0 then 
94330     fnpa_txt('WF',xmargin+1,lyne*(meter+=1)+ymargin) 
94335     pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94340   end if
94345   if g(8)<>0 then 
94350     fnpa_txt("MISC",xmargin+1,lyne*(meter+=1)+ymargin)
94355     pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94360   end if
94365   if g(9)<>0 then 
94370     fnpa_txt("TAX",xmargin+1,lyne*(meter+=1)+ymargin)
94375     pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94380   end if
94385   if g(10)<>0 then 
94390     fnpa_txt("SF",xmargin+1,lyne*(meter+=1)+ymargin)
94395     pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94400   end if
94405   if pb<>0 then 
94410     fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
94415     pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
94420   end if
94425   fnpa_fontsize
94430 ! ______________________________________________________________________
94435   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
94440   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",penaltyDueDate)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
94445   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
94450   pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",penaltyDueDate)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
94455   if bal>0 and g(5)+g(6)+g(7)>0 then let penalty=round(bal*.1,2) else let penalty =0
94460   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+penalty,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin)&')'
94465   pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
94470   pr #20: 'Call Print.AddText("Phone: 217-623-5542",'&str$(xmargin+1)&','&str$(lyne*27+ymargin)&')'
94475 ! ______________________________________________________________________
94480 ! special=28
94485 ! ______________________________________________________________________
94490   fnpa_fontsize(7)
94495   pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
94500   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
94505   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
94510   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
94515   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
94520   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
94525   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
94530   pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
94535   pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
94540   pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
94545   pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
94550   pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
94555   pr #20: 'Call Print.AddText("  Permit No 12",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
94560   fnpa_fontsize(9)
94565 ! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
94570   pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
94575   pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
94580   pr #20: 'Call Print.AddText("Village of Edinburg",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
94585   fnpa_fontsize
94590   pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",penaltyDueDate)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
94595   pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
94600   pr #20: 'Call Print.AddText("After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",penaltyDueDate)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
94605   pr #20: 'Call Print.AddText("'&fnformnumb$(bal+penalty,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
94610   fnpa_fontsize(9)
94615   addy=14
94620   fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
94625   fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
94630   fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
94635   addy+=1
94640   fnpa_fontsize
94645   if df$="Y" then 
94650     fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
94655   end if
94660   if final>0 then 
94665     fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
94670   end if
94675   if d(10)=1 then 
94680     fnpa_txt("Bill Estimated",xmargin+1,lyne*(addy+=1)+ymargin)
94685   end if
94690   fnpa_txt('#'&trim$(z$)&' '&bulk$,xmargin+68,lyne*(addy+=1)+ymargin)
94695   if pe$(1)<>"" then 
94700     fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
94705   end if
94710   if pe$(2)<>"" then 
94715     fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
94720   end if
94725   if pe$(3)<>"" then 
94730     fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
94735   end if
94740   if pe$(4)<>"" then 
94745     fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
94750   end if
94755   if checkcounter=1 then checkx=1.375 : checky=3.6875
94760   if checkcounter=2 then checkx=6.75 : checky=3.6875
94765   if checkcounter=3 then checkx=1.375 : checky=7.9375
94770   if checkcounter=0 then checkx=6.75 : checky=7.9375
94775   if checkcounter=0 then 
94780     fnpa_newpage
94785   end if
94790 fnend 
95000 def fn_print_bill_standard_pdf_a(z$,mat mg$; mat mg2$,enableIsDueNowAndPayable,enableReturnServiceRequested) ! inherrits all those local customer variables too
95010 ! based of fn_print_bill_raymond, but enhanced 
95020   if ~setup_standardA then 
95030     setup_standardA=1
95040     lyne=3
95050     fn_get_mat_at(mat at$)
95060   end if 
95070   subtotalAmt=g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9)
95072   payLateAmount=0
95080   if penaltyFlatAmount then
95090     if bal>0 then payLateAmount=bal+penaltyFlatAmount
95100   else if basePenaltyOnCurrentBillOnly then
95110     payLateAmount=bal+round(subtotalAmt*.10,2)
95120   else ! base Penalty on full amount 
95130     payLateAmount=bal+round(bal*.10,2)
95140   end if
95150   ! -- Standard 4 Per Page Even Perferated Card Stock Bills
95160   checkcounter+=1
95170   if checkcounter=1 then let xmargin=0 : let ymargin=0
95180   if checkcounter=2 then let xmargin=139 : let ymargin=0
95190   if checkcounter=3 then let xmargin=0 : let ymargin=108
95200   if checkcounter=4 then let xmargin=139 : let ymargin=108 : checkcounter=0
95210   ! ______________________________________________________________________
95220   fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3,1)
95230   fnpa_fontbold(1)
95240   fnpa_fontsize(12)
95250   fnpa_font
95260   fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin)
95270   fnpa_font("Lucida Console")
95280   fnpa_fontsize
95290   fnpa_fontbold
95300   fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
95310   fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin)
95320   fnpa_txt(poundBeforeAccount$&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
95330   fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
95340   fnpa_txt('From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3),xmargin+2,lyne*7+ymargin)
95350   if enableIsDueNowAndPayable>0 then
95360     fnpa_txt("Is due now and payable.",xmargin+2,lyne*8+ymargin)
95370   end if
95380   fnpa_txt('Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1),xmargin+2,lyne*11+ymargin)
95390   fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
95400   fnpa_txt("Reading",xmargin+10,lyne*13+ymargin)
95410   fnpa_txt("Usage",xmargin+33,lyne*13+ymargin)
95420   fnpa_txt("Charge",xmargin+50,lyne*13+ymargin)
95430   ! ______________________________________________________________________
95440   ! PRINTGRID: !
95450   let meter=14
95460   fnpa_fontsize(8)
95470   if g(1)<>0 then 
95480     fnpa_txt(servicecode$(1),xmargin+1,lyne*(meter+=1)+ymargin)
95490     fnpa_txt(fnformnumb$(d(1),0,9),xmargin+6 ,lyne*meter+ymargin)
95500     fnpa_txt(fnformnumb$(d(3),0,9),xmargin+25,lyne*meter+ymargin)
95510     fnpa_txt(fnformnumb$(g(1),2,9),xmargin+45,lyne*meter+ymargin)
95520   end if 
95530   if g(2)<>0 then 
95540     fnpa_txt(servicecode$(2),xmargin+1,lyne*(meter+=1)+ymargin)
95550     fnpa_txt(fnformnumb$(g(2),2,9),xmargin+45,lyne*meter+ymargin)
95560   end if 
95570   if g(3)<>0 or d(7)<>0 then 
95580     if d(5)=0 and d(7)=0 then ! there are no readings nor usage - pr the name instead of the code
95590       fnpa_txt(servicename$(3),xmargin+1,lyne*(meter+=1)+ymargin)
95600     else
95610       fnpa_txt(servicecode$(3),xmargin+1,lyne*(meter+=1)+ymargin)
95620       fnpa_txt(fnformnumb$(d(5),0,9),xmargin+6,lyne*meter+ymargin)
95630       fnpa_txt(fnformnumb$(d(7),0,9),xmargin+25,lyne*meter+ymargin)
95640     end if
95650     fnpa_txt(fnformnumb$(g(3),2,9),xmargin+45,lyne*meter+ymargin)
95660   end if  ! g(3)<>0 or d(7)<>0
95670   if g(4)<>0 then 
95680     fnpa_txt(servicecode$(4),xmargin+1,lyne*(meter+=1)+ymargin)
95690     fnpa_txt(fnformnumb$(d(9),0,9),xmargin+6,lyne*meter+ymargin)
95700     fnpa_txt(fnformnumb$(d(11),0,9),xmargin+25,lyne*meter+ymargin)
95710     fnpa_txt(fnformnumb$(g(4),2,9),xmargin+45,lyne*meter+ymargin)
95720   end if 
95730   if g(5)<>0 then 
95740     fnpa_txt(servicename$(5),xmargin+1,lyne*(meter+=1)+ymargin)
95750     fnpa_txt(fnformnumb$(g(5),2,9),xmargin+45,lyne*meter+ymargin)
95760     ! fnpa_txt(fnformnumb$(g(5),2,9),xmargin+91+8,lyne*meter+ymargin)
95770   end if  ! g(5)<>0
95780   if g(6)<>0 then 
95790     fnpa_txt(servicename$(6),xmargin+1,lyne*(meter+=1)+ymargin)
95800     fnpa_txt(fnformnumb$(g(6),2,9),xmargin+43,lyne*meter+ymargin)
95810   end if  ! g(6)<>0
95820   if g(7)<>0 then 
95830     fnpa_txt(servicename$(7),xmargin+1,lyne*(meter+=1)+ymargin)
95840     fnpa_txt(fnformnumb$(g(7),2,9),xmargin+43,lyne*meter+ymargin)
95850   end if  ! g(7)=0
95860   if g(8)<>0 then 
95870     fnpa_txt(servicename$(8),xmargin+1,lyne*(meter+=1)+ymargin)
95880     fnpa_txt(fnformnumb$(g(8),2,9),xmargin+45,lyne*meter+ymargin)
95890   end if 
95900   if g(9)<>0 then 
95910     fnpa_txt(servicename$(9),xmargin+1,lyne*(meter+=1)+ymargin)
95920     fnpa_txt(fnformnumb$(g(9),2,9),xmargin+45,lyne*meter+ymargin)
95930   end if 
95940   if pb><0 then 
95950     ! if trim$(z$)='1900003.10' then pause
95960     fnpa_line(xmargin+46,lyne*(meter+=1)+ymargin,15,0)
95970     fnpa_txt("   Subtotal",xmargin+1,lyne*(meter+=.25)+ymargin)
95980     fnpa_txt(fnformnumb$(subtotalAmt,2,9),xmargin+45,lyne*meter+ymargin)
95990     fnpa_txt("Previous Balance",xmargin+1,lyne*(meter+=1)+ymargin)
96000     fnpa_txt(fnformnumb$(pb,2,9),xmargin+45,lyne*meter+ymargin)
96010   end if 
96020   fnpa_fontsize
96030   ! ______________________________________________________________________
96040   if estimatedate=d1 then let fnpa_txt("Bill estimated!",xmargin+1,lyne*21+ymargin)
96050   fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
96060   fnpa_txt('   Pay By  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*24+ymargin)
96070 ! let fnpa_txt("Pay By "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*24+ymargin)
96080   fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
96090   if payLateAmount>0 then
96100     fnpa_txt('Pay After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*25+ymargin)
96110     ! if basePenaltyOnCurrentBillOnly then
96120       fnpa_txt(fnformnumb$(payLateAmount,2,9),xmargin+42,lyne*25+ymargin)
96130     ! end if
96140   end if
96150   fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
96160   for satItem=1 to udim(mat mg2$)
96170     fnpa_txt(mg2$(satItem),xmargin+1,lyne*(26+satItem)+ymargin)
96180   nex satItem
96190   ! 
96200   fnpa_fontsize(7)
96210   fnpa_line(xmargin+97,ymargin+0,29,lyne*5+2,1)
96220   fnpa_line(xmargin+90,ymargin+0,7,0)
96230   fnpa_line(xmargin+90,ymargin+2.8,7,0)
96240   fnpa_line(xmargin+90,ymargin+5.6,7,0)
96250   fnpa_line(xmargin+90,ymargin+8.4,7,0)
96260   fnpa_line(xmargin+90,ymargin+11.2,7,0)
96270   fnpa_line(xmargin+90,ymargin+14,7,0)
96280   fnpa_line(xmargin+90,ymargin+17,7,0)
96290   fnpa_txt("   Pre-Sorted",xmargin+100,lyne*1-1+ymargin)
96300   fnpa_txt("First Class Mail",xmargin+100,lyne*2-1+ymargin)
96310   fnpa_txt("  U.S. Postage  ",xmargin+100,lyne*3-1+ymargin)
96320   fnpa_txt("      Paid",xmargin+100,lyne*4-1+ymargin)
96330   fnpa_txt("  Permit No "&str$(usPostagePermitNumber),xmargin+100,lyne*5-1+ymargin)
96340   fnpa_fontsize(9)
96350   if enableReturnServiceRequested>0 then
96360   fnpa_txt("Return Service Requested",xmargin+68+12,lyne*7.6+.2+ymargin-6)
96370   end if
96380   fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
96390   fnpa_txt("side with payment to:",xmargin+68,lyne*8+ymargin)
96400   fnpa_txt(env$('cnam'),xmargin+68,lyne*9+ymargin)
96410   fnpa_fontsize
96420   fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*11+ymargin)
96430   fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
96440   if payLateAmount>0 then
96450     fnpa_txt('After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*12+ymargin)
96460     fnpa_txt(fnformnumb$(payLateAmount,2,9),xmargin+106,lyne*12+ymargin)
96470   end if
96480   fnpa_fontsize(9)
96490   addy=14
96500   fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
96510   fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
96520   fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
96530   addy+=1
96540   fnpa_fontsize
96550   if df$="Y" then 
96560     fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
96570   end if 
96580   if c4>0 then 
96590     fnpa_txt("Final Bill",xmargin+1,lyne*(addy+=1)+ymargin)
96600   end if 
96610   fnpa_txt(poundBeforeAccount$&trim$(z$)&' '&bulk$,(xmargin+68),lyne*(addy+=1)+ymargin)
96620   if pe$(1)<>"" then 
96630     fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
96640   end if 
96650   if pe$(2)<>"" then 
96660     fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
96670   end if 
96680   if pe$(3)<>"" then 
96690     fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
96700   end if 
96710   if pe$(4)<>"" then 
96720     fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
96730   end if 
96740   if checkcounter=1 then checkx=1.375 : checky=3.6875
96750   if checkcounter=2 then checkx=6.75 : checky=3.6875
96760   if checkcounter=3 then checkx=1.375 : checky=7.9375
96770   if checkcounter=0 then checkx=6.75 : checky=7.9375
96780   bc$=""
96790   if trim$(bc$)<>"" then let fnpa_barcode(checkx,checky,bc$)
96800   if checkcounter=0 then 
96810     fnpa_newpage
96820   end if 
96830 fnend 
97000 def fn_print_bill_billings(mat mg$,mat g,mat b,bal,mat penalty$,d1,d2x,d3x,d4,mat pe$,final$,z$)
97010   ! three per page RTF Bill
97020   if final=2 then let g(8)-=b(8): let g(11)=g(12)+g(8): bal+=g(8)
97030   let penalty=0
97040   for j=1 to 10
97050     if penalty$(j)="Y" then let penalty+=g(j) : let g(j)=0 ! accumulate all penalties and set charge to zero
97060   next j
97070   let pb=bal-g(11)
97080   pr #255: ''
97090   pr #255,using 'form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##)': "FROM",int(d2x*.01),"TO",int(d3x*.01),d1
97100   pr #255,using 'form pos 1,c 10,pos 13,c 18': trim$(z$),e$(1)(1:18)
97110   pr #255: ''
97120   pr #255: ''
97130   if pb<>0 then let pb$="   PRIOR BALANCE" else let pb$=""
97140   if g(1)=0 then let t$="" else let t$="WTR"
97150   pr #255,using billings_fL1620: t$,0,d(1),d(3),g(1)
97160   billings_fL1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,skip 1
97170   billings_fL1630: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,pos 38,pic(zz/zz/zz),skip 1
97180   if g(2)=0 then let t$="" else let t$="SWR"
97190   pr #255,using billings_fL1620: t$,0,0,0,g(2)
97200   if g(3)=0 then let t$="" else let t$="PRI"
97210   pr #255,using billings_fL1620: t$,0,0,0,g(3)
97220   if g(4)=0 then let t$="" else let t$="SF "
97230   pr #255,using billings_fL1630: t$,0,0,0,g(4),d4
97240   if g(5)=0 then let t$="" else let t$="SOL"
97250   pr #255,using billings_fL1620: t$,0,0,0,g(5)
97260   if g(9)=0 then let t$="" else let t$="TAX"
97270   pr #255,using billings_fL1620: t$,0,0,0,g(9)
97280   if bal>0 then 
97290     pr #255,using billings_fL1590: pb$,pb,bal+penalty,bal 
97300     billings_fL1590: form pos 1,c 17,nz 10.2,pos 35,nz 10.2,pos 50,nz 10.2,skip 1
97310   else
97320     pr #255,using billings_fL1590: pb$,pb,0,bal
97330   end if
97340   if d(10)=1 then est$="BILL ESTIMATED" else est$=""
97350   if c4>0 then let final$="FINAL BILL" else let final$=""
97360   if df$="Y" then let final$="DRAFTED"
97370   if bal>g(11) then let final$="DELINQUENT NOTICE"
97380   if bal<=0 then let g(10)=0
97390   if g(8)=0 then let t$="" else let t$="OTH"
97400   pr #255,using billings_fL1620: t$,0,0,0,g(8)
97410   if bal<=0 then 
97420     pr #255,using billings_f1840: est$,trim$(z$),pe$(1),pe$(2),0,d4,bal,pe$(3),final$,pe$(4)
97430     billings_f1840: form pos 7,c 20,pos 37,c 30,skip 1,pos 37,c 30,skip 1,pos 37,c 30,skip 1,pos 1,nz 7.2,x 1,pic(zz/zz/zz),nz 13.2,pos 37,c 30,skip 1,c 30,pos 37,c 30
97440   else
97450     pr #255,using billings_f1840: est$,trim$(z$),pe$(1),pe$(2),bal+penalty,d4,bal,pe$(3),final$,pe$(4)
97460   end if
97470   pr #255,using 'form skip 1,c 30,skip 1,c 30,skip 1,c 30': mg$(1),mg$(2),mg$(3)
97480   bills=bills+1
97490   if int(bills/3)<>bills/3 then pr #255,using 'form pos 2,c 30,skip 2': ""
97500   if int(bills/3)=bills/3 then  ! BOTTOM OF PAGE
97510     pr #255: newpage
97520   end if
97530   billsPrintedCount+=1
97540 fnend
98000 def fn_print_bill_choctaw(z$,mat g,mat b,mat penalty$,d1,d2x,d3x,d4,mat e$,final)
98010 !    Good margins in Word (5/2/2017)  Top .4", Bottom, .5", Left 1.5", Right .25"
98020 !    Good margins in Word (5/2/2017)  Top .4", Bottom, .5", Left 1.5", Right .25"
98030   if final=2 then 
98040     let g(8)-=b(8): let g(11)=g(12)+g(8): bal+=g(8)
98050   end if
98060   let penalty=0
98070   for j=1 to 10
98080     if penalty$(j)="Y" then 
98090       let penalty+=g(j) 
98100       let g(j)=0 ! accumulate all penalties and set charge to zero
98110     end if
98120   next j
98130   let pb=bal-g(11)
98140   pr #255: ''
98150   pr #255,using 'form pos 4,c 10,pos 32,c 10,pos 48,pic(zz/zz/zz)': z$,z$,d4
98160   pr #255,using 'form pos 1,c 5,pic(zz/zz/zz),c 4,pic(zz/zz/zz)': "From:",d2x," To:",d3x
98170   pr #255,using 'form pos 32,n 8.2,pos 48,n 8.2': g(12),g(11)
98180   pr #255,using 'form pos 32,n 8.2,pos 48,n 8.2': pb,pb
98190   pr #255,using 'form pos 32,n 8.2,pos 48,n 8.2': bal+penalty,bal
98200   pr #255,using 'form pos 1,pic(zzzzzzzz),2*pic(zzzzzzzzz)': d(1),d(2),d(3)
98210   pr #255: ''
98220   pr #255: ''
98230   pr #255: ''
98240   if g(1)>0 then cde=1: let d4$=cnvrt$("pic(zz/zz/zz)",d4) else cde=0 : let d4$=""
98250   pr #255,using ce_L1690: "Water",g(1),d4$,e$(2)
98260   ce_L1690: form pos 1,c 5,pic(-----.--),pos 18,pic(zz/zz/zz),pos 32,c 30,skip 1
98270   if g(9)>0 then cde=2: let d4$=cnvrt$("pic(zz/zz/zz)",d4) else cde=0 : let d4$=""
98280   pr #255,using ce_L1690: "Tax",g(9),d4$,e$(3)
98290   if g(8)>0 then cde=3: let d4$=cnvrt$("pic(zz/zz/zz)",d4) else cde = 0 : let d4$=""
98300   if g(8)<>0 then let mis$="Misc" else let mis$=""
98310   pr #255,using ce_L1690: mis$,g(8),d4$,e$(4)
98320   pr #255,using 'form pos 3,c 30': e$(2)
98330   pr #255: ""
98340   pr #255,using 'form pos 1,n 8.2,pos 20,n 8.2': g(12),g(11)
98350   pr #255,using 'form pos 1,n 8.2,pos 20,n 8.2': pb,pb
98360   pr #255,using 'form pos 1,n 8.2,pos 20,n 8.2': bal+penalty,bal
98370   pr #255: ''
98380   let d2=d3=0
98390   bills+=1
98400   billOnPage+=1
98410   if billOnPage=1 or billOnPage=2 then ! int(bills/3)<>bills/3 then ! space extra if 1st or 2nd bill
98420     pr #255: ''
98430     pr #255: ''
98440     pr #255: ''
98450     pr #255: ''
98460     pr #255: ''
98470     pr #255: ''
98480     ! pr #255: '' ! microsoft word seemed to need an extra line
98490   ! end if
98500   ! ! If billOnPage=1 Then  pr #255,Using 1910: " " ! extra line after 1st bill
98510   else ! if int(bills/3)=bills/3 then 
98520     pr #255: newpage ! BOTTOM OF PAGE
98530     billOnPage=0
98540   end if ! 
98550 fnend
