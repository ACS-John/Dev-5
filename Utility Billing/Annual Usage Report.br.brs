10000 ! Formerly S:\acsUB\ubUsage2
10040 ! ______________________________________________________________________
10060   library 'S:\Core\Library': fnacs,fnmsgbox,fnopenprn,fncloseprn,fnerror,fnlbl,fntxt,fntos,fnxit,fncomboa,fncmdset,fntop,fndate_mmddyy_to_ccyymmdd,fnd1,fnreg_read,fnreg_write,fngethandle,fnget_services
10080   on error goto ERTN
12000   fntop(program$)
12020 ! r: set constants, dims, etc
12040   dim cd1(12),e$*30,u1(13)
12060   dim total_usage_route(13)
12080   dim total_usage_route_code_date(10,13)
12100   dim total_count_route(13)
12120   dim total_count_route_code_date(10,13)
12140 ! 
12160   dim total_usage_grand(13)
12180   dim total_usage_grand_code_date(10,13)
12200   dim total_count_grand(13)
12220   dim total_count_grand_code_date(10,13)
12260   dim msgline$(2)*40,tg(11)
12280 ! ______________________________________________________________________
14120 ! 
14180 ! 
14200   fnd1(d1)
14220   let magicdate=fndate_mmddyy_to_ccyymmdd(d1)-20000
14240 ! 
14260   dim servicename$(10)*20
14280   dim srv$(10)*2
14300   fnget_services(mat servicename$,mat srv$)
14360 ! 
14380   dim opt_service_to_analyze$(3)*20,opt_accum_type$(2)
14400   let opt_service_to_analyze$(1)="Water"
14420   if srv$(3)="EL" then let opt_service_to_analyze$(2)=servicename$(3)
14440   if srv$(4)="GA" then let opt_service_to_analyze$(3)=servicename$(4)
14460 ! 
14480   dim opt_accum_type$(2)*60
14500   let opt_accum_type$(accum_type_total:=1)='Total'
14520   let opt_accum_type$(accum_type_average:=2)='Average'
14540 ! /r
14560 ! r: open files
14580   open #h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
14600 ! open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed
14610   open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
14620 ! /r
25000 ! r: get default answers
25010   dim scr1_resp$(14)*80
25020   fnreg_read('Company.'&env$('cno')&'.ubusage2.last_run_date',last_run_date$)
25040   if last_run_date$<>'' and days(last_run_date$,'ccyy/mm/dd')+30>days(date$) then ! if run within the last 30 days do this
25060     for resp_item=1 to udim(mat scr1_resp$)
25080       fnreg_read('Company.'&env$('cno')&'.ubusage2.answer.'&str$(resp_item),scr1_resp$(resp_item))
25100     next resp_item
25120   else 
25140 BDR_READ: ! 
25160     read #h_customer,using 'form pos 1,c 10',release: z$ eof EO_BUILD_DEFAULT_RESP ! get the first account number
25180     restore #h_trans,key>=z$&"         ": nokey BDR_READ
25200 L230: ! 
25220     read #h_trans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof EO_BUILD_DEFAULT_RESP
25240     if p$<>z$ then goto L300 ! history record must belong to this customer
25260     if tcode<>1 then goto L230 ! charge transaction
25280     if tdate<magicdate then goto L230 ! only last two years
25300 ! L270: !
25320     let j=j+1
25340     if j>12 then goto EO_BUILD_DEFAULT_RESP
25360     let scr1_resp$(j)=str$(tdate)
25380     goto L230
25400 L300: ! 
25420     if scr1_resp$(1)="" then goto BDR_READ ! make sure this customer has current charges
25440 EO_BUILD_DEFAULT_RESP: ! 
25460   end if 
25480 ! /r
28000 SCREEN1: ! r:
28020   restore #h_customer: 
28040   fntos(sn$="ubusage2")
28060   let rc=0
28080   fnlbl(1,1,"Billing dates to be printed:",35,1)
28100   fntxt(2,1,10,10,0,"3")
28120   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28140   fntxt(2,15,10,10,0,"3")
28160   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28180   fntxt(2,29,10,10,0,"3")
28200   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28220   fntxt(2,43,10,10,0,"3")
28240   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28260   fntxt(2,57,10,10,0,"3")
28280   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28300   fntxt(2,71,10,10,0,"3")
28320   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28340   fntxt(4,1,10,10,0,"3")
28360   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28380   fntxt(4,15,10,10,0,"3")
28400   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28420   fntxt(4,29,10,10,0,"3")
28440   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28460   fntxt(4,43,10,10,0,"3")
28480   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28500   fntxt(4,57,10,10,0,"3")
28520   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28540   fntxt(4,71,10,10,0,"3")
28560   let rc+=1 : let scr1_resp$(rc)=scr1_resp$(rc)
28580   fnlbl(6,1,"Service to Analyze:",24,1,0)
28600   fncomboa("ubusage21",6,26,mat opt_service_to_analyze$,"",13)
28620   let rc+=1
28640   if trim$(scr1_resp$(rc))='' then let scr1_resp$(rc)=opt_service_to_analyze$(1)
28660   fnlbl(8,1,"Accumulation Type:",24,1,0)
28680   fncomboa("ubusage21",8,26,mat opt_accum_type$)
28700   let rc+=1
28720   if trim$(scr1_resp$(rc))='' then let scr1_resp$(rc)=opt_accum_type$(1)
28740   fncmdset(2)
28760   fnacs(sn$,0,mat scr1_resp$,ckey)
30000   if ckey=5 then goto XIT
30020   for j=1 to 12
30040 L560: ! 
30060     let x=pos(scr1_resp$(j),"/",1)
30080     if x>0 then let scr1_resp$(j)(x:x)="": goto L560
30100     cd1(j)=val(scr1_resp$(j)) conv INVALID_DATES_MSGBOX
30120   next j
30140   if cd1(1)=0 then goto INVALID_DATES_MSGBOX
30160   if scr1_resp$(13)="Water" then codepos=143: let service=1
30180   if scr1_resp$(13)=trim$(opt_service_to_analyze$(2)) then codepos=147: let service=3
30200   if scr1_resp$(13)=trim$(opt_service_to_analyze$(3)) then codepos=149 : let service=4
30220   accum_type=max(1,srch(mat opt_accum_type$,scr1_resp$(14)))
30240 ! /r
34000 ! r: save answers
34020   fnreg_write('Company.'&env$('cno')&'.ubusage2.last_run_date',date$('ccyy/mm/dd'))
34040   for resp_item=1 to udim(mat scr1_resp$)
34060     fnreg_write('Company.'&env$('cno')&'.ubusage2.answer.'&str$(resp_item),scr1_resp$(resp_item))
34080   next resp_item
34100 ! /r
36020   fnopenprn
36040 F_OUT: form pos 1,c 12,c 25,13*pic(----,---,---)
36060   gosub HDR
40000   do 
40020     read #h_customer,using F_CUSTOMER: z$,e$,s4_deposit_date,servicecode,route eof DONE
40040     if route<>route_prior and route_prior<>0 then 
40060       fn_print_total("Route "&str$(route_prior)&" Totals",mat total_usage_route,mat total_usage_route_code_date,mat total_count_route,mat total_count_route_code_date)
40080       pr #255: newpage
40100       mat total_usage_route(13)=(0)
40120       mat total_usage_route_code_date(10,13)=(0)
40140       mat total_count_route(13)=(0)
40160       mat total_count_route_code_date(10,13)=(0)
40180     end if 
40200     let route_prior=route
40220 F_CUSTOMER: form pos 1,c 10,x 30,c 30,pos 213,pd 4,pos codepos,pd 2,pos 1741,n 2
40240 !   restore #h_trans,key>=z$&"         ": nokey TRANS_NOKEY
40260     restore #h_trans,key>=z$&rpt$(chr$(0),9): nokey TRANS_NOKEY
40280     accum_average_divider=0
41000 READ_TRANSACTION: ! 
41020     read #h_trans,using F_TRANS: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof CUSTOMER_RECORD_FINIS
41040 F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
41060 ! if trim$(z$)='101379.00' and tdate=20141001 then pr z$;tdate : pause !   restore #h_trans,key>=z$&"         ": nokey TRANS_NOKEY
41080     if p$<>z$ then goto CUSTOMER_RECORD_FINIS ! history record must belong to this customer
41100     if tcode<>1 then goto READ_TRANSACTION ! charge transactions only
44000 ! r: determine usage
44020     if service=1 then ! analyzing water
44040       let usage=wu
44060     else if service=3 then ! analyzing electric
44080       let usage=eu
44100     else if service=4 then ! analyzing gas
44120       let usage=gu
44140     end if 
44160 ! /r
46000 ! r: accumulate
46020     cd1_which=srch(mat cd1,tdate)
46040 ! 
46060     if cd1_which>0 then 
46080       if accum_type=accum_type_average then 
46100         accum_average_divider+=1
46120       end if 
46140 ! 
48000       if usage<>0 then 
48020         let total_count_grand(cd1_which)=total_count_grand(cd1_which)+1
48040         let total_count_grand(13)=total_count_grand(13)+1
48060         let u1(cd1_which)=u1(cd1_which)+usage
48080         let u1(13)=u1(13)+usage
48100       end if 
48120     end if 
48140 ! /r
48160     goto READ_TRANSACTION
48180 CUSTOMER_RECORD_FINIS: ! 
60000     if accum_type=accum_type_average then 
60020 !     let math$=str$(u1(13))&'/'&str$(max(1,accum_average_divider))
60040       let u1(13)=u1(13)/max(1,accum_average_divider)
60060     end if 
60070     if sum(u1)<>0 then 
60080 !     cd1_which=srch(mat cd1,tdate)
60100       for cd1_item=1 to 12
60140         let total_usage_grand(cd1_item)+=u1(cd1_item) ! usage
60160         let total_usage_grand(13)+=u1(cd1_item) ! usage
60180         let total_usage_route(cd1_item)+=u1(cd1_item) ! usage
60200         let total_usage_route(13)+=u1(cd1_item) ! usage
60240         if servicecode<1 or servicecode>9 then let servicecode=10
60260 ! 
60280         let total_count_route_code_date(servicecode,13)+=1
60300         let total_count_route_code_date(servicecode,cd1_item)+=1
60320         let total_usage_route_code_date(servicecode,13)+=u1(cd1_item) ! usage
60340         let total_usage_route_code_date(servicecode,cd1_item)+=u1(cd1_item) ! usage
60360 ! 
60380         let total_count_grand_code_date(servicecode,13)+=1
60400         let total_count_grand_code_date(servicecode,cd1_item)+=1
60420         let total_usage_grand_code_date(servicecode,13)+=u1(cd1_item) ! usage
60440         let total_usage_grand_code_date(servicecode,cd1_item)+=u1(cd1_item) ! usage
60480       next cd1_item
62020       pr #255,using F_OUT: z$,e$(1:25),mat u1 pageoflow NEWPGE
62040     end if 
62060     mat u1=(0)
62080   loop 
64000 TRANS_NOKEY: ! r:
64020   pr #255: z$&" has no transactions"
64040   goto CUSTOMER_RECORD_FINIS ! /r
68000 NEWPGE: ! r:
68020   pr #255: newpage
68040   gosub HDR
68060   continue  ! /r
70000 HDR: ! r:
70020   let pg=pg+1
70040   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
70060   pr #255: "\qc  {\f181 \fs22 \b "&env$('Program_Caption')&" - "&scr1_resp$(13)&"}"
70080   pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
70100   pr #255: "\ql "
70120   if accum_type=accum_type_average then 
70140     pr #255,using 'form pos 1,C 284': "{\ul Account No}  {\ul Customer Name            }  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(1))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(2))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(3))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(4))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(5))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(6))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(7))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(8))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(9))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(10))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(11))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(12))&"}  {\ul   Average  }"
70160   else if accum_type=accum_type_total then 
70180     pr #255,using 'form pos 1,C 284': "{\ul Account No}  {\ul Customer Name            }  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(1))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(2))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(3))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(4))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(5))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(6))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(7))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(8))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(9))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(10))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(11))&"}  {\ul "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",cd1(12))&"}  {\ul   Total    }"
70200   end if 
70220   return  ! /r
72000   def fn_print_total(totals_heading$*80,mat total_usage_grand,mat total_usage_grand_code_date,mat total_count_grand,mat total_count_grand_code_date)
72020     pr #255: ""
72040     pr #255: totals_heading$
72060     pr #255: ""
72080     for j=1 to 10
72100       if total_count_grand_code_date(j,13)<>0 then 
72120         pr #255,using F_OUT: "","  Code "&str$(j)&" Total Customers",total_count_grand_code_date(j,1),total_count_grand_code_date(j,2),total_count_grand_code_date(j,3),total_count_grand_code_date(j,4),total_count_grand_code_date(j,5),total_count_grand_code_date(j,6),total_count_grand_code_date(j,7),total_count_grand_code_date(j,8),total_count_grand_code_date(j,9),total_count_grand_code_date(j,10),total_count_grand_code_date(j,11),total_count_grand_code_date(j,12),total_count_grand_code_date(j,13)
72140         pr #255,using F_OUT: "","  Code "&str$(j)&" Total Usage",total_usage_grand_code_date(j,1),total_usage_grand_code_date(j,2),total_usage_grand_code_date(j,3),total_usage_grand_code_date(j,4),total_usage_grand_code_date(j,5),total_usage_grand_code_date(j,6),total_usage_grand_code_date(j,7),total_usage_grand_code_date(j,8),total_usage_grand_code_date(j,9),total_usage_grand_code_date(j,10),total_usage_grand_code_date(j,11),total_usage_grand_code_date(j,12),total_usage_grand_code_date(j,13)
72160       end if 
72180     next j
72200     pr #255,using F_OUT: "","  Total Customers",mat total_count_grand
72220     pr #255,using F_OUT: "","  Total Usage",mat total_usage_grand
72240   fnend 
74000 DONE: ! r:
74020   close #h_customer: 
74040   if file(255)=-1 then goto XIT ! printer never opened
74050   fn_print_total("Route "&str$(route_prior)&" Totals",mat total_usage_route,mat total_usage_route_code_date,mat total_count_route,mat total_count_route_code_date)
74060   fn_print_total("Grand Totals",mat total_usage_grand,mat total_usage_grand_code_date,mat total_count_grand,mat total_count_grand_code_date)
74080   fncloseprn
74100   goto XIT ! /r
74120 XIT: let fnxit
76000 ! <Updateable Region: ERTN>
76020 ERTN: let fnerror(program$,err,line,act$,"xit")
76040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
76060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
76080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
76100 ERTN_EXEC_ACT: execute act$ : goto ERTN
76120 ! /region
78000 INVALID_DATES_MSGBOX: ! r:
78020   let msgline$(1)="You have entered dates in an"
78040   let msgline$(2)="invalid format.  Use mmddyy format."
78060   fnmsgbox(mat msgline$,resp$,'',1)
78080   goto SCREEN1 ! /r
