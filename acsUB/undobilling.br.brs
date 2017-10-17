00001 forceRollBackNotMostRecentRec=0
00010 library 'S:\Core\Library': fnxit,fnopenprn,fncloseprn,fnerror,fnLastBillingDate,fngethandle,fntop,fntos,fnlbl,fntxt,fncmbact,fncmbrt2,fncmdset,fnacs,fnmsgbox,fnopt,fnget_services,fnAutomatedSavePoint
00040 on error goto ERTN
00080 ! msgbox("Reverse Billing Cycle is currently under construction.","Reverse Billing Cycle Unavailable","OK","Inf") : if env$('ACSDeveloper')='' then goto XIT
00120 fn_undobilling
00140 goto XIT
00160 def fn_undobilling ! main
00180   dim program_caption$*80,billingdate$*10,msgtext$(1)*1000,readings(12),charges(12),breakdown(10),readingdates(2),servicename$(10)*20
00200   do_all=1 : do_route=2 : do_individual=3
00220   fntop(program$,program_caption$="Reverse Billing Cycle")
00230   fnget_services(mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply)
00240   ! 
06000   ASK_OPTIONS: ! 
06020   cont=fn_options(route,billingdate$) ! collect user options
06040   if ckey=5 then goto XIT_FN_UNDOBILLING
06060   if trim$(billingdate$)="0" then 
06080     mat msgtext$(1)
06100     msgtext$(1)=("You must enter a billing date")
06120     fnmsgbox(mat msgtext$,answer$,"Invalid Entry",0)
06140     goto ASK_OPTIONS
06160   end if 
08000  ! 
08020   mat msgtext$(3)
08040   msgtext$(1) = "Warning: this action will reduce the balance and balance breakdown of all selected customers"
08060   msgtext$(2) = "with a matching billing date by the amount of the billing on that date."
08080   msgtext$(3) = "Are you sure?"
08100   fnmsgbox(mat msgtext$,answer$,"Confirm Action",4)
08120   if (answer$<>"Yes") then cont=0
08140   ! 
08160   undoCount=0
12000   if cont then 
12020     dim acct$*10,custname$*30,trcust$(3)*10,trdate(3),tramt(3),srvamt1(11),srvamt2(11),srvamt3(11),srvread1(6),srvread2(6),srvread3(6),trbal(3)
12040     CUSTFORM: form c 10,x 30,c 30,pos 1741,n 2,pos 217,12*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1750,2*n 6
12060     TRANSFORM: form c 10,n 8,x 1,12*pd 4.2,6*pd 5,pd 4.2
12070     fnAutomatedSavePoint('before')
12080     fn_openfiles ! open data files
12100     fnopenprn : fn_printheader
12120     do 
14000       NEXT_CUSTOMER: ! 
14020       if filter=do_individual then 
14040         read #h_customer,using CUSTFORM,key=lpad$(cust$,kln(h_customer)): acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates
14060       else 
14080         read #h_customer,using CUSTFORM: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates eof CUSTDONE ! get every customer one at a time
14100       end if 
14110           ! if trim$(acct$)='1000000.01' then pause
14120       if filter<>do_route or custroute=route then ! if a route was selected and customer doesn't match, skip customer
14140         if fn_get3trans(acct$,billingdate$,lastdate,priordate,priordate2) then ! get latest and 2 prior charge transactions for this customer
16000           ! get the three latest transactions and their data; most recent is in first array element, prior in second, prior2 in third
16020           if priordate>0 then 
16040             read #h_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(priordate)&"1": trcust$(2),trdate(2),tramt(2),mat srvamt2,mat srvread2,trbal(2) nokey NEXT_CUSTOMER
16060           end if 
16080           if priordate2>0 then 
16100             read #h_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(priordate2)&"1": trcust$(3),trdate(3),tramt(3),mat srvamt3,mat srvread3,trbal(3) nokey NEXT_CUSTOMER
16120           end if 
16140           undoCount+=1
16160           read #h_trans,using TRANSFORM,key=lpad$(acct$,10)&str$(lastdate)&"1": trcust$(1),trdate(1),tramt(1),mat srvamt1,mat srvread1,trbal(1)
16180           if priordate>0 then 
18000             ! update readings
18020             readings(4)-=readings(3) ! roll back YTD usage
18040             readings(8)-=readings(7)
18060             readings(12)-=readings(11)
18080             readings(1)=srvread2(1) ! update all current readings and usage
18100             readings(3)=srvread2(2)
18120             readings(5)=srvread2(3)
18140             readings(7)=srvread2(4)
18160             readings(9)=srvread2(5)
18180             readings(11)=srvread2(6)
18200             if priordate2>0 then ! update all prior readings
18220               readings(2)=srvread3(1)
18240               readings(6)=srvread3(3)
18260               readings(10)=srvread3(5)
18280             end if 
19000             ! update charge date
19020             chargedate=val(date$(days(trdate(2),"ccyymmdd"),"mmddyy"))
20000             ! update charges
20020             mat charges(1:11)=srvamt2(1:11)
20040             charges(12)=srvamt2(11)+srvamt2(10)
22000             ! update breakdowns
22020             !             if trim$(acct$)='100480.01' then pr acct$ : pause
22040             for breakdown_item=1 to 9
22060               if penalty$(breakdown_item)<>'Y' then 
22080                 breakdown(breakdown_item)=breakdown(breakdown_item)-srvamt1(breakdown_item)
22100               end if 
22120             next breakdown_item
24800             ! mat breakdown(1:9)=breakdown(1:9)-srvamt1(1:9)
24820             ! update current balance
24840             balance-=srvamt1(11)
24860             ! update reading dates; guess at prior reading date
24880             readingdates(2)=readingdates(1)
24900             readingdates(1)=val(date$(days(priordate2,"ccyymmdd"),"mmddyy"))
24920             ! update last billing date
24940             if priordate>lastbilling then lastbilling=priordate
24960           else 
24980             mat readings(1:12)=(0) : mat charges(1:12)=(0) : balance=0 : chargedate=0 : mat breakdown(1:10)=(0) : mat readingdates(1:2)=(0)
25000           end if 
25020           ! rewrite customer master record
25040           rewrite #h_customer,using CUSTFORM: acct$,custname$,custroute,mat readings,balance,chargedate,mat charges,mat breakdown,mat readingdates
25060           ! delete rolled-back transaction
25080           delete #h_trans: 
25100           pr #255,using "form pos 5,c 10,x 5,pic(zz/zz/zz)": trcust$(1),str$(trdate(1)) pageoflow PRINTPAGEOVERFLOW
25120         end if 
25140       end if 
25160     loop while filter<>do_individual
25180     goto CUSTDONE
26000     PRINTPAGEOVERFLOW: ! r:
26020      pr #255: newpage
26040      fn_printheader
26060     continue ! /r
28000     CUSTDONE: ! 
28020     if filter=do_all then 
28040       lastbilling=val(date$(days(lastbilling,"ccyymmdd"),"mmddyy"))
28060       fnLastBillingDate(lastbilling,1)
28080     end if 
28100     mat msgtext$(1)=("Customers reversed: "&str$(undoCount))
28120     fnmsgbox(mat msgtext$,answer$,"Report",0)
28140     fncloseprn
28160     fn_close_files
28180     if filter=do_individual then goto ASK_OPTIONS
28200   end if 
28220   ! 
28240   XIT_FN_UNDOBILLING: ! 
28260 fnend  ! fn_UndoBilling
30000 XIT: fnxit
44000   def fn_options(&route,&billingdate$) ! show options dialog to user and return selections
44020     dim screen_name$*100,resp$(20)*255
44040     fnLastBillingDate(lastbilling) ! get last billing date and use it for the default
44060     filter=0 : route=0 : cust$=''
44080 OPTIONS_TOS: ! 
44100     fntos(screen_name$="UndoBillingOptions")
44120     rcnt=0 : lc=0 : pos_col2=16
44140     lc+=1
44160     fnlbl(lc+=1,2,"Warning: only the most recent billing date can be reversed for any account(s).")
44180     lc+=1
44200 ! billing date text box
44220     fnlbl(lc+=1,2,"Billing Date:",13,1)
44240     fntxt(lc,pos_col2,8,0,0,"1001")
44260     resp_billing_date=rcnt+=1
44280     if resp$(resp_billing_date)='' then resp$(resp_billing_date)=str$(lastbilling)
44300 ! 
44320     lc+=1
44340     lc+=1
44360     fnlbl(lc+=1,2,"Use only one of options below to limit the customers to reverse.")
44380     lc+=1
44400 ! 
44420     fnopt(lc+=1,1,'All') ! fnopt(lyne,ps, txt$*196; align,contain,tabcon)
44440     resp_opt_all=rcnt+=1
44460     if resp$(resp_opt_all)='' then resp$(resp_opt_all)='True'
44480 ! 
44500     fnopt(lc+=1,1,'Route:')
44520     resp_opt_route=rcnt+=1
44540     if resp$(resp_opt_route)='' then resp$(resp_opt_route)='False'
44560     fncmbrt2(lc,pos_col2,1)
44580     resp_route=rcnt+=1
44600 ! if resp$(resp_route)='' then resp$(resp_route)="[All]"
44620 ! 
44640     fnopt(lc+=1,1,'Individual:')
44660     resp_opt_individual=rcnt+=1
44680     if resp$(resp_opt_individual)='' then resp$(resp_opt_individual)='False'
44700     fncmbact(lc,pos_col2) ! fncmbact(lyne,mypos; addall,c,a$*25)
44720     resp_individual=rcnt+=1
44740 ! if resp$(resp_individual)='' then resp$(resp_individual)="[All]"
44760 ! 
44780     fncmdset(2) ! show "Next" and "Cancel" buttons
44800     fnacs(screen_name$,0,mat resp$,ckey) ! run the screen
44820 ! 
44840     if ckey=5 then ! if user pressed Cancel
44860       fn_options=0
44880     else 
44900       billingdate$=resp$(resp_billing_date)
44920       if resp$(resp_opt_all)='True' then 
44940         filter=do_all
44960       else if resp$(resp_opt_route)='True' then 
44980         filter=do_route
45000         route=val(resp$(resp_route))
45020         if route=0 then pr bell;'please select a route' : goto OPTIONS_TOS
45040       else if resp$(resp_opt_individual)='True' then 
45060         filter=do_individual
45080         cust$=trim$(resp$(resp_individual)(1:10))
45090         if trim$(cust$)='' then pr bell;'please select a customer' : goto OPTIONS_TOS
45100       end if 
45120 !     pr 'answers retreived' : pause  !
45140       fn_options=1
45160     end if 
45180   fnend  ! fn_Options
46000   def fn_openfiles
46020     open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno'),internal,outin,keyed 
46040     open #h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno'),internal,outin,keyed 
46060   fnend 
48000   def fn_close_files
48020     close #h_customer: ioerr ignore
48040     close #h_trans: ioerr ignore
48060   fnend 
52000 def fn_get3trans(acct$,billingdate$,&lastdate,&priordate,&priordate2)
52020   dim transacct$*10,transacct2$*10
52040   CUSTTRANSFORM: form c 10,n 8,n 1
52060   ! 
52080   lastdate=0 : priordate=0 : priordate2=0
52100   dateshouldbe=date(days(val(billingdate$),"mmddyy"),"ccyymmdd") : if str$(dateshouldbe)(1:2)="19" then dateshouldbe+=1000000
52120   ! 
52140   ! first, check for the transaction for this customer on the date specified for rollback; if not found, exit
52160   read #h_trans,using CUSTTRANSFORM,key=lpad$(acct$,10)&str$(dateshouldbe)&"1": transacct$,transdate,transcode eof GOTTRANS nokey GOTTRANS
52170   if forceRollBackNotMostRecentRec then goto NOLATERTRANS
52180   ! next, see if there are any later charge transactions; if so, exit (cannot roll back any date except the most recent)
52200   read #h_trans,using CUSTTRANSFORM,next: transacct2$,transdate,transcode eof NOLATERTRANS
52220   if transcode=1 and transacct2$=transacct$ then goto GOTTRANS
52240   ! 
52260   NOLATERTRANS: !
52270   do  ! finally, read back up file to get 2 prior transaction dates
52280     lastdate=dateshouldbe
52300     read #h_trans,using CUSTTRANSFORM,prior: transacct2$,transdate,transcode eof GOTTRANS
52320     if transacct2$=transacct2$ and transcode=1 and transdate<lastdate then 
52340       if priordate=0 then 
52360         priordate=transdate
52380       else if priordate2=0 then 
52400         priordate2=transdate
52420       else 
52440         goto GOTTRANS
52460       end if 
52480     end if 
52500   loop while transacct2$=transacct$ and (priordate=0 or priordate2=0)
52520   ! 
52540   GOTTRANS: !
52550   if lastdate=0 then let fn_get3trans=0 else let fn_get3trans=1
52560 fnend 
55000   def fn_printheader
55020     pg+=1
55040     pr #255: "Reverse Calculation Status Report"
55060     pr #255: "Page "&str$(pg)
55080     pr #255: ""
55100     pr #255: "All accounts listed have been reversed."
55120     pr #255: ""
55140     pr #255: "Account           Billing Date"
55160     pr #255: "_______________   ____________"
55180   fnend 
59000 IGNORE: continue 
60000 ! <Updateable Region: ERTN>
60020 ERTN: fnerror(program$,err,line,act$,"NO")
60040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
60060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60100 ERTN_EXEC_ACT: execute act$ : goto ERTN
60120 ! </Updateable Region: ERTN>
