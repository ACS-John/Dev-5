00010 ! formerly S:\acsUB\UBUsage
20000 ! r: setup library, on err, dims, constants, etc
20020   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fncmbrt2,fntos,fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fnxit,fnd1,fncmdset,fntop,fngethandle,fnopt,fnget_services
20040   on error goto ERTN
20060   fntop(program$)
20080 ! ______________________________________________________________________
20100   dim z$*10,e$(4)*30,d(15)
20120   dim t(3,2),r(3,2),line$*212,serviceName$(10)*20,srv$(10)*2
20140   dim resp$(10)*128
20160 ! ______________________________________________________________________
22020   fnd1(filterBillingDate)
22040   fnget_services(mat serviceName$,mat srv$)
22060   if trim$(serviceName$(1))<>"" then service1enabled=1
22080   if trim$(serviceName$(3))="Electric" or trim$(srv$(3))="EL" or trim$(serviceName$(3))="Lawn Meter" then service3enabled=1
22100   if trim$(serviceName$(4))="Gas" or trim$(srv$(4))="GA" then service4enabled=1
22140   sequenceRoute=1
22160   sequenceAccount=2
24000   open #h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,input,keyed 
24020 ! /r
30000 SCREEN1: ! r:
30020   fntos(sn$="UBUsage")
30040   respc=0
30120   fnlbl(2,1,"Billing Date:",32,1)
30140   fntxt(2,34,8,0,1,"1")
30160   resp$(resp_billingDate:=respc+=1)=str$(filterBillingDate)
30180   fnlbl(3,1,"Route Number:",32,1)
30200   fncmbrt2(3,34)
30220   resp$(respc_routeFilter:=respc+=1)="[All]"
30240   fnlbl(5,1,"Sequence:",32,1)
30260   fnopt(5,34,'Route/Sequence  (includes totals by route)')
30280   resp$(respc_sequenceRoute:=respc+=1)='True'
30300   fnopt(6,34,'Account')
30320   resp$(respc_sequenceAccount:=respc+=1)='False'
30340   fncmdset(3)
30360   fnacs(sn$,0,mat resp$,ck)
30380   if ck=5 then goto XIT
30420   filterBillingDate=val(resp$(resp_billingDate))
30440   if trim$(resp$(respc_routeFilter))="[All]" then 
30460     filterRoute=0
30480   else 
30500     filterRoute=val(resp$(respc_routeFilter))
30520   end if 
30540   if filterBillingDate<10100 or filterBillingDate>123199 then goto SCREEN1
30600   if resp$(respc_sequenceRoute)='True' then reportSequence=sequenceRoute
30620   if resp$(respc_sequenceAccount)='True' then reportSequence=sequenceAccount
30630   filterBillingDateCcyymdd=fndate_mmddyy_to_ccyymmdd(filterBillingDate)
30640   if reportSequence=sequenceAccount then 
30660     open #hCustomerForReport:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
30700   else ! if reportSequence=sequenceRoute then
30720     open #hCustomerForReport:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndx5.h"&env$('cno')&",Shr",internal,input,keyed 
30760   end if 
36000   if filterRoute then 
36100     if reportSequence=sequenceRoute then restore #hCustomerForReport,key>=lpad$(str$(filterRoute),2)&"       ": nokey SCREEN1
36120     oldroute=filterRoute
36140   end if
36780 ! /r
47000   fnopenprn(cp,58,320,process)
47020   gosub HDR
48000 NextCustomer: ! r: main report loop
48020   read #hCustomerForReport,using F_Customer: z$,mat e$,mat d,CustomerLastBillingDate,route eof TOTAL_AND_FINISH
48040   F_Customer: form pos 1,c 10,4*c 30,pos 217,15*pd 5,pos 296,pd 4,pos 1741,n 2
48050   if reportSequence=sequenceAccount and filterRoute and route<>filterRoute then goto NextCustomer
48060   !  enable prior billing dates !  if CustomerLastBillingDate=filterBillingDate then goto L570
48080   d(1)=d(3)=d(5)=d(7)=d(9)=d(11)=0
48100   restore #h_trans,key>=z$&"         ": nokey L570
48120   do ! L520: ! 
48130     ! read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': transAcct$,transDate,tcode,tamount,mat tg,s1read,s1use,s3read,s3usage,s5read,s5use,tbal,pcode eof L570
48140     read #h_trans,using 'Form POS 1,C 10,N 8,pos 68,6*PD 5': transAcct$,transDate,s1read,s1use,s3read,s3usage,s5read,s5use eof L570
48160     if transAcct$<>z$ then goto L570
48210   loop until transDate=filterBillingDateCcyymdd
48220   ! L560: ! 
48240   d(1)=s1read
48260   d(3)=s1use
48280   d(5)=s3read
48300   d(7)=s3usage
48320   d(9)=s5read
48340   d(11)=s5use
48360   L570: ! 
48380   if filterRoute and route<>filterRoute then 
48400       goto TOTAL_AND_FINISH
48440   end if
48460   if route<>oldroute and oldroute<>0 then 
48480     s9=1
48500     gosub PRINT_ROUTE_TOTALS
48520   end if
48530   gosub PRINT_DETAILS
48540 goto NextCustomer ! /r
48580 FINIS: ! r:
48590   close #hCustomerForReport: ioerr ignore
48600   fncloseprn
48620 goto XIT ! /r
52000 IGNORE: continue 
54000 NEWPGE: ! r:
54020   pr #255: newpage
54040   gosub HDR
54060 continue  ! /r
56000 HDR: ! r:
56020   if ~setupHeader then
56040     setupHeader=1
56060     ! r: set heading$ and heading2$
56080     dim heading$*212
56100     dim heading2$*212
56120     if service1enabled+service3enabled+service4enabled=1 then
56140     heading$=rpt$(' ',16+7)   !  i don't know why, but it works.
56160     else
56180     heading$=rpt$(' ',16)
56200     end if
56220     heading2$=""
56240     reportWidth=32
56260     if service1enabled then 
56280       heading$=heading$&" {\ul            Water           }"
56300       heading2$=heading2$&" {\ul   Reading   Current       YTD}"
56320       reportWidth+=30
56340     end if 
56360     if service3enabled then 
56380       heading$=heading$&"   {\ul       "&serviceName$(3)(1:12)&"         }"
56400       heading2$=heading2$&" {\ul   Reading   Current       YTD}"
56420       reportWidth+=30
56440     end if 
56460     if service4enabled then 
56480       heading$=heading$&"   {\ul       "&serviceName$(4)(1:12)&"         }"
56500       heading2$=heading2$&" {\ul   Reading   Current       YTD}" ! gas used for something other than gas
56520       reportWidth+=30
56540     end if 
56560     ! /r
56580   end if
56600   pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
56620   pr #255: "\qc  {\f181 \fs24 \b "&env$('Program_Caption')&"}"
56640   pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
56660   pr #255: "\qc  {\f181 \fs16 \b Billing Date: "&cnvrt$('pic(##/##/##)',filterBillingDate)&"}"
56680   pr #255,using 'form pos 1,Cr reportWidth': "Page "&str$(p2+=1)
56700   pr #255: heading$
56720   pr #255: "{\ul  Account  } {\ul Name                } "&heading2$
56740 return  ! /r
58000 PRINT_DETAILS: ! r:
58010   if sum(mat d(3:12))<>0 then
58020     line$= z$&" "&e$(2)(1:21)
58040     if service1enabled then 
58060       line$=line$&cnvrt$("n 10",d(1))&cnvrt$("n 10",d(3))&cnvrt$("n 10",d(4))
58080     end if 
58100     if service3enabled then 
58280       line$=line$&cnvrt$("n 10",d(5))&cnvrt$("n 10",d(7))&cnvrt$("n 10",d(8))
58300     end if 
58340     if service4enabled then 
58400       line$=line$&cnvrt$("n 10",d(9))&cnvrt$("n 10",d(11))&cnvrt$("n 10",d(12))
58420     end if 
58460     pr #255: line$ pageoflow NEWPGE
58480     oldroute=route
58500     t(1,1)=t(1,1)+d(3)
58520     t(1,2)=t(1,2)+d(4)
58540     t(2,1)=t(2,1)+d(7)
58560     t(2,2)=t(2,2)+d(8)
58580     t(3,1)=t(3,1)+d(11)
58600     t(3,2)=t(3,2)+d(12)
58610   end if
58620 return  ! /r
58640 TOTAL_AND_FINISH: ! r:
58660   gosub PRINT_ROUTE_TOTALS
58680 goto FINIS ! /r
60000 PRINT_ROUTE_TOTALS: ! r:
60020   mat r=r+t
60030   if reportSequence=sequenceRoute then 
60040     pr #255: "" ! pageoflow NEWPGE
60060     pr #255: "" ! pageoflow NEWPGE
60080     pr #255: "" ! pageoflow NEWPGE
60100     pr #255: tab(40);"Totals for Route Number ";oldroute;
60120     if s9=0 then pr #255: tab(75);"Grand Totals";
60140     pr #255: ""
60160     pr #255: tab(39);"Current    Year to Date";
60180     if s9=0 then pr #255: tab(69);"Current    Year to Date"
60200     line$=""
60220     if trim$(serviceName$(1))="" then 
60240       goto L1180
60260     else 
60280       line$=serviceName$(1)(1:11)&line$&cnvrt$("n 10",t(1,1))&"      "&cnvrt$("n 10",t(1,2))
60300     end if 
60320     if s9=0 then 
60340       line$=line$ & "    "&cnvrt$("n 10",r(1,1))&"      "&cnvrt$("n 10",r(1,2))
60360     end if 
60380     pr #255,using "Form POS 25,C 120": line$
60400     L1180: ! 
60420     line$=""
60440     if service3enabled then 
60460       line$=serviceName$(3)(1:12)&line$&cnvrt$("n 10",t(2,1))&"      "&cnvrt$("n 10",t(2,2))
60740     end if 
60780     if s9=0 then 
60800       line$=line$ & "    "&cnvrt$("n 10",r(2,1))&"      "&cnvrt$("n 10",r(2,2))
60820     end if 
60840     pr #255,using "Form POS 25,C 120": line$
60880     line$=""
60900     if service4enabled then 
60920       line$=serviceName$(4)(1:11)&line$&cnvrt$("n 10",t(3,1))&"      "&cnvrt$("n 10",t(3,2))
60960     end if 
61120     if s9=0 then 
61140       line$=line$ & "    "&cnvrt$("n 10",r(3,1))&"      "&cnvrt$("n 10",r(3,2))
61160     end if 
61180     pr #255,using "Form POS 25,C 120": line$
61240     if s9=1 then s9=0 : pr #255: newpage ! gosub NEWPGE
61800   end if  ! reportSequence=sequenceRoute
61820   mat t=(0)
61860   s9=0
61900 return  ! /r
62000 XIT: fnxit
66000 ! <Updateable Region: ERTN>
66020 ERTN: fnerror(program$,err,line,act$,"xit")
66040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
66060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
66080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
66100 ERTN_EXEC_ACT: execute act$ : goto ERTN
66120 ! /region
