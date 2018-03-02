00010 ! formerly S:\acsUB\UBUsage
20000 ! r: setup library, on err, dims, constants, etc
20020   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fncmbrt2,fnTos,fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fnxit,fnLastBillingDate,fnCmdSet,fntop,fngethandle,fnOpt,fnget_services
20040   on error goto ERTN
20060   fntop(program$)
20080 ! ______________________________________________________________________
20100   dim z$*10,name$*30,dx(15)
20120   dim totalRoute(3,2),line$*212
20130   dim serviceName$(10)*20,srv$(10)*2
20140   dim resp$(10)*128
20160 ! ______________________________________________________________________
22020   fnLastBillingDate(filterBillingDate)
22040   fnget_services(mat serviceName$,mat srv$)
22060   if trim$(serviceName$(1))<>'' then service1enabled=1
22080   if trim$(serviceName$(3))="Electric" or trim$(srv$(3))="EL" or trim$(serviceName$(3))="Lawn Meter" then service3enabled=1
22100   if trim$(serviceName$(4))="Gas" or trim$(srv$(4))="GA" then service4enabled=1
22140   sequenceRoute=1
22160   sequenceAccount=2
24000   open #h_trans:=fngethandle: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
24020 ! /r
30000 SCREEN1: ! r:
30020   fnTos(sn$="UBUsage")
30040   respc=0
30120   fnLbl(2,1,"Billing Date:",32,1)
30140   fnTxt(2,34,8,0,1,"1")
30160   resp$(resp_billingDate:=respc+=1)=str$(filterBillingDate)
30180   fnLbl(3,1,"Route Number:",32,1)
30200   fncmbrt2(3,34)
30220   resp$(respc_routeFilter:=respc+=1)="[All]"
30240   fnLbl(5,1,"Sequence:",32,1)
30260   fnOpt(5,34,'Route/Sequence  (includes totals by route)')
30280   resp$(respc_sequenceRoute:=respc+=1)='True'
30300   fnOpt(6,34,'Account')
30320   resp$(respc_sequenceAccount:=respc+=1)='False'
30340   fnCmdSet(3)
30360   fnAcs(sn$,0,mat resp$,ck)
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
30660     open #hCustomerForReport:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
30700   else ! if reportSequence=sequenceRoute then
30720     open #hCustomerForReport:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed 
30760   end if 
36000   if filterRoute then 
36100     if reportSequence=sequenceRoute then restore #hCustomerForReport,key>=lpad$(str$(filterRoute),2)&"       ": nokey SCREEN1
36120     ! routePrior=filterRoute
36140   end if
36780 ! /r
47000 fnopenprn(cp,58,320,process)
47020 gosub HDR
48000 do ! r: main report loop
48020   read #hCustomerForReport,using F_Customer: z$,name$,mat dx,CustomerLastBillingDate,route eof TOTAL_AND_FINISH
48040   F_Customer: form pos 1,c 10,pos 41,C 30,pos 217,15*pd 5,pos 296,pd 4,pos 1741,n 2
48050   if reportSequence=sequenceAccount and filterRoute and route<>filterRoute then goto NextCustomer
48080   dx(1)=dx(3)=dx(5)=dx(7)=dx(9)=dx(11)=0
48100   restore #h_trans,key>=z$&"         ": nokey L570
48120   do 
48140     read #h_trans,using 'Form POS 1,C 10,N 8,pos 68,6*PD 5': transAcct$,transDate,trans_s1reading,trans_s1usage,trans_s3reading,trans_s3usage,trans_s5reading,trans_s5usage eof L570
48210   loop until transDate=filterBillingDateCcyymdd or transAcct$<>z$
48230   if transDate=filterBillingDateCcyymdd then
48240     dx(1) =trans_s1reading
48260     dx(3) =trans_s1usage
48280     dx(5) =trans_s3reading
48300     dx(7) =trans_s3usage
48320     dx(9) =trans_s5reading
48340     dx(11)=trans_s5usage
48350   end if
48360   L570: ! 
48380   if filterRoute and route<>filterRoute then 
48400       goto TOTAL_AND_FINISH
48440   end if
48460   if route<>routePrior then 
48500     if routePrior<>0 then 
48502       fn_printTotals(1)
48504     end if
48510     routePrior=route
48520   end if
48530   gosub PRINT_DETAILS
48532   NextCustomer: ! 
48540 loop ! /r
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
56140       heading$=rpt$(' ',16+7)   !  i don't know why, but it works.
56160     else
56180       heading$=rpt$(' ',16)
56200     end if
56220     heading2$=''
56240     reportWidth=32
56260     if service1enabled then 
56280       heading$(inf:inf)=" {\ul            Water           }"
56300       heading2$(inf:inf)=" {\ul   Reading   Current       YTD}"
56320       reportWidth+=30
56340     end if 
56360     if service3enabled then 
56380       heading$(inf:inf)="   {\ul       "&serviceName$(3)(1:12)&"         }"
56400       heading2$(inf:inf)=" {\ul   Reading   Current       YTD}"
56420       reportWidth+=30
56440     end if 
56460     if service4enabled then 
56480       heading$(inf:inf)="   {\ul       "&serviceName$(4)(1:12)&"         }"
56500       heading2$(inf:inf)=" {\ul   Reading   Current       YTD}" ! gas used for something other than gas
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
58010   if sum(mat dx(3:12))<>0 and days(CustomerLastBillingDate,'mmddyy')=>days(filterBillingDate,'mmddyy') then
58020     line$=z$&' '&name$(1:21)
58040     if service1enabled then 
58060       line$(inf:inf)=cnvrt$("n 10",dx(1))&cnvrt$("n 10",dx(3))&cnvrt$("n 10",dx(4))
58080     end if 
58100     if service3enabled then 
58280       line$(inf:inf)=cnvrt$("n 10",dx(5))&cnvrt$("n 10",dx(7))&cnvrt$("n 10",dx(8))
58300     end if 
58340     if service4enabled then 
58400       line$(inf:inf)=cnvrt$("n 10",dx(9))&cnvrt$("n 10",dx(11))&cnvrt$("n 10",dx(12))
58420     end if 
58460     pr #255: line$ pageoflow NEWPGE
58480     oldroute=route
58500     totalRoute(1,1)+=dx(3)
58520     totalRoute(1,2)+=dx(4)
58540     totalRoute(2,1)+=dx(7)
58560     totalRoute(2,2)+=dx(8)
58580     totalRoute(3,1)+=dx(11)
58600     totalRoute(3,2)+=dx(12)
58610   end if
58620 return  ! /r
58640 TOTAL_AND_FINISH: ! r:
58660   fn_printTotals(0)
58680 goto FINIS ! /r
60000 def fn_printTotals(totalRoute)
60020   dim totalGrand(3,2)
60040   mat totalGrand=totalGrand+totalRoute
60060   if reportSequence=sequenceRoute then 
60080     pr #255: '' ! pageoflow NEWPGE
60100     pr #255: '' ! pageoflow NEWPGE
60120     pr #255: tab(40);"Totals for Route Number ";routePrior;
60140     if ~totalRoute then pr #255: tab(75);"Grand Totals";
60160     pr #255: ''
60180     pr #255: tab(39);"Current    Year to Date";
60200     if ~totalRoute then pr #255: tab(69);"Current    Year to Date"
60220     !
60240     line$=''
60260     if trim$(serviceName$(1))<>'' then 
60280       line$=serviceName$(1)(1:11)&line$&cnvrt$("n 10",totalRoute(1,1))&"      "&cnvrt$("n 10",totalRoute(1,2))
60300       if ~totalRoute then 
60320         line$(inf:inf)="    "&cnvrt$("n 10",totalGrand(1,1))&"      "&cnvrt$("n 10",totalGrand(1,2))
60340       end if 
60360       pr #255,using "Form POS 25,C 120": line$
60380     end if
60400     !
60420     line$=''
60440     if service3enabled then 
60460       line$=serviceName$(3)(1:12)&line$&cnvrt$("n 10",totalRoute(2,1))&"      "&cnvrt$("n 10",totalRoute(2,2))
60480     end if 
60500     if ~totalRoute then 
60520       line$(inf:inf)="    "&cnvrt$("n 10",totalGrand(2,1))&"      "&cnvrt$("n 10",totalGrand(2,2))
60540     end if 
60560     pr #255,using "Form POS 25,C 120": line$
60580     !
60600     line$=''
60620     if service4enabled then 
60640       line$=serviceName$(4)(1:11)&line$&cnvrt$("n 10",totalRoute(3,1))&"      "&cnvrt$("n 10",totalRoute(3,2))
60660     end if 
60680     if ~totalRoute then 
60700       line$(inf:inf)="    "&cnvrt$("n 10",totalGrand(3,1))&"      "&cnvrt$("n 10",totalGrand(3,2))
60720     end if 
60740     pr #255,using "Form POS 25,C 120": line$
60742     !
60760     if totalRoute=1 then 
60780       pr #255: newpage ! gosub NEWPGE
60800     end if
60820   end if  ! reportSequence=sequenceRoute
60840   mat totalRoute=(0)
60880 fnend
62000 XIT: fnxit
66000 ! <Updateable Region: ERTN>
66020 ERTN: fnerror(program$,err,line,act$,"xit")
66040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
66060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
66080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr '' : pause : goto ERTN_EXEC_ACT
66100 ERTN_EXEC_ACT: execute act$ : goto ERTN
66120 ! /region
