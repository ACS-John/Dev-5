10000 ! Replace S:\acsUB\Company
10020 ! maintain UB company information
10040 ! r: setup
10060   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnd1,fnerror,fntos,fncmdset,fnchk,fngethandle,fnfra,fnCopy,fncreg_read,fncreg_write,fncomboa
10080   on error goto ERTN
10100 ! ______________________________________________________________________
10120   dim cap$*128
10140   dim resp$(256)*40
10160   dim at$(3)*40
10200   dim servicename$(10)*20
10220   dim servicecode$(10)*2
10240   dim tax_code$(10)*1
10260   dim default_rate$(10)*80
14000 ! /r
14060   fntop(program$)
15000   gosub COMPANY_LOAD
16020   gosub SERVICE_LOAD
18000 ! MAIN: !
18020   fntos(sn$="Company")
18030 ! r: company information portion of screen
18040   fnlbl(1,1,"Company Name:",35,1)
18060   fntxt(1,37,40)
18080   resp$(1)=at$(1)
18100   fnlbl(2,1,"Company Address:",35,1)
18120   fntxt(2,37,40)
18140   resp$(2)=at$(2)
18160   fnlbl(3,1,"Company City,State and Zip:",35,1)
18180   fntxt(3,37,40)
18200   resp$(3)=at$(3)
18220   fnlbl(4,1,"Last Billing Date:",35,1)
18240   fntxt(4,37,8,0,1,"1")
18260   resp$(resp_d1:=4)=str$(d1)
18280   fnchk(6,1,"Require Receipt Number on Collections")
18300   resp$(resp_require_receipt:=5)=rcpt$
18320   fnlbl(8,1,"Unusual Usage Percent:",35,1)
18340   fntxt(8,37,4,0,1,"30",0,"Percent to use for determining unusual usage   (Example: 50% as 50)")
18360   resp$(resp_pcent:=6)=str$(pcent)
20000 ! 
20020   fnlbl(8,44,"Water Unusual Usage Minimum:",31,1)
20040   fntxt(8,76,4,0,1,"30",0,"Do not report unusual usage if below this minimum.")
20060   resp$(resp_uum_water:=7)=uum_water$
20080 ! 
20100   fnlbl(9,44,"Electric Unusual Usage Minimum:",31,1)
20120   fntxt(9,76,4,0,1,"30",0,"Do not report unusual usage if below this minimum.")
20140   resp$(resp_uum_gas:=8)=uum_gas$
20160 ! 
20200   fnlbl(10,44,"Gas Unusual Usage Minimum:",31,1)
20220   fntxt(10,76,4,0,1,"30",0,"Do not report unusual usage if below this minimum.")
20240   resp$(resp_uum_electric:=9)=uum_electric$
21000 ! 
21020   mylen=35 : mypos=mylen+2
21040   fnlbl(12,1,"Starting Route Number:",mylen,1)
21060   fntxt(12,mypos,2,0,1,"30") 
21080   resp$(resp_route_low:=10)=str$(bkno1)
21100   fnlbl(13,1,"Ending Route Number:",mylen,1)
21120   fntxt(13,mypos,2,0,1,"30") 
21140   resp$(resp_route_high:=11)=str$(bkno2)
22000 ! 
22020   respc=11 ! criticial (for the next section) that this is the last resp$ used (above)
22040 ! 
23000 ! /r
24000 ! r: TYPEOSERVICE portion of screen
24020   disable_for_client=1
24040   if env$('ACSDeveloper')<>'' then disable_for_client=0
24050   srv_input_col_count=6
24060   fnfra(15,1,12,113, 'Type of Service') : fra=1
24080   fnlbl(2,13,"Full Name",20,2,0,fra)
24100   fnlbl(2,34,"Code",4,0,0,fra)
24120   fnlbl(2,39,"Taxable",7,0,0,fra)
24140   fnlbl(2,47,"Penalty",7,0,0,fra)
24160   fnlbl(1,55,"Subject",7,2,0,fra)
24180   fnlbl(2,55,"To",7,2,0,fra)
24190   fnlbl(1,64,"Collection",10,2,0,fra)
24192   fnlbl(2,64,"Order",10,2,0,fra)
24200   fnlbl(1,75,"Only",5,2,0,fra)
24202   fnlbl(2,75,"Month",5,2,0,fra)
24224   fnlbl(1,82,"Default",9,2,0,fra)
24228   fnlbl(2,82,"Rate Code",9,2,0,fra)
24240   respc_service_base=respc
24260   for service_item=1 to 10
24280     fnlbl(service_item+2,1,"Service "&str$(service_item)&":",11,1,0,fra)
24300     fntxt(service_item+2,13,20,0,0,'',disable_for_client,'',fra)
24320     resp$(respc+=1)=servicename$(service_item) ! resp$(respc_service_base+service_item*6-5)=servicename$(service_item)
24340     fntxt(service_item+2,34,3,0,0,'',disable_for_client,'',fra)
24360     resp$(respc+=1)=servicecode$(service_item) ! resp$(respc_service_base+service_item*6-4)=servicecode$(service_item)
24380     fnchk(service_item+2,41,"",align=0,fra,tabcon=0,disable_for_client)
24400     if tax_code$(service_item)="Y" then 
24420       resp$(respc+=1)="True" ! resp$(respc_service_base+service_item*6-3)="True"
24440     else 
24460       resp$(respc+=1)="False" ! resp$(respc_service_base+service_item*6-3)="False"
24480     end if 
24500     fnchk(service_item+2,49,"",align=0,fra,tabcon=0,disable_for_client)
24520     if penalty$(service_item)="Y" then 
24540       resp$(respc+=1)="True" ! resp$(respc_service_base+service_item*6-2)="True"
24560     else 
24580       resp$(respc+=1)="False" ! resp$(respc_service_base+service_item*6-2)="False"
24600     end if 
24620     fntxt(service_item+2,58,2,0,0,"30",disable_for_client,'',fra)
24640     resp$(respc+=1)=str$(subjectto(service_item)) ! resp$(respc_service_base+service_item*6-1)=str$(subjectto(service_item))
24660     fntxt(service_item+2,68,2,0,0,"30",disable_for_client,'',fra)
24680     resp$(respc+=1)=str$(ordertoapply(service_item)) ! resp$(respc_service_base+service_item*6)=str$(ordertoapply(service_item))
24700     fntxt(service_item+2,76,2,0,0,"30",0,'',fra)
24720     resp$(respc+=1)=str$(onlyMonth(service_item))
24760     fn_cmb_rate(servicecode$(service_item),service_item+2,82,'Select Default or 0 for None',fra)
24780     resp$(respc+=1)=default_rate$(service_item)
24800   next service_item
24900 ! /r
40000   fncmdset(4)
40020   fnacs(sn$,0,mat resp$,ck)
42000   if ck=5 then goto XIT
42020 ! r: RESP$ to Company Information variables
42040   at$(1)=resp$(1)
42060   at$(2)=resp$(2)
42080   at$(3)=resp$(3)
42100   d1=val(resp$(resp_d1))
42120   rcpt$=resp$(resp_require_receipt)
42140   pcent=val(resp$(resp_pcent))
42160 ! 
42180   uum_water$=resp$(resp_uum_water)
42200   uum_gas$=resp$(resp_uum_gas)
42220   uum_electric$=resp$(resp_uum_electric)
42240   bkno1=val(resp$(resp_route_low))
42260   bkno2=val(resp$(resp_route_high))
42280 ! /r
45000 ! r: RESP$ to Service variables
45002   respc=respc_service_base
45510   for service_item=1 to 10
45520     servicename$(service_item)=resp$(respc+=1) ! resp$(respc_service_base+service_item*6-5)
45530     servicecode$(service_item)=uprc$(resp$(respc+=1)) ! uprc$(resp$(respc_service_base+service_item*6-4))
45540     if resp$(respc+=1)="True" then ! resp$(respc_service_base+service_item*6-3)="True" then
45550       tax_code$(service_item)="Y"
45560     else 
45570       tax_code$(service_item)="N"
45580     end if 
45590     if resp$(respc+=1)="True" then ! resp$(respc_service_base+service_item*6-2)="True" then
45600       penalty$(service_item)="Y"
45610     else 
45620       penalty$(service_item)="N"
45630     end if 
45640     subjectto(service_item)=val(resp$(respc+=1)) ! respc_service_base+service_item*6-1))
45660     ordertoapply(service_item)=val(resp$(respc+=1)) ! resp$(respc_service_base+service_item*6))
45680     onlyMonth(service_item)=val(resp$(respc+=1))
45700     default_rate$(service_item)=resp$(respc+=1)
45720   next service_item
45900 ! /r
54000   gosub COMPANY_SAVE
56000   gosub SERVICE_SAVE
58000   goto XIT
59000 XIT: fnxit
59500 IGNORE: continue 
60640 ! <Updateable Region: ERTN>
60660 ERTN: fnerror(program$,err,line,act$,"xit")
60680   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
60700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60720   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60740 ERTN_EXEC_ACT: execute act$ : goto ERTN
60760 ! /region
72020 SERVICE_LOAD: ! r: Type Of Service Open
72040   open #service=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",RecL=280,use",internal,outin,relative 
72060   F_SERVICE: form pos 1,10*c 20,10*c 2,10*c 1,10*c 1,10*n 2,10*n 2
72080   read #service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply norec TOS_WRITE
72100   goto SERVICE_LOAD_FINIS
72120   TOS_WRITE: ! 
72140   write #service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
72160   SERVICE_LOAD_FINIS: ! 
72180   close #service: 
73000   ! 
73020   for service_item=1 to 10
73040     fncreg_read('default rate '&str$(service_item),default_rate$(service_item))
73050     fncreg_read('Service '&str$(service_item)&' only month',tmp$): onlyMonth(service_item)=val(tmp$)
73060   next service_item
73080 return  ! /r
76000 SERVICE_SAVE: ! r:
76020   open #service=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\Service.h"&env$('cno')&",RecL=280,use",internal,outin,relative 
76040   rewrite #service,using F_SERVICE,rec=1: mat servicename$,mat servicecode$,mat tax_code$,mat penalty$,mat subjectto,mat ordertoapply
76060   close #service: 
77000   ! 
77020   for service_item=1 to 10
77040     fncreg_write('default rate '&str$(service_item),default_rate$(service_item))
77050     fncreg_write('Service '&str$(service_item)&' only month',str$(onlyMonth(service_item)))
77060   next service_item
77080 return  ! /r
82000 COMPANY_LOAD: ! r:
82020   open #h_company:=1: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno'),internal,input 
82040   read #h_company,using "Form POS 1,3*C 40,X 6,N 1,C 1,c 1,n 4": mat at$,maintac,rcpt$,escrow$,pcent ioerr COMPANY_READ_ERR
82060   close #h_company: 
82080   fnd1(d1)
82090   if pcent=0 then pcent=100
82100   if uprc$(rcpt$)=uprc$("Y") then rcpt$="True" else rcpt$="False"
82120   if uprc$(escrow$)=uprc$("Y") then escrow$="True" else escrow$="False"
82140   fncreg_read('unusual usage minimum water',uum_water$)
82160   fncreg_read('unusual usage minimum gas',uum_gas$)
82180   fncreg_read('unusual usage minimum electric',uum_electric$)
82200   fncreg_read('Route Low',bkno1$) : bkno1=val(bkno1$) : if bkno1=0 then bkno1=1
82220   fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$) : if bkno2=0 then bkno2=99
82240 return  ! /r
83000 COMPANY_READ_ERR: ! r:
83020   if err<>714 then goto ERTN
83040   company_rln=rln(h_company)
83060   if company_rln=133 then goto ERTN
83080   pr "Converting UB Company Information"
83100   pr "From Record Length "&str$(company_rln)&" to 133"
83120   close #h_company: ioerr ignore
83140   fnCopy(env$('Q')&"\UBmstr\Company.h"&env$('cno'),env$('Q')&"\UBmstr\Company.h"&env$('cno'), 133)
83160 goto COMPANY_LOAD ! /r
84000 COMPANY_SAVE: ! r:
84020   if rcpt$="True" then rcpt$="Y" else rcpt$="N"
84040   if escrow$="True" then escrow$="Y" else escrow$="N"
84060   maintac=1 ! maintac was variable used for maintaining accumulated transaction file, no longer used but be want history to be retained no matter what (so set it to 1)
84080   close #1,free: ioerr ignore
84100   open #1: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Size=0,RecL=133,Replace",internal,outin 
84120   write #1,using "Form POS 1,3*C 40,x 6,N 1,C 1,c 1,n 4": mat at$,maintac,rcpt$,escrow$,pcent
84140   close #1: 
84160   fnd1(d1,1)
84180   fncreg_write('unusual usage minimum water',uum_water$)
84200   fncreg_write('unusual usage minimum gas',uum_gas$)
84220   fncreg_write('unusual usage minimum electric',uum_electric$)
84240   fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
84260   fncreg_write('Route High',str$(bkno2)) ! Route Number Range High
84280 return  ! /r
86000 def fn_cmb_rate(searchcode$,cr_lyne,cr_pos,ttt$*300,fra)
86020   ! GET_CODES: ! r: get applicable rate codes
86040   ! search routine must be passed code for service (WA for water) in searchcode$
86060   dim rates$(50)*30,rt$*54
86080   open #h_rate1:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
86100   restore #h_rate1: 
86120   cr_rate_item=1: mat rates$=("")
86140   mat rates$(50)
86160   rates$(1)=" 0=Not applicable"
86180   do 
86200     CR_READ_RATE: ! 
86220     read #h_rate1,using "Form POS 1,C 54",release: rt$ eof CR_EO_RATE
86240     if trim$(rt$(1:2))<>searchcode$ then goto CR_READ_RATE
86260     cr_rate_item+=1
86280     rates$(cr_rate_item)=rt$(3:4)&"="&rt$(5:25)
86300     if ratecode=val(rt$(3:4)) then rateinfo$(3)=rt$(3:4)&"="&rt$(5:25)
86320   loop 
86340   CR_EO_RATE: ! 
86360   if cr_rate_item>0 then mat rates$(cr_rate_item) else mat rates$(1)
86380   if ratecode=0 then rateinfo$(3)=" 0=Not applicable"
86400   ! 
86420   close #h_rate1: 
86440   ! /r
86460   fncomboa("ubfm-rates",cr_lyne,cr_pos,mat rates$,ttt$,30,fra)
86480 fnend 
