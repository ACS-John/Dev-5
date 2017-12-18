24000   library "S:\Core\Library.br": fncno,fnxit,fngethandle
24020   ! this one time fix program will combine two services into one.
24040   ! this will combine them both in the customer file as well as the transaction files
24060   ! service_from - service to be removed 
24080   ! service_to - service to be added to 
24100   ! all breakdown amounts in service_from will be zeroed out and ADDED into service_to
24120   service_from=5
24140   service_to=10
24160   fncno(cno)
30000   ! r: customer file
30020     open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubindex.h"&env$('cno')&",SHR",internal,outin,keyed 
30040     dim z$*10,charge(10)
30060     F_CUSTOMER: form pos 1,c 10,pos 300,10*pd 4.2,pos 388,10*pd 5.2
30080     customer_change_count=0
30100     do
30120       customer_did_change=0
30140       read #h_customer,using F_CUSTOMER: z$,mat customer_g,mat gb eof EO_CUSTOMER
30160       if customer_g(service_from)<>0 then
30170         customer_g(service_to)+=customer_g(service_from)
30180         customer_g(service_from)=0
30200         customer_did_change+=1
30220       end if
30240       if gb(service_from)<>0 then
30260         gb(service_to)+=gb(service_from)
30270         gb(service_from)=0
30280         customer_did_change+=1
30300       end if
30320       if customer_did_change then
30340         rewrite #h_customer,using F_CUSTOMER: z$,mat customer_g,mat gb
30360         customer_change_count+=1
30380       end if
30400     loop
30420     EO_CUSTOMER: !
30440     close #h_customer:
30460     pr 'customer_change_count=';customer_change_count
30480   ! /r
40000   ! r: trans file
40020     open #h_trans:=fngethandle: "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&env$('cno')&",SHR",internal,outin,keyed 
40040     dim a$*10,tg(10)
40060     TRANSFORM: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
40080     trans_change_count=0
40100     do 
40120       trans_did_change=0
40140       read #h_trans,using TRANSFORM: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode eof EO_TRANS
40160       if tg(service_from)<>0 then
40180         tg(service_to)+=tg(service_from)
40190         tg(service_from)=0
40200         trans_did_change+=1
40220       end if
40240       if trans_did_change then
40260         rewrite #h_trans,using TRANSFORM: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode
40280         trans_change_count+=1
40300       end if
40320     loop 
40340     EO_TRANS: !
40360     close #h_trans:
40380     pr 'trans_change_count=';trans_change_count
40400   ! /r
50000   pr 'go' : pause
55000 XIT: end ! fnxit
