24000   library "S:\Core\Library.br": fncno,fnxit,fngethandle
24020   ! this one time fix program will remove the service X ([pick 1-7]) standard charge from all customers 
24120   service=2 ! NOT READY FOR SERVICES 8, 9 nor 10, ADDITIONAL PROGRAMMING REQUIRED FOR THAT.
24160   fncno(cno)
30000   ! r: customer file
30020     open #h_customer:=fngethandle: "Name="&env$('Q')&"\UBmstr\customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubindex.h"&str$(cno)&",SHR",internal,outin,keyed 
30040     dim z$*10,customer_b(7)
30060     F_CUSTOMER: form pos 1,c 10,pos 157,7*pd 4.2
30080     customer_change_count=0
30100     do
30120       customer_did_change=0
30140       read #h_customer,using F_CUSTOMER: z$,mat customer_b eof EO_CUSTOMER
30160       if customer_b(service)<>0 then
30170         customer_b(service)=0
30200         customer_did_change+=1
30220       end if
30320       if customer_did_change then
30340         rewrite #h_customer,using F_CUSTOMER: z$,mat customer_b
30360         customer_change_count+=1
30380       end if
30400     loop
30420     EO_CUSTOMER: !
30440     close #h_customer:
30460     pr 'customer_change_count=';customer_change_count
30480   ! /r
50000   pr 'go' : pause
55000 XIT:  fnxit
