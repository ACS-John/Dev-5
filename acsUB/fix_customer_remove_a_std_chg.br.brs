autoLibrary
! this one time fix program will remove the service X ([pick 1-7]) standard charge from all customers 
service=2 ! NOT READY FOR SERVICES 8, 9 nor 10, ADDITIONAL PROGRAMMING REQUIRED FOR THAT.
! r: customer file
	open #h_customer=fnH: "Name=[Q]\UBmstr\customer.h[cno],KFName=[Q]\UBmstr\ubindex.h[cno],SHR",internal,outIn,keyed 
	dim z$*10,customer_b(7)
	F_CUSTOMER: form pos 1,c 10,pos 157,7*pd 4.2
	customer_change_count=0
	do
		customer_did_change=0
		read #h_customer,using F_CUSTOMER: z$,mat customer_b eof EO_CUSTOMER
		if customer_b(service)<>0 then
			customer_b(service)=0
			customer_did_change+=1
		end if
		if customer_did_change then
			rewrite #h_customer,using F_CUSTOMER: z$,mat customer_b
			customer_change_count+=1
		end if
	loop
	EO_CUSTOMER: !
	close #h_customer:
	pr 'customer_change_count=';customer_change_count
! /r
pr 'go' : pause
t:  fnXit
