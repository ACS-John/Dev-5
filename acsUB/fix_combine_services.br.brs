autoLibrary
! this one time fix program will combine two services into one.
! this will combine them both in the customer file as well as the transaction files
! service_from - service to be removed 
! service_to - service to be added to 
! all breakdown amounts in service_from will be zeroed out and ADDED into service_to
service_from=5
service_to=10
! r: customer file
	open #h_customer:=fnH: "Name=[Q]\UBmstr\customer.h[cno],KFName=[Q]\UBmstr\ubindex.h[cno],SHR",internal,outIn,keyed 
	dim z$*10,charge(10)
	F_CUSTOMER: form pos 1,c 10,pos 300,10*pd 4.2,pos 388,10*pd 5.2
	customer_change_count=0
	do
		customer_did_change=0
		read #h_customer,using F_CUSTOMER: z$,mat customer_g,mat gb eof EO_CUSTOMER
		if customer_g(service_from)<>0 then
			customer_g(service_to)+=customer_g(service_from)
			customer_g(service_from)=0
			customer_did_change+=1
		end if
		if gb(service_from)<>0 then
			gb(service_to)+=gb(service_from)
			gb(service_from)=0
			customer_did_change+=1
		end if
		if customer_did_change then
			rewrite #h_customer,using F_CUSTOMER: z$,mat customer_g,mat gb
			customer_change_count+=1
		end if
	loop
	EO_CUSTOMER: !
	close #h_customer:
	pr 'customer_change_count=';customer_change_count
! /r
! r: trans file
	open #h_trans:=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],SHR",internal,outIn,keyed 
	dim a$*10,tg(10)
	TRANSFORM: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
	trans_change_count=0
	do 
		trans_did_change=0
		read #h_trans,using TRANSFORM: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode eof EO_TRANS
		if tg(service_from)<>0 then
			tg(service_to)+=tg(service_from)
			tg(service_from)=0
			trans_did_change+=1
		end if
		if trans_did_change then
			rewrite #h_trans,using TRANSFORM: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode
			trans_change_count+=1
		end if
	loop 
	EO_TRANS: !
	close #h_trans:
	pr 'trans_change_count=';trans_change_count
! /r
pr 'go' : pause
Xit: end ! fnXit
