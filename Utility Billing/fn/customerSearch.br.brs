! replace S:\acsUB\Customer_Search.br
! search for a customer and return their act number
!
def library fnCustomerSearch(&x$;fixgrid)
	! x$=account   to extract the flexgrid information (master file)
	autoLibrary
	on error goto Ertn
	dim item$(12)*30,resp$(30)*80,ch$(12),cm$(12)
	fnTos
	open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',internal,input,keyed ioerr Ertn
	Fcustomer: Form POS 1,C 10,pos 1821,c 1,POS 41,C 30,C 30,POS 1864,C 30,POS 101,C 30,POS 11,C 30,POS 1741,C 2,C 7,POS 1894,C 12,POS 131,C 12,pos 354, c 7
	restore #hCustomer: 
	mat ch$(12) : mat cm$(12) : mat cm$(12)
	ch$(1) ='Account'
	ch$(2) ='Status'
	ch$(3) ='Name'
	ch$(4) ='Address'
	ch$(5) ='Address'
	ch$(6) ='City, ST Zip'
	ch$(7) ='Meter Address'
	ch$(8) ='Route'
	ch$(9) ='Sequence'
	ch$(10)='Phone'
	ch$(11)='Meter'
	ch$(12)='Alpha'
	mat cm$=('80') : cm$(2)='61' : cm$(8)='61': cm$(9)='61'
	fnflexinit1('Cust2',1,1,10,72,mat ch$,mat cm$,1)
	do 
		ReadCustomer: ! 
		read #hCustomer,using Fcustomer: mat item$ eof EoCustomer ioerr ErrRead
		fnflexadd1(mat item$)
	loop 

	ErrRead: ! 
		if err<>61 then goto Ertn
		! pr 'Record locked during Customer_Search flexgrid creation - skipped'
		read #hCustomer,release: 
	goto ReadCustomer

	EoCustomer: ! 
		fnCmdSet(2)
		fnAcs(mat resp$,ckey)
		x$=lpad$(resp$(1),10)
		if ckey=5 then x$='          ' ! no one selected
	goto Xit

	Xit: !
	close #hCustomer: ioerr ignore
fnend 
include: ertn
