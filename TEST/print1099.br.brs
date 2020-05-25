! Replace Test\Print1099
! pr 1099 Forms (From a File)
 
	autoLibrary
	on error goto Ertn
 
	dim cinfo$(6)*40,einfo$(6)*40,box(19)
 
	fncno(cno,cnam$)
	fnTop("Test\Print1099",cap$="Print 1099s")
	for j=1 to 3
		cinfo$(1)='Company Name' : _
		cinfo$(2)='company Address (1)' : _
		cinfo$(3)='Company Address (2)' : _
		cinfo$(4)='Company CSZ' : _
		cinfo$(5)='Company Phone Number' : _
		cinfo$(6)='Company Federal ID'
		einfo$(1)='Employee ID '&str$(j) : _
		einfo$(2)='Employee Name' : _
		einfo$(3)='Employee Address (1)' : _
		einfo$(4)='Employee Address (2)' : _
		einfo$(5)='Employee CSZ' : _
		einfo$(6)='Employee Account Number'
		box(1)=123 : box(2)=234.3 : box(3)=123.1 : box(4)=123
		fnadd1099(mat cinfo$,mat einfo$,mat box)
	next j
	lz1$='L'
	chain 'S:\Core\Print1099',lz1$ ! fnPRINT1099('L')
	pr ' you should some 1099'
	goto Xit
 
Xit: stop
 
include: Ertn
 
