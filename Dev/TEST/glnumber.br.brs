! Replace Test\GLNumber
! test the functions that send GLNumber comboboxes to the screen and pull and format the response from it.
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*50
 
	fnTop(program$)
	right=1 : center=2
	fnTos
	lc=0 : mylen=30 : mypos=mylen+2
	fnLbl(lc+=1,1,"General Ledger Account Number:",mylen,right)
	fnQgl(lc,mypos)
	resp$(1)=fnrgl$('  0   700  0')
! pr RESP$(1) : fnPAUSE ! XXX
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	x$=fnagl$(resp$(1))
	pr 'This is your returned value"'&x$&'".'
	goto Xit
 
Xit: fnXit
 
include: ertn
 
