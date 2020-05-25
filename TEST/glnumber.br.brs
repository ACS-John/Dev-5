! Replace Test\GLNumber
! test the functions that send GLNumber comboboxes to the screen and pull and format the response from it.
 
	autoLibrary
	on error goto Ertn
 
! Dim GL$*12,C$*12,P$*30,S$*2,A(3),DCODE$*24,GLC$*24
	dim cap$*128,resp$(10)*50
 
	fnTop(program$,cap$="General Ledger Number")
	right=1 : center=2
	fnTos(sn$="GLNumber") : _
	lc=0 : mylen=30 : mypos=mylen+2
	fnLbl(lc+=1,1,"General Ledger Account Number:",mylen,right)
	fnqgl(lc,mypos) : _
	resp$(1)=fnrgl$('  0   700  0')
! pr RESP$(1) : fnPAUSE ! XXX
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	x$=fnagl$(resp$(1))
	pr 'This is your returned value"'&x$&'".'
	goto Xit
 
Xit: fnXit
 
include: Ertn
 
