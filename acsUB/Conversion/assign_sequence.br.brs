! Replace S:\acsUB\conversion\assign_sequence
! used to assign new sequence numbers if not enough room between numbers on standard conversion
autoLibrary
on error goto Ertn
fnTop("S:\acsUB\UBZeroYt","Assign Sequence")
SCREEN1: !
	fnTos
	mylen=25 : mypos=mylen+2
	fnLbl(1,1,'Increment by what number:',mylen,1)
	fnTxt(1,mypos,3,0,1,"30")
	resp$(1)=""
	fnCmdSet(2): fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	incr=val(resp$(1))
	on fkey 5 goto DONE
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
READ_CUSTOMER: ! : _
	read #1,using "Form POS 1743,n 7": oldseq eof DONE
	newseq=newseq+max(incr,10)
	rewrite #1,using "Form pos 1743,n 7": newseq
goto READ_CUSTOMER
DONE: close #1:
goto Xit
Xit: fnXit
include: ertn