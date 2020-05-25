! Replace S:\acsUB\conversion\fixroutes
! -- Custom written for Monticello to change route and sequence numbers from a text file
 
	autoLibrary
	on errror goto ERTN
 
	dim text$*40,cap$*128,ln$*128
 
	fncno(cno)
 
	fnTop("S:\acsUB\TotalBal",cap$="Change Route and Sequence Numbers")
	fnopenprn
 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=newroute2.txt",display,input
READ_CUSTOMER: !
L170: linput #2: ln$ eof Xit
! z$=LPAD$(RTRM$(LN$(17:26)),10)
	z$=lpad$(rtrm$(ln$(1:9)),10)
! rOUTE=VAL(LN$(1:7))
	route=val(ln$(81:81))
! sEQUENCE=VAL(LN$(9:15))
	sequence=val(ln$(72:79))
	pr z$,route,sequence: pause
	read #1,using "Form POS 1,c 10,pos 1741,n 2,pos 1743,n 7",key=z$: oldz$,oldroute,oldsequence nokey L250
	rewrite #1,using "Form pos 1741,n 2,pos 1743,n 7": route,sequence
	goto READ_CUSTOMER
 
L250: pr #255,using "form pos 1,c 50": "Account "&z$&" not found"
	goto L170
Xit: fncloseprn
	close #1:
	execute "Index [Q]\UBmstr\Customer.h[cno]"&' '&"[Q]\UBmstr\UBIndx5.h[cno] 1741/1743 2/7 Replace DupKeys -n"
	fnXit
 
include: Ertn
