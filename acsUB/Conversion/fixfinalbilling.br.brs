! Replace S:\acsUB\conversion\fixfinalbilling
! this program will createa final billiing core based on the * in front of the alpha sort name.
 
	autoLibrary
	on error goto Ertn
	dim b(11),a(7),d(15),alpha$*7,f2$*12,extra(23),extra$(11)*30,ba(12)
	dim custname$*30,badr(2)
	dim z$*10,e$(4)*30,f$(3)*12,c(4),g(12),adr(2),alp$*7,gb(10)
	dim x$*10,p$*10
 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\uBIndex.h[cno]",internal,outIn,keyed
L130: read #1,using L140: alp$,final eof L190
L140: form pos 354,c 7,pos 1821,n 1
	if alp$(1:1)="*" and final=0 then final=1 ! cnange any zeros to ones
	rewrite #1,using L170: final
L170: form pos 1821,n 1
	goto L130
L190: close #1:
	execute "Index [Q]\UBmstr\Customer.h[cno]"&' '&"[Q]\UBmstr\UBIndx2.h[cno] 354 7 Replace DupKeys"
Xit: fnXit
! __________________________________________________
include: Ertn
