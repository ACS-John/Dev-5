! Replace S:\acsUB\conversion\booktitle_altbil
! this program converts a field from ALL CAPITAL LETTERS  to Book Title Capitalization
!
	autoLibrary
	on error goto Ertn
!
	dim nam$*30,ab$(4)*40
	cno=val(env$('cno'))
	pr newpage
L270: pr f "8,20,C 30,R,N": "Book Title Capital"
	pr f "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
!
	io1$(1)="10,40,N 2,UT,N"
L310: rinput fields mat io1$: cno conv L310
	if cno=0 or cmdkey=5 or cmdkey=99 then goto Xit
	open #1: "Name=[Q]\UBmstr\ubadrbil.h"&str$(cno),i,outi,r
	for j=1 to lrec(1)
		read #1,using "form pos 11,4*c 40",rec=j: mat ab$ noRec L390
		for x=1 to 4
			ab$(x)=fnbooktitle$(ab$(x))
		next x
! pr NAM$
		rewrite #1,using "form pos 11,4*C 40",rec=j: mat ab$ noRec L390
L390: next j
	goto DONE
!
DONE: close #1:
	pr "company number "&str$(cno)&" completed successfully"
	goto L270
Xit: stop
include: ertn
