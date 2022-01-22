! Replace S:\acsUB\conversion\booktitle
! this program converts a field from ALL CAPITAL LETTERS : _
	! to Book Title Capitalization
 
	autoLibrary
	on error goto Ertn
 
	dim nam$*30
 
def fn_booktitle$*80(x$*80)
		x$=lwrc$(trim$(x$)) : olda=0
		x$(1:1)=uprc$(x$(1:1))
! capitalize anthing after a SPACE
L130: a=pos(x$," ",olda) : _
		if a<>0 then : _
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L130
		a=olda=0
L150: a=pos(x$,"-",olda) : _
		if a<>0 then : _
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L150
		a=olda=0
L170: a=pos(x$,"/",olda) : _
		if a<>0 then : _
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L170
		a=olda=0
L190: a=pos(x$,"\",olda) : _
		if a<>0 then : _
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L190
		a=olda=0
L210: a=pos(x$,".",olda) : _
		if a<>0 then : _
			a+=1 : x$(a:a)=uprc$(x$(a:a)) : olda=a : goto L210
		fn_booktitle$=x$
fnend
 
	fncno(cno)
	pr newpage
L270: pr f "8,20,C 30,R,N": "Book Title Capital"
	pr f "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
 
	io1$(1)="10,40,N 2,UT,N"
L310: rinput fields mat io1$: cno conv L310
	if cno=0 or cmdkey=5 or cmdkey=99 then goto Xit
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubindex.h[cno],Shr",i,outIn,k
	for j=1 to lrec(1)
		read #1,using "form pos 71,c 30",rec=j: nam$ noRec L390
		nam$=fn_booktitle$(nam$)
		pr nam$
		rewrite #1,using "form pos 71,c 30",rec=j: nam$ noRec L390
L390: next j
	goto DONE
 
DONE: close #1:
	pr "company number [cno] completed successfully"
	goto L270
Xit: stop
 
include: ertn
 
