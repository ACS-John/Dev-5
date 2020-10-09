! Replace S:\acsUB\conversion\csz-wipe
! this program wipes CSZs from the UB customer file and replaces them with whatever you tell it to
 
	autoLibrary
	on error goto Ertn
 
	dim csz$*30
 
	fncno(cno)
	pr newpage
L110: pr f "8,20,C 30,R,N": "Mask Cisty State Zip"
	pr f "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
	pr f "11,1,Cr 38": "New City State and Zip:"
	io1$(1)="10,40,N 2,UT,N" : io1$(2)="11,40,C 30,UT,N"
L150: rinput fields mat io1$: cno,csz$ conv L150
	if cno=0 or cmdkey=5 or cmdkey=99 then goto Xit
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubindex.h[cno],Shr",internal,outIn,keyed
	for j=1 to lrec(1)
		rewrite #1,using "Form Pos 101,c 30",rec=j: csz$ noRec L210
		pr f "1,1,N 10,R,N": j
L210: next j
	goto DONE
 
DONE: close #1:
	pr "company number [cno] completed successfully"
	goto L110
Xit: stop
 
include: ertn
 
