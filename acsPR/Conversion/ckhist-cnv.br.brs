! Replace S:\acsPR\Conversion\CkHist-Cnv
! BUILD PAYROLL CHECK HISTORY FILE
 
	autoLibrary
	on error goto Ertn
 
L70: pr newpage
	pr f "08,08,C 34,R,N": " BUILD PAYROLL CHECK HISTORY FILE"
	pr f "10,5,C 60": "ENTER COMPANY NUMBER TO BE CONVERTED:"
	pr f "12,15,C 16,B,5": "Cancel (F5)"
L110: input fields "10,43,N 5,UE,N": cno conv L110
	if cmdkey=5 then goto Xit
 
	open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],SIZE=0,RecL=150,Replace",internal,output
	close #4:
	execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys -n"
	goto L70
 
include: Ertn
 
Xit: stop
 
