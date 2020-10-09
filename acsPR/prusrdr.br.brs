! Replace S:\acsPR\PrUsrDR
! pr User-Designed Reports
 
	autoLibrary
 
	dim jcs$(40),cap$*128,rn(20),rn$(20)*74
 
	fnconsole(1)
	fnTop(program$,cap$="User Designed Report (2)")
	open #1: "Name=S:\acsPR\JCReport.mst,KFName=S:\acsPR\JCReport.idx,Shr",internal,input,keyed
	for j=1 to 20 : _
		read #1,using 'Form POS 1,N 2,C 74': rn(j),rn$(j) eof L110 : _
	next j
L110: close #1:
L120: pr newpage : _
	fnopenwin(win=101,3,2,22,79,cap$)
	pr #win: newpage
	for j=1 to 20 : _
		jcs$(j)=str$(j)&",2,Pic(ZZ),N" : _
		jcs$(j+20)=str$(j)&",5,C 74,N" : _
	next j
	pr #win,fields mat jcs$: mat rn,mat rn$
	pr f "23,22,C 09,B,5": "Exit (F5)"
	pr f "23,32,C 23,R,N": "Report Number to Print:"
L180: rinput fields "23,56,Nz 2,UT,N": rno conv L180
	if rno=0 then goto Xit
	for j=1 to 20
		if rno=rn(j) then goto JCPRNT
	next j
	goto L120
 
JCPRNT: chain "S:\acsPR\jcPrnt"&str$(rno)
 
Xit: fnXit
 
include: ertn
 
