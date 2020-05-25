! Replace S:\acsPR\Conversion\CrCkHist-cnv
! does something to prCkHist
 
	autoLibrary
	on error goto Ertn
 
	pr newpage
	pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
L90: input fields "10,55,N 5,UE,N": cno conv L90
	if cno=0 then stop
 
	open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,output
	close #4:
	execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys -n"
	chain "S:\acsPR\company"
 
Xit: stop
 
include: Ertn
 
