! Replace S:\acsPR\Conversion\prCkHist-Cnv
! CONVERT FOR CC CHG
 
	autoLibrary
	on error goto Ertn
 
	pr newpage
	pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
L170: input fields "10,55,N 2,UE,N": cno conv L170
	if cno=0 then goto Xit
 
	open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,outIn
L210: read #4,using L220: d1 eof L310,conv L280
L220: form pos 9,n 6
	d1=fndate_mmddyy_to_ccyymmdd(d1) ! d1=19000000+FNCD(D1)
	rewrite #4,using 'form pos 9,pd 6': d1
	goto L210
 
L280: read #4,using 'form pos 9,pd 6': d1 eof L310
	goto L210
 
L310: close #4:
	execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys -n"
Xit: stop
 
include: ertn
 
