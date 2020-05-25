! Replace S:\acsPR\Conversion\prCkHist-Fix
! CONVERT FOR CC CHG
 
autoLibrary
on error goto Ertn
 
dim tdc(6),tc2(22)
 
pr newpage
pr f "10,15,C 50": "ENTER COMPANY # TO CONVERT OR 0 TO STOP:"
L130: input fields "10,55,N 2,UE,N": cno conv L130
if cno=0 then goto Xit
 
	open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,outIn,relative
L170: r1+=1
	read #4,using L190,rec=r1: eno,prd,ckno,mat tdc,mat tc2 eof L330,conv L220,noRec L330
L190: form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
	goto L170
 
L220: read #4,using L230,rec=r1: eno,prd,ckno,mat tdc,mat tc2 eof L330,conv L290,noRec L330
L230: form pos 1,n 8,n 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2
	prd=19000000+fncd(prd)
	rewrite #4,using L260,rec=r1: prd
L260: form pos 9,pd 6
	goto L170
 
L290: read #4,using L230,rec=r1: eno conv L300
L300: delete #4,rec=r1:
	goto L170
 
L330: if r1<lrec(4) then goto L170
	close #4:
	execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys"
Xit: stop
 
include: Ertn