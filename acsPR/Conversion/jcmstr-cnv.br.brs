! Replace S:\acsPR\Conversion\JCmstr-Cnv
! Convert Job Cost and Category Files
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2)
	dim contact$*30,ph$*12,email$*60
L80: pr newpage
	close #101: ioerr L100
L100: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT JOB COST AND CATEGORY FILES",display,outIn
	pr f "10,10,C 60": "COMPANY NUMBER (0 to stop):"
	pr f "12,20,C 32,R,N": "PRESS F1 TO CONTINUE; F5 TO STOP"
	input fields "10,56,Nz 5,U,N": cno
	if cmdkey=5 then goto Xit
 
	execute "Copy [Q]\PRmstr\JCMSTR.h[cno],X -D -n"
	open #2: "Name=X",internal,outIn,relative
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],RecL=300,Replace",internal,outIn,relative
	for j=1 to lrec(2)
		read #2,using L210,rec=j: jn$,n$,mat a$,mat b
L210: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 5.2,n 2
		if pcde=0 then pdte=0 else pdte=lpd
		write #1,using L231: jn$,n$,mat a$,mat b,contact$,ph$,email$
L231: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2,c 30,c 12,c 60
	next j
	close #1:
	close #2:
	execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCIndx.h[cno],1,6,Replace,DupKeys -n"
	execute "Copy [Q]\PRmstr\JCCAT.h[cno],X -D -n"
	open #2: "Name=X",internal,outIn,relative
	open #1: "Name=[Q]\PRmstr\JCCAT.h[cno],RecL=123,Replace",internal,outIn,relative
	for j=1 to lrec(2)
		read #2,using L340: cn$,k$,mat l,mat ta
L340: form pos 1,c 11,c 25,11*pd 5.2,2*pd 2,2*pd 3
		write #1,using L360: cn$,k$,mat l,mat ta
L360: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
	next j
	close #1:
	close #2:
	execute "Index [Q]\PRmstr\JCCAT.h[cno],[Q]\PRmstr\CatIndx.h[cno],1,11,Replace,DupKeys"
	pr f "12,5,C 60": "COMPLETED CONVERTING JOB FILE FOR COMPANY #: [cno]"
	pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
	input fields "13,40,C 1,IAE,N": pause$
	goto L80
 
include: ertn
 
Xit: fnXit
 
