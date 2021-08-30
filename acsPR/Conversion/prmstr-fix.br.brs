! Replace S:\acsPR\Conversion\PRmstr-Fix
! not sure
on error goto Ertn
dim em$(3)*30,ss$*11,rs(2),em(16),ta(2),tr$*467

open #10: "Name=[Q]\PRmstr\RPMstr.h[cno]",i,i,r 
open #11: "Name=RPMSTR.X,RecL=196,Replace",internal,output 
open #12: "Name=[Q]\PRmstr\RPTRAIL.h[cno]",i,i,r 
open #13: "Name=RPTRAIL.X,SIZE=0,RecL=474,Replace",i,outi,r 
ot13=1
write #13,using L170,rec=1: mat tdz,ot13,0
L170: form pos 1,n 8,n 3,n 3,n 6,n 3,4*n 6,3*n 2,24*pd 4.2,6*pd 3.2,60*pd 5.2,pd 3,pd 4.2
form pos 1,n 8,n 3,n 3,n 6,n 3,4*n 6,3*n 2,24*pd 4.2,5*pd 3.2,pos 471,pd 4.2,pos 165,pd 3.2,60*pd 5.2,pd 3
L190: !
r10=r10+1
if r10>lrec(10) then goto END1
read #10,using L220,rec=r10: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd noRec L190,conv L190,eof END1
L220: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,2*pd 3,c 12,n 6
adr=ta(1)
mat ta=(0)
L250: !
	if adr=0 then goto L370
	read #12,using L270,rec=adr: tr$,adr,wkm
	L270: form pos 1,c 467,pd 3,pd 4.2
	ot13=lrec(13)+1
	if adr=0 then ota=0 else ota=ot13+1
	write #13,using L270,rec=ot13: tr$,ota,wkm
	if ta(1)=0 then ta(1)=ot13
	ta(2)=ot13
	rewrite #13,using L340,rec=1: ot13
	L340: form pos 468,pd 3
goto L250

L370: !
	write #11,using L220: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
goto L190

END1: !
	close #10,free: 
	close #11: 
	close #12,free: 
	close #13: 
	execute "RENAME RPMSTR.X [Q]\PRmstr\RPMstr.h[cno] -n"
	execute "RENAME RPTRAIL.X [Q]\PRmstr\RPTRAIL.h[cno] -n"
	execute "Index [Q]\PRmstr\RPMstr.h[cno]"&' '&"[Q]\PRmstr\RPIndex.h[cno] 1 8 Replace DupKeys -n"
	execute "Index [Q]\PRmstr\RPMstr.h[cno]"&' '&"[Q]\PRmstr\RPIndx2.h[cno] 9 30 Replace DupKeys -n"
Xit: stop 
include: ertn
