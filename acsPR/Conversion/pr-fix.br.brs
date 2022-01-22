! Replace S:\acsPR\Conversion\pr-Fix
! beats me...
on error goto Ertn
dim ta(2)
open #1: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno]",i,outIn,k 
open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno]",i,outi,r 
do
	read #1,using L140: lpd,tgp,mat ta eof Xit
	L140: form pos 162,n 6,pd 5.2,2*pd 3
	if lpd then
		tgp=0
		r2=ta(1)
		L180: !
		if r2=0 then goto L250
		read #2,using L200,rec=r2: td4,gpd,nta
		L200: form pos 42,n 6,pos 458,pd 5.2,pos 468,pd 3
		if td4><42394 then goto L230
		tgp+=gpd
		L230: !
		r2=nta
		goto L180
		L250: !
		if tgp then
			rewrite #1,using L140: 32494,tgp
		end if
	end if
loop

Xit: stop 
include: ertn
