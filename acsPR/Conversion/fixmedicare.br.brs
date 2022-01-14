! Replace S:\acsPR\conversion\fixmedicare
! special program to fix medicare wh
 
	autoLibrary
	on error goto Ertn
 
	dim tcp(32)
	dim tdc(10)
 
	fnTop("S:\acsPR\fixmedicare",cap$="Fix Medicare")
	open #2: "Name=[Q]\PRmstr\RPMstr.h[cno],KFName=[Q]\PRmstr\RPIndex.h[cno],Shr",i,i,k
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
	L210: !
		read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof Xit
		if tcp(3)=0 and tcp(2)>0 then tcp(3)=round(tcp(2)*.189542,2): tcp(2)=tcp(2)-tcp(3) : goto L240
	goto L210
	L240: !
		rewrite #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp
	goto L210
 
Xit: fnXit
include: ertn
