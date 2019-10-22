! 
dim ta(2),z$*5,e$(3)*30,id$*20,ar(5),tr(6)
open #hTrans:=2: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative 
open #hClient:=1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed 
do
	read #hClient,using FORM_CLMSTR: z$,mat e$,bal,mat ta eof END1
	FORM_CLMSTR: form pos 1,c 5,pos 6,3*c 30,pos 283,pd 5.2,pos 299,2*pd 3
	ta1=ta(1)
	do
		if ta1=0 then goto END2
		read #hTrans,using L110,rec=ta1: p$,iv$,mat tr,id$,nta
		L110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
		tr1$=cnvrt$("n 6",tr(1))
		mo1=val(tr1$(1:2))
		da1=val(tr1$(3:4))
		yr1=val(tr1$(5:6))
		if yr1><11 then goto L190
		if mo1><4 then goto L190
		L180: ta1=nta
	loop
	L190: if tr(5)<4 or tf(5)=5 then bal=bal-tr(3) else bal=bal+tr(3)
	rewrite #hClient,using FORM_CLMSTR,key=z$: z$,mat e$,bal,mat ta
	tr(3)=0
	rewrite #hTrans,using L110,rec=ta1: p$,iv$,mat tr,id$,nta
	goto L180
	END2: !
loop
Xit: stop 
