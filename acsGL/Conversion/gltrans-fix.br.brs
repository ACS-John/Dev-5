	dim ta(2),tr(7),tr$*12,td$*30,cnam$*40
! 
	pr newpage
	autoLibrary
	fnTop(program$)
	close #101: ioerr ignore
	open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION=CONVERT ACCUMULATED TRANSACTIONS FILE",display,outIn 
	pr f "10,24,C 32": "ENTER COMPANY NUMBER TO CONVERT:"
	pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
L90: input fields "10,57,N 2,UE,N",attr "R": cno conv L90
	if cmdkey=5 then stop 
! 
	open #1: "Name=[Q]\GLmstr\GLTrans.h[cno]",internal,input ioerr L90
	open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=73,REPLACE",internal,output 
	pr f "14,32,C 16,BR,N": "   IN PROCESS"
L150: read #1,using L190: mat tr,tr$,td$ eof L220
	if tr(1)+tr(2)+tr(3)=0 then goto L150
! If FP(TR(4)*.01)*100<93 Then Goto 150
	if fndate_mmddyy_to_ccyymmdd(tr(4))<20030101 then goto L150
	actpd=int(tr(4)*.0001)
L190: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
	write #2,using L190: mat tr,tr$,td$,0
	goto L150
L220: close #1,free: 
	close #2: 
	execute "RENAME "&env$('Temp')&"\Work."&session$&' '&"[Q]\GLmstr\GLTrans.h[cno]"
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno]",internal,outIn,keyed 
	open #2: "Name=[Q]\GLmstr\GLTrans.h[cno]",internal,outIn,relative 
	pr newpage
	pr f "10,15,c 60,h,n": "REASSIGN TRANSACTION ADDRESSES IN PROCESS"
	restore #1,key>="            ": eof L300
L300: read #1,using L310: mat ta eof L340
L310: form pos 333,2*pd 3
	rewrite #1,using L310: 0,0
	goto L300
L340: lr2=lrec(2)
	rewrite #2,using L450,rec=1: lr2
	for j=1 to lr2
		read #2,using L380,rec=j: k$,nta noRec L460
L380: form pos 1,c 12,pos 71,pd 3
		read #1,using L310,key=k$: mat ta nokey L460
		if ta(1)=0 then ta(1)=j
		if ta(2)>0 then rewrite #2,using L450,rec=ta(2): j
		ta(2)=j
		rewrite #1,using L310,key=k$: mat ta
		rewrite #2,using L450,rec=j: 0
L450: form pos 71,pd 3
L460: next j
	stop 
