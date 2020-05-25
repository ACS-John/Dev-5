autoLibrary
dim tr(7),tr$*12,td$*30
fnTop(program$)
L40: !
pr newpage
close #101: ioerr ignore
open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION=CONVERT ACCUMULATED TRANSACTIONS FILE",display,outIn 
pr f "10,24,C 32": "ENTER COMPANY NUMBER TO CONVERT:"
pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
L100: input fields "10,57,N 2,UE,N",attr "R": cno conv L100
	if cmdkey=5 then stop 
!
	open #1: "Name=[Q]\GLmstr\AcTrans.h[cno]",internal,input ioerr L100
	pr f "14,32,C 16,BR,N": "   IN PROCESS"
	open #2: "Name=X,size=0,RecL=72,REPLACE",internal,output 
	L160: read #1,using L190: mat tr,tr$,td$ eof L220
	if tr(1)+tr(2)+tr(3)=0 then goto L160
	actpd=int(tr(4)*.0001)
	L190: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
	write #2,using L190: mat tr,tr$,td$,actpd
goto L160
L220: close #1,free: 
	close #2: 
	execute "RENAME X [Q]\GLmstr\AcTrans.h[cno]"
	execute "Index [Q]\GLmstr\AcTrans.h[cno]"&' '&"[Q]\GLmstr\AcTrIdx.h[cno] 1/71/17/13 12/2/2/4 REPLACE DupKeys"
goto L40
