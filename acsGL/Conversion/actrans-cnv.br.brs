00020   dim tr(7),tr$*12,td$*30,cnam$*40
00030   library 'S:\Core\Library': fntop
00040 L40: pr newpage
00050   fntop(program$,"CHANGE_ME")
00060   close #101: ioerr ignore
00070   open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION=CONVERT ACCUMULATED TRANSACTIONS FILE",display,outIn 
00080   pr f "10,24,C 32": "ENTER COMPANY NUMBER TO CONVERT:"
00090   pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
00100 L100: input fields "10,57,N 2,UE,N",attr "R": cno conv L100
00110   if cmdkey=5 then stop 
00120 ! 
00130   open #1: "Name="&env$('Q')&"\GLmstr\AcTrans.h"&env$('cno'),internal,input ioerr L100
00140   pr f "14,32,C 16,BR,N": "   IN PROCESS"
00150   open #2: "Name=X,size=0,RecL=72,REPLACE",internal,output 
00160 L160: read #1,using L190: mat tr,tr$,td$ eof L220
00170   if tr(1)+tr(2)+tr(3)=0 then goto L160
00180   actpd=int(tr(4)*.0001)
00190 L190: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
00200   write #2,using L190: mat tr,tr$,td$,actpd
00210   goto L160
00220 L220: close #1,free: 
00230   close #2: 
00240   execute "RENAME X "&env$('Q')&"\GLmstr\AcTrans.h"&env$('cno')
00250   execute "Index "&env$('Q')&"\GLmstr\AcTrans.h"&env$('cno')&' '&env$('Q')&"\GLmstr\AcTrIdx.h"&env$('cno')&" 1/71/17/13 12/2/2/4 REPLACE DupKeys"
00260   goto L40
01260 IGNORE: continue 
