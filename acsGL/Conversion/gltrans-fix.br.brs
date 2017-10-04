00020   dim ta(2),tr(7),tr$*12,td$*30,cnam$*40
00030 ! 
00040   pr newpage
00041   library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fntop
00048   fntop(program$,"CHANGE_ME")
00050   close #101: ioerr L60
00051   pause 
00060 L60: open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION=CONVERT ACCUMULATED TRANSACTIONS FILE",display,outin 
00070   pr f "10,24,C 32": "ENTER COMPANY NUMBER TO CONVERT:"
00080   pr f "12,32,C 16,R,N": "PRESS F5 TO STOP"
00090 L90: input fields "10,57,N 2,UE,N",attr "R": cno conv L90
00100   if cmdkey=5 then stop 
00110 ! 
00120   open #1: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno),internal,input ioerr L90
00130   open #2: "Name="&env$('Temp')&"\Work."&session$&",SIZE=0,RecL=73,REPLACE",internal,output 
00140   pr f "14,32,C 16,BR,N": "   IN PROCESS"
00150 L150: read #1,using L190: mat tr,tr$,td$ eof L220
00160   if tr(1)+tr(2)+tr(3)=0 then goto L150
00170 ! If FP(TR(4)*.01)*100<93 Then Goto 150
00171   if fndate_mmddyy_to_ccyymmdd(tr(4))<20030101 then goto L150
00180   actpd=int(tr(4)*.0001)
00190 L190: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,pd 3
00200   write #2,using L190: mat tr,tr$,td$,0
00210   goto L150
00220 L220: close #1,free: 
00230   close #2: 
00240   execute "RENAME "&env$('Temp')&"\Work."&session$&' '&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno)
00250   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),internal,outin,keyed 
00260   open #2: "Name="&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno),internal,outin,relative 
00270   pr newpage
00280   pr f "10,15,c 60,h,n": "REASSIGN TRANSACTION ADDRESSES IN PROCESS"
00290   restore #1,key>="            ": eof L300
00300 L300: read #1,using L310: mat ta eof L340
00310 L310: form pos 333,2*pd 3
00320   rewrite #1,using L310: 0,0
00330   goto L300
00340 L340: lr2=lrec(2)
00350   rewrite #2,using L450,rec=1: lr2
00360   for j=1 to lr2
00370     read #2,using L380,rec=j: k$,nta norec L460
00380 L380: form pos 1,c 12,pos 71,pd 3
00390     read #1,using L310,key=k$: mat ta nokey L460
00400     if ta(1)=0 then let ta(1)=j
00410     if ta(2)>0 then rewrite #2,using L450,rec=ta(2): j
00420     let ta(2)=j
00430     rewrite #1,using L310,key=k$: mat ta
00440     rewrite #2,using L450,rec=j: 0
00450 L450: form pos 71,pd 3
00460 L460: next j
00470   stop 
