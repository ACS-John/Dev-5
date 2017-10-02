00010 ! REPLACE S:\acsCL\conversion\TrAlloc-fix
00020   dim tr(2)
00030 ! 
00040   pr newpage
00050   pr fields "10,28,C 60": "ENTER COMPANY #: 1"
00060 L60: input fields "10,45,N 2,UE,N": cno conv L60
00070   if cno=0 then stop 
00080 ! 
00090   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno)&",Shr",internal,outin,keyed 
00100   open #3: "Name="&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&",Shr",internal,outin,relative 
00110 L110: read #trmstr,using 'Form POS 1,C 11,POS 79,2*PD 3': k$,mat tr eof END1
00120   adr=tr(1)
00130 L130: if adr=0 then goto L180
00140   read #3,using L150,rec=adr: ok$,ntr
00150 L150: form pos 1,c 11,pos 65,pd 3
00160   rewrite #3,using L150,rec=adr: k$
00170   adr=ntr: goto L130
00180 L180: goto L110
00190 END1: stop 
