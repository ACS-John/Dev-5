00020 ! 
00030   dim tr(2)
00050   pr newpage
00060   pr f "10,15,C 60": "ENTER THE COMPANY NUMBER TO BE CHECKED: 2"
00070 L70: input fields "10,55,N 2,UE,N": cno conv L70
00080   if cno=0 then stop 
00090 ! 
00100   open #1: "Name=[Q]\CLmstr\TRMSTR.H[cno]",internal,outIn 
00110   open #2: "Name=[Q]\CLmstr\TRALLOC.h[cno]",internal,outIn,relative 
00120 L120: read #1,using L130: ck$,mat tr eof END1
00130 L130: form pos 4,c 8,pos 79,2*pd 3
00150   v1=val(ck$) conv L120
00151   if v1>13070 then goto END1
00152   if v1<13042 or v1>13070 then goto L120
00160   ck$=cnvrt$("N 8",v1+1)
00170   rewrite #1,using L130: ck$
00180   ta=0
00190   r2=tr(1)
00200 L200: if r2=0 then goto L250
00210   read #2,using L220,rec=r2: ok$,nta
00220 L220: form pos 4,c 8,pos 65,pd 3
00230   rewrite #2,using L220,rec=r2: ck$
00240   r2=nta: goto L200
00250 L250: goto L120
00260 END1: stop 
