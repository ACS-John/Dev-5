00010 ! REPLACE S:\acsCL\conversion\trckno-fix
00020   dim tr(2)
00030   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00040   print newpage
00050   print fields "10,15,C 60": "          COMPANY NUMBER TO BE CHECKED: 2"
00060 L60: input fields "10,55,N 2,UE,N": cno conv L60
00070   if cno=0 then stop 
00080 ! 
00090   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),internal,outin 
00100   open #2: "Name="&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno),internal,outin,relative 
00110 L110: read #trmstr,using 'Form POS 1,N 2,N 1,C 8,POS 79,2*PD 3': bank_code,tcde,ck$,mat tr eof END1
00120 ! IF Bank_Code><4 THEN GOTO 120
00130 ! IF TCDE><1 THEN GOTO 120
00140   let v1=val(ck$) conv L110
00150   if v1<7822 or v1>7848 then goto L110
00160   let ck$=cnvrt$("N 8",v1-3000)
00170   rewrite #trmstr,using 'Form POS 4,C 8': ck$
00180   let ta=0
00190   let r2=tr(1)
00200 L200: if r2=0 then goto L250
00210   read #2,using L220,rec=r2: ok$,nta
00220 L220: form pos 4,c 8,pos 65,pd 3
00230   rewrite #2,using L220,rec=r2: ck$
00240   let r2=nta: goto L200
00250 L250: goto L110
00260 END1: close #trmstr: 
00270   close #2: 
00280   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno)&" 1 11 REPLACE DupKeys"
00290   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno)&" 1/28/4 3/8/8 REPLACE DupKeys"
00300   stop 
