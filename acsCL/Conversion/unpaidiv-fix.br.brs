00010 ! REPLACE S:\acsCL\conversion\unpaidiv-fix
00020 ! check for posting codes in unpaid file
00030 ! 
00040   dim tr(2),up$(4),aa(2)
00050   pr newpage
00060   pr f "10,15,C 60": "ENTER THE COMPANY NUMBER TO BE CHECKED: 1"
00070 L70: input fields "10,55,N 2,UE,N": cno conv L70
00080   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00090   if cno=0 then stop 
00100 ! 
00110   open #1: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",Shr",internal,outin,relative 
00120 L120: read #1,using L130: vn$,iv$,mat up$,upa,pcde,bank_code,ckn,cp,mat aa,gde eof END1
00130 L130: form pos 1,c 8,c 12,g 6,g 6,c 12,c 18,n 10.2,n 1,n 2,g 8,g 6,2*pd 3,n 1
00140   dat=val(up$(1)) conv L120
00150   if fncd(dat)>970101 then goto L120
00160   if gde= 0 then rewrite #1,using L170: 2
00170 L170: form pos 96,n 1
00180   if gde=0 then pr #255,using L190: vn$,iv$,up$(1)
00190 L190: form pos 1,c 10,c 15,c 10,skip 1
00200   goto L120
00210 END1: stop 
