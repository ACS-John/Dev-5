! Replace UNPAIDIV.FIX/CLSP
! check for posting codes in unpaid file
autoLibrary
dim tr(2),up$(4),aa(2)
! pr newpage
! pr f "10,15,C 60": "ENTER THE COMPANY NUMBER TO BE CHECKED: 1"
! L70: input fields "10,55,N 2,UE,N": cno conv L70
! 	if cno=0 then stop 
 
open #1: "Name=[Q]\CLmstr\PayTrans.h[cno],Shr",internal,outIn,relative 
L120: !
read #1,using L130: vn$,iv$,mat up$,upa,pcde,bank_code,ckn,cp,mat aa,gde eof END1
L130: form pos 1,c 8,c 12,g 6,g 6,c 12,c 18,n 10.2,n 1,n 2,g 8,g 6,2*pd 3,n 1
dat=val(up$(1)) conv L120
if fncd(dat)>970101 then goto L120
if gde= 0 then rewrite #1,using L170: 2
L170: form pos 96,n 1
if gde=0 then pr #255,using L190: vn$,iv$,up$(1)
L190: form pos 1,c 10,c 15,c 10,skip 1
goto L120
END1: stop 
