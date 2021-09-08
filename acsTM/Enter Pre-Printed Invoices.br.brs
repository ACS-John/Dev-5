autoLibrary
on error goto Ertn
on fkey 5 goto L1530
fnTop(program$,"Preprinted Invoices")
fnopenprn
namtab=int(66-(len(rtrm$(env$('Program_Caption')))/2))
dim scr1$(8),fl1$(11),in1$(10),ot1$(10),scrid$(3)*80,inp(9),iv$*12,a1$*30
dim m$*11,fm$*54,fp$*80,pt(4),fl2$(7),scr2$(4),ot2$(4)
open #1: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input
read #1,using L140: dept,subac
L140: form pos 162,2*n 1
close #1:
x8=8+subac+dept
if subac=0 and dept=0 then m$="x 3,n 6,x 3"
if subac=1 and dept=0 then m$="x 3,n 6,n 3"
if subac=1 and dept=1 then m$="n 3,n 6,n 3"
if subac=0 and dept=1 then m$="n 3,n 6,x 3"
fm$="FORM POS 1,n 5,n 1,pd 4.2,n 6,n 2,"&m$&",n 2,c 12"
fp$="form pos 1,n 5,n 10,n 10,n 15.2,n 8,n 8,x 7,"&m$&",n 10,x 7,c 12,skip 1"
fl1$(9)="2,10,c 60,h,n"
fl1$(10)="3,10,c 60,h,n"
fl1$(11)="15,10,c 70,h,n"
fl2$(5)="2,10,c 60,h,n"
fl2$(6)="14,10,c 60,h,n"
fl2$(7)="15,10,c 70,h,n"
in1$(1)="5,25,n 5,ut,n"
in1$(2)="6,25,n 1,ut,n"
in1$(3)="7,25,n 10.2,Cu,n"
in1$(4)="8,25,n 6,ut,n"
in1$(5)="9,25,n 2,ut,n"
if dept=1 then in1$(6)="10,20,n 3,ut,n"
in1$(6+dept)="10,25,n 6,ut,n"
if subac=1 then in1$(7+dept)="10,34,n 3,ut,n"
in1$(7+subac+dept)="11,25,n 2,ut,n"
in1$(8+subac+dept)="12,25,c 12,ut,n"
for j=1 to x8
	if j<9 then fl1$(j)=str$(j+4)&",2,c 20"
	if j<5 then ot2$(j)=str$(j+4)&",25,n 10.2,ut,n"
	if j<5 then fl2$(j)=fl1$(j)
	ot1$(j)=in1$(j)
next j
ot1$(3)="7,25,n 10.2,ut,n"
mat inp(x8-1)
data "CLIENT #"
data "BILLING CODE"
data "AMOUNT"
data "DATE"
data "CATEGORY"
data "G/L NUMBER"
data "SUB CATEGORY"
data "INVOICE #"
read mat scr1$
scr2$(1)="CLIENT #S"
scr2$(2)="AMOUNTS"
scr2$(3)="CATEGORIES"
scr2$(4)="SUB CATEGORIES"
open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,i,k
open #11: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",i,i,k
open #2: "Name=S:\Core\Data\acsllc\TMWk2[acsUserId].h[cno]",i,outi,r ioerr L630
close #2,free:
L630: !
open #2: "Name=S:\Core\Data\acsllc\TMWk2[acsUserId].h[cno],Replace,RecL=56",i,outi,r
L640: !
scrid$(1)="TIME MANAGEMENT INPUT OF INVOICES"
scrid$(2)="Enter CLIENT # as 0 when completed."
scrid$(3)="PRESS F1 IF YOU HAVE ANOTHER ALLOCATION FOR THE SAME INVOICE"
L670: !
mat inp=(0)
iv=val(iv$)+1 conv L710
iv$=str$(iv)
goto L720
L710: !
iv$=""
L720: !
inp(4)=inp4
inp(2)=inp2
L740: !
pr newpage
pr f mat fl1$: mat scr1$,mat scrid$
pr f mat ot1$: mat inp," " ! IV$
pr f "6,40,C 40,N": "(1=Partial  2=Final  3=Write-off)"
pr f "23,30,c 25": "F4 Search  F5 Stop"
if chg=2 then goto L870
L790: !
if cmdkey=1 then goto L800 else input fields "5,25,N 5,UE,N": inp(1) conv L790
if cmdkey=4 then goto TMSRCH
L800: !
if inp(1)=0 or cmdkey=5 then goto L1360
k$=rpad$(str$(inp(1)),5)
read #1,using L970,key=k$,release: a1$ nokey L850
pr f "5,45,C 30,N": a1$
goto L870
L850: !
pr f "5,45,C 18,R,N": "CLIENT NOT ON FILE"
goto L790
L870: !
input fields mat in1$: mat inp,iv$ conv L1690
if cv>0 then in1$(cv)(cv1:cv2)="U"
if inp(1)=0 and chg><2 then goto L1360
inp4=inp(4)
inp2=inp(2)
if inp(1)=0 then mat inp=(0)
if inp(1)=0 then iv$=""
if inp(1)=0 then goto L1240
k$=rpad$(str$(inp(1)),5)
read #1,using L970,key=k$,release: a1$ nokey L990
L970: form pos 6,c 30
goto L1010
L990: !
cv=1
goto L1710
L1010: if inp(2)<1 or inp(2)>3 then cv=2 else goto L1030
goto L1710
L1030: if inp(4)<10100 or inp(4)>123199 then cv=4 else goto L1050
goto L1710
L1050: if inp(5)<1 or inp(5)>10 then cv=5 else goto L1070
goto L1710
L1070: if env$('client')<>"ACS" then goto L1090
if inp(x8-1)<0 or inp(x8-1)>24 then cv=(x8-1) else goto L1090
goto L1710
L1090: pt(1)=pt(1)+inp(1)
pt(2)=pt(2)+inp(3)
pt(3)=pt(3)+inp(5)
pt(4)=pt(4)+inp(x8-1)
if chg=2 then goto L1240
rw=rw+1
write #2,using fm$,rec=rw: mat inp,iv$
if cmdkey=1 then goto L1170 else goto L670
L1170: !
inp(3)=0
inp(5)=0
inp(6)=0
inp(6+subac)=0
inp(6+subac+dept)=0
inp(7+subac+dept)=0
goto L740
L1240: !
rewrite #2,using fm$,rec=rr: mat inp,iv$
L1250: !
pr newpage
pr f "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
L1270: !
input fields "10,60,n 5,eu,n": rr conv L1270
if rr=0 then goto L1360
if rr>rw or rr<1 then goto L1270
read #2,using fm$,rec=rr: mat inp,iv$
pt(1)=pt(1)-inp(1)
pt(2)=pt(2)-inp(3)
pt(3)=pt(3)-inp(5)
pt(4)=pt(4)-inp(x8-1)
goto L740
L1360: !
pr newpage
scrid$(1)="TIME MANAGEMENT INPUT PROOF TOTALS"
scrid$(2)="Enter 1 for a listing, 2 for corrections,"
scrid$(3)=" 3 for additional entries, 4 to merge, 5 stop without posting."
pr f mat fl2$: mat scr2$,mat scrid$
pr f mat ot2$: mat pt
L1420: !
input fields "16,30,n 1,eu,n": chg conv L1420
on chg goto L1440,L1620,L640,L1660,L1680 none L1420
L1440: !
pr newpage
pr f "10,10,c 60,h,n": "TIME MANAGEMENT CORRECTION LISTING IN PROCESS"
pr f "23,2,C 50,N": "Press F5 to stop!"
gosub PrHeader
for j=1 to rw
	read #2,using fm$,rec=j: mat inp,iv$
	if inp(1)=0 then goto L1520
	pr #255,using fp$: j,mat inp,iv$ pageoflow L1590
	L1520: !
next j
L1530: !
fncloseprn
goto L1360
PrHeader: ! r:
	pr #255,using L1560: date$,env$('Program_Caption'),time$
	L1560: form skip 2,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,skip 2
	pr #255: "REF #  CLIENT #  BILLING-CODE     AMOUNT   DATE   CATEGORIES   G/L---NUMBER  SUB-CATEGORY   INVOICE #"
return ! /r
L1590: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
L1620: !
	scrid$(1)="TIME MANAGEMENT INPUT CORRECTION SCREEN"
	scrid$(2)="ENTER CLIENT # AS 0 TO DELETE THIS ENTRY"
	scrid$(3)=""
goto L1250
L1660: !
	close #1:
	close #2:
	L1680: !
fnChain("S:\acsTM\Merge Pre-Printed Invoices")
Xit: fnXit

L1690: !
	if cv>0 then in1$(cv)(cv1:cv2)="U"
	cv=cnt+1
L1710: !
	pr f "24,78,C 1": bell
	in1$(cv)=rtrm$(in1$(cv))
	cv1=pos(uprc$(in1$(cv)),"U",1)
	cv2=cv1+1
	in1$(cv)(cv1:cv1)="CR"
goto L870

TMSRCH: ! search for customer #
	dim selection$*70
	fnsearch(11,"form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2",'pic($$$,$$$.##)',selection$,5)
	k$=z$=selection$ ! pull key from first field in search line
	inp(1)=0
	inp(1)=val(selection$) conv L4910
L4910: goto L740

include: Ertn