! chained to from S:\Client Billing\Legacy\tminpinv
on error goto Ertn
autoLibrary
dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,b(8),sc$*4,ivr(6),arta(2),ga(10),pgl$*12
dim cde$(10)*6,gl$*12,des$*20
pr newpage
pr f "10,20,c 60,h,n": "T/M MERGE INVOICES IN PROCESS"
open #3: "Name=S:\Core\Data\acsllc\TMWk2[acsUserId].h[cno],NoShr",internal,input
open #12: "Name=S:\Core\Data\acsllc\Transactions.h[cno],Shr",i,outi,r
open #2: "Name=S:\Core\Data\acsllc\TMTRANS.h[cno],Shr",i,outi,r
L130: form pos 54,pd 3
open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",internal,outIn,keyed
open #4: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",i,outi,r
! open #h_armotran:=5: "Name=S:\Core\Data\acsllc\ARMoTran.h[cno],Shr",internal,output
open #6: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input
read #6,using L190: pgl$
L190: form pos 190,c 12
close #6:
L210: form pos 1,pd 3
L220: !
read #3,using L230: k$,b(7),b(3),b(4),b(5),gl$,b(8),iv$ eof L780
L230: form pos 1,c 5,n 1,pd 4.2,n 6,n 2,c 12,n 2,c 12
if rtrm$(k$)="0" or rtrm$(k$)="" then goto L220
if b(7)=3 and rtrm$(iv$)="" then iv$="WRITE OFF"
iv$=lpad$(rtrm$(iv$),12)
b(7)=-b(7)
if b(7)=-1 then des$="PARTIAL BILLING"
if b(7)=-2 then des$="FINAL BILLING"
if b(7)=-3 then des$="WRITE-OFF"
if b(8)=0 then b8=25 else b8=b(8)
read #1,using L330,key=k$: e$,mat sc,mat ca,ar1,mat arta nokey L770
L330: form pos 179,c 9,pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3
L340: !
lta=lrec(2)+1
write #2,using L360,rec=lta,reserve: k$," ",mat b,sc$,iv$,0 duprec L340
L360: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3
rewrite #2,using L130,rec=1,release: lta
L380: !
lar=lrec(12)+1
write #12,using L400,rec=lar,reserve: k$,iv$,b(4),b(3),b(3),0,1,0,des$,0 duprec L380
L400: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
if arta(2)>0 then rewrite #12,using L450,rec=arta(2): lar
arta(2)=lar
if arta(1)=0 then arta(1)=lar
rewrite #12,using L450,rec=1,release: lar
L450: form pos 58,pd 3
ar1=ar1+b(3)
if ca(b(5))=0 then goto L570
p1=1+(b8-1)*6
p2=150+b8
read #4,using L510,rec=ca(b(5)),reserve: ta1,ta2,fb1 noRec L770
L510: form pos p1,2*pd 3,pos p2,n 1
if ta2><0 then rewrite #2,using L130,rec=ta2: lta else ta1=lta
if fb1<2 then fb1=abs(b(7))
if ta1=0 then ta1=lta
rewrite #4,using L510,rec=ca(b(5)),release: ta1,lta,fb1
goto L690 ! 6/24/87
L570: !
lta4=lrec(4)+1
mat ta=(0)
mat fb=(0)
ca(b(5))=lta4
ta(b8,1)=lta
ta(b8,2)=lta
if b(7)=-2 then fb(b8)=2
if fb(b8)=2 then goto L660
if b(7)=-1 then fb(b8)=1
L660: write #4,using L670,rec=lta4,reserve: mat ta,mat fb duprec L570
L670: form pos 1,50*pd 3,25*n 1
rewrite #4,using L210,rec=1,release: lta4
L690: igl$(1)=gl$
ga(1)=b(3)
! write #h_armotran,using F_armotran: k$,iv$,b(4),b(3),b(3),0,1,0,des$,pgl$,mat igl$,mat ga
! F_armotran: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,11*c 12,10*pd 5.2
if b(7)=-2 then sc(b(5))=2
rewrite #1,using L750,key=k$: mat sc,mat ca,ar1,mat arta
L750: form pos 220,10*n 1,10*pd 3,pos 283,pd 5.2,pos 299,2*pd 3
ga(1)=b(3)
L770: goto L220
L780: close #1:
	close #2:
	close #3:
	close #4:
Xit: fnXit
include: ertn
