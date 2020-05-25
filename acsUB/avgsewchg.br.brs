! calculates average sewer charges
! calculates average water usage for selected date range
! uses the calculated water usage to calculate and store a  standard sewer charge

autoLibrary
on error goto Ertn
dim tg(11),ttg(11),e2$*30,rt(10,3)
fnTop(program$,"Set Sewer Standard Charges from Average Water Usage")
open #ratemst:=8: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed
open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
mat resp$=("")
SCR1: !
fnTos
mylen=52+4: fnLbl(1,5,"Average consumption for billing dates within range",mylen,0)
mylen=47+4: fnLbl(2,5,"Charge will be moved into Sewer Standard charge",mylen,0)
mylen=12
fnLbl(3,6,"Date From: ",mylen,1)
fnLbl(4,6,"Date To: ",mylen,1)
fnLbl(5,4,"Sewer Rate Code: ",16,1)
for j=1 to 2
	fnTxt(j+2,20,8,0,0,"3")
next j
fnTxt(5,22,2,0,0,"30")
fnCmdSet(2)
fnAcs2(mat resp$,ckey)
if ckey=5 then goto Xit
for j=1 to 8
	L250: !
	x=pos(resp$(j),"/",1)
	if x>0 then resp$(j)(x:x)="": goto L250
next j
sd1=val(resp$(1)) : sd2=val(resp$(2))
if sd1=0 or sd2=0 or sd2<sd1 then goto SCR1
swcde=val(resp$(3))
read #ratemst,using L298,key="SW"&lpad$(str$(swcde),2): mc1,mu1,mat rt nokey SCR1
L298: form pos 55,32*g 10
close #ratemst:

fnopenprn
gosub HDR
L340: read #1,using L350: x$,e2$,a2,oldavg,extra14,fbc eof DONE
L350: form pos 1,c 10,x 30,c 30,pos 145,pd 2,pos 1822,n 9,pos 1880,n 3,pos 1818,n 3
	if a2><swcde then goto L340
	if fbc>0 then goto L340
	restore #2,key>=x$&"         ": nokey L450
L370: read #2,using L380: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L450
L380: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>x$ then goto L450 ! check account
	if tcode<>1 then goto L370 ! charge transaction
	if tdate<sd1 or tdate>sd2 then goto L370 ! check date range
	ttg=ttg+1
	mat ttg=ttg+tg
	twu=twu+wu ! total water used
	goto L370
L450: if ttg=0 then goto L340 ! no transactions in date range
	mat g1=(0)
	j2=0
	for j=1 to 9
! if trim$(serviceName$(j))="" then goto L520
		j2=j2+1
		g1(j2)=ttg(j)/ttg
L520: next j
! g1(sz1)=ttg(11)/ttg
	su1=twu/ttg
	gosub SEWER_CALK
	pr #255,using L550: x$,e2$(1:24),g1(1),g1(2),su1,swchg pageoflow NEWPGE
L550: form pos 1,c 11,c 24,2*n 9.2,n 9,n 9.2,skip 1
	ttg=0 : mat ttg=(0)
	twu=0
	tg2=tg2+1 : mat g2=g2+g1
	goto L340
DONE: !
	pr #255:
L610: form pos 5,c 20,n 9
	pr #255,using L610: "Total Customers",tg2
	j2=0
	for j=1 to 9
		if trim$(serviceName$(j))<>"" then
			j2=j2+1
			pr #255,using L610: serviceName$(j),g2(j2)
		end if  ! trim$(serviceName$(j))<>""
	next j
! pr #255,using L610: "Total",g2(sz1)
	close #1:
	fncloseprn
Xit: fnXit
 
NEWPGE: !
	pr #255: newpage
	gosub HDR
continue
HDR: ! r:
	p1=p1+1
	pr #255,using "Form POS 20,CC 40,POS 70,C 5,N 4": env$('cnam'),"Page ",p1
	pr #255,using "Form POS 20,C 23,pic(####/##/##),c 6,pic(####/##/##)": "Average Charges From:",sd1,"  To:",sd2
	pr #255: "                                    <--------AVERAGE--------->    NEW  "
	pr #255: "Account    Customer Name             water    sewer    usage    std chg"
	pr #255: "__________ ________________________ ________ ________ ________ ________"
return ! /r

dim hd1$*400,hd2$*400,g1(11),g2(11)
dim serviceName$(10)*20,services$(10)*2,tax_code$(10)*1,tg(11),usages(3)

SEWER_CALK: ! r: calculate standard sewer charge
	swchg=mc1*max(1,extra14) ! units per meter - sewer (default to one)
	if su1<=mu1 then goto SEWER_COMPLETED else mu2=mu1
	for j=1 to 10
		if rt(j,1)>su1 then goto SEWER_COMPLETED
		if su1<rt(j,2) then w1=su1-mu2 else w1=rt(j,2)-mu2
		swchg=swchg+round(w1*rt(j,3),2)
		if rt(j,2)>su1 then goto SEWER_COMPLETED
		mu2=rt(j,2)
	next j
	SEWER_COMPLETED: !
	rewrite #1,using L1284,key=x$: swchg
	L1284: form pos 161,pd 4.2
return ! /r
include: Ertn No