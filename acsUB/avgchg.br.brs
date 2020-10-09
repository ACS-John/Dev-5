! calculates average charges for date range
 
	autoLibrary
 
	on error goto Ertn
 
	dim cap$*128,txt$*60,message$(5)*80,tt$*80,message$*60,tg(11),ttg(11),e2$*30
 
	fnTop("S:\acsUB\ubSewer",cap$="Calculate Average Charges for Date Range")
 
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	gosub BLDHDR
SCR1: !
	sn$="ubsewer-1" : _
	fnTos(sn$)
	txt$="Billing Dates for Months to be Averaged:" : _
	mylen=len(txt$)+4: fnLbl(2,5,txt$,mylen,0)
	mylen=12
	txt$="Date From: " : _
	fnLbl(3,6,txt$,mylen,1)
	txt$="Date To: " : _
	fnLbl(4,6,txt$,mylen,1)
	for j=1 to 2 : _
		fnTxt(j+2,20,8,0,0,"3") : _
		resp$(j)="" : _
	next j
	fnCmdSet(2): fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	for j=1 to 8
L250: x=pos(resp$(j),"/",1)
		if x>0 then resp$(j)(x:x)="": goto L250
	next j
	sd1=val(resp$(1)) : sd2=val(resp$(2))
	if sd1=0 or sd2=0 or sd2<sd1 then goto SCR1
 
	fnopenprn
	message$="Calculating: please wait..." : _
	fnwait(message$,1)
	gosub HDR
L340: read #1,using L350: x$,e2$,oldavg eof DONE
L350: form pos 1,c 10,x 30,c 30,pos 1822,n 9
	restore #2,key>=x$&"         ": nokey L450
L370: read #2,using L380: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L450
L380: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>x$ then goto L450 ! check account
	if tcode<>1 then goto L370 ! charge transaction
	if tdate<sd1 or tdate>sd2 then goto L370 ! check date range
	ttg=ttg+1
	mat ttg=ttg+tg
	goto L370
L450: if ttg=0 then goto L340 ! no transactions in date range
	mat g1=(0)
	j2=0
	for j=1 to 9
		if trim$(serviceName$(j))="" then goto L520
		j2=j2+1
		g1(j2)=ttg(j)/ttg
L520: next j
	g1(sz1)=ttg(11)/ttg
	pr #255,using L550: x$,e2$(1:24),mat g1 pageoflow NEWPGE
L550: form pos 1,c 11,c 24,sz1*n 9.2,skip 1
	ttg=0 : mat ttg=(0)
	tg2=tg2+1 : mat g2=g2+g1
	goto L340
DONE: !
	pr #255:
L610: form pos 5,c 20,pic(zzz,zzz,zzz.##cr),skip 1
	pr #255,using L610: "Total Customers",tg2
	j2=0
	for j=1 to 9
		if trim$(serviceName$(j))="" then goto L680
		j2=j2+1
		pr #255,using L610: serviceName$(j),g2(j2)
L680: next j
	pr #255,using L610: "Total",g2(sz1)
	close #1:
	fncloseprn
Xit: fnXit
 
NEWPGE: pr #255: newpage
	gosub HDR
continue
 
HDR: ! r:
	p1=p1+1
	pr #255,using "Form POS 20,CC 40,POS 70,C 5,N 4": env$('cnam'),"Page ",p1
	pr #255,using "Form POS 20,C 23,pic(####/##/##),c 6,pic(####/##/##)": "Average Charges From:",sd1,"  To:",sd2
	pr #255: ""
	pr #255: hd1$
	pr #255: hd2$
return ! /r
 
include: ertn No
 
	dim hd1$*400,hd2$*400,g1(11),g2(11)
	dim serviceName$(10)*20,services$(10)*2,tax_code$(10)*1,tg(11),usages(3)
BLDHDR: ! r: build pr headings
	fnGetServices(mat serviceName$,mat service$,mat tax_code$,mat penalty$)
	hd1$="Account                             " : _
	hd2$="{\ul Number   }  {\ul Name                   }  "
	for j=1 to 9 ! skip penalty
		x2=pos(trim$(serviceName$(j))," ",1) : _
		if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&"-"&serviceName$(j)(x2+1:len(serviceName$(j)))
		if trim$(serviceName$(j))<>"" then : _
			x1=pos (serviceName$(j)," ",1) : _
			x1=min(x1,7) : _
			hd1$=hd1$&"---------" : _
			hd2$=hd2$&"{\ul "&lpad$(trim$(serviceName$(j)(1:x1)),8)&"} " : _
			sz1=sz1+1
	next j
	sz1=sz1+1
	hd2$=hd2$&"{\ul    TOTAL} "
	mat g1(sz1)
	mat g2(sz1)
return ! /r
