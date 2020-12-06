! Replace S:\acsUB\conversion\collection
! try converting colletion for 402 when they do not have allocations on them in the transaction history.
 
	autoLibrary
	fnTop("S:\acsUB\conversion\collection",cap$="Convert Collections")
 
	dim cnam$*40,dat$*20,a$(61)*30,u(61),scr1$(10)*30,alloc(10),nam$*30,o(2)
	dim r(20,4),hd1$*190,hd2$*190,cap$*128,message$*40
 
	fncno(cno,cnam$)
 
	cap$="Convert Collections"
 
 
	open #6: "Name=[Q]\UBmstr\Collect.h[cno]",internal,outIn,relative
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
L230: !
L240: read #6,using L280: x$,m,n,mat o,adrnxt,rcpt$,mat alloc eof L440
	for j=1 to udim(alloc)
		if alloc(j)<-20202 then alloc(j)=0
	next j
L280: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pd 3,c 9,10*pd 4.2
	if m=0 then goto L230
	if o(1)<1 or o(1)>4 then o(1)=1
	if o(1)=3 then ti2=1 : tcode=3 ! REG.COLLECTION
	if o(1)=4 then ti2=2 : tcode=5 ! CREDIT MEMO
	if o(1)=1 and o(2)=4 then ti2=3 : tocde=6 ! DEBIT MEMO
	tdate=fndate_mmddyy_to_ccyymmdd(n)
	tamount=m
	for j=1 to 10 : tg(j)=alloc(j): next j
	read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=x$&cnvrt$("pic(########)",tdate)&str$(tcode): p$ nokey L420
	rewrite #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1',key=x$&cnvrt$("pic(########)",tdate)&str$(tcode): x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
	goto L430
L420: write #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
L430: goto L240
L440: close #2:
	fnIndex("[Q]\UBmstr\UBTransvb.h[cno]","[Q]\UBmstr\UBTrindx.h[cno]","1 19")
	fnIndex("[Q]\UBmstr\UBTransVB.h[cno]", "[Q]\UBmstr\UBTrdt.h[cno]","11/1 8/10")
Xit: fnXit
