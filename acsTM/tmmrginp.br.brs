on error goto Ertn
autoLibrary
fnTop(program$,cap$="Merge")
pr newpage
dim ta(25,2),fb(25),iv$*12,k$*5,e$*9,b(8),sc$*4,gl$*12,ivr(6),ca(10)
dim e(4,30),sc(2),scc(10),des$*30
pr newpage
pr f "10,20,c 60,h,n": "T/M MERGE INPUT IN PROCESS"
open #3: "Name=S:\Core\Data\acsllc\TMWK"&wsid$&".H[cno],NoShr",internal,input
open #2: "Name=S:\Core\Data\acsllc\TMTRANS.H[cno],Shr",internal,outIn,relative
L140: form pos 54,pd 3
	open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed
	open #4: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",internal,outIn,relative
	open #5: "Name=S:\Core\Data\acsllc\EMmstr.H[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",internal,outIn,keyed
	open #6: "Name=S:\Core\Data\acsllc\SCMSTR.H[cno],KFName=S:\Core\Data\acsllc\SCIndex.H[cno],Shr",internal,outIn,keyed
L190: form pos 1,pd 3
L200: read #3,using L280: k$,e$,mat b,sc$,iv$,nta,des$ eof L750
	if b(7)=0 then goto L200
	iv$=lpad$(rtrm$(iv$),12)
	if b(8)=0 then b8=25 else b8=b(8)
	if rtrm$(des$)="" then read #6,using L250,key=sc$: des$ nokey L260
L250: form pos 5,c 30
L260: lta=lrec(2)+1
	write #2,using L280,rec=lta,reserve: k$,e$,mat b,sc$,iv$,0,des$ duprec L260
L280: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
	rewrite #2,using L140,rec=1,release: lta
	if b(5)>10 then goto L580
	if val(k$)=0 then goto L580
	read #1,using L330,key=k$: mat scc,mat ca nokey L580
L330: form pos 220,10*n 1,10*pd 3
	if ca(b(5))=0 then goto L440
	p1=1+(b8-1)*6
	p2=150+b8
	read #4,using L380,rec=ca(b(5)): ta1,ta2,fb1 noRec L580
L380: form pos p1,2*pd 3,pos p2,n 1
	if ta2>0 then rewrite #2,using L140,rec=ta2: lta
	if b(7)=-2 then fb1=1
	if ta1=0 then ta1=lta
	rewrite #4,using L380,rec=ca(b(5)): ta1,lta,fb1
	if scc(b(5))=0 and b(7)>0 then goto L560 else goto L580
L440: lta4=lrec(4)+1
	mat ta=(0)
	mat fb=(0)
	ca(b(5))=lta4
	ta(b8,1)=lta
	ta(b8,2)=lta
	if b(7)=-2 then fb(b8)=2
	if fb(b8)=2 then goto L530
	if b(7)=-1 then fb(b8)=1
L530: write #4,using L540,rec=lta4,reserve: mat ta,mat fb duprec L440
L540: form pos 1,50*pd 3,25*n 1
	rewrite #4,using L190,rec=1,release: lta4
L560: scc(b(5))=1
	rewrite #1,using L330,key=k$: mat scc,mat ca
L580: if val(e$)=0 then goto L660 ! EMPLOYEE
	read #5,using L600,key=e$: mat e nokey L660
L600: form pos 38,60*pd 4.2,60*pd 5.2
	e(1,b(5))=e(1,b(5))+b(1)
	e(2,b(5))=e(2,b(5))+b(1)
	e(3,b(5))=e(3,b(5))+b(3)
	e(4,b(5))=e(4,b(5))+b(3)
	rewrite #5,using L600,key=e$: mat e
L660: if ltrm$(sc$)="0" then goto L200
	read #6,using L680,key=sc$: mat sc nokey L730
L680: form pos 35,pd 4.2,pd 5.2
	sc(1)=sc(1)+b(1)
	sc(2)=sc(2)+b(3)
	rewrite #6,using L680,key=sc$: mat sc nokey L730
	goto L200
L730: pr #255: "SERVICE CODE ";sc$;" IS NOT ON FILE"
	goto L200
L750: close #1: 
	close #2: 
	close #3: 
	close #4: 
	close #5: 
	close #6: 
XIT: fnxit
include: ertn
