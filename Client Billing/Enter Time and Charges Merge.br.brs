on error goto Ertn
autoLibrary
fnTop(program$) 

fnStatus("Processing: Enter Time and Charges Merge...")
scNotFoundCount=0
open #3: "Name=S:\Core\Data\acsllc\TMWK[acsUserId].h[cno],NoShr",i,i
open #2: "Name=S:\Core\Data\acsllc\TMTRANS.h[cno],Shr",i,outi,r
F2lta: form pos 54,pd 3
open #1: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,outIn,k
open #4: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",i,outi,r
open #hEmployee=fnH: "Name=S:\Core\Data\acsllc\EMmstr.h[cno],KFName=S:\Core\Data\acsllc\EMIndex.h[cno],Shr",i,outIn,k
open #hService=fnH: "Name=S:\Core\Data\acsllc\SCMSTR.h[cno],KFName=S:\Core\Data\acsllc\SCIndex.h[cno],Shr",i,outIn,k

LoopTop: !
	dim k$*5
	dim e$*9
	dim xb(8)
	dim sc$*4
	dim iv$*12
	dim des$*30
	read #3,using F32: k$,e$,mat xb,sc$,iv$,nta,des$ eof Finis
	if xb(7)=0 then goto LoopTop
	iv$=lpad$(rtrm$(iv$),12)
	if xb(8)=0 then b8=25 else b8=xb(8)
	if rtrm$(des$)="" then 
		read #hService,using L250,key=sc$: des$ nokey ignore
		L250: form pos 5,c 30
	end if
	L260: !
	lta=lrec(2)+1
	write #2,using F32,rec=lta,reserve: k$,e$,mat xb,sc$,iv$,0,des$ duprec L260
	F32: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
	rewrite #2,using F2lta,rec=1,release: lta
	if xb(5)>10 then goto L580
	if val(k$)=0 then goto L580
	dim scc(10)
	dim ca(10)
	read #1,using F1,key=k$: mat scc,mat ca nokey L580
	F1: form pos 220,10*n 1,10*pd 3
	if ca(xb(5))=0 then goto L440
	p1=1+(b8-1)*6
	p2=150+b8
	read #4,using F4a,rec=ca(xb(5)): ta1,ta2,fb1 noRec L580
	F4a: form pos p1,2*pd 3,pos p2,n 1
	if ta2>0 then 
		rewrite #2,using F2lta,rec=ta2: lta
	end if
	if xb(7)=-2 then fb1=1
	if ta1=0 then ta1=lta
	rewrite #4,using F4a,rec=ca(xb(5)): ta1,lta,fb1
	if scc(xb(5))=0 and xb(7)>0 then 
		goto L560 
	else 
		goto L580
	end if
	
	L440: !
	lta4=lrec(4)+1
	dim ta(25,2)
	mat ta=(0)
	dim fb(25)
	mat fb=(0)
	ca(xb(5))=lta4
	ta(b8,1)=lta
	ta(b8,2)=lta
	if xb(7)=-2 then fb(b8)=2
	if fb(b8)=2 then goto L530
	if xb(7)=-1 then fb(b8)=1
	L530: !
	write #4,using F4b,rec=lta4,reserve: mat ta,mat fb duprec L440
	F4b: form pos 1,50*pd 3,25*n 1
	rewrite #4,using F4c,rec=1,release: lta4
	F4c: form pos 1,pd 3
	L560: !
	scc(xb(5))=1
	rewrite #1,using F1,key=k$: mat scc,mat ca
	
	L580: !
	if val(e$)=0 then goto L660 ! EMPLOYEE
	dim xe(4,30)
	read #hEmployee,using F5,key=e$: mat xe nokey L660
	F5: form pos 38,60*pd 4.2,60*pd 5.2
	xe(1,xb(5))=xe(1,xb(5))+xb(1)
	xe(2,xb(5))=xe(2,xb(5))+xb(1)
	xe(3,xb(5))=xe(3,xb(5))+xb(3)
	xe(4,xb(5))=xe(4,xb(5))+xb(3)
	rewrite #hEmployee,using F5,key=e$: mat xe
	L660: !
	if ltrm$(sc$)="0" then goto LoopTop
	dim sc(2)
	read #hService,using F6,key=sc$: mat sc nokey NoSuchService
	F6: form pos 35,pd 4.2,pd 5.2
	sc(1)=sc(1)+xb(1)
	sc(2)=sc(2)+xb(3)
	rewrite #hService,using F6,key=sc$: mat sc nokey NoSuchService
goto LoopTop
NoSuchService: !
	scNotFoundCount+=1
	fnStatus("Service Code "&sc$&" not found.")
	pr bell;
goto LoopTop
Finis: ! r:
	close #1: 
	close #2: 
	close #3: 
	close #4: 
	close #hEmployee: 
	close #hService: 
	if scNotFoundCount then fnStatusPause
goto Xit ! /r
Xit: fnXit
include: ertn
