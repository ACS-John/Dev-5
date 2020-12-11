! formerly S:\acsGL\AcGlInc4
! -- Four Column Budget Income Statement
! r: library, on error and dims
	autoLibrary
	on error goto Ertn
!
	dim actpd$*6,cch$*20
	dim r$*5,d$*50,te$*1,ac(9),report$*50,foot$*132,underlin$*14
	dim accum(9,7)
	dim actpd$*6,bm(13),bp(13),by(13),cap$*128
! /r
	fnTop(program$)
	report$="Statement of Income and Expenses"
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	on fkey 5 goto FINIS
! r: setup files, etc
	cch$=fncch$
	actpd$=fnactpd$
	actpd=fnactpd
	fscode=fnfscode
	priorcd=fnpriorcd
!
	if fnps=2 then
		mp1=72
		open #hAcGlFnsX:=fnH:"Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr" ,internal,input,keyed
	else
		mp1=69
		open #hAcGlFnsX:=fnH:"Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr",internal,input,keyed
	end if
	fnIndex("[Q]\GLmstr\GLmstr.h[cno]",env$('temp')&"\fsindex.h[cno]",str$(mp1)&" 3")
	fnopenprn
	open #hGlMstr:=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr",internal,input,keyed
	fHlMstr: form pos mp1,pd 3,pos 81,41*pd 6.2
	! /r
ReadFinStmtLayout: ! r:
	read #hAcGlFnsX,using L400: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof FINIS
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto ReadFinStmtLayout
	if costcntr=0 then goto L400
	if fc=0 and te$="F" then goto L410
	if costcntr><fc then goto ReadFinStmtLayout
	L400: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	L410: if te$="S" or te$="F" then goto L430
	if heading=0 and te$><"R" then gosub PrHeaderPrimary
	L430: !
on pos ("RFHDTS",te$,1) goto FinRecRandS,FinRecF,FinRecH,FinRecD,FinRecT,FinRecRandS none ReadFinStmtLayout ! /r
FinRecH: ! r:
	pr #255,using L450: d$(1:40)
	L450: form pos sp,c 40
	gosub PrFootnotesA
	gosub AccumReset
goto ReadFinStmtLayout ! /r
!
FinRecD: ! r:
	if notrans=1 then goto L720
	if ir>=val(r$) and val(r$)><0 then goto L630
	FdReadGlMstr: ! read amounts for gl master file
	read #hGlMstr,using fHlMstr: ir,bb,cb,mat by,mat bp,mat bm eof L710
	if ir=0 then goto FdReadGlMstr
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L610
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L620
	L610: if fscode>1 then bb=bp(fscode-1) else bb=0
	L620: !
	L630: if ir=val(r$) then total=total+(cb-bb) else goto L690
	total2+=cb
	for z=1 to 13 : annualb+=bm(z) : next z
	if fscode=0 then
		monthb=monthb+bm(actpd)
	else
		monthb=monthb+bm(fscode)
	end if
	if fscode=0 then
		for j=1 to actpd : ytdb+=bm(j) : next j
	else
		for j=1 to fscode : ytdb+=bm(j) : next j
	end if
goto FdReadGlMstr ! /r
 
L690: ! r:
	if ir<val(r$) then goto FdReadGlMstr
	if ir>val(r$) then goto L720
	L710: !
	notrans=1
	L720: !
	unexpend=annualb-total2
	for j=1 to 9
		if ac(j)<>9 then
			accum(j,1)+=annualb : accum(j,2)+=total
			accum(j,3)+=total2 : accum(j,4)+=unexpend
		end if
	next j
	if rs=1 then
		total=-total
		total2=-total2 : annualb=-annualb : unexpend=unexpend
	end if
	if ds=1 then dollar$="$" else dollar$=" "
	if annualb><0 or total2><0 or total<>0 or ls+ds+ul+ic>0 then
		sp2=22-sp-1
		if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",annualb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",unexpend,"}" pageoflow PGOF : goto L840
		pr #255,using L840: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,total2,dollar$,unexpend pageoflow PGOF
		L840: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(---,---,---.##),skip 1
		L841: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(---,---,---.##),c 1,skip 1
		total=total2=annualb=unexpend=0
		gosub AccumReset
		if ul<>1 then gosub PrUnderlines
		gosub PrFootnotesA
	end if
goto ReadFinStmtLayout ! /r
FinRecT: ! r:
	if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=22-sp-1
	if ul=1 then
		pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum4,"}" pageoflow PGOF
	else
		pr #255,using L840: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4 pageoflow PGOF
	end if
	gosub AccumReset
	if ul<>1 then gosub PrUnderlines
	gosub PrFootnotesA
goto ReadFinStmtLayout ! /r
FinRecRandS: ! r:
	if te$="R" then
		report$=d$
	else if te$="S" then
		dim secondr$*50
		secondr$=d$
	end if
	gosub PrFootnotesA
goto ReadFinStmtLayout ! /r
FinRecF: ! r:
	if foot1=1 then
		foot$=rtrm$(foot$)&d$
	else
		tabnote=sp
		foot1=1
		foot$=d$
	end if
goto ReadFinStmtLayout ! /r
AccumReset: ! r: reset mat accum
	for j=1 to 9
		if ac(j)<>0 and ac(j)<>9 then
			accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
		end if
	next j
return ! /r
PrFootnotesA: ! r: footnotes
	if ls<>0 then
		if ls=99 then
			gosub PrHeaderSecondary
		else
			pr #255,using L1260: " "
			L1260: form pos 1,c 1,skip ls
		end if
	end if
return ! /r
PrHeaderSecondary: ! r:
	fnpglen(pglen)
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
	L1340: form skip sk,pos tabnote,c fl,pos 74,c 8,skip 1
	if eofcode<>1 then
		pr #255: newpage
		gosub PrHeaderPrimary
	end if
return ! /r
PGOF: gosub PrHeaderSecondary: continue
PrUnderlines: ! r:
	if ul=0 then goto L1500
	if ul=1 then goto L1470
	underlin$="=============="
	goto L1480
	!
	L1470: underlin$="______________"
	L1480: pr #255,using L1490: underlin$,underlin$,underlin$,underlin$
	L1490: form pos 22,3*c 15,x 1,c 15,skip 1
	L1500: ! if redir=0 then pr #255: ""
return ! /r
PrHeaderPrimary: ! r:
	heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(str$(actpd))&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255,using L1620: "Annual",lpad$(rtrm$(cch$),20),"Year To Date"," Budget"
	L1620: form pos 29,c 6,pos 35,cc 20,pos 55,c 15,pos 73,c 7,skip 1
	pr #255: tab(29);"Budget";tab(41);"Balance";tab(56);"  Balance";tab(72);"Over/Under"
	pr #255:
return ! /r
FINIS: ! r:
	eofcode=1
	gosub PrHeaderSecondary
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
goto Xit ! /r
Xit: fnXit
include: ertn
