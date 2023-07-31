! formerly S:\acsGL\AcGlInc4p
! -- pr INCOME STATEMENT WITH BUDGET and percent remaining

autoLibrary
on error goto Ertn

dim fl1$*256,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
dim r$*5,d$*50,te$*1,ac(9),secondr$*50,foot$*132
dim b$*3,a$(8)*30,oldtrans$*16,g(8)
dim pedat$*20

fnTop(program$)
fscode=fnfscode
priorcd=fnpriorcd
if fnGlAskFormatPriorCdPeriod=5 then goto Xit 		! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
cch$=fncch$
pedat$=fnpedat$
dim actpd$*6
actpd$=fnactpd$
actpd=fnactpd
fscode=fnfscode
priorcd=fnpriorcd

pors=1
hFinStmtDesign=fnOpenFsdAcglfnsIJ(mp1,mp2)
fnOpenPrn
if file$(255)(1:4)<>'PRN:' then redir=1 else redir=0
on fkey 5 goto Finis
dim report$*50
report$='Statement of Income and Expenses'
fnFsIndexIncStmt
open #3: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr',i,i,k
L350: !
	read #hFinStmtDesign,using L400: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof Finis
	if ltrm$(r$)='' or ltrm$(r$)='0' then goto L350
	if costcntr=0 then goto L400
	if fc=0 and te$='F' then goto L410
	if costcntr><fc then goto L350
L400: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L410: if te$='S' or te$='F' then goto L430
	if heading=0 and te$><'R' then gosub HDR
L430: !
on pos ('RFHDTS',te$,1) goto L1030,L1080,L440,L500,L900,L1030 none L350

L440: !
pr #255,using L450: d$(1:40)
	L450: form pos sp,c 40
	gosub PrFooterSpacer
	gosub L1170
goto L350

L500: !
	if notrans=1 then goto L720
	if ir>=val(r$) and val(r$)><0 then goto L630
L520: ! read amounts for gl master file
L530: !
	dim bm(13),bp(13),by(13)
	read #3,using 'form pos mp1,pd 3,pos 81,41*pd 6.2': ir,bb,cb,mat by,mat bp,mat bm eof L710
	if ir=0 then goto L530
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L610
	if fscode>1 then bb=by(fscode-1) else bb=0
goto L620
L610: !
	if fscode>1 then bb=bp(fscode-1) else bb=0
L620: !
L630: !
	if ir=val(r$) then total=total+(cb-bb) else goto L690
	total2+=cb
	for z=1 to 13 : annualb+=bm(z) : next z
	if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode)
	if fscode=0 then
		for j=1 to actpd : ytdb+=bm(j) : next j
		goto L520
	else 
		for j=1 to fscode : ytdb+=bm(j) : next j
		goto L520
	end if
	goto L520
L690: !
	if ir<val(r$) then goto L520
	if ir>val(r$) then goto L720
L710: !
	notrans=1
L720: !
	unexpend=annualb-total2
	dim accum(9,7)
	for j=1 to 9
		if ac(j)<>9 then 
			accum(j,1)+=annualb : accum(j,2)+=total 
			accum(j,3)+=total2 : accum(j,4)+=unexpend
		end if
	next j
	if rs=1 then total=-total else goto L780
	total2=-total2 : annualb=-annualb : unexpend=unexpend
L780: if ds=1 then dollar$='$' else dollar$=' '
	if annualb><0 or total2><0 then goto L820
	if total then goto L820
	if ls+ds+ul+ic>0 then goto L820 else goto L350
L820: sp2=22-sp-1
	if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,'{\ul ',annualb,'}',dollar$,'{\ul ',total,'}',dollar$,'{\ul ',total2,'}',dollar$,'{\ul ',unexpend,'}',percnt pageoflow PgOf : goto L840
	pr #255,using L840: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,total2,dollar$,unexpend,percnt pageoflow PgOf
L840: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(---,---,---.##),pic(--------.--),skip redir
L841: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(---,---,---.##),c 1,pic(--------.--),skip redir
	total=total2=annualb=unexpend=0
	gosub L1170
	if ul=1 then goto L880
	gosub PrUnderline
L880: !
	gosub PrFooterSpacer
goto L350
L900: !
	if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
	if ds=1 then dollar$='$' else dollar$=' '
	sp2=22-sp-1
	if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,'{\ul ',accum1,'}',dollar$,'{\ul ',accum2,'}',dollar$,'{\ul ',accum3,'}',dollar$,'{\ul ',accum4,'}',percnt pageoflow PgOf : goto L980
	pr #255,using L840: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4,percnt pageoflow PgOf
L980: !
	gosub L1170
	if ul<>1 then gosub PrUnderline
	gosub PrFooterSpacer
goto L350

L1030: !
	if te$='R' then report$=d$
	if te$='S' then secondr$=d$
	gosub PrFooterSpacer
goto L350

L1080: !
	if foot1=1 then 
		foot$=rtrm$(foot$)&d$
	else
		tabnote=sp
		foot1=1
		foot$=d$
	end if
goto L350


L1170: ! r: accum array conditional reset (if ~ac=0or9)
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1200 ! 10/14/87
		accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
		L1200: !
	next j
return ! /r

PrFooterSpacer: ! r:
	if ls=99 then 
		gosub PrFooter
	else if ls then
		pr #255,using L1260: ''
		L1260: form pos 1,c 1,skip ls
	end if
return ! /r

PrFooter: ! r:
	fnPgLen(pglen)
	! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	! If PGLEN=42 Then sK=SK+1
	pr #255,using L1340: rtrm$(foot$),'Page '&str$(pt1)
	L1340: form skip sk,pos tabnote,c fl,pos 74,c 8,skip 1
	if eofcode<>1 then 
		pr #255: newpage
		gosub HDR
	end if
return ! /r

PgOf: gosub PrFooter: continue
PrUnderline: ! r:
	if ul then 
		if ul=1 then 
			dim underlin$*14
			underlin$='______________'
		else
			underlin$='=============='
			pr #255:
		end if
		pr #255,using Ful: underlin$,underlin$,underlin$,underlin$ ! ,UNDERLIN$(1:10)
		Ful: form pos 22,3*c 15,x 1,c 15,c 15,skip redir
	end if
	if redir=0 then pr #255: ''
return ! /r

HDR: ! r:
	heading=1
	pt1+=1
	pr #255: '\qc  {\f181 \fs24 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f181 \fs24 \b '&trim$(report$)&'}'
	if trim$(secondr$)<>'' then pr #255: '\qc  {\f181 \fs18 \b '&trim$(secondr$)&'}'
	pr #255: '\qc  {\f181 \fs16 \b For the '&rtrm$(actpd$)&' month period ended '&rtrm$(fnpedat$)&'}'
	pr #255: '\ql '
	pr #255:
	pr #255,using L1620: 'Annual',lpad$(rtrm$(cch$),20),'Year To Date',' Budget','Percent'
	L1620: form pos 29,c 6,pos 35,cc 20,pos 55,c 15,pos 73,c 7,pos 84,c 7,skip 1
	pr #255: tab(29);'Budget';tab(41);'Balance';tab(56);'  Balance';tab(72);'Over/Under';tab(83);'Remaining'
	pr #255:
return ! /r

Finis: ! r:
	eofcode=1
	gosub PrFooter

	fnClosePrn
	fnfscode(actpd)
	fnpriorcd(1)
goto Xit ! /r

Xit: fnXit

include: ertn
