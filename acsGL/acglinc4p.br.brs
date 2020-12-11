! Replace S:\acsGL\AcGlInc4p
! -- pr INCOME STATEMENT WITH BUDGET and percent remaining
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
	dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13),cap$*128,udf$*256
 
	fnTop(program$,cap$="Four Column Budget With Percent")
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	fscode=fnfscode
	priorcd=fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	cch$=fncch$
	pedat$=fnpedat$
	actpd$=fnactpd$
	actpd=fnactpd
	fscode=fnfscode
	priorcd=fnpriorcd
 
	pors=1
	if fnps=2 then mp1=72 : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr" else : _
		mp1=69 : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	open #1: fl1$,internal,input,keyed
	fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	on fkey 5 goto L1670
	report$="Statement of Income and Expenses"
	if fnps=2 then goto L320 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 69 3 Replace DupKeys -N"
	goto L340
L320: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 72 3 Replace DupKeys -N"
	fnconsole(off=0)
L340: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
L350: read #1,using L400: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1670
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L350
	if costcntr=0 then goto L400
	if fc=0 and te$="F" then goto L410
	if costcntr><fc then goto L350
L400: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L410: if te$="S" or te$="F" then goto L430
	if heading=0 and te$><"R" then gosub HDR
L430: on pos ("RFHDTS",te$,1) goto L1030,L1080,L440,L500,L900,L1030 none L350
L440: pr #255,using L450: d$(1:40)
L450: form pos sp,c 40
	gosub L1230
	gosub L1170
	goto L350
 
L500: if notrans=1 then goto L720
	if ir>=val(r$) and val(r$)><0 then goto L630
L520: ! read amounts for gl master file
L530: read #3,using L620: ir,bb,cb,mat by,mat bp,mat bm eof L710
	if ir=0 then goto L530
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L610
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L620
L610: if fscode>1 then bb=bp(fscode-1) else bb=0
L620: form pos mp1,pd 3,pos 81,41*pd 6.2
L630: if ir=val(r$) then total=total+(cb-bb) else goto L690
	total2+=cb
	for z=1 to 13 : annualb+=bm(z) : next z
	if fscode=0 then monthb=monthb+bm(actpd) else : _
		monthb=monthb+bm(fscode)
	if fscode=0 then : _
		for j=1 to actpd : ytdb+=bm(j) : next j : goto L520 : _
	else : _
		for j=1 to fscode : ytdb+=bm(j) : next j : goto L520
	goto L520
L690: if ir<val(r$) then goto L520
	if ir>val(r$) then goto L720
L710: notrans=1
L720: unexpend=annualb-total2
	for j=1 to 9
		if ac(j)<>9 then : _
			accum(j,1)+=annualb : accum(j,2)+=total : _
			accum(j,3)+=total2 : accum(j,4)+=unexpend
	next j
	if rs=1 then total=-total else goto L780
	total2=-total2 : annualb=-annualb : unexpend=unexpend
L780: if ds=1 then dollar$="$" else dollar$=" "
	if annualb><0 or total2><0 then goto L820
	if total<>0 then goto L820
	if ls+ds+ul+ic>0 then goto L820 else goto L350
L820: sp2=22-sp-1
	if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",annualb,"}",dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",unexpend,"}",percnt pageoflow L1400 : goto L840
	pr #255,using L840: d$(1:sp2),dollar$,annualb,dollar$,total,dollar$,total2,dollar$,unexpend,percnt pageoflow L1400
L840: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(---,---,---.##),pic(--------.--),skip redir
L841: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(---,---,---.##),c 1,pic(--------.--),skip redir
	total=total2=annualb=unexpend=0
	gosub L1170
	if ul=1 then goto L880
	gosub L1410
L880: gosub L1230
	goto L350
L900: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=22-sp-1
	if ul=1 then pr #255,using L841: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum4,"}",percnt pageoflow L1400 : goto L980
	pr #255,using L840: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4,percnt pageoflow L1400
L980: gosub L1170
	if ul=1 then goto L1000
	gosub L1410
L1000: gosub L1230
	goto L350
 
L1030: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1230
	goto L350
 
L1080: if foot1=1 then goto L1140
	tabnote=sp
	foot1=1
	foot$=d$
	goto L350
 
L1140: foot$=rtrm$(foot$)&d$
	goto L350
 
L1170: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1200 ! 10/14/87
		accum(j,1)=accum(j,2)=accum(j,3)=accum(j,4)=0
L1200: next j
return
 
L1230: if ls=0 then goto L1380
	if ls=99 then goto L1290
	pr #255,using L1260: " "
L1260: form pos 1,c 1,skip ls
	goto L1380
 
L1290: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
L1340: form skip sk,pos tabnote,c fl,pos 74,c 8,skip 1
	if eofcode=1 then goto L1380
	pr #255: newpage
	gosub HDR
L1380: return
 
L1400: gosub L1290: continue
L1410: if ul=0 then goto L1500
	if ul=1 then goto L1470
	underlin$="=============="
	pr #255:
	goto L1480
 
L1470: underlin$="______________"
L1480: pr #255,using L1490: underlin$,underlin$,underlin$,underlin$ ! ,UNDERLIN$(1:10)
L1490: form pos 22,3*c 15,x 1,c 15,c 15,skip redir
L1500: if redir=0 then pr #255: ""
return
 
HDR: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255,using L1620: "Annual",lpad$(rtrm$(cch$),20),"Year To Date"," Budget","Percent"
L1620: form pos 29,c 6,pos 35,cc 20,pos 55,c 15,pos 73,c 7,pos 84,c 7,skip 1
	pr #255: tab(29);"Budget";tab(41);"Balance";tab(56);"  Balance";tab(72);"Over/Under";tab(83);"Remaining"
	pr #255:
return
 
L1670: eofcode=1
	gosub L1290
 
	fncloseprn
	fnfscode(actpd)
	fnpriorcd(1)
	goto Xit
 
Xit: fnXit
 
include: ertn
