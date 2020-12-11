! Replace S:\acsGL\AcGlIncO
! -- gasb 4 column budget statement with original and final budget
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20,p$(20)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7)
	dim pedat$*20,actpd$*6,bm(13),d(2),bp(13),by(13),revb(13),cap$*128,udf$*256
 
	fnTop(program$,cap$="Income Statement with GASB Budget")
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	cch$=fncch$
	pedat$=fnpedat$
	actpd=fnactpd
	actpd$=fnactpd$
	fscode=fnfscode
	priorcd=fnpriorcd
 
	pors=1
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr" : mp1=72 else : _
		fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr" : mp1=69
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L360
	fnTos(sn$="ACglinco") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L360: fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	report$="Budgetary Comparison Schedule"
	if fnps=2 then goto L410 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 69 3 Replace DupKeys -N"
	goto L420
L410: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 72 3 Replace DupKeys -N"
L420: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
L430: read #1,using L480: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1920
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L430
	if costcntr=0 then goto L480
	if fc=0 and te$="F" then goto L490
	if costcntr><fc then goto L430
L480: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L490: if te$="S" or te$="F" then goto L510
	if heading=0 and te$><"R" then gosub L1750
L510: on pos ("RFHDTS",te$,1) goto L1280,L1320,L520,L570,L1160,L1280 none L430
L520: pr #255,using L530: d$(1:40)
L530: form pos sp,c 40,skip 1
	gosub L1470
	gosub L1390
	goto L430
L570: if notrans=1 then goto L890
	if ir>=val(r$) and val(r$)><0 then goto L700
L590: ! read amounts from gl master file
L600: read #3,using L690: ir,bb,cb,mat by,mat bp,mat bm,mat revb eof L880
	if ir=0 then goto L600
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L690
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L680
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L690
L680: if fscode>1 then bb=bp(fscode-1) else bb=0
L690: form pos mp1,pd 3,pos 81,41*pd 6.2,pos 339,13*pd 6.2
L700: if ir=val(r$) then goto L710 else goto L860
L710: total2=total2+cb
	for z=1 to 13
		annualb=annualb+bm(z)
		finalb=finalb+revb(z)
	next z
	if fscode=0 then monthb=monthb+bm(actpd) else monthb=monthb+bm(fscode) ! 11/24/86
	if fscode=0 then goto L780 else goto L820 ! 11/24/86
L780: for j=1 to actpd
		ytdb=ytdb+bm(j)
	next j
	goto L590
L820: for j=1 to fscode ! 11/24/86
		ytdb=ytdb+bm(j) ! 11/24/86
	next j ! 11/24/86
	goto L590 ! 11/24/86
L860: if ir<val(r$) then goto L590
	if ir>val(r$) then goto L890
L880: notrans=1
L890: unexpend=finalb-total2
	for j=1 to 9
		if ac(j)=9 then goto L960 ! 10/14/87
		accum(j,1)=accum(j,1)+annualb
		accum(j,2)=accum(j,2)+finalb
		accum(j,3)=accum(j,3)+total2
		accum(j,4)=accum(j,4)+unexpend
L960: next j
	if rs=1 then finalb=-finalb else goto L1010
	total2=-total2
	annualb=-annualb
	unexpend=unexpend
L1010: if ds=1 then dollar$="$" else dollar$=" "
	if annualb><0 or total2><0 then goto L1050
	if finalb<>0 then goto L1050
	if ls+ds+ul+ic>0 then goto L1050 else goto L430
L1050: sp2=22-sp-1
	if ul=1 then pr #255,using L1071: d$(1:sp2),dollar$,"{\ul ",annualb,"}",dollar$,"{\ul ",finalb,"}",dollar$,"{\ul ",total2,"}",dollar$,"{\ul ",unexpend,"}" pageoflow L1620 : goto L1070
	pr #255,using L1070: d$(1:sp2),dollar$,annualb,dollar$,finalb,dollar$,total2,dollar$,unexpend pageoflow L1620
L1070: form pos sp,c sp2,pos 22,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
L1071: form pos sp,c sp2,pos 22,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
	finalb=0
	total2=0
	annualb=0
	unexpend=0
	gosub L1390
	if ul=1 then goto L1140
	gosub L1630
L1140: gosub L1470
	goto L430
L1160: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=accum(ap,4) else accum4=accum(ap,4)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=22-sp-1
	if ul=1 then pr #255,using L1071: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}",dollar$,"{\ul ",accum3,"}",dollar$,"{\ul ",accum4,"}" pageoflow L1620 : goto L1240
	pr #255,using L1070: d$(1:sp2),dollar$,accum1,dollar$,accum2,dollar$,accum3,dollar$,accum4 pageoflow L1620
L1240: gosub L1390
	if ul=1 then goto L1260
	gosub L1630
L1260: gosub L1470
	goto L430
L1280: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1470
	goto L430
L1320: if foot1=1 then goto L1370
	tabnote=sp
	foot1=1
	foot$=d$
	goto L430
L1370: foot$=rtrm$(foot$)&d$
	goto L430
L1390: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1450 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
L1450: next j
return
L1470: if ls=0 then goto L1610
	if ls=99 then goto L1520
	pr #255,using L1500: " "
L1500: form pos 1,c 1,skip ls
	goto L1610
L1520: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1570: rtrm$(foot$),"Page "&str$(pt1)
L1570: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1610
	pr #255: newpage
	gosub L1750
L1610: return
L1620: gosub L1520: continue
L1630: if ul=0 then goto L1720
	if ul=1 then goto L1690
	underlin$="=============="
! pr #255:
	goto L1700
	goto L1720
L1690: underlin$="______________"
L1700: pr #255,using L1710: underlin$,underlin$,underlin$,underlin$
L1710: form pos 22,4*c 15,skip redir
L1720: if redir=0 then pr #255,using L1730: " "
L1730: form skip 1,c 1,skip 0
return
L1750: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255: tab(67);"Varience with"
	pr #255: tab(68);"Final Budget"
	pr #255: tab(25);" Original";tab(40);"  Final";tab(56);"";tab(70);"Positive"
	pr #255,using L1870: "  Budget","  Budget"," Actual","- Negative"
L1870: form pos 25,c 10,pos 40,c 10,pos 56,c 10,pos 69,c 10,skip redir
	pr #255: tab(24);"____________   ____________  _____________   ____________"
	pr #255:
return
 
L1920: eofcode=1
	gosub L1520
 
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
	goto Xit
 
Xit: fnXit
 
include: ertn
