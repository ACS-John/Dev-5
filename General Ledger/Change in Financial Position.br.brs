! Replace S:\acsGL\acglChgR
! Statement of Change in Financial Position with Comparison for Letter size paper

	autoLibrary
	on error goto Ertn

	dim fl1$*256,cogl$(3)*12,pedat$*20
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2)
	dim acct$*12,bp(13),lastact$*12,d(2),by(13),message$*40
	eofcode=0

	fnTop(program$,"Change in Financial Position")
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	pedat$=fnpedat$
	actpd=fnactpd
	fscode=fnfscode
	priorcd=fnpriorcd

	mp1=75
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"

	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
	L280: !
	read #1,using L290: acct$,cb,mat by,mat bp eof L350
	L290: form pos 1,c 12,pos 87,27*pd 6.2
	if acct$>cogl$(3) then goto L350
	if fscode=0 then income=income-cb else goto L330
goto L340
L330: !
	if priorcd=2 then income=income-bp(fscode) else income=income-by(fscode)
L340: !
goto L280
L350: close #1:
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto BEGIN_PRINTING
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
BEGIN_PRINTING: !
	fnopenprn
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	report$="STATEMENT OF CHANGES IN FINANCIAL POSITION"
	fnopenprn
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
	if fnps=2 then goto L550 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] [temp]\fsindex.H[cno] 75 3 Replace DupKeys -N"
goto L560
L550: !
	execute "Index [Q]\GLmstr\GLmstr.h[cno] [temp]\fsindex.H[cno] 78 3 Replace DupKeys -N"
L560: !
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr",internal,input,keyed
L570: !
	read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof DONE
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L570
	if costcntr=0 then goto L610
	if costcntr><fc then goto L570
L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L640
	if heading=0 and te$><"R" then gosub L1610
L640: on pos ("RFHDTSP",te$,1) goto L1120,L1170,L650,L710,L1020,L1120,L1870 none L570
L650: pr #255,using L660: d$
L660: form pos sp,c 50,skip 1
	gosub L1310
	gosub L1260
	goto L570

L710: if notrans=1 then goto L880
	if fr=val(r$) and val(r$)><0 then goto L810
	if fr>val(r$) then goto L810
L740: ! read amounts form gl maste file
L750: !
	read #3,using L770: fr,bb,cb,mat by,mat bp,pbp noRec L740 eof L870
	if fr=0 then goto L750
L770: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
	if fscode=0 then goto L810
	if fscode<1 or fscode>12 then fscode=1
	if priorcd=2 then cb=bp(fscode) else cb=by(fscode)
	L810: !
	if fr=val(r$) then goto L820 else goto L850
	L820: !
	if priorcd=2 then total+=(cb-pbp) else total+=(cb-bp(12))
goto L740

L850: !
	if fr<val(r$) then goto L740
	if fr>val(r$) then goto L880
	L870: !
	notrans=1
	L880: !
	for j=1 to 9 : accum(j,1)+=total : next j
	if rs=1 then total=-total else goto L900
	L900: !
	if ds=1 then dollar$="$" else dollar$=" "
	if total><0 then goto L930
	if ls+ul+ds+ic>0 then
		goto L930
	else
		goto L570
	end if

L930: !
	sp2=67-sp-1
	pr #255,using L950: d$(1:sp2),dollar$,total pageoflow L1470
	L950: form pos sp,c sp2,pos 67,c 1,pic(--,---,---.##),skip redir
	total=0
	gosub L1260
	gosub L1480
	gosub L1310
goto L570

L1020: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=67-sp-1
	pr #255,using L950: d$(1:sp2),dollar$,accum1 pageoflow L1470
	gosub L1260
	gosub L1480
	gosub L1310
goto L570

L1120: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1310
goto L570

L1170: if foot1=1 then goto L1230
	tabnote=sp
	foot1=1
	foot$=d$
goto L570

L1230: !
	foot$=rtrm$(foot$)&d$
goto L570

L1260: ! r:
	for j=1 to 9
		if ac(j)<>0 then accum(j,1)=0
	next j
return ! /r

L1310: ! r:
	if ls then
		if ls<>99 then
			pr #255,using L1340: " "
			L1340: form pos 1,c 1,skip ls
			goto L1450
		end if
		L1360: !
		fnpglen(pglen)
		! If PGLEN<>42 Then pGLEN=58
		sk=pglen-krec(255): fl=len(rtrm$(foot$))
		! If PGLEN=42 Then sK=SK+1
		pr #255,using L1410: rtrm$(foot$),"Page "&str$(pt1)
		L1410: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
		if ~eofcode then
			pr #255: newpage
			gosub L1610
		end if
	end if
	L1450: !
return ! /r

L1470: gosub L1360 : continue

L1480: ! r:
	if ul=0 then goto L1570
	if ul=1 then goto L1540
	underlin$="=============="
	pr #255,using L1520: underlin$
	L1520: form skip 1,pos 67,c 14,skip redir
	goto L1570
	L1540: !
	underlin$="______________"
	pr #255,using L1560: underlin$
	L1560: form pos 67,c 14,skip redir
	L1570: !
	if redir=0 then pr #255,using 'form skip 1,c 1,skip 0': " "
return ! /r

L1610: ! r:
	heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255: ""
	on error goto L1760
	a=len(rtrm$(pedat$))
	b=val(rtrm$(pedat$(a-4:a)))
	pr #255,using L1730: b
	L1730: form pos 72,pic(zzzz),skip 2
	on error goto Ertn
goto L1790 ! /r

L1760: ! r:
	pr #255: tab(68);"Current Year"
	on error goto Ertn
	pr #255:
	L1790: !
return ! /r

DONE: !
	eofcode=1
	gosub L1360
	fncloseprn
goto Xit

L1870: !
	total=income
goto L880

Xit: fnXit

include: ertn
