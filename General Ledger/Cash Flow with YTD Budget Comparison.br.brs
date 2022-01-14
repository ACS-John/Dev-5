! formerly S:\acsGL\acglCasO

	autoLibrary
	on error goto Ertn

	dim bm(13),bp(13),by(13)
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),fl1$*256
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14

	fnTop(program$)
	report$=env$('program_caption')
	actpd$=fnactpd$
	actpd=fnactpd
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	fscode=fnfscode
	priorcd=fnpriorcd
	fnopenprn
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r
	read #20,using 'form pos 384,n 2',rec=1: nap
	close #20:
	fscode=fnfscode
	if nap<12 or nap>13 then nap=12
	in3$(1)="8,05,N 12.2,UT,N" : in3$(2)="8,25,N 12.2,UT,N"
	in3$(3)="8,45,N 12.2,UT,N" : in3$(4)="8,65,N 12.2,UT,N"
	if fnps=2 then mp1=78 else mp1=75
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
	open #1: fl1$,i,i,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L340
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
	L340: !
	open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\fsindex.h[cno],Shr",i,i,k
	NextGlRecord: !
	read #1,using L390: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof Done
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto NextGlRecord
	if costcntr=0 then goto L390
	if costcntr><fc then goto NextGlRecord
	L390: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L420
	if heading=0 and te$><"R" then gosub PrReportHeader
L420: on pos ("RFHDTSBC",te$,1) goto TeRandS,TeF,TeH,TeDandB,TeT,TeRandS,TeDandB,TeC none NextGlRecord

TeH: ! r:
	pr #255,using L450: d$(1:40)
	L450: form pos sp,c 40
	gosub SubL1590
	gosub SubL1470
goto NextGlRecord ! /r
TeDandB: ! r:
	if te$="B" and ap>0 then goto TeT ! ENDING BANK BALANCE
	if notrans=1 then goto L780
	if ir>=val(r$) and val(r$)><0 then goto L630
	READ_3: !
	read #3,using L620: ir,bb,cb,mat by,mat bp,mat bm eof L770
	if ir=0 then goto L640
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L620
	if fscode<1 or fscode>13 then fscode=1
	if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if fnpriorcd=2 then
		if fscode>1 then bb=bp(fscode-1) else bb=0
	else
		if fscode>1 then bb=by(fscode-1) else bb=0
	end if


	L620: form pos mp1,pd 3,pos 81,41*pd 6.2
	L630: !
	if ir=val(r$) then
		total+=(cb-bb)
	else
		goto L750
	end if
	L640: !
	if te$="B" then
		total-=(cb-bb): total-=bb: total2-=bp(nap)
		goto L660
	end if
	total2+=cb
	L660: !
	for z=1 to 13 : annualb+=bm(z) : next z
	if fnfscode=0 then monthb+=bm(fnactpd) else monthb+=bm(fnfscode)
	if fscode=0 then
		for j=1 to fnactpd : ytdb+=bm(j) : next j
	else
		for j=1 to fscode : ytdb+=bm(j) : next j
	end if
goto READ_3

L750: !
	if ir<val(r$) then goto READ_3
	if ir>val(r$) then goto L780

L770: !
	notrans=1
L780: overundr=ytdb-total2
	unexpend=annualb-total2
	for j=1 to 9
		if ac(j)<>9 then
			accum(j,1)+=total
			accum(j,2)+=total2
			accum(j,3)+=annualb
			accum(j,4)+=monthb
			accum(j,5)+=ytdb
			accum(j,6)+=overundr
			accum(j,7)+=unexpend
		end if
	next j
	if rs=1 then 
		total=-total 
		total2=-total2
		annualb=-annualb
		monthb=-monthb
		ytdb=-ytdb
		overundr=overundr
		unexpend=unexpend
	end if
	if ds=1 then dollar$="$" else dollar$=" "
	if annualb or total2 or total or ls+ds+ul+ic>0 then
		goto L1010
		L1010: !
		sp2=36-sp-1
		if te$="B" then total=-total: total2=-total2: unexpend=0: ! REVERSE SIGN ON BEGINNING BANK BALANCE
		pr #255,using L1040: d$(1:sp2),dollar$,total2,dollar$,annualb,dollar$,unexpend pageoflow L1760
		L1040: form pos sp,c sp2,pos 37,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
		total=0
		total2=0
		annualb=0
		monthb=0
		ytdb=0
		overundr=0
		unexpend=0
		gosub SubL1470
		gosub SubL1770
		gosub SubL1590
	end if
goto NextGlRecord ! /r
TeT: ! r:
	if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7=-accum(ap,7) else accum7=accum(ap,7)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=36-sp-1
	if te$="B" then accum3=accum4=accum7=0
	pr #255,using L1040: d$(1:sp2),dollar$,accum2,dollar$,accum3,dollar$,accum7 pageoflow L1760
	gosub SubL1470
	gosub SubL1770
	gosub SubL1590
goto NextGlRecord ! /r
TeRandS: ! r:
	if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub SubL1590
goto NextGlRecord ! /r
TeF: ! r:
	if foot1=1 then goto L1440
	tabnote=sp
	foot1=1
	foot$=d$
	goto NextGlRecord
	L1440: !
	foot$=rtrm$(foot$)&d$
goto NextGlRecord ! /r
SubL1470: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1560
		accum(j,1)=0
		accum(j,2)=0
		accum(j,3)=0
		accum(j,4)=0
		accum(j,5)=0
		accum(j,6)=0
		accum(j,7)=0
	L1560: !
	next j
return ! /r
SubL1590: ! r:
	if ls=0 then goto L1740
	if ls=99 then goto PrPageHeader
	pr #255,using L1620: " "
	L1620: form pos 1,c 1,skip ls
goto L1740
PrPageHeader: !
	fnpglen(pglen)
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
	pr #255,using L1700: rtrm$(foot$),"Page "&str$(pt1)
	L1700: form skip sk,pos tabnote,c fl,pos 75,c 8
	if eofcode=1 then goto L1740
	pr #255: newpage
	gosub PrReportHeader
	L1740: !
return ! /r
L1760: ! r:
	gosub PrPageHeader
continue ! /r
SubL1770: ! r:
	if ul=0 then goto L1860
	if ul=1 then goto L1830
	underlin$="=============="
	pr #255: ""
	goto L1840
	L1830: !
	underlin$="______________"
	L1840: !
	pr #255,using L1850: underlin$,underlin$,underlin$
	L1850: form skip redir,pos 37,3*c 15,skip redir
L1860: !
	if redir=0 then pr #255: ""
return ! /r
PrReportHeader: ! r:
	heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255: ''
	pr #255: tab(44);"Year To";tab(60);"Annual";tab(71);"Over/Under"
	pr #255: tab(45);"Date";tab(60);"Budget";tab(73);"Budget"
return ! /r
Done: ! r:
	eofcode=1
	gosub PrPageHeader
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
goto Xit ! /r

TeC: ! r:
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,10,d$)
	fnLbl(3,1,"Total Amount YTD:",mylen,right)
	fnTxt(3,mypos,12,0,right,'10',0,"Enter the total for the year.",0 )
	resp$(1)=str$(total2)
	fnLbl(4,1,"Total Budget Year to Date:",mylen,right)
	fnTxt(4,mypos,12,0,right,'10',0,"Enter the annual budget.",0 )
	resp$(2)=str$(annualb)
	fnCmdKey("&Next",1,1,0,"Accept the answer.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	total2=val(resp$(1))
	annualb=val(resp$(2))
return
Xit: fnXit

include: ertn
