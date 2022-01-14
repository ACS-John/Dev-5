! Replace S:\acsGL\AcGlIncF
! -- INCOME STATEMENT COMPARING UP TO 10 FUNDS  - USES 2ND I/C DESIGN
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,cogl$(3)*12,pedat$*20,cch$*20
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(10,9,2)
	dim by(13),bp(13),resp$(30)*50
	dim sendto$*80,bigul$*140,heading$*140,cap$*128,udf$*256
	dim fundnum(10),funddesc$(10)*20,io1$(20),dolcol$*140,accumcol$*140
	dim choices$(2)*21,io5$(2)
 
	fnTop(program$,cap$="Income Statemet - Fund Comparison")
	on fkey 5 goto L2170
	fncno(cno,cnam$)
	udf$=env$('temp')&'\'
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit : _
		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	actpd$=fnactpd$
	pedat$=fnpedat$
	actpd$=fnactpd$
	actpd=fnactpd
	fscode=fnfscode
	priorcd=fnpriorcd
 
	monthly=1 ! use 2 for ytd figures (set default for automatic processing to monthly information
	gosub L2510
	pors=1
	if fnps=2 then mp1=72 else mp1=69
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	form c 9,skip 0
L340: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
	form c 7,skip 0
	nametab=int(44-len(rtrm$(cnam$))/2)
	open #1: fl1$,i,i,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L480
	fnTos(sn$="ACglincf") : _
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
L480: report$="STATEMENT OF INCOME AND EXPENSES - FUND COMPARISON"
	fnopenprn
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
	if fnps=2 then goto L540 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 69 3 Replace DupKeys -N"
	goto L550
L540: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 72 3 Replace DupKeys -N"
L550: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",i,i,k
L560: read #1,using L610: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2170
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
	if costcntr=0 then goto L610
	if fc=0 and te$="F" then goto L620
	if costcntr><fc then goto L560
L610: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L620: if te$="S" or te$="F" then goto L640
	if heading=0 and te$><"R" then gosub L1970
L640: on pos ("RFHDTS",te$,1) goto L1430,L1470,L650,L700,L1280,L1430 none L560
L650: pr #255,using L660: d$(1:40)
L660: form pos sp,c 40,skip 1
	gosub L1640
	gosub L1540
	goto L560
L700: if notrans=1 then goto L950
	if ir=val(r$) and val(r$)><0 then goto L830
	if ir>val(r$) then goto L830
L730: ! read amounts from gl master file
L740: read #3,using L340: dno,ano,sno,ir,pcr,bb,cb,mat by,mat bp eof L940
	if ir=0 then goto L740
	if fscode=0 then goto L830
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L820
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L830
L820: if fscode>1 then bb=bp(fscode-1) else bb=0
L830: if ir=val(r$) then goto L840 else goto L880
L840: for x=1 to 10
		if dno=fundnum(x) then fund=x ! put in certain column
		if fundnum(x)>0 then totcol=x ! total columns needed
	next x
L880: if ir=val(r$) then total(fund)=total(fund)+(cb-bb) else goto L920
	total2(fund)=total2(fund)+cb
	k$=cnvrt$("N 5",pcr)
	goto L730
L920: if ir<val(r$) then goto L730
	if ir>val(r$) then goto L950
L940: notrans=1
L950: for j=1 to 9
		if ac(j)=9 then goto L1010 ! 10/14/87
		for j2=1 to 10
			accum(j2,j,1)=accum(j2,j,1)+total(j2)
			accum(j2,j,2)=accum(j2,j,2)+total2(j2)
		next j2
L1010: next j
	if rs=1 then goto L1030 else goto L1070
L1030: for j=1 to 10
		total(j)=-total(j)
		total2(j)=-total2(j)
	next j
L1070: if ds=1 then dollar$="$" else dollar$=" "
	if sum(total)><0 or sum(total2)><0 then goto L1100
	if ls+ul+ds+ic>0 then goto L1100 else goto L560
L1100: sp2=49-sp-1
! pr #255,USING 1070: D$(1:SP2),DOLLAR$,TOTAL(FUND),DOLLAR$,TOTAL2(FUND) PAGEOFLOW 1560
	dolcol$=""
	if monthly=2 then mat total=total2 ! substitute ytd figures if ytd stmt
	for j=1 to totcol
		dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
	next j
	pr #255,using L1180: d$(1:sp2),rtrm$(dolcol$) pageoflow L1790
L1180: form pos sp,c sp2,pos 49,c big,skip redir
! IF PC0=1 THEN GOSUB BLDPCT2
! IF PC3>0 OR PC4>0 THEN pr #255,USING 1100: PC3,PC4
	form pos 63,n 4,pos 82,n 4,skip redir
	mat total=(0)
	mat total2=(0)
	gosub L1540
	gosub L1810
	gosub L1640
	goto L560
L1280: accumcol$=""
	for j=1 to totcol
		if ap=0 then ap=1
		if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
		if rs=1 then accum2=-accum(j,ap,2) else accum2=accum(j,ap,2)
		if ds=1 then dollar$="$" else dollar$=" "
		if monthly=2 then accum1=accum2
		accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
	next j
	sp2=49-sp-1
	pr #255,using L1180: d$(1:sp2),rtrm$(accumcol$) pageoflow L1790
	gosub L1540
	gosub L1810
	gosub L1640
	goto L560
L1430: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1640
	goto L560
L1470: if foot1=1 then goto L1520
	tabnote=sp
	foot1=1
	foot$=d$
	goto L560
L1520: foot$=rtrm$(foot$)&d$
	goto L560
L1540: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1620 ! 10/14/87
		for j2=1 to 10
! aCCUM(FUND,J,1)=0
			accum(j2,j,1)=0
! aCCUM(FUND,J,2)=0
			accum(j2,j,2)=0
		next j2
L1620: next j
return
L1640: if ls=0 then goto L1780
	if ls=99 then goto L1690
	pr #255,using L1670: " "
L1670: form pos 1,c 1,skip ls
	goto L1780
L1690: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1740: rtrm$(foot$)
L1740: form skip sk,pos tabnote,c fl,skip 1
	if eofcode=1 then goto L1780
	pr #255: newpage
	gosub L1970
L1780: return
L1790: gosub L1690
	continue
L1810: if ul=0 then goto L1940
	if ul=1 then goto L1850
	underlin$="  ============"
	goto L1860
L1850: underlin$="  ____________"
L1860: bigul$=""
	for j=1 to totcol
		bigul$=bigul$&underlin$
	next j
	if ul=1 then pr #255,using L1920: bigul$
	if ul=2 then pr #255,using L1930: bigul$
L1920: form skip redir,pos 49,c big,skip redir
L1930: form skip 1,pos 49,c big,skip redir
L1940: if redir=0 then pr #255,using L1950: " "
L1950: form skip 1,c 1,skip redir
return
L1970: heading=1
	if pt1=0 then pt1=1 else pt1=pt1+1
	pr #255,using L2000: cnam$,"PAGE "&str$(pt1)
L2000: form skip 2,pos 15,cc 60,pos 73,c 7
	p1=44-len(rtrm$(report$))/2
	pr #255,using L2030: rtrm$(report$)
L2030: form pos 15,cc 60
	if rtrm$(secondr$)="" then goto L2070
	p1=44-len(rtrm$(secondr$))/2
	pr #255,using L2030: rtrm$(secondr$)
L2070: p1=30-len(rtrm$(actpd$))/2-len(rtrm$(pedat$))/2
	if monthly=1 then pr #255,using L2100: "For the one month period ended "&rtrm$(pedat$)
	if monthly=2 then pr #255,using L2100: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(pedat$)
L2100: form pos 15,cc 60
	pr #255:
	pr #255:
	pr #255,using L2140: heading$
L2140: form pos 49,c big,skip 2
return
 
L2170: eofcode=1
	gosub L1690
 
 
	fncloseprn
	goto Xit
 
Xit: fnXit
 
BLDPCT1: open #10: "Name=[Temp]\Work.[Session],KFName=[Temp]\Addr.[Session],Replace,RecL=17,KPS=1,KLN=5",internal,outIn,keyed
	for j=1 to lrec(3)
		read #3,using L2290,rec=j: pc1,bb,cb noRec L2380
L2290: form pos mp1,pd 3,pos 81,2*pd 6.2
		k$=cnvrt$("N 5",pc1)
		read #10,using L2320,key=k$: pc1,pc2,yt2 nokey L2370
L2320: form pos 1,g 5,2*pd 6.2
		pc2=pc2+cb-bb
		yt2=yt2+cb
		rewrite #10,using L2320: pc1,pc2,yt2
		goto L2380
L2370: write #10,using L2320: pc1,cb-bb,cb
L2380: next j
	pc0=1
return
BLDPCT2: !
	pc3=pc4=0
	if val(k$)=0 then goto L2510
	read #10,using L2320,key=k$: pc1,pc2,yt2 nokey L2510
	if total(fund)=0 then goto L2480
	pc3=round(((total(fund)-pc2)/total(fund))*100,0)
	if pc3<-999 or pc3>9999 then pc3=0
L2480: if total2(fund)=0 then goto L2510
	pc4=round(((total2(fund)-yt2)/total(fund))*100,0)
	if pc4<-999 or pc4>9999 then pc4=0
L2510: open #5: "Name=[Q]\GLmstr\GLfund.h[cno],RecL=230,use",i,outi,r
	read #5,using L2530: mat fundnum,mat funddesc$ ioerr L2530
L2530: form pos 1,10*n 3,10*c 20
	fnTos(sn$="ACglcasf3") : _
	mylen=1: mypos=mylen+3
	fnLbl(1,4,"Fund                 Description ")
	for j=1 to 10
		fnTxt(j+1,mypos,3,0,right,'30',0,"Enter the fund number.") : _
		resp$(j*2-1)=str$(fundnum(j))
		fnTxt(j+1,mypos+10,20,0,0,"",0,"Enter the fund description.") : _
		resp$(j*2)=funddesc$(j)
	next j
	fnCmdKey("&Next",1,1,0,"Continues with financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	for j=1 to 10
		fundnum(j)=val(resp$(j*2-1))
		funddesc$(j)=resp$(j*2)
	next j
	rewrite #5,using L2530: mat fundnum,mat funddesc$ ioerr L2700
	goto L2710
L2700: write #5,using L2530: mat fundnum,mat funddesc$
L2710: close #5:
	for j=1 to 10
		if fundnum(j)>0 then heading$=heading$&" "&lpad$(rtrm$(funddesc$(j)(1:13)),13): totcol=totcol+1
	next j
	big=totcol*14
return
ASK_MONTHLY: ! ask monthly info or ytd info
	fnTos(sn$="ACglcasf2") : _
	mylen=30: mypos=mylen+3 : right=1
	fnOpt(1,2,"Print Monthly Figures" ,0,0) : _
	resp$(2)='False'
	fnOpt(2,2,"Print Year to Date Figures" ,0,0) : _
	resp$(2)='True'
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)='True' then monthly=1
	if resp$(2)='True' then monthly=2
return
 
include: ertn
