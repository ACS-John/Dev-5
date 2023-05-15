! Replace S:\acsGL\acglCasF
! Cash Flow Statement with Fund Comparisons
 
	autoLibrary
	on error goto Ertn
 
	dim choices$(2)*21,io5$(2),bigul$*140,heading$*140
	dim fundnum(10),funddesc$(10)*20,io1$(20),dolcol$*140,accumcol$*140
	dim bm(13),bp(13),by(13),cap$*128,fl1$*256,in3$(4),sc1$(2)*20,udf$*256
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(10,9,7),resp$(30)*50
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
 
	fnTop(program$,cap$="Cash Flow with Fund Comparison")
	report$=cap$
	actpd$=fnactpd$
	actpd=fnactpd
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	fscode=fnfscode
	priorcd=fnpriorcd
	udf$=env$('temp')&'\'
	monthly=1 ! default to monthly information
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r: read #20,using 'form pos 384,n 2',rec=1: nap : close #20:
	fscode=fnfscode
	pors=1
	gosub L2370
	form pos 1,n 2,c 40,pos 89,2*n 1,pos 141,6*n 1,3*n 2,c 6,3*c 12,2*c 20,pos 237,n 2
	in3$(1)="8,25,N 12.2,UT,N" : in3$(2)="8,45,N 12.2,UT,N"
	mp1=75
	if fnPs=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnPs=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
	open #1: fl1$,i,i,k
	if fnProcess=1 or fnUseDeptNo=0 then goto L410
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
	gosub ASK_MONTHLY
L410: on fkey 5 goto L2250
	if fnPs=2 then goto L450 ! secondary
	close #3: ioerr L430
L430: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 75 3 Replace DupKeys -N"
	goto L460
L450: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 78 3 Replace DupKeys -N"
L460: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",i,i,k
	fnOpenPrn
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
L480: read #1,using L520: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2250
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L480
	if costcntr=0 then goto L520
	if costcntr><fc then goto L480
L520: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L550
	if heading=0 and te$><"R" then gosub L2130
L550: on pos ("RFHDTSBC",te$,1) goto L1550,L1600,L560,L620,L1390,L1550,L620,L2300 none L480
L560: pr #255,using L570: d$(1:40)
L570: form pos sp,c 40,skip 1
	gosub L1780
	gosub L1690
	goto L480
 
L620: if te$="B" and ap>0 then goto L1390 ! ENDING BANK BALANCE
	if notrans=1 then goto L1040
	if ir>=val(r$) and val(r$)><0 then goto L770
L650: ! read amounts from gl master file
L660: read #3,using L760: dno,ano,sno,ir,bb,cb,mat by,mat bp,mat bm eof L1030
	if ir=0 then goto L660
!  cB=5: bB=1: bP(12)=100.00
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L760
	if fscode<1 or fscode>13 then fscode=1
	if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if fnpriorcd=2 then goto L750
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L760
L750: if fscode>1 then bb=bp(fscode-1) else bb=0
L760: form pos 1,n 3,n 6,n 3,pos mp1,pd 3,pos 81,41*pd 6.2
L770: if ir=val(r$) then goto L780 else goto L840
L780: fund=0
	for x=1 to 10
		if dno=fundnum(x) then fund=x ! put in certain column
		if fundnum(x)>0 then totcol=x ! total columns needed
	next x
	if fund=0 then goto L650 ! no matching fund # - skip
L840: if ir=val(r$) then total(fund)=total(fund)+(cb-bb) else goto L1010
	if te$="B" then total(fund)=total(fund)-(cb-bb) : total(fund)=total(fund) - bb: total2(fund)=total2(fund)-bp(nap) : goto L870
	total2(fund)=total2(fund)+cb
L870: for z=1 to 13
		annualb=annualb+bm(z)
	next z
	if fscode=0 then monthb=monthb+bm(fnactpd) else monthb=monthb+bm(fscode)
	if fscode=0 then goto L920 else goto L960
L920: for j=1 to fnactpd
		ytdb=ytdb+bm(j)
	next j
	goto L650
L960: for j=1 to fscode
		ytdb=ytdb+bm(j)
	next j
	goto L650
 
L1010: if ir<val(r$) then goto L650
	if ir>val(r$) then goto L1040
L1030: notrans=1
L1040: for j=1 to 9
		if ac(j)=9 then goto L1100
		for j2= 1 to 10
			accum(j2,j,1)=accum(j2,j,1)+total(j2)
			accum(j2,j,2)=accum(j2,j,2)+total2(j2)
		next j2
L1100: next j
	if rs=1 then goto L1120 else goto L1160
L1120: for j=1 to 10
		total(j)=-total(j)
		total2(j)=-total2(j)
	next j
L1160: !
	if ds=1 then dollar$="$" else dollar$=" "
	if sum(total) or sum(total2) then goto L1200
	if ls+ds+ul+ic>0 then goto L1200 else goto L480
L1200: sp2=30-sp-1
	for j=1 to 10
		if te$="B" then total(j)=-total(j): total2(j)=-total2(j) ! REVERSE SIGN ON BEGINNING BANK BALANCE
	next j
	dolcol$=""
	if monthly=2 then mat total=total2 ! substitute ytd figures if ytd stmt
	for j=1 to totcol
		dolcol$=dolcol$&" "&dollar$&cnvrt$("PIC(-,---,---.##)",total(j))
	next j
	pr #255,using L1300: d$(1:sp2),rtrm$(dolcol$) pageoflow L1940
L1300: form pos sp,c sp2,pos 49,c big,skip redir
	form pos sp,c sp2,pos 52,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
	mat total=(0)
	mat total2=(0)
	gosub L1690
	gosub L1960
	gosub L1780
	goto L480
 
L1390: accumcol$=""
	for j=1 to totcol
		if ap=0 then ap=1
		if rs=1 then accum1=-accum(j,ap,1) else accum1=accum(j,ap,1)
		if rs=1 then accum2=-accum(j,ap,2) else accum2=accum(j,ap,2)
		if ds=1 then dollar$="$" else dollar$=" "
		if monthly=2 then accum1=accum2
		accumcol$=accumcol$&" "&dollar$&cnvrt$("pic(-,---,---.##)",accum1)
	next j
	sp2=30-sp-1
	pr #255,using L1300: d$(1:sp2),rtrm$(accumcol$) pageoflow L1940
	gosub L1690
	gosub L1960
	gosub L1780
	goto L480
 
L1550: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1780
	goto L480
 
L1600: if foot1=1 then goto L1660
	tabnote=sp
	foot1=1
	foot$=d$
	goto L480
 
L1660: foot$=rtrm$(foot$)&d$
	goto L480
 
L1690: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1750
		for j2=1 to 10
			accum(j2,j,1)=0
			accum(j2,j,2)=0
		next j2
L1750: next j
return
 
L1780: if ls=0 then goto L1920
	if ls=99 then goto L1830
	pr #255,using L1810: " "
L1810: form pos 1,c 1,skip ls
	goto L1920
L1830: fnPgLen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1880: rtrm$(foot$),"Page "&str$(pt1)
L1880: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1920
	pr #255: newpage
	gosub L2130
L1920: return
 
L1940: gosub L1830: continue
 
L1960: if ul=0 then goto L2090
	if ul=1 then goto L2000
	underlin$="  ============"
	goto L2010
L2000: underlin$="  ____________"
L2010: bigul$=""
	for j=1 to totcol
		bigul$=bigul$&underlin$
	next j
	if ul=1 then pr #255,using L2070: bigul$
	if ul=2 then pr #255,using L2080: bigul$
L2070: form skip redir,pos 49,c big,skip redir
L2080: form skip 1,pos 49,c big,skip redir
L2090: if redir=0 then pr #255,using L2100: " "
L2100: form skip 1,c 1,skip 0
return
 
L2130: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	if monthly=2 then pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	if monthly=1 then pr #255: "\qc  {\f181 \fs16 \b For the one month period ended "&rtrm$(fnpedat$)&"}"
	pr #255:
	pr #255,using L2220: heading$
L2220: form pos 49,c big,skip 2
return
 
L2250: eofcode=1
	gosub L1830
	fnfscode(actpd)
	fnClosePrn
	goto Xit
L2300: !
 
 
! need total,total2  current month, year to date
 
Xit: fnXit
 
L2370: open #5: "Name=[Q]\GLmstr\GLfund.h[cno],RecL=230,use",i,outi,r
	read #5,using L2390: mat fundnum,mat funddesc$ ioerr L2400
L2390: form pos 1,10*n 3,10*c 20
L2400: fnTos
	mylen=1: mypos=mylen+3
	fnTos
	mylen=1: mypos=mylen+3
	fnLbl(1,4,"Fund                 Description ")
	for j=1 to 10
		fnTxt(j+1,mypos,3,0,right,'30',0,"Enter the fund number.")
		resp$(j*2-1)=str$(fundnum(j))
		fnTxt(j+1,mypos+10,40,0,0,"",0,"Enter the fund description.")
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
	rewrite #5,using L2390: mat fundnum,mat funddesc$ ioerr L2570
	goto L2580
L2570: write #5,using L2390: mat fundnum,mat funddesc$
L2580: close #5:
	for j=1 to 10
		if fundnum(j)>0 then
			heading$=heading$&" "&lpad$(rtrm$(funddesc$(j)(1:13)),13)
			totcol+=1
		end if
	next j
	big=totcol*14
return
 
ASK_MONTHLY: ! ask monthly info or ytd info
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnOpt(1,2,"Print Monthly Figures" ,0,0)
	resp$(2)='False'
	fnOpt(2,2,"Print Year to Date Figures" ,0,0)
	resp$(2)='True'
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	if resp$(1)='True' then monthly=1
	if resp$(2)='True' then monthly=2
return
 
include: ertn
