! Replace S:\acsGL\AcGlIncY
! -- INCOME STATEMENT FOR THIRTEEN PERIODS
 
	autoLibrary
	on error goto Ertn
 
	dim p$(20)*50
	dim fl1$*256,actpd$*6,pedat$*20,m1$(13)*9,m2$(13)*8,total(13)
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*12
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,13)
	dim by(13),bp(13)
 
	fnTop(program$,"Income Statement with Period Comparison")
	on fkey 5 goto L2360
	data "     ONE","     TWO","   THREE","    FOUR","    FIVE","     SIX","   SEVEN","   EIGHT","    NINE","     TEN","  ELEVEN","  TWELVE",""
	read mat m1$
	data "     ONE","     TWO","   THREE","    FOUR","    FIVE","     SIX","   SEVEN","   EIGHT","    NINE","     TEN","  ELEVEN","  TWELVE","THIRTEEN"
	read mat m2$
 
	actpd=fnactpd
	actpd$=fnactpd$
 
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r
	read #20,using 'form pos 384,n 2,pos 296,N 2',rec=1: nap,lmu
 
	pors=1
	mp1=69
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	form c 9,skip 0
L350: form pos mp1,pd 3,pos 81,41*pd 6.2
	form c 7,skip 0
	nametab=int(95-len(rtrm$(env$('cnam')))/2)
	open #1: fl1$,i,i,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L490
	fnTos(sn$="Acglincy") : _
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
L490: !
	close #101: ioerr ignore
	open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION= INCOME STATEMENT WITH MONTHLY COMPARISONS ",display,outIn
	pr f "08,18,Cc 41,H,N": env$('cnam')
	pr f "09,18,C 41,H,N": "            COMPANY NUMBER [cno]"
	pr f "11,18,C 41,R,N": "              IN PROCESS"
	pr f "13,30,C 16,R,N": "PRESS F5 TO STOP"
	if cmdkey=5 then goto L2380 ! jb
	report$="STATEMENT OF INCOME AND EXPENSES"
	fnopenprn
	if fnps=2 then goto L630 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] [temp]\fsindex.h[cno] 69 3 Replace DupKeys -N"
	goto L640
L630: execute "Index [Q]\GLmstr\GLmstr.h[cno] [temp]\fsindex.h[cno] 72 3 Replace DupKeys -N"
L640: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr",i,i,k
	redir=0: if file$(255)(1:4)<>"PRN:" then redir=1
L660: read #1,using L710: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L2360
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L660
	if costcntr=0 then goto L710
	if fc=0 and te$="F" then goto L720
	if costcntr><fc then goto L660
L710: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
L720: if te$="S" or te$="F" then goto L740
	if heading=0 and te$><"R" then gosub L2160
L740: on pos ("RFHDTS",te$,1) goto L1630,L1670,L750,L800,L1350,L1630 none L660
L750: pr #255,using L760: d$(1:40)
L760: form pos sp,c 40,skip 1
	gosub L1810
	gosub L1740
	goto L660
L800: if notrans=1 then goto L1120
	if ir=val(r$) and val(r$)><0 then goto L860
	if ir>val(r$) then goto L860
L830: ! read amounts from gl master
L840: read #3,using L350: ir,bb,cb,mat by,mat bp eof L1110
	if ir=0 then goto L840
L860: if ir=val(r$) then goto L870 else goto L1090
L870: if fnpriorcd=2 then goto L1020
	for j=1 to 13
		if j=1 and actpd=1 then total(j)=total(j)+cb else goto L910
		goto L1000
L910: if j=1 then total(j)=total(j)+by(j) else goto L930
		goto L1000
L930: if j>actpd then goto L1000
		if j=<lmu then total(j)=total(j)+by(j)-by(j-1) else goto L960
		goto L1000
L960: if lmu=actpd then goto L970 else goto L990
L970: total(j)=total(j)+by(j)-by(j-1)
		goto L1000
L990: total(j)=total(j)+cb-by(j-1)
L1000: next j
	goto L830
L1020: for j=1 to 13
		if j>nap then goto L1070
		if j=1 then total(j)=total(j)+bp(j) else goto L1060
		goto L1070
L1060: total(j)=total(j)+bp(j)-bp(j-1)
L1070: next j
	goto L830
L1090: if ir<val(r$) then goto L830
	if ir>val(r$) then goto L1120
L1110: notrans=1
L1120: for j=1 to 9
		if ac(j)=9 then goto L1170
		for k=1 to 13
			accum(j,k)=accum(j,k)+total(k)
		next k
L1170: next j
	for j=1 to 13
		if rs=1 then total(j)=-total(j)
	next j
	goto L1230 ! IF SUM(TOTAL)><0 THEN GOTO 920
	if ls+ul+ds+ic>0 then goto L1230 else goto L660
L1230: sp2=48-sp-1
	if nap=13 then goto L1280
	pr #255,using L1260: d$(1:sp2),sum(total),total(1),total(2),total(3),total(4),total(5),total(6),total(7),total(8),total(9),total(10),total(11),total(12) pageoflow L1960
L1260: form pos sp,c sp2,pos 38,13*n 12.2,skip redir
	goto L1300
L1280: pr #255,using L1290: d$(1:sp2),sum(total),mat total pageoflow L1960
L1290: form pos sp,c sp2,pos 38,14*n 12.2,skip redir
L1300: mat total=(0)
	gosub L1740
	gosub L1970
	gosub L1810
	goto L660
L1350: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if rs=1 then accum3=-accum(ap,3) else accum3=accum(ap,3)
	if rs=1 then accum4=-accum(ap,4) else accum4=accum(ap,4)
	if rs=1 then accum5=-accum(ap,5) else accum5=accum(ap,5)
	if rs=1 then accum6=-accum(ap,6) else accum6=accum(ap,6)
	if rs=1 then accum7=-accum(ap,7) else accum7=accum(ap,7)
	if rs=1 then accum8=-accum(ap,8) else accum8=accum(ap,8)
	if rs=1 then accum9=-accum(ap,9) else accum9=accum(ap,9)
	if rs=1 then accum10=-accum(ap,10) else accum10=accum(ap,10)
	if rs=1 then accum11=-accum(ap,11) else accum11=accum(ap,11)
	if rs=1 then accum12=-accum(ap,12) else accum12=accum(ap,12)
	if rs=1 then accum13=-accum(ap,13) else accum13=accum(ap,13)
	accumt=0
	for j=1 to 13
		if rs=1 then accumt=accumt-accum(ap,j) else accumt=accumt+accum(ap,j)
	next j
	sp2=48-sp-1
	if nap=13 then goto L1580
	pr #255,using L1560: d$(1:sp2),accumt,accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12 pageoflow L1960
L1560: form pos sp,c sp2,pos 38,13*n 12.2,skip redir
	goto L1590
L1580: pr #255,using L1290: d$(1:sp2),accumt,accum1,accum2,accum3,accum4,accum5,accum6,accum7,accum8,accum9,accum10,accum11,accum12,accum13
L1590: gosub L1740
	gosub L1970
	gosub L1810
	goto L660
L1630: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1810
	goto L660
L1670: if foot1=1 then goto L1720
	tabnote=sp
	foot1=1
	foot$=d$
	goto L660
L1720: foot$=rtrm$(foot$)&d$
	goto L660
L1740: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1790
		for k=1 to 13
			accum(j,k)=0
		next k
L1790: next j
return
L1810: if ls=0 then goto L1950
	if ls=99 then goto L1860
	pr #255,using L1840: " "
L1840: form pos 1,c 1,skip ls
	goto L1950
L1860: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1910: rtrm$(foot$)
L1910: form skip sk,pos tabnote,c fl,skip 1
	if eofcode=1 then goto L1950
	pr #255: newpage
	gosub L2160
L1950: return
L1960: gosub L1860: continue
L1970: if ul=0 then goto L2120
	if ul=1 then goto L2060
	underlin$=" ==========="
	if nap=13 then goto L2030
	pr #255,using L2040: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
	goto L2120
L2030: pr #255,using L2040: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
L2040: form skip 1,pos 38,14*c 12,skip redir
	goto L2120
L2060: underlin$=" ___________"
	if nap=13 then goto L2100
	pr #255,using L2110: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,""
	goto L2120
L2100: pr #255,using L2110: underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$,underlin$
L2110: form skip redir,pos 38,14*c 12,skip redir
L2120: if redir=0 then pr #255,using L2130: " "
L2130: form skip 1,c 1,skip 0
return
 
L2160: heading=1
	pr #255,using L2180: env$('cnam')
L2180: form skip 2,pos 10,cc 70,skip 1
	p1=95-len(rtrm$(report$))/2
	pr #255,using L2210: rtrm$(report$)
L2210: form pos 10,cc 70
	if rtrm$(secondr$)="" then goto L2250
	p1=95-len(rtrm$(secondr$))/2
	pr #255,using L2210: rtrm$(secondr$)
L2250: p1=81-len(rtrm$(actpd$))/2-len(rtrm$(pedat$))/2
	pr #255,using L2210: "For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)
	pr #255:
	pr #255:
	if nap=13 then goto L2320
	pr #255,using L2330: "YTD-TOTAL",mat m1$
	goto L2340
L2320: pr #255,using L2330: "YTD-TOTAL",mat m2$
L2330: form pos 42,14*c 12,skip 2
L2340: return
 
L2360: eofcode=1
	gosub L1860
L2380: !
 
	fncloseprn
 
goto Xit
 
Xit: fnXit
 
include: ertn
