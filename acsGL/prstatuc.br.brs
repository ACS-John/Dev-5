! Replace S:\acsGL\PRSTATUC
! Quarterly UC Report (From the after-the-fact payroll files in gl)
 
	autoLibrary
	on error goto Ertn
 
	dim k(1),k$(3)*25,l$(1)*11,d(14),m(36),n(2),cap$*128
	dim fa$(3),sa$(3)*40,cnam$*40
	dim a$(3)*40,b$(2)*12,c$*5,e(2),e$(2)*11,pedat$*20
 
	fnTop(program$,cap$="Print State UC Report")
	fncno(cno,cnam$)
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input
	read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,POS 188,PD 7.2,POS 658,10*N 1': mat a$,mat b$,c$,ucm,mat deduc
	close #1:
	if fnprocess=1 then goto L240
 
	fnTos
	mylen=35: mypos=mylen+3 : right=1
	fnLbl(1,1,"Quarterly Period Ending Date:",mylen,right)
	fnTxt(1,mypos,20,0,left,"",0,"Enter the last day of the quarter.",0 )
	resp$(1)=""
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	pedat$=resp$(1)
L240: open #2: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",i,i,k
	fnopenprn
	gosub HDR
L340: read #2,using L350: mat k,mat k$,mat l$,mat m eof L720
L350: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
	if m(2)=0 or k(1)=0 then goto L530
	deducy=deducq=0
	for j=1 to 10
		if deduc(j)=1 then deducy=deducy+m(j*2+9)
		if deduc(j)=1 then deducq=deducq+m(j*2+10)
	next j
	m(1)=m(1)-deducy
	m(2)=m(2)-deducq
	if p1<57 then goto L480
	gosub L920
	pr #255: newpage
	gosub HDR
L480: gosub L790
	t1=t1+m(2)
	t2=t2+h3
	t3=t3+h2
	t4=t4+m(34)
L530: goto L340
 
HDR: !
	pr #255,using L570: "PAGE ",p2+=1
L570: form pos 70,c 5,pic(zzz),skip 2
	pr #255,using L590: "SCHEDULE A - EMPLOYER'S REPORT OF WAGE'S PAID TO EACH EMPLOYEE"
L590: form pos 10,c 63
	pr #255,using L610: "AS OF ",fnpedat$,"FED ID",b$(1)
L610: form pos 26,c 6,c 20,pos 65,c 6,pos 75,c 40
	pr #255,using L630: "RATE",a$(1),"STATE ID",b$(2)
L630: form pos 6,c 9,pos 22,c 40,pos 65,c 8,pos 75,c 12
	pr #255,using L650: c$,a$(2),a$(3)
L650: form pos 6,c 5,pos 22,c 40,skip 1,pos 22,c 40,skip 2
	pr #255: tab(39);"TOTAL WAGES  EXCESS WAGES    TAXABLE   WEEKS"
	pr #255,using L680: "SS NUMBER   NAME                      FOR QUARTER   OVER  $"&ltrm$(str$(ucm)),"WAGES   WORKED"
L680: form pos 1,c 68,pos 69,c 18
	p1=16
return
 
L720: gosub L920
	close #2:
	fncloseprn
	goto Xit
 
Xit: fnXit
 
L790: p3=p3+1
	if m(1)<ucm then goto L860
	if m(1)-m(2)>ucm then goto L840
	h2=ucm-(m(1)-m(2))
	goto L870
L840: h2=0
	goto L870
L860: h2=m(2)
L870: h3=m(2)-h2
	pr #255,using L890: l$(1),k$(1),m(2),h3,h2,m(34)
L890: form pos 1,c 11,pos 13,c 25,pos 39,n 11.2,pos 53,n 11.2,n 11.2,n 8,skip 2
	p1=p1+2
return
L920: j1=58-p1
	pr #255,using L940: "----------   -----------  ---------  ------"
L940: form skip j1,pos 40,c 48
	pr #255,using L960: "EMPLOYEES ON THIS PAGE "&ltrm$(str$(p3))&"    PAGE TOTALS",t1,t2,t3,t4
L960: form pos 1,c 42,pos 39,n 11.2,pos 53,n 11.2,n 11.2,n 8.2
	p3=0
	t1=0
	t2=0
	t3=0
	t4=0
return
 
include: ertn
 
