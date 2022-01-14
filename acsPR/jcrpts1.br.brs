! Replace S:\acsPR\jcRptS1
! Create Job Cost Report Program
 
	autoLibrary
	on error goto Ertn
 
	dim rn$*2,rt$*51,ch$(2)*132,psc(100),cap$*128,message$*40
	dim f$(20)*50,pp(20),ppr(20),dp(20),fc(20),tcj(20),tcs(20),ty$(24)*8
	dim a$(20)*32,a(20),underlin$*30
	dim ln$*255,pf$*255,af$*255,gpf$*255,gaf$*255,jpf$*255,jaf$*255,upf$*255
	dim uaf$*255
 
	fnTop("S:\acsPR\jcRptS1",cap$="User Designed Reports (1)")
	fncno(cno)
 
	data "JN"
	data "n$"
	data "A$(1)"
	data "a$(2)"
	data "a$(3)"
	data "X6"
	data "X7"
	data "X8"
	data "X9"
	data "cn"
	data "k$"
	data "X12"
	data "X13"
	data "X14"
	data "X15"
	data "X16"
	data "X17"
	data "X18"
	data "X19"
	data "X20"
	data "X21"
	data "X22"
	data "X23"
	data "X24"
	read mat ty$
! 
	rn=fnrx
	underlin$="______________________________"
	cap$="Create Job Cost Report Program"
	rn$=lpad$(str$(rn),2)
 
	open #1: "Name=S:\acsPR\JCREPORT.MST,KFName=S:\acsPR\JCREPORT.idx",i,i,k
	read #1,using L490,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey Xit
L490: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
	close #1:
 
	pr newpage
	fnwait(message$="Please wait...",0)
 
	open #11: "Name=PROC."&session$,d,o ioerr L570
	close #11,free:
L570: open #11: "Name=PROC.[Session],SIZE=0,RecL=255",d,o
	pr #11: "Clear"
	pr #11: "ProcErr Return"
	pr #11: "Load S:\acsPR\JCRpt-Mod"
	pr #11: "00010 ! Replace S:\acsPR\JCPrnt";str$(rn) : _
	! this program is dynamicaly created by S:\acsPR\jcRptS1 and : _
	! uses S:\acsPR\JCRpt-Mod as a base"
	pr #11: '00051 RN$="';rn$;'"'
	pf$="19900 pr #255, USING 19910: "
	af$="19910 form skip 1"
	gpf$="20140 pr #255, USING 20150: "
	gaf$="20150 form skip 2,""Grand Totals"""
	jpf$="20025 pr #255, USING 20026: "
	jaf$="20026 form skip 1,""Job Totals"""
	upf$="20000 pr #255,using 20020: "
	uaf$="20020 form skip 0"
	pr #11: "19850 on zdiv goto 25000"
	pr #11: "19851 on uflow goto 25000"
	pr #11: "19852 on oflow goto 25000"
	pr #11: "25000 continue"
	pr #11: "25001 !"
	pr #11: "25010 jn=0"
	pr #11: "25020 cn=0"
	pr #11: "25030 Continue"
	pr #11: "25031 !"
	pr #11: '25050 cn$=""'
	pr #11: "25060 Continue"
	pr #11: "25061 !"
	for j=1 to 20
		if rtrm$(f$(j))="" then goto L920
		for j2=1 to 30 ! Search for Column Within Formula
			if uprc$(f$(j)(j2:j2))="C" then goto L900
		next j2
		pr #11: str$(19850+j*2+1);" c(";str$(j);")=c(";str$(j);")+";f$(j)
		goto L910
L900: pr #11: str$(19850+j*2+1);" c("&str$(j);")=";f$(j)
L910: j1=j1+1
L920: next j
	pr #11: "19895 x6=0"
	pr #11: "19896 x7=0"
	pr #11: "19897 x8=0"
	pr #11: "19898 x9=0"
	pr #11: "19899 if sd = 1 then 19833"
	for j=1 to 20
		if rtrm$(f$(j))="" then goto L1350
		if f$(j)(3:3)>="0" and f$(j)(3:3)<="9" then goto L1060 : _
			! Search Category Record
		if f$(j)(1:2)="x1" or f$(j)(1:2)="X1" then : _
			i$="jn$(1:"&str$(ppr(j))&")"
		if f$(j)(1:2)="x2" or f$(j)(1:2)="X2" then : _
			i$="n$(1:"&str$(ppr(j))&")"
		if f$(j)(1:2)="x3" or f$(j)(1:2)="X3" then : _
			i$="a$(1)  (1:"&str$(ppr(j))&")"
		if f$(j)(1:2)="x4" or f$(j)(1:2)="X4" then : _
			i$="a$(2)  (1:"&str$(ppr(j))&")"
		if f$(j)(1:2)="x5" or f$(j)(1:2)="X5" then : _
			i$="a$(3)  (1:"&str$(ppr(j))&")"
L1060: if f$(j)(1:3)="x10" or f$(j)(1:3)="X10" then : _
			i$="cn$(7:11)"
		if f$(j)(1:3)="x11" or f$(j)(1:3)="X11" then : _
			i$="k$(1:"&str$(ppr(j))&")"
		if rtrm$(i$)="" then cn=1 else cn=0
		if rtrm$(i$)="" then i$="c("&str$(j)&")"
		if fc(j)=1 then goto L1170 ! skip Detail Print
		pf$=rtrm$(pf$)&","&i$ ! pr Statement
		if j=<1 then goto L1150
		if fc(j-1)=1 then goto L1150
		if pp(j)<pp(j-1)+ppr(j-1) then af$=af$&",skip 1"
L1150: if cn=1 then : _
			af$=rtrm$(af$)&",pos "&str$(pp(j))&",N "&str$(ppr(j)) : _
			! form Statement
		if cn<>1 then : _
			af$=rtrm$(af$)&",pos "&str$(pp(j))&",C "&str$(ppr(j)) : _
			! form Statement
L1170: if tcs(j)=0 then goto L1210
		i$(1:1)="t"
		gpf$=rtrm$(gpf$)&","&i$ ! pr Stmt-Grand Totals
		gaf$=rtrm$(gaf$)&",pos "&str$(pp(j))&",N "&str$(ppr(j)) : _
		! form Statement Grand Totals
L1210: if tcj(j)=0 then goto L1270
		i$(1:1)="s"
		jpf$=rtrm$(jpf$)&","&i$ ! pr Stmt-Job Totals
		jaf$=rtrm$(jaf$)&",pos "&str$(pp(j))&",N "&str$(ppr(j)) : _
		! form Statement Job Totals
		upf$=rtrm$(upf$)&","""&underlin$(1:ppr(j))&""""
		uaf$=rtrm$(uaf$)&",pos "&str$(pp(j))&",C "&str$(ppr(j)) : _
		! Underline form Statement
L1270: if dp(j)=0 then goto L1340
		if fc(j)=1 then goto L1300
		af$=rtrm$(af$)&"."&str$(dp(j)) ! Add Decimal Points
L1300: if tcs(j)=0 then goto L1320
		gaf$=rtrm$(gaf$)&"."&str$(dp(j)) : _
		! Add Decimal Points-Grand Totals
L1320: if tcj(j)=0 then goto L1340
		jaf$=rtrm$(jaf$)&"."&str$(dp(j)) ! ADD DECIMAL POINTS-JOB TOTALS
L1340: i$=" "
L1350: next j
	pf$(31:31)=" "
	pf$=rtrm$(pf$)&" pageoflow 300"
	pr #11: pf$
	af$(11:11)=" "
	af$=rtrm$(af$)&",skip 0"
	pr #11: af$
	pr #11: '19911 IF FILE$(255)(1:4)<>"PRN:" THEN pr #255:'
	pr #11: "19920 mat t=t+c"
	pr #11: "19930 mat s=s+c"
	pr #11: "19940 mat c=(0)"
	pr #11: "19941 jn$="""""
	pr #11: "19942 n$="""""
	pr #11: "19945 if sd><0 then 19800"
	pr #11: "19950 IF CN$(1:6)=JN1$ and sd=0 then 19833"
	if rtrm$(gpf$(31:255))="" then goto L1560
	gpf$(31:31)=" "
	gaf$=rtrm$(gaf$)&",skip 1"
	pr #11: gpf$
	pr #11: gaf$
	goto L1570
L1560: pr #11: "20105 goto 390"
L1570: if rtrm$(jpf$(31:255))="" then goto L1700
	jpf$(31:31)=" "
	jaf$=rtrm$(jaf$)&",skip 1"
	pr #11: jpf$
	pr #11: jaf$
	upf$(30:30)=" "
	uaf$(11:11)=" "
	pr #11: upf$
	uaf$=rtrm$(uaf$)&",skip 0"
	pr #11: uaf$
	pr #11: "20030 mat s=(0)"
	pr #11: "20040 goto 19800"
	goto L1710
L1700: pr #11: "20000 goto 19800"
L1710: pr #11: "20100 snd: !"
	pr #11: "20110 pr #255: newpage"
	pr #11: "20120 gosub hdr"
	pr #11: "20160 goto 390"
	if ips=0 then goto L2130
	if ips>9 then goto L1950
	on sc goto L1880,L1790,L1820,L1850 none L1880
 
L1790: pr #11: "19814 if ";ty$(ips);">=psc(1) then 19820 else 19800"
	goto L2130
 
L1820: pr #11: "19814 if ";ty$(ips);"<=psc(1) then 19820 else 19800"
	goto L2130
 
L1850: pr #11: "19814 if ";ty$(ips);">=psc(1) and ";ty$(ips);"<=psc(2) then 19820 else 19800"
	goto L2130
 
L1880: pr #11: "19811 for j=1 to 100"
	pr #11: "19814   IF "&ty$(ips)&"= PSC(J) then 19820"
	pr #11: "19813   IF PSC(J)=0 then 19800"
	pr #11: "19815 next j"
	pr #11: "19816 goto 19800"
	goto L2130
 
L1950: on sc goto L2060,L1970,L2000,L2030 none L2060
 
L1970: pr #11: "19837 if ";ty$(ips);">=psc(1) then 19850 else 19833"
	goto L2130
 
L2000: pr #11: "19837 if ";ty$(ips);"<=psc(1) then 19850 else 19833"
	goto L2130
 
L2030: pr #11: "19837 if ";ty$(ips);">=psc(1) and ";ty$(ips);"<=psc(2) then 19850 else 19833"
	goto L2130
 
L2060: pr #11: "19837 for j=1 to 100"
	pr #11: "19839   IF ";ty$(ips);"= PSC(J) then 19850"
	pr #11: "19838   IF PSC(J)=0 then 19833"
	pr #11: "19840 next j"
	pr #11: "19841 goto 19833"
	goto L2130
 
L2130: pr #11: "Free S:\acsPR\jcPrnt"&str$(rn)&".br -n"
	pr #11: "Save S:\acsPR\jcPrnt"&str$(rn)
	pr #11: "run Menu"
	close #11:
	chain "Proc=Proc."&session$
 
include: ertn
 
Xit: fnXit
 
