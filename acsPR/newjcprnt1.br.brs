! Replace S:\acsPR\newJCPrnt1
! JCRpt-MOD will be modified by S:\acsPR\jcRptS1 to make used designed JCPrntXX
! DO NOT RENUMBER !!!
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
	dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
	dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,jn1$*6
	dim message$*40
 
! fnTop("S:\acsPR\jcRpt-MOD",CAP$="User Designed Reports (2)")
	fndat(dh$)
 
 
	rn$=" 1"
	if fnProcess=1 then goto L300
 
MAIN_SCREEN: !
	fnTos(sn$="jcprnt1") : _
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fnLbl(1,1,"Report Heading Date:",23,left)
	fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") : _
	resp$(resp+=1)=dh$
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	dh$=resp$(1) ! heading date
	fndat(dh$,put=2)
 
L300: fnOpenPrn
 
 
	open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",i,i,k
	read #1,using L360,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
L360: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
	close #1:
 
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",i,i,k
	open #2: "Name=[Q]\PRmstr\JCCAT.h[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",i,i,k
	gosub HDR
	goto PRTRPT
 
PgOf: pr #255: newpage : gosub HDR : continue
 
HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dh$)&"}"
	pr #255: "\ql   "
	pr #255: ch$(1)
	pr #255: ch$(2)
return ! /r
 
EOF1: !
	fnClosePrn
	close #1:
	close #2:
	fnXit
 
PRTRPT: read #1,using L660: jn$,n$,mat a$,x6,x7,x8,x9 eof SND
	jn1$=jn$
	on conv goto L1240
	jn=val(jn$)
L660: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	if sd=2 then goto L1000
	jobcat$=jn$&"     "
	read #2,using L700,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
L700: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
	goto L730
L720: read #2,using L700: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L1280
L730: cn=val(cn$(7:11))
	if cn$(1:6)><jn1$ and sd=1 then goto L1000
	if cn$(1:6)><jn1$ and sd=0 then goto L1100
	for j=1 to 100
		if psc(j)=0 then goto L720
		if x12= psc(j) then goto L820
	next j
	goto L720
 
L820: on zdiv goto L1220
	on uflow goto L1220
	on oflow goto L1220
	c(1)=c(1)+x1
	c(2)=c(2)+x2
	c(3)=c(3)+x10
	c(4)=c(4)+x11
	c(5)=c(5)+x23
	c(7)=c(7)+x21
	c(8)=c(8)+x12/x22
	c(9)=c(9)+x15/x21
	c(10)=c(10)+((x15/x21)-(x12/x22))*x21
	c(11)=c(10)/x23*100
	x6=0
	x7=0
	x8=0
	x9=0
	if sd = 1 then goto L720
L1000: pr #255, using L1010: jn$(1:8),n$(1:23),cn$(7:11),k$(1:18),c(5),c(7),c(8),c(9),c(10),c(11) pageoflow PgOf
L1010: form skip 1,pos 1,c 8,pos 9,c 23,pos 33,c 11,pos 46,c 18,pos 64,n 3,pos 79,n 8.2,pos 88,n 6.2,pos 96,n 6.2,pos 106,n 8.2,pos 119,n 8.2,skip 0
	if file$(255)(1:4)<>"PRN:" then pr #255:
	mat t=t+c
	mat s=s+c
	mat c=(0)
	jn$=""
	n$=""
	if sd><0 then goto PRTRPT
	if cn$(1:6)=jn1$ and sd=0 then goto L720
L1100: pr #255,using L1110: "________","________","________"
L1110: form skip 0,pos 79,c 8,pos 106,c 8,pos 119,c 8,skip 0
	pr #255, using L1130: s(7),s(10),s(11)
L1130: form skip 1,"Job Totals",pos 79,n 8.2,pos 106,n 8.2,pos 119,n 8.2,skip 1
	mat s=(0)
	goto PRTRPT
SND: !
	pr #255: newpage
	gosub HDR
	pr #255, using L1200: t(7),t(10),t(11)
L1200: form skip 2,"Grand Totals",pos 79,n 8.2,pos 106,n 8.2,pos 119,n 8.2,skip 1
	goto EOF1
L1220: continue
 
L1240: jn=0
	cn=0
	continue
 
L1280: cn$=""
	continue
 
 
include: ertn
 
Xit: fnXit
 
