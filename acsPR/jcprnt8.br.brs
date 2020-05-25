! Replace S:\acsPR\JCPrnt8
! newJCRpt-MOD will be modified by S:\acsPR\newjcRptS1 to make used designed JCPrntXX
! DO NOT RENUMBER !!!
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
	dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
	dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,cnam$*40,jn1$*6
	dim cap$*128,message$*40
 
	fnTop("S:\acsPR\newPrUsrDR",cap$="Print User Designed Reports (2)")
	fncno(cno,cnam$) : _
	fndat(dh$)
 
 
	rn$=" 8"
	if fnprocess=1 then goto L103
 
MAIN_SCREEN: !
	fnTos(sn$="namlst1") : _
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fnLbl(1,1,"Report Heading Date:",23,left)
	fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") : _
	resp$(resp+=1)=dh$
	fnCmdSet(2)
	fnAcs2(mat resp$,ck)
	if ck=5 then goto Xit
	dat$=dh$=resp$(1) ! heading date
	close #win: ioerr L69
L69: fndat(dh$,put=2)
 
	fndat(dh$,2)
 
L103: fnopenprn
 
	open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",internal,input,keyed
	read #1,using L170,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
L170: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
	close #1:
 
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	gosub HDR
	goto PRTRPT
 
PGOF: pr #255: newpage : gosub HDR : continue
 
HDR: !
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&trim$(rt$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dh$)&"}"
! pr #255: "\qc  {\f181 \fs16 \b "&TRIM$(D$)&"}"
	pr #255: "\ql   "
	pr #255: ch$(1)
	pr #255: ch$(2)
return
 
EOF1: !
	fncloseprn
	close #1:
	close #2:
	fnXit
 
PRTRPT: read #1,using L19810: jn$,n$,mat a$,x6,x7,x8,x9 eof SND
	jn1$=jn$
	on conv goto L25010
	jn=val(jn$)
L19810: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	if sd=2 then goto L19900
	jobcat$=jn$&"     "
	read #2,using L19831,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
L19831: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
	goto L19834
L19833: read #2,using L19831: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L25050
L19834: cn=val(cn$(7:11))
	if cn$(1:6)><jn1$ and sd=1 then goto L19900
	if cn$(1:6)><jn1$ and sd=0 then goto L20000
 
	on zdiv goto L25000
	on uflow goto L25000
	on oflow goto L25000
	c(1)=c(1)+x1
	c(2)=c(2)+x2
	c(3)=c(3)+x12+x14
	c(4)=c(4)+((x12*x23)+(x14*x24))/100
	c(5)=(c(4)/c(3))*100
	c(6)=c(6)+x12+x14
	c(7)=c(7)+x12*(x23/100)+x14*(x24/100)
	c(8)=c(8)+x15+x17
	c(9)=c(9)+max((x15/(x23/100))-x15,(x12-x15))
	c(10)=c(10)+max((x17/(x24/100))-x17,(x14-x17))
	c(11)=c(9)+c(10)
	c(12)=c(12)+x7
	c(13)=c(13)+x8
	c(14)=c(14)+x7-x8
	c(15)=c(12)-(c(8)+c(11))
	x6=0
	x7=0
	x8=0
	x9=0
	if sd = 1 then goto L19833
L19900: pr #255, using L19910: jn$(1:6),n$(1:25),c(5),c(6),c(7),c(8),c(11),c(12),c(13),c(14),c(15) pageoflow PGOF
L19910: form skip 1,pos 1,c 6,pos 10,c 25,pos 35,n 4,pos 40,n 10.2,pos 50,n 10.2,pos 60,n 10.2,pos 70,n 10.2,pos 80,n 10.2,pos 90,n 10.2,pos 100,n 10.2,pos 110,n 10.2,skip 0
	if file$(255)(1:4)<>"PRN:" then pr #255:
	mat t=t+c
	mat s=s+c
	mat c=(0)
	jn$=""
	n$=""
	if sd><0 then goto PRTRPT
	if cn$(1:6)=jn1$ and sd=0 then goto L19833
L20000: pr #255,using L20020: "__________","__________","__________","__________","__________","__________","__________","__________"
L20020: form skip 0,pos 40,c 10,pos 50,c 10,pos 60,c 10,pos 70,c 10,pos 80,c 10,pos 90,c 10,pos 100,c 10,pos 110,c 10,skip 0
	pr #255, using L20026: s(6),s(7),s(8),s(11),s(12),s(13),s(14),s(15)
L20026: form skip 1,"Job Totals",pos 40,n 10.2,pos 50,n 10.2,pos 60,n 10.2,pos 70,n 10.2,pos 80,n 10.2,pos 90,n 10.2,pos 100,n 10.2,pos 110,n 10.2,skip 1
	mat s=(0)
	goto PRTRPT
SND: !
	pr #255: newpage
	gosub HDR
	pr #255, using L20150: t(6),t(7),t(8),t(11),t(12),t(13),t(14),t(15)
L20150: form skip 2,"Grand Totals",pos 40,n 10.2,pos 50,n 10.2,pos 60,n 10.2,pos 70,n 10.2,pos 80,n 10.2,pos 90,n 10.2,pos 100,n 10.2,pos 110,n 10.2,skip 1
	goto EOF1
L25000: continue
 
L25010: jn=0
	cn=0
	continue
 
L25050: cn$=""
	continue
 
 
include: Ertn
 
Xit: fnXit
 
