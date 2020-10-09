! Replace S:\acsPR\newjcRpt-MOD
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
 
 
	if fnprocess=1 then goto L103
 
MAIN_SCREEN: !
	fnTos(sn$="namlst1") : _
	mylen=25 : mypos=mylen+2: resp=0: left=1
	fnLbl(1,1,"Report Heading Date:",23,left)
	fnTxt(1,mypos,20,0,0,"",0,"Recommended to use full alpha date format.") : _
	resp$(resp+=1)=dh$
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
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
 
PRTRPT: read #1,using L19810: jn$,n$,mat a$,x6,x7,x8,x9 eof L20100
	jn1$=jn$
	on conv goto L25010
	jn=val(jn$)
L19810: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
	if sd=2 then goto L19900
	jobcat$=jn$&"     "
	read #2,using L19831,key>=jobcat$: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 nokey PRTRPT
L19831: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2
	goto L19834
	read #2,using L19831: cn$,k$,x12,x13,x14,x15,x16,x17,x18,x19,x20,x21,x22,x23,x24 eof L25050
L19834: cn=val(cn$(7:11))
	if cn$(1:6)><jn1$ and sd=1 then goto L19900
	if cn$(1:6)><jn1$ and sd=0 then goto L20000
 
! this section of lines will be replaced during dynamics
L19900: ! this section of lines will be replaced during dynamics
L20000: ! this section of lines will be replaced during dynamics
L20100: ! this section of lines will be replaced during dynamics
L25010: ! this section of lines will be replaced during dynamics
L25050: ! this section of lines will be replaced during dynamics
 
include: ertn
 
Xit: fnXit
 
