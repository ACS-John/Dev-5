! Replace S:\acsPR\jcRpt-MOD
! JCRpt-MOD will be modified by S:\acsPR\jcRptS1 to make used designed JCPrntXX
! DO NOT RENUMBER !!!
 
	autoLibrary
	on error goto Ertn
 
	dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2),t(20),s(20),c(20)
	dim rn$*2,rt$*78,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20)
	dim fc(20),tcj(20),tcs(20),dt(125),gt(125),dh$*20,aa$*40,jn1$*6
	dim cap$*128,message$*40
 
! fnTop("S:\acsPR\jcRpt-MOD",CAP$="User Designed Reports (2)")
	fncno(cno)
	fndat(dh$)
 
 
	pr newpage
	if fnprocess=1 then goto L90
 
	fnopenwin(win=101,10,18,14,61,cap$)
	pr #win,fields "4,2,C 20,N": "Report Heading Date:"
	pr f "15,34,C 09,B,5": "Exit (F5)"
	rinput #win, fields "4,23,C 20,UT,N": dh$
	close #win: ioerr L69
L69: fndat(dh$,put=2)
	if cmdkey=5 then goto Xit
 
	fndat(dh$,2)
 
L90: pr newpage : _
	fnwait(message$="Printing: please wait...",1) : _
	pr f "15,34,C 09,B,5": "Exit (F5)" : _
	on fkey 5 goto EOF1
	fnopenprn : _
	if file$(255)(1:3)<>"PRN" then jbskip=1
 
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input  : _
	read #1,using 'Form POS 1,C 40': aa$ : _
	close #1:
 
	open #1: "Name=S:\acsPR\JCReport.MST,KFName=S:\acsPR\jcReport.Idx,Shr",internal,input,keyed
	read #1,using L170,key=rn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
L170: form pos 1,n 2,c 78,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
	close #1:
 
	h1=66-len(rtrm$(aa$))/2 : h2=66-len(rtrm$(rt$))/2 : _
	h3=66-len(rtrm$(dh$))/2 : i2=len(rtrm$(rt$))
	open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	gosub HDR
	goto PRTRPT
 
PGOF: pr #255: newpage : gosub HDR : continue
 
HDR: pr #255:
	pr #255,using L350: aa$,rtrm$(rt$),dh$
L350: form pos h1,c 40,skip 1,pos h2,c i2,skip 1,pos h3,c 20,skip 2
	pr #255: ch$(1)
	pr #255: ch$(2)
return
 
EOF1: !
	fncloseprn
	close #1:
	close #2:
	goto Xit
 
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
 
