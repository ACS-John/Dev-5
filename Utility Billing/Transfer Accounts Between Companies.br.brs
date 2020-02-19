! formerly S:\acsUB\ubCoTr
! -- Transfer Accounts Between Companies
!
	library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnTos,fnopenprn,fncloseprn,fnerror,fncmbact,fnxit,fnCmdSet,fntop,fncmbcno,fnindex_sys,fngethandle
	on error goto Ertn
!
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),ta(2),alp$*7
	dim ubstd$*200,ubextra$*100,mstrform$*300,extra(23),extra$(11)*30
	dim rw4(22,13),ab$(4)*30,cap$*128
	dim df$*256
	dim rm$*60,rm$(20)*60,ra(2),resp$(10)*50,tg(11)
!
	cno=val(env$('cno'))
	fntop(program$)
	ubstd$="Form Pos 1,C 10,4*C 30,C 12,7*PD 2,11*PD 4.2,4*PD 4,15*PD 5,PD 4.2,PD 4,12*PD 4.2,2*PD 3,C 7,2*C 12,PD 3,10*PD 5.2,78*PD 5,13*PD 4.2,13*N 6,156*PD 4.2,13*N 6,13*PD 4.2,C 1,C 9,C 2,C 17"
	ubextra$=",n 2,n 7,n 6,n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30"
	mstrform$=rtrm$(ubstd$)&rtrm$(ubextra$)
MENU1: ! 
	fnTos(sn$="CoTr-1")
	mylen=5 : mypos=mylen+2
	fnLbl(1,1,"From:",mylen,1)
	fnTxt(1,mypos+.4,50, 0,0,'',1) ! fncmbcno(1,mypos)
	resp$(1)=env$('cnam')&' ([cno])'
	fnLbl(2,1,"To:",mylen,1)
	fncmbcno(2,mypos)
	resp$(2)=''
	fnLbl(4,10,"(Both companies must be set up in advance)",49,0)
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto XIT
	co1=cno ! val(resp$(1)(43:47))
	co2=val(resp$(2)(43:47))
	if co1=0 or co2=0 then goto MENU1
	close #1: ioerr ignore
	open #1: "Name=[Q]\UBmstr\Customer.h"&str$(co1)&",Shr,KFName=[Q]\UBmstr\UBIndex.h"&str$(co1)&",Shr",internal,outIn,keyed  ! Ioerr MENU1
	close #2: ioerr ignore
	open #2: "Name=[Q]\UBmstr\ubTransVB.h"&str$(co1)&",Shr,KFName=[Q]\UBmstr\ubTrIndx.h"&str$(co1)&",Shr",internal,outIn,keyed 
	close #3: ioerr ignore
	open #3: "Name=[Q]\UBmstr\UBADRBIL.h"&str$(co1)&",Shr,KFName=[Q]\UBmstr\AdrIndex.h"&str$(co1)&",Shr",internal,outIn,keyed 
	close #41: ioerr ignore
	open #41: "Name=[Q]\UBmstr\DEPOSIT1.h"&str$(co1)&",Shr,KFName=[Q]\UBmstr\DEPIDX1.h"&str$(co1)&",Shr,USE,RecL=16,KPS=1,KLN=10",internal,outIn,keyed 
	close #42: ioerr ignore
	open #42: 'Name=[Q]\UBmstr\Deposit2.h[cno],KFName=[Q]\UBmstr\Deposit2Index.h[cno],Shr,Use,RecL=73,KPs=1,KLn=10',internal,outIn,keyed ! "Name=[Q]\UBmstr\DEPOSIT2.h"&str$(co1)&",Shr,USE,RecL=73",internal,outIn,relative 
	close #26: ioerr ignore
	open #26: "Name=[Q]\UBmstr\Customer.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\UBIndex.h"&str$(co2)&",Shr",internal,outIn,keyed  ! Ioerr MENU1
	open #11: "Name=[Q]\UBmstr\Customer.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\UBIndx2.h"&str$(co2)&",Shr",internal,outIn,keyed 
	open #unused0:=fngethandle: "Name=[Q]\UBmstr\Customer.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\UBIndx3.h"&str$(co2)&",Shr",internal,outIn,keyed 
	open #unused1:=fngethandle: "Name=[Q]\UBmstr\Customer.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\UBIndx4.h"&str$(co2)&",Shr",internal,outIn,keyed 
	open #unused2:=fngethandle: "Name=[Q]\UBmstr\Customer.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\UBIndx5.h"&str$(co2)&",Shr",internal,outIn,keyed 
	open #hUbTranVb:=fngethandle: "Name=[Q]\UBmstr\ubTransVB.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\ubTrIndx.h"&str$(co2)&",Shr",internal,outIn,keyed 
	close #23: ioerr ignore
	open #23: "Name=[Q]\UBmstr\UBADRBIL.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\AdrIndex.h"&str$(co2)&",Shr",internal,outIn,keyed  ! Ioerr MENU1
	close #51: ioerr ignore
	open #51: "Name=[Q]\UBmstr\Deposit1.h"&str$(co2)&",Shr,KFName=[Q]\UBmstr\DepIdx1.h"&str$(co2)&",Shr,Use,RecL=16,KPs=1,KLn=10",internal,outIn,keyed ioerr MENU1
	close #52: ioerr ignore
	open #52: "Name=[Q]\UBmstr\Deposit2.h"&str$(co2)&",Shr,USE,RecL=73",internal,outIn,relative 
	fnopenprn
	gosub HDR
MENU2: ! 
	hcno=cno
L700: fnTos(sn$="CoTr-2")
	fnLbl(1,1,"Customer to Transfer:",28,1)
	fncmbact(1,30)
	resp$(1)=""
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto DONE
	z$=lpad$(trim$(resp$(1)(1:10)),10)
	read #1,using mstrform$,key=z$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat ta,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$ nokey L700
	z2$=z$
L820: read #26,using "Form POS 1,C 10",key=z2$: z2$ nokey L960
MENU3: ! 
	sn$="CoTr-3"
	fnTos(sn$)
	mylen=28
	mypos=mylen+2
	fnLbl(3,1,"New Account:",mylen,1)
	fnTxt(3,30,10)
	resp$(1)=z2$
	fnLbl(1,1,"Account "&z2$&" already exists!",0,2)
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto MENU2
	z2=val(resp$(1)) conv MENU3
	if z2=0 then goto MENU2
	z2$=cnvrt$("N 10.2",z2)
	goto L820
!
L960: restore #2,key>=z$&"         ": nokey L1040
L970: read #2,using L980: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L1040
L980: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>z$ then goto L1040
	write #hUbTranVb,using L980: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
	delete #2: 
	goto L970
!
L1040: gosub ALTBILLADDR
	write #26,using mstrform$: z2$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat ta,alp$,f$(2),f$(3),bra,mat gb,mat rw4,df$,dr$,dc$,da$,mat extra,mat extra$
	pr #255,using 'form pos 1,c 10,pos 15,c 30,pos 50,n 10.2': z$,e$(2),bal pageoflow PGOF
	delete #1,key=z$: 
	goto MENU2
!
	close #1: 
	close #2: 
	close #3: 
	close #26: 
	close #hUbTranVb: 
	close #23: 
FINIS: ! r:
	! close #31: 
	! close #61: 
	! close #32: 
	fncloseprn
	goto DONE
! /r
ALTBILLADDR: ! r: alternate billing address
	read #3,using "Form POS 1,C 10,4*C 30",key=z$: z$,mat ab$ nokey L1440
	write #23,using "Form POS 1,C 10,4*C 30": z2$,mat ab$
	L1440: !
return  ! /r
! def fn_moveKeyPartialMatches(hFrom,hTo,MatchPos,MatchLen,matchType$)
!   
! fnend
DONE: ! r:
	close #1: ioerr ignore
	close #2: ioerr ignore
	close #3: ioerr ignore
	! close #31: ioerr ignore
	close #41: ioerr ignore
	close #42: ioerr ignore
	close #26: ioerr ignore
	close #11: ioerr ignore
	close #unused0: ioerr ignore
	close #unused1: ioerr ignore
	close #unused2: ioerr ignore
	close #hUbTranVb: ioerr ignore
	close #23: ioerr ignore
	close #51: ioerr ignore
	close #52: ioerr ignore
	fnindex_sys(co1)
	fnindex_sys(co2)
XIT: fnxit ! /r
HDR: ! r:
	pr #255,using "Form POS 1,Cc 80": "Accounts Transferred from Company Number "&str$(co1)&" to Company Number "&str$(co2)
	pr #255,using "Form POS 5,CC 70": date$
	pr #255: ""
	pr #255,using "Form POS 2,C 9,POS 15,C 4,POS 53,C 7": "Act. Num.","Name","Balance"
return  ! /r
IGNORE: continue 
PGOF: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r
include: Ertn
