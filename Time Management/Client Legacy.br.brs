! formerly S:\acsTM\ClMaint
	on error goto ERTN
! 
	library "S:\Core\search.br": fnsearch
	library "S:\Core\Library.br": fnerror,fnmsgbox,fnwin3b
	library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnCmdSet,fnAcs,fnagl$
	fntop(program$)
! 
! 
	dim ar(5),ph2$*12,ss2$*11,arta(2),des$*30,cm$*70
	dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10),ca(10),ta(25,2),fb(25)
	dim z$(3)*5,ph$(6)*12,ss$(6)*11,h$(3,6)*30,l(3,10),m(3,10),pno(3),mye(3),har(3,5)
	dim fl1$(33),oi1$(38),scr1$(31)*20,scrid$(2)*40
	dim k$*5,d$*9,b(8),sc$*4,iv$*12,fl2$(15),scr2$(14)*25,ot2$(14),in2$(14)
	dim flit$(6)*18,scrt$(6)*20,scrz$(2)*79,desc$(8)*18
	dim hlp$(20)*78,flh$(22)*18,hhd$*60
	dim p$*5,iv$*12,tr(6),id$*20 ,cat$(10)*30
	dim st$(20)*24,scot$(21),sct$(40),app(20),ma(20)
	dim st2$(20)*24,ap2(20),ma2(20)
	dim wrd1$(10)*33
	open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",internal,input
	read #1,using L250: mat scr1$,mat fl1$,mat oi1$,mat scr2$,mat fl2$,mat ot2$,mat in2$,mat flh$,mat flit$,mat desc$,mat scrt$
L250: form pos 1,31*c 20,71*c 18,14*c 25,79*c 18,6*c 20
	close #1: 
	scrt$(5)=scrt$(6)=""
	for j=22 to 31: scr1$(j)=rtrm$(scr1$(j)): next j
	gosub GetMatStAndSt2
	open #hClient:=1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],Shr,KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed ioerr L4160
	open #11: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],Shr,KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,outIn,keyed ioerr L4160
	open #2: "Name=S:\Core\Data\acsllc\TMTrans.h[cno],Shr",internal,outIn,relative 
	open #3: "Name=S:\Core\Data\acsllc\TMTRAddr.h[cno],Shr",internal,input,relative 
	open #4: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative 
	open #5: "Name=S:\acsTM\CLMst.Hlp,Shr",internal,outIn,relative
	open #6: "Name=S:\Core\Data\acsllc\TMCat.h[cno],Shr",internal,outIn,relative ioerr L380
	goto L390
L380: chain "S:\acsTM\CTMAINT"
L390: read #6,using L400,rec=1: mat cat$ 
L400: form pos 1,10*c 30
	close #6: 
L420: pr newpage
	pr f "3,10,Cc 60,R,N": "Client File"
	pr f "4,10,Cc 60,R,N": "Company [cno] - "&ltrm$(env$('cnam'))
	wrd1$(1)="1. Initial File Preparation" 
	wrd1$(2)="2. Add " 
	wrd1$(3)="3. Edit or View" 
	wrd1$(4)="4. pr Proof List" 
	wrd1$(5)="5. Reassign Transaction Addresses" 
	wrd1$(6)="6. pr Directory" 
	wrd1$(7)="7.       Listing" 
	wrd1$(8)="8.       Labels" 
	wrd1$(9)="9. Reset Status Codes" 
	wrd1$(10)="" ! was 10. search by name
	for j=1 to 10 
		io1$(j)=str$(j+5)&",9,C 33,N" 
	next j 
	io1$(3)="8,9,C 33,C,N"
	pr f "17,9,C 60,B,99": "Exit (Esc or  F5)"
L480: rinput select mat io1$,attr "H": mat wrd1$
	ti=curfld
	if cmdkey=5 or cmdkey=99 then let fnxit
L510: on ti goto L420,L580,L630,L2130,L2740,L530,L540,LABELS,L560,L570 none L480
L520: chain "S:\acsTM\TMBLDCL"
L530: if new1=1 then goto L1970 else chain "S:\acsTM\TMCLIDIR"
L540: if new1=1 then goto L1970 else chain "S:\acsTM\TMCLILST"
LABELS: if new1=1 then goto L1970 else chain "S:\acsTM\CLILabel.wb"
L560: if new1=1 then goto L1970 else chain "S:\acsTM\CLRSETST"
L570: if new1=1 then goto L1970 else chain "S:\acsTM\TMNAMINQ"
L580: scrid$(1)="  TIME MANAGEMENT ADD CLIENTS"
	scrid$(2)="  (ENTER CLIENT # AS 0 WHEN COMPLETED)"
	mat ca=(0)
	mat arta=(0)
	goto ToL760
L630: scrid$(1)="  TIME MANAGEMENT MAINTAIN CLIENTS"
	scrid$(2)="  (ENTER CLIENT # AS 0 TO DELETE)"
L650: pr newpage
	pr f "10,30,C 14,N": "Client Number:"
	pr f "22,28,c 40": "Cancel F5    Search  F6"
L680: input fields "10,45,Nz 5,UT,N": ano conv L680
	if cmdkey=5 then goto L420
	if cmdkey=6 then goto TMSRCH
L700: if ano=0 or cmdkey=5 or cmdkey=99 then goto L420
	z$=lpad$(str$(ano),5)
	read #hClient,using L730,key=z$: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 nokey L650
	L730: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
	hbal=ar(1): chgbal=0
	d$=z$
ToL760: hsr=1: pr newpage
	pr f mat fl1$: mat scr1$,mat scrid$
	if ti=3 then pr f mat oi1$: val(z$),mat a$,ph$,ss$,pno,mye,ph2$,ss2$,mat ar,cm$,mat dd,mat sc
	pr f "24,30,C 40,N": "F1 Save  F5 Cancel F6=HELP"
L800: input fields mat oi1$: x1,mat a$,ph$,ss$,pno,mye,ph2$,ss2$,mat ar,cm$,mat dd,mat sc conv L2980
	if cmdkey=5 then goto L650
	if cv>0 then oi1$(cv)(cv1:cv2)="U": cv=0
	if cmdkey=6 then goto L3880
	if ti=2 and x1=0 then goto L420
	z$=lpad$(str$(x1),5)
	if x1=0 then goto L1100
	if mye<1 or mye>12 then cv=10: goto L3000
	if ar(5)<1 or ar(5)>2 then cv=17: goto L3000
	for j=1 to 10
		if dd(j)=0 then goto L930
		if dd(j)>0 and dd(j)<5 then goto L930
		if dd(j)<101 or dd(j)>1231 then cv=18+j: goto L3000
L930: if sc(j)<0 or sc(j)>2 then cv=28+j: goto L3000
	next j
	if ti=3 and hbal<>ar(1) then chgbal=1 else chgbal=0
	if ti=2 then goto L980
	if x1=ano then goto L1020
L980: read #hClient,using L990,key=z$: z$ nokey L1020
L990: form pos 1,c 5
	cv=1: goto L3000
	goto L800
L1020: pr newpage
! if cno<> 500 then goto 1080
	pr f mat scot$: mat st$,"ACCOUNT "&z$&"     APPLICATIONS  MAINTENANCE"
	if ti=3 then pr f mat sct$: mat app,mat ma
L1060: input fields mat sct$,attr "r": mat app,mat ma conv L1060
	gosub CAT2
	if ti=2 then goto L1210
	if x1=ano then goto L1620
L1100: pr newpage
	pr f "10,10,c 43,n": "ACCOUNT # "&d$&" WILL BE DELETED"
	pr f "11,10,c 43,n": "ENTER 1 TO DELETE, ENTER 2 TO RE-ENTER"
L1130: input fields "11,60,n 1,eu,n": delact conv L1130
	if delact=2 then goto L510
	if delact><1 then goto L1100
	delete #hClient,key=d$: 
	z$=""
	gosub L1390
	new1=1
	goto L510
L1210: if ti=2 then goto L1230 else delete #hClient,key=d$: nokey L1230
	gosub L1390
L1230: write #hClient,using L730: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2
	new1=1
	if ti<>2 or ar(1)=0 then goto L1380
	if ar(1)<0 then tr(5)=4 else tr(5)=1
	tr(1)=val(date$(4:5)&date$(7:8)&date$(1:2))
	tr(2)=abs(ar(1))
	tr(3)=tr(2)
	id$="BEGINNING BALANCE"
	nta=0
	rec4=lrec(4)+1
	write #4,using L3420,rec=rec4: z$,iv$,mat tr,id$,nta
	rewrite #4,using L1350,rec=1: rec4
L1350: form pos 58,pd 3
	mat arta=(rec4)
	rewrite #hClient,using L2930,key=z$: mat arta
L1380: goto L510
L1390: ! CHANGE TRANS
	if x1=ano then goto L1610
	for j=1 to 10
		if ca(j)=0 then goto L1540
		read #3,using L1750,rec=ca(j): mat ta noRec L1540
		for j1=1 to 25
			rec2=ta(j1,1)
L1460: if rec2=0 then goto L1530
			read #2,using L1480,rec=rec2: nta noRec L1530
L1480: form pos 54,pd 3
			rewrite #2,using L1500,rec=rec2: z$
L1500: form pos 1,c 5
			rec2=nta
			goto L1460
L1530: next j1
L1540: next j
	rec4=arta(1)
L1560: if rec4=0 then goto L1610
	read #4,using L1350,rec=rec4: nta
	rewrite #4,using L1500,rec=rec4: z$
	rec4=nta
	goto L1560
L1610: return 
L1620: rewrite #hClient,using L730,key=z$: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2
	if sum(ca)=0 then goto L3320
	pr newpage
	pr f "10,10,c 60,n": "ENTER 1 TO REVIEW CATEGORY TRANSACTIONS; ELSE ENTER 0"
L1660: input fields "10,65,N 1,UE,N": det conv L1660
	on det+1 goto L3320,L1680 none L1660
L1680: pr newpage
	pr f "10,10,c 52": "Enter Category; Enter 0 when completed"
L1700: input fields "10,55,N 2,UE,N": det conv L1700
	if det=0 then goto L3320
	if det<1 or det>10 then goto L1700
	if ca(det)=0 then goto L1680
	read #3,using L1750,rec=ca(det): mat ta,mat fb noRec L1680
L1750: form pos 1,50*pd 3,25*n 1
L1760: pr newpage
	pr f "10,10,c 52": "Enter Sub-Category; Enter 99 when completed"
L1780: input fields "10,55,N 2,UE,N": det conv L1780
	if det=99 then goto L1680
	if det=0 then det=25
	if det<1 or det>25 then goto L1780
	if ta(det,1)=0 then goto L1760
	tadr=ta(det,1)
L1840: read #2,using L1850,rec=tadr: k$,e$,mat b,sc$,iv$,nta,des$
L1850: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
	pr newpage
	if fb(det)=1 then pr f "2,27,c 20": "PARTIAL BILLED"
	if fb(det)=2 then pr f "2,27,c 20": "FINAL BILLED"
	pr f mat fl2$: mat scr2$,a$(1)
	pr f mat ot2$: k$,e$,mat b,sc$,iv$,nta,des$
L1910: input fields mat ot2$: k$,e$,mat b,sc$,iv$,qta,des$ conv L3140
	iv$=lpad$(rtrm$(iv$),12)
	rewrite #2,using L1850,rec=tadr: k$,e$,mat b,sc$,iv$,nta,des$
	if nta=0 then goto L1760
	tadr=nta
	goto L1840
	L1970: !
	close #hClient: 
	close #2: 
	close #3: 
	close #4: 
	close #5: 
	close #11: 
	if cmdkey=5 then let fnxit
	execute "Index S:\Core\Data\acsllc\CLmstr.h[cno]"&' '&"S:\Core\Data\acsllc\CLIndex.h[cno] 1 5 REPLACE DupKeys -n"
	execute "Index S:\Core\Data\acsllc\CLmstr.h[cno]"&' '&"S:\Core\Data\acsllc\CLIndx2.h[cno] 6 30 REPLACE DupKeys -n"
	if ti=6 then chain "S:\acsTM\TMCLIDIR"
	if ti=7 then chain "S:\acsTM\TMCLILST"
	if ti=8 then chain "S:\acsTM\CLILABEL"
	if ti=9 then chain "S:\acsTM\CLRSETST"
	if ti=10 then chain "S:\acsTM\TMNAMINQ"
PROOF_LIST: ! 
	fnopenprn
	L2130: !
	restore #hClient,key>="     ": 
	on fkey 5 goto L2330
	pr newpage 
	pr f "10,15,C 50,N": "CLIENT PROOF LISTING IN PROCESS" 
	pr f "12,30,Cc 20,B,5": "Cancel (F5)"
	namtab=66-int(len(rtrm$(env$('cnam')))/2)
	r=1
L2180: read #hClient,using L730: z$(r),mat a$,ph$(r),ss$(r),pno(r),mye(r),mat dd,mat sc,mat ca,ph$(r+3),ss$(r+3),mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 eof L2300
	for j=1 to 10
		if j>5 then goto L2230
		har(r,j)=ar(j)
		h$(r,j)=a$(j)
L2230: l(r,j)=dd(j)
		m(r,j)=sc(j)
	next j
	r+=1
	if r<4 then goto L2180
	gosub L2350
	goto L2180
L2300: if r>1 then gosub L2350
	fncloseprn
	goto L420
L2330: pr #255: newpage
	goto L420
L2350: gosub L2680
	pr #255,using L2370: "CLIENT NUMBER",z$(1),z$(2),z$(3)
L2370: form pos 1,c 25,3*c 35,skip 1
	for j=1 to 5
		pr #255,using L2370: rtrm$(scr1$(j+1)),h$(1,j),h$(2,j),h$(3,j)
	next j
	pr #255,using L2370: "BUSINESS PHONE #",ph$(1),ph$(2),ph$(3)
	pr #255,using L2370: "FEDERAL ID # OR SS #",ss$(1),ss$(2),ss$(3)
	pr #255,using L2450: "PARTNER NUMBER",pno(1),pno(2),pno(3)
	pr #255,using L2450: "MONTH OF YEAR-END",mye(1),mye(2),mye(3)
L2450: form pos 1,c 25,n 12,pos 64,n 12,pos 102,n 12,skip 1
	pr #255,using L2370: "FAX PHONE #",ph$(4),ph$(5),ph$(6)
	pr #255,using L2370: "SPOUCE SS #",ss$(4),ss$(5),ss$(6)
	for j=1 to 5
		pr #255,using L2500: scr1$(j+12),har(1,j),har(2,j),har(3,j)
L2500: form pos 1,c 25,n 12.2,pos 64,n 12.2,pos 102,n 12.2,skip 1
	next j
	for j=1 to 10
		pr #255,using L2450: "DUE DATE-"&cat$(j)(1:15),l(1,j),l(2,j),l(3,j)
		pr #255,using L2450: "STATUS CODE-"&cat$(j)(1:12),m(1,j),m(2,j),m(3,j)
	next j
	pr #255: newpage
	mat z$=(" ")
	mat h$=(" ")
	mat ph$=(" ")
	mat ss$=(" ")
	mat pno=(0)
	mat mye=(0)
	mat l=(0)
	mat m=(0)
	r=1
	mat har=(0)
return 
L2680: ! r:
	p1=p1+1
	pr #255,using L2700: date$,env$('cnam'),"PAGE",p1
	L2700: form skip 3,pos 1,c 8,pos namtab,c 40,pos 100,c 5,n 4,skip 1
	pr #255,using L2720: time$,"TIME MANAGEMENT CLIENT FILE PROOF LIST"
	L2720: form pos 1,c 8,pos 48,c 38,skip 2
return ! /r
L2740: ! r:
	pr newpage
	pr f "10,15,c 60,h,n": "REASSIGN TRANSACTION ADDRESSES IN PROCESS"
	restore #1,key>="     ": 
	do
		read #1,using L2780: mat ca,mat arta eof L2830
		L2780: form pos 230,10*pd 3,pos 299,2*pd 3
		mat ca=(0)
		mat arta=(0)
		rewrite #1,using L2780: mat ca,mat arta
	loop
	L2830: !
	lr4=lrec(4)
	rewrite #4,using L2950,rec=1: lr4
	for j=1 to lr4
		read #4,using L2870,rec=j: z$,nta noRec L2960
		L2870: form pos 1,c 5,pos 58,pd 3
		read #1,using L2930,key=z$: mat arta nokey L2960
		if arta(1)=0 then arta(1)=j
		if arta(2)>0 then rewrite #4,using L2950,rec=arta(2): j
		arta(2)=j
		rewrite #1,using L2930: mat arta
		L2930: form pos 299,2*pd 3
		rewrite #4,using L2950,rec=j: 0
		L2950: form pos 58,pd 3
	L2960: !
	next j
	chain "S:\acsTM\TMFIXADR"
	L2980: !
	if cv>0 then oi1$(cv)(cv1:cv2)="U"
	cv=cnt+1
	L3000: !
	pr f "24,78,C 1": bell
	L3010: !
	oi1$(cv)=rtrm$(oi1$(cv))
	cv1=pos(uprc$(oi1$(cv)),"U",1)
	cv2=cv1+1
	oi1$(cv)(cv1:cv1)="CR"
	if chgbal=1 then goto ToL760 else goto L800
	L3060: !
	if cv>0 then flit$(cv)(cv1:cv2)="U"
	cv=cnt+1
	L3080: !
	pr f "24,78,C 1": bell
	flit$(cv)=rtrm$(flit$(cv))
	cv1=pos(uprc$(flit$(cv)),"U",1)
	cv2=cv1+1
	flit$(cv)(cv1:cv1)="RC"
	goto ToL3630
	L3140: !
	if cv>0 then ot2$(cv)(cv1:cv2)="U"
	cv=cnt+1
	pr f "24,78,C 1": bell
	ot2$(cv)=rtrm$(ot2$(cv))
	cv1=pos(uprc$(ot2$(cv)),"U",1)
	cv2=cv1+1
	ot2$(cv)(cv1:cv1)="RC"
goto L1910 ! /r

	L3220: !
	pr newpage
	pr f "8,10,C 60,H,N": "CURRENT BALANCE DOES NOT AGREE WITH TOTAL TRANSACTIONS"
	pr f "10,21,C 78": "CURRENT BALANCE = "
	pr f "10,40,N 12.2,U,N": ar(1)
	pr f "11,18,C 60": "TOTAL TRANSACTIONS = "
	pr f "11,40,N 12.2,U,N": tt
	pr f "13,5,C 70": "ENTER 1 TO CORRECT CURRENT BALANCE; ENTER 2 TO CORRECT TRANSACTIONS"
	L3290: !
	input fields "15,37,n 1,eu,n": chgbal conv L3290
	if chgbal=1 then cv=13: goto L3010
	if chgbal=2 then goto L3380 else goto L3290 ! 9/21/87
	L3320: !
	if chgbal>0 then goto L3380
	if arta(1)=0 then goto L510
	pr newpage
	pr f "10,5,C 61": "ENTER 1 TO REVIEW A/R TRANSACTIONS OR 0 TO CONTINUE:"
	L3360: !
	input fields "10,60,N 1,UE,N": det conv L3360
	on det+1 goto L510,L3380 none L3360
	L3380: !
	tt=0
	rec4=arta(1)
	if arta(1)=0 then goto L3760
	L3410: !
	read #4,using L3420,rec=rec4: z$,iv$,mat tr,id$,nta
	L3420: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
	scrz$(1)="ACCOUNT #"&z$&"   BALANCE ="&str$(ar(1))&"   TOTAL TRANSACTIONS ="&str$(tt)
	scrz$(2)=" "
	if tr(5)=1 then
		tp$="INVOICE"
	else if tr(5)=2 then
		tp$="FINANCE CHARGE"
	else if tr(5)=3 then
		tp$="STANDARD CHARGE"
	else if tr(5)=4 then
		tp$="COLLECTION"
	else if tr(5)=5 then
		tp$="DEBIT MEMO"
	else if tr(5)=6 then
		tp$="CREDIT MEMO"
	else
		tp$=""
	end if
ToL3580: !
	hsr=2: pr newpage
	pr f mat desc$: mat scrz$,mat scrt$
	pr f "24,2,C 78,N": "F6=HELP"
	pr f mat flit$: iv$,tr(1),tr(3),id$
	pr f "6,40,C 15,H,N": tp$
	ToL3630: !
	input fields mat flit$,attr "R": iv$,tr(1),tr(3),id$ conv L3060
	if cv>0 then flit$(cv)(cv1:cv2)="U": cv=0
	if cmdkey=6 then goto L3880
	if tr(1)<10100 or tr(1)>123199 then cv=2: goto L3080
	if tr(5)=4 or tr(5)=6 then goto L3700
	tt=tt+tr(3)
	goto L3710
	L3700: !
	tt=tt-tr(3)
	L3710: !
	iv$=lpad$(rtrm$(iv$),12)
	rewrite #4,using L3420,rec=rec4: z$,iv$,mat tr,id$,nta
	if nta=0 then goto L3760
	rec4=nta
	goto L3410
	L3760: !
	if tt><ar(1) then goto L3220
	goto L510
	rec4=arta(1)
	if arta(1)=0 then goto L3870
	L3800: !
	read #4,using L3820,rec=rec4: amt,nta
	L3820: form pos 29,pd 5.2,pos 58,pd 3
	rewrite #4,using L3830,rec=rec4: "",0,nta
	L3830: form pos 1,c 5,pos 29,pd 5.2,pos 58,pd 3
	if nta=0 then goto L3870
	rec4=nta
	goto L3800
L3870: !
return 
L3880: ! r:
	pr newpage
	cv=currow
	col=curcol
	if cv<1 or cv>24 then cv=0: goto L4110
	on hsr goto L3930,L4010
	L3930: ! r:
		cv=cv-1
		oi1$(cv)=rtrm$(oi1$(cv))
		cv1=pos(uprc$(oi1$(cv)),"U",1)
		cv2=cv1+1
		oi1$(cv)(cv1:cv1)="UC"
		hhd$=scr1$(cv)
		hr=cv
	goto L4080 ! /r
	L4010: ! r:
		cv=cv/2-2
		flit$(cv)=rtrm$(flit$(cv))
		cv1=pos(uprc$(flit$(cv)),"U",1)
		cv2=cv1+1
		flit$(cv)(cv1:cv1)="UC"
		hhd$=scrt$(cv)
		hr=cv+24
	goto L4080 ! /r
	L4080: ! 
	read #5,using L4090,rec=hr: mat hlp$ noRec L4150
	L4090: form pos 1,20*c 78
	pr f mat flh$: mat hlp$,hhd$,"ENTER 0 TO CONTINUE OR 1 TO UPDATE HELP SCREEN:"
	L4110: !
	input fields "24,69,N 1,EU,N": j2 conv L4110
	if j2<>1 then goto L4150
	input fields mat flh$: mat hlp$
	rewrite #5,using L4090,rec=hr: mat hlp$
	L4150: !
on hsr goto ToL760,ToL3580 ! /r

XIT: fnxit
GetMatStAndSt2: ! r:
	data " 1. GENERAL LEDGER"
	data " 2. ACCOUNTS RECEIVABLE"
	data " 3. ACCOUNTS PAYABLE"
	data " 4. Utility Billing"
	data " 5. PATIENT BILLING"
	data " 6. PROPERTY TAX"
	data " 7. HOSPITAL A/R"
	data " 8. FIXED ASSET"
	data " 9. TIME MANAGEMENT"
	data "10. CASH REGISTER"
	data "11. POINT OF SALE"
	data "12. INVOICING"
	data "13. INVENTORY"
	data "14. PAYROLL"
	data "15. PURCHASE ORDER"
	data "16. MUNICIPAL COURT"
	data "17. PCAnywhere"
	data "18. Checkbook"
	data "19. HARDWARE"
	data "20. OTHER"
	read mat st$
	for j=1 to 20
		scot$(j)=str$(j+2)&",2,C 24"
		sct$(j)=str$(j+2)&",27,N 1,Ut,N"
		sct$(j+20)=str$(j+2)&",35,N 6.2,Ut,N"
	next j
	scot$(21)="1,2,C 60,H,N"
	data "21. JOB COST"
	data "22. BUSINESS LICENSE"
	data "23. BUDGET MANAGEMENT"
	data "24. GAS AND DIESEL"
	data "25. ENERGY ASSISTANCE"
	data "26."
	data "27."
	data "28."
	data "29."
	data "30."
	data "31."
	data "32."
	data "33."
	data "34."
	data "35."
	data "36."
	data "37."
	data "38."
	data "39."
	data "40."
	read mat st2$
return ! /r
CAT2: ! r:
	pr newpage
	pr f mat scot$: mat st2$,"ACCOUNT "&z$&"     APPLICATIONS  MAINTENANCE"
	if ti=3 then pr f mat sct$: mat ap2,mat ma2
	L4820: input fields mat sct$,attr "R": mat ap2,mat ma2 conv L4820
return ! /r
TMSRCH: ! r: search for customer #
	dim heading$*70,form$*80,numeric_format$*20,selection$*70
	file_num=11 ! alpha index on clients
	form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
	numeric_format$='pic($$$,$$$.##)'
	key_length=5
	heading$="Acct #횼ame컴컴컴컴컴컴컴컴컴컴Address컴컴컴컴Balance"
	fnsearch(env$('progrgam_caption'),file_num,heading$,form$,numeric_format$,selection$,key_length)
	k$=z$=selection$ ! pull key from first field in search line
	ano=0
	ano=val(selection$) conv ignore
goto L700 ! /r
L4160: if err=4152 then goto L520 else goto ERTN
include: ertn
