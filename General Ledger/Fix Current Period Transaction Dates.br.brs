	autoLibrary
	on error goto Ertn
	fnTop(program$,cap$="Fix Current Period Transactions Dates")
! r: constants and dims
	dim tr(7),tr$*12,td$*30,oldtr$*12
	dim cap$*128,resp$(50)*50
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*21,tgl(200,4)
	period_current=1
	period_prior=2
	a$(1)="Disbursements Journal"
	a$(2)="Receipts Journal"
	a$(3)="General Journal      (Adj)"
	a$(4)="General Journal      (A/P)"
	a$(5)="General Journal      (Payroll)"
	a$(6)="General Journal      (A/R)"
	a$(7)="Sales Journal"
	a$(8)="Purchases Journal"
! /r


	gosub ASK_PERIOD
	if ckey=5 then goto Xit

	fnopenprn
	open #h_gltrans:=3: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative 
	F_GLTRANS: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
	gosub HDR
	do 
		do 
			read #h_gltrans,using F_GLTRANS: mat tr,tr$,td$ eof EO_JOURNAL
		loop until tr(4)=date_bad
		tr(4)=date_good
		rewrite #h_gltrans,using F_GLTRANS: mat tr,tr$,td$

PJ_PRINT_REC: ! 
		if tr$="999999999999" then tr$=" "
		if tr(5)>0 then 
			pr #255,using L550: ltrm$(tr$),tr(4),td$,tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
		else 
			pr #255,using L560: ltrm$(tr$),tr(4),td$,tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
		end if 
		L550: form pos 3,cc 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
		L560: form pos 3,cc 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 82,pic(------,---,---.##)
		goto L620
		if tr(5)>=0 then 
			pr #255,using L600: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
		else 
			pr #255,using L610: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
		end if 
		L600: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
		L610: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 85,pic(--,---,---.##)
		L620: ! 
		goto L690 ! SKIP ACCUMULATING ARRAY TOTALS
! r:  ACCUMULATING ARRAY TOTALS
		if tg1=0 then goto L670
		for j=1 to tg1
			if tr(1)=tgl(j,1) and tr(2)=tgl(j,2) and tr(3)=tgl(j,3) then goto L680
		next j
		L670: ! 
		j=tg1=tg1+1
		L680: ! 
		tgl(j,1)=tr(1): tgl(j,2)=tr(2): tgl(j,3)=tr(3)
		tgl(j,4)=tgl(j,4)+tr(5)
! /r
		L690: ! 
		if tr(5)<0 then goto L720 ! CREDITS
		total1+=tr(5)
		goto L740
		L720: ! 
		total2+=tr(5)
		L740: ! 
		if uprc$(td$(1:6))="CONTRA" then goto L760 ! NO CONTRA ENTRIES IN NET
		net+=tr(5)
		L760: ! 
		oldtr$=tr$
	loop 
PJ_SOME_TOTAL: ! r:
	if tr(6)><1 and uprc$(oldtrans$(1:21))><"DISBURSEMENTS JOURNAL" then 
		goto L810
	end if 
	pr #255,using L800: net pageoflow PGOF
	L800: form pos 100,pic(---,---,---.##)
	L810: ! 
	if uprc$(a$(tr(6))(1:21))><uprc$(oldtrans$) or t9=9 then goto L830
	pr #255: pageoflow PGOF
	L830: ! 
	net=0
return  ! /r
HDR: ! r:
	pr #255,using F_HDR_1: date$('mm/dd/yy'),cnam$
	if tr(6)<>0 then 
		pr #255,using F_HDR_1: time$,rtrm$(a$(tr(6))(1:21))
	end if 
F_HDR_1: form pos 1,c 8,pos 15,cc 50
	pr #255,using F_HDR_1: "",fnpedat$
	pr #255: tab(115);"Page "&str$(p1+=1)
	pr #255: " Reference               Transaction";tab(79);"Debit";tab(92);"Credit"
	pr #255: "  Number         Date    Description";tab(61);"Account";
	if tr(6)=1 then b$="NET" else b$=" "
	pr #255,using 'form pos 79,c 19,pos 111,c 3': "Amount       Amount",b$
	pr #255: " ________      ________  ____________________";tab(59);"___________";tab(79);"______       ______";
	if tr(6)=1 then 
		pr #255,using 'form pos 111,c 3': "___"
	else 
		pr #255,using 'form pos 111,c 3': "   "
	end if 
	return  ! /r
JOURNAL_TOTALS: ! r:
	gosub PJ_SOME_TOTAL
	pr #255: tab(72);"_____________";tab(86);"______________"
	pr #255,using 'form pos 55,c 14,pos 70,pic(----,---,---.##),pic(----,---,---.##)': "Journal Totals",total1,total2
	pr #255: tab(72);"=============";tab(86);"=============="
! IF TR6=1 THEN GOSUB 1230
	total1=total2=net=0
	if t9=9 then goto L1150
	pr #255: newpage
	gosub HDR
	if tr(6)=0 then oldtrans$=" " else oldtrans$=a$(tr(6))(1:21)
	oldtr$=" "
L1150: return  ! /r
EO_JOURNAL: ! r:
	if tr(5)=0 and tr(6)=0 then goto L1210
	t9=9
	gosub PJ_SOME_TOTAL
	gosub JOURNAL_TOTALS
L1210: fncloseprn
	goto Xit ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r


Xit: fnXit
!
ASK_PERIOD: ! r:
! pr newpage
	fnTos(sn$="fix_trans_dates")
	respc=0
	fnLbl(1,1,"Only Current Period transactions will be processed.",51,2)
	fnLbl(2,1,"All matching dates will be changed.",51,2)
	resp$(respc+=1)=" "
	fnLbl(4,1,"Bad Date:",12,1) ! ,0,0,2)
	fnTxt(4,14,2,0,1,"1",0,"Prior period code is only applicable if printing from history.  Enter the period code for the month you want printed. Use blank for all and also if you chose current period transactions.")
	resp$(respc+=1)=" "
	fnLbl(5,1,"Good Date:",12,1) ! ,0,0,2)
	fnTxt(5,14,2,0,1,"1",0,"")
	resp$(respc+=1)=" "
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey<>5 then 
		date_bad=val(resp$(1))
		date_good=val(resp$(2))
		if date_bad=0 or date_good=0 then goto ASK_PERIOD
	end if 
return  ! /r
include: Ertn

