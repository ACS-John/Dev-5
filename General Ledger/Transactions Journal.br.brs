! formerly S:\acsGL\acglTrJr
! pr Disbursements, Receipts, General adj/ap/pr/ar, Sales,
! and Purchases Journals a.k.a. Transaction Journals

	autoLibrary
	on error goto Ertn

	dim flo$(8),fli$(8),tr(7),tr$*12,td$*30,oldtr$*12,oldtd$*30,p$(20)*50
	dim sc1$(2)*20,wrd1$(2)*30,resp$(50)*50
	dim b$*3,a$(8)*30,oldtrans$*21,journal_to_print(8),tgl(200,4)

	fnTop(program$)
	a$(1)="Disbursements Journal"
	a$(2)="Receipts Journal"
	a$(3)="General Journal      (Adj)"
	a$(4)="General Journal      (A/P)"
	a$(5)="General Journal      (Payroll)"
	a$(6)="General Journal      (A/R)"
	a$(7)="Sales Journal"
	a$(8)="Purchases Journal"
	mat journal_to_print=(1)

if fnprocess=1 then 
	cur_prior=1
	mat journal_to_print=(1)
else
	gosub ASK_PERIOD
end if
goto PR_JOURNAL
PR_JOURNAL: ! r:
	fnopenprn
	if cur_prior=1 then 
		execute "Index [Q]\GLmstr\GLTrans.h[cno] "&env$('Temp')&"\fsindex.H[cno]"& " 25/29/1 2/12/12 Replace DupKeys -N,Shr"
		open #3: "Name=[Q]\GLmstr\GLtrans.h[cno],KFName="&env$('Temp')&"\fsindex.h[cno],Shr",internal,input,keyed 
	else if cur_prior=2 then ! index current file
		execute "Index [Q]\GLmstr\AcTrans.h[cno] "&env$('Temp')&"\fsindex.H[cno]"& " 25/29/1 2/12/12 Replace DupKeys -N" ! index current file
		open #3: "Name=[Q]\GLmstr\ACtrans.h[cno],KFName="&env$('Temp')&"\fsindex.h[cno],Shr",internal,input,keyed 
	end if 
	PJ_READ_1: ! 
	if cur_prior=2 then 
		read #3,using L390: mat tr,tr$,td$,pcode eof EO_JOURNAL
	else 
		read #3,using L390: mat tr,tr$,td$ eof EO_JOURNAL ! read period code if from history
	end if 
	if cur_prior<>2 then goto L370
	if prior_period>0 and prior_period<>pcode then goto PJ_READ_1
	L370: ! 
	if tr(6)=0 and tr(5)=0 then goto PJ_READ_1
	if tr(6)>1 and tr(6)<9 then goto L390 else tr(6)=1
	L390: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
	if journal_to_print(tr(6))><1 then goto PJ_READ_1 ! JOURNAL NOT SELECTED
	oldtrans$=a$(tr(6))(1:21)
	gosub HDR
	goto PJ_PRINT_REC
	PJ_READ_2: ! 
	if cur_prior=2 then 
		read #3,using L390: mat tr,tr$,td$,pcode eof EO_JOURNAL
		if prior_period>0 and prior_period<>pcode then goto PJ_READ_2
	else ! read period code if from history
		read #3,using L390: mat tr,tr$,td$ eof EO_JOURNAL
		goto L490 ! don't check period code if current files
	end if 
	if tr(6)>1 and tr(6)<9 then goto L490 else tr(6)=1
	L490: ! 
	if journal_to_print(tr(6))><1 then goto PJ_READ_2
	if a$(tr(6))(1:21)><oldtrans$ then gosub JOURNAL_TOTALS
	tr6=tr(6)
	if tr$=oldtr$ and tr$><"999999999999" then 
		goto PJ_PRINT_REC
	else 
		gosub PJ_SOME_TOTAL
	end if 
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
	! pr #255: ""
	if tr(5)>=0 then 
		pr #255,using L600: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
	else 
		pr #255,using L610: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
	end if 
	L600: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
	L610: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 85,pic(--,---,---.##)
	L620: ! 
goto L690 ! /r SKIP ACCUMULATING ARRAY TOTALS
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
L690: ! r:
	if tr(5)<0 then goto L720 ! CREDITS
	total1+=tr(5)
	goto L740
	L720: ! 
	total2+=tr(5)
	L740: ! 
	if uprc$(td$(1:6))="CONTRA" then goto L760 ! NO CONTRA ENTRIES IN NET
	net+=tr(5)
	L760: ! 
	oldtr$=tr$ : oldtd$=td$
goto PJ_READ_2 ! /r
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
	pr #255,using L890: date$('mm/dd/yy'),env$('program_caption')
	if tr(6)<>0 then 
		pr #255,using L890: time$,rtrm$(a$(tr(6))(1:21))
	end if 
	L890: form pos 1,c 8,pos 15,cc 50
	pr #255,using L890: "",fnpedat$
	pr #255: tab(115);"Page "&str$(p1+=1)
	pr #255: " Reference               Transaction";tab(79);"Debit";tab(92);"Credit"
	pr #255: "  Number         Date    Description";tab(61);"Account";
	if tr(6)=1 then b$="NET" else b$=" "
	pr #255,using L960: "Amount       Amount",b$
	L960: form pos 79,c 19,pos 111,c 3
	pr #255: " ________      ________  ____________________";tab(59);"___________";tab(79);"______       ______";
	if tr(6)=1 then 
		pr #255,using L990: "___"
	else 
		pr #255,using L990: "   "
	end if 
	L990: form pos 111,c 3
return  ! /r
JOURNAL_TOTALS: ! r:
	gosub PJ_SOME_TOTAL
	pr #255: tab(72);"_____________";tab(86);"______________"
	pr #255,using L1060: "Journal Totals",total1,total2
	L1060: form pos 55,c 14,pos 70,pic(----,---,---.##),pic(----,---,---.##)
	pr #255: tab(72);"=============";tab(86);"=============="
	! IF TR6=1 THEN GOSUB 1230
	total1=total2=net=0
	if t9=9 then goto L1150
	pr #255: newpage
	gosub HDR
	if tr(6)=0 then oldtrans$=" " else oldtrans$=a$(tr(6))(1:21)
	oldtr$=" "
	L1150: !
return  ! /r
EO_JOURNAL: ! r:
	if tr(5)=0 and tr(6)=0 then goto L1210
	t9=9
	gosub PJ_SOME_TOTAL
	gosub JOURNAL_TOTALS
	L1210: !
	fncloseprn
goto Xit ! /r
PGOF: ! r:
	pr #255: newpage
	gosub HDR
continue  ! /r

Xit: fnXit

ASK_PERIOD: ! r:
! pr newpage
	fnTos(sn$="TRJR")
	respc=0
	fnFra(1,1,2,50,"Print from current month files or history"," ")
	fnOpt(1,3,"Current Period Transactions",0,1)
	resp$(respc+=1)="True"
	fnOpt(2,3,"Prior Period Transactions",0,1)
	resp$(respc+=1)="False"
	fnFra(5,1,8,50,"Select Journals to Print"," ")
	fnChk(1,3,"Disbursements Journal",0,2)
	resp$(respc+=1)="True"
	fnChk(2,3,"Receipts Journal",0,2)
	resp$(respc+=1)="True"
	fnChk(3,3,"General Journal (Adj)",0,2)
	resp$(respc+=1)="True"
	fnChk(4,3,"General Journal (A/P)",0,2)
	resp$(respc+=1)="False"
	fnChk(5,3,"General Journal (Payroll)",0,2)
	resp$(respc+=1)="False"
	fnChk(6,3,"General Journal (A/R)",0,2)
	resp$(respc+=1)="False"
	fnChk(7,3,"Sales Journal",0,2)
	resp$(respc+=1)="False"
	fnChk(8,3,"Purchases Journal",0,2)
	resp$(respc+=1)="False"
	fnLbl(16,1,"Prior period code (blank for all):",35,0)
	fnTxt(16,37,2,0,1,"30",0,"Prior period code is only applicable if printing from history.  Enter the period code for the month you want printed. Use blank for all and also if you chose current period transactions.")
	resp$(respc+=1)=" "
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto Xit
	if resp$(1)="True" then cur_prior=1 else cur_prior=2
	mat journal_to_print=(0)
	for j=1 to 8
		if resp$(j+2)="True" then journal_to_print(j)=1
	next j
	prior_period=val(resp$(11)) ! prior period code
	if prior_period<0 or prior_period>13 then prior_period=0
return  ! /r
include: Ertn
