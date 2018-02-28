10000 ! Replace S:\acsGL\acglTrJr
10020 ! pr Disbursements, Receipts, General adj/ap/pr/ar, Sales,
10040 ! and Purchases Journals a.k.a. Transaction Journals
10060 ! ______________________________________________________________________
10080   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnTos,fnCmdSet,fnAcs,fnLbl,fnTxt,fnconsole
10100   on error goto ERTN
10120 ! r: constants and dims
10140   dim tr(7),tr$*12,td$*30,oldtr$*12
10160   dim cap$*128,resp$(50)*50
10180   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*21,tgl(200,4)
10200   period_current=1
10220   period_prior=2
10240   a$(1)="Disbursements Journal"
10260   a$(2)="Receipts Journal"
10280   a$(3)="General Journal      (Adj)"
10300   a$(4)="General Journal      (A/P)"
10320   a$(5)="General Journal      (Payroll)"
10340   a$(6)="General Journal      (A/R)"
10360   a$(7)="Sales Journal"
10380   a$(8)="Purchases Journal"
12000 ! /r
12020   fntop(program$,cap$="Fix Current Period Transactions Dates")
12040   fncno(cno,cnam$)
12060   fnconsole(off=0) ! temporary-take out
16000 ! 
16040   gosub ASK_PERIOD
16050   if ck=5 then goto XIT
16060 ! 
18020   fnopenprn
18040 ! if cur_prior=period_current then
18080   open #h_gltrans:=3: "Name=[Q]\GLmstr\GLTrans.h[cno],Shr",internal,outIn,relative 
18100 F_GLTRANS: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
20000   gosub HDR
20020   do 
20040     do 
20060       read #h_gltrans,using F_GLTRANS: mat tr,tr$,td$ eof EO_JOURNAL
20080     loop until tr(4)=date_bad
20100     tr(4)=date_good
20120     rewrite #h_gltrans,using F_GLTRANS: mat tr,tr$,td$
20140 ! 
22000 PJ_PRINT_REC: ! 
22020     if tr$="999999999999" then tr$=" "
22040     if tr(5)>0 then 
22060       pr #255,using L550: ltrm$(tr$),tr(4),td$,tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22080     else 
22100       pr #255,using L560: ltrm$(tr$),tr(4),td$,tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22120     end if 
22140 L550: form pos 3,cc 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
22160 L560: form pos 3,cc 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 82,pic(------,---,---.##)
22180     goto L620
22200 ! pr #255: ""
22220     if tr(5)>=0 then 
22240       pr #255,using L600: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22260     else 
22280       pr #255,using L610: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22300     end if 
22320 L600: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
22340 L610: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 85,pic(--,---,---.##)
22360 L620: ! 
22380     goto L690 ! SKIP ACCUMULATING ARRAY TOTALS
24000 ! r:  ACCUMULATING ARRAY TOTALS
24020     if tg1=0 then goto L670
24040     for j=1 to tg1
24060       if tr(1)=tgl(j,1) and tr(2)=tgl(j,2) and tr(3)=tgl(j,3) then goto L680
24080     next j
24100 L670: ! 
24120     j=tg1=tg1+1
24140 L680: ! 
24160     tgl(j,1)=tr(1): tgl(j,2)=tr(2): tgl(j,3)=tr(3)
24180     tgl(j,4)=tgl(j,4)+tr(5)
24200 ! /r
26000 L690: ! 
26020     if tr(5)<0 then goto L720 ! CREDITS
26040     total1+=tr(5)
26060     goto L740
26080 L720: ! 
26100     total2+=tr(5)
26120 L740: ! 
26140     if uprc$(td$(1:6))="CONTRA" then goto L760 ! NO CONTRA ENTRIES IN NET
26160     net+=tr(5)
26180 L760: ! 
26200     oldtr$=tr$
26220   loop 
28000 PJ_SOME_TOTAL: ! r:
28020   if tr(6)><1 and uprc$(oldtrans$(1:21))><"DISBURSEMENTS JOURNAL" then 
28040     goto L810
28060   end if 
28080   pr #255,using L800: net pageoflow PGOF
28100 L800: form pos 100,pic(---,---,---.##)
28120 L810: ! 
28140   if uprc$(a$(tr(6))(1:21))><uprc$(oldtrans$) or t9=9 then goto L830
28160   pr #255: pageoflow PGOF
28180 L830: ! 
28200   net=0
28220   return  ! /r
30000 HDR: ! r:
30020   pr #255,using F_HDR_1: date$('mm/dd/yy'),cnam$
30040   if tr(6)<>0 then 
30060     pr #255,using F_HDR_1: time$,rtrm$(a$(tr(6))(1:21))
30080   end if 
30100 F_HDR_1: form pos 1,c 8,pos 15,cc 50
30120   pr #255,using F_HDR_1: "",fnpedat$
30140   pr #255: tab(115);"Page "&str$(p1+=1)
30160   pr #255: " Reference               Transaction";tab(79);"Debit";tab(92);"Credit"
30180   pr #255: "  Number         Date    Description";tab(61);"Account";
30200   if tr(6)=1 then b$="NET" else b$=" "
30220   pr #255,using 'form pos 79,c 19,pos 111,c 3': "Amount       Amount",b$
30260   pr #255: " ________      ________  ____________________";tab(59);"___________";tab(79);"______       ______";
30280   if tr(6)=1 then 
30300     pr #255,using 'form pos 111,c 3': "___"
30320   else 
30340     pr #255,using 'form pos 111,c 3': "   "
30360   end if 
30400   return  ! /r
32000 JOURNAL_TOTALS: ! r:
32020   gosub PJ_SOME_TOTAL
32040   pr #255: tab(72);"_____________";tab(86);"______________"
32060   pr #255,using 'form pos 55,c 14,pos 70,pic(----,---,---.##),pic(----,---,---.##)': "Journal Totals",total1,total2
32100   pr #255: tab(72);"=============";tab(86);"=============="
32120 ! IF TR6=1 THEN GOSUB 1230
32140   total1=total2=net=0
32160   if t9=9 then goto L1150
32180   pr #255: newpage
32200   gosub HDR
32220   if tr(6)=0 then oldtrans$=" " else oldtrans$=a$(tr(6))(1:21)
32240   oldtr$=" "
32260 L1150: return  ! /r
34000 EO_JOURNAL: ! r:
34020   if tr(5)=0 and tr(6)=0 then goto L1210
34040   t9=9
34060   gosub PJ_SOME_TOTAL
34080   gosub JOURNAL_TOTALS
34100 L1210: fncloseprn
34120   goto XIT ! /r
36000 PGOF: ! r:
36020   pr #255: newpage
36040   gosub HDR
36060   continue  ! /r
38300 ! ______________________________________________________________________
38320 ! <Updateable Region: ERTN>
38340 ERTN: fnerror(program$,err,line,act$,"xit")
38360   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
38380   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
38400   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
38420 ERTN_EXEC_ACT: execute act$ : goto ERTN
38440 ! /region
38460 ! ______________________________________________________________________
38480 XIT: fnxit
38500 ! ______________________________________________________________________
40000 ASK_PERIOD: ! r:
40020 ! pr newpage
40040   fnTos(sn$="fix_trans_dates")
40060   respc=0
40080 ! fnFra(1,1,5,50,"Print from current month files or history"," ")
40100 ! fnOpt(1,3,"Current Period Transactions",0,1)
40120 ! resp$(respc+=1)="True"
40140 ! fnOpt(3,3,"Prior Period Transactions",0,1)
40160 ! resp$(respc+=1)="False"
40180 ! fnLbl(4,1,"Prior period code (blank for all):",35,0,0,1)
40200   fnLbl(1,1,"Only Current Period transactions will be processed.",51,2)
40220   fnLbl(2,1,"All matching dates will be changed.",51,2)
40240   resp$(respc+=1)=" "
40260 ! fnFra(7,1,4,50,"Date Correction"," ")
40280   fnLbl(4,1,"Bad Date:",12,1) ! ,0,0,2)
40300   fnTxt(4,14,2,0,1,"1",0,"Prior period code is only applicable if printing from history.  Enter the period code for the month you want printed. Use blank for all and also if you chose current period transactions.")
40320   resp$(respc+=1)=" "
40340   fnLbl(5,1,"Good Date:",12,1) ! ,0,0,2)
40360   fnTxt(5,14,2,0,1,"1",0,"")
40380   resp$(respc+=1)=" "
40400   fnCmdSet(2)
40420   fnAcs(sn$,0,mat resp$,ck)
40440   if ck<>5 then 
40460     date_bad=val(resp$(1))
40480     date_good=val(resp$(2))
40490     if date_bad=0 or date_good=0 then goto ASK_PERIOD
40500   end if 
40520   return  ! /r
