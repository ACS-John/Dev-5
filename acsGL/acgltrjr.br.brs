10000 ! Replace S:\acsGL\acglTrJr
10020 ! pr Disbursements, Receipts, General adj/ap/pr/ar, Sales,
10040 ! and Purchases Journals a.k.a. Transaction Journals
10060 ! ______________________________________________________________________
10080   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fnps,fnpedat$,fnopt,fntos,fncmdset,fnacs,fnfra,fnchk,fnlbl,fntxt,fnconsole
10100   on error goto ERTN
10120 ! ______________________________________________________________________
10140   dim flo$(8),fli$(8),tr(7),tr$*12,td$*30,oldtr$*12,oldtd$*30,p$(20)*50
10160   dim sc1$(2)*20,cap$*128,wrd1$(2)*30,resp$(50)*50
10180   dim cnam$*40,b$*3,a$(8)*30,oldtrans$*21,journal_to_print(8),tgl(200,4)
10200 ! ______________________________________________________________________
10220   let fntop(program$,cap$="Transactions Journal")
10240   let fncno(cno,cnam$)
10260   let fnconsole(off=0) ! temporary-take out
10280   a$(1)="Disbursements Journal"
10300   a$(2)="Receipts Journal"
10320   a$(3)="General Journal      (Adj)"
10340   a$(4)="General Journal      (A/P)"
10360   a$(5)="General Journal      (Payroll)"
10380   a$(6)="General Journal      (A/R)"
10400   a$(7)="Sales Journal"
10420   a$(8)="Purchases Journal"
10440   mat journal_to_print=(1)
16000 ! ______________________________________________________________________
16020   if fnprocess=1 then cur_prior=1 : mat journal_to_print=(1) : goto PR_JOURNAL
16040   gosub ASK_PERIOD
16060 ! ______________________________________________________________________
18000 PR_JOURNAL: ! 
18020   let fnopenprn
18040   if cur_prior=1 then 
18060     execute "Index "&env$('Q')&"\GLmstr\GLTrans.h"&str$(cno)&" "&env$('Temp')&"\fsindex.H"&str$(cno)& " 25/29/1 2/12/12 Replace DupKeys -N,Shr"
18080     open #3: "Name="&env$('Q')&"\GLmstr\GLtrans.h"&str$(cno)&",KFName="&env$('Temp')&"\fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
18100   else if cur_prior=2 then ! index current file
18120     execute "Index "&env$('Q')&"\GLmstr\AcTrans.h"&str$(cno)&" "&env$('Temp')&"\fsindex.H"&str$(cno)& " 25/29/1 2/12/12 Replace DupKeys -N" ! index current file
18140     open #3: "Name="&env$('Q')&"\GLmstr\ACtrans.h"&str$(cno)&",KFName="&env$('Temp')&"\fsindex.h"&str$(cno)&",Shr",internal,input,keyed 
18160   end if 
20000 PJ_READ_1: ! 
20020   if cur_prior=2 then 
20040     read #3,using L390: mat tr,tr$,td$,pcode eof EO_JOURNAL
20060   else 
20080     read #3,using L390: mat tr,tr$,td$ eof EO_JOURNAL ! read period code if from history
20100   end if 
20120   if cur_prior<>2 then goto L370
20140   if prior_period>0 and prior_period<>pcode then goto PJ_READ_1
20160 L370: ! 
20180   if tr(6)=0 and tr(5)=0 then goto PJ_READ_1
20200   if tr(6)>1 and tr(6)<9 then goto L390 else let tr(6)=1
20220 L390: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,2*n 2,c 12,c 30,n 2
20240   if journal_to_print(tr(6))><1 then goto PJ_READ_1 ! JOURNAL NOT SELECTED
20260   let oldtrans$=a$(tr(6))(1:21)
20280   gosub HDR
20300   goto PJ_PRINT_REC
20320 PJ_READ_2: ! 
20340   if cur_prior=2 then 
20360     read #3,using L390: mat tr,tr$,td$,pcode eof EO_JOURNAL
20380     if prior_period>0 and prior_period<>pcode then goto PJ_READ_2
20400   else ! read period code if from history
20420     read #3,using L390: mat tr,tr$,td$ eof EO_JOURNAL
20440     goto L490 ! don't check period code if current files
20460   end if 
20480   if tr(6)>1 and tr(6)<9 then goto L490 else let tr(6)=1
20500 L490: ! 
20520   if journal_to_print(tr(6))><1 then goto PJ_READ_2
20540   if a$(tr(6))(1:21)><oldtrans$ then gosub JOURNAL_TOTALS
20560   let tr6=tr(6)
20580   if tr$=oldtr$ and tr$><"999999999999" then 
20600     goto PJ_PRINT_REC
20620   else 
20640     gosub PJ_SOME_TOTAL
20660   end if 
20680 ! 
22000 PJ_PRINT_REC: ! 
22020   if tr$="999999999999" then let tr$=" "
22040   if tr(5)>0 then 
22060     pr #255,using L550: ltrm$(tr$),tr(4),td$,tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22080   else 
22100     pr #255,using L560: ltrm$(tr$),tr(4),td$,tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22120   end if 
22140 L550: form pos 3,cc 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
22160 L560: form pos 3,cc 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 82,pic(------,---,---.##)
22180   goto L620
22200 ! pr #255: ""
22220   if tr(5)>=0 then 
22240     pr #255,using L600: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22260   else 
22280     pr #255,using L610: tr(1),tr(2),tr(3),tr(5) pageoflow PGOF
22300   end if 
22320 L600: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 69,pic(------,---,---.##)
22340 L610: form pos 57,pic(zzz),pic(zzzzzz),pic(zzz),pos 85,pic(--,---,---.##)
22360 L620: ! 
22380   goto L690 ! SKIP ACCUMULATING ARRAY TOTALS
24000 ! r:  ACCUMULATING ARRAY TOTALS
24020   if tg1=0 then goto L670
24040   for j=1 to tg1
24060     if tr(1)=tgl(j,1) and tr(2)=tgl(j,2) and tr(3)=tgl(j,3) then goto L680
24080   next j
24100 L670: ! 
24120   let j=tg1=tg1+1
24140 L680: ! 
24160   let tgl(j,1)=tr(1): let tgl(j,2)=tr(2): let tgl(j,3)=tr(3)
24180   let tgl(j,4)=tgl(j,4)+tr(5)
24200 ! /r
26000 L690: ! 
26020   if tr(5)<0 then goto L720 ! CREDITS
26040   let total1+=tr(5)
26060   goto L740
26080 L720: ! 
26100   let total2+=tr(5)
26120 L740: ! 
26140   if uprc$(td$(1:6))="CONTRA" then goto L760 ! NO CONTRA ENTRIES IN NET
26160   let net+=tr(5)
26180 L760: ! 
26200   let oldtr$=tr$ : let oldtd$=td$
26220   goto PJ_READ_2
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
28200   let net=0
28220   return  ! /r
30000 HDR: ! r:
30020   pr #255,using L890: date$('mm/dd/yy'),cnam$
30040   if tr(6)<>0 then 
30060     pr #255,using L890: time$,rtrm$(a$(tr(6))(1:21))
30080   end if 
30100 L890: form pos 1,c 8,pos 15,cc 50
30120   pr #255,using L890: "",fnpedat$
30140   pr #255: tab(115);"Page "&str$(p1+=1)
30160   pr #255: " Reference               Transaction";tab(79);"Debit";tab(92);"Credit"
30180   pr #255: "  Number         Date    Description";tab(61);"Account";
30200   if tr(6)=1 then b$="NET" else b$=" "
30220   pr #255,using L960: "Amount       Amount",b$
30240 L960: form pos 79,c 19,pos 111,c 3
30260   pr #255: " ________      ________  ____________________";tab(59);"___________";tab(79);"______       ______";
30280   if tr(6)=1 then 
30300     pr #255,using L990: "___"
30320   else 
30340     pr #255,using L990: "   "
30360   end if 
30380 L990: form pos 111,c 3
30400   return  ! /r
32000 JOURNAL_TOTALS: ! r:
32020   gosub PJ_SOME_TOTAL
32040   pr #255: tab(72);"_____________";tab(86);"______________"
32060   pr #255,using L1060: "Journal Totals",total1,total2
32080 L1060: form pos 55,c 14,pos 70,pic(----,---,---.##),pic(----,---,---.##)
32100   pr #255: tab(72);"=============";tab(86);"=============="
32120 ! IF TR6=1 THEN GOSUB 1230
32140   let total1=total2=net=0
32160   if t9=9 then goto L1150
32180   pr #255: newpage
32200   gosub HDR
32220   if tr(6)=0 then let oldtrans$=" " else let oldtrans$=a$(tr(6))(1:21)
32240   let oldtr$=" "
32260 L1150: return  ! /r
34000 EO_JOURNAL: ! r:
34020   if tr(5)=0 and tr(6)=0 then goto L1210
34040   let t9=9
34060   gosub PJ_SOME_TOTAL
34080   gosub JOURNAL_TOTALS
34100 L1210: let fncloseprn
34120   goto XIT ! /r
36000 PGOF: ! r:
36020   pr #255: newpage
36040   gosub HDR
36060   continue  ! /r
38000 ! ______________________________________________________________________
38020 !       pr #255: newpage
38040 !       let p1=p1+1
38060 !       pr #255,using L890: date$('mm/dd/yy'),cnam$,time$
38080 !       pr #255,using L890: oldtrans$
38100 !       pr #255,using L890: fnpedat$
38120 !       pr #255: tab(50);"  G/L Number          Amount"
38140 !       pr #255: tab(50);"______________  ____________"
38160 !       for j=1 to tg1
38180 !         pr #255,using L1370: tgl(j,1),tgl(j,2),tgl(j,3),tgl(j,4)
38200 !     L1370: form pos 50,pic(zzz),pic(zzzzzz),pic(zzz),n 14.2
38220 !       next j
38240 !       mat tgl=(0)
38260 !       let tg1=0
38280 !       return
38300 ! ______________________________________________________________________
38320 ! <Updateable Region: ERTN>
38340 ERTN: let fnerror(program$,err,line,act$,"xit")
38360   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
38380   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
38400   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
38420 ERTN_EXEC_ACT: execute act$ : goto ERTN
38440 ! /region
38460 ! ______________________________________________________________________
38480 XIT: let fnxit
38500 ! ______________________________________________________________________
40000 ASK_PERIOD: ! r:
40020 ! pr newpage
40040   let fntos(sn$="TRJR")
40060   let respc=0
40080   let fnfra(1,1,2,50,"Print from current month files or history"," ")
40100   let fnopt(1,3,"Current Period Transactions",0,1)
40120   let resp$(respc+=1)="True"
40140   let fnopt(2,3,"Prior Period Transactions",0,1)
40160   let resp$(respc+=1)="False"
40180   let fnfra(5,1,8,50,"Select Journals to Print"," ")
40200   let fnchk(1,3,"Disbursements Journal",0,2)
40220   let resp$(respc+=1)="True"
40240   let fnchk(2,3,"Receipts Journal",0,2)
40260   let resp$(respc+=1)="True"
40280   let fnchk(3,3,"General Journal (Adj)",0,2)
40300   let resp$(respc+=1)="True"
40320   let fnchk(4,3,"General Journal (A/P)",0,2)
40340   let resp$(respc+=1)="False"
40360   let fnchk(5,3,"General Journal (Payroll)",0,2)
40380   let resp$(respc+=1)="False"
40400   let fnchk(6,3,"General Journal (A/R)",0,2)
40420   let resp$(respc+=1)="False"
40440   let fnchk(7,3,"Sales Journal",0,2)
40460   let resp$(respc+=1)="False"
40480   let fnchk(8,3,"Purchases Journal",0,2)
40500   let resp$(respc+=1)="False"
40520   let fnlbl(16,1,"Prior period code (blank for all):",35,0)
40540   let fntxt(16,37,2,0,1,"30",0,"Prior period code is only applicable if printing from history.  Enter the period code for the month you want printed. Use blank for all and also if you chose current period transactions.")
40560   let resp$(respc+=1)=" "
40580   let fncmdset(2)
40590   let fnacs(sn$,0,mat resp$,ck)
40600   if ck=5 then goto XIT
40620   if resp$(1)="True" then cur_prior=1 else cur_prior=2
40640   mat journal_to_print=(0)
40660   for j=1 to 8
40680     if resp$(j+2)="True" then let journal_to_print(j)=1
40700   next j
40720   let prior_period=val(resp$(11)) ! prior period code
40740   if prior_period<0 or prior_period>13 then let prior_period=0
40760   return  ! /r
