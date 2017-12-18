00020 ! -- RECalculate All Current Bills    *** DO NOT FORGET TO PARALLEL CHANGES FROM S:\acsUB\UBCALK
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fnLastBillingDate,fncloseprn,fnopenprn,fnxit,fnerror,fntos,fnlbl,fnacs
00050   library 'S:\Core\Library': fntxt,fnmsgbox,fncmdset,fntop,fnpause,fncd,fnchk,fncreg_read,fncreg_write,fncomboa
00060   library 'S:\Core\Library': fnget_services,fnapply_default_rates,fnAutomatedSavePoint
00062   library 'S:\Core\Library': fngethandle
00070   if env$('client')="Chatom" then 
00072     library "S:\acsUB\calk_Chatom": fncalk
00074   else 
00076     library "S:\acsUB\calk_standard": fncalk
00078   end if 
00080   on error goto ERTN
00090   fntop(program$,"Calculate Bills")
00110 ! r: dims
00112   dim resp$(10)*128
00114   dim w(5)            ! only dimmed and reset locally - it is used and set in fncalk
00116   dim x$*10
00118   dim x(15)
00120   dim gb(10)
00122   dim rt(10,3)
00124   dim ba(13)
00126   dim da(2)
00128   dim txt$(3)*128
00130   dim a(7)
00132   dim b(11)
00134   dim c(4)
00136   dim d(15)
00138   dim g(12)
00140   dim bt1(14,2)
00142   dim p$*10
00144   dim bt2(14,2)
00146   dim badr(2)
00148   dim tg(11)
00150   dim meteradr$*30
00152   dim custname$*30
00154   dim watuse(12)
00156   dim watdat(12)
00158   dim elecuse(12)
00160   dim elecdat(12)
00162   dim gasuse(12)
00164   dim gasdat(12)
00166   dim servicename$(10)*20
00168   dim tax_code$(10)*1
00170   dim work$*256
00172   dim work_addr$*256
00174   dim subjectto(10)
00176   dim extra(23)
00200 ! /r
00250   fnLastBillingDate(d1)
00260   work$=env$('Q')&"\UBmstr\Reads_and_Chgs.h"&env$('cno')
00264   work_addr$=env$('Q')&"\UBmstr\Reads_and_Chgs-Key.h"&env$('cno')
00265   if env$('client')="Edinburg" or env$('client')="French Settlement" then btu_factor_enabled=1 else btu_factor_enabled=btu=0
00270 ! synchronize this setting with S:\Utility Billing\Enter Readings and Charges (Enter Readings and Charges)
00280 ! ______________________________________________________________________
00290 ! get date meter read
00300   fncreg_read('Meter Reading Date Current',tmp$) : dateread=val(tmp$)
00330 ! 
00340   fnget_services(mat servicename$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
00380   for j=1 to udim(servicename$)
00385     servicename$(j)=trim$(servicename$(j))
00390   next j
00400   fn_ask_billing_date
00420   if ck=5 then goto XIT
00462 ! fnwait("Calculating: please wait...",0)
00480   fnAutomatedSavePoint('before')
00500   fnopenprn
00505 ! 
00510   open #h_ratemst:=8: "Name="&env$('Q')&"\UBmstr\ubData\RateMst.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubData\RateIdx1.h"&env$('cno')&",Shr",internal,input,keyed 
00550 L550: form pos 55,32*g 10
00560   open #h_customer:=1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00570 F_CUSTOMER: form pos 11,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00580 F_CUSTOMER_W_ACCT: form pos 1,c 10,2*c 30,pos 143,7*pd 2,pos 157,11*pd 4.2,pos 201,4*pd 4,pos 217,15*pd 5,pos 292,pd 4.2,pos 296,pd 4,pos 300,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2,n 7,2*n 6,n 9,pd 5.2,n 3,3*n 9,3*n 2,3*n 3,n 1,3*n 9,3*pd 5.2,c 30,7*c 12,3*c 30
00590   open #h_ubtrans:=3: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&env$('cno')&",Shr",internal,outin,keyed 
00600 FORM_UBTRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00610 ! open #h_work:=2: "Name="&work$,internal,outin,relative
00620 F_WORK: form pos 1,c 10,pos 11,4*pd 5,pos 31,7*pd 4.2,pos 59,3*pd 5,n 1
00630   fn_deposit_open
00640   fn_bud_open
00650 ! ______________________________________________________________________
00660 TOP: ! 
00670   if r3=>lrec(h_customer) then goto FINIS
00680 ! read #h_work,using F_WORK,rec=r3+=1: x$,mat x eof FINIS,norec TOP
00690   read #h_customer,using F_CUSTOMER_W_ACCT,rec=r3+=1: x$,meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra norec TOP eof FINIS
00700   if f<>d1 then goto TOP
00710   mat x=(0)
00720   x(1)=d(1) ! current water reading
00730   x(2)=d(9) ! current gas reading
00740   x(3)=d(5) !  current electric reading
00750   if d(3)>0 and d(1)-d(2)<> d(3) then x(12)=d(3) ! if usage was override then use
00760   if d(7)>0 and d(5)-d(6)<> d(7) then x(13)=d(7) ! if usage was override then use
00770   if d(11)>0 and d(9)-d(10)<> d(11) then x(14)=d(11) ! if usage was override then use for gas
00780 ! if x$(1:2)="00" or uprc$(x$)=uprc$("   DELETED") then goto TOP
00790 ! read #h_customer,using F_CUSTOMER,key=x$: meteradr$,custname$,mat a,mat b,mat c,mat d, bal,f,mat g,mat gb,mat extra nokey NKT9
00792 ! r: set default rate codes
00794 ! if env$('client')="Pennington" then extra(12)=1 ! default to 1
00796 ! if env$('client')="Albany" and (a(1)=1 or a(1)=3 or a(1)=4 or a(1)=6) then a(6)=0 ! set residential sales tax code to zero
00798   if env$('client')="Raymond" and (a(1)<>0 and a(6)=0) then a(6)=1 ! if any water rate code and no water penalty than default to water penalty of 1.
00800   if env$('client')="Raymond" and (a(2)<>0 and a(7)=0) then a(7)=1 ! if any sewer rate code and no sewer penalty than default to sewer penalty of 1.
00802   fnapply_default_rates(mat extra, mat a)
00812 ! /r
00826   if f=d1 then ! else recalculation reduce balances
00830     for j=1 to 10
00840       if env$('client')="Divernon" then goto LX760 ! Divernon's penalties are added into gb
00850       if uprc$(penalty$(j))="Y" then goto LX770 ! don't subtract penalties out on recalculation
00860 LX760: ! 
00870       if env$('client')="White Hall" and (j=6 or j=7 or j=10) then goto LX770
00880       gb(j)=gb(j)-g(j) !  if j><6 then gb(j)=gb(j)-g(j)
00890 LX770: ! 
00900     next j
00910   end if  ! f=d1
00920   w7=g(11)
00930   mat g=(0)
00940   usage_srv1=0 ! WATER USAGE
00950   usage_srv3=0 ! ELECTRIC USAGE / lawn meter usage
00960   usage_srv4=0 ! GAS USAGE
00980   r9_usage_is_zero=0
00990   mat w=(0) ! mat w appears to never be set - never be used, but is passed to fncalk
01000   gosub CHECK_UNUSUAL_USAGE
01010   if r9_usage_is_zero=1 then goto TOP
01020   fncalk(x$,d1,f,usage_srv1,usage_srv3,usage_srv4,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,hDeposit2,btu, calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
01030   fn_date_meter_read ! update meter reading date
01040   if g(11)>99999 or g(12)>99999 then goto BILL_TOO_LARGE
01050   if g(11)<99999 and g(11)>-99999 then goto LX1230
01060 BILL_TOO_LARGE: ! r:
01062   pr #255: ""
01070   pr #255: "Net or Gross Bill too large on Account: ";x$
01080   pr #255: "   Net Bill: "&str$(g(11))&"  Gross Bill: "&str$(g(12))
01090   pr #255: "   Action: RECORD SKIPPED."
01092   print_count_skip+=1
01100   fn_cuu_report_usage
01110   goto TOP ! /r
01120 ! ______________________________________________________________________
01130 LX1230: ! r:
01140   fn_bud2
01150   fn_updtbal
01160   for j=1 to 10
01170     if uprc$(penalty$(j))<>"Y" then  ! don't add penalties into mat gb
01180       gb(j)=gb(j)+g(j)
01190     end if
01200   next j
01210   rewrite #h_customer,using F_CUSTOMER,key=x$: meteradr$,custname$,mat a,mat b,mat c,mat d,bal,f,mat g,mat gb,mat extra conv CONV_CUSTOMER_REWRITE
01220   fn_write_new_trans
01230   goto TOP ! /r
30000 CONV_CUSTOMER_REWRITE: ! r:
30020   pr #255: ""
30040   pr #255: "The bill ("&str$(g(12))&") on account "&x$&" is too large for"
30060   pr #255: "the system to handle.  This record is being skipped. "
30080   pr #255: 'You must determine what is wrong and re-enter the reading."'
30090   pr #255: 'ACTION: RECORD SKIPPED'
30100   print_count_skip+=1
30120   ! 
30140   txt$(1)="The bill ("&str$(g(12))&") on account "&x$&" is too large for"
30160   txt$(2)="the system to handle.  This record is being skipped. "
30180   txt$(3)='You must determine what is wrong and re-enter the reading."'
30200   fnmsgbox(mat txt$,resp$(1),'',48)
30220   goto TOP ! /r
31900 ! /r
32000 def fn_write_new_trans
32020   dim transkey$*19
32040   tamount=g(11)
32060   tdate=d1
32080   tdate=fndate_mmddyy_to_ccyymmdd(tdate)
32100   tcode=1
32120   wr=d(1): wu=d(3): er=d(5): eu=d(7): gr=d(9)
32140   gu=d(11)
32160   for j=1 to 11 : tg(j)=g(j) : next j
32180   transkey$=x$&cnvrt$("pic(########)",tdate)&cnvrt$("pic(#)",tcode)
32200   read #h_ubtrans,using FORM_UBTRANS,key=transkey$: y$ nokey UBTRANS_NOKEY ! check for recalk
32220   rewrite #h_ubtrans,using FORM_UBTRANS,key=transkey$: x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,bal,pcode
32240   goto WNT_XIT
32260   UBTRANS_NOKEY: ! 
32280     ! need to update the balance on any transaction that may have been processed since the original charge transaction was created.
32300     write #h_ubtrans,using FORM_UBTRANS: x$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,bal,pcode
32320   WNT_XIT: ! 
32340 fnend 
36000 FINIS: ! r:
36020   fn_t9notification
36040   close #h_customer: ioerr ignore
36060   ! close #h_work,free: ioerr ignore
36070   ! execute 'free '&work_addr$ ioerr ignore
36080   close #h_ubtrans: ioerr ignore
36090 ! pr 'print_count_unusual=';print_count_unusual : pr 'print_count_skip=';print_count_skip
36100   if print_count_unusual or print_count_skip then let fncloseprn
36120 ! /r
38000 XIT: fnxit
40000   def fn_t9notification
40020     if t9 then 
40040       txt$(1)="One or more reading(s) were encounterd for an account(s) that could not be located."
40050       txt$(2)="Set up the UB Accounts indicated on the report."
40060       txt$(3)='Then re-enter and calculate the readings for those customers.' !  by using "Enter Readings and Charges"'
40100       fnmsgbox(mat txt$,resp$(1),'',48)
40120     end if 
40140   fnend 
42000   def fn_cuu_report_main(unusual_service$*128)
42020     if d1<>f then 
42040       if unusual_usage_report=1 or unusual_usage_report=3 then 
42060         pr #255: ""
42080         pr #255: "Unusual "&unusual_service$&" Usage on Customer "&trim$(x$)&".  Bill was calculated."
42100         pr #255,using 'form pos 1,c 30,x 2,c 30': custname$,meteradr$
42120         fn_cuu_report_usage
42130         print_count_unusual+=1
42140       end if 
42160     end if 
42180   fnend 
44000 NKT9: ! r: NOKEY ROUTINE CODE T9=9
44020   t9=9
44040   pr #255: ""
44060   pr #255: "Could not locate an account for Account: "&x$
44080   pr #255: "   Action: RECORD SKIPPED."
44100   print_count_skip+=1
44120   goto TOP ! /r
58000 CHECK_UNUSUAL_USAGE: ! r:
58020   unusual_service$=''
58040   if fn_cuu_water then let fn_cuu_report_main(unusual_service$&'/'&servicename$(1))
58060   if fn_cuu_electric then let fn_cuu_report_main(unusual_service$&'/'&servicename$(3))
58080   if fn_cuu_gas then let fn_cuu_report_main(unusual_service$&'/'&servicename$(4))
58100   unusual_service$=trim$(unusual_service$,'/')
58120 ! 
58140   if r9_usage_is_zero=1 then 
58160     pr #255: ""
58180     pr #255: "Negative usage on Account: "&x$
58200     pr #255: "   Action: RECORD SKIPPED."
58210     print_count_skip+=1
58220     if d1<>f then 
58240       fn_cuu_report_usage
58260     end if 
58280     goto CUU_XIT
58300   end if 
58480 CUU_XIT: ! 
58500   return  ! /r
60000   def fn_cuu_report_usage
60020 ! CUU_REPORT_USAGE_MAIN: !
60040     x=0
60060     mat watuse=(0) : mat watdat=(0)
60080     mat elecuse=(0) : mat elecdat=(0)
60100     mat gasuse=(0) : mat gasdat=(0)
60120     restore #h_ubtrans,key>=x$&"         ": nokey CUU_REPORT_USAGE_PRINT ! find all old usages
60140 CUU_UBTRANS_READ: ! 
60160     read #h_ubtrans,using L2360: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof CUU_REPORT_USAGE_PRINT
60180 L2360: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
60200     if p$<>x$ then goto CUU_REPORT_USAGE_PRINT
60220     if tcode<>1 then goto CUU_UBTRANS_READ ! only charge transactions
60240     billdate=d1
60260     if tdate<fndate_mmddyy_to_ccyymmdd(billdate)-10000 then goto CUU_UBTRANS_READ ! only list last 12 months
60280     x=x+1
60300     if x>12 then goto CUU_REPORT_USAGE_PRINT
60320     watuse(x)=wu : watdat(x)=tdate
60340     elecuse(x)=eu : elecdat(x)=tdate
60360     gasuse(x)=gu : gasdat(x)=tdate
60380     goto CUU_UBTRANS_READ
60400 ! 
60420 CUU_REPORT_USAGE_PRINT: ! 
60440     pr #255: "Type of Service     Old Reading   Current Reading       Calculated Usage"
60460 F_PR_SERVICE: form c 22,pic(---------),x 9,pic(---------),x 11,pic(----------),x 2,c 30,x 2,c 30
60480 F_PR_PRIOR_USAGES: form pos 1,c 13,12*(pic(zzzz/zz/zz),nz 9,x 1)
60500     if trim$(servicename$(1))<>"" then ! test vs. water
60520       pr #255,using F_PR_SERVICE: "Water",d(1),x(1),usage_srv1
60540       pr #255,using F_PR_PRIOR_USAGES: " Prior Usages",watdat(1),watuse(1),watdat(2),watuse(2),watdat(3),watuse(3),watdat(4),watuse(4),watdat(5),watuse(5),watdat(6),watuse(6),watdat(7),watuse(7),watdat(8),watuse(8),watdat(9),watuse(9),watdat(10),watuse(10),watdat(11),watuse(11),watdat(12),watuse(12)
60560     end if 
60580     if trim$(servicename$(3))="Electric" or trim$(servicename$(3))="Lawn Meter" then ! test vs. Electric/lawn meter
60600       pr #255,using F_PR_SERVICE: "Electric",d(5),x(3),usage_srv3
60620       pr #255,using F_PR_PRIOR_USAGES: " Prior Usages",elecdat(1),elecuse(1),elecdat(2),elecuse(2),elecdat(3),elecuse(3),elecdat(4),elecuse(4),elecdat(5),elecuse(5),elecdat(6),elecuse(6),elecdat(7),elecuse(7),elecdat(8),elecuse(8),elecdat(9),elecuse(9),elecdat(10),elecuse(10),elecdat(11),elecuse(11),elecdat(12),elecuse(12)
60640     end if 
60660     if trim$(servicename$(4))="Gas" then ! test vs. Gas
60680       pr #255,using F_PR_SERVICE: "Gas",d(9),x(2),usage_srv4
60700       pr #255,using F_PR_PRIOR_USAGES: " Prior Usages",elecdat(1),gasuse(1),elecdat(2),gasuse(2),elecdat(3),gasuse(3),elecdat(4),gasuse(4),elecdat(5),gasuse(5),elecdat(6),gasuse(6),elecdat(7),gasuse(7),elecdat(8),gasuse(8),elecdat(9),gasuse(9),elecdat(10),gasuse(10),elecdat(11),gasuse(11),elecdat(12),gasuse(12)
60720     end if 
60740     pr #255: ""
60760   fnend 
61000   def fn_date_meter_read ! update meter reading dates
61020 ! f =  billing date (from customer record)
61040 ! d1 = billing date being processed
61060 ! extra(3) prior reading date (from customer record)
61080 ! extra(4) current reading date (from customer record)
61100 ! if trim$(x$)='101385.00' then pause
61120     if dateread<>0 then 
61140       if f=d1 then 
61160         extra(3)=dateread
61180       else 
61200         extra(4)=extra(3)
61220         extra(3)=dateread
61240       end if 
61260     end if  ! dateread<>0
61280   fnend 
62000   def fn_updtbal
62020     d2=f : f=d1
62040     if d1=d2 then bal=bal+g(11)-w7 else bal=bal+g(11)
62060   fnend 
64000   def fn_demand
64020     if env$('client')="Bethany" then 
64040       read #h_ratemst,using L550,key="DM"&lpad$(str$(extra(11)),2): mc1,mu1,mat rt nokey DEMAND_XIT
64060       goto L6360
64080     end if 
64100 !  Read #h_ratemst,Using 540,Key="DM"&LPAD$(STR$(B(2)),2): MC1,MU1,MAT RT Nokey 6070  ! don't have a demand code any where in record.  wlll have to customize for each client  on Bethany we used service 6 to hold demand
64120 L6360: ! 
64140     if env$('client')="Bethany" then 
64160       g(6)=mc1
64180       goto DEMAND_FINIS
64200     end if 
64220     if env$('client')="Lovington" then goto DEMAND_FINIS
64240     g(6)=round(x(4)*d(14)*.001*rt(1,3),2)
64260 DEMAND_FINIS: ! 
64280     d(15)=x(4)
64300 DEMAND_XIT: ! 
64320   fnend  ! return
68000   def fn_bud_open
68020     bud1=0
68040     open #budmstr:=6: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr BUD1_XIT
68060     open #budtrans:=7: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&env$('cno')&",Shr",internal,outin,relative 
68080     bud1=1
68100 BUD1_XIT: ! 
68120   fnend 
69000   def fn_deposit_open !
69020     open #hDeposit1:=fngethandle: "Name="&env$('Q')&"\UBmstr\Deposit1.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\DepIdx1.h"&env$('cno')&",Shr,Use,RecL=16,KPs=1,KLn=10",internal,outin,keyed 
69080     open #hDeposit2:=fngethandle: "Name="&env$('Q')&"\UBmstr\Deposit2.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\Deposit2Index.h"&env$('cno')&',Shr,Use,RecL=73,KPs=1,KLn=10',internal,outin,keyed
69120   fnend 
70000   def fn_bud2
70020     bud2=0
70040     if bud1=0 then goto L7110
70060     mat bt2=(0)
70080     read #budmstr,using FORM_BUDMSTR,key=x$: z$,mat ba,mat badr nokey L7110
70100     if sum(ba)=0 then goto L7110 ! if first screen of budget is blank, then skip budget information  kj 10/20/09
70120 FORM_BUDMSTR: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
70140 ! TRANS ROUTINE
70160     ta1=badr(1)
70180 L6840: if ta1=0 then goto UPDATE_BUDGET_FILE
70200     read #budtrans,using L6860,rec=ta1: z$,mat bt1,nba norec UPDATE_BUDGET_FILE
70220 L6860: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
70240     if bt1(1,2)=d1 then bud2=1 : goto UPDATE_BUDGET_FILE
70260     ta1=nba : goto L6840
70280 ! 
70300 UPDATE_BUDGET_FILE: ! 
70320 ! IF BUD2=1 THEN RE-CALCULATION
70340     bt2(1,1)=bt2(1,2)=d1
70360     bn1=totpen=0
70380     for j=2 to 13
70400       bt2(j,2)=g(j-1)
70420       if ba(j)=0 then bt2(j,1)=ba(j)
70440       if ba(12)>0 then bn1=ba(12) : goto L7020 ! TOTAL BILL BUDGETED
70460       if ba(j)=0 then bt2(j,1)=bt2(j,2) else bt2(j,1)=ba(j)
70480       if j>11 then goto L7010 ! only 1st 10 can be charges
70500       if penalty$(j-1)="Y" then totpen+=bt2(j,1): goto L7020
70520 L7010: if j<11 then bn1=bn1+bt2(j,1)
70540 L7020: ! 
70560     next j
70580     bt2(12,1)=bn1
70600     bt2(13,1)=bt2(12,1)+totpen
70620     if bud2=1 then 
70640       rewrite #budtrans,using L6860,rec=ta1: x$,mat bt2
70660       goto L7100
70680     end if 
70700     write #budtrans,using L6860: x$,mat bt2,badr(1)
70720     r82=lrec(budtrans)
70740     badr(1)=r82
70760     if badr(2)=0 then badr(2)=r82
70780 L7100: ! 
70800     rewrite #budmstr,using FORM_BUDMSTR,key=x$: x$,mat ba,mat badr
70820 L7110: ! 
70840   fnend 
72000 ! <Updateable Region: ERTN>
72020 ERTN: fnerror(program$,err,line,act$,"NO")
72040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
72060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
72080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
72100 ERTN_EXEC_ACT: execute act$ : goto ERTN
72120 ! /region
74000 IGNORE: continue 
74020   def fn_usage(usage_service_number)
74040 ! requires local variables: d1, f, mat x, mat d
74060     usage_return=0
74080     if usage_service_number=1 then 
74100       if x(1)=0 or d1=f then 
74120         usage_return=x(1)-d(2)
74140       else 
74160         usage_return=x(1)-d(1)
74180       end if 
74200     else if usage_service_number=3 then 
74220       if d1=f then 
74240         usage_return=x(3)-d(6)
74260       else 
74280         usage_return=x(3)-d(5)
74300       end if 
74320     else if usage_service_number=4 then 
74340       if d1=f then 
74360         usage_return=x(2)-d(10)
74380       else 
74400         usage_return=x(2)-d(9)
74420       end if 
74440     else 
74460       pr 'usage_service_number not recognized.' : pause 
74480     end if 
74500     fn_usage=usage_return
74520   fnend 
76000   def fn_cuu_water
76020     cuu_water_return=0
76040     if servicename$(1)="Water" then 
76060       if a(1)=0 and a(2)=0 then goto CUU_WATER_XIT ! skip if no water code and no sewer code
76080 ! 
76100       usage_srv1=fn_usage(1)
76120 ! 
76140       if usage_srv1>=0 then 
76160         if d(3)=0 then goto CUU_WATER_XIT
76180         if usage_srv1<d(3)-d(3)*pcent or usage_srv1>d(3)+d(3)*pcent then 
76200           if unusual_usage_report=3 then 
76220             pr #255: '* '&servicename$(1)&' unusual because '&str$(usage_srv1)&'<'&str$(d(3)-d(3)*pcent)&' or '&str$(usage_srv1)&'>'&str$(d(3)+d(3)*pcent)
76240           end if 
76260           cuu_water_return=1
76280         end if 
76300       else 
76320         if x(12)=0 then r9_usage_is_zero=1
76340       end if 
76360     end if 
76380 CUU_WATER_XIT: ! 
76400     fn_cuu_water=cuu_water_return
76420   fnend 
78000   def fn_cuu_electric
78020     cuu_electric_return=0
78040     if servicename$(3)="Electric" or servicename$(3)="Lawn Meter" or service$(3)="EL" then 
78060       if x(3)=0 then goto CUU_ELEC_XIT
78080 ! 
78100       usage_srv3=fn_usage(3)
78120 ! 
78140       if usage_srv3>=0 then 
78160         if d(7)=0 then goto CUU_ELEC_XIT
78180         if usage_srv3<d(7)-d(7)*pcent or usage_srv3>d(7)+d(7)*pcent then 
78200           if unusual_usage_report=3 then 
78220             pr #255: '* '&servicename$(3)&' unusual because '&str$(usage_srv3)&'<'&str$(d(7)-d(7)*pcent)&' or '&str$(usage_srv3)&'>'&str$(d(7)+d(7)*pcent)
78240           end if 
78260           cuu_electric_return=1
78280         end if 
78300       else 
78320         if x(13)=0 then r9_usage_is_zero=1
78340       end if 
78360     end if 
78380 CUU_ELEC_XIT: ! 
78400     fn_cuu_electric=cuu_electric_return
78420   fnend 
80000   def fn_cuu_gas
80020     cuu_gas_return=0
80040     if service$(4)="GA" or servicename$(4)="Gas" then 
80060       if x(2)=0 then goto CUU_GAS_XIT
80080 ! 
80100       usage_srv4=fn_usage(4)
80120 ! 
80140       if usage_srv4>=0 then 
80160         if d(11)=0 then goto CUU_GAS_XIT
80180         if usage_srv4<d(11)-d(11)*pcent or usage_srv4>d(11)+d(11)*pcent then 
80200           if unusual_usage_report=3 then 
80220             pr #255: '* '&servicename$(4)&' unusual because '&str$(usage_srv4)&'<'&str$(d(11)-d(11)*pcent)&' or '&str$(usage_srv4)&'>'&str$(d(11)+d(11)*pcent)
80240           end if 
80260           cuu_gas_return=1
80280         end if 
80300       else 
80320         if x(14)=0 then r9_usage_is_zero=1
80340       end if 
80360     end if 
80380 CUU_GAS_XIT: ! 
80400     fn_cuu_gas=cuu_gas_return
80420   fnend 
82000   def fn_ask_billing_date
82020 ! returns ck (if ck=5 upon return then cancel  was selected)
82040 ASK_BILLING_DATE: ! 
82060     fntos(sn$='ubCalk_'&str$(btu_factor_enabled)&'_'&env$('client')(1:1))
82080     mylen=24 : mypos=mylen+2
82090     respc=0 : linec=0
82100     fnlbl(linec+=1,1,"Billing Date:",mylen,1)
82120 ! fnlbl(1,1,"",34,1)
82140     fntxt(linec,mypos,8,0,1,"1001")
82160     resp$(respc_billing_date:=respc+=1)=str$(d1)
82180     if env$('client')='Campbell' then
82190         linec+=1
82200         fnlbl(linec+=1,1,"Sewer Cap Date:",mylen,1)
82220         fntxt(linec,mypos,8,0,1,"1")
82260         fncreg_read('ubcalk-sewer_cap_date',sewer_cap_date$)
82280         resp$(resp_sewer_cap_date:=respc+=1)=sewer_cap_date$
82300     end if 
82320     if btu_factor_enabled=1 then ! ask BTU question on Edinburg and French Settlement
82340       if env$('client')='French Settlement' then 
82360         fnlbl(linec+=1,1,"Cost of Gas Adjustment:",mylen,1)
82380       else 
82400         fnlbl(linec+=1,1,"Current BTU Factor:",mylen,1)
82420       end if 
82440       fntxt(linec,mypos,10,0,1,"1045")
82460       resp$(resp_btu_factor:=respc+=1)=str$(btu)
82480     end if 
82500     if env$('client')='French Settlement' then 
82510       linec+=1
82520       fnchk(linec+=1,1,"Calculate Interest on Deposit")
82540       resp$(resp_calc_interest_on_deposit:=respc+=1)='False'
82560       fnchk(linec+=1,1,"Charge Inspection Fee")
82580       resp$(resp_charge_inspection_fee:=respc+=1)='False'
82600       fnlbl(linec-2,35,"Interest Credit Rate:",21,1)
82620       fntxt(linec-2,58,10,0,1,"44")
82640       resp$(resp_interest_credit_rate:=respc+=1)='.0500' ! str$(.05)
82660     end if 
83000 ! r: unusual usage report qusetion
83020     dim unusual_usage_report_opt$(3)*52,unusual_usage_report$*52
83040     unusual_usage_report_opt$(1)="Unusual and Skipped (Classic)"
83060     unusual_usage_report_opt$(2)="Skipped Accounts Only"
83080     unusual_usage_report_opt$(3)="Unusual, Skipped and Show Calculations"
83320     fnlbl(linec+=1,1,"Unusual Usage Report:",mylen,1)
83340     fncomboa('ubcalk-unusal_usage_report',linec,mypos,mat unusual_usage_report_opt$, 'Select the unusual usage report style you prefer') ! ,width,contain,tabcon)
83360     fncreg_read('ubcalk-unusal_usage_report',unusual_usage_report$,unusual_usage_report_opt$(2))
83380     unusual_usage_report=srch(mat unusual_usage_report_opt$,unusual_usage_report$)
83400     if unusual_usage_report=0 then unusual_usage_report=1 : unusual_usage_report$=unusual_usage_report_opt$(unusual_usage_report)
83420     resp$(resp_unusual_usage_report:=respc+=1)=unusual_usage_report$
83440 ! /r
83460     fncmdset(2)
83480     fnacs(sn$,0,mat resp$,ck)
83500     if ck<>5 then 
83520       d1=val(resp$(respc_billing_date))
83540       if btu_factor_enabled then btu=val(resp$(resp_btu_factor)) ! Edinburg requires a monthly BTU factor for calculating taxes
83560       if resp_calc_interest_on_deposit and resp$(resp_calc_interest_on_deposit)='True' then calc_interest_on_deposit=1 else calc_interest_on_deposit=0
83580       if resp_charge_inspection_fee and resp$(resp_charge_inspection_fee)='True' then charge_inspection_fee=1 else charge_inspection_fee=0
83600       if resp_interest_credit_rate then interest_credit_rate=val(resp$(resp_interest_credit_rate))
83620       unusual_usage_report$=resp$(resp_unusual_usage_report)
83640       unusual_usage_report=srch(mat unusual_usage_report_opt$,unusual_usage_report$)
83660       fncreg_write('ubcalk-unusal_usage_report',unusual_usage_report$)
83680       if d1<10101 then pr bell; : goto ASK_BILLING_DATE
83700       fnLastBillingDate(d1,1)
83720       if resp_sewer_cap_date then ! if env$('client')='Campbell'
83740         sewer_cap_date$=resp$(resp_sewer_cap_date)
83760         fncreg_write('ubcalk-sewer_cap_date',sewer_cap_date$)
83780       end if 
83800     end if 
83820   fnend 
