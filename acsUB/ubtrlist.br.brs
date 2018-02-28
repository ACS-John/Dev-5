00020 ! Replace S:\acsUB\ubTrList
00022   debug_account_of_interest$='100416.10'
00040 ! r: NOTES, dims, defaults, constants, libraries, on err, etc
00060 ! -- Transaction Listing
00080 ! known problem - If transaction is actually process within the date range but the date is a wrong date outside the date range, the beginning balance the next period will not agree with the ending balance from the previous month. Don't know what to do about it!!
00100 ! code descriptions
00120 ! firstone=2   lastone=2   !no transactions
00140 ! firstone=1   lastone=0   ! 1st but more transactions exist
00160 ! firstone=0   lastone=1   ! more than one trans and this is last one
00180 ! firstone=1   lastone=1   ! just one transaction
00200 ! firstone=0   lastone=0   ! not first and not last
00220 ! ______________________________________________________________________
00240   library 'S:\Core\Library': fnAcs,fnopenprn,fncloseprn,fnerror,fnget_services,fndat,fnwait,fnTos,fnLbl,fnTxt,fnmsgbox,fnxit,fnCmdSet,fntop,fnFra,fnOpt,fnChk,fnreg_read,fnreg_write
00260   on error goto ERTN
16000   dim p$*10,foot$*16,gb(10),tgb(10),ggb(10),dat$*20
16020   dim subtotal_gb(10)
16040   dim tg(11)
16060   dim z$*10,e$(4)*30,cap$*128
16080   dim t1(5),tc$(5)*14,resp$(20)*256,msgline$(1)*128
16100   dim st1(5)
16120   if env$('ACSDeveloper')<>'' then raw_output=1 ! 
18000   fntop(program$,cap$="Transaction Listing")
18040   fndat(dat$)
18060   ccyymmdd_mask$="3"
18080   dim serviceName$(10)*20,service$(10)*2,tax_code$(10)*2,penalty$(10)*1,subjectto(10)
18100   fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
18160 ! 
18180   fnreg_read('ubtrlist.date.start',tmp$) : filter_date_start=val(tmp$) conv ignore
18200   fnreg_read('ubtrlist.date.end',tmp$) : filter_date_end=val(tmp$) conv ignore
18220   ! 
18240   fnreg_read('ubtrlist.skip_line_after_account',tmp$) : skip_line_after_account=1 : if tmp$='True' then skip_line_after_account=1 else if tmp$='False' then skip_line_after_account=0
18260   fnreg_read('ubtrlist.print_tbal',tmp$) : print_tbal=1 : if tmp$='True' then print_tbal=1 else if tmp$='False' then print_tbal=0
18280   fnreg_read('ubtrlist.sequence',tmp$) : seq=1 : if tmp$='True' then seq=1 else if tmp$='False' then seq=0
18300   ! 
18320   fnreg_read('ubtrlist.include_zero_balance_accounts',tmp$) : include_zero_balance_accounts=1 : include_zero_balance_accounts=val(tmp$) conv ignore
18340   fnreg_read('ubtrlist.include_no_activity_accounts',tmp$) : include_no_activity_accounts=1 : include_no_activity_accounts=val(tmp$) conv ignore
18360   ! 
18990 ! /r
20000 SCREEN1: ! r:
20020 fnTos(sn$='TrList')
20040 mylen=36 : mypos=mylen+2
20060 fnLbl(1,1,"Report Heading Date:",mylen,1,0)
20080 fnTxt(1,mypos,20)
20100 resp$(1)=dat$
20120 fnLbl(2,1,"Starting Date (blank for all):",mylen,1)
20140 fnTxt(2,mypos,10,0,1,ccyymmdd_mask$,0,"Usually the first day of the month, but it can be the beginning of any time period.")
20160 resp$(2)=str$(filter_date_start)
20180 fnLbl(3,1,"Ending Date (blank for all):",mylen,1)
20200 fnTxt(3,mypos,10,0,1,ccyymmdd_mask$,0,"Usually the Last day of the month, but it can be the end of any time period.")
20220 resp$(3)=str$(filter_date_end)
20240 ! fnLbl(5,2,"Note: Use CCYYMMDD format for all dates",50)
20260 fnFra(6,1,2,60,"Choose Balance To Be Printed","You can pr the current balance or the balance as of the ending date selected abov.")
20280 fnOpt(1,3,"Use the actual current balance",0,1)
20300 resp$(4)="True"
20320 fnOpt(2,3,"Use the balance as of the ending date",0,1)
20340 resp$(5)="False"
20360 fnFra(11,1,2,60,"Choose Order for Printing","You can pr in account order or in route sequence with subtotals.")
20380 fnOpt(1,3,"Account Sequence ",0,2)
20400 if seq=1 then resp$(6)="True" else resp$(6)='False'
20420 fnOpt(2,3,"Route Sequence",0,2)
20440 resp_seq=7
20460 if seq=2 then resp$(resp_seq)="True" else resp$(resp_seq)='False'
20480 fnChk(16,3,"Skip line after each account", 0,0) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
20500 resp_skip_line=8
20520 if skip_line_after_account then resp$(resp_skip_line)='True' else resp$(resp_skip_line)='False'
20540 fnChk(17,3,"Include Accounts with Zero Balances", 0,0) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
20560 resp_zero_balance=9
20580 if include_zero_balance_accounts then resp$(resp_zero_balance)='True' else resp$(resp_zero_balance)='False'
20600 fnChk(18,3,"Include Accounts without no activity", 0,0) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
20620 resp_no_activity=10
20640 if include_no_activity_accounts then resp$(resp_no_activity)='True' else resp$(resp_no_activity)='False'
20660 fnCmdSet(3)
20990 fnAcs(sn$,0,mat resp$,ckey)
22000 if ckey=5 then goto XIT
22020 dat$=resp$(1)
22040 filter_date_start=val(resp$(2))
22060 filter_date_end=val(resp$(3))
22080 if resp$(4)="True" then print_tbal=1
22100 if resp$(5)="True" then print_tbal=2
22120 if resp$(6)="True" then seq=1
22140 if resp$(resp_seq)="True" then seq=2
22160 if resp$(resp_skip_line)='True' then skip_line_after_account=1 else skip_line_after_account=0
22180 if resp$(resp_zero_balance)='True' then include_zero_balance_accounts=1 else include_zero_balance_accounts=0
22200 if resp$(resp_no_activity)='True' then include_no_activity_accounts=1 else include_no_activity_accounts=0
22990 ! 
23000 if filter_date_start>filter_date_end and filter_date_start>0 and filter_date_end>0 then 
23020   mat msgline$(1)
23040   msgline$(1)="Ending Date Before Starting Date!"
23060   fnmsgbox(mat msgline$,resp$,cap$,48)
23080   goto SCREEN1
23100 end if 
23990 ! 
24000 fndat(dat$,put=2)
24020 fnreg_write('ubtrlist.date.start',str$(filter_date_start))
24040 fnreg_write('ubtrlist.date.end',str$(filter_date_end))
24060 fnreg_write('ubtrlist.print_tbal',str$(print_tbal))
24080 fnreg_write('ubtrlist.sequence',str$(seq))
24100 fnreg_write('ubtrlist.skip_line_after_account',resp$(resp_skip_line))
24120 fnreg_write('ubtrlist.include_zero_balance_accounts',str$(include_zero_balance_accounts))
24140 fnreg_write('ubtrlist.include_no_activity_accounts',str$(include_no_activity_accounts))
24160 ! /r
26000 ! on fkey 5 goto DONE
26020 fnopenprn
26040 if seq=1 then 
26060   open #h_customer=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
26080 else 
26100   open #h_customer=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed 
26120 end if 
26140 open #ubtransvb=2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
26160 gosub HDR
27000 do 
28000 READ_CUSTOMER: ! r: report main loop
28020   holdroute=route : tdate=0
28040   read #h_customer,using 'Form POS 1,C 10,POS 41,C 30,POS 292,PD 4.2,POS 388,10*PD 5.2,POS 1741,N 2': z$,e$(2),bal,mat gb,route eof EO_CUSTOMER
28060   if seq=2 and holdroute<>0 and holdroute<>route then gosub PRINT_SUB_TOTALS ! consider subtotals
28080   noneprinted=0
28100   foot$=""
28120   if v=0 then v=route ! else If V<>ROUTE Then Gosub 1630  ! 001630 return      !  must change Index to indx5 to change to route seq
28140   t9=9
28150   q5=9
28160   firstone=2: lastone=2: begbal=0
28180   if print_tbal=2 then gosub DETERMINE_CURRRENT_BALANCE
28200   gosub DETERMINE_BEGINNING_BALANCE
28220   mat tgb=tgb+gb : mat ggb=ggb+gb : mat ggb=ggb+gb : mat subtotal_gb=subtotal_gb+gb
28230 ! tdate=0
28240   restore #ubtransvb,key>=z$&"         ": nokey TRANS_NOKEY
28260   lastone=0: firstone=1 : have_tbal=0
30000 READ_UBTRANSVB: ! 
30002 ! tdate=0
30020   read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof READ_CUSTOMER
30040   if p$=z$ and tbal<>0 then have_tbal=1 ! try to see if only transactions on customer were when converted and transaction balances were not set
30050   if p$<>z$ then tdate=0
30060   if p$<>z$ and noneprinted=0 then tamount=0 ! no transactions found
30080   if p$<>z$ and firstone=1 then lastone=1 : goto TRANS_EO_CUSTOMER ! no transactions
30100   if p$<>z$ then firstone=2 : lastone=2 : goto TRANS_EO_CUSTOMER ! no transactions
32000 TEST_TRANS: ! 
32020   if filter_date_end<>0 and tdate>filter_date_end then goto READ_UBTRANSVB
32040   if filter_date_start<>0 and tdate<filter_date_start then goto READ_UBTRANSVB
32042 !                              if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pause
32060   testp$=""
32080   read #ubtransvb,using 'Form POS 1,C 10,n 8': testp$,testtdate eof ignore
32100   if testp$<>p$ then lastone=1
32120   if filter_date_end>0 and testtdate>filter_date_end then lastone=1
32140   gosub PRINT_INFO
32160   firstone=0
32180   if lastone=1 then 
32200     goto READ_CUSTOMER
32220   else 
32240     reread #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gr,tbal,pcode eof READ_CUSTOMER
32260   end if 
32280   goto TEST_TRANS
32300 !   ----------      pr 'nothing hits this line!!!' : pause ! nothing hits this line!!!
33000 TRANS_NOKEY: ! r:
33020   tdate=0 !                               if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pr 'trans_nokey' : pause
33040   if include_no_activity_accounts and (bal<>0 or (bal=0 and include_zero_balance_accounts)) then 
33060     gosub PRINT_INFO ! gosub PRINT_INFO ! If BAL<>0 Then Gosub PRINT_INFO ! no transactions KJ
33080   end if 
33100   goto READ_CUSTOMER
33990 ! /r
34000 TRANS_EO_CUSTOMER: ! 
34020 !                              if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pr 'trans_EO_customer' : pause
34040   if bal<>0 or (bal=0 and include_zero_balance_accounts) then gosub PRINT_INFO ! gosub PRINT_INFO ! If BAL<>0 Then Gosub PRINT_INFO ! no transactions KJ
34060 loop 
34080 ! 
36000 EO_CUSTOMER: ! 
36020 q9=9
36040 if t9<>0 then gosub ACCUM_TOTALS
36060 if seq=2 then gosub PRINT_SUB_TOTALS
36080 gosub PRINT_TOTALS
36100 close #h_customer: 
36120 close #ubtransvb: 
36990 goto DONE ! /r Goto PRINT_GRAND_TOTALS  ! can't get totals by route in Account sequence
38000 HDR: ! r:
38020 pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
38040 pr #255: "\qc {\f181 \fs24 \b UB Transaction Listing}"
38060 pr #255: "\qc {\f181 \fs24 \b "&dat$&"}"
38080 if filter_date_start<>0 and filter_date_end<>0 then 
38100   pr #255: "\qc {\f181 \fs18 \b "&trim$("From "&cnvrt$("pic(zzzz/zz/zz)",filter_date_start)&" to "&cnvrt$("pic(zzzz/zz/zz)",filter_date_end))&"}"
38120 end if 
38140 pr #255,using 'Form POS 1,C 20,POS 107,C 12': "\ql","Page "&str$(p2+=1)
38160 pr #255: tab(58);"Beginning";tab(106);"Current"
38180 pr #255: "           {\ul Customer Name}                 {\ul    Date    }     {\ul  Balance }        {\ul    Debits}   {\ul    Credits}      {\ul    Balance}"
38200 return  ! /r
42000 PRINT_INFO: !  r: If TAMOUNT=0 Then Goto 1460
42020 if tcode<1 or tcode>5 then tcode=1 ! default to charge if no transaction code exits
42040 t1(tcode)=t1(tcode)+tamount
42060 st1(tcode)=st1(tcode)+tamount
42080 ! if q5=9 then goto L1230
42100 ! goto L1230
42120 ! pr #255: newpage
42140 ! gosub HDR
42160 ! L1230: !
42180 q5=0
42200 if tcode=1 then code$=" CHG": pos2=69 : r2=r2+tamount ! COLUMN 2
42220 if tcode=2 then code$=" PN" : pos2=69 : r2=r2+tamount !  COLUMN 2
42240 if tcode=3 then code$=" COL": pos2=84 : r3=r3+tamount ! COLUMN 3
42260 if tcode=4 then code$=" CM" : pos2=84 : r3=r3+tamount ! COLUMN 3
42280 if tcode=5 then code$=" DM" : pos2=69 : r2=r2+tamount ! COLUMN 2
42300 if firstone=1 and lastone=0 then ! first transaction and customer has more than 1 transaction
42320   pr #255,using 'Form POS 1,C 10,POS 12,C 30,POS 43,PIC(ZZZZ/ZZ/ZZ),POS 53,PIC(ZZ,ZZZ,ZZ#.## CR),POS POS2,N 12.2,C 4': z$,e$(2),tdate,begbal,tamount,code$ pageoflow PGOF
42340 else if lastone=2 and firstone=2 then ! No Transactions
42360   pr #255,using 'Form POS 1,C 10,POS 12,C 30,POS 53,PIC(ZZ,ZZZ,ZZ#.## CR),POS 100,PIC(ZZ,ZZZ,ZZ#.## CR),C 16': z$,e$(2),begbal,bal,foot$ pageoflow PGOF
42380 else if firstone=0 and lastone=0 then ! Not the First nor the Last Transaction
42400   pr #255,using 'Form POS 43,PIC(ZZZZ/ZZ/ZZ),POS POS2,N 12.2,C 4': tdate,tamount,code$ pageoflow PGOF
42420 else if firstone=0 and lastone=1 then ! Last Transaction of a series
42440   pr #255,using 'Form POS 43,PIC(ZZZZ/ZZ/ZZ),POS POS2,N 12.2,C 4,POS 100,PIC(ZZ,ZZZ,ZZ#.## CR),C 16': tdate,tamount,code$,bal,foot$ pageoflow PGOF
42460   if skip_line_after_account then pr #255: "" pageoflow PGOF
42480 else if firstone=1 and lastone=1 then ! Only One Transaction
42490 ! pr #255: "FIRST AND LAST" :   if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pr 'tdate=';tdate : pause
42500   pr #255,using 'Form POS 1,C 10,POS 12,C 30,POS 43,PIC(ZZZZ/ZZ/ZZ),POS 53,PIC(ZZ,ZZZ,ZZ#.## CR),POS POS2,N 12.2,C 4,POS 100,PIC(ZZ,ZZZ,ZZ#.## CR),C 16': z$,e$(2),tdate,begbal,tamount,code$,bal,foot$ pageoflow PGOF
42520   if skip_line_after_account then pr #255: "" pageoflow PGOF
42540 end if 
42560 if lastone=1 or lastone=2 then gosub ACCUM_TOTALS
42580 ! pr #40,Using "form pos 1,c 11,n 12.2": Z$,S4
42600 ! pr #255,Using "form pos 1,c 11,n 12.2": Z$,S4
42620 noneprinted=1
42640 return  ! /r
44000 !    BEGBAL: ! r: (UNUSED)  determine beginning balance
44020 !    if lastone=2 and firstone=2 and (tcode=1 or tcode=2 or tcode=5) then ! no trans
44040 !      begbal=bal : r1+=begbal
44060 !    end if
44080 !    if lastone=2 and firstone=2 and (tcode=3 or tcode=4) then ! no trans
44100 !      begbal=bal : r1+=begbal
44120 !    end if
44140 !    if firstone=1 and lastone=0 and (tcode=1 or tcode=2 or tcode=5) then ! first transaction
44160 !      begbal=bal-tamount : r1+=begbal
44180 !    end if
44200 !    if firstone=1 and lastone=0 and (tcode=3 or tcode=4) then ! first transaction, but more
44220 !      begbal=bal+tamount : r1+=begbal
44240 !    end if
44260 !    if firstone=1 and lastone=1 and (tcode=1 or tcode=2 or tcode=5) then ! first transaction and only one
44280 !      begbal=bal-tamount : r1+=begbal
44300 !    end if
44320 !    if firstone=1 and lastone=1 and (tcode=3 or tcode=4) then ! first transaction and only one with collections or credit memos
44340 !      begbal=bal+tamount : r1+=begbal
44360 !    end if
44380 !    return  ! /r
46000 PGOF: ! r:
46020 if ~raw_output then 
46040   pr #255: newpage
46060   gosub HDR
46080 end if 
46100 continue  ! /r
48000 ACCUM_TOTALS: ! r:
48020 s1+=r1
48040 s2+=r2
48060 s3+=r3
48080 s4+=bal
48100 st1+=r1
48120 st2+=r2
48140 st3+=r3
48160 st4+=bal
48180 r1=r2=r3=t9=0
48200 return  ! /r
50000 PRINT_TOTALS: ! r:
50020 pr #255: ""
50040 pr #255: ""
50060 pr #255,using 'Form POS 25,C 23,Nz 3,POS 53,N 13.2,POS 68,N 13.2,POS 84,N 12.2,POS 100,N 13.2': "Totals                ",0,s1,s2,s3,s4 pageoflow PGOF
50080 pr #255: ""
50100 pr #255,using "form pos 1,c 40": "Balance Breakdown by Type of Service:"
50120 for j=1 to 10
50140   if trim$(serviceName$(j))<>"" then 
50160     pr #255,using 'Form POS 5,C 30,N 10.2': serviceName$(j),tgb(j) pageoflow PGOF
50180   end if 
50200   bdtotal+=tgb(j)
50220 next j
50240 pr #255,using "form pos 5,c 30,n 10.2": "Total Breakdown",bdtotal
50260 !  grand_total_a1+=s1
50280 !  grand_total_a2+=s2
50300 !  grand_total_a3+=s3
50320 !  grand_total_a4+=s4
50340 s1=s2=s3=s4=0
50360 mat tgb=(0)
50380 if q9<>9 and ~raw_output then pr #255: newpage : gosub HDR
50400 return  ! /r
52000 PRINT_SUB_TOTALS: ! r:
52020 pr #255: ""
52040 pr #255: ""
52060 pr #255,using 'Form POS 34,C 16,POS 53,N 13.2,POS 68,N 13.2,POS 84,N 12.2,POS 100,N 13.2': "Sub-Totals",st1,st2,st3,st4 pageoflow PGOF
52080 pr #255: ""
52100 st1=st2=st3=st4=0
52120 for j=1 to 10
52140   if trim$(serviceName$(j))<>"" then 
52160     pr #255,using 'Form POS 5,C 30,N 10.2': serviceName$(j),subtotal_gb(j) pageoflow PGOF
52180   end if 
52200 next j
52220 mat subtotal_gb=(0)
52240 pr #255: "    ______________________________  __________"
52260 tc$(1)="Charges"
52280 tc$(2)="Penalties"
52300 tc$(3)="Collections"
52320 tc$(4)="Credit Memos"
52340 tc$(5)="Debit Memos"
52360 for j=1 to 5
52380   pr #255,using 'Form POS 5,C 25,N 15.2': tc$(j),st1(j) pageoflow PGOF
52400 next j
52420 mat st1=(0)
52440 return  ! /r
54000 !     PRINT_GRAND_TOTALS: ! r: (USUSED)
54020 !     if ~raw_output then pr #255: newpage
54040 !     gosub HDR
54060 !     pr #255: ""
54080 !     pr #255: ""
54100 !     pr #255,using 'Form POS 34,C 16,POS 53,N 13.2,POS 68,N 13.2,POS 84,N 12.2,POS 100,N 13.2': "Grand Totals",grand_total_a1,grand_total_a2,grand_total_a3,grand_total_a4 pageoflow PGOF
54120 !     pr #255: ""
54140 !     for j=1 to 10
54160 !       if trim$(serviceName$(j))<>"" then
54180 !         pr #255,using 'Form POS 5,C 30,N 10.2': serviceName$(j),ggb(j) pageoflow PGOF
54200 !       end if
54220 !     next j
54240 !     pr #255: "    ______________________________  __________"
54260 !     tc$(1)="Charges"
54280 !     tc$(2)="Penalties"
54300 !     tc$(3)="Collections"
54320 !     tc$(4)="Credit Memos"
54340 !     tc$(5)="Debit Memos"
54360 !     for j=1 to 5
54380 !       pr #255,using 'Form POS 5,C 25,N 15.2': tc$(j),t1(j) pageoflow PGOF
54400 !     next j
54420 !     goto DONE ! /r
56000 DONE: ! 
56020 close #ubtransvb: ioerr ignore
56040 fncloseprn
56060 XIT: fnxit
58000 IGNORE: continue 
58020 ! <Updateable Region: ERTN>
58040 ERTN: fnerror(program$,err,line,act$,"xit")
58060 if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
58080 execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
58100 pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
58120 ERTN_EXEC_ACT: execute act$ : goto ERTN
58140 ! /region
60000 DETERMINE_CURRRENT_BALANCE: !  r: determine current balance by subtracting or adding any transactions with a later date than the highest transaction date entered.
60020 restore #ubtransvb,key>=z$&"         ": nokey L2140
60040 READ_UBTRANSVB2: ! 
60060 read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2140
60080 ! If TRIM$(Z$)="100018.00" Then Pause
60100 if p$<>z$ then goto L2140
60120 if filter_date_end<>0 and tdate>filter_date_end then goto L2080
60140 goto READ_UBTRANSVB2
60160 L2080: if tcode=1 and print_tbal=2 then bal=bal-tamount : gosub FIX_BALANCE_BREAKDOWN ! subtract any changes out of current balance to get current balance to show on report
60180 if tcode=2 and print_tbal=2 then bal=bal-tamount : gosub FIX_BALANCE_BREAKDOWN ! subtract any penalties out of current balance to get current balance to show on report
60200 if tcode=3 and print_tbal=2 then bal=bal+tamount : gosub FIX_BALANCE_BREAKDOWN ! add any collections back in current balance to get current balance to show on report
60220 if tcode=4 and print_tbal=2 then bal=bal+tamount : gosub FIX_BALANCE_BREAKDOWN ! add any credit memos back in current balance to get current balance to show on report
60240 if tcode=5 and print_tbal=2 then bal=bal-tamount : gosub FIX_BALANCE_BREAKDOWN ! subtract any debit memo back out of current balance to get current balance to show on report
60260 goto READ_UBTRANSVB2
60280 L2140: return  ! /r
62000 DETERMINE_BEGINNING_BALANCE: ! r:
62020 ! If TRIM$(Z$)="100018.00" Then Pause
62040 begbal=bal
62060 restore #ubtransvb,key>=z$&"         ": nokey L2290
62080 L2190: read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2290
62100 if p$<>z$ then goto L2290
62120 ! If TRIM$(Z$)="100018.00" Then Pause
62140 if tdate>=filter_date_start and tdate<=filter_date_end then goto L2230 else goto L2280
62160 L2230: if tcode=1 then begbal=begbal-tamount ! subtract any changes out of current begbalance to get current begbalance to show on report
62180 if tcode=2 then begbal=begbal-tamount ! subtract any pealties out of current begbalance to get current begbalance to show on report
62200 if tcode=3 then begbal=begbal+tamount ! add any collections back in current begbalance to get current begbalance to show on report
62220 if tcode=4 then begbal=begbal+tamount ! add any credit memos back in current begbalance to get current begbalance to show on report
62240 if tcode=5 then begbal=begbal-tamount ! subtract any debit memo back out of current begbalance to get current begbalance to show on report
62260 L2280: goto L2190
62280 L2290: r1+=begbal: return 
62300 FIX_BALANCE_BREAKDOWN: ! fix balance breakdown so it matches balance when something other than current balance is chosen
62320 ! If TRIM$(Z$)="100130.00" Then Pause
62340 if tcode>2 then goto L2360
62360 for j1=1 to 10
62380   if penalty$(j1)="Y" and tcode=1 then tg(j1)=0 ! don't add or subtract penalties on charge record.
62400   if penalty$(j1)="N" and tcode=2 then tg(j1)=0 ! zero any other fields but the penalties
62420 next j1
62440 L2360: for j=1 to 10
62460   if tcode=1 then gb(j)=gb(j)-tg(j)
62480   if tcode=2 then gb(j)=gb(j)-tg(j)
62500   if tcode=3 then gb(j)=gb(j)+tg(j)
62520   if tcode=4 then gb(j)=gb(j)+tg(j)
62540   if tcode=5 then gb(j)=gb(j)-tg(j)
62560 next j
62580 ! pr MAT GB: Pause
62600 ! If SUM(GB)<>BAL Then Pause
62620 return  ! /r
