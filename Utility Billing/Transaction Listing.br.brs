! formerly S:\acsUB\ubTrList
	! debug_account_of_interest$='100416.10'
! r: NOTES, dims, defaults, constants, libraries, on err, etc
! -- Transaction Listing
! known problem - If transaction is actually process within the date range but the date is a wrong date outside the date range, the beginning balance the next period will not agree with the ending balance from the previous month. Don't know what to do about it!!
! code descriptions
! firstone=2   lastone=2   !no transactions
! firstone=1   lastone=0   ! 1st but more transactions exist
! firstone=0   lastone=1   ! more than one trans and this is last one
! firstone=1   lastone=1   ! just one transaction
! firstone=0   lastone=0   ! not first and not last
!
	autoLibrary
	on error goto Ertn
	dim p$*10,foot$*16,gb(10),tgb(10),ggb(10),dat$*20
	dim subtotal_gb(10)
	dim tg(11)
	dim z$*10,e$(4)*30
	dim t1(5),tc$(5)*14,resp$(20)*256,msgline$(1)*128
	dim st1(5)
	if env$('ACSDeveloper')<>'' then raw_output=1
	fnTop(program$)
	fndat(dat$)
	ccyymmdd_mask$="3"
	dim serviceName$(10)*20,service$(10)*2,tax_code$(10)*2,penalty$(10)*1,subjectto(10)
	fnGetServices(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto)
!
	fnreg_read('ubtrlist.date.start',tmp$) : filter_date_start=val(tmp$) conv ignore
	fnreg_read('ubtrlist.date.end',tmp$) : filter_date_end=val(tmp$) conv ignore
	!
	fnreg_read('ubtrlist.skip_line_after_account',tmp$) : skip_line_after_account=1 : if tmp$='True' then skip_line_after_account=1 else if tmp$='False' then skip_line_after_account=0
	fnreg_read('ubtrlist.print_tbal',tmp$) : print_tbal=1 : if tmp$='True' then print_tbal=1 else if tmp$='False' then print_tbal=0
	fnreg_read('ubtrlist.sequence',tmp$) : seq=1 : if tmp$='True' then seq=1 else if tmp$='False' then seq=0
	!
	fnreg_read('ubtrlist.include_zero_balance_accounts',tmp$) : include_zero_balance_accounts=1 : include_zero_balance_accounts=val(tmp$) conv ignore
	fnreg_read('ubtrlist.include_no_activity_accounts',tmp$) : include_no_activity_accounts=1 : include_no_activity_accounts=val(tmp$) conv ignore
	!
! /r
SCREEN1: ! r:
fnTos(sn$='TrList')
mylen=36 : mypos=mylen+2
fnLbl(1,1,"Report Heading Date:",mylen,1,0)
fnTxt(1,mypos,20)
resp$(1)=dat$
fnLbl(2,1,"Starting Date (blank for all):",mylen,1)
fnTxt(2,mypos,10,0,1,ccyymmdd_mask$,0,"Usually the first day of the month, but it can be the beginning of any time period.")
resp$(2)=str$(filter_date_start)
fnLbl(3,1,"Ending Date (blank for all):",mylen,1)
fnTxt(3,mypos,10,0,1,ccyymmdd_mask$,0,"Usually the Last day of the month, but it can be the end of any time period.")
resp$(3)=str$(filter_date_end)
! fnLbl(5,2,"Note: Use CCYYMMDD format for all dates",50)
fnFra(6,1,2,60,"Choose Balance To Be Printed","You can pr the current balance or the balance as of the ending date selected abov.")
fnOpt(1,3,"Use the actual current balance",0,1)
resp$(4)="True"
fnOpt(2,3,"Use the balance as of the ending date",0,1)
resp$(5)="False"
fnFra(11,1,2,60,"Choose Order for Printing","You can pr in account order or in route sequence with subtotals.")
fnOpt(1,3,"Account Sequence ",0,2)
if seq=1 then resp$(6)="True" else resp$(6)='False'
fnOpt(2,3,"Route Sequence",0,2)
resp_seq=7
if seq=2 then resp$(resp_seq)="True" else resp$(resp_seq)='False'
fnChk(16,3,"Skip line after each account", 0,0) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
resp_skip_line=8
if skip_line_after_account then resp$(resp_skip_line)='True' else resp$(resp_skip_line)='False'
fnChk(17,3,"Include Accounts with Zero Balances", 0,0) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
resp_zero_balance=9
if include_zero_balance_accounts then resp$(resp_zero_balance)='True' else resp$(resp_zero_balance)='False'
fnChk(18,3,"Include Accounts without no activity", 0,0) ! fnChk(lyne,ps,txt$*196; align,contain,tabcon)
resp_no_activity=10
if include_no_activity_accounts then resp$(resp_no_activity)='True' else resp$(resp_no_activity)='False'
fnCmdSet(3)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
dat$=resp$(1)
filter_date_start=val(resp$(2))
filter_date_end=val(resp$(3))
if resp$(4)="True" then print_tbal=1
if resp$(5)="True" then print_tbal=2
if resp$(6)="True" then seq=1
if resp$(resp_seq)="True" then seq=2
if resp$(resp_skip_line)='True' then skip_line_after_account=1 else skip_line_after_account=0
if resp$(resp_zero_balance)='True' then include_zero_balance_accounts=1 else include_zero_balance_accounts=0
if resp$(resp_no_activity)='True' then include_no_activity_accounts=1 else include_no_activity_accounts=0
!
if filter_date_start>filter_date_end and filter_date_start>0 and filter_date_end>0 then
	mat msgline$(1)
	msgline$(1)="Ending Date Before Starting Date!"
	fnmsgbox(mat msgline$,resp$,'',48)
	goto SCREEN1
end if
!
fndat(dat$,put=2)
fnreg_write('ubtrlist.date.start',str$(filter_date_start))
fnreg_write('ubtrlist.date.end',str$(filter_date_end))
fnreg_write('ubtrlist.print_tbal',str$(print_tbal))
fnreg_write('ubtrlist.sequence',str$(seq))
fnreg_write('ubtrlist.skip_line_after_account',resp$(resp_skip_line))
fnreg_write('ubtrlist.include_zero_balance_accounts',str$(include_zero_balance_accounts))
fnreg_write('ubtrlist.include_no_activity_accounts',str$(include_no_activity_accounts))
! /r
! on fkey 5 goto DONE
fnopenprn
if seq=1 then
	open #h_customer=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
else
	open #h_customer=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k
end if
open #ubtransvb=2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",i,i,k
gosub HDR
do
READ_CUSTOMER: ! r: report main loop
	holdroute=route : tdate=0
	read #h_customer,using 'Form POS 1,C 10,POS 41,C 30,POS 292,PD 4.2,POS 388,10*PD 5.2,POS 1741,N 2': z$,e$(2),bal,mat gb,route eof EO_CUSTOMER
	if seq=2 and holdroute<>0 and holdroute<>route then gosub PRINT_SUB_TOTALS ! consider subtotals
	noneprinted=0
	foot$=""
	if v=0 then v=route ! else If V<>ROUTE Then Gosub 1630  ! 001630 return      !  must change Index to indx5 to change to route seq
	t9=9
	q5=9
	firstone=2: lastone=2: begbal=0
	if print_tbal=2 then gosub DETERMINE_CURRRENT_BALANCE
	gosub DETERMINE_BEGINNING_BALANCE
	mat tgb=tgb+gb : mat ggb=ggb+gb : mat ggb=ggb+gb : mat subtotal_gb=subtotal_gb+gb
! tdate=0
	restore #ubtransvb,key>=z$&"         ": nokey TRANS_NOKEY
	lastone=0: firstone=1 : have_tbal=0
READ_UBTRANSVB: !
! tdate=0
	read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof READ_CUSTOMER
	if p$=z$ and tbal<>0 then have_tbal=1 ! try to see if only transactions on customer were when converted and transaction balances were not set
	if p$<>z$ then tdate=0
	if p$<>z$ and noneprinted=0 then tamount=0 ! no transactions found
	if p$<>z$ and firstone=1 then lastone=1 : goto TRANS_EO_CUSTOMER ! no transactions
	if p$<>z$ then firstone=2 : lastone=2 : goto TRANS_EO_CUSTOMER ! no transactions
TEST_TRANS: !
	if filter_date_end<>0 and tdate>filter_date_end then goto READ_UBTRANSVB
	if filter_date_start<>0 and tdate<filter_date_start then goto READ_UBTRANSVB
!                              if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pause
	testp$=""
	read #ubtransvb,using 'Form POS 1,C 10,n 8': testp$,testtdate eof ignore
	if testp$<>p$ then lastone=1
	if filter_date_end>0 and testtdate>filter_date_end then lastone=1
	gosub PRINT_INFO
	firstone=0
	if lastone=1 then
		goto READ_CUSTOMER
	else
		reread #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gr,tbal,pcode eof READ_CUSTOMER
	end if
	goto TEST_TRANS
!   ----------      pr 'nothing hits this line!!!' : pause ! nothing hits this line!!!
TRANS_NOKEY: ! r:
	tdate=0 !                               if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pr 'trans_nokey' : pause
	if include_no_activity_accounts and (bal<>0 or (bal=0 and include_zero_balance_accounts)) then
		gosub PRINT_INFO ! gosub PRINT_INFO ! If BAL<>0 Then Gosub PRINT_INFO ! no transactions KJ
	end if
	goto READ_CUSTOMER
! /r
TRANS_EO_CUSTOMER: !
!                              if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pr 'trans_EO_customer' : pause
	if bal<>0 or (bal=0 and include_zero_balance_accounts) then gosub PRINT_INFO ! gosub PRINT_INFO ! If BAL<>0 Then Gosub PRINT_INFO ! no transactions KJ
loop
 
EO_CUSTOMER: !
q9=9
if t9<>0 then gosub ACCUM_TOTALS
if seq=2 then gosub PRINT_SUB_TOTALS
gosub PRINT_TOTALS
close #h_customer:
close #ubtransvb:
goto DONE ! /r Goto PRINT_GRAND_TOTALS  ! can't get totals by route in Account sequence
HDR: ! r:
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc {\f181 \fs24 \b UB Transaction Listing}"
	pr #255: "\qc {\f181 \fs24 \b "&dat$&"}"
	if filter_date_start<>0 and filter_date_end<>0 then
		pr #255: "\qc {\f181 \fs18 \b "&trim$("From "&cnvrt$("pic(zzzz/zz/zz)",filter_date_start)&" to "&cnvrt$("pic(zzzz/zz/zz)",filter_date_end))&"}"
	end if
	pr #255,using 'Form POS 1,C 20,POS 107,C 12': "\ql","Page "&str$(p2+=1)
	pr #255: tab(58);"Beginning";tab(106);"Current"
	pr #255: "           {\ul Customer Name}                 {\ul    Date    }     {\ul  Balance }        {\ul    Debits}   {\ul    Credits}      {\ul    Balance}"
return  ! /r
PRINT_INFO: !  r: If TAMOUNT=0 Then Goto 1460
	if tcode<1 or tcode>5 then tcode=1 ! default to charge if no transaction type exits
	t1(tcode)=t1(tcode)+tamount
	st1(tcode)=st1(tcode)+tamount
	! if q5=9 then goto L1230
	! goto L1230
	! pr #255: newpage
	! gosub HDR
	! L1230: !
	q5=0
	if tcode=1 then code$=" CHG": pos2=69 : r2=r2+tamount ! COLUMN 2
	if tcode=2 then code$=" PN" : pos2=69 : r2=r2+tamount !  COLUMN 2
	if tcode=3 then code$=" COL": pos2=84 : r3=r3+tamount ! COLUMN 3
	if tcode=4 then code$=" CM" : pos2=84 : r3=r3+tamount ! COLUMN 3
	if tcode=5 then code$=" DM" : pos2=69 : r2=r2+tamount ! COLUMN 2
	if firstone=1 and lastone=0 then ! first transaction and customer has more than 1 transaction
		pr #255,using 'Form POS 1,C 10,POS 12,C 30,POS 43,PIC(ZZZZ/ZZ/ZZ),POS 53,PIC(ZZ,ZZZ,ZZ#.## CR),POS POS2,N 12.2,C 4': z$,e$(2),tdate,begbal,tamount,code$ pageoflow PGOF
	else if lastone=2 and firstone=2 then ! No Transactions
		pr #255,using 'Form POS 1,C 10,POS 12,C 30,POS 53,PIC(ZZ,ZZZ,ZZ#.## CR),POS 100,PIC(ZZ,ZZZ,ZZ#.## CR),C 16': z$,e$(2),begbal,bal,foot$ pageoflow PGOF
	else if firstone=0 and lastone=0 then ! Not the First nor the Last Transaction
		pr #255,using 'Form POS 43,PIC(ZZZZ/ZZ/ZZ),POS POS2,N 12.2,C 4': tdate,tamount,code$ pageoflow PGOF
	else if firstone=0 and lastone=1 then ! Last Transaction of a series
		pr #255,using 'Form POS 43,PIC(ZZZZ/ZZ/ZZ),POS POS2,N 12.2,C 4,POS 100,PIC(ZZ,ZZZ,ZZ#.## CR),C 16': tdate,tamount,code$,bal,foot$ pageoflow PGOF
		if skip_line_after_account then pr #255: "" pageoflow PGOF
	else if firstone=1 and lastone=1 then ! Only One Transaction
	! pr #255: "FIRST AND LAST" :   if env$('ACSDeveloper')<>'' and trim$(z$)=debug_account_of_interest$ then pr 'tdate=';tdate : pause
		pr #255,using 'Form POS 1,C 10,POS 12,C 30,POS 43,PIC(ZZZZ/ZZ/ZZ),POS 53,PIC(ZZ,ZZZ,ZZ#.## CR),POS POS2,N 12.2,C 4,POS 100,PIC(ZZ,ZZZ,ZZ#.## CR),C 16': z$,e$(2),tdate,begbal,tamount,code$,bal,foot$ pageoflow PGOF
		if skip_line_after_account then pr #255: "" pageoflow PGOF
	end if
	if lastone=1 or lastone=2 then gosub ACCUM_TOTALS
	! pr #40,Using "form pos 1,c 11,n 12.2": Z$,S4
	! pr #255,Using "form pos 1,c 11,n 12.2": Z$,S4
	noneprinted=1
return  ! /r
!  BEGBAL: ! r: (UNUSED)  determine beginning balance
!    if lastone=2 and firstone=2 and (tcode=1 or tcode=2 or tcode=5) then ! no trans
!      begbal=bal : r1+=begbal
!    end if
!    if lastone=2 and firstone=2 and (tcode=3 or tcode=4) then ! no trans
!      begbal=bal : r1+=begbal
!    end if
!    if firstone=1 and lastone=0 and (tcode=1 or tcode=2 or tcode=5) then ! first transaction
!      begbal=bal-tamount : r1+=begbal
!    end if
!    if firstone=1 and lastone=0 and (tcode=3 or tcode=4) then ! first transaction, but more
!      begbal=bal+tamount : r1+=begbal
!    end if
!    if firstone=1 and lastone=1 and (tcode=1 or tcode=2 or tcode=5) then ! first transaction and only one
!      begbal=bal-tamount : r1+=begbal
!    end if
!    if firstone=1 and lastone=1 and (tcode=3 or tcode=4) then ! first transaction and only one with collections or credit memos
!      begbal=bal+tamount : r1+=begbal
!    end if
!  return  ! /r
PGOF: ! r:
	if ~raw_output then
		pr #255: newpage
		gosub HDR
	end if
continue  ! /r
ACCUM_TOTALS: ! r:
	s1+=r1
	s2+=r2
	s3+=r3
	s4+=bal
	st1+=r1
	st2+=r2
	st3+=r3
	st4+=bal
	r1=r2=r3=t9=0
return  ! /r
PRINT_TOTALS: ! r:
	pr #255: ""
	pr #255: ""
	pr #255,using 'Form POS 25,C 23,Nz 3,POS 53,N 13.2,POS 68,N 13.2,POS 84,N 12.2,POS 100,N 13.2': "Totals                ",0,s1,s2,s3,s4 pageoflow PGOF
	pr #255: ""
	pr #255,using "form pos 1,c 40": "Balance Breakdown by Type of Service:"
	for j=1 to 10
		if trim$(serviceName$(j))<>"" then
			pr #255,using 'Form POS 5,C 30,N 10.2': serviceName$(j),tgb(j) pageoflow PGOF
		end if
		bdtotal+=tgb(j)
	next j
	pr #255,using "form pos 5,c 30,n 10.2": "Total Breakdown",bdtotal
	!  grand_total_a1+=s1
	!  grand_total_a2+=s2
	!  grand_total_a3+=s3
	!  grand_total_a4+=s4
	s1=s2=s3=s4=0
	mat tgb=(0)
	if q9<>9 and ~raw_output then pr #255: newpage : gosub HDR
return  ! /r
PRINT_SUB_TOTALS: ! r:
	pr #255: ""
	pr #255: ""
	pr #255,using 'Form POS 34,C 16,POS 53,N 13.2,POS 68,N 13.2,POS 84,N 12.2,POS 100,N 13.2': "Sub-Totals",st1,st2,st3,st4 pageoflow PGOF
	pr #255: ""
	st1=st2=st3=st4=0
	for j=1 to 10
		if trim$(serviceName$(j))<>"" then
			pr #255,using 'Form POS 5,C 30,N 10.2': serviceName$(j),subtotal_gb(j) pageoflow PGOF
		end if
	next j
	mat subtotal_gb=(0)
	pr #255: "    ______________________________  __________"
	tc$(1)="Charges"
	tc$(2)="Penalties"
	tc$(3)="Collections"
	tc$(4)="Credit Memos"
	tc$(5)="Debit Memos"
	for j=1 to 5
		pr #255,using 'Form POS 5,C 25,N 15.2': tc$(j),st1(j) pageoflow PGOF
	next j
	mat st1=(0)
return  ! /r
!   PRINT_GRAND_TOTALS: ! r: (USUSED)
!     if ~raw_output then pr #255: newpage
!     gosub HDR
!     pr #255: ""
!     pr #255: ""
!     pr #255,using 'Form POS 34,C 16,POS 53,N 13.2,POS 68,N 13.2,POS 84,N 12.2,POS 100,N 13.2': "Grand Totals",grand_total_a1,grand_total_a2,grand_total_a3,grand_total_a4 pageoflow PGOF
!     pr #255: ""
!     for j=1 to 10
!       if trim$(serviceName$(j))<>"" then
!         pr #255,using 'Form POS 5,C 30,N 10.2': serviceName$(j),ggb(j) pageoflow PGOF
!       end if
!     next j
!     pr #255: "    ______________________________  __________"
!     tc$(1)="Charges"
!     tc$(2)="Penalties"
!     tc$(3)="Collections"
!     tc$(4)="Credit Memos"
!     tc$(5)="Debit Memos"
!     for j=1 to 5
!       pr #255,using 'Form POS 5,C 25,N 15.2': tc$(j),t1(j) pageoflow PGOF
!     next j
!   goto DONE ! /r
DONE: !
close #ubtransvb: ioerr ignore
fncloseprn
Xit: fnXit
 
 
DETERMINE_CURRRENT_BALANCE: !  r: determine current balance by subtracting or adding any transactions with a later date than the highest transaction date entered.
	restore #ubtransvb,key>=z$&"         ": nokey L2140
	READ_UBTRANSVB2: !
	read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2140
	! If TRIM$(Z$)="100018.00" Then Pause
	if p$<>z$ then goto L2140
	if filter_date_end<>0 and tdate>filter_date_end then goto L2080
	goto READ_UBTRANSVB2
	L2080: if tcode=1 and print_tbal=2 then bal=bal-tamount : gosub FIX_BALANCE_BREAKDOWN ! subtract any changes out of current balance to get current balance to show on report
	if tcode=2 and print_tbal=2 then bal=bal-tamount : gosub FIX_BALANCE_BREAKDOWN ! subtract any penalties out of current balance to get current balance to show on report
	if tcode=3 and print_tbal=2 then bal=bal+tamount : gosub FIX_BALANCE_BREAKDOWN ! add any collections back in current balance to get current balance to show on report
	if tcode=4 and print_tbal=2 then bal=bal+tamount : gosub FIX_BALANCE_BREAKDOWN ! add any credit memos back in current balance to get current balance to show on report
	if tcode=5 and print_tbal=2 then bal=bal-tamount : gosub FIX_BALANCE_BREAKDOWN ! subtract any debit memo back out of current balance to get current balance to show on report
	goto READ_UBTRANSVB2
L2140: return  ! /r
DETERMINE_BEGINNING_BALANCE: ! r:
	! If TRIM$(Z$)="100018.00" Then Pause
	begbal=bal
	restore #ubtransvb,key>=z$&"         ": nokey L2290
	L2190: read #ubtransvb,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L2290
	if p$<>z$ then goto L2290
	! If TRIM$(Z$)="100018.00" Then Pause
	if tdate>=filter_date_start and tdate<=filter_date_end then goto L2230 else goto L2280
	L2230: if tcode=1 then begbal=begbal-tamount ! subtract any changes out of current begbalance to get current begbalance to show on report
	if tcode=2 then begbal=begbal-tamount ! subtract any pealties out of current begbalance to get current begbalance to show on report
	if tcode=3 then begbal=begbal+tamount ! add any collections back in current begbalance to get current begbalance to show on report
	if tcode=4 then begbal=begbal+tamount ! add any credit memos back in current begbalance to get current begbalance to show on report
	if tcode=5 then begbal=begbal-tamount ! subtract any debit memo back out of current begbalance to get current begbalance to show on report
	L2280: goto L2190
L2290: r1+=begbal: return
FIX_BALANCE_BREAKDOWN: ! fix balance breakdown so it matches balance when something other than current balance is chosen
	! If TRIM$(Z$)="100130.00" Then Pause
	if tcode>2 then goto L2360
	for j1=1 to 10
		if penalty$(j1)="Y" and tcode=1 then tg(j1)=0 ! don't add or subtract penalties on charge record.
		if penalty$(j1)="N" and tcode=2 then tg(j1)=0 ! zero any other fields but the penalties
	next j1
	L2360: for j=1 to 10
		if tcode=1 then gb(j)=gb(j)-tg(j)
		if tcode=2 then gb(j)=gb(j)-tg(j)
		if tcode=3 then gb(j)=gb(j)+tg(j)
		if tcode=4 then gb(j)=gb(j)+tg(j)
		if tcode=5 then gb(j)=gb(j)-tg(j)
	next j
	! pr MAT GB: Pause
	! If SUM(GB)<>BAL Then Pause
return  ! /r
include: ertn
