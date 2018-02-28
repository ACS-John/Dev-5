10000   fn_setup
10200   fntop(program$,cap$="Check Balance Breakdowns 2")
10600 MENU1: ! r:
10800   fnTos(sn$="bldtrans") : chk_align=0
11000   fnLbl(1,1,"Scan:")
11200   fnChk(2,5,"Scan Customer Balance Breakdowns",chk_align) : resp$(1)="True"
11400   fnChk(3,5,"Scan Transaction Breakdowns",chk_align)      : resp$(2)="True"
11600   fnLbl(5,1,"Error Handling:")
11800   fnChk(6,5,"Report Erroneous Transactions",chk_align)    : resp$(3)="True"
12000   fnChk(7,5,"Fix Erroneous Transactions",chk_align)       : resp$(4)="False"
12100   fnLbl(9,1,"Miscellaneous:")
12200   fnChk(10,5,"Move Credit Balnces to Other",chk_align)    : resp$(5)="False"
12220   fnChk(11,5,"  and apply credits",chk_align)             : resp$(6)="False"
12300   fnCmdSet(2)
12400   fnAcs(sn$,0,mat resp$,ck)
12500   if ck<>5 then 
12600     if resp$(1)='True' then do_fix_balance_breakdowns=1 else do_fix_balance_breakdowns=0
12800     if resp$(2)='True' then do_fix_trans_breakdowns=1 else do_fix_trans_breakdowns=0
13000     if resp$(3)='True' then do_report=1 else do_report=0
13200     if resp$(4)='True' then do_fix=1 else do_fix=0
13300     if resp$(5)='True' then do_move_credit=1 else do_move_credit=0
13320     if resp$(6)='True' then do_apply_credit=1 else do_apply_credit=0
13800     print_count=0
14000     if do_fix_balance_breakdowns then let fn_fix_balance_breakdowns(do_fix,do_report)
14200     if do_fix_trans_breakdowns then let fn_fix_trans_breakdowns(do_fix,do_report)
14300     if do_move_credit then let fn_move_credit(do_move_credit)
14320     if do_apply_credit then let fn_apply_credit_from_other(do_apply_credit)
14380   end if
14400 goto XIT ! /r
14600 XIT: fnxit 
14700 ignore: continue
14800 def fn_setup
15000   if ~setup then 
15200     setup=1
15400     library 'S:\Core\Library': fngethandle,fnStatus,fnget_services
15600     library 'S:\Core\Library': fnerror,fntop,fnxit,fnopenprn,fncloseprn,fnAcs,fnChk,fnCmdSet,fnLbl,fnTos
15800     on error goto ERTN
16000     ! ______________________________________________________________________
16200     dim cap$*128
17400     ! ______________________________________________________________________
17600   end if 
17800 fnend 
18000 ! <Updateable Region: ERTN>
18200 ERTN: fnerror(program$,err,line,act$,"xit")
18400   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
18600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
18800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
19000 ERTN_EXEC_ACT: execute act$ : goto ERTN
19200 ! /region
19400 def fn_report_it(mat report_g,bal_breakdown; heading$*80,col_2_heading$*12)
19600   if do_report then 
19800     if ~setup_report_it then 
20000       setup_report_it=1
20200       fnopenprn
20400       pr #255,using F_HDR1: heading$
20500       pr #255,using F_HDR2: 'Account',col_2_heading$,serviceName$(1)(1:12),serviceName$(2)(1:12),serviceName$(3)(1:12),serviceName$(4)(1:12),serviceName$(5)(1:12),serviceName$(6)(1:12),serviceName$(7)(1:12),serviceName$(8)(1:12),serviceName$(9)(1:12),serviceName$(10)(1:12),'*Calculated*'
20600       F_HDR1: form pos 1,cc 156
20700       F_HDR2: form pos 1,13*(cc 12,',')
20800       F_BODY: form pos 1,c 12,',',12*(n 12.2,',')
21000     end if  ! ~setup_report_it
21200     print_count+=1
21400     pr #255,using F_BODY: z$,bal,report_g(1),report_g(2),report_g(3),report_g(4),report_g(5),report_g(6),report_g(7),report_g(8),report_g(9),report_g(10),bal_breakdown
21600     ! pr #255: z$&' has a balance of '&str$(gb(10))&' but the breakdowns add up to '&str$(bal_breakdown)
21800   end if 
22000 fnend 
22200 def fn_any_gb_negative
22400   agn_return=0
22600   for agn_item=1 to 10
22800     if gb(agn_item)<0 then agn_return=1
23000   next agn_item
23200   fn_any_gb_negative=agn_return
23400 fnend  ! fn_any_gb_negative
23600 ! def library fnfix_balance_breakdowns(do_fix,do_report)
23800 !   fn_setup
24000 !   fnfix_balance_breakdowns=fn_fix_balance_breakdowns(do_fix,do_report)
24200 ! fnend
24400 def fn_fix_balance_breakdowns(do_fix,do_report) ! assumes balance is right, puts the difference into other
24420   fnStatus('Checking Customer Balance Breakdowns')
24500   dim customer_g(10)
24520   dim z$*10,service_rate_code(7)
24530   dim gb(10)
24600   gb_other=fn_service_other
24800   open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
25000   do 
25200     read #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof CUSTOMER_EOF
25400     F_CUSTOMER: form pos 1,c 10,pos 143,7*pd 2,pos 292,pd 4.2,pos 300,10*pd 4.2,pos 388,10*pd 5.2
25600     read_count+=1
25800     for gb_item=1 to udim(mat serviceName$) ! udim(mat gb)
26000       if trim$(serviceName$(gb_item))='' then gb(gb_item)=0
26200     next gb_item
26400     bal_breakdown=sum(gb)
26600     if bal<>bal_breakdown then 
26800       fn_report_it(mat customer_g,bal_breakdown,"Customer Balance Breakdowns",'Balance')
27000       if do_fix then 
27200         gb(gb_other)-=(bal_breakdown-bal)
27400         rewrite #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
27600       end if 
27800     end if 
28100   loop 
28200   CUSTOMER_EOF: ! 
28220 close #h_customer: ioerr ignore
28400   if print_count>0 then let fncloseprn
28600   pr 'print_count=';print_count
28800   setup_report_it=0
29200 fnend 
29400   def library fnservice_other
29600     fn_setup
29800     fnservice_other=fn_service_other
30000   fnend 
30200   def fn_service_other
30400 !  this function returns en enumeration of the OTHER balance breakdown service
30420 !   also returns sz1, mat serviceName$ and mat srv$
30600     dim serviceName$(10)*20,srv$(10)*2
30800     fnget_services(mat serviceName$,mat srv$)
31400     so_return=srch(mat srv$,'OT')
31600     if so_return<=0 then so_return=srch(mat srv$,'OC') ! Other Charge
31700     if so_return<=0 then so_return=srch(mat serviceName$,'Other') ! Other Charge
31800     if so_return<=0 then so_return=srch(mat serviceName$,rpad$('Other',20)) ! Other Charge
32000     if so_return<=0 then 
32200       pr "OT (Other) nor OC (Other Charge) not found in Service Code abbreviations"
32400       pr "(nor was Other found in Service Code names"
32600       so_return=0
32800       pause 
33000     end if 
33500 ! r: get sz1
33520     sz1=0
33540     for j=1 to 10
33560       if trim$(serviceName$(j))<>"" then 
33580         sz1+=1
33600       end if 
33620     next j
33640 ! /r
33660     fn_service_other=so_return
33680   fnend 
34000   def library fnfix_trans_breakdowns(do_fix,do_report)
34020     fn_setup
34040     fnfix_trans_breakdowns=fn_fix_trans_breakdowns(do_fix,do_report)
34060   fnend 
35000   def fn_fix_trans_breakdowns(do_fix,do_report)
35020     dim trans_g(11),ru(6)
35040     gb_other=fn_service_other
35060     fnStatus('Checking Transaction Breakdowns')
35080     if do_fix then 
35100       open #h_trans=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",internal,outIn,relative 
35120     else 
35140       open #h_trans=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",internal,input,relative 
35160     end if 
35180     do 
35200       read #h_trans,using F_TRANS: p$,tdate,transcode,tamt,mat trans_g,mat ru,bal,postcode eof TRANS_EOF
35220 F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
35240       read_count+=1
36000       for g_item=1 to udim(mat serviceName$) ! udim(mat trans_g)
36200         if trim$(serviceName$(g_item))='' then trans_g(g_item)=0
36400       next g_item
36500 ! r: get bal_breakdown (requires sz1)
36520       bal_breakdown=0
36540       for sz1_item=1 to 9 ! sz1
36560         bal_breakdown+=trans_g(sz1_item)
36580       next sz1_item
36600 !      bal_breakdown=sum(trans_g)
36620 ! /r
36800       if tamt<>bal_breakdown then 
37000         fn_report_it(mat trans_g,bal_breakdown,"Transaction Breakdowns",'T Amount')
37200         if do_fix then 
37400           trans_g(gb_other)-=(bal_breakdown-tamt)
37600           rewrite #h_trans,using F_TRANS: p$,tdate,transcode,tamt,mat trans_g,mat ru,bal,postcode
37800         end if 
38000       end if  ! gb(10)<>FICTIONAL_gb10
38200     loop 
38400 TRANS_EOF: ! 
38600     if print_count>0 then let fncloseprn
38800     pr 'print_count=';print_count
39000     setup_report_it=0
39200     close #h_trans: 
39400   fnend  ! fn_fix_balance_breakdowns
42000   def fn_move_credit(do_move_credit)
42020     fnStatus('Moving Credit Balances to Other in Customer Balance Breakdowns')
42040     dim customer_g(10)
42060     dim z$*10,service_rate_code(7)
42080     dim gb(10)
42100     gb_other=fn_service_other
42120     open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
42140     do 
42160       read #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof MC_CUSTOMER_EOF
42200       read_count+=1
42220       if fn_any_gb_negative then 
42240         fn_report_it(mat gb,bal,"Customer Balance Breakdowns Before Credits Moved to Other",'Balance')
42280         for gb_item=1 to 10
42300           if gb_item<>gb_other then 
42320             if gb(gb_item)<0 then 
42340               gb(gb_other)+=gb(gb_item)
42360               gb(gb_item)=0
42380             end if 
42400           end if 
42420         next gb_item
42440         rewrite #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
42460 !       if trim$(z$)='205320.70' then pause !
42480       end if 
42500     loop 
42520 MC_CUSTOMER_EOF: ! 
42540     close #h_customer: 
42560   fnend 
44000   def fn_apply_credit_from_other(do_apply_credit)
44020     fnStatus('Applying Credit Balances (from Other) to the rest of the Customer Balance Breakdowns')
44040     dim customer_g(10)
44060     dim z$*10,service_rate_code(7)
44080     dim gb(10)
44100     gb_other=fn_service_other
44120     open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
44140     do 
44160       read #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof ACFO_CUSTOMER_EOF
44180       read_count+=1
44200       if gb(gb_other)<0 then 
44220         fn_report_it(mat gb,bal,"Customer Balance Breakdowns Before Credits Moved to Other",'Balance')
44240         for gb_item=1 to 10
44260           if gb_item<>gb_other then ! 
44280             if gb(gb_item)=>abs(gb(gb_other)) then ! this item has more (or equal) charge than other has credit
44300               gb(gb_item)+=gb(gb_other)
44320               gb(gb_other)=0
44340               goto ACFO_REC_COMPLETE ! all of other is consumed, we are done here
44360             else if gb(gb_item)<abs(gb(gb_other)) then ! other has more than enough credit to cover this charge
44380               gb(gb_other)+=gb(gb_item)
44400               gb(gb_item)=0
44420             end if 
44440           end if 
44460         next gb_item
44480 ACFO_REC_COMPLETE: ! 
44500         rewrite #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
44520 !       if trim$(z$)='205320.70' then pause !
44540       end if 
44560     loop 
44580 ACFO_CUSTOMER_EOF: ! 
44600     close #h_customer: 
44620   fnend 
