  fn_setup
  fntop(program$,cap$="Check Balance Breakdowns 2")
MENU1: ! r:
  fnTos(sn$="bldtrans") : chk_align=0
  fnLbl(1,1,"Scan:")
  fnChk(2,5,"Scan Customer Balance Breakdowns",chk_align) : resp$(1)="True"
  fnChk(3,5,"Scan Transaction Breakdowns",chk_align)      : resp$(2)="True"
  fnLbl(5,1,"Error Handling:")
  fnChk(6,5,"Report Erroneous Transactions",chk_align)    : resp$(3)="True"
  fnChk(7,5,"Fix Erroneous Transactions",chk_align)       : resp$(4)="False"
  fnLbl(9,1,"Miscellaneous:")
  fnChk(10,5,"Move Credit Balnces to Other",chk_align)    : resp$(5)="False"
  fnChk(11,5,"  and apply credits",chk_align)             : resp$(6)="False"
  fnCmdSet(2)
  fnAcs(sn$,0,mat resp$,ck)
  if ck<>5 then 
    if resp$(1)='True' then do_fix_balance_breakdowns=1 else do_fix_balance_breakdowns=0
    if resp$(2)='True' then do_fix_trans_breakdowns=1 else do_fix_trans_breakdowns=0
    if resp$(3)='True' then do_report=1 else do_report=0
    if resp$(4)='True' then do_fix=1 else do_fix=0
    if resp$(5)='True' then do_move_credit=1 else do_move_credit=0
    if resp$(6)='True' then do_apply_credit=1 else do_apply_credit=0
    print_count=0
    if do_fix_balance_breakdowns then let fn_fix_balance_breakdowns(do_fix,do_report)
    if do_fix_trans_breakdowns then let fn_fix_trans_breakdowns(do_fix,do_report)
    if do_move_credit then let fn_move_credit(do_move_credit)
    if do_apply_credit then let fn_apply_credit_from_other(do_apply_credit)
  end if
goto XIT ! /r
XIT: fnxit 
ignore: continue
def fn_setup
  if ~setup then 
    setup=1
    library 'S:\Core\Library': fngethandle,fnStatus,fnget_services
    library 'S:\Core\Library': fnerror,fntop,fnxit,fnopenprn,fncloseprn,fnAcs,fnChk,fnCmdSet,fnLbl,fnTos
    on error goto ERTN
    ! ______________________________________________________________________
    dim cap$*128
		dim serviceName$(10)*20,srv$(10)*2
    fnget_services(mat serviceName$,mat srv$)
  end if 
fnend 
include: ertn
def fn_report_it(mat report_g,bal_breakdown; heading$*80,col_2_heading$*12)
  if do_report then 
    if ~setup_report_it then 
      setup_report_it=1
      fnopenprn
      pr #255,using F_HDR1: heading$
      pr #255,using F_HDR2: 'Account',col_2_heading$,serviceName$(1)(1:12),serviceName$(2)(1:12),serviceName$(3)(1:12),serviceName$(4)(1:12),serviceName$(5)(1:12),serviceName$(6)(1:12),serviceName$(7)(1:12),serviceName$(8)(1:12),serviceName$(9)(1:12),serviceName$(10)(1:12),'*Calculated*'
      F_HDR1: form pos 1,cc 156
      F_HDR2: form pos 1,13*(cc 12,',')
      F_BODY: form pos 1,c 12,',',12*(n 12.2,',')
    end if  ! ~setup_report_it
    print_count+=1
    pr #255,using F_BODY: z$,bal,report_g(1),report_g(2),report_g(3),report_g(4),report_g(5),report_g(6),report_g(7),report_g(8),report_g(9),report_g(10),bal_breakdown
    ! pr #255: z$&' has a balance of '&str$(gb(10))&' but the breakdowns add up to '&str$(bal_breakdown)
  end if 
fnend 
def fn_any_gb_negative
  agn_return=0
  for agn_item=1 to 10
    if gb(agn_item)<0 then agn_return=1
  next agn_item
  fn_any_gb_negative=agn_return
fnend  ! fn_any_gb_negative
! def library fnfix_balance_breakdowns(do_fix,do_report)
!   fn_setup
!   fnfix_balance_breakdowns=fn_fix_balance_breakdowns(do_fix,do_report)
! fnend
def fn_fix_balance_breakdowns(do_fix,do_report) ! assumes balance is right, puts the difference into other
  fnStatus('Checking Customer Balance Breakdowns')
  dim customer_g(10)
  dim z$*10,service_rate_code(7)
  dim gb(10)
  gb_other=fnservice_other
  open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
  do 
    read #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof CUSTOMER_EOF
    F_CUSTOMER: form pos 1,c 10,pos 143,7*pd 2,pos 292,pd 4.2,pos 300,10*pd 4.2,pos 388,10*pd 5.2
    read_count+=1
    for gb_item=1 to udim(mat serviceName$) ! udim(mat gb)
      if trim$(serviceName$(gb_item))='' then gb(gb_item)=0
    next gb_item
    bal_breakdown=sum(gb)
    if bal<>bal_breakdown then 
      fn_report_it(mat customer_g,bal_breakdown,"Customer Balance Breakdowns",'Balance')
      if do_fix then 
        gb(gb_other)-=(bal_breakdown-bal)
        rewrite #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
      end if 
    end if 
  loop 
  CUSTOMER_EOF: ! 
  close #h_customer: ioerr ignore
  if print_count>0 then let fncloseprn
  pr 'print_count=';print_count
  setup_report_it=0
fnend 
def library fnfix_trans_breakdowns(do_fix,do_report)
  fn_setup
  fnfix_trans_breakdowns=fn_fix_trans_breakdowns(do_fix,do_report)
fnend 
def fn_fix_trans_breakdowns(do_fix,do_report)
  dim trans_g(11),ru(6)
  gb_other=fnservice_other
  fnStatus('Checking Transaction Breakdowns')
  if do_fix then 
    open #h_trans=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",internal,outIn,relative 
  else 
    open #h_trans=11: "Name=[Q]\UBmstr\ubTransVB.h[cno],Shr",internal,input,relative 
  end if 
  do 
    read #h_trans,using F_TRANS: p$,tdate,transcode,tamt,mat trans_g,mat ru,bal,postcode eof TRANS_EOF
    F_TRANS: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
    read_count+=1
    for g_item=1 to udim(mat serviceName$) ! udim(mat trans_g)
      if trim$(serviceName$(g_item))='' then trans_g(g_item)=0
    next g_item
  ! r: get bal_breakdown
    bal_breakdown=0
    for sz1_item=1 to 9 ! sz1
      bal_breakdown+=trans_g(sz1_item)
    next sz1_item
  !      bal_breakdown=sum(trans_g)
  ! /r
    if tamt<>bal_breakdown then 
      fn_report_it(mat trans_g,bal_breakdown,"Transaction Breakdowns",'T Amount')
      if do_fix then 
        trans_g(gb_other)-=(bal_breakdown-tamt)
        rewrite #h_trans,using F_TRANS: p$,tdate,transcode,tamt,mat trans_g,mat ru,bal,postcode
      end if 
    end if  ! gb(10)<>FICTIONAL_gb10
  loop 
  TRANS_EOF: ! 
  if print_count>0 then let fncloseprn
  pr 'print_count=';print_count
  setup_report_it=0
  close #h_trans: 
fnend  ! fn_fix_balance_breakdowns
def fn_move_credit(do_move_credit)
  fnStatus('Moving Credit Balances to Other in Customer Balance Breakdowns')
  dim customer_g(10)
  dim z$*10,service_rate_code(7)
  dim gb(10)
  gb_other=fnservice_other
  open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
  do 
    read #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof MC_CUSTOMER_EOF
    read_count+=1
    if fn_any_gb_negative then 
      fn_report_it(mat gb,bal,"Customer Balance Breakdowns Before Credits Moved to Other",'Balance')
      for gb_item=1 to 10
        if gb_item<>gb_other then 
          if gb(gb_item)<0 then 
            gb(gb_other)+=gb(gb_item)
            gb(gb_item)=0
          end if 
        end if 
      next gb_item
      rewrite #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
      !       if trim$(z$)='205320.70' then pause !
    end if 
  loop 
  MC_CUSTOMER_EOF: ! 
  close #h_customer: 
fnend 
def fn_apply_credit_from_other(do_apply_credit)
  fnStatus('Applying Credit Balances (from Other) to the rest of the Customer Balance Breakdowns')
  dim customer_g(10)
  dim z$*10,service_rate_code(7)
  dim gb(10)
  gb_other=fnservice_other
  open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
  do 
    read #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb eof ACFO_CUSTOMER_EOF
    read_count+=1
    if gb(gb_other)<0 then 
      fn_report_it(mat gb,bal,"Customer Balance Breakdowns Before Credits Moved to Other",'Balance')
      for gb_item=1 to 10
        if gb_item<>gb_other then ! 
          if gb(gb_item)=>abs(gb(gb_other)) then ! this item has more (or equal) charge than other has credit
            gb(gb_item)+=gb(gb_other)
            gb(gb_other)=0
            goto ACFO_REC_COMPLETE ! all of other is consumed, we are done here
          else if gb(gb_item)<abs(gb(gb_other)) then ! other has more than enough credit to cover this charge
            gb(gb_other)+=gb(gb_item)
            gb(gb_item)=0
          end if 
        end if 
      next gb_item
      ACFO_REC_COMPLETE: ! 
      rewrite #h_customer,using F_CUSTOMER: z$,mat service_rate_code,bal,mat customer_g,mat gb
      !       if trim$(z$)='205320.70' then pause !
    end if 
  loop 
  ACFO_CUSTOMER_EOF: ! 
  close #h_customer: 
fnend 
