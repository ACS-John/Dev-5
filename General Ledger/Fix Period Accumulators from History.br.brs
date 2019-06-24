! formerly S:\acsGL\FixPA
fn_setup
  fntop(program$, cap$="Fix Period Accumulators from History")
  current_accounting_period=fnactpd
! 
  process_gltrans=1 ! if =1 than gltrans will be added into the period accumulators as well as actrans
! 
  open #company=1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,outIn,relative 
  read #company,using 'Form Pos 296,n 2,Pos 384,N 2',rec=1: lmu,nap
! lmu = Last Accounting Period Closed
! nap = Number of Accounting Periods
  close #company: 
  fn_get_fund_list(mat fund_list) ! pr 'fund_list:' : pr mat fund_list : pause
  mat last_retained_earnings_acct$(udim(mat fund_list)) : if udim(last_retained_earnings_acct$)=0 then mat last_retained_earnings_acct$(1)
  mat period_accumulator_current(nap)
  mat period_accumulator_prior(nap)
  if fn_screen_1(nap,mat period_date_start,mat prior_period_date_start)=5 then goto XIT
  fn_report(cap$)
  fn_report(date$('mm/dd/ccyy'))
  fn_report('')
  open #h_actrans:=fngethandle: "Name=[Q]\GLmstr\AcTrans.H[cno],KFName=[Q]\GLmstr\AcTrIdx.H[cno],Shr",internal,outIn,keyed 
F_ACTRANS: form pos 1,c 12,n 6,pd 6.2,n 2,pos 71,n 2
  if process_gltrans then 
    fnindex_it("[Q]\GLmstr\GLTrans.H[cno]",env$('Temp')&"\GLIndex.H[cno]","1 12")
    open #h_gltrans:=fngethandle: "Name=[Q]\GLmstr\GLTrans.H[cno],KFName="&env$('Temp')&"\GLIndex.h[cno],Shr",internal,outIn,keyed 
  end if  ! process_gltrans
F_GLTRANS: form pos 1,c 12,n 6,pd 6.2,n 2
  open #h_glmstr:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,outIn,keyed 
  open #h_glmstr2:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndx2.H[cno],Shr",internal,outIn,keyed 
F_GLMSTR: form pos 1,c 12,x 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
  do 
    read #h_glmstr,using F_GLMSTR: gl$,mat rf,bb,cb,mat balance_current_year_month,mat balance_prior_year_month eof EO_GLMSTR
    fn_report('*** '&gl$&' ***')
    mat period_accumulator_current=(0)
    if fn_is_a_retained_earn_account(gl$) then 
      period_accumulator_current(1)=balance_prior_year_month(nap)
! if gl$='  1   405  0' then pr 'initialize it to  ';period_accumulator_current(1) : pause
    else 
      period_accumulator_current(1)=0 ! bb ! bb = Beginning Balance (at the beginning of the fiscal year)
    end if 
    mat period_accumulator_prior=(0)
    gln_period_did_change=0
    for period=1 to nap
      if period=nap then 
        period_date_end=date(days(period_date_start(1)+1,'mmddyy')-1,'mmddyy')
        prior_period_date_end=date(days(prior_period_date_start(1)+1,'mmddyy')-1,'mmddyy')
      else 
        period_date_end=date(days(period_date_start(period+1),'mmddyy')-1,'mmddyy')
        prior_period_date_end=date(days(prior_period_date_start(period+1),'mmddyy')-1,'mmddyy')
      end if  ! period=nap   /   else 
! if gl$='  1   405  0' and period=3 then pr 'before fn_process_trans   period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
      fn_process_trans(h_actrans, 1)
! if gl$='  1   405  0' then pr 'after fn_process_trans' : pause
      if process_gltrans then let fn_process_trans(h_gltrans)
      if period>1 and period<=current_accounting_period then 
        period_accumulator_current(period)+=period_accumulator_current(period-1)
! if gl$='  1   405  0' and period=3 then pr 'after adding in the prior period    period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
      end if 
      if include_prior_periods then 
        if period>1 then period_accumulator_prior(period)+=period_accumulator_prior(period-1)
      end if  ! include_prior_periods
!   if period>1 then period_accumulator_current(period)<>0 then period_accumulator_current(period)+=period_accumulator_current(period-1)
      if period_accumulator_current(period)<>balance_current_year_month(period) then 
        gln_period_did_change+=1
        fn_report('changing GLmstr '&gl$&' period '&str$(period)&" from "&str$(balance_current_year_month(period))&' to '&str$(period_accumulator_current(period)))
        balance_current_year_month(period)=period_accumulator_current(period)
!  if gl$='  1   405  0' then pr ' about to write period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
      end if 
      if include_prior_periods and period_accumulator_prior(period)<>balance_prior_year_month(period) then 
        gln_period_did_change+=1
        fn_report('changing GLmstr '&gl$&' period '&str$(period)&" from "&str$(balance_prior_year_month(period))&' to '&str$(period_accumulator_prior(period)))
        balance_prior_year_month(period)=period_accumulator_prior(period)
      end if  ! period_accumulator_prior(period)<>balance_prior_year_month(period) then
    next period
    if current_accounting_period>1 then
      if cb<>balance_current_year_month(current_accounting_period) then gln_period_did_change+=1
      cb=balance_current_year_month(current_accounting_period)
    end if
    if current_accounting_period>2 then
      if bb<>balance_current_year_month(current_accounting_period-1) then gln_period_did_change+=1
      bb=balance_current_year_month(current_accounting_period-1)
    end if
!   if trim$(gl$)='1   405  0' then pause
    if gln_period_did_change>0 then 
      fn_report(' change detected to the current month balance column '&gl$)
      rewrite #h_glmstr,using F_GLMSTR,key=gl$: gl$,mat rf,bb,cb,mat balance_current_year_month,mat balance_prior_year_month
    end if  ! gln_period_did_change>0
  loop 
EO_GLMSTR: ! 
! fncloseprn : report_open=0
XIT: fnxit ! if env$('acsdeveloper')<>'' then stop else let fnxit ! XXX
def fn_setup
  library 'S:\Core\Library': fntop,fnxit,fnAcs,fnLbl,fnTxt,fngethandle,fnTos,fnerror,fnCmdSet,fnChk,fncreg_write,fncreg_read,fncd,fnactpd,fnStatus,fnqgl,fnagl$,fnindex_it,fnrgl$
  on error goto ERTN
! ______________________________________________________________________
  dim cap$*128,resp$(100)*60
  dim balance_current_year_month(13),balance_prior_year_month(13),rf(6)
  dim actrans_key$*20
fnend
  def fn_screen_1(nap,mat period_date_start,mat prior_period_date_start)
    mat period_date_start(nap)
    period_date_start=(0)
    mat prior_period_date_start(nap)
    prior_period_date_start=(0)
    fnTos(sn$="FixPA2_"&str$(nap))
    mylen=31
    mypos=mylen+2
    respc=0 : myline=0
    for period=1 to nap
      fnLbl(myline+=1,1,"Period "&str$(period)&" Start Date:",mylen,1)
      fnTxt(myline,mypos,8,0,1,"1")
      respc+=1
      fncreg_read("Period "&str$(period)&" Start Date",resp$(respc))
    next period
    fnChk(myline+=2,mypos,"Correct Prior Year",1)
    respc+=1
    fncreg_read("correct prior year",resp$(respc))
    if resp$(respc)='' then resp$(respc)='False'
! 
    myline=1 : col3_pos=mypos+20
    resp_lrea_fund_1=respc+1
    if use_dept then 
      col4_pos=col3_pos+10
      fnLbl(1,col3_pos,'Last Retained Earnings Account(s)')
      for fund_item=1 to udim(mat fund_list)
        fnLbl(myline+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",9,1)
        fnqgl(myline,col4_pos)
        respc+=1
        fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(respc)) : resp$(respc)=fnrgl$(resp$(respc))
      next fund_item
    else 
      col4_pos=col3_pos+32
      fnLbl(1,col3_pos,'Last Retained Earnings Account:',31,1)
      fnqgl(myline,col4_pos)
      respc+=1
      fncreg_read("last retained earnings account - no fund ",resp$(respc)) : resp$(respc)=fnrgl$(resp$(respc))
    end if 
    fnCmdSet(2)
    fnAcs(sn$,0,mat resp$,ck)
    if ck<>5 then 
      respc=0
      for period=1 to nap
        period_date_start(period)=val(resp$(period))
        prior_period_date_start(period)=period_date_start(period)-1
        fncreg_write("Period "&str$(period)&" Start Date",resp$(period))
      next period
      if resp$(nap+1)='True' then include_prior_periods=1 else include_prior_periods=0
      fncreg_write("correct prior year",resp$(nap+1))
      respc=resp_lrea_fund_1-1
      if use_dept then 
        for fund_item=1 to udim(mat fund_list)
          last_retained_earnings_acct$(fund_item)=fnagl$(resp$(respc+=1))
          fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
        next fund_item
      else 
        last_retained_earnings_acct$(1)=fnagl$(resp$(respc+=1))
        fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
      end if 
    end if  ! ck<>5
    fn_screen_1=ck
  fnend  ! fn_screen_1
  def fn_date_mmddyy_is_within_range(dmi_test_date,dmi_date_start,dmi_date_end)
    dmi_return=0
    dmi_test_date=fncd(dmi_test_date)
    dmi_date_start=fncd(dmi_date_start)
    dmi_date_end=fncd(dmi_date_end)
    if dmi_test_date=>dmi_date_start and dmi_test_date<=dmi_date_end then dmi_return=1
    fn_date_mmddyy_is_within_range=dmi_return
  fnend  ! fn_date_mmddyy_is_within_range
  def fn_process_trans(h_trans; pt_fix_trans_period_code)
    actrans_key$=rpad$(gl$,kln(h_trans))
    restore #h_trans,key>=actrans_key$: nokey EO_TRANS
    do 
      if pt_fix_trans_period_code then 
        read #h_trans,using F_ACTRANS: trgl$,tr_date,tr_amt,tr_6,pc2 eof EO_TRANS
      else 
        read #h_trans,using F_GLTRANS: trgl$,tr_date,tr_amt,tr_6 eof EO_TRANS
      end if  ! pt_fix_trans_period_code   /   else 
      if trgl$<>gl$ then goto EO_TRANS
! prior month
      if fn_date_mmddyy_is_within_range(tr_date,prior_period_date_start(period),prior_period_date_end) then 
        period_accumulator_prior(period)+=tr_amt
!         fn_report(rpt$(' ',40)&str$(tr_date)&' prior period '&str$(period)&'  + '&cnvrt$('pic(----------.--)',tr_amt))
        if pt_fix_trans_period_code and period<>pc2 then ! the period on the transaction is incorrect - correct it.
!         fn_report('changing actrans '&trgl$&'/'&str$(tr_date)&" from "&str$(pc2)&' to '&str$(period))
          pc2=period
          rewrite #h_trans,using F_ACTRANS: trgl$,tr_date,tr_amt,tr_6,pc2
        end if  ! pt_fix_trans_period_code and period<>pc2
      end if  ! fn_date_mmddyy_is_within_range
! current month
      if fn_date_mmddyy_is_within_range(tr_date,period_date_start(period),period_date_end) then 
        if period=>1 then 
          period_accumulator_current(period)+=tr_amt
          ! if trgl$=' 51   830  0' then
          !   pr 'period_accumulator_current(';period;')=';period_accumulator_current(period)
          !   pr 'just added  ';tr_amt;' due to date of ';tr_date
          !   pause
          ! end if
        end if 
!         fn_report(str$(tr_date)&' period '&str$(period)&'  + '&cnvrt$('pic(----------.--)',tr_amt)&' type '&str$(tr_6))
        if pt_fix_trans_period_code and period<>pc2 then ! the period on the transaction is incorrect - correct it.
!         fn_report('changing actrans '&trgl$&'/'&str$(tr_date)&" from "&str$(pc2)&' to '&str$(period))
          pc2=period
          rewrite #h_trans,using F_ACTRANS: trgl$,tr_date,tr_amt,tr_6,pc2
        end if  ! pt_fix_trans_period_code and period<>pc2
      end if  ! fn_date_mmddyy_is_within_range
    loop 
EO_TRANS: ! 
    if period_accumulator_current(period)<>0 then 
      fn_report('  period '&str$(period)&' totals '&cnvrt$('pic(----------.--)',period_accumulator_current(period)))
    end if 
    if period_accumulator_prior(period)<>0 then 
      fn_report(rpt$(' ',40)&'prior period '&str$(period)&' totals '&cnvrt$('pic(----------.--)',period_accumulator_prior(period)))
    end if 
  fnend 
  def fn_report(line$*256)
!   if ~report_open then
!     report_open=1
!     fnopenprn
!   end if  ! ~report_open
!   pr #255: line$
    fnStatus(line$) ! pr line$ ! XXX
  fnend 
! ______________________________________________________________________
! <Updateable Region: ERTN>
ERTN: fnerror(program$,err,line,act$,"xit")
  if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
  execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
  pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
  def fn_is_a_retained_earn_account(gl$)
! pr 'gl number passed is *'&gl$&'*'
! pr 'gl number last retained earnings *'&last_retained_earnings_acct$&'*'
    gl$=trim$(fnagl$(gl$))
    if use_dept then 
      fund_compare=val(gl$(1:3))
      fund_which=srch(mat fund_list,fund_compare)
    else 
      fund_which=1
    end if 
    if gl$<=trim$(last_retained_earnings_acct$(fund_which)) then 
!     pr '"'&gl$&'"<="'&trim$(last_retained_earnings_acct$(fund_which))&'" so it IS a retained earnings account - fund:'&str$(fund_which)
      iarea_return=1
!     pause
    else 
!     pr '"'&gl$&'">"'&trim$(last_retained_earnings_acct$(fund_which))&'" so it is NOT a retained earnings account - fund:'&str$(fund_which)
      iarea_return=0
!     pause
    end if 
    fn_is_a_retained_earn_account=iarea_return
  fnend 
def library fnGetFundList(mat fund_list)
  if ~setup then let fn_setup
  fnGetFundList=fn_get_fund_list(mat fund_list)
fnend
def fn_get_fund_list(mat fund_list)
  ! returns an array of all unique gl number funds
  open #company=fngethandle: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input 
  read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
  close #company: 
  if use_dept then 
    mat fund_list(999)
    open #gfl_h_glmstr:=fngethandle: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.H[cno],Shr",internal,input,keyed 
    do 
      read #gfl_h_glmstr,using 'form pos 1,N 3': fund eof GFL_EO_GLMSTR
      if fund<>fund_prior then 
        fund_list_count+=1
        fund_list(fund_list_count)=fund
        fund_prior=fund
      end if 
    loop 
    GFL_EO_GLMSTR: ! 
    close #gfl_h_glmstr: 
  else ! no departments
    fund_list_count=0
  end if 
  mat fund_list(fund_list_count)
fnend 
