10000 ! formerly S:\acsGL\FixPA
10200 let fn_setup
12000   let fntop(program$, cap$="Fix Period Accumulators from History")
12020   let current_accounting_period=fnactpd
12200 ! 
12400   let process_gltrans=1 ! if =1 than gltrans will be added into the period accumulators as well as actrans
12600 ! 
12800   open #company=1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
13000   read #company,using 'Form Pos 296,n 2,Pos 384,N 2',rec=1: lmu,nap
13020 ! lmu = Last Accounting Period Closed
13040 ! nap = Number of Accounting Periods
13200   close #company: 
13300   let fn_get_fund_list(mat fund_list) ! print 'fund_list:' : print mat fund_list : pause
13320   mat last_retained_earnings_acct$(udim(mat fund_list))
13400   mat period_accumulator_current(nap)
13600   mat period_accumulator_prior(nap)
13800   if fn_screen_1(nap,mat period_date_start,mat prior_period_date_start)=5 then goto XIT
14000   let fn_report(cap$)
14200   let fn_report(date$('mm/dd/ccyy'))
14400   let fn_report('')
14600   open #h_actrans:=fngethandle: "Name="&env$('Q')&"\GLmstr\AcTrans.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\AcTrIdx.H"&env$('cno')&",Shr",internal,outin,keyed 
15000 F_ACTRANS: form pos 1,c 12,n 6,pd 6.2,n 2,pos 71,n 2
15200   if process_gltrans then 
15400     let fnindex_it(env$('Q')&"\GLmstr\GLTrans.H"&env$('cno'),env$('Temp')&"\GLIndex.H"&env$('cno'),"1 12")
15600     open #h_gltrans:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLTrans.H"&env$('cno')&",KFName="&env$('Temp')&"\GLIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
15800   end if  ! process_gltrans
16000 F_GLTRANS: form pos 1,c 12,n 6,pd 6.2,n 2
16200   open #h_glmstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.H"&env$('cno')&",Shr",internal,outin,keyed 
16400   open #h_glmstr2:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\glIndx2.H"&env$('cno')&",Shr",internal,outin,keyed 
16600 F_GLMSTR: form pos 1,c 12,x 50,6*pd 3,42*pd 6.2,2*pd 3,13*pd 6.2
16800   do 
17000     read #h_glmstr,using F_GLMSTR: gl$,mat rf,bb,cb,mat balance_current_year_month,mat balance_prior_year_month eof EO_GLMSTR
17200     let fn_report('*** '&gl$&' ***')
17400     mat period_accumulator_current=(0)
17440     if fn_is_a_retained_earn_account(gl$) then 
17460       let period_accumulator_current(1)=balance_prior_year_month(nap)
17470 ! if gl$='  1   405  0' then pr 'initialize it to  ';period_accumulator_current(1) : pause
17480     else 
17500       let period_accumulator_current(1)=0 ! bb ! bb = Beginning Balance (at the beginning of the fiscal year)
17520     end if 
17800     mat period_accumulator_prior=(0)
18000     let gln_period_did_change=0
18200     for period=1 to nap
18400       if period=nap then 
18600         let period_date_end=date(days(period_date_start(1)+1,'mmddyy')-1,'mmddyy')
18800         let prior_period_date_end=date(days(prior_period_date_start(1)+1,'mmddyy')-1,'mmddyy')
19000       else 
19200         let period_date_end=date(days(period_date_start(period+1),'mmddyy')-1,'mmddyy')
19400         let prior_period_date_end=date(days(prior_period_date_start(period+1),'mmddyy')-1,'mmddyy')
19600       end if  ! period=nap   /   else 
19602 ! if gl$='  1   405  0' and period=3 then pr 'before fn_process_trans   period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
19800       let fn_process_trans(h_actrans, 1)
19802 ! if gl$='  1   405  0' then pr 'after fn_process_trans' : pause
20000       if process_gltrans then let fn_process_trans(h_gltrans)
20200       if period>1 and period<=current_accounting_period then 
20220         let period_accumulator_current(period)+=period_accumulator_current(period-1)
20240 ! if gl$='  1   405  0' and period=3 then pr 'after adding in the prior period    period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
20380       end if 
20400       if include_prior_periods then 
20600         if period>1 then let period_accumulator_prior(period)+=period_accumulator_prior(period-1)
20800       end if  ! include_prior_periods
21000 !   if period>1 then period_accumulator_current(period)<>0 then let period_accumulator_current(period)+=period_accumulator_current(period-1)
21200       if period_accumulator_current(period)<>balance_current_year_month(period) then 
21400         let gln_period_did_change+=1
21600         let fn_report('changing GLmstr '&gl$&' period '&str$(period)&" from "&str$(balance_current_year_month(period))&' to '&str$(period_accumulator_current(period)))
21800         let balance_current_year_month(period)=period_accumulator_current(period)
21820 !  if gl$='  1   405  0' then pr ' about to write period_accumulator_current(';period;')=';period_accumulator_current(period) : pause
22000       end if 
22200       if include_prior_periods and period_accumulator_prior(period)<>balance_prior_year_month(period) then 
22400         let gln_period_did_change+=1
22600         let fn_report('changing GLmstr '&gl$&' period '&str$(period)&" from "&str$(balance_prior_year_month(period))&' to '&str$(period_accumulator_prior(period)))
22800         let balance_prior_year_month(period)=period_accumulator_prior(period)
23000       end if  ! period_accumulator_prior(period)<>balance_prior_year_month(period) then
23100     next period
23120     if current_accounting_period>1 then
23140       if cb<>balance_current_year_month(current_accounting_period) then gln_period_did_change+=1
23160       cb=balance_current_year_month(current_accounting_period)
23180     end if
23200     if current_accounting_period>2 then
23220       if bb<>balance_current_year_month(current_accounting_period-1) then gln_period_did_change+=1
23240       bb=balance_current_year_month(current_accounting_period-1)
23260     end if
23280 !   if trim$(gl$)='1   405  0' then pause
23400     if gln_period_did_change>0 then 
23600       let fn_report(' change detected to the current month balance column '&gl$)
23800       rewrite #h_glmstr,using F_GLMSTR,key=gl$: gl$,mat rf,bb,cb,mat balance_current_year_month,mat balance_prior_year_month
24000     end if  ! gln_period_did_change>0
24200   loop 
24400 EO_GLMSTR: ! 
24600 ! let fncloseprn : let report_open=0
24800 XIT: let fnxit ! if env$('acsdeveloper')<>'' then stop else let fnxit ! XXX
25000 def fn_setup
25020   library 'S:\Core\Library': fntop,fnxit,fnacs,fnlbl,fntxt,fngethandle,fntos,fnerror,fncmdset,fnchk,fncreg_write,fncreg_read,fncd,fnactpd,fnstatus,fnqgl,fnagl$,fnindex_it,fnrgl$
25040   on error goto ERTN
25060 ! ______________________________________________________________________
25080   dim cap$*128,resp$(100)*60
25100   dim balance_current_year_month(13),balance_prior_year_month(13),rf(6)
25120   dim actrans_key$*20
25140 fnend
26000   def fn_screen_1(nap,mat period_date_start,mat prior_period_date_start)
26020     mat period_date_start(nap)
26040     let period_date_start=(0)
26060     mat prior_period_date_start(nap)
26080     let prior_period_date_start=(0)
26100     let fntos(sn$="FixPA2_"&str$(nap))
26120     let mylen=31
26140     let mypos=mylen+2
26160     let respc=0 : let myline=0
26180     for period=1 to nap
26200       let fnlbl(myline+=1,1,"Period "&str$(period)&" Start Date:",mylen,1)
26220       let fntxt(myline,mypos,8,0,1,"1")
26240       let respc+=1
26260       let fncreg_read("Period "&str$(period)&" Start Date",resp$(respc))
26280     next period
26300     let fnchk(myline+=2,mypos,"Correct Prior Year",1)
26320     let respc+=1
26340     let fncreg_read("correct prior year",resp$(respc))
26360     if resp$(respc)='' then let resp$(respc)='False'
26380 ! 
26400     let myline=1 : let col3_pos=mypos+20
26410     let resp_lrea_fund_1=respc+1
26420     if use_dept then 
26440       let col4_pos=col3_pos+10
26460       let fnlbl(1,col3_pos,'Last Retained Earnings Account(s)')
26500       for fund_item=1 to udim(mat fund_list)
26520         let fnlbl(myline+=1,col3_pos,"Fund "&str$(fund_list(fund_item))&":",9,1)
26540         let fnqgl(myline,col4_pos)
26560         let respc+=1
26580         let fncreg_read("last retained earnings account - fund "&str$(fund_list(fund_item)),resp$(respc)) : let resp$(respc)=fnrgl$(resp$(respc))
26600       next fund_item
26620     else 
26630       let col4_pos=col3_pos+32
26640       let fnlbl(1,col3_pos,'Last Retained Earnings Account:',31,1)
26660       let fnqgl(myline,col4_pos)
26680       let respc+=1
26700       let fncreg_read("last retained earnings account - no fund ",resp$(respc)) : let resp$(respc)=fnrgl$(resp$(respc))
26720     end if 
26740     let fncmdset(2)
26760     let fnacs(sn$,0,mat resp$,ck)
26780     if ck<>5 then 
26800       let respc=0
26820       for period=1 to nap
26840         let period_date_start(period)=val(resp$(period))
26860         let prior_period_date_start(period)=period_date_start(period)-1
26880         let fncreg_write("Period "&str$(period)&" Start Date",resp$(period))
26900       next period
26920       if resp$(nap+1)='True' then let include_prior_periods=1 else let include_prior_periods=0
26940       let fncreg_write("correct prior year",resp$(nap+1))
26960       let respc=resp_lrea_fund_1-1
26980       if use_dept then 
27000         for fund_item=1 to udim(mat fund_list)
27020           let last_retained_earnings_acct$(fund_item)=fnagl$(resp$(respc+=1))
27040           let fncreg_write("last retained earnings account - fund "&str$(fund_list(fund_item)),last_retained_earnings_acct$(fund_item))
27060         next fund_item
27080       else 
27100         let last_retained_earnings_acct$(1)=fnagl$(resp$(respc+=1))
27120         let fncreg_write("last retained earnings account - no fund ",last_retained_earnings_acct$(1))
27140       end if 
27160     end if  ! ck<>5
27180     let fn_screen_1=ck
27200   fnend  ! fn_screen_1
32000   def fn_date_mmddyy_is_within_range(dmi_test_date,dmi_date_start,dmi_date_end)
32020     let dmi_return=0
32040     let dmi_test_date=fncd(dmi_test_date)
32060     let dmi_date_start=fncd(dmi_date_start)
32080     let dmi_date_end=fncd(dmi_date_end)
32100     if dmi_test_date=>dmi_date_start and dmi_test_date<=dmi_date_end then let dmi_return=1
32120     let fn_date_mmddyy_is_within_range=dmi_return
32140   fnend  ! fn_date_mmddyy_is_within_range
33400   def fn_process_trans(h_trans; pt_fix_trans_period_code)
33600     let actrans_key$=rpad$(gl$,kln(h_trans))
33800     restore #h_trans,key>=actrans_key$: nokey EO_TRANS
34000     do 
34200       if pt_fix_trans_period_code then 
34400         read #h_trans,using F_ACTRANS: trgl$,tr_date,tr_amt,tr_6,pc2 eof EO_TRANS
34600       else 
34800         read #h_trans,using F_GLTRANS: trgl$,tr_date,tr_amt,tr_6 eof EO_TRANS
35000       end if  ! pt_fix_trans_period_code   /   else 
35200       if trgl$<>gl$ then goto EO_TRANS
35400 ! prior month
35600       if fn_date_mmddyy_is_within_range(tr_date,prior_period_date_start(period),prior_period_date_end) then 
35800         let period_accumulator_prior(period)+=tr_amt
36000 !         let fn_report(rpt$(' ',40)&str$(tr_date)&' prior period '&str$(period)&'  + '&cnvrt$('pic(----------.--)',tr_amt))
36200         if pt_fix_trans_period_code and period<>pc2 then ! the period on the transaction is incorrect - correct it.
36400 !         let fn_report('changing actrans '&trgl$&'/'&str$(tr_date)&" from "&str$(pc2)&' to '&str$(period))
36600           let pc2=period
36800           rewrite #h_trans,using F_ACTRANS: trgl$,tr_date,tr_amt,tr_6,pc2
37000         end if  ! pt_fix_trans_period_code and period<>pc2
37200       end if  ! fn_date_mmddyy_is_within_range
37400 ! current month
37600       if fn_date_mmddyy_is_within_range(tr_date,period_date_start(period),period_date_end) then 
37800         if period=>1 then 
37820           let period_accumulator_current(period)+=tr_amt
37840           ! if trgl$=' 51   830  0' then
37860           !   pr 'period_accumulator_current(';period;')=';period_accumulator_current(period)
37880           !   pr 'just added  ';tr_amt;' due to date of ';tr_date
37900           !   pause
37920           ! end if
37940         end if 
38000 !         let fn_report(str$(tr_date)&' period '&str$(period)&'  + '&cnvrt$('pic(----------.--)',tr_amt)&' type '&str$(tr_6))
38200         if pt_fix_trans_period_code and period<>pc2 then ! the period on the transaction is incorrect - correct it.
38400 !         let fn_report('changing actrans '&trgl$&'/'&str$(tr_date)&" from "&str$(pc2)&' to '&str$(period))
38600           let pc2=period
38800           rewrite #h_trans,using F_ACTRANS: trgl$,tr_date,tr_amt,tr_6,pc2
39000         end if  ! pt_fix_trans_period_code and period<>pc2
39200       end if  ! fn_date_mmddyy_is_within_range
39400     loop 
40000 EO_TRANS: ! 
40020     if period_accumulator_current(period)<>0 then 
40040       let fn_report('  period '&str$(period)&' totals '&cnvrt$('pic(----------.--)',period_accumulator_current(period)))
40060     end if 
40080     if period_accumulator_prior(period)<>0 then 
40100       let fn_report(rpt$(' ',40)&'prior period '&str$(period)&' totals '&cnvrt$('pic(----------.--)',period_accumulator_prior(period)))
40120     end if 
40200   fnend 
40400   def fn_report(line$*256)
40600 !   if ~report_open then
40800 !     let report_open=1
41000 !     let fnopenprn
41200 !   end if  ! ~report_open
41400 !   print #255: line$
41420     let fnstatus(line$) ! pr line$ ! XXX
41600   fnend 
51670 ! ______________________________________________________________________
51680 ! <Updateable Region: ERTN>
51690 ERTN: let fnerror(program$,err,line,act$,"xit")
51700   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
51710   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
51720   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
51730 ERTN_EXEC_ACT: execute act$ : goto ERTN
51740 ! /region
52000   def fn_is_a_retained_earn_account(gl$)
52020 ! pr 'gl number passed is *'&gl$&'*'
52040 ! pr 'gl number last retained earnings *'&last_retained_earnings_acct$&'*'
52060     let gl$=trim$(fnagl$(gl$))
52080     if use_dept then 
52100       let fund_compare=val(gl$(1:3))
52120       let fund_which=srch(mat fund_list,fund_compare)
52140     else 
52160       let fund_which=1
52180     end if 
52200     if gl$<=trim$(last_retained_earnings_acct$(fund_which)) then 
52220 !     pr '"'&gl$&'"<="'&trim$(last_retained_earnings_acct$(fund_which))&'" so it IS a retained earnings account - fund:'&str$(fund_which)
52240       let iarea_return=1
52260 !     pause
52280     else 
52300 !     pr '"'&gl$&'">"'&trim$(last_retained_earnings_acct$(fund_which))&'" so it is NOT a retained earnings account - fund:'&str$(fund_which)
52320       let iarea_return=0
52340 !     pause
52360     end if 
52380     let fn_is_a_retained_earn_account=iarea_return
52400   fnend 
55000 def library fnGetFundList(mat fund_list)
55020   if ~setup then let fn_setup
55040   fnGetFundList=fn_get_fund_list(mat fund_list)
55060 fnend
56000 def fn_get_fund_list(mat fund_list)
56020   ! returns an array of all unique gl number funds
56040   open #company=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input 
56060   read #company,using 'Form Pos 150,2*N 1': use_dept,use_sub ! read fund and sub codes from general
56080   close #company: 
56100   if use_dept then 
56120     mat fund_list(999)
56140     open #gfl_h_glmstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.H"&env$('cno')&",Shr",internal,input,keyed 
56160     do 
56180       read #gfl_h_glmstr,using 'form pos 1,N 3': fund eof GFL_EO_GLMSTR
56200       if fund<>fund_prior then 
56220         let fund_list_count+=1
56240         let fund_list(fund_list_count)=fund
56260         let fund_prior=fund
56280       end if 
56300     loop 
56320     GFL_EO_GLMSTR: ! 
56340     close #gfl_h_glmstr: 
56360   else ! no departments
56380     let fund_list_count=0
56400   end if 
56420   mat fund_list(fund_list_count)
56440 fnend 
