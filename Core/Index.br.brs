10000   ! r: reindex the *new* add company files
10020   dim syslist$(4)*2
10040   syslist$(1)='GL'
10060   syslist$(2)='UB'
10080   syslist$(3)='PR'
10100   syslist$(4)='CL'
10120   for sysitem=1 to udim(mat syslist$)
10140     execute 'config SUBSTITUTE "'&env$('Q')&'\'&syslist$(sysitem)&'mstr\" "S:\acs'&syslist$(sysitem)&'\mstr\"'
10160     fn_index_sys_do_one(99999,'GL')
10180     execute 'config SUBSTITUTE "'&env$('Q')&'\'&syslist$(sysitem)&'mstr\" "S:\acs'&syslist$(sysitem)&'\mstr\" Clear' ! this clear does not seem to work - just exit BR after running
10200   next sysitem
10220   end
10240   ! /r
12000   def fn_index_it_setup
12020     library 'S:\Core\Library': fnxit,fnerror,fnstatus,fnget_company_number_list,fngethandle,fnshortpath$
12040     if ~setup_index_it then 
12060       setup_index_it=1
12080       on error goto ERTN
12100       option retain 
12120 !     working_dir_rights=fnrights_test('',"Try Run As Administrator.",'Program','Indexes are unable to process without this access and will be skipped for the remainder of this session.')
12140     end if 
12160   fnend 
18000   def library fnindex_it(data_file$*256,index_statement_or_file$*512; index_parameters$*256)
18020     fn_index_it_setup
18040     fnindex_it=fn_index_it(data_file$,index_statement_or_file$, index_parameters$)
18060   fnend 
20000   def fn_index_it(data_file$*256,index_statement_or_file$*512; index_parameters$*256)
22000 ! r: constants, dims, library, on error, etc
22020     fn_index_it_setup
22040 !  if ~working_dir_rights then
22060 !    fnstatus('Skipping (due to inadaquate rights to working folder) index for '&data_file$)
22080 !  else 
22100     dim cap$*128
22120     dim index_execute_text$*512
22140     data_file$=trim$(data_file$)
22160     cap$='fnindex_it for '&data_file$(1:128) ! data_file$(len(data_file$)-80:len(data_file$)) ! cap$ is just for the error routine anyway
22180     is_index_statement=1
22200     is_index_file=2
22220 !   /r
22240     fail=0
22260     if index_parameters$='' then index_statement_or_file=is_index_statement else index_statement_or_file=is_index_file
25000     if exists(data_file$) then 
26000       if index_statement_or_file=is_index_statement then 
26010         fnstatus(index_statement_or_file$)
26012         index_execute_text$=index_statement_or_file$
26020         execute index_execute_text$ ioerr EXE_INDEX_ERR
27000       else ! index_statement_or_file=is_index_file
27010         fnstatus(os_filename$(index_statement_or_file$))
27020         index_parameters$=lwrc$(index_parameters$)
27040         index_parameters$=' '&index_parameters$&' '
27050         index_parameters$=srep$(index_parameters$,',',' ')
27060         index_parameters$=srep$(index_parameters$,' replace',' ')
27080         index_parameters$=srep$(index_parameters$,' dupkeys',' ')
27100         index_parameters$=srep$(index_parameters$,' -n',' ')
27140         index_parameters$=trim$(index_parameters$)&' Replace DupKeys Shr' ! -N
27141         if pos(data_file$,' ')>0 then data_file$=fnshortpath$(data_file$)
27142         if pos(index_statement_or_file$,' ')>0 then index_statement_or_file$=fnshortpath$(index_statement_or_file$)
27145 ! 
27150 !       pr 'index '&(data_file$)&' '&(index_statement_or_file$)&' '&index_parameters$ : pause
27154         index_execute_text$='index '&(data_file$)&' '&(index_statement_or_file$)&' '&index_parameters$
27156 !       if env$('ACSDeveloper')='' then execute 'CD '&env$('temp')(1:2)
27158 !       if env$('ACSDeveloper')='' then execute 'CD '&env$('temp')(3:len(env$('temp')))
27160         execute index_execute_text$ ioerr EXE_INDEX_ERR
27162 !       if env$('ACSDeveloper')='' then execute 'CD S:'
27180       end if 
28000     else 
28020       fail=1
28040       fnstatus("Could not find data file:")
28060       fnstatus("     "&os_filename$(data_file$))
28080     end if 
28090 !   end if
28100     goto INDEX_XIT
30000 EXE_INDEX_ERR: ! 
30020     fail=1
30040     fnstatus("Encountered error "&str$(err)&" executing index:")
30060     fnstatus("     ("&index_execute_text$&")") ! pause
30100     if err=7600 then 
30120       open #h_tmp:=fngethandle: 'name='&data_file$,internal,input error EIE_7600_XIT
30140       fnstatus('     (Record Length is '&str$(rln(h_tmp))&')')
30160       close #h_tmp: 
30180       EIE_7600_XIT: ! 
30182     else if err=7605 then 
30184       if env$('acsDeveloper')<>'' then pause
30200     end if 
32000 INDEX_XIT: ! 
32020     if fail then 
32040 !     fnstatus_pause
32060       index_it_return=0
32080     else 
32100       index_it_return=1
32120     end if 
32140     fn_index_it=index_it_return
32160   fnend 
34000 XIT: fnxit
44000 ! <Updateable Region: ERTN>
44020 ERTN: ! 
44040   library 'S:\Core\Library': fnerror
44060   fnerror(program$,err,line,act$,"xit")
44080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
44100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
44120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
44140 ERTN_EXEC_ACT: execute act$ : goto ERTN
44160 ! /region
50000   def library fnindex_sys(; only_cno,system_id$*2)
50020     fn_index_it_setup
50040     fnindex_sys=fn_index_sys( only_cno,system_id$)
50060   fnend 
52000   def fn_index_sys(; only_cno,system_id$*2)
52020 ! only_cno=0 means index all company numbers, otherwise index only the company number passed
52040 ! system_id$ of blank means to index the currenet system - otherwise index the system specified.
52060     if system_id$='' then system_id$=env$('CurSys')
52080     if only_cno then 
52100       fn_index_sys_do_one(only_cno,system_id$)
52120     else 
52140       if fnget_company_number_list(mat cno_list, system_id$) then 
52160         for cno_item=1 to udim(mat cno_list)
52180           fn_index_sys_do_one(cno_list(cno_item),system_id$)
52200         next cno_item
52220       else 
52240         fnstatus('no companies found in '&system_id$&' to index')
52260       end if 
52280     end if 
52300   fnend 
52320   def fn_index_sys_do_one(cno,system_id$*2)
52340     if system_id$='GL' then ! r:
52360 ! r: A
52380       fn_index_it(env$('Q')&"\GLmstr\ACGLSCHS.h"&str$(cno),env$('Q')&"\GLmstr\schindex.h"&str$(cno),"1 3")
52400       fn_index_it(env$('Q')&"\GLmstr\ACGLSCHS.h"&str$(cno),env$('Q')&"\GLmstr\SchIndX2.h"&str$(cno),"3 30")
52420 ! 
52440       fn_index_it(env$('Q')&"\GLmstr\ACTrans.h"&str$(cno),env$('Q')&"\GLmstr\AcTrIdx.h"&str$(cno),"1/71/17/13 12/2/2/4")
52460       fn_index_it(env$('Q')&"\GLmstr\AcTrans.h"&str$(cno),env$('Q')&"\GLmstr\tmp70.h"&str$(cno),"1 70")
52480 ! 
52500       fn_index_it(env$('Q')&"\GLmstr\ACGLFNSB.h"&str$(cno),env$('Q')&"\GLmstr\FNSbINDX.h"&str$(cno),",1 5")
52520       fn_index_it(env$('Q')&"\GLmstr\ACGLFNSc.h"&str$(cno),env$('Q')&"\GLmstr\FNScINDX.h"&str$(cno),",1 5")
52540       fn_index_it(env$('Q')&"\GLmstr\ACGLfNSf.h"&str$(cno),env$('Q')&"\GLmstr\FNSfINDX.h"&str$(cno),",1 5")
52560       fn_index_it(env$('Q')&"\GLmstr\ACGLfNSg.h"&str$(cno),env$('Q')&"\GLmstr\FNSGINDX.h"&str$(cno),",1 5")
52580       fn_index_it(env$('Q')&"\GLmstr\ACGLFNSi.h"&str$(cno),env$('Q')&"\GLmstr\FNSiINDX.h"&str$(cno),",1 5")
52600       fn_index_it(env$('Q')&"\GLmstr\ACGLFNSj.h"&str$(cno),env$('Q')&"\GLmstr\FNSjINDX.h"&str$(cno),",1 5")
52620 ! /r
52640 ! r: B
52660       fn_index_it(env$('Q')&"\GLmstr\bankrec.H"&str$(cno),env$('Q')&"\GLmstr\bankrec-idx.h"&str$(cno) ,"79/3/4 12/1/8")
52680 ! 
52700       fn_index_it(env$('Q')&"\GLmstr\BudgetInfo.h"&str$(cno),env$('Q')&"\GLmstr\BudIndx.h"&str$(cno),"1,14")
52720       fn_index_it(env$('Q')&"\GLmstr\BudInfo.h"&str$(cno),env$('Q')&"\GLmstr\BudInfo_Index.h"&str$(cno),"1,14")
52740 ! /r
52760 ! r: G
52780       fn_index_it(env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),env$('Q')&"\GLmstr\GLIndex.h"&str$(cno),"1 12")
52800       fn_index_it(env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),env$('Q')&"\GLmstr\glIndx2.h"&str$(cno),"13 30")
52820 ! 
52840 ! fn_index_it(env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),env$('Q')&"\GLmstr\fsindex.H"&str$(cno),"63 3") ! Secondary
52860 ! fn_index_it(env$('Q')&"\GLmstr\GLmstr.h"&str$(cno),env$('Q')&"\GLmstr\fsindex.H"&str$(cno),"66 3") ! Primary
52880 ! 
52900       fn_index_it(env$('Q')&"\GLmstr\gl1099.h"&str$(cno),env$('Q')&"\GLmstr\gl109Idx.h"&str$(cno),"1 8")
52920       fn_index_it(env$('Q')&"\GLmstr\GL1099.h"&str$(cno),env$('Q')&"\GLmstr\VNINDX2.h"&str$(cno),"9 25")
52940 ! 
52960       fn_index_it(env$('Q')&"\GLmstr\gltr1099.H"&str$(cno),env$('Q')&"\GLmstr\gltridx1.H"&str$(cno),"1 8")
52980 ! 
53000       fn_index_it(env$('Q')&"\GLmstr\GLBRec.h"&str$(cno),env$('Q')&"\GLmstr\GLRecIdx.h"&str$(cno),"1 24")
53020 ! 
53040       fn_index_it(env$('Q')&"\GLmstr\glstdad.H"&str$(cno),env$('Q')&"\GLmstr\glstdidx.h"&str$(cno),"1 12")
53060 ! 
53080       fn_index_it(env$('Q')&"\GLmstr\GLTrans.h"&str$(cno),env$('Q')&"\GLmstr\tmp70.h"&str$(cno),"1 70")
53100 ! /r
53120 ! r: P
53140       fn_index_it(env$('Q')&"\GLmstr\payeeglbreakdown.H"&str$(cno),env$('Q')&"\GLmstr\payeeglbkdidx.H"&str$(cno),"1 8")
53160 ! 
53180       fn_index_it(env$('Q')&"\GLmstr\paymstr.H"&str$(cno),env$('Q')&"\GLmstr\Payidx1.H"&str$(cno),"1 8")
53200       fn_index_it(env$('Q')&"\GLmstr\paymstr.H"&str$(cno),env$('Q')&"\GLmstr\Payidx2.H"&str$(cno),"9 38")
53220 ! 
53240       fn_index_it(env$('Q')&"\GLmstr\PRmstr.h"&str$(cno),env$('Q')&"\GLmstr\PRIndex.h"&str$(cno),"1 4")
53260 ! 
53280       fn_index_it(env$('Q')&"\GLmstr\PayeeGLBreakdown.h"&str$(cno),env$('Q')&"\GLmstr\payeeglbkdidx.h"&str$(cno),"1 8")
53300 ! /r
53320 ! r: R
53340       fn_index_it(env$('Q')&"\GLmstr\RatioMST.h"&str$(cno),env$('Q')&"\GLmstr\SchIndx2.h"&str$(cno),"3 30")
53360       fn_index_it(env$('Q')&"\GLmstr\RatioMST.h"&str$(cno),env$('Q')&"\GLmstr\RatioIdx.h"&str$(cno),"1 3")
53380       fn_index_it(env$('Q')&"\GLmstr\RatioMST.h"&str$(cno),env$('Q')&"\GLmstr\RaNamIdx.h"&str$(cno),"4 28")
53400 ! /r
53420 ! r: S
53440       sn=1
53460       fn_index_it(env$('Q')&"\GLmstr\schedule"&str$(sn)&".H"&str$(cno),env$('Q')&"\GLmstr\schedule"&str$(sn)&"-idx.h"&str$(cno),"1 12")
53480       for sn=1 to 8
53500         fn_index_it(env$('Q')&"\GLmstr\schedule"&str$(sn)&".H"&str$(cno),env$('Q')&"\GLmstr\schedule_idx"&str$(sn)&".h"&str$(cno),"1 12")
53520       next sn
53540 ! /r
53560 ! r: T
53580       fn_index_it(env$('Q')&"\GLmstr\TransCodes.h"&str$(cno),env$('Q')&"\GLmstr\transcodes-idx.h"&str$(cno),"1 2")
53600 ! /r
53620 ! r: W
53640       fn_index_it(env$('Q')&"\GLmstr\W2Box16.h"&str$(cno),env$('Q')&"\GLmstr\W2INDEX.h"&str$(cno),"1 8")
53660 ! /r
53680 ! /r
53700     else if system_id$='UB' then ! r:
53710       fn_index_it(env$('Q')&"\UBmstr\Reads_and_Chgs.h"&str$(cno), env$('Q')&"\UBmstr\Reads_and_Chgs-Key.h"&str$(cno),"1 10")
53720       fn_ub_index_customer(cno)
53740       fn_index_it(env$('Q')&"\UBmstr\UBAdrBil.h"&str$(cno), env$('Q')&"\UBmstr\adrIndex.h"&str$(cno),"1 10")
53780       fn_index_it(env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno), env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno),"1 19")
53800       fn_index_it(env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno), env$('Q')&"\UBmstr\UBTrdt.h"&str$(cno),"11/1 8/10")
53840       ! fn_index_it(env$('Q')&"\UBmstr\Note1.h"&str$(cno), env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno),"1 10")
53860       ! fn_index_it(env$('Q')&"\UBmstr\Deposit1.h"&str$(cno), env$('Q')&"\UBmstr\DepIdx1.h"&str$(cno),"1 10")
53880       fn_index_it(env$('Q')&"\UBmstr\Meter.h"&str$(cno), env$('Q')&"\UBmstr\Meter_Idx.h"&str$(cno),"1/11 10/2")
53900       fn_index_it(env$('Q')&"\UBmstr\MeterType.h"&str$(cno), env$('Q')&"\UBmstr\MeterTypeIdx.h"&str$(cno),"1 5")
53920       fn_index_it(env$('Q')&"\UBmstr\ubData\RateMst.h"&str$(cno), env$('Q')&"\UBmstr\ubData\RateIdx1.h"&str$(cno),"1 4")
53940       fn_index_it(env$('Q')&"\UBmstr\ubData\RateMst.h"&str$(cno), env$('Q')&"\UBmstr\ubData\RateIdx2.h"&str$(cno),"5 25")
53960       fn_index_it(env$('Q')&"\UBmstr\Cass1.h"&str$(cno), env$('Q')&"\UBmstr\Cass1Idx.h"&str$(cno),"1 10")
53970       fn_index_it(env$('Q')&"\UBmstr\workorder.h"&str$(cno), env$('Q')&"\UBmstr\wkindex.h"&str$(cno),"1/11 10/8")
53980 ! /r
54000     else if system_id$='PR' then ! r:
54020       fn_index_it(env$('Q')&"\PRmstr\EmpStatus.dat",env$('Q')&"\PRmstr\EmpStatus.Idx","1 2")
54040       fn_index_it(env$('Q')&"\PRmstr\MGLMstr.h"&str$(cno),env$('Q')&"\PRmstr\MGLIdx1.h"&str$(cno),"1 3")
54060       fn_index_it(env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno),env$('Q')&"\PRmstr\PRCKINDX.h"&str$(cno),"1 14")
54080       fn_index_it(env$('Q')&"\PRmstr\PRReport.h"&str$(cno),env$('Q')&"\PRmstr\PRRptIdx.h"&str$(cno),"1 2")
54100       fn_index_it(env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno),env$('Q')&"\PRmstr\RPIndex.h"&str$(cno),"1 8")
54120       fn_index_it(env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno),env$('Q')&"\PRmstr\RPIndx2.h"&str$(cno),"9 30")
54140       fn_index_it(env$('Q')&"\PRmstr\dd.h"&str$(cno),env$('Q')&"\PRmstr\DDidx1.h"&str$(cno),"1 10")
54160       fn_index_it(env$('Q')&"\PRmstr\glmstr.h"&str$(cno),env$('Q')&"\PRmstr\glIndex.h"&str$(cno),"1 12")
54180       fn_index_it(env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno), env$('Q')&"\PRmstr\SCIndex.h"&str$(cno),"1 3")
54200       fn_index_it(env$('Q')&"\PRmstr\W2Box16.h"&str$(cno), env$('Q')&"\PRmstr\W2Index.h"&str$(cno),"1 8")
54220       fn_index_it(env$('Q')&"\PRmstr\Burden.H"&str$(cno), env$('Q')&"\PRmstr\BurdenIdx.H"&str$(cno),"1 8")
54240       fn_index_it(env$('Q')&"\PRmstr\Category.H"&str$(cno), env$('Q')&"\PRmstr\categoryIDX.H"&str$(cno),"1 5")
54260       fn_index_it(env$('Q')&"\PRmstr\Department.h"&str$(cno), env$('Q')&"\PRmstr\Deptid4.h"&str$(cno),"12/1/9 12/8/3")
54280       fn_index_it(env$('Q')&"\PRmstr\Department.h"&str$(cno), env$('Q')&"\PRmstr\DeptIdx4.h"&str$(cno),"50/9/1 2/3/8")
54300       fn_index_it(env$('Q')&"\PRmstr\Department.h"&str$(cno), env$('Q')&"\PRmstr\DeptIdx.h"&str$(cno),"1/9 8/3")
54320       fn_index_it(env$('Q')&"\PRmstr\HourBreakdown.H"&str$(cno), env$('Q')&"\PRmstr\HourBreakdown-idx.H"&str$(cno),"1/9/14 8/5/8")
54340       fn_index_it(env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno), env$('Q')&"\PRmstr\checkidx.h"&str$(cno),"1 17")
54360       fn_index_it(env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno), env$('Q')&"\PRmstr\CheckIdx2.h"&str$(cno),"9/12/1 3/6/8")
54380       fn_index_it(env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno), env$('Q')&"\PRmstr\checkidx3.h"&str$(cno),"1/12/9 8/6/3")
54400       fn_index_it(env$('Q')&"\PRmstr\payrollreports.H"&str$(cno), env$('Q')&"\PRmstr\prrptidx.h"&str$(cno),"1 30")
54420       fn_index_it(env$('Q')&"\PRmstr\payrollreports.H"&str$(cno), env$('Q')&"\PRmstr\reportidx.H"&str$(cno),"1 30")
54440       fn_index_it(env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno), env$('Q')&"\PRmstr\PRCKINDX.h"&str$(cno),"1 14")
54460       fn_index_it(env$('Q')&"\PRmstr\PRReport.h"&str$(cno), env$('Q')&"\PRmstr\prrptidx.h"&str$(cno),"1 2")
54480       fn_index_it(env$('Q')&"\PRmstr\prTot.h"&str$(cno), env$('Q')&"\PRmstr\PRTotIdx.h"&str$(cno),"1 9")
54500       fn_index_it(env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&str$(cno), env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&str$(cno),"1 11")
54520       fn_index_it(env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&str$(cno), env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx2.h"&str$(cno),"1/27 8/14")
54540       fn_index_it(env$('Q')&"\PRmstr\DeptName.h"&str$(cno),env$('Q')&"\PRmstr\DepNameIdx.h"&str$(cno),"1 3")
54560 ! /r
54580     else if system_id$='CL' then ! r:
54582       fn_index_it(env$('Q')&"\CLmstr\BankMstr.h"&str$(cno), env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno),"1 2")
54584       fn_index_it(env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno), env$('Q')&"\CLmstr\DPTIDX1.h"&str$(cno),"1 5")
54586       fn_index_it(env$('Q')&"\CLmstr\GLmstr.H"&str$(cno), env$('Q')&"\CLmstr\GLINDEX.H"&str$(cno),"1 12")
54588       fn_index_it(env$('Q')&"\CLmstr\IvPaid.h"&str$(cno), env$('Q')&"\CLmstr\IVIndex.h"&str$(cno)," 1 20")
54590       fn_index_it(env$('Q')&"\CLmstr\JCBreakdownS"&wsid$&".h"&str$(cno), env$('Q')&"\CLmstr\jcbrkidx"&wsid$&".H"&str$(cno),"48 20")
54592       fn_index_it(env$('Q')&"\CLmstr\payeeglbreakdown.H"&str$(cno), env$('Q')&"\CLmstr\Payeeglbkdidx.H"&str$(cno),"1 8")
54594       fn_index_it(env$('Q')&"\CLmstr\paymstr.H"&str$(cno), env$('Q')&"\CLmstr\payidx1.H"&str$(cno),"1 8")
54596       fn_index_it(env$('Q')&"\CLmstr\PayTrans.h"&str$(cno), env$('Q')&"\CLmstr\Unpdidx2.H"&str$(cno),"31/27/1 2/4/26") ! index in year,monthday,reference
54598       fn_index_it(env$('Q')&"\CLmstr\PayTrans.h"&str$(cno), env$('Q')&"\CLmstr\UNPdIdx1.h"&str$(cno),"1 20")
54600       fn_index_it(env$('Q')&"\CLmstr\Receiptglbreakdown.h"&str$(cno), env$('Q')&"\CLmstr\receiptglbkdidx.h"&str$(cno),"1 8")
54602       fn_index_it(env$('Q')&"\CLmstr\Recmstr.h"&str$(cno), env$('Q')&"\CLmstr\Recidx1.h"&str$(cno)," 1 8")
54604       fn_index_it(env$('Q')&"\CLmstr\Tralloc.h"&str$(cno), env$('Q')&"\CLmstr\Tralloc-idx.h"&str$(cno)," 1 11")
54606       fn_index_it(env$('Q')&"\CLmstr\TrMstr.h"&str$(cno), env$('Q')&"\CLmstr\TrIdx1.h"&str$(cno)," 1 11")
54608       fn_index_it(env$('Q')&"\CLmstr\TrMstr.H"&str$(cno), env$('Q')&"\CLmstr\TrIdx2.H"&str$(cno)," 28/1 8/11")
54610       fn_index_it(env$('Q')&"\CLmstr\TrMstr.H"&str$(cno), env$('Q')&"\CLmstr\Tridx3.H"&str$(cno)," 16/12/4 2/4/8") ! index in year,monthday,reference
54614       fn_index_it(env$('Q')&"\CLmstr\unpdaloc.H"&str$(cno), env$('Q')&"\CLmstr\Uaidx1.H"&str$(cno),"9,12")
54616       fn_index_it(env$('Q')&"\CLmstr\unpdaloc.H"&str$(cno), env$('Q')&"\CLmstr\Uaidx2.H"&str$(cno),"1,20")
54620 ! /r
54640     else if system_id$='CR' then ! r:
54660       fn_index_it(env$('Q')&"\CRmstr\SCMSTR.H"&str$(cno),env$('Q')&"\CRmstr\SCIdx1.H"&str$(cno),"1 6")
54680       fn_index_it(env$('Q')&"\CRmstr\SCMSTR.H"&str$(cno),env$('Q')&"\CRmstr\SCIDX2.H"&str$(cno),"7 28")
54700       fn_index_it(env$('Q')&"\CRmstr\SCAlloc.h"&str$(cno),env$('Q')&"\CRmstr\allocidx.H"&str$(cno),"1/49 6/3")
54720       fn_index_it(env$('Q')&"\CRmstr\Batch.h"&str$(cno),env$('Q')&"\CRmstr\BatchIdx.h"&str$(cno),"1 2")
54740       fn_index_it(env$('Q')&"\CRmstr\GLmstr.h"&str$(cno),env$('Q')&"\CRmstr\glindex.h"&str$(cno),"1 12")
54760 ! fn_index_it(env$('Temp')&"\crWork."&session$,env$('Temp')&"\crworkidx."&wsid$,"98/1/157 1/8/3")
54780 ! fn_index_it(env$('Temp')&"\crwork2."&wsid$,env$('Temp')&"\crwork2idx."&wsid$,"1/56 8/3")
54800 ! fn_index_it(env$('Temp')&"\crWork."&session$,env$('Temp')&"\crworkidx2."&wsid$,"98/15/1/157 1/6/8/3")
54820       if exists(env$('Q')&'\CLmstr\Company.h'&str$(cno)) then ! r: CL
54840         fn_index_it(env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno),"1 11")
54860         fn_index_it(env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno),"28/1 8/11")
54880       end if  ! /r
54900       if exists(env$('Q')&"\UBmstr\UBMASTER.H"&str$(cno)) then ! r: ub
54920         fn_index_it(env$('Q')&"\UBmstr\UBMASTER.H"&str$(cno),env$('Q')&"\UBmstr\UBINDEX.H"&str$(cno),"1 10")
54940         fn_index_it(env$('Q')&"\UBmstr\UBMASTER.H"&str$(cno),env$('Q')&"\UBmstr\UBINDX2.H"&str$(cno),"354 7")
54960         fn_index_it(env$('Q')&"\UBmstr\UBMASTER.H"&str$(cno),env$('Q')&"\UBmstr\UBINDX3.H"&str$(cno),"11 30")
54980       end if  ! /r
55000       if exists(env$('Q')&"\PRmstr\Burden.h"&str$(cno)) then ! r: PR
55020         fn_index_it(env$('Q')&"\PRmstr\Burden.H"&str$(cno),env$('Q')&"\PRmstr\BurdenIdx.H"&str$(cno),"1 8")
55040       end if  ! /r
55140       if exists(env$('Q')&"\BLmstr\BLmstr.h"&str$(cno)) then ! r: BL
55160         fn_index_it(env$('Q')&"\BLmstr\BLmstr.h"&str$(cno),env$('Q')&"\BLmstr\BLINDX1.H"&str$(cno),"1 14")
55180         fn_index_it(env$('Q')&"\BLmstr\BLmstr.h"&str$(cno),env$('Q')&"\BLmstr\BLINDX2.H"&str$(cno),"15 28")
55200       end if  ! /r
55300 ! /r
55320     end if 
55340   fnend 
62000   def library fnub_index_customer(; cno)
62020     fn_index_it_setup
62040     if cno=0 then cno=val(env$('cno'))
62060     fnub_index_customer=fn_ub_index_customer(cno)
62080   fnend 
62100   def fn_ub_index_customer(cno)
62120     fn_index_it(env$('Q')&"\UBmstr\Customer.h"&str$(cno), env$('Q')&"\UBmstr\ubIndex.h"&str$(cno),"1 10")
62140     fn_index_it(env$('Q')&"\UBmstr\Customer.h"&str$(cno), env$('Q')&"\UBmstr\ubIndx2.h"&str$(cno),"354 7")
62160     fn_index_it(env$('Q')&"\UBmstr\Customer.h"&str$(cno), env$('Q')&"\UBmstr\ubIndx3.h"&str$(cno),"11 30u")
62180     fn_index_it(env$('Q')&"\UBmstr\Customer.h"&str$(cno), env$('Q')&"\UBmstr\ubIndx4.h"&str$(cno),"41 30")
62200     fn_index_it(env$('Q')&"\UBmstr\Customer.h"&str$(cno), env$('Q')&"\UBmstr\ubIndx5.h"&str$(cno),"1741/1743 2/7")
62220   fnend 
