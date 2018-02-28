02080 fn_setup
02100 library program$: fncheckfileversion
02120 fntop(program$)
02160 fncheckfileversion
06000 XIT: fnxit
06220 IGNORE: continue 
08000 def fn_setup
08020   if ~setup then 
08040     setup=1
08060     library 'S:\Core\Library': fntop
08080     library 'S:\Core\Library': fnerror
08100     library 'S:\Core\Library': fnxit
08120     library 'S:\Core\Library': fngethandle
08140     library 'S:\Core\Library': fnglcontrol
08160     library 'S:\Core\Library': fnindex_it
08180     library 'S:\Core\Library': fnCopy,fnFree,fnRename
08200     library 'S:\Core\Library': fnclient_has
08220     library 'S:\Core\Library': fngetdir2
08240     library 'S:\Core\Library': fnGetPp
08260     library 'S:\Core\Library': fncreg_write
08300     library 'S:\Core\Library': fnAddOneC
08320     library 'S:\Core\Library': fnkey_change
08340     library 'S:\Core\Library': fnSystemName$
08360     library 'S:\Core\Library': fnIniToReg
08380     library 'S:\Core\Library': fnOpenFile,fnCloseFile
08400     library 'S:\Core\Library': fnStatus,fnStatusPause
08402     library 'S:\Core\Library': fnInitialializeMeterLocation
08420     on error goto ERTN
08440     dim form$(0)*512
08460   end if
08480 fnend

10240 def library fncheckfileversion
10260   ! Checks the File versions and calls conversion programs if necessary
10280   ! This Fn is called from S:\Core\Program\Select Company.br and S:\Core\Company Import
10300   ! this library function checks to make sure all file versions for the
10320   ! current system are up to date - and runs the appropriate conversion  function if not
10340   if ~setup then let fn_setup
10380   ! there are other Library statements in this program - but they are placed in the section for which system they really belong to.
10400   ! _______________________________________________________________________
10420   dim tmpfile$*512,tmpkps(10),tmpkln(10),name$*512,kfname$*512
10430   dim kfnames$(1)*512
10440   ! ____________
10480   fnStatus('Running fnCheckFileVersion for '&env$('cursys')&' and Company Number '&env$('cno')) ! XXX
10490   fn_cfv_add_missing_files
10500   ! 
10520   if env$('cursys')='GL' then 
10540     fn_cfv_general_ledger
10600   else if env$('cursys')='PR' then 
10620     fn_cfv_payroll
10640     if fnclient_has('P2') then 
10660       fn_cfv_job_cost_payroll
10680     end if 
10740   else if env$('cursys')='UB' then 
10760     fn_cfv_utility_billing
10820   else if env$('cursys')='CL' then 
10840     fn_cfv_checkbook
10850   else if env$('cursys')='TM' then 
10852     fn_cfv_time_management
10860   end if 
10880   fnStatus('CheckFileVersion Completed')
19990 fnend 
24000 def fn_cfv_add_missing_files
24020   dim camf_filename$(0)*256
24040   dim camf_path$*256,camf_prog$*256,camf_ext$*128
24060   fngetdir2('S:\'&fnSystemName$&'\mstr\',mat camf_filename$, '','*.h99999')
24080   for camf_item=1 to udim(mat camf_filename$)
24100     fnGetPp(camf_filename$(camf_item),camf_path$,camf_prog$,camf_ext$)
24102     ! if lwrc$(camf_filename$(camf_item))='department' then pause
24120     if ~exists('[Q]\'&env$('cursys')&'mstr\'&camf_prog$&'.h[cno]') then 
24140       fnCopy('S:\'&fnSystemName$&'\mstr\'&camf_filename$(camf_item),'[Q]\'&env$('cursys')&'mstr\'&camf_prog$&'.h[cno]')
24160     end if 
24180   next camf_item
24200 fnend 
26000 def fn_file_setup_data(fsad_name$*512,fsad_recl,fsad_version_proper)
26020   dim g_fs_name$*512
26040   g_fs_name$=fsad_name$
26060   fn_make_data_file_exist(g_fs_name$,fsad_recl,fsad_version_proper)
26070   fn_min_rln(fsad_name$,fsad_recl)
26080 fnend 
28000 def fn_file_setup_index(fsi_kfname$*512,fsi_kps$,fsi_kln$)
28020   dim g_fs_kfname$*512
28040   g_fs_kfname$=fsi_kfname$
28160   if ~exists(fsi_kfname$) or lwrc$(env$('force_reindex'))='yes' or ~fn_check_indexes(g_fs_name$,fsi_kfname$,fsi_kps$,fsi_kln$) then 
28180     fnindex_it(g_fs_name$,fsi_kfname$,fsi_kps$&' '&fsi_kln$)
28200   end if 
28220 fnend 
30000 def fn_make_data_file_exist(name$*512,myrln,version_proper)
30040   if exists(name$)=0 then 
30050     fnStatus('Creating new file: Name='&name$&',Shr,Use,RecL='&str$(myrln)&',Version='&str$(version_proper))
30060     open #tmp:=fngethandle: 'Name='&name$&',Shr,Use,RecL='&str$(myrln)&',Version='&str$(version_proper),internal,outIn 
30080     close #tmp: 
30100   end if 
30120   ! 
30140 fnend 
31000 def fn_check_indexes(name$*512,mat kfnames$,mat kps$,mat kln$)
31020   ci_return=1 ! function should return 1 if all indexes tested are fine or 0 if any tested fail
31040   for ci_item=1 to udim(mat kfnames$)
31060     str2mat(kps$(ci_item),mat ci_kps$,'/')
31080     str2mat(kln$(ci_item),mat ci_kln$,'/')
31090     CI_OPEN_IT: ! 
31100     open #h_ci_tmp:=fngethandle: 'name='&name$&',KFName='&kfnames$(ci_item),internal,input,keyed ioerr CI_OPEN_ERR
31120     ! 
31140     for x=1 to udim(mat ci_kps$)
31160       if kps(h_ci_tmp,x)<>val(ci_kps$(x)) then 
31180         fnStatus('Key Position mismatch!') ! should these use fnStatus ??
31200         fnStatus(' Data File: '&name$)
31220         fnStatus('Index File: '&kfnames$(ci_item))
31240         fnStatus('Key Part: '&str$(x))
31260         fnStatus('Key Position should be '&ci_kps$(x)&' but it is '&str$(kps(h_ci_tmp,x)))
31280         ci_return=0
31300       end if 
31320       ! 
31340       if kln(h_ci_tmp,x)<>val(ci_kln$(x)) then 
31360         fnStatus('Key Length mismatch!')
31380         fnStatus(' Data File: '&name$)
31400         fnStatus('Index File: '&kfnames$(ci_item))
31420         fnStatus('Key Part: '&str$(x))
31440         fnStatus('Key Length should be '&ci_kln$(x)&' but it is '&str$(kln(h_ci_tmp,x)))
31460         ci_return=0
31480       end if 
31500       ! 
31520     next x
31540     close #h_ci_tmp: 
31560   next ci_item
31580   goto CI_XIT
31600   CI_OPEN_ERR: ! 
31620   fnStatus('error '&str$(err)&' opening Name='&name$&',KFName='&kfnames$(ci_item))
31640   if err=607 or err=632 then 
31660     fnStatus('indexing to fix it')
31680     fnindex_it(g_fs_name$,fsi_kfname$,fsi_kps$&' '&fsi_kln$)
31690     goto CI_OPEN_IT
31700   else 
31720     fnStatus('error unhandled')
31740   end if 
31760   CI_XIT: ! 
31780   fn_check_indexes=ci_return
31800 fnend 
32000 ! <Updateable Region: ERTN>
32020 ERTN: fnerror(program$,err,line,act$,"NO")
32040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
32060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
32080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
32100 ERTN_EXEC_ACT: execute act$ : goto ERTN
32120 ! /region
33000 def fn_min_rln(mr_filename$*512,mr_rln_minimum)
33020   open #h_mr_file:=fngethandle: "Name="&mr_filename$&",Shr",internal,input 
33040   mr_rln_current=rln(h_mr_file)
33060   close #h_mr_file: 
33080   if mr_rln_current<mr_rln_minimum then 
33100     fnCopy(mr_filename$,mr_filename$,mr_rln_minimum)
33140     fnFree("[Q]\x")
33160   end if  ! 
33180 fnend  ! fn_min_rln
34000 def fn_check_version(cv_version_current,cv_version_proper,cv_file$*256)
34020   cv_return=1 ! function should return 1 if version tested matches or 0 if versions are different
34040   if cv_version_current<>cv_version_proper then 
34060     fnStatus('Version Error of file:'&cv_file$ )
34080     fnStatus('     Version Current: '&str$(cv_version_current))
34100     fnStatus('     Version  Proper: '&str$(cv_version_proper))
34120     cv_return=0
34140   end if 
34160   fn_check_version=cv_return
34180 fnend 
35000 def fn_get_tmp(h_tmp,mat tmpkps,mat tmpkln,&tmpversion,&tmprln,&tmpfile$)
35020   mat tmpkps=(0)
35040   mat tmpkln=(0)
35060   tmpversion=version(tmp)
35080   tmprln=rln(tmp)
35100   tmpfile$=file$(tmp)
35120   for j=1 to udim(tmpkps)
35140     tmpkps(j)=kps(tmp,j)
35160     tmpkln(j)=kln(tmp,j)
35180   next j
35200   close #h_tmp: 
35220 fnend 
39000 def fn_cfv_time_management
39020   ! open #h_tmwk1:=fngethandle:
39040   ! fn_check_version(tmpversion,version_proper,'')
39060 fnend
43000 def fn_cfv_utility_billing
43010   if exists("[Q]\UBmstr")=0 then execute 'MkDir "[Q]\UBmstr"'
43020   if exists("[Q]\UBmstr\UBdata")=0 then execute 'MkDir "[Q]\UBmstr\UBdata"'
43030   if exists("[Q]\WorkOrder")=0 then execute 'MkDir "[Q]\WorkOrder"'
43040   ! if ~exists('[Q]\INI\Utility Billing') then execute 'mkdir "[Q]\INI\Utility Billing"'
43050   ! if ~exists('[Q]\INI\acs'&env$('cursys')&'\UBdata') then execute 'mkdir [Q]\INI\acs'&env$('cursys')&'\UBdata'
43060   fn_ini_move(env$('cursys'))
43070   fnIniToReg
43080   fn_reg_rename(env$('cursys'))
43090   ! r: move ubBkNo.h into CReg and delete ubBkNo.h
43100   if exists('[Q]\UBmstr\ubBkNo.h[cno]') then 
43110     open #h_ubbkno:=fngethandle: "Name=[Q]\UBmstr\ubBkNo.h[cno]",internal,outIn,relative 
43120     read #h_ubbkno,using "Form POS 1,2*N 3",rec=1: bkno1,bkno2  noRec CFVUB_RPDATE_NOREC
43130     fncreg_write('Route Low',str$(bkno1)) ! Route Number Range Low
43140     fncreg_write('Route High',str$(bkno2)) ! Route Number Range High
43150     CFVUB_RPDATE_NOREC: ! 
43160     close #h_ubbkno,free: 
43170   end if 
43180   ! /r
43190   ! UB_COMPANY: !
43200   ! Primary No Index
43210   ! do company different
43220   ! - don't make it exist,
43230   ! - and skip out if it don't exist
43240   name$="[Q]\UBmstr\Company.h[cno]"
43250   kfname$=''
43260   myrln=133
43270   version_proper=0
43280   open #tmp:=fngethandle: 'Name='&name$&',Shr',internal,outIn,relative ioerr SKIP_UB_COMPANY
43290   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
43300   fn_check_version(tmpversion,version_proper,tmpfile$)
43310   if tmprln<>myrln then 
43320     fnStatus('Record Length Error in File: '&tmpfile$)
43330     fnStatus('         RLn: '&str$(tmprln))
43340     fnStatus('Fixing the Record Length of Company')
43350     fnCopy("[Q]\UBmstr\Company.h[cno]","[Q]\UBmstr\Company.h[cno]",133)
43360   end if 
43370   SKIP_UB_COMPANY: ! 
43380   ! 
43390   fn_file_setup_data("[Q]\UBmstr\Customer.h[cno]",2067,1)
43400   fn_file_setup_index("[Q]\UBmstr\ubIndex.h[cno]","1","10")
43410   fn_file_setup_index("[Q]\UBmstr\ubIndx2.h[cno]","354","7")
43420   fn_file_setup_index("[Q]\UBmstr\ubIndx3.h[cno]","11","30")
43430   fn_file_setup_index("[Q]\UBmstr\ubIndx4.h[cno]","41","30")
43440   fn_file_setup_index("[Q]\UBmstr\ubIndx5.h[cno]","1741/1743","2/7")
43450   ! 
43460   fn_file_setup_data("[Q]\UBmstr\ubAdrBil.h[cno]",130,0)
43470   fn_file_setup_index("[Q]\UBmstr\AdrIndex.h[cno]",'1','10')
43480   ! 
43490   !     fn_file_setup_data("[Q]\UBmstr\Deposit1.h[cno]",16,0)
43500   !     fn_file_setup_index("[Q]\UBmstr\DepIdx1.h[cno]",'1','10')
43510   fn_file_setup_data("[Q]\UBmstr\Deposit2.h[cno]",73,0)
43520   fn_file_setup_index("[Q]\UBmstr\Deposit2Index.h[cno]",'1','10')
43530   ! 
43540   fn_file_setup_data("[Q]\UBmstr\workOrder.h[cno]",600,0)
43550   fn_file_setup_index("[Q]\UBmstr\wkIndex.h[cno]",'1/11','10/8')
43560   ! 
43570   fn_file_setup_data("[Q]\UBmstr\MeterType.h[cno]",128,1)
43580   fn_file_setup_index("[Q]\UBmstr\MeterTypeIdx.h[cno]",'1','5')
43590   ! 
43600   ! no need now that we have U4 Meter Location    !   fn_file_setup_data("[Q]\UBmstr\Meter.h[cno]",384,1)
43610   ! no need now that we have U4 Meter Location    !   fn_file_setup_index("[Q]\UBmstr\Meter_Idx.h[cno]",'1/11','10/2')
43612   !
43614   fnInitialializeMeterLocation
43620   !
43630   if exists('[Q]\UBmstr\CityStZip.dat') then
43640     fnStatus('Migrating UB City State Zip records into Core City State Zip table...')
43650     open #hUbCsz:=fngethandle: "Name=[Q]\UBmstr\CityStZip.dat,KFName=[Q]\UBmstr\CityStZip.idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outIn,keyed 
43660     dim cszData$(0)*128,cszDataN(0),csz$*30
43670     hCoCsz:=fn_open('CO City State Zip',mat cszData$,mat cszDataN,mat form$)
43680     do
43690       read #hUbCsz,using 'form pos 1,C 30': csz$ eof CszFinis
43700       restore #hCoCsz,key=csz$: nokey CszAdd
43710       goto CszNext
43720       CszAdd: !
43730       write #hCoCsz,using 'form pos 1,C 30': csz$
43740       delete #hUbCsz: 
43750       CszNext: !
43760     loop
43770     CszFinis: !
43780     close #hUbCsz:
43790     fnFree('[Q]\UBmstr\CityStZip.dat')
43800     fnFree('[Q]\UBmstr\CityStZip.idx')
43810     fnCloseFile(hCoCsz,'CO City State Zip')
43820   end if
43830   !
43840   if exists('[Q]\UBmstr\Collections-'&wsid$&'.h[cno]') then
43850     if fnCopy('[Q]\UBmstr\Collections-'&wsid$&'.h[cno]','[Q]\UBmstr\Collections-'&env$('acsUserId')&'.h[cno]') then
43860       fnFree('[Q]\UBmstr\Collections-'&wsid$&'.h[cno]')
43870     end if
43880   end if
43890   !
43900   if exists('[Q]\UBmstr\IpChg01.h[cno]') then
43910     open #hupipchg:=fngethandle: "Name=[Q]\UBmstr\IpChg01.h[cno],RecL=80,Use",internal,outIn ioerr ubipchgOpenErr
43920     read #hupipchg,using "Form pos 1,N 6": d2 ioerr ignore
43930     close #hupipchg,free: 
43940     for wsidItem=1 to 99
43950       fnFree('[Q]\UBmstr\IpChg'&cnvrt$('pic(##)',wsidItem)&'.h[cno]')
43960     nex wsidItem
43970     ubipchgOpenErr: !
43980     fncreg_write('Meter Reading Date Current',str$(d2))
43990   end if
44000   if exists("[Q]\UBmstr\per1000.h[cno]") then
44010     dim range(16)
44020     open #hPer1000:=fngethandle: "Name=[Q]\UBmstr\per1000.h[cno],Shr",internal,outIn,relative 
44030     read #hPer1000,using "Form pos 1,16*n 10,n 2,c 1": mat range,wrate,weg$
44040     fncreg_write('Per 1000 Usage - Rate Code ',weg$)
44050     fncreg_write('Per 1000 Usage - Service for Analysis ',str$(wrate))
44060     for rangeItem=1 to 16
44070       fncreg_write('Per 1000 Usage - Range '&str$(rangeItem),str$(range(rangeItem)))
44080     nex rangeItem
44090     close #hPer1000: 
44100     fnFree('[Q]\UBmstr\per1000.h[cno]')
44110   end if
44120   ! 
44130   fn_file_setup_data("[Q]\UBmstr\ubData\RateMst.h[cno]",374,0)
44140   fn_file_setup_index("[Q]\UBmstr\ubData\RateIdx1.h[cno]",'1','4')
44150   fn_file_setup_index("[Q]\UBmstr\ubData\RateIdx2.h[cno]",'5','25')
44160   ! 
44170 fnend 
52000 def fn_cfv_checkbook
52020   ! Checkbook Only
52040   library 'S:\Core\Library': fntrmstr_v1_to_v2, fntralloc_v1_to_v2, fnunpdaloc_v1_to_v2, fnpaytrans_v1_to_v2, fnpaymstr_v0_to_v1, fnglmstrtorecl62, fntrmstr_v0_to_v1
52060   if exists("[Q]\CLmstr")=0 then execute "MkDir [Q]\CLmstr"
52070   ! if ~exists('[Q]\INI\Checkbook') then execute 'mkdir "[Q]\INI\Checkbook"'
52072   fn_ini_move(env$('cursys'))
52074   fnIniToReg
52076   fn_reg_rename(env$('cursys'))
52080   ! 
52090   ! if ~exists('[Q]\CLmstr\PayeeType.dat') and exists('S:\acsCL\PayeeType.dat') then
52092   !   fnCopy('S:\acsCL\PayeeType.dat','[Q]\CLmstr\PayeeType.dat')
52094   !   fnCopy('S:\acsCL\PayeeType.Idx','[Q]\CLmstr\PayeeType.Idx')
52096   ! end if
52098   !
52100   CL_TRMSTR1: ! Primary Non-Split Index
52120   fn_file_setup_data("[Q]\CLmstr\TrMstr.h[cno]",78,2)
52140   fn_file_setup_index("[Q]\CLmstr\TrIdx1.h[cno]",'1','11')
52180   open #tmp:=fngethandle: 'Name='&g_fs_name$&',KFName='&g_fs_kfname$&',Shr',internal,outIn,keyed 
52200   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
52220   if ~fn_check_version(tmpversion,version_proper:=2,g_fs_name$) then 
52240     if tmpversion=0 or tmpversion=-1 then 
52260       fntrmstr_v0_to_v1
52280       goto CL_TRMSTR1
52300     end if 
52320     if tmpversion=1 then 
52340       fntrmstr_v1_to_v2
52360       goto CL_TRMSTR1
52380     end if 
52400   end if 
52420   ! CL_TRMSTR2: ! Secondary Split Index
52440   fn_file_setup_index("[Q]\CLmstr\TrIdx2.h[cno]",'28/1','8/11')
52460   ! CL_TRMSTR3: ! Secondary 3-Split Index
52480   fn_file_setup_index("[Q]\CLmstr\TrIdx3.h[cno]",'16/12/4','2/4/8')
52500   ! 
53880   CL_TRALLOC: ! 1 file and 1 key
53900   ! (index file did not exists in previous versions,
53920   ! so it'll build it if need be here, before it opens it)
53940   name$="[Q]\CLmstr\TrAlloc.h[cno]"
53960   kfname$="[Q]\CLmstr\TrAlloc-Idx.h[cno]"
53980   myrln=80
54000   version_proper=2
54020   fn_make_data_file_exist(name$,myrln,version_proper)
54040   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
54060     fnindex_it(name$,kfname$,'1 11')
54080   end if 
54100   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
54120   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
54140   fn_check_version(tmpversion,version_proper,tmpfile$)
54160   if tmpversion=1 or tmpversion=0 or tmpversion=-1 then 
54180     fntralloc_v1_to_v2
54200     goto CL_TRALLOC
54220   end if 
54240   if tmprln<>myrln then 
54260     pr 'Record Length Error in File: '&tmpfile$
54280     pr '         RLn: '&str$(tmprln)
54300   end if 
54320   if tmpkps(1)<>1 then 
54340     pr 'Key Position Error in '&kfname$
54360     pr '         KPs: '&str$(tmpkps(1))
54380   end if 
54400   if tmpkln(1)<>11 then 
54420     pr 'Key Length Error in '&kfname$
54440     pr '         KLn: '&str$(tmpkln(1))
54460   end if 
54480   ! 
54500   CL_UNPDALOC1: ! Primary, Non-Split Index
54520   ! it is important that if conversion from version 1 to 2 occur on this
54540   ! that this file process before PayTrans - the file it is linked to
54560   ! So that record sequence is maintained.
54580   name$="[Q]\CLmstr\UnPdAloc.h[cno]"
54600   kfname$="[Q]\CLmstr\UAIdx1.h[cno]"
54620   myrln=67
54640   version_proper=2
54660   fn_make_data_file_exist(name$,myrln,version_proper)
54680   L1840: ! 
54700   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
54720     fnindex_it(name$,kfname$,'9 12')
54740   end if 
54760   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
54780   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
54800   fn_check_version(tmpversion,version_proper,tmpfile$)
54820   if tmpversion=1 or tmpversion=0 or tmpversion=-1 then 
54840     fnunpdaloc_v1_to_v2
54860     goto CL_UNPDALOC1
54880   end if 
54900   if tmprln<>myrln then 
54920     pr 'Record Length Error in File: '&tmpfile$
54940     pr '         RLn: '&str$(tmprln)
54960   end if 
54980   x=1 : if tmpkps(x)<>9 then 
55000     pr 'Key Position ('&str$(x)&') Error in '&kfname$
55020     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
55040     pr 'fixing it'
55060     fnFree(kfname$)
55080     goto L1840
55100   end if 
55120   x=1 : if tmpkln(x)<>12 then 
55140     pr 'Key Length ('&str$(x)&') Error in '&kfname$
55160     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
55180   end if 
55200   ! 
55220   ! CL_UNPDALOC2: ! Secondary, Non-Split Index
55240   name$="[Q]\CLmstr\UnPdAloc.h[cno]"
55260   kfname$="[Q]\CLmstr\UAIdx2.h[cno]"
55280   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
55300     fnindex_it(name$,kfname$,'1 20')
55320   end if 
55340   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
55360   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
55380   x=1 : if tmpkps(x)<>1 then 
55400     pr 'Key Position ('&str$(x)&') Error in '&kfname$
55420     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
55440   end if 
55460   x=1 : if tmpkln(x)<>20 then 
55480     pr 'Key Length ('&str$(x)&') Error in '&kfname$
55500     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
55520   end if 
55540   ! 
55560   CL_PAYTRANS1: ! Primary Non-Split Index
55580   name$="[Q]\CLmstr\PayTrans.h[cno]"
55600   kfname$="[Q]\CLmstr\UnPdIdx1.h[cno]"
55620   myrln=114
55640   version_proper=2
55660   fn_make_data_file_exist(name$,myrln,version_proper)
55680   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
55700     fnindex_it(name$,kfname$,'1 20')
55720   end if 
55740   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
55760   fn_check_version(tmpversion,version_proper,tmpfile$)
55780   if tmpversion=1 or tmpversion=0 or tmpversion=-1 then 
55800     fnpaytrans_v1_to_v2 : goto CL_PAYTRANS1
55820   end if 
55840   if tmprln<>myrln then 
55860     pr 'Record Length Error in File: '&tmpfile$
55880     pr '         RLn: '&str$(tmprln)
55900     pr 'Fixing it'
55920     fnCopy("[Q]\CLmstr\PayTrans.h[cno]","[Q]\X."&session$&' -'&str$(myrln))
55940     fnFree("[Q]\CLmstr\PayTrans.h[cno]")
55960     fnRename("[Q]\X."&session$,"[Q]\CLmstr\PayTrans.h[cno]")
55980   end if 
56000   x=1 : if tmpkps(x)<>1 then 
56020     pr 'Key Position ('&str$(x)&') Error in '&kfname$
56040     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
56060   end if 
56080   x=1 : if tmpkln(x)<>20 then 
56100     pr 'Key Length ('&str$(x)&') Error in '&kfname$
56120     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
56140   end if 
56160   ! 
56180   ! CL_PAYTRANS2: ! seconday 3-Split Index
56200   name$="[Q]\CLmstr\PayTrans.h[cno]"
56220   kfname$="[Q]\CLmstr\UnPdIdx2.h[cno]"
56240   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
56260     fnindex_it(name$,kfname$,'31/27/1 2/4/26')
56280   end if 
56300   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
56320   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
56340   x=1 : if tmpkps(x)<>31 then 
56360     pr 'Key Position ('&str$(x)&') Error in '&kfname$
56380     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
56400   end if 
56420   x=2 : if tmpkps(x)<>27 then 
56440     pr 'Key Position ('&str$(x)&') Error in '&kfname$
56460     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
56480   end if 
56500   x=3 : if tmpkps(x)<>1 then 
56520     pr 'Key Position ('&str$(x)&') Error in '&kfname$
56540     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
56560   end if 
56580   x=1 : if tmpkln(x)<>2 then 
56600     pr 'Key Length ('&str$(x)&') Error in '&kfname$
56620     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
56640   end if 
56660   x=2 : if tmpkln(x)<>4 then 
56680     pr 'Key Length ('&str$(x)&') Error in '&kfname$
56700     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
56720   end if 
56740   x=3 : if tmpkln(x)<>26 then 
56760     pr 'Key Length ('&str$(x)&') Error in '&kfname$
56780     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
56800   end if 
56820   ! 
56840   CL_PAYMSTR1: ! Primary Non-Split Index
56860   name$="[Q]\CLmstr\PayMstr.h[cno]"
56880   kfname$="[Q]\CLmstr\PayIdx1.h[cno]"
56900   myrln=736
56920   version_proper=1
56940   fn_make_data_file_exist(name$,myrln,version_proper)
56960   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
56980     fnindex_it(name$,kfname$,'1 8')
57000   end if 
57020   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
57040   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
57060   fn_check_version(tmpversion,version_proper,tmpfile$)
57080   if tmpversion=0 or tmpversion=-1 then 
57100     fnpaymstr_v0_to_v1 : goto CL_PAYMSTR1
57120   end if 
57140   if tmprln<>myrln then 
57160     pr 'Record Length Error in File: '&tmpfile$
57180     pr '         RLn: '&str$(tmprln)
57200   end if 
57220   x=1 : if tmpkps(x)<>1 then 
57240     pr 'Key Position ('&str$(x)&') Error in '&kfname$
57260     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
57280   end if 
57300   x=1 : if tmpkln(x)<>8 then 
57320     pr 'Key Length ('&str$(x)&') Error in '&kfname$
57340     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
57360   end if 
57380   ! CL_RECMSTR1: ! Primary Non-Split Index
57400   name$="[Q]\CLmstr\RecMstr.h[cno]"
57420   kfname$="[Q]\CLmstr\RecIdx1.h[cno]"
57440   myrln=38
57460   version_proper=1
57480   fn_make_data_file_exist(name$,myrln,version_proper)
57500   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
57520     fnindex_it(name$,kfname$,'1 8')
57540   end if 
57560   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
57580   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
57600   fn_check_version(tmpversion,version_proper,tmpfile$)
57620   if tmpversion=0 or tmpversion=-1 then 
57640     fnpaymstr_v0_to_v1
57660     goto CL_PAYMSTR1
57680   end if 
57700   if tmprln<>myrln then 
57720     pr 'Record Length Error in File: '&tmpfile$
57740     pr '         RLn: '&str$(tmprln)
57760   end if 
57780   x=1 : if tmpkps(x)<>1 then 
57800     pr 'Key Position ('&str$(x)&') Error in '&kfname$
57820     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
57840   end if 
57860   x=1 : if tmpkln(x)<>8 then 
57880     pr 'Key Length ('&str$(x)&') Error in '&kfname$
57900     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
57920   end if 
57940   ! 
57960   ! CL_PAYMSTR2: ! Secondary, Non-Split Index
57980   name$="[Q]\CLmstr\PayMstr.h[cno]"
58000   kfname$="[Q]\CLmstr\PayIdx2.h[cno]"
58020   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
58040     fnindex_it(name$,kfname$,'9 30')
58060   end if 
58080   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
58100   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
58120   x=1 : if tmpkps(x)<>9 then 
58140     pr 'Key Position ('&str$(x)&') Error in '&kfname$
58160     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
58180   end if 
58200   x=1 : if tmpkln(x)<>28 then 
58220     pr 'Key Length ('&str$(x)&') Error in '&kfname$
58240     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
58260   end if 
58280   ! 
58300   ! CL_GLMSTR1: ! Primary Non-Split Index
58310   name$="[Q]\CLmstr\GLmstr.h[cno]"
58320   kfname$="[Q]\CLmstr\GLIndex.h[cno]"
58330   myrln=62
58340   !   version_proper=0
58350   !   fn_make_data_file_exist(name$,myrln,version_proper)
58360   !   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then
58370   !     fnindex_it(name$,kfname$,'1 12')
58380   !   end if
58390   fn_file_setup_data("[Q]\CLmstr\GLmstr.h[cno]",62,1)
58400   fn_file_setup_index("[Q]\CLmstr\GLIndex.h[cno]",'1','12')
58410   open #tmp:=fngethandle: 'Name='&g_fs_name$&',KFName='&g_fs_kfname$&',Shr',internal,outIn,keyed 
58420   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
58430   fn_check_version(tmpversion,version_proper,tmpfile$)
58440   !   if tmprln<>myrln then
58450   !     pr 'Record Length Error in File: '&tmpfile$
58460   !     pr '         RLn: '&str$(tmprln)
58470   !   end if
58480   if tmprln=72 or tmprln=80 then let fnglmstrtorecl62
58490   !   x=1 : if tmpkps(x)<>1 then
58500   !     pr 'Key Position ('&str$(x)&') Error in '&kfname$
58510   !     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
58520   !   end if
58530   !   x=1 : if tmpkln(x)<>12 then
58540   !     pr 'Key Length ('&str$(x)&') Error in '&kfname$
58550   !     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
58560   !   end if
58570   ! 
58820   ! CL_PAYEEGLBREAKDOWN: ! Primary Non-Split Index
58840   fn_file_setup_data("[Q]\CLmstr\payeeglbreakdown.h[cno]",56,1)
58860   fn_file_setup_index("[Q]\CLmstr\payeeglbkdidx.h[cno]",'1','8')
59000   ! CL_GLCONTROL: ! Primary Non-Split Index
59020   fn_file_setup_data("[Q]\CLmstr\fundmstr.h[cno]",75,0)
59040   fn_file_setup_index("[Q]\CLmstr\fundidx1.h[cno]",'1','3')
59060   open #tmp:=fngethandle: 'Name='&g_fs_name$&',KFName='&g_fs_kfname$&',Shr',internal,outIn,keyed 
59080   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
59100   fn_check_version(tmpversion,version_proper,tmpfile$)
59120   if tmprln=63 then let fnglcontrol
59140   ! 
59160   fn_file_setup_data("[Q]\CLmstr\BankMstr.h[cno]",64,1)
59180   fn_file_setup_index("[Q]\CLmstr\BankIdx1.h[cno]",'1','2')
59200   !
59220   if ~exists('[Q]\CLmstr\TransactionType.dat') then
59240     open #hTransactionType:=fngethandle: "Name=[Q]\CLmstr\TransactionType.dat,Version=1,KFName=[Q]\CLmstr\TransactionType.Idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outIn,keyed
59260     write #hTransactionType,using 'form pos 1,n 1,C 25': 1,'Check (Disbursment)'
59280     write #hTransactionType,using 'form pos 1,n 1,C 25': 2,'Deposit   (Receipt)'
59300     close #hTransactionType:
59320   end if
59340   if ~exists('[Q]\CLmstr\PayeeType.dat') then
59360     open #hPayeeType:=fngethandle: "Name=[Q]\CLmstr\PayeeType.dat,Version=1,KFName=[Q]\CLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed
59380     write #hPayeeType,using 'form pos 1,n 2,C 25': 0,'Not Applicable'
59400     write #hPayeeType,using 'form pos 1,n 2,C 25': 7,'Non-Employee Compensation'
59420     close #hPayeeType:
59440   end if
59460 fnend 
62000 def fn_cfv_payroll
62020   if exists("[Q]\PRmstr")=0 then execute "MkDir [Q]\PRmstr"
62022   ! if ~exists('[Q]\INI\Payroll') then execute 'mkdir "[Q]\INI\Payroll"'
62024   fn_ini_move(env$('cursys'))
62026   fnIniToReg
62028   fn_reg_rename(env$('cursys'))
62040   ! r: move CheckInfo.h into CReg and delete checkinfo.h
62060   if exists('[Q]\PRmstr\Checkinfo.h[cno]') then 
62080     open #h_pr_checkinfo:=fngethandle: "Name=[Q]\PRmstr\Checkinfo.h[cno],USE,RecL=128",internal,outIn,relative 
62100     read #h_pr_checkinfo,using "form pos 1,3*c 1,c 3,c 1,n 3,c 5",rec=1: pre$,acsclcv$,ficam1$,sc1$,accr$,bankcode,compcode$ noRec CFVPR_CHECKINFO_NOREC
62120     fncreg_write('Prenumbered Checks',pre$)
62140     fncreg_write('Post to CL',acsclcv$)
62160     fncreg_write('Post Employer Portion of FiCA',ficam1$)
62180     fncreg_write('Check Format',sc1$)
62200     fncreg_write('Print Vacation and Sick Leave on Check',accr$)
62220     fncreg_write('CL Bank Code',str$(bankcode))
62240     fncreg_write('Comp Time Code',compcode$)
62260   CFVPR_CHECKINFO_NOREC: ! 
62280     close #h_pr_checkinfo,free: 
62300   end if 
62320   ! /r
63000   ! r: move rpDate.h into CReg and delete rpDate.h
63020   if exists('[Q]\PRmstr\rpDate.h[cno]') then 
63040     open #h_pr_rpdate:=fngethandle: "Name=[Q]\PRmstr\rpDate.h[cno]",internal,outIn,relative 
63060     dim cfvpr_rpdate_d$*20
63080     read #h_pr_rpdate,using 'Form POS 1,N 6,C 20': cfvpr_rpdate_ppd,cfvpr_rpdate_d$  noRec CFVPR_RPDATE_NOREC
63100     fncreg_write('calculation date',str$(cfvpr_rpdate_ppd)) ! quarter ending date, i think - definately NOT the payroll calculation date!
63120     fncreg_write('calculation date text',cfvpr_rpdate_d$) ! quarter ending date
63140   CFVPR_RPDATE_NOREC: ! 
63160     close #h_pr_rpdate,free: 
63180   end if 
63200   ! /r
64000   fn_file_setup_data("[Q]\PRmstr\DeptName.h[cno]",32,0)
64020   fn_file_setup_index("[Q]\PRmstr\DeptNameIdx-idx.h[cno]",'1','3')
64040   ! 
64060   fn_file_setup_data("[Q]\PRmstr\mglmstr.h[cno]",135,0)
64080   fn_file_setup_index("[Q]\PRmstr\mglidx1-idx.h[cno]",'1','3')
64100   ! 
64120   fn_file_setup_data("[Q]\PRmstr\HourBreakdown.h[cno]",39,0)
64140   fn_file_setup_index("[Q]\PRmstr\HourBreakdown-idx.h[cno]",'1/9/14','8/5/8')
64160   ! r: Dates.h
64180   fn_file_setup_data("[Q]\PRmstr\Dates.h[cno]",76,0)
64200   open #tmp:=fngethandle: "Name=[Q]\PRmstr\Dates.h[cno],Use,RecL=76,Shr",internal,outIn,relative 
64220   read #tmp,using "form pos 1,6*n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4 noRec PR_WRITE_BLANK_DATE_REC
64240   goto PR_CLOSE_DATE
64260   PR_WRITE_BLANK_DATE_REC: ! 
64280   write #tmp,using "form pos 1,6*n 8",rec=1: beg_date,end_date,qtr1,qtr2,qtr3,qtr4
64300   PR_CLOSE_DATE: ! 
64320   close #tmp: 
64340   ! /r
64360   fn_file_setup_data("[Q]\PRmstr\PayrollChecks.h[cno]",224,0)
64380   fn_file_setup_index("[Q]\PRmstr\CheckIdx.h[cno]",'1','17')
64400   fn_file_setup_index("[Q]\PRmstr\CheckIdx2.h[cno]",'1/12/9','3/6/8')
64420   fn_file_setup_index("[Q]\PRmstr\checkidx3.h[cno]",'1/12/9','8/6/3')
64440   ! 
65000   ! r: DedNames.h setup
65020   ! if exists("[Q]\PRmstr\dednames.h[cno]")=0 then
65040   dim pr_dednames_fullname$(20)*20
65060   dim pr_dednames_abrevname$(20)*8
65080   dim pr_dednames_newcalcode(20)
65100   dim pr_dednames_newdedfed(20)
65120   dim pr_dednames_newdedcode(20)
65140   dim pr_dednames_dedfica(20)
65160   dim pr_dednames_dedst(20)
65180   dim pr_dednames_deduc(20)
65200   dim pr_dednames_gl$(20)*12
65220   open #h_dednames:=fngethandle: "Name=[Q]\PRmstr\dednames.h[cno],RecL=920,use",internal,outIn,relative 
65240   if lrec(h_dednames)=0 then 
65260     write #h_dednames,using 'form pos 1,20*c 20,20*c 8,120*n 1,20*c 12': mat pr_dednames_fullname$,mat pr_dednames_abrevname$,mat pr_dednames_newdedcode,mat pr_dednames_newcalcode,mat pr_dednames_newdedfed,mat pr_dednames_dedfica,mat pr_dednames_dedst,mat pr_dednames_deduc,mat pr_dednames_gl$
65280   end if 
65300   close #h_dednames: ioerr ignore
65320   ! end if
65340   ! /r
66100   PrGlindex: !
66110   open #h_tmp:=fngethandle: "Name=[Q]\PRmstr\GLMstr.h[cno],Version=0,KFName=[Q]\PRmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed ioerr Check4124OnPrGlindex
66120   close #h_tmp: 
66140   ! 
66160   if ~exists('[Q]\PRmstr\EmpStatus.dat') then 
66180     open #h_pr_emp_status:=fngethandle: "Name=[Q]\PRmstr\EmpStatus.dat,KFName=[Q]\PRmstr\Empstatus.idx,Use,RecL=32,KPs=1,KLn=2,Shr",internal,outIn,keyed 
66200     write #h_pr_emp_status,using 'form pos 1,N 2,C 25': 9,'Terminated'
66220   end if 
66900 fnend 
67000 Check4124OnPrGlindex: ! r:
67020  if err=4124 and (Check4124OnPrGlindexCount+=1)<=2 then
67040    fnindex_it('[Q]\PRmstr\GLMstr.h[cno]','[Q]\PRmstr\GLIndex.h[cno]','1 12')
67060    goto PrGlindex
67080  else
67100     fnStatus('Failure.')
67120     fnStatus('* Data File: PRmstr\GLMstr.h[cno]')
67140     fnStatus('* Index: PRmstr\GLIndex.h[cno]')
67160     fnStatus('* reindex completed however error 4124 persist.')
67180     fnStatusPause
67200  end if
67220  goto ERTN ! /r
68000 def fn_cfv_job_cost_payroll
68012   ! if ~exists('[Q]\INI\Payroll\Job Cost') then execute 'mkdir "[Q]\INI\Payroll\Job Cost"'
68014   fn_ini_move('JC')
68020   ! r: JCMSTR.h
68040   fn_file_setup_data("[Q]\PRmstr\JCMSTR.h[cno]",300,0)
68060   fn_file_setup_index("[Q]\PRmstr\JCINDX.h[cno]",'1','6')
68080   fn_file_setup_index("[Q]\PRmstr\JCINDX2.h[cno]",'7','25')
68100   ! /r
68120 fnend 
74000 def fn_cfv_general_ledger
74030   ! General Ledger Only
74032   fn_file_setup_data("[Q]\GLmstr\ACTrans.h[cno]",72,0)
74034   fn_file_setup_index("[Q]\GLmstr\AcTrIdx.h[cno]",'1/71/17/13','12/2/2/4')
74036   !
74042   fn_file_setup_data("[Q]\GLmstr\GLTrans.h[cno]",73,0)
74046   !
74040   library 'S:\Core\Library': fnfinstmt_v0_to_v1,fnglmstr_338_416
74060   library 'S:\Core\Library': fnglpayee_v0_to_v1 
74080   if exists("[Q]\GLmstr")=0 then execute "MkDir [Q]\GLmstr"
74100   ! if ~exists('[Q]\INI\General Ledger') then execute 'mkdir "[Q]\INI\General Ledger"'
74120   ! if ~exists('[Q]\INI\General Ledger\Accountants') then execute 'mkdir "[Q]\INI\General Ledger\Accountants"'
74140   fn_ini_move(env$('cursys'))
74142   fnIniToReg
74144   fn_reg_rename(env$('cursys'))
74160   !
74180   if ~exists('[Q]\GLmstr\PayeeType.dat') then
74200     open #hPayeeType:=fngethandle: "Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed
74220     write #hPayeeType,using 'form pos 1,n 2,C 25': 0,'Not Applicable'
74240     write #hPayeeType,using 'form pos 1,n 2,C 25': 7,'Non-Employee Compensation'
74260     close #hPayeeType:
74280   end if
74300   !
74320   ! BudgetInfo: ! Primary Non-Split Index
74340   name$="[Q]\GLmstr\BudgetInfo.h[cno]"
74360   kfname$="[Q]\GLmstr\BudIndx.h[cno]"
74380   myrln=28
74400   version_proper=0
74420   fn_make_data_file_exist(name$,myrln,version_proper)
74440   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
74460     fnindex_it(name$,kfname$,'1 14')
74480   end if 
74500   ! GL_GLMSTR1: ! Primary Non-Split Index
74520   name$="[Q]\GLmstr\GLmstr.h[cno]"
74540   kfname$="[Q]\GLmstr\GLIndex.h[cno]"
74560   myrln=416
74580   version_proper=0
74600   fn_make_data_file_exist(name$,myrln,version_proper)
74620   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
74640     fnindex_it(name$,kfname$,'1 12')
74660   end if 
74680   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
74700   fn_check_version(tmpversion,version_proper,tmpfile$)
74720   if tmprln<>myrln then 
74740     fnStatus('Record Length Error in File: '&tmpfile$)
74760     fnStatus('         RLn: '&str$(tmprln))
74780   end if 
74800   if tmprln=338 then let fnglmstr_338_416
74820   x=1 : if tmpkps(x)<>1 then 
74840     fnStatus('Key Position ('&str$(x)&') Error in '&kfname$)
74860     fnStatus('      KPs('&str$(x)&'): '&str$(tmpkps(x)))
74880   end if 
74900   x=1 : if tmpkln(x)<>12 then 
74920     fnStatus('Key Length ('&str$(x)&') Error in '&kfname$)
74940     fnStatus('      KLn('&str$(x)&'): '&str$(tmpkln(x)))
74960   end if 
74980   ! 
75000   ! GL_GLMSTR2: ! Secondary, Non-Split Index
75020   name$="[Q]\GLmstr\GLmstr.h[cno]"
75040   kfname$="[Q]\GLmstr\GLIndx2.h[cno]"
75060   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
75080     fnindex_it(name$,kfname$,'13 30')
75100   end if 
75120   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
75140   x=1 : if tmpkps(x)<>13 then 
75160     pr 'Key Position ('&str$(x)&') Error in '&kfname$
75180     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
75200   end if 
75220   x=1 : if tmpkln(x)<>30 then 
75240     pr 'Key Length ('&str$(x)&') Error in '&kfname$
75260     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
75280   end if 
75300   if tmpkln(x)=50 then 
75320     fnFree(kfname$)
75340     fnindex_it(name$,kfname$,'13 30')
75360   end if 
75380   ! 
75390   fn_file_setup_data("[Q]\GLmstr\AcGLFnSc.h[cno]",83,1)
75392   fn_file_setup_index("[Q]\GLmstr\FnScIndx.h[cno]",'1','5')
75400   ! r:  Six Files, with 1 primary index each
75410   !         acglfnsj, acglfnsi, acglfnsb, acglfnsc, acglfnsf, acglfnsg
75420   fn_file_setup_data("[Q]\GLmstr\acglfnsj.h[cno]",83,1)
75430   fn_file_setup_index("[Q]\GLmstr\Fnsjindx.h[cno]",'1','5')
75440   ! r: GLmstr\acglfnsi
75450   name$="[Q]\GLmstr\acglfnsi.h[cno]"
75460   kfname$="[Q]\GLmstr\fnsiindx.h[cno]"
75470   fn_file_setup_data(name$,83,1)
75480   fn_file_setup_index(kfname$,'1','5')
75490   ! myrln=83
75500   ! version_proper=1
75510   ! fn_make_data_file_exist(name$,myrln,version_proper)
75520   ! if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
75530   !   fnindex_it(name$,kfname$,'1 5')
75540   ! end if 
75550   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
75560   fn_check_version(tmpversion,version_proper,tmpfile$)
75570   if tmpversion=0 then let fnfinstmt_v0_to_v1
75580   if tmprln<>myrln then 
75590     pr 'Record Length Error in File: '&tmpfile$
75600     pr '         RLn: '&str$(tmprln)
75610   end if 
75620 ! If TMPRLN=81 OR TMPRLN=78 Then Let FNFINSTMT_v0_to_v1
75630   x=1 : if tmpkps(x)<>1 then 
75640     pr 'Key Position ('&str$(x)&') Error in '&kfname$
75650     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
75660   end if 
75670   x=1 : if tmpkln(x)<>5 then 
75680     pr 'Key Length ('&str$(x)&') Error in '&kfname$
75690     pr '      KLn('&str$(x)&'): '&str$(tmpkln(x))
75700   end if 
75710   !
75720   fn_file_setup_data("[Q]\GLmstr\acglfnsb.h[cno]",83,1)
75730   fn_file_setup_index("[Q]\GLmstr\Fnsbindx.h[cno]",'1','5')
75740   !
75750   fn_file_setup_data("[Q]\GLmstr\acglfnsb.h[cno]",83,1)
75760   fn_file_setup_index("[Q]\GLmstr\Fnsbindx.h[cno]",'1','5')
75770   !
75780   fn_file_setup_data("[Q]\GLmstr\acglfnsf.h[cno]",83,1)
75790   fn_file_setup_index("[Q]\GLmstr\Fnsfindx.h[cno]",'1','5')
75800   !
75810   fn_file_setup_data("[Q]\GLmstr\acglfnsg.h[cno]",83,1)
75820   fn_file_setup_index("[Q]\GLmstr\Fnsgindx.h[cno]",'1','5')
75830   !
75840   ! /r
75940   ! PAYEEGLBREAKDOWN: !
75960   name$="[Q]\GLmstr\payeeglbreakdown.h[cno]"
75980   kfname$="[Q]\GLmstr\Payeeglbkdidx.h[cno]"
76000   myrln=56
76020   version_proper=1
76040   fn_make_data_file_exist(name$,myrln,version_proper)
76060   L3510: ! 
76080   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
76100     fnindex_it(name$,kfname$,'1 8')
76120   end if 
76140   fn_check_version(tmpversion,version_proper,tmpfile$)
76160   if tmprln<>myrln then 
76180     pr 'Record Length Error in File: '&tmpfile$
76200     pr '         RLn: '&str$(tmprln)
76220   end if 
76240   x=1 : if tmpkps(x)<>1 then 
76260     pr 'Key Position ('&str$(x)&') Error in '&kfname$
76280     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
76300     pr 'fixing it'
76320     fnFree(kfname$)
76340     goto L3510
76360   end if 
76380   ! GLPAYMSTR: ! Primary, Non-Split Index  (Vendor or payee files in g/l)
76400   if exists("[Q]\GLmstr\gl1099.h[cno]")<>0 then let fnglpayee_v0_to_v1
76420   if exists("[Q]\GLmstr\gl1099.h[cno]")<>0 then let fnFree("[Q]\GLmstr\gl1099.h[cno]")
76440   name$="[Q]\GLmstr\PayMstr.h[cno]"
76460   kfname$="[Q]\GLmstr\Payidx1.h[cno]"
76480   myrln=276
76500   version_proper=1
76520   fn_make_data_file_exist(name$,myrln,version_proper)
76540   L3600: ! 
76560   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
76580     fnindex_it(name$,kfname$,'1 8') : fnglpayee_v0_to_v1
76600   end if 
76620   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
76640   fn_check_version(tmpversion,version_proper,tmpfile$)
76660   if tmprln<>myrln then 
76680     pr 'Record Length Error in File: '&tmpfile$
76700     pr '         RLn: '&str$(tmprln)
76720     fnglpayee_v0_to_v1
76740   end if 
76760   x=1 : if tmpkps(x)<>1 then 
76780     pr 'Key Position ('&str$(x)&') Error in '&kfname$
76800     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
76820     pr 'fixing it'
76840     fnFree(kfname$)
76860     goto L3600
76880   end if 
76900     ! 
76920     ! GLTR1099: ! Primary, Non-Split Index  (Vendor transactions)
76940   name$="[Q]\GLmstr\GlTr1099.h[cno]"
76960   kfname$="[Q]\GLmstr\gltridx1.h[cno]"
76980   myrln=64
77000   version_proper=1
77020   fn_make_data_file_exist(name$,myrln,version_proper)
77040   L3690: ! 
77060   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
77080     fnindex_it(name$,kfname$,'1 8')
77100   end if 
77120   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
77140   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
77160   fn_check_version(tmpversion,version_proper,tmpfile$)
77180   if tmprln<>myrln then 
77200     pr 'Record Length Error in File: '&tmpfile$
77220     pr '         RLn: '&str$(tmprln)
77240   end if 
77260   x=1 : if tmpkps(x)<>1 then 
77280     pr 'Key Position ('&str$(x)&') Error in '&kfname$
77300     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
77320     pr 'fixing it'
77340     fnFree(kfname$)
77360     goto L3690
77380   end if 
77400   ! GLBREC: ! Primary, Non-Split Index
77420   name$="[Q]\GLmstr\Glbrec.h[cno]"
77440   kfname$="[Q]\GLmstr\glrecidx.h[cno]"
77460   myrln=68
77480   version_proper=1
77500   fn_make_data_file_exist(name$,myrln,version_proper)
77540   if lwrc$(env$('force_reindex'))='yes' or exists(kfname$)=0 then 
77550   GlBrecIndex: ! 
77560     fnindex_it(name$,kfname$,'1 24')
77570   end if 
77580   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed error GlBrecOpenErr
77590   fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
77600   fn_check_version(tmpversion,version_proper,tmpfile$)
77610   if tmprln<>myrln then 
77620     pr 'Record Length Error in File: '&tmpfile$
77630     pr '         RLn: '&str$(tmprln)
77640   end if 
77650   x=1 : if tmpkps(x)<>1 then 
77660     fnStatus('Key Position ('&str$(x)&') Error in '&kfname$)
77670     fnStatus('      KPs('&str$(x)&'): '&str$(tmpkps(x)))
77680     fnStatus('fixing it')
77690     goto GlBrecIndex
77700   end if 
77710   goto GlBrecFinis
77720   GlBrecOpenErr: !
77730   if err=0632 then
77740      goto GlBrecIndex
77750   else
77760      goto ERTN
77770   end if
77780   GlBrecFinis: !
77790   ! 
77880   ! SCHEDULE: ! Primary, Non-Split Index  (General ledger schedules)
77900   dim sn$*78,ft$*78,gl$(80)*12
77920   name$="[Q]\GLmstr\acglschs.h[cno]"
77940   kfname$="[Q]\GLmstr\schindex.h[cno]"
77960   myrln=162
77980   version_proper=1
78000   fn_make_data_file_exist(name$,myrln,version_proper)
78020   L3870: fnindex_it(name$,kfname$,'1 3')
78040   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed : fn_get_tmp(tmp,mat tmpkps,mat tmpkln,tmpversion,tmprln,tmpfile$)
78060   fn_check_version(tmpversion,version_proper,tmpfile$)
78080   if tmprln<>myrln then 
78100     pr 'Record Length Error in File: '&tmpfile$
78120     pr '         RLn: '&str$(tmprln)
78140   else 
78160     goto L4050
78180   end if 
78200   open #tmp:=fngethandle: 'Name='&name$&',KFName='&kfname$&',Shr',internal,outIn,keyed 
78210   L3920: read #tmp, using "Form POS 1,N 2,2*C 78,3*N 1,80*C 12": sn,sn$,ft$,dp,rs,cm,mat gl$ eof EO_TMP conv L9000
78220   if sn=0 then goto L3920
78230   rewrite #tmp, using "Form POS 1,N 3,2*C 78,3*N 1": sn,sn$,ft$,dp,rs,cm
78240   if exists("[Q]\GLmstr\schedule"&str$(sn)&".h[cno]")=0 then open #schedule:=fngethandle: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],KFName=[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]"&',replace,RecL=12,kps=1,kln=12,Shr',internal,outIn,keyed: version(schedule,1): close #schedule: 
78250   if exists("[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]")=0 then 
78260     fnindex_it("[Q]\GLmstr\schedule"&str$(sn)&".h[cno]","[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]","1 12")
78270   end if 
78280   open #schedule:=fngethandle: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],KFName=[Q]\GLmstr\schedule_idx"&str$(sn)&".h[cno]"&',use,RecL=12,kps=1,kln=12,Shr',internal,outIn,keyed: version(schedule,1) ! open to update gl breakdowns
78290   for j=1 to 80
78300     if val(gl$(j))=0 then goto L4010
78310     write #schedule,using "form pos 1,c 12": gl$(j)
78320   L4010: next j
78330   close #schedule: 
78340   goto L3920
78350   EO_TMP: ! 
78360   close #tmp:
78370   x=1 : if tmpkps(x)<>1 then 
78380     pr 'Key Position ('&str$(x)&') Error in '&kfname$
78390     pr '      KPs('&str$(x)&'): '&str$(tmpkps(x))
78400     pr 'fixing it'
78410     fnFree(kfname$)
78420     goto L3870
78430   end if 
78440   L4050: ! 
78450   ! 
78460   ! r: these functions hav their own conversions and only need to be called to launch
78470   library 'S:\Core\Library': fnfscode,fnpedat$,fnpriorcd,fnpgnum,fnrx,fnstyp,fnps
78480   fnfscode
78490   fnpedat$
78500   fnpriorcd
78510   fnpgnum
78520   fnrx
78530   fnstyp
78540   fnps
78550   ! /r
78560   ! 
78570   if ~exists('[Q]\GLmstr\Period.h[cno]') then
78580     open #hGlPeriod:=fngethandle: "Name=[Q]\GLmstr\Period.h[cno],Version=1,KFName=[Q]\GLmstr\Period-Idx.h[cno],Use,RecL=35,KPs=1,KLn=2,Shr",internal,outIn,keyed 
78590     for periodRecord=1 to 12
78600       write #hGlPeriod,using 'form pos 1,N 2,C 30': periodRecord,date$(days(cnvrt$('pic(##)',periodRecord)&'0117','mmddyy'),'month')
78610     nex periodRecord
78620     write #hGlPeriod,using 'form pos 1,N 2,C 30': 13,'End of Year Adjustments'
78630     close #hGlPeriod:
78640   end if
78650   ! 
78660   if exists("[Q]\GLmstr\GLWK1"&wsid$&".h[cno]") and ~exists("[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]") then
78670     if fncopy("[Q]\GLmstr\GLWK1"&wsid$&".h[cno]","[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".h[cno]") then
78680       fnFree("[Q]\GLmstr\GLWK1"&wsid$&".h[cno]")
78690     end if
78700   end if
78710   !
78720   if exists("[Q]\GLmstr\GLWK1"&wsid$&".dat") and ~exists("[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".dat") then
78730     if fncopy("[Q]\GLmstr\GLWK1"&wsid$&".dat","[Q]\GLmstr\GL_Work_"&env$('acsUserId')&".dat") then
78740       fnFree('[Q]\GLmstr\GLWK1'&wsid$&'.dat')
78750     end if
78760   end if
78770   ! 
78780   ! 
78998 fnend 
79000 L9000: ! r: skip bad schedule records
79020   reread #tmp, using "Form POS 1,c 2": a$ eof EO_TMP ioerr ignore
79040 goto L3920 ! /r
80000 def fn_ini_move(cursys$*2)
80010   dim imProgramOld$(0)*256
80020   dim imProgramNew$(0)*256
80030   imProgramCount=0
80040   mat imProgramOld$(imProgramCount)
80050   mat imProgramNew$(imProgramCount)
80060   if cursys$='PR' then
80070     fn_programMoveAdd('acsPR\newprfm','Payroll\Employee')
80080     fn_programMoveAdd('acsPR\newprcalk','Payroll\Calc')
80090     fn_programMoveAdd('acsPR\newprRevCal','Payroll\Reverse Calculation')
80100     fn_programMoveAdd('acsPR\newprchangedate','Payroll\Change Payroll Dates')
80110     fn_programMoveAdd('acsPR\newprinput','Payroll\Enter Time Sheets')
80120     fn_programMoveAdd('acsPR\newprCkPrt','Payroll\Print Payroll Checks')
80130     if fnclient_has('P2') then 
80140       fn_programMoveAdd('acsPR\Category','Payroll\Job Cost\Category')
80150     end if
80160   else if cursys$='UB' then
80170     fn_programMoveAdd('acsUB\ubfm','Utility Billing\Customer')
80180     fn_programMoveAdd('acsUB\ubipcoll','Utility Billing\Collections')
80190     fn_programMoveAdd('acsUB\ubPDTnOf','Utility Billing\Past Due Turn Off List')
80200     fn_programMoveAdd('acsUB\ubUsage','Utility Billing\Usage Report')
80210     fn_programMoveAdd('acsUB\ubIpChg','Utility Billing\Enter Readings and Charges')
80220     fn_programMoveAdd('acsUB\workOrderAdd','Utility Billing\Work Order Add')
80230     fn_programMoveAdd('acsUB\workOrderList','Utility Billing\Work Order List')
80240     fn_programMoveAdd('acsUB\ubBilJrn','Utility Billing\Billing Journal')
80250     fn_programMoveAdd('acsUB\ubRate','Utility Billing\Rates')
80260     fn_programMoveAdd('acsUB\BkDraft','Utility Billing\Create Bank Draft File')
80270     fn_programMoveAdd('acsUB\ubpencal','Utility Billing\Penalty Calculation')
80280     fn_programMoveAdd('acsUB\ubUnbill','Utility Billing\Unbilled Customer Listing')
80290     fn_programMoveAdd('acsUB\FlexTran','Utility Billing\Transactions')
80300     fn_programMoveAdd('acsUB\ubNoUsage','Utility Billing\Zero Usage Report')
80310     fn_programMoveAdd('acsUB\Per1000','Utility Billing\Per 1000 Usage')
80320     fn_programMoveAdd('acsUB\PrintBill','Utility Billing\Print Bills')
80330     fn_programMoveAdd('acsUB\analyze','Utility Billing\Rate Analysis')
80340     fn_programMoveAdd('acsUB\label','Utility Billing\Labels')
80350   else if cursys$='GL' then
80360     fn_programMoveAdd('acsGL\glPrt109','General Ledger\Print 1099 Forms')
80370     fn_programMoveAdd('acsGL\acGLClos','General Ledger\Close Books at Year End')
80380     fn_programMoveAdd('acsGL\CloseMonth','General Ledger\Close Month')
80390     fn_programMoveAdd('acsGL\AcGLAcTB','General Ledger\Print Accumulated Trial Balance')
80400     fn_programMoveAdd('acsGL\GLInput','General Ledger\Enter Transactions')
80410     fn_programMoveAdd('acsGL\AcGlInc4','General Ledger\Four Column Budget Income Statement')
80420     fn_programMoveAdd('acsGL\ACGLTB','General Ledger\Trial Balance')
80430     fn_programMoveAdd('acsGL\AcGLBalC','General Ledger\Comparative Balance Sheet')
80440     fn_programMoveAdd('acsGL\AcGLBalY','General Ledger\Period Comparison Balance Sheet')
80450     if fnclient_has('G2') then ! Accountant's General Ledger Add-On
80460       fn_programMoveAdd('acsGL\AcPrReg','General Ledger\Accountants\Print Payroll Registers')
80470       fn_programMoveAdd('acsGL\Employee','General Ledger\Accountants\Employee')
80480     end if
80490   else if cursys$='CL' then
80500     fn_programMoveAdd('acsCL\Transaction','Checkbook\Transaction')
80510     fn_programMoveAdd('acsCL\cl1099','Checkbook\Print 1099 Forms')
80520     fn_programMoveAdd('acsCL\payee','Checkbook\Payee')
80530     fn_programMoveAdd('acsCL\UnpaidInvoice','Checkbook\Unpaid Invoice')
80540   end if
80550   dim favData$(0)*128,favDataN(0)
80560   hFavProgram=fn_open('CO Favorites', mat favData$, mat favDataN, mat form$, 0, 2)
80570   for imItem=1 to imProgramCount
80580     dim imIniFrom$*256
80590     dim imIniTo$*256
80600     dim imbrFrom$*256
80610     dim imbrTo$*256
80620     imBrFrom$='S:\'&imProgramOld$(imItem)&'.br'
80630     imBrTo$  ='S:\'&imProgramNew$(imItem)&'.br'
80640     imIniFrom$=fn_programIniFileName$(imProgramOld$(imItem), 1)
80650     imIniTo$  =fn_programIniFileName$(imProgramNew$(imItem), 1)
80660        ! if pos(lwrc$(imBrFrom$),'acglbalc')>0 then pause
80670 ! if pos(lwrc$(imIniFrom$),'ubipcoll')>0 and env$('acsdeveloper')<>'' then pause
80680     if ~(imIniFrom$='S:\Core\Default\Program.ini' or imIniTo$='S:\Core\Default\Program.ini') then
80690       if exists(imIniFrom$) and ~exists(imIniTo$) then 
80700         if fnCopy(imIniFrom$,imIniTo$)>0 then
80710           fnFree(imIniFrom$)
80720         end if
80730       end if
80740     end if
80750     fnkey_change(hFavProgram,'form pos '&str$(kps(hFavProgram))&',C '&str$(kln(hFavProgram)),imBrFrom$,imBrTo$)
80760   nex imItem
80770   hFavProgram=fnCloseFile(hFavProgram,'CO Favorites')
80780 fnend
80790 def fn_programMoveAdd(programNameOld$*256,programNameNew$*256)
80800   fnAddOneC(mat imProgramOld$,programNameOld$)
80810   imProgramCount=fnAddOneC(mat imProgramNew$,programNameNew$)
80820 fnend
81000 def fn_reg_rename(cursys$*2)
81010   if cursys$='PR' then
81020     ! nuffin yet
81030     if fnclient_has('P2') then 
81040       ! nuffin yet
81050     end if
81060   else if cursys$='UB' then
81070     ! fn_rrOne('acsUB\ubDepChg','Utility Billing\Deposit Change List')  <-- just wrong - only with program caption name changes, not file name changes.
81080   else if cursys$='GL' then
81090     ! nuffin yet
81100     if fnclient_has('G2') then ! Accountant's General Ledger Add-On
81110       ! nuffin yet
81120     end if
81130   else if cursys$='CL' then
81140   end if
81150 fnend
82000 def fn_rrOne(from$*256,to$*256)
82010   if ~rr1Setup then 
82020     rr1Setup=1
82030     if env$('ACSDeveloper')<>'' then
82040       library 'S:\Core\Library': fnsreg_rename
82050     else
82060       library 'S:\Core\Library': fnreg_rename
82070     end if
82080     dim property$(0)*128
82090     mat property$(0)
82100     fnAddOneC(mat property$,'Orientation' )
82110     fnAddOneC(mat property$,'Height'      )
82120     fnAddOneC(mat property$,'Width'       )
82130     fnAddOneC(mat property$,'Lines'       )
82140     fnAddOneC(mat property$,'FontSize'    )
82150     fnAddOneC(mat property$,'TopMargin'   )
82160     fnAddOneC(mat property$,'BottomMargin')
82170     fnAddOneC(mat property$,'LeftMargin'  )
82180     fnAddOneC(mat property$,'RightMargin' )
82190   end if
82200   for propertyItem=1 to udim(mat property$)
82210     if env$('ACSDeveloper')<>'' then
82220       fnsreg_rename(env$('cursys')&'.'&trim$(from$)&'.Print.'&property$(propertyItem),env$('cursys')&'.'&trim$(to$)&'.Print.'&property$(propertyItem))
82230     else
82240       fnreg_rename(env$('cursys')&'.'&trim$(from$)&'.Print.'&property$(propertyItem),env$('cursys')&'.'&trim$(to$)&'.Print.'&property$(propertyItem))
82250     
82260     end if
82270   nex propertyItem
82280 fnend
89000 ! <updateable region: fn_open (supressprompt:=2)>  
89020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
89040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
89060   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
89080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
89100     mat loadedsubs$(udim(loadedsubs$)+1) 
89120     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
89140     for index=1 to udim(mat _fileiosubs$) 
89160       execute (_fileiosubs$(index)) 
89180     next index
89200   end if
89220 fnend
89240 ! </updateable region: fnopen>
90000 def fn_programIniFileName$*256(pif_program$*256; doNotCreate)
90020   dim pif_return$*256
90040   pif_return$=''
90060   pif_program$=trim$(pif_program$)
90080     posDotBr=pos(pif_program$,'.br')
90100     if posDotBr>0 then pif_program$(posDotBr:posDotBr+2)=''
90120   pif_return$='[Q]\INI\'&pif_program$&'.ini'
90160   fn_programIniFileName$=pif_return$
90180 fnend 