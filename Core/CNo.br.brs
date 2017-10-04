10000 ! Replace S:\Core\CNo.br
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12080     library 'S:\Core\Library': fngetdir2,fnerror
12100     library 'S:\Core\Library': fngethandle
12120     library 'S:\Core\Library': fnCnoLegacyNtoCReg
12140     library 'S:\Core\Library': fnreg_read,fnreg_write
12160     library 'S:\Core\Library': fncreg_read,fncreg_write
12960   end if
12980 fnend
14000 ! <Updateable Region: ERTN>
14020 ERTN: let fnerror(program$,err,line,act$,"xit")
14040   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
14060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
14080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
14100 ERTN_EXEC_ACT: execute act$ : goto ERTN
14120 ! /region
20000 def library fncno(&cno; &cnam$)
20020   if ~setup then let fn_setup
20060   cursys$=env$('cursys')
22000   ! r: Read CNo (normal method - tied to session and cursys$
22020   fnreg_read(session$&'.'&cursys$&'.cno',cno$)
22040   cno=val(cno$)
22060   ! /r
25010   ! r: read cno tied to WSID (v 5 but before feb 2015)
25020   if ~cno then 
25040     fnreg_read(wsid$&'.'&cursys$&'.cno',cno$)
25060     cno=val(cno$)
25080     fn_putcno(cno)
25100   end if 
25900   ! /r
30000   ! r: legacy cno fetch
30020   if ~cno then ! it's not yet converted to be used in the registry (5.0)
30022     cno=1
30160   end if 
30900   ! /r
32000   ! r: read cnam
32020   dim cnam_read$*40
32040   cnam_read$=''
32060   open #tf1:=fngethandle: "Name="&env$('Q')&"\"&cursys$&"mstr\Company.h"&str$(cno)&",Shr",internal,input ioerr CNAM_XIT
32080   read #tf1,using "Form pos 1,C 40": cnam_read$ ioerr ignore
32100   close #tf1: 
34000   CNAM_XIT: ! 
34020   ! /r
34040   cnam$=cnam_read$ soflow ignore
34080   setenv('cnam',rtrm$(cnam_read$))
34100   if env$('cno')<>str$(cno) then
34110     setenv('cno',str$(cno))
34120     execute 'config substitute [cno] '&str$(cno)
34140   end if
34160   fncno=cno
34180 fnend 
36000 def library fnputcno(cno)
36020   if ~setup then let fn_setup
36040   fnputcno=fn_putcno(cno)
36060 fnend
36080 def fn_putcno(cno)
36100   fnreg_write(session$&'.'&env$('CurSys')&'.cno',str$(cno))
36120   setenv('cno',str$(cno))
36140   execute 'config substitute [cno] '&str$(cno)
36160 fnend 
38000 def library fnget_company_number_list(mat cno_list; sysid$*2)
38020   if ~setup then let fn_setup
38030   if sysid$='' then sysid$=env$('cursys')
38040   fngetdir2(env$('Q')&'\'&sysid$&"mstr",mat filename$,'/od /ta',"Company.*")
38060   company_count=filename_item=0
38080   mat cno_list(99999)
38100   for filename_item=1 to udim(mat filename$)
38120     let tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
38140     if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
38160       company_count+=1
38180       cno_list(company_count)=tmp_cno
38200     end if 
38210   ACNO_CONV: ! 
38220   next filename_item
38240   mat cno_list(company_count)
38260   fnget_company_number_list=company_count
38280 fnend 
40000 def library fnCnoLegacyNtoCReg(legacyFilename$*256,legacyForm$*64,registryKey$*128; valuePassedIn)
40020   if ~setup then let fn_setup
40040   fnCnoLegacyNtoCReg=fn_CnoLegacyNtoCReg(legacyFilename$,legacyForm$,registryKey$, valuePassedIn)
40060 fnend
42000 def fn_CnoLegacyNtoCReg(legacyFilename$*256,legacyForm$*64,registryKey$*128; valuePassedIn)
42040   ! Get_or_Put=1 then GET 
42060   ! Get_or_Put=2 then PUT
42080   if valuePassedIn>0 then let get_or_put=2 else let get_or_put=1
42100   if get_or_put=1 then 
42120     fncreg_read(registryKey$,fscode$) : valuePassedIn=val(fscode$)
42140     if valuePassedIn=0 then
42160       open #tmp:=fngethandle: "Name="&legacyFilename$,internal,outin,relative ioerr LegacyOpenFail
42180       read #tmp,using legacyForm$,rec=1: valuePassedIn norec ignore
42200       close #tmp: ioerr ignore
42220       fncreg_write(registryKey$,str$(valuePassedIn))
42240       LegacyOpenFail: !
42260     end if
42280   else if get_or_put=2 then 
42300     fncreg_write(registryKey$,str$(valuePassedIn))
42320   end if
42340   fn_CnoLegacyNtoCReg=valuePassedIn
42360 fnend 
44000 def library fnpedat$*20(;pedat$*20)
44020   if ~setup then let fn_setup
44040   ! Get_or_Put=1 then GET 
44060   ! Get_or_Put=2 then PUT
44080   if trim$(pedat$)="" then let get_or_put=1 else let get_or_put=2
44100   if get_or_put=1 then 
44120     fncreg_read('Pay Period Ending Date',pedat$)
44140     if pedat$='' then
44160       dim pedatLegacyFile$*256
44180       if exists(env$('temp')&"\pedat-"&session$&".h"&env$('cno')) then
44200         pedatLegacyFile$=env$('temp')&"\pedat-"&session$&".h"&env$('cno')
44220       else if exists(env$('temp')&"\pedat$-"&session$&".h"&env$('cno')) then
44240         pedatLegacyFile$=env$('temp')&"\pedat$-"&session$&".h"&env$('cno')
44260       else
44280         goto xLegacyOpenFail
44300       end if
44320       open #tmp:=fngethandle: "Name="&pedatLegacyFile$,internal,outin,relative ioerr xLegacyOpenFail
44340       read #tmp,using "Form POS 1,C 20",rec=1: pedat$ norec ignore
44360       close #tmp: ioerr ignore
44380       fncreg_write('Pay Period Ending Date',pedat$)
44400       xLegacyOpenFail: !
44420     end if
44440   else if get_or_put=2 then 
44460     fncreg_write('Pay Period Ending Date',pedat$)
44480   end if
44500   fnpedat$=pedat$
44520 fnend 
46000 def library fnfscode(;fscode)
46020   if ~setup then let fn_setup
46040   fnfscode=fnCnoLegacyNtoCReg(env$('temp')&"\fscode-"&session$&".dat","Form POS 1,N 9",'Financial Statement Code', fscode)
46060 fnend 
46080 def library fnpriorcd(;PriorCD)
46100   if ~setup then let fn_setup
46120   fnpriorcd=fnCnoLegacyNtoCReg(env$('temp')&"\priorcd-"&session$&".dat","Form POS 1,N 9",'PriorCD', PriorCD)
46140 fnend
46160 def library fnpgnum(;pgnum)
46180   if ~setup then let fn_setup
46200   fnpgnum=fnCnoLegacyNtoCReg(env$('temp')&"\PgNum-"&session$&".dat","Form POS 1,N 9",'PgNum', pgnum)
46220 fnend
46240 def library fnrx(;rx)
46260   if ~setup then let fn_setup
46280   fnrx=fnCnoLegacyNtoCReg(env$('temp')&"\rx-"&session$&".dat","Form POS 1,N 9",'rx', rx)
46300 fnend
46320 def library fnstyp(;STyp)
46340   if ~setup then let fn_setup
46360   fnstyp=fnCnoLegacyNtoCReg(env$('temp')&"\STyp-"&session$&".dat","Form POS 1,N 9",'STyp', STyp)
46380 fnend
46400 def library fnps(;ps)
46420   if ~setup then let fn_setup
46440   fnps=fnCnoLegacyNtoCReg(env$('temp')&"\ps-"&session$&".dat","Form POS 1,N 9",'ps', ps)
46460 fnend
48000 def library fnUseDeptNo
48010   if ~setup then let fn_setup
48020   if ~useDeptNosetup then ! r:
48040     useDeptNosetup=1
48080     if env$('cursys')="GL" then ! read directly from gl if in gl system
48100       open #company:=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative 
48120       read #company ,using "Form POS 150, n 1",rec=1: gld1 norec ignore
48140       close #company: 
48160     else 
48180       pr 'needs to read use department number setting some other way because cursys is not GL' : pause
48200       ! open #tmp:=fngethandle: "Name="&env$('temp')&"\gld1-"&session$&".dat,Use,RecL=9",internal,outin,relative 
48220       ! read #tmp ,using "Form POS 150, n 1",rec=1: gld1 norec ignore
48240       ! close #tmp: 
48260     end if
48280   end if ! /r
48300   fnUseDeptNo=gld1
48320 fnend 
51000 def library fndat(&dat$;get_or_put)
51010   if ~setup then let fn_setup
51040   ! Get_or_Put=0 then READ Dat$ (default to Read)
51060   ! Get_or_Put=1 then READ Dat$
51080   ! Get_or_Put=2 then REWRITE Dat$
51100   if get_or_put=0 or get_or_put=1 then 
51120     fnreg_read('Report Heading Date',dat$)
51140     let dat$=trim$(dat$)
51160     if dat$="" then 
51180       let dat$=date$("Month DD, CCYY")
51200       fnreg_write('Report Heading Date',dat$)
51220     end if 
51240   else if get_or_put=2 then 
51260     fnreg_write('Report Heading Date',dat$)
51280   end if 
51300 fnend 
54000 def library fnprg(&curprg$; g_p)
54020   if g_p=2 then ! Put
54040     !     r: remove leading  S:\ 
54060     dim curprg_tmp$*1024
54080     curprg_tmp$=curprg$
54100     if uprc$(curprg_tmp$(1:3))='S:\' then ! or uprc$(curprg_tmp$(1:3))='S:\' then 
54120       curprg_tmp$(1:3)=''
54140     else if uprc$(curprg_tmp$(1:2))='S:' then 
54160       curprg_tmp$(1:2)=''
54180     end if 
54200     !     /r
54220     setenv('Core_Program_Current',curprg_tmp$)
54240   else ! Get
54260     curprg$=env$('Core_Program_Current')
54280   end if 
54300 fnend 
56000 def library fnSystemName$*40(; as2n_abbr$*2)
56020   if ~setup then let fn_setup
56040   fnSystemName$=fn_system_abbr_2_name$( as2n_abbr$)
56060 fnend
56080 def fn_system_abbr_2_name$*40(; as2n_abbr$*2)
56100   dim as2n_return$*40
56120   if as2n_abbr$='' then as2n_abbr$=env$('CurSys')
56140   as2n_abbr$=lwrc$(as2n_abbr$)
56160   !   if as2n_abbr$='aa' then
56180   !     as2n_return$='ACS Programmer'
56200   if as2n_abbr$='ar' then 
56220     as2n_return$='Accounts Receivable'
56240   else if as2n_abbr$='bl' then 
56260     as2n_return$='Business License'
56280   else if as2n_abbr$='cl' then 
56300     as2n_return$='Checkbook'
56320   else if as2n_abbr$='co' then 
56340     as2n_return$='ACS Core'
56360   else if as2n_abbr$='cr' then 
56380     as2n_return$='Cash Register'
56400   else if as2n_abbr$='ea' then 
56420     as2n_return$='Home Energy Assistance'
56440   else if as2n_abbr$='fa' then 
56460     as2n_return$='Fixed Asset'
56480   else if as2n_abbr$='gl' or as2n_abbr$='g1' then 
56500     as2n_return$='General Ledger'
56520   else if as2n_abbr$='g2' then 
56540     as2n_return$='General Ledger - Accountants Add-On'
56560   else if as2n_abbr$='hh' then 
56580     as2n_return$='LapTop Meter Reading'
56600   else if as2n_abbr$='mc' then 
56620     as2n_return$='Municipal Court'
56640   else if as2n_abbr$='po' then 
56660     as2n_return$='Purchase Order'
56680   !   else if as2n_abbr$='p1' then
56700   !     as2n_return$='Payroll (Legacy)'
56720   else if as2n_abbr$='p2' then 
56740     as2n_return$='Payroll - Job Cost Add-On'
56760   else if as2n_abbr$='pr' or as2n_abbr$='p4' then 
56780     as2n_return$='Payroll'
56800   else if as2n_abbr$='su' then 
56820     as2n_return$='Support Tracking'
56840   else if as2n_abbr$='ub' then 
56860     as2n_return$='Utility Billing'
56880   else if as2n_abbr$='u4' then 
56900     as2n_return$='Utility Billing - Hand Held Add-On'
56920   else if as2n_abbr$='tm' then 
56940     as2n_return$='Time Management'
56960   else if as2n_abbr$='oe' then 
56980     as2n_return$='BR Order Entry'
57000   end if 
57020   fn_system_abbr_2_name$=as2n_return$
57040 fnend 
58000 def library fncursys$(; cursys_set$*2)
58020   if ~setup then let fn_setup
58040   if cursys_set$<>'' then 
58060     cursys_cache$=uprc$(cursys_set$)
58080     fnreg_write(session$&'.CurSys',cursys_cache$)
58100   else 
58120     cursys_cache$=uprc$(env$('CurSys'))
58140   end if 
58160   ! 
58180   if cursys_cache$="" then 
58200     fnreg_read(session$&'.CurSys',cursys_cache$)
58220     if cursys_cache$="" then 
58240       fngetdir2('S:\',mat system_abbr_list$, '/ON','??.mnu')
58260       if udim(system_abbr_list$)=>1 then 
58280         cursys_cache$=trim$(system_abbr_list$(1)(1:len(system_abbr_list$(1))-4))
58300       end if 
58320       if cursys_cache$="" then 
58340         cursys_cache$="CO"
58360       end if 
58380     end if 
58400   end if 
58420   ! 
58440   if uprc$(cursys_cache$)="P1" then cursys_cache$="PR" ! Payroll
58460   if uprc$(cursys_cache$)="P2" then cursys_cache$="PR" ! Job Cost Payroll
58480   if uprc$(cursys_cache$)="P4" then cursys_cache$="PR" ! version 4 Payroll
58500   if uprc$(cursys_cache$)="G1" then cursys_cache$="GL" ! General Ledger
58520   if uprc$(cursys_cache$)="G2" then cursys_cache$="GL" ! Accountant's GL
58540   if uprc$(cursys_cache$)="G3" then cursys_cache$="GL" ! Budget Management
58560   if env$('CurSys')<>cursys_cache$ then
58580     setenv('CurSys',cursys_cache$)
58600     setenv('CurSystem',fn_system_abbr_2_name$(cursys_cache$))
58620     execute 'config substitute [CurSys] '&cursys_cache$
58640   end if
58660   fncursys$=cursys_cache$
58680 fnend 