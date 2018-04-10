10000 ! S:\Core\Programs\Update
10100   force_update=1
10200 ! r: dims and constants
10300   if ~setup then let fn_setup
10400 ! dim message$(1)*256
10500   dim batch_name$*256
10600   dim script_name$*256
10700   dim return_name$*256
10800   dim line$*256
10900   batch_name$=env$('temp')&'\update'&session$&'.cmd'
11000   script_name$=env$('temp')&'\ftp_script'&session$&'.txt'
11100   return_name$=env$('temp')&'\return'&session$&'.txt'
11110   grace_days=45
11120   fnclient_support(mat system_id$,mat system_support_end_date,mat on_support,grace_days)
11200 ! /r
12000   fntop(program$,"ACS Update")
20000 ! r: main loop
20002   fn_status('launching update')
20028   if ~fn_simple_connectivity_test then
20040 !   fn_status('No newer update is available.')
20060     fn_status_pause
20100   else
20110     fn_drive_sys_must_exist
20120     fn_update_license
20140     fn_update
20160   end if
20180 XIT: fnxit
20200 ! /r
25000   def fn_setup
25020     setup=1
25040     library 'S:\Core\Library': fnreg_read,fnreg_write,fnmsgbox,fntop,fnxit,fngethandle,fnclient_has_on_support_list,fnSystemName$,fnclient_support,fnerror
25042     library 'S:\Core\Library': fnFree
25050     on error goto ERTN
25060   fnend
26000 IGNORE: continue
26020 ! <updateable region: ertn>
26040 ERTN: fnerror(program$,err,line,act$,"xit")
26060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
26080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
26100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
26120 ERTN_EXEC_ACT: execute act$ : goto ERTN
26140 ! </updateable region: ertn>
30000 ! def fn_acs_update_date$*40(;setit$*40)
30020 !   dim aud_return$*40
30040 !   if setit$='(default)' then setit$=date$('ccyy/mm/dd')&' - '&time$
30060 !   if setit$<>'' then
30080 !     aud_return$=setit$
30100 !     fnreg_write('ACS.Update.Date',setit$)
30120 !   else
30140 !     fnreg_read('ACS.Update.Date',aud_return$)
30160 !   end if
30180 !   fn_acs_update_date$=aud_return$
30200 ! fnend
32000   def fn_simple_connectivity_test
32020     dim conectivity_test_result$*40
32040     dim ua_acs_datetime$*40
32060 !
32080     conectivity_test_result$=fn_conectivity_test$
32100     if env$('ACSDeveloper')<>'' then
32120       fn_status('ACS Developers ('&env$('ACSDeveloper')&') should not update from the web.  It would overwrite all their local programs.')
32140       ua_return=0
32160     else if conectivity_test_result$='' then
32180       fn_status('Connectivity test failed.')
32200       ua_return=0
32220     else
32240       ua_return=1
32260     end if
32280 !
32300 !
32320 !   ua_acs_datetime$=fn_acs_update_date$
32340     fn_simple_connectivity_test=ua_return
32360   fnend
40000   def fn_conectivity_test$*40
40010     fn_status("testing connectivity...")
40020     wud_success=0
40040     dim wud_return$*40
40060     wud_return$=''
40080     open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output
40100     pr #h_script: 'user acs5update'
40120     pr #h_script: 'ACSKen!1'
40140     pr #h_script: 'Dir ACS-5-Update-CO.exe'
40160     pr #h_script: 'quit'
40180     close #h_script:
40190 !
40200     open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output
40220     pr #h_batch: 'prompt $p$g'
40260     pr #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
40400     close #h_batch:
40420     fn_execute('-m',os_filename$(batch_name$))
40440     open #h_return:=fngethandle: 'Name='&return_name$,display,input
40460     do
40480       linput #h_return: line$ eof WUD_RETURN_EOF
40500       if line$(3:3)='-' then
40540         wud_date$=date$(days(line$(1:8),'mm-dd-yy'),'ccyy/mm/dd')
40560         wud_time$=line$(11:15)
40580         wud_ampm$=line$(16:17)
40600         if wud_ampm$='PM' then
40620           wud_time$(1:2)=str$(val(wud_time$(1:2))+12)
40640         end if
40660         wud_return$=wud_date$&' - '&wud_time$&':00'
40700       end if
40720     loop
40740 WUD_RETURN_EOF: !
40760     close #h_return:
40780     fnFree(return_name$)
40800     fnFree(script_name$)
40820     fnFree(batch_name$)
40900     fn_conectivity_test$=wud_return$
40920   fnend
41000   def fn_update_license
41020     fn_status("updating license information...")
41040     ul_success=0
41060     dim ul_return$*40
41080     ul_return$=''
41100     open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output
41120     pr #h_script: 'user acs5update'
41140     pr #h_script: 'ACSKen!1'
41160     pr #h_script: 'LCD '&env$('Temp')
41180     pr #h_script: 'get ACS_5_Update_Support_Cache.exe'
41200     pr #h_script: 'quit'
41220     close #h_script:
41240 !
41260     open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output
41280     pr #h_batch: 'prompt $p$g'
41300     pr #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
41320     close #h_batch:
41340     fn_execute('-m',os_filename$(batch_name$))
41360     open #h_return:=fngethandle: 'Name='&return_name$,display,input
41380     do
41400       linput #h_return: line$ eof ul_RETURN_EOF
41420       if line$(3:3)='-' then
41440         ul_date$=date$(days(line$(1:8),'mm-dd-yy'),'ccyy/mm/dd')
41460         ul_time$=line$(11:15)
41480         ul_ampm$=line$(16:17)
41500         if ul_ampm$='PM' then
41520           ul_time$(1:2)=str$(val(ul_time$(1:2))+12)
41540         end if
41560         ul_return$=ul_date$&' - '&ul_time$&':00'
41580       end if
41600     loop
41620 ul_RETURN_EOF: !
41640     close #h_return:
41660     fnFree(return_name$)
41680     fnFree(script_name$)
41700     fnFree(batch_name$)
41720     fn_execute('-c',env$('temp')&'\acs_5_Update_Support_Cache.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS /NOCANCEL /SILENT')
41740     fn_update_license=1
41760   fnend
42000 def fn_update
42020   if env$('acsClient')='Ed Horton' then
42040     mat client_has$(5)
42060     client_has$(1)='UB'
42080     client_has$(2)='PR'
42100     client_has$(3)='GL'
42120     client_has$(4)='CL'
42130     client_has$(5)='CO'
42140     client_has_count=5
42160   else
42180     client_has_count=fnclient_has_on_support_list(mat client_has$, grace_days)
42200   end if
42220   if client_has_count=0 then
42240     fn_status('')
42260     fn_status('No ACS Support detected.')
42280     fn_status('To update contact ACS at 1-800-643-6318.')
42300     fn_status('')
42320     fn_status_pause
42340   else
42360     fn_status('Systems on Support:')
44000     for client_has_item=1 to client_has_count
44020       fn_status('   '&fnSystemName$(client_has$(client_has_item)))
44040       dim support_text$*256
44060       u_which=srch(mat system_id$,client_has$(client_has_item))
44080       if u_which>0 then
44100         if days(date('ccyymmdd'),'ccyymmdd')<=days(system_support_end_date(u_which),'ccyymmdd') then
44120           support_text$='      active until '
44140           support_text$(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
44160         else
44180           support_text$='      Support expired on '
44200           support_text$(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
44220           if on_support(u_which) then
44240             support_text$(inf:inf)=' but update is allowed during '&str$(grace_days)&' day grace period.'
44250           end if
44260         end if
44280         fn_status(support_text$)
44300       else
44320         fn_status(chr$(9)&chr$(9)&'      (no support data)')
44340       end if
46000     next client_has_item
46240     ! client_has_count=udim((mat client_has$))
46260     if srch(mat client_has$,'G2')>0 then client_has_count=client_has_count-1
46280     if srch(mat client_has$,'HH')>0 then client_has_count=client_has_count-1
46300     if srch(mat client_has$,'P4')>0 then client_has_count=client_has_count-1
46320     if srch(mat client_has$,'U4')>0 then client_has_count=client_has_count-1
46340     fn_status("Downloading Updates for "&str$(client_has_count)&" support licensed systems.")
46360     fn_status("This may take up to "&str$(client_has_count*2+11)&" minutes on systems with slow internet connections.")
46380     fn_status("Download started at "&time$)
46400     fn_status("Please wait...")
48000     !  fn_download_an_update(system_id$*2)
48020     open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output
48040     pr #h_script: 'user acs5update'
48060     pr #h_script: 'ACSKen!1'
48080     pr #h_script: 'LCD '&env$('Temp')
48100     pr #h_script: 'get ACS-5-Update-CO.exe'
48120     for ch_item=1 to udim(mat client_has$)
48140       if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then
48160         pr #h_script: 'get ACS-5-Update-'&client_has$(ch_item)&'.exe'
48180       end if
48200     next ch_item
48220     pr #h_script: 'quit'
48240     close #h_script:
48260     !
50000     open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output
50020     pr #h_batch: 'prompt $p$g'
50040     pr #h_script: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
50060     close #h_batch:
50080     fn_execute('-m',os_filename$(batch_name$))
50100     u_download_success=0
50120     open #h_return:=fngethandle: 'Name='&return_name$,display,input
50140     do
50160       linput #h_return: line$ eof U_RETURN_EOF
50180       if line$(1:4)='226 ' then u_download_success=1
50200     loop
50220     U_RETURN_EOF: !
51000     close #h_return:
51020     if u_download_success then
51040       for ch_item=1 to udim(mat client_has$)
51060         if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then
51080           fn_status('launching '&client_has$(ch_item)&' update.')
51100           fn_execute('',env$('temp')&'\acs-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS /NOCANCEL /SILENT')
51120        !       execute 'sy '&env$('temp')&'\acs-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS' ! /SILENT
51140         end if
51160       next ch_item
51180       fn_status('Closing program and launching Core update.')
51200       fn_execute('-c',env$('temp')&'\acs-5-Update-CO.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS')
51220       sleep(4)
51240       fnFree(batch_name$)
51260       fnFree(script_name$)
51280       fnFree(return_name$)
51300       fnreg_write('Last Update',date$('ccyy/mm/dd')) ! &' '&time$)
51320       execute "System"
51340     else
51360       fn_status('*** Update failed to download ***')
51380       fn_status_pause
51400     end if
51420   end if
51990 fnend
52000   def library fnAcsInstallationPath$*256(; longFileName)
52020     if ~setup then let fn_setup
52040     fnAcsInstallationPath$=fn_acs_installation_path$( longFileName)
52060   fnend
54000   def fn_acs_installation_path$*256(; longFileName)
54020     dim acs_installation_path$*256
54040 !   if ~setup_acs_installation_path then
54060 !     setup_acs_installation_path=1
54080 !     fnIniOpen('acs.ini')
54100 !     acs_installation_path$=fnIniRead$('Core','InstallPath')
54120 !     if acs_installation_path$='' then
54140     acs_installation_path$=os_filename$('S:\')
54160 !     end if
54180 !   end if
54200     if longFileName then
54220       acs_installation_path$=srep$(acs_installation_path$,'PROGRA~2','Program Files (x86)')
54240       acs_installation_path$=srep$(acs_installation_path$,'ACS5~1','ACS 5')
54260     end if
54280     fn_acs_installation_path$=acs_installation_path$
54300   fnend
60000   def fn_drive_sys_must_exist
60020     if ~exists('S:\Drive.sys') then
60030       fn_status('creating missing Drive.sys')
60040       open #h_drive_sys:=fngethandle: 'Name=S:\Drive.sys,RecL=256,New',display,output
60060       open #h_brconfig_sys:=fngethandle: 'Name=S:\BRConfig.sys',display,input
60080       pr #h_drive_sys: 'Rem Drive.sys automatically created by update process on '&date$&' at '&time$
60100       do
60120         linput #h_return: line$ eof DSME_BRCONFIG_EOF
60140         if lwrc$(line$(1:6))='drive ' or line$(1:1)='@' then
60160           pr #h_drive_sys: line$
60180         end if
60200       loop
60220 DSME_BRCONFIG_EOF: !
60240       close #h_brconfig_sys:
60260       close #h_drive_sys:
60280     end if
60300   fnend
62000   def fn_make_mnu_file(mmf_file$*256)
62020     open #h_tmp:=fngethandle: 'Name='&mmf_file$&',RecL=256,New',display,output ioerr MMF_FAIL
62040     close #h_tmp:
62060     goto MMF_XIT
62080 MMF_FAIL: !
62100     dim mg$(2)*80
62120     mat mg$(2)
62140     mg$(1)='Failed to create'
62160     mg$(2)=os_filename$(mmf_file$)
62180     fnmsgbox(mat mg$,response$, cap$,48+0)
62200 MMF_XIT: !
62220   fnend
79000   def library fnStatus(text$*512)
79010     if ~setup then let fn_setup
79020     fnStatus=fn_status(text$)
79040   fnend
80000   def fn_status(text$*512)
80020     if ~status_initialized or file$(h_status_win)='' then
80040       status_initialized=1
80060       dim headings$(1)*40,widths(1),forms$(1)*40,status_gridspec$*80
80080       open #h_status_win:=fngethandle: 'SRow=1,SCol=1,Rows=20,Cols=80,Parent=None,Caption=Status',display,output
80100       status_gridspec$='#'&str$(h_status_win)&',1,1,List 20/80'
80120       headings$(1)='Status'
80140       widths(1)=80
80160       forms$(1)='C 512'
80180       pr f status_gridspec$&",headers,[gridheaders]": (mat headings$,mat widths, mat forms$)
80200     end if
80220     if env$('ACSDeveloper')<>'' then
80240       pr f status_gridspec$&",+": text$(1:512)
80260     else
80280       pr f status_gridspec$&",+": text$(1:512) error ignore
80300     end if
80320 !
80340     input fields status_gridspec$&",rowcnt,all,nowait": grid_rows
80360     curfld(1,grid_rows+1)
80380 !
80400 fnend
81000 def library fnStatusPause
81010   if ~setup then let fn_setup
81020   fnStatusPause=fn_status_pause
81040 fnend
82000 def fn_status_pause
82020   fn_status('Press any key to continue.')
82040   kstat$(1)
82060 fnend
86000 def fn_execute(flags$*128,exe_what$*256)
86020   if env$('ACSDeveloper')<>'' then
86040     pr 'Flags:  '&flags$
86060     if exists(exe_what$) then
86070       ! PAusE
86080       execute 'sy -c -w notepad '&exe_what$
86100     else
86120       pr 'HMMM:  sy '&flags$&' '&exe_what$
86130     end if
86140   else
86160     execute 'sy '&flags$&' '&exe_what$
86180   end if
86200 fnend
87000 def library fnStatusClose
87020   if ~setup then let fn_setup
87040   fnStatusClose=fn_status_close
87060 fnend
87080 def fn_status_close
87100   close #h_status_win: ioerr ignore
87120   h_status_win=0
87140   status_initialized=0
87160 fnend
