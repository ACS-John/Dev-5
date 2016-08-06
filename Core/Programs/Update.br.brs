10000 ! R:\Core\Programs\Update
10100   let force_update=1
10200 ! r: dims and constants
10300   if ~setup then let fn_setup
10400 ! dim message$(1)*256
10500   dim batch_name$*256
10600   dim script_name$*256
10700   dim return_name$*256
10800   dim line$*256
10900   let batch_name$=env$('temp')&'\update'&session$&'.cmd'
11000   let script_name$=env$('temp')&'\ftp_script'&session$&'.txt'
11100   let return_name$=env$('temp')&'\return'&session$&'.txt'
11110   let grace_days=45
11120   let fnclient_support(mat system_id$,mat system_support_end_date,mat on_support,grace_days)
11200 ! /r
12000   let fntop(program$,"ACS Update")
20000 ! r: main loop
20002   let fn_status('launching update')
20028   if ~fn_update_available then 
20040 !   let fn_status('No newer update is available.')
20060     let fn_status_pause
20100   else 
20120     let fn_drive_sys_must_exist
20140     let fn_update
20160   end if 
20180 XIT: let fnxit
20200 ! /r
25000   def fn_setup
25020     let setup=1
25040     library 'R:\Core\library': fnreg_read,fnreg_write,fnmsgbox,fntop,fnxit,fngethandle,fnclient_has_on_support_list,fnsystem_abbr_2_name$,fnclient_support,fnerror
25050     on error goto ERTN
25060   fnend 
26000 IGNORE: continue 
26020 ! <updateable region: ertn>
26040 ERTN: let fnerror(cap$,err,line,act$,"xit")
26060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
26080   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
26100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
26120 ERTN_EXEC_ACT: execute act$ : goto ERTN
26140 ! </updateable region: ertn>
30000   def fn_acs_update_date$*40(;setit$*40)
30020     dim aud_return$*40
30040     if setit$='(default)' then let setit$=date$('ccyy/mm/dd')&' - '&time$
30060     if setit$<>'' then 
30080       let aud_return$=setit$
30100       let fnreg_write('ACS.Update.Date',setit$)
30120     else 
30140       let fnreg_read('ACS.Update.Date',aud_return$)
30160     end if 
30180     let fn_acs_update_date$=aud_return$
30200   fnend 
32000   def fn_update_available
32020     dim conectivity_test_result$*40
32040     dim ua_acs_datetime$*40
32060 ! 
32080     let conectivity_test_result$=fn_conectivity_test$
32100     if conectivity_test_result$='' and env$('ACSDeveloper')='' then 
32120       let fn_status('Connectivity test failed.')
32140 !     let fn_status_pause
32160       let ua_return=0
32180     else 
32200       let ua_return=1
32220     end if 
32240     let ua_acs_datetime$=fn_acs_update_date$
32260     let fn_update_available=ua_return
32280   fnend 
40000   def fn_conectivity_test$*40
40010     let fn_status("testing connectivity...")
40020     let wud_success=0
40040     dim wud_return$*40
40060     let wud_return$=''
40080     open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output 
40100     print #h_script: 'user acs5update'
40120     print #h_script: 'ACSKen!1'
40140     print #h_script: 'Dir ACS-5-Update-CO.exe'
40160     print #h_script: 'quit'
40180     close #h_script: 
40190 ! 
40200     open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output 
40220     print #h_batch: 'prompt $p$g'
40260     print #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
40400     close #h_batch: 
40420     let fn_execute('-m',os_filename$(batch_name$))
40440     open #h_return:=fngethandle: 'Name='&return_name$,display,input 
40460     do 
40480       linput #h_return: line$ eof WUD_RETURN_EOF
40500       if line$(3:3)='-' then 
40540         let wud_date$=date$(days(line$(1:8),'mm-dd-yy'),'ccyy/mm/dd')
40560         let wud_time$=line$(11:15)
40580         let wud_ampm$=line$(16:17)
40600         if wud_ampm$='PM' then 
40620           let wud_time$(1:2)=str$(val(wud_time$(1:2))+12)
40640         end if 
40660         let wud_return$=wud_date$&' - '&wud_time$&':00'
40700       end if 
40720     loop 
40740 WUD_RETURN_EOF: ! 
40760     close #h_return: 
40780     execute 'free '&return_name$
40800     execute 'free '&script_name$
40820     execute 'free '&batch_name$
40900     let fn_conectivity_test$=wud_return$
40920   fnend 
42000   def fn_update
42020 ! let fnclient_has_mat(mat client_has$)
42040     let client_has_count=fnclient_has_on_support_list(mat client_has$, grace_days)
42060     if client_has_count=0 then 
42080       let fn_status('')
42100       let fn_status('No ACS Support detected.')
42120       let fn_status('To update contact ACS at 1-800-643-6318.')
42140       let fn_status('')
42160       let fn_status_pause
42180     else 
42200       let fn_status('Systems on Support:')
44000       for client_has_item=1 to client_has_count
44020         let fn_status('   '&fnsystem_abbr_2_name$(client_has$(client_has_item)))
44040         dim support_text$*256
44060         let u_which=srch(mat system_id$,client_has$(client_has_item))
44080         if u_which>0 then 
44100           if days(date('ccyymmdd'),'ccyymmdd')<=days(system_support_end_date(u_which),'ccyymmdd') then 
44120             let support_text$='      active until '
44140             let support_text$(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
44160           else 
44180             let support_text$='      Support expired on '
44200             let support_text$(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
44220             if on_support(u_which) then 
44240               let support_text$(inf:inf)=' but update is allowed during '&str$(grace_days)&' day grace period.'
44250             end if 
44260           end if 
44280           let fn_status(support_text$)
44300         else 
44320           let fn_status(chr$(9)&chr$(9)&'      (no support data)')
44340         end if 
46000       next client_has_item
46020 ! r: if a sub system is registered but does not have the XX.mnu for it than make it so the installer will update it.
46040       if srch(mat client_has$,'HH')>0 and ~exists('hh.mnu') then 
46060         let fn_make_mnu_file ('HH.mnu')
46080       end if 
46100       if srch(mat client_has$,'P2')>0 and ~exists('P2.mnu') then 
46120         let fn_make_mnu_file('P2.mnu')
46140       end if 
46160       if srch(mat client_has$,'G2')>0 and ~exists('G2.mnu') then 
46180         let fn_make_mnu_file('G2.mnu')
46200       end if 
46220 ! /r
46240 ! let client_has_count=udim((mat client_has$))
46260       if srch(mat client_has$,'G2')>0 then let client_has_count=client_has_count-1
46280       if srch(mat client_has$,'HH')>0 then let client_has_count=client_has_count-1
46300       if srch(mat client_has$,'P4')>0 then let client_has_count=client_has_count-1
46320       if srch(mat client_has$,'U4')>0 then let client_has_count=client_has_count-1
46340       let fn_status("Downloading Updates for "&str$(client_has_count)&" support licensed systems.")
46360       let fn_status("This may take up to "&str$(client_has_count*2+11)&" minutes on systems with slow internet connections.")
46380       let fn_status("Download started at "&time$)
46400       let fn_status("Please wait...")
48000 !  fn_download_an_update(system_id$*2)
48020       open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output 
48040       print #h_script: 'user acs5update'
48060       print #h_script: 'ACSKen!1'
48080       print #h_script: 'LCD '&env$('Temp')
48100       print #h_script: 'get ACS-5-Update-CO.exe'
48120       for ch_item=1 to udim(mat client_has$)
48140         if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then 
48160           print #h_script: 'get ACS-5-Update-'&client_has$(ch_item)&'.exe'
48180         end if 
48200       next ch_item
48220       print #h_script: 'quit'
48240       close #h_script: 
48260 ! 
50000       open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output 
50020       print #h_batch: 'prompt $p$g'
50040       print #h_script: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
50060       close #h_batch: 
50080       let fn_execute('-m',os_filename$(batch_name$))
50100       let u_download_success=0
50120       open #h_return:=fngethandle: 'Name='&return_name$,display,input 
50140       do 
50160         linput #h_return: line$ eof U_RETURN_EOF
50180         if line$(1:4)='226 ' then let u_download_success=1
50200       loop 
50220 U_RETURN_EOF: ! 
51000       close #h_return: 
51020       if u_download_success then 
51040         for ch_item=1 to udim(mat client_has$)
51060           if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then 
51080             let fn_status('launching Core '&client_has$(ch_item)&' update.')
51100             let fn_execute('',env$('temp')&'\ACS-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS')
51120 !       execute 'sy '&env$('temp')&'\ACS-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS' ! /SILENT
51140           end if 
51160         next ch_item
51180         let fn_status('Closing program and launching Core update.')
51200         let fn_execute('-c',env$('temp')&'\ACS-5-Update-CO.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS')
51220         let sleep(4)
51240         execute 'free '&batch_name$
51260         execute 'free '&script_name$
51280         execute 'free '&return_name$
51300         let fnreg_write('Last Update',date$('ccyy/mm/dd')) ! &' '&time$)
51320         execute "System"
51340       else 
51360         let fn_status('*** Update failed to download ***')
51380         let fn_status_pause
51400       end if 
51420     end if 
51990   fnend 
52000   def fn_acs_installation_path$*256
52020     dim acs_installation_path$*256
52040 !   if ~setup_acs_installation_path then
52060 !     let setup_acs_installation_path=1
52080 !     let fnini_open('acs.ini')
52100 !     let acs_installation_path$=fnini_read$('Core','InstallPath')
52120 !     if acs_installation_path$='' then
52140     let acs_installation_path$=os_filename$('R:\')
52160 !     end if
52180 !   end if
52200     let fn_acs_installation_path$=acs_installation_path$
52220   fnend 
60000   def fn_drive_sys_must_exist
60020     if ~exists('R:\Drive.sys') then 
60030       let fn_status('creating missing Drive.sys')
60040       open #h_drive_sys:=fngethandle: 'Name=R:\Drive.sys,RecL=256,New',display,output 
60060       open #h_brconfig_sys:=fngethandle: 'Name=R:\BRConfig.sys',display,input 
60080       print #h_drive_sys: 'Rem Drive.sys automatically created by update process on '&date$&' at '&time$
60100       do 
60120         linput #h_return: line$ eof DSME_BRCONFIG_EOF
60140         if lwrc$(line$(1:6))='drive ' or line$(1:1)='@' then 
60160           print #h_drive_sys: line$
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
62140     let mg$(1)='Failed to create'
62160     let mg$(2)=os_filename$(mmf_file$)
62180     let fnmsgbox(mat mg$,response$, cap$,48+0)
62200 MMF_XIT: ! 
62220   fnend 
79000   def library fnstatus(text$*512)
79010     if ~setup then let fn_setup
79020     let fnstatus=fn_status(text$)
79040   fnend 
80000   def fn_status(text$*512)
80020     if ~status_initialized or file$(h_status_win)='' then 
80040       let status_initialized=1
80060       dim headings$(1)*40,widths(1),forms$(1)*40,status_gridspec$*80
80080       open #h_status_win:=fngethandle: 'SRow=1,SCol=1,Rows=20,Cols=80,Parent=None,Caption=Status',display,output 
80100       let status_gridspec$='#'&str$(h_status_win)&',1,1,List 20/80'
80120       let headings$(1)='Status'
80140       let widths(1)=80
80160       let forms$(1)='C 512'
80180       print fields status_gridspec$&",headers,[gridheaders]": (mat headings$,mat widths, mat forms$)
80200     end if 
80220     if env$('ACSDeveloper')<>'' then 
80240       print fields status_gridspec$&",+": text$(1:512)
80260     else 
80280       print fields status_gridspec$&",+": text$(1:512) error ignore
80300     end if 
80320 ! 
80340     input fields status_gridspec$&",rowcnt,all,nowait": grid_rows
80360     let curfld(1,grid_rows+1)
80380 ! 
80400   fnend 
81000   def library fnstatus_pause
81010     if ~setup then let fn_setup
81020     let fnstatus_pause=fn_status_pause
81040   fnend 
82000   def fn_status_pause
82020     let fn_status('Press any key to continue.')
82040     let kstat$(1)
82060   fnend 
86000   def fn_execute(flags$*128,exe_what$*256)
86020     if env$('ACSDeveloper')<>'' then 
86040       print 'Flags:  '&flags$
86060       if exists(exe_what$) then 
86070 !       PAusE
86080         execute 'sy -c -w notepad '&exe_what$
86100       else 
86120         print 'HMMM:  sy '&flags$&' '&exe_what$
86130       end if 
86140     else 
86160       execute 'sy '&flags$&' '&exe_what$
86180     end if 
86200   fnend 
87000   def library fnstatus_close
87020     if ~setup then let fn_setup
87040     let fnstatus_close=fn_status_close
87060   fnend 
87080   def fn_status_close
87100     close #h_status_win: ioerr ignore
87120     let h_status_win=0
87140     let status_initialized=0
87160   fnend 
