03000 ! Replace R:\Core\Start
03060   print "Loading ACS System..."
03080   if env$('ACSDeveloper')='' then execute "config statusline off"
03100   let fn_env_data_default
03200   let fn_map_to_virtural_drive(env$('data'),'Q:')
03300   let fn_map_to_virtural_drive(os_filename$(''),'R:')
03400 !
04000   if env$('BR_MODEL')='CLIENT/SERVER' then
04020    execute 'config shell default client'
04040    fncs_env
04060    setenv('local_program_dir','@:'&env$("CLIENT_BR")(1:pos(env$("CLIENT_BR"),'\',-1)-1))
04080    setenv('at','@:') 
04100  else
04120    setenv('local_program_dir',os_filename$('R:'))
04140    setenv('at','')
04160   end if
04400 !
05000   execute "load R:\Core\start.br,Resident"
05020   execute "load R:\Core\fnindex_it.br,Resident"
05040   execute "load R:\Core\acs_component.br,Resident"
05060   execute "load R:\Core\ace\cursys.br,resident"
05080   execute "load R:\Core\ace\console.br,resident"
05100   execute "load R:\Core\ace\getcd.br,resident"
05120   execute "load R:\Core\ace\cap.br,resident"
05140   execute "load R:\Core\ace\prg.br,resident"
10080   let fn_setup
10140   let cap$='R:\Core\Start'
10160   if env$('ACSDeveloper')<>'' and env$('BR_MODEL')<>'CLIENT/SERVER' then let fncheckcompiled ! sets the current directory to "R:" if it is not already
10180   if env$('ACSDeveloper')<>'' and env$('BR_MODEL')<>'CLIENT/SERVER' then let fn_update_version_for_inno
11000   if env$('BR_MODEL')='CLIENT/SERVER' then
11010     execute 'config editor'
11020     if ~exists(env$('programdata')&'\ACS_BR_CS_Temp_s'&session$) then
11040       execute 'MkDir '&env$('programdata')&'\ACS_BR_CS_Temp_s'&session$
11060     end if
11080     setenv('Temp',env$('programdata')&'\ACS_BR_CS_Temp_s'&session$)
11100   end if
11120   if ~fn_temp_dir_validate then goto XIT ! if env$('BR_MODEL')<>'CLIENT/SERVER' and ~fn_temp_dir_validate then goto XIT
11130   if env$('client_temp')='' then let setenv('Client_TEMP',env$('Temp'))
11140   if ~fn_rights_test('Q:',"Try Run As Administrator.",'Data') then goto XIT
11160   if ~fn_rights_test(env$('temp'),'Correct your Temp environment varialbe.','Temp') then goto XIT ! to %USERPROFILE%\AppData\Local\Temp
12000   let setenv("Icon","R:\Core\Icon\ACS-v5-32x32-32bit.ico")
12020   if ~exists("Q:\Data") then execute "MkDir Q:\Data"
12040   if ~exists("Q:\INI") then execute "MkDir Q:\INI"
12060 ! if ~exists("Q:\Share") then execute "MkDir Q:\Share"
12080   if ~exists('Q:\Report Cache') then execute 'MKDir "Q:\Report Cache"'
12100   if fn_move_core_data('CityStZip.dat') then let fn_move_core_data('CityStZip.idx',1)
12120   if fn_move_core_data('1099Box.dat') then let fn_move_core_data('1099Box.idx',1)
12140   let fn_udf_resolve
12160 ! if exists(udf$&"Reads_and_Chgs.h1") then
12180 !   fn_move_data(udf$&"Reads_and_Chgs.h*","Q:\UBmstr\Reads_and_Chgs.h*",1)
12200 !   fn_move_data(udf$&"Reads_and_Chgs-Key.h*","Q:\UBmstr\Reads_and_Chgs-Key.h*",1)
12220 !  end if
12240 ! if env$('ACSDeveloper')<>'' then
12260   execute 'CD '&env$('temp')(1:2)
12280   execute 'CD '&env$('temp')(3:len(env$('temp')))
13000   fncopy('r:\core\ScreenIO\ScreenIO.ini.prc','ScreenIO.ini')
13020   fncopy('r:\core\FileIO\FileIO.ini.prc','FileIO.ini')
13040   open #1: 'name=r,replace',d,o
13060   pr #1: 'rem This R file is just a convinience for developers and is not an intregal part of the system.'
13080   pr #1: 'stop'
13100   pr #1: 'clear resi'
13120   pr #1: 'run '&program$
13140   close #1:
14000   let setenv("PD",'R:\') ! for fnsnap compatibility - just in case we implement it later 10/2/2012
14020   let fnapply_theme
14040   if ~fn_multisession_test then goto XIT
15000   let version_prior$=fn_last_version_used$
15020   let version_current$=fnacs_version$
15040   if fn_update_needed(version_prior$,version_current$) then 
15060     let fnchain('R:\Core\Programs\Update.br')
15080   end if 
15140   if version_current$>version_prior$ then 
15160     let fn_last_version_used$(acs_version_running$)
15180   end if 
15990   let fnchain('R:\Core\Menu.br')
18000 XIT: execute "System"
19000 IGNORE: continue 
19800 ! <Updateable Region: ERTN>
19900 ERTN: let fnerror(cap$,err,line,act$,"xit")
20000   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20200   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
20300 ERTN_EXEC_ACT: execute act$ : goto ERTN
20400 ! /region
22000   def fn_setup
22020     if ~setup then 
22040       let setup=1
22060       option retain 
22100       dim cap$*128
22180       library 'R:\Core\Library': fnerror,fnchain,fncheckcompiled,fnapply_theme,fngetdir2,fncopy,fnshortpath$,fnureg_read,fnacs_version$,fncheckfileversion
22190       library 'R:\Core\Library': fnreg_read,fnreg_write
22200     end if 
22220   fnend 
27000   def library fnrights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048)
27010     if ~setup then let fn_setup
27020     let fnrights_test=fn_rights_test(rt_folder$,rt_how_to_fix$,folder_name$, additional_text_for_failure$)
27040   fnend 
29000   def fn_rights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048)
29020     let rt_return=1 ! returns 1 if passed test or 0 if failed.
29040     let rt_folder$=trim$(rt_folder$)
29060     if rt_folder$<>'' and rt_folder$(len(rt_folder$):len(rt_folder$))<>'\' then let rt_folder$=rt_folder$&'\'
29080 ! 
29100     open #h_test:=1: 'Name='&rt_folder$&'tmp_rights_test'&session$&'.dat,Replace,RecL=384',internal,outin,relative ioerr RT_FAIL
29120     close #h_test: 
29140     execute 'free "'&rt_folder$&'tmp_rights_test'&session$&'.dat"' ioerr RT_FAIL
29160     goto RT_PASS
29180 RT_FAIL: ! 
29200     let rt_return=0
29220     if err=4205 then 
29240       let msgbox("Insufficient rights to access "&folder_name$&" Folder ("&os_filename$(rt_folder$)&")"&chr$(13)&rt_how_to_fix$&chr$(13)&additional_text_for_failure$)
29260       let rt_return=0
29280     end if 
29300 RT_PASS: ! 
29320     let fn_rights_test=rt_return
29340   fnend 
34000   def fn_move_data(file_name$*256,destination_name$*256; ignore_exists)
34020     let md_return=0
34040     if ignore_exists or (exists(file_name$) and ~exists(destination_name$)) then 
34060       execute 'Copy "'&file_name$&'" "'&destination_name$&'"' ioerr MOVE_DATA_XIT
34080       execute 'Free "'&file_name$&'"' ioerr ignore
34100       let md_return=1
34120     end if 
34130 MOVE_DATA_XIT: ! 
34140     let fn_move_data=md_return
34160   fnend 
34180   def fn_move_core_data(file_name$*256; ignore_exists)
34200     let fn_move_core_data=fn_move_data('R:\Core\Data\'&file_name$,"Q:\Data\"&file_name$, ignore_exists)
34220   fnend 
36000   def fn_map_to_virtural_drive(path_to_map$*256,drive_id$*2)
36020     execute 'config drive '&drive_id$(1:1)&','&rtrm$(path_to_map$,'\')&',X,\' ioerr ignore
36040   fnend 
38000   def fn_temp_dir_validate
38020     let tdt_return=1
38040     if lwrc$(env$('temp'))='c:\windows\temp' then 
38060       if ~exists(env$('USERPROFILE')) then 
38080         let msgbox('Security Error: User Profile directory ('&env$('USERPROFILE')&') does not exist.')
38100         let tdt_return=0
38140       else 
38160         let tdt_return=fn_change_temp
38180       end if 
38200     end if 
38280     let fn_temp_dir_validate=tdt_return
38300   fnend 
40000   def fn_change_temp
40020     let ct_return=1
40040     if ~exists(env$('USERPROFILE')&'\AppData') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData"'
40060     if ~exists(env$('USERPROFILE')&'\AppData\Local') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData\Local"'
40080     if ~exists(env$('USERPROFILE')&'\AppData\Local\Temp') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData\Local\Temp"'
40100     if ~exists(env$('USERPROFILE')&'\AppData\Local\Temp') then 
40120       let msgbox('Startup Error: Tried to create a new Temp directory ('&env$('USERPROFILE')&'\AppData\Local\Temp) but failed.')
40140       let ct_return=0
40160     else 
40180       let setenv('Tmp',env$('USERPROFILE')&'\AppData\Local\Temp')
40200       let setenv('Temp',env$('USERPROFILE')&'\AppData\Local\Temp')
40220     end if 
40240     let fn_change_temp=ct_return
40260   fnend 
42000   def fn_udf_resolve
42010     dim udf$*1024
42020     let fn_get_udf(udf$)
42040 ! pr 'udf$:'&udf$ : pause
42060     if udf$<>'' and exists(udf$)=1 then ! then it is a directory that exists
42080       let tmp_dir_count=1
42100       let tmp_dir$(1)=rtrm$(udf$,'\')
42120       dim filename$(1)*1024,tmp_dir$(1)*1024
42140 !   pr mat filename$ ! pause
42160       execute 'sy -m del "'&os_filename$(udf$)&'\*.scr"'
42180       execute 'sy -m del "'&os_filename$(udf$)&'\*.tmp"'
42200       execute 'sy -m xcopy "'&os_filename$(udf$)&'" "'&os_filename$('Q:\')&'" /S /T'
42220       let fngetdir2(udf$,mat filename$, '/s /b','*.*') ! fngetdir2(udf$&'ini',mat filename$, '/s /b','*.*')
42240       for f_i=1 to udim(mat filename$)
42260         if exists(filename$(f_i))=1 then ! it is a directory
42280           let tmp_dir_count+=1
42300           mat tmp_dir$(tmp_dir_count)
42320           let tmp_dir$(tmp_dir_count)=filename$(f_i)
42340         else ! it is a file.
42360           dim tmp_to$*1024,tmp_from$*1024
42380           let tmp_to$='q:\'&filename$(f_i)(len(udf$)+1:len(filename$(f_i)))
42400           let tmp_from$=lwrc$(filename$(f_i))
42420 ! pr 'tmp_from$:'&tmp_from$ : pr '  tmp_to$:'&tmp_to$ : pause
42440           if pos(tmp_from$,lwrc$('Reads_and_Chgs'))>0 then 
42460             let fncopy(tmp_from$,'Q:\UBmstr\*.*')
42480             execute 'free "'&tmp_from$&'"' ioerr ignore
42500           else if ~exists(tmp_to$) then 
42520             if fncopy(tmp_from$,tmp_to$) then 
42540               execute 'free "'&tmp_from$&'"' ioerr ignore
42560             end if 
42580           else if exists(tmp_to$) then 
42600             execute 'free "'&tmp_from$&'"' ioerr ignore
42620           end if 
42640         end if 
42660       next f_i
42680       for d_i=tmp_dir_count to 1, step -1
42700         execute 'rmdir '&tmp_dir$(d_i) ioerr ignore
42720       next d_i
42740     end if 
42760   fnend 
43000   def fn_get_udf(&udf$)
43040     dim oldudf$*256
43120 ! 
43140     if oldudf$<>"" then 
43160       let udf$=oldudf$
43220     else if env$("ScreenAceTemp")="" then !    NEW - just return blank - we do not need to make anything
43260 !    dim app_data$*256
43280 !    let app_data$=fnshortpath$(env$("AppData"))
43300 !    if ~exists(app_data$&"\ACS") then execute 'mkdir '&app_data$&"\ACS"
43320 !    if ~exists(app_data$&"\ACS\Temp") then execute 'mkdir '&app_data$&"\ACS\Temp"
43340       let udf$=oldudf$="" ! app_data$&"\ACS\Temp\"
43420     else 
43440       let oldudf$=udf$=fnshortpath$(env$("ScreenAceTemp"))&'\'
43460     end if 
43480     let udf$(3:len(udf$))=srep$(udf$(3:len(udf$)),'\\','\')
43500   fnend 
44000   def fn_env_data_default
44020     if env$('data')='' then ! if env$('data') is blank than set it here.
44040       dim edd_base$*256
44060       if env$('ProgramData')='' then 
44070         library 'core\library': fnshortpath$
44080         let edd_base$=fnshortpath$(env$('appdata'))
44100       else 
44120         let edd_base$=env$('ProgramData')
44140       end if 
44160       let setenv('data',edd_base$&'\ACS\')
44180       if ~exists(env$('data')) then 
44200         execute 'mkdir '&env$('data')
44220       end if 
44240     else if env$('data')(len(env$('data')):len(env$('data')))<>'\' then ! if env$('data') does not end with a backslash than add one here.
44260       let setenv('data',env$('data')&'\')
44280     end if 
44300   fnend 
46000   def fn_update_needed(acs_version_prior$,acs_version_running$)
46080     let un_return=0
46100     if acs_version_running$<acs_version_prior$ then 
46120       let un_return=1
46140       let msgbox("The ACS Software version ("&acs_version_running$&") of this workstation is less than the last version ("&acs_version_prior$&") used to access ACS Data."&chr$(13)&"You must update this workstation to continue.")
46200     end if 
46220     let fn_update_needed=un_return
46240   fnend 
48000   def fn_last_version_used$(; setit$*256)
48020     dim lvu_line$*256
48040     if setit$<>'' then 
48060       let lvu_line$=trim$(setit$)
48080       let fnreg_write('ACS last version used',setit$)
48100     else 
48120       let fnreg_read('ACS last version used',lvu_line$)
48140       if lvu_line$='' then 
48160         open #1: 'Name=Q:\Data\ACS_Version.txt,RecL=256',display,input ioerr LVU_OLD_FILE_OPEN_IOERR
48180         linput #1: lvu_line$
48200         close #1,free: 
48220         let fnreg_write('ACS last version used',lvu_line$)
48240       end if 
48260     end if 
48280 LVU_OLD_FILE_OPEN_IOERR: ! 
48300     let fn_last_version_used$=trim$(lvu_line$)
48320   fnend 
50000   def fn_multisession_test
50020     let fnureg_read('Disable_MultiSession',disable_multisession$)
50040     if val(session$(len(session$):len(session$)))=>2 and disable_multisession$='True' then 
50060       let msgbox('Multipe sessions have been disabled in user prefrences.')
50080       let mt_return=0
50100     else 
50120       let mt_return=1
50140     end if 
50160     let fn_multisession_test=mt_return
50180   fnend 
52000   def fn_update_version_for_inno
52020     library 'r:\core\library': fngethandle
52040     open #h_tmp:=fngethandle: 'name=:C:\ACS\Setup\ACS 5 - AppVersion.iss,RecL=256,Replace',display,output 
52060     print #h_tmp: ';This file is dynamically built by '&os_filename$(program$)&' when run by an ACSDeveloper.'
52080     print #h_tmp: ';Attempts to edit it directly are moot and will be quickly overwritten.'
52100     print #h_tmp: 'AppVersion='&fnacs_version$
52120     close #h_tmp: 
52140   fnend 
54000 def fncs_env
54020   dim ce_line$*2048
54040   dim ce_prefix$
54060   ce_prefix$="Client_"
54080   dim ce_field$*2048
54100   dim ce_value$*2048
54120   dim ce_os_temp_file$*1048
54140   dim ce_br_temp_file$*1048
54160   ce_os_temp_file$=rtrm$(env$('data'),'\')&'\cs-'&session$&'.txt'
54180   ce_br_temp_file$='Q:\cs-'&session$&'.txt'
54182   ce_retry_4152_count=0
54190   CE_MAKE_TEMP_FILE: !
54200   execute '*sys -M set > "'&ce_os_temp_file$&'"'
54220   open #1: "Name="&ce_br_temp_file$,display,input error CE_DEBUG_OPEN_ERR ! error XIT_FNCS_ENV
54240   do
54260     linput #1: ce_line$ error XIT_LOOP
54280     let gw_wholeline=len(rtrm$(ce_line$)) 
54300     let gw_addlen=1 
54320     let gw_posfnwp=pos(uprc$(ce_line$),"=")
54340     if gw_posfnwp>0 then
54360       let gw_equal =pos(ce_line$,'=')
54380       let gw_nextequal =pos(ce_line$,'=',gw_posfnwp+gw_addlen)
54400       if gw_equal > 0 then
54420         let ce_field$ = ce_prefix$&ce_line$(1:gw_posfnwp-1)
54440         let ce_value$ = ce_line$(gw_posfnwp+1:gw_wholeline)
54460         let setenv(ce_field$,ce_value$) error ignore
54480 !       pr 'setenv("'&ce_field$&'","'&ce_value$&'")'
54500 ! Should SETENV FAIL, Ignore it
54520       end if
54540     end if
54560   loop
56000 CE_DEBUG_OPEN_ERR: ! 
56010 if err=4152 and (ce_retry_4152_count+=1)<=3 then goto CE_MAKE_TEMP_FILE
56020 pr 'error '&str$(err)&' on open of ce_br_temp_file$'
56040 pr '     ce_os_temp_file$='&ce_os_temp_file$
56060 pr '     ce_br_temp_file$='&ce_br_temp_file$
56080 pr '          exists=';exists(ce_br_temp_file$)
56100 pause
58000 XIT_LOOP: ! End of Startloop
58020   close #1,free: error ignore
58040 !
58060 XIT_FNCS_ENV: !
58080   execute "*sy -M CD > "&ce_os_temp_file$
58100   open #1: "Name="&ce_br_temp_file$,display,input error XIT_FNCS_OS_PATH
58120   linput #1: client_os_path$ error ignore
58140   close #1,free: error ignore
58160 setenv('client_os_path',client_os_path$)
58180 XIT_FNCS_OS_PATH: ! 

58200 fnend 