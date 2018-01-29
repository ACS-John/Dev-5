00100 fn_acsSystemInitialize
00200 fn_setup
00800 fnChain('S:\Core\Menu.br', 0,1)
01000 def library fnAcsSystemInitialize(; isScreenIOtest)
01012    if ~setup then let fn_setup
01020    fnAcsSystemInitialize=fn_acsSystemInitialize( isScreenIOtest)
01040 fnend
02000 def fn_acsSystemInitialize(; isScreenIOtest)
02020   if ~isScreenIOtest or env$('acsVersion')='' then
02040     startStatusLine=0 : pr newpage
02050     fn_startStatus("Loading ACS System..." )
02060     if env$('ACSDeveloper')='' and login_name$<>'niceguywinning@gmail.com' then execute "config statusLine off"
03000     ! r: set environment variables based on login_name$ and/or BR_MODEL
03020     ! if env$('ACSDeveloper')<>'' then let setenv('disableAutomatedSavePoints','Yes') else let setenv('disableAutomatedSavePoints','')
03040     if env$('ACSDeveloper')<>'' or login_name$='acsbowman' or login_name$='niceguywinning@gmail.com' or env$("AcsClient")='Ed Horton' then 
03060       setenv('enableClientSelection','Yes')
03080     end if
03100     if env$('BR_MODEL')='CLIENT/SERVER' then ! 
03120       if login_name$='niceguywinning@gmail.com' then 
03140         setenv('enableDataFolderByClient','Yes')
03160         pr 'enableDataFolderByClient='&env$('enableDataFolderByClient')
03180       end if
03220     end if
03240     ! /r
04000     execute 'Config FieldBreak Min_Spaces 3, UnderScore Off'
04020     if ~setup then let fn_setup
04040     fnClient$ ! this needs to be called to set client environment variables (before fn_env_data_default)
04060     fn_env_data_default
04080     !
04100     if env$('Q')='' then
04120       if env$('CsServerData')<>'' and env$('BR_MODEL')='CLIENT/SERVER' and env$('enableDataFolderByClient')='Yes' then
04140         if pos(env$('CsServerData'),'/')>0 then slash$='/' else slash$='\'
04160         fn_setQ(rtrm$(env$('CsServerData'),slash$)&slash$&env$('client')) ! fn_map_to_virtural_drive(env$('CsServerData'),'Q:')
04180         fn_setQBase(env$('CsServerData'))
04200       else if env$('CsServerData')<>'' and env$('BR_MODEL')='CLIENT/SERVER' then
04220         fn_setQ(env$('CsServerData')) ! fn_map_to_virtural_drive(env$('CsServerData'),'Q:')
04240         fn_setQBase(env$('CsServerData'))
04260       else if env$('enableDataFolderByClient')='Yes' then
04280         if pos(env$('data'),'/')>0 then slash$='/' else slash$='\'
04300         fn_setQ(rtrm$(env$('data'),slash$)&slash$&env$('client')) ! fn_map_to_virtural_drive(env$('data')&clientDataFolderSuffix$,'Q:') 
04320         fn_setQBase(env$('data'))
04340       else 
04360         fn_setQ(env$('data')) ! fn_map_to_virtural_drive(env$('data')&clientDataFolderSuffix$,'Q:') 
04380         fn_setQBase(env$('data'))
04400       end if
04420     end if
04440     if env$('BR_MODEL')='CLIENT/SERVER' then
04460       execute 'config shell default client'
04480       setenv('at','@::')  ! second colon added 01/18/2018 - to fix client server making files with UNC paths - i.e.  Create Hand Held Files
04490       fn_startStatus('Collecting local environment variables...')
04500       fncs_env
04510       if env$('client_acsDeveloper')<>'' then let setenv('acsDeveloper',env$('client_acsDeveloper'))
04520       setenv('local_program_dir','@:'&env$("CLIENT_BR")(1:pos(env$("CLIENT_BR"),'\',-1)-1))
04540       setenv('userprofile','@::'&env$('client_userprofile'))
04560     else
04580       setenv('local_program_dir',os_filename$('S:'))
04600       setenv('at','')
04620     end if
04640     !
04660     if env$('acsDeveloper')<>'' then 
04680       execute 'config substitute [ScreenIO_ScreenFldDrive] S:'
04700     end if
04720     !
04740     execute "load S:\Core\Menu.br,Resident" error ignore ! hopefully will decrease the amount of time it takes to load the menu between programs
04900     execute "load S:\Core\Library.br,Resident" error ignore
05000     !  fails on windows XP  !  execute "load S:\Core\Start.br,Resident"
05020     execute "load S:\Core\Index.br,Resident"
05040     execute "load S:\Core\ACS_Component.br,Resident"
05080     execute "load S:\Core\fnWindowsStart.br,resident"
05081     execute 'load "S:\Core\FileIO\fileio.br",Resident'
05082     !  maybe but not yet ...     execute "load S:\Core\Client.br,resident"
10080     ! fn_setup  <-- already called
10160     if env$('acsEnableComplier')='Yes' and env$('BR_MODEL')<>'CLIENT/SERVER' and ~isScreenIOtest then let fncheckcompiled ! sets the current directory to "S:" if it is not already 
10180     if env$('acsEnableComplier')='Yes' and env$('BR_MODEL')<>'CLIENT/SERVER' then let fn_update_version_for_inno
11000     if env$('BR_MODEL')='CLIENT/SERVER' then
11020       execute 'config editor'
11040       if env$('programdata')='' and env$('CsServerTemp')<>'' then
11060       setenv('programdata',env$('CsServerTemp'))
11080       end if
11100       setenv('Temp',env$('programdata')&'\ACS\Temp\Session'&session$)
11120       fnmakesurepathexists(env$('Temp')&'\')
11140     end if
11160     if ~fn_temp_dir_validate then goto XIT ! if env$('BR_MODEL')<>'CLIENT/SERVER' and ~fn_temp_dir_validate then goto XIT
11180     if pos(env$('Q'),' ')>0 then 
11200       fn_setQ(fnshortpath$(env$('Q')))
11220     end if
11240     if pos(env$('Qbase'),' ')>0 then 
11260       dim New2Qbase$*256
11280       New2Qbase$=fnshortpath$(env$('Qbase')) 
11300       setenv('QBase','') 
11320       fn_setQbase(New2Qbase$)
11340     end if
11360     if env$('client_temp')='' then let setenv('Client_TEMP',env$('Temp'))
11380     if ~fn_rights_test(env$('Q'),"Try Run As Administrator.",'Data') then goto XIT
11400     if ~fn_rights_test(env$('temp'),'Correct your Temp environment varialbe.','Temp') then goto XIT ! to %USERPROFILE%\AppData\Local\Temp
11420     fn_spoolPath$(1)
11501     ! r: set to last client selected (if appropriate)
11502       if env$('enableClientSelection')='Yes' and env$('clientSelected')='' then
11503         library 'S:\Core\Library': fnSetClient,fnmcreg_read
11504         dim tmpClientSelected$*128
11505         fnmcreg_read('clientSelected',tmpClientSelected$)
11506         fnSetClient(tmpClientSelected$)
11507       end if
11508     ! /r
12000     if env$('acsProduct')='ACS Online' then
12010       setenv("Icon","S:\Core\Icon\ACS Client 32x32-32bit.ico") ! commented out because it made the icon look funny - filled with white and so long as i change the icon on the brclient executable than I'll shouldn't need to re-set it anyway.
12020     else
12030       setenv("Icon","S:\Core\Icon\ACS-v5-32x32-32bit.ico")
12040     end if
12050     fnMakeSurepathExists(env$('Q')&"\Data\")
12120     fnMakeSurepathExists(env$('Q')&'\Report Cache\')
12140     if fn_move_core_data('CityStZip.dat') then let fn_move_core_data('CityStZip.idx',1)
12160     if fn_move_core_data('1099Box.dat') then let fn_move_core_data('1099Box.idx',1)
12180     ! fn_udf_resolve
12200     ! if exists(udf$&"Reads_and_Chgs.h1") then
12220     !   fn_move_data(udf$&"Reads_and_Chgs.h*",env$('Q')&"\UBmstr\Reads_and_Chgs.h*",1)
12240     !   fn_move_data(udf$&"Reads_and_Chgs-Key.h*",env$('Q')&"\UBmstr\Reads_and_Chgs-Key.h*",1)
12260     !  end if
12280     if env$('temp')(2:2)=':' then
12300       execute 'CD '&env$('temp')(1:2)
12320       execute 'CD '&env$('temp')(3:len(env$('temp')))
13000       fnCopy('S:\ScreenIO.ini','screenio.ini')   ! note that destination screenio.ini must be all lowercase as it is case sensitive on some systems
13010       fnCopy('S:\sio.lic','sio.lic')
13020       fn_CopySfileIoIniToFileIoIni
13040     end if
13060     open #hR:=fn_gethandle: 'name=r,replace',d,o
13080     pr #hR: 'stop'
13100     pr #hR: 'clear resi'
13120     pr #hR: 'run '&program$
13140     close #hR:
13160     open #hR:=fn_gethandle: 'name=relive,replace',d,o
13180     pr #hR: 'stop'
13200     pr #hR: 'execute ''load "''&program$&''"'''
13220     pr #hR: 'run '
13240     close #hR:
13260     if env$('ACSDeveloper')<>'' then 
13280       open #hReload:=fn_gethandle: 'name=reload,replace',d,o
13300       pr #hReload: 'execute ''load "''&program$&''"'''
13320       close #hReload:
13340     end if
13360     open #hEd:=fn_gethandle: 'name=ed,replace',d,o
13380     pr #hEd: "exec 'sy "&os_filename$('S:\brEdit.cmd')&' "''&os_filename$(program$)&''"'''
13400     close #hEd:
14020     setenv("PD",'S:\') ! for modified fnsnap compatibility (Core\fnsnap)
14040     ! if isScreenIOtest then disableConScreenOpenDflt=1 else disableConScreenOpenDflt=0
14050     fn_startStatus('Identifying your system...')
14060     fn_uniqueComputerId_initialize ! called to initialize env$('unique_computer_id')
14080     if env$('unique_computer_id')='42601D50-D3A4-81E4-29A3-605718581E48' then ! little koi
14100       setenv('enableClientSelection','Yes')
14120     end if
14140     fn_AcsUserId_Initialize ! called to initialize env$('acsUserId')
14160     fnapply_theme ! ( disableConScreenOpenDflt)
14180     if ~fn_multisession_test then goto XIT
14200     ! setenv('path_to_7z_exe','"'&os_filename$(env$('local_program_dir')&'\Core\Programs\7zip-'&env$('client_platform.os_bits')&'bit\7z.exe')&'"')
14202     setenv('path_to_7z_exe','"'&os_filename$('S:\Core\Programs\7zip-'&env$('server_platform.os_bits')&'bit\7z.exe')&'"')
15000     version_prior$=fn_last_version_used$ 
15020     version_current$=fn_acsVersion$       
15030     setenv('acsVersion',version_current$)
15040     if fn_update_needed(version_prior$,version_current$) then 
15044       fnclient$ ! this needs to be called to set client environment variables
15060       fnchain('S:\Core\Programs\Update.br')
15080     end if 
15140     if version_current$>version_prior$ or env$('ForceScreenIOUpdate')<>'' then 
15150       fn_show_release_notes(version_prior$,version_current$)
15152       fn_FreeVirtualStore
15154       fn_UpdateQFileIO
15156       fn_UpdateQScreenIO
15160       fn_last_version_used$(version_current$)
15180     end if 
15200   end if
15220   !
15240   ! fn_uniqueComputerId_initialize ! called to initialize env$('unique_computer_id')
15260   ! fn_AcsUserId_Initialize ! called to initialize env$('acsUserId')
15280   !
15990 fnend
16000 def fn_uniqueComputerId_initialize
16020   if env$('Unique_Computer_ID')='' then 
16040     dim uci_tmp_filename$*512
16042     dim tmp_line$*128
16044     dim uuid$*36
16046     dim hdd_serial$*36
16060     uci_tmp_filename$='acs_uuid_tmp'&session$&'.txt'
16080     hdd_serial$=''
16100     uuid$=''
16110     ! OLD failed in some places (like local on acs online server)  execute 'sy -m wmic csproduct get UUID |more >"%temp%\'&uci_tmp_filename$&'"'
16120     execute 'sy -m wmic csproduct get UUID |more >"'&env$('client_temp')&'\'&uci_tmp_filename$&'"'
16140     open #h_tmp:=fn_gethandle: 'name=@:'&env$('client_temp')&'\'&uci_tmp_filename$&',EoL=None',display,input ! ioerr NO_WMIC
16160     linput #h_tmp: tmp_line$
16180     tmp_line$=srep$(tmp_line$,chr$(10),'~')
16200     tmp_line$=srep$(tmp_line$,chr$(13),'~')
16220     tmp_line$=srep$(tmp_line$,' ','')
16240     do while pos(tmp_line$,'~~')>0
16260       tmp_line$=srep$(tmp_line$,'~~','~')
16280     loop 
16300     if tmp_line$(1:4)<>'UUID' then 
16320      ! windows server 2003
16340       tmp_pos_uuid=pos(tmp_line$,'UUID~')
16360       if tmp_pos_uuid>0 then 
16380         tmp_line$(1:tmp_pos_uuid)=''
16400       else 
16420         pr 'problem in fn_uniqueComputerId_initialize$ - expected to say UUID' : pause 
16440       end if 
16460     end if 
16480 !   if tmp_line$(1:4)<>'UUID' then pr 'problem in fn_uniqueComputerId_initialize$ - expected to say UUID' : pause
16500     uuid$=tmp_line$(6:len(tmp_line$)-1)
16520     close #h_tmp,free: 
16540     if uuid$='FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF' then 
16560       uuid_valid=0
16580       execute 'sy -m wmic  DISKDRIVE get SerialNumber |more >'&uci_tmp_filename$ ioerr NO_WMIC
16600 !  execute 'sy -m wmic /output:"'&uci_tmp_filename$&'" DISKDRIVE get SerialNumber'   <--encoded in something other than ANSI, hard to read
16620       open #h_tmp:=fn_gethandle: 'name='&uci_tmp_filename$&',EoL=None',display,input ioerr NO_WMIC
16640       linput #h_tmp: tmp_line$
16660       tmp_line$=srep$(tmp_line$,chr$(10),'~')
16680       tmp_line$=srep$(tmp_line$,chr$(13),'~')
16700       tmp_line$=srep$(tmp_line$,' ','')
16720       do while pos(tmp_line$,'~~')>0
16740         tmp_line$=srep$(tmp_line$,'~~','~')
16760       loop 
16780       if tmp_line$(1:12)<>'SerialNumber' then pr 'problem in fn_uniqueComputerId_initialize$ - expected to say SerialNumber' : pause 
16800       hdd_serial$=tmp_line$(14:len(tmp_line$)-1)
16820       close #h_tmp,free: 
16840     else 
16860       uuid_valid=1
16880     end if 
16900     if uuid_valid then 
16920       setenv('Unique_Computer_ID',uuid$)
16940     else 
16960       setenv('Unique_Computer_ID',hdd_serial$)
16980     end if 
17000   end if 
17020   goto UcaFinis
17040   NO_WMIC: ! r: ! windows XP does not support WMIC
17060     setenv('Unique_Computer_ID',wsid$)
17080   goto UcaFinis ! /r
17100   UcaFinis: ! 
17120 fnend 
18000 def fn_AcsUserId_Initialize
18020   ! this function returns nothing but is used to initiate env$('acsUserId')
18040   ! env$('acsUserId') is a replacement for wsid$ which is a filename safe and derived from env$('Unique_Computer_ID')
18060   if env$('acsUserId')='' then
18080     ! if env$('Unique_Computer_ID')='' then let fn_uniqueComputerId_initialize
18100     fnreg_read('ACS UserID:'&env$('Unique_Computer_ID'),acs_userid$)
18120     if acs_userid$='' then 
18140       fnreg_read('ACS UserID Number Last Assigned',acs_userid$)
18160       acs_userid$=str$(val(acs_userid$)+1)
18180       fnreg_write('ACS UserID Number Last Assigned',acs_userid$)
18190       fnreg_write('ACS UserID:'&env$('Unique_Computer_ID'),acs_userid$)
18200     end if
18220     setenv('acsUserId','u'&acs_userid$)
18240     execute 'config substitute [acsUserId] u'&acs_userid$
18260   end if
18280 fnend
19000 XIT: execute "System"
19100 IGNORE: continue 
19200 ! <Updateable Region: ERTN>
19220 ERTN: fnerror(program$,err,line,act$,"xit")
19240   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
19260   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
19280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
19300 ERTN_EXEC_ACT: execute act$ : goto ERTN
19320 ! /region
22000 def fn_setup
22020   if ~setup then 
22040     setup=1
22060     option retain 
22100     library 'S:\Core\Library': fnerror,fnchain,fncheckcompiled,fnapply_theme
22110     library 'S:\Core\Library': fnCopy,fnshortpath$,fnureg_read
22120     library 'S:\Core\Library': fnreg_read,fnreg_write
22140     library 'S:\Core\Library': fnAcsInstallationPath$,fnMakeSurepathExists,fnclient$
22160   end if 
22180 fnend 
24000 def library fnSpoolPath$*256(; initialize)
24020   if ~setup then let fn_setup
24040   fnSpoolPath$=fn_spoolPath$( initialize)
24060 fnend
24080 def fn_spoolPath$*256(; initialize)
24100   if initialize then
24120     fnmakesurepathexists(env$('temp')&'\acs\Spool\')
24160     execute 'config spoolpath '&env$('temp')&'\acs\Spool' 
24220   end if
24240   fn_spoolPath$=env$('temp')&'\acs\Spool'
24260 fnend
27000 def library fnrights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048)
27010   if ~setup then let fn_setup
27020   fnrights_test=fn_rights_test(rt_folder$,rt_how_to_fix$,folder_name$, additional_text_for_failure$)
27040 fnend 
29000 def fn_rights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048)
29020   rt_return=1 ! returns 1 if passed test or 0 if failed.
29040   rt_folder$=trim$(rt_folder$)
29060   if rt_folder$<>'' and rt_folder$(len(rt_folder$):len(rt_folder$))<>'\' then rt_folder$=rt_folder$&'\'
29080   ! 
29100   open #h_test:=fn_gethandle: 'Name='&rt_folder$&'tmp_rights_test'&session$&'.dat,Replace,RecL=384',internal,outIn,relative ioerr RT_FAIL
29120   close #h_test: 
29140   execute 'free "'&rt_folder$&'tmp_rights_test'&session$&'.dat"' ioerr RT_FAIL
29160   goto RT_PASS
29180   RT_FAIL: ! 
29200   rt_return=0
29220   if err=4205 then 
29240     msgbox("Insufficient rights to access "&folder_name$&" Folder ("&os_filename$(rt_folder$)&")"&chr$(13)&rt_how_to_fix$&chr$(13)&additional_text_for_failure$)
29260     rt_return=0
29280   else if err then 
29300     msgbox("Error "&str$(err)&" in rights test - making/removing a file in "&folder_name$&" Folder ("&os_filename$(rt_folder$)&")"&chr$(13)&rt_how_to_fix$&chr$(13)&additional_text_for_failure$)
29320     if env$('ACSDeveloper')<>'' then pause
29340     rt_return=0
29360   end if 
29380   RT_PASS: ! 
29400   fn_rights_test=rt_return
29420 fnend 
34000 def fn_move_data(file_name$*256,destination_name$*256; ignore_exists)
34020   md_return=0
34040   if ignore_exists or (exists(file_name$) and ~exists(destination_name$)) then 
34060     execute 'Copy "'&file_name$&'" "'&destination_name$&'"' ioerr MOVE_DATA_XIT
34080     execute 'Free "'&file_name$&'"' ioerr ignore
34100     md_return=1
34120   end if 
34130   MOVE_DATA_XIT: ! 
34140   fn_move_data=md_return
34160 fnend 
34180 def fn_move_core_data(file_name$*256; ignore_exists)
34200   fn_move_core_data=fn_move_data('S:\Core\Data\'&file_name$,env$('Q')&"\Data\"&file_name$, ignore_exists)
34220 fnend 
36000 def library fnMapToVirturalDrive(path_to_map$*256,drive_id$*2)
36020   fnMapToVirturalDrive=fn_map_to_virtural_drive(path_to_map$,drive_id$)
36040 fnend
36060 def fn_map_to_virtural_drive(path_to_map$*256,drive_id$*2)
36080   execute 'config drive '&drive_id$(1:1)&','&rtrm$(path_to_map$,'\')&',X,\' ioerr ignore
36100 fnend 
38000 def fn_temp_dir_validate
38020   tdt_return=1
38040   if lwrc$(env$('temp'))='c:\windows\temp' then 
38060     if ~exists(env$('USERPROFILE')) then 
38080       msgbox('Security Error: User Profile directory ('&env$('USERPROFILE')&') does not exist.')
38100       tdt_return=0
38140     else 
38160       tdt_return=fn_change_temp
38180     end if 
38200   end if 
38280   fn_temp_dir_validate=tdt_return
38300 fnend 
40000 def fn_change_temp
40020   ct_return=1
40040   if ~exists(env$('USERPROFILE')&'\AppData') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData"'
40060   if ~exists(env$('USERPROFILE')&'\AppData\Local') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData\Local"'
40080   if ~exists(env$('USERPROFILE')&'\AppData\Local\Temp') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData\Local\Temp"'
40100   if ~exists(env$('USERPROFILE')&'\AppData\Local\Temp') then 
40120     msgbox('Startup Error: Tried to create a new Temp directory ('&env$('USERPROFILE')&'\AppData\Local\Temp) but failed.')
40140     ct_return=0
40160   else 
40180     setenv('Tmp',env$('USERPROFILE')&'\AppData\Local\Temp')
40200     setenv('Temp',env$('USERPROFILE')&'\AppData\Local\Temp')
40220   end if 
40240   fn_change_temp=ct_return
40260 fnend 
42000 ! def fn_udf_resolve ! r: migration tool no longer used
42010 !   dim udf$*1024
42020 !   fn_get_udf(udf$)
42060 !   if udf$<>'' and exists(udf$)=1 then ! then it is a directory that exists
42080 !     tmp_dir_count=1
42100 !     tmp_dir$(1)=rtrm$(udf$,'\')
42120 !     dim filename$(1)*1024,tmp_dir$(1)*1024
42160 !     execute 'sy -m del "'&os_filename$(udf$)&'\*.scr"'
42180 !     execute 'sy -m del "'&os_filename$(udf$)&'\*.tmp"'
42200 !     execute 'sy -m xcopy "'&os_filename$(udf$)&'" "'&env$('Q')&'\'&'" /S /T'
42220 !     fngetdir2(udf$,mat filename$, '/s /b','*.*') ! fngetdir2(udf$&'ini',mat filename$, '/s /b','*.*')
42240 !     for f_i=1 to udim(mat filename$)
42260 !       if exists(filename$(f_i))=1 then ! it is a directory
42280 !         tmp_dir_count+=1
42300 !         mat tmp_dir$(tmp_dir_count)
42320 !         tmp_dir$(tmp_dir_count)=filename$(f_i)
42340 !       else ! it is a file.
42360 !         dim tmp_to$*1024,tmp_from$*1024
42380 !         tmp_to$=env$('Q')&'\'&filename$(f_i)(len(udf$)+1:len(filename$(f_i)))
42400 !         tmp_from$=lwrc$(filename$(f_i))
42440 !         if pos(tmp_from$,lwrc$('Reads_and_Chgs'))>0 then 
42460 !           fnCopy(tmp_from$,env$('Q')&'\UBmstr\*.*')
42480 !           execute 'free "'&tmp_from$&'"' ioerr ignore
42500 !         else if ~exists(tmp_to$) then 
42520 !           if fnCopy(tmp_from$,tmp_to$) then 
42540 !             execute 'free "'&tmp_from$&'"' ioerr ignore
42560 !           end if 
42580 !         else if exists(tmp_to$) then 
42600 !           execute 'free "'&tmp_from$&'"' ioerr ignore
42620 !         end if 
42640 !       end if 
42660 !     next f_i
42680 !     for d_i=tmp_dir_count to 1, step -1
42700 !       execute 'rmdir '&tmp_dir$(d_i) ioerr ignore
42720 !     next d_i
42740 !   end if 
42760 ! fnend /r
43000 ! def fn_get_udf(&udf$) r:
43040 !   dim oldudf$*256
43140 !   if oldudf$<>"" then 
43160 !     udf$=oldudf$
43220 !   else if env$("ScreenAceTemp")="" then !    NEW - just return blank - we do not need to make anything
43340 !     udf$=oldudf$="" ! app_data$&"\ACS\Temp\"
43420 !   else 
43440 !     oldudf$=udf$=fnshortpath$(env$("ScreenAceTemp"))&'\'
43460 !   end if 
43480 !   udf$(3:len(udf$))=srep$(udf$(3:len(udf$)),'\\','\')
43500 ! fnend /r
44000 def fn_env_data_default
44020   if env$('data')='' then ! if env$('data') is blank than set it here.
44040     dim edd_base$*256
44060     if env$('ProgramData')='' then 
44100       edd_base$=fnshortpath$(env$('appdata'))
44120     else 
44140       edd_base$=env$('ProgramData')
44160     end if 
44180     setenv('data',edd_base$&'\ACS\')
44240     fnmakesurepathexists(env$('data')&'\Data\')
44280     if env$('data')(len(env$('data')):len(env$('data')))<>'\' then ! if env$('data') does not end with a backslash nor forward slash than add one.
44440       setenv('data',env$('data')&'\')
44460     end if 
44900   end if
44920 fnend 
45000 def library fnSetQ(setQ$*256)
45020   if ~setup then let fn_setup
45040   fnSetQ=fn_setQ(setQ$)
45060 fnend
45080 def fn_setQ(setQ$*256)
45100   setQ$=rtrm$(setQ$,'\')
45110   if pos(setQ$,' ')>0 then setQ$=fnshortpath$(setQ$)
45120   setenv('Q',setQ$)
45140   execute 'config substitute [Q] "'&env$('Q')&'"'
45160   if env$('acsDeveloper')='' then 
45180     execute 'config substitute [ScreenIO_ScreenFldDrive] '&env$('Q')
45200   end if
45220   fnmakesurepathexists(env$('Q')&'\Data\')
45240   fnmakesurepathexists(env$('Q')&'\'&env$('CurSys')&'mstr\')
45260   ! if env$('acsDebug')<>'' then
45280   !   pr 'SetQ to '&env$('Q')
45300   !   pause
45320   ! end if
45340 fnend
46000 def fn_setQBase(newQBase$*256)
46020   if env$('QBase')='' then
46030     newQBase$=rtrm$(newQBase$,'\')
46032     if pos(newQBase$,' ')>0 then newQBase$=fnshortpath$(newQBase$)
46040     setenv('QBase',newQBase$)
46060     exe 'config substitute [QBase] '&env$('QBase') 
46300   end if
46320 fnend
47000 def fn_update_needed(acs_version_prior$,acs_version_running$)
47080   un_return=0
47100   if acs_version_running$<acs_version_prior$ and lwrc$(env$('acsIgnoreDataVersion'))<>'yes' then 
47120     un_return=1
47140     msgbox("The ACS Software version ("&acs_version_running$&") of this workstation is less than the last version ("&acs_version_prior$&") used to access ACS Data."&chr$(13)&"You must update this workstation to continue.")
47200   end if 
47220   fn_update_needed=un_return
47240 fnend 
48000 def fn_last_version_used$(; setit$*256)
48020   dim lvu_line$*256
48040   if setit$<>'' then 
48060     lvu_line$=trim$(setit$)
48080     fnreg_write('ACS last version used',setit$)
48100   else 
48120     fnreg_read('ACS last version used',lvu_line$)
48140     if lvu_line$='' then 
48160       open #hAcsVersion:=fn_gethandle: 'Name='&env$('Q')&'\Data\ACS_Version.txt,RecL=256',display,input ioerr LVU_OLD_FILE_OPEN_IOERR
48180       linput #hAcsVersion: lvu_line$
48200       close #hAcsVersion,free: 
48220       fnreg_write('ACS last version used',lvu_line$)
48240     end if 
48260   end if 
48280   LVU_OLD_FILE_OPEN_IOERR: ! 
48300   fn_last_version_used$=trim$(lvu_line$)
48320 fnend 
50000 def fn_multisession_test
50020   fnureg_read('Disable_MultiSession',disable_multisession$)
50040   if val(session$(len(session$):len(session$)))=>2 and disable_multisession$='True' then 
50060     msgbox('Multiple sessions have been disabled in user preferences.')
50080     mt_return=0
50100   else 
50120     mt_return=1
50140   end if 
50160   fn_multisession_test=mt_return
50180 fnend 
52000 def fn_update_version_for_inno
52040   open #h_tmp:=fn_gethandle: 'name=:C:\ACS\Setup\ACS 5 - AppVersion.iss,RecL=256,Replace',display,output 
52060   pr #h_tmp: ';This file is dynamically built by '&os_filename$(program$)&' when run by an ACSDeveloper.'
52080   pr #h_tmp: ';Attempts to edit it directly are moot and will be quickly overwritten.'
52100   pr #h_tmp: 'AppVersion='&env$('acsVersion')
52120   close #h_tmp: 
52140 fnend 
54000 def fncs_env
54020   dim ce_line$*2048
54040   dim ce_prefix$
54060   ce_prefix$="Client_"
54080   dim ce_field$*2048
54100   dim ce_value$*2048
54110   dim ce_os_temp_file$*1048
54120   dim ce_br_temp_file$*1048
54130   ! if env$('data')='/br/orders/brc_oe/Data' then        !  Gordon's Linux CS Server
54140   !   setenv('data','\\JAZZ\BR Order Entry\brc_oe\Data')  !  Gordon's Linux CS Server
54150   ! end if                                                !  Gordon's Linux CS Server
54160   ce_os_temp_file$=rtrm$(env$('data'),'\')&'\cs-'&session$&'.txt'
54180   ce_br_temp_file$=env$('Q')&'\cs-'&session$&'.txt'
54182   ce_retry_4152_count=0
54190   CE_MAKE_TEMP_FILE: !
54192   fnmakesurepathexists(ce_br_temp_file$)
54200   execute '*sys -M set > "'&ce_os_temp_file$&'"'
54220   open #hOsSet:=fn_gethandle: "Name="&ce_br_temp_file$,display,input error CE_DEBUG_OPEN_ERR ! error XIT_FNCS_ENV
54240   do
54260     linput #hOsSet: ce_line$ error XIT_LOOP
54280     gw_wholeline=len(rtrm$(ce_line$)) 
54300     gw_addlen=1 
54320     gw_posfnwp=pos(uprc$(ce_line$),"=")
54340     if gw_posfnwp>0 then
54360       gw_equal =pos(ce_line$,'=')
54380       gw_nextequal =pos(ce_line$,'=',gw_posfnwp+gw_addlen)
54400       if gw_equal > 0 then
54420         ce_field$ = ce_prefix$&ce_line$(1:gw_posfnwp-1)
54440         ce_value$ = ce_line$(gw_posfnwp+1:gw_wholeline)
54460         setenv(ce_field$,ce_value$) error ignore
54480 !       pr 'setenv("'&ce_field$&'","'&ce_value$&'")'
54500 ! Should SETENV FAIL, Ignore it
54520       end if
54540     end if
54560   loop
56000   CE_DEBUG_OPEN_ERR: ! 
56020   if err=4152 or err=4203 then
56040     if (ce_retry_4152_count+=1)<=3 then 
56060       goto CE_MAKE_TEMP_FILE
56080     else if (ce_retry_4152_count+=1)<=6 then 
56100       if ce_br_temp_file$(1:3)<>'@::' then
56120         ce_br_temp_file$(0:0)='@::'
56140       end if
56160       goto CE_MAKE_TEMP_FILE
56180     else if (ce_retry_4152_count+=1)<=9 then 
56200       fnmakesurepathexists('@::C:\ProgramData\ACS\Temp\Session'&session$&'\')
56240       ce_os_temp_file$='C:\ProgramData\ACS\Temp\Session'&session$&'\cs.txt'
56250       ce_br_temp_file$='@::'&ce_os_temp_file$
56260       goto CE_MAKE_TEMP_FILE
56280     end if
56300   end if
56320   exec 'con gui off'
56340   pr 'error '&str$(err)&' on open of ce_br_temp_file$'
56360   pr '     ce_os_temp_file$='&ce_os_temp_file$
56380   pr '     ce_br_temp_file$='&ce_br_temp_file$
56400   pr '          exists=';exists(ce_br_temp_file$)
56420   pr '    Press ENTER to Exit' : kstat$(1)
56440   goto XIT
58000   XIT_LOOP: ! End of Startloop
58020     close #hOsSet,free: error ignore
58040   !
58060   ! XIT_FNCS_ENV: !
58080   execute "*sy -M CD > "&ce_os_temp_file$
58100   open #hOsCd:=fn_gethandle: "Name="&ce_br_temp_file$,display,input error XIT_FNCS_OS_PATH
58120   linput #hOsCd: client_os_path$ error ignore
58140   close #hOsCd,free: error ignore
58160   setenv('client_os_path',client_os_path$)
58180   XIT_FNCS_OS_PATH: ! 
58200 fnend 
60000 def fn_show_release_notes(version_prior$,version_current$)
60020   dim srnLine$*1024,srnItem$(0)*1024
60040   open #hSrnOut:=fn_gethandle: 'name=ACS_tmp_Release_Note_Report.txt,recl=1024,replace',d,o
60060   open #hReleaseNotes:=fn_gethandle: 'name=S:\Core\Release_Notes.txt',d,i
60080   pr #hSrnOut: 'You just updated from '&version_prior$&' to '&version_current$&'.'
60100   pr #hSrnOut: ''
60120   do
60140   linput #hReleaseNotes: srnLine$ eof srnReleaseNotesEof
60160     str2mat(srnLine$,mat srnItem$,chr$(9))
60180     if udim(mat srnItem$)=>3 then
60200       srnItem3Value=val(srnItem$(3)) conv ignore
60220     end if
60240     if udim(mat srnItem$)=1 then
60260       pr #hSrnOut: chr$(9)&chr$(9)&srnLine$
60280     else
60300       pr #hSrnOut: srnLine$
60320     end if
60340   loop until srnItem3Value<>0 and (udim(srnItem$)=>3 and srnItem$(3)<=version_prior$)
60360   srnReleaseNotesEof: !
60380   close #hSrnOut:
60400   exec 'sy -c -m'&os_filename$('ACS_tmp_Release_Note_Report.txt')&'"'
60420 fnend
62000 def library fngethandle
62020   fngethandle=fn_gethandle
62040 fnend
62060 def fn_gethandle
62080   hMaybe=189
62100   ghReturn=0
62120   do 
62140     if file(hMaybe)<0 and file$(hMaybe)='' then 
62160       ghReturn=hMaybe
62180       goto gethandleFINIS
62200     end if 
62220     hMaybe-=1
62240   loop until hMaybe=-1
62260   pr 'fn_gethandle found no available file handles, so it is returning -1' : pause
62280   gethandleFINIS: ! 
62300   fn_gethandle=ghReturn
62320 fnend 
64000 def fn_FreeVirtualStore ! does not seem to work.
64020   dim acsInstallationPath$*256
64040   acsInstallationPath$=fnAcsInstallationPath$(1)
64060   if acsInstallationPath$(2:2)=':' then ! it is not a unc
64080     if exists(env$('LocalAppData')&'\VirtualStore'&acsInstallationPath$(3:len(acsInstallationPath$))) then
64100       ! pr 'sy Del "'&env$('LocalAppData')&'\VirtualStore'&acsInstallationPath$(3:len(acsInstallationPath$))&'\*.*" /s /y' : pause
64110       exe 'sy Del "'&env$('LocalAppData')&'\VirtualStore'&acsInstallationPath$(3:len(acsInstallationPath$))&'\*.*" /s /y'
64112 ! this DEL to the VIRTUALSTORE does not seem to work....  perhaps if it were put inside a batch file???
64120     end if
64140   end if
64160 fnend
65000 def fn_UpdateQFileIO
65020   if env$('acsDeveloper')='' then ! because I used symbolic link
65040     fnMakeSurepathExists(env$('QBase')&'\Core\FileIO\Layout\')
65060     fnMakeSurepathExists(env$('QBase')&'\Core\FileIO\Layout\version\')
65080     fnCopy('S:\Core\FileIO\Layout\*.*'        ,env$('QBase')&'\Core\FileIO\Layout\*.*'        )
65100     fnCopy('S:\Core\FileIO\Layout\version\*.*',env$('QBase')&'\Core\FileIO\Layout\version\*.*')
65120   end if
65140 fnend
66000 def fn_UpdateQScreenIO
66020   if env$('acsDeveloper')='' then ! because I used: mklink /J "C:\Users\John\OneDrive\ACS\Dev-5 Data\Core\ScreenIO\Screen" "C:\ACS\Dev-5\Core\ScreenIO\Screen"
66040     fnMakeSurepathExists(env$('QBase')&'\Core\ScreenIO\screen\')
66060     fnCopy('S:\Core\ScreenIO\screen\*.*'      ,env$('QBase')&'\Core\ScreenIO\screen\*.*'      )
66080   end if
66100 fnend
66500 RetryAFewTimes: ! r:
66520   retryAfewTimesCount+=1
66540   if retryAfewTimesCount=10 then 
66560     pr 'error '&str$(err)&' on line '&str$(line)&'.  Retried '&str$(retryAfewTimesCount)&' times.'
66580     pause
66600     retryAfewTimesCount=0
66620   end if
66640   sleep(.4)
66660 retry ! /r
67000 def fn_CopySfileIoIniToFileIoIni
67010   ! note that destination fileio.ini must be all lowercase as it is case sensitive on some systems
67020   if env$('acsDeveloper')='' then
67040     fnCopy('S:\FileIO.ini','fileio.ini')
67060   else
67080     open #hIn_fileIoIni:=fn_gethandle: 'name=S:\fileio.ini',d,i ioerr RetryAFewTimes
67100     open #hOut_FileIoIni:=fn_gethandle: 'name=FileIO.ini,recl=256,replace',d,o ioerr RetryAFewTimes
67120     dim line$*256
67140     do
67160       linput #hIn_fileIoIni: line$  eof Csf2f_Eof
67180       posForDev= pos(lwrc$(line$),'<for developer>')
67200       if posForDev>0 then
67220         posForDev+=15
67240         posEndDev=pos(lwrc$(line$),'</for developer>')-1
67260         pr #hOut_FileIoIni: line$(posForDev:posEndDev)
67280         ! pr line$(posForDev:posEndDev)
67300         ! pause
67320       else
67340         pr #hOut_FileIoIni: line$
67360         ! pr line$
67380       end if
67400     loop
67420     Csf2f_Eof: !
67440   end if
67460 fnend
68000 def fn_startStatus(text$*128)
68020   pr f str$(startStatusLine+=1)&',1,C': text$
68040 fnend
70000 def fn_acsVersion$
70020   if ~setup then let fn_setup
70040   open #hBuild:=fn_gethandle: 'name=S:\Core\Build.txt',d,i
70060   linput #hBuild: build$
70080   close #hBuild:
70090    setenv('acsVersion','5.'&rtrm$(build$))
70100   fn_acsVersion$=env$('acsVersion')
70120 fnend 
