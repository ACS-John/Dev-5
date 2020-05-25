fn_acsSystemInitialize
fn_setup
fnChain('S:\Core\Menu.br', 0,1)
def library fnAcsSystemInitialize(; syInitMode)
	 if ~setup then fn_setup
	 fnAcsSystemInitialize=fn_acsSystemInitialize( syInitMode)
fnend
def fn_acsSystemInitialize(; syInitMode)
	! syInitMode=1 =  Screen IO Test
	! syInitMode=2 =  Collection-Master Add-On
	if ~syInitMode or env$('acsVersion')='' then
		startStatusLine=0 : pr newpage
		if syInitMode=2 then
			fn_startStatus("Loading Collection-Master Add-On core components..." )
		else
			fn_startStatus("Loading ACS System..." )
			if env$('ACSDeveloper')='' and login_name$<>'niceguywinning@gmail.com' then execute "config statusLine off"
			! r: set environment variables based on login_name$ and/or BR_MODEL
			! if env$('ACSDeveloper')<>'' then setenv('disableAutomatedSavePoints','Yes') else setenv('disableAutomatedSavePoints','')
			if env$('ACSDeveloper')<>'' or login_name$='acsbowman' or login_name$='niceguywinning@gmail.com' then
				setenv('enableClientSelection','Yes')
			end if
			if env$('BR_MODEL')='CLIENT/SERVER' then !
				if login_name$='niceguywinning@gmail.com' then
					setenv('enableDataFolderByClient','Yes')
					pr 'enableDataFolderByClient='&env$('enableDataFolderByClient')
				end if
			end if
			! /r
		end if
		execute 'Config FieldBreak Min_Spaces 3, UnderScore Off'
		if ~setup then fn_setup
		fnClient$ ! this needs to be called to set client environment variables (before fn_env_data_default)

		if syInitMode=2 then
			fn_env_data_default(1)
			setenv('data',os_filename$('//6')&'\ACS')
			! setenv('data',os_filename$(env$('Status.Files.Drives.[I]')&'ACS')
			fnMakeSurepathExists(env$('data')&'\')
			! pause
		else
			fn_env_data_default(0)
		end if

		!
		if env$('Q')='' then
			if env$('CsServerData')<>'' and env$('BR_MODEL')='CLIENT/SERVER' and env$('enableDataFolderByClient')='Yes' then
				if pos(env$('CsServerData'),'/')>0 then slash$='/' else slash$='\'
				fn_setQ(rtrm$(env$('CsServerData'),slash$)&slash$&env$('client')) ! fn_map_to_virtural_drive(env$('CsServerData'),'Q:')
				fn_setQBase(env$('CsServerData'))
			else if env$('CsServerData')<>'' and env$('BR_MODEL')='CLIENT/SERVER' then
				fn_setQ(env$('CsServerData')) ! fn_map_to_virtural_drive(env$('CsServerData'),'Q:')
				fn_setQBase(env$('CsServerData'))
			else if env$('enableDataFolderByClient')='Yes' then
				if pos(env$('data'),'/')>0 then slash$='/' else slash$='\'
				fn_setQ(rtrm$(env$('data'),slash$)&slash$&env$('client')) ! fn_map_to_virtural_drive(env$('data')&clientDataFolderSuffix$,'Q:')
				fn_setQBase(env$('data'))
			else
				fn_setQ(env$('data')) ! fn_map_to_virtural_drive(env$('data')&clientDataFolderSuffix$,'Q:')
				fn_setQBase(env$('data'))
			end if
		end if
		if env$('BR_MODEL')='CLIENT/SERVER' then
			execute 'config shell default client'
			setenv('at','@::')  ! second colon added 01/18/2018 - to fix client server making files with UNC paths - i.e.  Create Hand Held Files
			fn_startStatus('Collecting local environment variables...')
			fn_csEnv
			if env$('client_acsDeveloper')<>'' then setenv('acsDeveloper',env$('client_acsDeveloper'))
			setenv('local_program_dir','@:'&env$("CLIENT_BR")(1:pos(env$("CLIENT_BR"),'\',-1)-1))
			setenv('userprofile','@::'&env$('client_userprofile'))
		else
			setenv('local_program_dir',os_filename$('S:'))
			setenv('at','')
		end if
		!
		if env$('acsDeveloper')<>'' then
			execute 'config substitute [ScreenIO_ScreenFldDrive] S:'
		end if
		!
		execute "load S:\Core\Menu.br,Resident" error ignore ! hopefully will decrease the amount of time it takes to load the menu between programs
		execute "load S:\Core\Library.br,Resident" error ignore
		!  fails on windows XP  !  execute "load S:\Core\Start.br,Resident"
		execute "load S:\Core\Index.br,Resident"
		execute "load S:\Core\ACS_Component.br,Resident"
		execute "load S:\Core\fn\windowsStart.br,Resident"
		execute 'load "S:\Core\FileIO\fileio.br",Resident'
		!  maybe but not yet ...     execute "load S:\Core\Client.br,resident"
		! fn_setup  <-- already called
		if env$('acsEnableComplier')='Yes' and env$('BR_MODEL')<>'CLIENT/SERVER' and ~syInitMode then fncheckcompiled ! sets the current directory to "S:" if it is not already
		if env$('acsEnableComplier')='Yes' and env$('BR_MODEL')<>'CLIENT/SERVER' then fn_update_version_for_inno
		if env$('BR_MODEL')='CLIENT/SERVER' then
			! execute 'config editor'   !  editor setting removed from brconfig.sys - not necessary
			if env$('programdata')='' and env$('CsServerTemp')<>'' then
				setenv('programdata',env$('CsServerTemp'))
			end if
			! setenv('Temp','C:\ACS_Data\Temp\Session'&session$)
			dim tmpFolder$*256
			tmpFolder$='C:\ACS_Data\Temp\'&srep$(srep$(srep$(login_name$,' ','_'),',',''),'.','')&'-Session'&session$
			setenv('Temp',tmpFolder$)
			fnmakesurepathexists(env$('Temp')&'\')
		end if
		if ~fn_temp_dir_validate then goto Xit ! if env$('BR_MODEL')<>'CLIENT/SERVER' and ~fn_temp_dir_validate then goto Xit
		if pos(env$('Q'),' ')>0 then
			fn_setQ(fnshortpath$(env$('Q')))
		end if
		if pos(env$('Qbase'),' ')>0 then
			dim New2Qbase$*256
			New2Qbase$=fnshortpath$(env$('Qbase'))
			setenv('QBase','')
			fn_setQbase(New2Qbase$)
		end if
		if env$('client_temp')='' then setenv('Client_TEMP',env$('Temp'))
		if ~fn_rights_test(env$('Q'),"Try Run As Administrator.",'Data') then goto Xit
		if ~fn_rights_test(env$('temp'),'Correct your Temp environment variable.','Temp') then goto Xit ! to %USERPROFILE%\AppData\Local\Temp
		fn_spoolPath$(1)
		! r: set to last client selected (if appropriate)
			if env$('enableClientSelection')='Yes' and env$('clientSelected')='' then
				library 'S:\Core\Library': fnSetClient,fnmcreg_read
				dim tmpClientSelected$*128
				fnmcreg_read('clientSelected',tmpClientSelected$)
				fnSetClient(tmpClientSelected$)
			end if
		! /r
		if syInitMode<>2 then
			if env$('acsProduct')='ACS Online' then
				setenv("Icon","S:\Core\Icon\ACS Client 32x32-32bit.ico") ! commented out because it made the icon look funny - filled with white and so long as i change the icon on the brclient executable than I'll shouldn't need to re-set it anyway.
			else
				! this isn't even necessary because the dll and exe have updated icons.
				setenv("Icon","S:\Core\Icon\ACS BR.ico") ! setenv("Icon","S:\Core\Icon\ACS-v5-32b.ico") ! "S:\Core\Icon\ACS-v5-32x32-32bit.ico"
			end if
			fnMakeSurepathExists("[Q]\Data\")
			fnMakeSurepathExists('[Q]\Report Cache\')
			if fn_move_core_data('CityStZip.dat') then fn_move_core_data('CityStZip.idx',1)
			if fn_move_core_data('1099Box.dat') then fn_move_core_data('1099Box.idx',1)
			! fn_udf_resolve
			! if exists(udf$&"Reads_and_Chgs.h1") then
			!   fn_move_data(udf$&"Reads_and_Chgs.h*","[Q]\UBmstr\Reads_and_Chgs.h*",1)
			!   fn_move_data(udf$&"Reads_and_Chgs-Key.h*","[Q]\UBmstr\Reads_and_Chgs-Key.h*",1)
			!  end if
			dim workingDir$*512
			if env$('br_model')='CLIENT/SERVER' then
				workingDir$=env$('temp')
			else
				workingDir$=env$('LocalAppData')&'\ACS'
			end if
			if workingDir$(2:2)=':' then
				fnmakesurepathexists(workingDir$&'\')
				execute 'CD '&workingDir$(1:2)
				execute 'CD "'&workingDir$(3:len(workingDir$))&'"'
				if exists('S:\ScreenIO.ini') then
					fnCopy('S:\ScreenIO.ini','screenio.ini')   ! note that destination screenio.ini must be all lowercase as it is case sensitive on some systems
				end if
				if exists('S:\sio.lic') and ~exists('sio.lic') then
					fnCopy('S:\sio.lic','sio.lic')
				end if
				fn_CopySfileIoIniToFileIoIni
			else
				pr 'Problem: workingDir is "'&workingDir$&'"'
				pr '         a : was expected as the second character but was not found.'
				pr '         Contact ACS Techincal Support'
				pause
			end if
		end if
		if ~exists('r') and env$('cursys')<>'CM' then
			fn_writeProc('r','stop'             )
			fn_writeProc('' ,'clear resi'       )
			fn_writeProc('' ,'run '&program$    )
		end if
		!
		! fn_writeProc('relive','stop'                              )
		! fn_writeProc(''      ,'execute ''load "''&program$&''"''' )
		! fn_writeProc(''      ,'run '                              )
		!
		if env$('ACSDeveloper')<>'' then
			if ~exists('reload') then
				fn_writeProc('reload','end')
				fn_writeProc(''      ,'execute ''load "''&program$&''"''')
				fn_writeProc('out',"exec 'sy "&os_filename$('S:\brEdit.cmd')&' "''&os_filename$(program$)&''"''')
			end if
			if ~exists('ed') then
				fn_writeProc('ed' ,"exec 'sy "&os_filename$('S:\brEdit.cmd')&' "''&os_filename$(program$)&''"''')
			end if
			if ~exists('in') then
				fn_writeProc('in' ,'end')
				fn_writeProc(''   ,"setenv('source',program$&program$(pos(program$,'.',-1):inf)&'s')")
				fn_writeProc(''   ,"setenv('source',os_filename$(env$('source')))")
				fn_writeProc(''   ,"exec 'sy """"C:\ACS\Dev-5\Sad Panda\Compile.cmd"" ""'&env$('source')&'""""'")
				fn_writeProc(''   ,'execute ''load "''&program$&''"''')
			end if
		end if
		setenv("PD",'S:\') ! for modified fnsnap compatibility (Core\fnsnap)
		! if syInitMode then disableConScreenOpenDflt=1 else disableConScreenOpenDflt=0
		fn_startStatus('Identifying your system...')
		fn_uniqueComputerId_initialize ! called to initialize env$('unique_computer_id')
		if env$('unique_computer_id')='42601D50-D3A4-81E4-29A3-605718581E48' then ! little koi
			setenv('enableClientSelection','Yes')
		end if
		fn_AcsUserId_Initialize ! called to initialize env$('acsUserId')
		fnapply_theme ! ( disableConScreenOpenDflt)
		if ~fn_multisession_test then goto Xit
		! setenv('path_to_7z_exe','"'&os_filename$(env$('local_program_dir')&'\Core\Programs\7zip-'&env$('client_platform.os_bits')&'bit\7z.exe')&'"')
		setenv('path_to_7z_exe','"'&os_filename$('S:\Core\Programs\7zip-'&env$('server_platform.os_bits')&'bit\7z.exe')&'"')
		version_prior$=fn_last_version_used$
		version_current$=fn_acsVersion$
		setenv('acsVersion',version_current$)
		if fn_update_needed(version_prior$,version_current$) then
			fnclient$ ! this needs to be called to set client environment variables
			fnchain('S:\Core\Programs\Update.br')
		end if
		library 'S:\Core\Library': fnSpecialFolderPath$
		setenv('Desktop',fnSpecialFolderPath$('Desktop'))
		! pr 'point XXX1' : pause
		if version_current$>version_prior$ or env$('ForceScreenIOUpdate')<>'' then
			if  env$('cursys')<>'CM' then
				fn_show_release_notes(version_prior$,version_current$)
				fn_FreeVirtualStore
			end if
			fn_UpdateQFileIO
			fn_UpdateQScreenIO
			fn_last_version_used$(version_current$)
		else if  env$('cursys')='CM' then
			fn_UpdateQFileIO
		end if
	end if
	!
	! fn_uniqueComputerId_initialize ! called to initialize env$('unique_computer_id')
	! fn_AcsUserId_Initialize ! called to initialize env$('acsUserId')
	!
fnend
def fn_uniqueComputerId_initialize
	if env$('Unique_Computer_ID')='' then
		dim uci_tmp_filename$*512
		dim tmp_line$*128
		dim uuid$*36
		dim hdd_serial$*36
		uci_tmp_filename$='acs_uuid_tmp'&session$&'.txt'
		hdd_serial$=''
		uuid$=''
		! OLD failed in some places (like local on acs online server)  execute 'sy -m wmic csproduct get UUID |more >"%temp%\'&uci_tmp_filename$&'"'
		execute 'sy -m wmic csproduct get UUID |more >"'&env$('client_temp')&'\'&uci_tmp_filename$&'"'
		open #h_tmp:=fn_gethandle: 'name=@:'&env$('client_temp')&'\'&uci_tmp_filename$&',EoL=None',display,input ! ioerr NO_WMIC
		linput #h_tmp: tmp_line$
		tmp_line$=srep$(tmp_line$,chr$(10),'~')
		tmp_line$=srep$(tmp_line$,chr$(13),'~')
		tmp_line$=srep$(tmp_line$,' ','')
		do while pos(tmp_line$,'~~')>0
			tmp_line$=srep$(tmp_line$,'~~','~')
		loop
		if tmp_line$(1:4)<>'UUID' then
		 ! windows server 2003
			tmp_pos_uuid=pos(tmp_line$,'UUID~')
			if tmp_pos_uuid>0 then
				tmp_line$(1:tmp_pos_uuid)=''
			else
				pr 'problem in fn_uniqueComputerId_initialize$ - expected to say UUID' : pause
			end if
		end if
!   if tmp_line$(1:4)<>'UUID' then pr 'problem in fn_uniqueComputerId_initialize$ - expected to say UUID' : pause
		uuid$=tmp_line$(6:len(tmp_line$)-1)
		close #h_tmp,free:
		if uuid$='FFFFFFFF-FFFF-FFFF-FFFF-FFFFFFFFFFFF' then
			uuid_valid=0
			execute 'sy -m wmic  DISKDRIVE get SerialNumber |more >'&uci_tmp_filename$ ioerr NO_WMIC
!  execute 'sy -m wmic /output:"'&uci_tmp_filename$&'" DISKDRIVE get SerialNumber'   <--encoded in something other than ANSI, hard to read
			open #h_tmp:=fn_gethandle: 'name='&uci_tmp_filename$&',EoL=None',display,input ioerr NO_WMIC
			linput #h_tmp: tmp_line$
			tmp_line$=srep$(tmp_line$,chr$(10),'~')
			tmp_line$=srep$(tmp_line$,chr$(13),'~')
			tmp_line$=srep$(tmp_line$,' ','')
			do while pos(tmp_line$,'~~')>0
				tmp_line$=srep$(tmp_line$,'~~','~')
			loop
			if tmp_line$(1:12)<>'SerialNumber' then pr 'problem in fn_uniqueComputerId_initialize$ - expected to say SerialNumber' : pause
			hdd_serial$=tmp_line$(14:len(tmp_line$)-1)
			close #h_tmp,free:
		else
			uuid_valid=1
		end if
		if uuid_valid then
			setenv('Unique_Computer_ID',uuid$)
		else
			setenv('Unique_Computer_ID',hdd_serial$)
		end if
	end if
	goto UcaFinis
	NO_WMIC: ! r: ! windows XP does not support WMIC
		setenv('Unique_Computer_ID',wsid$)
	goto UcaFinis ! /r
	UcaFinis: !
fnend
def fn_AcsUserId_Initialize
	! this function returns nothing but is used to initiate env$('acsUserId')
	! env$('acsUserId') is a replacement for wsid$ which is a filename safe and derived from env$('Unique_Computer_ID')
	if env$('acsUserId')='' then
		! if env$('Unique_Computer_ID')='' then fn_uniqueComputerId_initialize
		fnreg_read('ACS UserID:'&env$('Unique_Computer_ID'),acs_userid$)
		if acs_userid$='' then
			fnreg_read('ACS UserID Number Last Assigned',acs_userid$)
			acs_userid$=str$(val(acs_userid$)+1)
			fnreg_write('ACS UserID Number Last Assigned',acs_userid$)
			fnreg_write('ACS UserID:'&env$('Unique_Computer_ID'),acs_userid$)
		end if
		setenv('acsUserId','u'&acs_userid$)
		execute 'config substitute [acsUserId] u'&acs_userid$
	end if
fnend
Xit: execute "System"
def fn_setup
	if ~setup then
		setup=1
		option retain
		library 'S:\Core\Library': fnchain
		library 'S:\Core\Library': fncheckcompiled
		library 'S:\Core\Library': fnapply_theme
		library 'S:\Core\Library': fnCopy
		library 'S:\Core\Library': fnshortpath$
		library 'S:\Core\Library': fnureg_read
		library 'S:\Core\Library': fnreg_read,fnreg_write
		library 'S:\Core\Library': fnAcsInstallationPath$
		library 'S:\Core\Library': fnMakeSurepathExists
		library 'S:\Core\Library': fnclient$
	end if
fnend
def library fnSpoolPath$*256(; initialize)
	if ~setup then fn_setup
	fnSpoolPath$=fn_spoolPath$( initialize)
fnend
def fn_spoolPath$*256(; initialize)
	if initialize then
		fnmakesurepathexists(env$('temp')&'\acs\Spool\')
		execute 'config spoolpath '&env$('temp')&'\acs\Spool'
	end if
	fn_spoolPath$=env$('temp')&'\acs\Spool'
fnend
def library fnrights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048)
	if ~setup then fn_setup
	fnrights_test=fn_rights_test(rt_folder$,rt_how_to_fix$,folder_name$, additional_text_for_failure$)
fnend
def fn_rights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048,skipmsg)
	rt_return=1 ! returns 1 if passed test or 0 if failed.
	rt_folder$=trim$(rt_folder$)
	if rt_folder$<>'' and rt_folder$(len(rt_folder$):len(rt_folder$))<>'\' then rt_folder$=rt_folder$&'\'
	!
	open #h_test:=fn_gethandle: 'Name='&rt_folder$&'tmp_rights_test'&session$&'.dat,Replace,RecL=384',internal,outIn,relative ioerr RT_FAIL
	close #h_test:
	execute 'free "'&rt_folder$&'tmp_rights_test'&session$&'.dat"' ioerr RT_FAIL
	goto RT_PASS
	RT_FAIL: !
	rt_return=0
	if skipmsg=1 then goto RT_PASS
	if err=4205 then
		msgbox("Insufficient rights to access "&folder_name$&" Folder ("&os_filename$(rt_folder$)&")"&chr$(13)&rt_how_to_fix$&chr$(13)&additional_text_for_failure$)
		rt_return=0
	else if err then
		msgbox("Error "&str$(err)&" in rights test - making/removing a file in "&folder_name$&" Folder ("&os_filename$(rt_folder$)&")"&chr$(13)&rt_how_to_fix$&chr$(13)&additional_text_for_failure$)
		if env$('ACSDeveloper')<>'' then pause
		rt_return=0
	end if
	RT_PASS: !
	fn_rights_test=rt_return
fnend
def fn_move_data(file_name$*256,destination_name$*256; ignore_exists)
	md_return=0
	if ignore_exists or (exists(file_name$) and ~exists(destination_name$)) then
		execute 'Copy "'&file_name$&'" "'&destination_name$&'"' ioerr MOVE_DATA_XIT
		execute 'Free "'&file_name$&'"' ioerr ignore
		md_return=1
	end if
	MOVE_DATA_XIT: !
	fn_move_data=md_return
fnend
def fn_move_core_data(file_name$*256; ignore_exists)
	fn_move_core_data=fn_move_data('S:\Core\Data\'&file_name$,"[Q]\Data\"&file_name$, ignore_exists)
fnend
def library fnMapToVirturalDrive(path_to_map$*256,drive_id$*2)
	fnMapToVirturalDrive=fn_map_to_virtural_drive(path_to_map$,drive_id$)
fnend
def fn_map_to_virtural_drive(path_to_map$*256,drive_id$*2)
	execute 'config drive '&drive_id$(1:1)&','&rtrm$(path_to_map$,'\')&',X,\' ioerr ignore
fnend
def fn_temp_dir_validate
	tdt_return=1
	if lwrc$(env$('temp'))='c:\windows\temp' then
		if ~exists(env$('USERPROFILE')) then
			msgbox('Security Error: User Profile directory ('&env$('USERPROFILE')&') does not exist.')
			tdt_return=0
		else
			tdt_return=fn_change_temp
		end if
	end if
	fn_temp_dir_validate=tdt_return
fnend
def fn_change_temp
	ct_return=1
	if ~exists(env$('USERPROFILE')&'\AppData') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData"'
	if ~exists(env$('USERPROFILE')&'\AppData\Local') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData\Local"'
	if ~exists(env$('USERPROFILE')&'\AppData\Local\Temp') then execute 'sy -m mkdir "'&env$('USERPROFILE')&'\AppData\Local\Temp"'
	if ~exists(env$('USERPROFILE')&'\AppData\Local\Temp') then
		msgbox('Startup Error: Tried to create a new Temp directory ('&env$('USERPROFILE')&'\AppData\Local\Temp) but failed.')
		ct_return=0
	else
		setenv('Tmp',env$('USERPROFILE')&'\AppData\Local\Temp')
		setenv('Temp',env$('USERPROFILE')&'\AppData\Local\Temp')
	end if
	fn_change_temp=ct_return
fnend
! def fn_udf_resolve ! r: migration tool no longer used
!   dim udf$*1024
!   fn_get_udf(udf$)
!   if udf$<>'' and exists(udf$)=1 then ! then it is a directory that exists
!     tmp_dir_count=1
!     tmp_dir$(1)=rtrm$(udf$,'\')
!     dim filename$(1)*1024,tmp_dir$(1)*1024
!     execute 'sy -m del "'&os_filename$(udf$)&'\*.scr"'
!     execute 'sy -m del "'&os_filename$(udf$)&'\*.tmp"'
!     execute 'sy -m xcopy "'&os_filename$(udf$)&'" "[Q]\'&'" /S /T'
!     fngetdir2(udf$,mat filename$, '/s /b','*.*') ! fngetdir2(udf$&'ini',mat filename$, '/s /b','*.*')
!     for f_i=1 to udim(mat filename$)
!       if exists(filename$(f_i))=1 then ! it is a directory
!         tmp_dir_count+=1
!         mat tmp_dir$(tmp_dir_count)
!         tmp_dir$(tmp_dir_count)=filename$(f_i)
!       else ! it is a file.
!         dim tmp_to$*1024,tmp_from$*1024
!         tmp_to$='[Q]\'&filename$(f_i)(len(udf$)+1:len(filename$(f_i)))
!         tmp_from$=lwrc$(filename$(f_i))
!         if pos(tmp_from$,lwrc$('Reads_and_Chgs'))>0 then
!           fnCopy(tmp_from$,'[Q]\UBmstr\*.*')
!           execute 'free "'&tmp_from$&'"' ioerr ignore
!         else if ~exists(tmp_to$) then
!           if fnCopy(tmp_from$,tmp_to$) then
!             execute 'free "'&tmp_from$&'"' ioerr ignore
!           end if
!         else if exists(tmp_to$) then
!           execute 'free "'&tmp_from$&'"' ioerr ignore
!         end if
!       end if
!     next f_i
!     for d_i=tmp_dir_count to 1, step -1
!       execute 'rmdir '&tmp_dir$(d_i) ioerr ignore
!     next d_i
!   end if
! fnend /r
! def fn_get_udf(&udf$) r:
!   dim oldudf$*256
!   if oldudf$<>"" then
!     udf$=oldudf$
!   else if env$("ScreenAceTemp")="" then !    NEW - just return blank - we do not need to make anything
!     udf$=oldudf$="" ! app_data$&"\ACS\Temp\"
!   else
!     oldudf$=udf$=fnshortpath$(env$("ScreenAceTemp"))&'\'
!   end if
!   udf$(3:len(udf$))=srep$(udf$(3:len(udf$)),'\\','\')
! fnend /r
def fn_env_data_default(; colletionMasterMode)
	if colletionMasterMode then
		fnmakesurepathexists(env$('status.files.drives.[i]')&'ACS\')
		setenv('data',env$('status.files.drives.[i]')&'ACS\')
	else
		if env$('data')='' then ! if env$('data') is blank than set it here.
			dim edd_base$*256
			if env$('ProgramData')='' then
				edd_base$=fnshortpath$(env$('appdata'))
			else
				edd_base$=env$('ProgramData')
			end if
			setenv('data',edd_base$&'\ACS\')
			fnmakesurepathexists(env$('data')&'\Data\')
			if env$('data')(len(env$('data')):len(env$('data')))<>'\' then ! if env$('data') does not end with a backslash nor forward slash than add one.
				setenv('data',env$('data')&'\')
			end if
		end if
	end if
fnend
def library fnSetQ(setQ$*256)
	if ~setup then fn_setup
	fnSetQ=fn_setQ(setQ$)
fnend
def fn_setQ(setQ$*256)
	setQ$=rtrm$(setQ$,'\')
	if pos(setQ$,' ')>0 then setQ$=fnshortpath$(setQ$)
	setenv('Q',setQ$)
	execute 'config substitute [Q] "'&env$('Q')&'"'
	if env$('acsDeveloper')='' then
		execute 'config substitute [ScreenIO_ScreenFldDrive] '&env$('Q')
	end if
	fnmakesurepathexists('[Q]\Data\')
	fnmakesurepathexists('[Q]\'&env$('CurSys')&'mstr\')
	! if env$('acsDebug')<>'' then
	!   pr 'SetQ to '&env$('Q')
	!   pause
	! end if
fnend
def fn_setQBase(newQBase$*256)
	if env$('QBase')='' then
		newQBase$=rtrm$(newQBase$,'\')
		if pos(newQBase$,' ')>0 then newQBase$=fnshortpath$(newQBase$)
		setenv('QBase',newQBase$)
		exe 'config substitute [QBase] '&env$('QBase')
	end if
fnend
def fn_update_needed(acs_version_prior$,acs_version_running$)
	un_return=0
	if acs_version_running$<acs_version_prior$ and lwrc$(env$('acsIgnoreDataVersion'))<>'yes' then
		un_return=1
		msgbox("The ACS Software version ("&acs_version_running$&") of this workstation is less than the last version ("&acs_version_prior$&") used to access ACS Data."&chr$(13)&"You must update this workstation to continue.")
	end if
	fn_update_needed=un_return
fnend
def fn_last_version_used$(; setit$*256)
	dim lvu_line$*256
	if setit$<>'' then
		lvu_line$=trim$(setit$)
		fnreg_write('ACS last version used',setit$)
	else
		fnreg_read('ACS last version used',lvu_line$)
		if lvu_line$='' then
			open #hAcsVersion:=fn_gethandle: 'Name=[Q]\Data\ACS_Version.txt,RecL=256',display,input ioerr LVU_OLD_FILE_OPEN_IOERR
			linput #hAcsVersion: lvu_line$
			close #hAcsVersion,free:
			fnreg_write('ACS last version used',lvu_line$)
		end if
	end if
	LVU_OLD_FILE_OPEN_IOERR: !
	fn_last_version_used$=trim$(lvu_line$)
fnend
def fn_multisession_test
	fnureg_read('Disable_MultiSession',disable_multisession$)
	if val(session$(len(session$):len(session$)))=>2 and disable_multisession$='True' then
		msgbox('Multiple sessions have been disabled in user preferences.')
		mt_return=0
	else
		mt_return=1
	end if
	fn_multisession_test=mt_return
fnend
def fn_update_version_for_inno
	open #h_tmp:=fn_gethandle: 'name=:C:\ACS\Setup\ACS 5 - AppVersion.iss,RecL=256,Replace',display,output
	pr #h_tmp: ';This file is dynamically built by '&os_filename$(program$)&' when run by an ACSDeveloper.'
	pr #h_tmp: ';Attempts to edit it directly are moot and will be quickly overwritten.'
	pr #h_tmp: 'AppVersion='&env$('acsVersion')
	pr #h_tmp: 'AppVerName=ACS '&env$('acsVersion')
	close #h_tmp:
fnend
def fn_csEnv
	dim ce_line$*2048
	dim ce_prefix$
	ce_prefix$="Client_"
	dim ce_field$*2048
	dim ce_value$*2048
	dim ce_os_temp_file$*1048
	dim ce_br_temp_file$*1048
	! if env$('data')='/br/orders/brc_oe/Data' then        !  Gordon's Linux CS Server
	!   setenv('data','\\JAZZ\BR Order Entry\brc_oe\Data')  !  Gordon's Linux CS Server
	! end if                                                !  Gordon's Linux CS Server
	if env$('cursys')='CM' then
		ce_os_temp_file$=rtrm$(env$('cs_temp'),'\')&'\cs-'&session$&'.txt'
		ce_br_temp_file$='@::'&env$('cs_temp')&'\cs-'&session$&'.txt'
	else
		ce_os_temp_file$=rtrm$(env$('data'),'\')&'\cs-'&session$&'.txt'
		ce_br_temp_file$='[Q]\cs-'&session$&'.txt'
	end if
	if env$('cursys')='CM' then
		ce_br_temp_file$='F:\CLSINC\temp\cs-'&session$&'.txt'
	end if
	ce_retry_4152_count=0
	CE_MAKE_TEMP_FILE: !
	fnmakesurepathexists(ce_br_temp_file$)
	execute '*sys -M set > "'&ce_os_temp_file$&'"'
	open #hOsSet:=fn_gethandle: "Name="&ce_br_temp_file$,display,input error CE_DEBUG_OPEN_ERR ! error XIT_fn_csEnv
	do
		linput #hOsSet: ce_line$ error XIT_LOOP
		gw_wholeline=len(rtrm$(ce_line$))
		gw_addlen=1
		gw_posfnwp=pos(uprc$(ce_line$),"=")
		if gw_posfnwp>0 then
			gw_equal =pos(ce_line$,'=')
			gw_nextequal =pos(ce_line$,'=',gw_posfnwp+gw_addlen)
			if gw_equal > 0 then
				ce_field$ = ce_prefix$&ce_line$(1:gw_posfnwp-1)
				ce_value$ = ce_line$(gw_posfnwp+1:gw_wholeline)
				setenv(ce_field$,ce_value$) error ignore
!       pr 'setenv("'&ce_field$&'","'&ce_value$&'")'
! Should SETENV FAIL, Ignore it
			end if
		end if
	loop
	CE_DEBUG_OPEN_ERR: !
	if err=4152 or err=4203 then
		if (ce_retry_4152_count+=1)<=3 then
			goto CE_MAKE_TEMP_FILE
		else if (ce_retry_4152_count+=1)<=6 then
			if ce_br_temp_file$(1:3)<>'@::' then
				ce_br_temp_file$(0:0)='@::'
			end if
			goto CE_MAKE_TEMP_FILE
		else if (ce_retry_4152_count+=1)<=9 then

			ce_os_temp_file$='C:\Session'&session$&'\cs.txt'
			ce_br_temp_file$='@::'&ce_os_temp_file$
			fnmakesurepathexists(ce_br_temp_file$)
			goto CE_MAKE_TEMP_FILE
		end if
	end if
	exec 'con gui off'
	pr 'error '&str$(err)&' on open of ce_br_temp_file$'
	pr '     ce_os_temp_file$='&ce_os_temp_file$
	pr '     ce_br_temp_file$='&ce_br_temp_file$
	pr '          exists=';exists(ce_br_temp_file$)
	pr '    Press ENTER to Exit' : kstat$(1)
	goto Xit
	XIT_LOOP: ! End of Startloop
		close #hOsSet,free: error ignore
	!
	! XIT_fn_csEnv: !
	execute "*sy -M CD > "&ce_os_temp_file$
	open #hOsCd:=fn_gethandle: "Name="&ce_br_temp_file$,display,input error XIT_FNCS_OS_PATH
	linput #hOsCd: client_os_path$ error ignore
	close #hOsCd,free: error ignore
	setenv('client_os_path',client_os_path$)
	XIT_FNCS_OS_PATH: !
fnend
def fn_show_release_notes(version_prior$,version_current$; ___,didOpen)
	dim srnLine$*1024,srnItem$(0)*1024
	open #hSrnOut:=fn_gethandle: 'name=ACS_tmp_Release_Note_Report.txt,recl=1024,replace',d,o
	open #hReleaseNotes:=fn_gethandle: 'name=S:\Core\Release_Notes.txt',d,i ioerr SrnReleaseNotesEof
	didOpen=1
	pr #hSrnOut: 'You just updated from '&version_prior$&' to '&version_current$&'.'
	pr #hSrnOut: ''
	do
	linput #hReleaseNotes: srnLine$ eof SrnReleaseNotesEof
		str2mat(srnLine$,mat srnItem$,chr$(9))
		if udim(mat srnItem$)=>3 then
			srnItem3Value=val(srnItem$(3)) conv ignore
		end if
		if udim(mat srnItem$)=1 then
			pr #hSrnOut: chr$(9)&chr$(9)&srnLine$
		else
			pr #hSrnOut: srnLine$
		end if
	loop until srnItem3Value<>0 and (udim(srnItem$)=>3 and srnItem$(3)<=version_prior$)
	SrnReleaseNotesEof: !
	close #hSrnOut:
	if didOpen then
		exec 'sy -c -m'&os_filename$('ACS_tmp_Release_Note_Report.txt')&'"'
	end if
fnend
def library fngethandle
	fngethandle=fn_gethandle
fnend
def fn_gethandle
	hMaybe=189
	ghReturn=0
	do
		if file(hMaybe)<0 and file$(hMaybe)='' then
			ghReturn=hMaybe
			goto gethandleFINIS
		end if
		hMaybe-=1
	loop until hMaybe=-1
	pr 'fn_gethandle found no available file handles, so it is returning -1' : pause
	gethandleFINIS: !
	fn_gethandle=ghReturn
fnend
def fn_FreeVirtualStore ! does not seem to work.
	dim acsInstallationPath$*256
	acsInstallationPath$=fnAcsInstallationPath$(1)
	if acsInstallationPath$(2:2)=':' then ! it is not a unc
		if exists(env$('LocalAppData')&'\VirtualStore'&acsInstallationPath$(3:len(acsInstallationPath$))) then
			! pr 'sy Del "'&env$('LocalAppData')&'\VirtualStore'&acsInstallationPath$(3:len(acsInstallationPath$))&'\*.*" /s /y' : pause
			exe 'sy Del "'&env$('LocalAppData')&'\VirtualStore'&acsInstallationPath$(3:len(acsInstallationPath$))&'\*.*" /s /y'
! this DEL to the VIRTUALSTORE does not seem to work....  perhaps if it were put inside a batch file???
		end if
	end if
fnend
def fn_UpdateQFileIO
	if env$('acsDeveloper')='' and env$('cursys')<>'CM' then ! because I used symbolic link
		fnMakeSurepathExists(env$('QBase')&'\Core\FileIO\Layout\')
		fnMakeSurepathExists(env$('QBase')&'\Core\FileIO\Layout\version\')
		fnCopy('S:\Core\FileIO\Layout\*.*'        ,env$('QBase')&'\Core\FileIO\Layout\*.*'        )
		fnCopy('S:\Core\FileIO\Layout\version\*.*',env$('QBase')&'\Core\FileIO\Layout\version\*.*')
	else if env$('cursys')='CM' then
		fn_updateAlienFolder('filelay','S:\Core\FileIO\Layout','*.fio','*.')
		fn_updateAlienFolder('filelay\version','S:\Core\FileIO\Layout\version','*.*','*.*')
		! pause
	end if
fnend
def fn_updateAlienFolder(localAlien$*512,sourceFolder$*512,filterFrom$,filterTo$)
		library 'S:\Core\Library': fngetdir2
		dim sLayFile$(0)*256
		dim sLayDate$(1)*32
		dim sLayTime$(1)*32
		fngetdir2(sourceFolder$&'\',mat sLayFile$, '',filterFrom$,mat sLayDate$,mat sLayTime$)
		dim dLayFile$(0)*256
		dim dLayDate$(1)*32
		dim dLayTime$(1)*32
		if udim(mat sLayFile$)=0 then 
			pr 'fngetdir2 found no files found in: S:\Core\FileIO\Layout\'
			pause
		end if
		fngetdir2(localAlien$&'\',mat dLayFile$, '',filterTo$,mat dLayDate$,mat dLayTime$)
		for sItem=1 to udim(mat sLayFile$)
			if sLayFile$(sItem)(1:3)='CM ' or sLayFile$(sItem)(1:3)='CO ' then
				dWhich=srch(mat dLayFile$,sLayFile$(sItem)(1:pos(sLayFile$(sItem),'.',-1)-1))
				dDay=sDay=0
				sDay=days(slaydate$(sitem),'mm/dd/ccyy')
				if dWhich=>0 then dDay=days(dlaydate$(dWhich),'mm/dd/ccyy')
				if dDay<=sDay then
					fnCopy(sourceFolder$&'\'&sLayFile$(sItem)        ,localAlien$&'\'&rtrm$(filterTo$,'.')        )
					! pr 'from: '&sourceFolder$&'\'&sLayFile$(sItem)   
					! pr '  to: '&localAlien$&'\'&rtrm$(filterTo$,'.')
					! pause
				end if
			end if
		nex sItem
fnend

def fn_UpdateQScreenIO
	if env$('acsDeveloper')='' then ! because I used: mklink /J "C:\Users\John\OneDrive\ACS\Dev-5 Data\Core\ScreenIO\Screen" "C:\ACS\Dev-5\Core\ScreenIO\Screen"
		fnMakeSurepathExists(env$('QBase')&'\Core\ScreenIO\screen\')
		fnCopy('S:\Core\ScreenIO\screen\*.*'      ,env$('QBase')&'\Core\ScreenIO\screen\*.*'      )
	end if
fnend
RetryAFewTimes: ! r:
	retryAfewTimesCount+=1
	if retryAfewTimesCount=10 then
		pr 'error '&str$(err)&' on line '&str$(line)&'.  Retried '&str$(retryAfewTimesCount)&' times.'
		pause
		retryAfewTimesCount=0
	end if
	sleep(.4)
retry ! /r
def fn_CopySfileIoIniToFileIoIni
	! note that destination fileio.ini must be all lowercase as it is case sensitive on some systems
	if env$('acsDeveloper')='' then
		fnCopy('S:\FileIO.ini','fileio.ini')
	else
		open #hIn_fileIoIni:=fn_gethandle: 'name=S:\fileio.ini',d,i ioerr RetryAFewTimes
		open #hOut_FileIoIni:=fn_gethandle: 'name=FileIO.ini,recl=256,replace',d,o ioerr RetryAFewTimes
		dim line$*256
		do
			linput #hIn_fileIoIni: line$  eof Csf2f_Eof
			posForDev= pos(lwrc$(line$),'<for developer>')
			if posForDev>0 then
				posForDev+=15
				posEndDev=pos(lwrc$(line$),'</for developer>')-1
				pr #hOut_FileIoIni: line$(posForDev:posEndDev)
				! pr line$(posForDev:posEndDev)
				! pause
			else
				pr #hOut_FileIoIni: line$
				! pr line$
			end if
		loop
		Csf2f_Eof: !
	end if
fnend
def fn_startStatus(text$*128)
	pr f str$(startStatusLine+=1)&',1,C': text$
fnend
def fn_acsVersion$
	if ~setup then fn_setup
	open #hBuild:=fn_gethandle: 'name=S:\Core\Build.txt',d,i
	linput #hBuild: build$
	close #hBuild:
	 setenv('acsVersion','5.'&rtrm$(build$))
	fn_acsVersion$=env$('acsVersion')
fnend
def library fnWriteProc(procName$*64,procLine$*256)
	if ~setup then fn_setup
	fnWriteProc=fn_writeProc(procName$,procLine$)
fnend
def fn_writeProc(procName$*64,procLine$*256)
	dim procNameHold$*64
	if procName$='' then ! append last one
		open #hEd:=fn_gethandle: 'name='&procNameHold$&',use',d,o
	else
		procNameHold$=procName$
		open #hEd:=fn_gethandle: 'name='&procName$&',replace',d,o
	end if
	pr #hEd: procLine$
	close #hEd:
fnend
def library fnProgramDataDir$*256
	if ~setup then fn_setup
	fnProgramDataDir$=fn_programDataDir$
fnend
def fn_programDataDir$*256(;___,return$*256,pddTryItem)
	! to handle windows access to programdata folder
	if ~setup_programDataDir then
		setup_programDataDir=1
		dim pddTry$(2)*256
		pddTry$(1)='C:\ProgramData\ACS\'
		pddTry$(2)='C:\ACS_Data\'
		dim pddTryRetainedAnswer$*256
		pddTryRetainedAnswer$=''
	end if
	if pddTryRetainedAnswer$<>'' then
		return$=pddTryRetainedAnswer$
	else
		do until pddTryItem=>udim(mat pddTry$) or return$<>''
			pddTryItem+=1
			! fn_rights_test(rt_folder$*256,rt_how_to_fix$*256,folder_name$; additional_text_for_failure$*2048,skipmsg)
			fnmakesurepathexists(pddTry$(pddTryItem))
			if fn_rights_test(pddTry$(pddTryItem),'','','', 1) then
				return$=pddTry$(pddTryItem)
			end if
		loop
		if return$='' then
			msgbox('Insufficient rights to access local data folder. Please call ACS technical support.')
			exe 'sy'
		end if
		pddTryRetainedAnswer$=return$
	end if
	fn_programDataDir$=return$
fnend
include: Ertn
