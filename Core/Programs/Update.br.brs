! S:\Core\Programs\Update
	force_update=1
! r: dims and constants
	if ~setup then let fn_setup
! dim message$(1)*256
	dim batch_name$*256
	dim script_name$*256
	dim return_name$*256
	dim line$*256
	batch_name$=env$('temp')&'\update'&session$&'.cmd'
	script_name$=env$('temp')&'\ftp_script'&session$&'.txt'
	return_name$=env$('temp')&'\return'&session$&'.txt'
	grace_days=45
	fnclient_support(mat system_id$,mat system_support_end_date,mat on_support,grace_days)
! /r
	fntop(program$,"ACS Update")
! r: main loop
	fn_status('launching update')
	if ~fn_simple_connectivity_test then
!   fn_status('No newer update is available.')
		fn_status_pause
	else
		fn_drive_sys_must_exist
		fn_update_license
		fn_update
	end if
XIT: fnxit
! /r
	def fn_setup
		setup=1
		library 'S:\Core\Library': fnreg_read,fnreg_write,fnmsgbox,fntop,fnxit,fngethandle
		library 'S:\Core\Library': fnclient_has_on_support_list,fnSystemName$,fnclient_support,fnerror
		library 'S:\Core\Library': fnFree
		library 'S:\Core\Library': fnSrepEnv$
		on error goto ERTN
	fnend
! def fn_acs_update_date$*40(;setit$*40)
!   dim aud_return$*40
!   if setit$='(default)' then setit$=date$('ccyy/mm/dd')&' - '&time$
!   if setit$<>'' then
!     aud_return$=setit$
!     fnreg_write('ACS.Update.Date',setit$)
!   else
!     fnreg_read('ACS.Update.Date',aud_return$)
!   end if
!   fn_acs_update_date$=aud_return$
! fnend
	def fn_simple_connectivity_test
		dim conectivity_test_result$*40
		dim ua_acs_datetime$*40
!
		conectivity_test_result$=fn_conectivity_test$
		if env$('ACSDeveloper')<>'' then
			fn_status('ACS Developers ('&env$('ACSDeveloper')&') should not update from the web.  It would overwrite all their local programs.')
			ua_return=0
		else if conectivity_test_result$='' then
			fn_status('Connectivity test failed.')
			ua_return=0
		else
			ua_return=1
		end if
!
!
!   ua_acs_datetime$=fn_acs_update_date$
		fn_simple_connectivity_test=ua_return
	fnend
	def fn_conectivity_test$*40
		fn_status("testing connectivity...")
		wud_success=0
		dim wud_return$*40
		wud_return$=''
		open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output
		pr #h_script: 'user acs5update'
		pr #h_script: 'ACSKen!1'
		pr #h_script: 'Dir ACS-5-Update-CO.exe'
		pr #h_script: 'quit'
		close #h_script:
!
		open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output
		pr #h_batch: 'prompt $p$g'
		pr #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
		close #h_batch:
		fn_execute('-m',os_filename$(batch_name$))
		open #h_return:=fngethandle: 'Name='&return_name$,display,input
		do
			linput #h_return: line$ eof WUD_RETURN_EOF
			if line$(3:3)='-' then
				wud_date$=date$(days(line$(1:8),'mm-dd-yy'),'ccyy/mm/dd')
				wud_time$=line$(11:15)
				wud_ampm$=line$(16:17)
				if wud_ampm$='PM' then
					wud_time$(1:2)=str$(val(wud_time$(1:2))+12)
				end if
				wud_return$=wud_date$&' - '&wud_time$&':00'
			end if
		loop
WUD_RETURN_EOF: !
		close #h_return:
		fnFree(return_name$)
		fnFree(script_name$)
		fnFree(batch_name$)
		fn_conectivity_test$=wud_return$
	fnend
	def fn_update_license
		fn_status("updating license information...")
		ul_success=0
		dim ul_return$*40
		ul_return$=''
		open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output
		pr #h_script: 'user acs5update'
		pr #h_script: 'ACSKen!1'
		pr #h_script: 'LCD '&env$('Temp')
		pr #h_script: 'get ACS_5_Update_Support_Cache.exe'
		pr #h_script: 'quit'
		close #h_script:
!
		open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output
		pr #h_batch: 'prompt $p$g'
		pr #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
		close #h_batch:
		fn_execute('-m',os_filename$(batch_name$))
		open #h_return:=fngethandle: 'Name='&return_name$,display,input
		do
			linput #h_return: line$ eof ul_RETURN_EOF
			if line$(3:3)='-' then
				ul_date$=date$(days(line$(1:8),'mm-dd-yy'),'ccyy/mm/dd')
				ul_time$=line$(11:15)
				ul_ampm$=line$(16:17)
				if ul_ampm$='PM' then
					ul_time$(1:2)=str$(val(ul_time$(1:2))+12)
				end if
				ul_return$=ul_date$&' - '&ul_time$&':00'
			end if
		loop
ul_RETURN_EOF: !
		close #h_return:
		fnFree(return_name$)
		fnFree(script_name$)
		fnFree(batch_name$)
		fn_execute('-c',env$('temp')&'\acs_5_Update_Support_Cache.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS /NOCANCEL /SILENT')
		fn_update_license=1
	fnend
def fn_update
	if env$('acsClient')='Ed Horton' then
		mat client_has$(5)
		client_has$(1)='UB'
		client_has$(2)='PR'
		client_has$(3)='GL'
		client_has$(4)='CL'
		client_has$(5)='CO'
		client_has_count=5
	else
		client_has_count=fnclient_has_on_support_list(mat client_has$, grace_days)
	end if
	if client_has_count=0 then
		fn_status('')
		fn_status('No ACS Support detected.')
		fn_status('To update contact ACS at 1-800-643-6318.')
		fn_status('')
		fn_status_pause
	else
		fn_status('Systems on Support:')
		for client_has_item=1 to client_has_count
			fn_status('   '&fnSystemName$(client_has$(client_has_item)))
			dim support_text$*256
			u_which=srch(mat system_id$,client_has$(client_has_item))
			if u_which>0 then
				if days(date('ccyymmdd'),'ccyymmdd')<=days(system_support_end_date(u_which),'ccyymmdd') then
					support_text$='      active until '
					support_text$(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
				else
					support_text$='      Support expired on '
					support_text$(inf:inf)=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
					if on_support(u_which) then
						support_text$(inf:inf)=' but update is allowed during '&str$(grace_days)&' day grace period.'
					end if
				end if
				fn_status(support_text$)
			else
				fn_status(chr$(9)&chr$(9)&'      (no support data)')
			end if
		next client_has_item
		! client_has_count=udim((mat client_has$))
		if srch(mat client_has$,'G2')>0 then client_has_count=client_has_count-1
		if srch(mat client_has$,'HH')>0 then client_has_count=client_has_count-1
		if srch(mat client_has$,'P4')>0 then client_has_count=client_has_count-1
		if srch(mat client_has$,'U4')>0 then client_has_count=client_has_count-1
		fn_status("Downloading Updates for "&str$(client_has_count)&" support licensed systems.")
		fn_status("This may take up to "&str$(client_has_count*2+11)&" minutes on systems with slow internet connections.")
		fn_status("Download started at "&time$)
		fn_status("Please wait...")
		!  fn_download_an_update(system_id$*2)
		open #h_script:=fngethandle: 'Name='&script_name$&',RecL=256,replace',display,output
		pr #h_script: 'user acs5update'
		pr #h_script: 'ACSKen!1'
		pr #h_script: 'LCD '&env$('Temp')
		pr #h_script: 'get ACS-5-Update-CO.exe'
		for ch_item=1 to udim(mat client_has$)
			if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then
				pr #h_script: 'get ACS-5-Update-'&client_has$(ch_item)&'.exe'
			end if
		next ch_item
		pr #h_script: 'quit'
		close #h_script:
		!
		open #h_batch:=fngethandle: 'Name='&batch_name$&',RecL=256,replace',display,output
		pr #h_batch: 'prompt $p$g'
		pr #h_script: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
		close #h_batch:
		fn_execute('-m',os_filename$(batch_name$))
		u_download_success=0
		open #h_return:=fngethandle: 'Name='&return_name$,display,input
		do
			linput #h_return: line$ eof U_RETURN_EOF
			if line$(1:4)='226 ' then u_download_success=1
		loop
		U_RETURN_EOF: !
		close #h_return:
		if u_download_success then
			for ch_item=1 to udim(mat client_has$)
				if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then
					fn_status('launching '&client_has$(ch_item)&' update.')
					fn_execute('',env$('temp')&'\acs-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS /NOCANCEL /SILENT')
			!       execute 'sy '&env$('temp')&'\acs-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS' ! /SILENT
				end if
			next ch_item
			fn_status('Closing program and launching Core update.')
			fn_execute('-c',env$('temp')&'\acs-5-Update-CO.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS')
			sleep(4)
			fnFree(batch_name$)
			fnFree(script_name$)
			fnFree(return_name$)
			fnreg_write('Last Update',date$('ccyy/mm/dd')) ! &' '&time$)
			execute "System"
		else
			fn_status('*** Update failed to download ***')
			fn_status_pause
		end if
	end if
fnend
	def library fnAcsInstallationPath$*256(; longFileName)
		if ~setup then let fn_setup
		fnAcsInstallationPath$=fn_acs_installation_path$( longFileName)
	fnend
	def fn_acs_installation_path$*256(; longFileName)
		dim acs_installation_path$*256
!   if ~setup_acs_installation_path then
!     setup_acs_installation_path=1
!     fnIniOpen('acs.ini')
!     acs_installation_path$=fnIniRead$('Core','InstallPath')
!     if acs_installation_path$='' then
		acs_installation_path$=os_filename$('S:\')
!     end if
!   end if
		if longFileName then
			acs_installation_path$=srep$(acs_installation_path$,'PROGRA~2','Program Files (x86)')
			acs_installation_path$=srep$(acs_installation_path$,'ACS5~1','ACS 5')
		end if
		fn_acs_installation_path$=acs_installation_path$
	fnend
	def fn_drive_sys_must_exist
		if ~exists('S:\Drive.sys') then
			fn_status('creating missing Drive.sys')
			open #h_drive_sys:=fngethandle: 'Name=S:\Drive.sys,RecL=256,New',display,output
			open #h_brconfig_sys:=fngethandle: 'Name=S:\BRConfig.sys',display,input
			pr #h_drive_sys: 'Rem Drive.sys automatically created by update process on '&date$&' at '&time$
			do
				linput #h_return: line$ eof DSME_BRCONFIG_EOF
				if lwrc$(line$(1:6))='drive ' or line$(1:1)='@' then
					pr #h_drive_sys: line$
				end if
			loop
			DSME_BRCONFIG_EOF: !
			close #h_brconfig_sys:
			close #h_drive_sys:
		end if
	fnend
def fn_execute(flags$*128,exe_what$*256)
	if env$('ACSDeveloper')<>'' then
		pr 'Flags:  '&flags$
		if exists(exe_what$) then
			! PAusE
			execute 'sy -c -w notepad '&exe_what$
		else
			pr 'HMMM:  sy '&flags$&' '&exe_what$
		end if
	else
		execute 'sy '&flags$&' '&exe_what$
	end if
fnend
! r: fnStatus*
	def library fnStatus(text$*512)
		if ~setup then let fn_setup
		fnStatus=fn_status(text$)
	fnend
	def fn_status(text$*512)
		if ~status_initialized or file$(h_status_win)='' or h_status_win=0 then
			status_initialized=1
			dim headings$(1)*40,widths(1),forms$(1)*40,status_gridspec$*80
			open #h_status_win:=fngethandle: 'SRow=1,SCol=1,Rows=20,Cols=80,Parent=None,Caption=Status',display,output
			status_gridspec$='#'&str$(h_status_win)&',1,1,List 20/80'
			headings$(1)='Status'
			widths(1)=80
			forms$(1)='C 512'
			pr f status_gridspec$&",headers,[gridheaders]": (mat headings$,mat widths, mat forms$)
		end if
		text$=fnSrepEnv$(text$)
		if env$('ACSDeveloper')<>'' then
			pr f status_gridspec$&",+": text$(1:512)
		else
			pr f status_gridspec$&",+": text$(1:512) error ignore
		end if
!
		input fields status_gridspec$&",rowcnt,all,nowait": grid_rows
		curfld(1,grid_rows+1)
!
fnend

	def library fnStatusPause
		if ~setup then let fn_setup
		fnStatusPause=fn_status_pause
	fnend
	def fn_status_pause
		fn_status('Press any key to continue.')
		kstat$(1)
	fnend


	def library fnStatusClose
		if ~setup then let fn_setup
		fnStatusClose=fn_status_close
	fnend
	def fn_status_close
		close #h_status_win: ioerr ignore
		h_status_win=0
		status_initialized=0
	fnend
! /r
include: ertn
