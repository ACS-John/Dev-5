! S:\Core\Programs\Update
force_update=1
! r: dims and constants
	if ~setup then fn_setup

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
fnTop(program$,"ACS Update")

fnToS : lc=rc=0 : col1len=32 : col2pos=34
	dim resp$(10)*128
	lc+=1
	fnLbl(lc+=1,1,'Last Automated Update Completed:',col1len,1)
	fnTxt(lc,col2pos,10, 128,0,'',1) : fnreg_read('Last Update',resp$(rc_lastUpdate=rc+=1), '(never)',1) : if resp$(rc_lastUpdate)<>'(never)' then last_update$=date$(days(resp$(rc_lastUpdate),'ccyy/mm/dd'),'mm/dd/ccyy')
	lc+=1
	fnOpt(lc+=1,15,'Automated FTP Update'  , 0) : fnPcReg_read('Update Automated',resp$(rc_auto=rc+=1), 'True' ,1)
	fnOpt(lc+=1,15,'Manual Download Update', 0) : fnPcReg_read('Update Manual'   ,resp$(rc_manual=rc+=1), 'False',1)
	fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey<>5 then 
	fnPcReg_write('Update Automated',resp$(rc_auto))
	fnPcReg_write('Update Manual'   ,resp$(rc_manual))
	if resp$(rc_auto)='True' then ! r: Automated Update
		fnStatus('launching automated update')
		if ~fn_simple_connectivity_test then
			fnStatusPause
		else
			fn_drive_sys_must_exist
			fn_update_license
			fn_update
		end if 
		! /r
	else ! r: Manual Update
		fnclient_support(mat system_id$,mat system_support_end_date,mat on_support,grace_days)
		fnArraySortC(mat system_id$)
		itemToRemove=srch(mat system_id$,'G2')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'UB-EFT')           	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'HH')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'P4')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'U4')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'U5')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'GB')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'EM')                	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		itemToRemove=srch(mat system_id$,'Client Billing')   	: if itemToRemove>0 then fnArrayItemRemoveC(mat system_id$,itemToRemove) : fnArrayItemRemoveN(mat system_support_end_date,itemToRemove) : fnArrayItemRemoveN(mat on_support,itemToRemove)
		dim url$*512
		manHitCount=0
		url$='http://planetacs.net/acs5update/'
		for x=1 to udim(mat system_id$)
			if on_support(x) then
				url$&=system_id$(x)&'_'
				manHitCount+=1
			end if
		nex x
		url$=url$(1:len(url$)-1) ! remove last underscore
		url$&='.html'
		if manHitCount then
			execute 'sy -C start '&url$
			execute 'System'
		else
			dim message$(0)*256
			fnAddOneC(mat message$,'Nothing is licensed to update.')
			fnAddOneC(mat message$,'If this message is in error, contact ACS Support.')
			fnMsgBox(mat message$)
		end if
		! /r
	end if
end if
goto Xit
Xit: fnXit

def fn_setup
	setup=1
	autoLibrary
	on error goto Ertn
fnend

def fn_simple_connectivity_test
	dim conectivity_test_result$*40
	dim ua_acs_datetime$*40

	conectivity_test_result$=fn_conectivity_test$
	if env$('ACSDeveloper')<>'' then
		fnStatus('ACS Developers ('&env$('ACSDeveloper')&') should not update from the web.  It would overwrite all their local programs.')
		ua_return=0
	else if conectivity_test_result$='' then
		fnStatus('Connectivity test failed.')
		ua_return=0
	else
		ua_return=1
	end if


	!   ua_acs_datetime$=fn_acs_update_date$
	fn_simple_connectivity_test=ua_return
fnend
def fn_conectivity_test$*40
	fnStatus("testing connectivity...")
	wud_success=0
	dim wud_return$*40
	wud_return$=''
	open #h_script=fnH: 'Name='&script_name$&',RecL=256,replace',d,o
	pr #h_script: 'user acs5update'
	pr #h_script: 'ACSKen!1'
	pr #h_script: 'Dir ACS-5-Update-CO.exe'
	pr #h_script: 'quit'
	close #h_script:

	open #h_batch=fnH: 'Name='&batch_name$&',RecL=256,replace',d,o
	pr #h_batch: 'prompt $p$g'
	pr #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
	close #h_batch:
	fn_execute('-m',os_filename$(batch_name$))
	open #h_return=fnH: 'Name='&return_name$,display,input
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
def library fnUpdateLicense
	if ~setup then fn_setup
	fnUpdateLicense=fn_update_license
fnend
def fn_update_license
	fnStatus("updating license information...")
	ul_success=0
	dim ul_return$*40
	ul_return$=''
	open #h_script=fnH: 'Name='&script_name$&',RecL=256,replace',d,o
	pr #h_script: 'user acs5update'
	pr #h_script: 'ACSKen!1'
	pr #h_script: 'LCD '&env$('Temp')
	pr #h_script: 'get ACS_5_Update_Support_Cache.exe'
	pr #h_script: 'quit'
	close #h_script:

	open #h_batch=fnH: 'Name='&batch_name$&',RecL=256,replace',d,o
	pr #h_batch: 'prompt $p$g'
	pr #h_batch: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
	close #h_batch:
	fn_execute('-m',os_filename$(batch_name$))
	open #h_return=fnH: 'Name='&return_name$,display,input
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
def fn_update(; ___,u_which,support_text$*256,client_has_count)
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
		fnStatus('')
		fnStatus('No ACS Support detected.')
		fnStatus('To update contact ACS at 1-800-643-6318.')
		fnStatus('')
		fnStatusPause
	else
		fnStatus('Systems on Support:')
		for client_has_item=1 to client_has_count
			fnStatus('   '&fnSystemName$(client_has$(client_has_item)))
			u_which=srch(mat system_id$,client_has$(client_has_item))
			if u_which>0 then
				if days(date('ccyymmdd'),'ccyymmdd')<=days(system_support_end_date(u_which),'ccyymmdd') then
					support_text$='      active until '
					support_text$&=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
				else
					support_text$='      Support expired on '
					support_text$&=cnvrt$('pic(####/##/##)',system_support_end_date(u_which))
					if on_support(u_which) then
						support_text$&=' but update is allowed during '&str$(grace_days)&' day grace period.'
					end if
				end if
				fnStatus(support_text$)
			else
				fnStatus(chr$(9)&chr$(9)&'      (no support data)')
			end if
		next client_has_item
		! client_has_count=udim((mat client_has$))
		if srch(mat client_has$,'G2')>0 then client_has_count-=1
		if srch(mat client_has$,'HH')>0 then client_has_count-=1
		if srch(mat client_has$,'P4')>0 then client_has_count-=1
		if srch(mat client_has$,'U4')>0 then client_has_count-=1
		fnStatus("Downloading Updates for "&str$(client_has_count)&" support licensed systems.")
		fnStatus("This may take up to "&str$(client_has_count*2+11)&" minutes on systems with slow internet connections.")
		fnStatus("Download started at "&time$)
		fnStatus("Please wait...")
		!  fn_download_an_update(system_id$*2)
		open #h_script=fnH: 'Name='&script_name$&',RecL=256,replace',d,o
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
		
		open #h_batch=fnH: 'Name='&batch_name$&',RecL=256,replace',d,o
		pr #h_batch: 'prompt $p$g'
		pr #h_script: 'ftp -n -s:"'&os_filename$(script_name$)&'" ftp.planetacs.net >"'&os_filename$(return_name$)&'"'
		close #h_batch:
		fn_execute('-m',os_filename$(batch_name$))
		u_download_success=0
		open #h_return=fnH: 'Name='&return_name$,display,input
		do
			linput #h_return: line$ eof U_RETURN_EOF
			if line$(1:4)='226 ' then u_download_success=1
		loop
		U_RETURN_EOF: !
		close #h_return:
		if u_download_success then
			for ch_item=1 to udim(mat client_has$)
				if client_has$(ch_item)<>'CO' and client_has$(ch_item)<>'G2' and client_has$(ch_item)<>'HH' and client_has$(ch_item)<>'P4' and client_has$(ch_item)<>'U4' then
					fnStatus('launching '&client_has$(ch_item)&' update.')
					fn_execute('',env$('temp')&'\acs-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS /NOCANCEL /SILENT')
			!       execute 'sy '&env$('temp')&'\acs-5-Update-'&client_has$(ch_item)&'.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS' ! /SILENT
				end if
			next ch_item
			fnStatus('Closing program and launching Core update.')
			fn_execute('-c',env$('temp')&'\acs-5-Update-CO.exe /DIR="'&fn_acs_installation_path$&'" /NOICONS')
			sleep(4)
			fnFree(batch_name$)
			fnFree(script_name$)
			fnFree(return_name$)
			fnreg_write('Last Update',date$('ccyy/mm/dd')) ! &' '&time$)
			execute "System"
		else
			fnStatus('*** Update failed to download ***')
			fnStatusPause
		end if
	end if
fnend
def library fnAcsInstallationPath$*256(; longFileName)
	if ~setup then fn_setup
	fnAcsInstallationPath$=fn_acs_installation_path$( longFileName)
fnend
def fn_acs_installation_path$*256(; longFileName)
	dim acs_installation_path$*256
	acs_installation_path$=os_filename$('S:\')
	if longFileName then
		acs_installation_path$=srep$(acs_installation_path$,'PROGRA~2','Program Files (x86)')
		acs_installation_path$=srep$(acs_installation_path$,'ACS5~1','ACS 5')
	end if
	fn_acs_installation_path$=acs_installation_path$
fnend
def fn_drive_sys_must_exist
	if ~exists('S:\Drive.sys') then
		fnStatus('creating missing Drive.sys')
		open #h_drive_sys=fnH: 'Name=S:\Drive.sys,RecL=256,New',d,o
		open #h_brconfig_sys=fnH: 'Name=S:\BRConfig.sys',display,input
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

include: ertn
