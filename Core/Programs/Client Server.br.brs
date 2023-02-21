if ~setup then fn_setup
fnTop(program$)
dim resp$(10)*256
dim server_name$*128
dim anon_user$*128
dim anon_pass$*128
do ! r: main loop
	fnTos
	fnLbl(2,2,'Server IP or Name:',19,1)
	fnTxt(2,22,20,128,0,'',0,'localhost for single user, IP Address for internet based access')
	fnureg_read('CS Server Name',server_name$)
	if server_name$='' and env$('user_limit')='1' then
		server_name$='localhost'
	else if server_name$='' then
		server_name$=env$('computername')
	end if
	resp$(1)=server_name$
 
	fnLbl(4,2,'Port:',19,1)
	fnTxt(4,22,4,0,0,'number',0,'Default is 8555')
	fnureg_read('CS Server Port',cs_port$) : if cs_port$='' or cs_port$='0' then cs_port$='8555'
	resp$(2)=cs_port$
	fnLbl(6,2,'Anonymous User:',19,1)
	fnTxt(6,22,20,128,0,'',0,'leave blank to disable')
	fnureg_read('CS Anonymous User',anon_user$)
	resp$(3)=anon_user$
	fnLbl(7,2,'Anonymous Password:',19,1)
	fnTxt(7,22,20,128,0,'',0,'leave blank to disable')
	fnureg_read('CS Anonymous Password',anon_pass$)
	resp$(4)=anon_pass$
 
	fnCmdKey("Install Server",2,1,0)
	fnCmdKey("Uninstall Server",3,0,0)
	fnCmdKey('&Back',5,0,1)
	ckey=fnAcs(mat resp$)
	server_name$=resp$(1) : fnureg_write('CS Server Name',server_name$)
	cs_port$=resp$(2) : fnureg_write('CS Server Port',cs_port$)
	anon_user$=resp$(3) : fnureg_write('CS Anonymous User',anon_user$)
	anon_pass$=resp$(4) : fnureg_write('CS Anonymous Password',anon_pass$)
	if ckey=5 then goto Xit
	if ckey=2 then let fn_server_install
	if ckey=3 then let fn_server_uninstall
	fnStatusClose
loop ! /r
def fn_server_install
	fnStatus('fn_server_install')
	fnMakeSurePathExists('[temp]\acs\brCsInstall\ACS 5 Client\Core\')
	! if ~exists(env$('temp')&'\acs\brCsInstall') then execute 'mkdir "'&env$('temp')&'\acs\brCsInstall"'
	! if ~exists(env$('temp')&'\acs\brCsInstall\ACS 5 Client') then exe 'mkdir "'&env$('temp')&'\acs\brCsInstall\ACS 5 Client"'
	! if ~exists(env$('temp')&'\acs\brCsInstall\ACS 5 Client\Core') then exe 'mkdir "'&env$('temp')&'\acs\brCsInstall\ACS 5 Client\Core"'
	fnCopy('S:\brclient*.*',env$('temp')&'\acs\brCsInstall\ACS 5 Client\*.*')
	execute 'sy xcopy "'&os_filename$('S:\Core')&'\Client\*.*" "'&os_filename$(env$('temp')&'\acs\brCsInstall\ACS 5 Client')&'\*.*" /S'
	fnStatus('make [Q]\brListener.conf')
	open #h_br_parms_txt=fnH: 'Name='&env$('temp')&'\acs\brCsInstall\ACS 5 Client\br_parms.txt,RecL=256,replace',d,o
	pr #h_br_parms_txt: 'host='&server_name$
	pr #h_br_parms_txt: 'label=ACS_5_CS'
	close #h_br_parms_txt:
	open #h_brlistener_conf=fnH: 'Name=[Q]\brListener.conf,RecL=256,replace',d,o
	pr #h_brlistener_conf: 'LogFile='&env$('temp')&'\acs-Log-CS.txt'
	pr #h_brlistener_conf: 'LogLevel=10'
	pr #h_brlistener_conf: '['
	pr #h_brlistener_conf: 'Label=ACS_5_CS'
	pr #h_brlistener_conf: 'StartDir='&env$('temp')&'\' ! startdir does not support quotes
	pr #h_brlistener_conf: 'Executable="'&env$("STATUS.FILES.EXECUTABLE")&'"' ! br_prog$&br_ext$
	pr #h_brlistener_conf: 'Config="'&os_filename$('S:\brconfig.sys')&'"' ! br_prog$&br_ext$
	pr #h_brlistener_conf: 'Caption="ACS 5"'
	if anon_user$<>'' and anon_pass$<>'' then
		pr #h_brlistener_conf: 'Anonymous="'&anon_user$&'@'&anon_pass$&'"'
	end if
	if cs_port$<>'8555' and cs_port$<>'0' and cs_port$<>'' then
		pr #h_brlistener_conf: 'Port='&cs_port$
	end if
	pr #h_brlistener_conf: 'MultiSession'
	pr #h_brlistener_conf: ']'
	close #h_brlistener_conf:
	fnStatus('  and copy it into windows')
	fnStatus('  and copy DLL to 32 bit system folder (System32 or SysWOW64)')
  ! execute 'copy "S:\Core\Run_As_Admin.cmd" "'&env$('temp')&'\acs\brCsInstall\Install_BR_Server_'&session$&'.cmd"'
	open #h_copy_cmd=fnH: 'Name='&env$('temp')&'\acs\brCsInstall\Install_BR_Server_'&session$&'.cmd,replace,recl=256',d,o
    pr #h_copy_cmd:     '@echo on'
	pr #h_copy_cmd: 'copy "'&os_filename$('[Q]\brListener.conf')&'" "'&os_filename$(env$('windir')&'\brListener.conf')&'"'
	! pr #h_copy_cmd: 'copy "'&os_filename$(env$('temp')&'\acs\brCsInstall\ACS 5 Client\br_parms.txt')&'" "'&os_filename$('S:\')&'\*.*"'
	pr #h_copy_cmd: 'type "'&os_filename$(env$('windir')&'\brListener.conf')&'"' ! just to see some nice info on the screen.
	dim windowsSystem32bitFolder$*256
	if exists(os_filename$(env$('SystemRoot')&'\SysWOW64')) then
		windowsSystem32bitFolder$=os_filename$(env$('SystemRoot')&'\SysWOW64')
	else
		windowsSystem32bitFolder$=os_filename$(env$('SystemRoot')&'\System32')
	end if
	pr #h_copy_cmd: 'copy "'&os_filename$('S:\brlistener'&env$('BRListener_Version')&'-'&env$('BR_Architecture')&'-'&env$('BRListener_Date')&'.exe')&'" "'&windowsSystem32bitFolder$&'\brListener.exe"'
	pr #h_copy_cmd: '"'&os_filename$('S:\brListenerInstaller-'&env$('BR_Architecture')&'.exe')&'" /release'
	pr #h_copy_cmd: '"'&os_filename$('S:\brListenerInstaller-'&env$('BR_Architecture')&'.exe')&'"'
    pr #h_copy_cmd:     'pause'
	close #h_copy_cmd:
	execute 'sy -c explorer "'&env$('temp')&'\acs\brCsInstall\"' ! Install_BR_Server_'&session$&'.cmd"'
fnend
def fn_server_uninstall
  dim sd_br_server_executable$*1024
  fnureg_read('CS Server Activate Executable',sd_br_server_executable$)
  if sd_br_server_executable$<>'' then
  end if
	fnStatus('fn_server_uninstall')
	fnMakeSurePathExists('[temp]\acs\brCsInstall\')
	open #h_copy_cmd=fnH: 'Name='&env$('temp')&'\acs\brCsInstall\Remove_BR_Server_'&session$&'.cmd,replace,recl=256',d,o
	pr #h_copy_cmd: '"'&os_filename$('S:\brListenerInstaller-'&env$('BR_Architecture')&'.exe')&'" /release'
	pr #h_copy_cmd: 'del "'&os_filename$(env$('windir')&'\brListener.conf')&'"'
	pr #h_copy_cmd: 'del "'&os_filename$(env$('SystemRoot')&'\System32\brListener.exe')&'"'
	close #h_copy_cmd:
	! execute 'sy "'&env$('temp')&'\acs\brCsInstall\Remove_BR_Server_'&session$&'.cmd"'
	execute 'sy -C explorer "'&env$('temp')&'\acs\brCsInstall\"' ! Remove_BR_Server_'&session$&'.cmd"'
fnend
def fn_server_is_active ! unused
	dim sia_br_server_executable$*1024
	fnureg_read('CS Server Activate Executable',sia_br_server_executable$)
	if sia_br_server_executable$<>'' then
		sia_return=1
	else
		sia_return=0
	end if
	fn_server_is_active=sia_return
fnend
execute 'Sy -w '&os_filename$('S:\Core\ACS_PrAce_Support_Install_ocx.exe')
execute 'Sy '&os_filename$('S:\Core\ACS_PrAce_Reg.cmd')&' /s'
Xit: fnXit

include: fn_setup
