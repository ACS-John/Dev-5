12000   if ~setup then let fn_setup
12040   fntop(program$, cap$="Client Server")
14000   do 
14020     fntos(sn$="test-Button")
14040     fnlbl(2,2,'Server IP or Name:',19,1)
14060     fntxt(2,22,20,128,0,'',0,'localhost for single user, IP Address for internet based access')
14080     fnureg_read('CS Server Name',server_name$)
14100     if server_name$='' and env$('user_limit')='1' then 
14120       server_name$='localhost'
14140     else if server_name$='' then 
14160       server_name$=env$('computername')
14180     end if 
14200     resp$(1)=server_name$
14220 ! 
14240     fnlbl(4,2,'Port:',19,1)
14260     fntxt(4,22,4,0,0,'number',0,'Default is 8555')
14280     fnureg_read('CS Server Port',cs_port$) : if cs_port$='' or cs_port$='0' then cs_port$='8555'
14300     resp$(2)=cs_port$
14320     fnlbl(6,2,'Anonymous User:',19,1)
14340     fntxt(6,22,20,128,0,'',0,'leave blank to disable')
14360     fnureg_read('CS Anonymous User',anon_user$)
14380     resp$(3)=anon_user$
14400     fnlbl(7,2,'Anonymous Password:',19,1)
14420     fntxt(7,22,20,128,0,'',0,'leave blank to disable')
14440     fnureg_read('CS Anonymous Password',anon_pass$)
14460     resp$(4)=anon_pass$
14480 ! 
15000     fncmdkey("Install Server",2,1,0)
15020     fncmdkey("Uninstall Server",3,0,0)
15040     fncmdkey('&Back',5,0,1)
15060     fnacs(sn$,0,mat resp$,ck)
16000     server_name$=resp$(1) : fnureg_write('CS Server Name',server_name$)
16020     cs_port$=resp$(2) : fnureg_write('CS Server Port',cs_port$)
16040     anon_user$=resp$(3) : fnureg_write('CS Anonymous User',anon_user$)
16060     anon_pass$=resp$(4) : fnureg_write('CS Anonymous Password',anon_pass$)
16080     if ck=5 then goto XIT
16100     if ck=2 then let fn_server_install
16120     if ck=3 then let fn_server_uninstall
16140     fnstatus_close
16160   loop 
34000   def fn_server_install
34020     fnstatus('fn_server_install')
34030     if ~exists(env$('temp')&'\acs\brCsInstall') then execute 'mkdir "'&env$('temp')&'\acs\brCsInstall"'
34031     if ~exists(env$('temp')&'\acs\brCsInstall\ACS 5 Client') then exe 'mkdir "'&env$('temp')&'\acs\brCsInstall\ACS 5 Client"'
34032     if ~exists(env$('temp')&'\acs\brCsInstall\ACS 5 Client\Core') then exe 'mkdir "'&env$('temp')&'\acs\brCsInstall\ACS 5 Client\Core"'
34034     fnCopy('S:\brclient*.*',env$('temp')&'\acs\brCsInstall\ACS 5 Client\*.*')
34036     execute 'sy xcopy "'&os_filename$('S:\Core')&'\Client\*.*" "'&os_filename$(env$('temp')&'\acs\brCsInstall\ACS 5 Client')&'\*.*" /S'
34040     fnstatus('make '&env$('Q')&'\brListener.conf')
34060     open #h_br_parms_txt:=fngethandle: 'Name='&env$('temp')&'\acs\brCsInstall\ACS 5 Client\br_parms.txt,RecL=256,replace',display,output 
34080     pr #h_br_parms_txt: 'host='&server_name$
34100     pr #h_br_parms_txt: 'label=ACS_5_CS'
34120     close #h_br_parms_txt: 
34140     open #h_brlistener_conf:=fngethandle: 'Name='&env$('Q')&'\brListener.conf,RecL=256,replace',display,output 
34160     pr #h_brlistener_conf: 'LogFile='&env$('temp')&'\acs-Log-CS.txt'
34180     pr #h_brlistener_conf: 'LogLevel=10'
34200     pr #h_brlistener_conf: '['
34220     pr #h_brlistener_conf: 'Label=ACS_5_CS'
34240     pr #h_brlistener_conf: 'StartDir='&env$('temp')&'\' ! startdir does not support quotes
34260     pr #h_brlistener_conf: 'Executable="'&env$("STATUS.FILES.EXECUTABLE")&'"' ! br_prog$&br_ext$
34280     pr #h_brlistener_conf: 'Config="'&os_filename$('S:\brconfig.sys')&'"' ! br_prog$&br_ext$
34300     pr #h_brlistener_conf: 'Caption="ACS 5"'
34305     if anon_user$<>'' and anon_pass$<>'' then 
34310       pr #h_brlistener_conf: 'Anonymous="'&anon_user$&'@'&anon_pass$&'"'
34315     end if 
34320     if cs_port$<>'8555' and cs_port$<>'0' and cs_port$<>'' then 
34340       pr #h_brlistener_conf: 'Port='&cs_port$
34360     end if 
34380     pr #h_brlistener_conf: 'MultiSession'
34400     pr #h_brlistener_conf: ']'
34420     close #h_brlistener_conf: 
34440     fnstatus('  and copy it into windows')
34460     fnstatus('  and copy DLL to 32 bit system folder (System32 or SysWOW64)')
34480 !   execute 'copy "S:\Core\Run_As_Admin.cmd" "'&env$('temp')&'\acs\brCsInstall\Install_BR_Server_'&session$&'.cmd"'
34500     open #h_copy_cmd:=fngethandle: 'Name='&env$('temp')&'\acs\brCsInstall\Install_BR_Server_'&session$&'.cmd,replace,recl=256',display,output 
34520 !     pr #h_copy_cmd:     '@echo on'
34540     pr #h_copy_cmd: 'copy "'&os_filename$(env$('Q')&'\brListener.conf')&'" "'&os_filename$(env$('windir')&'\brListener.conf')&'"'
34560     ! pr #h_copy_cmd: 'copy "'&os_filename$(env$('temp')&'\acs\brCsInstall\ACS 5 Client\br_parms.txt')&'" "'&os_filename$('S:\')&'\*.*"'
34580     pr #h_copy_cmd: 'type "'&os_filename$(env$('windir')&'\brListener.conf')&'"' ! just to see some nice info on the screen.
35000     dim windowsSystem32bitFolder$*256
35020     if exists(os_filename$(env$('SystemRoot')&'\SysWOW64')) then
35040       windowsSystem32bitFolder$=os_filename$(env$('SystemRoot')&'\SysWOW64')
35060     else
35080       windowsSystem32bitFolder$=os_filename$(env$('SystemRoot')&'\System32')
35100     end if
35120     pr #h_copy_cmd: 'copy "'&os_filename$('S:\brlistener'&env$('BRListener_Version')&'-'&env$('BR_Architecture')&'-'&env$('BRListener_Date')&'.exe')&'" "'&windowsSystem32bitFolder$&'\brListener.exe'&'"'
35140     pr #h_copy_cmd: '"'&os_filename$('S:\brListenerInstaller-'&env$('BR_Architecture')&'.exe')&'" /release'
35160     pr #h_copy_cmd: '"'&os_filename$('S:\brListenerInstaller-'&env$('BR_Architecture')&'.exe')&'"'
35180 !     pr #h_copy_cmd:     'pause'
35200     close #h_copy_cmd: 
35220     execute 'sy -c explorer "'&env$('temp')&'\acs\brCsInstall\"' ! Install_BR_Server_'&session$&'.cmd"'
35240   fnend 
38000   def fn_server_uninstall
38020 !   dim sd_br_server_executable$*1024
38040 !   fnureg_read('CS Server Activate Executable',sd_br_server_executable$)
38060 !   if sd_br_server_executable$<>'' then
38080 !   end if
38100     fnstatus('fn_server_uninstall')
38120     fnmakesurepathexists(env$('temp')&'\acs\brCsInstall\')
38140     open #h_copy_cmd:=fngethandle: 'Name='&env$('temp')&'\acs\brCsInstall\Remove_BR_Server_'&session$&'.cmd,replace,recl=256',display,output 
38160     pr #h_copy_cmd: '"'&os_filename$('S:\brListenerInstaller-'&env$('BR_Architecture')&'.exe')&'" /release'
38180     pr #h_copy_cmd: 'del "'&os_filename$(env$('windir')&'\brListener.conf')&'"'
38200     pr #h_copy_cmd: 'del "'&os_filename$(env$('SystemRoot')&'\System32\brListener.exe')&'"'
38260     close #h_copy_cmd: 
38280     ! execute 'sy "'&env$('temp')&'\acs\brCsInstall\Remove_BR_Server_'&session$&'.cmd"'
38290     execute 'sy -C explorer "'&env$('temp')&'\acs\brCsInstall\"' ! Remove_BR_Server_'&session$&'.cmd"'
38300   fnend 
42000   def fn_server_is_active
42020     dim sia_br_server_executable$*1024
42040     fnureg_read('CS Server Activate Executable',sia_br_server_executable$)
42060     if sia_br_server_executable$<>'' then 
42080       sia_return=1
42100     else 
42120       sia_return=0
42140     end if 
42160     fn_server_is_active=sia_return
42180   fnend 
53000   execute 'Sy -w '&os_filename$('S:\Core\ACS_PrAce_Support_Install_ocx.exe')
53020   execute 'Sy '&os_filename$('S:\Core\ACS_PrAce_Reg.cmd')&' /s'
60000 XIT: fnxit
62000   def fn_setup
62020     if ~setup then 
62040       setup=1
62060       library 'S:\Core\Library': fntop,fnxit,fnerror,fnbutton_or_disabled,fnacs,fncmdkey,fnbutton,fnCopy,fnureg_read,fnureg_write,fntos,fngetpp,fnstatus,fngethandle,fnlbl,fntxt,fnstatus_close
62070       library 'S:\Core\Library': fnmakesurepathexists
62080       on error goto ERTN
62100       dim cap$*128
62110       dim server_name$*128,resp$(10)*256
62140       dim anon_user$*128
62160       dim anon_pass$*128
62480     end if 
70990   fnend 
72000 IGNORE: continue 
74000 ! <Updateable Region: ERTN>
74040 ERTN: fnerror(program$,err,line,act$,"xit")
74060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
74080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
74100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
74120 ERTN_EXEC_ACT: execute act$ : goto ERTN
74140 ! /region
