00010 ! Replace S:\Core\copy.br
50020 ! <Updateable Region: ERTN
50040 ERTN: let fnerror(program$,err,line,act$,"xit")
50060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
50120 ERTN_EXEC_ACT: execute act$ : goto ERTN
50140 ! /region
52000 def library fnCopy(from$*256,to$*256; new_record_length)
52020   library 'S:\Core\Library': fnerror,fnstatus,fngethandle,fnMakeSurePathExists
52040   on error goto ERTN
52060   let from$=trim$(from$,'"')
52080   let to$=trim$(to$,'"')
52090   fnMakeSurePathExists(to$)
54000   ! if new_record_length and as_admin then 
54020   !   pr 'sorry new_record_length and as_admin are exclusive options and you can not use both' 
54040   !   pause 
54060   !   goto COPY_XIT
56000   ! else if as_admin then
56020   !   ! copy_return does not yet work for as_admin.....  perhaps check errorlevel or something.
56030   !   ! copy_return does not yet work for as_admin.....  perhaps check errorlevel or something.
56040   !   exec 'copy "S:\Core\Run_As_Admin.cmd" "'&env$('temp')&'\copy_as_admin_'&session$&'.cmd"'
56060   !   open #h_copy_cmd:=fngethandle: 'Name='&env$('temp')&'\copy_as_admin_'&session$&'.cmd',d,o
56080   !   print #h_copy_cmd:     'echo copy "'&os_filename$(from$)&'" "'&os_filename$(to$)&'"'
56100   !   print #h_copy_cmd:     'copy "'&os_filename$(from$)&'" "'&os_filename$(to$)&'"'
56140   !   close #h_copy_cmd:
56160   !   execute 'sy "'&env$('temp')&'\copy_as_admin_'&session$&'.cmd"'
58000   ! else ! either new_record_length or (neither new_record_length nor as_admin)
58020     dim parameters$*128
58040     let parameters$=''
58060     if new_record_length then 
58080       let parameters$=parameters$&' -'&str$(abs(new_record_length))
58100       if new_record_length and uprc$(from$)=uprc$(to$) then 
58120         execute 'copy "'&from$&'" "'&env$('temp')&'\acs_recl_chg_'&session$&'" '&parameters$ ioerr COPY_FAIL
58140         execute 'copy "'&env$('temp')&'\acs_recl_chg_'&session$&'" "'&to$&'"' ioerr COPY_FAIL
58160         execute 'free "'&env$('temp')&'\acs_recl_chg_'&session$&'"' ioerr ignore
58180       end if 
58200     end if 
58220     execute 'copy "'&from$&'" "'&to$&'"'&parameters$ ioerr COPY_FAIL
58240     let copy_return=1
58260   ! end if
60000   goto COPY_XIT
64000   COPY_FAIL: ! r:
64020   let copy_return=min(-1,-err)
64040   if new_record_length then 
64060     execute 'Copy "'&from$&'" "'&env$('Temp')&'\tmp_rln_chg_s'&session$&'"' ioerr COPY_RETRY_NEW_RLN_FAILED
64080     execute 'Copy "'&env$('Temp')&'\tmp_rln_chg_s'&session$&'" "'&to$&'" -'&str$(abs(new_record_length)) ioerr COPY_RETRY_NEW_RLN_FAILED
64100     execute 'Free "'&env$('Temp')&'\tmp_rln_chg_s'&session$&'"' ioerr ignore
64120     let copy_return=2
65000   else if env$("ACSDeveloper")<>"" then 
65020     print 'first copy failed with error ';err
65040     pr 'From: "'&from$&'"'
65060     pr '  To: "'&to$&'"'
65140     pause 
65160   end if 
65180   goto COPY_XIT ! /r
66000   COPY_RETRY_NEW_RLN_FAILED: ! r:
66020   if env$("ACSDeveloper")<>"" then 
66040     print 'first copy failed with error ';abs(copy_return)
66060     print 'second attempt failed with error ';err
66080     pause 
66100   end if 
66120   let copy_return=copy_return*10000-err
66140   goto COPY_XIT ! /r
68000   COPY_XIT: ! 
68020   let fnCopy=copy_return
68040 fnend 
80040 def library fncscopy(&source$,&destination$)
80042   ! client server copy function
80050   library 'S:\Core\Library': fnerror
80060   on error goto ERTN
80070   ! source$ = the file to copy from
80071   ! destination$ = file to copy to
80072   ! (start either source$ or destination$ with a @ in pos 1 to specify it's location is on the client)
80080   dim serverip$*20
80090   open #20: "Name=ServerIP.txt",display,input 
80100   linput #20: serverip$
80104   close #20: 
80108   if source$(1:1)="@" then 
80112     let source$=source$(2:len(source$))
80116     let copy_from_client=1
80117   else 
80118     let copy_from_server=1
80120   end if 
80124   if destination$(1:1)="@" then 
80128     let destination$=destination$(2:len(destination$))
80132     let copy_to_client=1
80133   else 
80134     let copy_to_server=1
80136   end if 
80140   if copy_from_client=1 and copy_to_server=1 then 
80144     gosub COPY_FROM_CLIENT_TO_SERVER
80148   end if 
80152   if copy_from_client=1 and copy_to_client=1 then 
80156     gosub COPY_FROM_CLIENT_TO_CLIENT
80160   end if 
80164   if copy_from_server=1 and copy_to_client=1 then 
80168     gosub COPY_FROM_SERVER_TO_CLIENT
80172   end if 
80176   if copy_from_server=1 and copy_to_server=1 then 
80180     gosub COPY_FROM_SERVER_TO_SERVER
80184   end if 
80188   goto XIT
80190   ! ______________________________________________________________________
80200   COPY_FROM_CLIENT_TO_SERVER: ! 
80210   open #20: "Name=ftp"&wsid$&".tmp,Size=0,RecL=255,Replace",display,output 
80220   ! Print #20: "open "&RTRM$(SERVERIP$)
80230   print #20: "WO"&str$(val(wsid$)-50) ! env$("LOGIN_NAME")
80240   print #20: "WOCS"&str$(val(wsid$)-50)
80250   print #20: "put "&rtrm$(source$)&" "&rtrm$(destination$)
80260   print #20: "bye"
80270   close #20: 
80280   open #20: "Name=csCopy"&wsid$&".bat,Size=0,RecL=255,Replace",display,output 
80290   print #20: "ftp -s:ftp"&wsid$&".tmp "&rtrm$(serverip$)
80300   print #20: "pause"
80310   close #20: 
80320   execute "Sy csCopy"&wsid$&".bat"
80330   return 
80340   ! ______________________________________________________________________
80350   COPY_FROM_CLIENT_TO_CLIENT: pause 
80360   return 
80370   ! ______________________________________________________________________
80380   COPY_FROM_SERVER_TO_CLIENT: pause 
80390   return 
80400   ! ______________________________________________________________________
80410   COPY_FROM_SERVER_TO_SERVER: pause 
80420   return 
80520   XIT: ! 
80522 fnend 
82000 def library fnFree(fileToDelete$*256)
82020   fileToDelete$=trim$(fileToDelete$,'"')
82040   execute 'Free "'&fileToDelete$&'"' ioerr ignore
82060 fnend
84000 def library fnRename(from$*256,to$*256)
84020   from$=trim$(from$,'"')
84040   to$=trim$(to$,'"')
84060   execute 'Rename "'&from$&'" "'&to$&'"'
84080 fnend
