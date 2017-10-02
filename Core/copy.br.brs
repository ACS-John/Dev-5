00010 ! Replace S:\Core\copy.br
50020 ! <Updateable Region: ERTN
50040 ERTN: fnerror(program$,err,line,act$,"xit")
50060   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
50080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
50100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
50120 ERTN_EXEC_ACT: execute act$ : goto ERTN
50140 ! /region
52000 def library fnCopy(from$*256,to$*256; new_record_length,options$)
52020   ! options$ (seperate by space)  supported options$ values inclue
52040   !           recursive - includes all subdirectories and their files
52060   library 'S:\Core\Library': fnerror,fnstatus,fngethandle,fnMakeSurePathExists,fngetdir2,fngetpp
52080   on error goto ERTN
52100   from$=trim$(from$,'"')
52120   to$=trim$(to$,'"')
52140   options$=rtrm$(options$)&' ' 
52160   copyRecursive=0
52170    if from$(1:2)='@:' then let fromAt$='@:' else fromAt$=''
52172    if to$(1:2)='@:' then let toAt$='@:' else toAt$=''
52180   if pos(lwrc$(options$),'recursive ') then copyRecursive=1
52200   fnMakeSurePathExists(to$)
54000   ! if new_record_length and as_admin then 
54020   !   pr 'sorry new_record_length and as_admin are exclusive options and you can not use both' 
54040   !   pause 
54060   !   goto COPY_XIT
56000   ! else if as_admin then
56020   !   ! copy_return does not yet work for as_admin.....  perhaps check errorlevel or something.
56030   !   ! copy_return does not yet work for as_admin.....  perhaps check errorlevel or something.
56040   !   exec 'copy "S:\Core\Run_As_Admin.cmd" "'&env$('temp')&'\copy_as_admin_'&session$&'.cmd"'
56060   !   open #h_copy_cmd:=fngethandle: 'Name='&env$('temp')&'\copy_as_admin_'&session$&'.cmd',d,o
56080   !   pr #h_copy_cmd:     'echo copy "'&os_filename$(from$)&'" "'&os_filename$(to$)&'"'
56100   !   pr #h_copy_cmd:     'copy "'&os_filename$(from$)&'" "'&os_filename$(to$)&'"'
56140   !   close #h_copy_cmd:
56160   !   execute 'sy "'&env$('temp')&'\copy_as_admin_'&session$&'.cmd"'
58000   ! else ! either new_record_length or (neither new_record_length nor as_admin)
58020     if copyRecursive then
58040       fngetpp(from$,fromPath$,fromFile$,fromExt$)
58060       fngetpp(to$,toPath$,toFile$,toExt$)
58080       dim fromPath$*256,fromFile$*256,fromExt$*256
58100       dim toPath$*256,toFile$*256,toExt$*256
58120       ! pr 'fromFile$=';fromFile$
58140       ! pr 'fromExt$=';fromExt$
58160       ! pr 'toFile$=';toFile$
58180       ! pr 'toExt$=';toExt$
58200       ! pause
58220       ! execute 'copy "'&toPath$&'" "'&env$('temp')&'\acs_recl_chg_'&session$&'" '&parameters$ ioerr COPY_FAIL
58240       dim copyFromFolder$(0)*256
58260       gd2_return=fngetdir2(fromPath$,mat copyFromFolder$,'/s /b /ad')
58300       ! 
58320       ! pr 'gd2_return=';gd2_return : pause
58340       for cfi=1 to udim(mat copyFromFolder$)
58360         dim copyToFolder$*256
58380         copyToFolder$=toPath$&(copyFromFolder$(cfi)(len(srep$(fromPath$,fromAt$,''))+1:inf))
58400         fnmakesurepathexists(copyToFolder$)
58410         fnStatus ('Creating files  in "'&copyToFolder$&'"') 
58420         execute 'copy "'&fromat$&copyfromfolder$(cfi)&'\'&fromfile$&fromext$&'" "'&toat$&copyToFolder$&'\*.*"' ioerr copyFailA ! ignore because not all folders have files in them
58440         copy_return+=1! if int(cfi/10)=cfi/10 then pause
58450         copyFailA: ! 
58460       nex cfi
58480       ! pause
59000     else
59020       if new_record_length then 
59040         dim parameters$*128
59060         parameters$=''
59080         parameters$=parameters$&' -'&str$(abs(new_record_length))
59100         if new_record_length and uprc$(from$)=uprc$(to$) then 
59120           execute 'copy "'&from$&'" "'&env$('temp')&'\acs_recl_chg_'&session$&'" '&parameters$ ioerr COPY_FAIL
59140           execute 'copy "'&env$('temp')&'\acs_recl_chg_'&session$&'" "'&to$&'"' ioerr COPY_FAIL
59160           execute 'free "'&env$('temp')&'\acs_recl_chg_'&session$&'"' ioerr ignore
59180         end if 
59200       end if 
59220       execute 'copy "'&from$&'" "'&to$&'"' ioerr COPY_FAIL
59240       copy_return=1
59260     end if
59280   ! end if
60000   goto COPY_XIT
64000   COPY_FAIL: ! r:
64020     copy_return=min(-1,-err)
64040     if new_record_length then 
64060       execute 'Copy "'&from$&'" "'&env$('Temp')&'\tmp_rln_chg_s'&session$&'"' ioerr COPY_RETRY_NEW_RLN_FAILED
64080       execute 'Copy "'&env$('Temp')&'\tmp_rln_chg_s'&session$&'" "'&to$&'" -'&str$(abs(new_record_length)) ioerr COPY_RETRY_NEW_RLN_FAILED
64100       execute 'Free "'&env$('Temp')&'\tmp_rln_chg_s'&session$&'"' ioerr ignore
64120       copy_return=2
65000     else if env$("ACSDeveloper")<>"" then 
65020       pr 'first copy failed with error ';err
65040       pr 'From: "'&from$&'"'
65060       pr '  To: "'&to$&'"'
65140       pause 
65160     end if 
65180   goto COPY_XIT ! /r
66000   COPY_RETRY_NEW_RLN_FAILED: ! r:
66020     if env$("ACSDeveloper")<>"" then 
66040       pr 'first copy failed with error ';abs(copy_return)
66060       pr 'second attempt failed with error ';err
66080       pause 
66100     end if 
66120     copy_return=copy_return*10000-err
66140   goto COPY_XIT ! /r
68000   COPY_XIT: ! 
68020   fnCopy=copy_return
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
80112     source$=source$(2:len(source$))
80116     copy_from_client=1
80117   else 
80118     copy_from_server=1
80120   end if 
80124   if destination$(1:1)="@" then 
80128     destination$=destination$(2:len(destination$))
80132     copy_to_client=1
80133   else 
80134     copy_to_server=1
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
80220   ! pr #20: "open "&RTRM$(SERVERIP$)
80230   pr #20: "WO"&str$(val(wsid$)-50) ! env$("LOGIN_NAME")
80240   pr #20: "WOCS"&str$(val(wsid$)-50)
80250   pr #20: "put "&rtrm$(source$)&" "&rtrm$(destination$)
80260   pr #20: "bye"
80270   close #20: 
80280   open #20: "Name=csCopy"&wsid$&".bat,Size=0,RecL=255,Replace",display,output 
80290   pr #20: "ftp -s:ftp"&wsid$&".tmp "&rtrm$(serverip$)
80300   pr #20: "pause"
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

