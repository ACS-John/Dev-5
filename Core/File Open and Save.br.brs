18000 def fn_setup
18010   if ~setup then
18020     setup=1
18028     library 'S:\Core\Library': fnsave_as_path$,fngethandle,fnreg_close,fnreg_write
18030     library 'S:\Core\Library': fnmsgbox,fntext_editor,fnSystemName$,fnlog
18032     library 'S:\Core\Library': fnacs,fncmdset,fntos,fnlbl,fntxt,fncomboa
18034     library 'S:\Core\Library': fnputcno,fncursys$,fncheckfileversion,fnmakesurepathexists
18036     library 'S:\Core\Library': fnstatus,fnstatus_close,fnstatus_pause,fnCopy,fnindex_sys
18038     library 'S:\Core\Library': fnaddonec
18040     dim company_import_path$*256
18050     dim resp$(5)*256
18060     dim ml$(0)*128
18070   end if
18080 fnend
19000 ignore: continue
22000 def library fnFileOpen
22020   if ~setup then let fn_setup
22040   fnFileOpen=fn_fileOpen
22060 fnend
24000 def fn_fileOpen(; file_open$*256)
24020   if file_open$='' then
24040     execute 'free '&br_filename$(env$('client_temp')&'\Open_Log.txt') ioerr ignore
24060     open #h_tmp:=fngethandle: "Name=OPEN:"&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OPEN_OPEN_ERR
24080     let file_open$=os_filename$(file$(h_tmp))
24100     close #h_tmp: 
24120   end if
24140   let fnreg_close
24160   ! r: new way 12/4/2015
24180   open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\open_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
24200   print #h_tmp: '@echo off'
24220   print #h_tmp: '@echo Advanced Computer Services LLC'
24240   print #h_tmp: '@echo Opening: "'&file_open$&'"'
24260   print #h_tmp: '@echo.'
24280   print #h_tmp: '@echo.'
24300   print #h_tmp: '@echo Command: '&env$('path_to_7z_exe')&' x -r -aoa "'&file_open$&'" -o"'&os_filename$(env$('Q')&'\')&'" > "'&env$('temp')&'\Open_Log.txt"'
24320   print #h_tmp: '@echo.'
24340   print #h_tmp: '@echo.'
24360   print #h_tmp: '@echo Relative To: '&os_filename$(env$('Q')&'\')
24380   print #h_tmp: '@echo.'
24400   print #h_tmp: '@echo.'
24420   print #h_tmp: '@echo Output Log: "'&env$('client_temp')&'\Open_Log.txt"'
24440   print #h_tmp: '@echo.'
24460   print #h_tmp: '@echo.'
24480   print #h_tmp: '@echo OPEN PROCESSING...'
24500   print #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&file_open$&'" -o"'&os_filename$(env$('Q')&'\')&'" > "'&env$('client_temp')&'\Open_Log.txt"'
24520   close #h_tmp: 
24540   execute 'sy '&env$('client_temp')&'\open_as_'&session$&'.cmd'
24560   ! /r
24580   if fn_analyze_7zip_compresslog(env$('client_temp')&'\Open_Log.txt','Successfully Opened',file_open$) then 
24600     let fnreg_write('Last Open Date',date$('ccyy/mm/dd'))
24620     let fnreg_write('Last Open File',file_open$(pos(file_open$,'\',-1)+1:len(file_open$)))
24640     let fnreg_write('Last Open Path',file_open$(1:pos(file_open$,'\',-1)))
24660   end if 
24680   goto OPEN_XIT
24700   OPEN_OPEN_ERR: ! 
24720   if err=622 then ! it was just cancelled
24740     print 'cancelled' : goto OPEN_XIT
24760   else 
24780     mat ml$(2)
24800     let ml$(1)='Select a different file name.'
24820     let ml$(2)='Error: '&str$(err)
24840     let fnmsgbox(mat ml$)
24860     !     if err=4150 then print "Could not create file:";file$(1) : let fnpause ! file$(1) is blank!
24880     print "Err:";err;" Line:";line
24900   end if 
24920   OPEN_XIT: ! 
24940 fnend
26000 def library fnFileSaveAs(save_what$)
26020   if ~setup then let fn_setup
26040   fnFileSaveAs=fn_FileSaveAs(save_what$)
26060 fnend
28000 def fn_FileSaveAs(save_what$; fsa_automatedSaveFileName$*256,suppressErrorLog)
28020   dim save_name$*256,ln$*512
28040   dim save_log_filename$*256
28060   let failure=0
28080   let save_log_filename$=env$('client_temp')&'\Save_As_Log.txt'
28100   execute 'free '&br_filename$(save_log_filename$) ioerr ignore
28120   if fsa_automatedSaveFileName$<>'' then
28140     save_name$=fsa_automatedSaveFileName$
28160   else
28180     open #h_tmp:=fngethandle: "Name=SAVE:"&fnsave_as_path$&"\*.zip,RecL=1,replace",external,output ioerr SAVE_AS_OPEN_ERR
28200     save_name$=os_filename$(file$(h_tmp))
28220     close #h_tmp,free: 
28240     fnCopy('S:\drive.sys',env$('Q')&'\*.*')
28260     fnCopy('S:\brserial.dat',env$('Q')&'\*.*')
28280   end if
28300   fnreg_close
28320   open #h_tmp:=fngethandle: 'Name=@:'&br_filename$(env$('client_temp')&'\save_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
28340   dim tmp7ZipCommand$*512
28360   if enableBackupReportCache$='True' then
28380     tmp7zipcommand$=env$('path_to_7z_exe')&' a -r -tzip "'&save_name$&'" "'&env$('Q')&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.wrk'
28400   else
28420     tmp7ZipCommand$=env$('path_to_7z_exe')&' a -r -tzip "'&save_name$&'" "'&env$('Q')&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.wrk -xr!"Report Cache\*"'
28440   end if
28460   print #h_tmp: '@echo off'
28480   print #h_tmp: '@echo Advanced Computer Services LLC'
28500   print #h_tmp: '@echo Saving to: "'&save_name$&'"'
28520   print #h_tmp: '@echo.'
28540   print #h_tmp: '@echo.'
28560   print #h_tmp: '@echo Command: '&tmp7ZipCommand$
28580   print #h_tmp: '@echo.'
28600   print #h_tmp: '@echo Save What: '&env$('Q')&save_what$
28620   print #h_tmp: '@echo.'
28640   print #h_tmp: '@echo Relative To: '&os_filename$(env$('Q')&'\')
28660   print #h_tmp: '@echo.'
28680   if enableBackupReportCache$<>'True' then
28700     print #h_tmp: '@echo Excluding Report Cache'
28720   end if
28740   print #h_tmp: '@echo.'
28760   print #h_tmp: '@echo.'
28780   print #h_tmp: '@echo Output Log: "'&save_log_filename$&'"'
28800   print #h_tmp: '@echo.'
28820   print #h_tmp: '@echo.'
28840   print #h_tmp: '@echo SAVE PROCESSING...'
28860   print #h_tmp: tmp7ZipCommand$&' > "'&save_log_filename$&'"'
28880   close #h_tmp: 
28900   execute 'sy '&env$('client_temp')&'\save_as_'&session$&'.cmd'
28920   if fsa_automatedSaveFileName$<>'' then
28940     if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$, 1,suppressErrorLog) then 
28960       let fnreg_write('Last Automated Save Date',date$('ccyy/mm/dd'))
28980       let fnreg_write('Last Automated Save Time',time$)
29000       let fnreg_write('Last Automated Save File',save_name$)
29020       let fnreg_write('Last Automated Save Path',save_name$(1:pos(save_name$,'\',-1)))
29040     end if 
29060   else
29080     if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$,0,suppressErrorLog) then 
29100       let fnreg_write('Last Save Date',date$('ccyy/mm/dd'))
29120       let fnreg_write('Last Save Time',time$)
29140       let fnreg_write('Last Save File',save_name$)
29160       let fnreg_write('Last Save Path',save_name$(1:pos(save_name$,'\',-1)))
29180     end if 
29200   end if
29220   ! 
29240   goto SAVE_AS_XIT
29260   SAVE_AS_OPEN_ERR: ! there was a problem opening the file.
29280   if fsa_automatedSaveFileName$<>'' then
29300     mat ml$(3)
29320     let ml$(1)='Automated save failed'
29340     let ml$(2)='Error: '&str$(err)
29360     let ml$(3)='File: '&fsa_automatedSaveFileName$
29380     let fnmsgbox(mat ml$)
29400     ! goto SAVE_AS_XIT
29420   ! else if err=622 then ! it was just cancelled
29440   !   goto SAVE_AS_XIT
29460   else if err<>622 then
29480     mat ml$(2)
29500     let ml$(1)='Select a different file name.'
29520     let ml$(2)='Error: '&str$(err)
29540     let fnmsgbox(mat ml$)
29560     print "Err:";err;" Line:";line
29580   end if 
29600   SAVE_AS_XIT: ! 
29620   !  let fn_fsa_clean_up
29640 fnend 
32000 def fn_analyze_7zip_compresslog(arc_filename$*256,success_text_line1$*256,save_name$*256; statusInsteadOfMsgBox,suppressErrorLog)
32020   open #h_compresslog:=fngethandle: 'Name=@:'&arc_filename$,display,input ioerr A7C_OPEN_ERR
32040   let failure=1
32060   do 
32080     linput #h_compresslog: ln$ eof ARC_EO_COMPRESSLOG
32100     if lwrc$(ln$)='everything is ok' then 
32120       let failure=0
32140     end if 
32160   loop 
32180   ARC_EO_COMPRESSLOG: ! 
32200   close #h_compresslog: 
32220   if failure then 
32240     fnlog(save_name$&': '&'FAILURE: '&success_text_line1$)
32260     if suppressErrorLog then
32280       fnCopy(arc_filename$,save_name$&'(failureLog).txt')
32300       fnstatus('Automated Save Point encountered had errors.')
32320       fnstatus('Automated Save Point log file made: "'&save_name$&'(failureLog).txt"')
32340     else
32360       mat ml$(4)
32380       let ml$(1)='An error occurred during the process.'
32400       let ml$(2)='The following log was created:'
32420       let ml$(3)=arc_filename$
32440       let ml$(4)='Display the log now?'
32460       let fnmsgbox(mat ml$,resp$,"ACS",4+64)
32480       if resp$="Yes" then 
32500         let fntext_editor(arc_filename$)
32520       end if 
32540     end if 
32560   else 
32580     fnlog(save_name$&': '&success_text_line1$)
32600     if statusInsteadOfMsgBox then 
32620       fnstatus(success_text_line1$)
32640       fnstatus(save_name$)
32660     else
32680       mat ml$(2)
32700       let ml$(1)=success_text_line1$
32720       let ml$(2)=save_name$
32740       let fnmsgbox(mat ml$,resp$,"ACS",0)
32760     end if
32780   end if 
32800   goto ARC_XIT
32820   A7C_OPEN_ERR: ! 
32840   mat ml$(2)
32860   let ml$(1)='FAILURE: The log file could not be opened.'
32880   let ml$(2)=arc_filename$
32900   let fnmsgbox(mat ml$,resp$,"ACS",0)
32920   ARC_XIT: ! 
32940   let fn_analyze_7zip_compresslog=~failure
32960 fnend 
34000 def library fnOpenPartial
34020   if ~setup then let fn_setup
34040   fnOpenPartial=fn_openPartial
34060 fnend
36000 def fn_7zFileListFromArchive(file_open$*512,mat filename$)
36020   dim gflfaTmpFile$*512
36040   gflfaTmpFile$=env$('client_temp')&'\acs\7zGetFileList'&session$&'.txt'
36060   open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\open_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
36080   print #h_tmp: '@echo off'
36100   print #h_tmp: '@echo Advanced Computer Services LLC'
36120   print #h_tmp: '@echo Reading file list from "'&file_open$&'"'
36140   print #h_tmp: env$('path_to_7z_exe')&' l "'&file_open$&'" > "'&gflfaTmpFile$&'"'
36160   close #h_tmp: 
36180   execute 'sy '&env$('client_temp')&'\open_as_'&session$&'.cmd'
36200   open #h_tmp:=fngethandle: 'Name=@:'&gflfaTmpFile$,display,input
36220   do 
36240     linput #h_tmp: ln$
36260   loop until ln$='------------------- ----- ------------ ------------  ------------------------'
36280   ln$=''
36300   fileCount=0
36320   do
36340     linput #h_tmp: ln$
36360     if ln$<>'------------------- ----- ------------ ------------  ------------------------' then
36380       ! lnDate$=ln$(1:10)
36400       ! lnTime$=ln$(12:19)
36420       mat filename$(fileCount+=1)
36440       filename$(fileCount)=ln$(54:len(ln$))
36460     end if
36480   loop until ln$='------------------- ----- ------------ ------------  ------------------------'
36520   close #h_tmp,free: 
36540   fn_7zFileListFromArchive=fileCount
36560 fnend
37000 def fn_fileListToArchiveList(mat fileList$,mat archiveList$)
37020   archiveCount=0
37030   mat archiveList$(archiveCount+=1)
37032   archiveList$(archiveCount)='(All Companies)'
37040   for fileItem=1 to udim(mat fileList$)
37060     posCompany=pos(lwrc$(fileList$(fileItem)),'company.h')
37080     if posCompany>0 then
37100       dim systemName$*40
37110       companyNumber$=fileList$(fileItem)(posCompany+9:len(fileList$(fileItem)))
37120       systemName$=fnSystemName$(fileList$(fileItem)(1:2))
37130       mat archiveList$(archiveCount+=1)
37140       archiveList$(archiveCount)=systemName$&' - Company '&companyNumber$
37150       mat archiveSysAbbr$(archiveCount)
37160       archiveSysAbbr$(archiveCount)=fileList$(fileItem)(1:2)
37170       mat archiveCNo(archiveCount)
37180       archiveCNo(archiveCount)=val(companyNumber$)
37200     end if
37220   nex fileItem
37240   fn_fileListToArchiveList=archiveCount
37260 fnend
38000 def fn_openPartial
38020   dim file_open$*256
38040   execute 'free '&br_filename$(env$('client_temp')&'\Open_Log.txt') ioerr ignore
38060   open #h_tmp:=fngethandle: "Name=OPEN:"&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OP_OP_ERR
38080   file_open$=os_filename$(file$(h_tmp))
38100   close #h_tmp: 
38120   dim fileList$(0)*256,archiveList$(0)*50
38140   fnstatus('Getting list of companies from "'&file_open$&'"...')
38160   fn_7zFileListFromArchive(file_open$,mat fileList$)
38180   fn_fileListToArchiveList(mat fileList$,mat archiveList$)
38200   fnstatus_close
38220   fnreg_close
38240   fn_opMain(file_open$)
38260   goto OP_XIT
38280   OP_OP_ERR: ! 
38300   if err=622 then ! it was just cancelled
38320     print 'cancelled' : goto OP_XIT
38340   else 
38360     mat ml$(2)
38380     ml$(1)='Select a different file name.'
38400     ml$(2)='Error: '&str$(err)
38420     fnmsgbox(mat ml$,resp$)
38440     !     if err=4150 then print "Could not create file:";file$(1) : let fnpause ! file$(1) is blank!
38460     print "Err:";err;" Line:";line
38480   end if 
38500   OP_XIT: ! 
38520 fnend
42000 def fn_opMain(file_open$*256)
42020   destination_company_number=val(env$('cno'))
42040   OpmAskWhichToOpen: ! r: screen
42060   let fntos(sn$="Open Partial")
42080   let col1_width=27 : let col2_pos=col1_width+2 : lc=rc=0
42100   let fnlbl(lc+=1,1,"Source File:",col1_width,1)
42120   let fntxt(lc,col2_pos,30,256,0,'',1,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
42140   resp$(rc+=1)=file_open$
42160   let fnlbl(lc+=1,1,"Source Company:",col1_width,1)
42180   fncomboa('compList',lc,col2_pos,mat archiveList$)
42200   let resp$(resp_fileSource:=rc+=1)=archiveList$(1)
42220   let fnlbl(lc+=1,1,"Destination Company Number:",col1_width,1)
42240   let fntxt(lc,col2_pos,5,5,0,'1030',0,'')
42250   let fnlbl(lc,col2_pos+7,"(only applies if a specific Source Company is selected)")
42260   let resp$(resp_cnoDestination:=rc+=1)=str$(destination_company_number)
42280   ! let fnlbl(lc+=1,1,"System Abbreviation:",col1_width,1)
42300   ! let fntxt(lc,col2_pos,2,2,0)
42320   ! let resp$(resp_SystemAbbr:=rc+=1)=cursys$
42340   let fncmdset(2)
42360   let fnacs(sn$,0,mat resp$,ckey)
42380   ! /r
42400   dim selectedSource$*128
42420   selectedSource$=resp$(resp_fileSource)
42440   sourceWhich=srch(mat archiveList$,selectedSource$)
42460   if ckey=5 or sourceWhich<=0 then 
42480     opScreenReturn=0
42500   else
42520     if selectedSource$='(All Companies)' then
42540       fn_fileOpen( file_open$)
42560       opScreenReturn=1 
42580     else 
44000       source_company_number=archiveCNo(sourceWhich)
44020       destination_company_number=val(resp$(resp_cnoDestination))
44040       cursys$=archiveSysAbbr$(sourceWhich)
44060       fnstatus('** Open Partial Settings **')
44080       fnstatus('Source File: '&file_open$)
44100       fnstatus('Source System: '&cursys$)
44120       fnstatus('Source Company Number: '&str$(source_company_number))
44140       fnstatus('Destination Company Number: '&str$(destination_company_number))
44160       fnstatus('**')
44180       fnstatus('Set current system to: '&cursys$&' from '&cursys_origional$)
44200       cursys$=fncursys$(cursys$)
44220       fnputcno(destination_company_number) : let cno=destination_company_number
44240       fnstatus('Set active Company Number to: '&str$(destination_company_number))
44260       ! 
44280       dim omSourceFilter$(0)*64
44300       if cursys$='UB' then
44320         mat omSourceFilter$(1)
44340         omSourceFilter$(1)='*.h'&str$(source_company_number)&' Notes.h'&str$(source_company_number)&'\*'
44360       else
44380         mat omSourceFilter$(1)
44400         omSourceFilter$(1)='*.h'&str$(source_company_number)
44420       end if
44440       fn_extract_appropriate_files(file_open$,mat omSourceFilter$,env$('client_temp')&'\acs\OpenPartial\')
46000       if fn_analyze_7zip_compresslog(env$('client_temp')&'\acs\OpenPartial_Log.txt','Successfully Opened '&fnSystemName$&' company '&env$('cno')&' from ',file_open$, 1) then 
46020         fnreg_write('Last Open Partial Date',date$('ccyy/mm/dd'))
46040         fnreg_write('Last Open Partial File',file_open$(pos(file_open$,'\',-1)+1:len(file_open$)))
46060         fnreg_write('Last Open Partial Path',file_open$(1:pos(file_open$,'\',-1)))
46080         fnreg_write('Last Open Partial System',env$('cursys'))
46100         fnreg_write('Last Open Partial Company Number',env$('cno'))
46120         fn_copy_files_in(env$('client_temp')&'\acs\OpenPartial\'&env$('cursys')&'mstr\','.h'&str$(source_company_number),val(env$('cno')))
46140         opScreenReturn=1 
46160         setenv('force_reindex','yes') 
46180         fncheckfileversion
46200         fnindex_sys(cno)
46210         fnstatus_close
46220         dim msgTmp$(0)*128
46240         fnaddonec(mat msgTmp$,'Completed.')
46260         fnaddonec(mat msgTmp$,'Company '&env$('cno')&' created from copy of company '&str$(source_company_number))
46280         fnaddonec(mat msgTmp$,'from the file: '&file_open$)
46300         fnmsgbox(mat msgTmp$)
46320       end if 
46340       goto OpmAskWhichToOpen
46360     end if
46380   end if
46400   fn_opMain=opScreenReturn
46420 fnend
52000 def fn_extract_appropriate_files(eafSourceFile$*256,mat eafSourceFilter$,eafDestinationFolder$*256)
52020   ! pr 'eafSourceFile$="'&eafSourceFile$&'"'
52040   ! pr 'eafSourceFilter$="'&eafSourceFilter$&'"'
52060   ! pr 'eafDestinationFolder$="'&eafSourceFilter$&'"'
52080   execute 'Sy RmDir "'&eafDestinationFolder$&'" /s /q'
52100   open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\acs\openPartial'&session$&'.cmd')&',RecL=512,Replace',display,output 
52120   print #h_tmp: '@echo off'
52140   print #h_tmp: '@echo Advanced Computer Services LLC'
52160   print #h_tmp: '@echo Opening: "'&eafSourceFile$&'"'
52180   print #h_tmp: '@echo.'
52200   print #h_tmp: '@echo.'
52220   for eafSourceFilterItem=1 to udim(mat eafSourceFilter$)
52240     print #h_tmp: '@echo Command('&str$(eafSourceFilterItem)&'): '&env$('path_to_7z_exe')&' x -r -aoa "'&eafSourceFile$&'" -o"'&eafDestinationFolder$&'" '&eafSourceFilter$(eafSourceFilterItem)&' > "'&env$('temp')&'\acs\OpenPartial_Log.txt"'
52260   nex eafSourceFilterItem
52280   print #h_tmp: '@echo.'
52300   print #h_tmp: '@echo.'
52320   print #h_tmp: '@echo Relative To: '&eafDestinationFolder$
52340   print #h_tmp: '@echo.'
52360   print #h_tmp: '@echo.'
52380   print #h_tmp: '@echo Output Log: "'&env$('client_temp')&'\acs\OpenPartial_Log.txt"'
52400   print #h_tmp: '@echo.'
52420   print #h_tmp: '@echo.'
52440   print #h_tmp: '@echo OPEN PROCESSING...'
52460
52480   for eafSourceFilterItem=1 to udim(mat eafSourceFilter$)
52500     print #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&eafSourceFile$&'" -o"'&eafDestinationFolder$&'" '&eafSourceFilter$(eafSourceFilterItem)&' > "'&env$('client_temp')&'\acs\OpenPartial_Log.txt"'
52520   nex eafSourceFilterItem
52540   close #h_tmp: 
52560   execute 'sy '&env$('client_temp')&'\acs\openPartial'&session$&'.cmd'
52580     ! if env$('acsDeveloper')<>'' and env$('cursys')='UB' then pr 'Notes..h### should be extracted too' : pause
52990 fnend
54000 def fn_copy_files_in(company_import_path$*256,company_import_extension$,destination_company_number)
54020   execute 'free "'&env$('Q')&'\'&env$('cursys')&'mstr\*.h'&str$(destination_company_number)&'"' ioerr ignore
54040   let fnstatus('Existing files ('&os_filename$(env$('Q')&'\'&env$('cursys')&'mstr\*.h'&str$(destination_company_number))&') have been removed.')
54060   cfiReturn=fnCopy(company_import_path$&'*'&company_import_extension$,env$('Q')&'\'&env$('cursys')&'mstr\*.h'&str$(destination_company_number))
54080   if cfiReturn>0 then
54100     if env$('cursys')='UB' then
54120       cfiReturn=fn_ub_copy_extras(company_import_path$,company_import_extension$,destination_company_number)
54140     end if
54160     if cfiReturn>0 then
54180       let fnstatus('Import data copied in.')
54200     end if
54220   end if
54240   fn_copy_files_in=cfiReturn
54260 fnend 
58000 def fn_ub_copy_extras(company_import_path$*256,company_import_extension$,destination_company_number)
58020   ! r: import rates
58040   if exists(company_import_path$&'ubdata') then 
58060     if exists(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number)) then 
58080       execute 'free "'&env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number)&'"'
58100     end if  ! exists(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
58120     uceReturn=fnCopy(company_import_path$&'ubdata\*'&company_import_extension$,env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
58140     if uceReturn>0 then
58160       let fnstatus('UBmstr\ubData found in source and is replacing destination.')
58180     end if
58440   else 
58460     let fnstatus('UBmstr\ubData did not exist in source. Destination ubData remains unchanged.')
58680   end if 
58700   ! /r
58720   ! r: import notes folder
58740   if exists(company_import_path$&'UBmstr\notes'&company_import_extension$) then 
58760     execute 'free "'&env$('Q')&'\'&env$('cursys')&'mstr\notes.h'&str$(destination_company_number)&'"'
58780     execute 'sy xcopy "'&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*" "'&os_filename$(env$('Q')&'\UBmstr\notes.h'&str$(destination_company_number))&'\*.*" /t /y'
58800     let fnstatus('UB Notes imported.')
58820   end if  ! exists [import path]'&env$('Q')&'\UBmstr\notes.h[company_import_extension]
58840   ! /r
58850   fn_ub_copy_extras=uceReturn
58860 fnend 
60000 def library fnAutomatedSavePoint(fileNameAddition$*128)
60020   if ~setup then let fn_setup
60030   if ~env$('disableAutomatedSavePoints')='Yes' then
60040     dim asp_path$*256
60060     dim asp_filename$*256
60080     dim asp_saveFilter$*64
60100     asp_saveFilter$=env$('cursys')&'mstr\*.h'&env$('cno')
60120     asp_path$=env$('temp')&'\acs\Automated Saves'
60130     asp_filename$=env$('cursys')&' Company '&env$('cno')&' '&date$('CCYY-MM-DD')&' '&srep$(time$,':','-')&' '&env$('Program_Caption')&' - '&fileNameAddition$&'.zip'
60140     fnmakesurepathexists(asp_path$&'\')
60160     fn_FileSaveAs(asp_saveFilter$, asp_path$&'\'&asp_filename$,1)
60170   end if
60180 fnend
