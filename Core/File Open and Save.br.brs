18000 def fn_setup
18010   if ~setup then
18020     setup=1
18028     library 'S:\Core\Library': fnsave_as_path$,fngethandle,fnreg_close,fnreg_write
18030     library 'S:\Core\Library': fnmsgbox,fntext_editor,fnSystemName$,fnlog
18032     library 'S:\Core\Library': fnacs,fncmdset,fntos,fnlbl,fntxt,fncomboa
18034     library 'S:\Core\Library': fnputcno,fncursys$,fncheckfileversion,fnmakesurepathexists
18036     library 'S:\Core\Library': fnstatus,fnstatus_close,fnstatus_pause,fnCopy,fnindex_sys
18038     library 'S:\Core\Library': fnaddonec,fnFree
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
24010   if file_open$='' then
24020     execute 'free '&br_filename$(env$('client_temp')&'\Open_Log.txt') ioerr ignore
24030     open #h_tmp:=fngethandle: "Name=OPEN:"&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OPEN_OPEN_ERR
24040     let file_open$=os_filename$(file$(h_tmp))
24050     close #h_tmp: 
24060   end if
24070   let fnreg_close
24080   ! r: new way 12/4/2015
24090   dim fileOpenDestination$*256
24100   if env$('BR_MODEL')='CLIENT/SERVER' then 
24110     fileOpenDestination$=env$('client_temp')&'\acs\OpenPartial\'
24120     fnmakesurepathexists(env$('at')&fileOpenDestination$)
24130   else
24140     fileOpenDestination$=os_filename$(env$('Q')&'\')
24150   end if
24300   open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\open_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
24320   print #h_tmp: '@echo off'
24340   print #h_tmp: '@echo Advanced Computer Services LLC'
24360   print #h_tmp: '@echo Opening: "'&file_open$&'"'
24380   print #h_tmp: '@echo.'
24400   print #h_tmp: '@echo.'
24420   print #h_tmp: '@echo Command: '&env$('path_to_7z_exe')&' x -r -aoa "'&file_open$&'" -o"'&fileOpenDestination$&'" > "'&env$('temp')&'\Open_Log.txt"'
24440   print #h_tmp: '@echo.'
24460   print #h_tmp: '@echo.'
24480   print #h_tmp: '@echo Relative To: '&fileOpenDestination$
24500   print #h_tmp: '@echo.'
24520   print #h_tmp: '@echo.'
24540   print #h_tmp: '@echo Output Log: "'&env$('client_temp')&'\Open_Log.txt"'
24560   print #h_tmp: '@echo.'
24580   print #h_tmp: '@echo.'
24600   print #h_tmp: '@echo OPEN PROCESSING...'
24620   print #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&file_open$&'" -o"'&fileOpenDestination$&'" > "'&env$('client_temp')&'\Open_Log.txt"'
24640   print #h_tmp: 'pause'
24660   close #h_tmp: 
24680   execute 'sy '&env$('client_temp')&'\open_as_'&session$&'.cmd'
24700   ! /r
24720   if fn_analyze_7zip_compresslog(env$('client_temp')&'\Open_Log.txt','Successfully Opened',file_open$) then 
24740     let fnreg_write('Last Open Date',date$('ccyy/mm/dd'))
24760     let fnreg_write('Last Open File',file_open$(pos(file_open$,'\',-1)+1:len(file_open$)))
24780     let fnreg_write('Last Open Path',file_open$(1:pos(file_open$,'\',-1)))
25000     if env$('BR_MODEL')='CLIENT/SERVER' then
25020       if filter$='' then
25040       else
25060         for company=1 to udim(mat archiveCNo)
25080         nex company
25100       end if
25120       pr 'before call to fnCopy' : pause
25140       fnCopy(env$('at')&env$('client_temp')&'\acs\OpenPartial\*.*',env$('q')&'\*.*',0,'recursive')
25160       pause : opScreenReturn=1 
25180       setenv('force_reindex','yes') 
25200     end if
25220     fncheckfileversion
25240     fnindex_sys(cno)
25260     fnstatus_close
25280   else if env$('acsDebug')='Yes' then
25290     pr 'fn_analyze_7zip_compresslog failed.'
25300     pause
25310   end if 
25320   goto OPEN_XIT
26000   OPEN_OPEN_ERR: ! 
26010   if err=622 then ! it was just cancelled
26020     print 'cancelled' : goto OPEN_XIT
26030   else 
26040     mat ml$(2)
26050     let ml$(1)='Select a different file name.'
26060     let ml$(2)='Error: '&str$(err)
26070     let fnmsgbox(mat ml$)
26080     !     if err=4150 then print "Could not create file:";file$(1) : let fnpause ! file$(1) is blank!
26090     print "Err:";err;" Line:";line
26100   end if 
26110   OPEN_XIT: ! 
26120 fnend
36000 def library fnFileSaveAs(save_what$)
36020   if ~setup then let fn_setup
36040   fnFileSaveAs=fn_FileSaveAs(save_what$)
36060 fnend
38000 def fn_FileSaveAs(save_what$; fsa_automatedSaveFileName$*256,suppressErrorLog)
38020   dim save_name$*256,ln$*512
38040   dim save_log_filename$*256
38060   let failure=0
38080   let save_log_filename$=env$('client_temp')&'\Save_As_Log.txt'
38100   execute 'free '&br_filename$(save_log_filename$) ioerr ignore
38120   if fsa_automatedSaveFileName$<>'' then
38140     save_name$=fsa_automatedSaveFileName$
38160   else
38180     open #h_tmp:=fngethandle: "Name=SAVE:"&fnsave_as_path$&"\*.zip,RecL=1,replace",external,output ioerr SAVE_AS_OPEN_ERR
38200     save_name$=os_filename$(file$(h_tmp))
38220     close #h_tmp,free: 
38240     fnCopy('S:\drive.sys',env$('Q')&'\*.*')
38260     fnCopy('S:\brserial.dat',env$('Q')&'\*.*')
38280   end if
38300   fnreg_close
38320   open #h_tmp:=fngethandle: 'Name=@:'&br_filename$(env$('client_temp')&'\save_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
38340   dim tmp7ZipCommand$*512
38360   if enableBackupReportCache$='True' then
38380     tmp7zipcommand$=env$('path_to_7z_exe')&' a -r -tzip "'&save_name$&'" "'&env$('Q')&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.wrk'
38400   else
38420     tmp7ZipCommand$=env$('path_to_7z_exe')&' a -r -tzip "'&save_name$&'" "'&env$('Q')&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.wrk -xr!"Report Cache\*"'
38440   end if
38460   print #h_tmp: '@echo off'
38480   print #h_tmp: '@echo Advanced Computer Services LLC'
38500   print #h_tmp: '@echo Saving to: "'&save_name$&'"'
38520   print #h_tmp: '@echo.'
38540   print #h_tmp: '@echo.'
38560   print #h_tmp: '@echo Command: '&tmp7ZipCommand$
38580   print #h_tmp: '@echo.'
38600   print #h_tmp: '@echo Save What: '&env$('Q')&save_what$
38620   print #h_tmp: '@echo.'
38640   print #h_tmp: '@echo Relative To: '&os_filename$(env$('Q')&'\')
38660   print #h_tmp: '@echo.'
38680   if enableBackupReportCache$<>'True' then
38700     print #h_tmp: '@echo Excluding Report Cache'
38720   end if
38740   print #h_tmp: '@echo.'
38760   print #h_tmp: '@echo.'
38780   print #h_tmp: '@echo Output Log: "'&save_log_filename$&'"'
38800   print #h_tmp: '@echo.'
38820   print #h_tmp: '@echo.'
38840   print #h_tmp: '@echo SAVE PROCESSING...'
38860   print #h_tmp: tmp7ZipCommand$&' > "'&save_log_filename$&'"'
38880   close #h_tmp: 
38900   execute 'sy '&env$('client_temp')&'\save_as_'&session$&'.cmd'
38920   if fsa_automatedSaveFileName$<>'' then
38940     if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$, 1,suppressErrorLog) then 
38960       let fnreg_write('Last Automated Save Date',date$('ccyy/mm/dd'))
38980       let fnreg_write('Last Automated Save Time',time$)
39000       let fnreg_write('Last Automated Save File',save_name$)
39020       let fnreg_write('Last Automated Save Path',save_name$(1:pos(save_name$,'\',-1)))
39040     end if 
39060   else
39080     if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$,0,suppressErrorLog) then 
39100       let fnreg_write('Last Save Date',date$('ccyy/mm/dd'))
39120       let fnreg_write('Last Save Time',time$)
39140       let fnreg_write('Last Save File',save_name$)
39160       let fnreg_write('Last Save Path',save_name$(1:pos(save_name$,'\',-1)))
39180     end if 
39200   end if
39220   ! 
39240   goto SAVE_AS_XIT
39260   SAVE_AS_OPEN_ERR: ! there was a problem opening the file.
39280   if fsa_automatedSaveFileName$<>'' then
39300     mat ml$(3)
39320     let ml$(1)='Automated save failed'
39340     let ml$(2)='Error: '&str$(err)
39360     let ml$(3)='File: '&fsa_automatedSaveFileName$
39380     let fnmsgbox(mat ml$)
39400     ! goto SAVE_AS_XIT
39420   ! else if err=622 then ! it was just cancelled
39440   !   goto SAVE_AS_XIT
39460   else if err<>622 then
39480     mat ml$(2)
39500     let ml$(1)='Select a different file name.'
39520     let ml$(2)='Error: '&str$(err)
39540     let fnmsgbox(mat ml$)
39560     print "Err:";err;" Line:";line
39580   end if 
39600   SAVE_AS_XIT: ! 
39620   !  let fn_fsa_clean_up
39640 fnend 
42000 def fn_analyze_7zip_compresslog(arc_filename$*256,success_text_line1$*256,save_name$*256; statusInsteadOfMsgBox,suppressErrorLog)
42020   open #h_compresslog:=fngethandle: 'Name=@:'&arc_filename$,display,input ioerr A7C_OPEN_ERR
42040   let failure=1
42060   do 
42080     linput #h_compresslog: ln$ eof ARC_EO_COMPRESSLOG
42100     if lwrc$(ln$)='everything is ok' then 
42120       let failure=0
42140     end if 
42160   loop 
42180   ARC_EO_COMPRESSLOG: ! 
42200   close #h_compresslog: 
42220   if failure then 
42240     fnlog(save_name$&': '&'FAILURE: '&success_text_line1$)
42260     if suppressErrorLog then
42280       fnCopy(arc_filename$,save_name$&'(failureLog).txt')
42300       fnstatus('Automated Save Point encountered had errors.')
42320       fnstatus('Automated Save Point log file made: "'&save_name$&'(failureLog).txt"')
42340     else
42360       mat ml$(4)
42380       let ml$(1)='An error occurred during the process.'
42400       let ml$(2)='The following log was created:'
42420       let ml$(3)=arc_filename$
42440       let ml$(4)='Display the log now?'
42460       let fnmsgbox(mat ml$,resp$,"ACS",4+64)
42480       if resp$="Yes" then 
42500         let fntext_editor(arc_filename$)
42520       end if 
42540     end if 
42560   else 
42580     fnlog(save_name$&': '&success_text_line1$)
42600     if statusInsteadOfMsgBox then 
42620       fnstatus(success_text_line1$)
42640       fnstatus(save_name$)
42660     else
42680       mat ml$(2)
42700       let ml$(1)=success_text_line1$
42720       let ml$(2)=save_name$
42740       let fnmsgbox(mat ml$,resp$,"ACS",0)
42760     end if
42780   end if 
42800   goto ARC_XIT
42820   A7C_OPEN_ERR: ! 
42840   mat ml$(2)
42860   let ml$(1)='FAILURE: The log file could not be opened.'
42880   let ml$(2)=arc_filename$
42900   let fnmsgbox(mat ml$,resp$,"ACS",0)
42920   ARC_XIT: ! 
42940   let fn_analyze_7zip_compresslog=~failure
42960 fnend 
44000 def library fnOpenPartial
44020   if ~setup then let fn_setup
44040   fnOpenPartial=fn_openPartial
44060 fnend
46000 def fn_7zFileListFromArchive(file_open$*512,mat filename$)
46020   dim gflfaTmpFile$*512
46040   gflfaTmpFile$=env$('client_temp')&'\acs\7zGetFileList'&session$&'.txt'
46060   open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\open_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
46080   print #h_tmp: '@echo off'
46100   print #h_tmp: '@echo Advanced Computer Services LLC'
46120   print #h_tmp: '@echo Reading file list from "'&file_open$&'"'
46140   print #h_tmp: env$('path_to_7z_exe')&' l "'&file_open$&'" > "'&gflfaTmpFile$&'"'
46160   close #h_tmp: 
46180   execute 'sy '&env$('client_temp')&'\open_as_'&session$&'.cmd'
46200   open #h_tmp:=fngethandle: 'Name=@:'&gflfaTmpFile$,display,input
46220   do 
46240     linput #h_tmp: ln$
46260   loop until ln$='------------------- ----- ------------ ------------  ------------------------'
46280   ln$=''
46300   fileCount=0
46320   do
46340     linput #h_tmp: ln$
46360     if ln$<>'------------------- ----- ------------ ------------  ------------------------' then
46380       ! lnDate$=ln$(1:10)
46400       ! lnTime$=ln$(12:19)
46420       mat filename$(fileCount+=1)
46440       filename$(fileCount)=ln$(54:len(ln$))
46460     end if
46480   loop until ln$='------------------- ----- ------------ ------------  ------------------------'
46520   close #h_tmp,free: 
46540   fn_7zFileListFromArchive=fileCount
46560 fnend
47000 def fn_fileListToArchiveList(mat fileList$,mat archiveList$)
47020   archiveCount=0
47030   mat archiveList$(archiveCount+=1)
47032   archiveList$(archiveCount)='(All Companies)'
47040   for fileItem=1 to udim(mat fileList$)
47060     posCompany=pos(lwrc$(fileList$(fileItem)),'company.h')
47080     if posCompany>0 then
47100       dim systemName$*40
47110       companyNumber$=fileList$(fileItem)(posCompany+9:len(fileList$(fileItem)))
47120       systemName$=fnSystemName$(fileList$(fileItem)(1:2))
47130       mat archiveList$(archiveCount+=1)
47140       archiveList$(archiveCount)=systemName$&' - Company '&companyNumber$
47150       mat archiveSysAbbr$(archiveCount)
47160       archiveSysAbbr$(archiveCount)=fileList$(fileItem)(1:2)
47170       mat archiveCNo(archiveCount)
47180       archiveCNo(archiveCount)=val(companyNumber$)
47200     end if
47220   nex fileItem
47240   fn_fileListToArchiveList=archiveCount
47260 fnend
48000 def fn_openPartial
48020   dim file_open$*256
48040   execute 'free '&br_filename$(env$('client_temp')&'\Open_Log.txt') ioerr ignore
48060   open #h_tmp:=fngethandle: "Name=OPEN:"&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OP_OP_ERR
48080   file_open$=os_filename$(file$(h_tmp))
48100   close #h_tmp: 
48120   dim fileList$(0)*256,archiveList$(0)*50
48140   fnstatus('Getting list of companies from "'&file_open$&'"...')
48160   fn_7zFileListFromArchive(file_open$,mat fileList$)
48180   fn_fileListToArchiveList(mat fileList$,mat archiveList$)
48200   fnstatus_close
48220   fnreg_close
48240   fn_opMain(file_open$)
48260   goto OP_XIT
48280   OP_OP_ERR: ! 
48300   if err=622 then ! it was just cancelled
48320     print 'cancelled' : goto OP_XIT
48340   else 
48360     mat ml$(2)
48380     ml$(1)='Select a different file name.'
48400     ml$(2)='Error: '&str$(err)
48420     fnmsgbox(mat ml$,resp$)
48440     !     if err=4150 then print "Could not create file:";file$(1) : let fnpause ! file$(1) is blank!
48460     print "Err:";err;" Line:";line
48480   end if 
48500   OP_XIT: ! 
48520 fnend
52000 def fn_opMain(file_open$*256)
52020   destination_company_number=val(env$('cno'))
52040   OpmAskWhichToOpen: ! r: screen
52060   let fntos(sn$="Open Partial")
52080   let col1_width=27 : let col2_pos=col1_width+2 : lc=rc=0
52100   let fnlbl(lc+=1,1,"Source File:",col1_width,1)
52120   let fntxt(lc,col2_pos,30,256,0,'',1,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
52140   resp$(rc+=1)=file_open$
52160   let fnlbl(lc+=1,1,"Source Company:",col1_width,1)
52180   fncomboa('compList',lc,col2_pos,mat archiveList$)
52200   let resp$(resp_fileSource:=rc+=1)=archiveList$(1)
52220   let fnlbl(lc+=1,1,"Destination Company Number:",col1_width,1)
52240   let fntxt(lc,col2_pos,5,5,0,'1030',0,'')
52250   let fnlbl(lc,col2_pos+7,"(only applies if a specific Source Company is selected)")
52260   let resp$(resp_cnoDestination:=rc+=1)=str$(destination_company_number)
52280   ! let fnlbl(lc+=1,1,"System Abbreviation:",col1_width,1)
52300   ! let fntxt(lc,col2_pos,2,2,0)
52320   ! let resp$(resp_SystemAbbr:=rc+=1)=cursys$
52340   let fncmdset(2)
52360   let fnacs(sn$,0,mat resp$,ckey)
52380   ! /r
52400   dim selectedSource$*128
52420   selectedSource$=resp$(resp_fileSource)
52440   sourceWhich=srch(mat archiveList$,selectedSource$)
52460   if ckey=5 or sourceWhich<=0 then 
52480     opScreenReturn=0
52500   else
52520     if selectedSource$='(All Companies)' then
52540       fn_fileOpen( file_open$)
52560       opScreenReturn=1 
52580     else 
54000       source_company_number=archiveCNo(sourceWhich)
54020       destination_company_number=val(resp$(resp_cnoDestination))
54040       cursys$=archiveSysAbbr$(sourceWhich)
54060       fnstatus('** Open Partial Settings **')
54080       fnstatus('Source File: '&file_open$)
54100       fnstatus('Source System: '&cursys$)
54120       fnstatus('Source Company Number: '&str$(source_company_number))
54140       fnstatus('Destination Company Number: '&str$(destination_company_number))
54160       fnstatus('**')
54180       fnstatus('Set current system to: '&cursys$&' from '&cursys_origional$)
54200       cursys$=fncursys$(cursys$)
54220       fnputcno(destination_company_number) : let cno=destination_company_number
54240       fnstatus('Set active Company Number to: '&str$(destination_company_number))
54260       ! 
54280       dim omSourceFilter$(0)*64
54300       if cursys$='UB' then
54320         mat omSourceFilter$(1)
54340         omSourceFilter$(1)='*.h'&str$(source_company_number)&' Notes.h'&str$(source_company_number)&'\*'
54360       else
54380         mat omSourceFilter$(1)
54400         omSourceFilter$(1)='*.h'&str$(source_company_number)
54420       end if
54440       fn_extract_appropriate_files(file_open$,mat omSourceFilter$,env$('client_temp')&'\acs\OpenPartial\')
56000       if fn_analyze_7zip_compresslog(env$('client_temp')&'\acs\OpenPartial_Log.txt','Successfully Opened '&fnSystemName$&' company '&env$('cno')&' from ',file_open$, 1) then 
56020         fnreg_write('Last Open Partial Date',date$('ccyy/mm/dd'))
56040         fnreg_write('Last Open Partial File',file_open$(pos(file_open$,'\',-1)+1:len(file_open$)))
56060         fnreg_write('Last Open Partial Path',file_open$(1:pos(file_open$,'\',-1)))
56080         fnreg_write('Last Open Partial System',env$('cursys'))
56100         fnreg_write('Last Open Partial Company Number',env$('cno'))
56120         fn_copy_files_in(env$('at')&env$('client_temp')&'\acs\OpenPartial\'&env$('cursys')&'mstr\','.h'&str$(source_company_number),val(env$('cno')))
56140         opScreenReturn=1 
56160         setenv('force_reindex','yes') 
56180         fncheckfileversion
56200         fnindex_sys(cno)
56210         fnstatus_close
56220         dim msgTmp$(0)*128
56240         fnaddonec(mat msgTmp$,'Completed.')
56260         fnaddonec(mat msgTmp$,'Company '&env$('cno')&' created from copy of company '&str$(source_company_number))
56280         fnaddonec(mat msgTmp$,'from the file: '&file_open$)
56300         fnmsgbox(mat msgTmp$)
56320       end if 
56340       goto OpmAskWhichToOpen
56360     end if
56380   end if
56400   fn_opMain=opScreenReturn
56420 fnend
62000 def fn_extract_appropriate_files(eafSourceFile$*256,mat eafSourceFilter$,eafDestinationFolder$*256)
62020   ! pr 'eafSourceFile$="'&eafSourceFile$&'"'
62040   ! pr 'eafSourceFilter$="'&eafSourceFilter$&'"'
62060   ! pr 'eafDestinationFolder$="'&eafSourceFilter$&'"'
62080   execute 'Sy RmDir "'&eafDestinationFolder$&'" /s /q'
62100   open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\acs\openPartial'&session$&'.cmd')&',RecL=512,Replace',display,output 
62120   print #h_tmp: '@echo off'
62140   print #h_tmp: '@echo Advanced Computer Services LLC'
62160   print #h_tmp: '@echo Opening: "'&eafSourceFile$&'"'
62180   print #h_tmp: '@echo.'
62200   print #h_tmp: '@echo.'
62220   for eafSourceFilterItem=1 to udim(mat eafSourceFilter$)
62240     print #h_tmp: '@echo Command('&str$(eafSourceFilterItem)&'): '&env$('path_to_7z_exe')&' x -r -aoa "'&eafSourceFile$&'" -o"'&eafDestinationFolder$&'" '&eafSourceFilter$(eafSourceFilterItem)&' > "'&env$('temp')&'\acs\OpenPartial_Log.txt"'
62260   nex eafSourceFilterItem
62280   print #h_tmp: '@echo.'
62300   print #h_tmp: '@echo.'
62320   print #h_tmp: '@echo Relative To: '&eafDestinationFolder$
62340   print #h_tmp: '@echo.'
62360   print #h_tmp: '@echo.'
62380   print #h_tmp: '@echo Output Log: "'&env$('client_temp')&'\acs\OpenPartial_Log.txt"'
62400   print #h_tmp: '@echo.'
62420   print #h_tmp: '@echo.'
62440   print #h_tmp: '@echo OPEN PROCESSING...'
62460
62480   for eafSourceFilterItem=1 to udim(mat eafSourceFilter$)
62500     print #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&eafSourceFile$&'" -o"'&eafDestinationFolder$&'" '&eafSourceFilter$(eafSourceFilterItem)&' > "'&env$('client_temp')&'\acs\OpenPartial_Log.txt"'
62520   nex eafSourceFilterItem
62540   close #h_tmp: 
62560   execute 'sy '&env$('client_temp')&'\acs\openPartial'&session$&'.cmd'
62580     ! if env$('acsDeveloper')<>'' and env$('cursys')='UB' then pr 'Notes..h### should be extracted too' : pause
62990 fnend
64000 def fn_copy_files_in(company_import_path$*256,company_import_extension$,destination_company_number)
64020   fnFree(env$('Q')&'\'&env$('cursys')&'mstr\*.h'&str$(destination_company_number))
64040   let fnstatus('Existing files ('&os_filename$(env$('Q')&'\'&env$('cursys')&'mstr\*.h'&str$(destination_company_number))&') have been removed.')
64060   cfiReturn=fnCopy(company_import_path$&'*'&company_import_extension$,env$('Q')&'\'&env$('cursys')&'mstr\*.h'&str$(destination_company_number))
64080   if cfiReturn>0 then
64100     if env$('cursys')='UB' then
64120       cfiReturn=fn_ub_copy_extras(company_import_path$,company_import_extension$,destination_company_number)
64140     end if
64160     if cfiReturn>0 then
64180       let fnstatus('Import data copied in.')
64200     end if
64220   end if
64240   fn_copy_files_in=cfiReturn
64260 fnend 
68000 def fn_ub_copy_extras(company_import_path$*256,company_import_extension$,destination_company_number)
68020   ! r: import rates
68040   if exists(company_import_path$&'ubdata') then 
68060     if exists(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number)) then 
68080       execute 'free "'&env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number)&'"'
68100     end if  ! exists(env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
68120     uceReturn=fnCopy(company_import_path$&'ubdata\*'&company_import_extension$,env$('Q')&'\UBmstr\ubdata\*.h'&str$(destination_company_number))
68140     if uceReturn>0 then
68160       let fnstatus('UBmstr\ubData found in source and is replacing destination.')
68180     end if
68440   else 
68460     let fnstatus('UBmstr\ubData did not exist in source. Destination ubData remains unchanged.')
68680   end if 
68700   ! /r
68720   ! r: import notes folder
68740   if exists(company_import_path$&'UBmstr\notes'&company_import_extension$) then 
68760     execute 'free "'&env$('Q')&'\'&env$('cursys')&'mstr\notes.h'&str$(destination_company_number)&'"'
68780     execute 'sy xcopy "'&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*" "'&os_filename$(env$('Q')&'\UBmstr\notes.h'&str$(destination_company_number))&'\*.*" /t /y'
68800     let fnstatus('UB Notes imported.')
68820   end if  ! exists [import path]'&env$('Q')&'\UBmstr\notes.h[company_import_extension]
68840   ! /r
68850   fn_ub_copy_extras=uceReturn
68860 fnend 
70000 def library fnAutomatedSavePoint(fileNameAddition$*128)
70020   if ~setup then let fn_setup
70030   if ~env$('disableAutomatedSavePoints')='Yes' then
70040     dim asp_path$*256
70060     dim asp_filename$*256
70080     dim asp_saveFilter$*64
70100     asp_saveFilter$=env$('cursys')&'mstr\*.h'&env$('cno')
70120     asp_path$=env$('temp')&'\acs\Automated Saves'
70130     asp_filename$=env$('cursys')&' Company '&env$('cno')&' '&date$('CCYY-MM-DD')&' '&srep$(time$,':','-')&' '&env$('Program_Caption')&' - '&fileNameAddition$&'.zip'
70140     fnmakesurepathexists(asp_path$&'\')
70160     fn_FileSaveAs(asp_saveFilter$, asp_path$&'\'&asp_filename$,1)
70170   end if
70180 fnend
