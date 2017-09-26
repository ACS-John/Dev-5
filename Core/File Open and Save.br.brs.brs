18000   def fn_setup
18010     if ~setup then 
18020       let setup=1
18028       library 'S:\Core\Library': fnsave_as_path$,fngethandle,fnreg_close,fnreg_write
18030       library 'S:\Core\Library': fnmsgbox,fntext_editor,fnsystemname$,fnlog
18032       library 'S:\Core\Library': fnacs,fncmdset,fntos,fnlbl,fntxt,fncomboa
18034       library 'S:\Core\Library': fnputcno,fncursys$,fncheckfileversion,fnmakesurepathexists
18036       library 'S:\Core\Library': fnstatus,fnstatus_close,fnstatus_pause,fncopy,fnindex_sys
18038       library 'S:\Core\Library': fnaddonec
18040       dim company_import_path$*256
18050       dim resp$(5)*256
18060       dim ml$(0)*128
18070     end if 
18080   fnend 
19000 IGNORE: continue 
22000   def library fnfileopen
22020     if ~setup then let fn_setup
22040     let fnfileopen=fn_fileopen
22060   fnend 
24000   def fn_fileopen(; file_open$*256)
24020     if file_open$='' then 
24040       execute 'free '&br_filename$(env$('client_temp')&'\Open_Log.txt') ioerr ignore
24060       open #h_tmp:=fngethandle: "Name=OPEN:"&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OPEN_OPEN_ERR
24080       let file_open$=os_filename$(file$(h_tmp))
24100       close #h_tmp: 
24120     end if 
24140     let fnreg_close
24160 ! r: new way 12/4/2015
24180     open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\open_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
24200     print #h_tmp: '@echo off'
24220     print #h_tmp: '@echo Advanced Computer Services LLC'
24240     print #h_tmp: '@echo Opening: "'&file_open$&'"'
24260     print #h_tmp: '@echo.'
24280     print #h_tmp: '@echo.'
24300     print #h_tmp: '@echo Command: '&env$('path_to_7z_exe')&' x -r -aoa "'&file_open$&'" -o"'&os_filename$(env$('Q')&'\')&'" > "'&env$('temp')&'\Open_Log.txt"'
24320     print #h_tmp: '@echo.'
24340     print #h_tmp: '@echo.'
24360     print #h_tmp: '@echo Relative To: '&os_filename$(env$('Q')&'\')
24380     print #h_tmp: '@echo.'
24400     print #h_tmp: '@echo.'
24420     print #h_tmp: '@echo Output Log: "'&env$('client_temp')&'\Open_Log.txt"'
24440     print #h_tmp: '@echo.'
24460     print #h_tmp: '@echo.'
24480     print #h_tmp: '@echo OPEN PROCESSING...'
24500     print #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&file_open$&'" -o"'&os_filename$(env$('Q')&'\')&'" > "'&env$('client_temp')&'\Open_Log.txt"'
24520     close #h_tmp: 
24540     execute 'sy '&env$('client_temp')&'\open_as_'&session$&'.cmd'
24560 ! /r
24580     if fn_analyze_7zip_compresslog(env$('client_temp')&'\Open_Log.txt','Successfully Opened',file_open$) then 
24600       let fnreg_write('Last Open Date',date$('ccyy/mm/dd'))
24620       let fnreg_write('Last Open File',file_open$(pos(file_open$,'\',-1)+1:len(file_open$)))
24640       let fnreg_write('Last Open Path',file_open$(1:pos(file_open$,'\',-1)))
24660     end if 
24680     goto OPEN_XIT
24700 OPEN_OPEN_ERR: ! 
24720     if err=622 then ! it was just cancelled
24740       print 'cancelled' : goto OPEN_XIT
24760     else 
24780       mat ml$(2)
24800       let ml$(1)='Select a different file name.'
24820       let ml$(2)='Error: '&str$(err)
24840       let fnmsgbox(mat ml$)
24860 !     if err=4150 then print "Could not create file:";file$(1) : let fnpause ! file$(1) is blank!
24880       print "Err:";err;" Line:";line
24900     end if 
24920 OPEN_XIT: ! 
24940   fnend 
26000   def library fnfilesaveas(save_what$)
26020     if ~setup then let fn_setup
26040     let fnfilesaveas=fn_filesaveas(save_what$)
26060   fnend 
28000   def fn_filesaveas(save_what$; fsa_automatedsavefilename$*256,suppresserrorlog)
28020     dim save_name$*256,ln$*512
28040     dim save_log_filename$*256
28060     let failure=0
28080     let save_log_filename$=env$('client_temp')&'\Save_As_Log.txt'
28100     execute 'free '&br_filename$(save_log_filename$) ioerr ignore
28120     if fsa_automatedsavefilename$<>'' then 
28140       let save_name$=fsa_automatedsavefilename$
28160     else 
28180       open #h_tmp:=fngethandle: "Name=SAVE:"&fnsave_as_path$&"\*.zip,RecL=1,replace",external,output ioerr SAVE_AS_OPEN_ERR
28200       let save_name$=os_filename$(file$(h_tmp))
28220       close #h_tmp,free: 
28240       let fncopy('S:\drive.sys',env$('Q')&'\*.*')
28260       let fncopy('S:\brserial.dat',env$('Q')&'\*.*')
28280     end if 
28300     let fnreg_close
28320     open #h_tmp:=fngethandle: 'Name=@:'&br_filename$(env$('client_temp')&'\save_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
28340     dim tmp7zipcommand$*512
28360     if enablebackupreportcache$='True' then 
28380       let tmp7zipcommand$=env$('path_to_7z_exe')&' a -r -tzip "'&save_name$&'" "'&env$('Data')&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.wrk'
28400     else 
28420       let tmp7zipcommand$=env$('path_to_7z_exe')&' a -r -tzip "'&save_name$&'" "'&env$('Data')&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.wrk -xr!"Report Cache\*"'
28440     end if 
28460     print #h_tmp: '@echo off'
28480     print #h_tmp: '@echo Advanced Computer Services LLC'
28500     print #h_tmp: '@echo Saving to: "'&save_name$&'"'
28520     print #h_tmp: '@echo.'
28540     print #h_tmp: '@echo.'
28560     print #h_tmp: '@echo Command: '&tmp7zipcommand$
28580     print #h_tmp: '@echo.'
28600     print #h_tmp: '@echo Save What: '&env$('Data')&save_what$
28620     print #h_tmp: '@echo.'
28640     print #h_tmp: '@echo Relative To: '&os_filename$(env$('Q')&'\')
28660     print #h_tmp: '@echo.'
28680     if enablebackupreportcache$<>'True' then 
28700       print #h_tmp: '@echo Excluding Report Cache'
28720     end if 
28740     print #h_tmp: '@echo.'
28760     print #h_tmp: '@echo.'
28780     print #h_tmp: '@echo Output Log: "'&save_log_filename$&'"'
28800     print #h_tmp: '@echo.'
28820     print #h_tmp: '@echo.'
28840     print #h_tmp: '@echo SAVE PROCESSING...'
28860     print #h_tmp: tmp7zipcommand$&' > "'&save_log_filename$&'"'
28880     close #h_tmp: 
28900     execute 'sy '&env$('client_temp')&'\save_as_'&session$&'.cmd'
28920     if fsa_automatedsavefilename$<>'' then 
28940       if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$, 1,suppresserrorlog) then 
28960         let fnreg_write('Last Automated Save Date',date$('ccyy/mm/dd'))
28980         let fnreg_write('Last Automated Save Time',time$)
29000         let fnreg_write('Last Automated Save File',save_name$)
29020         let fnreg_write('Last Automated Save Path',save_name$(1:pos(save_name$,'\',-1)))
29040       end if 
29060     else 
29080       if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$,0,suppresserrorlog) then 
29100         let fnreg_write('Last Save Date',date$('ccyy/mm/dd'))
29120         let fnreg_write('Last Save Time',time$)
29140         let fnreg_write('Last Save File',save_name$)
29160         let fnreg_write('Last Save Path',save_name$(1:pos(save_name$,'\',-1)))
29180       end if 
29200     end if 
29220 ! 
29240     goto SAVE_AS_XIT
29260 SAVE_AS_OPEN_ERR: ! there was a problem opening the file.
29280     if fsa_automatedsavefilename$<>'' then 
29300       mat ml$(3)
29320       let ml$(1)='Automated save failed'
29340       let ml$(2)='Error: '&str$(err)
29360       let ml$(3)='File: '&fsa_automatedsavefilename$
29380       let fnmsgbox(mat ml$)
29400 ! goto SAVE_AS_XIT
29420 ! else if err=622 then ! it was just cancelled
29440 !   goto SAVE_AS_XIT
29460     else if err<>622 then 
29480       mat ml$(2)
29500       let ml$(1)='Select a different file name.'
29520       let ml$(2)='Error: '&str$(err)
29540       let fnmsgbox(mat ml$)
29560       print "Err:";err;" Line:";line
29580     end if 
29600 SAVE_AS_XIT: ! 
29620 !  let fn_fsa_clean_up
29640   fnend 
32000   def fn_analyze_7zip_compresslog(arc_filename$*256,success_text_line1$*256,save_name$*256; statusinsteadofmsgbox,suppresserrorlog)
32020     open #h_compresslog:=fngethandle: 'Name=@:'&arc_filename$,display,input ioerr A7C_OPEN_ERR
32040     let failure=1
32060     do 
32080       linput #h_compresslog: ln$ eof ARC_EO_COMPRESSLOG
32100       if lwrc$(ln$)='everything is ok' then 
32120         let failure=0
32140       end if 
32160     loop 
32180 ARC_EO_COMPRESSLOG: ! 
32200     close #h_compresslog: 
32220     if failure then 
32240       let fnlog(save_name$&': '&'FAILURE: '&success_text_line1$)
32260       if suppresserrorlog then 
32280         let fncopy(arc_filename$,save_name$&'(failureLog).txt')
32300         let fnstatus('Automated Save Point encountered had errors.')
32320         let fnstatus('Automated Save Point log file made: "'&save_name$&'(failureLog).txt"')
32340       else 
32360         mat ml$(4)
32380         let ml$(1)='An error occurred during the process.'
32400         let ml$(2)='The following log was created:'
32420         let ml$(3)=arc_filename$
32440         let ml$(4)='Display the log now?'
32460         let fnmsgbox(mat ml$,resp$,"ACS",4+64)
32480         if resp$="Yes" then 
32500           let fntext_editor(arc_filename$)
32520         end if 
32540       end if 
32560     else 
32580       let fnlog(save_name$&': '&success_text_line1$)
32600       if statusinsteadofmsgbox then 
32620         let fnstatus(success_text_line1$)
32640         let fnstatus(save_name$)
32660       else 
32680         mat ml$(2)
32700         let ml$(1)=success_text_line1$
32720         let ml$(2)=save_name$
32740         let fnmsgbox(mat ml$,resp$,"ACS",0)
32760       end if 
32780     end if 
32800     goto ARC_XIT
32820 A7C_OPEN_ERR: ! 
32840     mat ml$(2)
32860     let ml$(1)='FAILURE: The log file could not be opened.'
32880     let ml$(2)=arc_filename$
32900     let fnmsgbox(mat ml$,resp$,"ACS",0)
32920 ARC_XIT: ! 
32940     let fn_analyze_7zip_compresslog=~failure
32960   fnend 
34000   def library fnopenpartial
34020     if ~setup then let fn_setup
34040     let fnopenpartial=fn_openpartial
34060   fnend 
36000   def fn_7zfilelistfromarchive(file_open$*512,mat filename$)
36020     dim gflfatmpfile$*512
36040     let gflfatmpfile$=env$('client_temp')&'\acs\7zGetFileList'&session$&'.txt'
36060     open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\open_as_'&session$&'.cmd')&',RecL=512,Replace',display,output 
36080     print #h_tmp: '@echo off'
36100     print #h_tmp: '@echo Advanced Computer Services LLC'
36120     print #h_tmp: '@echo Reading file list from "'&file_open$&'"'
36140     print #h_tmp: env$('path_to_7z_exe')&' l "'&file_open$&'" > "'&gflfatmpfile$&'"'
36160     close #h_tmp: 
36180     execute 'sy '&env$('client_temp')&'\open_as_'&session$&'.cmd'
36200     open #h_tmp:=fngethandle: 'Name=@:'&gflfatmpfile$,display,input 
36220     do 
36240       linput #h_tmp: ln$
36260     loop until ln$='------------------- ----- ------------ ------------  ------------------------'
36280     let ln$=''
36300     let filecount=0
36320     do 
36340       linput #h_tmp: ln$
36360       if ln$<>'------------------- ----- ------------ ------------  ------------------------' then 
36380 ! lnDate$=ln$(1:10)
36400 ! lnTime$=ln$(12:19)
36420         mat filename$(filecount+=1)
36440         let filename$(filecount)=ln$(54:len(ln$))
36460       end if 
36480     loop until ln$='------------------- ----- ------------ ------------  ------------------------'
36520     close #h_tmp,free: 
36540     let fn_7zfilelistfromarchive=filecount
36560   fnend 
37000   def fn_filelisttoarchivelist(mat filelist$,mat archivelist$)
37020     let archivecount=0
37030     mat archivelist$(archivecount+=1)
37032     let archivelist$(archivecount)='(All Companies)'
37040     for fileitem=1 to udim(mat filelist$)
37060       let poscompany=pos(lwrc$(filelist$(fileitem)),'company.h')
37080       if poscompany>0 then 
37100         dim systemname$*40
37110         let companynumber$=filelist$(fileitem)(poscompany+9:len(filelist$(fileitem)))
37120         let systemname$=fnsystemname$(filelist$(fileitem)(1:2))
37130         mat archivelist$(archivecount+=1)
37140         let archivelist$(archivecount)=systemname$&' - Company '&companynumber$
37150         mat archivesysabbr$(archivecount)
37160         let archivesysabbr$(archivecount)=filelist$(fileitem)(1:2)
37170         mat archivecno(archivecount)
37180         let archivecno(archivecount)=val(companynumber$)
37200       end if 
37220     next fileitem
37240     let fn_filelisttoarchivelist=archivecount
37260   fnend 
38000   def fn_openpartial
38020     dim file_open$*256
38040     execute 'free '&br_filename$(env$('client_temp')&'\Open_Log.txt') ioerr ignore
38060     open #h_tmp:=fngethandle: "Name=OPEN:"&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OP_OP_ERR
38080     let file_open$=os_filename$(file$(h_tmp))
38100     close #h_tmp: 
38120     dim filelist$(0)*256,archivelist$(0)*50
38140     let fnstatus('Getting list of companies from "'&file_open$&'"...')
38160     let fn_7zfilelistfromarchive(file_open$,mat filelist$)
38180     let fn_filelisttoarchivelist(mat filelist$,mat archivelist$)
38200     let fnstatus_close
38220     let fnreg_close
38240     let fn_opmain(file_open$)
38260     goto OP_XIT
38280 OP_OP_ERR: ! 
38300     if err=622 then ! it was just cancelled
38320       print 'cancelled' : goto OP_XIT
38340     else 
38360       mat ml$(2)
38380       let ml$(1)='Select a different file name.'
38400       let ml$(2)='Error: '&str$(err)
38420       let fnmsgbox(mat ml$,resp$)
38440 !     if err=4150 then print "Could not create file:";file$(1) : let fnpause ! file$(1) is blank!
38460       print "Err:";err;" Line:";line
38480     end if 
38500 OP_XIT: ! 
38520   fnend 
42000   def fn_opmain(file_open$*256)
42020     let destination_company_number=val(env$('cno'))
42040 OPMASKWHICHTOOPEN: ! r: screen
42060     let fntos(sn$="Open Partial")
42080     let col1_width=27 : let col2_pos=col1_width+2 : let lc=rc=0
42100     let fnlbl(lc+=1,1,"Source File:",col1_width,1)
42120     let fntxt(lc,col2_pos,30,256,0,'',1,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
42140     let resp$(rc+=1)=file_open$
42160     let fnlbl(lc+=1,1,"Source Company:",col1_width,1)
42180     let fncomboa('compList',lc,col2_pos,mat archivelist$)
42200     let resp$(resp_filesource:=rc+=1)=archivelist$(1)
42220     let fnlbl(lc+=1,1,"Destination Company Number:",col1_width,1)
42240     let fntxt(lc,col2_pos,5,5,0,'1030',0,'')
42260     let resp$(resp_cnodestination:=rc+=1)=str$(destination_company_number)
42280 ! let fnlbl(lc+=1,1,"System Abbreviation:",col1_width,1)
42300 ! let fntxt(lc,col2_pos,2,2,0)
42320 ! let resp$(resp_SystemAbbr:=rc+=1)=cursys$
42340     let fncmdset(2)
42360     let fnacs(sn$,0,mat resp$,ckey)
42380 ! /r
42400     dim selectedsource$*128
42420     let selectedsource$=resp$(resp_filesource)
42440     let sourcewhich=srch(mat archivelist$,selectedsource$)
42460     if ckey=5 or sourcewhich<=0 then 
42480       let opscreenreturn=0
42500     else 
42520       if selectedsource$='(All Companies)' then 
42540         let fn_fileopen( file_open$)
42560         let opscreenreturn=1
42580       else 
44000         let source_company_number=archivecno(sourcewhich)
44020         let destination_company_number=val(resp$(resp_cnodestination))
44040         let cursys$=archivesysabbr$(sourcewhich)
44060         let fnstatus('** Open Partial Settings **')
44080         let fnstatus('Source File: '&file_open$)
44100         let fnstatus('Source System: '&cursys$)
44120         let fnstatus('Source Company Number: '&str$(source_company_number))
44140         let fnstatus('Destination Company Number: '&str$(destination_company_number))
44160         let fnstatus('**')
44180         let fnstatus('Set current system to: '&cursys$&' from '&cursys_origional$)
44200         let cursys$=fncursys$(cursys$)
44220         let fnputcno(destination_company_number) : let cno=destination_company_number
44240         let fnstatus('Set active Company Number to: '&str$(destination_company_number))
44260 ! 
44280         dim omsourcefilter$(0)*64
44300         if cursys$='UB' then 
44320           mat omsourcefilter$(1)
44340           let omsourcefilter$(1)='*.h'&str$(source_company_number)&' Notes.h'&str$(source_company_number)&'\*'
44360         else 
44380           mat omsourcefilter$(1)
44400           let omsourcefilter$(1)='*.h'&str$(source_company_number)
44420         end if 
44440         let fn_extract_appropriate_files(file_open$,mat omsourcefilter$,env$('client_temp')&'\acs\OpenPartial\')
46000         if fn_analyze_7zip_compresslog(env$('client_temp')&'\acs\OpenPartial_Log.txt','Successfully Opened '&fnsystemname$&' company '&env$('cno')&' from ',file_open$, 1) then 
46020           let fnreg_write('Last Open Partial Date',date$('ccyy/mm/dd'))
46040           let fnreg_write('Last Open Partial File',file_open$(pos(file_open$,'\',-1)+1:len(file_open$)))
46060           let fnreg_write('Last Open Partial Path',file_open$(1:pos(file_open$,'\',-1)))
46080           let fnreg_write('Last Open Partial System',env$('cursys'))
46100           let fnreg_write('Last Open Partial Company Number',env$('cno'))
46120           let fn_copy_files_in(env$('client_temp')&'\acs\OpenPartial\'&env$('cursys')&'mstr\','.h'&str$(source_company_number),val(env$('cno')))
46140           let opscreenreturn=1
46160           let setenv('force_reindex','yes')
46180           let fncheckfileversion
46200           let fnindex_sys(cno)
46210           let fnstatus_close
46220           dim msgtmp$(0)*128
46240           let fnaddonec(mat msgtmp$,'Completed.')
46260           let fnaddonec(mat msgtmp$,'Company '&env$('cno')&' created from copy of company '&str$(source_company_number))
46280           let fnaddonec(mat msgtmp$,'from the file: '&file_open$)
46300           let fnmsgbox(mat msgtmp$)
46320         end if 
46340         goto OPMASKWHICHTOOPEN
46360       end if 
46380     end if 
46400     let fn_opmain=opscreenreturn
46420   fnend 
52000   def fn_extract_appropriate_files(eafsourcefile$*256,mat eafsourcefilter$,eafdestinationfolder$*256)
52020 ! pr 'eafSourceFile$="'&eafSourceFile$&'"'
52040 ! pr 'eafSourceFilter$="'&eafSourceFilter$&'"'
52060 ! pr 'eafDestinationFolder$="'&eafSourceFilter$&'"'
52080     execute 'Sy RmDir "'&eafdestinationfolder$&'" /s /q'
52100     open #h_tmp:=fngethandle: 'Name= '&env$('at')&br_filename$(env$('client_temp')&'\acs\openPartial'&session$&'.cmd')&',RecL=512,Replace',display,output 
52120     print #h_tmp: '@echo off'
52140     print #h_tmp: '@echo Advanced Computer Services LLC'
52160     print #h_tmp: '@echo Opening: "'&eafsourcefile$&'"'
52180     print #h_tmp: '@echo.'
52200     print #h_tmp: '@echo.'
52220     for eafsourcefilteritem=1 to udim(mat eafsourcefilter$)
52240       print #h_tmp: '@echo Command('&str$(eafsourcefilteritem)&'): '&env$('path_to_7z_exe')&' x -r -aoa "'&eafsourcefile$&'" -o"'&eafdestinationfolder$&'" '&eafsourcefilter$(eafsourcefilteritem)&' > "'&env$('temp')&'\acs\OpenPartial_Log.txt"'
52260     next eafsourcefilteritem
52280     print #h_tmp: '@echo.'
52300     print #h_tmp: '@echo.'
52320     print #h_tmp: '@echo Relative To: '&eafdestinationfolder$
52340     print #h_tmp: '@echo.'
52360     print #h_tmp: '@echo.'
52380     print #h_tmp: '@echo Output Log: "'&env$('client_temp')&'\acs\OpenPartial_Log.txt"'
52400     print #h_tmp: '@echo.'
52420     print #h_tmp: '@echo.'
52440     print #h_tmp: '@echo OPEN PROCESSING...'
52480     for eafsourcefilteritem=1 to udim(mat eafsourcefilter$)
52500       print #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&eafsourcefile$&'" -o"'&eafdestinationfolder$&'" '&eafsourcefilter$(eafsourcefilteritem)&' > "'&env$('client_temp')&'\acs\OpenPartial_Log.txt"'
52520     next eafsourcefilteritem
52540     close #h_tmp: 
52560     execute 'sy '&env$('client_temp')&'\acs\openPartial'&session$&'.cmd'
52580 ! if env$('acsDeveloper')<>'' and env$('cursys')='UB' then pr 'Notes..h### should be extracted too' : pause
52990   fnend 
54000   def fn_copy_files_in(company_import_path$*256,company_import_extension$,destination_company_number)
