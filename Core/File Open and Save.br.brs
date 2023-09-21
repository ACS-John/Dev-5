dim resp$(5)*256
dim ml$(0)*1024
def fn_isClientServer(; ___,returnN)
	if env$('BR_MODEL')='CLIENT/SERVER' then returnN=1
	fn_isClientServer=returnN
fnend

def library fnFileSaveAs(save_what$; saveFileName$*256)
	if ~setup then fn_setup
	fnFileSaveAs=fn_FileSaveAs(save_what$, saveFileName$)
fnend
def fn_FileSaveAs(save_what$*128; fsa_automatedSaveFileName$*256,suppressErrorLog,disableCopyToLocal)
	dim save_name$*256,ln$*512
	dim save_log_filename$*256
	failure=0
	save_what$=fnSrepEnv$(save_what$)
	fsa_automatedSaveFileName$=fnSrepEnv$(fsa_automatedSaveFileName$)
	save_log_filename$=env$('temp')&'\Save_As_Log.txt'
	fnFree(br_filename$(save_log_filename$))
	if fsa_automatedSaveFileName$<>'' then
		save_name$=fsa_automatedSaveFileName$
	else
		open #h_tmp=fnH: 'Name=SAVE:'&fnsave_as_path$&'\*.zip,RecL=1,replace',external,output ioerr SAVE_AS_OPEN_ERR
		save_name$=os_filename$(file$(h_tmp))
		close #h_tmp,free:
		fnCopy('S:\drive.sys','[Q]\*.*')
		fnCopy('S:\brserial.dat','[Q]\*.*')
	end if
	fnreg_close
	open #h_tmp=fnH: 'Name='&br_filename$(env$('temp')&'\save_as_'&session$&'.cmd')&',RecL=512,Replace',d,o
	dim tmp7ZipCommand$*512
	dim zOmitReportCacheOption$*64
	if enableBackupReportCache$='True' then
		zOmitReportCacheOption$=''
	else
		zOmitReportCacheOption$=' -xr!"Report Cache\*"'
	end if
	dim serverTempSaveFile$*256
	if fn_isClientServer then
		serverTempSaveFile$=env$('temp')&'\save_'&session$&'.zip'
		fnFree(serverTempSaveFile$)
	else
		serverTempSaveFile$=save_name$
	end if
	tmp7ZipCommand$=env$('path_to_7z_exe')&' a -r -tzip "'&serverTempSaveFile$&'" "[Q]\'&save_what$&'" -w"'&os_filename$(env$('Q')&'\')&'" -x!wbserver.dat -x!*.$$$ -x!*.tmp -x!*.tmp1 -x!*.wrk -xr!"FileIO\*"'&zOmitReportCacheOption$&' -xr!"Temp\*"'
	tmp7ZipCommand$=fnSrepEnv$(tmp7ZipCommand$)
	pr #h_tmp: '@echo off'
	pr #h_tmp: '@echo Advanced Computer Services LLC'
	pr #h_tmp: '@echo Saving to: "'&save_name$&'"'
	if fn_isClientServer then
		pr #h_tmp: '@echo temp on server: "'&serverTempSaveFile$&'"'
	end if
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Command: '&tmp7ZipCommand$
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Save What: '&env$('Q')&'\'&save_what$
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Relative To: '&os_filename$(env$('Q')&'\')
	pr #h_tmp: '@echo.'
	if enableBackupReportCache$<>'True' then
		pr #h_tmp: '@echo Excluding Report Cache'
	end if
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Output Log: "'&save_log_filename$&'"'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	if fsa_automatedSaveFileName$<>'' then
	pr #h_tmp: '@echo SAVE PROCESSING...'
	else
	pr #h_tmp: '@echo PROCESSING AUTOMATED SAVE...'
	end if
	pr #h_tmp: tmp7ZipCommand$&' > "'&save_log_filename$&'"'
	close #h_tmp:
	if fn_isClientServer and ~disableCopyToLocal then
		execute 'sy -s '&env$('temp')&'\save_as_'&session$&'.cmd'
		fnMakeSurePathExists(env$('at')&save_name$)
		fnCopyFile(serverTempSaveFile$,env$('at')&save_name$)
	else if fn_isClientServer and disableCopyToLocal then ! fnAutomatedSavePoint on client/server
		execute 'sy -s '&env$('temp')&'\save_as_'&session$&'.cmd'
		dim save_path$*1024,save_filename$*256,save_ext$
		fnGetPp(save_name$,save_path$,save_filename$,save_ext$)
		fnMakeSurePathExists(fnProgramDataDir$&'\Temp\'&env$('client')&'\Automated Saves\'&save_filename$&save_ext$)
		fnRename(serverTempSaveFile$,fnProgramDataDir$&'\Temp\'&env$('client')&'\Automated Saves\'&save_filename$&save_ext$)
	else
		execute 'sy '&env$('temp')&'\save_as_'&session$&'.cmd'
	end if
	if fsa_automatedSaveFileName$<>'' then
		if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$, 1,suppressErrorLog) then
			fnreg_write('Last Automated Save Date',date$('ccyy/mm/dd'))
			fnreg_write('Last Automated Save Time',time$)
			fnreg_write('Last Automated Save File',save_name$)
			fnreg_write('Last Automated Save Path',save_name$(1:pos(save_name$,'\',-1)))
		end if
	else
		if fn_analyze_7zip_compresslog(save_log_filename$,'All ACS Data has successfully been saved to',save_name$,0,suppressErrorLog) then
			fnreg_write('Last Save Date',date$('ccyy/mm/dd'))
			fnreg_write('Last Save Time',time$)
			fnreg_write('Last Save File',save_name$)
			fnreg_write('Last Save Path',save_name$(1:pos(save_name$,'\',-1)))
		end if
	end if

	goto SAVE_AS_XIT
	SAVE_AS_OPEN_ERR: ! there was a problem opening the file.
	if fsa_automatedSaveFileName$<>'' then
		mat ml$(3)
		ml$(1)='Automated save failed'
		ml$(2)='Error: '&str$(err)
		ml$(3)='File: '&fsa_automatedSaveFileName$
		fnMsgBox(mat ml$)
		! goto SAVE_AS_XIT
	! else if err=622 then ! it was just cancelled
	!   goto SAVE_AS_XIT
	else if err<>622 then
		mat ml$(2)
		ml$(1)='Select a different file name.'
		ml$(2)='Error: '&str$(err)
		fnMsgBox(mat ml$)
		pr 'Err:';err;' Line:';line
	end if
	SAVE_AS_XIT: !
	!  fn_fsa_clean_up
fnend
def fn_analyze_7zip_compresslog(arc_filename$*256,success_text_line1$*256,save_name$*256; statusInsteadOfMsgBox,suppressErrorLog)
	open #h_compresslog=fnH: 'Name='&arc_filename$,display,input ioerr A7C_OPEN_ERR
	failure=1
	do
		linput #h_compresslog: ln$ eof ARC_EO_COMPRESSLOG
		if lwrc$(ln$)='everything is ok' then
			failure=0
		end if
	loop
	ARC_EO_COMPRESSLOG: !
	close #h_compresslog:
	if failure then
		fnLog(save_name$&': FAILURE: '&success_text_line1$)
		if suppressErrorLog then
			fnCopy(arc_filename$,save_name$&'(failureLog).txt')
			fnStatus('Automated Save Point encountered had errors.')
			fnStatus('Automated Save Point log file made: "'&save_name$&'(failureLog).txt"')
		else
			mat ml$(4)
			ml$(1)='An error occurred during the process.'
			ml$(2)='The following log was created:'
			ml$(3)=arc_filename$
			ml$(4)='Display the log now?'

			fnMsgBox(mat ml$,resp$,'ACS',4+64)
			if resp$='Yes' then
				! if env$('acsDeveloper')<>'' then pr 'just before fnEditFile('text',"'&arc_filename$&'")' : pause
				fnEditFile('text',arc_filename$)
			end if
		end if
	else
		fnLog(save_name$&': '&success_text_line1$)
		if statusInsteadOfMsgBox then
			fnStatus(success_text_line1$)
			fnStatus(save_name$)
		else
			mat ml$(2)
			ml$(1)=success_text_line1$
			ml$(2)=save_name$
			fnMsgBox(mat ml$,resp$,'ACS',0)
		end if
	end if
	goto ARC_XIT
	A7C_OPEN_ERR: !
	mat ml$(2)
	ml$(1)='FAILURE: The log file could not be opened.'
	ml$(2)=arc_filename$
	fnMsgBox(mat ml$,resp$,'ACS',0)
	if env$('acsDeveloper')<>'' then pause
	ARC_XIT: !
	fn_analyze_7zip_compresslog=~failure
fnend
def library fnOpenPartial
	if ~setup then fn_setup
	fnOpenPartial=fn_openPartial
fnend
def fn_7zFileListFromArchive(zFileOpen$*512,mat filename$)
	dim gflfaTmpFile$*512
	gflfaTmpFile$=env$('temp')&'\acs\7zGetFileList'&session$&'.txt'
	open #h_tmp=fnH: 'Name= '&br_filename$(env$('temp')&'\acs\OpenEverything_'&session$&'.cmd')&',RecL=512,Replace',d,o
	pr #h_tmp: '@echo off'
	pr #h_tmp: '@echo Advanced Computer Services LLC'
	pr #h_tmp: '@echo Reading file list from "'&zFileOpen$&'"'
	pr #h_tmp: env$('path_to_7z_exe')&' l "'&zFileOpen$&'" > "'&gflfaTmpFile$&'"'
	close #h_tmp:
	execute 'sy -s '&env$('temp')&'\acs\OpenEverything_'&session$&'.cmd'
	open #h_tmp=fnH: 'Name='&gflfaTmpFile$,display,input
	fileCount=0
	do
		linput #h_tmp: ln$ eof ZflaEof
	loop until ln$='------------------- ----- ------------ ------------  ------------------------'
	ln$=''
	do
		linput #h_tmp: ln$
		if ln$<>'------------------- ----- ------------ ------------  ------------------------' then
			! lnDate$=ln$(1:10)
			! lnTime$=ln$(12:19)
			mat filename$(fileCount+=1)
			filename$(fileCount)=ln$(54:len(ln$))
		end if
	loop until ln$='------------------- ----- ------------ ------------  ------------------------'
	ZflaEof: close #h_tmp,free:
	fn_7zFileListFromArchive=fileCount
fnend
def fn_fileListToArchiveList(mat fileList$,mat archiveList$)
	archiveCount=0
	mat archiveList$(archiveCount+=1)
	archiveList$(archiveCount)='(All Companies)'
	for fileItem=1 to udim(mat fileList$)
		posCompany=pos(lwrc$(fileList$(fileItem)),'company.h')
		if posCompany>0 then
			dim systemName$*40
			companyNumber$=fileList$(fileItem)(posCompany+9:len(fileList$(fileItem)))
			systemName$=fnSystemName$(fileList$(fileItem)(1:2))
			mat archiveList$(archiveCount+=1)
			archiveList$(archiveCount)=systemName$&' - Company '&companyNumber$
			mat archiveSysAbbr$(archiveCount)
			archiveSysAbbr$(archiveCount)=fileList$(fileItem)(1:2)
			mat archiveCNo(archiveCount)
			archiveCNo(archiveCount)=val(companyNumber$)
		end if
	nex fileItem
	fn_fileListToArchiveList=archiveCount
fnend
def fn_openPartial
	dim opFileOpen$*256
	fnFree(br_filename$(env$('temp')&'\acs\Open_Log.txt'))
	open #h_tmp=fnH: 'Name=OPEN:'&env$('at')&'ACS Data Set (*.zip) |'&fnsave_as_path$&'\*.zip,RecL=1,Shr',external,input ioerr OP_OP_ERR
	opFileOpen$=os_filename$(file$(h_tmp))
	close #h_tmp:
	dim fileList$(0)*256,archiveList$(0)*50
	dim tmpFileOpen$*256
	if fn_isClientServer then
		tmpFileOpen$=env$('temp')&'\acs\OpenPartial_tmpFileOpen'&session$&'.zip'
		fnMakeSurePathExists(tmpFileOpen$)
		! if env$('acsDeveloper')<>'' and exists(tmpFileOpen$) then goto SKIPFORDEV! XXX DELETE ME
			fnCopyFile(env$('at')&opFileOpen$,tmpFileOpen$)
			if env$('acsDeveloper')<>'' then pr bell; : sleep(.2) : pr bell; : sleep(.1) : pr bell;
		! SKIPFORDEV: ! XXX DELETE ME
	else
		tmpFileOpen$=opFileOpen$
	end if
	fnStatus('Getting list of companies from "'&opFileOpen$&'"...')
	fn_7zFileListFromArchive(tmpFileOpen$,mat fileList$)
	fn_fileListToArchiveList(mat fileList$,mat archiveList$)
	fnStatusClose
	fnreg_close
	fn_opMain(tmpFileOpen$)
	goto OP_XIT
	OP_OP_ERR: !
	if err=622 then ! it was just canceled
		pr 'canceled' : goto OP_XIT
	else
		mat ml$(2)
		ml$(1)='Select a different file name.'
		ml$(2)='Error: '&str$(err)
		fnMsgBox(mat ml$,resp$)
		!     if err=4150 then pr 'Could not create file:';file$(1) : fnpause ! file$(1) is blank!
		pr 'Err:';err;' Line:';line
	end if
	OP_XIT: !
fnend

def fn_opMain(omFileOpen$*256)
	! destination_company_number=val(env$('cno'))
	fn_automatedSavePoint('before Open')
	OpmAskWhichToOpen: !
	fnTos
	col1_width=24 : col2_pos=col1_width+2 : lc=rc=0
	fnLbl(lc+=1,1,'Source File:',col1_width,1)
	fnTxt(lc,col2_pos,30,256,0,'',1,'select any data file from the data set to be imported.  i.e. Z:\vol002\CLmstr\BankIdx.h2')
	resp$(rc+=1)=omFileOpen$
	fnLbl(lc+=2,1,'Company to Load:',col1_width,1)
	fnComboA('compList',lc,col2_pos,mat archiveList$)
	resp$(resp_fileSource:=rc+=1)=archiveList$(1)
	! fnLbl(lc+=1,1,'Destination Company Number:',col1_width,1)
	! fnTxt(lc,col2_pos,5,5,0,'1030',0,'')
	! fnLbl(lc,col2_pos+7,'(only applies if a specific Source Company is selected)')
	! resp$(resp_cnoDestination:=rc+=1)=str$(destination_company_number)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)

	dim selectedSource$*128
	selectedSource$=resp$(resp_fileSource)
	sourceWhich=srch(mat archiveList$,selectedSource$)
	if ckey=5 or sourceWhich<=0 then
		opScreenReturn=0
	else

		if ~fnConfirmOpen(omFileOpen$,selectedSource$) then goto OpmAskWhichToOpen

		if selectedSource$='(All Companies)' then
			fn_fileOpenEverything( omFileOpen$)
			opScreenReturn=1
		else
			source_company_number=archiveCNo(sourceWhich)
			destination_company_number=source_company_number
			cursys$=archiveSysAbbr$(sourceWhich)
			fnStatus('** Open Partial Settings **')
			fnStatus('Source File: '&omFileOpen$)
			fnStatus('Source System: '&cursys$)
			fnStatus('Source Company Number: '&str$(source_company_number))
			! fnStatus('Destination Company Number: '&str$(destination_company_number))
			fnStatus('**')
			fnStatus('Set current system to: '&cursys$&' from '&cursys_origional$)
			cursys$=fncursys$(cursys$)
			cno=fnPutCno(destination_company_number)
			fnStatus('Set active Company Number to: '&str$(destination_company_number))
			!
			dim omSourceFilter$*64
			if cursys$='UB' then
				omSourceFilter$='*.h'&str$(source_company_number)&' Notes.h'&str$(source_company_number)&'\*'
			else
				omSourceFilter$='*.h'&str$(source_company_number)
			end if

			fn_extract_appropriate_files(tmpFileOpen$,omSourceFilter$,env$('temp')&'\acs\OpenPartial\')
			if fn_analyze_7zip_compresslog(env$('temp')&'\acs\OpenPartial_Log.txt','Successfully Opened '&fnSystemName$&' company '&str$(destination_company_number)&' from ',omFileOpen$, 1) then
				fnreg_write('Last Open Partial Date',date$('ccyy/mm/dd'))
				fnreg_write('Last Open Partial File',omFileOpen$(pos(omFileOpen$,'\',-1)+1:len(omFileOpen$)))
				fnreg_write('Last Open Partial Path',omFileOpen$(1:pos(omFileOpen$,'\',-1)))
				fnreg_write('Last Open Partial System',env$('cursys'))
				fnreg_write('Last Open Partial Company Number',env$('cno'))
				fn_copy_files_in('[temp]\acs\OpenPartial\[cursys]mstr\','.h'&str$(source_company_number),source_company_number)
				opScreenReturn+=1
				setenv('force_reindex','yes')
				fnCheckFileVersion
				fnindex_sys(cno)
				fnStatusClose
				dim msgTmp$(0)*128
				mat msgTmp$(0)
				fnaddonec(mat msgTmp$,'Completed.')
				fnaddonec(mat msgTmp$,'Company '&str$(destination_company_number)&' loaded from')
				fnaddonec(mat msgTmp$,omFileOpen$)
				fnMsgBox(mat msgTmp$)
			end if
			if selectedSource$<>'(All Companies)' then goto OpmAskWhichToOpen
		end if
	end if
	fn_opMain=opScreenReturn
fnend
def fn_fileOpenEverything(foeSource$*256)
	fnreg_close
	dim foeDestinationFolder$*256
	dim foeLogFile$*256
	dim foeFileOpen$*256
	if fn_isClientServer then
		foeFileOpen$=env$('temp')&'\acs\OpenPartialTmpFileOpenEverything'&session$&'.zip'
		fnMakeSurePathExists(foeFileOpen$)
		fnCopyFile(foeSource$,foeFileOpen$)
	else
		foeFileOpen$=foeSource$
	end if
	foeDestinationFolder$=os_filename$(env$('Q'))
	foeLogFile$=env$('temp')&'\acs\Open_Log.txt'
	open #h_tmp=fnH: 'Name= '&br_filename$(env$('temp')&'\acs\OpenEverything_'&session$&'.cmd')&',RecL=512,Replace',d,o
	pr #h_tmp: '@echo off'
	pr #h_tmp: '@echo Advanced Computer Services LLC'
	pr #h_tmp: '@echo Opening: "'&foeSource$&'"'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Command: '&env$('path_to_7z_exe')&' x -r -aoa "'&foeSource$&'" -o"'&foeDestinationFolder$&'\" > "'&foeLogFile$&'"'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Relative To: '&foeDestinationFolder$&'\'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Output Log: "'&env$('temp')&'\acs\Open_Log.txt"'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo OPEN PROCESSING...'
	pr #h_tmp: env$('path_to_7z_exe')&' x -r -aoa "'&foeSource$&'" -o"'&foeDestinationFolder$&'\" > "'&env$('temp')&'\acs\Open_Log.txt"'
	! pr #h_tmp: 'pause'
	close #h_tmp:
	execute 'sy -s '&env$('temp')&'\acs\OpenEverything_'&session$&'.cmd'
	if fn_analyze_7zip_compresslog(env$('temp')&'\acs\Open_Log.txt','Successfully Opened',foeSource$,1) then
		fnreg_write('Last Open Date',date$('ccyy/mm/dd'))
		fnreg_write('Last Open File',foeSource$(pos(foeSource$,'\',-1)+1:len(foeSource$)))
		fnreg_write('Last Open Path',foeSource$(1:pos(foeSource$,'\',-1)))
		if fn_isClientServer then
			fnStatus('Copying Files in...')
			fnCopy(foeDestinationFolder$&'\*.*','[Q]\*.*',0,'recursive')
			opScreenReturn=1
			setenv('force_reindex','yes')
		end if
		fnCheckFileVersion
		fnindex_sys(cno)
		fnStatusClose
	else if env$('acsDebug')='Yes' then
		pr 'fn_analyze_7zip_compresslog failed.'
		pause
	end if
fnend
def fn_extract_appropriate_files(eafSourceFile$*256,eafSourceFilter$*128,eafDestinationFolder$*256)
	open #h_tmp=fnH: 'Name= '&env$('temp')&'\acs\openPartial'&session$&'.cmd,RecL=512,Replace',d,o
	pr #h_tmp: '@echo off'
	pr #h_tmp: '@echo Advanced Computer Services LLC'
	pr #h_tmp: 'set openFile="'&eafSourceFile$&'"'
	pr #h_tmp: 'set log="'&env$('temp')&'\acs\OpenPartial_Log.txt"'
	pr #h_tmp: 'set destinationDir="'&eafDestinationFolder$&'"'
	pr #h_tmp: 'set filter='&eafSourceFilter$
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Opening: %openFile%'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Command: '&env$('path_to_7z_exe')&' x -r -aoa %openFile% -o%destinationDir% %filter%'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Relative To: %destinationDir%'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo Output Log: %log%'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo.'
	pr #h_tmp: '@echo OPENING...'
	pr #h_tmp: 'RmDir %destinationDir% /s /q'
	!
	pr #h_tmp: env$('path_to_7z_exe')&' x -r -aoa %openFile% -o%destinationDir% %filter% > %log%'
	close #h_tmp:
	! if env$('acsDeveloper')<>'' and env$('cursys')='UB' then pr 'Notes.h### should be extracted too' : pause
	execute 'sy -s "'&env$('temp')&'\acs\openPartial'&session$&'.cmd"'
fnend
def fn_copy_files_in(company_import_path$*256,company_import_extension$,destination_company_number)
	fnFree('[Q]\[CurSys]mstr\*.h'&str$(destination_company_number))
	fnStatus('Existing files ('&os_filename$('[Q]\[CurSys]mstr\*.h'&str$(destination_company_number))&') have been removed.')
	cfiReturn=fnCopy(company_import_path$&'*'&company_import_extension$,'[Q]\[CurSys]mstr\*.h'&str$(destination_company_number))
	if cfiReturn>0 then
		if env$('cursys')='UB' then
			cfiReturn=fn_ub_copy_extras(company_import_path$,company_import_extension$,destination_company_number)
		end if
		if cfiReturn>0 then
			fnStatus('Import data copied in.')
		end if
	end if
	fn_copy_files_in=cfiReturn
fnend
def fn_ub_copy_extras(company_import_path$*256,company_import_extension$,destination_company_number)
	! r: import rates
	if exists(company_import_path$&'ubdata') then
		if exists('[Q]\UBmstr\ubdata\*.h'&str$(destination_company_number)) then
			fnFree('[Q]\UBmstr\ubdata\*.h'&str$(destination_company_number))
		end if  ! exists('[Q]\UBmstr\ubdata\*.h'&str$(destination_company_number))
		uceReturn=fnCopy(company_import_path$&'ubdata\*'&company_import_extension$,'[Q]\UBmstr\ubdata\*.h'&str$(destination_company_number))
		if uceReturn>0 then
			fnStatus('UBmstr\ubData found in source and is replacing destination.')
		end if
	else
		fnStatus('UBmstr\ubData did not exist in source. Destination ubData remains unchanged.')
	end if
	! /r
	! r: import notes folder
	if exists(company_import_path$&'UBmstr\notes'&company_import_extension$) then
		fnFree('[Q]\[CurSys]mstr\notes.h'&str$(destination_company_number))
		execute 'sy xcopy "'&company_import_path$&'UBmstr\notes'&company_import_extension$&'\*.*" "'&os_filename$('[Q]\UBmstr\notes.h'&str$(destination_company_number))&'\*.*" /t /y'
		fnStatus('UB Notes imported.')
	end if  ! exists [import path][Q]\UBmstr\notes.h[company_import_extension]
	! /r
	fn_ub_copy_extras=uceReturn
fnend
def library fnAutomatedSavePoint(fileNameAddition$*128)
	if ~setup then fn_setup
	fnAutomatedSavePoint=fn_automatedSavePoint(fileNameAddition$)
fnend
def fn_automatedSavePoint(fileNameAddition$*128)
	if ~env$('disableAutomatedSavePoints')='Yes' then
		dim asp_path$*256
		dim asp_filename$*256
		dim asp_saveFilter$*64
		asp_saveFilter$='[cursys]mstr\*.h[cno]'
		asp_path$='[temp]\acs\Automated Saves'
		asp_filename$='[cursys] Company [cno] '&date$('CCYY-MM-DD')&' '&srep$(time$,':','-')&' '&env$('Program_Caption')&' - '&fileNameAddition$&'.zip'
		asp_path$=rtrm$(rtrm$(asp_path$),'\')
		fnMakeSurePathExists(asp_path$&'\')
		fn_FileSaveAs(asp_saveFilter$, asp_path$&'\'&asp_filename$,1,1)
	end if
fnend
include: fn_setup