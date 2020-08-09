! Replace S:\Core\copy.br
def library fnCopy(from$*256,to$*256; new_record_length,options$)
	if ~setup the fn_setup
	fnCopy=fn_Copy(from$,to$, new_record_length,options$)
fnend
def fn_Copy(from$*256,to$*256; new_record_length,options$)
	! options$ (separate by space)  supported options$ values include
	!           recursive   - includes all subdirectories and their files
	!           errorNotify - displays a message box when an error is detected letting the user know of the failure and then proceeds as normal

	from$=fnSrepEnv$(from$)
	to$=fnSrepEnv$(to$)

	from$=trim$(from$,'"')
	to$=trim$(to$,'"')
	options$=rtrm$(options$)&' '
	copyRecursive=0
	errorNotify=0

	if from$(1:2)='@:' then fromAt$='@:' else fromAt$=''
	if to$(1:2)='@:' then toAt$='@:' else toAt$=''
	if pos(lwrc$(options$),'recursive ') then copyRecursive=1
	if pos(lwrc$(options$),'errornotify ') then errorNotify=1
	fnMakeSurePathExists(to$)
	if copyRecursive then
		fnGetPp(from$,fromPath$,fromFile$,fromExt$)
		fnGetPp(to$,toPath$,toFile$,toExt$)
		dim fromPath$*256,fromFile$*256,fromExt$*256
		dim toPath$*256,toFile$*256,toExt$*256
		dim copyFromFolder$(0)*256
		gd2_return=fnGetDir2(fromPath$,mat copyFromFolder$,'/s /b /ad')

		! pr 'gd2_return=';gd2_return : pause
		for cfi=1 to udim(mat copyFromFolder$)
			dim copyToFolder$*256
			copyToFolder$=toPath$&(copyFromFolder$(cfi)(len(srep$(fromPath$,fromAt$,''))+1:inf))
			fnMakeSurePathExists(copyToFolder$)
			fnStatus ('Creating files  in "'&copyToFolder$&'"')
include: filenamesPushMixedCase
			execute 'copy "'&fromAt$&copyFromFolder$(cfi)&'\'&fromFile$&fromExt$&'" "'&toat$&copyToFolder$&'\*.*" -n' ioerr copyFailA ! ignore because not all folders have files in them
include: filenamesPopUpperCase
			copy_return+=1
			! if int(cfi/10)=cfi/10 then pause
			copyFailA: !
include: filenamesPopUpperCase
		nex cfi
	else
		if new_record_length then
			if new_record_length and (uprc$(from$)=uprc$(to$) or to$='') then
				if to$='' then to$=from$
include: filenamesPushMixedCase
				execute 'copy "'&from$&'" "'&env$('temp')&'\acs\recl_chg_'&session$&'" -'&str$(abs(new_record_length))&' -n' ioerr COPY_FAIL
				execute 'copy "'&env$('temp')&'\acs\recl_chg_'&session$&'" "'&to$&'" -n' ioerr COPY_FAIL
				execute 'free "'&env$('temp')&'\acs\recl_chg_'&session$&'" -n' ioerr ignore
include: filenamesPopUpperCase
			end if
		end if
include: filenamesPushMixedCase
		execute 'copy "'&from$&'" "'&to$&'" -n' ioerr COPY_FAIL
include: filenamesPopUpperCase
		copy_return=1
	end if
	goto COPY_XIT
	COPY_FAIL: ! r:
include: filenamesPopUpperCase
		copy_return=min(-1,-err)
		if new_record_length then
include: filenamesPushMixedCase
			execute 'Copy "'&from$&'" "'&env$('Temp')&'\acs\tmp_rln_chg_s'&session$&'" -n' ioerr COPY_RETRY_NEW_RLN_FAILED
			execute 'Copy "'&env$('Temp')&'\acs\tmp_rln_chg_s'&session$&'" "'&to$&'" -'&str$(abs(new_record_length))&' -n' ioerr COPY_RETRY_NEW_RLN_FAILED
			execute 'Free "'&env$('Temp')&'\acs\tmp_rln_chg_s'&session$&'" -n' ioerr ignore
include: filenamesPopUpperCase
			copy_return=2
		else if errorNotify then
			fnStatus('**************************************************************************************')
			fnStatus('**** File Copy process failed! ****')
			fnStatus('Error: '&chr$(9)&str$(err))
			fnStatus('Line: '&chr$(9)&str$(line))
			fnStatus('     Source:'&chr$(9)&'"'&from$&'"')
			fnStatus('Desitnation:'&chr$(9)&'"'&to$&'"')
			fnStatus('The program will attempt to proceed as normal, but errors may occur and the current process will not complete successfully.')
			fnStatus('**************************************************************************************')
			fnStatusPause
		else if env$("ACSDeveloper")<>"" then
			pr '*** COPY FAIL ***'
			pr '     first copy failed with error ';err
			pr '     From: "'&from$&'"'
			pr '       To: "'&to$&'"'
			pause
		end if
	goto COPY_XIT ! /r
	COPY_RETRY_NEW_RLN_FAILED: ! r:
		if env$("ACSDeveloper")<>"" then
			pr '*** COPY FAIL ***'
			pr '     first copy (new record length) failed with error ';abs(copy_return)
			pr '     second attempt failed with error ';err
			pause
		end if
		copy_return=copy_return*10000-err
	goto COPY_XIT ! /r
	COPY_XIT: !
	fn_Copy=copy_return
fnend
def library fnFree(fileToDelete$*256)
	if ~setup then fn_setup
	fileToDelete$=fnSrepEnv$(fileToDelete$)
	freeReturn=0
	fileToDelete$=trim$(fileToDelete$,'"')
	if exists(fileToDelete$) then
		execute 'Free "'&fileToDelete$&'" -n' ioerr FreeErr
		freeReturn=1
	else
		freeReturn=-4152
	end if
	goto FreeXit
	FreeErr: !
	freeReturn=-err
	FreeXit: !
	fnFree=freeReturn
fnend
def library fnRename(from$*256,to$*256; ___,returnN)
	if ~setup then fn_setup
	from$=fnSrepEnv$(from$)
	to$=fnSrepEnv$(to$)
	from$=trim$(from$,'"')
	to$=trim$(to$,'"')
	if (from$(1:2)='@:' and to$(1:2)<>'@:') or (from$(1:2)<>'@:' and to$(1:2)='@:') then
		returnN=fn_Copy(from$,to$)
		if returnN then
			exec 'Free "'&from$&'"'
		end if
	else
		if exists(to$) then
			exec 'Free "'&to$&'"'
		end if
include: filenamesPushMixedCase
		execute 'Rename "'&from$&'" "'&to$&'" -n'
		returnN=1
include: filenamesPopUpperCase
	end if
	fnRename=returnN
fnend
def library fnRemoveDeletedRecords(from$*256)
	if ~setup then fn_setup
	from$=fnSrepEnv$(from$)
	rdrReturn=0
include: filenamesPushMixedCase
	execute 'copy "'&from$&'" "'&env$('temp')&'\acs\temp\Session'&session$&'\removeDeletedRecords.tmp" -n' ioerr RdrFail
	execute 'copy "'&env$('temp')&'\acs\temp\Session'&session$&'" "'&from$&'\removeDeletedRecords.tmp" -D' ioerr RdrFail
include: filenamesPopUpperCase
	execute 'free "'&env$('temp')&'\acs\temp\Session'&session$&'\removeDeletedRecords.tmp" -n' ioerr ignore
	rdrReturn=1
	goto RdrFinis
	RdrFail: !
include: filenamesPopUpperCase
	rdrReturn=-err
	goto RdrFinis
	RdrFinis: !
	fnRemoveDeletedRecords=rdrReturn
fnend
include: fn_setup
