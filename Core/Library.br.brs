! test changes
! r: functions that do not redirect
	def library fnVal(stringToConvert$*128; ___,returnN)
		returnN=val(stringToConvert$) conv ValConv
		goto ValXit
		ValConv: !
			stringToConvert$=srep$(stringToConvert$,'$','')
			stringToConvert$=srep$(stringToConvert$,',','')
			stringToConvert$=srep$(stringToConvert$,'"','')
			stringToConvert$=trim$(stringToConvert$)
			returnN=val(stringToConvert$) conv ignore
		goto ValXit
		ValXit: !
		fnVal=returnN
	fnend

	def library fnSaveToAsStart(filenameToCopyTo$*400)
		setEnv('saveToAsStart',trim$(filenameToCopyTo$))
	fnend
	def library fnBrFilename$*512(filename$*512; return$*512)
		if trim$(filename$)='' then
			return$=''
		else
			return$=br_filename$(filename$)
		end if
		fnBrFilename$=return$
	fnend
	def library fnOsFilename$*512(filename$*512; return$*512)
		if trim$(filename$)='' then
			return$=''
		else
			return$=os_filename$(filename$)
		end if
		fnOsFilename$=return$
	fnend

	def library fnKeyExists(hFile,&keyToTest$; attemptFix,___,returnN, _
		origionalKey$*256,attemptFix2Count,tried0,triedDot0,triedDot00,lenTmp,dotPos,tmp$*128)
		! attemptFix =1 try it: As-Is, Left-padded, Right-padded
		!            =2 also try it: with adding 0, 00, .00 in all of the previous ways
		origionalKey$=keyToTest$
		! if trim$(origionalKey$)='101650' then pr 'yes' : pause
		keyToTest$=rpad$(keyToTest$,kLn(hFile))
		read #hFile,key=keyToTest$,release: nokey KeTop
		returnN=1
		goto MaeFinis

		KeTop: !
		if attemptFix then

			keyToTest$=lpad$(rtrm$(keyToTest$),kln(hFile))
			read #hFile,key=keyToTest$,release: nokey KeAlign1
			returnN=1
			goto MaeFinis

			KeAlign1: ! try lpad(trim)
				keyToTest$=lpad$(trim$(keyToTest$),kln(hFile))
				read #hFile,key=keyToTest$,release: nokey KeAlign2
				returnN=1
			goto MaeFinis
			KeAlign2: ! try rpad(trim)
				keyToTest$=rpad$(trim$(keyToTest$),kln(hFile))
				read #hFile,key=keyToTest$,release: nokey KeAlign3
				returnN=1
			goto MaeFinis
			KeAlign3: !

			if attemptFix=>2 then ! treat it like a mistreated UB Customer number - look for missing trailing 0 or .00
				! attempt additions to make it work (may have been truncated)
				! attemptFix2Count+=1

				tmp$=trim$(keyToTest$)
				lenTmp=len(tmp$)
				dotPos=pos(tmp$,'.',-1)
				if dotPos<=0 and ~triedDot00 then
					keyToTest$=tmp$&'.00' soflow PastDot00
					triedDot00=1
					goto KeTop
				end if
				PastDot00: !


				if dotPos=len(tmp$)-1 and ~tried0 then
					keyToTest$=tmp$&'0' soflow Past0
					tried0=1
					goto KeTop
				end if
				Past0: !


				! removed because never finished developing - very unusual situation anyway
				! if dotPos<=0 and ~triedDot0 then
				! 	keyToTest$=tmp$&'.0' soflow PastDot0
				! 	triedDot0=1
				! 	goto KeTop
				! end if
				! PastDot0: !


			end if
			MaeJustFail: !
			! if attemptFix2Count<4 goto KeTop
			returnN=0
			keyToTest$=origionalKey$
		end if
		goto MaeFinis
		MaeFinis: !
		fnKeyExists=returnN
	fnend

	def library fnSetEnv(from$*256,to$*256; conSubOnly, ___,quoteF$*1,quoteT$*1,fromLen,returnN) ! it works but it is currently unused.
		! setenv(from$,to$)
		if from$(1:1)<>'[' then from$(0:0)='['
		fromLen=len(from$)
		if from$(fromLen:fromLen)<>']' then from$&=']'
		if pos(from$,' ')>0 then	quoteF$='"'
		if pos(to$,' ')>0 then		quoteT$='"'
		! pr 'config substitute '&quoteF$&from$&quoteF$&' '&quoteT$&to$&quoteT$
		returnN=0
		if ~conSubOnly then
			setenv(from$(2:len(from$)-1),to$) error SetEnvFinis
		end if
		exe 'config substitute '&quoteF$&from$&quoteF$&' '&quoteT$&to$&quoteT$
		returnN=1
		SetEnvFinis: !
		fnSetEnv=returnN
	fnend
	def library fnSrepEnv$*2048(text$*2048; exclude$*64,___,sePosOpen,sePosClose,seVariable$*128,seStartSearchPos)
		do
			sePosOpen =pos(text$,'[', seStartSearchPos)
			sePosClose=pos(text$,']', max(seStartSearchPos,sePosOpen))
			if sePosOpen>0 and sePosClose>sePosOpen then
				if lwrc$(text$(sePosOpen:sePosClose))=lwrc$(exclude$)  then
					seStartSearchPos=sePosClose+1
				else
					seVariable$=text$(sePosOpen+1:sePosClose-1)
					! pr 'changing ['&seVariable$&'] to '&env$(seVariable$)&'.' : pause
					text$=srep$(text$,'['&seVariable$&']',env$(seVariable$))
				end if
			end if
		loop while sePosOpen>0 and sePosClose>sePosOpen
		fnSrepEnv$=text$
	fnend
	def library fnFixPd(mat arrayOrVariableToFix; ___,fpReturn,fpItem)
		fpReturn=0
		for fpItem=1 to udim(mat arrayOrVariableToFix)
			if str$(arrayOrVariableToFix(fpItem))(1:5)='-2020' then
				arrayOrVariableToFix(fpItem)=0
				fpReturn+=1
			end if
		nex fpItem
		fnFixPd=fpReturn
	fnend
	def library fnCd(x)
		fncd=(x-int(x*.01)*100)*10000+int(x*.01)
	fnend
	def library fnFormNumb$(numb,decimals,size)
		fnformnumb$=lpad$(cnvrt$('N 10.'&str$(decimals),numb),size)
	fnend
	def library fnpause(;unused)
		if env$('ACSDeveloper')<>'' then pr 'fnpause enacted.' : exe 'go XITPAUSE step'
	XITPAUSE: fnend

	def library fnGetUseDeptAndSub(&useDept,&useSub; ___,guSys$,gudsSetupTimeN)
		! find out if I should use the department number and/or the sub account number
		! only reads once every three seconds, but resets counter if checked within 3 seconds
		! Retains: gudsSetup$,useDeptCache,useSubCache
		! Guds_

		guSys$=env$('CurSys')
		if env$('CurSys')='UB' and exists('[Q]\GLmstr\Company.h[cno]') then guSys$='GL': goto Guds_CoOpen
		if env$('CurSys')='UB' and exists('[Q]\UBmstr\GLmstr.h[cno]' ) then guSys$='UB': goto GudsDefault
		if env$('CurSys')='PR' and exists('[Q]\GLmstr\Company.h[cno]') then guSys$='GL': goto Guds_CoOpen
		if env$('CurSys')='PR' and exists('[Q]\CLmstr\Company.h[cno]') then guSys$='CL': goto Guds_CoOpen
		if env$('CurSys')='PR' and exists('[Q]\PRmstr\glmstr.h[cno]' ) then guSys$='PR': goto Guds_CoOpen
		if env$('CurSys')='CR' and exists('[Q]\GLmstr\GLmstr.h[cno]' ) then guSys$='GL': goto Guds_CoOpen
		if env$('CurSys')='CR' and ~exists('[Q]\GLmstr\GLmstr.h[cno]') then guSys$='CR': goto Guds_CoOpen
		if env$('CurSys')='CL' then guSys$='CL' else guSys$='GL'

		Guds_CoOpen: !
			if guSys$='GL' or guSys$='CL' then

				gudsSetupTimeN=val(gudsSetup$(4:inf))
				if gudsSetup$(1:2)<>guSys$ or fn_stime>gudsSetupTimeN+3 then
					gudsSetup$=guSys$&'-'&str$(fn_stime)
					open #hCompany=fnH: 'Name=[Q]\'&guSys$&'mstr\Company.h[cno],Shr',internal,input ioerr GudsDefault
					read #hCompany,using 'Form Pos 150,2*N 1': useDept,useSub
					close #hCompany: ioerr ignore
					useDeptCache=useDept
					useSubCache =useSub
				else
					gudsSetup$=guSys$&'-'&str$(fn_stime)
					useDept=useDeptCache
					useSub =useSubCache
				end if
			else if guSys$='PR' or guSys$='UB' or guSys$='CR' then
				GudsDefault: ! default both to use
				useDept=useSub=1
			end if

	fnend
	def fn_stime(; returnN,___,tmp$*8)
	tmp$=time$  ! i.e. 10:31:52
	returnN+=val(time$(1:2))*60*60
	returnN+=val(time$(4:5))*60
	returnN+=val(time$(7:8))
	fn_stime=returnN
	fnend


	! 6/17/21 - JB - moved the fnAddOne* here from S:\Core\Array to hopefully increase speed of these very heavy use utilities
	def library fnAddOneC(mat addTo$,one$*2048; skipBlanks,skipDupes,___,size)
		! must dim an array to 0 before you can add a first item
		!    Mat addTo$ - the array to add One$ item to
		!    One$ - the One$ item to add to Mat addTo$
		!    skipBlanks - if =1 than only add One$ if Trim$(One$)<>''
		!    skipDupes - if =1 than only add One$ if One$ is not yet in Mat addTo$
		!    This function returns the number of items in the array
		size=udim(mat addTo$)
		if ~skipBlanks or (skipBlanks and trim$(one$)<>'') then
			if ~skipDupes or (skipDupes and srch(mat addTo$,one$)<=0) then
				size+=1
				mat addTo$(size)
				addTo$(size)=one$
			end if
		end if
		fnAddOneC=size
	fnend
	def library fnAddOneN(mat addTo,oneN; skipZeros,skipDupes,___,size)
		! must dim an array to 0 before you can add a first item
		!    Mat addTo - the array to add oneN item to
		!    oneN - the oneN item to add to Mat addTo
		!    skipZeros - if =1 than only add oneN if oneN<>0
		!    skipDupes - if =1 than only add oneN if oneN is not yet in Mat addTo
		!    This function returns the number of items in the array
		size=udim(mat addTo)
		if ~skipZeros or (skipZeros and oneN) then
			if ~skipDupes or (skipDupes and srch(mat addTo,oneN)<=0) then
				size+=1
				mat addTo(size)
				addTo(size)=oneN
			end if
		end if
		fnAddOneN=size
	fnend

! /r
! r: S:\Core\Start.br
	def library fnWriteProc(procName$*64,procLine$*256)
		library 'S:\Core\Start.br': fnWriteProc
		fnWriteProc=fnWriteProc(procName$,procLine$)
	fnend
	def library fnSetQ(setQ$*256)
		library 'S:\Core\Start.br': fnSetQ
		fnSetQ=fnSetQ(setQ$)
	fnend
	def library fnH
		library 'S:\Core\Start.br': fnH
		fnH=fnH
	fnend
	def library fnAcsSystemInitialize(; isScreenIOtest)
		library 'S:\Core\Start.br': fnAcsSystemInitialize
		fnAcsSystemInitialize=fnAcsSystemInitialize( isScreenIOtest)
	fnend
! /r
def library fnSpecialFolderPath$*256(folderName$*64)
	library 'S:\Core\specialFolderPath.br': fnSpecialFolderPath$
	fnSpecialFolderPath$=fnSpecialFolderPath$(folderName$)
fnend
def library fnProgramDataDir$*256
	library 'S:\Core\Start.br': fnProgramDataDir$
	fnProgramDataDir$=fnProgramDataDir$
fnend
def library fnSwithEmail(mat toEmail$,emailMessage$*10000; subject$*256,mat attachFile$,mat ccEmails$,mat bccEmail$)
	library 'S:\Core\SwithMail\SwithMail.br': fnSwithEmail
	fnSendEmail=fnSwithEmail(mat toEmail$,emailMessage$, subject$,mat attachFile$,mat ccEmails$,mat bccEmail$)
fnend

! r: ScreenIO
def library fnDesignScreen
	library 'S:\Core\ScreenIO\screenio.br': fnDesignScreen
	fnDesignScreen=fnDesignScreen
fnend
! def library fnfm(screenname$; keyval$*255,srow,scol,parent_key$*255,parent_window,display_only,dontRedoListView,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$*255,selecting,savedontask)
! 	library 'S:\Core\ScreenIO\screenio.br': fnfm
! 	fnfm=fnfm(screenname$, keyval$,srow,scol,parent_key$,parent_window,display_only,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$,selecting,savedontask)
! fnend
! def library fnfm$(screenname$; keyval$*255,srow,scol,parent_key$*255,parent_window,display_only,dontRedoListView,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$*255,selecting,savedontask)
! 	library 'S:\Core\ScreenIO\screenio.br': fnfm$
! 	fnfm$=fnfm$(screenname$, keyval$,srow,scol,parent_key$,parent_window,display_only,dontredolistview,recordval,mat passeddata$,usemyf,mat myf$,mat myf,path$,selecting,savedontask)
! fnend
! /r
! r: FileIO
def library fnProgressBar(percent)
	library 'S:\Core\FileIO\fileio.br': fnProgressBar
	fnProgressBar=fnProgressBar(percent)
fnend
def library fnProgressBarClose
	library 'S:\Core\FileIO\fileio.br': fnCloseBar
	fnProgressBarClose=fnCloseBar
fnend

def library fnReadLayoutHeader(layoutName$*255; &fileName$,mat key$,mat keyDescription$,leaveOpen,&prefix$)
	library 'S:\Core\FileIO\fileio.br': fnReadLayoutHeader
	fnReadLayoutHeader=fnReadLayoutHeader(layoutName$, fileName$,mat key$,mat keyDescription$,leaveOpen,prefix$)
fnend
def library fnFileioEnums(filename$*255; mat subs$)
	library 'S:\Core\FileIO\fileio.br': fnMakeSubProc
	fnFileioEnums=fnMakeSubProc(filename$, mat subs$)
fnend
def library fnOpenFile(filename$*64,mat d$,mat dn,mat form$; inputonly,keynum,dont_sort_subs,&path$,mat description$,mat fieldwidths,mat fileiosubs$,supressprompt,ignoreerrors,callingprogram$*255,suppresslog)
	! fnOpenFile(&filename$,mat f$,mat fn,mat form$;inputonly,keynum,dont_sort_subs,&path$,mat description$,mat fieldwidths,mat fileiosubs$,supressprompt,ignoreerrors,callingprogram$*255,suppresslog)
	library 'S:\Core\FileIO\fileio.br': fnOpenFile
	fnOpenFile=fnOpenFile(filename$,mat d$,mat dn,mat form$, inputonly,keynum,dont_sort_subs,path$,mat description$,mat fieldwidths,mat fileiosubs$,supressprompt,ignoreerrors,callingprogram$,suppresslog)
fnend
def library fnMakeSurePathExists(Filename$*255; Path$*255) ! mkdir funciton from fileio.brs - except fileio version is not a library 2/7/2017
	library 'S:\Core\fn\makeSurePathExists.br':fnMakeSurePathExists
	fnMakeSurePathExists=fnMakeSurePathExists(Filename$, Path$)
fnend
def library fnBuildKey$*255(layout$*30,mat bkf$,mat bkf; keynum)
	library 'S:\Core\FileIO\fileio.br': fnBuildKey$
	fnBuildKey$=fnBuildKey$(layout$,mat bkf$,mat bkf, keynum)
fnend
def library fnCloseFile(filenumber,filelay$*255; path$*255,out)
	library 'S:\Core\FileIO\fileio.br': fnCloseFile
	fnCloseFile=fnCloseFile(filenumber,filelay$, path$,out)
fnend
def library fnClearLayoutCache
	library 'S:\Core\FileIO\fileio.br': fnClearLayoutCache
	fnClearLayoutCache=fnClearLayoutCache
fnend
def library fnCopyFile(FromFile$*255,ToFile$*255; NoProgressBar)
	library 'S:\Core\FileIO\fileio.br': fnCopyFile
	fnCopyFile=fnCopyFile(FromFile$,ToFile$, NoProgressBar)
fnend
def library fnReIndex(fileioLayout$*255; indexNum)
	library 'S:\Core\FileIO\fileio.br': fnReIndex
	fnReIndex=fnReIndex(fileioLayout$, env$('Program_Caption'),indexNum) ! ,path$*255)
fnend
! /r

! r: Confirm
def library fnConfirm(Verb$*64; textAddition$*2048,Confirm_Dont_Ask_Again_Key$*28)
	library 'S:\Core\Confirm.br': fnConfirm
	fnConfirm=fnConfirm(Verb$, textAddition$,Confirm_Dont_Ask_Again_Key$)
fnend
def library fnConfirmHard(chVerb$; whatYouAreVerbingGeneral$*20,whatYouAreVerbingSpecific$*60)
	library 'S:\Core\Confirm.br': fnConfirmHard
	fnConfirmHard=fnConfirmHard(chVerb$, whatYouAreVerbingGeneral$,whatYouAreVerbingSpecific$)
fnend
def library fnConfirmDelete(What_You_Deleting$*60; Confirm_Dont_Ask_Again_Key$*28)
	library 'S:\Core\Confirm.br': fnConfirmDelete
	fnConfirmDelete=fnConfirmDelete(What_You_Deleting$,Confirm_Dont_Ask_Again_Key$)
fnend
def library fnConfirmDeleteHard(whatYouAreDeletingGeneral$*32,whatYouAreDeletingSpecific$*60)
	library 'S:\Core\Confirm.br': fnConfirmDeleteHard
	fnConfirmDeleteHard=fnConfirmDeleteHard(whatYouAreDeletingGeneral$,whatYouAreDeletingSpecific$)
fnend
def library fnConfirmOpen(filename$*256,filterDesc$*128)
	library 'S:\Core\Confirm.br': fnConfirmOpen
	fnConfirmOpen=fnConfirmOpen(filename$, filterDesc$)
fnend
! /r
def library fnEncodeBase64(&content$)
	library 'S:\Core\base64_l.br': fnEncodeBase64
	fnEncodeBase64=fnEncodeBase64(content$)
fnend
def library fnDecodeBase64(&Content$)
	library 'S:\Core\base64_l.br': fnDecodeBase64
	fnDecodeBase64=fnDecodeBase64(Content$)
fnend
def library fnKeyChange(h_filehandle,f_fileform_key_only$*128,key_from$*128,key_to$*128)
	library 'S:\Core\key.br': fnKeyChange
	fnKeyChange=fnKeyChange(h_filehandle,f_fileform_key_only$,key_from$,key_to$)
fnend
def library fnKeyDelete(h_filehandle,f_fileform_key_only$*128,key$*128)
	library 'S:\Core\key.br': fnKeyDelete
	fnKeyDelete=fnKeyDelete(h_filehandle,f_fileform_key_only$,key$)
fnend
def library fnFkey(scrline,mat fkey$,mat disfk,&em$,es)
	library 'S:\Core\ace\win3b.br': fnfkey
	fnfkey=fnfkey(scrline,mat fkey$,mat disfk,em$,es)
fnend
def library fnWin3(win,&cap$,wh,ww,dc,bo,win_align)
	library 'S:\Core\ace\Win3b.br': fnwin3b
	fnwin3b(win, cap$,wh,ww,dc,bo,win_align,0)
fnend
def library fnOpenWin(win,sr,sc,er,ec,&cap$)
	library 'S:\Core\OpenWin.br': fnopenwin
	fnopenwin=fnopenwin(win,sr,sc,er,ec, cap$)
fnend
def library fnCopy(from$*256,to$*256; new_record_length,options$*256)
	library 'S:\Core\copy.br': fnCopy
	fnCopy=fnCopy(from$,to$, new_record_length,options$)
fnend
def library fnFree(fileToDelete$*256)
	library 'S:\Core\copy.br': fnFree
	fnFree=fnFree(fileToDelete$)
fnend
def library fnRename(from$*256,to$*256)
	library 'S:\Core\copy.br': fnRename
	fnRename=fnRename(from$,to$)
fnend
def library fnRemoveDeletedRecords(from$*256)
	library 'S:\Core\copy.br': fnRemoveDeletedRecords
	fnRemoveDeletedRecords=fnRemoveDeletedRecords(from$)
fnend
def library fnWaitForShellCloseStart(whatsRunning$*256)
	library 'S:\Core\Print.br': fnWaitForShellCloseStart
	fnWaitForShellCloseStart=fnWaitForShellCloseStart(whatsRunning$)
fnend
def library fnWaitForShellCloseEnd
	library 'S:\Core\Print.br': fnWaitForShellCloseEnd
	fnWaitForShellCloseEnd=fnWaitForShellCloseEnd
fnend
! r: fnSnap
def library fnlistprint(winno,spec$*100;header$*200,footer$*200,title$*200,mat selected,nolines,nosort,nototals$*200,nosubtotal,_print)
	library 'S:\Core\fnsnap\rtflib_dll.br': fnlistprint
	fnlistprint=fnlistprint(winno,spec$, header$,footer$,title$,mat selected,nolines,nosort,nototals$,nosubtotal,_print)
fnend
def library fnMsExe$*256(l$)
	library 'S:\Core\fnSnap\fnMsExe.br': fnMsExe$
	fnMsExe$=fnMsExe$(l$)
fnend
! /r
! r: Client
	def library fnClientNameShort$(clientId$)
		library 'S:\Core\Client.br': fnClientNameShort$
		fnClientNameShort$=fnClientNameShort$(clientId$)
	fnend
	def library fnSetClient(clientSelected$*128)
		library 'S:\Core\Client.br': fnSetClient
		fnSetClient=fnSetClient(clientSelected$)
	fnend
	def library fnClientSelect
		library 'S:\Core\Client.br': fnClientSelect
		fnClientSelect=fnClientSelect
	fnend
	def library fnsystem_code_standardize$(st_code$*2)
		library 'S:\Core\Client.br': fnsystem_code_standardize$
		fnsystem_code_standardize$=fnsystem_code_standardize$(st_code$)
	fnend
	def library fnclient$
		library 'S:\Core\Client.br': fnclient$
		fnclient$=fnclient$
	fnend
	def library fnclient_has_mat(mat c_has$)
		library 'S:\Core\Client.br': fnclient_has_mat
		fnclient_has_mat=fnclient_has_mat(mat c_has$)
	fnend
	def library fnClientHas(ch_sys$*256)
		library 'S:\Core\Client.br': fnClientHas
		fnClientHas=fnClientHas(ch_sys$)
	fnend
	def library fnub_printbill_program$*256
		library 'S:\Core\Client.br': fnub_printbill_program$
		fnub_printbill_program$=fnub_printbill_program$
	fnend
	def library fnpayroll_client_state$
		library 'S:\Core\Client.br': fnpayroll_client_state$
		fnpayroll_client_state$=fnpayroll_client_state$
	fnend
	def library fnclient_is_converting
		library 'S:\Core\Client.br': fnclient_is_converting
		fnclient_is_converting=fnclient_is_converting
	fnend
	def library fnclient_has_on_support_item(chosi_item$*2; grace_days)
		library 'S:\Core\Client.br': fnclient_has_on_support_item
		fnclient_has_on_support_item=fnclient_has_on_support_item(chosi_item$, grace_days)
	fnend
	def library fnclient_has_on_support_list(mat chosl_list$; chosl_grace_days)
		library 'S:\Core\Client.br': fnclient_has_on_support_list
		fnclient_has_on_support_list=fnclient_has_on_support_list(mat chosl_list$, chosl_grace_days)
	fnend
	def library fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support; css_grace_days)
		library 'S:\Core\Client.br': fnclient_support
		fnclient_support=fnclient_support(mat css_system_id$,mat css_system_support_end_date,mat css_on_support, css_grace_days)
	fnend
! /r
	def library fnUpdateLicense
		library 'S:\Core\Update.br': fnUpdateLicense
		fnUpdateLicense=fnUpdateLicense
	fnend
! r: core W-2, W-3, 1099 stuff
	def library fnask_w2_info(&taxYear$,&beg_date,&end_date,&empStart$,&empEnd$,&ssrate,&ssmax,&mcrate,&mcmax,mat w2destinationOpt$,&enableW3$,&enableBackground$,&w2Copy,&w2Copy$,&exportFormatID,&w2laser_output_filename$,&pn1,&dc1,&topmargin,&bottom,&state$,enableAskCLocality,&cLocality$)
		library 'S:\Core\Print\w2.br': fnask_w2_info
		fnask_w2_info=fnask_w2_info(taxYear$,beg_date,end_date,empStart$,empEnd$,ssrate,ssmax,mcrate,mcmax,mat w2destinationOpt$,enableW3$,enableBackground$,w2Copy,w2Copy$,exportFormatID,w2laser_output_filename$,pn1,dc1,topmargin,bottom,state$,enableAskCLocality,cLocality$)
	fnend
	def library fnFormCopyAwithBackgroundWarn
		library 'S:\Core\Print\w2.br': fnFormCopyAwithBackgroundWarn
		fnFormCopyAwithBackgroundWarn=fnFormCopyAwithBackgroundWarn
	fnend
	def library fnw2_text(w2Yoffset,maskSsn,mat a$,empId$*12,ss$,controlNumber$,mat w,dcb$,nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$*6; box14Amt)
		library 'S:\Core\Print\w2.br': fnw2_text
		fnw2_text=fnw2_text(w2Yoffset,maskSsn,mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$, box14Amt)
	fnend
	def library fnw3(taxYear$,empId$,mat a$,mat w,dcb,state$,stcode$)
		library 'S:\Core\Print\w3.br': fnw3
		fnw3=fnw3(taxYear$,empId$,mat a$,mat w,dcb,state$,stcode$)
	fnend
	def library fnNameParse(fullname$*128,&nameFirst$,&nameMiddle$,&nameLast$,&nameSuffix$)
		library 'S:\Core\Print\w2.br': fnNameParse
		fnNameParse=fnNameParse(fullname$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
	fnend
	def library fn_FormCopyAwithBackgroundWarn
		library 'S:\Core\Print\w2.br': fn_FormCopyAwithBackgroundWarn
	fn_FormCopyAwithBackgroundWarn=fn_FormCopyAwithBackgroundWarn
	fnend
	def library fn1099print(vn$*8,nam$*30,mat ad$,ss$*11,mat box)
		library 'S:\Core\Programs\1099.br': fn1099print
		fn1099print=fn1099print(vn$,nam$,mat ad$,ss$,mat box)
	fnend
	def library fn1099print_close
		library 'S:\Core\Programs\1099.br': fn1099print_close
		fn1099print_close=fn1099print_close
	fnend
	def library fnask_1099_info(&seltp,&type,&min1,&beg_date,&end_date)
		library 'S:\Core\Programs\1099.br': fnask_1099_info
		fnask_1099_info=fnask_1099_info(seltp,type,min1,beg_date,end_date)
	fnend
! /r
! r: Favorites
	def library fnFavoriteAdd(programCaption$*256)
		library 'S:\Core\Favorites.br': fnFavoriteAdd
		fnFavoriteAdd=fnFavoriteAdd(programCaption$)
	fnend
	def library fnFavoriteDel(programCaption$*256)
		library 'S:\Core\Favorites.br': fnFavoriteDel
		fnFavoriteDel=fnFavoriteDel(programCaption$)
	fnend
	def library fnFavoriteList(mat favorite$)
		library 'S:\Core\Favorites.br': fnFavoriteList
		fnFavoriteList=fnFavoriteList(mat favorite$)
	fnend
! /r
! r: File Open and Save
	def library fnAskFileName(&opFileOpen$,purpose$; filter$,filterDescription$*64,path$*256,recallAddOn$*64)
		library 'S:\Core\fn\askFileName.br': fnAskFileName
		fnAskFileName=fnAskFileName(opFileOpen$,purpose$, filter$,filterDescription$,path$,recallAddOn$)
	fnend
	def library fnOpenPartial
		library 'S:\Core\File Open and Save.br': fnOpenPartial
		fnOpenPartial=fnOpenPartial
	fnend
	def library fnFileSaveAs(save_what$; saveFileName$*256)
		library 'S:\Core\File Open and Save.br': fnFileSaveAs
		fnFileSaveAs=fnFileSaveAs(save_what$, saveFileName$)
	fnend
	def library fnAutomatedSavePoint(fileNameAddition$*128)
		library 'S:\Core\File Open and Save.br': fnAutomatedSavePoint
		fnAutomatedSavePoint=fnAutomatedSavePoint(fileNameAddition$)
	fnend
! /r
! r: Status ( a pop up window that displays a running status of progress
	def library fnStatus(text$*512)
		library 'S:\Core\status.br': fnStatus
		fnStatus=fnStatus(text$)
	fnend
	def library fnStatusPause
		library 'S:\Core\status.br': fnStatusPause
		fnStatusPause=fnStatusPause
	fnend
	def library fnStatusClose
		library 'S:\Core\status.br': fnStatusClose
		fnStatusClose=fnStatusClose
	fnend
! /r
! r: core   all the libraries that aren't filed anywhere else
	def library fnWindowsStart(wsFile$*1024)
		library 'S:\Core\fn\windowsStart.br': fnWindowsStart
		fnWindowsStart=fnWindowsStart(wsFile$)
	fnend
	def library fnCheckCompiled
		library 'S:\Core\Compile.br': fnCheckCompiled
		fnCheckCompiled=fnCheckCompiled
	fnend
	def library fnbooktitle$*256(x$*256)
		library 'S:\Core\booktitle.br': fnbooktitle$
		fnbooktitle$=fnbooktitle$(x$)
	fnend
	def library fnsave_as_path$*256
		library 'S:\Core\Programs\Preferences.br': fnsave_as_path$
		fnsave_as_path$=fnsave_as_path$
	fnend
	def library fndecimal_assumed
		library 'S:\Core\Programs\Preferences.br': fndecimal_assumed
		fndecimal_assumed=fndecimal_assumed
	fnend
	def library fnget_wordprocessor_exe(&wp_exe$; force$)
		library 'S:\Core\Programs\Preferences.br': fnget_wordprocessor_exe
		fnget_wordprocessor_exe=fnget_wordprocessor_exe(wp_exe$, force$)
	fnend
	def library fnEditFile(editorType$,fileToEdit$*256)
		library 'S:\Core\Programs\Preferences.br': fnEditFile
		fnEditFile=fnEditFile(editorType$,fileToEdit$)
	fnend
	def library fnapply_theme(; disableConScreenOpenDflt)
		library 'S:\Core\Programs\Preferences.br': fnapply_theme
		fnapply_theme=fnapply_theme( disableConScreenOpenDflt)
	fnend
	def library fnprogram_properties(; forceProgramCaption$*256)
		library 'S:\Core\program_properties.br': fnprogram_properties
		fnprogram_properties=fnprogram_properties( forceProgramCaption$)
	fnend
	def library fnFixWordingOnGrid(ev$*50,outputfile$*50)
		library 'S:\Core\fnFixWordingOnGrid.br': fnFixWordingOnGrid
		fnFixWordingOnGrid=fnFixWordingOnGrid(ev$,outputfile$)
	fnend

	def library fnIndex(data_file$*256,index_statement$*512; index_parameters$*256)
		library 'S:\Core\Index.br': fnIndex
		fnIndex=fnIndex(data_file$,index_statement$, index_parameters$)
	fnend
	def library fnindex_sys(; only_cno,system_id$*256)
		library 'S:\Core\Index.br': fnindex_sys
		fnindex_sys=fnindex_sys( only_cno,system_id$)
	fnend
	def library fnub_index_customer
		library 'S:\Core\Index.br': fnub_index_customer
		fnub_index_customer=fnub_index_customer
	fnend
	def library fnAcsInstallationPath$*256(; longFileName)
		library 'S:\Core\Programs\Update.br': fnAcsInstallationPath$
		fnAcsInstallationPath$=fnAcsInstallationPath$( longFileName)
	fnend
	def library fnqgl(myline,mypos; container,x,forceGLsysIfPossible,qgllength)
		library 'S:\Core\ACS_Component.br': fnqgl
		fnqgl=fnqgl(myline,mypos,container,x,forceGLsysIfPossible,qgllength)
	fnend
	def library fnqglbig(myline,mypos; container,x,forceGLsysIfPossible)
		library 'S:\Core\ACS_Component.br': fnqgl
		fnqglbig=fnqgl(myline,mypos,container,x,forceGLsysIfPossible,60)
	fnend
	def library fnqgl25(myline,mypos; container,x,forceGLsysIfPossible)
		library 'S:\Core\ACS_Component.br': fnqgl
		fnqgl=fnqgl(myline,mypos,container,x,forceGLsysIfPossible,25)
	fnend
	def library fnagl$*12(&x$)
		library 'S:\Core\fn\agl$.br': fnagl$
		fnagl$=fnagl$(x$)
	fnend
	def library fnrgl$*60(x$; returnmaxlength,leaveDescFileOpen) ! passed '  1  101  1' returns '1-101-1 Account Description'
		library 'S:\Core\fn\rgl$.br': fnrgl$
		fnrgl$=fnrgl$(x$, returnmaxlength,leaveDescFileOpen)
	fnend
	def library fnrglbig$*60(x$)
		! library 'S:\Core\fnRGLbig$.br': fnrglbig$
		! fnrglbig$=fnrglbig$(x$)
		library 'S:\Core\fn\rgl$.br': fnrgl$
		fnrglbig$=fnrgl$(x$, 60)
	fnend
	def library fnosver(&osver$;get_or_put)
		library 'S:\Core\OSVer.br': fnosver
		fnosver=fnosver(osver$,get_or_put)
	fnend
	def library fndec2hex(input_dec,&output_hex$)
		library 'S:\Core\Dec2Hex.br': fndec2hex
		fndec2hex=fndec2hex(input_dec, output_hex$)
	fnend
	def library fnhex2dec(input_hex$)
		library 'S:\Core\fn\hex2dec.br': fnhex2dec
		fnhex2dec=fnhex2dec(input_hex$)
	fnend
	def library fnwin3b(win,&cap$,win_height,win_width; display_cnam,button_option,win_align,pr_newpg)
		library 'S:\Core\Ace\Win3B.br': fnwin3b
		fnwin3b=fnwin3b(win,cap$,win_height,win_width, display_cnam,button_option,win_align,pr_newpg)
	fnend
	def library fngetcd(&mcd$)
		library 'S:\Core\Ace\GetCD.br': fngetcd
		fngetcd=fngetcd(mcd$)
	fnend
	def library fnXit(;cursys$)
		library 'S:\Core\fn\Xit.br': fnXit
		fnXit=fnXit(cursys$)
	fnend
	def library fninch2twip(&x)
		library 'S:\Core\Inch2Twip.br': fninch2twip
		fninch2twip=fninch2twip(x)
	fnend
	def library fntwip2inch(&x)
		library 'S:\Core\Twip2Inch.br': fntwip2inch
		fntwip2inch=fntwip2inch(x)
	fnend
	def library fnChain(prg$*255; no_fnprg_setting,noLog)
		library 'S:\Core\fn\chain.br': fnChain
		fnChain=fnChain(prg$, no_fnprg_setting,noLog)
	fnend
	def library fnError(callingProgram$*256,errornumber,linenumber,&ertnAct$,stopable$)
		library 'S:\Core\fn\error.br': fnError
		fnError=fnError(callingProgram$,errornumber,linenumber,ertnAct$,stopable$)
	fnend
	def library fnlog(log$*512; x)
		library 'S:\Core\Log.br': fnlog
		fnlog=fnlog(log$, x)
	fnend
	def library fngetdir(&dir$,mat filename$; option$,filter$*40)
		library 'S:\Core\fn\getDir.br': fngetdir
		fngetdir=fngetdir(dir$,mat filename$, option$,filter$)
	fnend
	def library fnGetDir2(dir$*256,mat filename$; option$,filter$*40,mat fileDate$,mat fileTime$,forceFullPath,mat fileSize)
		library 'S:\Core\fn\getDir2.br': fngetdir2
		fngetdir2=fnGetDir2(dir$,mat filename$, option$,filter$,mat fileDate$,mat fileTime$,forceFullPath,mat fileSize)
	fnend
	def library fnGetDirClient(dir$*256,mat filename$; filter$*40)
		library 'S:\Core\fn\getDir2.br': fnGetDirClient
		fnGetDirClient=fnGetDirClient(dir$,mat filename$, filter$)
	fnend
	!
	def library fnwait(; message$*40,stopable)
		library 'S:\Core\fn\wait.br': fnwait
		fnwait(message$,stopable)
	fnend
	! old and unused XXX 7/2/21   def library fnadd1099(mat cinfo$, mat einfo$, mat box) r:
	! old and unused XXX 7/2/21   	library 'S:\Core\Print1099.br': fnadd1099
	! old and unused XXX 7/2/21   	fnadd1099(mat cinfo$, mat einfo$, mat box)
	! old and unused XXX 7/2/21   fnend
	! old and unused XXX 7/2/21   def library fnprint1099(; lz1$)
	! old and unused XXX 7/2/21   	library 'S:\Core\Print1099.br': fnprint1099
	! old and unused XXX 7/2/21   	fnprint1099(lz1$)
	! old and unused XXX 7/2/21   fnend ! /r
	def library fnCheckFileVersion
		library 'S:\Core\Check File Versions.br': fnCheckFileVersion
		fnCheckFileVersion=fnCheckFileVersion
	fnend
	def library fnconsole(; on_off)
		library 'S:\Core\Ace\Console.br': fnconsole
		fnconsole(on_off)
	fnend
	def library fncmbcno(myline,mypos; mysys$)
		library 'S:\Core\CmbCNo.br': fncmbcno
		fncmbcno(myline,mypos,mysys$)
	fnend
	def library fnprocess(; chgpro)
		library 'S:\Core\process.br': fnprocess
		fnprocess=fnprocess(chgpro)
	fnend
	def library fnkillauto
		library 'S:\Core\process.br': fnkillauto
		fnkillauto=fnkillauto
	fnend
	def library fnoldmsgbox(mat response$,&cap$,mat msgline$,mtype)
		library 'S:\Core\OldMsgBox.br': fnoldmsgbox
		fnoldmsgbox=fnoldmsgbox(mat response$,cap$,mat msgline$,mtype)
	fnend
	def library fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
		library 'S:\Core\Menu.br': fnGetProgramList
		fnGetProgramList=fnGetProgramList(mat program_plus$,mat program_name$,mat program_name_trim$,mat program_file$,mat ss_text$)
	fnend
! /r

! r: parse   S:\Core\parse\
	def library fnremove2(&and$,&word$)
		library 'S:\Core\parse\remove2.br': fnremove2
		fnremove2=fnremove2(and$,word$)
	fnend
	def library fncsz(&csz$,&city$,&state$,&zip$)
		library 'S:\Core\Parse\csz.br': fncsz
		fncsz=fncsz(csz$,city$,state$,zip$)
	fnend
	def library fnGetPp(&input$,&path$,&prog$,&ext$)
		library 'S:\Core\Parse\GetPP.br': fnGetPp
		fnGetPp=fnGetPp(input$,path$,prog$,ext$)
	fnend
! /r
! r: label   S:\Core\label\
	def library fnLabel(mat lineStyle$)
		library 'S:\Core\Label.br': fnLabel
		fnlabel=fnLabel(mat lineStyle$)
	fnend
	def library fnAddLabel(mat in_labelText$)
		library 'S:\Core\Label.br': fnAddLabel
		fnAddLabel=fnAddLabel(mat in_labelText$)
	fnend
! /r
! r: PrintAce   S:\Core\printAce
	def library fnPrintAceTest(; format$)
		library 'S:\Core\Programs\PrintAce_Test': fnPrintAceTest
		fnPrintAceTest=fnPrintAceTest( format$)
	fnend
	def library fnpa_background(background_pdf$*256)
		library 'S:\Core\PrintAce.br': fnpa_background
		fnpa_background=fnpa_background(background_pdf$)
	fnend
	def library fnpa_line(pl_left_pos,pl_top_pos,pl_width; pl_height,pl_line_instead_of_box,h_printace)
		library 'S:\Core\PrintAce.br': fnpa_line
		fnpa_line=fnpa_line(pl_left_pos,pl_top_pos,pl_width, pl_height,pl_line_instead_of_box,h_printace)
	fnend
	def library fnpa_elipse(pe_a,pe_b,pe_c,pe_d; h_printace)
		library 'S:\Core\PrintAce.br': fnpa_elipse
		fnpa_elipse=fnpa_elipse(pe_a,pe_b,pe_c,pe_d, h_printace)
	fnend
	def library fnpa_pic(pp_pic$*1024,pp_x,pp_y; imgWidth,imgHeight,style$)
		library 'S:\Core\PrintAce.br': fnpa_pic
		fnpa_pic=fnpa_pic(pp_pic$,pp_x,pp_y, imgWidth,imgHeight,style$)
	fnend
	def library fnpa_txt(pt_text$*128,pt_x; pt_y,pt_h)
		library 'S:\Core\PrintAce.br': fnpa_txt
		fnpa_txt=fnpa_txt(pt_text$,pt_x,pt_y, pt_h)
	fnend
	def library fnpa_barcode(pb_a,pb_b,pb_bc$*256; h_printace)
		library 'S:\Core\PrintAce.br': fnpa_barcode
		fnpa_barcode=fnpa_barcode(pb_a,pb_b,pb_bc$, h_printace)
	fnend
	def library fnpa_finis(; pt_h)
		library 'S:\Core\PrintAce.br': fnpa_finis
		fnpa_finis=fnpa_finis(pt_h)
	fnend  ! fn_pa_finis
	def library fnpa_open(; pa_orientation$,pa_sendto_base_name_addition$*128,formsFormatForce$,h)
		library 'S:\Core\PrintAce.br': fnpa_open
		fnpa_open=fnpa_open( pa_orientation$,pa_sendto_base_name_addition$,formsFormatForce$,h)
	fnend
	def library fnpa_fontsize(; pfs_fontsize,h_printace)
		library 'S:\Core\PrintAce.br': fnpa_fontsize
		fnpa_fontsize=fnpa_fontsize( pfs_fontsize,h_printace)
	fnend
	def library fnpa_font(; pf_fontname$*256,h_printace)
		library 'S:\Core\PrintAce.br': fnpa_font
		fnpa_font=fnpa_font( pf_fontname$,h_printace)
	fnend
	def library fnpa_fontbold(; pfb_off_or_on)
		library 'S:\Core\PrintAce.br': fnpa_fontbold
		fnpa_fontbold=fnpa_fontbold( pfb_off_or_on)
	fnend
	def library fnpa_fontitalic(; pfb_off_or_on)
		library 'S:\Core\PrintAce.br': fnpa_fontitalic
		fnpa_fontitalic=fnpa_fontitalic( pfb_off_or_on)
	fnend
	def library fnpa_newpage(;h_printace)
		library 'S:\Core\PrintAce.br': fnpa_newpage
		fnpa_newpage=fnpa_newpage( h_printace)
	fnend
	def library fnbarcode(barcode$,rightleft,updown)
		library 'S:\Core\PrintAce.br': fnbarcode
		fnbarcode=fnbarcode(barcode$,rightleft,updown)
	fnend
	def library fnbarcodewide(barcode$,rightleft,updown)
		library 'S:\Core\PrintAce.br': fnbarcodewide
		fnbarcodewide=fnbarcodewide(barcode$,rightleft,updown)
	fnend
	def library fnpa_filename$*256
		library 'S:\Core\PrintAce.br': fnpa_filename$
		fnpa_filename$=fnpa_filename$
	fnend
! /r
! r: pr   S:\Core\Print.br and S:\Core\Print\*
	def library fnSafeFilename$*256(sf_in$*256)
		library 'S:\Core\Print.br': fnSafeFilename$
		fnSafeFilename$=fnSafeFilename$(sf_in$)
	fnend
	def library fnPrintFileName$*1024(; pfn_sendto_base_name_addition$*128,pfn_extension$,programCaptionOverride$*256)
		library 'S:\Core\Print.br': fnPrintFileName$
		fnPrintFileName$=fnPrintFileName$( pfn_sendto_base_name_addition$,pfn_extension$,programCaptionOverride$)
	fnend
	def library fnReportCacheFolderCurrent$*512
		library 'S:\Core\Print.br': fnReportCacheFolderCurrent$
		fnReportCacheFolderCurrent$=fnReportCacheFolderCurrent$
	fnend
	def library fnopenprn(; sendto_base_name_addition$*128,programNameOverride$*256,programCaptionOverride$*256)
		library 'S:\Core\Print.br': fnopenprn
		fnopenprn=fnopenprn( sendto_base_name_addition$,programNameOverride$,programCaptionOverride$)
	fnend
	def library fncloseprn(;forceWordProcessor$)
		library 'S:\Core\Print.br': fncloseprn
		fncloseprn=fncloseprn(forceWordProcessor$)
	fnend
	def library fnpglen(&pglen)
		library 'S:\Core\program_properties.br': fnpglen
		fnpglen=fnpglen(pglen)
	fnend
	def library fnopen_receipt_printer(; orp_only_if_it_is_assigned)
		library 'S:\Core\Print.br': fnopen_receipt_printer
		fnopen_receipt_printer=fnopen_receipt_printer( orp_only_if_it_is_assigned)
	fnend
	def library fnclose_receipt_printer
		library 'S:\Core\Print.br': fnclose_receipt_printer
		fnclose_receipt_printer=fnclose_receipt_printer
	fnend
	def library fnopen_cash_drawer
		library 'S:\Core\Print.br': fnopen_cash_drawer
		fnopen_cash_drawer=fnopen_cash_drawer
	fnend
! /r
! r: hamster
	def library fnHamsterFio(fileid$*64)
		library 'S:\Core\HamsterFio.br': fnHamsterFio
		fnHamsterFio=fnHamsterFio(fileid$)
	fnend
	def library fnHamster(uniqueWord$*20,mat lbl$,mat fln,hIn,mat p$; mat flTyp$,mat sln,mat mask,mat startPos,mat incontrol$,mat mxl)
		library 'S:\Core\Hamster.br': fnHamster
		fnHamster=fnHamster(uniqueWord$,mat lbl$,mat fln,hIn,mat p$, mat flTyp$,mat sln,mat mask,mat startPos,mat incontrol$,mat mxl)
	fnend
	def library fnH2AddComboF(hac_screen_item,hac_data_file$*256,hac_key_pos,hac_key_len,hac_desc_pos,hac_desc_len,hac_index_file$*256,hac_limit_to_list)
		library 'S:\Core\Hamster_Setup.br': fnH2AddComboF
		fnH2AddComboF=fnH2AddComboF(hac_screen_item,hac_data_file$,hac_key_pos,hac_key_len,hac_desc_pos,hac_desc_len,hac_index_file$,hac_limit_to_list)
	fnend
	def library fnH2AddComboA(hac_screen_item,mat hac_option$)
		library 'S:\Core\Hamster_Setup.br': fnH2AddComboA
		fnH2AddComboA=fnH2AddComboA(hac_screen_item,mat hac_option$)
	fnend
	def library fnHamster2(a$*128; h_file)
		library 'S:\Core\Hamster_Setup.br': fnHamster2
		fnHamster2=fnHamster2(a$, h_file)
	fnend
	def library fnHamster2b(uniqueWord$*128,mat lbl$,mat fln,hIn,mat p$; mat flTyp$,mat sln,mat mask$,mat startPos,mat incontrol$,mat mxl)
		library 'S:\Core\Hamster.br': fnHamster2b
		fnHamster2b=fnHamster2b(uniqueWord$,mat lbl$,mat fln,hIn,mat p$, mat flTyp$,mat sln,mat mask$,mat startPos,mat incontrol$,mat mxl)
	fnend
	def library fnhamster_print(a$*128,mat b$,mat l,c,mat e$; mat f$,mat d,mat g,mat h,mat j$,mat k)
		library 'S:\Core\Hamster_print.br': fnhamster_print
		fnhamster_print=fnhamster_print(a$,mat b$,mat l,c,mat e$,mat f$,mat d,mat g,mat h,mat j$,mat k)
	fnend
	def library fnH2Init
		library 'S:\Core\Hamster_Setup.br': fnH2Init
		fnH2Init=fnH2Init
	fnend
	def library fnH2AddText(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
		library 'S:\Core\Hamster_Setup.br': fnH2AddText
		fnH2AddText=fnH2AddText(label$,textbox_len, field_type$,storage_length,ar_mask,storage_position)
	fnend
	def library fnHamster2AddCombo(mat c$)
		library 'S:\Core\Hamster_Setup.br': fnHamster2AddCombo
		fnHamster2AddCombo=fnHamster2AddCombo(mat c$)
	fnend
! /r
! r: FileIO + Screen Ace
	def library fnComboFio(lyne,ps,layoutName$*128; limitToListorAddAll,whichIndex)
		library 'S:\Core\ACS_Component.br': fnComboFio
		fnComboFio=fnComboFio(lyne,ps,layoutName$, limitToListorAddAll,whichIndex)
	fnend
! /r
! r: Screen Ace
	def library fnTop(; prg$*256,cap$*128,force80x24)
		library 'S:\Core\fn\top.br': fnTop
		fnTop=fnTop( prg$,cap$,force80x24)
	fnend
	def library fnCompanyName(window,win_cols)
		library 'S:\Core\ACS_Component.br': fnCompanyName
		fnCompanyName=fnCompanyName(window,win_cols)
	fnend
	def library fnCmdKey(caption$*200,returnkey; default,cancel,tt$*200)
		library 'S:\Core\ACS_Component.br': fnCmdKey
		fnCmdKey=fnCmdKey(caption$,returnkey, default,cancel,tt$)
	fnend
	def library fnflexAdd1(mat item$)
		library 'S:\Core\ACS_Component.br': fnflexadd1
		fnflexadd1=fnflexadd1(mat item$)
	fnend
	def library fnTos(; sn$*100)
		library 'S:\Core\ACS_Component.br': fnTos
		fnTos=fnTos( sn$)
	fnend
	def library fnLbl(myline,mypos,t$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
		library 'S:\Core\ACS_Component.br': fnLbl
		fnLbl=fnLbl(myline,mypos,t$,mylen,myalign,font_mod,container,tabcon,lbl_tooltip$)
	fnend
	def library fnTxt(lyne,ps,width;maxlen,ali,mask$,disable,tooltip$*300,contain,tabcon,addtomask$*40)
		library 'S:\Core\ACS_Component.br': fnTxt
		fnTxt=fnTxt(lyne,ps,width, maxlen,ali,mask$,disable,tooltip$,contain,tabcon,addtomask$)
	fnend
	def library fnOpt(lyne,ps,txt$*196; align,contain,tabcon)
		library 'S:\Core\ACS_Component.br': fnOpt
		fnOpt=fnOpt(lyne,ps,txt$, align,contain,tabcon)
	fnend
	def library fnChk(lyne,ps,txt$*196; align,contain,tabcon,chk_disable)
		library 'S:\Core\ACS_Component.br': fnChk
		fnChk=fnChk(lyne,ps,txt$, align,contain,tabcon,chk_disable)
	fnend
	def library fnflexInit1(sfn$*256,lyne,ps,height,width,mat ch$;mat cm$,seltype,usr,container,tabcon)
		library 'S:\Core\ACS_Component.br': fnflexinit1
		fnflexinit1=fnflexinit1(sfn$,lyne,ps,height,width,mat ch$,mat cm$,seltype,usr,container,tabcon)
	fnend
	def library fnComboA(sfn$*256,lyne,ps,mat opt$; ttt$*200,width,contain,tabcon)
		library 'S:\Core\ACS_Component.br': fnComboA
		fnComboA=fnComboA(sfn$,lyne,ps,mat opt$, ttt$,width,contain,tabcon)
	fnend
	def library fnComboF(sfn$*100,lyne,ps,width,df$*200,psk,lnk,psd,lnd; if$*200,limlis,unused_userOrReplace,ttt$*200,contain,tabcon,keyFormat$)
		library 'S:\Core\ACS_Component.br': fnComboF
		fnComboF=fnComboF(sfn$,lyne,ps,width,df$,psk,lnk,psd,lnd, if$,limlis,unused_userOrReplace,ttt$,contain,tabcon,keyFormat$)
	fnend
	def library fnButton(lyne,ps,txt$*200,comkey; tt$*200,height,width,container,tabcon,default,cancel)
		library 'S:\Core\ACS_Component.br': fnButton
		fnButton=fnButton(lyne,ps,txt$,comkey, tt$,height,width,container,tabcon,default,cancel)
	fnend
	def library fnButtonOrDisabled(enable,lyne,ps,text$*200,comkey; toolTip$*200,width,container,tabcon,default,cancel)
		library 'S:\Core\fn\buttonOrDisabled.br': fnButtonOrDisabled
		fnButtonOrDisabled=fnButtonOrDisabled(enable,lyne,ps,text$,comkey, toolTip$,width,container,tabcon,default,cancel)
	fnend
	def library fnPicBut(lyne,mypos,txt$*40,comkey,pic1$*150,btnh,btnw; pic2$*150,tt$*150,container,tabcon,default,cancel)
		library 'S:\Core\ACS_Component.br': fnpicbut
		fnpicbut=fnpicbut(lyne,mypos,txt$,comkey,pic1$,btnh,btnw, pic2$,tt$,container,tabcon,default,cancel)
	fnend
	def library fnDisplayMenu(mat _menu$,mat _program$,mat _status$)
		library 'S:\Core\ACS_Component.br': fnDisplayMenu
		fnDisplayMenu=fnDisplayMenu(mat _menu$,mat _program$,mat _status$)
	fnend
	def library fnClearMenu
		library 'S:\Core\ACS_Component.br': fnClearMenu
		fnClearMenu=fnClearMenu
	fnend
	def library fnAcs(mat resp$; &ckey,startfield,close_on_exit,parent_none,disabled_background)
		library 'S:\Core\ACS_Component.br': fnAcs
		fnAcs=fnAcs(mat resp$, ckey,startfield,close_on_exit,parent_none,disabled_background) : fnend  ! fnend should be on the same line as fn call so that f12 program pause will work properly
	def library fnpic(lyne,ps,hi,wd,picture$*300; x,y)
		library 'S:\Core\ACS_Component.br': fnpic
		fnpic=fnpic(lyne,ps,hi,wd,picture$, x,y)
	fnend
	def library fnFra(lyne,ps,hi,wd; cap$*128,tooltip$*300,contain,tabcon)
		library 'S:\Core\ACS_Component.br': fnFra
		fnFra=fnFra(lyne,ps,hi,wd, cap$,tooltip$,contain,tabcon)
	fnend
	def library fntab(lyne,mypos,height,width,mat cap$)
		library 'S:\Core\ACS_Component.br': fntab
		fntab=fntab(lyne,mypos,height,width,mat cap$)
	fnend
	def library fnmultiline(lyne,ps,height,width;contain,tabcon,tt$*200)
		library 'S:\Core\ACS_Component.br': fnmultiline
		fnmultiline=fnmultiline(lyne,ps,height,width, contain,tabcon,tt$)
	fnend
	def library fnCmdSet(a)
		library 'S:\Core\ACS_Component.br': fnCmdSet
		fnCmdSet=fnCmdSet(a)
	fnend
	

	def library fnMsgBox(mat message$; &response$,cap$*128,mtype)
		library 'S:\Core\fnMsgBox.br': fnmsgbox
		fnMsgBox=fnMsgBox(mat message$, response$,cap$,mtype)
	fnend
	def library fnMb(mat mb$; mtype,___,returnN,resp$) ! an alias for fnMsgBox with optimized parameters
		library 'S:\Core\fnMsgBox.br': fnMsgBox
		fnMsgBox(mat mb$, resp$,'',mtype)
		if resp$='OK'      	then returnN=1 ! mb_ok     = 1
		if resp$='Cancel'  	then returnN=2 ! mb_cancel = 2
		if resp$='Abort'   	then returnN=3 ! mb_abort  = 3
		if resp$='Retry'   	then returnN=4 ! mb_retry  = 4
		if resp$='Ignore'  	then returnN=5 ! mb_ignore = 5
		if resp$='Yes'     	then returnN=6 ! mb_yes    = 6
		if resp$='No'      	then returnN=7 ! mb_no     = 7
		fnMb=returnN
	fnend
	def library fnBackgroundDisable(; Activate)
		library 'S:\Core\ACS_Component.br': fnBackgroundDisable
		fnBackgroundDisable=fnBackgroundDisable( Activate)
	fnend
! /r
! r: registry stuff
	def library fnsreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128)
		library 'S:\Core\Reg.br': fnsreg_read
		fnsreg_read=fnsreg_read(reg_field_name$,reg_field_value$, reg_field_default$)
	fnend
	def library fnsreg_write(reg_field_name$*128,reg_field_value$*256)
		library 'S:\Core\Reg.br': fnsreg_write
		fnsreg_write=fnsreg_write(reg_field_name$,reg_field_value$)
	fnend
	def library fncreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128,cr_alsoApplyDefaultIfReadBlank)
		library 'S:\Core\Reg.br': fncreg_read
		fncreg_read=fncreg_read(reg_field_name$,reg_field_value$, reg_field_default$,cr_alsoApplyDefaultIfReadBlank)
	fnend
	def library fncreg_write(reg_field_name$*128,reg_field_value$*256)
		library 'S:\Core\Reg.br': fncreg_write
		fncreg_write=fncreg_write(reg_field_name$,reg_field_value$)
	fnend
	def library fnPcReg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128,cr_alsoApplyDefaultIfReadBlank)
		library 'S:\Core\Reg.br': fnPcReg_read
		fnPcReg_read=fnPcReg_read(reg_field_name$,reg_field_value$, reg_field_default$,cr_alsoApplyDefaultIfReadBlank)
	fnend
	def library fnPcReg_write(reg_field_name$*128,reg_field_value$*256)
		library 'S:\Core\Reg.br': fnPcReg_write
		fnPcReg_write=fnPcReg_write(reg_field_name$,reg_field_value$)
	fnend
	def library fnreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128,alsoUseDefaultIfReadBlank)
		library 'S:\Core\Reg.br': fnreg_read
		fnreg_read=fnreg_read(reg_field_name$,reg_field_value$, reg_field_default$,alsoUseDefaultIfReadBlank)
	fnend
	def library fnreg_write(reg_field_name$*128,reg_field_value$*256)
		library 'S:\Core\Reg.br': fnreg_write
		fnreg_write=fnreg_write(reg_field_name$,reg_field_value$)
	fnend
	def library fnureg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128,alsoUseDefaultIfReadBlank)
		library 'S:\Core\Reg.br': fnureg_read
		fnureg_read=fnureg_read(reg_field_name$,reg_field_value$, reg_field_default$,alsoUseDefaultIfReadBlank)
	fnend
	def library fnureg_write(reg_field_name$*128,reg_field_value$*256)
		library 'S:\Core\Reg.br': fnureg_write
		fnureg_write=fnureg_write(reg_field_name$,reg_field_value$)
	fnend
	def library fnreg_close
		library 'S:\Core\Reg.br': fnreg_close
		fnreg_close=fnreg_close
	fnend
	def library fnsreg_rename(field_name_old$*128,fieldNameNew$*128)
		library 'S:\Core\Reg.br': fnsreg_rename
		fnsreg_rename=fnsreg_rename(field_name_old$,fieldNameNew$)
	fnend
	def library fnreg_rename(field_name_old$*128,fieldNameNew$*128)
		library 'S:\Core\Reg.br': fnreg_rename
		fnreg_rename=fnreg_rename(field_name_old$,fieldNameNew$)
	fnend
	! def library fnIniToReg
	! 	library 'S:\Core\Reg.br': fnIniToReg
	! 	fnIniToReg=fnIniToReg
	! fnend
	def library fnReadProgramPrintProperty(key$*80,&value$; programFileOverride$*256)
		library 'S:\Core\program_properties.br': fnReadProgramPrintProperty
		fnReadProgramPrintProperty=fnReadProgramPrintProperty(key$,value$, programFileOverride$)
	fnend
	def library fnwriteProgramPrintProperty(key$*80,value$*256; programFileOverride$*256)
		library 'S:\Core\program_properties.br': fnwriteProgramPrintProperty
		fnwriteProgramPrintProperty=fnwriteProgramPrintProperty(key$,value$, programFileOverride$)
	fnend
	def library fnmcreg_read(reg_field_name$*128,&reg_field_value$; reg_field_default$*128)
		library 'S:\Core\Reg.br': fnmcreg_read
		fnmcreg_read=fnmcreg_read(reg_field_name$,reg_field_value$, reg_field_default$)
	fnend
	def library fnmcreg_write(reg_field_name$*128,reg_field_value$*256)
		library 'S:\Core\Reg.br': fnmcreg_write
		fnmcreg_write=fnmcreg_write(reg_field_name$,reg_field_value$)
	fnend
! /r
! r: File stuff - whole file processes

def library fnRemoveExcessCRLF$*256(csvFile$*256; minColCount)
	library 'S:\Core\fn\removeExcessCRLF.br': fnRemoveExcessCRLF$
	fnRemoveExcessCRLF$=fnRemoveExcessCRLF$(csvFile$, minColCount)
fnend

! /r
! r: Array stuff
	def library fnArrayItemRemoveC(mat array$,itemToRemove)
		library 'S:\Core\Array.br': fnArrayItemRemoveC
		fnArrayItemRemoveC=fnArrayItemRemoveC(mat array$,itemToRemove)
	fnend
	def library fnArrayItemRemoveN(mat arrayN,itemToRemove)
		library 'S:\Core\Array.br': fnArrayItemRemoveN
		fnArrayItemRemoveN=fnArrayItemRemoveN(mat arrayN,itemToRemove)
	fnend
	def library fnSetForCombo$*256(mat option$,key$; kpos,klen)
		library 'S:\Core\Array.br': fnSetForCombo$
		fnSetForCombo$=fnSetForCombo$(mat option$,key$, kpos,klen)
	fnend
	def library fnPosOfAny(textToSearch$*1024,mat searchFor$; fromEnd)
		library 'S:\Core\Array.br': fnPosOfAny
		fnPosOfAny=fnPosOfAny(textToSearch$,mat searchFor$, fromEnd)
	fnend
	def library fnChrCount(String_To_Search$*10480,Chr_To_Count$*1)
		library 'S:\Core\Array.br': fnChrCount
		fnChrCount=fnChrCount(String_To_Search$,Chr_To_Count$)
	fnend
	def library fnArrayReverseC(mat in$,mat out$)
		library 'S:\Core\Array.br': fnArrayReverseC
		fnArrayReverseC=fnArrayReverseC(mat in$,mat out$)
	fnend
	def library fn2arraySortNc(mat arrayOneN,mat arrayTwo$) ! untested unimplemented
		library 'S:\Core\Array.br': fn2arraySortNc
		fn2arraySortNc=fn2arraySortNc(mat arrayOneN,mat arrayTwo$)
	fnend
	def library fnArraySortC(mat array$)
		library 'S:\Core\Array.br': fnArraySortC
		fnArraySortC=fnArraySortC(mat array$)
	fnend
	def library fnArrayAddC(mat array_combined$,mat arrayPartOne$,mat arrayPartTwo$)
		library 'S:\Core\Array.br': fnArrayAddC
		fnArrayAddC=fnArrayAddC(mat array_combined$,mat arrayPartOne$,mat arrayPartTwo$)
	fnend
	def library fnArrayAddN(mat array_combinedN,mat arrayPartOneN,mat arrayPartTwoN)
		library 'S:\Core\Array.br': fnArrayAddN
		fnArrayAddN=fnArrayAddN(mat array_combinedN,mat arrayPartOneN,mat arrayPartTwoN)
	fnend
	def library fnSrepExcludeStringLiterals$*1024(in$*1024,srepFrom$,srepTo$)
		library 'S:\Core\Srep.br': fnSrepExcludeStringLiterals$
		fnSrepExcludeStringLiterals$=fnSrepExcludeStringLiterals$(in$,srepFrom$,srepTo$)
	fnend
	def library fnArrayEmpty(mat array$)
		library 'S:\Core\Array.br': fnArrayEmpty
		fnArrayEmpty=fnArrayEmpty(mat array$)
	fnend
	def library fnArrayWasPassedC(mat array$)
		library 'S:\Core\Array.br': fnArrayWasPassedC
		fnArrayWasPassedC=fnArrayWasPassedC(mat array$)
	fnend
	def library fnArrayWasPassedN(mat arrayN)
		library 'S:\Core\Array.br': fnArrayWasPassedN
		fnArrayWasPassedN=fnArrayWasPassedN(mat arrayN)
	fnend
	def library fnarray_item_insert$(mat array$, insert_item$*1024, insert_item_number)
		library 'S:\Core\Array.br': fnarray_item_insert$
		fnarray_item_insert$=fnarray_item_insert$(mat array$, insert_item$, insert_item_number)
	fnend
	def library fnarray_item_insert(mat array, insert_item, insert_item_number)
		library 'S:\Core\Array.br': fnarray_item_insert
		fnarray_item_insert=fnarray_item_insert(mat array, insert_item, insert_item_number)
	fnend
	def library fnsrch_case_insensitive(mat srch_array$,srch_for$*256; srch_start_ele)
		library 'S:\Core\Array.br': fnsrch_case_insensitive
		fnsrch_case_insensitive=fnsrch_case_insensitive(mat srch_array$,srch_for$, srch_start_ele)
	fnend
	def library fnCountMatchesC(mat arrayToSearch$,valueToMatch$*256)
		library 'S:\Core\Array.br': fnCountMatchesC
		fnCountMatchesC=fnCountMatchesC(mat arrayToSearch$,valueToMatch$)
	fnend
	def library fnCountMatchesN(mat arrayToSearch,valueToMatch)
		library 'S:\Core\Array.br': fnCountMatchesN
		fnCountMatchesN=fnCountMatchesN(mat arrayToSearch,valueToMatch)
	fnend
	def library fnArrayMax(mat arrayToSearch)
		library 'S:\Core\Array.br': fnArrayMax
		fnArrayMax=fnArrayMax(mat arrayToSearch)
	fnend
	def library fnFileTo2Arrays(ftaFile$*512,mat ftaArrayLeft$,mat ftaArrayRight$; ftaSkipFirstLine,ftaDelimiter$*1)
		library 'S:\Core\Array.br': fnFileTo2Arrays
		fnFileTo2Arrays=fnFileTo2Arrays(ftaFile$,mat ftaArrayLeft$,mat ftaArrayRight$, ftaSkipFirstLine,ftaDelimiter$)
	fnend
	def library fnRead1column(mat r1Return$,r1File$*256,r1ColumnNumber,r1Delimiter$)
		library 'S:\Core\Array.br': fnRead1column
		fnRead1column=fnRead1column(mat r1Return$,r1File$,r1ColumnNumber,r1Delimiter$)
	fnend
	def library fnRead2column(mat r2Return1$,mat r2Return2$,r2File$*256,r2ColumnNumber1,r2ColumnNumber2,r2Delimiter$)
		library 'S:\Core\Array.br': fnRead2column
		fnRead2column=fnRead2column(mat r2Return1$,mat r2Return2$,r2File$,r2ColumnNumber1,r2ColumnNumber2,r2Delimiter$)
	fnend
	def library fnRead2columnFixedWidth(mat r2fReturn1$,mat r2fReturn2$,r2fFile$*256,r2fColumn1Width)
		library 'S:\Core\Array.br': fnRead2columnFixedWidth
		fnRead2columnFixedWidth=fnRead2columnFixedWidth(mat r2fReturn1$,mat r2fReturn2$,r2fFile$,r2fColumn1Width)
	fnend
	def library fnRead3column(mat r3Return1$,mat r3Return2$,mat r3Return3$,r3File$*256,r3ColumnNumber1,r3ColumnNumber2,r3ColumnNumber3,r3Delimiter$)
		library 'S:\Core\Array.br': fnRead3column
		fnRead3column=fnRead3column(mat r3Return1$,mat r3Return2$,mat r3Return3$,r3File$,r3ColumnNumber1,r3ColumnNumber2,r3ColumnNumber3,r3Delimiter$)
	fnend
	def library fnRead4column(mat r4Return1$,mat r4Return2$,mat r4Return3$,mat r4Return4$,r4File$*256,r4ColumnNumber1,r4ColumnNumber2,r4ColumnNumber3,r4ColumnNumber4,r4Delimiter$)
		library 'S:\Core\Array.br': fnRead4column
		fnRead4column=fnRead4column(mat r4Return1$,mat r4Return2$,mat r4Return3$,mat r4Return4$,r4File$,r4ColumnNumber1,r4ColumnNumber2,r4ColumnNumber3,r4ColumnNumber4,r4Delimiter$)
	fnend
! /r
! r: Date stuff
def library fnDateSelect$ (;_date$,format$,row,column)
	library 'S:\Core\Date.br': fnDateSelect$
	fnDateSelect$=fnDateSelect$ ( _date$,format$,row,column)
fnend
def library fnFirstMondayOfMonth(; day)
	library 'S:\Core\Date.br': fnFirstMondayOfMonth
	fnFirstMondayOfMonth=fnFirstMondayOfMonth( day)
fnend
def library fnEndOfMonth(day)
	library 'S:\Core\Date.br': fnEndOfMonth
	fnEndOfMonth=fnEndOfMonth(day)
fnend
def library fndate_mmddyy_to_ccyymmdd(x_mmddyy)
	library 'S:\Core\Date.br': fndate_mmddyy_to_ccyymmdd
	fndate_mmddyy_to_ccyymmdd=fndate_mmddyy_to_ccyymmdd(x_mmddyy)
fnend
	def library fnSetMonth(mat mo$)
		library 'S:\Core\Date.br': fnSetMonth
		fnSetMonth=fnSetMonth(mat mo$)
	fnend
! /r
! r: ini functions and quick calls
	def library fnIniOpen(ii_file$*256) ! unused as of 12/1/2021 but previously fully functional
		library 'S:\Core\ini.br': fnIniOpen
		fnIniOpen=fnIniOpen(ii_file$)
	fnend
	def library fnIniRead$*256(il_section$*256,il_field$*256)
		library 'S:\Core\ini.br': fnIniRead$
		fnIniRead$=fnIniRead$(il_section$,il_field$)
	fnend
	def library fnIniSet(inis_section$*256,inis_field$*256,inis_value$*256)
		library 'S:\Core\ini.br': fnIniSet
		fnIniSet=fnIniSet(inis_section$,inis_field$,inis_value$)
	fnend
	def library fnIniWrite
		library 'S:\Core\ini.br': fnIniWrite
		fnIniWrite=fnIniWrite
	fnend
! /r
	def library fnshortpath$*256(longpath$*256)
		library 'S:\Core\Ace\fnShortPath.br': fnshortpath$
		fnshortpath$=fnshortpath$(longpath$)
	fnend
! r: S:\Core\CNo.br - SYSTEM
	def library fnStandardizeSysId$(return$*256)
		library 'S:\Core\CNo.br': fnStandardizeSysId$
		fnStandardizeSysId$=fn_standardizeSysId$(return$)
	fnend
	fnend
	def library fnSystemIsAddOn( sia_systemAbbr$*256)
		library 'S:\Core\CNo.br': fnSystemIsAddOn
		fnSystemIsAddOn=fnSystemIsAddOn( sia_systemAbbr$)
	fnend
	def library fnUseDeptNo
		library 'S:\Core\CNo.br': fnUseDeptNo
		fnUseDeptNo=fnUseDeptNo
	fnend
	def library fnSystemName$*256(; sysId$*256)
		library 'S:\Core\CNo.br': fnSystemName$
		fnSystemName$=fnSystemName$(sysId$)
	fnend
! r: cno   S:\Core\CNo.br - COMPANY NUMBER (and misc)
	def library fncno(&cno;&cnam$)
		library 'S:\Core\CNo.br': fncno
		fncno=fncno(cno,cnam$)
	fnend
	def library fnget_company_number_list(mat cno_list; sysid$*256)
		library 'S:\Core\CNo.br': fnget_company_number_list
		fnget_company_number_list=fnget_company_number_list(mat cno_list, sysid$)
	fnend
	def library fnpgnum(;pgnum)
		library 'S:\Core\CNo.br': fnpgnum
		fnpgnum=fnpgnum(pgnum)
	fnend
	def library fnrx(;rx)
		library 'S:\Core\CNo.br': fnrx
		fnrx=fnrx(rx)
	fnend
	def library fnstyp(;styp)
		library 'S:\Core\CNo.br': fnstyp
		fnstyp=fnstyp(styp)
	fnend
	def library fnps(;ps)
		library 'S:\Core\CNo.br': fnps
		fnps=fnps(ps)
	fnend
	def library fnfscode(;a)
		library 'S:\Core\CNo.br': fnfscode
		fnfscode=fnfscode(a)
	fnend
	def library fnpedat$*20(;a$*20)
		library 'S:\Core\CNo.br': fnpedat$
		fnpedat$=fnpedat$(a$)
	fnend
	def library fnpriorcd(;a)
		library 'S:\Core\CNo.br': fnpriorcd
		fnpriorcd=fnpriorcd(a)
	fnend
	def library fnputcno(cno)
		library 'S:\Core\CNo.br': fnputcno
		fnputcno=fnputcno(cno)
	fnend
	def library fndat(&dat$; get_or_put)
		library 'S:\Core\CNo.br': fndat
		fndat=fndat(dat$,get_or_put)
	fnend
	def library fncursys$(; cursys_set$*256,resetCache)
		library 'S:\Core\CNo.br': fncursys$
		fncursys$=fncursys$( cursys_set$,resetCache)
	fnend
	def library fnprg(&prg$; g_p)
		library 'S:\Core\CNo.br': fnprg
		fnprg=fnprg(prg$,g_p)
	fnend
! /r
! r: UB   utility billing
	def library fnEnableCostOfGas(; setIt$)
		library 'S:\Utility Billing\Company.br': fnEnableCostOfGas
		fnEnableCostOfGas=fnEnableCostOfGas( setIt$)
	fnend
	def library fnCalculateBills(goal$*11)
		library 'S:\Utility Billing\fn\calculateBills.br': fnCalculateBills
		fnCalculateBills=fnCalculateBills(goal$)
	fnend
	def library fnCalk(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,deposit2,btu; calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
		library 'S:\Utility Billing\fn\calk.br': fncalk
		fncalk=fncalk(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,h_ratemst,deposit2,btu, calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
	fnend
	def library fnCalkChatom(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,ratemst,unused,btu; calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
		library 'S:\Utility Billing\fn\calkChatom.br': fncalkChatom
		fncalkChatom=fncalkChatom(x$,d1,f,usage_water,x2,x3,mc1,mu1,mat rt,mat a,mat b,mat c,mat d,mat g,mat w,mat x,mat extra,mat gb,ratemst,unused,btu, calc_interest_on_deposit,charge_inspection_fee,interest_credit_rate)
	fnend
	def library fnDepositChangeLog(z$*10,odp,ndp,chgDate,comment$*32)
		library 'S:\Utility Billing\Customer.br': fnDepositChangeLog
		fnDepositChangeLog=fnDepositChangeLog(z$,odp,ndp,chgDate,comment$)
	fnend
	! def library fnMeterAddressLocationID(meterAddress$*30; leaveFileOpen)
	! 	library 'S:\Utility Billing\Hand Held\Meter Location.br': fnMeterAddressLocationID
	! 	fnMeterAddressLocationID=fnMeterAddressLocationID(meterAddress$, leaveFileOpen)
	! fnend
	def library fnCustomerMeterLocationSelect(account$*10,serviceCode$*2)
		library 'S:\Utility Billing\Hand Held\Meter Location.br': fnCustomerMeterLocationSelect
		fnCustomerMeterLocationSelect=fnCustomerMeterLocationSelect(account$,serviceCode$)
	fnend
	def library fnMeterAddressName$*30(locationID; leaveFileOpen)
		library 'S:\Utility Billing\Hand Held\Meter Location.br': fnMeterAddressName$
		fnMeterAddressName$=fnMeterAddressName$(locationID, leaveFileOpen)
	fnend
	def library fnAccountFromLocationId$*10(locationID; leaveFileOpen)
		library 'S:\Utility Billing\Hand Held\Meter Location.br': fnAccountFromLocationId$
		fnAccountFromLocationId$=fnAccountFromLocationId$(locationID, leaveFileOpen)
	fnend
	def library fnLocationIdFromAccountAndServ$*30(account$*10,serviceId$*2; field$*14,leaveFileOpen)
		library 'S:\Utility Billing\Hand Held\Meter Location.br': fnLocationIdFromAccountAndServ$
		fnLocationIdFromAccountAndServ$=fnLocationIdFromAccountAndServ$(account$,serviceId$, field$,leaveFileOpen)
	fnend
	def library fnNoteDir$*256
		library 'S:\Utility Billing\Customer.br': fnNoteDir$
		fnNoteDir$=fnNoteDir$
	fnend
	def library fnWorkOrderAdd(z$*10)
		library 'S:\Utility Billing\Work Order Add.br': fnWorkOrderAdd
		fnWorkOrderAdd=fnWorkOrderAdd(z$)
	fnend
	def library fnInitialializeMeterLocation
		library 'S:\Utility Billing\Hand Held\Meter Location.br': fnInitialializeMeterLocation
		fnInitialializeMeterLocation=fnInitialializeMeterLocation
	fnend
	def library fnWorkOrderList(; z$*10)
		library 'S:\Utility Billing\Work Order List.br': fnWorkOrderList
		fnWorkOrderList=fnWorkOrderList(z$)
	fnend
	def library fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$; cell$)
		library 'S:\Utility Billing\Work Order Print.br': fnWorkOrderPrint
		fnWorkOrderPrint=fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$, cell$)
	fnend
	def library fnCustomerData$*128(account$*10,fieldName$*40; leaveOpen)
		library 'S:\Utility Billing\fn\customerData.br': fnCustomerData$
		fnCustomerData$=fnCustomerData$(account$,fieldName$, leaveOpen)
	fnend
	def library fncustomer_address(account$,mat addr$; ca_address_type,ca_closeFiles)
		library 'S:\Utility Billing\Labels.br': fncustomer_address
		fncustomer_address=fncustomer_address(account$,mat addr$, ca_address_type,ca_closeFiles)
	fnend
	def library fnCustomerNotes(z$)
		library 'S:\Utility Billing\Customer.br': fnCustomerNotes
		fnCustomerNotes=fnCustomerNotes(z$)
	fnend
	def library fnApply_default_rates(mat extra, mat a)
		library 'S:\Utility Billing\Customer.br': fnapply_default_rates
		fnapply_default_rates=fnapply_default_rates(mat extra, mat a)
	fnend
	def library fnApplyDefaultRatesFio(mat customerN)
		library 'S:\Utility Billing\Rates.br': fnapplyDefaultRatesFio
		fnapplyDefaultRatesFio=fnapplyDefaultRatesFio(mat customerN)
	fnend
	def library fnGetServices(mat serviceName$; mat serviceCode$, mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply)
		library 'S:\Utility Billing\Type of Service.br': fnGetServices
		fnGetServices=fnGetServices(mat serviceName$,mat serviceCode$,mat taxCode$,mat penalty$,mat subjectTo,mat orderToApply)
	fnend
	def library fnService_other
		library 'S:\Utility Billing\Type of Service.br': fnservice_other
		fnservice_other=fnservice_other
	fnend
	def library fnGetServiceCodesMetered(mat serviceCodeMetered$)
		library 'S:\Utility Billing\Type of Service.br': fnGetServiceCodesMetered
		fnGetServiceCodesMetered=fnGetServiceCodesMetered(mat serviceCodeMetered$)
	fnend
	def library fncmbact(lyne,mypos; addall,container,indexfile$*256)
		library 'S:\Utility Billing\fn\CmbAct.br': fncmbact
		fncmbact=fncmbact(lyne,mypos, addall,container,indexfile$)
	fnend
	def library fnLastBillingDate(; &d1,get_or_put)
		library 'S:\Utility Billing\fn\lastBillingDate.br': fnLastBillingDate
		fnLastBillingDate=fnLastBillingDate(d1,get_or_put)
	fnend
	def library fnCustomerSearch(&x$;fixgrid)
		library 'S:\Utility Billing\fn\customerSearch.br': fnCustomerSearch
		fnCustomerSearch=fnCustomerSearch(x$,fixgrid)
	fnend
	def library fnCustomer(; &editOne$)
		library 'S:\Utility Billing\Customer.br': fnCustomer
		fnCustomer=fnCustomer( editOne$)
	fnend
	def library fnask_account(prev_list_id$,&x$,hCustomer; select_button_text$,aa_button_enable_add)
		library 'S:\Utility Billing\Customer.br': fnask_account
		fnask_account=fnask_account(prev_list_id$,x$,hCustomer, select_button_text$,aa_button_enable_add)
	fnend
	def library fncmbrt2(lyne,mypos; all)
		library 'S:\Utility Billing\fn\cmbRt2.br': fncmbrt2
		fncmbrt2=fncmbrt2(lyne,mypos, all)
	fnend
	def library fntransfile(hact$*81,&bal,mat gb)
		library 'S:\Utility Billing\Transactions.br': fntransfile
		fntransfile=fntransfile(hact$,bal,mat gb)
	fnend
	def library fntrans_total_as_of(; customer_key$,date_ccyymmdd,trans_type)
		library 'S:\Utility Billing\Transactions.br': fntrans_total_as_of
		fntrans_total_as_of=fntrans_total_as_of( customer_key$,date_ccyymmdd,trans_type)
	fnend
	def library fnub_cnv_build_transactions
		library 'S:\acsUB\conversion\bld_trans.br': fnub_cnv_build_transactions
		fnub_cnv_build_transactions=fnub_cnv_build_transactions
	fnend
	def library fnub_cnv_ubmstr_vb
		library 'S:\acsUB\conversion\ubmstr-vb.br': fnub_cnv_ubmstr_vb
		fnub_cnv_ubmstr_vb=fnub_cnv_ubmstr_vb
	fnend
	def library fnub_cnv_note
		library 'S:\acsUB\conversion\note-cnv.br': fnub_cnv_note
		fnub_cnv_note=fnub_cnv_note
	fnend
	def library fnub_cnv_note_phase_1
		library 'S:\acsUB\conversion\note-cnv-c7.br': fnub_cnv_note_phase_1
		fnub_cnv_note_phase_1=fnub_cnv_note_phase_1
	fnend
	def library fnub_cnv_adrbil
		library 'S:\acsUB\conversion\ubadrbil-cnv.br': fnub_cnv_adrbil
		fnub_cnv_adrbil=fnub_cnv_adrbil
	fnend
	def library fntotal_ar
		library 'S:\Utility Billing\View Total Accounts Receivable.br': fntotal_ar
		fntotal_ar=fntotal_ar
	fnend
	def library fnfix_trans_breakdowns(do_fix,do_report)
		library 'S:\Utility Billing\Check Balance Breakdowns.br': fnfix_trans_breakdowns
		fnfix_trans_breakdowns=fnfix_trans_breakdowns(do_fix,do_report)
	fnend
	! r: UB-EFT
		def library fnEftData$*128(field$*128; return$*256)
			library 'S:\Utility Billing\fn\eftData.br': fnEftData$
			fnEftData$=fnEftData$(field$, return$)
		fnend
	! /r
	! r: Hand Held
		def library fnhand_held_device$*20
			library 'S:\Core\Client.br': fnhand_held_device$
			fnhand_held_device$=fnhand_held_device$
		fnend
		def library fnHandHeldList(mat device$; mat deviceOption$)
			library 'S:\Utility Billing\Hand Held\Create Hand Held File.br': fnHandHeldList
			fnHandHeldList=fnHandHeldList(mat device$, mat deviceOption$)
		fnend
		def library fnRetrieveHandHeldFile(; automationBookNumber)
			library 'S:\Utility Billing\Hand Held\Import from Hand Held to Book.br': fnRetrieveHandHeldFile
		 fnRetrieveHandHeldFile=fnRetrieveHandHeldFile( automationBookNumber)
		fnend
		def library fnMeterInfo$*30(mi_field$,z$*10,serviceCode$; closeHandle)
			library 'S:\Utility Billing\Hand Held\Create Hand Held File.br': fnMeterInfo$
		 fnMeterInfo$=fnMeterInfo$(mi_field$,z$,serviceCode$, closeHandle)
		fnend
	! /r
! /r
! r: GL   General Ledger
	def library fnReassignTransactionAddresses(cno)
		library 'S:\General Ledger\Reassign Transaction Addresses.br': fnReassignTransactionAddresses
		fnReassignTransactionAddresses=fnReassignTransactionAddresses(cno)
	fnend
	def library fnLastAccountingPeriodClosed(; setit)
		library 'S:\acsGL\company.br': fnLastAccountingPeriodClosed
		fnLastAccountingPeriodClosed=fnLastAccountingPeriodClosed( setit)
	fnend
	def library fnAddGlPayee
		library 'S:\General Ledger\fn\addGlPayee.br': fnAddGlPayee
		fnAddGlPayee=fnAddGlPayee
	fnend
	def library fnBalanceSheet(; defaultFormat$)
		library 'S:\General Ledger\fn\balanceSheet.br': fnBalanceSheet
		fnBalanceSheet=fnBalanceSheet( defaultFormat$)
	fnend
	def library fnGetFundList(mat fund_list)
		library 'S:\General Ledger\fn\getFundList.br': fnGetFundList
		fnGetFundList=fnGetFundList(mat fund_list)
	fnend
	def library fnW2supEdit(;empNo$)
		library 'S:\acsGL\w2box16.br': fnW2supEdit
		fnW2supEdit=fnW2supEdit( empNo$)
	fnend
	def library fnCmbBud(&indexfile$)
		library 'S:\acsGL\CmbBud.br': fnCmbBud
		fnCmbBud=fnCmbBud(indexfile$)
	fnend
	def library fnActPd(;a)
		library 'S:\General Ledger\fn\actPd.br': fnActPd
		fnActPd=fnActPd(a)
	fnend
	def library fnActPd$(;a$)
		library 'S:\acsGL\fnActPd$.br': fnActPd$
		fnActPd$=fnActPd$(a$)
	fnend
	def library fnCch$*20(;a$*20)
		library 'S:\acsGL\fnCCH$.br': fnCch$
		fnCch$=fnCch$(a$)
	fnend
	def library fnGlAskFormatPriorCdPeriod(; defaultFormatOption$)
		library 'S:\acsGL\fnglfs.br': fnGlAskFormatPriorCdPeriod
		fnGlAskFormatPriorCdPeriod=fnGlAskFormatPriorCdPeriod( defaultFormatOption$)
	fnend
	def library fnGlAccountSearch(&account$)
		library 'S:\General Ledger\fn\glAccountSearch.br': fnGlAccountSearch
		fnGlAccountSearch=fnGlAccountSearch(account$)
	fnend
	def library fnacglblds
		library 'S:\acsGL\fnacglblds.br': fnacglblds
		fnacglblds=fnacglblds
	fnend
	def library fnemployee_search(&x$;fixgrid)
		library 'S:\acsGL\employee_search.br': fnemployee_search
		fnemployee_search=fnemployee_search(x$,fixgrid)
	fnend
	def library fnfinstmt_v0_to_v1
		library 'S:\acsGL\Conversion\FinStmt_v0_to_v1.br': fnfinstmt_v0_to_v1
		fnfinstmt_v0_to_v1=fnfinstmt_v0_to_v1
	fnend
	def library fnglmstr_338_416
		library 'S:\acsGL\Conversion\glMstr-338-416.br': fnglmstr_338_416
		fnglmstr_338_416=fnglmstr_338_416
	fnend
	def library fnglpayee_v0_to_v1
		library 'S:\acsGL\Conversion\glPayee_v0_to_v1.br': fnglpayee_v0_to_v1
		fnglpayee_v0_to_v1=fnglpayee_v0_to_v1
	fnend
	def library fnrepr(x$)
		library 'S:\Core\repr.br': fnrepr
		fnrepr=fnrepr(x$)
	fnend
! /r
! r: CL   Checkbook
	def library fnApMstrConversion
	library 'S:\acsCL\Conversion\apmstr-cnv.br': fnApMstrConversion
	fnApMstrConversion=fnApMstrConversion
	fnend
	def library fnPostCheckbookToGl(; enablePost)
		library 'S:\Checkbook\fn\postCheckbookToGl.br': fnPostCheckbookToGl
		fnPostCheckbookToGl( enablePost)
	fnend
	def library fntrmstr_v1_to_v2
		library 'S:\acsCL\Conversion\TrMstr-v1-to-v2.br': fntrmstr_v1_to_v2
		fntrmstr_v1_to_v2=fntrmstr_v1_to_v2
	fnend
	def library fntralloc_v1_to_v2
		library 'S:\acsCL\Conversion\TrAlloc-v1-to-v2.br': fntralloc_v1_to_v2
		fntralloc_v1_to_v2=fntralloc_v1_to_v2
	fnend
	def library fnpaytrans_v1_to_v2
		library 'S:\acsCL\Conversion\PayTrans-v1-to-v2.br': fnpaytrans_v1_to_v2
		fnpaytrans_v1_to_v2=fnpaytrans_v1_to_v2
	fnend
	def library fnunpdaloc_v1_to_v2
		library 'S:\acsCL\Conversion\UnPdAloc-v1-to-v2.br': fnunpdaloc_v1_to_v2
		fnunpdaloc_v1_to_v2=fnunpdaloc_v1_to_v2
	fnend
	def library fnpaymstr_v0_to_v1
		library 'S:\acsCL\Conversion\PayMstr-v0-to-v1.br': fnpaymstr_v0_to_v1
		fnpaymstr_v0_to_v1=fnpaymstr_v0_to_v1
	fnend
	def library fnglmstrtorecl62
		library 'S:\acsCL\Conversion\GLMstr-to-RecL62.br': fnglmstrtorecl62
		fnglmstrtorecl62=fnglmstrtorecl62
	fnend
	def library fnaddpayee
		library 'S:\Checkbook\Payee.br': fnaddpayee
		fnaddpayee=fnaddpayee
	fnend
	def library fnaddreceipt
		library 'S:\acsCL\fnReceipt.br': fnaddreceipt
		fnaddreceipt=fnaddreceipt
	fnend
	def library fnbankbal(x)
		library 'S:\acsCL\fnBankBal.br': fnbankbal
		fnbankbal=fnbankbal(x)
	fnend
	def library fnupdatebankbal(bank_code,modification)
		library 'S:\acsCL\fnUpdateBankBal.br': fnupdatebankbal
		fnupdatebankbal=fnupdatebankbal(bank_code,modification)
	fnend
! /r
! r: PR   payroll
	def library fnEmployeeEdit(eno)
		library 'S:\Payroll\Employee.br': fnEmployeeEdit
		fnEmployeeEdit=fnEmployeeEdit(eno)
	fnend
	def library fnEmployeeData$(eno,field$*64; setIt$*64,defaultIfNokey$*64,defaultIfBlank$*64)
		library 'S:\Payroll\fn\employeeData.br': fnEmployeeData$
		fnEmployeeData$=fnEmployeeData$(eno,field$, setIt$,defaultIfNokey$,defaultIfBlank$)
	fnend
	def library fnEmployeeDataClose
		library 'S:\Payroll\fn\employeeData.br': fnEmployeeDataClose
		fnEmployeeDataClose=fnEmployeeDataClose
	fnend


	def library fnCheckPayrollCalculation
		library 'S:\Payroll\Calculation.br': fnCheckPayrollCalculation
		fnCheckPayrollCalculation=fnCheckPayrollCalculation
	fnend
	def library fnGetEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)
		! dim marriedOption$(0)*58
		! dim eicOption$(0)*29
		! dim w4yearOption$(0)*4
		! dim payPeriodOption$(0)*16
		library 'S:\Payroll\Employee.br': fnGetEmpOptions
		fnGetEmpOptions=fnGetEmpOptions(mat marriedOption$,mat eicOption$,mat w4yearOption$,mat payPeriodOption$)
	fnend


	def library fnDeptName$*25(departmentCode)
		library 'S:\Payroll\fn\deptName.br': fnDeptName$
		fnDeptName$=fnDeptName$(departmentCode)
	fnend

	def library fnPrPrintNetZeroDefault$(; ___,return$)
		if env$('client')='Divernon' or env$('client')='Payroll Done Right' then
			return$='True'
		else
			return$='False'
		end if
		fnPrPrintNetZeroDefault$=return$
	fnend
	def library fnDedNames(mat fullname$; mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$,doWrite)
		library 'S:\Payroll\fn\dedNames.br': fnDedNames
		fnDedNames=fnDedNames(mat fullname$, mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$,doWrite)
	fnend
	def library fnprint_designed_report(rptn)
		library 'S:\acsPR\newprRptS1.br': fnprint_designed_report
		fnprint_designed_report=fnprint_designed_report(rptn)
	fnend
	def library fnpayroll_register_2(; det,include_tips_in_other_wh,append_reg1,ppdOverride)
		library 'S:\acsPR\newprreg2.br': fnpayroll_register_2
		fnpayroll_register_2=fnpayroll_register_2( det,include_tips_in_other_wh,append_reg1,ppdOverride)
	fnend
	def library fnss_employee
		library 'S:\acsPR\ss_emp.br': fnss_employee
		fnss_employee=fnss_employee
	fnend
	def library fnss_employer
		library 'S:\acsPR\ss_emp.br': fnss_employer
		fnss_employer=fnss_employer
	fnend
	def library fnemployee_srch(&x$;fixgrid)
		library 'S:\acsPR\Employee_srch.br': fnemployee_srch
		fnemployee_srch(x$,fixgrid)
	fnend
	def library fncat_srch2(&x$,&ckey;fixgrid)
		library 'S:\acsPR\CAT_srch2.br': fncat_srch2
		fncat_srch2(x$,ckey,fixgrid)
	fnend
	def library fncategory_srch(&cn$;fixgrid)
		library 'S:\acsPR\CATegory_srch.br': fncategory_srch
		fncategory_srch(x$,fixgrid)
	fnend
	def library fnsubcat_srch(&cde$,&ckey;fixgrid)
		library 'S:\acsPR\SubCat_srch.br': fnsubcat_srch
		fnsubcat_srch(cde$,ckey,fixgrid)
	fnend
	def library fncmbemp(lyne,mypos; addall,container)
		library 'S:\acsPR\CmbEmp.br': fncmbemp
		fncmbemp=fncmbemp(lyne,mypos, addall,container)
	fnend
	def library fncmbcategory(lyne,mypos;addall,c,a$*30)
		library 'S:\acsPR\CmbCategory.br': fncmbcategory
		fncmbcategory(lyne,mypos,addall,c,a$)
	fnend
	def library fnCheckFile(hact$,filnum,hCheckHistory,hEmployee)
		library 'S:\Payroll\Payroll Check History.br': fnCheckFile
		fnCheckFile=fnCheckFile(hact$,filnum,hCheckHistory,hEmployee)
	fnend
	def library fnhours(eno)
		library 'S:\acsPR\hours_lib.br': fnhours
		fnhours(eno)
	fnend
	def library fncmbjob(lyne,mypos;addall,c,a$*30)
		library 'S:\acsPR\Cmbjob.br': fncmbjob
		fncmbjob(lyne,mypos,addall,c,a$)
	fnend
	def library fncmbcat(lyne,mypos;addall,c,a$*30)
		library 'S:\acsPR\CmbCat.br': fncmbcat
		fncmbcat(lyne,mypos,addall,c,a$)
	fnend
	def library fncmbsubcat(lyne,mypos;addall,c)
		library 'S:\acsPR\CmbSubCat.br': fncmbsubcat
		fncmbsubcat(lyne,mypos,addall,c)
	fnend
	def library fnpr_conversion_department(cno; medicare_is_seperated)
		library 'S:\acsPR\Conversion\v4_cnv.br': fnpr_conversion_department
		fnpr_conversion_department=fnpr_conversion_department(cno, medicare_is_seperated)
	fnend
	def library fnpr_conversion_add_missing(cno)
		library 'S:\acsPR\Conversion\v4_part2.br': fnpr_conversion_add_missing
		fnpr_conversion_add_missing=fnpr_conversion_add_missing(cno)
	fnend
	def library fnjob_srch(&x$; fixgrid)
		library 'S:\acsPR\Job_srch.br': fnjob_srch
		fnjob_srch=fnjob_srch(x$,fixgrid)
	fnend
	def library fnGetPayrollDates(&beg_date,&end_date; &qtr1,&qtr2,&qtr3,&qtr4)
		library 'S:\Payroll\Change Payroll Dates.br': fnGetPayrollDates
		fnGetPayrollDates=fnGetPayrollDates(beg_date,end_date, qtr1,qtr2,qtr3,qtr4)
	fnend
	def library fnPayPeriodEndingDate(; setIt)
		library 'S:\Payroll\Change Payroll Dates.br': fnPayPeriodEndingDate
		fnPayPeriodEndingDate=fnPayPeriodEndingDate( setIt)
	fnend
	def library fnSetPayrollDatesForYear(; year)
		library 'S:\Payroll\Change Payroll Dates.br': fnSetPayrollDatesForYear
		fnSetPayrollDatesForYear=fnSetPayrollDatesForYear( year)
	fnend

	def library fnCompanyPayPeriodEndingDate(cno)
		library 'S:\Payroll\Change Payroll Dates.br': fnCompanyPayPeriodEndingDate
		fnCompanyPayPeriodEndingDate=fnCompanyPayPeriodEndingDate(cno)
	fnend
	! r: Job Cost Payroll
		def library fncmbburden(lyne,mypos;addall,c,a$*30)
			library 'S:\Payroll\Job Cost\fnCmbBurden.br': fncmbburden
			fncmbburden(lyne,mypos,addall,c,a$)
		fnend
		def library fnburden_srch(&x$;fixgrid)
			library 'S:\Payroll\Job Cost\fnBurden_srch.br': fnburden_srch
			fnburden_srch(x$,fixgrid)
		fnend
	! /r
! /r
! r: Client Billing
	def library fnSearch(h,form$*128,nformat$*20,&sel$,klength)
		library 'S:\Client Billing\fn\search.br': fnSearch
		fnSearch=fnSearch(h,form$,nformat$,sel$,klength)
	fnend
	def library fnReassignNTA(filename$*256,keyForm$,ntaForm$)
		library 'S:\Client Billing\fn\reassignNTA.br': fnReassignNTA
		fnReassignNTA=fnReassignNTA(filename$,keyForm$,ntaForm$)
	fnend
	def library fnInvoiceOpen
		library 'S:\Client Billing\fn\invoice.br': fnInvoiceOpen
		fnInvoiceOpen=fnInvoiceOpen
	fnend
	def library fnInvoiceAdd(actnum$,mat billto$,invNum$,invDate,mat desc$,mat amt,pbal)
		library 'S:\Client Billing\fn\invoice.br': fnInvoiceAdd
		fnInvoiceAdd=fnInvoiceAdd(actnum$,mat billto$,invNum$,invDate,mat desc$,mat amt,pbal)
	fnend
	def library fnInvoiceClose(inv_date; filenameAddOn$*128)
		library 'S:\Client Billing\fn\invoice.br': fnInvoiceClose
		fnInvoiceClose=fnInvoiceClose(inv_date, filenameAddOn$)
	fnend
	def library fnEmailQueuedInvoices(emailDate$)
		library 'S:\Client Billing\Email Queued Invoices.br': fnEmailQueuedInvoices
		fnEmailQueuedInvoices=fnEmailQueuedInvoices(emailDate$)
	fnend
	def library fnRead30Categories(mat dimTo30$)
		library 'S:\Client Billing\Category.br': fnRead30Categories
		fnRead30Categories=fnRead30Categories(mat dimTo30$)
	fnend
	def library fnCustomerHasEbilling(Client_id$)
		library 'S:\Client Billing\fn\customerHasEbilling.br': fnCustomerHasEbilling
		fnCustomerHasEbilling=fnCustomerHasEbilling(Client_id$)
	fnend
	def library fnMergeInvoices
		library 'S:\Client Billing\ACS Invoices.br': fnMergeInvoices
		fnMergeInvoices=fnMergeInvoices
	fnend
	def library fnTransactionTypeDescription$(typeCode)
		library 'S:\Client Billing\Delete Transaction.br': fnTransactionTypeDescription$
		fnTransactionTypeDescription$=fnTransactionTypeDescription$(typeCode)
	fnend
	def library fnClientName$*30(clientId$)
		library 'S:\Client Billing\Delete Transaction.br': fnClientName$
		fnClientName$=fnClientName$(clientId$)
	fnend

! /r

