
if env$('acsDeveloper')='' then
	pr 'this program is not intended to be run directly.'
	goto Xit
end if
! r: test zone - use this area to test local functions when running the program directly (as a developer)
	fn_setup
	dim testValue$(0)*64
	fnAddOneC(mat testValue$,'Desktop')
	fnAddOneC(mat testValue$,'My Pictures')
	fnAddOneC(mat testValue$,'My Video')
	fnAddOneC(mat testValue$,'Personal') ! my documents
	for testValueItem=1 to udim(mat testValue$)
		print 'fn_specialFolderPath$("'&testValue$(testValueItem)&'") returns "'&fn_specialFolderPath$(testValue$(testValueItem))&'".'
	nex testValueItem
goto Xit ! /r
Xit: !
end ! /r
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnH
	end if
fnend
def library fnSpecialFolderPath$*256(folderName$*64)
	if ~setup then fn_setup
	fnSpecialFolderPath$=fn_specialFolderPath$(folderName$)
fnend
def fn_specialFolderPath$*256(folderName$*64; ___,line$*256,lineCount,return$*256,tmpFile$*512)
	!  6/17/21 - switched to [temp] for CM/stern where this wasn't working.       open #hTmp=fnH: 'name=[at][client_temp]\sfp[session].txt',d,input
	!  6/30/21 - switched to [cleint_temp] for White Hall where this wasn't working.
	if env$('cursys')='CM' then
		tmpFile$=env$('temp')&'\sfp'&session$&'.txt"' ! [temp]\sfp[session].txt
	else
		tmpFile$=env$('client_temp')&'\sfp'&session$&'.txt' ! [client_temp]\sfp[session].txt
		
	end if
	! r: warning found 1/18/24 in Regedit
	! HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders
	! !Do not use this registry key
	! /r
	! Use the SHGetFolderPath or SHGetKnownFolderPath function instead
	exe 'sy -M reg query "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" /v "'&folderName$&'" >"'&tmpFile$&'"'
	open #hTmp=fnH: 'name='&env$('at')&tmpFile$,d,input
	do
		linput #hTmp: line$ EoF EoTmp
		if trim$(line$)<>'' then
			lineCount+=1
			if lineCount=2 then
				line$=srep$(trim$(line$),'    ',chr$(9))
				dim item$(0)*256
				str2mat(line$,mat item$,chr$(9))
				if udim(mat item$)=3 then
					return$=item$(3)
					goto EoTmp
				else
					pr 'issue in fn_specialFolderPath$ - contact ACS Technical Support.  Potentially unsupported OS.'
					pause
				end if
			end if
		end if
	loop
	EoTmp: !
	close #hTmp,free:
	fn_specialFolderPath$=return$
fnend
