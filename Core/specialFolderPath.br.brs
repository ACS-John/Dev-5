
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
	fnAddOneC(mat testValue$,'Personal')
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
		library 'S:\Core\Library': fngethandle
	end if
fnend
def library fnSpecialFolderPath$*256(folderName$*64)
	if ~setup then let fn_setup
	fnSpecialFolderPath$=fn_specialFolderPath$(folderName$)
fnend
def fn_specialFolderPath$*256(folderName$*64; ___,line$*256,lineCount,return$*256)
	exe 'sy -M reg query "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders" /v "'&folderName$&'" >"'&env$('client_temp')&'\sfp'&session$&'.txt"'
	open #hTmp:=fngethandle: 'name='&env$('at')&env$('client_temp')&'\sfp'&session$&'.txt',d,input
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
