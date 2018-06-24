fn_setup
dim filename$*512
if fn_askFileName(filename$,'open','*.txt;*.csv','Tab Delimited',env$('cap'))>0 then
	! r: read file and make mat col1$ and mat col2$ 
	open #hIn:=fngethandle: 'name='&filename$,d,i
	dim line$*512
	dim item$(0)*80
	dim col1$(0)*80
	dim col2$(0)*80
	do
		linput #hIn: line$ eof EoIn
		if trim$(line$)<>'' then
			str2mat(line$,mat item$,chr$(9))
			newCount=fnAddOneC(mat col1$,item$(1),1,1)
			if newCount<=udim(mat col2$) then
				pr 'dupe key'
				newCount=srch(mat col1$,item$(1))
				if newCount>0 then
					if col2$(newCount)<>item$(2) then
						pr 'mismatched translations encountered'
						pr 'col1$(newCount)='&col1$(newCount)
						pr 'item$(2)='&item$(2)
						pause
					end if
				else
					pr 'invalid col1: '&item$(1)
				end if
			else
				fnAddOneC(mat col2$,item$(2))
			end if
		en if
	loop
	EoIn: !
	! /r
	! r: update the Meter Location table from mat col1$ and mat col2$ (and update mat used)
		mat used(udim(mat col1$))
		dim location$(0)*256,locationN(0)
		hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,1)
		do
			read #hLocation,using form$(hLocation): mat location$,mat locationN eof EoLocation
			which=srch(mat col1$,trim$(location$(loc_meternumber)))
			if which>0 then
				matchCount+=1
			else 
				missCount+=1
			end if
		loop
		EoLocation: !
		pr 'matchCount=';matchCount
		pr 'missCount=';missCount
		pause
	! /r
	! r: determine if any updates were done more than once or not at all
	! /r
end if
goto xit
Xit: stop ! fnXit
include: fn_open
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnmsgbox
		library 'S:\Core\Library': fnXit
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnureg_read,fnureg_write
		on error goto Ertn
	end if
fnend
def library fnAskFileName
	if ~setup then let fn_setup
	fnAskFileName=fn_askFileName
fnend
def fn_askFileName(&opFileOpen$,purpose$; filter$,filterDescription$*64,path$*256,recallAddOn$*64,___,returnN)
	! recallAddOn$ - adds on to the purpose to store the path for future default
	if purpose$<>'open' and purpose$<>'save' then
		pr 'fn_askFileName does not yet supports "'&purpose$&'".  if you are using it for another purpose, you will need to add the code for that.'
		pause
	end if
	if filter$='' then filter$='*.*'
	! fnFree(br_filename$(env$('temp')&'\acs\Open_Log.txt'))
	if filterDescription$<>'' then filterDescription$&=' ('&filter$&') '
	dim filePath$*256
	fnureg_read('askFileName.'&recallAddOn$,filePath$, os_filename$(env$('userprofile')&'\Desktop'))
	open #h_tmp:=fngethandle: 'Name='&purpose$&':'&env$('at')&filterDescription$&'|'&filePath$&'\'&filter$&',RecL=1,Shr',external,input ioerr OP_OP_ERR
	opFileOpen$=os_filename$(file$(h_tmp))
	close #h_tmp: 
	returnN=1
	goto OP_XIT
	OP_OP_ERR: ! 
	if err=622 then ! it was just canceled
		pr 'canceled' : goto OP_XIT
	else 
		dim ml$(0)*128
		mat ml$(2)
		ml$(1)='Select a different file name.'
		ml$(2)='Error: '&str$(err)
		fnmsgbox(mat ml$,resp$)
		!     if err=4150 then pr "Could not create file:";file$(1) : fnpause ! file$(1) is blank!
		pr "Err:";err;" Line:";line
	end if 
	OP_XIT: ! 
	if returnN then fnureg_write('askFileName.'&recallAddOn$,filePath$)
	fn_askFileName=returnN
fnend
include: ertn