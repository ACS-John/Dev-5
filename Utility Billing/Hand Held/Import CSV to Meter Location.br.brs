include: top
dim filename$*512
if fn_askFileName(filename$,'open','*.txt;*.csv','Tab Delimited',env$('cap'))>0 then
	open #hIn:=fngethandle: 'name=',d,i
end if

include: end
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnGetHandle
		library 'S:\Core\Library': fnmsgbox
		library 'S:\Core\Library': fnureg_read,fnureg_write
	end if
fnend
def library fnAskFileName
	if ~setup then let fn_setup
	fnAskFileName=fn_askFileName
fnend
def fn_askFileName(opFileOpen$*512,purpose$; filter$,filterDescription$,path$*256,recallAddOn$*64,___,returnN)
	! recallAddOn$ - adds on to the purpose to store the path for future default
	if purpose$<>'open' and purpose$<>'save' then
		pr 'fn_askFileName does not yet supports "'&purpose$&'".  if you are using it for another purpose, you will need to add the code for that.'
		pause
	end if
	if filter$='' then filter$='*.*'
	! fnFree(br_filename$(env$('temp')&'\acs\Open_Log.txt'))
	open #h_tmp:=fngethandle: 'Name='&purpose$&':'&env$('at')&"ACS Data Set (*.zip) |"&fnsave_as_path$&"\*.zip,RecL=1,Shr",external,input ioerr OP_OP_ERR
	opFileOpen$=os_filename$(file$(h_tmp))
	close #h_tmp: 

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
	fn_askFileName=returnN
fnend
