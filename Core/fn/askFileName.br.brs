! r: test zone.
fn_setup
dim filename$*512
if fn_askFileName(filename$,'open','*.txt;*.csv','Tab Delimited',env$('cap'))>0 then
	! r: read file and make mat col_account$ and mat col_meterNumber$ 
	pr 'selected filename is: "'&filename$&'"'
else
	pr 'canceled'
end if
end
! /r
Xit: fnXit
def library fnAskFileName(&opFileOpen$,purpose$; filter$,filterDescription$*64,path$*256,recallAddOn$*64)
	if ~setup then fn_setup
	fnAskFileName=fn_askFileName(opFileOpen$,purpose$, filter$,filterDescription$,path$,recallAddOn$)
fnend
def fn_askFileName(&opFileOpen$,purpose$; filter$,filterDescription$*64,path$*256,recallAddOn$*64,___,returnN)
	! recallAddOn$ - adds on to the purpose to store the path for future default
	if purpose$<>'open' and purpose$<>'save' then
		pr 'fn_askFileName does not yet supports "'&purpose$&'".  if you are using it for another purpose, you will need to add the code for that.'
		pause
	end if
	if filter$='' then filter$='*.*'
	! fnFree(br_filename$(env$('temp')&'\acs\Open_Log.txt'))
	if filterDescription$<>'' then filterDescription$=filterDescription$&' ('&filter$&') '
	dim filePath$*256
	fnureg_read('askFileName.'&recallAddOn$,filePath$, os_filename$(env$('Desktop')))
	open #h_tmp:=fnH: 'Name='&purpose$&':'&env$('at')&filterDescription$&'|'&filePath$&'\'&filter$&',RecL=1,Shr',external,input ioerr OP_OP_ERR
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
	if returnN then 
		fnureg_write('askFileName.'&recallAddOn$,filePath$)
	end if
	fn_askFileName=returnN
fnend
include: fn_setup
include: fn_open