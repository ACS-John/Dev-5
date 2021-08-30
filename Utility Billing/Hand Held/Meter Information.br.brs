! you're probably looking for Meter Location instead of this

autoLibrary
fnTop(program$)
if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$(1:6)<>'[Ask]' and fnhand_held_device$(1:6)<>'Aclara' and fnhand_held_device$<>'Master Meter' then 
	dim msg_text$(2)*256
	msg_text$(1)='The '&env$('program_caption')&' file is not necessary'
	msg_text$(2)="for your hand held device type."
	fnmsgbox(mat msg_text$, response$,'',64)
end if 
fnHamsterFio("UB Meter Info")
goto Xit
Xit: fnXit
def fn_CsvToMeterInformation
	colP=16
	dim line$*1024,item$(0)*256
	open #hIn=fnH: 'name=[at]'&'C:\ACS\(Client_Files)\Purdy\Purdy MO Installation List 20171015 Final All.txt',d,input
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	linput #hIn: line$ eof CtmiEof ! consume headers
	do
		linput #hIn: line$ eof CtmiEof
		str2mat(line$,mat item$, chr$(9))
		fn_isKeyValid(item$(1),hCustomer) ! pr item$(1),item$(colP),fn_isKeyValid(item$(1),hCustomer)
		if item$(colP)='1 inch' then 
			MeterType=1
		else if item$(colP)='2 inch T-10' then 
			MeterType=21
		else if item$(colP)='1.5 inch' then 
			MeterType=15
		else if item$(colP)='2 inch Turbine' then 
			MeterType=2
		else if item$(colP)='3 inch' then 
			MeterType=3
		else if item$(colP)='4 inch' then 
			MeterType=4
		else if item$(colP)='6 inch' then 
			MeterType=6
		else
			if item$(colP)<>'5/8x3/4' then pr item$(colP) : pause
			MeterType=5
		end if
		write #open_file(1),using 'form pos 1,c 10,c 2,C 17,c 17,c 12,c 20,n 5': trim$(item$(1)),'WA','','','','',MeterType
	loop
	CtmiEof: !
fnend
def fn_isKeyValid(&key$,hFile)
	ikvReturn=0
	key$=lpad$(trim$(key$),kln(hFile))
	read #hFile,key=key$: nokey IkvTryRpad
	ikvReturn=1
	goto IkvFinis
	IkvTryRpad: !
		key$=rpad$(trim$(key$),kln(hFile))
		read #hFile,key=key$: nokey IkvNoKey
		ikvReturn=2
	goto IkvFinis
	IkvNoKey: !
		ikvReturn=0 : pr 'key failed: '&key$ ! pause
	goto IkvFinis
	IkvFinis: !
	fn_isKeyValid=ikvReturn
fnend
