autoLibrary
on error goto Ertn
fntop(program$)
dim device$*256
device$=fnhand_held_device$
if device$<>'Itron FC300' and device$(1:6)<>'[Ask]' and device$(1:6)<>'Aclara'  and device$<>'Badger Beacon' and device$<>'Neptune (Equinox v4)' then 
	dim msg_text$(2)*256
	msg_text$(1)='The '&env$('Program Caption')&' file is not necessary'
	msg_text$(2)="for your hand held device type."
	fnmsgbox(mat msg_text$, response$,'',64)
end if 
fnHamsterFio("U4 Meter Type")
goto Xit
Xit: fnxit
include: ertn