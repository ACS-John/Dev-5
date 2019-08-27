library 'S:\Core\Library': fntop,fnxit, fnmsgbox,fnhand_held_device$,fnHamsterFio
on error goto ERTN
fntop(program$)
if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$(1:6)<>'[Ask]' and fnhand_held_device$(1:6)<>'Aclara'  and fnhand_held_device$<>'Badger Beacon' then 
	dim msg_text$(2)*256
	msg_text$(1)='The '&env$('Program Caption')&' file is not necessary'
	msg_text$(2)="for your hand held device type."
	fnmsgbox(mat msg_text$, response$,'',64)
end if 
fnHamsterFio("U4 Meter Type")
goto XIT
XIT: fnxit
include: ertn