autoLibrary
fnTop(program$)
on error goto Ertn
dim a$(10)*40
fnCreg_read('PLAWA Accrual Rate'	,a$(1), '0.025',1)
fnCreg_read('PLAWA Frontload'   	,a$(2), '0',1)


Screen1: ! r:
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	! fram1=1: fnFra(1,1,10,80,'PLAWA Settings for Company '&env$('cno'))
	fnLbl(1,1,'Accrual Rate:',mylen,right)              	: fnTxt(1,mypos,8,0,left,'36',0,'The default 0.025 is 1 hour of PLAWA for every 40ye hours worked (1/40).')
	fnLbl(1,1,'Frontload:',mylen,right)                 	: fnTxt(1,mypos,8,0,left,'36',0,'Number of hours to frontload for employees at year beginning. i.e. 40')
	fnCmdKey('&Save'  ,ck_save=1   	,1,0,'Saves any changes and returns to menu without reviewing remainder of screens.')
	fnCmdKey('&Cancel',ck_cancel=5	,0,1,'Returns to menu without saving any changes on any screen.')
	ckey=fnAcs(mat a$)
	if ckey=ck_save then
		fnCreg_write('PLAWA Accrual Rate',a$(1))
		fnCreg_write('PLAWA Frontload'   ,a$(2))
	end if
	if ckey<>ck_cancel then goto Screen1
goto Xit

Xit: fnXit
include: ertn
