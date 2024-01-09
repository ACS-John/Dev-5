
! extract  city$,state$,zip$ from csz$
def library fnCsz(&csz$,&city$,&state$,&zip$; ___,returnN,posBreak1,posBreak2,lenZip)
	autoLibrary
	csz$=rtrm$(csz$)
	do
		csz$=srep$(csz$,'  ',' ')
	loop until pos(csz$,'  ')<=0
	do
		posBreak1=pos(csz$,'.',1)
		if posBreak1>0 then csz$(posBreak1:posBreak1)=''
	loop while posBreak1>0
	! l1=len(csz$)   ! use inf instead
	posBreak1=pos(csz$,',',1)-1
	if posBreak1=-1 then posBreak1=pos(csz$,' ',1)-1
	posBreak2=pos(csz$,' ',posBreak1+3)
	city$=uprc$(rtrm$(csz$(1:posBreak1))(1:15))
	if posBreak2=0 then posBreak2=pos(csz$,' ',-1)+2
		! just in case they forgot a space after the state before the zip,
		! but didn't forget the space after the city comma.
	if uprc$(city$(1:3))='FT ' or uprc$(city$(1:3))='FT.' then
		city$(1:3)='Fort '
	end if
	state$=uprc$(rtrm$(csz$(posBreak2-2:posBreak2))(1:2))
	zip$=uprc$(ltrm$(rtrm$(csz$(posBreak2+1:inf))))
	zip5$=zip$(1:5)
	zip4$=''
	lenZip=len(zip$)

	if lenZip<9 then goto Finis

	on error goto ReportError

	posBreak2=pos(csz$,' ',posBreak1+3)
	city$=rtrm$(csz$(1:posBreak1))(1:15)
	state$=rtrm$(csz$(posBreak2-2:posBreak2))(1:2)
	zip$=uprc$(ltrm$(rtrm$(csz$(posBreak2+1:inf))))
	zip5$=zip$(1:5)
	zip4$=''
	lenZip=len(zip$)
	!	pr STATE$ ! XXX
	returnN=1
	goto Finis

	ReportError: ! r:
		returnN=-err
		dim ml$(2)*80
		mat ml$(2)
		ml$(1)='You have a bad address: '&csz$
		ml$(2)='You should fix the address and run this option again.'
		fnMsgBox(mat ml$,resp$,'',48)
	goto Finis ! /r
	Finis: !
	fnCsz=returnN
fnend

