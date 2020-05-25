! Replace S:\Core\Programs\PrintTest
! ___________
	autoLibrary
	on error goto Ertn
! __________________
	dim prg$*256,cap$*128,ln$*900
! ___________________
	cancel=5
	fnTop(prg$='S:\Core\Programs\PrintTest',cap$='Print Test')
! ___________________
	fnTos(sn$='PrintTest') : _
	rc=0
	fnLbl(1,1,'Characters Per Line:',40,2)
	fnTxt(1,42,3,0,0,'30') : _
	resp$(rc+=1)='80'
	fnLbl(2,1,'Lines to print:',40,2)
	fnTxt(2,42,3,0,0,'30') : _
	resp$(rc+=1)='54'
	fnCmdSet(2)
	fnAcs2(mat resp$,ck)
	if ck=cancel then goto Xit
! _
! add fnWAIT here when it been rewritten
	fnopenprn
	ln$=rpt$("X",val(resp$(1)))
	for j=1 to val(resp$(2)) : pr #255: ln$ : next j
	fncloseprn
	goto Xit
 
Xit: !
	fnXit
 
include: Ertn
 
