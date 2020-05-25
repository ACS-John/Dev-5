! r: setup
	autoLibrary
	on error goto Ertn
 
	dim Comp$(0)*128,CompN(0),form$(0)*256
	dim cap$*128
! /r
	fnTop(program$)
	gosub COMPANY_LOAD
	fnTos(sn$="Company") : col1Len=19 : col2Pos=col1Len+2
! r: company information portion of screen
	fnLbl(1,1,"Name:",col1Len,1)
	fnTxt(1,col2Pos,40)
	fnLbl(2,1,"Address:",col1Len,1)
	fnTxt(2,col2Pos,40)
	fnLbl(3,1,"City,State and Zip:",col1Len,1)
	fnTxt(3,col2Pos,40)
! /r
	fnCmdSet(2)
	fnAcs2(mat Comp$,ck)
	if ck<>5 then
		gosub COMPANY_SAVE
	end if
Xit: fnXit
IGNORE: continue
include: Ertn
COMPANY_LOAD: ! r:
	hCompany=fnOpenFile(env$('cursys')&' Company',mat Comp$,mat CompN,mat form$, 0,0,0,unused$,mat unused$,mat unused,mat unused$,supressprompt:=2)
	read #hCompany,using form$(hCompany): mat Comp$,mat CompN ioerr ignore
	close #hCompany: ioerr ignore
return  ! /r
COMPANY_SAVE: ! r:
	fnFree('[Q]\'&env$('cursys')&'mstr\Company.h[cno]')
	dim fileiosubs$(0)*512
	hCompany=fnOpenFile(env$('cursys')&' Company',mat Comp$,mat CompN,mat form$, 0,0,0,unused$,mat unused$,mat unused,mat fileiosubs$,supressprompt:=2)
	write #hCompany,using form$(hCompany): mat Comp$,mat CompN
	close #hCompany:
return  ! /r
 
