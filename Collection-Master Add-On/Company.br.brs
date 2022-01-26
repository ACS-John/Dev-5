autoLibrary
on error goto Ertn
fnTop(program$)
gosub CompanyLoad
fnTos : col1Len=19 : col2Pos=col1Len+2
! r: company information portion of screen
	fnLbl(1,1,"Name:",col1Len,1)
	fnTxt(1,col2Pos,40)
	fnLbl(2,1,"Address:",col1Len,1)
	fnTxt(2,col2Pos,40)
	fnLbl(3,1,"City,State and Zip:",col1Len,1)
	fnTxt(3,col2Pos,40)
! /r
fnCmdSet(2)
fnAcs(mat Comp$,ckey)
if ckey<>5 then
	gosub CompanySave
end if
Xit: fnXit
 
CompanyLoad: ! r:
	dim comp$(0)*128,compN(0),form$(0)*256
	hCompany=fnFioOpenFile(env$('cursys')&' Company',mat comp$,mat compN,mat form$, 0,0,0,unused$,mat unused$,mat unused,mat unused$,supressprompt:=2)
	read #hCompany,using form$(hCompany): mat comp$,mat compN ioerr ignore
	close #hCompany: ioerr ignore
return  ! /r
CompanySave: ! r:
	fnFree('[Q]\[CurSys]mstr\Company.h[cno]')
	dim fileiosubs$(0)*512
	hCompany=fnOpenFile(env$('cursys')&' Company',mat comp$,mat compN,mat form$, 0,0,0,unused$,mat unused$,mat unused,mat fileiosubs$,supressprompt:=2)
	write #hCompany,using form$(hCompany): mat comp$,mat compN
	close #hCompany:
return  ! /r
include: ertn
 
