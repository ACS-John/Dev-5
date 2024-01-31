! Replace S:\acsGL\PRClose
! GENERAL LEDGER Payroll Only Month End Closing
! empty the General Ledger Payroll Checks File
autoLibrary
on error goto Ertn
fnTop(program$)
open #hTmp=fnH: 'Name=[Q]\GLmstr\AcPrCks.h[cno],RecL=110,Replace',internal,output
close #hTmp:
open #hPr=fnH: 'Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],NoShr',i,outIn,k
do
	read #hPr,using 'form pos 271,2*N 5': n1,n2 eof Finis
	rewrite #hPr,using 'form pos 271,2*N 5': 0,0
loop

Finis: !
	close #hPr:
goto Xit

Xit: fnXit
include: ertn No

