! Replace S:\acsGL\acglAuto
! automatic processing - chain program

autoLibrary
on error goto Ertn

ProcessCompany: ! r:
	if fnProcess=0 then goto Xit

	pgNum=fnPgNum
	open #hOrder=fnH: 'Name=[Q]\GLmstr\ACGLPGMN.h[cno],Shr',i,i,r ioerr SkipOut
	dim prg$*35
	read #hOrder,using 'form pos 1,C 35,pos 71,N 3,x 1,2*N 1',rec=pgNum+=1: prg$,pn,ps,srq eof Xit,noRec Xit
	close #hOrder:
	if rtrm$(prg$)<>'' then
		fnSetCoreProgramCurrent(prg$,put=2)
		fnPgNum(pgNum) : fnPs(ps)
		fnChain(prg$)
	else
		goto Finis
	end if
! /r
SkipOut: ! r:
	dim ml$(3)*80
	mat ml$(3)
	ml$(1)='The order for automatic processing has'
	ml$(2)='never been set for company '&str$(cno)&'.'
	ml$(3)='Click OK to skip this company.'
	fnMsgBox(mat ml$,resp$,cap$,49)
goto Finis ! /r

Finis: ! r:
	fnkillauto : fnPgNum(-1)
	! ! CHECK FOR ADDITIONAL COMPANIES
	open #glclnt=1: 'Name=[Q]\GLmstr\glClnt.dat,NoShr',i,outi,r ioerr Xit
	for j=2 to 20
		read #glclnt,using 'form pos 1,N 5',rec=j: cno
		if cno<>0 then
			cno=fnPutCno(cno) : fnProcess(process=1)
			rewrite #glclnt,using 'form pos 1,N 5,C 40',rec=j: 0,' '
			close #glclnt:
			goto ProcessCompany
		end if
	next j
goto Xit ! /r

Xit: ! r:
	fnFree('AutoPrn.'&wsid$)
fnXit ! /r

include: ertn
