! Replace S:\acsPR\newprAuto
! automatic processing - chain program

autoLibrary
on error goto Ertn

cno=val(env$('cno'))
goto ProcessCompany

ProcessCompany: !
	if fnProcess=0 then goto Xit

	pgnum=fnPgNum
	if ~pgnum then open #prclnt=1: "Name=[Q]\PRmstr\PrClnt.dat,NoShr",i,outi,r
		read #prclnt,using 'form pos 46,3*N 1',rec=1: w,m,q
		close #prclnt:
	L140: !
	open #20: "Name=[Q]\PRmstr\newPrPGMN.h[cno],Shr",i,i,r ioerr MSGBOX1
	dim prg$*20
	read #20,using 'form pos 1,C 20,X 35,3*N 1',rec=pgnum+=1: prg$,wk,mo,qt eof Xit,noRec Xit
	close #20:
	if w=1 and wk<>1 then goto L140 ! WEEKLY PERIOD NOT SELECTED
	if m=1 and mo<>1 then goto L140 ! MONTHY PERIOD NOT SELECTED
	if q=1 and qt<>1 then goto L140 ! QUARTER PERIOD NOT SELECTED
	if rtrm$(prg$)="" then goto L260
	fnSetCoreProgramCurrent(prg$,put=2)
	fnPgNum(pgnum) : fnPs(ps)
fnChain(prg$)

MSGBOX1: ! r:
	dim ml$(3)*80
	mat ml$(3)
	ml$(1)="The order for automatic processing has"
	ml$(2)="never been set for company # "&env$('cno')&"."
	ml$(3)="Click OK to skip this company."
	fnMsgBox(mat ml$,resp$,cap$,49)
goto L260 ! /r

L260: ! r:
	fnkillauto : fnPgNum(-1)
	! ! CHECK FOR ADDITIONAL COMPANIES
	open #prclnt=1: "Name=[Q]\PRmstr\PrClnt.dat,NoShr",i,outi,r ioerr Xit
	for j=2 to 20
		read #prclnt,using 'form pos 1,N 5,pos 46,3*N 1',rec=j: cno,w,m,q
		if cno then 
			cno=fnPutCno(cno) : fnProcess(process=1)
			rewrite #prclnt,using 'form pos 1,N 5,C 40,3*N 1',rec=j: 0," ",0,0,0
			close #prclnt:
			goto ProcessCompany
		end if
	next j
goto Xit ! /r

Xit: !
	fnFree('AutoPrn.[wsid]')
fnXit

include: ertn
