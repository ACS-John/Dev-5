! Replace S:\acsPR\newprAuto
! automatic processing - chain program
 
	autoLibrary
	on error goto Ertn
 
	dim prg$*20,a$*40,cnam$*40,ml$(3)*80
 
	fncno(cno)
L100: if fnprocess=0 then goto Xit
 
	pgnum=fnpgnum
	if pgnum=0 then open #prclnt=1: "Name=[Q]\PRmstr\PrClnt.dat,NoShr",i,outi,r  : _
		read #prclnt,using 'form pos 46,3*N 1',rec=1: w,m,q : _
		close #prclnt:
L140: open #20: "Name=[Q]\PRmstr\newPrPGMN.h[cno],Shr",i,i,r ioerr MSGBOX1 : _
	read #20,using 'form pos 1,C 20,X 35,3*N 1',rec=pgnum+=1: prg$,wk,mo,qt eof Xit,noRec Xit : _
	close #20:
	if w=1 and wk<>1 then goto L140 ! WEEKLY PERIOD NOT SELECTED
	if m=1 and mo<>1 then goto L140 ! MONTHY PERIOD NOT SELECTED
	if q=1 and qt<>1 then goto L140 ! QUARTER PERIOD NOT SELECTED
	if rtrm$(prg$)="" then goto L260
	fnSetCoreProgramCurrent(prg$,put=2)
	fnpgnum(pgnum) : fnps(ps)
	goto CHAIN_PRG
MSGBOX1: !
	mat ml$(3) : _
	ml$(1)="The order for automatic processing has" : _
	ml$(2)="never been set for company # [cno]." : _
	ml$(3)="Click OK to skip this company." : _
	fnMsgBox(mat ml$,resp$,cap$,49)
	goto L260
 
L260: fnkillauto : fnpgnum(-1) : _
	! ! CHECK FOR ADDITIONAL COMPANIES
	open #prclnt=1: "Name=[Q]\PRmstr\PrClnt.dat,NoShr",i,outi,r ioerr Xit
	for j=2 to 20
		read #prclnt,using 'form pos 1,N 5,pos 46,3*N 1',rec=j: cno,w,m,q
		if cno<>0 then goto L340
	next j
	goto Xit
 
L340: fnputcno(cno) : fnprocess(process=1)
	rewrite #prclnt,using 'form pos 1,N 5,C 40,3*N 1',rec=j: 0," ",0,0,0
	close #prclnt:
	goto L100
 
Xit: !
	execute "Free AutoPrn."&wsid$&" -n" ioerr L410
L410: fnXit
CHAIN_PRG: fnchain(prg$)
 
include: ertn
