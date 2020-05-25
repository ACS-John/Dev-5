! Replace S:\acsGL\acglAuto
! automatic processing - chain program
 
	autoLibrary
	on error goto Ertn
 
	dim prg$*35,a$*40,cnam$*40,ml$(3)*80
 
	fncno(cno)
L100: if fnprocess=0 then goto Xit
 
	pgnum=fnpgnum
	open #20: "Name=[Q]\GLmstr\ACGLPGMN.h[cno],Shr",internal,input,relative ioerr MSGBOX1 : _
	read #20,using 'Form POS 1,C 35,POS 71,N 3,x 1,2*N 1',rec=pgnum+=1: prg$,pn,ps,srq eof Xit,noRec Xit : _
	close #20:
	if rtrm$(prg$)="" then goto L220
	fnprg(prg$,put=2)
	fnpgnum(pgnum) : fnps(ps)
	goto CHAIN_PRG
MSGBOX1: !
	mat ml$(3) : _
	ml$(1)="The order for automatic processing has" : _
	ml$(2)="never been set for company # [cno]." : _
	ml$(3)="Click OK to skip this company." : _
	fnmsgbox(mat ml$,resp$,cap$,49)
	goto L220
 
L220: fnkillauto : fnpgnum(-1) : _
	! ! CHECK FOR ADDITIONAL COMPANIES
	open #glclnt=1: "Name=[Q]\GLmstr\glClnt.dat,NoShr",internal,outIn,relative ioerr Xit
	for j=2 to 20
		read #glclnt,using 'Form POS 1,N 5',rec=j: cno
		if cno<>0 then goto L300
	next j
	goto Xit
 
L300: fnputcno(cno) : fnprocess(process=1)
	rewrite #glclnt,using 'Form POS 1,N 5,C 40',rec=j: 0," "
	close #glclnt:
	goto L100
 
Xit: !
	fnFree("AutoPrn."&wsid$)
L370: fnXit
CHAIN_PRG: fnchain(prg$)
 
include: Ertn
