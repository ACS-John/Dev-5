! search for general ledger accounts

def library fnGlAccountSearch(&account$ ) ! ; fixgrid)
	autoLibrary
	on error goto Ertn
	open #hAccount=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndex.h[cno],Shr",internal,input,keyed ioerr ERTN
	! restore #hAccount:
	dim resp$(30)*80
	fnTos
	mat ch$(9)
	ch$(1)="Account" 	: ch$(2)="Description" 	: ch$(3)="Balance"
	ch$(4)="B/S Ref" 	: ch$(5)="B/S Ref2"    	: ch$(6)="I/C Ref"
	ch$(7)="I/C Ref2"	: ch$(8)="Fund Ref"    	: ch$(9)="Fund Ref2"
	mat cm$(9)
	cm$(1)=cm$(2)="80": cm$(3)="10"
	cm$(4)=cm$(5)=cm$(6)=cm$(7)=cm$(8)=cm$(9)="30"
	! if fixgrid=99 then usefile=0 else usefile=1 ! set to rebuild grid file only as you exit ubfm and the    fixgrid code has been changed to necessary
	! usefile=fnflexinit1('Acct',1,1,10,70,mat ch$,mat cm$,1,usefile)
	fnflexinit1('Acct',1,1,10,70,mat ch$,mat cm$,1)
	! if usefile>0 then goto EoAccount ! file already exists, do not recreate
	do
		dim item$(10)*50
		ReadAccount: !
		dim rf(6)
		read #hAccount,using 'Form POS 1,C 12,c 50,pos 87,pd 6.2,pos 63,6*pd 3': item$(1),item$(2),cb,mat rf eof EoAccount ioerr ERR_READ
		item$(3)=str$(cb)
		for j=1 to 6
			item$(j+3)=str$(rf(j))
		next j
		fnflexadd1(mat item$)
	loop
	EoAccount: ! If FIXGRID=99 Then goto Xit ! FIXING NEW GRID FILE without displaying it
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)

	account$=lpad$(resp$(1),12)
	if ckey=5 then account$="            " ! no one selected
	goto Xit

	ERR_READ: ! r:
		if err<>61 then goto Ertn
		pr bell;'Record locked during Account_Search flexgrid creation'
		pr 'It was skipped'
		read #hAccount,release:
	goto ReadAccount ! /r

Xit: close #hAccount: : fnend

include: ertn
