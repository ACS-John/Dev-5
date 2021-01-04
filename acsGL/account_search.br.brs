! replace S:\acsGL\Account_Search.br
! search for general ledger accounts
 
def library fnaccount_search(&x$;fixgrid)
		autoLibrary
		on error goto Ertn
 
		dim item$(10)*50,resp$(30)*80,rf(6)
 
! x$=account #     : _
		! to extract the flexgrid information (master file)
		fncno(cno)
		open #file_num=fnH: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndex.h[cno],Shr",internal,input,keyed ioerr ERTN
 
		restore #file_num:
		fnTos(sn$="AccountSrch")
		ch$(1)="Account" : ch$(2)="Description" : ch$(3)="Balance" : _
		ch$(4)="B/S Ref" : ch$(5)="B/S Ref2" : _
		ch$(6)="I/C Ref" : ch$(7)="I/C Ref2" : _
		ch$(8)="Fund Ref" : ch$(9)="Fund Ref2" : _
		mat ch$(9) : mat cm$(9) : mat cm$(9) : _
		cm$(1)=cm$(2)="80": cm$(3)="10" : _
		cm$(4)=cm$(5)=cm$(6)=cm$(7)=cm$(8)=cm$(9)="30"
		if fixgrid=99 then usefile=0 else usefile=1 : _
			! set to rebuild grid file only as you exit ubfm and the : _
			! fixgrid code has been changed to necessary
		usefile=fnflexinit1('Acct',1,1,10,70,mat ch$,mat cm$,1,usefile)
		if usefile>0 then goto L330 ! file already exists, do not recreate
READ_FILE: !
		read #file_num,using 'Form POS 1,C 12,c 50,pos 87,pd 6.2,pos 63,6*pd 3': item$(1),item$(2),cb,mat rf eof L330 ioerr ERR_READ
		item$(3)=str$(cb)
		for j=1 to 6
			item$(j+3)=str$(rf(j))
		next j
		fnflexadd1(mat item$)
		goto READ_FILE
 
ERR_READ: !
		if err<>61 then goto ERTN
		pr 'Record locked during Account_Search flexgrid creation' : _
		pr 'It was skipped' : _
		read #file_num,release: : _
		goto READ_FILE
 
L330: ! If FIXGRID=99 Then goto Xit ! FIXING NEW GRID FILE without displaying it
		fnCmdSet(2): fnAcs(mat resp$,ckey) : _
		! CALL FLEXGRID
		x$=lpad$(resp$(1),12)
		if ckey=5 then x$="            " ! no one selected
		goto Xit
 
include: ertn
 
Xit: close #file_num: : fnend
 
