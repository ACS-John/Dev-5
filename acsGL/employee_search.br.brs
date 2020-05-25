! replace S:\acsGL\Employee_Search.br
! search for employees in after fact payroll

def library fnemployee_search(&x$;fixgrid)
		! x$=employee #
		! to extract the flexgrid information (master file)
		autoLibrary
		on error goto Ertn

		dim item$(5)*30
		dim resp$(30)*80
	open #file_num:=fngethandle: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRINDEX.h[cno],Shr",internal,input,keyed ioerr ERTN

	restore #file_num:
	fnTos
	ch$(1)="Emp #"
	ch$(2)="Name"
	ch$(3)="Address"
	ch$(4)="City, ST Zip"
	ch$(5)="Social Security"
	mat ch$(5) : mat cm$(5) : mat cm$=("5")
	mat cm$=(""): cm$(1)="" ! "n 4"
	if fixgrid=99 then usefile=0 else usefile=1
		! set to rebuild grid file only as you exit ubfm and the
		! fixgrid code has been changed to necessary
	usefile=fnflexinit1('Employee',1,1,10,70,mat ch$,mat cm$,1,usefile)
	if usefile>0 then goto L280 ! file already exists, do not recreate
	READ_FILE: !
		read #file_num,using 'Form POS 1,c 4,3*c 25,c 11': mat item$ eof L280 ioerr ERR_READ
		fnflexadd1(mat item$)
	goto READ_FILE

	ERR_READ: !
		if err<>61 then goto ERTN
		pr 'Record locked during Customer_Search flexgrid creation'
		pr 'It was skipped'
		read #file_num,release:
	goto READ_FILE

	L280: !
		if fixgrid=99 then goto Xit ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
		fnLbl(12,1,"")
		fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
		! CALL FLEXGRID
		x$=lpad$(resp$(1),4)
		if ckey=5 then x$="    " ! no one selected
	goto Xit
	Xit: !
	close #file_num:
fnend
include: Ertn

