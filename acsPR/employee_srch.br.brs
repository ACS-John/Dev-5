! replace S:\acsPR\Employee_Srch.br
! search for an employee

def library fnemployee_srch(&x$; fixgrid)
	! x$=account #     
	! to extract the flexgrid information (master file)
	autoLibrary
	on error goto Ertn
	dim item$(6)*30
	dim resp$(30)*80

	open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k ioerr ERTN

	restore #hEmployee: 
	fnTos
	ch$(1)="Employee"
	ch$(2)="Name"
	ch$(3)="Address" 
	ch$(4)="City, ST Zip" 
	ch$(5)="SS Number"
	ch$(6)="Phone" 
	mat ch$(6) : mat cm$(6)
	usefile=0 ! if fixgrid=99 then usefile=0 else usefile=1 
	! set to rebuild grid file only as you exit prfm and the 
	! fixgrid code has been changed to necessary
	usefile=fnflexinit1('Employee',1,1,10,70,mat ch$,mat cm$,1,usefile)
	if usefile>0 then goto L280 ! file already exists, do not recreate
	READ_FILE: ! 
		read #hEmployee,using 'form pos 1,c 8,3*c 30,pos 99,c 11,pos 179,c 12': mat item$ eof L280 ioerr ERR_READ
		fnflexadd1(mat item$)
	goto READ_FILE

	ERR_READ: ! 
		if err<>61 then goto ERTN
		pr 'Record locked during employee_search flexgrid creation'
		read #hEmployee,release: 
	goto READ_FILE

	L280: !
	if fixgrid=99 then goto Xit ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	x$=lpad$(resp$(1)(1:8),8)
	if ckey=5 then x$="        " ! no one selected
goto Xit
Xit: close #hEmployee: : fnend 
include: ertn
