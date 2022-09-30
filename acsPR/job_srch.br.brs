! replace S:\acsPR\Job_Srch.br
! search for an job numbers
!
def library fnjob_srch(&jn$;fixgrid)
	! jn$=Job #     
	! to extract the flexgrid information (master file)
	autoLibrary
	on error goto Ertn

	dim item$(4)*40
	dim resp$(30)*80

	open #h=fnH: "Name=[Q]\PRmstr\Jcmstr.h[cno],KFName=[Q]\PRmstr\jcIndx.h[cno],Shr",i,i,k ioerr ERTN

	restore #h: 
	fnTos
	ch$(1)="Job #" 
	ch$(2)="Job Name" 
	ch$(3)="Address" 
	ch$(4)="City, ST Zip" 
	mat ch$(4) : mat cm$(4) : mat cm$=("4")
	if fixgrid=99 then usefile=0 else usefile=1 ! set to rebuild grid file only as you exit program and the   fixgrid code has been changed to necessary
	usefile=fnFlexInit1('Job',1,1,10,70,mat ch$,mat cm$,1,usefile)
	if usefile>0 then goto L280 ! file already exists, do not recreate
	READ_FILE: ! 
		read #h,using 'form pos 1,c 6,c 40,c 30,x 30,c 30': mat item$ eof L280 ioerr ERR_READ
		fnFlexAdd1(mat item$)
	goto READ_FILE

	ERR_READ: ! 
		if err<>61 then goto ERTN
		pr 'Record locked during job_search flexgrid creation' 
		pr 'It was skipped' 
		read #h,release: 
	goto READ_FILE

	L280: !
		if fixgrid=99 then goto Xit ! FIXING NEW GRID FILE BEFORE LEAVING job files
		fnCmdSet(2)
		ckey=fnAcs(mat resp$) 
		jn$=lpad$(resp$(1),6)
		if ckey=5 then jn$="      " ! no one selected
	goto Xit
	Xit: !
	close #h:
fnend 
include: ertn
