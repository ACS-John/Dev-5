def library fnJobSearch(&jobNumber$; fixgrid,___,h,usefile)
	! search for a job numbers
	autoLibrary
	on error goto Ertn

	open #h=fnH: 'Name=[Q]\PRmstr\Jcmstr.h[cno],KFName=[Q]\PRmstr\jcIndx.h[cno],Shr',i,i,k ioerr ERTN
	restore #h:

	fnTos
	ch$(1)='Job #'
	ch$(2)='Job Name'
	ch$(3)='Address'
	ch$(4)='City, ST Zip'
	mat ch$(4) : mat cm$(4) : mat cm$=('4')
	dim item$(4)*40
	if fixgrid<>99 then usefile=1 ! set to rebuild grid file only as you exit program and the   fixgrid code has been changed to necessary
	usefile=fnFlexInit1('Job',1,1,10,70,mat ch$,mat cm$,1,usefile)
	if usefile>0 then
		goto EoFile ! file already exists, do not recreate
	else
		! r: read #h, fnFlexAdd1 loop, skips locked
		ReadFile: !
			read #h,using 'form pos 1,c 6,c 40,c 30,x 30,c 30': mat item$ eof EoFile ioerr ReadErr
			fnFlexAdd1(mat item$)
		goto ReadFile
			ReadErr: !
				if err<>61 then goto ERTN
				pr 'Record locked during job_search flexgrid creation'
				pr 'It was skipped'
				read #h,release:
			goto ReadFile
		! /r

	end if
	EoFile: !

		if fixgrid<>99 then
			! fixing new grid file before leaving job files
			fnCmdSet(2)
			dim resp$(30)*80
			ckey=fnAcs(mat resp$)
			jobNumber$=lpad$(resp$(1),6)
			if ckey=5 then jobNumber$='      ' ! no one selected
		end if
	goto Xit
	Xit: !
	close #h:
fnend
include: ertn
