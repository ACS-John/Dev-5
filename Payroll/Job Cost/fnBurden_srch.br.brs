! formerly S:\acsPR\Burden_Srch.br
! search for a personnel burden record

def library fnburden_srch(&x$;fixgrid)
	autoLibrary
	on error goto Ertn

	dim item$(6)*30,resp$(30)*80

	! x$=account #     
	! to extract the flexgrid information (personnel burden)
	open #file_num=fnH: "Name=[Q]\PRmstr\burden.h[cno],KFName=[Q]\PRmstr\burdenidx.h[cno],Shr",i,i,k ioerr ERTN

	restore #file_num: 
	fnTos(sn$="BurdenSrch")
	ch$(1)="Employee" : ch$(2)="Name" : ch$(3)="Burden" 
	ch$(4)="Unused" 
	ch$(5)="Unused" 
	mat ch$(5) : mat cm$(5)
	fnFlexInit1('BurdenSrch',1,1,10,70,mat ch$,mat cm$,1,usefile)
	if usefile>0 then goto L300 ! file already exists, do not recreate
	READ_FILE: ! 
	read #file_num,using 'form pos 1,c 8,c 30,n 6.3': item$(1),item$(2),rate eof L300 ioerr ERR_READ
	item$(3)=cnvrt$("pic(zzz.###)",rate)
	fnFlexAdd1(mat item$)
	goto READ_FILE

ERR_READ: ! 
		if err<>61 then goto ERTN
		pr 'Record locked during burden_search flexgrid creation' 
		pr 'It was skipped' 
		read #file_num,release: 
		goto READ_FILE

	L300: !
	if fixgrid=99 then goto Xit ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
	fnCmdKey("&Edit",2,1,0,"Allows you to change the highlighted record.")
	fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
	ckey=fnAcs(mat resp$)           ! CALL FLEXGRID
	x$=lpad$(resp$(1)(1:8),8)
	if ckey=5 then x$="        " ! no one selected
	goto Xit

	Xit: !
	close #file_num: 
fnend
include: ertn
