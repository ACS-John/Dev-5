! formerly S:\acsPR\Burden_Srch.br
! search for a personnel burden record

def library fnburden_srch(&x$;fixgrid)
	library 'S:\Core\Library': fnTos,fnflexinit1,fnflexadd1,fnAcs,fnCmdSet,fnerror,fngethandle,fnCmdKey
	on error goto ERTN

	dim item$(6)*30,resp$(30)*80

	! x$=account #     
	! to extract the flexgrid information (personnel burden)
	open #file_num:=fngethandle: "Name=[Q]\PRmstr\burden.h[cno],KFName=[Q]\PRmstr\burdenidx.h[cno],Shr",internal,input,keyed ioerr ERTN

	restore #file_num: 
	fnTos(sn$="BurdenSrch")
	ch$(1)="Employee" : ch$(2)="Name" : ch$(3)="Burden" 
	ch$(4)="Unused" 
	ch$(5)="Unused" 
	mat ch$(5) : mat cm$(5)
	fnflexinit1('BurdenSrch',1,1,10,70,mat ch$,mat cm$,1,usefile)
	if usefile>0 then goto L300 ! file already exists, do not recreate
	READ_FILE: ! 
	read #file_num,using 'Form POS 1,c 8,c 30,n 6.3': item$(1),item$(2),rate eof L300 ioerr ERR_READ
	item$(3)=cnvrt$("pic(zzz.###)",rate)
	fnflexadd1(mat item$)
	goto READ_FILE

ERR_READ: ! 
		if err<>61 then goto ERTN
		pr 'Record locked during burden_search flexgrid creation' 
		pr 'It was skipped' 
		read #file_num,release: 
		goto READ_FILE

	L300: !
	if fixgrid=99 then goto XIT ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
	fnCmdKey("&Edit",2,1,0,"Allows you to change the highlighted record.")
	fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
	fnAcs(sn$,0,mat resp$,ckey)           ! CALL FLEXGRID
	x$=lpad$(resp$(1)(1:8),8)
	if ckey=5 then x$="        " ! no one selected
	goto XIT

	XIT: !
	close #file_num: 
fnend
include: ertn
