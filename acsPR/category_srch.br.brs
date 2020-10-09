! replace S:\acsPR\Category_Srch.br
! search for a Category record
 
def library fncategory_srch(&cn$;fixgrid)
		autoLibrary
		on error goto Ertn
 
		dim item$(2)*30,resp$(30)*80
 
! cn$=account #     ! to extract the flexgrid information (Cagegory)
		fncno(cno)
		open #file_num:=fnH: "Name=[Q]\PRmstr\Category.h[cno],KFName=[Q]\PRmstr\Categoryidx.h[cno],Shr",internal,input,keyed ioerr ERTN
 
		restore #file_num:
		fnTos(sn$="CategorySrch")
		ch$(1)="Category" : ch$(2)="Name"
		mat ch$(2) : mat cm$(2) : cm$(1)="30"
		fnflexinit1('CategorySrch',1,1,10,70,mat ch$,mat cm$,1,usefile)
		if usefile>0 then goto L300 ! file already exists, do not recreate
		READ_FILE: !
		read #file_num,using 'Form POS 1,c 5,c 30': item$(1),item$(2) eof L300 ioerr ERR_READ
		fnflexadd1(mat item$)
		goto READ_FILE
 
ERR_READ: ! r:
		if err<>61 then goto ERTN
		pr 'Record locked during Category_search flexgrid creation'
		pr 'It was skipped'
		read #file_num,release:
	goto READ_FILE ! /r
	L300: ! r:
		if fixgrid=99 then goto Xit ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
		fnCmdKey("&Next",2,1,0,"Allows you to select the highlighted record.")
		fnCmdKey("E&Xit",5,0,1,"Returns to main screen.")
		fnAcs(mat resp$,ckey) ! CALL FLEXGRID
		cn$=lpad$(resp$(1)(1:5),5)
		if ckey=5 then cn$="     " ! no one selected
	goto Xit ! /r
include: ertn
Xit: close #file_num: : fnend
