! replace S:\acsPR\CAT_SRCH2.br
! search for an valid caterories for this job
 
def library fncat_srch2(&cn$,&ckey;fixgrid)
		autoLibrary
		on error goto Ertn
 
		dim item$(2)*40,resp$(30)*80
 
		jn$=lpad$(rtrm$(cn$),6) ! pass job over in category #, but pass back the category
		category=16 ! category file in jcmaint
 
		fnTos(sn$="CatSrch")
		ch$(1)="Job & Category": ch$(2)="Category Name" : : _
		mat ch$(2) : mat cm$(2) : mat cm$=("2")
		fnflexinit1('Cat',1,1,10,70,mat ch$,mat cm$,1,usefile)
		restore #category:
READ_FILE: !
		read #category,using 'Form POS 1,c 5,c 25': mat item$ eof L270 ioerr ERR_READ
		fnflexadd1(mat item$)
		goto READ_FILE
 
ERR_READ: !
		if err<>61 then goto ERTN
		pr 'Record locked during cat_search flexgrid creation' : _
		pr 'It was skipped' : _
		read #category,release: : _
		goto READ_FILE
 
L270: ! If FIXGRID=99 Then goto Xit ! FIXING NEW GRID FILE BEFORE LEAVING UBFM
		fnCmdKey("&Add",97,0,0,"Add a new category record." ) : _
		fnCmdKey("&Edit",98,0,0,"Review or change category breakdown record." ) : _
		fnCmdKey("Re&view Details",95,1,0,"Review detail transactions") : _
		fnCmdKey("&Delete",96,0,0,"Deletes the highlited record") : _
		fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information") : _
		fnCmdKey("D&uplicate",12,0,1,"Duplicates all Caterories from anouther existing job.") : _
		fnCmdKey("E&xit",6,0,1,"Returns to main screen.")
		fnAcs(mat resp$,ckey) : _
		! CALL FLEXGRID
		cn$=lpad$(resp$(1),11)
		if ckey=5 then cn$=cn$(1:6)&"     " ! no one selected
		goto Xit
 
 
Xit: fnend
include: ertn
