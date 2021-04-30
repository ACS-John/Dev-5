! replace S:\acsPR\subCAT_SRCH.br
! search for an valid sub-caterories for this job
 
def library fnsubcat_srch(&cde$,&ckey;fixgrid)
		autoLibrary
		on error goto Ertn
 
		dim item$(2)*40,resp$(30)*80
 
		cde$=lpad$(rtrm$(cde$),3) ! pass back sub-category #
		fncno(cno)
		subcat=1 ! # of subcatergory file in calling program
 
		restore #subcat:
		fnTos(sn$="SubCatSrch")
		ch$(1)="Sub-Category #" : : _
		ch$(2)="Description" : : _
		mat ch$(2) : mat cm$(2) : mat cm$=("2")
		usefile=fnflexinit1('SubCat',1,1,20,40,mat ch$,mat cm$,1,usefile)
READ_FILE: !
		read #subcat,using 'Form POS 1,c 3,c 25': mat item$ eof L280 ioerr ERR_READ
		fnflexadd1(mat item$)
		goto READ_FILE
 
ERR_READ: !
		if err<>61 then goto ERTN
		pr 'Record locked during cat_search flexgrid creation' : _
		pr 'It was skipped' : _
		read #subcat,release: : _
		goto READ_FILE
 
L280: fnCmdKey("&Add",97,0,0,"Add a new sub-category record." ) : _
		fnCmdKey("E&dit",98,1,0,"Access the highlited record") : _
		fnCmdKey("&Delete",96,0,0,"Deletes the highlited record") : _
		fnCmdKey("&Listing",94,0,0,"Print a list of sub-category records") : _
		fnCmdKey("E&xit",5,0,1,"Returns to main menu.")
		ckey=fnAcs(mat resp$) : _
		! CALL FLEXGRID
		x$=cde$=lpad$(resp$(1),3)
		if ckey=5 then cde$="   " ! no one selected
		goto Xit
 
include: ertn
 
Xit: fnend
 
