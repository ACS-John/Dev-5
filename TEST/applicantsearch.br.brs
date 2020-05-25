! Replace Test\ApplicantSearch
! def library fnAPPLLICANT_SEARCH(&X$,FILE_NUM;FIXGRID)
	autoLibrary
	fnTop(program$,"Test Applicant Search")
! ____________-
	open #1: "Name=[Q]\EAmstr\Applicant.h1,KFName=[Q]\EAmstr\AppIdx1.h1,Shr",internal,outIn,keyed
	fnapplicant_search(x$,1,1)
! third paramater - 0=don't rebuild, use if possible : _
	!                   1=rebuild : _
	!                   99=rebuild it, but don't show it
	pr x$
 
	stop
