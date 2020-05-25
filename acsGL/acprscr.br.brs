! Replace S:\acsGL\acprScr
! after fact payroll screens
 
	autoLibrary
 
	fnTop(program$,cap$="Payroll Screens")
	on error goto Ertn
 
	fnacprscr
	goto Xit
 
include: Ertn
Xit: fnXit
