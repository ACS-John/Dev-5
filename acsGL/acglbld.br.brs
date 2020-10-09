! Replace S:\acsGL\acglBld
 
	autoLibrary
	on error goto Ertn
 
	fnTop(program$,"Build Screens")
	fnacglbld
 
Xit: fnXit
 
include: ertn
