! Replace S:\acsGL\acglBldS
! this program calls fnacglblds to builds the file    [Q]\GLmstr\ACGLScr.h
 
	autoLibrary
	on error goto Ertn
 
	dim flo$(31),fli$(65),scr$(30)*20,otd$(65)*30,d(2)
 
	fnTop(program$,"Build Screens")
	fnacglblds
	goto Xit
 
ERTN: ! <Updateable Region: ERTN>
	fnerror(program$,err,line,act$,"Xit")
	if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
 
Xit: fnXit
 
