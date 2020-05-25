! Replace S:\acsPR\hours.br
! access breakdown of hours by diffenent classifications
 
	autoLibrary
	on error goto Ertn
 
	fnTop("S:\acsPR\hours","Breakdown of Hours")
	fnhours(eno)
	goto Xit
 
! <Updateable Region: ERTN>
ERTN: fnerror(program$,err,line,act$,"NO")
	if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
	execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
! /region
 
Xit: fnXit
 
