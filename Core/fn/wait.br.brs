! Replace S:\Core\Wait.br
 
def library fnwait(message$*40,stopable)
! if stopable=1 will display "Cancel (F5)" button
! win = window number
 
		autoLibrary
		on error goto Ertn
		if trim$(message$)="" then message$="Please wait..."
		fnStatus(message$)
! if stopable=1 then
!   "Cancel (F5)"
! else
!   "Do Not Stop"
! end if
		goto Xit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
		if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
		execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
		pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
ERTN_EXEC_ACT: execute act$ : goto ERTN
 
Xit: fnend
 
