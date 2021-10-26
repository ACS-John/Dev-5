autoLibrary
fnTop(program$)
on error goto Ertn
dim s1$(0)*128,s1N(0)
hi=fn_openFio('CO Systems',mat s1$,mat s1N, 1)
dim s2$(0)*128,s2N(0)
ho=fn_openFio('CO Systems 2',mat s2$,mat s2N)
do
	read #hi,using form$(hi): mat s1$,mat s1N eof EoH

	mat s2$=('')
	mat s2N=(0)
	s2$(sys_id     )=trim$(s1$(sys_id     ))
	s2$(sys_name   )=trim$(s1$(sys_name   ))
	s2N(sys_isChild)=s1N(sys_isAddOn)
	if s1N(sys_isAddOn) then
		s2$(sys_parent )='(Yes-Pick)'
	end if
	write #ho,using form$(ho): mat s2$,mat s2N

loop

EoH: !
	close #hi:
	close #ho:
goto Xit

Xit: !
	pr 'complete.' : end 
fnXit

include: fn_open
include: ertn