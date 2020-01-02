! r: doNotInclude
pr 'this clip is not intended to be compiled directly nor run directly'
pr 'this clip replaces 'include: fn_open' when processed with lexi'

end
! /r doNotInclude
! ertn
Ertn: !
	library 'S:\Core\Library': fnerror
	dim ertnAct$*256
	fnerror(program$,err,line,ertnAct$,'xit')
	if lwrc$(ertnAct$)='pause' then
		if env$('ACSDeveloper')<>'' then
			execute 'List '&str$(line)
		else
			pr 'PROGRAM PAUSE: Type GO and press [Enter] to continue.'
			pr ''
		en if
		pause
	en if
	goto Ertn_executeAct
Ertn_executeAct: !
	execute ertnAct$
goto Ertn
