! r: doNotInsert
pr 'this clip is not intended to be compiled directly nor run directly'
pr 'this clip replaces 'include: fn_open' when processed with lexi'

end
! /r doNotInsert
! ertn
Ertn: !
	library 'S:\Core\Library': fnerror
	fnerror(program$,err,line,act$,'xit')
	if lwrc$(act$)='pause' then 
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
execute act$ : goto Ertn
