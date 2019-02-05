def library fntop(; prg$*256,cap$*128)
	!	 top of (every) program function
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnAcsSystemInitialize
		library 'S:\Core\Library': fnprg
		library 'S:\Core\Library': fnCopy
		library 'S:\Core\Library': fnBookTitle$
		on error goto ERTN
	end if
	pr newpage
	cap$=trim$(cap$)
	prg$=trim$(prg$)
	if cap$='' and prg$<>'' then
		cap$=prg$(pos(prg$,'\',-1)+1:pos(prg$,'.',-1)-1)
	else if cap$='' then
		cap$="(untitled)"
	end if
	if prg$='' then
		prg$="(unknown)"
		if env$('acsDeveloper')<>'' then pr 'fnTop called but program was not set.' : pause
	end if
	fnprg(prg$,put=2)
	if cap$=uprc$(cap$) then
		setenv('Program_Caption',fnBookTitle$(cap$))
	else
		setenv('Program_Caption',cap$)
	end if
	if env$('acsDeveloper')='' then
		if exists('S:\FileIO.ini') and ~exists('FileIO.ini') then
			fnCopy('S:\FileIO.ini','FileIO.ini')
		end if
		! if ~exists('ScreenIO.ini') then
		! 		fnCopy('S:\ScreenIO.ini','ScreenIO.ini')
		! end if
	end if
	if env$('cursys')='CM' and env$('CM_Core_Initialized')='' then
		setenv('CM_Core_Initialized','Yes')
		! library 'S:\Core\Library': fnapply_theme
		! fnapply_theme
		fnAcsSystemInitialize(2)
	end if
fnend
include: ertn
