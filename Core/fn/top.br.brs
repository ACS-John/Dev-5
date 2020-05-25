def library fnTop(; prg$*256,cap$*128,force80x24)
	!	 top of (every) program function
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn
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
		if exists('S:\ScreenIO.ini') and ~exists('ScreenIO.ini') then
				fnCopy('S:\ScreenIO.ini','ScreenIO.ini')
		end if
	end if
	if env$('cursys')='CM' then
		execute 'config OPTION 48 OFF' ! make ENTER key return fkey value instead of zero
		if force80x24 then
			setenv('force80x24','Yes')
		else
			setenv('force80x24','')
		end if
		if env$('CM_Core_Initialized')='' then
			setenv('CM_Core_Initialized','Yes')
			! fnapply_theme
			fnAcsSystemInitialize(2)
		end if
	end if
fnend
include: Ertn
