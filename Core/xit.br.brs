! Replace S:\Core\Xit.br
! ______________________________________________________________________
	def library fnxit(;cursys$)
		library 'S:\Core\Library': fnchain,fnprocess
		on error goto ERTN
! ______________________________________________________________________
		if env$('CurSys')="GL" and fnprocess=1 then 
			fnchain("S:\acsGL\acglAuto")
		else if env$('CurSys')="PR" and fnprocess=1 then 
			fnchain("S:\acsPR\newprAuto")
		else if env$("xit_override")<>'' then 
			dim tmp$*1024
			tmp$=env$("xit_override")
			setenv("xit_override","")
			fnchain(tmp$)
		else if env$('CurSys')='CM' then 
			execute 'Proc RUN'
		end if 
		fnchain('S:\Core\Menu.br',0,1)
XIT: fnend 
include: ertn