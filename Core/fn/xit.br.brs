def library fnXit(;cursys$)
	autoLibrary

	if env$('CurSys')="GL" and fnProcess=1 then 
		fnChain("S:\acsGL\acglAuto")
	else if env$('CurSys')="PR" and fnProcess=1 then 
		fnChain("S:\acsPR\newprAuto")
	else if env$("xit_override")<>'' then 
		dim tmp$*1024
		tmp$=env$("xit_override")
		setenv("xit_override","")
		fnChain(tmp$)
	else if env$('CurSys')='CM' then 
		setenv('Session_Rows',24)
		setenv('Session_Cols',80)
		if env$('cursys')='CM' then
			if force80x24 then
				setenv('force80x24','Yes')
			else
				setenv('force80x24','')
			end if
		end if
		open #0: 'SRow=1,SCol=1,Rows=24,Cols=80,buttonrows=2,Font=MEDIUM:Width+',display,outIn
		execute 'Proc RUN'
	end if 
	fnChain('S:\Core\Menu.br',0,1)
fnend 
