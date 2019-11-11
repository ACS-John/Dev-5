library 'S:\Core\Library.br': fnTop,fnXit,fnGetHandle
if env$('acsdeveloper')<>'' then 
	setenv('CM_Core_Initialized','')  !  forces File IO Layouts to update without restarting
end if
fnTop(program$)
open  #hProc:=fnGetHandle: 'name=fileioproc'&session$&'.$$$,replace',display,output
! open  #hProc:=fnGetHandle: 'name='&env$('temp')&'\fileioproc'&session$&'.$$$,replace',display,output
pr #hProc: 'load S:\Core\FileIO\fileio.br'
pr #hProc: 'cd S:'
pr #hProc: 'Run'
pr #hProc: 'cd '&program$(1:2)
if env$('cursys')='CM' then
	pr #hProc: 'load S:\Core\xit.br'
	pr #hProc: 'run'
elsesy
	pr #hProc: 'load S:\Core\Menu.br'
	pr #hProc: 'run'
end if
close #hProc:
! execute 'proc '&env$('temp')&'\fileioproc'&session$&'.$$$'
execute 'proc fileioproc'&session$&'.$$$'