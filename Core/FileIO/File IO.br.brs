library 'S:\Core\Library.br': fnTop,fnXit,fnGetHandle
fnTop(program$)
open  #hProc:=fnGetHandle: 'name=fileioproc'&session$&'.$$$,replace',display,output
! open  #hProc:=fnGetHandle: 'name='&env$('temp')&'\fileioproc'&session$&'.$$$,replace',display,output
pr #hProc: 'load S:\Core\FileIO\fileio.br'
pr #hProc: 'cd S:'
pr #hProc: 'Run'
pr #hProc: 'cd '&program$(1:2)
if env$('cursys')='CM' then
	pr #hProc: 'Proc Run'
else
	pr #hProc: 'load S:\Core\Menu.br'
end if
pr #hProc: 'run'
close #hProc:
! execute 'proc '&env$('temp')&'\fileioproc'&session$&'.$$$'
execute 'proc fileioproc'&session$&'.$$$'