autoLibrary
if env$('acsdeveloper')<>'' then 
	setenv('CM_Core_Initialized','')  !  forces File IO Layouts to update without restarting
end if
fnTop(program$)
open  #hProc:=fnH: 'name=fileioproc'&session$&'.$$$,replace',display,output
pr #hProc: 'load S:\Core\FileIO\fileio.br'
pr #hProc: 'cd S:'
pr #hProc: 'Run'
pr #hProc: 'cd '&program$(1:2)
if env$('cursys')='CM' then
	pr #hProc: 'load S:\Core\Xit.br'
	pr #hProc: 'run'
elsesy
	pr #hProc: 'load S:\Core\Menu.br'
	pr #hProc: 'run'
end if
close #hProc:
execute 'proc fileioproc'&session$&'.$$$'