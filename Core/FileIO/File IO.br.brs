autoLibrary
if env$('acsdeveloper')<>'' then 
	setenv('CM_Core_Initialized','')  !  forces File IO Layouts to update without restarting
end if
fnTop(program$)
dim procFile$*256
procFile$='fileioproc[session].$$$'
open  #hProc=fnH: 'name='&procFile$&',replace',d,o
pr #hProc: 'load S:\Core\FileIO\fileio.br'
pr #hProc: 'cd S:'
pr #hProc: 'Run'
pr #hProc: 'cd '&program$(1:2)
if env$('cursys')='CM' then
	pr #hProc: 'load S:\Core\Xit.br'
	pr #hProc: 'run'
else
	pr #hProc: 'load S:\Core\Menu.br'
	pr #hProc: 'run'
end if
close #hProc:
execute 'proc '&procFile$