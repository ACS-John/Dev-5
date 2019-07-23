fn_setup
fntop(program$)
on error goto Ertn
Fnsel(80,"Select Test Destination")
! fnSel(1024, 'Select Output for '&env$('cap') ,255, 'Cancel','HTML',env$('cap'))
if fkey=93 or fkey=99 then goto Xit
pr #255: env$('program_caption')
fnClose
Xit: fnXit
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library.br': fnXit
		library 'S:\Core\Library.br': fnTop
		gosub Enum
		gosub SetupPrint
	end if
fnend
include: cm\enum\common
include: cm\enum\master
include: cm\enum\debtor
include: cm\err
include: cm\print