on error goto Ertn
fn_setup
fnTop(program$)
fnPremierCardiologyImport('direct')
goto Xit


def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library.br': fnTop
		library 'S:\Core\Library.br': fnXit
		library 'S:\Collection-Master Add-On\fn\Library.br': fnPremierCardiologyImport
	end if
fnend

Xit: !
fnXit
include: ertn
