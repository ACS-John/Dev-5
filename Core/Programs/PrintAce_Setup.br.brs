autoLibrary
on error goto Ertn
fnTop(program$,'PrintAce Install Dependencies')
execute 'Sy -w '&os_filename$('S:\Core\ACS_PrAce_Support_Install_ocx.exe')
execute 'Sy '&os_filename$('S:\Core\ACS_PrAce_Reg.cmd')&' /s'
Xit: fnXit
include: ertn
