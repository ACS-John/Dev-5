def library fnSsEmployee
	fnSsEmployee=fn_setupFicaRate
fnend
def library fnSsEmployer
	fnSsEmployer=fn_setupFicaRate
fnend
def fn_setupFicaRate(; ___,hCompany)
	! retains: ficarate and setup_ficaRate
	if ~setup_ficaRate=val(env$('cno')) or ~ficarate then
		setup_ficaRate=val(env$('cno'))
		autoLibrary
		open #hCompany=fnH: 'Name=[Q]\PRmstr\Company.h[cno]',i,i 
		read #hCompany,using 'form pos 236,pd 3.3': ficarate
		close #hCompany: 
		if ficarate=0 then ficarate=6.2
	end if
	fn_setupFicaRate=ficarate
fnend
