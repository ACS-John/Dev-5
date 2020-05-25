def library fnss_employee
	cno=val(env$('cno'))
	if ss_empee_setup<>cno then 
		autoLibrary
		open #h_company:=fngethandle: "Name=[Q]\PRmstr\Company.h[cno]",internal,input 
		read #h_company,using 'form pos 236,pd 3.3': ficarate
		close #h_company: 
		if ficarate=0 then ficarate=6.2
	end if
	fnss_employee=ficarate
fnend
def library fnss_employer
	cno=val(env$('cno'))
	if ss_empee_setup<>cno then 
		autoLibrary
		open #h_company:=fngethandle: "Name=[Q]\PRmstr\Company.h[cno]",internal,input 
		read #h_company,using 'form pos 236,pd 3.3': ficarate
		close #h_company: 
		if ficarate=0 then ficarate=6.2
	end if 
	fnss_employer=ficarate
fnend
