def library fnClaimFolder$*128(fileno$; ___,return$*8)
	library 'Library\clsUtil': fnreport_path$
	return$=srep$(rtrm$(fileno$),".","_") 
	return$=srep$(return$,",","_") 
	return$=srep$(return$,"\","_") 
	return$=srep$(return$," ","_")
	return$=fnreport_path$&trim$(return$(1:3))&"\"&trim$(return$)
	fnMakeSurePathExists(return$&'\')
	fnClaimFolder$=return$
fnend
