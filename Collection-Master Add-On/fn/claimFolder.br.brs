def library fnClaimFolder$*128(fileno$; ___,return$*128)
	if ~setup_claimFolder then
		setup_claimFolder=1
		library 'S:\Core\Library': fnMakeSurePathExists
		library 'Library\clsUtil': fnreport_path$
	end if
	return$=srep$(rtrm$(fileno$),".","_") 
	return$=srep$(return$,",","_") 
	return$=srep$(return$,"\","_") 
	return$=srep$(return$," ","_")
	return$=fnreport_path$&trim$(return$(1:3))&"\"&trim$(return$)
	fnMakeSurePathExists(return$&'\')
	fnClaimFolder$=return$
fnend
