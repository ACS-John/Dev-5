def library fnEftData$*128(field$*128; return$*256)
	if ~setup then fn_setup
	field$=trim$(lwrc$(field$))
	if return$<>'' then
		fncreg_write('UB.EFT.'&field$, return$)
	else
		fncreg_read('UB.EFT.'&field$,return$,'(use default)')
		if return$='(use default)' then
			if field$='immediate destination' then
				if env$('client')="Billings"       then return$=" 071000301"
			else if field$='immediate origin' then
				if env$('client')="Billings"       then return$=" 071000301"
			else if field$='immediate origin name' then
				if env$('client')="Billings"       then return$="                      " 
			else if field$='company identification' then
				if env$('client')="Billings"       then return$=" 430903099"
			else if field$='originating dfi identification' then
				if env$('client')="Billings"       then return$="08150596" 
			else if field$='standard entry class code' then
				return$="PPD" ! STANDARD ENTRY CLASS CODE
				if env$('client')="Billings" then 
					return$='CCD' ! Corporate
				else
					return$="PPD"
				end if
			end if
		end if
		if return$='(use default)' then return$=''
	end if
	fnEftData$=return$
fnend
include: fn_setup