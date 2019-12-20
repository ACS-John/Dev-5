def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fncreg_write,fncreg_read
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnclient_has
		
		on error goto ERTN
	end if
fnend
def library fnEftData$*128(field$*128; return$*256)
	if ~setup then let fn_setup
	field$=trim$(lwrc$(field$))
	if return$<>'' then
		fncreg_write('UB.EFT.'&field$, return$)
	else
		fncreg_read('UB.EFT.'&field$,return$,'(use default)')
		if return$='(use default)' then
			if field$='immediate destination' then
				! if env$('client')="Ashland"      then return$=" 084000039"
				if env$('client')="Billings"       then return$=" 071000301"
				! if env$('client')="Franklinton"  then return$=" 061000146"
				! if env$('client')="Monticello"   then return$=" 071121963"
			else if field$='immediate origin' then
				! if env$('client')="Ashland"      then return$=" 084201676"  
				if env$('client')="Billings"       then return$=" 071000301"
				! if env$('client')="Franklinton"  then return$=" 065201611"
				! if env$('client')="Monticello"   then return$=" 071121963" 
				if env$('client')="Thomasboro"     then return$=" 071113175"
			else if field$='immediate origin name' then
				! if env$('client')="Ashland"      then return$="Merchants & Farmers    "
				if env$('client')="Billings"       then return$="                      " 
				! if env$('client')="Franklinton"  then return$="Parish National Bank  " ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
				! if env$('client')="Monticello"   then return$="First State Bank       "
				if env$('client')="Thomasboro"     then return$="Gifford State Bank    " 
			else if field$='company identification' then
				! if env$('client')="Ashland"      then return$="1646018046"
				if env$('client')="Billings"       then return$=" 430903099"
				! if env$('client')="Franklinton"  then return$="Franklinto"
				! if env$('client')="Monticello"   then return$="1376001815"
				if env$('client')="Thomasboro"     then return$=" 376000521"
			else if field$='originating dfi identification' then
				! if env$('client')="Ashland"      then return$="08420167" 
				if env$('client')="Billings"       then return$="08150596" 
				! if env$('client')="Franklinton"  then return$="06520161" 
				! if env$('client')="Monticello"   then return$="010251"   
				if env$('client')="Thomasboro"     then return$=" 2000412" 
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
include: Ertn