def library fngetcd(&mcd$)
	autoLibrary
	option retain 

	dim getcd_ln$*60
	dim oldcd$*60

	if oldcd$<>"" then 
		mcd$=oldcd$ 
	else
		oldcd$=mcd$=fnshortpath$("")
	end if
fnend 

