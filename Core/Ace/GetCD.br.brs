def library fngetcd(&mcd$)
	library 'S:\Core\Library': fnshortpath$
	option retain 

	dim getcd_ln$*60
	dim oldcd$*60

	if oldcd$<>"" then 
		mcd$=oldcd$ 
	else
		oldcd$=mcd$=fnshortpath$("")
	end if
fnend 

