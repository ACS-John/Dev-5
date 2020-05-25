! Replace S:\Core\twip2inch.br
! converts twips to inches
! this function rounds to 4 decimal places
 
def library fntwip2inch(&x)
	x=round(x/1440,4)
	fntwip2inch=x
fnend
 
