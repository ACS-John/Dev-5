! Replace S:\Core\inch2twip.br
! converts inches to twips : _
	! rounds to 0 decimal places
 
def library fninch2twip(&x)
 
		library 'S:\Core\Library': fnerror
		on error goto Ertn
 
		x=round(x*1440,0)
		goto Xit
 
include: ertn
 
Xit: fnend
 
