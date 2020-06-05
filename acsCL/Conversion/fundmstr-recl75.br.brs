! Replace S:\acsCL\Conversion\fundmstr-recL75
! convert CL gl control file to hold discounts                                ! for use from version 0 to version 0 : _
! okay to use on RecL of 63
def library fnglcontrol
	autoLibrary
	fnCopy('[Q]\CLmstr\fundmstr.h[cno]','[Q]\CLmstr\fundmstr.h[cno]',75)
	fnIndex('[Q]\CLmstr\fundmstr.h[cno]','[Q]\CLmstr\fundidx1.h[cno]','1,3')
fnend
 
