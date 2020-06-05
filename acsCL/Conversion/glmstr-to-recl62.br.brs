! Replace S:\acsCL\Conversion\GLmstr-to-recL62
! convert CL [Q]\GLmstr from any record length to 62 
! for use from version 0 to version 0 
! okay to use on RecL of 72 or 80
def library fnglmstrtorecl62
	autoLibrary
	fnCopy('[Q]\CLmstr\GLmstr.H[cno]','[Q]\CLmstr\GLmstr.H[cno]',62)
	fnRemoveDeletedRecords('[Q]\CLmstr\GLmstr.H[cno]')
	fnIndex('[Q]\CLmstr\GLmstr.H[cno]','[Q]\CLmstr\GLIndex.h[cno]','1,12')
fnend

