! Replace S:\acsCL\Receipt
! Standard receipt file
autoLibrary
on error goto Ertn
fnTop(program$)
fnaddreceipt
goto Xit
Xit: fnXit
include: Ertn
