dim location$(0)*256,locationN(0)
hLfaLocation=fn_openFio('U4 Meter Location',mat location$,mat locationN, 1,4)
pr 'kln(hLfaLocation,1)=';kln(hLfaLocation,1)
end
include: fn_open
