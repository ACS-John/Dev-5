dim location$(0)*256,locationN(0)
hLfaLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 1,4)
pr 'kln(hLfaLocation,1)=';kln(hLfaLocation,1)
end
include: fn_open
