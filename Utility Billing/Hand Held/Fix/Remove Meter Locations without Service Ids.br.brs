autoLibrary
dim form$(0)*128
dim location$(0)*128
dim locationN(0)
hLocation=fn_openFio('U4 Meter Location',mat location$,mat locationN)
do
	read #hLocation,using form$(hLocation): mat location$,mat locationN eof EoHlocation
	if trim$(location$(loc_serviceid))='' then
		delete #hLocation:
	end if
loop
EoHlocation: !
fnChain('S:\Utility Billing\Hand Held\Meter Location')
include: fn_open
