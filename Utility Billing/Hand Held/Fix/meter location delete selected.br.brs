dim form$(0)*2048
dim location$(0)*256,locationN(0)
autoLibrary
hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$)
do
	read #hLocation,using form$(hLocation): mat location$,mat locationN eof EoLocation
	if trim$(location$(loc_transmitter))='' then
		delete #hLocation:
		delCount+=1
	end if
loop
EoLocation: !
pr 'deleted '&str$(delCount)
Xit: fnXit
include: fn_open
