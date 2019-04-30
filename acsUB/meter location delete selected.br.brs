dim form$(0)*2048
dim location$(0)*256,locationN(0)
library 'S:\Core\Library': fnOpenFile,fnXit
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
def fn_open(fileName$*255, mat openData$, mat openDataN, mat form$; inputOnly,openKeyNum,disableEnumSort,openPath$*255,mat openDescription$,mat fieldWidths,disableUpdate, ___,fileIoEnumItem)
	dim form$(0)*2048 ! global
	dim fileIoEnum$(1)*800
	dim loadedEnums$(1)*32
	fn_open=fnOpenFile(fileName$, mat openData$, mat openDataN, mat form$, inputOnly,openKeyNum,disableEnumSort, openPath$,mat openDescription$,mat fieldWidths, mat fileIoEnum$,disableUpdate)
	if ~max(srch(mat loadedEnums$,uprc$(fileName$)),0) then
		mat loadedEnums$(udim(mat loadedEnums$)+1)
		loadedEnums$(udim(mat loadedEnums$))=uprc$(fileName$)
		for fileIoEnumItem=1 to udim(mat fileIoEnum$)
			execute fileIoEnum$(fileIoEnumItem)
		next fileIoEnumItem
	end if
fnend

