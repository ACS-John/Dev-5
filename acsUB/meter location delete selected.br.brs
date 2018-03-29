20000 dim form$(0)*2048
20020 dim location$(0)*256,locationN(0)
20040 library 'S:\Core\Library': fnOpenFile,fnXit
20060 hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$)
20080 do
20100   read #hLocation,using form$(hLocation): mat location$,mat locationN eof EoLocation
20120   if trim$(location$(loc_transmitter))='' then
20140     delete #hLocation:
20160     delCount+=1
20180   end if
20200 loop
20220 EoLocation: !
20240 pr 'deleted '&str$(delCount)
20260 Xit: fnXit
48000 def fn_open(fileName$*255, mat openData$, mat openDataN, mat form$; inputOnly,openKeyNum,disableEnumSort,openPath$*255,mat openDescription$,mat fieldWidths,disableUpdate, ___,fileIoEnumItem)
48020   dim form$(0)*2048 ! global
48040   dim fileIoEnum$(1)*800
48060   dim loadedEnums$(1)*32
48080   fn_open=fnOpenFile(fileName$, mat openData$, mat openDataN, mat form$, inputOnly,openKeyNum,disableEnumSort, openPath$,mat openDescription$,mat fieldWidths, mat fileIoEnum$,disableUpdate)
48100   if ~max(srch(mat loadedEnums$,uprc$(fileName$)),0) then
48120     mat loadedEnums$(udim(mat loadedEnums$)+1)
48140     loadedEnums$(udim(mat loadedEnums$))=uprc$(fileName$)
48160     for fileIoEnumItem=1 to udim(mat fileIoEnum$)
48180       execute fileIoEnum$(fileIoEnumItem)
48200     next fileIoEnumItem
48220   end if
48240 fnend

