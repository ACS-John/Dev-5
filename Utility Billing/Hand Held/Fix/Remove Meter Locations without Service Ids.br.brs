20000 library 'S:\Core\Library': fnOpenFile,fnchain
20020 dim form$(0)*128
20040 dim location$(0)*128
20060 dim locationN(0)
20080 hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$)
20100 do
20120   read #hLocation,using form$(hLocation): mat location$,mat locationN eof EoHlocation
20140   if trim$(location$(loc_serviceid))='' then
20160     delete #hLocation:
20180   end if
20200 loop
20220 EoHlocation: !
20240 fnchain('S:\Utility Billing\Hand Held\Meter Location')
78000 ! <updateable region: fn_open (supressprompt:=2)>  
78020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
78040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
78060   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
78080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
78100     mat loadedsubs$(udim(loadedsubs$)+1) 
78120     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
78140     for index=1 to udim(mat _fileiosubs$) 
78160       execute (_fileiosubs$(index)) 
78180     next index
78200   end if
78220 fnend
78240 ! </updateable region: fnopen>