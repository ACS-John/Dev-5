00010 ! B change
10000 library 'S:\Core\Library': fnOpenFile,fnXit,fnCopy,fnStime,fnChain,fnclosefile,fnHamsterFio,fnGetHandle
12000 ! fnCopy('[Q]\UBmstr\MeterLocation*.h[cno]','[Q]\UBmstr\MeterLocation*_before.h[cno]')
12020 fnCopy('[Q]\UBmstr\MeterLocation_before.h[cno]','[Q]\UBmstr\MeterLocation.h[cno]')
12040 fnCopy('[Q]\UBmstr\MeterLocationIdx1_before.h[cno]','[Q]\UBmstr\MeterLocationIdx1.h[cno]')
12060 fnCopy('[Q]\UBmstr\MeterLocationIdx2_before.h[cno]','[Q]\UBmstr\MeterLocationIdx2.h[cno]')
12080 fnCopy('[Q]\UBmstr\MeterLocationIdx3_before.h[cno]','[Q]\UBmstr\MeterLocationIdx3.h[cno]')
12100 fnCopy('[Q]\UBmstr\MeterLocationIdx4_before.h[cno]','[Q]\UBmstr\MeterLocationIdx4.h[cno]')
12120 fnCopy('[Q]\UBmstr\MeterLocationIdx5_before.h[cno]','[Q]\UBmstr\MeterLocationIdx5.h[cno]')
12140 ! fnCopy('[Q]\UBmstr\MeterLocation*.h[cno]','[Q]\UBmstr\MeterLocation*_'&date$('ccyy-mm-dd')&'-'&str$(fnStime)&'.h[cno]')
13000 fn_quickExport(' - before')
13001 fn_quickExport(' - before 2')
13002 fn_quickExport(' - before 3')
13020 fn_fixIt
13040 fn_quickExport(' - after')
21040 Xit: !
21060 ! fnChain('S:\Utility Billing\Hand Held\Meter Location')
21080 fnHamsterFio('U4 Meter Location')
21100 end
32000 def fn_fixIt
32020   dim location$(0)*256,locationN(0)
32040   hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$)
32060   do
32080     read #hLocation,using form$(hLocation): mat location$,mat locationN eof FiEoLocation
32100     if fn_fixTransmitterMeterMixup(location$(loc_transmitter),location$(loc_meterNumber)) then
32120       rewrite #hLocation,using form$(hLocation): mat location$,mat locationN
32140     end if
32160   loop
32180   FiEoLocation: !
32200   fnclosefile(hLocation,'U4 Meter Location')
32240 fnend
34000 def fn_isTransmitterNumber(number$*64)
34020   itnReturn=0 : itnTestCount+=1
34040   if number$(1:3)='220' then itnReturn=1
34060   ! if number$(1:3)='693' then itnReturn=1
34070   if itnReturn then itnTrue+=1 else itnFalse+=1
34080   fn_isTransmitterNumber=itnReturn
34100 fnend
36000 def fn_fixTransmitterMeterMixup(&transmitter$,&meter$)
36020   ftmnmReturn=0
36040   transmitter$=trim$(transmitter$)
36060   meter$=trim$(meter$)
36080   if fn_isTransmitterNumber(transmitter$) and fn_isTransmitterNumber(meter$) then
36100     if meter$<>'' then ftmnmReturn=1
36120     meter$=''
36140   else if fn_isTransmitterNumber(transmitter$) and ~fn_isTransmitterNumber(meter$) then
36160     ! it's perfect do nothing
36180     ftmnmReturn=0
36200   else if ~fn_isTransmitterNumber(transmitter$) and fn_isTransmitterNumber(meter$) then
36220     dim holdTransmitter$*64
36240     holdTransmitter$=meter$
36260     meter$=transmitter$
36280     transmitter$=holdTransmitter$
36300     ftmnmReturn=3
38000   else if ~fn_isTransmitterNumber(transmitter$) and ~fn_isTransmitterNumber(meter$) then
38020     if transmitter$<>'' then 
38040       if meter$<>'' and transmitter$<>meter$ then 
38060         pr 'prob   meter:'&meter$&'  transmitter:'&transmitter$ 
38080         ! pause
38100         ftmnmReturn=0
38120         goto OutOfThis
38140       end if
38160       ftmnmReturn=2
38180       meter$=transmitter$
38200       transmitter$=''
38220     end if
38240   end if
38260   OutOfThis: !
38280   fn_fixTransmitterMeterMixup=ftmnmReturn
38300 fnend

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
52000 def fn_quickExport(; append$*18)
52020   dim location$(0)*256,locationN(0)
52040   hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,1)
52060   open #hOut:=fnGetHandle: 'name='&env$('userprofile')&'\Desktop\Meter Location'&append$&',replace,recl=2048',d,o
52080   pr #hOut: 'LocationID ,MeterNumber ,Transmitter         '
52100   do
52120     read #hLocation,using form$(hLocation): mat location$,mat locationN eof QeEoLocation
52180     pr #hOut: lpad$(str$(locationN(loc_locationId)),11)&','&location$(loc_meterNumber)&','&location$(loc_transmitter)
52220   loop
52240   QeEoLocation: !
52260   close #hLocation:
52280   close #hOut:
52300 fnend
