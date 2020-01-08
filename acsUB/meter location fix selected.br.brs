! D change
library 'S:\Core\Library': fnOpenFile,fnXit,fnCopy,fnStime,fnChain,fnclosefile,fnHamsterFio,fnGetHandle,fnRead4column
! fnCopy('[Q]\UBmstr\MeterLocation*.h[cno]','[Q]\UBmstr\MeterLocation*_before.h[cno]')
fnCopy('[Q]\UBmstr\MeterLocation_before.h[cno]','[Q]\UBmstr\MeterLocation.h[cno]')
fnCopy('[Q]\UBmstr\MeterLocationIdx1_before.h[cno]','[Q]\UBmstr\MeterLocationIdx1.h[cno]')
fnCopy('[Q]\UBmstr\MeterLocationIdx2_before.h[cno]','[Q]\UBmstr\MeterLocationIdx2.h[cno]')
fnCopy('[Q]\UBmstr\MeterLocationIdx3_before.h[cno]','[Q]\UBmstr\MeterLocationIdx3.h[cno]')
fnCopy('[Q]\UBmstr\MeterLocationIdx4_before.h[cno]','[Q]\UBmstr\MeterLocationIdx4.h[cno]')
fnCopy('[Q]\UBmstr\MeterLocationIdx5_before.h[cno]','[Q]\UBmstr\MeterLocationIdx5.h[cno]')
! fnCopy('[Q]\UBmstr\MeterLocation*.h[cno]','[Q]\UBmstr\MeterLocation*_'&date$('ccyy-mm-dd')&'-'&str$(fnStime)&'.h[cno]')
fn_quickExport(' - before')
dim inLocationId$(0)*64,inLocationIdN(0)
dim inTransmitter$(0)*64
dim inMeter$(0)*64
dim inPort$(0)*64
fnRead4column(mat inLocationId$,mat inTransmitter$,mat inMeter$,mat inPort$,'C:\ACS\(Client_Files)\Purdy\Purdy ReportUSGMTUHardwareInstallation3292018.csv',1,2,8,3,',')
mat inLocationIdN(udim(mat inLocationId$))
for x=1 to udim(mat inLocationId$)
	inLocationIdN(x)=val(inLocationId$(x))
	inTransmitter$(x)=inTransmitter$(x)&'-'&inPort$(x)
nex x
fn_fixIt
fn_quickExport(' - after')
Xit: !
! fnChain('S:\Utility Billing\Hand Held\Meter Location')
fnHamsterFio('U4 Meter Location')
end
def fn_fixIt
	dim location$(0)*256,locationN(0)
	hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$)
	do
		read #hLocation,using form$(hLocation): mat location$,mat locationN eof FiEoLocation
		inWhich=srch(mat inLocationIdN,locationN(loc_locationId))
		if inWhich<=0 then 
			pr 'could not find location '&str$(locationN(loc_locationId))&' in Jessica table.'
			pause
			fn_fixTransmitterMeterMixup(location$(loc_transmitter),location$(loc_meterNumber))
		else
			location$(loc_transmitter)=inTransmitter$(inWhich)
			location$(loc_meterNumber)=inMeter$(inWhich)
		end if
		rewrite #hLocation,using form$(hLocation): mat location$,mat locationN
	loop
	FiEoLocation: !
	fnclosefile(hLocation,'U4 Meter Location')
fnend
def fn_isTransmitterNumber(number$*64)
	itnReturn=0 : itnTestCount+=1
	if number$(1:3)='220' then itnReturn=1
	! if number$(1:3)='693' then itnReturn=1
	if itnReturn then itnTrue+=1 else itnFalse+=1
	fn_isTransmitterNumber=itnReturn
fnend
def fn_fixTransmitterMeterMixup(&transmitter$,&meter$)
	ftmnmReturn=0
	transmitter$=trim$(transmitter$)
	meter$=trim$(meter$)
	if fn_isTransmitterNumber(transmitter$) and fn_isTransmitterNumber(meter$) then
		if meter$<>'' then ftmnmReturn=1
		meter$=''
	else if fn_isTransmitterNumber(transmitter$) and ~fn_isTransmitterNumber(meter$) then
		! it's perfect do nothing
		ftmnmReturn=0
	else if ~fn_isTransmitterNumber(transmitter$) and fn_isTransmitterNumber(meter$) then
		dim holdTransmitter$*64
		holdTransmitter$=meter$
		meter$=transmitter$
		transmitter$=holdTransmitter$
		ftmnmReturn=3
	else if ~fn_isTransmitterNumber(transmitter$) and ~fn_isTransmitterNumber(meter$) then
		if transmitter$<>'' then 
			if meter$<>'' and transmitter$<>meter$ then 
				pr 'prob   meter:'&meter$&'  transmitter:'&transmitter$ 
				! pause
				ftmnmReturn=0
				goto OutOfThis
			end if
			ftmnmReturn=2
			meter$=transmitter$
			transmitter$=''
		end if
	end if
	OutOfThis: !
	fn_fixTransmitterMeterMixup=ftmnmReturn
fnend

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
def fn_quickExport(; append$*18)
	dim location$(0)*256,locationN(0)
	hLocation=fn_open('U4 Meter Location',mat location$,mat locationN,mat form$, 0,1)
	open #hOut:=fnGetHandle: 'name='&env$('Desktop')&'\Meter Location'&append$&',replace,recl=2048',d,o
	pr #hOut: 'LocationID ,MeterNumber ,Transmitter         '
	do
		read #hLocation,using form$(hLocation): mat location$,mat locationN eof QeEoLocation
		pr #hOut: lpad$(str$(locationN(loc_locationId)),11)&','&location$(loc_meterNumber)&','&location$(loc_transmitter)
	loop
	QeEoLocation: !
	close #hLocation:
	close #hOut:
fnend
