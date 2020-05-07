! formerly S:\acsUB\hhfro
! -- Transfer Data From Hand Held to Computer
library program$: fnRetrieveHandHeldFile
fn_setup
if ~fnregistered_for_hh then ! r:
	mat ml$(2) 
	ml$(1)="You must purchase the ACS Utility Billing Hand Held"
	ml$(2)="module to access these features"
	fnmsgbox(mat ml$, response$, '',64)
	goto XIT
end if  ! /r
fnRetrieveHandHeldFile
fnxit
def library fnRetrieveHandHeldFile(; automationBookNumber)
	if ~setup then let fn_setup
	fntop(program$)
	if automationBookNumber>0 and lwrc$(devicePreference$)<>'[ask]' and lwrc$(preferenceHandHeldFromFile$)<>'[ask]' then
		respc=0
		resp$(rc_book:=respc+=1)=str$(automationBookNumber)
		resp$(rc_merge:=respc+=1)='False'
		automationBookNumber=0
		ckey=0
		goto Screen1ProcessResponse
	end if
	SCREEN1: ! r:
		respc=0 : lc=0
		fnUreg_read('bookNumberToStoreReadings',bookNumberToStoreReadings$)
		fnTos
		lc+=1
		fnLbl(lc+=1,1,"Book Number to store readings:",30,1)
		fnTxt(lc,32,2,0,1,"20",0,"Be careful not to use the same route # twice in the same billing cycle.  The first route will be lost if it has not been calculated.")
		resp$(rc_book:=respc+=1)=bookNumberToStoreReadings$
		lc+=1
		fnChk(lc+=1,33,'Merge into book', 1,0,0,0) ! requires a format that utilizes [ACS Hand Held File Generic Version 2]
		fnLbl(lc,35,'(only supported by some devices)') ! requires a format that utilizes [ACS Hand Held File Generic Version 2]
		resp$(rc_merge:=respc+=1)='False'
		lc+=1
		fnLbl(lc+=1,1,"Hand Held model:",30,1)
		if lwrc$(devicePreference$)='[ask]' then
			fncomboa("HH-FroCBox",lc,32,mat deviceOption$)
			resp$(rc_Device:=respc+=1)=deviceSelected$
		else
			fnLbl(lc,32,devicePreference$)
		end if
		lc+=1
		if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
			lc+=1
			fnLbl(lc+=1,1,"Source File:",30,1)
			fnTxt(lc,32,20,256,0,"70",0,'Source file should be drive designation and file name of the file returned from the Hand Held.')
			rc_path:=respc+=1 : if resp$(rc_path)="" then resp$(rc_path)=askPath$
		else
			fnLbl(lc+=1,1,"Importing from "&fn_hh_input_filename$,len("Importing from "&fn_hh_input_filename$),1)
		end if
		fnCmdSet(2)
		fnAcs2(mat resp$,ckey)
		Screen1ProcessResponse: !
		if ckey<>5 then
			bookNumberToStoreReadings$=resp$(rc_book)
			bookNumberToStoreReadings$
			fnUreg_write('bookNumberToStoreReadings',bookNumberToStoreReadings$)
			if lwrc$(devicePreference$)='[ask]' then
				deviceSelected$=resp$(rc_Device)
				fnureg_write('Hand Held Device Asked',deviceSelected$)
			else
				deviceSelected$=devicePreference$
			end if
			enableMerge$=resp$(rc_merge)
			if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
				askPath$=resp$(rc_path)
				fnureg_write('Hand Held From File Asked',askPath$)
				if askPath$='' or ~exists(env$('at')&askPath$) then
					mat ml$(1)
					ml$(1)="File not found: "&askPath$
					fnmsgbox(mat ml$, response$)
					goto SCREEN1
				end if
			end if
			if fn_transfer(bookNumberToStoreReadings$,enableMerge$,env$('at')&askPath$)=-1 then goto SCREEN1
		end if
	goto XIT ! /r
	XIT: ! target of Ertn exits
fnend
def fn_transfer(bookNumberToStoreReadings$,enableMerge$,askPath$*128)
	transferReturn=0
	dim bookFile$*512
	bookFile$="[Q]\UBmstr\Readings."&ltrm$(bookNumberToStoreReadings$)
	if enableMerge$='True' and exists(bookFile$) then
		dim mergeFileOrigional$*512
		mergeFileOrigional$=env$('temp')&'\acs\mergeFileOrigional-book'&bookNumberToStoreReadings$&'-session'&session$&'.txt'
		fnMakesurePathExists(mergeFileOrigional$)
		fnCopy(bookFile$,mergeFileOrigional$)
	else
		enableMerge$='False'
	end if
	if deviceSelected$='Aclara' then
		transferReturn=fn_aclara(bookFile$,enableMerge$)
	! else if deviceSelected$="Aclara Work Order" then
	! 	transferReturn=fn_aclaraWorkOrder(bookFile$,enableMerge$)
	else if deviceSelected$="ACS Meter Reader" then
		fn_acsmr(bookFile$)
	else if deviceSelected$="AMR" then
		fn_amr(bookFile$)
	else if deviceSelected$="Badger Beacon" then
	  transferReturn=fn_badgerBeacon(fn_hh_input_filename$,bookFile$)
	else if deviceSelected$="Badger" or deviceSelected$="Badger Connect C" then
		fnCopy(fn_hh_input_filename$,bookFile$)
	else if deviceSelected$="Boson" then
		fn_boson(bookFile$)
	else if deviceSelected$='CSV by LocationID' then
		transferReturn=fn_CsvByLocationId(bookFile$,enableMerge$)
	else if deviceSelected$="DriveBy" then
		fnCopy(fn_hh_input_filename$,bookFile$)
	else if deviceSelected$="EZReader" then
		fn_ezreader(bookFile$)
	else if deviceSelected$="Green Tree" then
		fnCopy(fn_hh_input_filename$,bookFile$)
	else if deviceSelected$="Hersey" then
		fn_hersey(bookFile$)
	else if deviceSelected$="Itron FC300" then
		fn_itron(bookFile$)
	else if deviceSelected$="LapTop" then
		fn_laptop(bookFile$)
	else if deviceSelected$="Master Meter" then
		fn_import_l_readings_txt(bookFile$, 358)
	else if deviceSelected$="Neptune (Equinox v4)" then
		fn_neptuneEquinoxV4(fn_hh_input_filename$,bookFile$)
	else if deviceSelected$="Other" and env$('client')="Brier Lake" then
		fn_import_l_readings_txt(bookFile$)
	else if deviceSelected$="Psion Workabout" then
		fn_psion_workabout(bookFile$)
	else if deviceSelected$="READy Water" then
		fn_import_l_readings_txt(bookFile$)
	else if deviceSelected$="Sensus" then
		fn_sensus_in(bookFile$)
	else if deviceSelected$="Unisys" then
		fnCopy(fn_hh_input_filename$,bookFile$)
	else
		mat ml$(1)
		ml$(1)='Unrecognized device: '&deviceSelected$
		fnmsgbox(mat ml$)
	end if
	if transferReturn>0 then
		mat ml$(1)
		ml$(1)=str$(transferReturn)&' records imported to book '&bookNumberToStoreReadings$&'.'
		fnmsgbox(mat ml$)
	end if
	fn_transfer=transferReturn
fnend
def fn_hh_input_filename$*256
	! requires local variables: deviceSelected$,preferenceHandHeldFromFile$,askPath$
	dim hif_return$*256
	hif_return$=preferenceHandHeldFromFile$
	if lwrc$(hif_return$)='[ask]' then
		hif_return$=askPath$
	else if trim$(hif_return$)='' then
		if deviceSelected$="Sensus" then
			hif_return$='c:\vol002\amrs\READ.DAT'
		else if deviceSelected$="EZReader" then
			hif_return$=askPath$
		else if deviceSelected$="ACS Meter Reader" then
			hif_return$=askPath$
		else if deviceSelected$="AMR" then
			hif_return$=askPath$
		else if deviceSelected$="Itron FC300" then
			! if env$('client')='Findlay' then
			!   hif_return$='\\vof-pc\itronshared\FCS\Export\Output\UPLOAD.DAT' ! "C:\Itron\FCSShare\Export\Output\upload.dat"
			! else
				hif_return$="C:\mvrs\xfer\upload\UPLOAD.DAT"
			! end if
		else if deviceSelected$="Psion Workabout" then
			hif_return$="[Q]\UBmstr\Readings.out"
		else if deviceSelected$="Badger" or deviceSelected$="Badger Connect C" or deviceSelected$="DriveBy" then
			hif_return$="c:\connect\connect.ot3"
		else if deviceSelected$="Unisys" then
			hif_return$="[Q]\UBmstr\ReadOut.dat"
		else if deviceSelected$="Boson" then
			hif_return$="[Q]\UBmstr\outofpalm.txt"
		else if deviceSelected$="Other" and env$('client')="Brier Lake" then
			hif_return$="L:\readings.txt"
		end if
	end if
	if hif_return$(1:2)='@:' then let hif_return$(1:2)='' ! take it off if it is already there before putting it back on.
	fn_hh_input_filename$=env$('at')&hif_return$
fnend

def fn_readingsFileVersion$*128(bookFile$*512)
	dim rfvLine$*512,rfvReturn$*128
	open #hRfv:=fngethandle: "Name="&bookFile$,display,input
	linput #hRfv: rfvLine$
	close #hRfv:
	if rtrm$(rfvLine$)='[ACS Hand Held File Generic Version 2]' then
		rfvReturn$='[ACS Hand Held File Generic Version 2]'
	else
		rfvReturn$='legacy'
		fnPause
	end if
	fn_readingsFileVersion$=rfvReturn$
fnend
def fn_acsmr(bookFile$*256) ! ACS Meter Reader
	source$=resp$(1)
	open #2: "Name="&fn_hh_input_filename$&",RecL=256",display,input ! acs_meter_data.txt
	fn_readings_backup(bookFile$)
	open #3: "Name="&bookFile$&",RecL=30,replace",display,output
	do
		linput #2: amr$ eof ACSMR_XIT
		z$=amr$(1:10)
		reading=val(amr$(133:142))
		pr #3,using "form pos 1,c 10,n 10": z$,reading
	loop
	ACSMR_XIT: !
fnend
def fn_CsvByLocationId(bookFile$*512,enableMerge$)
	if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then aclaraWorkOrderReturn=-1 : goto CblEoF
	open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
	open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
	pr #hOut: '[ACS Hand Held File Generic Version 2]'
	pr #hOut: 'Source File='&fn_hh_input_filename$
	linput #hIn: line$ eof CblEoF
	if srch(line$,chr$(9))>0 then cblDelimiter$=chr$(9) else cblDelimiter$=','
	dim cblItem$(0)*256
	str2mat(line$,mat cblItem$,cblDelimiter$)
	cblCsv_LocationId=fn_findFirstMatch(mat cblItem$,'Location ID','LocationID')
	cblCsv_ReadingWater=fn_findFirstMatch(mat cblItem$,'Water Reading')
	cblCsv_WaterTransmitter=fn_findFirstMatch(mat cblItem$,'Water Transmitter Number','MTU ID')
	cblCsv_WaterTransmitterSuffix=fn_findFirstMatch(mat cblItem$,'Port')
	do
		linput #hIn: line$ eof CblEoF
		str2mat(line$,mat cblItem$,cblDelimiter$)
		pr #hOut: 'Customer.Number='&fnAccountFromLocationId$(val(cblItem$(cblCsv_LocationId)),1)
		fn_g2IfTherePrOut(cblCsv_ReadingWater,'Reading.Water',mat cblItem$)
		if cblCsv_LocationId<>0 then
			pr #hOut: 'MeterAddress.LocationID='&str$(val(cblItem$(cblCsv_LocationId)))
		end if
		if cblCsv_WaterTransmitter<>0 and cblCsv_WaterTransmitterSuffix<>0 then
			pr #hOut: 'Meter.Transmitter.Water='&trim$(cblItem$(cblCsv_WaterTransmitter))&'-'&trim$(cblItem$(cblCsv_WaterTransmitterSuffix))
		else if cblCsv_WaterTransmitter<>0 then
			pr #hOut: 'Meter.Transmitter.Water='&trim$(cblItem$(cblCsv_WaterTransmitter))
		end if
		pr #hOut: ''
	loop
	CblEoF: !
	close #hIn:
	close #hOut:
	if enableMerge$='True' then
		fn_mergeBooks(mergeFileOrigional$,bookFile$)
	end if
fnend
def fn_g2IfTherePrOut(gitproItemEnum,gitproLabel$*128,mat gitproItem$)
	! utility for [ACS Hand Held File Generic Version 2]
		if gitproItemEnum<>0 and val(gitproItem$(gitproItemEnum))<>0 then
			pr #hOut: gitproLabel$&'='&gitproItem$(gitproItemEnum)
		end if
fnend
! r: hand helds
	def fn_badgerBeacon(fileIn$*256,bookFile$*512; ___,returnN)
		if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then
			returnN=-1
		else
			returnN=0
			open #hIn:=fngethandle: "Name="&fileIn$&',eol=lf',display,input
			open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
			pr #hOut: '[ACS Hand Held File Generic Version 2]'
			pr #hOut: 'Source File='&fn_hh_input_filename$
			do
				linput #hIn: line$ eof EO_BadgerBeacon
				if fn_BadgerBeaconParseLine(line$,mat tmpDataName$,mat tmpDataValue$) then
					for awoX=1 to udim(mat tmpDataName$)
						pr #hOut: tmpDataName$(awoX)&'='&tmpDataValue$(awoX)
					nex awoX
					pr #hOut: ''
					returnN+=1
				end if
			loop
			EO_BadgerBeacon: !
			close #hIn:
			close #hOut:
		end if
		fn_badgerBeacon=returnN
	fnend
	def fn_BadgerBeaconParseLine(line$*1024,mat tmpDataName$,mat tmpDataValue$; ___,returnN,delim$,quotesTrim$,readingColumnName$*64)
		reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
		delim$=tab$
		if ~bbplHeaderProcessed then 
			! r: parse header and get enumerations
			bbplHeaderProcessed=1
			! if env$('client')='Campbell' then ! 11/19/2018 = seemed to have changed from [Tab]
			! 	delim$=','
			! 	quotesTrim$="QUOTES:TRIM"
			! 	readingColumnName$='Read'
			! else
				quotesTrim$=''
				readingColumnName$='Current_Read'
			! end if
			str2mat(line$,mat lineItem$,delim$,quotesTrim$)
			bbpl_Account_ID      =srch(mat lineItem$,'Account_ID'        )
			bbpl_Location_ID     =srch(mat lineItem$,'Location_ID'       )
			bbpl_Service_Point_ID=srch(mat lineItem$,'Service_Point_ID' )
			bbpl_Read            =srch(mat lineItem$,readingColumnName$  )
			bbpl_Read_Time       =srch(mat lineItem$,'Current_Read_Date')
	
			if bbpl_Account_ID      =0 then
				pr 'critical header (Account_ID) not found' : pause
			else if bbpl_Location_ID     =0 then
				pr 'critical header (Location_ID) not found' : pause
			else if bbpl_Service_Point_ID=0 then
				pr 'critical header (Service_Point_ID) not found' : pause
			else if bbpl_Read            =0 then
				pr 'critical header (Read) not found' : pause
			! else if bbpl_Read_Time       =0 then
			! 	pr 'critical header (Read_Time) not found' : pause
			end if
			! /r
		else
			str2mat(line$,mat lineItem$,delim$)
			mat tmpDataName$(0)
			mat tmpDataValue$(0)
			! pr ' how do we parse this? ' :  pause
			if trim$(lineItem$(bbpl_Service_Point_ID))='WA' then
				fn_addTmpData('Customer.Number'   ,lineItem$(bbpl_Account_ID))
				fn_addTmpData('Reading.Water'     ,lineItem$(bbpl_Read))
				if bbpl_Read_Time then
					fn_addTmpData('Reading.Water.Date',date$(days(lineItem$(bbpl_Read_Time)(1:pos(lineItem$(bbpl_Read_Time),' ')-1),'ccyy-mm-dd'),'mm/dd/ccyy'))
				else
					fn_addTmpData('Reading.Water.Date',date$('mm/dd/ccyy'))
				end if
				returnN=1
			else 
				pr 'unexpected Service_Point_ID: '&lineItem$(bbpl_Service_Point_ID) : pause
			end if
		end if
		fn_BadgerBeaconParseLine=returnN
	fnend
	def fn_aclara(bookFile$*512,enableMerge$)
		! pr ' this import is not yet written.'
		! pr ' this import will only import active clients'
		! pause
		if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then aclaraWorkOrderReturn=-1 : goto CblEoF
		open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
		open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
		pr #hOut: '[ACS Hand Held File Generic Version 2]'
		pr #hOut: 'Source File='&fn_hh_input_filename$
		if dataIncludesHeaders then
			linput #hIn: line$ eof EO_Aclara ! just consume the headers
		end if
		do
			z$=''
			linput #hIn: line$ eof EO_Aclara
			fn_aclaraParseLine(line$,mat tmpDataName$,mat tmpDataValue$)
			for awoX=1 to udim(mat tmpDataName$)
				pr #hOut: tmpDataName$(awoX)&'='&tmpDataValue$(awoX)
			nex awoX
			pr #hOut: ''
			aclaraReturn+=1
		loop
		EO_Aclara: !
		close #hIn:
		close #hOut:
		if enableMerge$='True' then
			fn_mergeBooks(mergeFileOrigional$,bookFile$)
		end if
		fn_aclara=aclaraReturn
	fnend
	def fn_aclaraParseLine(line$*1024,mat tmpDataName$,mat tmpDataValue$)
		reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
		str2mat(line$,mat lineItem$,chr$(9))
		mat tmpDataName$(0)
		mat tmpDataValue$(0)
		fn_addTmpData('Customer.Number',lineItem$(2))
		fn_addTmpData('Reading.Water'  ,lineItem$(7))
		! fn_addTmpData('Reading.Water.Date'  ,lineItem$(8))
	fnend
	! r: aclara work order
	! def fn_aclaraWorkOrder(bookFile$*512,enableMerge$)
	! 	dataIncludesHeaders=1
	! 	if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then aclaraWorkOrderReturn=-1 : goto EO_AW
	! 	open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
	! 	open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
	! 	pr #hOut: '[ACS Hand Held File Generic Version 2]'
	! 	pr #hOut: 'Source File='&fn_hh_input_filename$
	! 	if dataIncludesHeaders then
	! 		linput #hIn: line$ eof EO_AW ! just consume the headers
	! 	end if
	! 	do
	! 		z$=''
	! 		linput #hIn: line$ eof EO_AW
	! 		fn_awoParseLine(line$,mat tmpDataName$,mat tmpDataValue$)
	! 		for awoX=1 to udim(mat tmpDataName$)
	! 			pr #hOut: tmpDataName$(awoX)&'='&tmpDataValue$(awoX)
	! 		nex awoX
	! 		pr #hOut: ''
	! 		aclaraWorkOrderReturn+=1
	! 	loop
	! 	EO_AW: !
	! 	close #hIn:
	! 	close #hOut:
	! 	if enableMerge$='True' then
	! 		fn_mergeBooks(mergeFileOrigional$,bookFile$)
	! 	end if
	! 	fn_aclaraWorkOrder=aclaraWorkOrderReturn
	! fnend
	! def fn_awoParseLine(line$*1024,mat tmpDataName$,mat tmpDataValue$)
	! 		reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
	! 		str2mat(line$,mat lineItem$,chr$(9))
	! 		for x=1 to udim(mat lineItem$) : lineItem$(x)=trim$(lineItem$(x),'"') : next x
	! 		mat tmpDataName$(0)
	! 		mat tmpDataValue$(0)
	! 		! fn_addTmpData('Customer.Number'                              ,lineItem$(2)             ) ! account numbers aren't necessarally correct
	! 		fn_addTmpData('Customer.Number'                              ,fnAccountFromLocationId$(val(lineItem$(1))) )
	! 		fn_addTmpData('MeterAddress.LocationID'                      ,str$(val(lineItem$(1)))                        )
	! 		fn_addTmpData('MeterChangeOut.ReadingBefore.Water'           ,lineItem$(12)                                   )
	! 		fn_addTmpData('MeterChangeOut.ReadingAfter.Water'            ,lineItem$(14)                                   ) ! usually 0
	! 		fn_addTmpData('Meter.Transmitter.Water'                      ,lineItem$(10)&'-'&lineItem$(17)                ) !
	! 		fn_addTmpData('Meter.Meter Number.Water'                     ,lineItem$(13)                                   )
	! 		fn_addTmpData('Meter.Longitude.Water'                        ,lineItem$(21)                                   )
	! 		fn_addTmpData('Meter.Latitude.Water'                         ,lineItem$(22)                                   )
	! fnend
	def fn_addTmpData(name$*128,value$*128)
		dim tmpDataName$(0)*128
		dim tmpDataValue$(0)*128
		fnaddonec(mat tmpDataName$,name$)
		fnaddonec(mat tmpDataValue$,value$)
	fnend
	! /r
	def fn_amr(bookFile$*512)
		fn_readings_backup(bookFile$)
		open #3: "Name="&bookFile$&",RecL=30,replace",display,output
		open #2: "Name="&fn_hh_input_filename$&",RecL=620",display,input
		linput #2: amr$ ioerr AMR_NOTHING_TO_READ ! read header
		do
			linput #2: amr$ eof AMR_XIT
			z$=lpad$(trim$(amr$(3:22)),10)
			reading=val(amr$(47:56))
			pr #3,using "form pos 1,c 10,n 10": z$,reading
		loop
		goto AMR_XIT !  AMR_NOTHING_TO_READ
		AMR_NOTHING_TO_READ: !
		mat ml$(1)
		ml$(1)="The File ("&fn_hh_input_filename$&") is empty."
		fnmsgbox(mat ml$,resp$,'',0)
		goto AMR_XIT !  AMR_NOTHING_TO_READ
		AMR_XIT: !
		close #2: ioerr ignore
		close #3: ioerr ignore
		!
	fnend
	def fn_boson(bookFile$*512)
		if ~exists(fn_hh_input_filename$) then
			mat ml$(2)
			ml$(1)='The import file ('&os_filename$(fn_hh_input_filename$)&') could not be found.'
			ml$(2)='You may need to perform the Hot Sync and try again.'
			fnmsgbox(mat ml$, response$, '',0)
		else
			fnCopy(fn_hh_input_filename$,bookFile$)
			fnRename(fn_hh_input_filename$,"[Q]\UBmstr\outofpalm."&ltrm$(bookNumberToStoreReadings$)&"."&date$("YYMMDD")&srep$(time$("HHMMSS"),":","")&".txt")
		end if
	fnend
	def fn_ezreader(bookFile$*512)
		fn_readings_backup(bookFile$,bookNumberToStoreReadings$)
		open #h_out:=3: "Name="&bookFile$&",RecL=30,replace",display,output
		open #2: "Name="&fn_hh_input_filename$&",RecL=578",display,input
		do
			linput #2: line$ eof EXREADER_XIT
			z$=lpad$(trim$(line$(209:228)),10)
			reading=val(line$(309:318))
			pr #h_out,using "form pos 1,c 10,n 10": z$,reading
			! if fncustomerdata$('meter multiplier')
			if env$('client')='GreeneCo' then reading=reading*10
			if env$('client')='Morrisonville' then reading=reading*100
		loop
		EXREADER_XIT: !
		close #2: ioerr ignore
		close #h_out: ioerr ignore
	fnend
	def fn_hersey(bookFile$*512)
		fn_readings_backup(bookFile$,bookNumberToStoreReadings$)
		open #h_out:=3: "Name="&bookFile$&",RecL=30,replace",display,output
		open #2: "Name=" &fn_hh_input_filename$&",RecL=282",display,input
		do
			linput #2: hersey$ eof HERSEY_EOF
			z$=lpad$(trim$(hersey$(1:10)),10)
			reading=val(hersey$(229:238))
			pr #h_out,using "form pos 1,c 10,n 10": z$,reading
		loop
		HERSEY_EOF: !
		close #2: ioerr ignore
		close #h_out: ioerr ignore
		!
	fnend
	def fn_itron(bookFile$*512)
		open #h_itron:=fngethandle: "Name="&fn_hh_input_filename$,display,input
		open #h_itron_out:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
		pr #h_itron_out: '[ACS Hand Held File Generic Version 2]'
		z$=''
		do
			linput #h_itron: line$ eof EO_ITRON
			line_type$=line$(1:3)
			if line_type$="CUS" then
				if z$<>'' then let fn_itron_write ! write the previous one
				reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
				z$=trim$(line$(15:34))(1:10)
			else if line_type$="MTR" then
				itron_meter_category$=line$(94:94)
			else if line_type$="RDG" then
				itron_reading=val(line$(34:43))
								!    itron_read_date$=line$(48:55)
				itron_meter_chenge_out$=line$(92:92)
				if itron_meter_chenge_out$="Y" then meterroll=1 else meterroll=0
				if itron_meter_category$="E" then ! Electric
					reading_electric=itron_reading
					meterroll_electric=meterroll
				else if itron_meter_category$="G" then ! Gas
					reading_gas=itron_reading
					meterroll_gas=meterroll
				!    else if itron_meter_category$="I" then ! Irrigation
				!    else if itron_meter_category$="S" then ! Steam/sewer
				else if itron_meter_category$="W" then ! Water
					if env$('client')='Millry' then reading_water=itron_reading*10 else reading_water=itron_reading
					meterroll_water=meterroll
				end if
			else if line_type$="RFF" or line_type$="WRR" then
				tmpr$=line$(55:56)
				if val(tmpr$)=0 then tmpr$=line$(57:58)
			end if
		loop
		EO_ITRON: !
		fn_itron_write ! write the last one
		close #h_itron:
		close #h_itron_out:
	fnend
	def fn_itron_write
		! pr #h_itron_out,using "form pos 1,c 10,3*n 10,3*n 1": z$,reading_water,reading_electric,reading_gas,meterroll_water,meterroll_electric,meterroll_gas
		if reading_water+reading_electric+reading_gas+meterroll_water+meterroll_electric+meterroll_gas=0 then
			pr #h_itron_out: '! customer number '&z$&' has zero reading.'
		else
			pr #h_itron_out: 'Customer.Number='&z$
			fn_inz(reading_water     ,'Reading.Water'       ,h_itron_out)
			fn_inz(reading_electric  ,'Reading.Electric'    ,h_itron_out)
			fn_inz(reading_gas       ,'Reading.Gas'         ,h_itron_out)
			fn_inz(meterroll_water   ,'MeterRoll.Water'     ,h_itron_out)
			fn_inz(meterroll_electric,'MeterRoll.Electric'  ,h_itron_out)
			fn_inz(meterroll_gas     ,'MeterRoll.Gas'       ,h_itron_out)
			fn_inz(customer_sequence ,'customer.sequence'   ,h_itron_out)
			pr #h_itron_out: 'Meter.Tamper='&str$(val(tmpr$))
		end if
	fnend
	def fn_laptop(bookFile$*512)
		route=val(bookNumberToStoreReadings$)
		L1420: !
		fnTos
		mat resp$=("")
		fnLbl(1,1,"Source Drive:",20,1)
		fnTxt(1,23,20,100,0,"",0,"Source drive should be drive designation for the usb drive, including a : and a \ ")
		if resp$(1)="" then resp$(1)="F:\"
		fnCmdSet(2)
		fnAcs2(mat resp$,ckey)
		if ckey=5 then goto XIT
		source$=resp$(1)
		if len(source$)=0 then goto L1420
		if len(source$)=1 then source$(2:2)=":"
		if source$(3:3)=" " then source$(3:3)="\"
		fnCopy(source$&"readings."&str$(route),"[Q]\UBmstr\readings."&str$(route))
	fnend
	def fn_psion_workabout(bookFile$*512)
		if env$('client')="Ash Grove" then
			execute 'Sy "'&os_filename$("S:\RCom\RCom.exe")&'" /w'
		else if exists("RCom\RComW.exe")<>0 then
			execute 'Sy "'&os_filename$("S:\RCom\RComW.exe")&'" /w'
		else
			execute 'Sy "'&os_filename$("S:\acsUB\PreRoute.bat")&'"'
		end if
		! in august 2006 meters.opo changed to send back route as meters.out; before that it came back with the route # on the file name  (readings.1, etc)
		fnCopy(fn_hh_input_filename$,bookFile$)
	fnend
	def fn_sensus_in(bookFile$*512)
	open #h_sensus:=fngethandle: "Name="&fn_hh_input_filename$&",RecL=22",external,input
	fn_readings_backup(bookFile$)
	open #h_readings:=fngethandle: "Name="&bookFile$&",RecL=30,replace",display,output
	do
		read #h_sensus,using "form pos 1,c 22": line$ eof SENSUS_IN_XIT ioerr SENSUS_IN_XIT
		pr #h_readings,using "form pos 1,c 132": line$
	loop
	SENSUS_IN_XIT: !
	close #h_sensus: ioerr ignore
	close #h_readings: ioerr ignore
fnend
	! r: legacy multi-device hand held type
		def fn_import_l_readings_txt(bookFile$*512; inFileRecordLen)
			fn_readings_backup(bookFile$)
			open #hReadingsOut:=fngethandle: "Name="&bookFile$&",RecL=30,replace",display,output
			! if inFileRecordLen=0 then inFileRecordLen=129
			open #hHandHeld:=fngethandle: "Name="&fn_hh_input_filename$,display,input
			do
				linput #hHandHeld: line$ eof ilrt_EO_L_READINGS_TXT
				if deviceSelected$="Other" and env$('client')="Brier Lake" then
					parseResponse=fn_ilrt_lineParse_BrierLake(line$,z$,reading$)
				else if deviceSelected$='READy Water' then
					! parseResponse=fn_ilrt_lineParse_READy_Water(line$,z$,reading$)
					parseResponse=fn_ilrt_lineParseDelimited(line$,z$,1,reading$,3)
				else if deviceSelected$='Master Meter' then
					parseResponse=fn_ilrt_lineParseFixedWidth(line$,z$,1,10,reading$,14,14, readingDate$,35,8)
				else
					pr 'deviceSelected$ ('&deviceSelected$&') is not recognized in the parse import routines.'
					pause
				end if
				if parseResponse then
					pr #hReadingsOut,using "form pos 1,c 10,c 9": z$,trim$(reading$)
				end if
			loop
			ilrt_EO_L_READINGS_TXT: !
			close #hHandHeld: ioerr ignore
			close #hReadingsOut: ioerr ignore
		fnend
		def fn_ilrt_lineParse_BrierLake(line$*150,&z$,&reading$)
			ilpblReturn=0
			x=val(line$(1:3)) conv ilpbl_finis
			z$=""
			for j=1 to 8
				x=val(line$(j:j)) conv ilrt_L1060
				z$=z$&line$(j:j)
			next j
			ilrt_L1060: !
			z=val(z$)
			z$=cnvrt$("pic(zzzzzzz.##",z)
			reading$=""
			for j1=1 to 20
				x=val(line$(j1+j:j1+j)) conv ilrt_L1120
				reading$=reading$&line$(j1+j:j1+j)
				ilrt_L1120: !
			next j1
			ilpblReturn=1
			ilpbl_finis: !
			fn_ilrt_lineParse_BrierLake=ilpblReturn
		fnend
		def fn_ilrt_lineParse_READy_Water(line$*150,&z$,&reading$)
			ilprwReturn=0
			z$=reading$=""
			str2mat(line$,mat ilprwItem$, chr$(9))
			! ilprwItem$(1)=account number
			! ilprwItem$(2)=meter serial number (from 'U4 Meter Location' table     formerly from meter information file)
			! ilprwItem$(3)=water reading
			! ilprwItem$(4)=reading date
			z$=lpad$(ilprwItem$(1),10)
			reading$=ilprwItem$(3)
			ilprwReturn=1
			fn_ilrt_lineParse_READy_Water=ilprwReturn
		fnend
		def fn_ilrt_lineParseDelimited(line$*512,&key$,item_key,&reading$,item_reading; &readingDate$,item_readingDate)
			dim ilpdItem$(0)*512
			ilprwReturn=0
			z$=reading$=""
			str2mat(line$,mat ilpdItem$, chr$(9))
			key$=lpad$(ilpdItem$(item_key),10)
			reading$=ilpdItem$(item_reading)
			if item_readingDate then
				readingDate$=ilpdItem$(item_readingDate)
			end if
			ilprwReturn=1
			fn_ilrt_lineParseDelimited=ilprwReturn
		fnend
		def fn_ilrt_lineParseFixedWidth(line$*512,&key$,pos_key,len_key,&reading$,pos_reading,len_reading; &readingDate$,pos_date,len_date)
			ilpfwReturn=0
			key$=reading$=readingDate$=''
			key$=line$(pos_key:pos_key+len_key-1)
			reading$=line$(pos_reading:pos_reading+len_reading-1)
			readingDate$=line$(pos_date:pos_date+len_date-1)
			! pr key$,reading$ : pause
			ilpfwReturn=1
			fn_ilrt_lineParseFixedWidth=ilpfwReturn
		fnend
	! /r
def fn_prHout(what$*512; hOverride,___,hPrOut)
	if hOverride>0 then hPrOut=hOverride else hPrOut=hOut
	pr #hPrOut: what$
	pr what$ !  print to console also :)
fnend
	def fn_neptuneEquinoxV4(inputFile$*2048,bookFile$*512; ___,returnN,line_type$,tmpr$,line$*2048,itron_meter_category$*1,itron_meter_chenge_out$*1,itron_reading,meterroll,z$*10,reading_water,reading_electric,reading_gas,meterroll_wate,meterroll_electric,meterroll_gas,hIn,hOut)
		open #hIn:=fngethandle: "Name="&inputFile$,display,input
		open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
		fn_prHout('[ACS Hand Held File Generic Version 2]')
		z$=''
		do
			linput #hIn: line$ eof Eo_nev4
			line_type$=lwrc$(line$(1:5))
			if line_type$="prmdt" then ! r: Premises Detail
				if z$<>'' then 
					! write the previous one
					fn_nev4_write(hOut,z$,reading_water,customer_sequence) 
				end if
				! reset for the next one
				reading_water=0
				customer_sequence=0
				z$=trim$(line$(104:123)) soflow Sub_soFlowAccountNo
				! pr 'z$="'&z$&'"' 
				! pr '*TODO: complete this account number code' : pr line$ : fnPause
				! z$=trim$(z$)(1:10)
				! /r
			else if line_type$="" then 
			else if line_type$="rdgdt" then ! r: Reading Detail
				! pr 'line '&line_type$&' detected.' : pause
				! Record ID                    Req UB  1 - 5 5 A / N RDGDT.
				! Read Type                    Req UB  6 - 9 4 A / N Details of a particular register. For Example: HIGH,LOW,GAL,CFT,WTR,GAS
				! Collection ID                Req UB 10 - 22 13 NUM Indicates the MIU serial number. Use spaces if device provides no ID value.
				! For Future Use                   UB 23 - 29 7 A / N
				! Changed Collection ID        Opt HH 30 - 49 20 NUM
				! Dials                        Req UB 50 - 51 2   NUM Range for dials is 01 to 08, inclusive.
				! Changed Dials                Opt HH 52 - 53 2 NUM Range for dials is 01 to 08, inclusive. 
				! Decimals                     Req UB 54 - 55 2 NUM Range for decimals is 00 to 08, inclusive.
				! Changed Decimals             Opt HH 56 - 57 2 NUM Range for decimals is 00 to 08, inclusive.
				! Read Direction               Opt UB 58 1 A / N R, L, C, or blank only.
				! Hi Limit                     Req UB 59 - 68 10 NUM
				! Low Limit                    Req UB 69 - 78 10 NUM
				! Prev Read                    Req UB 79 - 88 10 NUM
				! Reading                      Req HH 89 - 98 10 A / N
				! Collector Reading            Req HH 99 - 108 10 A / N Raw unadjusted reading from the MIU.
				if 'WTR'=trim$(line$(6:9)) then
					reading_water=val(line$(99:108))
				else 
					pr 'unhandled service in RDGDT record: '&line$(6:9)
					pause
				end if
				
				
				! Read Code                    Req HH 109 - 110 2 A / N
				! Re-entry Count               Req HH 111 - 112 2 NUM
				! Water No Flow 35 Days        Req HH 113 1 NUM Number of days.
				! Peak Backflow                Req HH 114 1 NUM Reverse flow.
				! Leak 35 Days                 Req HH 115 1 NUM Number of days.
				! Current Leak                 Req HH 116 1 NUM Leak status. 
				! Previous Error Count         Opt UB 117 1 NUM Gas tamper. Use 8 to suppress tamper check. 
				! Current Error Count          Req HH 118 1 NUM Current error / tamper count for R900G. 
				! Fatal Error                  Req HH 119 1 NUM Fatal error flag for R900G. 
				! Non-Fatal Error / Flag       Req HH 120 1 NUM Non-fatal error flag for R900G. 
				! Voltage                      Req HH 121 - 123 3 NUM Operating meter voltage for R900.
				! MIU Type                     Req HH 124 - 125 2 NUM Utility meter type.
				! AMR Read Type                Req HH 126 - 127 2 NUM AMR reading type.
				! High Power                   Req HH 128 1 NUM High versus low power indicator for all R900.
				! R900 Format                  Req HH 129 - 130 2 NUM The R900 reading format: 0 = Binary,2 = Data Stream (not used),3 = E-CODER,4 = Mlog
				! Display Digits               Req HH 131 1 NUM Number of digits in main reading display. Multiplier Applied            Req HH 132 1 NUM
				! Gas No Flow                  Req HH 133 1 NUM Period for which there has been no gas flow. 
				! Current Gas Backflow Tamper  Req HH 134 1 NUM
				! Current Gas Removal Tamper   Req HH 135 1 NUM
				! Current Gas Magnetic Tamper  Req HH 136 1 NUM
				! ERT Inversion Tamper         Req HH 137 1 NUM
				! ERT Reverse Tamper           Req HH 138 1 NUM
				! 35-Day Gas Backflow Tamper   Req HH 139 1 NUM
				! 35-Day Gas Removal Tamper    Req HH 140 1 NUM
				! 35-Day Gas Magnetic Tamper   Req HH 141 1 NUM
				! 35-Day Program Flag          Req HH 142 1 NUM R900G only.
				! Reed Switch Failure Flag     Req HH 143 1 NUM R900G only.
				! Additional Flags             Req HH 144 - 212 69 For future use.
				! Register Manufacturer        Req HH 213 - 237 25 A / N
				! Register Install Date        Req HH 238 - 245 8 NUM YYYYMMDD.
				! Register ID                  Req HH 246 - 255 10 A / N
				! CRLF                         Req UB 256 - 257 2 Carriage return, line feed.



				! pr line_type$ 
				! pr '*TODO: complete this Reading Detail code' : pr line$ : fnPause
				
				! /r
			else if line_type$="ordst" then ! r: Order Status  
				! pr """Unfortunately, the .exp doesn't have readings but you'll see where the Order Status Record"
				! pr "is created & this is where the readings will be placed."" -Kieth"
				! pr line_type$ 
				! pr '*TODO: complete this code Order Status' : pr line$ : fnPause
				! /r
			else if line_type$="mtrdt" then ! r: Meter Detail
				                                    ! Record ID             Req UB 1 - 5 5 A / N MTRDT.
				                                    ! Read Sequence         Req UB 6 - 11 6 NUM Right-justify, zero-fill.
				customer_sequence=val(line$(12:17)) ! Changed Read Sequence Opt HH 12 - 17 6 NUM Changed value coming back from field.
				                                    ! Meter Key             Req UB 18 - 37 20 NUM Key to identify the meter within the premises key. Use meter number for this unless the CIS utility billing system vendor has a better key. Meter Number          Req UB 38 - 57 20 A / N Meter serial number.
				                                    ! Changed Meter Number  Opt HH 58 - 77 20 A / N Actual meter number found in the field.
				                                    ! Meter Type            Req UB 78 - 81 4 A / N
				                                    ! Changed Meter Type    Opt HH 82 - 85 4 A / N
				                                    ! Meter Size            Req UB 86 - 93 8 A / N See field description "Meter Size" on page 34.
				                                    ! Changed Meter Size    Opt HH 94 - 101 8 A / N
				! pr line_type$ 
				! pr '*TODO: complete this Meter Detail code' : pr line$ : fnPause
				
				! /r
			end if
		loop
		Eo_nev4: !
		! write the last one
		fn_nev4_write(hOut,z$,reading_water,customer_sequence)
		fnMeterInfo$('','','', 1)
		close #hIn:
		close #hOut:
		! pr 'nev4 complete' : fnPause
	fnend
	Sub_soFlowAccountNo: ! r:
		z$='*invalid*'
		pr bell;'account number too long: ';line$(194:123)
		pause
	continue ! /r
	def fn_nev4_write(hOut,z$,reading_water,customer_sequence; ___,meterMultiplier)
		! pr #hOut,using "form pos 1,c 10,3*n 10,3*n 1": z$,reading_water,reading_electric,reading_gas,meterroll_water,meterroll_electric,meterroll_gas
		if reading_water+reading_electric+reading_gas+meterroll_wate+meterroll_electric+meterroll_gas<>0 then
			fn_prHout('Customer.Number='&z$)
			if reading_water then
				meterMultiplier=val(fnMeterInfo$('reading multipler',z$,'WA'))
				! pr 'meterMultiplier=';meterMultiplier : pause
				if meterMultiplier then
					reading_water=reading_water*meterMultiplier
					reading_water=round(reading_water,0)
				end if
			fn_inz(reading_water     ,'Reading.Water'       )
			end if
			fn_inz(customer_sequence ,'customer.sequence'   )
		else
			fn_prHout('! customer number '&z$&' has all zero readings.')
		end if
	fnend
	def fn_inz(notZeroAmt,what$*512; hOverride) ! ifNotZeroPrHoutEqualsNotZeroAmt
		if notZeroAmt<>0 then fn_prHout(what$&'='&str$(notZeroAmt), hOverride)
	fnend

! /r
def fn_readings_backup(bookFile$*512)
	if exists(bookFile$) then
		fnCopy(bookFile$,"[Q]\UBmstr\readings_"&bookNumberToStoreReadings$&'.bak')
	end if
fnend

def fn_findFirstMatch(mat ffmItemsToSearch$,ffmCriteria1$*256; ffmCriteria2$*256,ffmCriteria3$*256,ffmCriteria4$*256)
	ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria1$)
	if ffmReturn<=0 then ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria2$)
	if ffmReturn<=0 then ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria3$)
	if ffmReturn<=0 then ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria4$)
	fn_findFirstMatch=ffmReturn
fnend
def fn_okToMerge(bookFile$*512,requiredFormat$*128)
	if fn_readingsFileVersion$(bookFile$)<>requiredFormat$ then
		mat ml$(2)
		ml$(1)='The existing book (number '&bookNumberToStoreReadings$&') is not in a format that permits'
		ml$(2)='merging with the '&deviceSelected$&' format.'
		fnmsgbox(mat ml$)
		aclaraWorkOrderReturn=-1
		okayToMergeReturn=0
	else
		okayToMergeReturn=1
	end if
	fn_okToMerge=okayToMergeReturn
fnend
def fn_mergeBooks(mbFile1$*512,mbFile2$*512)
	! mbFile1$ is the origional file.
	! mbFile2$ is the new file whom's values will override mbFile1$'s in the final product.  This file will also be overwritten with the final product.
	dim mbF1Label$(0)*256,  mbF1Value$(0)*256
	mat mbF1Label$(0) : mat mbF1Value$(0)
	dim mbF2Label$(0)*256,  mbF2Value$(0)*256
	mat mbF2Label$(0) : mat mbF2Value$(0)
	fnFileTo2Arrays(mbFile1$,mat mbF1Label$,mat mbF1Value$, 1)
	fnFileTo2Arrays(mbFile2$,mat mbF2Label$,mat mbF2Value$, 1)
	dim mbTmpNewFile$*512
	mbTmpNewFile$=env$('temp')&'\acs\mergeTmpNew-session'&session$&'.txt'
	fnMakesurePathExists(mbTmpNewFile$)
	open #hMergeNew:=fngethandle: 'name='&mbTmpNewFile$&",RecL=512,Replace",d,o
	pr #hMergeNew: '[ACS Hand Held File Generic Version 2]'
	fn_getCustomerNumbers(mat mbF1Label$,mat mbF1Value$,mat mbF1CustomerNumbers$)
	fn_getCustomerNumbers(mat mbF2Label$,mat mbF2Value$,mat mbF2CustomerNumbers$)
	dim mbCg1Label$(0)*256,mbCg1Value$(0)*256
	dim mbCg2Label$(0)*256,mbCg2Value$(0)*256
	for mbX=1 to udim(mat mbF1CustomerNumbers$)
		fn_getCustomerGroup(mat mbF1Label$,mat mbF1Value$,mbF1CustomerNumbers$(mbX),mat mbCg1Label$,mat mbCg1Value$)
		cg2Count=fn_getCustomerGroup(mat mbF2Label$,mat mbF2Value$,mbF1CustomerNumbers$(mbX),mat mbCg2Label$,mat mbCg2Value$)
		pr #hMergeNew: 'Customer.Number='&mbF1CustomerNumbers$(mbX)
		if cg2Count then
			for mb1x=2 to udim(mbCg1Label$)
				if mbCg1Label$(mb1x)<>'' then
					mb2Match=srch(mat mbCg2Label$,mbCg1Label$(mb1x))
					if mb2Match<=0 or mbCg1Value$(mb1x)=mbCg2Value$(mb2Match) then
						pr #hMergeNew: mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
						! pr 'a>>';mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
						mbCg1Label$(mb1x)=''
						if mb2Match then mbCg2Label$(mb2Match)=''
					else
						pr #hMergeNew: mbCg2Label$(mb2Match)&'='&mbCg2Value$(mb2Match)
						! pr 'b>>';mbCg2Label$(mb2Match)&'='&mbCg2Value$(mb2Match)
						mbCg2Label$(mb2Match)=''
						! pr 'customer number '&mbF1CustomerNumbers$(mbX)&' has different data in both files for '&mbCg1Label$(mb1x)
						! pr '  file 1: "'&mbCg1Value$(mb1x)&'"'
						! pr '  file 2: "'&mbCg2Value$(mb2Match)&'"'
						! pr '  The value from File 2 will override the value from File 1.'
						! pr '  type GO and press Enter to continue'
						! fnPause
					end if
					mbCg1Label$(mb1x)=''
				end if
				! fnPause
			nex mb1x
			for mb2x=2 to udim(mbCg2Label$)
				if trim$(mbCg2Label$(mb2x))<>'' then
					pr #hMergeNew: mbCg2Label$(mb2x)&'='&mbCg2Value$(mb2x)
					! pr 'c>>';mbCg2Label$(mb2x)&'='&mbCg2Value$(mb2x)
				end if
			nex mb2x
			mbF2which=srch(mat mbF2CustomerNumbers$,mbF1CustomerNumbers$(mbX))
			if mbF2which>0 then mbF2CustomerNumbers$(mbF2which)=''
			mbF1CustomerNumbers$(mbX)=''
		else
			for mb1x=2 to udim(mbCg1Label$)
					pr #hMergeNew: mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
					! pr 'd>>';mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
			nex mb1x
		end if
		! fnPause
		pr #hMergeNew: ''
	nex mbX
	for mbX=1 to udim(mat mbF2CustomerNumbers$)
		if mbF2CustomerNumbers$(mbX)<>'' then
			pr 'adding '&mbF2CustomerNumbers$(mbX)&' from the second file that was not in the first file.'
			! fnPause
			cg2Count=fn_getCustomerGroup(mat mbF2Label$,mat mbF2Value$,mbF2CustomerNumbers$(mbX),mat mbCg2Label$,mat mbCg2Value$)
			pr #hMergeNew: 'Customer.Number='&mbF2CustomerNumbers$(mbX)
			for mb1x=2 to udim(mat mbCg2Label$)
				if trim$(mbCg2Label$(mb1x))<>'' then
					pr #hMergeNew: mbCg2Label$(mb1x)&'='&mbCg2Value$(mb1x)
				end if
			nex mb1x
		end if
	nex mbX
	close #hMergeNew:
	fncopy(mbTmpNewFile$,mbFile2$)
	! fndel(mbTmpNewFile$)
fnend

def fn_getCustomerNumbers(mat gcnLabel$,mat gcnValue$,mat gcnCustomerNumbers$)
	gcnMatch=0
	mat gcnCustomerNumbers$(0)
	do
		gcnMatch=srch(mat gcnLabel$,'Customer.Number',gcnMatch+1)
		if gcnMatch then
			fnaddonec(mat gcnCustomerNumbers$,trim$(gcnValue$(gcnMatch)),1,1)
		end if
	loop until gcnMatch<=0
fnend
def fn_getCustomerGroup(mat gcgFromLabel$,mat gcgFromValue$,gcgCustomerNumbers$,mat gcgToLabel$,mat gcgToValue$)
	mat gcgToLabel$(0)
	mat gcgToValue$(0)
	gcgReturn=0
	gcgIndex=0
	gcgTop: !
	gcgIndex=srch(mat gcgFromValue$,gcgCustomerNumbers$, gcgIndex+1)
	if gcgIndex<=0 then
		pr 'could not find gcgCustomerNumbers$="'&gcgCustomerNumbers$&'"'
		! for x=1 to udim(mat gcgFromValue$) : if gcgFromValue$(x)<>'' then pr x;gcgFromValue$(x) : nex x
		! fnPause
	else
		if gcgFromLabel$(gcgIndex)<>'Customer.Number' then goto gcgTop
		do
			gcgReturn+=1
			fnaddonec(mat gcgToLabel$,gcgFromLabel$(gcgIndex))
			fnaddonec(mat gcgToValue$,gcgFromValue$(gcgIndex))
			gcgIndex+=1
		loop until gcgIndex>=udim(mat gcgFromLabel$) or gcgFromLabel$(gcgIndex)='Customer.Number'
	end if
	fn_getCustomerGroup=gcgReturn
fnend
def fn_setup
	autoLibrary
	on error goto Ertn
	dim preferenceHandHeldFromFile$*128
	fnureg_read('Hand Held From File',preferenceHandHeldFromFile$, '[ask]')
	if deviceSelected$="EZReader" then preferenceHandHeldFromFile$='[ask]'
	dim ml$(2)*256
	dim resp$(32)*256
	dim amr$*619
	dim hersey$*290
	dim askPath$*128
	dim line$*2048       ! temp variable for reading in lines before they are parsed.
	dim lineItem$(0)*256 ! temp variable for reading in lines before they are parsed.
	tab$=chr$(9)
	dim devicePreference$*20
	devicePreference$=fnhand_held_Device$ ! fn_ctext_setup

	if fn_hh_input_filename$='' then
		preferenceHandHeldFromFile$='[ask]'
		! fnureg_write('Hand Held From File','[ask]')
	end if


	if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
		fnureg_read('Hand Held From File Asked',askPath$)
	end if
		fnureg_read('Hand Held From File Asked',askPath$)
	dim deviceSelected$*20
	if lwrc$(devicePreference$)='[ask]' then
		fnureg_read('Hand Held Device Asked',deviceSelected$)
		dim deviceOption$(0)*20
		fnHandHeldList(mat deviceOption$)
	end if
fnend
include: Ertn
