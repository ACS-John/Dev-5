fn_setup
fntop(program$)
!   ! r: restore unconverted files and remove already converted files (for testing only, of course)
!      if env$('acsDeveloper')<>'' and env$('client')='Campbell' then
!   !   exec 'copy "C:\ACS\(Client_Files)\Bethany\ACS meter location mess\autosave before first one\UB Company 1 2018-01-02 14-02-30 Menu - before meter location initialize\Meter*.h1" "[Q]\UBmstr\*.h[cno]"'
!       forceKeepLeft=1
!       exec 'free "[Q]\UBmstr\MeterLocation*.h[cno]"' ioerr ignore
!       fn_populateLocationNonSeq
!   !   exec 'free "[Q]\UBmstr\MeterAddress*.h[cno]"' ioerr ignore
!   !   fncreg_write('u4 meter location account numbers left justified','False')
!      end if
!  if forceKeepLeft then  pr 'forceKeepLeftCount=';forceKeepLeftCount : pause : end ! /r
fnHamsterFio(table$)
XIT: !
fnxit
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fntop,fnxit
		library 'S:\Core\Library': fngethandle,fnerror
		library 'S:\Core\Library': fnindex_it,fnHamsterFio
		library 'S:\Core\Library': fnStatusClose,fnStatus
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnmsgbox
		library 'S:\Core\Library': fnOpenFile,fnCloseFile
		library 'S:\Core\Library': fnAutomatedSavePoint
		library 'S:\Core\Library': fnreg_read
		library 'S:\Core\Library': fncreg_read,fncreg_write
		library 'S:\Core\Library': fnget_services,fnGetServiceCodesMetered
		library 'S:\Core\Library': fnBuildKey$
		library 'S:\Core\Library': fnKeyExists
		library 'S:\Core\Library': fnLastBillingDate
		library 'S:\Core\Library': fnCustomerData$
		library 'S:\Core\Library': fnFree,fnRename
		library 'S:\Core\Library': fnlbl,fntos,fnacs,fntxt,fncmdset,fncombof
		library 'S:\Core\Library': fncmdkey,fnflexinit1,fnflexadd1
		library 'S:\Core\Library': fnapplyDefaultRatesFio
		dim info$(0)*20,infoN(0)
		dim addr$(0)*30,addrN(0)
		dim location$(0)*256,locationN(0)
		dim mg$(0)*128
		dim serviceName$(10)*60,serviceCode$(10)*2
		dim resp$(128)*128
		fnget_services(mat serviceName$, mat serviceCode$)
		for snI=1 to udim(mat serviceName$) : serviceName$(snI)=trim$(serviceName$(snI)) : nex snI
		table$='U4 Meter Location'
		fnGetServiceCodesMetered(mat serviceCodeMetered$)
	end if
	fnLastBillingDate(lastBillingDate)
	fnreg_read('Meter Location Id Sequential',u4_meterLocationIdSequential$, 'True')
	if exists('[Q]\UBmstr\Meter.h[cno]') or ~exists('[Q]\UBmstr\MeterLocation.h[cno]') then let fn_InitialializeMeterLocation
fnend

def fn_populateLocationNonSeq
	dim cus$(0)*256,cusN(0)
	hCustomer=fn_open('UB Customer',mat cus$,mat cusN,mat form$)
	hLocation=fn_open(table$,mat location$,mat locationN,mat form$, 0,1)
	do
		read #hCustomer,using form$(hCustomer): mat cus$,mat cusN eof PlnsEoCustomer
		fnapplyDefaultRatesFio(mat cusN)
		if lastBillingDate=cusN(c_lastBillingDate) and cusN(c_finalBilling)=0 then
			for serviceItem=1 to udim(mat serviceCode$)
				if srch(mat serviceCodeMetered$,serviceCode$(serviceItem))>0 then
					locationN(loc_locationID    )=fn_newLocationIdNonSequential(cus$(c_account))
					location$(loc_name          )=cus$(c_meterAddress)
					location$(loc_activeCustomer)=trim$(cus$(c_account))
					location$(loc_serviceId     )=serviceCode$(serviceItem)
					location$(loc_longitude     )=''
					location$(loc_latitude      )=''
					location$(loc_meterNumber   )=cus$(c_s1meterNumber)
					location$(loc_transmitter   )=''
					! if env$('client')='Campbell' then
						location$(loc_meterType     )='1'    ! probably all auto-conversions could just default to a meterType of 1
					! else
					!   location$(loc_meterType     )=''
					! end if
					fnLocationWrite(mat location$,mat locationN, 1)
				end if
			nex serviceItem
		end if
	loop
	PlnsEoCustomer: !
	fnCloseFile(hLocation,table$)
	fncloseFile(hCustomer,'UB Customer')
fnend

def library fnInitialializeMeterLocation
	if ~setup then let fn_setup
	! fnInitialializeMeterLocation=fn_InitialializeMeterLocation    <---  fn_setup handles it if it is necessary.
fnend
def fn_InitialializeMeterLocation
	imlCreateNew  = imlImportFromInfo = 0
	deleteEnabled = 0
	if ~exists('[Q]\UBmstr\MeterLocation.h[cno]') then
		imlCreateNew=1
	end if
	if exists('[Q]\UBmstr\Meter.h[cno]') then
		imlImportFromInfo=1
	end if
	if imlCreateNew or imlImportFromInfo then
		fnAutomatedSavePoint('before U4 Initialize Meter Location')
		hInfo=fn_open('UB Meter Info',mat info$,mat infoN,mat form$)
		hLocation=fn_open(table$,mat location$,mat locationN,mat form$)
		fnCloseFile(hLocation,table$)
		fnindex_it('[Q]\UBmstr\MeterLocation.h[cno]','[Q]\UBmstr\MeterLocationIdx2.h[cno]', '12 30u')
	end if
	hLocation=fn_open(table$,mat location$,mat locationN,mat form$, 0,2)
	fncreg_read('u4 meter location clean zeros from Location ID',umlCleanZeroLocationId$,'True')
	fncreg_read('u4 meter location account numbers left justified',umlCustomerLeftJustified$,'False')
	if ~imlCreateNew and umlCustomerLeftJustified$='False' or umlCleanZeroLocationId$='True' then
		do
			read #hLocation,using form$(hLocation): mat location$,mat locationN eof LjEoLocation   !   ,release removed on 11/27/19 because following rewrite got an error 62
			if umlCustomerLeftJustified$='False' then
				location$(loc_activeCustomer)=trim$(location$(loc_activeCustomer))
			end if
			if umlCleanZeroLocationId$='True' then
				if locationN(loc_locationId)=0 then
					if u4_meterLocationIdSequential$='True' then
						locationN(loc_locationId)=fn_newLocationIdSequential
					else
						locationN(loc_locationId)=fn_newLocationIdNonSequential(account$)
					end if
				end if
			end if
			if locationN(loc_locationId)=0 then pr 'AAA - about to write a ZERO location Id' : pause
			rewrite #hLocation,using form$(hLocation): mat location$,mat locationN
		loop
		LjEoLocation: !
		fncreg_write('u4 meter location account numbers left justified','True')
		restore #hLocation:
	end if
	if exists('[Q]\UBmstr\MeterAddress.h[cno]') then
		! r: import UB Meter Address (and subordinate UB Meter Info data into U4 Meter Location)
			fnStatus('Initializing U4 Meter Location table...')
			hAddress=fn_open('UB Meter Address',mat addr$,mat addrN,mat form$, 0,2)
			fnStatus('Record Count of UB Meter Address: '&str$(lrec(hAddress)))
			dim loacationRecordsAdded(11)
			mat loacationRecordsAdded=(0)
			fnStatus('Record Count of UB Meter Info: '&str$(lrec(hInfo)))
			do
				mat location$=('') : mat locationN=(0)
				read #hAddress,using form$(hAddress): mat addr$,mat addrN eof EoAddress
				locationId=addrN(loc_LocationID)
				account$=trim$(fn_accountFromLocIdViaLocation$(locationId, 1))
				if account$='' then
					fnStatus('No account found for Location ID '&str$(locationId)&' from Address file.')
				else
					locationN(loc_locationID     )=locationId
					location$(loc_name           )=addr$(ma_Name)
					location$(loc_activeCustomer )=account$
					servicesFound=0
					for serviceItem=1 to udim(mat serviceName$)
						if serviceCode$(serviceItem)<>'' then
							if ~imlImportFromInfo then goto InfoNokey
							mat info$=('') : mat infoN=(0)
							info$(meter_customer )=trim$(account$)
							info$(meter_serviceId)=serviceCode$(serviceItem)
							read #hInfo,using form$(hInfo),key=fnBuildKey$('UB Meter Info',mat info$,mat infoN): mat info$,mat infoN nokey InfoNokey
							servicesFound+=1
							location$(loc_serviceId      )=info$(meter_serviceId      )
							location$(loc_longitude      )=info$(meter_longitude      )
							location$(loc_latitude       )=info$(meter_latitude       )
							location$(loc_meterNumber    )=info$(meter_meterNumber    )
							location$(loc_transmitter    )=info$(meter_transmitter    )
							location$(loc_meterType      )=info$(meter_meterType      )
							fnstatus('importing '&account$&'.'&location$(loc_serviceId)&'.'&str$(locationId)&': ')
							fnLocationWrite(mat location$,mat locationN)
							loacationRecordsAdded(serviceItem)+=1
							if deleteEnabled then delete #hInfo:
							InfoNokey: !
						end if
					next serviceItem
					if servicesFound=0 then
						if udim(mat serviceCodeMetered$)=1 then ! only one metered service, it is safe to assume
							location$(loc_serviceId      )=serviceCodeMetered$(1)
						else
							pr 'no locations found for "'&account$&'"- just write a record without any services'
							pause
						end if
						fnLocationWrite(mat location$,mat locationN)
						loacationRecordsAdded(11)+=1
					end if
					if deleteEnabled then delete #hAddress:
				end if
			loop
			EoAddress: !
			for x=1 to 10
				if serviceName$(x)<>'' and loacationRecordsAdded(x)>0 then
					fnStatus('Imported '&str$(loacationRecordsAdded(x))&' records '&serviceName$(x)&' from Info (with added data from Address)')
				end if
			nex x
			if loacationRecordsAdded(11) then
				fnStatus('Imported '&str$(loacationRecordsAdded(11))&' records  with NO service from Info.')
			end if
		! /r
	end if
	if imlImportFromInfo then
		! r: import from Info only - if you're importing both, do address first, because it add's this info too, this one is to add whatever is left after the other one.  it still leaves ones with accounts which do not point to a customer record.
			fnStatus('checking Meter Information file for valid data to migrate to Meter Location table')
			open #hCustomerOutinUnused:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outin,keyed
			open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
			restore #hInfo:
			do
				read #hInfo,using form$(hInfo): mat info$,mat infoN eof EoInfo
				dim Customer_MeterAddress$*30
				Customer_MeterAddress$=''
				account$=lpad$(trim$(info$(meter_customer)),kln(hCustomer))
				if trim$(account$)<>'' then
					read #hCustomer,using 'form pos 11,c 30',key=account$: Customer_MeterAddress$ nokey NextInfoRecord
					mat location$=('') : mat locationN=(0)
					locationN(loc_locationID     )=0
					location$(loc_name           )=Customer_MeterAddress$
					location$(loc_activeCustomer )=info$(meter_customer)
					location$(loc_serviceId      )=info$(meter_serviceId      )
					location$(loc_longitude      )=info$(meter_longitude      )
					location$(loc_latitude       )=info$(meter_latitude       )
					location$(loc_meterNumber    )=info$(meter_meterNumber    )
					location$(loc_transmitter    )=info$(meter_transmitter    )
					location$(loc_meterType      )=info$(meter_meterType      )
					! fnStatus('creating new location from '&account$&' - '&location$(loc_serviceId)&' meter address file')
					fnLocationWrite(mat location$,mat locationN)
					if deleteEnabled then delete #hInfo:
				end if
				NextInfoRecord:!
			loop
			EoInfo: !
		! /r
		fn_accountFromLocIdViaLocation$(1, 0) ! close the files it had opened previously
		fnCloseFile(hAddress,table$)
		fnCloseFile(hInfo,'UB Meter Info')
		if exists('[Q]\UBmstr\Meter(old).h[cno]') then
			fnFree('[Q]\UBmstr\Meter.h[cno]')
		else
			fnRename('[Q]\UBmstr\Meter.h[cno]','[Q]\UBmstr\Meter(old).h[cno]')
		end if
		fnFree('[Q]\UBmstr\Meter_Idx.h[cno]')
		! if env$('acsDeveloper')<>'' then let fnStatusPause
		fnStatusClose
	end if
	fn_InitialializeMeterLocation=hLocation
fnend
def fnLocationWrite(mat location$,mat locationN; leaveFileOpen) ! inherits local dim form$
	if ~hLocation(1) then ! r:
		hLocation(1)=fn_open(table$,mat location$,mat locationN,mat form$)
		for lwIndex=2 to 5
			hLocation(lwIndex)=hLocation(lwIndex-1)+1
		nex lwIndex
	end if ! /r
	dim locRead$(0)*256
	dim locReadN(0)
	mat locRead$(udim(mat location$))
	mat locReadN(udim(mat locationN))
	mat locRead$=('')
	mat locReadN=(0)
	dim lwKey$*128
	for lwIndex=4 to 5 ! check activeCustomer/serviceId and locationID/serviceId
		lwKey$=fnBuildKey$(table$,mat location$,mat locationN, lwIndex)
		read #hLocation(lwIndex),using form$(hLocation(lwIndex)),key=lwKey$,release: mat locRead$,mat locReadN nokey LwNoKeyEncountered
		if fn_AllStringsMatch(mat locRead$,mat location$, 1) then
			pr 'duplicate add detected.  record already exist.'
			pr 'delete source to remove this message.' ! pause
			goto LwFinis
		else if locationN(loc_locationID)=0 and locRead$(loc_serviceId)<>location$(loc_serviceId) then
			! it's a new service for an existing location
			if u4_meterLocationIdSequential$='True' then
				locationN(loc_locationID)=fn_newLocationIdSequential
			else
				locationN(loc_locationID)=fn_newLocationIdNonSequential(location$(loc_activeCustomer))
			end if
			goto LwWrite
		else if location$(loc_locationID)=locRead$(loc_locationID) and location$(loc_serviceId)=locRead$(loc_serviceId) then
			for lwLocItem=1 to udim(mat locRead$)
				if fn_leftIsSuperior(locRead$(lwLocItem),location$(lwLocItem), lwLocItem==loc_activeCustomer) then location$(lwLocItem)=locRead$(lwLocItem)
			nex lwLocItem
		else if locationN(loc_locationID)=0 then
			pr 'new location being added, but it already exists - gather any new info on it'
			locationN(loc_locationID)=locReadN(loc_locationID)
			for lwLocItem=1 to udim(mat locRead$)
				if fn_leftIsSuperior(locRead$(lwLocItem),location$(lwLocItem), lwLocItem==loc_activeCustomer) then location$(lwLocItem)=locRead$(lwLocItem)
			nex lwLocItem
			goto LwRewrite
		else
			gosub LwKeyMatchDisplay
			! if locationN(loc_locationId)=100590 then pr ' POINT B 100590 found - about to write.' : pause
			if lisReturn=1 then goto LwFinis ! it is already written, skip writing the new one
			if lisReturn=0 then goto LwRewrite ! it is already written, overwrite it
			pr 'impossible lisReturn value' : pause : end
		end if
		LwNoKeyEncountered: !
	nex lwIndex
	goto LwWrite
	!
	LwRewrite: ! r:
		if locationN(loc_locationID)=0 then
			pr 'attempted to rewrite a record setting its locationID to 0'
			pause
		else
			lwKey$=fnBuildKey$(table$,mat locRead$,mat locReadN, 1)
			if locationN(loc_locationId)=0 then pr 'BBB - about to write a ZERO location Id' : pause
			rewrite #hLocation(1),using form$(hLocation(1)),key=lwKey$: mat location$,mat locationN
		end if
	goto LwFinis ! /r
	!
	LwWrite: ! r:
		if locationN(loc_locationID)=0 then
			if u4_meterLocationIdSequential$='True' then
				locationN(loc_locationID)=fn_newLocationIdSequential
			else
				locationN(loc_locationID)=fn_newLocationIdNonSequential(account$)
			end if
		end if
		if locationN(loc_locationId)=0 then pr 'CCC - about to write a ZERO location Id' : pause
		write #hLocation(1),using form$(hLocation(1)): mat location$,mat locationN
	goto LwFinis ! /r
	LwFinis: !
	if ~leaveFileOpen then
		fnCloseFile(hLocation(1),table$)   !  <--  does this close them all?  too many?
		mat hLocation=(0)
	end if
fnend
def fn_leftIsSuperior(left$*128,right$*128; isAccountNumber)
	lisReturn=0
	left$=trim$(left$)
	right$=trim$(right$)
	if left$=right$ then
		lisReturn=0
	else if left$<>'' and right$='' then
		lisReturn=1
	else if left$='' and right$<>'' then
		lisReturn=0
	else if uprc$(right$)=uprc$(left$) then
		if uprc$(right$)=uprc$(right$) then
			lisReturn=1
		else
			lisReturn=0
		end if
	else if isAccountNumber then
		leftFinalBillingCode$=fnCustomerData$(left$,'final billing code')
		rightFinalBillingCode$=fnCustomerData$(right$,'final billing code')
		if leftFinalBillingCode$<>'' and rightFinalBillingCode$='' then
			lisReturn=0
		else if leftFinalBillingCode$='' and rightFinalBillingCode$<>'' then
			lisReturn=1
		else
			leftLastBillingDay=val(fnCustomerData$(left$,'last billing day'))
			rightLastBillingDay=val(fnCustomerData$(right$,'last billing day'))
			if leftLastBillingDay>rightLastBillingDay then
				lisReturn=1
			else if leftLastBillingDay<rightLastBillingDay then
				lisReturn=0
			else
				pr 'not sure which ACCOUNT NUMBER is superior:  "'&left$&'" or "'&right$&'"'
				pr 'final billing codes are '&leftFinalBillingCode$&' and '&rightFinalBillingCode$&'.'
				pr 'last billing date on both are '&date$(leftLastBillingDay,'mm/dd/ccyy')&'.'
				gosub LwKeyMatchDisplay
				lisReturn=0
			end if
		end if
		!
	else
		pr ' not sure which is superior:  "'&left$&'" or "'&right$&'"'
		gosub LwKeyMatchDisplay
		lisReturn=0
	end if
	fn_leftIsSuperior=lisReturn
fnend
LwKeyMatchDisplay: ! r: returns lisReturn (1=left is superior) lisReturn=1 means the already written record (mat locRead$,mat locReadN) is superior to passed record (mat location$, mat locationN)
	fntos(sn$='LwKeyMatchDisplay') : lc=0
	fnlbl(lc+=1,1,'key match found on index '&str$(lwIndex))
	fnlbl(lc+=1,1,'Data Comparison')
	fn_lwCompareLine('Location ID          :',str$(locReadN(loc_locationID     )),str$(locationN(loc_locationID     )))
	fn_lwCompareLine('Meter Address        :',locRead$(loc_name           ),location$(loc_name           ))
	fn_lwCompareLine('Current Customer     :',locRead$(loc_activeCustomer ),location$(loc_activeCustomer ))
	fn_lwCompareLine('Service ID           :',locRead$(loc_serviceId      ),location$(loc_serviceId      ))
	fn_lwCompareLine('Longitude            :',locRead$(loc_longitude      ),location$(loc_longitude      ))
	fn_lwCompareLine('Latitude             :',locRead$(loc_latitude       ),location$(loc_latitude       ))
	fn_lwCompareLine('Meter Number         :',locRead$(loc_meterNumber    ),location$(loc_meterNumber    ))
	fn_lwCompareLine('Transmitter Number   :',locRead$(loc_transmitter    ),location$(loc_transmitter    ))
	fn_lwCompareLine('Meter Type           :',locRead$(loc_meterType      ),location$(loc_meterType      ))
	fnlbl(lc+=1,1,'what now?')
	fncmdkey('Keep Left',2)
	fncmdkey('Keep Right',4)
	if forceKeepLeft then
		lisReturn=1
		forceKeepLeftCount+=1
	else
		fnacs(sn$,0,mat resp$,ckey)
		if ckey=2 then lisReturn=1 else lisReturn=0
	end if
return ! /r
def fn_lwCompareLine(label$*128,valueLeft$*128,valueRight$*128)
	if rtrm$(valueLeft$)<>rtrm$(valueRight$) then
		fnlbl(lc+=1,1,label$&' (DIFF) "'&rpad$(valueLeft$&'"',30)&' vs "'&rpad$(valueRight$&'"',30)&'   (DIFF)')
	else
		fnlbl(lc+=1,1,label$&' (same) '&valueLeft$)
	end if
fnend
def fn_AllStringsMatch(mat a$,mat b$; caseInsensitive)
	asmReturn=asmMatchCount=0
	if udim(mat a$)=udim(mat b$) then
		for asmItem=1 to udim(mat a$)
			if caseInsensitive then
				if rtrm$(lwrc$(a$(asmItem)))=rtrm$(lwrc$(b$(asmItem))) then asmMatchCount+=1
			else
				if rtrm$(a$(asmItem))=rtrm$(b$(asmItem)) then asmMatchCount+=1
			end if
		nex asmItem
	else
		pr ' arrays are different size'
		pause
	end if
	if asmMatchCount=udim(mat a$) then
		asmReturn=1
	end if
	fn_AllStringsMatch=asmReturn
fnend
def library fnAccountFromLocationId$*10(locationId; leaveFileOpen)
	if ~setup then let fn_setup
	fnAccountFromLocationId$=fn_accountFromLocIdViaLocation$(locationId, leaveFileOpen)
fnend
def fn_accountFromLocIdViaLocation$(locationId; leaveFileOpen)
	aliReturn$=''
	dim location$(0)*128,locationN(0)
	if ~hAliLocation then hAliLocation=fn_open(table$,mat location$,mat locationN,mat form$, 1)
	mat location$=('')
	mat locationN=(0)
	locationN(loc_locationId)=locationId
	read #hAliLocation,using form$(hAliLocation),key=fnBuildKey$(table$,mat location$,mat locationN): mat location$,mat locationN nokey ignore
	aliReturn$=lpad$(trim$(location$(loc_activeCustomer)),10)
	if ~leaveFileOpen then
		close #hAliLocation:
		hAliLocation=0
	end if
	fn_accountFromLocIdViaLocation$=aliReturn$
fnend
def library fnLocationIdFromAccountAndServ$*30(account$*10,serviceId$*2; field$*14,leaveFileOpen)
	if ~setup then let fn_setup
	if ~hLfaLocation then hLfaLocation=fn_open(table$,mat location$,mat locationN,mat form$, 1,4)
	dim lfaReturn$*30
	lfaReturn$=''
	if field$='' then field$='LocationId'
	field$=lwrc$(field$)
	mat location$=('')
	mat locationN=(0)
	locationKey$=rpad$(trim$(account$),kln(hLfaLocation,1))&rpad$(trim$(serviceId$),kln(hLfaLocation,2))
	read #hLfaLocation,using form$(hLfaLocation),key=locationKey$: mat location$,mat locationN nokey ignore
	if field$='locationid' then
		lfaReturn$=str$(locationN(loc_locationID))
	else if field$='name' then
		lfaReturn$=location$(loc_name          )
	else if field$='activecustomer' then
		lfaReturn$=location$(loc_activeCustomer)
	else if field$='serviceid' then
		lfaReturn$=location$(loc_serviceId     )
	else if field$='longitude' then
		lfaReturn$=location$(loc_longitude     )
	else if field$='latitude' then
		lfaReturn$=location$(loc_latitude      )
	else if field$='meternumber' then
		lfaReturn$=location$(loc_meterNumber   )
	else if field$='transmitter' then
		lfaReturn$=location$(loc_transmitter   )
	else if field$='metertype' then
		lfaReturn$=location$(loc_meterType     )
	else
		pr 'meter location field ('&field$&') not recognized.'
		pause
	end if
	if ~leaveFileOpen then
		close #hLfaLocation:
		hLfaLocation=0
	end if
	fnLocationIdFromAccountAndServ$=lfaReturn$
fnend
def library fnMeterAddressLocationID(meterAddress$*30; leaveFileOpen) ! returns the locationID for a provided meterAddress$
	if ~setup then let fn_setup
	if leaveFileOpen and hMaLocationByName<>0 then goto maliPastOpen
	dim location$(0)*128,locationN(0),locationKey$*128
	hMaLocationByName=fn_open(table$,mat location$,mat locationN,mat form$, 1,2)
	maliPastOpen: !
	locationN(loc_LocationID)=-1
	read #hMaLocationByName,using form$(hMaLocationByName),key=rpad$(meterAddress$,KLN(hMaLocationByName)),release: mat location$,mat locationN nokey ignore
	if ~leaveFileOpen then
		close #hMaLocationByName:
		hMaLocationByName=0
	end if
	fnMeterAddressLocationID=locationN(loc_LocationID)
fnend
def library fnMeterAddressName$*30(locationId; leaveFileOpen) ! returns the meterAddress$ for a provided LocationID
	if ~setup then let fn_setup
	if leaveFileOpen and hMaLocationByLocationId<>0 then goto manPastOpen
	hMaLocationByLocationId=fn_open(table$,mat location$,mat locationN,mat form$, 1)
	manPastOpen: !
	locationN(loc_LocationID)=-1
	read #hMaLocationByLocationId,using form$(hMaLocationByLocationId),key=cnvrt$('N 11',locationId),release: mat location$,mat locationN nokey ignore
	if ~leaveFileOpen then
		close #hMaLocationByLocationId:
		hMaLocationByLocationId=0
	end if
	fnMeterAddressName$=location$(loc_name)
fnend
def fn_newLocationIdNonSequential(account$)
	! if env$('client')='Campbell' then
	newLocationIdNonSequential=val(fnCustomerData$(account$,'route'))*100000+val(fnCustomerData$(account$,'sequence'))
	! end if
	fn_newLocationIdNonSequential=newLocationIdNonSequential
fnend

def fn_newLocationIdSequential(; alterAmount)
	if alterAmount=0 then let alterAmount=1
	fncreg_read('Last Location ID Assigned',nliLastLocation$)
	nliLastLocation=val(nliLastLocation$)
	nliLastLocation+=alterAmount
	fncreg_write('Last Location ID Assigned',str$(nliLastLocation))
	fn_newLocationIdSequential=nliLastLocation ! pr 'fn_newLocationIdSequential is returning ';nliLastLocation
fnend
! def fn_askAddNew$(meterAddressBefore$*30,meterAddressAfter$*80) r: unused fns
!   mat mg$(0)
!   fnAddOneC(mat mg$,'The Meter Address was changed from')
!   fnAddOneC(mat mg$,'From: "'&meterAddressBefore$&'"')
!   fnAddOneC(mat mg$,'  To: "'&meterAddressAfter$&'"')
!   fnAddOneC(mat mg$,'')
!   fnAddOneC(mat mg$,'Is this a new entry?')
!   fnAddOneC(mat mg$,'')
!   fnAddOneC(mat mg$,'  Yes    - Add an entry to Meter Address file')
!   fnAddOneC(mat mg$,'  No     - Update previous entry in Meter Address file')
!   fnAddOneC(mat mg$,'  Cancel - Revert Changes')
!   fnmsgbox(mat mg$, aaResponse$, '', 3) ! mtype 3 is yes/no/cancel
!   fn_askAddNew$=aaResponse$
! fnend
! def fn_askAddDuplicate$(meterAddressBefore$*30,meterAddressAfter$*80)
!   mat mg$(0)
!   fnAddOneC(mat mg$,'The new Meter Address entered already exist.')
!   fnAddOneC(mat mg$,'')
!   fnAddOneC(mat mg$,'Do you want to continue?')
!   fnAddOneC(mat mg$,'')
!   fnAddOneC(mat mg$,'  Yes    - use "'&meterAddressAfter$&'" as entered.')
!   fnAddOneC(mat mg$,'  No     - revert to "'&meterAddressBefore$&'"')
!   fnmsgbox(mat mg$, aaResponse$, '', 4) ! mtype 4 is yes/no
!   fn_askAddDuplicate$=aaResponse$
! fnend  /r

def library fnCustomerMeterLocationSelect(account$*10,serviceCode$*2) ! cmls
	if ~setup then let fn_setup
	hCmlsLocation(1)=fn_open(table$,mat location$,mat locationN,mat form$)
	for j=2 to 5 : hCmlsLocation(j)=hCmlsLocation(1)+j-1 : nex j
	CmlsSelect: !
	fntos(sn$='cmls'&account$) : respc=0
	fnlbl(1,1,'Account: ',20,1)
	fntxt(1,22,10, 0,0,'',1)
	resp$(respc+=1)=account$
	fnlbl(3,1,'Select '&serviceCode$&' Meter Location for Account')
	dim cmlsFlexItem$(8)*128
	cmlsFlexItem$(1)='Location ID       '
	cmlsFlexItem$(2)='Meter Address     '
	cmlsFlexItem$(3)='Current Customer  '
	cmlsFlexItem$(4)='Longitude         '
	cmlsFlexItem$(5)='Latitude          '
	cmlsFlexItem$(6)='Meter Number      '
	cmlsFlexItem$(7)='Transmitter Number'
	cmlsFlexItem$(8)='Meter Type        '
	fnflexinit1('locationSelect',4,1,20,20,mat cmlsFlexItem$) : cmlsFlexCount=0
	do
		read #hCmlsLocation(5),using form$(hCmlsLocation(1)): mat location$,mat locationN eof CmlsEoLocation
		if location$(loc_serviceId)=serviceCode$ then
			cmlsFlexItem$(1)=str$(locationN(loc_LocationID))
			cmlsFlexItem$(2)=location$(loc_name           )
			cmlsFlexItem$(3)=location$(loc_activeCustomer )
			cmlsFlexItem$(4)=location$(loc_longitude      )
			cmlsFlexItem$(5)=location$(loc_latitude       )
			cmlsFlexItem$(6)=location$(loc_meterNumber    )
			cmlsFlexItem$(7)=location$(loc_transmitter    )
			cmlsFlexItem$(8)=location$(loc_meterType      )
			fnflexadd1(mat cmlsFlexItem$) : cmlsFlexCount+=1
		end if
	loop
	CmlsEoLocation: !
	if cmlsFlexCount=0 then
		ckey=2
		cmlsAddForceServiceId$=serviceCode$
		gosub CmlsAdd
		if ckey=5 then goto CmslFinis
		goto CmlsSelect
	end if
	fncmdkey('Select',1,1,0)
	fncmdkey('New',2,0,0)
	fncmdkey('Cancel',5,0,1)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey<>5 then
		cmlsSelectedLocationId=val(resp$(2))
		if ckey=1 then
			mat location$=('')
			mat locationN=(0)
			locationN(loc_locationID)=cmlsSelectedLocationId
			location$(loc_serviceId)=serviceCode$
			dim cmlsLocationKey$*128
			cmlsLocationKey$=fnBuildKey$(table$,mat location$,mat locationN, 5)
			read #hCmlsLocation(5),using form$(hCmlsLocation(1)),key=cmlsLocationKey$,release: mat location$,mat locationN
			if trim$(location$(loc_activeCustomer))='' and trim$(location$(loc_activeCustomer))<>trim$(account$) then
				location$(loc_activeCustomer)=trim$(account$)
				rewrite #hCmlsLocation(5),using form$(hCmlsLocation(1)),key=cmlsLocationKey$: mat location$,mat locationN
			else if trim$(location$(loc_activeCustomer))<>trim$(account$) then
				mat mg$(0)
				fnAddOneC(mat mg$,'Location ID '&str$(cmlsSelectedLocationId)&' currently belongs to customer '&trim$(location$(loc_activeCustomer)))
				fnAddOneC(mat mg$,'Are you sure you want to change the active customer to '&trim$(account$)&'?')
				fnmsgbox(mat mg$,resp$,'',32+4)
				if resp$='Yes' then
					fn_purgeSrvAccountFromLocation(serviceCode$,account$)
					location$(loc_activeCustomer)=trim$(account$)
					rewrite #hCmlsLocation(5),using form$(hCmlsLocation(1)),key=cmlsLocationKey$: mat location$,mat locationN
				else
					release #hCmlsLocation(5):
					goto CmlsSelect
				end if
			end if
		else if ckey=2 then
			gosub CmlsAdd
		end if
	end if
	CmslFinis: !
	fnclosefile(hCmlsLocation(1),table$)
fnend
CmlsAdd: ! r: returns ckey, optionally accepts cmlsAddForceServiceId$, requires a whole lot of local stuff
	if u4_meterLocationIdSequential$='True' then
		locationN(loc_locationID    )=fn_newLocationIdSequential
	else                          
		locationN(loc_locationID    )=fn_newLocationIdNonSequential(account$)
	end if                        
	location$(loc_name          )=''
	location$(loc_activeCustomer)=trim$(account$)
	location$(loc_serviceId     )=serviceCode$
	location$(loc_longitude     )=''
	location$(loc_latitude      )=''
	location$(loc_meterNumber   )=''
	location$(loc_transmitter   )=''
	location$(loc_meterType     )=''
	CmlsAddReEdit: !
	fntos(sn$='LocationAdd') : lc=respc=0
	if u4_meterLocationIdSequential$='True' then
		fnlbl(lc+=1,1,'Location ID     ', 20,1) : fntxt(lc,22,11, 0,0,'',1,'') : resp$(respc+=1)=str$(locationN(loc_locationID    ))
	else
		fnlbl(lc+=1,1,'Location ID     ', 20,1) : fntxt(lc,22,11, 0,0,'',0,'') : resp$(respc+=1)=str$(locationN(loc_locationID    ))
	end if
	fnlbl(lc+=1,1,'Meter Address     ', 20,1) : fntxt(lc,22,30, 0,0,'',0,'') : resp$(respc+=1)=location$(loc_name          )
	fnlbl(lc+=1,1,'Current Customer  ', 20,1) : fntxt(lc,22,10, 0,0,'',1,'') : resp$(respc+=1)=location$(loc_activeCustomer)
	fnlbl(lc+=1,1,'Service ID        ', 20,1) : fntxt(lc,22, 2, 0,0,'',1,'') : resp$(respc+=1)=location$(loc_serviceId     )
	fnlbl(lc+=1,1,'Longitude         ', 20,1) : fntxt(lc,22,17, 0,0,'',0,'') : resp$(respc+=1)=location$(loc_longitude     )
	fnlbl(lc+=1,1,'Latitude          ', 20,1) : fntxt(lc,22,17, 0,0,'',0,'') : resp$(respc+=1)=location$(loc_latitude      )
	fnlbl(lc+=1,1,'Meter Number      ', 20,1) : fntxt(lc,22,12, 0,0,'',0,'') : resp$(respc+=1)=location$(loc_meterNumber   )
	fnlbl(lc+=1,1,'Transmitter Number', 20,1) : fntxt(lc,22,20, 0,0,'',0,'') : resp$(respc+=1)=location$(loc_transmitter   )
	fnlbl(lc+=1,1,'Meter Type        ', 20,1)
	fncombof('',lc,22,46,'[Q]\UBmstr\MeterType.h[cno]',1,5,6,40,'[Q]\UBmstr\MeterTypeIdx.h[cno]',1)
	resp$(respc+=1)=location$(loc_meterType     )
	fncmdset(4)
	fnacs(sn$,0,mat resp$,ckey)
	if ckey=5 then
		if u4_meterLocationIdSequential$='True' then
			fn_newLocationIdSequential(-1)
		end if
	else
		mat locationN=(0)
		mat location$=('')
		respc=0
		locationN(loc_locationID    )=val(resp$(respc+=1))
		location$(loc_name          )=resp$(respc+=1)
		location$(loc_activeCustomer)=resp$(respc+=1)
		location$(loc_serviceId     )=resp$(respc+=1) : if cmlsAddForceServiceId$<>'' then resp$(respc)=cmlsAddForceServiceId$
		location$(loc_longitude     )=resp$(respc+=1)
		location$(loc_latitude      )=resp$(respc+=1)
		location$(loc_meterNumber   )=resp$(respc+=1)
		location$(loc_transmitter   )=resp$(respc+=1)
		location$(loc_meterType     )=resp$(respc+=1)(1:5)
		dim tmpKey$*256
		tmpKey$=fnbuildkey$(table$,mat location$,mat locationN,5)
		if fnKeyExists(hCmlsLocation(5),tmpKey$) then
			dim mg$(0)*256
			mat mg$(0)
			fnAddOneC(mat mg$,' The Key '&tmpKey$&' already exists.')
			fnAddOneC(mat mg$,' Please select a different Location ID or Service')
			fnmsgbox(mat mg$)
			goto CmlsAddReEdit
		end if
		fn_purgeSrvAccountFromLocation(serviceCode$,account$)
		fnLocationWrite(mat location$,mat locationN)
	end if
return ! /r
def fn_purgeSrvAccountFromLocation(serviceCode$*2,account$*10)
	! remove account$ from all previously assigned Meter Location records
	dim tmpLoc$(0)*128,tmpLocN(0),tmpLocKey$*128
	mat tmpLoc$(udim(mat location$))
	mat tmpLocN(udim(mat locationN))
	mat tmpLoc$=('')
	mat tmpLocN=(0)
	tmpLoc$(loc_activeCustomer)=trim$(account$)
	tmpLoc$(loc_serviceId)=serviceCode$
	tmpLocKey$=fnBuildKey$(table$,mat tmpLoc$,mat tmpLocN, 4)
	restore #hCmlsLocation(4),key=tmpLocKey$: nokey CmlsDelFinis
	do
		read #hCmlsLocation(4),using form$(hCmlsLocation(1)): mat tmpLoc$,mat tmpLocN eof CmlsDelFinis ! locked CmlsDelLocked
		if trim$(tmpLoc$(loc_activeCustomer))=trim$(account$) then
			tmpLoc$(loc_activeCustomer)=''
			rewrite #hCmlsLocation(4),using form$(hCmlsLocation(1)): mat tmpLoc$,mat tmpLocN
			tmpMatch=1
		else
			release #hCmlsLocation(4):
			tmpMatch=0
		end if
	loop while tmpMatch
	CmlsDelFinis: !
fnend
include: fn_open
include: ertn


