def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnapplyDefaultRatesFio
		library 'S:\Core\Library': fnOpenFile,fnCloseFile
		library 'S:\Core\Library': fnMeterInfo$
		library 'S:\Core\Library': fncustomer_address
		on error goto Ertn
		
		library 'S:\Core\Library': fnget_services
		dim serviceName$(0)*20
		dim serviceCode$(0)*2
		fnget_services(mat serviceName$,mat serviceCode$)
		fnget_services(mat serviceName$,mat serviceCode$)
		
	end if
fnend

def library fnCustomerData$*128(account$*10,fieldName$*40; leaveOpen)
	if ~setup then let fn_setup
	fnCustomerData$=fn_customerData$(account$,fieldName$, leaveOpen)
fnend
def fn_customerData$*128(account$*10,fieldName$*40; leaveOpen,___,return$*128,whichService)
	account$=lpad$(trim$(account$),10)
	if customerDataSetup$<>account$ then ! r:
		customerDataSetup$=account$
		if ~hCustomer then
			dim cus$(0)*256
			dim cusN(0)
			hCustomer=fn_open('UB Customer',mat cus$,mat cusN,mat form$, 1)
			! open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',internal,input,keyed
		end if
		dim account$*10
		mat cus$=('')
		mat cusN=(0)
		read #hCustomer,using form$(hCustomer),key=account$,release: mat cus$,mat cusN nokey CustomerDataFinis
		fnapplyDefaultRatesFio(mat cusN)
		ao_primary  :=1
		ao_alternate:=2
		ao_billing  :=3
	end if ! /r
	return$=''
	fieldName$=lwrc$(fieldName$)
	if pos(fieldName$,' address ')>0 and customerAddressSetup$<>account$ then
		customerAddressSetup$=account$
		
		dim customerAddrPrimary$(4)*30
		fncustomer_address(account$,mat customerAddrPrimary$  , ao_primary  )
		dim customerAddrAlternate$(4)*30
		fncustomer_address(account$,mat customerAddrAlternate$, ao_alternate)
		dim customerAddrBilling$(4)*30
		fncustomer_address(account$,mat customerAddrBilling$  , ao_billing  ,1)
		
	end if
	if fieldName$(3:3)=' ' and fieldName$(1:1)<>' ' and fieldName$(2:2)<>' ' then ! check to see if it starts with a serviceCode$
		! i.e.' XX '
		whichService=srch(mat serviceCode$,uprc$(fieldName$(1:2)))
		if whichService>0 then
			fieldName$(1:3)='service '&str$(whichService)&'.'
		end if
	end if
		
	if fieldName$='route' then
		if cusN(c_route)<>0 then return$=str$(cusN(c_route))
	else if fieldName$='sequence' then
		if cusN(c_sequence)<>0 then return$=str$(cusN(c_sequence))
	else if fieldName$='final billing code' then
		if cusN(c_finalBilling)<>0 then return$=str$(cusN(c_finalBilling))
	else if fieldName$='last reading day' then
		if cusN(c_meterReadDatePri)<>0 then return$=str$(days(cusN(c_meterReadDatePri),'mmddyy'))
	else if fieldName$='last billing day' then
		if cusN(c_lastBillingDate)<>0 then return$=str$(days(cusN(c_lastBillingDate),'mmddyy'))
	else if fieldName$='name' then
		return$=cus$(c_name)
	else if fieldName$='service 1.rate code' then
		if cusN(c_s01rate)<>0 then return$=str$(cusN(c_s01rate))
	else if fieldName$='service 2.rate code' then
		if cusN(c_s02rate)<>0 then return$=str$(cusN(c_s02rate))
	else if fieldName$='service 3.rate code' then
		if cusN(c_s03rate)<>0 then return$=str$(cusN(c_s03rate))
	else if fieldName$='service 4.rate code' then
		if cusN(c_s04rate)<>0 then return$=str$(cusN(c_s04rate))
	else if fieldName$='service 5.rate code' then
		if cusN(c_s05rate)<>0 then return$=str$(cusN(c_s05rate))
	else if fieldName$='service 9.rate code' then
		if cusN(c_s09rate)<>0 then return$=str$(cusN(c_s09rate))
	else if fieldName$='service 10.rate code' then
		if cusN(c_s10rate)<>0 then return$=str$(cusN(c_s10rate))
	else if fieldName$='service 1.reading.current' then
		if cusN(c_s01readingCur)<>0 then return$=str$(cusN(c_s01readingCur))
	else if fieldName$='service 1.reading.prior' then
		if cusN(c_s01readingPri)<>0 then return$=str$(cusN(c_s01readingPri))
	else if fieldName$='service 1.usage.current' then
		if cusN(c_s01UsageCur)<>0 then return$=str$(cusN(c_s01UsageCur))
	else if fieldName$='service 1.usage.ytd' then
		if cusN(c_s01UsageYtd)<>0 then return$=str$(cusN(c_s01UsageYtd))
	else if fieldName$='service 3.reading.current' then
		if cusN(c_s03readingCur)<>0 then return$=str$(cusN(c_s03readingCur))
	else if fieldName$='service 3.reading.prior' then
		if cusN(c_s03ReadingPri)<>0 then return$=str$(cusN(c_s03ReadingPri))
	else if fieldName$='service 3.usage.current' then
		if cusN(c_s03UsageCur)<>0 then return$=str$(cusN(c_s03UsageCur))
	else if fieldName$='service 3.usage.ytd' then
		if cusN(c_s03UsageYtd)<>0 then return$=str$(cusN(c_s03UsageYtd))
	else if fieldName$='service 4.reading.current' then
		if cusN(c_s04readingCur)<>0 then return$=str$(cusN(c_s04readingCur))
	else if fieldName$='service 4.reading.prior' then
		if cusN(c_s04readingPri)<>0 then return$=str$(cusN(c_s04readingPri))
	else if fieldName$='service 4.usage.current' then
		if cusN(c_s04usageCur)<>0 then return$=str$(cusN(c_s04usageCur))
	else if fieldName$='service 4.usage.ytd' then
		if usN(c_s04usageYtd)<>0 then return$=str$(usN(c_s04usageYtd))
	else if fieldName$='service 1.unit count' then
		if cusN(c_s01unitCount)<>0 then return$=str$(cusN(c_s01unitCount))
	else if fieldName$='demand multiplier' then
		if cusN(c_demandMultiplier)<>0 then return$=str$(cusN(c_demandMultiplier))
	else if fieldName$='demand reading' then
		if cusN(c_demandReading)<>0 then return$=str$(cusN(c_demandReading))
	else if fieldName$='balance' then
		if cusN(c_balance)<>0 then return$=str$(cusN(c_balance))
	else if fieldName$='meter address' then
		return$=cus$(c_meterAddress)
	else if fieldName$(3:inf)=' meter address' then
		return$=fnMeterInfo$(mi_field$,account$,uprc$(fieldname$(1:2)))
		didOpenMeterInfo=1
	else if pos(fieldName$,' address ')>0 then ! i.e. 'billing address 1' or 'alternate address 3'
		tmpIndex=val(fieldName$(pos(fieldname$,' ',-1)+1:len(fieldname$)))
		if tmpIndex<=0 then
			pr bell;' index not parsed'
			pause
		else
			if fieldName$(1:pos(fieldname$,' ')-1)='primary' then
				if tmpIndex<=udim(mat customerAddrPrimary$) then
					return$=customerAddrPrimary$(tmpIndex)
				end if
			else if fieldName$(1:pos(fieldname$,' ')-1)='alternate' then
				if tmpIndex<=udim(mat customerAddrAlternate$) then
					return$=customerAddrAlternate$(tmpIndex)
				end if
			else if fieldName$(1:pos(fieldname$,' ')-1)='billing' then
				if tmpIndex<=udim(mat customerAddrBilling$) then
					return$=customerAddrBilling$(tmpIndex)
				end if
			else
				pr bell;'could not parse address type from '&fieldName$
				pause
			end if
		end if
	else 
		pr 'fn_customerData$ does not recognize the field: '&fieldName$
		pause
	end if
	CustomerDataFinis: !
	if ~leaveOpen then
		close #hCustomer: 
		hCustomer=0
		if didOpenMeterInfo then 
			fnMeterInfo$('','','', 1)
		end if
	end if
	fn_customerData$=return$
fnend

include: fn_open
include: ertn