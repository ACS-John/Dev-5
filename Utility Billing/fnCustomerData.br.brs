def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fnerror
		library 'S:\Core\Library': fnapplyDefaultRatesFio
		library 'S:\Core\Library': fnOpenFile,fnCloseFile
		on error goto ERTN
	! dims, constants, top, etc
		dim cus$(0)*256,cusN(0)
	!
	end if
fnend

def library fnCustomerData$*128(account$*10,fieldName$*40; leaveOpen)
	if ~setup then let fn_setup
	fnCustomerData$=fn_customerData$(account$,fieldName$, leaveOpen)
fnend
def fn_customerData$*128(account$*10,fieldName$*40; leaveOpen)
	account$=lpad$(trim$(account$),10)
	if customerDataSetup$<>account$ then ! r:
		customerDataSetup$=account$
		if ~hCustomer then
			hCustomer=fn_open('UB Customer',mat cus$,mat cusN,mat form$, 1)
			! open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',internal,input,keyed
		end if
		dim account$*10
		mat cus$=('')
		mat cusN=(0)
		read #hCustomer,using form$(hCustomer),key=account$,release: mat cus$,mat cusN nokey CustomerDataFinis
		fnapplyDefaultRatesFio(mat cusN)
	end if ! /r
	dim customerDataReturn$*128
	customerDataReturn$=''
	fieldName$=lwrc$(fieldName$)
	if fieldName$='route' then
		if cusN(c_route)<>0 then customerDataReturn$=str$(cusN(c_route))
	else if fieldName$='sequence' then
		if cusN(c_sequence)<>0 then customerDataReturn$=str$(cusN(c_sequence))
	else if fieldName$='final billing code' then
		if cusN(c_finalBilling)<>0 then customerDataReturn$=str$(cusN(c_finalBilling))
	else if fieldName$='last billing day' then
		if cusN(c_lastBillingDate)<>0 then customerDataReturn$=str$(days(cusN(c_lastBillingDate),'mmddyy'))
	else if fieldName$='name' then
		customerDataReturn$=cus$(c_name)
	else if fieldName$='service 1.rate code' then
		if cusN(c_s01rate)<>0 then customerDataReturn$=str$(cusN(c_s01rate))
	else if fieldName$='service 2.rate code' then
		if cusN(c_s02rate)<>0 then customerDataReturn$=str$(cusN(c_s02rate))
	else if fieldName$='service 3.rate code' then
		if cusN(c_s03rate)<>0 then customerDataReturn$=str$(cusN(c_s03rate))
	else if fieldName$='service 4.rate code' then
		if cusN(c_s04rate)<>0 then customerDataReturn$=str$(cusN(c_s04rate))
	else if fieldName$='service 5.rate code' then
		if cusN(c_s05rate)<>0 then customerDataReturn$=str$(cusN(c_s05rate))
	else if fieldName$='service 9.rate code' then
		if cusN(c_s09rate)<>0 then customerDataReturn$=str$(cusN(c_s09rate))
	else if fieldName$='service 10.rate code' then
		if cusN(c_s10rate)<>0 then customerDataReturn$=str$(cusN(c_s10rate))
	else if fieldName$='service 1.reading.current' then
		if cusN(c_s01readingCur)<>0 then customerDataReturn$=str$(cusN(c_s01readingCur))
	else if fieldName$='service 1.reading.prior' then
		if cusN(c_s01readingPri)<>0 then customerDataReturn$=str$(cusN(c_s01readingPri))
	else if fieldName$='service 1.usage.current' then
		if cusN(c_s01UsageCur)<>0 then customerDataReturn$=str$(cusN(c_s01UsageCur))
	else if fieldName$='service 1.usage.ytd' then
		if cusN(c_s01UsageYtd)<>0 then customerDataReturn$=str$(cusN(c_s01UsageYtd))
	else if fieldName$='service 3.reading.current' then
		if cusN(c_s03readingCur)<>0 then customerDataReturn$=str$(cusN(c_s03readingCur))
	else if fieldName$='service 3.reading.prior' then
		if cusN(c_s03ReadingPri)<>0 then customerDataReturn$=str$(cusN(c_s03ReadingPri))
	else if fieldName$='service 3.usage.current' then
		if cusN(c_s03UsageCur)<>0 then customerDataReturn$=str$(cusN(c_s03UsageCur))
	else if fieldName$='service 3.usage.ytd' then
		if cusN(c_s03UsageYtd)<>0 then customerDataReturn$=str$(cusN(c_s03UsageYtd))
	else if fieldName$='service 4.reading.current' then
		if cusN(c_s04readingCur)<>0 then customerDataReturn$=str$(cusN(c_s04readingCur))
	else if fieldName$='service 4.reading.prior' then
		if cusN(c_s04readingPri)<>0 then customerDataReturn$=str$(cusN(c_s04readingPri))
	else if fieldName$='service 4.usage.current' then
		if cusN(c_s04usageCur)<>0 then customerDataReturn$=str$(cusN(c_s04usageCur))
	else if fieldName$='service 4.usage.ytd' then
		if usN(c_s04usageYtd)<>0 then customerDataReturn$=str$(usN(c_s04usageYtd))
	else if fieldName$='service 1.unit count' then
		if cusN(c_s01unitCount)<>0 then customerDataReturn$=str$(cusN(c_s01unitCount))
	else if fieldName$='demand multiplier' then
		if cusN(c_demandMultiplier)<>0 then customerDataReturn$=str$(cusN(c_demandMultiplier))
	else if fieldName$='demand reading' then
		if cusN(c_demandReading)<>0 then customerDataReturn$=str$(cusN(c_demandReading))
	else if fieldName$='balance' then
		if cusN(c_balance)<>0 then customerDataReturn$=str$(cusN(c_balance))
	else 
		pr 'fn_customerData$ does not recognize the field: '&fieldName$
		pause
	end if
	CustomerDataFinis: !
	if ~leaveOpen then
		close #hCustomer: 
		hCustomer=0
	end if
	fn_customerData$=customerDataReturn$
fnend

include: fn_open
include: ertn