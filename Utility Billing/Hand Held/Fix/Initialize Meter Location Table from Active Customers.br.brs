! load 'S:\Utility Billing\Hand Held\Fix\Initialize Meter Location Table from Active Customers.br'
! if this program has duplicates it should not be used or should be modified to determine the location id differently

! currently configured best for Thomasboro and Omaha

fn_setup
fntop(program$)
! MeterId - either Customer.S1MeterNumber or Customer.s01serialNumber
disableReportBlank=1
fixDuplicateMeterIds=1  ! these fixes happen by inventing new data replacing the conversion's souce and destination datasets
fixBlankMeterIds=1      ! these fixes happen by inventing new data replacing the conversion's souce and destination datasets
if env$('client')='Omaha' then getMeterNoFromS1SerialNo=0 ! gets from S1MeterNumber instead
if env$('client')='Thomasboro' then getMeterNoFromS1SerialNo=1

pr env$('program_caption')&' are you sure?' : pause


! does not change   fnCopy('[Q]\UBmstr\Customer.h[cno]','[Q]\UBmstr\Customer[datetime] h[cno].old')
fnCopy('[Q]\UBmstr\MeterLocation.h[cno]','[Q]\UBmstr\MeterLocation[datetime] h[cno].old')
fnFree('[Q]\UBmstr\MeterLocation.h[cno]')
fnFree('[Q]\UBmstr\MeterLocationIdx1.h[cno]')
fnFree('[Q]\UBmstr\MeterLocationIdx2.h[cno]')
fnFree('[Q]\UBmstr\MeterLocationIdx3.h[cno]')
fnFree('[Q]\UBmstr\MeterLocationIdx4.h[cno]')
fnFree('[Q]\UBmstr\MeterLocationIdx5.h[cno]')

dim l$(0)*128
dim lN(0)
dim c$(0)*256
dim cN(0)
hCustomer=fn_openFio('UB Customer',mat c$,mat cN)
hLocation=fn_openFio('U4 Meter Location',mat l$,mat lN)
dupCount=countWrite=0
do
	read #hCustomer,using form$(hCustomer): mat c$,mat cN eof EoCustomer
	customerStatus=cN(c_finalBilling)
	if customerStatus=0 or customerStatus=3 then ! if active
		dim meterNumber$
		if getMeterNoFromS1SerialNo t
			meterNumber$=c$(c_s01serialNumber)
		else
			meterNumber$=c$(c_s1meterNumber)
		end if
		fn_testMeterNumber(c$(c_account),meterNumber$,hCustomer,mat c$,mat cN)
		id=cN(c_route)*100000000+cN(c_sequence)*10

		ReDoLocation: !
		mat l$=('')
		mat lN=(0)
		lN(loc_locationID     )=id
		l$(loc_name           )=c$(c_meterAddress)
		l$(loc_activeCustomer )=trim$(c$(c_account))
		l$(loc_serviceId      )='WA'
		l$(loc_longitude      )=''
		l$(loc_latitude       )=''

		l$(loc_meterNumber    )=                                                str$(id)
		l$(loc_transmitter    )=                                                meterNumber$
		l$(loc_meterType      )=fn_endPointType$(meterNumber$)
		locKey$=fnBuildKey$('U4 Meter Location',mat l$,mat lN)
		read #hLocation,using form$(hLocation),key=locKey$: mat l$,mat lN noKey NoKeyGoodContinue
			fn_reportError(c$(c_account),' Fixing Duplicate Key: '&locKey$)
			dupCount+=1
			id+=1
			goto ReDoLocation
		goto NextCustomer

		NoKeyGoodContinue: !
			write #hLocation,using form$(hLocation): mat l$,mat lN
			countWrite+=1
		goto NextCustomer
		NextCustomer: !
	en if
loop

EoCustomer: !
	if setupReportError then ! r: print totals and release
		pr #255: '____________________________________'
		pr #255: '*  '&env$('program_caption')&'  *'
		pr #255: '____________________________________'
		if dupCount then
			pr #255: '** Duplicate Location ID Count: '&str$(dupCount)&'  **'
		end if
		if fixDuplicateMeterIds then
			pr #255: 'Duplicate Meter ID Fixed: '&str$(countDuplicateMeterIds)
		else
			pr #255: 'Duplicate Meter ID Count: '&str$(countDuplicateMeterIds)
		end if
		if fixBlankMeterIds then
			pr #255: '   Blank Meter IDs Fixed: '&str$(countBlankS1MeterNumber)
		else
			pr #255: '    Blank Meter ID Count: '&str$(countBlankS1MeterNumber)
		end if
		pr #255: '____________________________________'

		pr #255: '    Records Added: '&str$(countWrite)
		pr #255: '     Meter Type Z: '&str$(countTypeZ)
		pr #255: '     Meter Type N: '&str$(countTypeN)
		pr #255: '     Meter Type C: '&str$(countTypeC)
		pr #255: '____________________________________'
		fncloseprn : setupReportError=0
		if dupCount then pause
		countBlankS1MeterNumber=dupCount=countDuplicateMeterIds=countWrite=0
	end if ! /r
	pause
fnchain('S:\Utility Billing\Hand Held\Meter Location')


def fn_endPointType$(meterId$; ___,return$,mLen)
	! 1.	If the endpoint SN starts with a 7 or an 8 and is 8 digits long it is an ORION CE and that is endpoint type “Z”
	! 2.	If the endpoint SN starts with a 3 and is 8 digits long it is an ORION ME and that is endpoint type “N”
	meterId$=trim$(meterId$)
	mLen=len(meterId$)

	! if meterId$<>'' then pr meterId$
	! if meterId$(1:1)='8' then pause

	return$='C'
	if mLen=8 then
		if (meterId$(1:1)='7' or meterId$(1:1)='8') then
			return$='Z'
		else if meterId$(1:1)='3' then
			return$='N'
		end if
	end if
	if return$='Z' then countTypeZ+=1
	if return$='N' then countTypeN+=1
	if return$='C' then countTypeC+=1
	fn_endPointType$=return$
fnend
def fn_testMeterNumber(z$*10,&meterId$,hCustomer,mat c$,mat cN; ___,which) ! now updates customer record too - loads of locals
	if ~setupTestMeterNumber then
		setupTestMeterNumber=1
		dim meterIdList$(0)*18
		dim meterIdListZ$(0)*10
		meterIdListCount=0 : mat meterIdList$(meterIdListCount) : mat meterIdListZ$(meterIdListCount)
	end if

	TestMeterNumberTop: !
	which=srch(mat meterIdList$,meterId$)
	if trim$(meterId$)='' then
		! r: processing for blanks
		countBlankS1MeterNumber+=1
		if fixBlankMeterIds then
			gosub CreateNewMeterId
			fn_reportError(z$,'Fixed Blank MeterId with '&meterId$)
			newMeterId$='x'&str$(newMeterIdNumber+=1)
		else if ~disableReportBlank then
			fn_reportError(z$,'Blank S1MeterNumber')
		end if
		! /r
	else if which>0 then
		! r: processing for duplicates
		countDuplicateMeterIds+=1
		if fixDuplicateMeterIds then
		oldMeterId$=meterId$
			gosub CreateNewMeterId
			fn_reportError(z$,oldMeterId$&' Fixed Duplicate MeterId - changed to : '&newMeterId$)

			goto TestMeterNumberTop
		else
			fn_reportError(z$,meterId$&' Duplicate MeterId - already used by '&meterIdListZ$(which))
		end if
		! /r
	else
		meterIdListCount+=1 : mat meterIdList$(meterIdListCount) : mat meterIdListZ$(meterIdListCount)
		meterIdList$(meterIdListCount)=meterId$ : meterIdListZ$(meterIdListCount)=z$
	end if

fnend
CreateNewMeterId: ! r:
	newMeterId$='x'&str$(newMeterIdNumber+=1)

	! if getMeterNoFromS1SerialNo then
	! 	c$(c_s01serialNumber)=newMeterId$
	! else ! gets from S1MeterNumber instead
	! 	c$(c_s1meterNumber)=newMeterId$
	! end if
	! rewrite #hCustomer,using form$(hCustomer): mat c$,mat cN

	meterId$=newMeterId$
return ! /r
def fn_reportError(z$*10,text$*256)
	if ~setupReportError then
		fnopenprn : setupReportError=1
		pr #255: 'Duplicate or missing Meter ID Report'
		pr #255: 'Account    Outstanding Error'
		pr #255: '__________ __________________________'
	end if
	pr #255,u 'form pos 1,c 10,x 1,c': z$,text$
fnend
include: fn_open
include: fn_setup
