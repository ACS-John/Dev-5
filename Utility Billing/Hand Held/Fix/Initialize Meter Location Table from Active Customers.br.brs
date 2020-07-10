
! if this program has duplicates it should not be used or should be modified to determine the location id differently

fn_setup
fntop(program$)
pr env$('program_caption')&' are you sure?' : pause
fnCopy('[Q]\UBmstr\MeterLocation.h[cno]','[Q]\UBmstr\MeterLocation'&date$('ccyymmdd')&srep$(time$,':','')&'.h[cno]')
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
hCustomer=fn_open('UB Customer',mat c$,mat cN,mat form$)
hLocation=fn_open('U4 Meter Location',mat l$,mat lN,mat form$)
dupCount=writeCount=0
do
	read #hCustomer,using form$(hCustomer): mat c$,mat cN eof EoCustomer
	customerStatus=cN(c_finalBilling)
	if customerStatus=0 or customerStatus=3 then ! if active
		mat l$=('')
		mat lN=(0)
		lN(loc_locationID     )=int(val(c$(c_account)))
		l$(loc_name           )=c$(c_meterAddress)
		l$(loc_activeCustomer )=trim$(c$(c_account))
		l$(loc_serviceId      )='WA'
		l$(loc_longitude      )=''
		l$(loc_latitude       )=''
		l$(loc_meterNumber    )=c$(c_s1meterNumber)
		l$(loc_transmitter    )=str$(int(val(c$(c_account)))) ! c$(c_s01serialNumber) ! Service 1 (Water) – Serial Number
		l$(loc_meterType      )=' C'
		locKey$=fnBuildKey$('U4 Meter Location',mat l$,mat lN)
		read #hLocation,using form$(hLocation),key=locKey$: mat l$,mat lN noKey NoKeyGoodContinue
		! Key already exists...
			pr 'Key already exists.';bell
			dupCount+=1
			pause
		goto NextCustomer
		
		NoKeyGoodContinue: !
			write #hLocation,using form$(hLocation): mat l$,mat lN
			writeCount+=1
		goto NextCustomer
		NextCustomer: !
	en if
loop
EoCustomer: !
dim tmpMsg$(0)*256
mat tmpMsg$(0)
if dupCount then
	fnAddOneC(mat tmpMsg$,'duplicate errors   =' &str$(dupCount  )&' !!!')
end if
fnAddOneC(  mat tmpMsg$,'total records added='&str$(writeCount))
fnAddOneC(mat tmpMsg$,'Continue to review the new meter location table')
fnMsgBox(mat tmpMsg$)
if dupCount then pause
fnchain('S:\Utility Billing\Hand Held\Meter Location')
include: fn_open
include: fn_setup
