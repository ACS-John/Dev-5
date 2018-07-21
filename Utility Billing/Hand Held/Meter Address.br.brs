fn_setup
fntop(program$)
fnHamsterFio('UB Meter Address')
XIT: !
fnxit
def fn_setup
	if ~setup then
		setup=1
		library 'S:\Core\Library': fntop,fnxit,fngethandle,fnerror,fnindex_it,fnStatusClose,fnStatus,fnHamsterFio
		library 'S:\Core\Library': fnAddOneC,fnAddOneN,fnCountMatchesN,fnArrayMax
		library 'S:\Core\Library': fnmsgbox,fnOpenFile,fnCloseFile,fnBuildKey$
		library 'S:\Core\Library': fncreg_read,fncreg_write,fnreg_read,fnreg_write
		dim form$(0)*256
		dim maData$(0)*30,maDataN(0)
		dim mg$(0)*128
	end if
	if ~exists('[Q]\UBmstr\MeterAddress.h[cno]') then let fn_InitialializeMeterAddress
fnend
def fn_InitialializeMeterAddress
	imaNeedsInitialization=0
	if ~exists('[Q]\UBmstr\MeterAddress.h[cno]') then
		imaNeedsInitialization=1
	end if
	hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, imaInputOnly,2)
	if imaNeedsInitialization then
		fnStatus('Initializing UB Meter Address table...')
		fnCloseFile(hMeterAddressLocationID,'UB Meter Address') 
		fnindex_it('[Q]\UBmstr\MeterAddress.h[cno]','[Q]\UBmstr\MeterAddress_Idx2.h[cno]', '12 30u')
		hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 0,2)
		fn_newLocationID( 1)
		open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],Shr",internal,input,relative
		dim imaMeterAddress$*30
		do
			read #hCustomer,using 'form pos 11,C 30':  imaMeterAddress$ eof imaCustomerFinis
			fn_imaAdd(hMeterAddressLocationID,imaMeterAddress$)
		loop
		imaCustomerFinis: !
		close #hCustomer:
		! fnStatusPause
		fnStatusClose
	end if
	fn_InitialializeMeterAddress=hMeterAddress
fnend
def fn_imaAdd(hMeterAddressName,imaMeterAddress$*30) ! add the record if it does not exist (used for initializing the Meter Address file)
	 read #hMeterAddressName,key=imaMeterAddress$,release: nokey IaAdd
	 goto IaXit
	 IaAdd: !
	 maDataN(ma_LocationID)=fn_newLocationID
	 maData$(ma_Name)=imaMeterAddress$
	 write #hMeterAddressName,using form$(hMeterAddressLocationID),release: mat maData$,mat maDataN
	 fn_imaAdd=1
	 IaXit: !
fnend

def fn_newLocationID(; initialize)
	if initialize then
		nliLastLocation=0
		fncreg_write('Last Location ID Assigned',str$(nliLastLocation))
	else
		fncreg_read('Last Location ID Assigned',nliLastLocation$)
		nliLastLocation=val(nliLastLocation$)
		nliLastLocation+=1
		fncreg_write('Last Location ID Assigned',str$(nliLastLocation))
	end if
	fn_newLocationID=nliLastLocation
fnend
def fn_askAddNew$(meterAddressBefore$*30,meterAddressAfter$*80)
	mat mg$(0)
	fnAddOneC(mat mg$,'The Meter Address was changed from')
	fnAddOneC(mat mg$,'From: "'&meterAddressBefore$&'"')
	fnAddOneC(mat mg$,'  To: "'&meterAddressAfter$&'"')
	fnAddOneC(mat mg$,'')
	fnAddOneC(mat mg$,'Is this a new entry?')
	fnAddOneC(mat mg$,'')
	fnAddOneC(mat mg$,'  Yes    - Add an entry to Meter Address file')
	fnAddOneC(mat mg$,'  No     - Update previous entry in Meter Address file')
	fnAddOneC(mat mg$,'  Cancel - Revert Changes')
	fnmsgbox(mat mg$, aaResponse$, '', 3) ! mtype 3 is yes/no/cancel
	fn_askAddNew$=aaResponse$
fnend
def fn_askAddDuplicate$(meterAddressBefore$*30,meterAddressAfter$*80)
	mat mg$(0)
	fnAddOneC(mat mg$,'The new Meter Address entered already exist.')
	fnAddOneC(mat mg$,'')
	fnAddOneC(mat mg$,'Do you want to continue?')
	fnAddOneC(mat mg$,'')
	fnAddOneC(mat mg$,'  Yes    - use "'&meterAddressAfter$&'" as entered.')
	fnAddOneC(mat mg$,'  No     - revert to "'&meterAddressBefore$&'"')
	fnmsgbox(mat mg$, aaResponse$, '', 4) ! mtype 4 is yes/no
	fn_askAddDuplicate$=aaResponse$
fnend

include: fn_open
include: ertn