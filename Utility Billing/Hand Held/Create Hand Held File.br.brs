! formerly S:\acsUB\hhto
! -- Tranfer Data From Computer to Hand Held
fn_setup
fnTop(program$)
open #hCustomeri1=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,i,k
open #hCustomeri5=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr',i,i,k
goto Screen1

Screen1: ! r:

	selection_method=fncreg_read('hhto.selection_method',selection_method$,'2')
	fnTos : respc=0
	fnLbl(2,1,'Hand Held model:',16,1)
	if lwrc$(devicePreference$)='[ask]' then
		fnComboA('HH-FroCBox',2,18,mat deviceName$)
		resp$(rc_Device:=respc+=1)=deviceSelected$
	else
		fnLbl(2,18,deviceSelected$)
	end if
	fnLbl(4,1,'Select:',16,1)
	if u4_includeFinalBilled$='True' then
		fnOpt(4,18,'[All] (including inactive)')
	else
		fnOpt(4,18,'[All] Active Accounts')
	end if
	rc_selectionMethod1:=respc+=1 : if selection_method=sm_allExceptFinal then resp$(rc_selectionMethod1)='True' else resp$(rc_selectionMethod1)='False'
	fnOpt(5,18,'An Entire Route')
	rc_selectionMethod2:=respc+=1 : if selection_method=sm_aRoute then resp$(rc_selectionMethod2)='True' else resp$(rc_selectionMethod2)='False'
	fnOpt(6,18,'A Range of Accounts')
	rc_selectionMethod3:=respc+=1 : if selection_method=sm_routeRange then resp$(rc_selectionMethod3)='True' else resp$(rc_selectionMethod3)='False'
	fnOpt(7,18,'Specific Accounts')
	rc_selectionMethod4:=respc+=1 : if selection_method=sm_Individuals then resp$(rc_selectionMethod4)='True' else resp$(rc_selectionMethod4)='False'
	if udim(mat serviceCodeMetered$) then
		fnOpt(8,18,'Only for Active Accounts with '&serviceCodeMetered$(1)&' Meter Type of selected Hand Held model.')
		rc_selectionMethod5:=respc+=1 : if selection_method=sm_meterTypes then resp$(rc_selectionMethod5)='True' else resp$(rc_selectionMethod5)='False'
	end if

	fnLbl(9,1,'',46,2)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
			if lwrc$(devicePreference$)='[ask]' then
				deviceSelected$=resp$(rc_Device)
				fnureg_write('Hand Held Device Asked',deviceSelected$)
			else
				deviceSelected$=devicePreference$
			end if
		if resp$(rc_selectionMethod1)='True' then
			selection_method=sm_allExceptFinal
		else if resp$(rc_selectionMethod2)='True' then
			selection_method=sm_aRoute
		else if resp$(rc_selectionMethod3)='True' then
			selection_method=sm_routeRange
		else if resp$(rc_selectionMethod4)='True' then
			selection_method=sm_Individuals
		else if rc_selectionMethod5 and resp$(rc_selectionMethod5)='True' then
			selection_method=sm_meterTypes
		end if
		fncreg_write('hhto.selection_method',str$(selection_method))
	end if
	mat resp$=('')
	if deviceSelected$='Badger Beacon' then filterNoLocationId=1 else filterNoLocationId=0


	if ckey=5 then
		goto Xit
	else if ckey=2 then
		goto Finis
	else ! ckey=1
		if ~workopen then
			fn_openOutFile ! open work files based on type of Hand Held
		end if
		if deviceSelected$='Aclara' then
			! u4_includeFinalBilled$='True'
			selection_method=sm_LocationId ! all Location IDs
		end if
		if selection_method=sm_allExceptFinal or selection_method=sm_meterTypes then
			goto StartForSelectAll
		else if selection_method=sm_aRoute then
			goto AskRoute
		else if selection_method=sm_routeRange then
			goto AskRange
		else if selection_method=sm_Individuals then
			goto NextAskAccount
		else if selection_method=sm_LocationId then
			goto NextLocationId
		end if
	end if
! /r
AskRange: ! r:
	fnTos
	fnFra(1,1,1,57,'Starting Account:')
	fnFra(4,1,1,57,'Ending Account:')
	fncmbact(1,1,0,1)
	fnButton(1,48,'Search',6,blank$,0,7,1)
	resp$(1)=resp$(2)=''
	fncmbact(1,1,0,2)
	fnButton(1,48,'Search',7,blank$,0,7,2)
	fnCmdKey('&Finish',2,1,0,'Completed with all routes')
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	bk1$=lpad$(trim$(resp$(1)(1:10)), 10)
	bk2$=lpad$(trim$(resp$(2)(1:10)), 10)
	if ckey=2 then goto Finis
	if ckey=99 or ckey=5 then mat resp$=(''): goto Screen1
	if ckey=6 then
		fn_searchScreen(x$,resp$(1))
		goto AskRange
	else if ckey=7 then
		fn_searchScreen(x$,resp$(2))
		goto AskRange
	end if
	mat resp$=('')
	! read #hCustomeri1,using F_CUSTOMER,key=bk1$,release: z$,mat e$,mat a,final,mat xd,mat f$,route,sequence,extra$(3),extra$(7),extra(1),alp$ eof AskRange ! get first and last route and sequence number to select
	! read #hCustomeri1,using F_CUSTOMER,key=bk2$,release: z$,mat e$,mat a,final,mat xd,mat f$,last_route,last_sequence,extra$(3),extra$(7),extra(1),alp$ eof AskRange
	read #hCustomeri1,using 'form pos 1741,n 2,n 7',key=bk1$,release: route,sequence ! get first and last route and sequence number to select
	read #hCustomeri1,using 'form pos 1741,n 2,n 7',key=bk2$,release: last_route,last_sequence
	restore #hCustomeri5,key=cnvrt$('pic(zz)',route)&cnvrt$('pic(zzzzzzz',sequence):
	NextReadForRange: !
	if fn_customerRead=-54 then goto AskRange
	! if (route=last_route and sequence>last_sequence) or route>last_route Then Goto AskRange
	if trim$(z$)<trim$(bk1$) or trim$(z$)>trim$(bk2$) then goto AskRange
goto SendRecordToOutFile ! /r
AskRoute: ! r:
	fnTos
	if hbk<>0 then
		fnLbl(1,1,'Last Route Number Selected: '&str$(hbk))
		myline=3
	else
		myline=1
	end if
	fnLbl(myline,1,'Route Number:')
	fncmbrt2(myline,22,0)
	resp$(1)=''
	fnCmdKey('&Next',1,1,0,'Add the selected route' )
	fnCmdKey('&Finish',2,0,1,'Completed with all routes')
	fnCmdKey('&Cancel',5,0,0,'Don''t sent to Hand Held')
	fnAcs(mat resp$, ckey)
	if resp$(1)='[All]' and ckey=1 then selection_method=sm_allExceptFinal : goto StartForSelectAll ! if they select all on the route screen, handle same as pr all option from 1st menu
	bk1=val(resp$(1)) conv ignore
	resp$(1)=''
	if ckey=1 then
		goto StartForSelectAll
	else if ckey=2 then
		goto Finis
	else if ckey=5 then
		goto Screen1
	else
		goto StartForSelectAll
	end if

! /r
AskMeterType: ! r:
	fnTos
	if lastMeterType$<>'' then
		fnLbl(1,1,'Last Meter Type Selected: '&lastMeterType$)
		myline=3
	else
		myline=1
	end if
	fnLbl(myline,1,'Meter Type:')
	fnComboF('cmbMeterType',myline,13,width,'[Q]\UBmstr\MeterType.h[cno]',1,5,6,40, '[Q]\UBmstr\MeterTypeIdx.h[cno]',1)
	resp$(1)=''
	fnCmdKey('&Next',1,1,0,'Add the selected route' )
	fnCmdKey('&Finish',2,0,1,'Completed with all routes')
	fnCmdKey('&Cancel',5,0,0,'Don''t sent to Hand Held')
	fnAcs(mat resp$, ckey)
	if resp$(1)='[All]' and ckey=1 then selection_method=sm_allExceptFinal : goto StartForSelectAll ! if they select all on the route screen, handle same as pr all option from 1st menu
	bk1=val(resp$(1)) conv ignore
	lastMeterType$=resp$(1)
	resp$(1)=''

	if ckey=1 then
		goto StartForSelectAll
	else if ckey=2 then
		goto Finis
	else if ckey=5 then
		goto Screen1
	else
		goto StartForSelectAll
	end if
! /r
StartForSelectAll: ! r:
	! if deviceSelected$='Aclara Work Order' then
	! 	fn_getFilterAccount(mat filterAccount$)
	! end if
	if bk1=0 then bk1=1
	restore #hCustomeri5,key>=cnvrt$('pic(zz)',bk1)&'       ': nokey AskRoute
goto NextReadForAll ! /r

SendRecordToOutFile: ! r:
	! if trim$(z$)='100100.99' then pause
	if udim(mat filterAccount$) or ~final or u4_includeFinalBilled$='True' then ! skip IF FINAL BILLED

		if ~(selection_method=sm_meterTypes) or deviceSelected$=fn_meterInfo$('Device',z$,serviceCodeMetered$(1)) then
			if ~filterNoLocationId or val(fn_meterInfo$('Location_ID',z$,'WA'))>0 then
				dim ft$*20
				ft$=fn_rmk1$(z$)
				if sq1=0 then sq1=1234 ! DEFALT SEQ=W,E,D,G
				seq$=str$(sq1)
				if deviceSelected$='Aclara' then
					fn_aclara(readLocationId)
				! else if deviceSelected$='Aclara Work Order' then
				! 	fn_aclaraWorkOrder
				else if deviceSelected$='ACS Meter Reader' then
					fn_acsMeterReader
				else if deviceSelected$='AMR' then
					fn_amr
				else if deviceSelected$='Badger' or deviceSelected$='Badger Connect C' then
					fn_badgerConnectC
				else if deviceSelected$='Badger Beacon' then
					fn_badgerBeacon(z$,'WA')   ! newest/current interface as of 2/2/2018
				else if deviceSelected$='Boson' then
					fn_boson
				else if deviceSelected$='EZReader' or deviceSelected$='Green Tree' or deviceSelected$='Hersey' or deviceSelected$='Sensus' then
					fn_legacyMultiDevice
				else if deviceSelected$='Itron FC300' then
					fn_itron
				else if deviceSelected$='LapTop' then
					fn_laptop
				else if deviceSelected$='Master Meter' then
					fn_masterMeter
				else if deviceSelected$='Neptune (Equinox v4)' then
					fn_neptuneEquinoxV4(h_out)
				! else if deviceSelected$='Psion Workabout' then
				! 	fn_workabout
				else if deviceSelected$='READy Water' then
					fn_readyWater
				else if deviceSelected$='READy Kamstrup (Geo)' then
					fn_readyKamstrup(h_out,z$,'WA') ! newer than READy Water, has long/lat, requires meter location
				else if deviceSelected$='Unitech HT630' then
					fn_unitech_ht630
				else
					goto Screen1 ! go back if Hand Held information is not available for their selection
				end if
			end if
		end if
	end if
	SendRecordToOutFileFinis: !
	if selection_method=sm_allExceptFinal or selection_method=sm_meterTypes then
		goto NextReadForAll
	else if selection_method=sm_aRoute then
		goto NextReadForAll
	else if selection_method=sm_routeRange then
		goto NextReadForRange
	else if selection_method=sm_Individuals then
		goto NextAskAccount
	else if selection_method=sm_LocationId then
		goto NextLocationId
	else
		goto NextReadForAll
	end if
! /r

NextLocationId: ! r:
	!  pr 'readLocationId=';readLocationId : pause ! if readLocationId=118 then pr 'about to do location 119' : pause
	nliCustomerReadResponse=fn_customerRead( '',readLocationId+=1)
	! if final<>0 then ! can not trust accounts to be unique if they are not active.
	!   goto NextLocationId
	if nliCustomerReadResponse=0 then ! else if nliCustomerReadResponse=0 then ! no active account found for LocationID
		goto NextLocationId
	else if nliCustomerReadResponse=-54 then ! end of file
		goto End1
	end if
goto SendRecordToOutFile ! /r
NextReadForAll: ! ! r:
	if fn_customerRead=-54 then
		goto End1
	else if selection_method=sm_aRoute then
		if ~route then
			goto NextReadForAll
		else if bk1<>route then
			goto End1
		end if
	end if
goto SendRecordToOutFile ! /r
NextAskAccount: ! r:
	fnTos
	if z$<>'' then
		fnLbl(1,1,'Last Account Selected: '&z$,40,2)
		myline=3
	else
		myline=1
	end if
	fnLbl(myline,1,'Account:',15,1)
	fncmbact(myline,16)
	resp$(1)=z$
	fnCmdSet(5)
	ckey=fnAcs(mat resp$)
	if ckey=6 then
		fnCustomerSearch(resp$(1))
	end if
	if ckey=99 or ckey=5 or resp$(1)='          ' then goto Screen1
	z$=lpad$(trim$(resp$(1)(1:10)), 10)
	if fn_customerRead(z$)=-4272 then goto NextAskAccount
goto SendRecordToOutFile
! /r

End1: ! r:
	if deviceSelected$='Itron FC300' then
		fn_itron_close
	else if deviceSelected$='Neptune (Equinox v4)' then
		fn_neptuneEquinoxV4_close
	end if

	if selection_method=sm_allExceptFinal or selection_method=sm_meterTypes then
		goto Finis
	else if selection_method=sm_aRoute then
		hbk=bk1
		goto AskRoute
	else if selection_method=sm_LocationId then
		goto Finis
	else
		goto NextAskAccount
	end if
goto NextAskAccount ! /r
Finis: ! r: Transfer to or from Hand Held Computer
	dim out_filename_report$*512
	out_filename_report$=file$(h_out)
	close #h_out: ioerr ignore
	close #hCustomeri1: ioerr ignore
	close #hCustomeri5: ioerr ignore

	dim hhrun$*256
	fnureg_read('Hand Held Run File',hhrun$)
	if hhrun$<>'' then
		hhrun$=srep$(hhrun$,'[file]',out_filename_report$)
		exec 'sy -C "'&hhrun$&'"'
	end if

	fn_reportCreatedFile(out_filename_report$)
	fn_transfer
goto Xit ! /r
Xit: fnXit

def fn_openOutFile ! open work areas based on type of Hand Held
	dim out_filename$*256
	! changed to next line on 2/2/2018      fnureg_read('Hand Held To File',out_filename$,'C:\mvrs\xfer\Download\Download.dat')
	fnureg_read('Hand Held To File',out_filename$,br_filename$(env$('Desktop')&'\ACS to '&deviceSelected$&'.txt'))
	if deviceSelected$='Itron FC300' then
		fn_itron_open ! default
	else
		h_out                 =fn_ifMatchOpenDo('Sensus'              ,'C:\vol002\amrs\READINGS.DAT'                       ,  80)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Green Tree'          ,'C:\READINGS.DAT'                                   ,  80)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Badger'              ,'C:\CONNECT\CONNECT.IN3'                            , 256)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Badger Connect C'    ,'C:\CONNECT\CONNECT.IN3'                            , 256)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Boson'               ,'[Q]\UBmstr\intopalm.txt'                           , 204)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('LapTop'              ,'[Q]\UBmstr\Laptop.Out'                             , 200)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('AMR'                 ,'C:\ezreader\download.dat'                          , 256)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Hersey'              ,'[Q]\UBmstr\READINGS.DAT'                           , 282,',eol=none')
		if h_out<=0 then h_out=fn_ifMatchOpenDo('EZReader'            ,'c:\ezreader\Download.dat'                          , 578,',eol=none')
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Unitech HT630'       ,'[temp]\[session]_uni_ht630.dat'          , 256)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Unitech HT630'       ,'[temp]\[session]_uni_ht630.dat'          , 256,',eol=none')
		if h_out<=0 then h_out=fn_ifMatchOpenDo('ACS Meter Reader'    ,'[temp]\[session]_acs_meter_data.txt'     , 256)
		! if h_out<=0 then h_out=fn_ifMatchOpenDo('Psion Workabout'   ,'[Q]\UBmstr\Readings.dat'                           , 128)
		! if h_out<=0 then h_out=fn_ifMatchOpenDo('Aclara Work Order' ,'[Desktop]\Aclara Work Order.txt',1048)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Aclara'              ,'[Desktop]\ACS to Aclara.txt'    ,1048)
		if h_out<=0 then h_out=fn_ifMatchOpenDo('Neptune (Equinox v4)','[Desktop]\ACS to Neptune (Equinox v4).txt',1048,',eol=none')
		if h_out<=0 then h_out=fn_ifMatchOpenDo(''                    ,'[Desktop]\ACS Hand Held Out.txt',1048)
	end if
	workopen=1
fnend
def fn_ifMatchOpenDo(deviceTest$*40,defaultOut_filename$*256,recordLength; extraParameter$*256)
	! inherrits deviceSelected$,out_filename$
	! returns open file handle
	if deviceTest$='' or deviceSelected$=deviceTest$ then
		if out_filename$='' then out_filename$=defaultOut_filename$
		fnMakeSurePathExists(env$('at')&out_filename$)
		open #hImodoReturn=fnH: 'Name=[at]'&out_filename$&',RecL='&str$(recordLength)&extraParameter$&',Replace',d,o
		if extraParameter$=',eol=none' then
			gRecLenRequired=1
		else
			gRecLenRequired=0
		end if
	end if
	fn_ifMatchOpenDo=hImodoReturn
fnend

! r: Hand Helds' SendRecordToOutFile functions (create line for customer in #h_out)
! r: aclara
def fn_aclara(aclaraLocationId) ! z$,mat e$,extra$(1-2),route
	dim tmpCity$*64,tmpState$*64,tmpZip$*64
	fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
	transmitterSerialNumber$=trim$(fn_meterInfo$('Transmitter Number',z$,'WA'))
	portNumber$=''
	posTsnDash=pos(transmitterSerialNumber$,'-')
	if posTsnDash>0 then
		portNumber$=transmitterSerialNumber$(posTsnDash+1:len(transmitterSerialNumber$))
		transmitterSerialNumber$(posTsnDash:len(transmitterSerialNumber$))=''
	end if

	fn_record_init(chr$(9))                                      ! Aclara Name               ACS Name (if different)
	fn_record_addc(5,cnvrt$('pic(#####)',aclaraLocationId))      ! LocationID
	fn_record_addc(10,z$)                                        ! Account Number
	fn_record_addc(30,e$(2))                                     ! Customer Name
	fn_record_addc(12,extra$(2))                                 ! Phone Number
	fn_record_addc(30,fn_meterInfo$('address',z$,'WA'))          ! Meter Address 1
	fn_record_addc(30,'')                                        ! blank
	fn_record_addc(30,tmpCity$)
	fn_record_addc(10,tmpState$)
	fn_record_addc(15,tmpZip$)
	fn_record_addn(3,route)                                      ! Cycle and Route            Route Number
	fn_record_addn(7,sequence)                                   ! Sequence                   Sequence
	fn_record_addc(8,fn_meterInfo$('Meter Number',z$,'WA'))    ! Meter Serial Number        Meter.Meter Number
	fn_record_addc(20,transmitterSerialNumber$)                  ! Transmitter Serial Number  Meter.Transmitter Number
	fn_record_addc(40,fn_meterInfo$('Meter Type',z$,'WA'))     ! Meter Model/Type
	fn_record_addc(1,portNumber$)                                ! Port Number
	fn_record_write(h_out, enableTrailingDelimiterOnLine=1)
fnend
! r: def fn_aclaraWorkOrder ! z$,mat e$,extra$(1-2),route
! 	dim tmpCity$*64,tmpState$*64,tmpZip$*64
! 	fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
! 	!
! 	fn_record_init(chr$(9))                                                            ! Aclara Name               ACS Name (if different)
! 	fn_record_addc(5,cnvrt$('pic(#####)',fnMeterAddressLocationID(e$(1), 1)))     ! LocationID
! 	fn_record_addc(10,z$)                                                              ! Account Number
! 	fn_record_addc(30,e$(2))                                                           ! Customer Name
! 	fn_record_addc(30,e$(1))                                                           ! Meter Address
! 	fn_record_addc(30,tmpCity$)
! 	fn_record_addc(10,tmpState$)
! 	fn_record_addc(15,tmpZip$)
! 	fn_record_addn(3,route)                                                            ! Cycle and Route            Route Number
! 	! fn_record_addn(7,sequence)                                                         ! Sequence                   Sequence
! 	fn_record_addc(12,f$(1)) ! fn_meterInfo$('Meter Number',z$,'WA')                         ! Meter Serial Number        Meter.Meter Number
! 	fn_record_addc(20,fn_meterInfo$('Transmitter Number',z$,'WA'))                  ! Transmitter Serial Number  Meter.Transmitter Number
! ! fn_record_addc(20,'(Rate Code Description??)')                                       ! Service Type
! 	aWmeterType=val(fn_meterInfo$('Meter Type',z$,'WA'))
! 	if aWmeterType=1 then ! r: get aWmeterType$
! 		aWmeterType$='1 inch'
! 	else if aWmeterType=21 then
! 		aWmeterType$='2 inch T-10'
! 	else if aWmeterType=15 then
! 		aWmeterType$='1.5 inch'
! 	else if aWmeterType=2 then
! 		aWmeterType$='2 inch Turbine'
! 	else if aWmeterType=3 then
! 		aWmeterType$='3 inch'
! 	else if aWmeterType=4 then
! 		aWmeterType$='4 inch'
! 	else if aWmeterType=6 then
! 		aWmeterType$='6 inch'
! 	else
! 		if aWmeterType<>5 then pr aWmeterType : pause
! 		aWmeterType$='5/8x3/4'
! 	end if ! /r
! 	fn_record_addc(40,aWmeterType$)                                                   ! Meter Model/Type
! 	fn_record_addn(10,xd(1))                                                           ! Service 1 (Water) – Reading – Current
! ! fn_record_addc(9,,fn_meterInfo$fn_meterInfo$('reading multiplier',z$,'WA'))                       ! Meter Size
! 	fn_record_addc(30,e$(3))                                                           ! Service Address 1          Address 1 - Primary
! 	fn_record_addc(30,extra$(1))                                                       ! Service Address 2          Address 2 - Primary
! 	fn_record_write(h_out)
! /r fnend
! /r
def fn_acsMeterReader
	! FILE (from ACS to Hand Held and from Hand Held to ACS) needs to contain the following fields:
	!   Account - 10 characters
	!   Route and Sequence - 12 digits (this is the order for accounts to be displayed in - it might contain duplicates and/or skip large ranges of numbers)
	!   Meter Type - 10 characters - 'Gas', 'Water', 'Electric', etc.  Each house may have multiple meters that need to be read.  If a house has both gas and water than it would have two records in the file so that both can be ask.  The Meter Type will need to be displayed so the user will know which they should be entering.
	!   Customer Name - 40 characters - The name of the customer who's meter is being read.  This should be displayed when the reading is ask for.
	!   Meter Address - 40 characters - The address of the customer who's meter is being read.
	!   This should be displayed when the reading is ask for.
	!   Reading High - 10 digits - used to validate entry of new reading
	!   Reading Low - 10 digits - used to validate entry of new reading
	!   Reading - 10 digits - the new reading
	for a_item=1 to udim(mat a)
		if serviceCode$(a_item)='WA' or serviceCode$(a_item)='GA' or serviceCode$(a_item)='EL' then ! or (demand)   it is a metered service
			if a(a_item)>0 then
				usage_current=fn_serviceDataN('current','usage',serviceCode$(a_item))
				reading_current=fn_serviceDataN('current','reading',serviceCode$(a_item))
				unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
				unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
				pr #h_out,using FORM_ACSMR: z$,route*100000000+sequence,serviceName$(a_item)(1:10),e$(2),e$(1)(1:20),unusual_usage_low,unusual_usage_high,0
				FORM_ACSMR: form pos 1,c 10,n 12,c 10,2*c 40,2*n 10,n 10
			end if  ! a(a_item)>0
		end if  ! it is a metered service
	next a_item
fnend  ! fn_acsMeterReader
def fn_amr ! AMR software solutions  ! same as ezreader, but specifically for Albany (who no longer uses ACS UB)
	if header=0 then
		if alp$(1:1)<>'*' then
			header=1 ! create header record
			if bk1>0 then route=bk1 else route=1 ! if they selected all for route number, make route number =1 else use the actual route number
			pr #h_out,using 'form pos 1,c 2,pic(##),pic(######),c 2': 'R1',1,route,crlf$
		end if
	end if
	! AMR Water
	pr #h_out,using L3230: 'M1', lpad$(rtrm$(z$),20),f$(1)(1:10),extra(2),'W',xd(1)+(xd(3)*2),xd(1)+(xd(3)*.50),'    ','    ','    ',e$(1),e$(2)(1:20),xd(1),extra(8),0,0,0,0,0,0,0,0,0,0,0,0,crlf$
	L3230: form pos 1,c 2,c 20,c 10,pic(######),c 4,2*pic(##########),3*c 4,c 40,c 20,pic(##########),n 4,pic(##),pic(#),2*pic(##########),2*pic(############),5*pic(##########),pic(########),c 2
fnend
def fn_badgerBeacon(account$*10,srvCode$*2)
		if ~beaconHeaderSent then ! r:
			beaconHeaderSent=1
			fn_record_init(chr$(9))
			fn_record_addc(10,'Account_ID')
			fn_record_addc(30,'Account_Full_Name')
			fn_record_addc(13,'Account_Phone')
			fn_record_addc(16,'Service_Point_ID')
			fn_record_addc(11,'Location_ID')
			fn_record_addc(30,'Location_Address_Line1')
			fn_record_addc(40,'Location_City')
			fn_record_addc(14,'Location_State')
			fn_record_addc(12,'Location_Zip')
			fn_record_addc(18,'Location_Latitude')
			fn_record_addc(18,'Location_Longitude')
			fn_record_addc(19,'Service_Point_Route')
			fn_record_addc(23,'Service_Point_Latitude')
			fn_record_addc(23,'Service_Point_Longitude')
			fn_record_addc(12,'Meter_ID') ! (meter number)
			fn_record_addc(12,'Meter_SN') ! (meter number)
			fn_record_addc(15,'Register_Number') ! (blank)
			fn_record_addc(24,'Register_Unit_Of_Measure') ! (ask and add)
			fn_record_addc(19,'Register_Resolution') ! meter type - reading multiplier   -   sorta - make sure logic is right
			fn_record_addc(20,'Endpoint_SN') ! Meter Location - Transmitter Serial Number
			fn_record_addc(19,'Endpoint_Type') ! meter type - read type
			fn_record_addc(13,'Read_Sequence') ! sequence
			fn_record_addc(15,'High_Read_Limit')
			fn_record_addc(14,'Low_Read_Limit')
			fn_record_write(h_out)
		end if ! /r
		dim tmpCity$*64,tmpState$*64,tmpZip$*64
		fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
		fn_record_init(tab$)                                                     ! BadgerBeacon Name      ACS Name (if different)
		fn_record_addC(10,account$)                                              ! Account_ID             Account Number
		fn_record_addC(30,fnCustomerData$(account$,'name', 1))                   ! Account_Full_Name      Customer Name
		fn_record_addC(13,extra$(2))                                             ! Account_Phone          Phone Number
		fn_record_addC(16,srvCode$)                                              ! Service_Point_ID
		fn_record_addC(11,fn_meterInfo$('Location_ID',account$,srvCode$))        ! Location_ID
		fn_record_addC(30,fn_meterInfo$('address',account$,srvCode$))            ! Location_Address_Line1
		fn_record_addC(40,tmpCity$)                                              ! Location_City
		fn_record_addC(14,tmpState$)                                             ! Location_State
		fn_record_addC(12,tmpZip$)                                               ! Location_Zip
		fn_record_addC(18,fn_meterInfo$('Latitude' ,account$,srvCode$))          ! Location_Latitude
		fn_record_addC(18,fn_meterInfo$('Longitude',account$,srvCode$))          ! Location_Longitude
		fn_record_addN(19,route)                                                 ! Service_Point_Route    Route Number
		fn_record_addC(23,fn_meterInfo$('Latitude' ,account$,srvCode$))          ! Service_Point_Latitude
		fn_record_addC(23,fn_meterInfo$('Longitude',account$,srvCode$))          ! Service_Point_Longitude
		fn_record_addC(12,fn_meterInfo$('Meter Number',account$,srvCode$))       ! Meter_ID                  (meter number)
		if env$('client')='Kincaid' then 
			fn_record_addC(12," ")                                                     ! removed for kincaid
		else 
			fn_record_addC(12,fn_meterInfo$('Meter Number',account$,srvCode$))       ! Meter_SN                  (meter number)
		end if 
		fn_record_addC(15,'')                                                    ! Register_Number           (blank)                  (meter number)
		fn_record_addC(24,'GAL')                                                 ! Register_Unit_Of_Measure
		fn_record_addC(19,fn_meterInfo$('reading multipler',account$,srvCode$))  ! Register_Resolution
		fn_record_addC(20,fn_meterInfo$('Transmitter Number',account$,srvCode$)) ! Endpoint_SN'                 Meter Location - Transmitter Serial Number
		
		if env$('client')='Moweaqua' or env$('client')='Kincaid' then 
			fn_record_addC(19,fn_meterInfo$('Meter Type',account$,srvCode$)(1:1))   ! just the first letter of the type code for Moweaqua
		else
			fn_record_addC(19,fn_meterInfo$('Meter Type',account$,srvCode$))         ! Endpoint_Type                meter type - read type
		end if
		
		fn_record_addN(13,sequence)                                              ! Read_Sequence                Sequence
		fn_record_addN(15,fn_unusualUsage('high',account$,srvCode$, 1))
		fn_record_addN(14,fn_unusualUsage('low' ,account$,srvCode$, 1))

		fn_record_write(h_out)
fnend
def fn_badgerConnectC ! older than BadgerBeacon
	for j=1 to len(seq$)
		on val(seq$(j:j)) goto BadgerCcWater,BadgerCcElectric,BadgerCcDemand,BadgerCcGas none BadgerCcNextSequence
		BadgerCcWater: !
		if a(1)=0 then goto BadgerCcNextSequence
		m$=ltrm$(f$(1))(1:10)
		if env$('client')='Moweaqua' then manual_or_dialog$=extra$(3)
		if env$('client')='Moweaqua' then extra$(3)=f$(1) ! they have meter number in first water meter number and a code in the second number
		! if env$('client')='Moweaqua' then xd(1)=xd(1) : xd(2)=xd(2) : xd(3)=xd(3)  ! this line dones nothing.  removed during customizations review 8/23/21
		dim rt$*4
		rt$=cnvrt$('pic(##)',extra(1))&'  '
		if env$('client')='Raymond' then manual_or_dialog$='N'
		if env$('client')='Raymond' and trim$(extra$(7))='' then extra$(7)='54'
		pr #h_out,using 'form pos 1,C 8,2*C 20,C 9,C 4,C 1,C 1,C 2,C 2,C 9,C 1,3*PIC(#########),C 8,C 2,C 2,C 4,C 15,C 8,C 1,3*C 6,C 2,PIC(######),C 20,C 30,C 3,C 2,C 2,C 2,C 6,C 18,C 1': '',e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9),'','A','','1 ','  ','        ',' ',xd(1)+(xd(3)*2),xd(1),0,'        ','  ','  ',rt$,z$,'        ',manual_or_dialog$(1:1),' ',' ',' ',extra$(7)(1:2),sequence,' ',' ',' ',' ',' ',' ',' ',' ','X'
		! serial # can be extra$(3) rather than f$(1)
		! replaced UPRC$(TRIM$(F$(1)))(1:1) with manual_or_dialog$
		goto BadgerCcNextSequence

		BadgerCcElectric: !
		if a(3)=0 or trim$(serviceName$(3))<>'Electric' then goto BadgerCcNextSequence
		m$=ltrm$(f$(2))(1:10)
		pr #h_out,using 'form pos 1,C 8,2*C 20,C 9,C 4,C 1,C 1,C 2,C 2,C 9,C 1,3*PIC(#########),C 8,C 2,C 2,C 4,C 15,C 8,C 1,3*C 6,C 2,PIC(######),C 20,C 30,C 3,C 2,C 2,C 2,C 6,C 18,C 1': ' ',e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9),' ','A',' ','3 ',' ',f$(2)(1:9),' ',xd(5)+(xd(7)*1.5),xd(5),0,' ',' ',' ',' ',z$,' ',uprc$(trim$(f1$))(1:1),' ',' ',' ',extra$(7)(1:2),sequence,' ',' ',' ',' ',' ',' ',' ',' ','X'
		L2010: form pos 1,c 8,2*c 20,c 9,c 4,c 1,c 1,c 2,c 2,c 9,c 1,3*pic(#########),c 8,c 2,c 2,c 4,c 15,c 8,c 1,3*c 6,c 2,pic(######),c 20,c 30,c 3,c 2,c 2,c 2,c 6,c 18,c 1
		goto BadgerCcNextSequence

		BadgerCcDemand: !
		! if ...
		! m$=''
		! pr #h_out,using L2010: ' ',e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9),' ','A',' ','4 ',' ',f$(2)(1:9),' ',xd(15)+(xd(15)*.5),xd(15)-(xd(15)*.5),0,' ',' ',' ',' ',z$,' ',manual_or_dialog$,' ',' ',' ',extra$(7)(1:2),sequence,' ',' ',' ',' ',' ',' ',' ',' ','X'
		goto BadgerCcNextSequence

		BadgerCcGas: !
		if a(4)=0 or trim$(serviceName$(4))<>'Gas' then goto BadgerCcNextSequence
		m$=ltrm$(f$(3))(1:10)
		pr #h_out,using L2010: ' ',e$(2)(1:20),e$(1)(1:20),trim$(extra$(3))(1:9),' ','A',' ','2 ',' ',f$(2)(1:9),' ',xd(9)+(xd(11)*1.5),xd(9),0,' ',' ',' ',' ',z$,' ','D',' ',' ',' ',extra$(7)(1:2),sequence,' ',' ',' ',' ',' ',' ',' ',' ','X'
		goto BadgerCcNextSequence

		BadgerCcNextSequence: !
	next j
fnend
def fn_boson(; ___,z_out$*14,custname$*30)
	for j=1 to len(seq$)
		if val(seq$(j:j))=1 then
			svc_flag$='W'
		else if val(seq$(j:j))=2 then
			svc_flag$='E'
		else if val(seq$(j:j))=4 then
			svc_flag$='G'
		end if
		custname$=e$(2)
		z_out$=trim$(z$)&svc_flag$
		on val(seq$(j:j)) goto Boson_Water,Boson_Electric,Boson_Demand,Boson_Gas none Boson_NextSequence
		Boson_Water: !
		if a(1)=0 or final<>0 then goto Boson_NextSequence
		x$=cnvrt$('pic(######)',xd(5)) : readdate$=x$(1:2)&'-'&x$(3:4)&'-'&x$(5:6)
		if env$('client')='Kincaid' then
			readingt$='S'
		else if env$('client')='Moweaqua' then
			if trim$(f$(1))='' then
				readingt$='S'
			else
				readingt$='P'
			end if
		else if trim$(extra$(3))='' then
			readingt$='S'
		else
			readingt$='P'
		end if
		if env$('client')='Purdy' or env$('client')='Billings' or env$('client')='Cerro Gordo V' then readingt$='S'
		metertag=0: metertag=val(extra$(3)) conv ignore
		if env$('client')='Moweaqua' then metertag=0: metertag=val(f$(1)) conv ignore
		if env$('client')='Moweaqua' and (a(1)=1 or a(1)=2) then xd(1)=xd(1): xd(2)=xd(2): xd(3)=xd(3)
		meterdials=0 ! if env$('client')='Purdy' or env$('client')='Billings' then meterdials=0 else meterdials=7
		! if trim$(z_out$)='200670' then pause
		pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),'',custname$,e$(1),'','',svc_flag$,f$(1),' ',0,xd(1)+(xd(3)*2),xd(1)+(xd(3)*.50),readdate$,route,'',sequence,meterdials,xd(1),readingt$,metertag
		!     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),'',custname$,e$(1),'','',svc_flag$,f$(1),' ',0,xd(1)+(xd(3)*2),xd(1)+(xd(3)*.50),readdate$,val(z$(1:2)),'',val(z$(3:7)),meterdials,xd(1),readingt$,metertag
		F_BOSON_OUT: form pos 1,c 14,c 3,3*c 30,2*c 1,c 20,c 5,3*pic(#########),pic(########),pic(####),c 1,pic(######),pic(##),pic(#########),c 1,pic(############)
		goto Boson_NextSequence

		Boson_Electric: if a(3)=0 or trim$(serviceName$(3))<>'Electric' then goto Boson_NextSequence
		pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),'',custname$,e$(1),'','',svc_flag$,f$(1),' ',0,xd(5)+(xd(7)*2),xd(5)+(xd(7)*.50),xd(5),route,'',sequence,0,xd(5),'R',f$(2)
		!     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),'',custname$,e$(1),'','',svc_flag$,f$(1),' ',0,xd(5)+(xd(7)*2),xd(5)+(xd(7)*.50),xd(5),val(z$(1:2)),'',val(z$(3:7)),0,xd(5),'R',f$(2)
		goto Boson_NextSequence

		Boson_Demand: goto Boson_NextSequence
		goto Boson_NextSequence

		Boson_Gas: if a(4)=0 or trim$(serviceName$(4))<>'Gas' then goto Boson_NextSequence
		readingt$='R'
		pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),'',custname$,e$(1),'','',svc_flag$,f$(1),' ',0,xd(9)+(xd(11)*2),xd(9)+(xd(11)*.50),xd(9),route,'',sequence,0,xd(9),readingt$,f$(2)
		!     pr #h_out,using F_BOSON_OUT: lpad$(rtrm$(z_out$),14),'',custname$,e$(1),'','',svc_flag$,f$(1),' ',0,xd(9)+(xd(11)*2),xd(9)+(xd(11)*.50),xd(9),val(z$(1:2)),'',val(z$(3:7)),0,xd(9),readingt$,f$(2)
		goto Boson_NextSequence

		Boson_NextSequence: !
	next j
fnend
def fn_laptop
	if a(1) and trim$(serviceName$(1))='Water' then
		write #h_out,using 'form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20':          z$,e$(2),e$(1),'W',watread ,watusage ,xd(1),xd(3) ,f$(1),ft$
	else if a(3) and trim$(serviceName$(3))='Electric' then
		write #h_out,using 'form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20':          z$,e$(2),e$(1),'E',elecread,elecusage,xd(5),xd(8) ,f$(2),ft$
	else if a(4) and trim$(serviceName$(4))='Gas' then
		write #h_out,using 'form pos 1,c 10,c 30,c 30,c 1,4*n 9,c 12,c 20,n 3,n 7':  z$,e$(2),e$(1),'G',gasread ,gasusage ,xd(9),xd(12),f$(3),ft$,route,sequence
	end if
fnend
def fn_masterMeter(; ___,tmpCity$*64,tmpState$*64,tmpZip$*64) ! z$,mat e$,extra$(1-2),route
	fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
	usage_current=xd(3) ! Water usage - current
	reading_current=xd(1)
	unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
	unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)

	fn_record_init(chr$(9))
	fn_record_addc(10,z$)                                             ! Account Number
	fn_record_addc(30,e$(2))                                          ! Customer Name
	fn_record_addc(30,e$(1))                                          ! Meter Address
	fn_record_addn(3,route)                                           ! Route Number
	fn_record_addn(7,sequence)                                        ! Sequence
	fn_record_addc(12,fn_meterInfo$('Meter Number',z$,'WA'))       ! Meter.Meter Number
	fn_record_addc(20,fn_meterInfo$('Transmitter Number',z$,'WA')) ! Transmitter Serial Number  Meter.Transmitter Number
	fn_record_addn(9,xd(1))                                            ! Service 1 (Water) – Reading – Current
	! pr 'AAA - '&srep$(rec_line$,chr$(9),'>') : pause
	fn_record_addc(17,fn_meterInfo$('longitude',z$,'WA'))          ! Meter.Longitude
	! pr 'BBB - '&srep$(rec_line$,chr$(9),'>') : pause
	fn_record_addc(17,fn_meterInfo$('latitude',z$,'WA'))           ! Meter.Latitude
	fn_record_addc(40,fn_meterInfo$('Meter Type',z$,'WA'))         ! Meter Model/Type
	tmp$=fn_meterInfo$('reading multiplier',z$,'WA') : if tmp$='' then tmp$='1'
	fn_record_addc(40,tmp$)                                           ! Meter Reading Multiplier (default to 1 if blank)
	fn_record_addc(9,'')                                              ! Service 1 (Water) – Reading – Bring Back (leave an empty column for it
	fn_record_addc(9,'')                                              ! Service 1 (Water) – Reading Date – Bring Back (leave an empty column for it
	fn_record_addn(10,unusual_usage_low)                              ! Unusual Usage Low Reading
	fn_record_addn(10,unusual_usage_high)                             ! Unusual Usage High Reading
	fn_record_write(h_out)
fnend
! r: neptuneEquinoxV4
def fn_neptuneEquinoxV4(h_out)
	! uses local mat xd
	! z$,route,sequence,e$(3),extra$(1),mat serviceCodeMetered$,mat serviceCode$
	! ; ___,serviceItem,sc$*2,reading_current,unusual_usage_low,unusual_usage_high
	if ~nev4_company_init then	! r: Company Record (COMHD)
		nev4_company_init=1

		nev4_routePrior=0
		nev4_routeCount=0

		fn_record_init
		fn_record_addc( 5,'COMHD'           ) ! Record ID
		fn_record_addc( 4,env$('cno')       ) ! Company Code  ???
		fn_record_addc( 8,date$('ccyymmdd') ) ! Create Date
		fn_record_addc(40,env$('cnam')      ) ! Description
		fn_record_addc( 1,'4'               ) ! File Version
		fn_record_addc( 1,'N'               ) ! Service Orders Y or N
		fn_record_write(h_out)
	end if	! /r
	if route<>nev4_routePrior then	! r: Route Record (RTEHD)
		if nev4_routePrior<>route and nev4_routePrior<>0 then
			fn_neptuneEquinoxV4_routeTrail(h_out,nev4_routePrior,nev4_routePremiseCount,nev4_routeMeterCount)
		end if
		nev4_routePrior=route
		nev4_routeCount+=1
		fn_record_init
		fn_record_addC( 5,'RTEHD'             ) ! Record ID        Req UB  1- 5 5 A/N 'RTEHD'
		fn_record_addC( 4,'WATE'              ) ! Office           Req UB  6- 9 4 A/N            = ???
		fn_record_addN( 4,1                   ) ! Cycle            Req UB 10-13 4 A/N            = ???
		fn_record_addN(10,route               ) ! Route            Req UB 14-23 10 A/N
		fn_record_addN( 8,date('ccyymmdd'),'0') ! Read Date        Req UB 24-31 8 NUM YYYYMMDD; 00000000 if not used.
		fn_record_addN( 8,date('ccyymmdd'),'0') ! Deactivate Date  Req UB 32-39 8 NUM YYYYMMDD; 00000000 if not used.
		fn_record_addC(80,''                  ) ! Route Message    Opt UB 40-119 80 A/N
		fn_record_write(h_out)                  ! CRLF             Req UB 120-121 2
	end if	! /r
	! r: Premise Detail Record (PRMDT)
	fn_record_init
	fn_record_addC(  5,'PRMDT'        ) !  Record ID      Req UB   1-5  5 A/N 'PRMDT'
	fn_record_addC( 26,e$(3)          ) !  Address 1      Req UB  6-31 26 A/N     =  Address 1 - Primary
	fn_record_addC( 26,extra$(1)      ) !  Address 2      Opt UB 32-57 26 A/N     =  Address 2 - Primary
	fn_record_addC( 26,e$(2)          ) !  Customer Name  Req UB 58-83 26 A/N
	fn_record_addC( 20,z$             ) !  Premise Key          Req UB 84-103   20 A/N Uniquely identifies the premise. Use account number, unless the billing system has a better key.
										! fn_meterInfo$('location_id',z$,sc$)  Would be a good premise key EXCEPT it's tied to a service, so each user could have multiple if they had multiple metered services
	fn_record_addC( 20,z$             ) !  Account Number       Req UB 104-123  20 A/N
	fn_record_addC(  4,'ACTI'         ) !  Account Status       Req UB 124-127   4 A/N Account status codes     !  'I believe they are INAC for inactive accounts & ACTI for active accounts.' -Keith
	fn_record_addC( 26,''             ) !  Premise Custom 1     Opt UB 128-153  26 A/N Custom display fields for premise screen.
	fn_record_addC( 26,''             ) !  Premise Custom 2     Opt UB 154-179  26 A/N
	fn_record_addC(128,''             ) !  Utility Pass Through Opt UB 180-307 128 A/N Any utility-defined information.
	fn_record_write(h_out)  !  CRLF                 Req UB 308-309   2
	! /r
	nev4_routePremiseCount+=1

	for serviceItem=1 to udim(mat serviceCode$)
		sc$=serviceCode$(serviceItem)
		if srch(mat serviceCodeMetered$,sc$)>0 then

			reading_current=fn_serviceDataN('current','reading',sc$)
			usage_current=fn_serviceDataN('current','usage',sc$)
			unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
			unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)

			! r: Meter Detail Record(s)
			fn_record_init
			fn_record_addC( 5,'MTRDT'                               ) !  Record ID
			fn_record_addN( 6,sequence, '0'                         ) ! Read Sequence
			fn_record_addC( 6,''                                    ) ! Changed Read Sequence
			fn_record_addC(20,fn_meterInfo$('meter number',z$,sc$)  ) ! Meter Key
			fn_record_addC(20,fn_meterInfo$('meter number',z$,sc$)  ) ! Meter Number
			fn_record_addC(20,''                                    ) ! Changed Meter Number
			fn_record_addC( 4,sc$                                   ) ! Meter Type
			fn_record_addC( 4,''                                    ) ! ChangedMeter Type
			fn_record_addC( 8,fn_meterInfo$('size',z$,sc$)          ) ! Meter Size
			fn_record_addC( 8,''                                    ) ! ChangedMeter Size
			fn_record_addC( 3,''                                    ) ! Meter Manufacturer
			fn_record_addC( 3,''                                    ) ! Changed Meter Manufacturer
			fn_record_addC( 3,fn_meterInfo$('unit',z$,sc$)          ) ! Meter Unit of Measure
			fn_record_addC( 3,''                                    ) ! Changed Meter Unit of Measure
			fn_record_addC( 4,''                                    ) ! Meter Location
			fn_record_addC( 4,''                                    ) ! Changed Meter Location
			fn_record_addC( 4,''                                    ) ! Meter Location 2
			fn_record_addC( 4,''                                    ) ! Changed Meter Location 2
			fn_record_addC( 4,''                                    ) ! Read Instruction 1
			fn_record_addC( 4,''                                    ) ! Changed Read Instruction 1
			fn_record_addC( 4,''                                    ) ! Read Instruction 2
			fn_record_addC( 4,''                                    ) ! Changed Read Instruction 2
			fn_record_addC(10,''                                    ) ! Seal Number
			fn_record_addC(10,''                                    ) ! Changed Seal Number
			fn_record_addC( 8,''                                    ) ! Meter Install Date (CCYYMMDD)
			fn_record_addC(26,''                                    ) ! Meter Custom 1
			fn_record_addC(26,''                                    ) ! Meter Custom 2
			fn_record_addC( 4,''                                    ) ! Meter Condition Code 1
			fn_record_addC( 4,''                                    ) ! Meter Condition Code 2
			fn_record_addC( 1,'N'                                   ) ! Must Read Code (Y/N)
			fn_record_addC(10,''                                    ) ! Collector Error
			fn_record_addC( 8,date$(val(fnCustomerData$(z$,'last reading day')),'ccyymmdd')   ) ! Previous Read Date (CCYYMMDD)
			fn_record_addC( 6,fn_meterInfo$('reading multipler',z$,sc$)                        ) ! Constant / Multiplier
			fn_record_addC( 6,''                                    ) ! Changed Constant / Multiplier
			fn_record_addC(12,''     ) ! Longitude   !  fn_meterInfo$('longitude',z$,sc$)   they didn't like the values when i sent them
			fn_record_addC(12,''     ) ! Latitude    !  fn_meterInfo$('latitude',z$,sc$)    they didn't like the values when i sent them
			fn_record_addC(12,''                                    ) ! For future use
			fn_record_addC(12,''                                    ) ! For future use
			fn_record_addC(12,''                                    ) ! For future use
			fn_record_addC(12,''                                    ) ! For future use
			fn_record_write(h_out)
			! /r
			nev4_routeMeterCount+=1
			! r: Read Detal Record
			fn_record_init
			fn_record_addC( 5,'RDGDT'                                          ) !  Record ID
			fn_record_addC( 4,fn_meterInfo$('read type',z$,sc$)                ) !  Read Type
			fn_record_addC(20,fn_meterInfo$('meter number',z$,sc$)             ) !  Changed Collection ID
			fn_record_addC(20,''                                                  ) !  Changed Collection ID
			! fn_record_addC(13,''                                               ) !  Collection ID ! swapped spots 
			! fn_record_addC( 7,''                                               ) !  For future use
			fn_record_addN( 2,val(fn_meterInfo$('number of dials',z$,sc$)),'0' ) ! Dials                         Req  UB 50-51     2 NUM
			fn_record_addN( 2,0                                                ) ! Changed Dials                 Opt  HH 52-453    2 NUM
			fn_record_addN( 2,0                                                ) ! Decimals                      Req  UB 54-55     2 NUM
			fn_record_addN( 2,0                                                ) ! Changed Decimals              Opt  HH 56-57     2 NUM
			fn_record_addC( 1,''                                               ) ! Read Direction                Opt  UB 58        1 A/N R, L, C, or blank only
			fn_record_addN(10,unusual_usage_high                               ) ! Hi Limit                      Req  UB 59-68    10 NUM
			fn_record_addN(10,unusual_usage_low                                ) ! Low Limit                     Req  UB 69-78    10 NUM
			fn_record_addN(10,fn_serviceDataN('prior','reading',sc$)           ) ! Prev Read                     Req  UB 79-88    10 NUM
			fn_record_addN(10,reading_current                                  ) ! Reading                       Req  HH 89-98    10 A/N
			fn_record_addC(10,''                                               ) ! Collector Reading             Req  HH 99-108   10 A/N Actual reading that came from collector, before truncation
			fn_record_addC( 2,''                                               ) ! Read Code                     Req  HH 109-110   2 A/N
			fn_record_addN( 2,0                                                ) ! Re-entry Count                Req  HH 111-112   2 NUM
			fn_record_addN( 1,0                                                ) ! Water No Flow 35 Days         Req  HH 113       1 NUM Number of Days
			fn_record_addN( 1,0                                                ) ! Peak Backflow                 Req  HH 114       1 NUM Reverse Flow Event
			fn_record_addN( 1,0                                                ) ! Leak 35 Days                  Req  HH 115       1 NUM Number of Days
			fn_record_addN( 1,0                                                ) ! Current Leak                  Req  HH 116       1 NUM Leak Status
			fn_record_addN( 1,0                                                ) ! Previous Error Count          Opt  UB 117       1 NUM R900 Electric or Gas tamper. Use '8' tosuppress tamper check.
			fn_record_addN( 1,0                                                ) ! Current Error Count           Req HH 118        1 NUM Current error/tamper count for R900 Electric or Gas
			fn_record_addN( 1,0                                                ) ! Fatal Error                   Req HH 119        1 NUM Fatal error flag for R900 Electric
			fn_record_addN( 1,0                                                ) ! Non-Fatal Error/Flags         Req HH 120        1 NUM Non-fatal error flag for R900 Electric or Gas
			fn_record_addN( 3,0                                                ) ! Voltage                       Req HH 121-123    3 NUM Operating meter voltage for R900 Electric
			fn_record_addC( 2,''                                               ) ! MIU Type                      Req HH 124-125    2 NUM Utility meter type
			fn_record_addN( 2,0                                                ) ! AMR Read Type                 Req HH 126-127    2 NUM AMR reading type
			fn_record_addN( 1,0                                                ) ! High Power                    Req HH 128        1 NUM High versus low power indicator for all R900s
			fn_record_addN( 2,0                                                ) ! R900 Format                   Req HH 129-130    2 NUM The R900 reading formal:  0 - Binary  1 - BCD  2 - 'Data Stream' (not used)  3 - E-Coder  4 - Mlog
			fn_record_addN( 1,0                                                ) ! Display Digits                Req HH 131        1 NUM Number of digits in main reading display
			fn_record_addN( 1,0                                                ) ! Multiplier Applied            Req HH 132        1 NUM
			fn_record_addN( 1,0                                                ) ! Gas No Flow                   Req HH 133        1 NUM Period for which there has been no gas flow
			fn_record_addN( 1,0                                                ) ! Current Gas Backflow Tamper   Req HH 134        1 NUM
			fn_record_addN( 1,0                                                ) ! Current Gas Removal Tamper    Req HH 135        1 NUM
			fn_record_addN( 1,0                                                ) ! Current Gas Magnetic Tamper   Req HH 136        1 NUM
			fn_record_addN( 1,0                                                ) ! ERT Inversion Tamper          Req HH 137        1 NUM
			fn_record_addN( 1,0                                                ) ! ERT Reverse Tamper            Req HH 138        1 NUM
			fn_record_addN( 1,0                                                ) ! 35-Day Gas Backflow  Tamper   Req HH 139        1 NUM
			fn_record_addN( 1,0                                                ) ! 35-Day Gas Removal Tamper     Req HH 140        1 NUM
			fn_record_addN( 1,0                                                ) ! 35-Day Gas Magnetic Tamper    Req HH 141        1 NUM
			fn_record_addN( 1,0                                                ) ! 35-Day Program Flag           Req HH 142        1 NUM R900G only
			fn_record_addN( 1,0                                                ) ! Reed Switch Failure Flag      Req HH 143        1 NUM R900G only
			fn_record_addC(69,''                                               ) ! Additional Flags              Req HH 144-212   69 For future use
			fn_record_addC(25,''                                               ) ! Register Manufacturer         Req HH 213-237   25 A/N
			fn_record_addc( 8,''                                               ) ! Register Install Date         Req HH 238-245    8 NUM YYYYMMDD
			fn_record_addC(10,''                                               ) ! Register ID                   Req HH 246-255   10 A/N
			fn_record_write(h_out)                                               ! CRLF                          Req UB 256-257    2
			! /r
		end if
	next serviceItem
fnend
def fn_neptuneEquinoxV4_routeTrail(h_out,nev4_routePrior,&nev4_routePremiseCount,&nev4_routeMeterCount)
	fn_record_init
	fn_record_addC( 5,'RTETR'                ) ! Record ID  Req UB 1-5    5 A/N 'RTETR'
	fn_record_addC( 4,'WATE'                 ) ! Office     Req UB 6-9    4 A/N
	fn_record_addN( 4,1                      ) ! Cycle      Req UB 10-13  4 A/N
	fn_record_addN(10,nev4_routePrior        ) ! Route      Req UB 14-23 10 A/N
	fn_record_addN( 6,nev4_routePremiseCount ) ! # Premises Req UB 24-29  6 A/N One input field may be blank if total is unavailable.
	fn_record_addN( 6,nev4_routeMeterCount   ) ! # Meters   Req UB 30-35  6 A/N One input field may be blank if total is unavailable.
	fn_record_write(h_out)                     ! CRLF       Req UB 36-37  2
	nev4_routePremiseCount=0
	nev4_routeMeterCount=0
fnend
def fn_neptuneEquinoxV4_close
	fn_neptuneEquinoxV4_routeTrail(h_out,nev4_routePrior,nev4_routePremiseCount,nev4_routeMeterCount)
	! Company Trailer
	fn_record_init
	fn_record_addC( 5,'COMTR'            ) ! Record ID      Opt UB 1-5   5 A/N 'COMTR'
	fn_record_addC( 4,env$('cno')        ) ! Company Code   Req UB 6-9   4 A/N
	fn_record_addN( 5,nev4_routeCount    ) ! # Routes       Req UB 10-15 6 A/N One input field may be blank if total is unavailable
	fn_record_write(h_out)                 ! CRLF           Req UB 16-17 2
fnend
! /r
def fn_readyWater ! z$,mat e$,extra$(1-2),route
	dim tmpCity$*64,tmpState$*64,tmpZip$*64
	fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
	fn_record_init(chr$(9))                                           ! ACS Name (if different)
	fn_record_addc(10,z$)                                             ! Account Number
	fn_record_addc(30,e$(2))                                          ! Customer Name
	fn_record_addc(12,extra$(2))                                      ! Phone Number
	fn_record_addc(30,e$(1))                                          ! Meter Address (switched to 7/5/17 as per request by Sheri)
	! fn_record_addc(30,e$(3))                                          ! Address 1 - Primary
	fn_record_addc(30,extra$(1))                                      ! Address 2 - Primary
	fn_record_addc(30,tmpCity$)                                       ! City
	fn_record_addc(10,tmpState$)                                      ! State
	fn_record_addc(15,tmpZip$)                                        ! Zip
	fn_record_addn(3,route)                                           ! Route Number
	fn_record_addn(7,sequence)                                        ! Sequence
	fn_record_addc(12,fn_meterInfo$('Meter Number',z$,'WA'))         ! Meter.Meter Number
	fn_record_write(h_out)
fnend
def fn_readyKamstrup(h_out,account$*10,srvCode$*2)
	dim tmpCity$*64,tmpState$*64,tmpZip$*64
	fncsz(e$(4),tmpCity$,tmpState$,tmpZip$)
	fn_record_init(chr$(9))                                             ! ACS Names
	fn_record_addc(10,account$)                                         ! Account Number
	fn_record_addc(30,e$(2))                                            ! Customer Name
	fn_record_addc(12,extra$(2))                                        ! Phone Number
	fn_record_addc(30,e$(1)    )                                        ! Meter Address
	! fn_record_addc(30,e$(3))                                          ! Address 1 - Primary
	fn_record_addc(30,extra$(1))                                        ! Address 2 - Primary
	fn_record_addc(30,tmpCity$ )                                        ! City
	fn_record_addc(10,tmpState$)                                        ! State
	fn_record_addc(15,tmpZip$  )                                        ! Zip
	fn_record_addn( 3,route    )                                        ! Route Number
	fn_record_addn( 7,sequence )                                        ! Sequence
	fn_record_addc(12,fn_meterInfo$('Meter Number',account$,srvCode$))  ! Meter.Meter Number
	fn_record_addC(12,fn_meterInfo$('longitude',account$,srvCode$)    )
	fn_record_addC(12,fn_meterInfo$('latitude' ,account$,srvCode$)    )
  fn_record_addN(10,fn_serviceDataN('prior','reading',srvCode$)     ) ! Prev Read
  fn_record_addN(10,fn_serviceDataN('current','reading',srvCode$)     ) ! current Read
	fn_record_write(h_out)
fnend
! r: itron
def fn_itron_open
	open #h_out=fnH: 'Name=[Q]\HH[session].int,RecL=128,EoL=None,Replace',i,outi,r
	fn_itron_record_fhd
	itron_rdg_count=0
	itron_cus_count=0
	itron_mtr_count=0
	itron_rtr_count=0
	itron_chd_count=0
fnend
def fn_itron_close
	fn_itron_route_trailer
	fn_itron_record_ftr

	rec_current=0 ! restore #h_out:
	do
		rec_current+=1
		if rec_current>lrec(h_out) then goto IC_EOF_1
		read #h_out,using 'form pos 1,C 126',rec=rec_current: rec_line$
		rec_type$=rec_line$(1:3)
		if rec_type$='RHD' then ! route header
			! itron_rhd_current=rec(h_out)
		else if rec_type$='RTR' then ! route trailer
			itron_rtr_current=rec(h_out)
			rewrite #h_out,using 'form pos 18,n 4,pos 34,N 4,N 4,N 4,pos 52,3*N 4',rec=itron_rtr_current: itron_rdg_count,itron_rff_count,itron_cus_count,itron_mtr_count,itron_mtr_g_count,itron_mtr_w_count,itron_mtr_e_count noRec ignore
			itron_rdg_count=0
			itron_rdg_count=0
			itron_rff_count=0
			itron_cus_count=0
			itron_mtr_count=0
			itron_mtr_e_count=0
			itron_mtr_g_count=0
			itron_mtr_i_count=0
			itron_mtr_s_count=0
			itron_mtr_w_count=0
		else if rec_type$='RFF' then
			itron_rff_count+=1
		else if rec_type$='CUS' then
			itron_cus_count+=1
		else if rec_type$='MTR' then
			itron_mtr_count+=1
			itron_meter_category$=rec_line$(102:102)
			if itron_meter_category$='E' then
				itron_mtr_e_count+=1
			else if itron_meter_category$='G' then
				itron_mtr_g_count+=1
			else if itron_meter_category$='I' then
				itron_mtr_i_count+=1
			else if itron_meter_category$='S' then
				itron_mtr_s_count+=1
			else if itron_meter_category$='W' then
				itron_mtr_w_count+=1
			end if
		else if rec_type$='RDG' then
			itron_rdg_count+=1
		else if rec_type$='FTR' then
			itron_ftr_current=rec(h_out)
			rewrite #h_out,using 'form pos 14,n 2',rec=itron_fhd_current: itron_chd_count
			rewrite #h_out,using 'form pos 14,n 2',rec=itron_ftr_current: itron_chd_count
			itron_chd_count=0
		else if rec_type$='FHD' then
			itron_fhd_current=rec(h_out)
		else if rec_type$='CHD' then
			itron_chd_count+=1
		end if  ! rec_type$=...
	loop
	IC_EOF_1: !
	!
	open #h_out2=fnH: 'Name=[Q]\Download.dat,RecL=128,EoL=None,Replace',d,o
	restore #h_out:
	do
		read #h_out,using 'form pos 1,C 126': rec_line$ eof IC_EOF_2
		pr #h_out2,using 'form pos 1,C 126,c 2': rec_line$,crlf$
	loop
	IC_EOF_2: !
	close #h_out2:
	close #h_out,free:
	fnMakeSurePathExists(env$('at')&out_filename$)
	fnCopy('[Q]\Download.dat',env$('at')&out_filename$)
	fn_reportCreatedFile(out_filename$)
	!   if exists ('C:\MVRS\MVRSWin5.exe') then
	!     if ~exists ('C:\MVRS\MVRSWin5.cmd') then
	!       open #h_tmp=fnH: 'Name=C:\MVRS\MVRSWin5.cmd,RecL=256,replace',d,o
	!       pr #h_tmp: 'c:'
	!       pr #h_tmp: 'cd \MVRS'
	!       pr #h_tmp: 'C:\MVRS\MVRSWin5.exe'
	!       close #h_tmp:
	!     end if
	!     execute 'Sy -c C:\MVRS\MVRSWin5.cmd'
	!   end if
fnend
def fn_itron_route_trailer
	fn_itron_record_rtr
	fn_itron_record_ctr
	itron_rtr_count+=1
fnend  ! fn_itron_route_trailer
def fn_itron
	for a_item=1 to udim(mat a)
		if serviceCode$(a_item)='WA' or serviceCode$(a_item)='GA' or serviceCode$(a_item)='EL' then ! or (demand)   it is a metered service
			if a(a_item)>0 then
				usage_current=fn_serviceDataN('current','usage',serviceCode$(a_item))
				reading_current=fn_serviceDataN('current','reading',serviceCode$(a_item))
				unusual_usage_low=int(usage_current-usage_current*fn_pcent) : if unusual_usage_low<0 then unusual_usage_low=0
				unusual_usage_high=int(usage_current+usage_current*fn_pcent)
				if z$<>z_prior$ then
					z_prior$=z$
					if route<>route_prior then
						if route_prior<>0 then
							fn_itron_route_trailer
						end if  ! route_prior<>0
						route_prior=route
						fn_itron_record_chd
						route_itron$=cnvrt$('pic(##)',route)&cnvrt$('pic(######)',route)
						fn_itron_record_rhd
					end if  ! route<>route_prior
					fn_itron_record_cus
				end if  ! z$<>z_prior$
				fn_itron_record_mtr
				fn_itron_record_mtx
				unusual_usage_low=int(usage_current-usage_current*fn_pcent) : if unusual_usage_low<0 then unusual_usage_low=0
				unusual_usage_high=int(usage_current+usage_current*fn_pcent)
				fn_itron_record_rdg
				fn_itron_record_rff
			end if  ! a(a_item)>0
		end if  ! it is a metered service
	next a_item
fnend
def fn_itron_record_rdg ! reading - pg 19
	fn_record_init
	fn_record_addc(3,'RDG')
	fn_record_addc(8,route_itron$)
	fn_record_addc(4,serviceCode$(a_item))
	fn_record_addc(1,'Y')
	fn_record_addc(1,'L') ! field 5
	fn_record_addn(3,0)
	fn_record_addn(3,0)
	fn_record_addx(1)
	fn_record_addn(2,0)
	!
	itron_number_of_dials=val(fn_meterInfo$('Number of Dials',z$,serviceCode$(a_item)))
	! pr z$&' '&serviceCode$(a_item)&' itron_number_of_dials=';itron_number_of_dials : prdebugcount+=1 : if prdebugcount/24=int(prdebugcount/24) then pause
	! if trim$(z$)='200030.00' then pr 'itron_number_of_dials=';itron_number_of_dials : pause
	if itron_number_of_dials=0 then itron_number_of_dials=6
	fn_record_addn(2,itron_number_of_dials) ! field 10  -  number of dials
	fn_record_addn(2,0) !
	dim transmitter_number$*128
	transmitter_number$=fn_meterInfo$('transmitter number',z$,serviceCode$(a_item))
	if transmitter_number$<>'' then
		fn_record_addc(1,'R')
	else
		fn_record_addc(1,'K')
		skip_next_rff_record=1
	end if
	fn_record_addn(10,reading_current)
	fn_record_addn(10,unusual_usage_high)
	fn_record_addn(10,unusual_usage_low) ! field 15
	fn_record_addn(6,0)
	fn_record_addn(1,0)
	fn_record_addn(1,0)
	fn_record_addn(5,0)
	fn_record_addn(1,0) ! field 20
	fn_record_addx(1)
	itron_read_type=0
	itron_read_type=val(fn_meterInfo$('Read Type',z$,serviceCode$(a_item))) conv ignore
	if itron_read_type=0 then itron_read_type=a_item ! gas, water, electric a unique number for each - a_item (service number) is as good as any
	fn_record_addc(2,cnvrt$('pic(##)',itron_read_type))
	fn_record_addn(6,0)
	fn_record_addn(6,0)
	fn_record_addn(5,0) ! field 25
	fn_record_addx(31)
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend  ! fn_itron_record_rdg
def fn_itron_record_rhd ! route header - pg 6
	fn_record_init
	fn_record_addc(3,'RHD')
	fn_record_addc(8,route_itron$)
	fn_record_addc(1,'N')
	fn_record_addc(1,'N')
	fn_record_addn(4,0) ! field 5 - total number of keys
	fn_record_addn(4,0) ! field 6 - total number of reading records
	fn_record_addn(4,0) ! field 7 - total number of demand meters
	fn_record_addn(4,0) ! field 8 - total number of keyed readings
	fn_record_addn(4,0) ! field 9 - total number of optical probe readings
	fn_record_addn(4,0) ! field 10 - total number of off-site (Radio) readings
	fn_record_addn(4,0) ! field 11 - total number of customer records
	fn_record_addn(4,0) ! field 12 - total number of meter records
	fn_record_addn(6,0)
	fn_record_addn(4,0)
	fn_record_addn(4,0) ! field 15
	fn_record_addn(4,0)
	fn_record_addn(4,0)
	fn_record_addn(4,0)
	fn_record_addc(2,'')
	fn_record_addc(2,'') ! field 20 - zone
	fn_record_addc(2,'')
	fn_record_addn(2,0)
	fn_record_addn(2,0)
	fn_record_addn(4,0)
	fn_record_addc(1,'') ! field 25
	fn_record_addx(40)
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend  ! fn_itron_record_rhd
def fn_itron_record_rtr ! route trailer - pg 6
	fn_record_init
	fn_record_addc(3,'RTR')
	fn_record_addc(8,route_itron$)
	fn_record_addc(1,'N')
	fn_record_addc(1,'N')
	fn_record_addn(4,0) ! field 5 - total number of keys
	fn_record_addn(4,0) ! field 6 - total number of reading records
	fn_record_addn(4,0) ! field 7 - total number of demand meters
	fn_record_addn(4,0) ! field 8 - total number of keyed readings
	fn_record_addn(4,0) ! field 9 - total number of optical probe readings
	fn_record_addn(4,0) ! field 10 - total number of off-site (Radio) readings
	fn_record_addn(4,0) ! field 11 - total number of customer records
	fn_record_addn(4,0) ! field 12 - total number of meter records
	fn_record_addn(6,0)
	fn_record_addn(4,0)
	fn_record_addn(4,0) ! field 15
	fn_record_addn(4,0)
	fn_record_addn(4,0)
	fn_record_addn(4,0)
	fn_record_addc(2,'')
	fn_record_addc(2,'') ! field 20 - zone
	fn_record_addc(2,'')
	fn_record_addn(2,0)
	fn_record_addn(2,0)
	fn_record_addn(4,0)
	fn_record_addc(1,'') ! field 25
	fn_record_addx(40)
			! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend  ! fn_itron_record_rtr
def fn_itron_record_cus ! Customer - pg 11
	fn_record_init
	fn_record_addc(3,'CUS')
	fn_record_addc(8,route_itron$)
	fn_record_addn(3,fn_CountOfMeteredSvcsActive)
	fn_record_addc(20,z$)
	fn_record_addc(20,e$(2)) ! field 5 - name
	fn_record_addc(20,e$(1))
	! fn_record_addc(6,'')
	! fn_record_addc(14,'')
	fn_record_addc(20,'')
	fn_record_addx(2)
	fn_record_addn(1,0)
	fn_record_addc(20,'') ! field 10 - Customer Information
	fn_record_addc(1,'N')
	fn_record_addc(4,'')
	fn_record_addc(2,'')
	fn_record_addc(1,'')
	fn_record_addx(1) ! field 15
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend  ! fn_itron_record_cus
def fn_itron_record_mtx ! latitude, longitude, etc - pg 16
	fn_record_init
	fn_record_addc(3,'MTX')
	fn_record_addc(8,route_itron$)
	fn_record_addc(12,fn_meterInfo$('meter number',z$,serviceCode$(a_item)))
	dim irm_tmp$*20
	irm_tmp$=lwrc$(fn_meterInfo$('longitude',z$,serviceCode$(a_item)))
	if irm_tmp$(1:1)='n' or irm_tmp$(1:1)='s' or irm_tmp$(1:1)='e' or irm_tmp$(1:1)='w' then irm_tmp$=str$(fn_dms_to_dec(irm_tmp$))
	fn_record_addc(17,irm_tmp$)
	irm_tmp$=lwrc$(fn_meterInfo$('latitude',z$,serviceCode$(a_item)))
	if irm_tmp$(1:1)='n' or irm_tmp$(1:1)='s' or irm_tmp$(1:1)='e' or irm_tmp$(1:1)='w' then irm_tmp$=str$(fn_dms_to_dec(irm_tmp$))
	fn_record_addc(17,irm_tmp$)
	fn_record_addc(12,'')
	fn_record_addx(57)
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend  ! fn_itron_record_mtx
def fn_dms_to_dec(dtd_in$*20) ! for longitude and latitude
	! N31 35 47.8
	if dtd_in$(1:1)='n' then dtd_sign$='+' : dtd_in$(1:1)=''
	if dtd_in$(1:1)='e' then dtd_sign$='+' : dtd_in$(1:1)=''
	if dtd_in$(1:1)='s' then dtd_sign$='-' : dtd_in$(1:1)=''
	if dtd_in$(1:1)='w' then dtd_sign$='-' : dtd_in$(1:1)=''

	dtd_pos_space=pos(dtd_in$,' ')
	dtd_degrees=val(dtd_in$(1:dtd_pos_space))
	dtd_in$(1:dtd_pos_space)=''

	dtd_pos_space=pos(dtd_in$,' ')
	dtd_minutes=val(dtd_in$(1:dtd_pos_space))
	dtd_in$(1:dtd_pos_space)=''

	dtd_seconds=val(dtd_in$) conv ignore
	dtd_return=dtd_degrees+dtd_minutes/60+dtd_seconds/3600
	if dtd_sign$='-' then dtd_return=-dtd_return
	fn_dms_to_dec=dtd_return
fnend
def fn_itron_record_rff ! off-site (Radio) reads - pg 22
	if skip_next_rff_record=1 then
		skip_next_rff_record=0
	else
		fn_record_init
		fn_record_addc(3,'RFF')
		fn_record_addc(8,route_itron$)
		if env$('client')='Bethany' then let transmitterlen=10 else let transmitterlen=8 ! longer transmitter number for some at Bethany
		fn_record_addc(transmitterlen,fn_meterInfo$('transmitter number',z$,serviceCode$(a_item)))
		if env$('client')='Bethany' then let blanklen=4 else let blanklen=6 ! adjust blank after it for those at Bethany
		fn_record_addc(6,'')
		fn_record_addc(4,'ERT ') ! field 5
		fn_record_addx(7)
		fn_record_addn(2,0)
		fn_record_addn(12,0)
		fn_record_addn(4,0)
		fn_record_addx(10) ! field 10
		fn_record_addc(2,'16')
		fn_record_addc(1,'')
		fn_record_addc(1,'')
		fn_record_addc(1,'')
		fn_record_addc(1,'') ! field 15
		fn_record_addx(56)
		! fn_record_addc(2,crlf$)
		fn_record_write(h_out)
	end if
fnend
def fn_itron_record_fhd ! file header - pg 3
	fn_record_init
	fn_record_addc(3,'FHD')
	fn_record_addc(1,'N')
	fn_record_addc(1,'N')
	fn_record_addc(5,'')
	fn_record_addx(3) ! field 5
	fn_record_addn(2,99) ! field 6 - number of cycles - should be one for each route
	fn_record_addc(1,'Y') ! field 7 - RFF records present?  Y/N
	fn_record_addc(1,'N')
	fn_record_addc(1,'N')
	fn_record_addx(108) ! field 10
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend
def fn_itron_record_ftr ! file trailer - pg 3
	fn_record_init
	fn_record_addc(3,'FTR')
	fn_record_addc(1,'N')
	fn_record_addc(1,'N')
	fn_record_addc(5,'')
	fn_record_addx(3) ! field 5
	fn_record_addn(2,99) ! field 6 - number of cycles - should be one for each route
	fn_record_addc(1,'Y') ! field 7 - RFF records present?  Y/N
	fn_record_addc(1,'N')
	fn_record_addc(1,'N')
	fn_record_addx(108) ! field 10
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend
def fn_itron_record_chd ! cycle header - pg 5
	fn_record_init
	fn_record_addc(3,'CHD')
	fn_record_addc(2,cnvrt$('pic(##)',route))
	fn_record_addn(4,1)
	fn_record_addc(8,date$('mmddccyy'))
	fn_record_addx(109) ! field 5
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend
def fn_itron_record_ctr ! cycle trailer - pg 5
	fn_record_init
	fn_record_addc(3,'CTR')
	fn_record_addc(2,cnvrt$('pic(##)',route))
	fn_record_addn(4,1)
	fn_record_addc(8,date$('mmddccyy'))
	fn_record_addx(109) ! field 5
	! fn_record_addc(2,crlf$)
	fn_record_write(h_out)
fnend
def fn_itron_record_mtr ! meter record - pg 13
	fn_record_init
	fn_record_addc(3,'MTR')
	fn_record_addc(8,route_itron$)
	fn_record_addn(3,1)
	fn_record_addx(2)
	fn_record_addn(1,0) ! field 5
	fn_record_addx(8)
	fn_record_addn(1,0)
	fn_record_addx(2)
	fn_record_addn(1,0)
	fn_record_addc(1,' ') ! field 10
	fn_record_addc(1,'A')
	fn_record_addc(14,'') ! field 12 - optiocal probe recorder ID
	fn_record_addc(12,fn_meterInfo$('Meter Number',z$,serviceCode$(a_item)))
	fn_record_addx(2)
	fn_record_addc(2,'00') ! field 15 - meter type
	fn_record_addn(8,sequence*10+a_item)
	fn_record_addx(20)
	fn_record_addx(1)
	fn_record_addc(2,'00')
	fn_record_addx(1) ! field 20
	fn_record_addc(2,'00')
	fn_record_addx(1)
	fn_record_addc(2,'00')
	fn_record_addn(1,3)
	fn_record_addc(1,'Y') ! field 25
	fn_record_addc(1,'N')
	fn_record_addc(1,serviceCode$(a_item)(1:1))
	fn_record_addc(1,'L')
	fn_record_addn(3,0)
	fn_record_addc(2,'') ! field 30 - meter audit 1
	fn_record_addc(2,'')
	fn_record_addc(1,'')
	fn_record_addc(1,'')
	fn_record_addx(14)
	! fn_record_addc(2,crlf$) ! field 35 (the end CR/LF)
	fn_record_write(h_out)
fnend
! /r
def fn_legacyMultiDevice
	cd$='M' !  - included in several records - maybe some sort of meter id - not sure
	! r: make c$ - a legacy customer service list for the following loop to walk through
	c$=''
	if a(1)>0 then c$='1'
	if a(3)=5 then c$=c$&'5' else if a(3)>0 then c$=c$&'3'
	if a(4)>0 then c$=c$&'4'
	! /r
	if rtrm$(f$)='' then f$=z$
	for j=1 to len(c$)
		if deviceSelected$='Green Tree' then
			if val(c$(j:j))=1 then ! Water
				pr #h_out,using 'form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 1': z$,e$(2)(1:18)&'-W',e$(1)(1:20),xd(1),xd(3),1,extra$(3)(1:10),cd$
			end if
		else if deviceSelected$='Hersey' then
			if val(c$(j:j))=1 then ! Water
				pr #h_out,using 'form pos 1,c 10,c 4,c 6,c 1,c 25,c 21,c 20,c 1,n 10,n 10,c 100,c 2,c 1,c 5,c 12,c 52,pos 281,c 2': z$,' ',' ','W',e$(2)(1:25),e$(1)(1:21),f$(1),'V',xd(1)+(xd(3)*2),xd(1),' ',' ',' ',' ',z$,' ',chr$(13)&chr$(10)
			end if
		else if deviceSelected$='EZReader' then
			if val(c$(j:j))=1 then ! Water
				pr #h_out,using 'form pos 1,c 12,c 2,c 1,c 66,c 64,c 14,c 1,2*pic(##########),pic(##),c 120,c 24,c 24,c 20,c 80,c 125,c 1,c 2': cnvrt$('pic(##)',route)&cnvrt$('pic(#######)',sequence),'  ','W',e$(2),e$(1),f$(1),extra$(3)(1:1),xd(1)+(xd(3)*2),xd(1),0,' ',' ',' ',z$,' ',' ','X',chr$(13)&chr$(10)
			end if
		else if deviceSelected$='Sensus' then
			L2520: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 1,n 9
			if val(c$(j:j))=1 then ! Water
				pr #h_out,using L2520: z$,e$(2)(1:18)&'-W',e$(1)(1:20),xd(1),xd(3),1,extra$(3)(1:10),cd$
			else if val(c$(j:j))=3 then ! Electric
				if xd(14)<>0 then xd(7)=xd(7)/(xd(14)*.01) ! COMPARE USAGE BEFORE MULTIPLIER
				pr #h_out,using L2520: z$,e$(2)(1:18)&'-E',e$(1)(1:20),xd(5),xd(7),3,extra$(3)(1:10),cd$
			else if val(c$(j:j))=5 then ! Demand
				pr #h_out,using L2520: z$,e$(2)(1:18)&'-D',e$(1)(1:20),xd(15),xd(7),4,extra$(3)(1:9)&'D',cd$
			end if
		end if
	next j
fnend
! def fn_workabout r: old and no longer used
! 	for j=1 to len(seq$)
! 		on val(seq$(j:j)) goto WorkaboutWATER,WorkaboutELECTRIC,WorkaboutDEMAND,WorkaboutGAS none WorkaboutNextSequence
!
! 		FM_WORKABOUT: form pos 1,c 10,2*c 20,2*n 9,n 1,c 10,c 20
!
! 		WorkaboutWATER: !
! 			if a(1)=0 then goto WorkaboutNextSequence
! 			m$=ltrm$(f$(1))(1:10)
! 			pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&' (W)',e$(1)(1:20),xd(1),xd(3),1,m$,ft$
! 		goto WorkaboutNextSequence
!
! 		WorkaboutELECTRIC: !
! 			if a(3)=0 or trim$(serviceName$(3))<>'Electric' then goto WorkaboutLAWNMETER
! 			m$=ltrm$(f$(2))(1:10)
! 			pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&' (E)',e$(1)(1:20),xd(5),xd(7),3,m$,ft$
! 			WorkaboutLAWNMETER: !
! 			if a(3)=0 or trim$(serviceName$(3))<>'Lawn Meter' then goto WorkaboutNextSequence
! 			m$=ltrm$(f$(2))(1:10)
! 			pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&' (L)',e$(1)(1:20),xd(5),xd(7),3,m$,ft$
! 		goto WorkaboutNextSequence
!
! 		WorkaboutDEMAND: !
! 		goto WorkaboutNextSequence
!
! 		WorkaboutGAS: !
! 			if a(4)=0 or trim$(serviceName$(4))<>'Gas' then goto WorkaboutNextSequence
! 			m$=ltrm$(f$(3))(1:10)
! 			pr #h_out,using FM_WORKABOUT: z$,e$(2)(1:16)&' (G)',e$(1)(1:20),xd(9),xd(11),2,m$,ft$
! 		goto WorkaboutNextSequence
!
! 		WorkaboutNextSequence: !
! 	next j
! fnend /r
def fn_unitech_ht630
	! INPUT FILE (from ACS to Hand Held) needs to contain the following fields:
	!   Account - 10 characters
	!   Route and Sequence - 12 digits (this is the order for accounts to be displayed in - it might contain duplicates and/or skip large ranges of numbers)
	!   Meter Type - 10 characters - 'Gas', 'Water', 'Electric', etc.  Each house may have multiple meters that need to be read.  If a house has both gas and water than it would have two records in the file so that both can be ask.  The Meter Type will need to be displayed so the user will know which they should be entering.
	!   Customer Name - 40 characters - The name of the customer who's meter is being read.  This should be displayed when the reading is ask for.
	!   Meter Address - 40 characters - The address of the customer who's meter is being read.
	!   This should be displayed when the reading is ask for.
	!   Reading High - 10 digits - used to validate entry of new reading
	!   Reading Low - 10 digits - used to validate entry of new reading
	for a_item=1 to udim(mat a)
		if serviceCode$(a_item)='WA' or serviceCode$(a_item)='GA' or serviceCode$(a_item)='EL' then ! or (demand)   it is a metered service
			if a(a_item)>0 then
				usage_current=fn_serviceDataN('current','usage',serviceCode$(a_item))
				reading_current=fn_serviceDataN('current','reading',serviceCode$(a_item))
				unusual_usage_low=round(reading_current+usage_current*fn_pcent,2)
				unusual_usage_high=round(reading_current+usage_current+usage_current*fn_pcent,2)
				pr #h_out,using FORM_UH_OUT: z$,route*100000000+sequence,serviceName$(a_item)(1:10),e$(2),e$(1)(1:20),unusual_usage_low,unusual_usage_high
				FORM_UH_OUT: form pos 1,c 10,n 12,c 10,2*c 40,2*n 10
			end if  ! a(a_item)>0
		end if  ! it is a metered service
	next a_item
fnend  ! fn_Unitech_HT630
! /r
! r: local fn_record_* utilities
def fn_record_init(; setDelimiter$)
	dim rec_line$*2048
	rec_line$=''
	gRecordDelimiter$=setDelimiter$
fnend
def fn_record_addc(fieldLength,fieldText$*256)
	rec_line$&=rpad$(fieldText$(1:fieldLength),fieldLength)&gRecordDelimiter$
fnend
def fn_record_addn(fieldLength,fieldValue; padCharacter$)
	if padCharacter$<>'' then
		rec_line$&=lpad$(str$(fieldValue)(1:fieldLength),fieldLength,padCharacter$)&gRecordDelimiter$
	else
		rec_line$&=lpad$(str$(fieldValue)(1:fieldLength),fieldLength              )&gRecordDelimiter$
	end if
fnend
def fn_record_addx(fieldLength)
	rec_line$=rec_line$&rpt$(' ',fieldLength)&gRecordDelimiter$
fnend
def fn_record_write(h_out; enableTrailingDelimiterOnLine)
	if ~enableTrailingDelimiterOnLine and gRecordDelimiter$<>'' then ! remove trailing delimiter
		rec_line$((len(rec_line$)-len(gRecordDelimiter$)+1):len(rec_line$))=''
	end if
	if deviceSelected$='Itron FC300' then
		write #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
	else if gRecLenRequired then
		rec_line$&=crlf$
		pr #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
	else
		pr #h_out,using 'form pos 1,C '&str$(len(rec_line$)): rec_line$
	end if
fnend
! /r

def fn_searchScreen(x$,&res$)
	fnCustomerSearch(x$)
	if x$<>'' then
		read #hCustomeri1,using 'form pos 1,C 10,x 30,c 30',key=x$: z$,e2$
		res$=rpad$(trim$(z$),10)&' '&trim$(e2$)
	end if
fnend
def fn_transfer
	if deviceSelected$='ACS Meter Reader' then
		fnTos
		mat resp$=('')
		fnLbl(1,1,'Android Drive:',20,1)
		fnComboA('USB-Drive',1,23,mat drive$,'Drive letter of the destination android device.')
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey<>5 then
			dest$=resp$(1)
			fnCopy(out_filename$,trim$(dest$)&'acs_meter_data.txt')
		end if
		goto TRANSFER_XIT
	end if
	if deviceSelected$='LapTop' then
		goto TRANSFER_TO_LAPTOP
	else if deviceSelected$='Psion Workabout' then
		if exists('S:\RCom\RComW.exe')<>0 then ! else  if ...
			execute 'Sy "'&os_filename$('S:\RCom\RComW.exe')&'" /w -n'
		else
			execute 'Sy "'&os_filename$('S:\acsUB\PreRoute.bat')&'" -n' ! 'Psion Workabout'
		end if  ! deviceSelected$='Psion Workabout'
	end if
	goto TRANSFER_XIT
	TRANSFER_TO_LAPTOP: ! r: transfer files for laptop
		fnTos
		mat resp$=('')
		fnLbl(1,1,'Destination Drive:',20,1)
		fnTxt(1,23,20,100,0,'',0,'Destination can be a drive designation including folders')
		if resp$(1)='' then resp$(1)='A:\'
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey<>5 then
			dest$=resp$(1)
			if len(dest$)=0 then goto TRANSFER_TO_LAPTOP
			if len(dest$)=1 then dest$=dest$=':'
			if len(dest$)=3 and dest$(3:3)='/' then dest$(3:3)=''
			fnCopy('[Q]\UBmstr\laptop.out',env$('at')&trim$(dest$)&'\laptop.out')
		end if
	goto TRANSFER_XIT ! /r
	TRANSFER_XIT: !
fnend  ! fn_transfer
def fn_reportCreatedFile(out_filename_report$*512)
	if out_filename_report$<>'' and out_filename_report$<>':CON:' and deviceSelected$<>'LapTop' then ! and deviceSelected$<>'Psion Workabout'
		mat m$(2)
		m$(1)='Hand Held File created:'
		m$(2)=os_filename$(out_filename_report$)
		fnMsgBox(mat m$, response$, '',mb_information+mb_okonly)
	end if
fnend

def fn_rmk1$*20(z$; ___,return$*20)
	! read the footnote from the note file  (any note with * as first character
	dim rm$*1320
	dim notefile$*256
	notefile$='[Q]\UBmstr\notes.h[cno]\'&trim$(z$)&'.txt'
	if exists(notefile$) then
		open #20: 'Name='&notefile$,display,input ioerr Rmk1_Finis
		do
			linput #20: rm$ eof Rmk1_Finis
			if rm$(1:1)='*' then
				return$=rpad$(rm$(2:21),20)
			end if
		loop until rm$(1:1)='*'
	end if
	Rmk1_Finis: !
	close #20: ioerr ignore
	fn_rmk1$=return$
fnend
def fn_pcent
	if ~pcent_setup then
		pcent_setup=1
		open #h_company=fnH: 'Name=[Q]\UBmstr\Company.h[cno]',i,i
		read #h_company,using 'form pos 130,n 4': pcent_return
		close #h_company:
		if pcent_return=0 then pcent_return=100
		pcent_return=pcent_return*.01 ! convert to percent
	end if  ! ~pcent_setup
	fn_pcent=pcent_return
fnend  ! fn_pcent
def fn_unusualUsage(highOrLow$,account$,srvCode$; leaveOpen,___,returnN,uuServiceWhich$*2,reading_current,usage_current)
	! requires local mat serviceCode$
	highOrLow$=lwrc$(trim$(highOrLow$))
	uuServiceWhich$=str$(srch(mat servicecode$,srvCode$))
	reading_current=val(fnCustomerData$(account$,'service '&uuServiceWhich$&'.reading.current', leaveOpen))
	usage_current  =val(fnCustomerData$(account$,'service '&uuServiceWhich$&'.usage.current'  , leaveOpen))

	if highOrLow$='high' then
		returnN=round(reading_current+usage_current+usage_current*fn_pcent,0)
	else if highOrLow$='low' then
		returnN=round(reading_current+usage_current*fn_pcent,0)
		if returnN<0 then returnN=0
	else
	end if
	fn_unusualUsage=returnN
fnend
def fn_serviceDataN(adjetive$,noun$,sc$*2; ___,returnN) ! uses local: mat xd
! adjetive$ = current
!           = prior
! noun$     = usage
!             reading
! sc$ must be WA, GA or EL.
	if adjetive$='current' then ! r:
		if lwrc$(noun$)='usage' then
			if sc$='WA' then
				returnN=xd(3) ! Water usage - current
			else if sc$='GA' then
				returnN=xd(11) ! Gas usage - curent
			else if sc$='EL' then
				returnN=xd(7) ! KWH usage - curent
			end if
		else if lwrc$(noun$)='reading' then
			if sc$='WA' then
				returnN=xd(1)
			else if sc$='GA' then
				returnN=xd(9)
			else if sc$='EL' then
				returnN=xd(5)
			end if
		else
			pr 'serviceDataN: invalid noun' : pause
		end if
		! /r
	else if adjetive$='prior' then ! r:
		if lwrc$(noun$)='usage' then
			pr 'prior usage is not yet programmed.  it would have to be read in from their [last billing date]''s charge transaction.' : pause
		else if lwrc$(noun$)='reading' then
			if sc$='WA' then
				returnN=xd(2) ! Service 1 (Water) – Reading – Prior
			else if sc$='GA' then
				returnN=xd(10) ! Service 4 (Gas) – Reading - Prior
			else if sc$='EL' then
				returnN=xd(6) ! Service 3 (Electric) – Reading – Prior
			end if
			! /r
		end if
	else
		pr 'serviceDataN: adjetive not expected' : pause
	end if
	fn_serviceDataN=returnN
fnend
def fn_CountOfMeteredSvcsActive(; ___,returnN)
	! this function returns the number of metered services the customer has that have a non 0 rate code.
	! if env$('client')='Bethany' then ! the new way
	! 	returnN=0
		if a(1) then returnN+=1 ! service1  WA
		if a(3) then returnN+=1 ! service3  EL
		if a(4) then returnN+=1 ! service4  GA
	! else ! the old way
	! 	returnN=max(1,xd(13))
	! end if
	fn_CountOfMeteredSvcsActive=returnN
fnend
def library fnMeterInfo$*30(mi_field$,z$*10,serviceCode$; closeHandle)
	if ~setup then fn_setup
	fnMeterInfo$=fn_meterInfo$(mi_field$,z$,serviceCode$, closeHandle)
fnend
def fn_meterInfo$*30(mi_field$,z$*10,serviceCode$; closeHandle,___,return$*30)
	if ~mi_setup then
		mi_setup=1
		dim location$(0)*128
		dim locationN(0)
		hLocation=fn_openFio('U4 Meter Location',mat location$,mat locationN, 1,4)
		dim mt_data$(5)*40
		dim mt_dataN(0)
		mi_h_metertype=fn_openFio('U4 Meter Type',mat mt_data$,mat mt_dataN, 1)
	end if
	return$=''
	location$(loc_activeCustomer)=trim$(z$)
	location$(loc_serviceId)=serviceCode$
	locationKey$=fnbuildkey$('U4 Meter Location',mat location$,mat locationN, 4) ! pr locationKey$ : pause
! if trim$(z$)='100025.05' and serviceCode$='EL' then pr 'transmitter_number$=';transmitter_number$ : pause
	if mi_locationKey_prior$<>locationKey$ then
		mat location$=('') : mat location=(0)
		mi_locationKey_prior$=locationKey$
		read #hLocation,using form$(hLocation),key=locationKey$,release: mat location$,mat locationN nokey MI_FINIS
	end if
	mi_field$=lwrc$(trim$(mi_field$))
	if mi_field$='location_id' then
		return$=str$(locationN(loc_locationId))
	else if mi_field$='address' or mi_field$='name' then
		return$=location$(loc_name)
	else if mi_field$='longitude' then
		return$=location$(loc_longitude)
	else if mi_field$='latitude' then
		return$=location$(loc_latitude)
	else if mi_field$='meter number' then
		return$=location$(loc_meterNumber)
	else if mi_field$='transmitter number' then
		return$=location$(loc_transmitter)
	else if mi_field$='meter type' then
		return$=location$(loc_meterType)
	else ! it's probably a MeterType field
		mt_key$=location$(loc_meterType)
		if mt_key_prior$<>mt_key$ then
			mt_key_prior$=mt_key$
			mat mt_data$=('')
			mat mt_dataN=(0)
			read #mi_h_metertype,using form$(mi_h_metertype),key=rpad$(trim$(mt_key$),kln(mi_h_metertype)): mat mt_data$,mat mt_dataN nokey MI_FINIS
		end if
		if mi_field$='key' then
			return$=rtrm$(mt_data$(type_key))
		else if mi_field$='name' then
			return$=rtrm$(mt_data$(type_name))
		else if mi_field$='reading multipler' or mi_field$='reading multiplier' then
			return$=rtrm$(mt_data$(type_readingMultiplier))
		else if mi_field$='number of dials' then
			return$=str$(mt_dataN(type_dialCount))
		else if mi_field$='read type' then
			return$=rtrm$(mt_data$(type_readType))
		else if mi_field$='device' then
			return$=rtrm$(mt_data$(type_deviceType))
		else if mi_field$='size' then
			return$=rtrm$(mt_data$(type_size))
		else if mi_field$='unit' then
			return$=rtrm$(mt_data$(type_unit))
		end if
	end if
	MI_FINIS: !
	if closeHandle then
		close #hLocation: ioerr ignore
		close #mi_h_meter: ioerr ignore
		close #mi_h_metertype: ioerr ignore
		mi_setup=0
	end if
	fn_meterInfo$=return$
fnend
def library fnHandHeldList(mat deviceName$; mat deviceOption$)
	if ~setup then fn_setup
	fnHandHeldList=fn_handHeldList(mat deviceName$)
fnend
def fn_handHeldList(mat deviceName$; mat deviceOption$)
	if ~hhlSetup then
		hhlSetup=1
		dim deviceNameCache$(0)*20
		dim deviceOptionCache$(0)*128
		mat deviceNameCache$(0)
		mat deviceOptionCache$(0)
		dim hd$(0)*256,hdN(0)
		hU4Device=fn_openFio('U4 Device', mat hd$,mat hdN, 1)
		do
			read #hU4Device,using form$(hU4Device): mat hd$,mat hdN eof EoU4Device
			if hdN(device_disable)<>1 then
				fnAddOneC(mat deviceNameCache$  ,rtrm$(hd$(device_name)       ))
				fnAddOneC(mat deviceOptionCache$,rtrm$(hd$(device_optionList) ))
			end if
		loop
		EoU4Device: !
		close #hU4Device:
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Aclara'              ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'Aclara Work Order'   ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'ACS Meter Reader'    ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Badger'              ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Badger Connect C'    ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Badger Beacon'       ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Boson'               ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'CSV by LocationID'   ) : fnAddOneC(mat deviceOptionCache$,'ImportOnly')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Itron FC300'         ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Master Meter'        ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Neptune (Equinox v4)') : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'READy Water'         ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'Sensus'              ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! r: developed but currently unused
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'Psion Workabout') : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'LapTop'         ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'Green Tree'     ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'Hersey'         ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      fnAddOneC(mat deviceNameCache$,'EZReader'       ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'AMR'            ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! fnAddOneC(mat deviceNameCache$,'Unitech HT630'  ) : fnAddOneC(mat deviceOptionCache$,'')
		!  moved to a file      ! /r
	end if
	mat deviceName$(udim(mat deviceNameCache$))
	mat deviceName$=deviceNameCache$
	if fnArrayWasPassedC(mat deviceOption$) then
		mat deviceOption$(udim(mat deviceOptionCache$))
		mat deviceOption$=deviceOptionCache$
	end if
fnend
def fn_customerRead(; accountKey$,locationId) ! all values read are passed back as local variables
	if locationId and ~LastLocationIdOnFileSetup then ! r: get LastLocationIdOnFile
		LastLocationIdOnFileSetup=1
		dim form$(0)*256
		dim location$(0)*128,locationN(0)
		hLocationByLocationID=fn_openFio('U4 Meter Location',mat location$,mat locationN, 1)
		read #hLocationByLocationID,using form$(hLocationByLocationID),last: mat location$,mat locationN
		close #hLocationByLocationID:
		LastLocationIdOnFile=locationN(loc_LocationID)
	end if ! /r
	! #hCustomeri1 and #hCustomeri5 are inherited local variables
	dim extra$(11)*30
	crReturn=0
	! r: clear all the variables that are returned (locally) by this function
		z$=''
		mat e$=('')
		mat a=(0)
		final=0
		mat xd=(0)
		mat f$=('')
		route=0
		sequence=0
		mat extra$=('')
		mat extra=(0)
		alp$=''
	! /r
	F_CUSTOMER: form pos 1,c 10,4*c 30,pos 143,7*pd 2,pos 1821,n 2,pos 217,15*pd 5,pos 131,c 12,pos 361,2*c 12,pos 1741,n 2,n 7,pos 1864,C 30,7*C 12,3*C 30,pos 1741,n 2,pos 354,c 7
	if accountKey$='' and locationId=0 then ! read Sequential
		CrReadSequential: !
		read #hCustomeri5,using F_CUSTOMER: z$,mat e$,mat a,final,mat xd,mat f$,route,sequence,mat extra$,extra(1),alp$ eof CrEoF
		if udim(mat filterAccount$)>0 and trim$(filterAccount$(1))<>'' then
			if srch(mat filterAccount$,trim$(z$))<=0 then
				goto CrReadSequential
			end if
		end if
	else if locationId<>0 then
		z$=lpad$(trim$(fnAccountFromLocationId$(locationId,1)),kln(hCustomeri1))
		if trim$(z$)='' then
			if locationId>LastLocationIdOnFile then
				goto CrEoF
			else
				crReturn=0
				goto CrFinis
			end if
		else
			read #hCustomeri1,using F_CUSTOMER,key=z$: z$,mat e$,mat a,final,mat xd,mat f$,route,sequence,mat extra$,extra(1),alp$ nokey CrNoKey
		end if
	else
		read #hCustomeri1,using F_CUSTOMER,key=accountKey$: z$,mat e$,mat a,final,mat xd,mat f$,route,sequence,mat extra$,extra(1),alp$ nokey CrNoKey
	end if
	crReturn=1
	goto CrFinis
	CrNoKey: ! r:
		crReturn=-4272
	goto CrFinis ! /r
	CrEoF: ! r:
		crReturn=-54
	goto CrFinis ! /r
	CrFinis: !
	fn_customerRead=crReturn
fnend
! r: def fn_getFilterAccount(mat filterAccount$)
! 	mat filterAccount$(0)
! 	fnAddOneC(mat filterAccount$,'100050.05')
! 	fnAddOneC(mat filterAccount$,'100110.00')
! 	...
! 	fnAddOneC(mat filterAccount$,'100114.00')
! /r fnend

def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		on error goto Ertn

		dim resp$(64)*125
		dim f$(3)*12,e2$*30
		dim z$*10,e$(4)*30,xd(15),a(7)
		dim res$*41,m$(2)*80
		dim serviceName$(10)*20,serviceCode$(10)*2

		dim extra(23)
		dim filterAccount$(0)
		! r: set mat drive
			dim drive$(22)*3
			drive$(1)='E:\'
			drive$(2)='F:\'
			drive$(3)='G:\'
			drive$(4)='H:\'
			drive$(5)='I:\'
			drive$(6)='J:\'
			drive$(7)='K:\'
			drive$(8)='L:\'
			drive$(9)='M:\'
			drive$(10)='N:\'
			drive$(11)='O:\'
			drive$(12)='P:\'
			drive$(13)='Q:\'
			drive$(14)='R:\'
			drive$(15)='S:\'
			drive$(16)='T:\'
			drive$(17)='U:\'
			drive$(18)='V:\'
			drive$(19)='W:\'
			drive$(20)='X:\'
			drive$(21)='Y:\'
			drive$(22)='Z:\'
		! /r
		gosub Enum
		fnGetServices(mat serviceName$, mat serviceCode$)
		fnreg_read('Hand Held includeFinalBilled',u4_includeFinalBilled$, 'False')
		dim devicePreference$*20
		devicePreference$=fnhand_held_device$
		dim deviceName$(0)*20,deviceNameCompleteList$(0)*20,deviceNameCompleteListOption$(0)*128
		fn_handHeldList(mat deviceNameCompleteList$,mat deviceNameCompleteListOption$)
		for dnclItem=1 to udim(mat deviceNameCompleteList$)
			if pos(deviceNameCompleteListOption$(dnclItem),'ImportOnly')<=0 then
				fnAddOneC(mat deviceName$,deviceNameCompleteList$(dnclItem))
			end if
		nex dnclItem
		dim deviceSelected$*20
		if lwrc$(devicePreference$)='[ask]' then
			fnureg_read('Hand Held Device Asked',deviceSelected$, deviceName$(1))
		else
			deviceSelected$=devicePreference$
		end if
	end if
	sm_allExceptFinal=1
	sm_aRoute=2
	sm_routeRange=3
	sm_Individuals=4
	sm_LocationId=5
	sm_meterTypes=6

	meterDataSourceOverrideEnabled=1
	dim serviceCodeMetered$(0)*2
	fnGetServiceCodesMetered(mat serviceCodeMetered$)

fnend
include: fn_open
include: enum
include: ertn