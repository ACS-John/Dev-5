00010 fn_setup
00020 fntop(program$)
00030 ! r: restore unconverted files and remove already converted files (for testing only, of course)
00040   if env$('acsDeveloper')<>'' and env$('client')='Campbell' then
00050 !   exec 'copy "C:\ACS\(Client_Files)\Bethany\ACS meter location mess\autosave before first one\UB Company 1 2018-01-02 14-02-30 Menu - before meter location initialize\Meter*.h1" "[Q]\UBmstr\*.h[cno]"'
00060     exec 'free "[Q]\UBmstr\MeterLocation*.h[cno]"' ioerr ignore
00062     fn_populateLocationNonSeq
00070 !   exec 'free "[Q]\UBmstr\MeterAddress*.h[cno]"' ioerr ignore
00072 !   fncreg_write('u4 meter location account numbers left justified','False')
00080   end if
08000 pr hitCount : pause : end ! /r
10300 fnHamsterFio(table$)
10320 XIT: !
10340 fnxit
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12050     library 'S:\Core\Library': fntop,fnxit
12052     library 'S:\Core\Library': fngethandle,fnerror
12054     library 'S:\Core\Library': fnindex_it,fnHamsterFio
12056     library 'S:\Core\Library': fnStatusClose,fnStatus
12060     library 'S:\Core\Library': fnAddOneC
12062     library 'S:\Core\Library': fnmsgbox,fnOpenFile,fnCloseFile
12064     library 'S:\Core\Library': fnAutomatedSavePoint
12066     library 'S:\Core\Library': fnreg_read
12068     library 'S:\Core\Library': fncreg_read,fncreg_write
12070     library 'S:\Core\Library': fnget_services,fnGetServiceCodesMetered
12072     library 'S:\Core\Library': fnBuildKey$
12080     library 'S:\Core\Library': fnKeyExists
12082     library 'S:\Core\Library': fnCustomerData$
12084     library 'S:\Core\Library': fnFree,fnRename
12086     library 'S:\Core\Library': fnlbl,fntos,fnacs,fntxt,fncmdset,fncombof
12088     library 'S:\Core\Library': fncmdkey,fnflexinit1,fnflexadd1
12090     library 'S:\Core\Library': fnapplyDefaultRatesFio
12100     dim info$(0)*20,infoN(0)
12101     dim addr$(0)*30,addrN(0)
12102     dim form$(0)*256
12110     dim location$(0)*256,locationN(0)
12120     dim mg$(0)*128
12430     dim serviceName$(10)*60,serviceCode$(10)*2
12431     dim resp$(128)*128
12432     fnget_services(mat serviceName$, mat serviceCode$)
12433     for snI=1 to udim(mat serviceName$) : serviceName$(snI)=trim$(serviceName$(snI)) : nex snI
12434     table$='U4 Meter Location'
12450     fnGetServiceCodesMetered(mat serviceCodeMetered$)
12480   end if
12490   fnreg_read('Meter Location Id Sequential',u4_meterLocationIdSequential$, 'True')
12500   if exists('[Q]\UBmstr\Meter.h[cno]') or ~exists('[Q]\UBmstr\MeterLocation.h[cno]') then let fn_InitialializeMeterLocation
12900 fnend

20000 def fn_populateLocationNonSeq
20020   dim cus$(0)*256,cusN(0)
20040   hCustomer=fn_open('UB Customer',mat cus$,mat cusN,mat form$)
20060   hLocation4=(hLocation=fn_open(table$,mat location$,mat locationN,mat form$, 0,1)+3) ! activeCustomer/serviceId
20080   do
20100     read #hCustomer,using form$(hCustomer): mat cus$,mat cusN eof PlnsEoCustomer
20120     fnapplyDefaultRatesFio(mat cusN)
20140     if cusN(c_finalBilling)=0 then
20160       for serviceItem=1 to udim(mat serviceCode$)
20180         if srch(mat serviceCodeMetered$,serviceCode$(serviceItem))>0 then
20200           locationN(loc_locationID    )=fn_newLocationIdNonSequential(cus$(c_account))
20220           location$(loc_name          )=cus$(c_meterAddress)
20240           location$(loc_activeCustomer)=cus$(c_account)
20260           location$(loc_serviceId     )=serviceCode$(serviceItem)
20280           location$(loc_longitude     )=''
20300           location$(loc_latitude      )=''
20320           location$(loc_meterNumber   )=cus$(c_s1meterNumber)
20340           location$(loc_transmitter   )=''
20360           location$(loc_meterType     )=''
20362           fnLocationWrite(mat location$,mat locationN, 1)
20370           ! dim locationKey4$*12
20380           ! locationKey4$=fnBuildKey$(table$,mat location$,mat locationN, 4)
20390           ! dim locationKey5$*13
20420           ! if fnKeyExists(hLocation4,locationKey4$) then
20440           !   pr 'location for locationKey5: '&locationKey4$&' what should we do?'
20460           ! else if fnKeyExists(hLocation4+1,locationKey5$:=fnBuildKey$(table$,mat location$,mat locationN, 5)) then
20480           !   pr 'location for locationKey4: '&locationKey5$&' what should we do?'
20500           !   pause
20520           ! else
20540           !   write #hLocation4,using form$(hLocation): mat location$,mat locationN
20560           ! end if
20580         end if
20600       nex serviceItem
20620     end if
20640   loop
20660   PlnsEoCustomer: !
20680   fnCloseFile(hLocation,table$)
20700   fncloseFile(hCustomer,'UB Customer')
20720 fnend

36000 def library fnInitialializeMeterLocation
36010   if ~setup then let fn_setup
36020   ! fnInitialializeMeterLocation=fn_InitialializeMeterLocation    <---  fn_setup handles it if it is necessary.
36030 fnend
38000 def fn_InitialializeMeterLocation
38010   imlCreateNew  = imlImportFromInfo = 0
38020   deleteEnabled = 0
38030   if ~exists('[Q]\UBmstr\MeterLocation.h[cno]') then
38040     imlCreateNew=1
38050   end if
38060   if exists('[Q]\UBmstr\Meter.h[cno]') then
38070     imlImportFromInfo=1
38080   end if
38090   if imlCreateNew or imlImportFromInfo then
38100     fnAutomatedSavePoint('before U4 Initialize Meter Location')
38110     hInfo=fn_open('UB Meter Info',mat info$,mat infoN,mat form$)
38120     hLocation=fn_open(table$,mat location$,mat locationN,mat form$)
38130     fnCloseFile(hLocation,table$)
38140     fnindex_it('[Q]\UBmstr\MeterLocation.h[cno]','[Q]\UBmstr\MeterLocationIdx2.h[cno]', '12 30u')
38150   end if
38160   hLocation=fn_open(table$,mat location$,mat locationN,mat form$, 0,2)
38170   fncreg_read('u4 meter location clean zeros from Location ID',umlCleanZeroLocationId$,'True')
38180   fncreg_read('u4 meter location account numbers left justified',umlCustomerLeftJustified$,'False')
38190   if ~imlCreateNew and umlCustomerLeftJustified$='False' or umlCleanZeroLocationId$='True' then
38200     do
38210       read #hLocation,using form$(hLocation): mat location$,mat locationN eof LjEoLocation
38220       if umlCustomerLeftJustified$='False' then
38230         location$(loc_activeCustomer)=trim$(location$(loc_activeCustomer))
38240       end if
38250       if umlCleanZeroLocationId$='True' then
38260         if locationN(loc_locationId)=0 then
38270           if u4_meterLocationIdSequential$='True' then
38280             locationN(loc_locationId)=fn_newLocationIdSequential
38290           else
38300             locationN(loc_locationId)=fn_newLocationIdNonSequential(account$)
38310           end if
38320         end if
38330       end if
38340       if locationN(loc_locationId)=0 then pr 'AAA - about to write a ZERO location Id' : pause
38350       rewrite #hLocation,using form$(hLocation): mat location$,mat locationN
38360     loop
38370     LjEoLocation: !
38380     fncreg_write('u4 meter location account numbers left justified','True')
38390     restore #hLocation:
38400   end if
38410   if exists('[Q]\UBmstr\MeterAddress.h[cno]') then
38420     ! r: import UB Meter Address (and subordinate UB Meter Info data into U4 Meter Location)
38430       fnStatus('Initializing U4 Meter Location table...')
38440       hAddress=fn_open('UB Meter Address',mat addr$,mat addrN,mat form$, 0,2)
38450       fnStatus('Record Count of UB Meter Address: '&str$(lrec(hAddress)))
38460       dim loacationRecordsAdded(11)
38470       mat loacationRecordsAdded=(0)
38480       fnStatus('Record Count of UB Meter Info: '&str$(lrec(hInfo)))
38490       do
38500         mat location$=('') : mat locationN=(0)
38510         read #hAddress,using form$(hAddress): mat addr$,mat addrN eof EoAddress
38520         locationId=addrN(loc_LocationID)
38530         account$=trim$(fn_accountFromLocIdViaLocation$(locationId, 1))
38540         if account$='' then
38550           fnStatus('No account found for Location ID '&str$(locationId)&' from Address file.')
38560         else
38570           locationN(loc_locationID     )=locationId
38580           location$(loc_name           )=addr$(ma_Name)
38590           location$(loc_activeCustomer )=account$
38600           servicesFound=0
38610           for serviceItem=1 to udim(mat serviceName$)
38620             if serviceCode$(serviceItem)<>'' then
38630               if ~imlImportFromInfo then goto InfoNokey
38640               mat info$=('') : mat infoN=(0)
38650               info$(meter_customer )=trim$(account$)
38660               info$(meter_serviceId)=serviceCode$(serviceItem)
38670               read #hInfo,using form$(hInfo),key=fnBuildKey$('UB Meter Info',mat info$,mat infoN): mat info$,mat infoN nokey InfoNokey
38680               servicesFound+=1
38690               location$(loc_serviceId      )=info$(meter_serviceId      )
38700               location$(loc_longitude      )=info$(meter_longitude      )
38710               location$(loc_latitude       )=info$(meter_latitude       )
38720               location$(loc_meterNumber    )=info$(meter_meterNumber    )
38730               location$(loc_transmitter    )=info$(meter_transmitter    )
38740               location$(loc_meterType      )=info$(meter_meterType      )
38750               fnstatus('importing '&account$&'.'&location$(loc_serviceId)&'.'&str$(locationId)&': ')
38760               fnLocationWrite(mat location$,mat locationN) ! write #hLocation,using form$(hLocation): mat location$,mat locationN
38770               loacationRecordsAdded(serviceItem)+=1
38780               if deleteEnabled then delete #hInfo:
38790               InfoNokey: !
38800             end if
38810           next serviceItem
38820           if servicesFound=0 then
38830             if udim(mat serviceCodeMetered$)=1 then ! only one metered service, it is safe to assume
38840               location$(loc_serviceId      )=serviceCodeMetered$(1)
38850             else
38860               pr 'no locations found for "'&account$&'"- just write a record without any services'
38870               pause
38880             end if
38890             fnLocationWrite(mat location$,mat locationN) ! write #hLocation,using form$(hLocation): mat location$,mat locationN
38900             loacationRecordsAdded(11)+=1
38910           end if
38920           if deleteEnabled then delete #hAddress:
38930         end if
38940       loop
38950       EoAddress: !
38960       for x=1 to 10
38970         if serviceName$(x)<>'' and loacationRecordsAdded(x)>0 then
38980           fnStatus('Imported '&str$(loacationRecordsAdded(x))&' records '&serviceName$(x)&' from Info (with added data from Address)')
38990         end if
39000       nex x
39010       if loacationRecordsAdded(11) then
39020         fnStatus('Imported '&str$(loacationRecordsAdded(11))&' records  with NO service from Info.')
39030       end if
39040     ! /r
39050   end if
39060   if imlImportFromInfo then
39070     ! r: import from Info only - if you're importing both, do address first, because it add's this info too, this one is to add whatever is left after the other one.  it still leaves ones with accounts which do not point to a customer record.
39080       fnStatus('checking Meter Information file for valid data to migrate to Meter Location table')
39090       open #hCustomerOutinUnused:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outin,keyed
39100       open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
39110       restore #hInfo:
39120       do
39130         read #hInfo,using form$(hInfo): mat info$,mat infoN eof EoInfo
39140         dim Customer_MeterAddress$*30
39150         Customer_MeterAddress$=''
39160         account$=lpad$(trim$(info$(meter_customer)),kln(hCustomer))
39170         if trim$(account$)<>'' then
39180           read #hCustomer,using 'form pos 11,c 30',key=account$: Customer_MeterAddress$ nokey NextInfoRecord
39190           mat location$=('') : mat locationN=(0)
39200           locationN(loc_locationID     )=0
39210           location$(loc_name           )=Customer_MeterAddress$
39220           location$(loc_activeCustomer )=info$(meter_customer)
39230           location$(loc_serviceId      )=info$(meter_serviceId      )
39240           location$(loc_longitude      )=info$(meter_longitude      )
39250           location$(loc_latitude       )=info$(meter_latitude       )
39260           location$(loc_meterNumber    )=info$(meter_meterNumber    )
39270           location$(loc_transmitter    )=info$(meter_transmitter    )
39280           location$(loc_meterType      )=info$(meter_meterType      )
39290           ! fnStatus('creating new location from '&account$&' - '&location$(loc_serviceId)&' meter address file')
39300           fnLocationWrite(mat location$,mat locationN)
39310           if deleteEnabled then delete #hInfo:
39320         end if
39330         NextInfoRecord:!
39340       loop
39350       EoInfo: !
39360     ! /r
39370     fn_accountFromLocIdViaLocation$(1, 0) ! close the files it had opened previously
39380     fnCloseFile(hAddress,table$)
39390     fnCloseFile(hInfo,'UB Meter Info')
39400     if exists('[Q]\UBmstr\Meter(old).h[cno]') then
39410       fnFree('[Q]\UBmstr\Meter.h[cno]')
39420     else
39430       fnRename('[Q]\UBmstr\Meter.h[cno]','[Q]\UBmstr\Meter(old).h[cno]')
39440     end if
39450     fnFree('[Q]\UBmstr\Meter_Idx.h[cno]')
39460     ! if env$('acsDeveloper')<>'' then let fnStatusPause
39470     fnStatusClose
39480   end if
39490   fn_InitialializeMeterLocation=hLocation
39500 fnend
40000 def fnLocationWrite(mat location$,mat locationN; leaveFileOpen) ! inherits local dim form$
40020   if ~hLocation(1) then ! r:
40040     hLocation(1)=fn_open(table$,mat location$,mat locationN,mat form$)
40060     for lwIndex=2 to 5
40080       hLocation(lwIndex)=hLocation(lwIndex-1)+1
40100     nex lwIndex
40120   end if ! /r
40140   dim locRead$(0)*256
40160   dim locReadN(0)
40180   mat locRead$(udim(mat location$))
40200   mat locReadN(udim(mat locationN))
40220   mat locRead$=('')
40240   mat locReadN=(0)
40260   dim lwKey$*128
40280   for lwIndex=4 to 5 ! check activeCustomer/serviceId and locationID/serviceId
40300     lwKey$=fnBuildKey$(table$,mat location$,mat locationN, lwIndex)
40320     read #hLocation(lwIndex),using form$(hLocation(lwIndex)),key=lwKey$,release: mat locRead$,mat locReadN nokey LwNoKeyEncountered
40340     if fn_AllStringsMatch(mat locRead$,mat location$, 1) then
40360       pr 'duplicate add detected.  record already exist.'
40380       pr 'delete source to remove this message.' ! pause
40400       goto LwFinis
40420     else if locationN(loc_locationID)=0 and locRead$(loc_serviceId)<>location$(loc_serviceId) then
40440       ! it's a new service for an existing location
40460       if u4_meterLocationIdSequential$='True' then
40480         locationN(loc_locationID)=fn_newLocationIdSequential
40500       else
40520         locationN(loc_locationID)=fn_newLocationIdNonSequential(location$(loc_activeCustomer))
40540       end if
40580       goto LwWrite
40600     else if location$(loc_locationID)=locRead$(loc_locationID) and location$(loc_serviceId)=locRead$(loc_serviceId) then
40620       for lwLocItem=1 to udim(mat locRead$)
40640         if fn_leftIsSuperior(locRead$(lwLocItem),location$(lwLocItem), lwLocItem==loc_activeCustomer) then location$(lwLocItem)=locRead$(lwLocItem)
40660       nex lwLocItem
40680     else if locationN(loc_locationID)=0 then
40700       pr 'new location being added, but it already exists - gather any new info on it'
40720       locationN(loc_locationID)=locReadN(loc_locationID)
40740       for lwLocItem=1 to udim(mat locRead$)
40760         if fn_leftIsSuperior(locRead$(lwLocItem),location$(lwLocItem), lwLocItem==loc_activeCustomer) then location$(lwLocItem)=locRead$(lwLocItem)
40780       nex lwLocItem
40800       goto LwRewrite
40820     else
40840       gosub LwKeyMatchDisplay
40860     end if
40880     LwNoKeyEncountered: !
40900   nex lwIndex
40920   goto LwWrite
40940   LwRewrite: ! r:
40960     if locationN(loc_locationID)=0 then
40980       pr 'attempted to rewrite a record setting its locationID to 0'
41000       pause
41020     else
41040       lwKey$=fnBuildKey$(table$,mat locRead$,mat locReadN, 1)
41060       if locationN(loc_locationId)=0 then pr 'BBB - about to write a ZERO location Id' : pause
41080       rewrite #hLocation(1),using form$(hLocation(1)),key=lwKey$: mat location$,mat locationN
41100     end if
41120   goto LwFinis ! /r
41140   LwWrite: ! r:
41160     if locationN(loc_locationID)=0 then
41180       if u4_meterLocationIdSequential$='True' then
41200         locationN(loc_locationID)=fn_newLocationIdSequential
41220       else
41240         locationN(loc_locationID)=fn_newLocationIdNonSequential(account$)
41260       end if
41280     end if
41300     if locationN(loc_locationId)=0 then pr 'CCC - about to write a ZERO location Id' : pause
41320     write #hLocation(1),using form$(hLocation(1)): mat location$,mat locationN
41340   goto LwFinis ! /r
41360   LwFinis: !
41380   if ~leaveFileOpen then
41400     fnCloseFile(hLocation(1),table$)   !  <--  does this close them all?  too many?
41420     mat hLocation=(0)
41440   end if
41460 fnend
42070 def fn_leftIsSuperior(left$*128,right$*128; isAccountNumber)
42080   lisReturn=0
42090   left$=trim$(left$)
42100   right$=trim$(right$)
42110   if left$=right$ then
42120     lisReturn=0
42130   else if left$<>'' and right$='' then
42140     lisReturn=1
42150   else if left$='' and right$<>'' then
42160     lisReturn=0
42170   else if uprc$(right$)=uprc$(left$) then
42180     if uprc$(right$)=uprc$(right$) then
42190       lisReturn=1
42200     else
42210       lisReturn=0
42220     end if
42230   else if isAccountNumber then
42240     leftFinalBillingCode$=fnCustomerData$(left$,'final billing code')
42250     rightFinalBillingCode$=fnCustomerData$(right$,'final billing code')
42260     if leftFinalBillingCode$<>'' and rightFinalBillingCode$='' then
42270       lisReturn=0
42280     else if leftFinalBillingCode$='' and rightFinalBillingCode$<>'' then
42290       lisReturn=1
42300     else
42310       leftLastBillingDay=val(fnCustomerData$(left$,'last billing day'))
42320       rightLastBillingDay=val(fnCustomerData$(right$,'last billing day'))
42330       if leftLastBillingDay>rightLastBillingDay then
42340         lisReturn=1
42350       else if leftLastBillingDay<rightLastBillingDay then
42360         lisReturn=0
42370       else
42380         pr 'not sure which ACCOUNT NUMBER is superior:  "'&left$&'" or "'&right$&'"'
42390         pr 'final billing codes are '&leftFinalBillingCode$&' and '&rightFinalBillingCode$&'.'
42400         pr 'last billing date on both are '&date$(leftLastBillingDay,'mm/dd/ccyy')&'.'
42410         gosub LwKeyMatchDisplay
42420         lisReturn=0
42422       end if
42430     end if
42440     !
42450   else
42460     pr ' not sure which is superior:  "'&left$&'" or "'&right$&'"'
42470     gosub LwKeyMatchDisplay
42480     lisReturn=0
42490   end if
42500   fn_leftIsSuperior=lisReturn
42510 fnend
42700 LwKeyMatchDisplay: ! r:
42710   fntos(sn$='LwKeyMatchDisplay') : lc=0
42720   fnlbl(lc+=1,1,'key match found on index '&str$(lwIndex))
42730   fnlbl(lc+=1,1,'Data Comparison')
42740   fn_lwCompareLine('Location ID          :',str$(locReadN(loc_locationID     )),str$(locationN(loc_locationID     )))
42750   fn_lwCompareLine('Meter Address        :',locRead$(loc_name           ),location$(loc_name           ))
42760   fn_lwCompareLine('Current Customer     :',locRead$(loc_activeCustomer ),location$(loc_activeCustomer ))
42770   fn_lwCompareLine('Service ID           :',locRead$(loc_serviceId      ),location$(loc_serviceId      ))
42780   fn_lwCompareLine('Longitude            :',locRead$(loc_longitude      ),location$(loc_longitude      ))
42790   fn_lwCompareLine('Latitude             :',locRead$(loc_latitude       ),location$(loc_latitude       ))
42800   fn_lwCompareLine('Meter Number         :',locRead$(loc_meterNumber    ),location$(loc_meterNumber    ))
42810   fn_lwCompareLine('Transmitter Number   :',locRead$(loc_transmitter    ),location$(loc_transmitter    ))
42820   fn_lwCompareLine('Meter Type           :',locRead$(loc_meterType      ),location$(loc_meterType      ))
42830   fnlbl(lc+=1,1,'what now?')
42840   fncmdkey('Keep Left',2)
42850   fncmdkey('Keep Right',4)
42860   hitCount+=1 : ckey=4 ! fnacs(sn$,0,mat resp$,ckey)
42870   if ckey=2 then lisReturn=1 else lisReturn=0
42880 return ! /r
42890 def fn_lwCompareLine(label$*128,valueLeft$*128,valueRight$*128)
42900   if rtrm$(valueLeft$)<>rtrm$(valueRight$) then
42910     fnlbl(lc+=1,1,label$&' (DIFF) "'&rpad$(valueLeft$&'"',30)&' vs "'&rpad$(valueRight$&'"',30)&'   (DIFF)')
42920   else
42930     fnlbl(lc+=1,1,label$&' (same) '&valueLeft$)
42940   end if
42950 fnend
43000 def fn_AllStringsMatch(mat a$,mat b$; caseInsensitive)
43010   asmReturn=asmMatchCount=0
43020   if udim(mat a$)=udim(mat b$) then
43030     for asmItem=1 to udim(mat a$)
43040       if caseInsensitive then
43050         if rtrm$(lwrc$(a$(asmItem)))=rtrm$(lwrc$(b$(asmItem))) then asmMatchCount+=1
43060       else
43070         if rtrm$(a$(asmItem))=rtrm$(b$(asmItem)) then asmMatchCount+=1
43080       end if
43090     nex asmItem
43100   else
43110     pr ' arrays are different size'
43120     pause
43130   end if
43140   if asmMatchCount=udim(mat a$) then
43150     asmReturn=1
43160   end if
43170   fn_AllStringsMatch=asmReturn
43180 fnend
44000 def library fnAccountFromLocationId$*10(locationId; leaveFileOpen)
44010   if ~setup then let fn_setup
44060   fnAccountFromLocationId$=fn_accountFromLocIdViaLocation$(locationId, leaveFileOpen)
44080 fnend
46000 def fn_accountFromLocIdViaLocation$(locationId; leaveFileOpen)
46010   aliReturn$=''
46020   dim location$(0)*128,locationN(0)
46030   if ~hAliLocation then hAliLocation=fn_open(table$,mat location$,mat locationN,mat form$, 1)
46040   mat location$=('')
46050   mat locationN=(0)
46060   locationN(loc_locationId)=locationId
46070   read #hAliLocation,using form$(hAliLocation),key=fnBuildKey$(table$,mat location$,mat locationN): mat location$,mat locationN nokey ignore
46080   aliReturn$=lpad$(trim$(location$(loc_activeCustomer)),10)
46090   if ~leaveFileOpen then
46100     close #hAliLocation:
46110     hAliLocation=0
46120   end if
46130   fn_accountFromLocIdViaLocation$=aliReturn$
46140 fnend
48000 def library fnLocationIdFromAccountAndServ$*30(account$*10,serviceId$*2; field$*14,leaveFileOpen)
48020   if ~setup then let fn_setup
48040   if ~hLfaLocation then hLfaLocation=fn_open(table$,mat location$,mat locationN,mat form$, 1,4)
48050   dim lfaReturn$*30
48060   lfaReturn$=''
48070   if field$='' then field$='LocationId'
48080   field$=lwrc$(field$)
48100   mat location$=('')
48120   mat locationN=(0)
48130   locationKey$=rpad$(trim$(account$),kln(hLfaLocation,1))&rpad$(trim$(serviceId$),kln(hLfaLocation,2))
48140   read #hLfaLocation,using form$(hLfaLocation),key=locationKey$: mat location$,mat locationN nokey ignore
48160   if field$='locationid' then
48180     lfaReturn$=str$(locationN(loc_locationID))
48240   else if field$='name' then
48260     lfaReturn$=location$(loc_name          )
48280   else if field$='activecustomer' then
48300     lfaReturn$=location$(loc_activeCustomer)
48320   else if field$='serviceid' then
48340     lfaReturn$=location$(loc_serviceId     )
48360   else if field$='longitude' then
48380     lfaReturn$=location$(loc_longitude     )
48400   else if field$='latitude' then
48420     lfaReturn$=location$(loc_latitude      )
48440   else if field$='meternumber' then
48460     lfaReturn$=location$(loc_meterNumber   )
48480   else if field$='transmitter' then
48500     lfaReturn$=location$(loc_transmitter   )
48520   else if field$='metertype' then
48540     lfaReturn$=location$(loc_meterType     )
48560   else
48580     pr 'meter location field ('&field$&') not recognized.'
48600     pause
48620   end if
48640   if ~leaveFileOpen then
48660     close #hLfaLocation:
48680     hLfaLocation=0
48700   end if
48720   fnLocationIdFromAccountAndServ$=lfaReturn$
48740 fnend
53000 def library fnMeterAddressLocationID(meterAddress$*30; leaveFileOpen) ! returns the locationID for a provided meterAddress$
53020   if ~setup then let fn_setup
53040   if leaveFileOpen and hMaLocationByName<>0 then goto maliPastOpen
53042   dim location$(0)*128,locationN(0),locationKey$*128
53060   hMaLocationByName=fn_open(table$,mat location$,mat locationN,mat form$, 1,2)
53080   maliPastOpen: !
53100   locationN(loc_LocationID)=-1
53120   read #hMaLocationByName,using form$(hMaLocationByName),key=rpad$(meterAddress$,KLN(hMaLocationByName)),release: mat location$,mat locationN nokey ignore
53140   if ~leaveFileOpen then
53160     close #hMaLocationByName:
53180     hMaLocationByName=0
53200   end if
53220   fnMeterAddressLocationID=locationN(loc_LocationID)
53240 fnend
55000 def library fnMeterAddressName$*30(locationId; leaveFileOpen) ! returns the meterAddress$ for a provided LocationID
55020   if ~setup then let fn_setup
55040   if leaveFileOpen and hMaLocationByLocationId<>0 then goto manPastOpen
55060   hMaLocationByLocationId=fn_open(table$,mat location$,mat locationN,mat form$, 1)
55080   manPastOpen: !
55100   locationN(loc_LocationID)=-1
55120   read #hMaLocationByLocationId,using form$(hMaLocationByLocationId),key=cnvrt$('N 11',locationId),release: mat location$,mat locationN nokey ignore
55140   if ~leaveFileOpen then
55160     close #hMaLocationByLocationId:
55180     hMaLocationByLocationId=0
55200   end if
55220   fnMeterAddressName$=location$(loc_name)
55240 fnend
60000 ! def library fnMeterAddressUpdate(meterAddressBefore$*30,&meterAddressAfter$) r: unused fn
60001 !   pr 'fnMeterAddressUpdate is broken - must be fixed to update location table instead'
60002 !   pause
60020 !   if ~setup then let fn_setup
60040 !   meterAddressBefore$=rtrm$(meterAddressBefore$)
60060 !   meterAddressAfter$=rtrm$(meterAddressAfter$)
60080 !   if meterAddressBefore$<>meterAddressAfter$ then
60100 !     hMeterAddressLocationID=fn_open(table$,mat location$,mat locationN,mat form$)
60120 !     hMeterAddressName=hMeterAddressLocationID+1
60140 !     if fnKeyExists(hMeterAddressName,meterAddressAfter$) then
60160 !       doAdd$=fn_askAddDuplicate$(meterAddressBefore$,meterAddressAfter$)
60170 !       if doAdd$='No' then meterAddressAfter$=meterAddressBefore$ : doAdd$='Cancel'
60200 !     else if meterAddressBefore$='' and ~fnKeyExists(hMeterAddressName,meterAddressAfter$) then ! changed from blank - it is new
60220 !       doAdd$='Yes'
60300 !     else if lwrc$(meterAddressBefore$)=lwrc$(meterAddressAfter$) then  ! only case changes - it is an update
60320 !       doAdd$='No'
60340 !     else
60360 !       doAdd$=fn_askAddNew$(meterAddressBefore$,meterAddressAfter$)
60380 !     end if
60400 ! ! pr 'doAdd$=';doAdd$ : pause
60460 !     if doAdd$='Yes' then
60480 !       maDataN(loc_LocationID)=0 ! fn_newLocationIdSequential
60500 !       maData$(ma_Name)=meterAddressAfter$
60520 !       write #hMeterAddressName,using form$(hMeterAddressLocationID): mat maData$,mat maDataN
60540 !       ! pr 'just wrote one' : pause
60560 !     else if doAdd$='No' then
60580 !       read #hMeterAddressName,using form$(hMeterAddressLocationID),key=rpad$(meterAddressBefore$,kln(hMeterAddressName)): mat maData$,mat maDataN
60600 !       maData$(ma_Name)=meterAddressAfter$
60620 !       rewrite #hMeterAddressName,using form$(hMeterAddressLocationID),key=rpad$(meterAddressBefore$,kln(hMeterAddressName)): mat maData$,mat maDataN
60640 !       ! pr 'rewrote ' : pause
60660 !     else if doAdd$='Cancel' then
60680 !       meterAddressAfter$=meterAddressBefore$
60700 !     end if
60720 !     fnCloseFile(hMeterAddressLocationID,table$)
60740 !   end if
60760 ! fnend /r
61000 def fn_newLocationIdNonSequential(account$)
61020   ! if env$('client')='Campbell' then
61040   newLocationIdNonSequential=val(fnCustomerData$(account$,'route'))*1000000+val(fnCustomerData$(account$,'sequence'))
61060   ! end if
61080   fn_newLocationIdNonSequential=newLocationIdNonSequential
61100 fnend

62000 def fn_newLocationIdSequential(; alterAmount)
62020   if alterAmount=0 then let alterAmount=1
62100   fncreg_read('Last Location ID Assigned',nliLastLocation$)
62120   nliLastLocation=val(nliLastLocation$)
62140   nliLastLocation+=alterAmount
62160   fncreg_write('Last Location ID Assigned',str$(nliLastLocation))
62200   fn_newLocationIdSequential=nliLastLocation ! pr 'fn_newLocationIdSequential is returning ';nliLastLocation
62220 fnend
64000 ! def fn_askAddNew$(meterAddressBefore$*30,meterAddressAfter$*80) r: unused fns
64040 !   mat mg$(0)
64060 !   fnAddOneC(mat mg$,'The Meter Address was changed from')
64080 !   fnAddOneC(mat mg$,'From: "'&meterAddressBefore$&'"')
64100 !   fnAddOneC(mat mg$,'  To: "'&meterAddressAfter$&'"')
64120 !   fnAddOneC(mat mg$,'')
64140 !   fnAddOneC(mat mg$,'Is this a new entry?')
64160 !   fnAddOneC(mat mg$,'')
64180 !   fnAddOneC(mat mg$,'  Yes    - Add an entry to Meter Address file')
64200 !   fnAddOneC(mat mg$,'  No     - Update previous entry in Meter Address file')
64220 !   fnAddOneC(mat mg$,'  Cancel - Revert Changes')
64240 !   fnmsgbox(mat mg$, aaResponse$, '', 3) ! mtype 3 is yes/no/cancel
64260 !   fn_askAddNew$=aaResponse$
64280 ! fnend
65000 ! def fn_askAddDuplicate$(meterAddressBefore$*30,meterAddressAfter$*80)
65040 !   mat mg$(0)
65060 !   fnAddOneC(mat mg$,'The new Meter Address entered already exist.')
65120 !   fnAddOneC(mat mg$,'')
65140 !   fnAddOneC(mat mg$,'Do you want to continue?')
65160 !   fnAddOneC(mat mg$,'')
65180 !   fnAddOneC(mat mg$,'  Yes    - use "'&meterAddressAfter$&'" as entered.')
65200 !   fnAddOneC(mat mg$,'  No     - revert to "'&meterAddressBefore$&'"')
65240 !   fnmsgbox(mat mg$, aaResponse$, '', 4) ! mtype 4 is yes/no
65260 !   fn_askAddDuplicate$=aaResponse$
65280 ! fnend  /r

66000 def library fnCustomerMeterLocationSelect(account$*10,serviceCode$*2) ! cmls
66020   if ~setup then let fn_setup
66040   hCmlsLocation(1)=fn_open(table$,mat location$,mat locationN,mat form$)
66060   for j=2 to 5 : hCmlsLocation(j)=hCmlsLocation(1)+j-1 : nex j
66070   CmlsSelect: !
66080   fntos(sn$='cmls'&account$) : respc=0
66100   fnlbl(1,1,'Account: ',20,1)
66120   fntxt(1,22,10, 0,0,'',1)
66140   resp$(respc+=1)=account$
66160   fnlbl(3,1,'Select '&serviceCode$&' Meter Location for Account')
66180   dim cmlsFlexItem$(8)*128
66200   cmlsFlexItem$(1)='Location ID       '
66220   cmlsFlexItem$(2)='Meter Address     '
66240   cmlsFlexItem$(3)='Current Customer  '
66260   cmlsFlexItem$(4)='Longitude         '
66280   cmlsFlexItem$(5)='Latitude          '
66300   cmlsFlexItem$(6)='Meter Number      '
66320   cmlsFlexItem$(7)='Transmitter Number'
66340   cmlsFlexItem$(8)='Meter Type        '
66360   fnflexinit1('locationSelect',4,1,20,20,mat ch$) : cmlsFlexCount=0
66380   do
66400     read #hCmlsLocation(5),using form$(hCmlsLocation(1)): mat location$,mat locationN eof CmlsEoLocation
66420     if location$(loc_serviceId)=serviceCode$ then
66440       cmlsFlexItem$(1)=str$(locationN(loc_LocationID))
66460       cmlsFlexItem$(2)=location$(loc_name           )
66480       cmlsFlexItem$(3)=location$(loc_activeCustomer )
66500       cmlsFlexItem$(4)=location$(loc_longitude      )
66520       cmlsFlexItem$(5)=location$(loc_latitude       )
66540       cmlsFlexItem$(6)=location$(loc_meterNumber    )
66560       cmlsFlexItem$(7)=location$(loc_transmitter    )
66580       cmlsFlexItem$(8)=location$(loc_meterType      )
66600       fnflexadd1(mat cmlsFlexItem$) : cmlsFlexCount+=1
66620     end if
66640   loop
66660   CmlsEoLocation: !
66666   if cmlsFlexCount=0 then
66667     ckey=2
66668     cmlsAddForceServiceId$=serviceCode$
66669     gosub CmlsAdd
66670     if ckey=5 then goto CmslFinis
66671     goto CmlsSelect
66672   end if
66680   fncmdkey('Select',1,1,0)
66700   fncmdkey('New',2,0,0)
66720   fncmdkey('Cancel',5,0,1)
66740   fnAcs(sn$,0,mat resp$,ckey)
66760   if ckey<>5 then
66780     cmlsSelectedLocationId=val(resp$(2))
66800     if ckey=1 then
66820       mat location$=('')
66840       mat locationN=(0)
66860       locationN(loc_locationID)=cmlsSelectedLocationId
66880       location$(loc_serviceId)=serviceCode$
66900       dim cmlsLocationKey$*128
66920       cmlsLocationKey$=fnBuildKey$(table$,mat location$,mat locationN, 5)
66940       read #hCmlsLocation(5),using form$(hCmlsLocation(1)),key=cmlsLocationKey$,release: mat location$,mat locationN
66960       if trim$(location$(loc_activeCustomer))='' and trim$(location$(loc_activeCustomer))<>trim$(account$) then
66980         location$(loc_activeCustomer)=trim$(account$)
67000         rewrite #hCmlsLocation(5),using form$(hCmlsLocation(1)),key=cmlsLocationKey$: mat location$,mat locationN
67020       else if trim$(location$(loc_activeCustomer))<>trim$(account$) then
67040         mat mg$(0)
67060         fnAddOneC(mat mg$,'Location ID '&str$(cmlsSelectedLocationId)&' currently belongs to customer '&trim$(location$(loc_activeCustomer)))
67080         fnAddOneC(mat mg$,'Are you sure you want to change the active customer to '&trim$(account$)&'?')
67100         fnmsgbox(mat mg$,resp$,'',32+4)
67120         if resp$='Yes' then
67140           fn_purgeSrvAccountFromLocation(serviceCode$,account$)
67160           location$(loc_activeCustomer)=trim$(account$)
67180           rewrite #hCmlsLocation(5),using form$(hCmlsLocation(1)),key=cmlsLocationKey$: mat location$,mat locationN
67200         else
67220           release #hCmlsLocation(5):
67240         end if
67260       end if
67280     else if ckey=2 then
67300       gosub CmlsAdd
67320     end if
67340   end if
67350   CmslFinis: !
67360   fnclosefile(hCmlsLocation(1),table$)
67380 fnend
68000 CmlsAdd: ! r: returns ckey, optionally accepts cmlsAddForceServiceId$, requires a whole lot of local stuff
68010   fntos(sn$='LocationAdd') : lc=respc=0
68020   if u4_meterLocationIdSequential$='True' then
68030     fnlbl(lc+=1,1,'Location ID     ', 20,1) : fntxt(lc,22,11, 0,0,'',1,'') : resp$(respc+=1)=str$(fn_newLocationIdSequential)
68040   else
68050     fnlbl(lc+=1,1,'Location ID     ', 20,1) : fntxt(lc,22,11, 0,0,'',0,'') : resp$(respc+=1)=str$(fn_newLocationIdNonSequential(account$))
68060   end if
68070   fnlbl(lc+=1,1,'Meter Address     ', 20,1) : fntxt(lc,22,30, 0,0,'',0,'') : resp$(respc+=1)=''
68080   fnlbl(lc+=1,1,'Current Customer  ', 20,1) : fntxt(lc,22,10, 0,0,'',1,'') : resp$(respc+=1)=trim$(account$)
68100   fnlbl(lc+=1,1,'Service ID        ', 20,1) : fntxt(lc,22, 2, 0,0,'',1,'') : resp$(respc+=1)=serviceCode$
68120   fnlbl(lc+=1,1,'Longitude         ', 20,1) : fntxt(lc,22,17, 0,0,'',0,'') : resp$(respc+=1)=''
68140   fnlbl(lc+=1,1,'Latitude          ', 20,1) : fntxt(lc,22,17, 0,0,'',0,'') : resp$(respc+=1)=''
68160   fnlbl(lc+=1,1,'Meter Number      ', 20,1) : fntxt(lc,22,12, 0,0,'',0,'') : resp$(respc+=1)=''
68180   fnlbl(lc+=1,1,'Transmitter Number', 20,1) : fntxt(lc,22,20, 0,0,'',0,'') : resp$(respc+=1)=''
68200   fnlbl(lc+=1,1,'Meter Type        ', 20,1)
68220   fncombof('',lc,22,46,'[Q]\UBmstr\MeterType.h[cno]',1,5,6,40,'[Q]\UBmstr\MeterTypeIdx.h[cno]',1)
68240   resp$(respc+=1)=''
68260   fncmdset(4)
68280   fnacs(sn$,0,mat resp$,ckey)
68300   if ckey=5 then
68320     if u4_meterLocationIdSequential$='True' then
68322       fn_newLocationIdSequential(-1)
68324     end if
68340   else
68360     fn_purgeSrvAccountFromLocation(serviceCode$,account$)
68380     mat locationN=(0)
68400     mat location$=('')
68420     respc=0
68440     locationN(loc_locationID    )=val(resp$(respc+=1))
68460     location$(loc_name          )=resp$(respc+=1)
68480     location$(loc_activeCustomer)=resp$(respc+=1)
68500     location$(loc_serviceId     )=resp$(respc+=1) : if cmlsAddForceServiceId$<>'' then resp$(respc)=cmlsAddForceServiceId$
68520     location$(loc_longitude     )=resp$(respc+=1)
68540     location$(loc_latitude      )=resp$(respc+=1)
68560     location$(loc_meterNumber   )=resp$(respc+=1)
68580     location$(loc_transmitter   )=resp$(respc+=1)
68600     location$(loc_meterType     )=resp$(respc+=1)(1:5)
68620     fnLocationWrite(mat location$,mat locationN)
68640   end if
68660 return ! /r
70000 def fn_purgeSrvAccountFromLocation(serviceCode$*2,account$*10)
70020   ! remove account$ from all previously assigned Meter Location records
70040   dim tmpLoc$(0)*128,tmpLocN(0),tmpLocKey$*128
70060   mat tmpLoc$(udim(mat location$))
70080   mat tmpLocN(udim(mat locationN))
70100   mat tmpLoc$=('')
70120   mat tmpLocN=(0)
70140   tmpLoc$(loc_activeCustomer)=trim$(account$)
70160   tmpLoc$(loc_serviceId)=serviceCode$
70180   tmpLocKey$=fnBuildKey$(table$,mat tmpLoc$,mat tmpLocN, 4)
70200   restore #hCmlsLocation(4),key=tmpLocKey$: nokey CmlsDelFinis
70220   do
70240     read #hCmlsLocation(4),using form$(hCmlsLocation(1)): mat tmpLoc$,mat tmpLocN eof CmlsDelFinis ! locked CmlsDelLocked
70260     if trim$(tmpLoc$(loc_activeCustomer))=trim$(account$) then
70280       tmpLoc$(loc_activeCustomer)=''
70300       rewrite #hCmlsLocation(4),using form$(hCmlsLocation(1)): mat tmpLoc$,mat tmpLocN
70320       tmpMatch=1
70340     else
70360       release #hCmlsLocation(4):
70380       tmpMatch=0
70400     end if
70420   loop while tmpMatch
70440   CmlsDelFinis: !
70460 fnend
86000 ! <updateable region: fn_open (supressprompt:=2)>
86020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
86040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
86060   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
86080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then
86100     mat loadedsubs$(udim(loadedsubs$)+1)
86120     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
86140     for index=1 to udim(mat _fileiosubs$)
86160       execute (_fileiosubs$(index))
86180     next index
86200   end if
86220 fnend
86240 ! </updateable region: fnopen>
88000 ! <updateable region: ertn>
88020 ERTN: fnerror(program$,err,line,act$,"xit")
88040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
88060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
88080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
88100 ERTN_EXEC_ACT: execute act$ : goto ERTN
88120 ! </updateable region: ertn>

