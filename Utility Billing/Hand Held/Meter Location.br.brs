00035 ! exec 'copy "C:\Users\John\OneDrive\ACS\DEV-5D~1\Purdy\UBmstr\Meter*.h2" "C:\Users\John\OneDrive\ACS\DEV-5D~1\Purdy\UBmstr\*.h1"'
00036 ! exec 'free "C:\Users\John\OneDrive\ACS\DEV-5D~1\Purdy\UBmstr\MeterLocation*.h1"' ioerr ignore
10000 fn_setup
10020 fntop(program$)
10040 fnreg_read('Meter Address Enable',u4_meterAddress$,'False')
10060 if u4_meterAddress$='False' then
10100   fnAddOneC(mat mg$,'Meter Location tracking is currently disabled.')
10120   fnAddOneC(mat mg$,'Would you like to enable it now?')
10160   fnmsgbox(mat mg$,mgResp$, '', 4)
10180   if mgResp$='Yes' then
10200   u4_meterAddress$='True'
10220   fnreg_write('Meter Address Enable',u4_meterAddress$)
10240   else
10260     goto XIT
10280   end if
10290 end if
10300 fnHamsterFio(table$)
10320 XIT: !
10340 fnxit
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12050     library 'S:\Core\Library': fntop,fnxit,fngethandle,fnerror,fnindex_it,fnStatusClose,fnStatus,fnHamsterFio
12060     library 'S:\Core\Library': fnAddOneC,fnAddOneN,fnCountMatchesN,fnArrayMax
12070     library 'S:\Core\Library': fnmsgbox,fnOpenFile,fnCloseFile
12072     library 'S:\Core\Library': fncreg_read,fncreg_write,fnreg_read,fnreg_write
12074     library 'S:\Core\Library': fnAccountFromLocationId$
12076     library 'S:\Core\Library': fnget_services
12078     library 'S:\Core\Library': fnBuildKey$,fnKeyExists
12080     library 'S:\Core\Library': fnFree
12082     library 'S:\Core\Library': fnStatusPause
12084     dim info$(0)*20,infoN(0)
12086     dim addr$(0)*30,addrN(0)
12102     dim form$(0)*256
12110     dim location$(0)*256,locationN(0)
12120     dim mg$(0)*128
12430     dim serviceName$(10)*60,serviceCode$(10)*2
12432     fnget_services(mat serviceName$, mat serviceCode$)
12434     table$='U4 Meter Location'
12480   end if
12500   if ~exists(env$('Q')&'\UBmstr\MeterLocation.h'&env$('cno')) then let fn_InitialializeMeterLocation
12900 fnend
13000 def library fnInitialializeMeterLocation
13020   if ~setup then let fn_setup
13040   fnInitialializeMeterLocation=fn_InitialializeMeterLocation
13060 fnend
14000 def fn_InitialializeMeterLocation
14020   imlCreateNew=0
14030   deleteEnabled=1
14040   if ~exists(env$('Q')&'\UBmstr\MeterLocation.h'&env$('cno')) then
14060     imlCreateNew=1
14080   end if
14100   hMeterLocation=fn_open(table$,mat location$,mat locationN,mat form$)
14120   if imlCreateNew then
14140     hAddress=fn_open('UB Meter Address',mat addr$,mat addrN,mat form$, 0,2)
14160     fnStatus('Initializing U4 Meter Location table...')
14180     fnCloseFile(hLocation,table$) 
14200     fnindex_it(env$('Q')&'\UBmstr\MeterLocation.h'&env$('cno'),env$('Q')&'\UBmstr\MeterLocationIdx2.h'&env$('cno'), '12 30u')
14220     hLocation=fn_open(table$,mat location$,mat locationN,mat form$, 0,2)
14230     hInfo=fn_open('UB Meter Info',mat info$,mat infoN,mat form$)
14240     do
14260       mat location$=('') : mat locationN=(0)
14300       read #hAddress,using form$(hAddress): mat addr$,mat addrN eof EoAddress
14380       locationId=addrN(loc_LocationID)
14400       account$=trim$(fnAccountFromLocationId$(locationId, 1))
14420       locationN(loc_locationID     )=locationId
14440       location$(loc_name           )=addr$(ma_Name)
14460       location$(loc_activeCustomer )=account$
14480       servicesFound=0
14500       for serviceItem=1 to udim(mat serviceName$)
14520         if serviceCode$(serviceItem)<>'' then
14540           mat info$=('') : mat infoN=(0)
14560           info$(meter_customer)=trim$(account$)
14580           info$(meter_serviceId)=serviceCode$(serviceItem)
14600           read #hInfo,using form$(hInfo),key=fnBuildKey$('UB Meter Info',mat info$,mat infoN): mat info$,mat infoN nokey InfoNokey
14620           servicesFound+=1
14640           location$(loc_serviceId      )=info$(meter_serviceId      )
14660           location$(loc_longitude      )=info$(meter_longitude      )
14680           location$(loc_latitude       )=info$(meter_latitude       )
14700           location$(loc_meterNumber    )=info$(meter_meterNumber    )
14720           location$(loc_transmitter    )=info$(meter_transmitter    )
14740           location$(loc_meterType      )=info$(meter_meterType      )
14750           fnstatus('importing '&account$&'.'&location$(loc_serviceId)&'.'&str$(locationId)&': ')
14760           write #hLocation,using form$(hLocation): mat location$,mat locationN
14770           if deleteEnabled then delete #hInfo:
14780           InfoNokey: !
14800         end if
14820       next serviceItem
14840       if servicesFound=0 then
14860         pr 'no locations found for "'&account$&'"- just write a record without any services'
14880         ! pause
14900         write #hLocation,using form$(hLocation): mat location$,mat locationN
14920       end if
14930       if deleteEnabled then delete #hAddress: 
14940     loop
14960     EoAddress: !
14980     fnAccountFromLocationId$(1, 0) ! close the files it had opened previously
15000     ! fnStatusPause
15020     fnStatusClose
15040     fnCloseFile(hAddress,table$)
15060     fnCloseFile(hInfo,'UB Meter Info')
15080   end if
15100   fn_InitialializeMeterLocation=hLocation
15120 fnend
24000 def library fnAccountFromLocationId$*10(locationId; leaveFileOpen)
24010   if ~setup then let fn_setup
24060   fnAccountFromLocationId$=fn_accountFromLocIdViaLocation$(locationId, leaveFileOpen)
24080 fnend
26000 def fn_accountFromLocIdViaLocation$(locationId; leaveFileOpen)
26010   aliReturn$=''
26020   dim location$(0)*128,locationN(0)
26030   if ~hAliLocation then hAliLocation=fn_open(table$,mat location$,mat locationN,mat form$, 1)
26040   mat location$=('')
26050   mat locationN=(0)
26060   locationN(loc_locationId)=locationId
26070   read #hAliLocation,using form$(hAliLocation),key=fnBuildKey$(table$,mat location$,mat locationN): mat location$,mat locationN nokey ignore
26080   aliReturn$=location$(loc_activeCustomer)
26090   if ~leaveFileOpen then
26100     close #hAliLocation:
26110     hAliLocation=0
26120   end if
26130   fn_accountFromLocIdViaLocation$=aliReturn$
26140 fnend

33000 def library fnMeterAddressLocationID(meterAddress$*30; leaveFileOpen) ! returns the locationID for a provided meterAddress$
33020   if ~setup then let fn_setup
33040   if leaveFileOpen and hMaLocationByName<>0 then goto maliPastOpen
33042   dim location$(0)*128,locationN(0)
33060   hMaLocationByName=fn_open(table$,mat location$,mat locationN,mat form$, 1,2)
33080   maliPastOpen: !
33100   locationN(loc_LocationID)=-1
33120   read #hMaLocationByName,using form$(hMaLocationByName),key=rpad$(meterAddress$,KLN(hMaLocationByName)),release: mat location$,mat locationN nokey ignore
33140   if ~leaveFileOpen then
33160     close #hMaLocationByName:
33180     hMaLocationByName=0
33200   end if
33220   fnMeterAddressLocationID=locationN(loc_LocationID)
33240 fnend
35000 def library fnMeterAddressName$*30(locationId; leaveFileOpen) ! returns the meterAddress$ for a provided LocationID
35020   if ~setup then let fn_setup
35040   if leaveFileOpen and hMaLocationByLocationId<>0 then goto manPastOpen
35060   hMaLocationByLocationId=fn_open(table$,mat location$,mat locationN,mat form$, 1)
35080   manPastOpen: !
35100   locationN(loc_LocationID)=-1
35120   read #hMaLocationByLocationId,using form$(hMaLocationByLocationId),key=cnvrt$('N 11',locationId),release: mat location$,mat locationN nokey ignore
35140   if ~leaveFileOpen then
35160     close #hMaLocationByLocationId:
35180     hMaLocationByLocationId=0
35200   end if
35220   fnMeterAddressName$=location$(loc_name)
35240 fnend
40000 def library fnMeterAddressUpdate(meterAddressBefore$*30,&meterAddressAfter$)
40001   pr 'fnMeterAddressUpdate is broken - must be fixed to update location table instead'
40002   pause
40020   if ~setup then let fn_setup
40040   meterAddressBefore$=rtrm$(meterAddressBefore$)
40060   meterAddressAfter$=rtrm$(meterAddressAfter$)
40080   if meterAddressBefore$<>meterAddressAfter$ then
40100     hMeterAddressLocationID=fn_open(table$,mat location$,mat locationN,mat form$)
40120     hMeterAddressName=hMeterAddressLocationID+1
40140     if fnKeyExists(hMeterAddressName,meterAddressAfter$) then
40160       doAdd$=fn_askAddDuplicate$(meterAddressBefore$,meterAddressAfter$)
40170       if doAdd$='No' then meterAddressAfter$=meterAddressBefore$ : doAdd$='Cancel'
40200     else if meterAddressBefore$='' and ~fnKeyExists(hMeterAddressName,meterAddressAfter$) then ! changed from blank - it is new
40220       doAdd$='Yes'
40300     else if lwrc$(meterAddressBefore$)=lwrc$(meterAddressAfter$) then  ! only case changes - it is an update
40320       doAdd$='No'
40340     else
40360       doAdd$=fn_askAddNew$(meterAddressBefore$,meterAddressAfter$)
40380     end if
40400 ! pr 'doAdd$=';doAdd$ : pause
40460     if doAdd$='Yes' then
40480       maDataN(loc_LocationID)=fn_newLocationID
40500       maData$(ma_Name)=meterAddressAfter$
40520       write #hMeterAddressName,using form$(hMeterAddressLocationID): mat maData$,mat maDataN
40540       ! pr 'just wrote one' : pause
40560     else if doAdd$='No' then 
40580       read #hMeterAddressName,using form$(hMeterAddressLocationID),key=rpad$(meterAddressBefore$,kln(hMeterAddressName)): mat maData$,mat maDataN
40600       maData$(ma_Name)=meterAddressAfter$
40620       rewrite #hMeterAddressName,using form$(hMeterAddressLocationID),key=rpad$(meterAddressBefore$,kln(hMeterAddressName)): mat maData$,mat maDataN
40640       ! pr 'rewrote ' : pause
40660     else if doAdd$='Cancel' then 
40680       meterAddressAfter$=meterAddressBefore$
40700     end if
40720     fnCloseFile(hMeterAddressLocationID,table$)
40740   end if
40760 fnend
42000 def fn_newLocationID(; initialize)
42020   if initialize then
42040     nliLastLocation=0
42060     fncreg_write('Last Location ID Assigned',str$(nliLastLocation))
42080   else
42100     fncreg_read('Last Location ID Assigned',nliLastLocation$)
42120     nliLastLocation=val(nliLastLocation$)
42140     nliLastLocation+=1
42160     fncreg_write('Last Location ID Assigned',str$(nliLastLocation))
42180   end if
42200   fn_newLocationID=nliLastLocation
42220 fnend
44000 def fn_askAddNew$(meterAddressBefore$*30,meterAddressAfter$*80)
44040   mat mg$(0)
44060   fnAddOneC(mat mg$,'The Meter Address was changed from')
44080   fnAddOneC(mat mg$,'From: "'&meterAddressBefore$&'"')
44100   fnAddOneC(mat mg$,'  To: "'&meterAddressAfter$&'"')
44120   fnAddOneC(mat mg$,'')
44140   fnAddOneC(mat mg$,'Is this a new entry?')
44160   fnAddOneC(mat mg$,'')
44180   fnAddOneC(mat mg$,'  Yes    - Add an entry to Meter Address file')
44200   fnAddOneC(mat mg$,'  No     - Update previous entry in Meter Address file')
44220   fnAddOneC(mat mg$,'  Cancel - Revert Changes')
44240   fnmsgbox(mat mg$, aaResponse$, '', 3) ! mtype 3 is yes/no/cancel
44260   fn_askAddNew$=aaResponse$
44280 fnend
45000 def fn_askAddDuplicate$(meterAddressBefore$*30,meterAddressAfter$*80)
45040   mat mg$(0)
45060   fnAddOneC(mat mg$,'The new Meter Address entered already exist.')
45120   fnAddOneC(mat mg$,'')
45140   fnAddOneC(mat mg$,'Do you want to continue?')
45160   fnAddOneC(mat mg$,'')
45180   fnAddOneC(mat mg$,'  Yes    - use "'&meterAddressAfter$&'" as entered.')
45200   fnAddOneC(mat mg$,'  No     - revert to "'&meterAddressBefore$&'"')
45240   fnmsgbox(mat mg$, aaResponse$, '', 4) ! mtype 4 is yes/no
45260   fn_askAddDuplicate$=aaResponse$
45280 fnend



76000 ! <updateable region: fn_open (supressprompt:=2)>  
76020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
76040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
76060   fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
76080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
76100     mat loadedsubs$(udim(loadedsubs$)+1) 
76120     loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
76140     for index=1 to udim(mat _fileiosubs$) 
76160       execute (_fileiosubs$(index)) 
76180     next index
76200   end if
76220 fnend
76240 ! </updateable region: fnopen>
78000 ! <updateable region: ertn>
78020 ERTN: fnerror(program$,err,line,act$,"xit")
78040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
78060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
78080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
78100 ERTN_EXEC_ACT: execute act$ : goto ERTN
78120 ! </updateable region: ertn>

