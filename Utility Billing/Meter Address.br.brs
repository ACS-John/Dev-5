00010 ! if env$('acsDeveloper')<>'' then exe 'free '&env$('Q')&'\ubmstr\meteraddress*.*' err ignore
10000 let fn_setup
10020 fntop(program$)
10040 fnreg_read('Meter Address Enable',u4_meterAddress$,'False')
10060 if u4_meterAddress$='False' then
10100   fnAddOneC(mat mg$,'Meter Address LocationID tracking is currently disabled.')
10120   fnAddOneC(mat mg$,'Would you like to enable it now?')
10160   fnmsgbox(mat mg$,mgResp$, '', 4)
10180   if mgResp$='Yes' then
10200   u4_meterAddress$='True'
10220   fnreg_write('Meter Address Enable',u4_meterAddress$)
10240   else
10260     goto XIT
10280   end if
10290 end if
10300 fnHamsterFio('UB Meter Address')
10320 XIT: !
10340 fnxit
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library': fnAddOneC,fnmsgbox,fnOpenFile,fncreg_read,fncreg_write,fntop,fnxit,fngethandle,fnCloseFile,fnerror,fnreg_read,fnreg_write,fnindex_it,fnstatus_close,fnstatus,fnHamsterFio
12080     dim form$(0)*256
12100     dim maData$(0)*30,maDataN(0)
12120     dim mg$(0)*80
12480   end if
12500   if ~exists(env$('Q')&'\UBmstr\MeterAddress.h'&env$('cno')) then let fn_InitialializeMeterAddress
12900 fnend
14000 def fn_InitialializeMeterAddress
14020   imaNeedsInitialization=0
14040   if ~exists(env$('Q')&'\UBmstr\MeterAddress.h'&env$('cno')) then
14060     imaNeedsInitialization=1
14080   end if
14100   hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, imaInputOnly,2)
15000   if imaNeedsInitialization then
15020     fnstatus('Initializing UB Meter Address table...')
15040     fnCloseFile(hMeterAddressLocationID,'UB Meter Address') 
15060     fnindex_it(env$('Q')&'\UBmstr\MeterAddress.h'&env$('cno'),env$('Q')&'\UBmstr\MeterAddress_Idx2.h'&env$('cno'), '12 30u')
15080     hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 0,2)
15100     fn_newLocationID( 1)
15120     open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",Shr",internal,input,relative
15140     dim imaMeterAddress$*30
15160     do
15180       read #hCustomer,using 'form pos 11,C 30':  imaMeterAddress$ eof imaCustomerFinis
15200       fn_imaAdd(hMeterAddressLocationID,imaMeterAddress$)
15220     loop
15240     imaCustomerFinis: !
15260     close #hCustomer:
15280     ! fnstatus_pause
15290     fnstatus_close
15300   end if
16000   fn_InitialializeMeterAddress=hMeterAddress
16020 fnend
18000 def fn_imaAdd(hMeterAddressName,imaMeterAddress$*30) ! add the record if it does not exist (used for initializing the Meter Address file)
18020    read #hMeterAddressName,key=imaMeterAddress$,release: nokey IaAdd
18040    goto IaXit
18060    IaAdd: !
18080    maDataN(ma_LocationID)=fn_newLocationID
18100    maData$(ma_Name)=imaMeterAddress$
18120    write #hMeterAddressName,using form$(hMeterAddressLocationID),release: mat maData$,mat maDataN
18140    fn_imaAdd=1
18160    IaXit: !
18180 fnend

33000 def library fnMeterAddressLocationID(meterAddress$*30; leaveFileOpen)
33020   if ~setup then let fn_setup
33040   if leaveFileOpen and hMeterAddressName<>0 then goto maliPastOpen
33060   hMeterAddressName=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 1,2)
33080   maliPastOpen: !
33100   maDataN(ma_LocationID)=-1
33120   read #hMeterAddressName,using form$(hMeterAddressName),key=rpad$(meterAddress$,KLN(hMeterAddressName)),release: mat maData$,mat maDataN nokey ignore
33140   if ~leaveFileOpen then
33160     close #hMeterAddressName:
33180     hMeterAddressName=0
33200   end if
33220   fnMeterAddressLocationID=maDataN(ma_LocationID)
33240 fnend
35000 def library fnMeterAddressName$*30(locationId; leaveFileOpen)
35020   if ~setup then let fn_setup
35040   if leaveFileOpen and hMeterAddressLocationID<>0 then goto manPastOpen
35060   hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 1)
35080   manPastOpen: !
35100   maDataN(ma_LocationID)=-1
35120   read #hMeterAddressLocationID,using form$(hMeterAddressLocationID),key=cnvrt$('N 11',locationId),release: mat maData$,mat maDataN nokey ignore
35140   if ~leaveFileOpen then
35160     close #hMeterAddressLocationID:
35180     hMeterAddressLocationID=0
35200   end if
35220   fnMeterAddressName$=maData$(ma_name)
35240 fnend

40000 def library fnMeterAddressUpdate(meterAddressBefore$*30,&meterAddressAfter$)
40020   if ~setup then let fn_setup
40040   meterAddressBefore$=rtrm$(meterAddressBefore$)
40060   meterAddressAfter$=rtrm$(meterAddressAfter$)
40080   if meterAddressBefore$<>meterAddressAfter$ then
40100     hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$)
40120     hMeterAddressName=hMeterAddressLocationID+1
40140     if fn_keyExists(hMeterAddressName,meterAddressAfter$) then
40160       doAdd$=fn_askAddDuplicate$(meterAddressBefore$,meterAddressAfter$)
40170       if doAdd$='No' then meterAddressAfter$=meterAddressBefore$ : doAdd$='Cancel'
40200     else if meterAddressBefore$='' and ~fn_keyExists(hMeterAddressName,meterAddressAfter$) then ! changed from blank - it is new
40220       doAdd$='Yes'
40300     else if lwrc$(meterAddressBefore$)=lwrc$(meterAddressAfter$) then  ! only case changes - it is an update
40320       doAdd$='No'
40340     else
40360       doAdd$=fn_askAddNew$(meterAddressBefore$,meterAddressAfter$)
40380     end if
40400 ! pr 'doAdd$=';doAdd$ : pause
40460     if doAdd$='Yes' then
40480       maDataN(ma_LocationID)=fn_newLocationID
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
40720     fnCloseFile(hMeterAddressLocationID,'UB Meter Address')
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
46000 def fn_keyExists(hFile,keyToTest$*128)
46020   read #hFile,key=rpad$(keyToTest$,KLN(hFile)),release: nokey MaeNo
46040   maeReturn=1
46060   goto MaeFinis
46080   MaeNo: !
46100   maeReturn=0
46120   MaeFinis: !
46140   fn_keyExists=maeReturn
46160 fnend
76000 ! <updateable region: fn_open (supressprompt:=2)>  
76020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
76040   dim _fileiosubs$(1)*800, loadedsubs$(1)*32
76060   let fn_open=fnOpenFile(filename$, mat f$, mat fn, mat form$, inputonly, keynum, dont_sort_subs, path$, mat descr$, mat field_widths, mat _fileiosubs$,supressprompt:=2)
76080   if ~max(srch(loadedsubs$,uprc$(filename$)),0) then 
76100     mat loadedsubs$(udim(loadedsubs$)+1) 
76120     let loadedsubs$(udim(loadedsubs$))=uprc$(filename$)
76140     for index=1 to udim(mat _fileiosubs$) 
76160       execute (_fileiosubs$(index)) 
76180     next index
76200   end if
76220 fnend
76240 ! </updateable region: fnopen>
78000 ! <updateable region: ertn>
78020 ERTN: let fnerror(program$,err,line,act$,"xit")
78040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
78060   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
78080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
78100 ERTN_EXEC_ACT: execute act$ : goto ERTN
78120 ! </updateable region: ertn>
