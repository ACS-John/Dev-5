10000 fn_setup
10020 fntop(program$)
10300 fnHamsterFio('UB Meter Address')
10320 XIT: !
10340 fnxit
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12050     library 'S:\Core\Library': fntop,fnxit,fngethandle,fnerror,fnindex_it,fnStatusClose,fnStatus,fnHamsterFio
12060     library 'S:\Core\Library': fnAddOneC,fnAddOneN,fnCountMatchesN,fnArrayMax,fnKeyExists
12070     library 'S:\Core\Library': fnmsgbox,fnOpenFile,fnCloseFile,fnBuildKey$
12072     library 'S:\Core\Library': fncreg_read,fncreg_write,fnreg_read,fnreg_write
12080     dim form$(0)*256
12100     dim maData$(0)*30,maDataN(0)
12120     dim mg$(0)*128
12480   end if
12500   if ~exists('[Q]\UBmstr\MeterAddress.h[cno]') then let fn_InitialializeMeterAddress
12900 fnend
14000 def fn_InitialializeMeterAddress
14020   imaNeedsInitialization=0
14040   if ~exists('[Q]\UBmstr\MeterAddress.h[cno]') then
14060     imaNeedsInitialization=1
14080   end if
14100   hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, imaInputOnly,2)
15000   if imaNeedsInitialization then
15020     fnStatus('Initializing UB Meter Address table...')
15040     fnCloseFile(hMeterAddressLocationID,'UB Meter Address') 
15060     fnindex_it('[Q]\UBmstr\MeterAddress.h[cno]','[Q]\UBmstr\MeterAddress_Idx2.h[cno]', '12 30u')
15080     hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 0,2)
15100     fn_newLocationID( 1)
15120     open #hCustomer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],Shr",internal,input,relative
15140     dim imaMeterAddress$*30
15160     do
15180       read #hCustomer,using 'form pos 11,C 30':  imaMeterAddress$ eof imaCustomerFinis
15200       fn_imaAdd(hMeterAddressLocationID,imaMeterAddress$)
15220     loop
15240     imaCustomerFinis: !
15260     close #hCustomer:
15280     ! fnStatusPause
15290     fnStatusClose
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
