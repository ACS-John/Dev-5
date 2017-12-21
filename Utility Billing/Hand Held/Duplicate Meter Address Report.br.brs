10000 fn_setup
10020 fntop(program$)
14000 ! r: leave the program if Meter Address is not enabled
14020   fnreg_read('Meter Address Enable',u4_meterAddress$,'False')
14040   activeOnly=1
14060   if u4_meterAddress$='False' then
14080     mat mg$(0)
14100     fnAddOneC(mat mg$,'Meter Address LocationID tracking is currently disabled.')
14120     fnAddOneC(mat mg$,'This report is not applicable.')
14140     fnmsgbox(mat mg$,resp$)
14160     goto Xit
14180   end if
14200 ! /r
20020 fnopenprn
30000 ! r: Duplicate LocationIDs vs Customer records
30020   pr #255: 'Duplicate LocationIDs vs Customer'
30040   open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
30060   customersWithDuplicateLiCount=0
30080   ! r: build list: mat customerAccountList$ (account numbers) and mat customerLocationIdList (LocationIDs)
30100     dim customerMeterAddress$*30
30120     dim customerAccountList$(0)*10
30140     dim customerLocationIdList(0)
30160     dim customerMeterAddressList$(0)*30
30180     mat customerAccountList$(0)
30200     mat customerLocationIdList(0)
30220     mat customerMeterAddressList$(0)
30240     do
30260       read #hCustomer,using 'form pos 1,C 10,C 30,pos 1821,n 1':  acct$,customerMeterAddress$,finalBillingCode eof EndBuildList
30280       if ~activeOnly or finalBillingCode=0 then
30300         fnAddOneC(mat customerAccountList$,acct$)
30320         fnAddOneN(mat customerLocationIdList,fnMeterAddressLocationID(customerMeterAddress$, 1))
30340         fnAddOneC(mat customerMeterAddressList$,customerMeterAddress$)
30360       end if
30380     loop
30400     EndBuildList: ! 
30420     close #hCustomer:
30440   ! /r
30460   ! r: process that data and print it
30480     for customer=1 to udim(mat customerAccountList$)
30500       searchBefore=srch(mat customerLocationIdList,customerLocationIdList(customer))
30520       if searchBefore=customer then searchBefore=0
30540       searchAfter=srch(mat customerLocationIdList,customerLocationIdList(customer),customer+1)
30560       if searchBefore or searchAfter then
30580         pr #255,using 'form pos 1,C 10,x 1,N 9,x 2,C 30': customerAccountList$(customer),customerLocationIdList(customer),customerMeterAddressList$(customer)
30600         customersWithDuplicateLiCount+=1
30620       end if
30640     nex customer
30660     if customersWithDuplicateLiCount=0 then 
30680       pr #255: 'no customers with duplicate Location IDs were found'
30700     end if
30720     pr #255: ''
30740   ! /r
30760 ! /r
40000 ! r: Duplicate Meter Addresses vs LocationIDs
40020   pr #255: 'Duplicate Meter Addresses vs LocationIDs'
40040   ! r: gather all meter addresses
40060     dim form$(0)*256
40080     dim maData$(0)*30,maDataN(0)
40100     hMeterAddressLocationID=fn_open('UB Meter Address',mat maData$,mat maDataN,mat form$, 1)
40120     dim maLocationIdList(0)
40140     mat maLocationIdList(0)
40160     dim maMeterAddressList$(0)*30
40180     mat maMeterAddressList$(0)
40200     do
40220       read #hMeterAddressLocationID,using form$(hMeterAddressLocationID): mat maData$,mat maDataN eof EoMeterAddress
40240       fnAddOneN(mat maLocationIdList,maDataN(ma_LocationID))
40260       fnAddOneC(mat maMeterAddressList$,maData$(ma_name))
40280     loop
40300     EoMeterAddress: !
40320     close #hMeterAddressLocationID:
40340   ! /r
40360   ! r: process that data and print it
40380     for maItem=1 to udim(mat maLocationIdList)
40400       searchBefore=srch(mat maMeterAddressList$,maMeterAddressList$(maItem))
40420       if searchBefore=maItem then searchBefore=0
40440       searchAfter=srch(mat maMeterAddressList$,maMeterAddressList$(maItem),maItem+1)
40460       if searchBefore or searchAfter then
40480         pr #255,using 'form pos 1,N 9,x 2,C 30': maLocationIdList(maItem),maMeterAddressList$(maItem)
40500         LocationIdsWithDupNamesCount+=1
40520       end if
40540     nex maItem
40560     if LocationIdsWithDupNamesCount=0 then
40580       pr #255: 'no Location Ids with duplicate names (Meter Addresses) were found.'
40600     end if
40620     pr #255:  ''
40640   ! /r
40660 ! /r
50000 fncloseprn
60000 Xit: fnxit
62000 def fn_setup
62010   if ~setup then
62020     setup=1
62030     library 'S:\Core\Library': fntop,fnxit,fnerror
62040     library 'S:\Core\Library': fnCloseFile,fngethandle
62050     library 'S:\Core\Library': fnOpenFile
62060     library 'S:\Core\Library': fnopenprn,fncloseprn
62070     library 'S:\Core\Library': fnAddOneC,fnAddOneN
62080     library 'S:\Core\Library': fnmsgbox
62090     library 'S:\Core\Library': fnMeterAddressLocationID
62100     library 'S:\Core\Library': fnreg_read
62110     dim mg$(0)*80
62120   end if
62130 fnend
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
