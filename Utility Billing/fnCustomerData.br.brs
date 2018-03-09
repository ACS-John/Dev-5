02000 def fn_setup
02020   if ~setup then
02040     setup=1
02060     library 'S:\Core\Library': fnerror
02260     library 'S:\Core\Library': fnapplyDefaultRatesFio
02322     library 'S:\Core\Library': fnOpenFile,fnCloseFile
03000     on error goto ERTN
03020   ! dims, constants, top, etc
03040     dim cus$(0)*256,cusN(0)
04420   !
05240   end if
05260 fnend

28000 def library fnCustomerData$*128(account$*10,fieldName$*40; leaveOpen)
28020   if ~setup then let fn_setup
28040   fnCustomerData$=fn_customerData$(account$,fieldName$, leaveOpen)
28060 fnend
30000 def fn_customerData$*128(account$*10,fieldName$*40; leaveOpen)
30020   account$=lpad$(trim$(account$),10)
30040   if customerDataSetup$<>account$ then ! r:
30060     customerDataSetup$=account$
30080     if ~hCustomer then
30100       hCustomer=fn_open('UB Customer',mat cus$,mat cusN,mat form$, 1)
30120       ! open #hCustomer:=fngethandle: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',internal,input,keyed
30140     end if
30160     dim account$*10
30180     mat cus$=('')
30200     mat cusN=(0)
30220     read #hCustomer,using form$(hCustomer),key=account$,release: mat cus$,mat cusN nokey CustomerDataFinis
30240     fnapplyDefaultRatesFio(mat cusN)
30340   end if ! /r
30360   dim customerDataReturn$*128
30380   customerDataReturn$=''
30400   fieldName$=lwrc$(fieldName$)
32000   if fieldName$='route' then
32020     if cusN(c_route)<>0 then customerDataReturn$=str$(cusN(c_route))
32040   else if fieldName$='sequence' then
32060     if cusN(c_sequence)<>0 then customerDataReturn$=str$(cusN(c_sequence))
32080   else if fieldName$='final billing code' then
32100     if cusN(c_finalBilling)<>0 then customerDataReturn$=str$(cusN(c_finalBilling))
32120   else if fieldName$='last billing day' then
32140     if cusN(c_lastBillingDate)<>0 then customerDataReturn$=str$(days(cusN(c_lastBillingDate),'mmddyy'))
32160   else if fieldName$='name' then
32180     customerDataReturn$=cus$(c_name)
32200   else if fieldName$='service 1.rate code' then
32220     if cusN(c_s01rate)<>0 then customerDataReturn$=str$(cusN(c_s01rate))
32240   else if fieldName$='service 2.rate code' then
32260     if cusN(c_s02rate)<>0 then customerDataReturn$=str$(cusN(c_s02rate))
32280   else if fieldName$='service 3.rate code' then
32300     if cusN(c_s03rate)<>0 then customerDataReturn$=str$(cusN(c_s03rate))
32320   else if fieldName$='service 4.rate code' then
32340     if cusN(c_s04rate)<>0 then customerDataReturn$=str$(cusN(c_s04rate))
32360   else if fieldName$='service 5.rate code' then
32380     if cusN(c_s05rate)<>0 then customerDataReturn$=str$(cusN(c_s05rate))
32400   else if fieldName$='service 9.rate code' then
32420     if cusN(c_s09rate)<>0 then customerDataReturn$=str$(cusN(c_s09rate))
32440   else if fieldName$='service 10.rate code' then
32460     if cusN(c_s10rate)<>0 then customerDataReturn$=str$(cusN(c_s10rate))
32480   else if fieldName$='service 1.reading.current' then
32500     if cusN(c_s01readingCur)<>0 then customerDataReturn$=str$(cusN(c_s01readingCur))
32520   else if fieldName$='service 1.reading.prior' then
32540     if cusN(c_s01readingPri)<>0 then customerDataReturn$=str$(cusN(c_s01readingPri))
32560   else if fieldName$='service 1.usage.current' then
32580     if cusN(c_s01UsageCur)<>0 then customerDataReturn$=str$(cusN(c_s01UsageCur))
32600   else if fieldName$='service 1.usage.ytd' then
32620     if cusN(c_s01UsageYtd)<>0 then customerDataReturn$=str$(cusN(c_s01UsageYtd))
32640   else if fieldName$='service 3.reading.current' then
32660     if cusN(c_s03readingCur)<>0 then customerDataReturn$=str$(cusN(c_s03readingCur))
32680   else if fieldName$='service 3.reading.prior' then
32700     if cusN(c_s03ReadingPri)<>0 then customerDataReturn$=str$(cusN(c_s03ReadingPri))
32720   else if fieldName$='service 3.usage.current' then
32740     if cusN(c_s03UsageCur)<>0 then customerDataReturn$=str$(cusN(c_s03UsageCur))
32760   else if fieldName$='service 3.usage.ytd' then
32780     if cusN(c_s03UsageYtd)<>0 then customerDataReturn$=str$(cusN(c_s03UsageYtd))
32800   else if fieldName$='service 4.reading.current' then
32820     if cusN(c_s04readingCur)<>0 then customerDataReturn$=str$(cusN(c_s04readingCur))
32840   else if fieldName$='service 4.reading.prior' then
32860     if cusN(c_s04readingPri)<>0 then customerDataReturn$=str$(cusN(c_s04readingPri))
32880   else if fieldName$='service 4.usage.current' then
32900     if cusN(c_s04usageCur)<>0 then customerDataReturn$=str$(cusN(c_s04usageCur))
32920   else if fieldName$='service 4.usage .ytd' then
32940     if usN(c_s04usageYtd)<>0 then customerDataReturn$=str$(usN(c_s04usageYtd))
32960   else if fieldName$='service 1.unit count' then
32980     if cusN(c_s01unitCount)<>0 then customerDataReturn$=str$(cusN(c_s01unitCount))
33000   else if fieldName$='demand multiplier' then
33020     if cusN(c_demandMultiplier)<>0 then customerDataReturn$=str$(cusN(c_demandMultiplier))
33040   else if fieldName$='demand reading' then
33060     if cusN(c_demandReading)<>0 then customerDataReturn$=str$(cusN(c_demandReading))
33080   else 
33100     pr 'fn_customerData$ does not recognize the field: '&fieldName$
33120     pause
33140   end if
33160   CustomerDataFinis: !
33180   if ~leaveOpen then
33200     close #hCustomer: 
33220     hCustomer=0
33240   end if
33260   fn_customerData$=customerDataReturn$
33280 fnend

86000 ! <updateable region: fn_open (supressprompt:=2)>  
86020 def fn_open(filename$*255, mat f$, mat fn, mat form$; inputonly, keynum, dont_sort_subs, path$*255, mat descr$, mat field_widths,dontupdate,___,index)
86030   dim _fileiosubs$(1)*800
86040   dim loadedsubs$(1)*32
86050   dim form$(0)*2048
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

