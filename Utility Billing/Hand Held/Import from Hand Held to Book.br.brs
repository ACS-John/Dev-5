12000 ! formerly S:\acsUB\hhfro
12020 ! -- Transfer Data From Hand Held to Computer
12040 library program$: fnRetrieveHandHeldFile
12060 fn_setup
12080   if ~fnregistered_for_hh then
12100     mat ml$(2)
12120     ml$(1)="You must purchase the ACS Utility Billing Hand Held"
12140     ml$(2)="module to access these features"
12160     fnmsgbox(mat ml$, response$, cap$,64)
12180     goto XIT
12200   end if  ! ~fnregistered_for_hh
12220 fnRetrieveHandHeldFile
12240 fnxit
16000 def fn_setup
16020   library 'S:\Core\Library': fnxit,fnureg_read
16040   library 'S:\Core\Library': fntop,fnerror,fngethandle
16060   library 'S:\Core\Library': fnregistered_for_hh,fnhand_held_Device$,fnHandHeldList
16080   library 'S:\Core\Library': fnTos,fnLbl,fnAcs,fnTxt,fnCmdSet,fnChk,fncomboa
16100   library 'S:\Core\Library': fnmsgbox
16120   library 'S:\Core\Library': fnureg_write
16140   library 'S:\Core\Library': fnCopy,fnRename
16150   library 'S:\Core\Library': fnaddonec,fnFileTo2Arrays
16152   library 'S:\Core\Library': fnAccountFromLocationId$
16154   library 'S:\Core\Library': fnsrch_case_insensitive
16156   library 'S:\Core\Library': fnmakesurepathexists
16160   on error goto ERTN
16180   dim preferenceHandHeldFromFile$*128
16200   fnureg_read('Hand Held From File',preferenceHandHeldFromFile$)
16220   if deviceSelected$="EZReader" then preferenceHandHeldFromFile$='[ask]'
16240   ! if deviceSelected$="Green Tree" then preferenceHandHeldFromFile$='[ask]'
16260   ! if deviceSelected$="Hersey" then preferenceHandHeldFromFile$='[ask]'
16280   dim ml$(2)*256
16300   dim cap$*128
16320   dim a$*512
16340   dim ln$*22
16360   dim resp$(32)*256
16380   dim amr$*619
16400   dim hersey$*290
16420   dim askPath$*128
16440   dim easy$*619
16460   dim devicePreference$*20
16480   dim deviceSelected$*20
16490   dim line$*2048 ! temp variable for reading in lines before they are parsed.
16492   dim lineItem$(0)*256 ! temp variable for reading in lines before they are parsed.
16500   devicePreference$=fnhand_held_Device$ ! fn_ctext_setup
16520   if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
16540     fnureg_read('Hand Held From File Asked',askPath$)
16560   end if
16580   if lwrc$(devicePreference$)='[ask]' then
16600     fnureg_read('Hand Held Device Asked',deviceSelected$)
16620     dim deviceOption$(0)*20
16640     fnHandHeldList(mat deviceOption$)
16660   end if
16680 fnend
24000 def library fnRetrieveHandHeldFile
24020   if ~setup then let fn_setup
24040   fntop(program$)
25000   SCREEN1: ! r:
25020     respc=0 : lc=0
25040     fnTos(sn$="hh_fro")
25060     lc+=1
25080     fnLbl(lc+=1,1,"Book Number to store readings:",30,1)
25100     fnTxt(lc,32,2,0,1,"20",0,"Be careful not to use the same route # twice in the same billing cycle.  The first route will be lost if it has not been calculated.")
25120     resp$(rc_book:=respc+=1)=''
25140     lc+=1
25160     fnChk(lc+=1,33,'Merge into book', 1,0,0,0) ! requires a format that utilizes [ACS Hand Held File Generic Version 2]
25180     fnLbl(lc,35,'(only supported by some devices)') ! requires a format that utilizes [ACS Hand Held File Generic Version 2]
25200     resp$(rc_merge:=respc+=1)='False'
25220     lc+=1
25240     fnLbl(lc+=1,1,"Hand Held model:",30,1)
25260     if lwrc$(devicePreference$)='[ask]' then
25280       fncomboa("HH-FroCBox",lc,32,mat deviceOption$)
25300       resp$(rc_Device:=respc+=1)=deviceSelected$
25320     else
25340       fnLbl(lc,32,deviceSelected$)
25360     end if
25380       lc+=1
25400     if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
25420       lc+=1
25440       fnLbl(lc+=1,1,"Source File:",30,1)
25460       fnTxt(lc,32,20,256,0,"70",0,'Source file should be drive designation and file name of the file returned from the Hand Held.')
25480       rc_path:=respc+=1 : if resp$(rc_path)="" then resp$(rc_path)=askPath$
25500     else
25520       fnLbl(lc+=1,1,"Importing from "&fn_hh_input_filename$,len("Importing from "&fn_hh_input_filename$),1)
25540     end if
25560     fnCmdSet(2)
26000     fnAcs(sn$,0,mat resp$,ckey)
26020     if ckey<>5 then
26040       bk$=resp$(rc_book)
26060       if lwrc$(devicePreference$)='[ask]' then
26080         deviceSelected$=resp$(rc_Device)
26100         fnureg_write('Hand Held Device Asked',deviceSelected$)
26120       else
26140         deviceSelected$=devicePreference$
26160       end if
26170 ! pr 'deviceSelected$='&deviceSelected$ : pause
26180       enableMerge$=resp$(rc_merge)
26200       if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
26220         askPath$=resp$(rc_path)
26240         fnureg_write('Hand Held From File Asked',askPath$)
26260         if ~exists(env$('at')&askPath$) then
26280           mat ml$(1)
26300           ml$(1)="There is no file by this name at "&askPath$
26320           fnmsgbox(mat ml$, response$)
26330           goto SCREEN1
26340         end if
26350       end if
26360       if fn_transfer(bk$,enableMerge$,env$('at')&askPath$)=-1 then goto SCREEN1
26380     end if
26400   goto XIT ! /r
26700   XIT: ! target of ERTN exits
26720 fnend
28000 def fn_transfer(bk$,enableMerge$,askPath$*128)
28020   transferReturn=0
28040   dim bookFile$*512
28060   bookFile$=env$('Q')&"\UBmstr\Readings."&ltrm$(bk$)
28080   if enableMerge$='True' and exists(bookFile$) then 
28100     dim mergeFileOrigional$*512
28120     mergeFileOrigional$=env$('temp')&'\acs\mergeFileOrigional-book'&bk$&'-session'&session$&'.txt'
28140     fnmakesurepathexists(mergeFileOrigional$)
28160     fnCopy(bookFile$,mergeFileOrigional$)
28180   else 
28200     enableMerge$='False'
28220   end if
28240   if deviceSelected$='Aclara' then
28260     transferReturn=fn_aclara(bookFile$,enableMerge$)
28280   else if deviceSelected$="Aclara Work Order" then
28300     transferReturn=fn_aclaraWorkOrder(bookFile$,enableMerge$)
28320   else if deviceSelected$="ACS Meter Reader" then
28340     fn_acsmr(bookFile$)
28360   else if deviceSelected$="AMR" then
28380     fn_amr(bookFile$)
28400   else if deviceSelected$="Badger" or deviceSelected$="Badger Connect C" then
28420     fnCopy(fn_hh_input_filename$,bookFile$)
28440   else if deviceSelected$="Boson" then
28460     fn_boson(bookFile$)
28480   else if deviceSelected$='CSV by LocationID' then
28500     transferReturn=fn_CsvByLocationId(bookFile$,enableMerge$)
28520   else if deviceSelected$="DriveBy" then
28540     fnCopy(fn_hh_input_filename$,bookFile$)
28560   else if deviceSelected$="EZReader" then
28580     fn_ezreader(bookFile$)
28600   else if deviceSelected$="Green Tree" then
28620     fnCopy(fn_hh_input_filename$,bookFile$)
28640   else if deviceSelected$="Hersey" then
28660     fn_hersey(bookFile$)
28680   else if deviceSelected$="Itron FC300" then
28700     fn_itron(bookFile$)
28720   else if deviceSelected$="LapTop" then
28740     fn_laptop(bookFile$)
28760   else if deviceSelected$="Master Meter" then
28780     fn_import_l_readings_txt(bookFile$, 358)
28800   else if deviceSelected$="Other" and env$('client')="Brier Lake" then
28820     fn_import_l_readings_txt(bookFile$)
28840   else if deviceSelected$="Psion Workabout" then
28860     fn_psion_workabout(bookFile$)
28880   else if deviceSelected$="READy Water" then
28900     fn_import_l_readings_txt(bookFile$)
28920   else if deviceSelected$="Sensus" then
28940     fn_sensus_in(bookFile$)
28960   else if deviceSelected$="Unisys" then
28980     fnCopy(fn_hh_input_filename$,bookFile$)
29000   end if
29020   if transferReturn>0 then
29040     mat ml$(1)
29060     ml$(1)=str$(transferReturn)&' records imported to book '&bk$&'.'
29080     fnmsgbox(mat ml$)
29100   end if
29120   fn_transfer=transferReturn
29140 fnend
30000 def fn_hh_input_filename$*256
30010   ! requires local variables: deviceSelected$,preferenceHandHeldFromFile$,askPath$
30020   dim hif_return$*256
30030   hif_return$=preferenceHandHeldFromFile$
30040   if lwrc$(hif_return$)='[ask]' then
30050     hif_return$=askPath$
30060   else if trim$(hif_return$)='' then
30080     if deviceSelected$="Sensus" then
30100       hif_return$='c:\vol002\amrs\READ.DAT'
30200     else if deviceSelected$="EZReader" then
30220       hif_return$=askPath$
30240     else if deviceSelected$="ACS Meter Reader" then
30260       hif_return$=askPath$
30280     else if deviceSelected$="AMR" then
30300       hif_return$=askPath$
30320     else if deviceSelected$="Itron FC300" then
30340       ! if env$('client')='Findlay' then
30360       !   hif_return$='\\vof-pc\itronshared\FCS\Export\Output\UPLOAD.DAT' ! "C:\Itron\FCSShare\Export\Output\upload.dat"
30400       ! else
30420         hif_return$="C:\mvrs\xfer\upload\UPLOAD.DAT"
30440       ! end if
30480     else if deviceSelected$="Psion Workabout" then
30500       hif_return$=env$('Q')&"\UBmstr\Readings.out"
30520     else if deviceSelected$="Badger" or deviceSelected$="Badger Connect C" or deviceSelected$="DriveBy" then
30540       hif_return$="c:\connect\connect.ot3"
30640     else if deviceSelected$="Unisys" then
30660       hif_return$=env$('Q')&"\UBmstr\ReadOut.dat"
30680     else if deviceSelected$="Boson" then
30700       if env$('client')="Monticello" then
30710         hif_return$=env$('Q')&"\UBmstr\outpalm.txt"
30712       ! else if env$('client')="Cerro Gordo" then
30714       !   hif_return$=env$('at')&'C:\ProgramData\ACS\UBmstr\outofpalm.txt'
30740       else
30760         hif_return$=env$('Q')&"\UBmstr\outofpalm.txt"
30780       end if
30840     else if deviceSelected$="Other" and env$('client')="Brier Lake" then
30860       hif_return$="L:\readings.txt"
30880     end if
30890   end if
30892   if hif_return$(1:2)='@:' then let hif_return$(1:2)='' ! take it off if it is already there before putting it back on.
30900   fn_hh_input_filename$=env$('at')&hif_return$
30920 fnend
31000 IGNORE: continue
33000 def fn_readingsFileVersion$*128(bookFile$*512)
33020   dim rfvLine$*512,rfvReturn$*128
33040   open #hRfv:=fngethandle: "Name="&bookFile$,display,input
33060   linput #hRfv: rfvLine$
33080   close #hRfv:
33100   if rtrm$(rfvLine$)='[ACS Hand Held File Generic Version 2]' then
33120     rfvReturn$='[ACS Hand Held File Generic Version 2]'
33140   else
33160     rfvReturn$='legacy'
33180     if env$('acsDeveloper')<>'' then pause
33200   end if
33210   fn_readingsFileVersion$=rfvReturn$
33220 fnend
36000 def fn_acsmr(bookFile$*256) ! ACS Meter Reader
36030   source$=resp$(1)
36060   open #2: "Name="&fn_hh_input_filename$&",RecL=256",display,input ! acs_meter_data.txt
36090   fn_readings_backup(bookFile$)
36120   open #3: "Name="&bookFile$&",RecL=30,replace",display,output
36150   do
36180     linput #2: amr$ eof ACSMR_XIT
36210     z$=amr$(1:10)
36240     reading=val(amr$(133:142))
36270     pr #3,using "form pos 1,c 10,n 10": z$,reading
36300   loop
36330   ACSMR_XIT: !
36360 fnend
37000 def fn_CsvByLocationId(bookFile$*512,enableMerge$)
37020   if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then aclaraWorkOrderReturn=-1 : goto CblEoF
37100   open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
37120   open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
37140   pr #hOut: '[ACS Hand Held File Generic Version 2]'
37150   pr #hOut: 'Source File='&fn_hh_input_filename$
37160   linput #hIn: line$ eof CblEoF 
37180   if srch(line$,chr$(9))>0 then cblDelimiter$=chr$(9) else cblDelimiter$=','
37200   dim cblItem$(0)*256
37220   str2mat(line$,mat cblItem$,cblDelimiter$)
37240   cblCsv_LocationId=fn_findFirstMatch(mat cblItem$,'Location ID','LocationID')
37260   cblCsv_ReadingWater=fn_findFirstMatch(mat cblItem$,'Water Reading')
37280   cblCsv_WaterTransmitter=fn_findFirstMatch(mat cblItem$,'Water Transmitter Number','MTU ID')
37300   cblCsv_WaterTransmitterSuffix=fn_findFirstMatch(mat cblItem$,'Port')
37320   do
37340     linput #hIn: line$ eof CblEoF 
37360     str2mat(line$,mat cblItem$,cblDelimiter$)
37380     pr #hOut: 'Customer.Number='&fnAccountFromLocationId$(val(cblItem$(cblCsv_LocationId)),1)
37400     fn_g2IfTherePrOut(cblCsv_ReadingWater,'Reading.Water',mat cblItem$)
37410     if cblCsv_LocationId<>0 then
37412       pr #hOut: 'MeterAddress.LocationID='&str$(val(cblItem$(cblCsv_LocationId)))
37414     end if
37420     if cblCsv_WaterTransmitter<>0 and cblCsv_WaterTransmitterSuffix<>0 then 
37440       pr #hOut: 'Meter.Transmitter.Water='&trim$(cblItem$(cblCsv_WaterTransmitter))&'-'&trim$(cblItem$(cblCsv_WaterTransmitterSuffix))
37460     else if cblCsv_WaterTransmitter<>0 then
37480       pr #hOut: 'Meter.Transmitter.Water='&trim$(cblItem$(cblCsv_WaterTransmitter))
37500     end if
37520     pr #hOut: ''
37540   loop
37560   CblEoF: !
37580   close #hIn:
37600   close #hOut:
37620   if enableMerge$='True' then
37640     fn_mergeBooks(mergeFileOrigional$,bookFile$)
37660   end if
37680 fnend
38000 def fn_g2IfTherePrOut(gitproItemEnum,gitproLabel$*128,mat gitproItem$)
38020   ! utility for [ACS Hand Held File Generic Version 2]
38040     if gitproItemEnum<>0 and val(gitproItem$(gitproItemEnum))<>0 then 
38060       pr #hOut: gitproLabel$&'='&gitproItem$(gitproItemEnum)
38080     end if
38100 fnend

40000 def fn_aclara(bookFile$*512,enableMerge$)
40020   ! pr ' this import is not yet written.'
40040   ! pr ' this import will only import active clients'
40060   ! pause
40080   if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then aclaraWorkOrderReturn=-1 : goto CblEoF
40100   open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
40120   open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
40140   pr #hOut: '[ACS Hand Held File Generic Version 2]'
40160   pr #hOut: 'Source File='&fn_hh_input_filename$
40180   if dataIncludesHeaders then
40200     linput #hIn: line$ eof EO_Aclara ! just consume the headers
40220   end if
40240   do
40260     z$=''
40280     linput #hIn: line$ eof EO_Aclara
40300     fn_aclaraParseLine(line$,mat tmpDataName$,mat tmpDataValue$)
40320     for awoX=1 to udim(mat tmpDataName$)
40340       pr #hOut: tmpDataName$(awoX)&'='&tmpDataValue$(awoX)
40360     nex awoX
40380     pr #hOut: ''
40400     aclaraReturn+=1
40420   loop
40440   EO_Aclara: !
40460   close #hIn:
40480   close #hOut:
40500   if enableMerge$='True' then
40520     fn_mergeBooks(mergeFileOrigional$,bookFile$)
40540   end if
40560   fn_aclara=aclaraReturn
40580 fnend
42000 def fn_aclaraParseLine(line$*1024,mat tmpDataName$,mat tmpDataValue$)
42020     reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
42040     str2mat(line$,mat lineItem$,chr$(9))
42060     mat tmpDataName$(0)
42080     mat tmpDataValue$(0)
42100     fn_addTmpData('Customer.Number',lineItem$(2))
42120     fn_addTmpData('Reading.Water'  ,lineItem$(7))
42140     ! fn_addTmpData('Reading.Water.Date'  ,lineItem$(8))
42160 fnend
57000 def fn_aclaraWorkOrder(bookFile$*512,enableMerge$)
57020   dataIncludesHeaders=1
57040   if enableMerge$='True' and ~fn_okToMerge(bookFile$,'[ACS Hand Held File Generic Version 2]') then aclaraWorkOrderReturn=-1 : goto EO_AW
57120   open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
57140   open #hOut:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
57160   pr #hOut: '[ACS Hand Held File Generic Version 2]'
57170   pr #hOut: 'Source File='&fn_hh_input_filename$
57180   if dataIncludesHeaders then
57200     linput #hIn: line$ eof EO_AW ! just consume the headers
57220   end if
57240   do
57260     z$=''
57280     linput #hIn: line$ eof EO_AW
57300     fn_awoParseLine(line$,mat tmpDataName$,mat tmpDataValue$)
57320     for awoX=1 to udim(mat tmpDataName$)
57340       pr #hOut: tmpDataName$(awoX)&'='&tmpDataValue$(awoX)
57360     nex awoX
57370     pr #hOut: ''
57380     aclaraWorkOrderReturn+=1
57400   loop
57420   EO_AW: !
57440   close #hIn:
57460   close #hOut:
57480   if enableMerge$='True' then
57500     fn_mergeBooks(mergeFileOrigional$,bookFile$)
57520   end if
57540   fn_aclaraWorkOrder=aclaraWorkOrderReturn
57560 fnend
58000 def fn_awoParseLine(line$*1024,mat tmpDataName$,mat tmpDataValue$)
58020     reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
58040     str2mat(line$,mat lineItem$,chr$(9))
58042     for x=1 to udim(mat lineItem$) : lineItem$(x)=trim$(lineItem$(x),'"') : next x
58060     mat tmpDataName$(0)
58080     mat tmpDataValue$(0)
58100     ! fn_addTmpData('Customer.Number'                              ,lineItem$(2)             ) ! account numbers aren't necessarally correct
58102     fn_addTmpData('Customer.Number'                              ,fnAccountFromLocationId$(val(lineItem$(1))) )
58110     fn_addTmpData('MeterAddress.LocationID'                      ,str$(val(lineItem$(1)))                        )
58120     fn_addTmpData('MeterChangeOut.ReadingBefore.Water'           ,lineItem$(12)                                   )
58130     fn_addTmpData('MeterChangeOut.ReadingAfter.Water'            ,lineItem$(14)                                   ) ! usually 0
58140     fn_addTmpData('Meter.Transmitter.Water'                      ,lineItem$(10)&'-'&lineItem$(17)                ) !
58180     fn_addTmpData('Meter.Meter Number.Water'                     ,lineItem$(13)                                   )
58220     fn_addTmpData('Meter.Longitude.Water'                        ,lineItem$(21)                                   )
58240     fn_addTmpData('Meter.Latitude.Water'                         ,lineItem$(22)                                   )
58300 fnend
59000 def fn_addTmpData(name$*128,value$*128)
59020   dim tmpDataName$(0)*128
59040   dim tmpDataValue$(0)*128
59060   fnaddonec(mat tmpDataName$,name$)
59080   fnaddonec(mat tmpDataValue$,value$)
59100 fnend
61000 def fn_amr(bookFile$*512)
61020   fn_readings_backup(bookFile$)
61040   open #3: "Name="&bookFile$&",RecL=30,replace",display,output
61060   open #2: "Name="&fn_hh_input_filename$&",RecL=620",display,input
61080   linput #2: amr$ ioerr AMR_NOTHING_TO_READ ! read header
61100   do
61120     linput #2: amr$ eof AMR_XIT
61140     z$=lpad$(trim$(amr$(3:22)),10)
61160     reading=val(amr$(47:56))
61180     pr #3,using "form pos 1,c 10,n 10": z$,reading
61200   loop
61220   goto AMR_XIT !  AMR_NOTHING_TO_READ
61240   AMR_NOTHING_TO_READ: !
61260   mat ml$(1)
61280   ml$(1)="The File ("&fn_hh_input_filename$&") is empty."
61300   fnmsgbox(mat ml$,resp$,cap$,0)
61320   goto AMR_XIT !  AMR_NOTHING_TO_READ
61340   AMR_XIT: !
61360   close #2: ioerr ignore
61380   close #3: ioerr ignore
61400   !
61420 fnend
62000 def fn_boson(bookFile$*512)
62030   if ~exists(fn_hh_input_filename$) then
62060     mat ml$(2)
62090     ml$(1)='The import file ('&os_filename$(fn_hh_input_filename$)&') could not be found.'
62120     ml$(2)='You may need to perform the Hot Sync and try again.'
62150     fnmsgbox(mat ml$, response$, cap$,0)
62270   else
62300     fnCopy(fn_hh_input_filename$,bookFile$)
62330     fnRename(fn_hh_input_filename$,env$('Q')&"\UBmstr\outofpalm."&ltrm$(bk$)&"."&date$("YYMMDD")&srep$(time$("HHMMSS"),":","")&".txt")
62360   end if
62390 fnend
64000 def fn_ezreader(bookFile$*512)
64030   fn_readings_backup(bookFile$)
64060   open #h_out:=3: "Name="&bookFile$&",RecL=30,replace",display,output
64090   open #2: "Name="&fn_hh_input_filename$&",RecL=578",display,input
64120   do
64150     linput #2: easy$ eof EXREADER_XIT
64180     z$=lpad$(trim$(easy$(209:228)),10)
64210     reading=val(easy$(309:318))
64240     pr #h_out,using "form pos 1,c 10,n 10": z$,reading
64270   loop
64300   EXREADER_XIT: !
64330   close #2: ioerr ignore
64390   close #h_out: ioerr ignore
64450 fnend
68000 def fn_hersey(bookFile$*512)
68030   fn_readings_backup(bookFile$)
68060   open #h_out:=3: "Name="&bookFile$&",RecL=30,replace",display,output
68090   open #2: "Name=" &fn_hh_input_filename$&",RecL=282",display,input
68110   do
68120     linput #2: hersey$ eof HERSEY_EOF
68150     z$=lpad$(trim$(hersey$(1:10)),10)
68180     reading=val(hersey$(229:238))
68210     pr #h_out,using "form pos 1,c 10,n 10": z$,reading
68240   loop
68250   HERSEY_EOF: !
68270   close #2: ioerr ignore
68300   close #h_out: ioerr ignore
68330   !
68360 fnend
70000 def fn_itron(bookFile$*512)
70050   open #h_itron:=fngethandle: "Name="&fn_hh_input_filename$,display,input
70070   open #h_itron_out:=fngethandle: "Name="&bookFile$&",RecL=512,replace",display,output
70120   pr #h_itron_out: '[ACS Hand Held File Generic Version 2]'
70150   z$=''
70180   do
70210     linput #h_itron: line$ eof EO_ITRON
70240     line_type$=line$(1:3)
70270     if line_type$="CUS" then
70300       if z$<>'' then let fn_itron_write ! write the previous one
70330       reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
70360       z$=trim$(line$(15:34))(1:10)
70390     else if line_type$="MTR" then
70420       itron_meter_category$=line$(94:94)
70450     else if line_type$="RDG" then
70480       itron_reading=val(line$(34:43))
70510               !    itron_read_date$=line$(48:55)
70540       itron_meter_chenge_out$=line$(92:92)
70570       if itron_meter_chenge_out$="Y" then meterroll=1 else meterroll=0
70600       if itron_meter_category$="E" then ! Electric
70630         reading_electric=itron_reading
70660         meterroll_electric=meterroll
70690       else if itron_meter_category$="G" then ! Gas
70720         reading_gas=itron_reading
70750         meterroll_gas=meterroll
70780       !    else if itron_meter_category$="I" then ! Irrigation
70810       !    else if itron_meter_category$="S" then ! Steam/sewer
70840       else if itron_meter_category$="W" then ! Water
70870         if env$('client')='Millry' then reading_water=itron_reading*10 else reading_water=itron_reading
70900         meterroll_water=meterroll
70930       end if
70960     else if line_type$="RFF" or line_type$="WRR" then
70990       tmpr$=line$(55:56)
71020       if val(tmpr$)=0 then tmpr$=line$(57:58)
71050     end if
71080   loop
71110    EO_ITRON: !
71140   fn_itron_write ! write the last one
71170   close #h_itron:
71200   close #h_itron_out:
71230 fnend
72000 def fn_itron_write
72030   ! pr #h_itron_out,using "form pos 1,c 10,3*n 10,3*n 1": z$,reading_water,reading_electric,reading_gas,meterroll_water,meterroll_electric,meterroll_gas
72060   if reading_water+reading_electric+reading_gas+meterroll_wate+meterroll_electric+meterroll_gas<>0 then
72090     pr #h_itron_out: 'Customer.Number='&z$
72120     if reading_water<>0 then pr #h_itron_out: 'Reading.Water='&str$(reading_water)
72150     if reading_electric<>0 then pr #h_itron_out: 'Reading.Electric='&str$(reading_electric)
72180     if reading_gas<>0 then pr #h_itron_out: 'Reading.Gas='&str$(reading_gas)
72210     if meterroll_water<>0 then pr #h_itron_out: 'MeterRoll.Water='&str$(meterroll_water)
72240     if meterroll_electric<>0 then pr #h_itron_out: 'MeterRoll.Electric='&str$(meterroll_electric)
72270     if meterroll_gas<>0 then pr #h_itron_out: 'MeterRoll.Gas='&str$(meterroll_gas)
72300     pr #h_itron_out: 'Meter.Tamper='&str$(val(tmpr$))
72302   else
72303     pr #h_itron_out: '! customer number '&z$&' has all zero readings.'
72330   end if
72360 fnend
74000 def fn_laptop(bookFile$*512)
74030   route=val(bk$)
74060   L1420: !
74090   fnTos(sn$="Retrieve")
74120   mat resp$=("")
74150   fnLbl(1,1,"Source Drive:",20,1)
74210   fnTxt(1,23,20,100,0,"",0,"Source drive should be drive designation for the usb drive, including a : and a \ ")
74240   if resp$(1)="" then resp$(1)="F:\"
74270   fnCmdSet(2)
74300   fnAcs(sn$,0,mat resp$,ckey) !
74330   if ckey=5 then goto XIT
74360   source$=resp$(1)
74390   if len(source$)=0 then goto L1420
74420   if len(source$)=1 then source$(2:2)=":"
74450   if source$(3:3)=" " then source$(3:3)="\"
74480   fnCopy(source$&"readings."&str$(route),env$('Q')&"\UBmstr\readings."&str$(route))
74510 fnend
76000 def fn_psion_workabout(bookFile$*512)
76240   if env$('client')="Ash Grove" then
76270     execute 'Sy "'&os_filename$("S:\RCom\RCom.exe")&'" /w'
76300   else if exists("RCom\RComW.exe")<>0 then
76330     execute 'Sy "'&os_filename$("S:\RCom\RComW.exe")&'" /w'
76360   else
76390     execute 'Sy "'&os_filename$("S:\acsUB\PreRoute.bat")&'"'
76400   end if
76420    ! in august 2006 meters.opo changed to send back route as meters.out; before that it came back with the route # on the file name  (readings.1, etc)
76520   fnCopy(fn_hh_input_filename$,bookFile$)
76630 fnend
78000 def fn_sensus_in(bookFile$*512)
78020   open #h_sensus:=fngethandle: "Name="&fn_hh_input_filename$&",RecL=22",external,input
78040   fn_readings_backup(bookFile$)
78060   open #h_readings:=fngethandle: "Name="&bookFile$&",RecL=30,replace",display,output
78080   do
78100     read #h_sensus,using "form pos 1,c 22": ln$ eof SENSUS_IN_XIT ioerr SENSUS_IN_XIT
78120     pr #h_readings,using "form pos 1,c 132": ln$
78140   loop
78160   SENSUS_IN_XIT: !
78180   close #h_sensus: ioerr ignore
78200   close #h_readings: ioerr ignore
78220 fnend
80030 ! <Updateable Region: ERTN>
80060 ERTN: fnerror(program$,err,line,act$,"xit")
80090   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
80120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
80150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
80180 ERTN_EXEC_ACT: execute act$ : goto ERTN
80210 ! /region
82000 def fn_readings_backup(bookFile$*512)
82030   if exists(bookFile$) then
82060     fnCopy(bookFile$,env$('Q')&"\UBmstr\readings_"&bk$&'.bak')
82090   end if  ! exists UBmstr\readings.[bk$]
82120 fnend
84000 def fn_import_l_readings_txt(bookFile$*512; inFileRecordLen)
84020   fn_readings_backup(bookFile$)
84040   open #hReadingsOut:=fngethandle: "Name="&bookFile$&",RecL=30,replace",display,output
84060   ! if inFileRecordLen=0 then inFileRecordLen=129
84070   open #hHandHeld:=fngethandle: "Name="&fn_hh_input_filename$,display,input
84080   do
84100     linput #hHandHeld: a$ eof ilrt_EO_L_READINGS_TXT
84120     if deviceSelected$="Other" and env$('client')="Brier Lake" then
84140       parseResponse=fn_ilrt_lineParse_BrierLake(a$,z$,reading$)
84150     else if deviceSelected$='READy Water' then
84160       ! parseResponse=fn_ilrt_lineParse_READy_Water(a$,z$,reading$)
84170       parseResponse=fn_ilrt_lineParseDelimited(a$,z$,1,reading$,3)
84180     else if deviceSelected$='Master Meter' then
84190       parseResponse=fn_ilrt_lineParseFixedWidth(a$,z$,1,10,reading$,14,14, readingDate$,35,8)
84200     else
84220       pr 'deviceSelected$ ('&deviceSelected$&') is not recognized in the parse import routines.'
84240       pause
84260     end if
84280     if parseResponse then
84300       pr #hReadingsOut,using "form pos 1,c 10,c 9": z$,trim$(reading$)
84320     end if
84340   loop
84360   ilrt_EO_L_READINGS_TXT: !
84380   close #hHandHeld: ioerr ignore
84400   close #hReadingsOut: ioerr ignore
84420 fnend
86000 def fn_ilrt_lineParse_BrierLake(a$*150,&z$,&reading$)
86020   ilpblReturn=0
86040   x=val(a$(1:3)) conv ilpbl_finis
86060   z$=""
86080   for j=1 to 8
86100     x=val(a$(j:j)) conv ilrt_L1060
86120     z$=z$&a$(j:j)
86140   next j
86160   ilrt_L1060: !
86180   z=val(z$)
86200   z$=cnvrt$("pic(zzzzzzz.##",z)
86220   reading$=""
86240   for j1=1 to 20
86260     x=val(a$(j1+j:j1+j)) conv ilrt_L1120
86280     reading$=reading$&a$(j1+j:j1+j)
86300     ilrt_L1120: !
86320   next j1
86340   ilpblReturn=1
86360   ilpbl_finis: !
86380   fn_ilrt_lineParse_BrierLake=ilpblReturn
86400 fnend
88000 def fn_ilrt_lineParse_READy_Water(a$*150,&z$,&reading$)
88020   ilprwReturn=0
88040   z$=reading$=""
88060   str2mat(a$,mat ilprwItem$, chr$(9))
88080   ! ilprwItem$(1)=account number
88100   ! ilprwItem$(2)=meter serial number (from 'U4 Meter Location' table     formerly from meter information file)
88110   ! ilprwItem$(3)=water reading
88120   ! ilprwItem$(4)=reading date
88140   z$=lpad$(ilprwItem$(1),10)
88160   reading$=ilprwItem$(3)
88180   ilprwReturn=1
88200   fn_ilrt_lineParse_READy_Water=ilprwReturn
88220 fnend
89000 def fn_ilrt_lineParseDelimited(a$*512,&key$,item_key,&reading$,item_reading; &readingDate$,item_readingDate)
89010   dim ilpdItem$(0)*512
89020   ilprwReturn=0
89040   z$=reading$=""
89060   str2mat(a$,mat ilpdItem$, chr$(9))
89080   key$=lpad$(ilpdItem$(item_key),10)
89100   reading$=ilpdItem$(item_reading)
89120   if item_readingDate then
89140     readingDate$=ilpdItem$(item_readingDate)
89160   end if
89180   ilprwReturn=1
89200   fn_ilrt_lineParseDelimited=ilprwReturn
89220 fnend
90000 def fn_ilrt_lineParseFixedWidth(line$*512,&key$,pos_key,len_key,&reading$,pos_reading,len_reading; &readingDate$,pos_date,len_date)
90020   ilpfwReturn=0
90040   key$=reading$=readingDate$=''
90140   key$=line$(pos_key:pos_key+len_key-1)
90160   reading$=line$(pos_reading:pos_reading+len_reading-1)
90170   readingDate$=line$(pos_date:pos_date+len_date-1)
90175   ! pr key$,reading$ : pause
90180   ilpfwReturn=1
90200   fn_ilrt_lineParseFixedWidth=ilpfwReturn
90220 fnend
91000 def fn_findFirstMatch(mat ffmItemsToSearch$,ffmCriteria1$*256; ffmCriteria2$*256,ffmCriteria3$*256,ffmCriteria4$*256)
91020   ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria1$)
91040   if ffmReturn<=0 then ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria2$)
91060   if ffmReturn<=0 then ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria3$)
91080   if ffmReturn<=0 then ffmReturn=fnsrch_case_insensitive(mat ffmItemsToSearch$,ffmCriteria4$)
91100   fn_findFirstMatch=ffmReturn
91120 fnend
92000 def fn_okToMerge(bookFile$*512,requiredFormat$*128)
92020   if fn_readingsFileVersion$(bookFile$)<>requiredFormat$ then
92040     mat ml$(2)
92060     ml$(1)='The existing book (number '&bk$&') is not in a format that permits'
92080     ml$(2)='merging with the '&deviceSelected$&' format.'
92090     fnmsgbox(mat ml$)
92100     aclaraWorkOrderReturn=-1
92120     okayToMergeReturn=0
92140   else
92160     okayToMergeReturn=1
92180   end if
92200   fn_okToMerge=okayToMergeReturn
92220 fnend
93000 def fn_mergeBooks(mbFile1$*512,mbFile2$*512)
93010   ! mbFile1$ is the origional file.
93020   ! mbFile2$ is the new file whom's values will override mbFile1$'s in the final product.  This file will also be overwritten with the final product.
93030   dim mbF1Label$(0)*256,  mbF1Value$(0)*256
93040   mat mbF1Label$(0) : mat mbF1Value$(0)
93050   dim mbF2Label$(0)*256,  mbF2Value$(0)*256
93060   mat mbF2Label$(0) : mat mbF2Value$(0)
93070   fnFileTo2Arrays(mbFile1$,mat mbF1Label$,mat mbF1Value$, 1)
93080   fnFileTo2Arrays(mbFile2$,mat mbF2Label$,mat mbF2Value$, 1)
93090   dim mbTmpNewFile$*512
93100   mbTmpNewFile$=env$('temp')&'\acs\mergeTmpNew-session'&session$&'.txt'
93110   fnmakesurepathexists(mbTmpNewFile$)
93120   open #hMergeNew:=fngethandle: 'name='&mbTmpNewFile$&",RecL=512,Replace",d,o
93130   pr #hMergeNew: '[ACS Hand Held File Generic Version 2]'
93140   fn_getCustomerNumbers(mat mbF1Label$,mat mbF1Value$,mat mbF1CustomerNumbers$)
93150   fn_getCustomerNumbers(mat mbF2Label$,mat mbF2Value$,mat mbF2CustomerNumbers$)
93160   dim mbCg1Label$(0)*256,mbCg1Value$(0)*256
93170   dim mbCg2Label$(0)*256,mbCg2Value$(0)*256
93180   for mbX=1 to udim(mat mbF1CustomerNumbers$)
93190     fn_getCustomerGroup(mat mbF1Label$,mat mbF1Value$,mbF1CustomerNumbers$(mbX),mat mbCg1Label$,mat mbCg1Value$)
93200     cg2Count=fn_getCustomerGroup(mat mbF2Label$,mat mbF2Value$,mbF1CustomerNumbers$(mbX),mat mbCg2Label$,mat mbCg2Value$)
93210     pr #hMergeNew: 'Customer.Number='&mbF1CustomerNumbers$(mbX)
93220     if cg2Count then
93230       for mb1x=2 to udim(mbCg1Label$)
93250         if mbCg1Label$(mb1x)<>'' then
93252           mb2Match=srch(mat mbCg2Label$,mbCg1Label$(mb1x))
93260           if mb2Match<=0 or mbCg1Value$(mb1x)=mbCg2Value$(mb2Match) then
93270             pr #hMergeNew: mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
93272             ! pr 'a>>';mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
93274             mbCg1Label$(mb1x)=''
93276             if mb2Match then mbCg2Label$(mb2Match)=''
93280           else  
93282             pr #hMergeNew: mbCg2Label$(mb2Match)&'='&mbCg2Value$(mb2Match)
93284             ! pr 'b>>';mbCg2Label$(mb2Match)&'='&mbCg2Value$(mb2Match)
93286             mbCg2Label$(mb2Match)=''
93290             ! pr 'customer number '&mbF1CustomerNumbers$(mbX)&' has different data in both files for '&mbCg1Label$(mb1x)
93300             ! pr '  file 1: "'&mbCg1Value$(mb1x)&'"'
93310             ! pr '  file 2: "'&mbCg2Value$(mb2Match)&'"'
93320             ! pr '  The value from File 2 will override the value from File 1.'
93330             ! pr '  type GO and press Enter to continue'
93340             ! pause
93360           end if
93362           mbCg1Label$(mb1x)=''
93370         end if
93372         ! pause
93380       nex mb1x
93390       for mb2x=2 to udim(mbCg2Label$)
93400         if trim$(mbCg2Label$(mb2x))<>'' then 
93410           pr #hMergeNew: mbCg2Label$(mb2x)&'='&mbCg2Value$(mb2x)
93412           ! pr 'c>>';mbCg2Label$(mb2x)&'='&mbCg2Value$(mb2x)
93420         end if
93430       nex mb2x
93440       mbF2which=srch(mat mbF2CustomerNumbers$,mbF1CustomerNumbers$(mbX))
93450       if mbF2which>0 then mbF2CustomerNumbers$(mbF2which)=''
93460       mbF1CustomerNumbers$(mbX)=''
93470     else
93480       for mb1x=2 to udim(mbCg1Label$)
93490           pr #hMergeNew: mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
93492           ! pr 'd>>';mbCg1Label$(mb1x)&'='&mbCg1Value$(mb1x)
93500       nex mb1x
93510     end if
93512     ! pause
93520     pr #hMergeNew: ''
93530   nex mbX
93540   for mbX=1 to udim(mat mbF2CustomerNumbers$)
93550     if mbF2CustomerNumbers$(mbX)<>'' then
93560       pr 'adding '&mbF2CustomerNumbers$(mbX)&' from the second file that was not in the first file.'
93570       ! pause
93580       cg2Count=fn_getCustomerGroup(mat mbF2Label$,mat mbF2Value$,mbF2CustomerNumbers$(mbX),mat mbCg2Label$,mat mbCg2Value$)
93590       pr #hMergeNew: 'Customer.Number='&mbF2CustomerNumbers$(mbX)
93600       for mb1x=2 to udim(mat mbCg2Label$)
93610         if trim$(mbCg2Label$(mb1x))<>'' then
93620           pr #hMergeNew: mbCg2Label$(mb1x)&'='&mbCg2Value$(mb1x)
93630         end if
93640       nex mb1x
93650     end if
93660   nex mbX
93670   close #hMergeNew:
93680   fncopy(mbTmpNewFile$,mbFile2$)
93690   ! fndel(mbTmpNewFile$)
93700 fnend

95000 def fn_getCustomerNumbers(mat gcnLabel$,mat gcnValue$,mat gcnCustomerNumbers$)
95020   gcnMatch=0 
95040   mat gcnCustomerNumbers$(0)
95060   do
95080     gcnMatch=srch(mat gcnLabel$,'Customer.Number',gcnMatch+1)
95100     if gcnMatch then 
95120       fnaddonec(mat gcnCustomerNumbers$,trim$(gcnValue$(gcnMatch)),1,1)
95140     end if
95160   loop until gcnMatch<=0
95180 fnend
96000 def fn_getCustomerGroup(mat gcgFromLabel$,mat gcgFromValue$,gcgCustomerNumbers$,mat gcgToLabel$,mat gcgToValue$)
96020   mat gcgToLabel$(0)
96040   mat gcgToValue$(0)
96060   gcgReturn=0
96080   gcgIndex=0
96100   gcgTop: ! 
96120   gcgIndex=srch(mat gcgFromValue$,gcgCustomerNumbers$, gcgIndex+1)
96140   if gcgIndex<=0 then
96160     pr 'could not find gcgCustomerNumbers$="'&gcgCustomerNumbers$&'"' 
96180     ! for x=1 to udim(mat gcgFromValue$) : if gcgFromValue$(x)<>'' then pr x;gcgFromValue$(x) : nex x
96200     ! pause
96220   else
96240     if gcgFromLabel$(gcgIndex)<>'Customer.Number' then goto gcgTop
96260     do
96280       gcgReturn+=1
96300       fnaddonec(mat gcgToLabel$,gcgFromLabel$(gcgIndex))
96320       fnaddonec(mat gcgToValue$,gcgFromValue$(gcgIndex))
96340       gcgIndex+=1
96360     loop until gcgIndex>=udim(mat gcgFromLabel$) or gcgFromLabel$(gcgIndex)='Customer.Number'
96380   end if
96400   fn_getCustomerGroup=gcgReturn
96420 fnend
