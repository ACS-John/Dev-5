12000 ! formerly S:\acsUB\hhfro
12020 ! -- Transfer Data From Hand Held to Computer
12040 library program$: fnretrieve_hand_held_file
12060 fn_setup
12080   if ~fnregistered_for_hh then
12100     mat ml$(2)
12120     ml$(1)="You must purchase the ACS Utility Billing Hand Held"
12140     ml$(2)="module to access these features"
12160     fnmsgbox(mat ml$, response$, cap$,64)
12180     goto XIT
12200   end if  ! ~fnregistered_for_hh
12220 fnretrieve_hand_held_file
12240 fnxit
16000 def fn_setup
16020   library 'S:\Core\Library': fnxit,fnureg_read
16040   library 'S:\Core\Library': fntop,fnerror,fngethandle
16060   library 'S:\Core\Library': fnregistered_for_hh,fnhand_held_Device$,fnHand_Held_Device_list
16080   library 'S:\Core\Library': fntos,fnlbl,fnacs,fntxt,fncmdset,fnchk,fncomboa
16100   library 'S:\Core\Library': fnmsgbox
16120   library 'S:\Core\Library': fnureg_write
16140   library 'S:\Core\Library': fnCopy,fnRename
16150   library 'S:\Core\Library': fnaddonec
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
16360   dim resp$(32)*128
16380   dim amr$*619
16400   dim hersey$*290
16420   dim askPath$*128
16440   dim easy$*619
16460   dim devicePreference$*20
16480   dim deviceSelected$*20
16500   devicePreference$=fnhand_held_Device$ ! fn_ctext_setup
16520   if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
16540     fnureg_read('Hand Held From File Asked',askPath$)
16560   end if
16580   if lwrc$(devicePreference$)='[ask]' then
16600     fnureg_read('Hand Held Device Asked',deviceSelected$)
16620     dim deviceOption$(0)*20
16640     fnHand_Held_Device_list(mat deviceOption$)
16660   end if
16680 fnend
24000 def library fnretrieve_hand_held_file
24020   if ~setup then let fn_setup
24040   fntop(program$)
25000   SCREEN1: ! r:
25020     respc=0 : lc=0
25040     fntos(sn$="hh_fro")
25060     lc+=1
25080     fnlbl(lc+=1,1,"Book Number to store readings:",30,1)
25100     fntxt(lc,32,2,0,1,"20",0,"Be careful not to use the same route # twice in the same billing cycle.  The first route will be lost if it has not been calculated.")
25120     resp$(rc_book:=respc+=1)=''
25140     lc+=1
25160     fnlbl(lc+=1,1,"Hand Held model:",30,1)
25180     if lwrc$(devicePreference$)='[ask]' then
25200       fncomboa("HH-FroCBox",lc,32,mat deviceOption$)
25220       resp$(rc_Device:=respc+=1)=deviceSelected$
25240     else
25260       fnlbl(lc,32,devicePreference$)
25280     end if
25300       lc+=1
25320     if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
25340       lc+=1
25360       fnlbl(lc+=1,1,"Source File:",30,1)
25380       fntxt(lc,32,20,100,0,"70",0,'Source file should be drive designation and file name of the file returned from the Hand Held.')
25400       if resp$(rc_path:=respc+=1)="" then resp$(2)=askPath$
25420     else
25440       fnlbl(lc+=1,1,"Importing from "&fn_hh_input_filename$,len("Importing from "&fn_hh_input_filename$),1)
25460     end if
25480     lc+=1
25500     fnchk(lc+=1,33,'Merge into book', 1,0,0,0) ! requires a format that utilizes [ACS Hand Held File Generic Version 2]
25520     fnlbl(lc,35,'(only check if directed to by ACS Support)') ! requires a format that utilizes [ACS Hand Held File Generic Version 2]
25540     resp$(rc_merge:=respc+=1)='False'
25560     fncmdset(2)
26000     fnacs(sn$,0,mat resp$,ckey)
26020     if ckey<>5 then
26040       bk$=resp$(rc_book)
26060       if lwrc$(devicePreference$)='[ask]' then
26080         deviceSelected$=resp$(rc_Device)
26100         fnureg_write('Hand Held Device Asked',deviceSelected$)
26120       else
26140         deviceSelected$=devicePreference$
26160       end if
26180       enableMerge$=resp$(rc_merge)
26200       if lwrc$(preferenceHandHeldFromFile$)='[ask]' then
26220         askPath$=resp$(rc_path)
26240         fnureg_write('Hand Held From File Asked',askPath$)
26260         if ~exists(askPath$) then
26280           mat ml$(1)
26300           ml$(1)="There is no file by this name at "&askPath$
26320           fnmsgbox(mat ml$, response$)
26330           goto SCREEN1
26340         end if
26350       end if
26360       if fn_transfer(bk$,enableMerge$,askPath$)=-1 then goto SCREEN1
26380     end if
26400   goto XIT ! /r
26700   XIT: ! target of ERTN exits
26720 fnend
28000 def fn_transfer(bk$,enableMerge$,askPath$*128)
28020   transferReturn=0
28040   if deviceSelected$="Aclara Work Order" then
28060     fn_aclaraWorkOrder
28080     transferReturn=fn_aclaraWorkOrder
28100   else if deviceSelected$="Sensus" then
28120     fn_sensus_in
28140   else if deviceSelected$="LapTop" then
28160     fn_laptop
28180   else if deviceSelected$="Hersey" then
28200     fn_hersey
28220   else if deviceSelected$="EZReader" then
28240     fn_ezreader
28260   else if deviceSelected$="ACS Meter Reader" then
28280     fn_acsmr
28300   else if deviceSelected$="AMR" then
28320     fn_amr
28340   else if deviceSelected$="Itron FC300" then
28360     fn_itron
28380   else if deviceSelected$="Psion Workabout" then
28400     fn_psion_workabout
28420   else if deviceSelected$="Badger" or deviceSelected$="DriveBy" then
28440     fn_badger
28460   else if deviceSelected$="Unisys" then
28480     fn_unisys
28500   else if deviceSelected$="Boson" then
28520     fn_boson
28540   else if deviceSelected$="Green Tree" then
28560     fn_green_tree
28580   else if deviceSelected$="Other" and env$('client')="Brier Lake" then
28600     fn_import_l_readings_txt(bk$)
28620   else if deviceSelected$="READy Water" then
28640     fn_import_l_readings_txt(bk$)
28660   else if deviceSelected$="Master Meter" then
28680     fn_import_l_readings_txt(bk$, 358)
28700   end if
28720   if transferReturn>0 then
28740     mat ml$(1)
28760     ml$(1)=str$(transferReturn)&' records imported to book '&bk$&'.'
28780     fnmsgbox(mat ml$)
28800   end if
28820   fn_transfer=transferReturn
28840 fnend
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
30340       if env$('client')='Findlay' then
30360         hif_return$='\\vof-pc\itronshared\FCS\Export\Output\UPLOAD.DAT' ! "C:\Itron\FCSShare\Export\Output\upload.dat"
30400       else
30420         hif_return$="C:\mvrs\xfer\upload\UPLOAD.DAT"
30440       end if
30480     else if deviceSelected$="Psion Workabout" then
30500       hif_return$=env$('Q')&"\UBmstr\Readings.out"
30520     else if deviceSelected$="Badger" or deviceSelected$="DriveBy" then
30540       if env$('client')="Sangamon" then
30560         hif_return$="c:\Progra~1\connect3\connect.ot3"
30580       else
30600         hif_return$="c:\connect\connect.ot3"
30620       end if
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
30900   fn_hh_input_filename$=env$('at')&hif_return$
30920 fnend
31000 IGNORE: continue
32000 def fn_biteN(biteSize,&candyBar$)
32020   fn_biteN=val(fn_bite$(biteSize,candyBar$))
32040 fnend
32060 def fn_bite$*256(biteSize,&candyBar$)
32080   dim biteReturn$*256
32100   biteReturn$=candyBar$(1:biteSize)
32120   candyBar$(1:biteSize)=''
32140   fn_bite$=biteReturn$
32160 fnend
33000 def fn_readingsFileVersion$
33020   dim rfvLine$*512
33040   open #hRfv:=fngethandle: "Name="&fn_hh_input_filename$,display,input
33060   linput #hRfv: rfvLine$
33080   close #hRfv:
33100   if rtrm$(rfvLine$)='[ACS Hand Held File Generic Version 2]' then
33120     rfvReturn$='[ACS Hand Held File Generic Version 2]'
33140   else
33160     rfvReturn$='legacy'
33180     if env$('acsDeveloper')<>'' then pause
33200   end if
33220 fnend

46000 def fn_acsmr ! ACS Meter Reader
46030   source$=resp$(1)
46060   open #2: "Name="&fn_hh_input_filename$&"acs_meter_data.txt,RecL=256",display,input
46090   fn_readings_backup(bk$)
46120   open #3: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output
46150   do
46180     linput #2: amr$ eof ACSMR_XIT
46210     z$=amr$(1:10)
46240     reading=val(amr$(133:142))
46270     pr #3,using "form pos 1,c 10,n 10": z$,reading
46300   loop
46330   ACSMR_XIT: !
46360 fnend
48000 def fn_aclaraWorkOrder
48020   dataIncludesHeaders=1
48060   if enableMerge$<>'' then
48080     if fn_readingsFileVersion$<>'[ACS Hand Held File Generic Version 2]' then
48100        mat ml$(3)
48120        ml$(1)='The existing book '&bk$&' is not in a format that permits'
48140        ml$(2)='merging with the '&deviceSelected$&' format.'
48160        aclaraWorkOrderReturn=-1
48180      end if
48200   end if
48210   open #hIn:=fngethandle: "Name="&fn_hh_input_filename$,display,input
48220   open #hOut:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=512,replace",display,output
48240   pr #hOut: '[ACS Hand Held File Generic Version 2]'
48260   if dataIncludesHeaders then
48280     linput #hIn: line$ eof EO_AW ! just consume the headers
48300   end if
48320   do
48340     z$=''
48360     linput #hIn: line$ eof EO_AW
48380     fn_awoParseLine(line$,mat awoDataName$,mat awoDataValue$)
48400     pause
48420     !
48440     fn_aclaraWorkOrder_write ! write the last one
48460     aclaraWorkOrderReturn+=1
48480   loop
48500   EO_AW: !
48520   close #hIn:
48540   close #hOut:
48560   fn_aclaraWorkOrder=aclaraWorkOrderReturn
48580 fnend
50000 def fn_awoParseLine(line$*256,mat awoDataName$,mat awoDataValue$)
50020     reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
50040     str2mat(line$,mat lineItem$)
50060     mat awoDataName$(0)
50080     mat awoDataValue$(0)
50100     fn_addAwoData('Customer.Number'                        ,lineItem$(2) )
50120     fn_addAwoData('Location ID'                            ,lineItem$(1) )
50140     fn_addAwoData('Meter.Transmitter Number'               ,lineItem$(10))
50160     fn_addAwoData('Meter.Service1.ReadingBeforeSwap'       ,lineItem$(12))
50180     fn_addAwoData('Meter.Meter Number'                     ,lineItem$(13))
50200     fn_addAwoData('Meter.Service1.NewMeterReadingAfterSwap',lineItem$(14))
50220     fn_addAwoData('Meter.Longitude'                        ,lineItem$(21))
50240     fn_addAwoData('Meter.Latitude'                         ,lineItem$(22))
50260     fn_addAwoData('Meter.Service ID'                       ,lineItem$(2) )
50280     fn_addAwoData('Meter.Meter Type'                       ,lineItem$(2) )
50300 fnend
52000 def fn_addAwoData(name$*128,value$*128)
52020   fnaddonec(mat awoDataName$,name$)
52040   fnaddonec(mat awoDataValue$,value$)
52060 fnend
54000 def fn_aclaraWorkOrder_write
54020   pr 'this write needs a lot of work' : pause
54040   if reading_water+reading_electric+reading_gas+meterroll_wate+meterroll_electric+meterroll_gas<>0 then
54060     pr #hOut: 'Customer.Number='&z$
54080     if reading_water<>0 then pr #hOut: 'Reading.Water='&str$(reading_water)
54100     if reading_electric<>0 then pr #hOut: 'Reading.Electric='&str$(reading_electric)
54120     if reading_gas<>0 then pr #hOut: 'Reading.Gas='&str$(reading_gas)
54140     if meterroll_water<>0 then pr #hOut: 'MeterRoll.Water='&str$(meterroll_water)
54160     if meterroll_electric<>0 then pr #hOut: 'MeterRoll.Electric='&str$(meterroll_electric)
54180     if meterroll_gas<>0 then pr #hOut: 'MeterRoll.Gas='&str$(meterroll_gas)
54200     pr #hOut: 'Meter.Tamper='&str$(val(tmpr$))
54220   else
54240     pr #hOut: '! customer number '&z$&' has all zero readings.'
54260   end if
54280 fnend
56000   def fn_amr
56030     askPath$="c:\ezreader\upload.dat"
56060     fn_readings_backup(bk$)
56090     open #3: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output
56120     open #2: "Name="&fn_hh_input_filename$&",RecL=620",display,input
56150     linput #2: amr$ ioerr AMR_NOTHING_TO_READ ! read header
56160     do
56180       linput #2: amr$ eof AMR_XIT
56210       z$=lpad$(trim$(amr$(3:22)),10)
56240       reading=val(amr$(47:56))
56330       pr #3,using "form pos 1,c 10,n 10": z$,reading
56360     loop
56390     goto AMR_XIT !  AMR_NOTHING_TO_READ
56420 AMR_NOTHING_TO_READ: !
56450     mat ml$(1)
56480     ml$(1)="The File ("&askPath$&") is empty."
56510     fnmsgbox(mat ml$,resp$,cap$,0)
56540     goto AMR_XIT !  AMR_NOTHING_TO_READ
56570 AMR_XIT: !
56600     close #2: ioerr ignore
56630     close #3: ioerr ignore
56660 !
56690   fnend
58000   def fn_unisys
58020     fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
58080   fnend
60000   def fn_badger ! need to copy badger files into '&env$('Q')&'\UBmstr\readings.x
60040     fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
60120   fnend
62000   def fn_boson
62030     if ~exists(fn_hh_input_filename$) then
62060       mat ml$(2)
62090       ml$(1)='The import file ('&os_filename$(fn_hh_input_filename$)&') could not be found.'
62120       ml$(2)='You may need to perform the Hot Sync and try again.'
62150       fnmsgbox(mat ml$, response$, cap$,0)
62270     else
62300       fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
62330       fnRename(fn_hh_input_filename$,env$('Q')&"\UBmstr\outofpalm."&ltrm$(bk$)&"."&date$("YYMMDD")&srep$(time$("HHMMSS"),":","")&".txt")
62360     end if
62390   fnend
64000 def fn_ezreader
64030   fn_readings_backup(bk$)
64060   open #h_out:=3: "Name="&env$('Q')&"\UBmstr\Readings."&bk$&",RecL=30,replace",display,output
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
66000 def fn_green_tree
66030   fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$)) ! only for Gilbertown at this time
66060 fnend
68000 def fn_hersey
68030   fn_readings_backup(bk$)
68060   open #h_out:=3: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output
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
70000 def fn_itron
70010   dim line$*128
70050   open #h_itron:=fngethandle: "Name="&fn_hh_input_filename$,display,input
70070   open #h_itron_out:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=512,replace",display,output
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
74000 def fn_laptop
74030   route=val(bk$)
74060   L1420: !
74090   fntos(sn$="Retrieve")
74120   mat resp$=("")
74150   fnlbl(1,1,"Source Drive:",20,1)
74210   fntxt(1,23,20,100,0,"",0,"Source drive should be drive designation for the usb drive, including a : and a \ ")
74240   if resp$(1)="" then resp$(1)="F:\"
74270   fncmdset(2)
74300   fnacs(sn$,0,mat resp$,ckey) !
74330   if ckey=5 then goto XIT
74360   source$=resp$(1)
74390   if len(source$)=0 then goto L1420
74420   if len(source$)=1 then source$(2:2)=":"
74450   if source$(3:3)=" " then source$(3:3)="\"
74480   fnCopy(source$&"readings."&str$(route),env$('Q')&"\UBmstr\readings."&str$(route))
74510 fnend
76000 def fn_psion_workabout
76240   if env$('client')="Ash Grove" then
76270     execute 'Sy "'&os_filename$("S:\RCom\RCom.exe")&'" /w'
76300   else if exists("RCom\RComW.exe")<>0 then
76330     execute 'Sy "'&os_filename$("S:\RCom\RComW.exe")&'" /w'
76360   else
76390     execute 'Sy "'&os_filename$("S:\acsUB\PreRoute.bat")&'"'
76400   end if
76420    ! in august 2006 meters.opo changed to send back route as meters.out; before that it came back with the route # on the file name  (readings.1, etc)
76520   fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
76630 fnend
78000 def fn_sensus_in
78020   open #h_sensus:=fngethandle: "Name="&fn_hh_input_filename$&",RecL=22",external,input
78040   fn_readings_backup(bk$)
78060   open #h_readings:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output
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
82000 def fn_readings_backup(bk$)
82030   if exists(env$('Q')&"\UBmstr\readings."&bk$) then
82060     fnCopy(env$('Q')&"\UBmstr\readings."&bk$,env$('Q')&"\UBmstr\readings_"&bk$&'.bak')
82090   end if  ! exists UBmstr\readings.[bk$]
82120 fnend  ! fn_readings_backup
84000 def fn_import_l_readings_txt(bk$; inFileRecordLen)
84020   fn_readings_backup(bk$)
84040   open #hReadingsOut:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output
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
88100   ! ilprwItem$(2)=meter serial number (from meter information file)
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
