05000 ! formerly S:\acsUB\hhfro
05020 ! -- Transfer Data From Hand Held to Computer
05040   library program$: fnretrieve_hand_held_file
05060   fn_setup
05100   fnretrieve_hand_held_file
05120   fnxit
08000   def fn_setup
08020     library 'S:\Core\Library': fnxit,fnureg_read
08040     library 'S:\Core\Library': fntop, fnerror,fntos,fnlbl,fnacs,fntxt,fncmdset,fnmsgbox,fngethandle,fnregistered_for_hh,fnhand_held_device$,fnCopy
08060     on error goto ERTN
08140   fnend 
10000   def library fnretrieve_hand_held_file
10030     if ~setup then let fn_setup
10040     fntop(program$)
10080     dim ml$(2)*256,cap$*128,a$*512,ln$*22,tip$*120
10090     dim resp$(3)*100,amr$*619,hersey$*290,path$*100,easy$*619
10092     dim device$*20
10140     if ~fnregistered_for_hh then 
10150       mat ml$(2)
10160       ml$(1)="You must purchase the ACS Utility Billing Hand Held"
10170       ml$(2)="module to access these features"
10180       fnmsgbox(mat ml$, response$, cap$,64)
10190       goto XIT
10200     end if  ! ~fnregistered_for_hh
10220     device$=fnhand_held_device$ ! fn_ctext_setup
10230 SCREEN1: ! 
10240     fntos(sn$="hh_fro")
10250     fnlbl(2,1,"Hand Held model:",30,1)
10252     fnlbl(2,32,device$)
10260 ! fncomboa("HH-FroCBox",1,44,mat ctext$)
10270 ! resp$(0)=device$
10280     fnlbl(4,1,"Book Number to store readings:",30,1)
10290     fntxt(4,32,2,0,1,"20",0,"Be careful not to use the same route # twice in the same billing cycle.  The first route will be lost if it has not been calculated.")
10380     if device$="Green Tree" or device$="Hersey" or device$="EZReader" then goto L630 else goto L650
10390 L630: ! 
10400     fnlbl(6,1,"Source File:",30,1)
10410     tip$="Source file should be drive designation and file name of the file returned from the Hand Held. "
10420     fntxt(6,32,20,100,0,"",0,tip$)
10430     if resp$(2)="" then resp$(2)=path$
10432     goto L660
10440 L650: ! 
10442     fnlbl(6,1,"Importing from "&fn_hh_input_filename$,len("Importing from "&fn_hh_input_filename$),1)
10444 L660: ! 
10450     fncmdset(2)
10460     fnacs(sn$,0,mat resp$,ckey)
10470     if ckey=5 then goto XIT
10480     bk$=resp$(1)
10490 ! device$=ctext_default$ ! resp$(0)
10500     path$=resp$(2)
10510     if ~exists(path$) then 
10520       mat ml$(1)
10530       ml$(1)="There is no file by this name at "&path$
10540       fnmsgbox(mat ml$, response$)
10550       goto SCREEN1
10560     else 
10570       goto TRANSFER
10580     end if 
32000 TRANSFER: ! Transfer from Hand Held Computer
32030 ! device$=trim$(device$)
32180 ! 
32210     if device$="Sensus" then 
32240       fn_sensus_in
32270     else if device$="LapTop" then 
32300       fn_laptop
32330     else if device$="Hersey" then 
32360       fn_hersey
32390     else if device$="EZReader" then 
32420       fn_ezreader
32450     else if device$="ACS Meter Reader" then 
32480       fn_acsmr
32510     else if device$="AMR" then 
32540       fn_amr
32570     else if device$="Itron FC300" then 
32600       fn_itron
32630     else if device$="Psion Workabout" then 
32660       fn_psion_workabout
32690     else if device$="Badger" or device$="DriveBy" then 
32720       fn_badger
32750     else if device$="Unisys" then 
32780       fn_unisys
32810     else if device$="Boson" then 
32840       fn_boson
32870     else if device$="Green Tree" then 
32900       fn_green_tree
32930     else if device$="Other" and env$('client')="Brier Lake" then 
32960       fn_import_l_readings_txt(bk$)
32970     else if device$="READy Water" then 
32980       fn_import_l_readings_txt(bk$)
32982     else if device$="Master Meter" then 
32984       fn_import_l_readings_txt(bk$, 358)
32990     end if 
33020     goto XIT
33980 XIT: ! 
33990   fnend 
34000   def fn_hh_input_filename$*256
34020 ! requires local variables: device$
34040     dim hif_return$*256
34060     hif_return$=''
34062     fnureg_read('Hand Held From File',hif_return$)
34064     if trim$(hif_return$)='' then 
34080       if device$="Sensus" then 
34100         hif_return$='c:\vol002\amrs\READ.DAT'
34120       else if device$="LapTop" then 
34140         hif_return$="*ask*"
34160       else if device$="Hersey" then 
34180         hif_return$=path$
34200       else if device$="EZReader" then 
34220         hif_return$=path$
34240       else if device$="ACS Meter Reader" then 
34260         hif_return$=path$
34280       else if device$="AMR" then 
34300         hif_return$=path$
34320       else if device$="Itron FC300" then 
34340         if env$('client')='Findlay' then 
34360           hif_return$='\\vof-pc\itronshared\FCS\Export\Output\UPLOAD.DAT'
34380 !         hif_return$= "C:\Itron\FCSShare\Export\Output\upload.dat"
34400         else 
34420           hif_return$="C:\mvrs\xfer\upload\UPLOAD.DAT"
34440         end if 
34480       else if device$="Psion Workabout" then 
34500         hif_return$=env$('Q')&"\UBmstr\Readings.out"
34520       else if device$="Badger" or device$="DriveBy" then 
34540         if env$('client')="Sangamon" then 
34560           hif_return$="c:\Progra~1\connect3\connect.ot3"
34580         else 
34600           hif_return$="c:\connect\connect.ot3"
34620         end if 
34640       else if device$="Unisys" then 
34660         hif_return$=env$('Q')&"\UBmstr\ReadOut.dat"
34680       else if device$="Boson" then 
34700         if env$('client')="Monticello" then 
34720           hif_return$=env$('Q')&"\UBmstr\outpalm.txt"
34740         else 
34760           hif_return$=env$('Q')&"\UBmstr\outofpalm.txt"
34780         end if 
34800       else if device$="Green Tree" then 
34820         hif_return$=path$
34840       else if device$="Other" and env$('client')="Brier Lake" then 
34860         hif_return$="L:\readings.txt"
34880       end if 
34890     end if 
34900     fn_hh_input_filename$=hif_return$
34920   fnend 
35000 IGNORE: continue 
36000   def fn_unisys
36020     fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
36080   fnend 
38000   def fn_green_tree
38030     fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$)) ! only for Gilbertown at this time
38060   fnend 
40000   def fn_boson
40030     if ~exists(fn_hh_input_filename$) then 
40060       mat ml$(2)
40090       ml$(1)='The import file ('&os_filename$(fn_hh_input_filename$)&') could not be found.'
40120       ml$(2)='You may need to perform the Hot Sync and try again.'
40150       fnmsgbox(mat ml$, response$, cap$,0)
40270     else 
40300       fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
40330       execute "Rename "&fn_hh_input_filename$&' '&env$('Q')&"\UBmstr\outofpalm."&ltrm$(bk$)&"."&date$("YYMMDD")&srep$(time$("HHMMSS"),":","")&".txt"
40360     end if 
40390   fnend 
42000   def fn_psion_workabout
42240     if env$('client')="Ash Grove" then 
42270       execute 'Sy "'&os_filename$("S:\RCom\RCom.exe")&'" /w'
42300     else if exists("RCom\RComW.exe")<>0 then 
42330       execute 'Sy "'&os_filename$("S:\RCom\RComW.exe")&'" /w'
42360     else 
42390       execute 'Sy "'&os_filename$("S:\acsUB\PreRoute.bat")&'"'
42400     end if 
42420 ! in august 2006 meters.opo changed to send back route as meters.out; before that it came back with the route # on the file name  (readings.1, etc)
42520     fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
42630   fnend 
43000   def fn_badger ! need to copy badger files into '&env$('Q')&'\UBmstr\readings.x
43040     fnCopy(fn_hh_input_filename$,env$('Q')&"\UBmstr\Readings."&ltrm$(bk$))
43120   fnend 
44000   def fn_sensus_in
44020     open #h_sensus:=fngethandle: "Name="&fn_hh_input_filename$&",RecL=22",external,input 
44040     fn_readings_backup(bk$)
44060     open #h_readings:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output 
44080     do 
44100       read #h_sensus,using "form pos 1,c 22": ln$ eof SENSUS_IN_XIT ioerr SENSUS_IN_XIT
44120       pr #h_readings,using "form pos 1,c 132": ln$
44140     loop 
44160 SENSUS_IN_XIT: ! 
44180     close #h_sensus: ioerr ignore
44200     close #h_readings: ioerr ignore
44220   fnend 
46000   def fn_laptop
46030     route=val(bk$)
46060 L1420: ! 
46090     fntos(sn$="Retrieve")
46120     mat resp$=("")
46150     fnlbl(1,1,"Source Drive:",20,1)
46180     tip$="Source drive should be drive designation for the usb drive, including a : and a \ "
46210     fntxt(1,23,20,100,0,"",0,tip$)
46240     if resp$(1)="" then resp$(1)="F:\"
46270     fncmdset(2)
46300     fnacs(sn$,0,mat resp$,ckey) ! 
46330     if ckey=5 then goto XIT
46360     source$=resp$(1)
46390     if len(source$)=0 then goto L1420
46420     if len(source$)=1 then source$(2:2)=":"
46450     if source$(3:3)=" " then source$(3:3)="\"
46480     fnCopy(source$&"readings."&str$(route),env$('Q')&"\UBmstr\readings."&str$(route))
46510   fnend 
48000   def fn_acsmr ! ACS Meter Reader
48030     source$=resp$(1)
48060     open #2: "Name="&fn_hh_input_filename$&"acs_meter_data.txt,RecL=256",display,input 
48090     fn_readings_backup(bk$)
48120     open #3: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output 
48150     do 
48180       linput #2: amr$ eof ACSMR_XIT
48210       let z$=amr$(1:10)
48240       reading=val(amr$(133:142))
48270       pr #3,using "form pos 1,c 10,n 10": z$,reading
48300     loop 
48330 ACSMR_XIT: ! 
48360   fnend 
50000   def fn_itron
50010     dim line$*128
50050     open #h_itron:=fngethandle: "Name="&fn_hh_input_filename$,display,input 
50070     open #h_itron_out:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=512,replace",display,output 
50120     pr #h_itron_out: '[ACS Hand Held File Generic Version 2]'
50150     let z$=''
50180     do 
50210       linput #h_itron: line$ eof EO_ITRON
50240       line_type$=line$(1:3)
50270       if line_type$="CUS" then 
50300         if z$<>'' then let fn_itron_write ! write the previous one
50330         reading_water=meterroll_water=reading_electric=meterroll_electric=reading_gas=meterroll_gas=0
50360         let z$=trim$(line$(15:34))(1:10)
50390       else if line_type$="MTR" then 
50420         itron_meter_category$=line$(94:94)
50450       else if line_type$="RDG" then 
50480         itron_reading=val(line$(34:43))
50510 !    itron_read_date$=line$(48:55)
50540         itron_meter_chenge_out$=line$(92:92)
50570         if itron_meter_chenge_out$="Y" then meterroll=1 else meterroll=0
50600         if itron_meter_category$="E" then ! Electric
50630           reading_electric=itron_reading
50660           meterroll_electric=meterroll
50690         else if itron_meter_category$="G" then ! Gas
50720           reading_gas=itron_reading
50750           meterroll_gas=meterroll
50780 !    else if itron_meter_category$="I" then ! Irrigation
50810 !    else if itron_meter_category$="S" then ! Steam/sewer
50840         else if itron_meter_category$="W" then ! Water
50870           if env$('client')='Millry' then reading_water=itron_reading*10 else reading_water=itron_reading
50900           meterroll_water=meterroll
50930         end if 
50960       else if line_type$="RFF" or line_type$="WRR" then 
50990         tmpr$=line$(55:56)
51020         if val(tmpr$)=0 then tmpr$=line$(57:58)
51050       end if 
51080     loop 
51110 EO_ITRON: ! 
51140     fn_itron_write ! write the last one
51170     close #h_itron: 
51200     close #h_itron_out: 
51230   fnend 
52000   def fn_itron_write
52030 ! pr #h_itron_out,using "form pos 1,c 10,3*n 10,3*n 1": z$,reading_water,reading_electric,reading_gas,meterroll_water,meterroll_electric,meterroll_gas
52060     if reading_water+reading_electric+reading_gas+meterroll_wate+meterroll_electric+meterroll_gas<>0 then 
52090       pr #h_itron_out: 'Customer.Number='&z$
52120       if reading_water<>0 then pr #h_itron_out: 'Reading.Water='&str$(reading_water)
52150       if reading_electric<>0 then pr #h_itron_out: 'Reading.Electric='&str$(reading_electric)
52180       if reading_gas<>0 then pr #h_itron_out: 'Reading.Gas='&str$(reading_gas)
52210       if meterroll_water<>0 then pr #h_itron_out: 'MeterRoll.Water='&str$(meterroll_water)
52240       if meterroll_electric<>0 then pr #h_itron_out: 'MeterRoll.Electric='&str$(meterroll_electric)
52270       if meterroll_gas<>0 then pr #h_itron_out: 'MeterRoll.Gas='&str$(meterroll_gas)
52300       pr #h_itron_out: 'Meter.Tamper='&str$(val(tmpr$))
52302     else 
52303       pr #h_itron_out: '! customer number '&z$&' has all zero readings.'
52330     end if 
52360   fnend 
54000   def fn_amr
54030     path$="c:\ezreader\upload.dat"
54060     fn_readings_backup(bk$)
54090     open #3: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output 
54120     open #2: "Name="&fn_hh_input_filename$&",RecL=620",display,input 
54150     linput #2: amr$ ioerr AMR_NOTHING_TO_READ ! read header
54160     do 
54180       linput #2: amr$ eof AMR_XIT
54210       let z$=lpad$(trim$(amr$(3:22)),10)
54240       reading=val(amr$(47:56))
54330       pr #3,using "form pos 1,c 10,n 10": z$,reading
54360     loop 
54390     goto AMR_XIT !  AMR_NOTHING_TO_READ
54420 AMR_NOTHING_TO_READ: ! 
54450     mat ml$(1)
54480     ml$(1)="The File ("&path$&") is empty."
54510     fnmsgbox(mat ml$,resp$,cap$,0)
54540     goto AMR_XIT !  AMR_NOTHING_TO_READ
54570 AMR_XIT: ! 
54600     close #2: ioerr ignore
54630     close #3: ioerr ignore
54660 ! 
54690   fnend 
56000   def fn_hersey
56030     fn_readings_backup(bk$)
56060     open #h_out:=3: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output 
56090     open #2: "Name=" &fn_hh_input_filename$&",RecL=282",display,input 
56110     do 
56120       linput #2: hersey$ eof HERSEY_EOF
56150       let z$=lpad$(trim$(hersey$(1:10)),10)
56180       reading=val(hersey$(229:238))
56210       pr #h_out,using "form pos 1,c 10,n 10": z$,reading
56240     loop 
56250 HERSEY_EOF: ! 
56270     close #2: ioerr ignore
56300     close #h_out: ioerr ignore
56330 ! 
56360   fnend 
58000   def fn_ezreader
58030     fn_readings_backup(bk$)
58060     open #h_out:=3: "Name="&env$('Q')&"\UBmstr\Readings."&bk$&",RecL=30,replace",display,output 
58090     open #2: "Name="&fn_hh_input_filename$&",RecL=578",display,input 
58120     do 
58150       linput #2: easy$ eof EXREADER_XIT
58180       let z$=lpad$(trim$(easy$(209:228)),10)
58210       reading=val(easy$(309:318))
58240       pr #h_out,using "form pos 1,c 10,n 10": z$,reading
58270     loop 
58300 EXREADER_XIT: ! 
58330     close #2: ioerr ignore
58390     close #h_out: ioerr ignore
58450   fnend 
60030 ! <Updateable Region: ERTN>
60060 ERTN: fnerror(program$,err,line,act$,"xit")
60090   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
60120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
60150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
60180 ERTN_EXEC_ACT: execute act$ : goto ERTN
60210 ! /region
62000   def fn_readings_backup(bk$)
62030     if exists(env$('Q')&"\UBmstr\readings."&bk$) then 
62060       fnCopy(env$('Q')&"\UBmstr\readings."&bk$,env$('Q')&"\UBmstr\readings_"&bk$&'.bak')
62090     end if  ! exists UBmstr\readings.[bk$]
62120   fnend  ! fn_readings_backup
64000 def fn_import_l_readings_txt(bk$; inFileRecordLen)
64020   fn_readings_backup(bk$)
64040   open #hReadingsOut:=fngethandle: "Name="&env$('Q')&"\UBmstr\readings."&bk$&",RecL=30,replace",display,output 
64060   ! if inFileRecordLen=0 then inFileRecordLen=129
64060   open #hHandHeld:=fngethandle: "Name="&fn_hh_input_filename$,display,input 
64080   do 
64100     linput #hHandHeld: a$ eof ilrt_EO_L_READINGS_TXT
64120     if device$="Other" and env$('client')="Brier Lake" then
64140       parseResponse=fn_ilrt_lineParse_BrierLake(a$,z$,reading$)
64150     else if device$='READy Water' then
64160       ! parseResponse=fn_ilrt_lineParse_READy_Water(a$,z$,reading$)
64170       parseResponse=fn_ilrt_lineParseDelimited(a$,z$,1,reading$,3)
64180     else if device$='Master Meter' then
64190       parseResponse=fn_ilrt_lineParseFixedWidth(a$,z$,1,10,reading$,14,14, readingDate$,35,8)
64200     else 
64220       pr 'Device$ ('&device$&') is not recognized in the parse import routines.'
64240       pause
64260     end if
64280     if parseResponse then
64300       pr #hReadingsOut,using "form pos 1,c 10,c 9": z$,trim$(reading$)
64320     end if
64340   loop
64360   ilrt_EO_L_READINGS_TXT: ! 
64380   close #hHandHeld: ioerr ignore
64400   close #hReadingsOut: ioerr ignore
64420 fnend 
66000 def fn_ilrt_lineParse_BrierLake(a$*150,&z$,&reading$)
66020   ilpblReturn=0
66040   let x=val(a$(1:3)) conv ilpbl_finis
66060   let z$=""
66080   for j=1 to 8
66100     let x=val(a$(j:j)) conv ilrt_L1060
66120     let z$=z$&a$(j:j)
66140   next j
66160   ilrt_L1060: ! 
66180   let z=val(z$)
66200   let z$=cnvrt$("pic(zzzzzzz.##",z)
66220   reading$=""
66240   for j1=1 to 20
66260     let x=val(a$(j1+j:j1+j)) conv ilrt_L1120
66280     reading$=reading$&a$(j1+j:j1+j)
66300     ilrt_L1120: ! 
66320   next j1
66340   ilpblReturn=1
66360   ilpbl_finis: !
66380   fn_ilrt_lineParse_BrierLake=ilpblReturn
66400 fnend
68000 def fn_ilrt_lineParse_READy_Water(a$*150,&z$,&reading$)
68020   ilprwReturn=0
68040   let z$=reading$=""
68060   str2mat(a$,mat ilprwItem$, chr$(9))
68080   ! ilprwItem$(1)=account number
68100   ! ilprwItem$(2)=meter serial number (from meter information file)
68110   ! ilprwItem$(3)=water reading
68120   ! ilprwItem$(4)=reading date
68140   z$=lpad$(ilprwItem$(1),10)
68160   reading$=ilprwItem$(3)
68180   ilprwReturn=1
68200   fn_ilrt_lineParse_READy_Water=ilprwReturn
68220 fnend
69000 def fn_ilrt_lineParseDelimited(a$*512,&key$,item_key,&reading$,item_reading; &readingDate$,item_readingDate)
69010   dim ilpdItem$(0)*512
69020   ilprwReturn=0
69040   let z$=reading$=""
69060   str2mat(a$,mat ilpdItem$, chr$(9))
69080   key$=lpad$(ilpdItem$(item_key),10)
69100   reading$=ilpdItem$(item_reading)
69120   if item_readingDate then
69140     readingDate$=ilpdItem$(item_readingDate)
69160   end if
69180   ilprwReturn=1
69200   fn_ilrt_lineParseDelimited=ilprwReturn
69220 fnend
70000 def fn_ilrt_lineParseFixedWidth(line$*512,&key$,pos_key,len_key,&reading$,pos_reading,len_reading; &readingDate$,pos_date,len_date)
70020   ilpfwReturn=0
70040   key$=reading$=readingDate$=''
70140   key$=line$(pos_key:pos_key+len_key-1)
70160   reading$=line$(pos_reading:pos_reading+len_reading-1)
70170   readingDate$=line$(pos_date:pos_date+len_date-1)
70175   ! pr key$,reading$ : pause
70180   ilpfwReturn=1
70200   fn_ilrt_lineParseFixedWidth=ilpfwReturn
70220 fnend
