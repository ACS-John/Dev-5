00020   fn_setup
00070   fntop(program$)
00090   dim srvnam$(10)*20,srv$(10)*2
00110 ! 
00130   if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$(1:6)<>'[Ask]' and fnhand_held_device$(1:6)<>'Aclara' and fnhand_held_device$<>'Master Meter' then 
00140     dim msg_text$(2)*256
00150     msg_text$(1)='The '&env$('program_caption')&' file is not necessary'
00160     msg_text$(2)="for your hand held device type."
00170     fnmsgbox(mat msg_text$, response$,'',64)
00190   end if 
00200   fnget_services(mat srvnam$,mat srv$)
00230 ! 
00240   fn_hamster_setup
00241   ! if env$('acsdeveloper')<>'' then 
00242   !   exec 'free "'&env$('Q')&"\UBmstr\Meter.h"&env$('cno')&'"'
00243   !   exec 'free "'&env$('Q')&"\UBmstr\Meter_Idx.h"&env$('cno')&'"'
00244   !   fn_open_meter
00245   !   fn_CsvToMeterInformation 
00246   !   stop
00247   ! end if
00250   fn_open_meter
00260   fnhamster_2("Meter_Hamster",open_file(1))
00270   fn_close_file
00280   goto XIT
00290 XIT: fnxit
10000 ! ______________________________________________________________________
12000 def fn_setup
12020   if ~setup then
12040     setup=1
12060     library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fngethandle,fnhand_held_device$,fnget_services
12080     library 'S:\Core\Library': fnhamster_field_reset,fnhamster_field_add,fnhamster_add_comboa,fnhamster_add_combof,fnhamster_2
12100     on error goto ERTN
12120   end if
12140 fnend 
14000 def library fnopen_meter
14020   fn_setup
14040   fnopen_meter=fn_open_meter
14060 fnend 
14080 ! 
16000 def fn_open_meter
16020   fn_open_file
16040   fn_close_file
16060   fn_open_meter=fn_open_file
16080 fnend 
18000 def fn_open_file
18020   open_file_count=0 ! this value is used in the close_file sub routine
18040   mat open_file(1)
18060   open #open_file(open_file_count+=1):=fngethandle: "Name="&env$('Q')&"\UBmstr\Meter.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\Meter_Idx.h"&env$('cno')&",Use,RecL=384,KPs=1/11,KLn=10/2,Shr",internal,outin,keyed 
18080   fn_open_file=open_file(1)
18100 fnend 
18120 def fn_close_file
18140   for cf_h_item=1 to open_file_count
18160     close #open_file(cf_h_item): 
18180   next cf_h_item
18200 fnend  ! fn_close_file
18220     ! 
20400 def fn_hamster_setup
20410   mask_pointtwo=32 : mask_number=30
20420   mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
20430   textlen_mmddyy=8 : textlen_ccyymmdd=10
20440   storage_len_mmddyy=6 : storage_len_ccyymmdd=8
20450   ! 
20460   fnhamster_field_reset
20470   ! 
20480   ! fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
20490   fnhamster_field_add("Account No",10)
20500   fnhamster_field_add("Service ID",2)
20510   fnhamster_field_add("Longitude",17)
20520   fnhamster_field_add("Latitude",17)
20530   fnhamster_field_add("Meter Number",12) ! 8 digit number
20540   fnhamster_field_add("Transmitter Number",20) ! 8 digit number
20550   fnhamster_field_add("Meter Type",5) ! 8 digit number
20560   ! ** Combo Boxes **
20570   ! fnhamster_add_combof(Field Number,Linked File Name,Key Position,Key Length,Description Position,Description LengthIndex File,limit to list (1=Yes; 0=No))
20580   fnhamster_add_combof(1,env$('Q')&"\UBmstr\Customer.h"&env$('cno'),1,10,41,30,env$('Q')&"\UBmstr\ubIndex.h"&env$('cno'),1)
20590   c_y=0
20600   for srv_item=1 to 10
20610     if (srv_item=1 and trim$(srv$(srv_item))<>'') or (srvnam$(srv_item)="GAS" or srv$(srv_item)="GA") or srv$(srv_item)='EL' or srvnam$(srv_item)="Lawn Meter" then ! if it is a metered service
20620       mat option$(c_y+=1)
20630       option$(c_y)=srv$(srv_item)
20640     end if 
20650   next srv_item
20660   fnhamster_add_comboa(2,mat option$)
20670   fnhamster_add_combof(7,env$('Q')&"\UBmstr\MeterType.h"&env$('cno'),1,5,6,40,env$('Q')&"\UBmstr\MeterTypeIdx.h"&env$('cno'),1)
20680 fnend 
20700 ! <Updateable Region: ERTN>
20710 ERTN: fnerror(program$,err,line,act$,"xit")
20720   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20730   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20740   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20750 ERTN_EXEC_ACT: execute act$ : goto ERTN
20760 ! /region

28000   ! 101075.03  should be 101070.06
28020   ! 200271.00  should be 200272.00

32000 def fn_CsvToMeterInformation
32020   colP=16
32040   dim line$*1024,item$(0)*256
32060   open #hIn:=fngethandle: 'name='&env$('at')&'C:\ACS\(Client_Files)\Purdy\Purdy MO Installation List 20171015 Final All.txt',d,input
32080   open #hCustomer:=fngethandle: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed
32100   linput #hIn: line$ eof CtmiEof ! consume headers
32120   do
32140     linput #hIn: line$ eof CtmiEof
32160     str2mat(line$,mat item$, chr$(9))
32180     fn_isKeyValid(item$(1),hCustomer) ! pr item$(1),item$(colP),fn_isKeyValid(item$(1),hCustomer)
32200     if item$(colP)='1 inch' then 
32220       MeterType=1
32240     else if item$(colP)='2 inch T-10' then 
32260       MeterType=21
32280     else if item$(colP)='1.5 inch' then 
32300       MeterType=15
32320     else if item$(colP)='2 inch Turbine' then 
32340       MeterType=2
32360     else if item$(colP)='3 inch' then 
32380       MeterType=3
32400     else if item$(colP)='4 inch' then 
32420       MeterType=4
32440     else if item$(colP)='6 inch' then 
32460       MeterType=6
32480     else
32500       if item$(colP)<>'5/8x3/4' then pr item$(colP) : pause
32520       MeterType=5
32540     end if
32550     write #open_file(1),using 'form pos 1,c 10,c 2,C 17,c 17,c 12,c 20,n 5': trim$(item$(1)),'WA','','','','',MeterType
32560   loop
32580   CtmiEof: !
32600 fnend
34000 def fn_isKeyValid(&key$,hFile)
34020   ikvReturn=0
34040   key$=lpad$(trim$(key$),kln(hFile))
34060   read #hFile,key=key$: nokey IkvTryRpad
34080   ikvReturn=1
34100   goto IkvFinis
34120   IkvTryRpad: !
34140     key$=rpad$(trim$(key$),kln(hFile))
34160     read #hFile,key=key$: nokey IkvNoKey
34180     ikvReturn=2
34200   goto IkvFinis
34220   IkvNoKey: !
34240     ikvReturn=0 : pr 'key failed: '&key$ ! pause
34260   goto IkvFinis
34280   IkvFinis: !
34300   fn_isKeyValid=ikvReturn
34320 fnend
      
