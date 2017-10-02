00010 ! ______________________________________________________________________
00020   let fn_setup
00070   let fntop(program$)
00090   dim srvnam$(10)*20,srv$(10)*2
00110 ! 
00130   if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$<>'Aclara' and fnhand_held_device$<>'Master Meter' then 
00140     dim msg_text$(2)*256
00150     let msg_text$(1)='The '&env$('program_caption')&' file is not necessary'
00160     let msg_text$(2)="for your hand held device type."
00170     let fnmsgbox(mat msg_text$, response$,'',64)
00190   end if 
00200   fnget_services(mat srvnam$,mat srv$)
00230 ! 
00240   let fn_hamster_setup
00250   let fn_open_meter
00260   let fnhamster_2("Meter_Hamster",open_file(1))
00270   let fn_close_file
00280   goto XIT
00290 XIT: let fnxit
10000 ! ______________________________________________________________________
10020   def fn_setup
10040     library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fngethandle,fnhand_held_device$,fnget_services
10050     library 'S:\Core\Library': fnhamster_field_reset,fnhamster_field_add,fnhamster_add_combo,fnhamster_add_combof,fnhamster_add_comboa,fnhamster_add_combof,fnhamster_2
10060 ! 
10080     on error goto ERTN
10100 ! 
10180   fnend 
10200   def library fnopen_meter
10220     let fn_setup
10240     let fnopen_meter=fn_open_meter
10260   fnend 
10280 ! 
16000   def fn_open_meter
16020     let fn_open_file
16040     let fn_close_file
16060     let fn_open_meter=fn_open_file
16080   fnend 
18000   def fn_open_file
18020     let open_file_count=0 ! this value is used in the close_file sub routine
18040     mat open_file(1)
18060     open #open_file(open_file_count+=1):=fngethandle: "Name="&env$('Q')&"\UBmstr\Meter.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\Meter_Idx.h"&env$('cno')&",Use,RecL=384,KPs=1/11,KLn=10/2,Shr",internal,outin,keyed 
18080     let fn_open_file=open_file(1)
18100   fnend 
18120   def fn_close_file
18140     for cf_h_item=1 to open_file_count
18160       close #open_file(cf_h_item): 
18180     next cf_h_item
18200   fnend  ! fn_close_file
18220 ! 
20400   def fn_hamster_setup
20410     let mask_pointtwo=32 : let mask_number=30
20420     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
20430     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
20440     let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
20450 ! 
20460     let fnhamster_field_reset
20470 ! 
20480 ! fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
20490     let fnhamster_field_add("Account No",10)
20500     let fnhamster_field_add("Service ID",2)
20510     let fnhamster_field_add("Longitude",17)
20520     let fnhamster_field_add("Latitude",17)
20530     let fnhamster_field_add("Meter Number",12) ! 8 digit number
20540     let fnhamster_field_add("Transmitter Number",20) ! 8 digit number
20550     let fnhamster_field_add("Meter Type",5) ! 8 digit number
20560 ! ** Combo Boxes **
20570 ! fnhamster_add_combof(Field Number,Linked File Name,Key Position,Key Length,Description Position,Description LengthIndex File,limit to list (1=Yes; 0=No))
20580     let fnhamster_add_combof(1,env$('Q')&"\UBmstr\Customer.h"&env$('cno'),1,10,41,30,env$('Q')&"\UBmstr\ubIndex.h"&env$('cno'),1)
20590     c_y=0
20600     for srv_item=1 to 10
20610       if (srv_item=1 and trim$(srv$(srv_item))<>'') or (srvnam$(srv_item)="GAS" or srv$(srv_item)="GA") or srv$(srv_item)='EL' or srvnam$(srv_item)="Lawn Meter" then ! if it is a metered service
20620         mat option$(c_y+=1)
20630         let option$(c_y)=srv$(srv_item)
20640       end if 
20650     next srv_item
20660     let fnhamster_add_comboa(2,mat option$)
20670     let fnhamster_add_combof(7,env$('Q')&"\UBmstr\MeterType.h"&env$('cno'),1,5,6,40,env$('Q')&"\UBmstr\MeterTypeIdx.h"&env$('cno'),1)
20680   fnend 
20700 ! <Updateable Region: ERTN>
20710 ERTN: let fnerror(program$,err,line,act$,"xit")
20720   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20730   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20740   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20750 ERTN_EXEC_ACT: execute act$ : goto ERTN
20760 ! /region
