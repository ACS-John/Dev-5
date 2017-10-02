00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnmsgbox,fnhamster,fnhamster_field_reset,fnhamster_field_add,fnhamster_add_combo,fnhamster_add_combof,fnhamster_add_comboa,fnhamster_add_combof,fnhamster_2,fnhand_held_device$
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00070   let fntop(program$)
00120   if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$<>'Aclara' then 
00130     dim msg_text$(2)*256
00140     let msg_text$(1)='The '&env$('Program Caption')&' file is not necessary'
00150     let msg_text$(2)="for your hand held device type."
00160     let fnmsgbox(mat msg_text$, response$,'',64)
00180   end if 
00190   let fn_hamster_setup
00200   let fn_open_file : let fn_close_file : let fn_open_file
00210   let fnhamster_2("Meter_Type")
00220   let fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     let open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: "Name="&env$('Q')&"\UBmstr\MeterType.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\MeterTypeIdx.h"&env$('cno')&",Use,RecL=128,KPs=1,KLn=5,Shr",internal,outin,keyed 
00280   fnend 
00290   def fn_close_file
00300     for j=1 to open_file_count : close #j: : next j
00310   fnend  ! fn_close_file
00320 XIT: let fnxit
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: let fnerror(program$,err,line,act$,"xit")
00360   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00370   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
50000   def fn_hamster_setup
50020     let mask_pointtwo=32 : let mask_number=30
50040     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
50060     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
50080     let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
50100     let fnhamster_field_reset
50120 !     fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
50140     let fnhamster_field_add("Code",5)
50160     let fnhamster_field_add("Description",40)
50180     let fnhamster_field_add("Reading Multipler",9,'C',0,22) ! 22=unlimited decimals, no commas
50200     let fnhamster_field_add("Number of Dials",2,'N',0,mask_number) ! 30 (mask_number) =no decimals, no commas
50220     let fnhamster_field_add("Read Type",2,'N',0,mask_number)
50240   fnend 
