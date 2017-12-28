00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnmsgbox,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnH2AddComboF,fnHamster2,fnhand_held_device$
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00070   fntop(program$)
00120   if fnhand_held_device$<>'Itron FC300' and fnhand_held_device$(1:6)<>'[Ask]' and fnhand_held_device$(1:6)<>'Aclara' then 
00130     dim msg_text$(2)*256
00140     msg_text$(1)='The '&env$('Program Caption')&' file is not necessary'
00150     msg_text$(2)="for your hand held device type."
00160     fnmsgbox(mat msg_text$, response$,'',64)
00180   end if 
00190   fn_hamster_setup
00200   fn_open_file : fn_close_file : fn_open_file
00210   fnHamster2("Meter_Type")
00220   fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: "Name="&env$('Q')&"\UBmstr\MeterType.h"&env$('cno')&",Version=1,KFName="&env$('Q')&"\UBmstr\MeterTypeIdx.h"&env$('cno')&",Use,RecL=128,KPs=1,KLn=5,Shr",internal,outIn,keyed 
00280   fnend 
00290   def fn_close_file
00300     for j=1 to open_file_count : close #j: : next j
00310   fnend  ! fn_close_file
00320 XIT: fnxit
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: fnerror(program$,err,line,act$,"xit")
00360   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00370   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
50000   def fn_hamster_setup
50020     mask_pointtwo=32 : mask_number=30
50040     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
50060     textlen_mmddyy=8 : textlen_ccyymmdd=10
50080     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
50100     fnH2Init
50120 !     fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
50140     fnH2AddText("Code",5)
50160     fnH2AddText("Description",40)
50180     fnH2AddText("Reading Multipler",9,'C',0,22) ! 22=unlimited decimals, no commas
50200     fnH2AddText("Number of Dials",2,'N',0,mask_number) ! 30 (mask_number) =no decimals, no commas
50220     fnH2AddText("Read Type",2,'N',0,mask_number)
50240   fnend 
