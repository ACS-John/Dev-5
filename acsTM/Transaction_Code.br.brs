00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster,fnhamster_field_reset,fnhamster_field_add,fnhamster_add_combo,fnhamster_add_combof,fnhamster_add_comboa,fnhamster_add_combof,fnhamster_2,fnhand_held_device$
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00050   dim cap$*128
00060 ! ______________________________________________________________________
00070   fntop(program$,cap$='TM Transaction Code')
00190   fn_hamster_setup
00200   fn_open_file : let fn_close_file : let fn_open_file
00210   fnhamster_2("TransactionCode")
00220   fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     let open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: "Name=S:\Core\Data\TransactionCode.dat,Version=1,KFName=S:\Core\Data\TransactionCode.idx,Use,RecL=41,KPs=1,KLn=1,Shr",internal,outin,keyed 
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
50100     fnhamster_field_reset
50140     fnhamster_field_add("Code",1,'N')
50160     fnhamster_field_add("Description",18)
50240   fnend 
