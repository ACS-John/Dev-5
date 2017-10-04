00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fnhamster,fnhamster_field_reset,fnhamster_field_add,fnhamster_add_combo,fnhamster_add_combof,fnhamster_add_comboa,fnhamster_add_combof,fnhamster_2
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00050   dim cap$*128
00060 ! ______________________________________________________________________
00070   fntop(program$,cap$='GLMaster')
00090   dim srvnam$(10)*20,srv$(10)*2
00190   fn_hamster_setup
00200   fn_open_file : let fn_close_file : let fn_open_file
00210   fnhamster_2("GLmstr")
00220   fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     let open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",Version=0,KFName="&env$('Q')&"\GLmstr\glIndx2.h"&env$('cno')&",Use,RecL=416,KPs=13,KLn=30,Shr",internal,outin,keyed 
00280   fnend 
00290   def fn_close_file
00300     for j=1 to open_file_count : close #j: : next j
00310   fnend  ! fn_close_file
00320 XIT: let fnxit
00330 ! ______________________________________________________________________
00340 ! region
00350 ERTN: let fnerror(program$,err,line,act$,"xit")
00360   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00370   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
00420   def fn_hamster_setup
00430     let mask_pointtwo=32 : let mask_number=30
00440     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
00450     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
00460     let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
00470     fnhamster_field_reset
00480 !     fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
00490     fnhamster_field_add("Account",12)
00500     fnhamster_field_add("Income Stmt Ref",5,"PD",3,mask_number,69)
00501     fnhamster_field_add("Beginning Balance",11,"PD",6,mask_pointtwo,81)
00502     fnhamster_field_add("Current Balance",11,"PD",6,mask_pointtwo,87)
00503     fnhamster_field_add("2-Yr Beginning",11,"PD",6,mask_pointtwo,327)
00520   fnend 
00530 ! 
