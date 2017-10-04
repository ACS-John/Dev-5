00100 ! ______________________________________________________________________
00200 ! library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster
00220   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster,fnhamster_field_reset,fnhamster_field_add,fnhamster_add_combo,fnhamster_add_combof,fnhamster_add_comboa,fnhamster_2,fnhand_held_device$
00300   on error goto ERTN
00400 ! ______________________________________________________________________
00700   fntop(program$)
00900   fn_setup_hamster
01000   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
01110   fnhamster_2(env$('Program_Caption'))
01200   gosub CLOSE_FILE
01300   goto XIT
01400 ! ______________________________________________________________________
01500 OPEN_FILE: ! 
01600   let open_file_count=0 ! this value is used in the close_file sub routine
01700   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\TIMESHEET.h"&env$('cno')&",Version=0,Use,RecL=86,Shr",internal,outin,relative 
01800 return 
01900 ! ______________________________________________________________________
02000 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
02100 ! ______________________________________________________________________
02200 XIT: let fnxit
02300 ! ______________________________________________________________________
02400 ! <Updateable Region: ERTN>
02500 ERTN: let fnerror(program$,err,line,act$,"xit")
02600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02900 ERTN_EXEC_ACT: execute act$ : goto ERTN
03000 ! /region
35000 def fn_setup_hamster
35020   let mask_pointtwo=32 : let mask_number=30
35040   let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
35060   let textlen_mmddyy=8 : let textlen_ccyymmdd=10
35080   let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
35100   ! 
35120   dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
35140   mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
35160   let mask_pointtwo=32 : let mask_number=30
35180   let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
35200   let textlen_mmddyy=8 : let textlen_ccyymmdd=10
35220   let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
35240   fnhamster_field_reset
35260   fnhamster_field_add("Client ID",5)
35280   fnhamster_field_add("?? (N 9)",9)
35300   fnhamster_field_add("Hours",5.2,'PD',3.2,mask_pointtwo)
35320   fnhamster_field_add("Rate",5.2,'PD',3.2,mask_pointtwo)
35340   fnhamster_field_add("Amount",7.2,'PD',4.2,mask_pointtwo)
35360   fnhamster_field_add("Date",textlen_mmddyy,'N',storage_len_mmddyy,mask_mmddyy)
35380   fnhamster_field_add("Category",2,'N',2,mask_number)
35400   fnhamster_field_add("(??) PD 2",3,'PD',2,mask_number)
35420   fnhamster_field_add("(??) PD 3",3,'PD',1,mask_number)
35440   fnhamster_field_add("System Code",2,'N',0,mask_number)
35460   fnhamster_field_add("(??) N 4",4,'N',0,mask_number)
35480   fnhamster_field_add("(??) X 12",12,'C',0,mask_number)
35500   fnhamster_field_add("(??) PD 3",5,'PD',3,mask_number)
35520   fnhamster_field_add("(??) C 30",30,'C',0,mask_number)
35540   !   fnhamster_add_combof(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
35560 fnend  ! fn_setup_hamster
