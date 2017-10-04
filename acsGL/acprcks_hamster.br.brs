10000 ! "Replace "&program$
10200 ! another Hamster
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim cap$*128
11400 ! ______________________________________________________________________
11600   fntop(program$,cap$='AcPrCks Hamster')
12000   fn_setup_hamster
12400   fn_open_file
12600   if sum(mat sp)=0 then mat sp(0)
12800   fnhamster("AcPrCks",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
13000   fn_close_file
13200   goto XIT
13400 ! ______________________________________________________________________
13600   def fn_open_file
13800     let open_file_count=0 ! this value is used in the close_file sub routine
14000     open #open_file_count+=1: "Name="&env$('Q')&"\GLmstr\AcPrCks.h"&env$('cno')&",Shr",internal,outin,relative 
14200   fnend 
14400   def fn_close_file
14600     for j=1 to open_file_count : close #j: : next j
14800   fnend  ! fn_close_file
15000 XIT: let fnxit
15200 ! ______________________________________________________________________
15400 ! <Updateable Region: ERTN>
15600 ERTN: let fnerror(program$,err,line,act$,"xit")
15800   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
16000   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
16200   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
16400 ERTN_EXEC_ACT: execute act$ : goto ERTN
16600 ! /region
16800 ! ______________________________________________________________________
17000   def fn_setup_hamster
17200     let mask_pointtwo=32 : let mask_number=30
17400     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
17600     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
17800     let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
18000 ! 
18200     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
18400     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
18600 ! 
18800 ! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
19000     fn_add_rec("ENo ",12,'N',4,mask_number)
19200     fn_add_rec("Date ",12,'PD',4,mask_number)
19400     fn_add_rec("Check Number ",12,'PD',4,mask_number)
19600     for x=1 to 19
19800       fn_add_rec("D("&str$(x+3)&")",12,'PD',5.2,mask_pointtwo)
20000     next x
20200     fn_add_rec("NTA",12,'PD',3,mask_number)
20400   fnend  ! fn_setup_hamster
20600   def fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
20800     if storage_length=0 then let storage_length=textbox_len
21000 ! storage_length_prior=storage_length
21200     add_rec_item=udim(mat lbl$)+1
21400     mat lbl$(add_rec_item) : let lbl$(add_rec_item)=label$
21600     mat tln(add_rec_item) : let tln(add_rec_item)=textbox_len
21800     mat p$(add_rec_item)
22000     mat fltyp$(add_rec_item) : let fltyp$(add_rec_item)=field_type$
22200     mat sln(add_rec_item) : let sln(add_rec_item)=storage_length
22400     mat mask(add_rec_item) : let mask(add_rec_item)=ar_mask
22600 ! if storage_position=0 then
22800 !   storage_position=1
23000 !   auto_storage_position=1
23200 ! else if auto_storage_position and storage_position=0 then
23400 !   storage_position=storage_position_prior+storage_length_prior
23600 ! end if
23800     mat sp(add_rec_item) : let sp(add_rec_item)=storage_position
24000 ! storage_length_prior=storage_position
24200     mat c$(add_rec_item,8)
24400   fnend  ! fn_add_rec

