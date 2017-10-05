20000 ! ______________________________________________________________________
20200   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
20400   on error goto ERTN
20600 ! ______________________________________________________________________
20800   dim cap$*128
21000 ! ______________________________________________________________________
21200   fntop(program$,cap$='Reg Hamster')
21400   fncno(cno)
21600   fn_setup_hamster
21800   fn_open_file : fn_close_file : fn_open_file
22000   gosub HAMSTER
22200   fn_close_file
22400   goto XIT
22600 ! ______________________________________________________________________
22800   def fn_open_file
23000     open_file_count=0 ! this value is used in the close_file sub routine
23200     open #open_file_count+=1: 'Name='&env$('Q')&'\Data\reg.dat,Version=1,KFName='&env$('Q')&'\Data\reg.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed 
23400 ! open #open_file_count+=1: 'Name=data\reg.dat,Version=1,KFName=data\reg.idx,Shr',internal,outin,keyed
23600 ! open #open_file_count+=1: 'Name=data\reg.dat,Shr',internal,outin,relative
23800   fnend 
24000   def fn_close_file
24200     for j=1 to open_file_count : close #j: : next j
24400   fnend  ! fn_close_file
24600 ! ______________________________________________________________________
24800 HAMSTER: ! 
25000   fnhamster("Reg_Hamster",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
25200   return 
25400 ! ______________________________________________________________________
25600 XIT: fnxit
25800 ! ______________________________________________________________________
26000 ! <Updateable Region: ERTN>
26200 ERTN: fnerror(program$,err,line,act$,"xit")
26400   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
26600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
26800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
27000 ERTN_EXEC_ACT: execute act$ : goto ERTN
27200 ! /region
27400 ! ______________________________________________________________________
27600   def fn_add_rec(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
27800     if field_type$='' then field_type$='C'
28000     if storage_length=0 then storage_length=textbox_len
28200 ! storage_length_prior=storage_length
28400     add_rec_item=udim(mat lbl$)+1
28600     mat lbl$(add_rec_item) : lbl$(add_rec_item)=label$
28800     mat tln(add_rec_item) : tln(add_rec_item)=textbox_len
29000     mat p$(add_rec_item)
29200     mat fltyp$(add_rec_item) : fltyp$(add_rec_item)=field_type$
29400     mat sln(add_rec_item) : sln(add_rec_item)=storage_length
29600     mat mask(add_rec_item) : mask(add_rec_item)=ar_mask
29800 ! if storage_position=0 then
30000 !   storage_position=1
30200 !   auto_storage_position=1
30400 ! else if auto_storage_position and storage_position=0 then
30600 !   storage_position=storage_position_prior+storage_length_prior
30800 ! end if
31000     mat sp(add_rec_item) : sp(add_rec_item)=storage_position
31200 ! storage_length_prior=storage_position
31400     mat c$(add_rec_item,8)
31600   fnend  ! fn_add_rec
31800   def fn_setup_hamster
32000     mask_pointtwo=32 : mask_number=30
32200     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
32400     textlen_mmddyy=8 : textlen_ccyymmdd=10
32600     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
32800 ! 
33000     dim lbl$(1)*38,tln(1),p$(1)*256,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
33200     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
33400 ! 
33600 ! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
33800     fn_add_rec("Key",30,'C',128,0,1)
34000     fn_add_rec("Value",40,'C',256,0,129)
34200   fnend  ! fn_setup_hamster
