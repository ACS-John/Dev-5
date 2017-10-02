10000 ! "Replace "&program$
10200 ! another Hamster
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim cap$*128
11400 ! ______________________________________________________________________
11600   let fntop(program$,cap$='PRmstr Hamster')
11800   let fncno(cno)
12000   let fn_setup_hamster
12200   let fn_build_layout
12400   let fn_open_file : let fn_close_file : let fn_open_file
12600   if sum(mat sp)=0 then mat sp(0)
12800   let fnhamster("PRmstr",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
13000   let fn_close_file
13200   goto XIT
13400 ! ______________________________________________________________________
13600   def fn_open_file
13800     let open_file_count=0 ! this value is used in the close_file sub routine
14000     open #open_file_count+=1: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\PRINDEX.h"&str$(cno)&",Shr",internal,outin,keyed 
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
19000     let fn_add_rec("ENo ",12,'N',4,mask_number)
19200     let fn_add_rec("Name ",25)
19400     let fn_add_rec("Address ",25)
19600     let fn_add_rec("CSZ ",25)
19800     let fn_add_rec("SSN ",11)
20000     for x=1 to 36
20200       let fn_add_rec("M("&str$(x)&")",12,'PD',5.2,mask_pointtwo)
20400     next x
20600     let fn_add_rec("First Check Address",12,'N',5,mask_number)
20800     let fn_add_rec("Last Check Address",12,'N',5,mask_number)
21000   fnend  ! fn_setup_hamster
21200   def fn_add_rec(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
21400     if field_type$='' then let field_type$='C'
21420     if storage_length=0 then let storage_length=textbox_len
21600 ! storage_length_prior=storage_length
21800     add_rec_item=udim(mat lbl$)+1
22000     mat lbl$(add_rec_item) : let lbl$(add_rec_item)=label$
22200     mat tln(add_rec_item) : let tln(add_rec_item)=textbox_len
22400     mat p$(add_rec_item)
22600     mat fltyp$(add_rec_item) : let fltyp$(add_rec_item)=field_type$
22800     mat sln(add_rec_item) : let sln(add_rec_item)=storage_length
23000     mat mask(add_rec_item) : let mask(add_rec_item)=ar_mask
23200 ! if storage_position=0 then
23400 !   storage_position=1
23600 !   auto_storage_position=1
23800 ! else if auto_storage_position and storage_position=0 then
24000 !   storage_position=storage_position_prior+storage_length_prior
24200 ! end if
24400     mat sp(add_rec_item) : let sp(add_rec_item)=storage_position
24600 ! storage_length_prior=storage_position
24800     mat c$(add_rec_item,8)
25000   fnend  ! fn_add_rec
25200   def fn_build_layout
25400 ! 
25600 ! ** Combo Boxes **
25800 ! CL=Field Number  : C$(CL,1)='ComboF'
26000 ! C$(CL,2)=Linked File Name
26200 ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
26400 ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
26600 ! C$(CL,7)=Index File
26800 ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
27000     let limit_to_list$='1'
27200 ! 
27400 ! cl=1 : c$(cl,1)='ComboF'
27600 ! c$(cl,2)=env$('Q')&'\TMmstr\Client.h'&str$(cno)
27800 ! c$(cl,3)='1' : c$(cl,4)='6'
28000 ! c$(cl,5)='7' : c$(cl,6)='50'
28200 ! c$(cl,7)=env$('Q')&'\TMmstr\Client-Idx.h'&str$(cno)
28400 ! c$(cl,8)=limit_to_list$
28600 ! 
28800 ! cl=3 : c$(cl,1)='ComboF'
29000 ! c$(cl,2)=env$('Q')&'\TMmstr\Systems.h'&str$(cno)
29200 ! c$(cl,3)='1' : c$(cl,4)='2'
29400 ! c$(cl,5)='3' : c$(cl,6)='50'
29600 ! c$(cl,7)=env$('Q')&'\TMmstr\Systems-Idx.h'&str$(cno)
29800 ! c$(cl,8)=limit_to_list$
30000 ! 
30200 ! cl=5 : c$(cl,1)='ComboF'
30400 ! c$(cl,2)=env$('Q')&'\TMmstr\TimeFrame.h'&str$(cno)
30600 ! c$(cl,3)='1' : c$(cl,4)='2'
30800 ! c$(cl,5)='3' : c$(cl,6)='50'
31000 ! c$(cl,7)=env$('Q')&'\TMmstr\TimeFrame-Idx.h'&str$(cno)
31200 ! c$(cl,8)=limit_to_list$
31400   fnend 
