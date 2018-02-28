20000 ! ______________________________________________________________________
20200   library 'S:\Core\Library': fntop,fnxit, fnget_services,fnerror,fnHamster
20400   on error goto ERTN
20600 ! ______________________________________________________________________
20800   dim cap$*128
21000 ! ______________________________________________________________________
21200   fntop(program$,cap$='Customer Hamster')
21600   dim srvnam$(10)*20,srv$(10)*2
21800   fnget_services(mat srvnam$, mat srv$)
23400   fn_setup_hamster
23600   fn_build_layout
23800   fn_open_file : fn_close_file : fn_open_file
24000   gosub HAMSTER
24200   fn_close_file
24400   goto XIT
24600 ! ______________________________________________________________________
24800   def fn_open_file
25000     open_file_count=0 ! this value is used in the close_file sub routine
25200     open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
25400     open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.h[cno],Shr",internal,outIn,keyed 
25600     open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx3.h[cno],Shr",internal,outIn,keyed 
25800     open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx4.h[cno],Shr",internal,outIn,keyed 
26000     open #open_file_count+=1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,outIn,keyed 
26200   fnend 
26400   def fn_close_file
26600     for j=1 to open_file_count : close #j: : next j
26800   fnend  ! fn_close_file
27000   def fn_build_layout
27200 ! 
27400 ! ** Combo Boxes **
27600 ! CL=Field Number  : C$(CL,1)='ComboF'
27800 ! C$(CL,2)=Linked File Name
28000 ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
28200 ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
28400 ! C$(CL,7)=Index File
28600 ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
28800     limit_to_list$='1'
29000 ! 
29200 ! cl=1 : c$(cl,1)='ComboF'
29400 ! c$(cl,2)='[Q]\TMmstr\Client.h[cno]'
29600 ! c$(cl,3)='1' : c$(cl,4)='6'
29800 ! c$(cl,5)='7' : c$(cl,6)='50'
30000 ! c$(cl,7)='[Q]\TMmstr\Client-Idx.h[cno]'
30200 ! c$(cl,8)=limit_to_list$
30400 ! cl=3 : c$(cl,1)='ComboF'
30600 ! c$(cl,2)='[Q]\TMmstr\Systems.h[cno]'
30800 ! c$(cl,3)='1' : c$(cl,4)='2'
31000 ! c$(cl,5)='3' : c$(cl,6)='50'
31200 ! c$(cl,7)='[Q]\TMmstr\Systems-Idx.h[cno]'
31400 ! c$(cl,8)=limit_to_list$
31600 ! cl=5 : c$(cl,1)='ComboF'
31800 ! c$(cl,2)='[Q]\TMmstr\TimeFrame.h[cno]'
32000 ! c$(cl,3)='1' : c$(cl,4)='2'
32200 ! c$(cl,5)='3' : c$(cl,6)='50'
32400 ! c$(cl,7)='[Q]\TMmstr\TimeFrame-Idx.h[cno]'
32600 ! c$(cl,8)=limit_to_list$
32800   fnend 
33000 ! ______________________________________________________________________
33200 HAMSTER: ! 
33400   fnHamster("Customer_Hamster",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
33600   return 
33800 ! ______________________________________________________________________
34000 XIT: fnxit
34200 ! ______________________________________________________________________
34400 ! <Updateable Region: ERTN>
34600 ERTN: fnerror(program$,err,line,act$,"xit")
34800   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
35000   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
35200   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
35400 ERTN_EXEC_ACT: execute act$ : goto ERTN
35600 ! /region
35800 ! ______________________________________________________________________
36000   def fn_add_rec(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position)
36200     if field_type$='' then field_type$='C'
36400     if storage_length=0 then storage_length=textbox_len
36600 ! storage_length_prior=storage_length
36800     add_rec_item=udim(mat lbl$)+1
37000     mat lbl$(add_rec_item) : lbl$(add_rec_item)=label$
37200     mat tln(add_rec_item) : tln(add_rec_item)=textbox_len
37400     mat p$(add_rec_item)
37600     mat fltyp$(add_rec_item) : fltyp$(add_rec_item)=field_type$
37800     mat sln(add_rec_item) : sln(add_rec_item)=storage_length
38000     mat mask(add_rec_item) : mask(add_rec_item)=ar_mask
38200 ! if storage_position=0 then
38400 !   storage_position=1
38600 !   auto_storage_position=1
38800 ! else if auto_storage_position and storage_position=0 then
39000 !   storage_position=storage_position_prior+storage_length_prior
39200 ! end if
39400     mat sp(add_rec_item) : sp(add_rec_item)=storage_position
39600 ! storage_length_prior=storage_position
39800     mat c$(add_rec_item,8)
40000   fnend  ! fn_add_rec
40200   def fn_setup_hamster
40400     mask_pointtwo=32 : mask_number=30
40600     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
40800     textlen_mmddyy=8 : textlen_ccyymmdd=10
41000     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
41200 ! 
41400     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
41600     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
41800 ! 
41820 ! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
41822     fn_add_rec("Account No",10,'C',10,0,1)
41824     fn_add_rec("Meter Address",30,'C',30,0,11)
42102     fn_add_rec("Date of Charge",8,'PD',4,mask_mmddyy,292)
42103     fn_add_rec("Last Billing",8,'PD',4,mask_mmddyy,296)
42104     fn_add_rec("Bulk",7,'C',7,0,354)
42106     fn_add_rec("Route",2,'N',2,mask_number,1741)
42108     fn_add_rec("Sequence",7,'N',7,mask_number,1743)
42110     fn_add_rec("Final",10,'C',10,0,1)
42400     fn_add_rec("G(1) ",12,'PD',4.2,mask_pointtwo,300)
42600     fn_add_rec("G(2) ",12,'PD',4.2,mask_pointtwo,304)
42800     fn_add_rec("G(3) ",12,'PD',4.2,mask_pointtwo,308)
43000     fn_add_rec("G(4) ",12,'PD',4.2,mask_pointtwo,312)
43200     fn_add_rec("G(5) ",12,'PD',4.2,mask_pointtwo,316)
43400     fn_add_rec("G(6) ",12,'PD',4.2,mask_pointtwo,320)
43600     fn_add_rec("G(7) ",12,'PD',4.2,mask_pointtwo,324)
43800     fn_add_rec("G(8) ",12,'PD',4.2,mask_pointtwo,328)
44000     fn_add_rec("G(9) ",12,'PD',4.2,mask_pointtwo,332)
44200     fn_add_rec("G(10)",12,'PD',4.2,mask_pointtwo,336)
44400     fn_add_rec("G(11)",12,'PD',4.2,mask_pointtwo,340)
44600     fn_add_rec("G(12)",12,'PD',4.2,mask_pointtwo,344)
44800     fn_add_rec(srv$(1)&' Reading - Current',12,'PD',5,mask_number,217)
45000     fn_add_rec(srv$(1)&' Reading - Prior  ',12,'PD',5,mask_number,222)
45200     fn_add_rec(srv$(1)&' Usage - Current  ',12,'PD',5,mask_number,227)
45400     fn_add_rec(srv$(1)&' Usage - YTD      ',12,'PD',5,mask_number,232)
45600     fn_add_rec(srv$(3)&' Reading - Current',12,'PD',5,mask_number,237)
45800     fn_add_rec(srv$(3)&' Reading - Prior  ',12,'PD',5,mask_number,242)
46000     fn_add_rec(srv$(3)&' Usage - Current  ',12,'PD',5,mask_number,247)
46200     fn_add_rec(srv$(3)&' Usage - YTD      ',12,'PD',5,mask_number,252)
46400     fn_add_rec(srv$(4)&' Reading - Current',12,'PD',5,mask_number,257)
46600     fn_add_rec(srv$(4)&' Reading - Prior  ',12,'PD',5,mask_number,262)
46800     fn_add_rec(srv$(4)&' Usage - Current  ',12,'PD',5,mask_number,267)
47000     fn_add_rec(srv$(4)&' Usage - YTD      ',12,'PD',5,mask_number,272)
47200     fn_add_rec('Units Per Meter',12,'PD',5,mask_number,277)
47400     fn_add_rec('Demand Multiplier',12,'PD',5,mask_number,282)
47600     fn_add_rec("Demand Reading",12,'PD',5,mask_number,287)
47800   fnend  ! fn_setup_hamster
