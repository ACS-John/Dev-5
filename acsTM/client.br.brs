20000 ! Replace S:\acsTM\Client
20100 ! TM Client - Hamster
20200 ! ______________________________________________________________________
20300   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster
20400   on error goto ERTN
20500 ! ______________________________________________________________________
20600   dim cap$*128
20700 ! ______________________________________________________________________
20800   let fntop(program$,cap$='Client 420')
21000 ! 
21200   let fn_hamster_setup_1
21300   let fn_hamster_setup_2
21400   let fn_open_file : let fn_close_file : let fn_open_file
21500   gosub HAMSTER
21600   let fn_close_file
21700   goto XIT
21800 ! ______________________________________________________________________
21900   def fn_open_file
22000     let open_file_count=0 ! this value is used in the close_file sub routine
22100     open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\CLmstr.h420,Version=0,KFName="&env$('Q')&"\TMmstr\CLIndex.h420,Use,RecL=534,KPs=1,KLn=5,Shr",internal,outin,keyed 
22200     open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\CLmstr.h420,Version=0,KFName="&env$('Q')&"\TMmstr\CLIndx2-Idx.h420,Use,RecL=534,KPs=6,KLn=28,Shr",internal,outin,keyed 
22300   fnend 
22400   def fn_close_file
22500     for cf_h_item=1 to open_file_count
22600       close #cf_h_item: 
22700     next cf_h_item
22800   fnend  ! fn_close_file
22900   def fn_hamster_setup_2
23000 ! 
23100 ! ** Combo Boxes **
23200 ! c_x=Field Number  : C$(c_x,1)='ComboF'
23300 ! C$(c_x,2)=Linked File Name
23400 ! C$(c_x,3)=Key Position         : C$(c_x,4)=Key Length
23500 ! C$(c_x,5)=Description Position : C$(c_x,6)=Description Length
23600 ! C$(c_x,7)=Index File
23700 ! C$(c_x,8)=limit to list option ('1'=Yes; '0'=No)
23800     let limit_to_list$='1'
23900 ! 
24000 ! cl=1 : c$(cl,1)='ComboF'
24100 ! c$(cl,2)=env$('Q')&'\TMmstr\Client.h420'
24200 ! c$(cl,3)='1' : c$(cl,4)='6'
24300 ! c$(cl,5)='7' : c$(cl,6)='50'
24400 ! c$(cl,7)=env$('Q')&'\TMmstr\Client-Idx.h420'
24500 ! c$(cl,8)=limit_to_list$
24600 ! cl=3 : c$(cl,1)='ComboF'
24700 ! c$(cl,2)=env$('Q')&'\TMmstr\Systems.h420'
24800 ! c$(cl,3)='1' : c$(cl,4)='2'
24900 ! c$(cl,5)='3' : c$(cl,6)='50'
25000 ! c$(cl,7)=env$('Q')&'\TMmstr\Systems-Idx.h420'
25100 ! c$(cl,8)=limit_to_list$
25200 ! cl=5 : c$(cl,1)='ComboF'
25300 ! c$(cl,2)=env$('Q')&'\TMmstr\TimeFrame.h420'
25400 ! c$(cl,3)='1' : c$(cl,4)='2'
25500 ! c$(cl,5)='3' : c$(cl,6)='50'
25600 ! c$(cl,7)=env$('Q')&'\TMmstr\TimeFrame-Idx.h420'
25700 ! c$(cl,8)=limit_to_list$
25800 ! 
25900 ! c_x=1 : c$(c_x,1)='ComboF'
26000 ! c$(c_x,2)=env$('Q')&"\UBmstr\Customer.h420"
26100 ! c$(c_x,3)='1' : c$(c_x,4)='10' ! Key
26200 ! c$(c_x,5)='41' : c$(c_x,6)='30' ! Description
26300 ! c$(c_x,7)=env$('Q')&"\UBmstr\ubIndex.h420"
26400 ! c$(c_x,8)='1'
26500 ! 
26600 ! c_x=2 : c_y=1
26700 ! c$(c_x,c_y)='ComboA'
26800 ! for srv_item=1 to 10
26900 !  if (srv_item=1 and trim$(srv$(srv_item))<>'') or (srvnam$(srv_item)="GAS" or srv$(srv_item)="GA") or srv$(srv_item)='EL' or srvnam$(srv_item)="Lawn Meter" then ! if it is a metered service
27000 !   c$(c_x,c_y+=1)=srv$(srv_item)
27100 !  end if
27200 ! next srv_item
27300   fnend 
27400 ! ______________________________________________________________________
27500 HAMSTER: ! 
27600   let fnhamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
27700   return 
27800 ! ______________________________________________________________________
27900 XIT: let fnxit
28000 ! ______________________________________________________________________
28100 ! <Updateable Region: ERTN>
28200 ERTN: let fnerror(program$,err,line,act$,"xit")
28300   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
28400   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
28500   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
28600 ERTN_EXEC_ACT: execute act$ : goto ERTN
28700 ! /region
28800 ! ______________________________________________________________________
28900   def fn_hamster_setup_1
29000     let mask_pointtwo=32 : let mask_number=30
29100     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
29200     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
29300     let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
29400 ! 
29500     let fn_hamster_field_reset
29600 ! 
29700 ! fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
29800 ! 
29900     let fn_hamster_field_add("Client ID" ,5,'N',0,mask_number)
30000     let fn_hamster_field_add("Name",30,'C')
30020     let fn_hamster_field_add("Balance",12,'pd',5.2,mask_pointtwo,283) ! ,disable:=1) ! ar(1)
30100     let fn_hamster_field_add("Address" ,30,'C',0,0,36)
30200     let fn_hamster_field_add("CSZ" ,30,'C',0,0,66)
30300     let fn_hamster_field_add("Contact",30,'C')
30400     let fn_hamster_field_add("Business Type",30,'C')
30500     let fn_hamster_field_add("Phone",12,'C') ! ph$
30600     let fn_hamster_field_add("Federal ID",11,'c') ! ss$
30700 ! let fn_hamster_field_add("Partner #",9,'n',0,mask_number) ! pno
30800 ! let fn_hamster_field_add("Month of Year-End",2,'n',0,mask_number) ! mye
30900 ! let fn_hamster_field_add("dd(01)",4,"PD", 3,mask_number) ! due date
31000 ! let fn_hamster_field_add("dd(02)",4,"PD", 3,mask_number)
31100 ! let fn_hamster_field_add("dd(03)",4,"PD", 3,mask_number)
31200 ! let fn_hamster_field_add("dd(04)",4,"PD", 3,mask_number)
31300 ! let fn_hamster_field_add("dd(05)",4,"PD", 3,mask_number)
31400 ! let fn_hamster_field_add("dd(06)",4,"PD", 3,mask_number)
31500 ! let fn_hamster_field_add("dd(07)",4,"PD", 3,mask_number)
31600 ! let fn_hamster_field_add("dd(08)",4,"PD", 3,mask_number)
31700 ! let fn_hamster_field_add("dd(09)",4,"PD", 3,mask_number)
31800 ! let fn_hamster_field_add("dd(10)",4,"PD", 3,mask_number)
31900 ! let fn_hamster_field_add("sc(01)",1,"N") ! status
32000 ! let fn_hamster_field_add("sc(02)",1,"N")
32100 ! let fn_hamster_field_add("sc(03)",1,"N")
32200 ! let fn_hamster_field_add("sc(04)",1,"N")
32300 ! let fn_hamster_field_add("sc(05)",1,"N")
32400 ! let fn_hamster_field_add("sc(06)",1,"N")
32500 ! let fn_hamster_field_add("sc(07)",1,"N")
32600 ! let fn_hamster_field_add("sc(08)",1,"N")
32700 ! let fn_hamster_field_add("sc(09)",1,"N")
32800 ! let fn_hamster_field_add("sc(10)",1,"N")
32900 ! let fn_hamster_field_add("ca(01)" ,5,"PD", 3)
33000 ! let fn_hamster_field_add("ca(02)" ,5,"PD", 3)
33100 ! let fn_hamster_field_add("ca(03)" ,5,"PD", 3)
33200 ! let fn_hamster_field_add("ca(04)" ,5,"PD", 3)
33300 ! let fn_hamster_field_add("ca(05)" ,5,"PD", 3)
33400 ! let fn_hamster_field_add("ca(06)" ,5,"PD", 3)
33500 ! let fn_hamster_field_add("ca(07)" ,5,"PD", 3)
33600 ! let fn_hamster_field_add("ca(08)" ,5,"PD", 3)
33700 ! let fn_hamster_field_add("ca(09)" ,5,"PD", 3)
33800 ! let fn_hamster_field_add("ca(10)" ,5,"PD", 3)
33900     let fn_hamster_field_add("Home Phone",12,'c',0,0,260) ! ph2$
34000 ! let fn_hamster_field_add("Spouse SSN",11,'c') ! ss2$
34100 ! let fn_hamster_field_add("Balance",12,'pd',5.2,mask_pointtwo,283) ! ar(1)
34200 ! let fn_hamster_field_add("ar(2)",12,'pd',5.2,mask_pointtwo)
34300 ! let fn_hamster_field_add("ar(3)",12,'pd',4.3,mask_pointtwo)
34400 ! let fn_hamster_field_add("ar(4)",1,'n')
34500 ! let fn_hamster_field_add("ar(5)",1,'n')
34600 ! let fn_hamster_field_add("arta(1)",5,'pd',3)
34700 ! let fn_hamster_field_add("arta(2)",5,'pd',3)
34800 ! let fn_hamster_field_add("cm$",70,'c')
34900 ! let fn_hamster_field_add("app(01)",1,'N')
35000 ! let fn_hamster_field_add("app(02)",1,'N')
35100 ! let fn_hamster_field_add("app(03)",1,'N')
35200 ! let fn_hamster_field_add("app(04)",1,'N')
35300 ! let fn_hamster_field_add("app(05)",1,'N')
35400 ! let fn_hamster_field_add("app(06)",1,'N')
35500 ! let fn_hamster_field_add("app(07)",1,'N')
35600 ! let fn_hamster_field_add("app(08)",1,'N')
35700 ! let fn_hamster_field_add("app(09)",1,'N')
35800 ! let fn_hamster_field_add("app(10)",1,'N')
35900 ! let fn_hamster_field_add("app(11)",1,'N')
36000 ! let fn_hamster_field_add("app(12)",1,'N')
36100 ! let fn_hamster_field_add("app(13)",1,'N')
36200 ! let fn_hamster_field_add("app(14)",1,'N')
36300 ! let fn_hamster_field_add("app(15)",1,'N')
36400 ! let fn_hamster_field_add("app(16)",1,'N')
36500 ! let fn_hamster_field_add("app(17)",1,'N')
36600 ! let fn_hamster_field_add("app(18)",1,'N')
36700 ! let fn_hamster_field_add("app(19)",1,'N')
36800 ! let fn_hamster_field_add("app(20)",1,'N')
36900 ! let fn_hamster_field_add("ma(01)",6,'pd',3.2)
37000 ! let fn_hamster_field_add("ma(02)",6,'pd',3.2)
37100 ! let fn_hamster_field_add("ma(03)",6,'pd',3.2)
37200 ! let fn_hamster_field_add("ma(04)",6,'pd',3.2)
37300 ! let fn_hamster_field_add("ma(05)",6,'pd',3.2)
37400 ! let fn_hamster_field_add("ma(06)",6,'pd',3.2)
37500 ! let fn_hamster_field_add("ma(07)",6,'pd',3.2)
37600 ! let fn_hamster_field_add("ma(08)",6,'pd',3.2)
37700 ! let fn_hamster_field_add("ma(09)",6,'pd',3.2)
37800 ! let fn_hamster_field_add("ma(10)",6,'pd',3.2)
37900 ! let fn_hamster_field_add("ma(11)",6,'pd',3.2)
38000 ! let fn_hamster_field_add("ma(12)",6,'pd',3.2)
38100 ! let fn_hamster_field_add("ma(13)",6,'pd',3.2)
38200 ! let fn_hamster_field_add("ma(14)",6,'pd',3.2)
38300 ! let fn_hamster_field_add("ma(15)",6,'pd',3.2)
38400 ! let fn_hamster_field_add("ma(16)",6,'pd',3.2)
38500 ! let fn_hamster_field_add("ma(17)",6,'pd',3.2)
38600 ! let fn_hamster_field_add("ma(18)",6,'pd',3.2)
38700 ! let fn_hamster_field_add("ma(19)",6,'pd',3.2)
38800 ! let fn_hamster_field_add("ma(20)",6,'pd',3.2)
38900 ! let fn_hamster_field_add("app2(01)",1,'n')
39000 ! let fn_hamster_field_add("app2(02)",1,'n')
39100 ! let fn_hamster_field_add("app2(03)",1,'n')
39200 ! let fn_hamster_field_add("app2(04)",1,'n')
39300 ! let fn_hamster_field_add("app2(05)",1,'n')
39400 ! let fn_hamster_field_add("app2(06)",1,'n')
39500 ! let fn_hamster_field_add("app2(07)",1,'n')
39600 ! let fn_hamster_field_add("app2(08)",1,'n')
39700 ! let fn_hamster_field_add("app2(09)",1,'n')
39800 ! let fn_hamster_field_add("app2(10)",1,'n')
39900 ! let fn_hamster_field_add("app2(11)",1,'n')
40000 ! let fn_hamster_field_add("app2(12)",1,'n')
40100 ! let fn_hamster_field_add("app2(13)",1,'n')
40200 ! let fn_hamster_field_add("app2(14)",1,'n')
40300 ! let fn_hamster_field_add("app2(15)",1,'n')
40400 ! let fn_hamster_field_add("app2(16)",1,'n')
40500 ! let fn_hamster_field_add("app2(17)",1,'n')
40600 ! let fn_hamster_field_add("app2(18)",1,'n')
40700 ! let fn_hamster_field_add("app2(19)",1,'n')
40800 ! let fn_hamster_field_add("app2(20)",1,'n')
40900 ! let fn_hamster_field_add("ma2(01)",6,'pd',3.2)
41000 ! let fn_hamster_field_add("ma2(02)",6,'pd',3.2)
41100 ! let fn_hamster_field_add("ma2(03)",6,'pd',3.2)
41200 ! let fn_hamster_field_add("ma2(04)",6,'pd',3.2)
41300 ! let fn_hamster_field_add("ma2(05)",6,'pd',3.2)
41400 ! let fn_hamster_field_add("ma2(06)",6,'pd',3.2)
41500 ! let fn_hamster_field_add("ma2(07)",6,'pd',3.2)
41600 ! let fn_hamster_field_add("ma2(08)",6,'pd',3.2)
41700 ! let fn_hamster_field_add("ma2(09)",6,'pd',3.2)
41800 ! let fn_hamster_field_add("ma2(10)",6,'pd',3.2)
41900 ! let fn_hamster_field_add("ma2(11)",6,'pd',3.2)
42000 ! let fn_hamster_field_add("ma2(12)",6,'pd',3.2)
42100 ! let fn_hamster_field_add("ma2(13)",6,'pd',3.2)
42200 ! let fn_hamster_field_add("ma2(14)",6,'pd',3.2)
42300 ! let fn_hamster_field_add("ma2(15)",6,'pd',3.2)
42400 ! let fn_hamster_field_add("ma2(16)",6,'pd',3.2)
42500 ! let fn_hamster_field_add("ma2(17)",6,'pd',3.2)
42600 ! let fn_hamster_field_add("ma2(18)",6,'pd',3.2)
42700 ! let fn_hamster_field_add("ma2(19)",6,'pd',3.2)
42800 ! let fn_hamster_field_add("ma2(20)",6,'pd',3.2)
42900   fnend  ! fn_hamster_setup_1
43000   def fn_hamster_field_reset
43100     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40,sp(1)
43200     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
43300   fnend  ! fn_hamster_field_reset
43400   def fn_hamster_field_add(label$*38,textbox_len; field_type$*2,storage_length,ar_mask,storage_position) ! ,disable) <- didn't quite work as easily as i was hoping for it to.
43500     if field_type$='' then let field_type$='C'
43600     if storage_length=0 then let storage_length=textbox_len
43700     add_rec_item=udim(mat lbl$)+1
43800     mat lbl$(add_rec_item) : let lbl$(add_rec_item)=label$
43900     mat tln(add_rec_item) : let tln(add_rec_item)=textbox_len
44000     mat p$(add_rec_item)
44100     mat fltyp$(add_rec_item) : let fltyp$(add_rec_item)=field_type$
44200     mat sln(add_rec_item) : let sln(add_rec_item)=storage_length
44300     mat mask(add_rec_item) : let mask(add_rec_item)=ar_mask
44400     mat sp(add_rec_item) : let sp(add_rec_item)=storage_position
44500     if storage_position=0 then 
44600       if add_rec_item=1 then 
44700         let sp(add_rec_item)=1
44800         auto_storage_position=1
44900       else 
45000         let sp(add_rec_item)=sp(add_rec_item-1)+sln(add_rec_item-1)
45100       end if 
45200     end if 
45300     mat c$(add_rec_item,8)
45310 ! c$(add_rec_item,7)=str$(disable)
45400   fnend  ! fn_hamster_field_add
