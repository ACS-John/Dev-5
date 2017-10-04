00100 ! ______________________________________________________________________
00220   library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fnhamster,fnhamster_field_reset,fnhamster_field_add,fnhamster_add_combo,fnhamster_add_combof,fnhamster_add_comboa,fnhamster_2,fnhand_held_device$
00300   on error goto ERTN
00400 ! ______________________________________________________________________
00500   dim cap$*128
00600 ! ______________________________________________________________________
00602   cap$=srep$(program$(pos(program$,'\',-1)+1:pos(program$,'.',-1)-1),'Hamster','')
00700   fntop(program$,cap$)
00900   fn_setup_hamster
01000   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
01110   fnhamster_2(cap$)
01200   gosub CLOSE_FILE
01300   goto XIT
01500 OPEN_FILE: ! r:
01600   open_file_count=0 ! this value is used in the close_file sub routine
01700   open #open_file_count+=1: 'Name='&env$('Q')&'\'&env$('cursys')&'mstr\W2Box16.h'&env$('cno')&',KFName='&env$('Q')&'\'&env$('cursys')&'mstr\W2INDEX.h'&env$('cno')&',Use,RecL=158,Version=0,KPs=1,KLn=8,Shr',internal,outin,keyed
01800 return ! /r
02000 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
02200 XIT: let fnxit
02400 ! <Updateable Region: ERTN>
02500 ERTN: let fnerror(program$,err,line,act$,"xit")
02600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02900 ERTN_EXEC_ACT: execute act$ : goto ERTN
03000 ! /region
35000   def fn_setup_hamster
35020     let mask_pointtwo=32 : let mask_number=30
35040     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
35060     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
35080     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35100 ! 
35120     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
35140     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
35160     let mask_pointtwo=32 : let mask_number=30
35180     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
35200     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
35220     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35240     fnhamster_field_reset
35260     fnhamster_field_add("Client ID",8)
35280     boxid$(1)="Box 11:"
35300     boxid$(2)="Unused:"
35320     boxid$(3)="Box 12a:"
35340     boxid$(4)="Box 12b:"
35360     boxid$(5)="Box 12c:"
35380     boxid$(6)="Box 12d:"
35400     for tmp=1 to 6
35420       fnhamster_field_add(boxid$(tmp)&" Desc",12)
35440       fnhamster_field_add(boxid$(tmp)&" Amount",10.2,'N',10.2,mask_pointtwo)
35460       fnhamster_field_add(boxid$(tmp)&" Fed",1)
35480       fnhamster_field_add(boxid$(tmp)&" FICA",1)
35500       fnhamster_field_add(boxid$(tmp)&" State",1)
35520     nex tmp
35540 !   fnhamster_add_combof(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
35560   fnend  ! fn_setup_hamster
