00100 ! ______________________________________________________________________
00220   library 'S:\Core\Library': fntop,fnxit, fnerror,fnmsgbox,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnHamster2,fnhand_held_device$
00300   on error goto ERTN
00400 ! ______________________________________________________________________
00700   fntop(program$)
00900   fn_setup_hamster
01000   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
01100   ! fnHamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
01110   fnHamster2("ARTrans")
01200   gosub CLOSE_FILE
01300   goto XIT
01400 ! ______________________________________________________________________
01500 OPEN_FILE: ! 
01600   open_file_count=0 ! this value is used in the close_file sub routine
01700   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\Support.h420,Version=2,KFName="&env$('Q')&"\TMmstr\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outIn,keyed 
01800 return 
01900 ! ______________________________________________________________________
02000 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
02100 ! ______________________________________________________________________
02200 XIT: fnxit
02300 ! ______________________________________________________________________
02400 ! <Updateable Region: ERTN>
02500 ERTN: fnerror(program$,err,line,act$,"xit")
02600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02800   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02900 ERTN_EXEC_ACT: execute act$ : goto ERTN
03000 ! /region
35000   def fn_setup_hamster
35020     mask_pointtwo=32 : mask_number=30
35040     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
35060     textlen_mmddyy=8 : textlen_ccyymmdd=10
35080     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35100 ! 
35120     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
35140     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
35160     mask_pointtwo=32 : mask_number=30
35180     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
35200     textlen_mmddyy=8 : textlen_ccyymmdd=10
35220     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35240     fnH2Init
35260  !
35280  !
35300   fnH2AddText("Client ID"       ,6               ,'N'                     ,mask_number   )    
35320   fnH2AddText("Sys#"            ,2               ,'N',0                   ,mask_number   )    
35340   fnH2AddText("System ID"       ,2               ,'C'                                    )    
35360   fnH2AddText("Starting Date"   ,textlen_ccyymmdd,'N',storage_len_ccyymmdd,mask_ccyymmdd )    
35380   fnH2AddText("Time Frame"      ,2               ,'C'                                    )    
35400   fnH2AddText("Ending Date"     ,textlen_ccyymmdd,'N',storage_len_ccyymmdd,mask_ccyymmdd )
35420   fnH2AddText("Cost to User"    ,10              ,'N'                                    )
35440   fnH2AddText("Name"            ,50              ,'C'                                    )     
35460 ! fnH2AddText("Contact (1)"     ,50              ,'C'                                    )           
35480 ! fnH2AddText("Contact (2)"     ,50              ,'C'                                    )           
35500 ! fnH2AddText("Contact (3)"     ,50              ,'C'                                    )           
35520  !
35540     fnH2AddComboF(1,env$('Q')&'\TMmstr\Clmstr.h420',1,5,6,30,env$('Q')&'\TMmstr\CLIndex.h420',1)
35560  !
35580 ! old program !  ! ** Combo Boxes **
35600 ! old program !  ! CL=Field Number  : C$(CL,1)='ComboF'
35620 ! old program !  ! C$(CL,2)=Linked File Name
35640 ! old program !  ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
35660 ! old program !  ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
35680 ! old program !  ! C$(CL,7)=Index File
35700 ! old program !  ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
35720  !
35740 !          fnH2AddComboF(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
35760   fnend


55000 ! old program !  ! Replace S:\acsTM\Support
55020 ! old program !  ! ______________________________________________________________________
55040 ! old program !    library 'S:\Core\Library': fntop,fnxit, fnerror,fnHamster
55060 ! old program !    on error goto ERTN
55080 ! old program !  ! ______________________________________________________________________
55100 ! old program !    dim cap$*128,lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),sln(11),mask(11),c$(11,8)*256 ! SP(11) - not used
55120 ! old program !  ! ______________________________________________________________________
55140 ! old program !    fntop(program$,cap$='Support 420')
55160 ! old program !    gosub BUILD_LAYOUT
55180 ! old program !    gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
55200 ! old program !    gosub HAMSTER: gosub CLOSE_FILE
55220 ! old program !    execute "Index "&env$('Q')&"\TMmstr\support.h420  "&env$('Q')&"\TMmstr\support-idx.h420 1/7,6/2,replace,DupKeys"
55240 ! old program !    goto XIT
55260 ! old program !  ! ______________________________________________________________________
55280 ! old program !  OPEN_FILE: ! 
55300 ! old program !    open_file_count=0 ! this value is used in the close_file sub routine
55320 ! old program !    open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\Support.h420,Version=2,KFName="&env$('Q')&"\TMmstr\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outIn,keyed 
55340 ! old program !    return 
55360 ! old program !  ! ______________________________________________________________________
55380 ! old program !  CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
55400 ! old program !  ! ______________________________________________________________________
55420 ! old program !  BUILD_LAYOUT: ! 
55440 ! old program !  ! ** Field Labels    **
55460 ! old program !    ic=0 ! temporary Item Counter
55480 ! old program !    lbl$(ic+=1)="Client ID"
55500 ! old program !    lbl$(ic+=1)="Sys#"
55520 ! old program !    lbl$(ic+=1)="System ID"
55540 ! old program !    lbl$(ic+=1)="Starting Date"
55560 ! old program !    lbl$(ic+=1)="Time Frame"
55580 ! old program !    lbl$(ic+=1)="Ending Date"
55600 ! old program !    lbl$(ic+=1)="Cost to User"
55620 ! old program !    lbl$(ic+=1)="Name"
55640 ! old program !    lbl$(ic+=1)="Contact (1)"
55660 ! old program !    lbl$(ic+=1)="Contact (2)"
55680 ! old program !    lbl$(ic+=1)="Contact (3)"
55700 ! old program !  ! ** Text Box / Field Display   Lengths   **
55720 ! old program !    ic=0 ! temporary Item Counter
55740 ! old program !    mmddyy=8
55760 ! old program !    ccyymmdd=10
55780 ! old program !    tln(ic+=1)=6
55800 ! old program !    tln(ic+=1)=2
55820 ! old program !    tln(ic+=1)=2
55840 ! old program !    tln(ic+=1)=ccyymmdd
55860 ! old program !    tln(ic+=1)=2
55880 ! old program !    tln(ic+=1)=ccyymmdd
55900 ! old program !    tln(ic+=1)=10
55920 ! old program !    tln(ic+=1)=50
55940 ! old program !    tln(ic+=1)=50
55960 ! old program !    tln(ic+=1)=50
55980 ! old program !    tln(ic+=1)=50
56000 ! old program !  ! ** Field Types **
56020 ! old program !    ic=0
56040 ! old program !    fltyp$(ic+=1)='N'
56060 ! old program !    fltyp$(ic+=1)='N'
56080 ! old program !    fltyp$(ic+=1)='C'
56100 ! old program !    fltyp$(ic+=1)='N'
56120 ! old program !    fltyp$(ic+=1)='C'
56140 ! old program !    fltyp$(ic+=1)='N'
56160 ! old program !    fltyp$(ic+=1)='N'
56180 ! old program !    fltyp$(ic+=1)='C'
56200 ! old program !    fltyp$(ic+=1)='C'
56220 ! old program !    fltyp$(ic+=1)='C'
56240 ! old program !    fltyp$(ic+=1)='C'
56260 ! old program !  ! ** Field Storage Lengths **
56280 ! old program !    ic=0
56300 ! old program !    mmddyy=6 : ccyymmdd=8
56320 ! old program !    sln(ic+=1)=6
56340 ! old program !    sln(ic+=1)=2
56360 ! old program !    sln(ic+=1)=2
56380 ! old program !    sln(ic+=1)=ccyymmdd
56400 ! old program !    sln(ic+=1)=2
56420 ! old program !    sln(ic+=1)=ccyymmdd
56440 ! old program !    sln(ic+=1)=10.2
56460 ! old program !    sln(ic+=1)=50
56480 ! old program !    sln(ic+=1)=50
56500 ! old program !    sln(ic+=1)=50
56520 ! old program !    sln(ic+=1)=50
56540 ! old program !  ! ** Field Masks **
56560 ! old program !    ic=0
56580 ! old program !    pointtwo=32 : number=30
56600 ! old program !    ccyymmdd=3 : mmddyy=1 : glnumber=53
56620 ! old program !    mask(ic+=1)=number
56640 ! old program !    mask(ic+=1)=number
56660 ! old program !    mask(ic+=1)=0
56680 ! old program !    mask(ic+=1)=ccyymmdd
56700 ! old program !    mask(ic+=1)=0
56720 ! old program !    mask(ic+=1)=ccyymmdd
56740 ! old program !    mask(ic+=1)=pointtwo
56760 ! old program !    mask(ic+=1)=0
56780 ! old program !    mask(ic+=1)=0
56800 ! old program !    mask(ic+=1)=0
56820 ! old program !    mask(ic+=1)=0
56840 ! old program !  ! ** Combo Boxes **
56860 ! old program !  ! CL=Field Number  : C$(CL,1)='ComboF'
56880 ! old program !  ! C$(CL,2)=Linked File Name
56900 ! old program !  ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
56920 ! old program !  ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
56940 ! old program !  ! C$(CL,7)=Index File
56960 ! old program !  ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
56980 ! old program !    limit_to_list$='1'
57000 ! old program !    cl=1 : c$(cl,1)='ComboF'
57020 ! old program !    c$(cl,2)=env$('Q')&"\TMmstr\Clmstr.h420"
57040 ! old program !    c$(cl,3)='1' : c$(cl,4)='5'
57060 ! old program !  ! c$(cl,3)='1' : c$(cl,4)='6'
57080 ! old program !    c$(cl,5)='6' : c$(cl,6)='30'
57100 ! old program !  ! c$(cl,5)='7' : c$(cl,6)='50'
57120 ! old program !    c$(cl,7)=env$('Q')&"\TMmstr\CLIndex.h420"
57140 ! old program !    c$(cl,8)=limit_to_list$
57160 ! old program !    cl=3 : c$(cl,1)='ComboF'
57180 ! old program !    c$(cl,2)=env$('Q')&"\TMmstr\Systems.h420"
57200 ! old program !    c$(cl,3)='1' : c$(cl,4)='2'
57220 ! old program !    c$(cl,5)='3' : c$(cl,6)='50'
57240 ! old program !    c$(cl,7)=env$('Q')&"\TMmstr\Systems-Idx.h420"
57260 ! old program !    c$(cl,8)=limit_to_list$
57280 ! old program !    cl=5 : c$(cl,1)='ComboF'
57300 ! old program !    c$(cl,2)=env$('Q')&"\TMmstr\TimeFrame.h420"
57320 ! old program !    c$(cl,3)='1' : c$(cl,4)='2'
57340 ! old program !    c$(cl,5)='3' : c$(cl,6)='50'
57360 ! old program !    c$(cl,7)=env$('Q')&"\TMmstr\TimeFrame-Idx.h420"
57380 ! old program !    c$(cl,8)=limit_to_list$
57400 ! old program !    return 
57420 ! old program !  ! ______________________________________________________________________
57440 ! old program !  HAMSTER: ! 
57460 ! old program !    fnHamster("Support",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
57480 ! old program !    return 
57500 ! old program !  ! ______________________________________________________________________
57520 ! old program !  XIT: fnxit
57540 ! old program !  ! ______________________________________________________________________
57560 ! old program !  ! <Updateable Region: ERTN>
57580 ! old program !  ERTN: fnerror(program$,err,line,act$,"xit")
57600 ! old program !    if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
57620 ! old program !    execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
57640 ! old program !    pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
57660 ! old program !  ERTN_EXEC_ACT: execute act$ : goto ERTN
57680 ! old program !  ! /region
57700 ! old program !  ! ______________________________________________________________________
