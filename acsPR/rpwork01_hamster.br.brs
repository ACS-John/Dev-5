00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnmsgbox,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnH2AddComboF,fnHamster2
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00050   dim cap$*128
00060 ! ______________________________________________________________________
00070   fntop(program$,cap$='RPWork01 Hamster')
00190   fn_hamster_setup
00200   fn_open_file : fn_close_file : fn_open_file
00210   fnHamster2("RPWork")
00220   fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: 'Name='&env$('Q')&'\PRmstr\rpwork01.h'&env$('cno')&',Version=0,KFName='&env$('Q')&'\PRmstr\rpwork01Idx.h'&env$('cno')&',Use,RecL=167,KPs=1,KLn=11,Shr',internal,outIn,keyed 
00280   fnend 
00290   def fn_close_file
00300     for j=1 to open_file_count : close #j: : next j
00310   fnend  ! fn_close_file
00320 XIT: fnxit
00330 ! ______________________________________________________________________
00340 ! region
00350 ERTN: fnerror(program$,err,line,act$,"xit")
00360   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00370   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
20000   def fn_hamster_setup
20020     mask_pointtwo=32 : mask_number=30
20040     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
20060     textlen_mmddyy=8 : textlen_ccyymmdd=10
20080     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
20100     fnH2Init
20120 !     fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
20140 ! dim inp(29),hr(2)
20160 ! eno,dep,mat inp,gpd,mat hr
20180 ! F_RPWORK: form pos 1,n 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
20200     fnH2AddText("eno",8)
20220     fnH2AddText("dep",3)
20240     fnH2AddText("Regular Hours  inp(01)",7,"PD",4.2,mask_pointtwo,12)
20260     fnH2AddText("OverTime Hours inp(02)",7,"PD",4.2,mask_pointtwo,16)
20280     fnH2AddText("Vacation Hours inp(03)",7,"PD",4.2,mask_pointtwo,20)
20300     fnH2AddText("Sick Hours     inp(04)",7,"PD",4.2,mask_pointtwo,24)
20320     fnH2AddText("Holiday Hours  inp(05)",7,"PD",4.2,mask_pointtwo,28)
20340     fnH2AddText("Other Compensa inp(06)",7,"PD",5.2,mask_pointtwo,32)
20360     fnH2AddText("inp(07)",7,"PD",5.2,mask_pointtwo,37)
20380     fnH2AddText("inp(08)",7,"PD",5.2,mask_pointtwo,42)
20400     fnH2AddText("inp(09)",7,"PD",5.2,mask_pointtwo,47)
20420     fnH2AddText("inp(10)",7,"PD",5.2,mask_pointtwo,52)
20440     fnH2AddText("inp(11)",7,"PD",5.2,mask_pointtwo,57)
20460     fnH2AddText("inp(12)",7,"PD",5.2,mask_pointtwo,62)
20480     fnH2AddText("inp(13)",7,"PD",5.2,mask_pointtwo,67)
20500     fnH2AddText("inp(14)",7,"PD",5.2,mask_pointtwo,72)
20520     fnH2AddText("inp(15)",7,"PD",5.2,mask_pointtwo,77)
20540     fnH2AddText("inp(16)",7,"PD",5.2,mask_pointtwo,82)
20560     fnH2AddText("inp(17)",7,"PD",5.2,mask_pointtwo,87)
20580     fnH2AddText("inp(18)",7,"PD",5.2,mask_pointtwo,92)
20600     fnH2AddText("inp(19)",7,"PD",5.2,mask_pointtwo,97)
20620     fnH2AddText("inp(20)",7,"PD",5.2,mask_pointtwo,102)
20640     fnH2AddText("inp(21)",7,"PD",5.2,mask_pointtwo,107)
20660     fnH2AddText("inp(22)",7,"PD",5.2,mask_pointtwo,112)
20680     fnH2AddText("inp(23)",7,"PD",5.2,mask_pointtwo,117)
20700     fnH2AddText("inp(24)",7,"PD",5.2,mask_pointtwo,122)
20720     fnH2AddText("inp(25)",7,"PD",5.2,mask_pointtwo,127)
20740     fnH2AddText("inp(26)",7,"PD",5.2,mask_pointtwo,132)
20760     fnH2AddText("inp(27)",7,"PD",5.2,mask_pointtwo,137)
20780     fnH2AddText("inp(28)",7,"PD",5.2,mask_pointtwo,142)
20800     fnH2AddText("inp(29)",7,"PD",5.2,mask_pointtwo,147)
20820     fnH2AddText("GPD",7,"PD",5.2,mask_pointtwo,152)
20840     fnH2AddText("hr(1)",7,"PD",4.2,mask_pointtwo,157)
20860     fnH2AddText("hr(2)",7,"PD",4.2,mask_pointtwo,161)
20880   fnend 
