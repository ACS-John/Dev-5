00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnH2AddComboF,fnHamster2
00070   fntop(program$)
00090   dim srvnam$(10)*20,srv$(10)*2
00190   fn_hamster_setup
00200   fn_open_file : fn_close_file : fn_open_file
00210   fnHamster2("GLmstr")
00220   fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: "Name=[Q]\GLmstr\GLmstr.h[cno],Version=0,KFName=[Q]\GLmstr\glIndx2.h[cno],Use,RecL=416,KPs=13,KLn=30,Shr",internal,outIn,keyed 
00280   fnend 
00290   def fn_close_file
00300     for j=1 to open_file_count : close #j: : next j
00310   fnend  ! fn_close_file
00320 XIT: fnxit
00410 ! ______________________________________________________________________
00420   def fn_hamster_setup
00430     mask_pointtwo=32 : mask_number=30
00440     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
00450     textlen_mmddyy=8 : textlen_ccyymmdd=10
00460     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
00470     fnH2Init
00480 !     fn_hamster_field_add(label$*38,textbox_len,field_type$*2; storage_length,ar_mask,storage_position)
00490     fnH2AddText("Account",12)
00492     fnH2AddText("Description",50,"C",50,0,13)
00500     fnH2AddText("Income Stmt Ref",5,"PD",3,mask_number,69)
00501     fnH2AddText("Beginning Balance",11,"PD",6,mask_pointtwo,81)
00502     fnH2AddText("Current Balance",11,"PD",6,mask_pointtwo,87)
00503     fnH2AddText("2-Yr Beginning",11,"PD",6,mask_pointtwo,327)
00520   fnend 
00530 ! 
