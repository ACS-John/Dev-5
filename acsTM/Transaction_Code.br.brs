00010 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnH2AddComboF,fnHamster2,fnhand_held_device$
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00050   dim cap$*128
00060 ! ______________________________________________________________________
00070   fntop(program$,cap$='TM Transaction Code')
00190   fn_hamster_setup
00200   fn_open_file : fn_close_file : fn_open_file
00210   fnHamster2("TransactionCode")
00220   fn_close_file
00230   goto XIT
00240 ! ______________________________________________________________________
00250   def fn_open_file
00260     open_file_count=0 ! this value is used in the close_file sub routine
00270     open #open_file_count+=1: "Name=S:\Core\Data\TransactionCode.dat,Version=1,KFName=S:\Core\Data\TransactionCode.idx,Use,RecL=41,KPs=1,KLn=1,Shr",internal,outIn,keyed 
00280   fnend 
00290   def fn_close_file
00300     for j=1 to open_file_count : close #j: : next j
00310   fnend  ! fn_close_file
00320 XIT: fnxit
00330 ! ______________________________________________________________________
00340 ! <Updateable Region: ERTN>
00350 ERTN: fnerror(program$,err,line,act$,"xit")
00360   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00370   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00380   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00390 ERTN_EXEC_ACT: execute act$ : goto ERTN
00400 ! /region
00410 ! ______________________________________________________________________
50000   def fn_hamster_setup
50020     mask_pointtwo=32 : mask_number=30
50040     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
50060     textlen_mmddyy=8 : textlen_ccyymmdd=10
50080     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
50100     fnH2Init
50140     fnH2AddText("Code",1,'N')
50160     fnH2AddText("Description",18)
50240   fnend 
