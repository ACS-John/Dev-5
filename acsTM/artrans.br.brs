00100 ! ______________________________________________________________________
00200 ! library 'S:\Core\Library': fntop,fnxit, fnerror,fnHamster
00220   library 'S:\Core\Library': fntop,fnxit,fnerror,fnmsgbox,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnHamster2,fnhand_held_device$

00300   on error goto ERTN
00400 ! ______________________________________________________________________
00600 ! ______________________________________________________________________
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
01700   open #open_file_count+=1: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Version=0,Use,RecL=60,Shr",internal,outIn,relative 
01800   return 
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
35400     mask_pointtwo=32 : mask_number=30
35420     mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
35440     textlen_mmddyy=8 : textlen_ccyymmdd=10
35460     storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35480     fnH2Init
35500     fnH2AddText("Client ID",5)
35520     fnH2AddText("Invoice Number",12)
35540     fnH2AddText("Date",6,'N',0,mask_date) ! 30 (mask_number) =no decimals, no commas
35560     fnH2AddText("Origional Amount",10,'PD',5.2,mask_pointtwo)
35580     fnH2AddText("Amount",10,'PD',5.2,mask_pointtwo)
35600     fnH2AddText("Salesman Number",3,'PD',2,mask_pointtwo)
35620     itemTCode=fnH2AddText("Trans Code",1,'N',0,mask_number)
35630 !   fnH2AddComboF(fnH2AddText("Trans Code",1,'N',0,mask_number),'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
35640     fnH2AddText("Posting Code",1,'N',0,mask_number)
35660     fnH2AddText("Invoice Description",20,'C')
35680     fnH2AddText("Next Trans Addr",5,'PD',3)
35700     fnH2AddComboF(itemTCode,'S:\Core\Data\TransactionCode.dat',1,1,2,40,'S:\Core\Data\TransactionCode.idx',1)
35720   fnend  ! fn_setup_hamster
