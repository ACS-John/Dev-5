00100 ! ______________________________________________________________________
00220   library 'S:\Core\Library': fntop,fnxit, fnerror,fnmsgbox,fnHamster,fnH2Init,fnH2AddText,fnHamster2AddCombo,fnH2AddComboF,fnH2AddComboA,fnHamster2,fnhand_held_device$
00300   on error goto ERTN
00400 ! ______________________________________________________________________
00700   fntop(program$)
00900   fn_setup_hamster
01000   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
01100   ! fnHamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
01110   fnHamster2("workorder")
01200   gosub CLOSE_FILE
01300   goto XIT
01400 ! ______________________________________________________________________
01500 OPEN_FILE: ! 
01600   open_file_count=0 ! this value is used in the close_file sub routine
01700   open #open_file_count+=1: "Name="&env$('Q')&"\UBmstr\workorder.h"&env$('cno')&",Use,RecL=600,Shr",internal,outIn,relative
01720 ! open #open_file_count+=1: "Name="&env$('Q')&"\UBmstr\workorder.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\wkindex.h"&env$('cno')&",Use,RecL=600,KPs=1/11,KLn=10/8,Shr",internal,outIn,keyed 
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
35000 def fn_setup_hamster
35020   mask_pointtwo=32 : mask_number=30
35040   mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
35060   textlen_mmddyy=8 : textlen_ccyymmdd=10
35080   storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35100   ! 
35120   dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
35140   mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
35160   mask_pointtwo=32 : mask_number=30
35180   mask_ccyymmdd=3 : mask_mmddyy=1 : mask_glnumber=53
35200   textlen_mmddyy=8 : textlen_ccyymmdd=10
35220   storage_len_mmddyy=6 : storage_len_ccyymmdd=8
35240   fnH2Init
35260   !
36000   fnH2AddText("Account  "       ,10              ,'C'                                    )    
36020   ! fnH2AddText("Date"            ,8,'N',8,mask_number )    
36022   fnH2AddText("Date"            ,textlen_ccyymmdd,'N',storage_len_ccyymmdd,mask_ccyymmdd )    
36040   fnH2AddText("e$(2)(1:30)"     ,30              ,'C'                                    )    
36060   fnH2AddText("line$(1)"        ,100             ,'C'                                    )    
36080   fnH2AddText("line$(2)"        ,100             ,'C'                                    )    
36100   fnH2AddText("line$(3)"        ,100             ,'C'                                    )    
36120   fnH2AddText("line$(4)"        ,100             ,'C'                                    )    
36140   ! fnH2AddText("line$(5)"        ,100             ,'C'                                    ) ! adding this line causes err 58 - at least with Pennington it does   
36160   !
36180     fnH2AddComboF(1,env$('Q')&'\UBmstr\Customer.h'&env$('cno'),1,10,41,30,env$('Q')&'\UBmstr\ubIndex.h'&env$('cno'),0)
36200   !
36400 fnend

