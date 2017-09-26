00010 ! Replace S:\acsTM\Client
00020 ! TM Client - Hamster
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='TMTrans')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER : gosub CLOSE_FILE
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00152   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\TMTrans.h"&str$(cno)&",Version=0,Use,RecL=86,Shr",internal,outin,relative 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   let fncno(cno)
00220 ! 
00300   let fn_setup_hamster
00400 ! 
00420 ! ** Combo Boxes **                                                   !:
        ! CL=Field Number  : C$(CL,1)='ComboF'                                !:
        ! C$(CL,2)=Linked File Name                                           !:
        ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 !:
        ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         !:
        ! C$(CL,7)=Index File                                                 !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00430 ! let cl=1 : let c$(cl,1)='ComboF' !:
        ! let c$(cl,2)=env$('Q')&'\TMmstr\Client.h'&str$(cno) !:
        ! let c$(cl,3)='1' : let c$(cl,4)='6' !:
        ! let c$(cl,5)='7' : let c$(cl,6)='50' !:
        ! let c$(cl,7)=env$('Q')&'\TMmstr\Client-Idx.h'&str$(cno) !:
        ! let c$(cl,8)=limit_to_list$
00440 ! let cl=3 : let c$(cl,1)='ComboF' !:
        ! let c$(cl,2)=env$('Q')&'\TMmstr\Systems.h'&str$(cno) !:
        ! let c$(cl,3)='1' : let c$(cl,4)='2' !:
        ! let c$(cl,5)='3' : let c$(cl,6)='50' !:
        ! let c$(cl,7)=env$('Q')&'\TMmstr\Systems-Idx.h'&str$(cno) !:
        ! let c$(cl,8)=limit_to_list$
00450 ! let cl=5 : let c$(cl,1)='ComboF' !:
        ! let c$(cl,2)=env$('Q')&'\TMmstr\TimeFrame.h'&str$(cno) !:
        ! let c$(cl,3)='1' : let c$(cl,4)='2' !:
        ! let c$(cl,5)='3' : let c$(cl,6)='50' !:
        ! let c$(cl,7)=env$('Q')&'\TMmstr\TimeFrame-Idx.h'&str$(cno) !:
        ! let c$(cl,8)=limit_to_list$
00460   return 
00470 ! ______________________________________________________________________
00480 HAMSTER: ! 
00490   let fnhamster("Client",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00500   return 
00510 ! ______________________________________________________________________
00520 XIT: let fnxit
00530 ! ______________________________________________________________________
00540 ! <Updateable Region: ERTN>
00550 ERTN: let fnerror(program$,err,line,act$,"xit")
00560   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00570   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00580   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00590 ERTN_EXEC_ACT: execute act$ : goto ERTN
00600 ! /region
00610 ! ______________________________________________________________________
70000   def fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask)
70020     if storage_length=0 then let storage_length=textbox_len
70040     let add_rec_item=udim(mat lbl$)+1
70060     mat lbl$(add_rec_item) : let lbl$(add_rec_item)=label$
70080     mat tln(add_rec_item) : let tln(add_rec_item)=textbox_len
70100     mat p$(add_rec_item)
70120     mat fltyp$(add_rec_item) : let fltyp$(add_rec_item)=field_type$
70140     mat sln(add_rec_item) : let sln(add_rec_item)=storage_length
70150     mat mask(add_rec_item) : let mask(add_rec_item)=ar_mask
70160     mat c$(add_rec_item,8)
70180   fnend  ! fn_add_rec
80000   def fn_setup_hamster
80200     let mask_pointtwo=32 : let mask_number=30
80400     let mask_ccyymmdd=3 : let mask_mmddyy=1 : let mask_glnumber=53
80600     let textlen_mmddyy=8 : let textlen_ccyymmdd=10
80800     let storage_len_mmddyy=6 : let storage_len_ccyymmdd=8
81000 ! 
81200     dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),c$(1,8)*40 ! SP(1) - not used
81400     mat lbl$(0) : mat tln(0) : mat p$(0) : mat fltyp$(0) : mat sln(0) : mat mask(0) : mat c$(0,8) : mat sp(0)
81600 ! 
81800 ! fn_add_rec(label$*38,textbox_len,field_type$*2; storage_length,ar_mask)
82000     let fn_add_rec("Client ID",5,'C')
82200     let fn_add_rec("Employee",9,'C')
82400     let fn_add_rec("Hours",6,'PD',3.2,mask_pointtwo)
82600     let fn_add_rec("Hourly Rate",6,'PD',3.2,mask_pointtwo)
82800     let fn_add_rec("Amount",8,'PD',4.2,mask_pointtwo)
83000     let fn_add_rec("Trans Date",6,'N',0,mask_number)
83200     let fn_add_rec("Category Code",2,'N',0,mask_number)
83400     let fn_add_rec("Department Code",3,'PD',2)
83600     let fn_add_rec("Trans Code",2,'PD',1)
83800     let fn_add_rec("System Code",2,'N',0,mask_number)
84000     let fn_add_rec("Service Code",4,'C') ! ph$
84200     let fn_add_rec("Invoice Number",12,'C') ! ph$
84400     let fn_add_rec("Next Trans Addr",5,'PD',3) ! ph$
84600     let fn_add_rec("Description",30,'C') ! ph$
84800   fnend  ! fn_setup_hamster
