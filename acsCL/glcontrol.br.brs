00010 ! Replace S:\Core\GLControl
00020 ! Checkbook Transaction Allocation File - Hamster !:
        ! pretty useless to the end user - but quite usefull to the programmer
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(6)*38,tln(6),p$(6)*160,fltyp$(6),sln(6),mask(6),c$(6,8)*40
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$='General Ledger Control File')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\FundMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\FundIdx1.h"&env$('cno')&",Use,RecL=75,KPs=1,KLn=3,Shr",internal,outin,keyed 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   fncno(cno)
00220 ! ** Field Labels    ** !:
        ic=0 ! temporary Item Counter
00230   lbl$(ic+=1)="Fund Number" !:
        lbl$(ic+=1)="Description" !:
        lbl$(ic+=1)="General Ledger Number Due To" !:
        lbl$(ic+=1)="General Ledger Number Due From" !:
        lbl$(ic+=1)="General Ledger Number Accounts Payable" !:
        lbl$(ic+=1)="General Ledger Number for Discounts"
00240 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00250   tln(ic+=1)=3 !:
        tln(ic+=1)=30 !:
        tln(ic+=1)=9 !:
        tln(ic+=1)=9 !:
        tln(ic+=1)=12 !:
        tln(ic+=1)=12
00260 ! ** Field Types ** !:
        ic=0
00270   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C'
00280 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00290   sln(ic+=1)=3 !:
        sln(ic+=1)=30 !:
        sln(ic+=1)=9 !:
        sln(ic+=1)=9 !:
        sln(ic+=1)=12 !:
        sln(ic+=1)=12
00300 ! ** Field Masks ** !:
        ic=0 !:
        pointtwo=32 : number=30 !:
        ccyymmdd=3 : mmddyy=1 : glnumber=53
00310   mask(ic+=1)=1000+number !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0
00320 ! ** Combo Boxes **                                                   !:
        ! CL=Field Number  : C$(CL,1)='ComboF'                                !:
        ! C$(CL,2)=Linked File Name                                           !:
        ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 !:
        ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         !:
        ! C$(CL,7)=Index File                                                 !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        limit_to_list$='1'
00330   cl=3: c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\GLmstr.h"&env$('cno') !:
        c$(cl,3)='4' : c$(cl,4)='9' !:
        c$(cl,5)='13' : c$(cl,6)='50' !:
        c$(cl,7)=env$('Q')&"\CLmstr\GLIndex.h"&env$('cno') : c$(cl,8)=limit_to_list$
00340   cl=4: c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\GLmstr.h"&env$('cno') !:
        c$(cl,3)='4' : c$(cl,4)='9' !:
        c$(cl,5)='13' : c$(cl,6)='50' !:
        c$(cl,7)=env$('Q')&"\CLmstr\GLIndex.h"&env$('cno') : c$(cl,8)='1'
00350   cl=5: c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\GLmstr.h"&env$('cno') !:
        c$(cl,3)='1' : c$(cl,4)='12' !:
        c$(cl,5)='13' : c$(cl,6)='50' !:
        c$(cl,7)=env$('Q')&"\CLmstr\GLIndex.h"&env$('cno') : c$(cl,8)='1'
00352   cl=6: c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\GLmstr.h"&env$('cno') !:
        c$(cl,3)='1' : c$(cl,4)='12' !:
        c$(cl,5)='13' : c$(cl,6)='50' !:
        c$(cl,7)=env$('Q')&"\CLmstr\GLIndex.h"&env$('cno') : c$(cl,8)='1'
00360   return 
00370 ! ______________________________________________________________________
00380 HAMSTER: ! 
00390   fnhamster("GLControl",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00400   return 
00410 ! ______________________________________________________________________
00420 XIT: fnxit
00430 ! ______________________________________________________________________
00440 ! <Updateable Region: ERTN>
00450 ERTN: fnerror(program$,err,line,act$,"xit")
00460   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! /region
00510 ! ______________________________________________________________________
