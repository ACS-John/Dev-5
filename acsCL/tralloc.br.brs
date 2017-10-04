00010 ! Replace S:\acsCL\TrAlloc
00020 ! Checkbook Transaction Allocation File - Hamster !:
        ! pretty useless to the end user - but quite usefull to the programmer
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(9)*38,tln(9),p$(9)*160,fltyp$(9),sln(9),mask(9),sp(9),c$(9,8)*40
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$='Transaction Allocation (Hamster)')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&str$(cno)&",Use,RecL=80,KPs=1,KLn=11,Shr",internal,outin,keyed 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   fncno(cno)
00220 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00230   let lbl$(ic+=1)="Bank Code" !:
        let lbl$(ic+=1)="Transaction Type" !:
        let lbl$(ic+=1)="Check/Reference" !:
        let lbl$(ic+=1)="General Ledger Number" !:
        let lbl$(ic+=1)="Amount"
00240   let lbl$(ic+=1)="Description" !:
        let lbl$(ic+=1)="Invoice Date" !:
        let lbl$(ic+=1)="Purchase Order Number" !:
        let lbl$(ic+=1)="Posting Code"
00250 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        ccyymmdd=10
00260   let tln(ic+=1)=2 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=8 !:
        let tln(ic+=1)=12 !:
        let tln(ic+=1)=9
00270   let tln(ic+=1)=30 !:
        let tln(ic+=1)=mmddyy !:
        let tln(ic+=1)=12 !:
        let tln(ic+=1)=1
00280 ! ** Field Types ** !:
        let ic=0
00290   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='Cr' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='PD'
00300   let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='G' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='N'
00310 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : ccyymmdd=8
00320   let sln(ic+=1)=2 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=8 !:
        let sln(ic+=1)=12 !:
        let sln(ic+=1)=5.2
00330   let sln(ic+=1)=30 !:
        let sln(ic+=1)=6 !:
        let sln(ic+=1)=12 !:
        let sln(ic+=1)=1
00340 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00350   let mask(ic+=1)=number !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=glnumber !:
        let mask(ic+=1)=pointtwo
00360   let mask(ic+=1)=0 !:
        let mask(ic+=1)=mmddyy !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=number
00370 ! ** Storage Positions ** !:
        ! default to the same as order displayed !:
        let ic=0
00380   let sp(ic+=1)=1 !:
        let sp(ic+=1)=3 !:
        let sp(ic+=1)=4 !:
        let sp(ic+=1)=12 !:
        let sp(ic+=1)=24
00390   let sp(ic+=1)=29 !:
        let sp(ic+=1)=59 !:
        let sp(ic+=1)=68 !:
        let sp(ic+=1)=80
00400 ! ** Combo Boxes **                                                   !:
        ! CL=Field Number  : C$(CL,1)='ComboF'                                !:
        ! C$(CL,2)=Linked File Name                                           !:
        ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 !:
        ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         !:
        ! C$(CL,7)=Index File                                                 !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00410 ! cL=1 : c$(CL,1)='ComboF' !:
        ! c$(CL,2)=env$('Q')&'\CLmstr\PayMstr.h'&STR$(CNO) !:
        ! c$(CL,3)='1' : c$(CL,4)='8' !:
        ! c$(CL,5)='9' : c$(CL,6)='30' !:
        ! c$(CL,7)=env$('Q')&'\CLmstr\PayIdx1.h'&STR$(CNO) !:
        ! c$(CL,8)=LIMIT_TO_LIST$
00420   return 
00430 ! ______________________________________________________________________
00440 HAMSTER: ! 
00450   fnhamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00460   return 
00470 ! ______________________________________________________________________
00480 XIT: let fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: let fnerror(program$,err,line,act$,"xit")
00520   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
