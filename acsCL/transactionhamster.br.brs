00010 ! Replace S:\acsCL\TransactionHamster
00020 ! Checkbook Transaction File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),mask(11),sln(11),c$(11,8)*40
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='Transaction (Hamster)')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&str$(cno)&",Use,RecL=78,KPs=1,KLn=11,Shr",internal,outin,keyed 
00160   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&str$(cno)&",Use,RecL=78,KPs=28/1,KLn=8/11,Shr",internal,outin,keyed 
00170   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&str$(cno)&",Version=2,KFName="&env$('Q')&"\CLmstr\TrIdx3.h"&str$(cno)&",Use,RecL=78,KPs=16/12/4,KLn=2/4/8,Shr",internal,outin,keyed 
00180   return 
00190 ! ______________________________________________________________________
00200 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230   let fncno(cno)
00240   let lbl$(1)="Bank" !:
        let lbl$(2)="Transaction Code" !:
        let lbl$(3)="Check/Reference Number" !:
        let lbl$(4)="Check Date" !:
        let lbl$(5)="Amount"
00250   let lbl$(6)="Payee" !:
        let lbl$(7)="Name/Description (1 of 2)" !:
        let lbl$(8)="Name/Description (2 of 2)" !:
        let lbl$(9)="Posting Code" !:
        let lbl$(10)="Statement Date Cleared"
00260   let lbl$(11)="Source Code"
00270 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00280   let tln(ic+=1)=2 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=8 !:
        let tln(ic+=1)=mmddyy !:
        let tln(ic+=1)=19
00290   let tln(ic+=1)=8 !:
        let tln(ic+=1)=34 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=1 !:
        let tln(ic+=1)=mmddyy
00300   let tln(ic+=1)=1
00310 ! ** Field Types ** !:
        let ic=0
00320   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='Cr' !:
        let fltyp$(ic+=1)='G' !:
        let fltyp$(ic+=1)='PD'
00330   let fltyp$(ic+=1)='Cr' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='N'
00340   let fltyp$(ic+=1)='N'
00350 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00360   let sln(ic+=1)=2 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=8 !:
        let sln(ic+=1)=mmddyy !:
        let sln(ic+=1)=10.2
00370   let sln(ic+=1)=8 !:
        let sln(ic+=1)=34 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=1 !:
        let sln(ic+=1)=mmddyy
00380   let sln(ic+=1)=1
00390 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        let ccyymmdd=3 : let mmddyy=1
00400   let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=mmddyy !:
        let mask(ic+=1)=pointtwo
00410   let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=0 !:
        let mask(ic+=1)=number !:
        let mask(ic+=1)=mmddyy
00420   let mask(ic+=1)=number
00430 ! ** Storage Positions ** !:
        ! default to the same as order displayed !:
        let ic=0
00440 ! ** Combo Boxes **                                                   !:
        ! CL=Field Number  : C$(CL,1)='ComboF'                                !:
        ! C$(CL,2)=Linked File Name                                           !:
        ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 !:
        ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         !:
        ! C$(CL,7)=Index File                                                 !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00450   let cl=1 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\CLmstr\BankMstr.h"&str$(cno) !:
        let c$(cl,3)='1' : let c$(cl,4)='2' !:
        let c$(cl,5)='3' : let c$(cl,6)='30' !:
        let c$(cl,7)=env$('Q')&"\CLmstr\BankIdx1.h"&str$(cno) !:
        let c$(cl,8)=limit_to_list$
00460   let cl=2 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\CLmstr\TransactionType.dat" !:
        let c$(cl,3)='1' : let c$(cl,4)='1' !:
        let c$(cl,5)='2' : let c$(cl,6)='25' !:
        let c$(cl,7)=env$('Q')&"\CLmstr\TransactionType.idx" !:
        let c$(cl,8)=limit_to_list$
00470   let cl=6 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)=env$('Q')&"\CLmstr\PayMstr.h"&str$(cno) !:
        let c$(cl,3)='1' : let c$(cl,4)='8' !:
        let c$(cl,5)='9' : let c$(cl,6)='30' !:
        let c$(cl,7)=env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno) !:
        let c$(cl,8)=limit_to_list$
00480   let cl=9 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)="S:\acsCL\PostingCode.dat" !:
        let c$(cl,3)='1' : let c$(cl,4)='1' !:
        let c$(cl,5)='2' : let c$(cl,6)='25' !:
        let c$(cl,7)="S:\acsCL\PostingCode.idx" !:
        let c$(cl,8)=limit_to_list$
00490   let cl=11 : let c$(cl,1)='ComboF' !:
        let c$(cl,2)="S:\acsCL\SourceCode.dat" !:
        let c$(cl,3)='1' : let c$(cl,4)='1' !:
        let c$(cl,5)='2' : let c$(cl,6)='25' !:
        let c$(cl,7)="S:\acsCL\SourceCode.idx" !:
        let c$(cl,8)=limit_to_list$
00500   return 
00510 ! ______________________________________________________________________
00520 HAMSTER: ! 
00530   let fnhamster("Transaction",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00540   return 
00550 ! ______________________________________________________________________
00560 XIT: let fnxit
00570 ! ______________________________________________________________________
00580 ! <Updateable Region: ERTN>
00590 ERTN: let fnerror(program$,err,line,act$,"xit")
00600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00610   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00620   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00630 ERTN_EXEC_ACT: execute act$ : goto ERTN
00640 ! /region
00650 ! ______________________________________________________________________
