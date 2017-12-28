00010 ! Replace S:\acsCL\TransactionHamster
00020 ! Checkbook Transaction File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),mask(11),sln(11),c$(11,8)*40
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$='Transaction (Hamster)')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",Version=2,KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Use,RecL=78,KPs=1,KLn=11,Shr",internal,outIn,keyed 
00160   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",Version=2,KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Use,RecL=78,KPs=28/1,KLn=8/11,Shr",internal,outIn,keyed 
00170   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",Version=2,KFName="&env$('Q')&"\CLmstr\TrIdx3.h"&env$('cno')&",Use,RecL=78,KPs=16/12/4,KLn=2/4/8,Shr",internal,outIn,keyed 
00180   return 
00190 ! ______________________________________________________________________
00200 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230   fncno(cno)
00240   lbl$(1)="Bank" !:
        lbl$(2)="Transaction Code" !:
        lbl$(3)="Check/Reference Number" !:
        lbl$(4)="Check Date" !:
        lbl$(5)="Amount"
00250   lbl$(6)="Payee" !:
        lbl$(7)="Name/Description (1 of 2)" !:
        lbl$(8)="Name/Description (2 of 2)" !:
        lbl$(9)="Posting Code" !:
        lbl$(10)="Statement Date Cleared"
00260   lbl$(11)="Source Code"
00270 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00280   tln(ic+=1)=2 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=8 !:
        tln(ic+=1)=mmddyy !:
        tln(ic+=1)=19
00290   tln(ic+=1)=8 !:
        tln(ic+=1)=34 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=1 !:
        tln(ic+=1)=mmddyy
00300   tln(ic+=1)=1
00310 ! ** Field Types ** !:
        ic=0
00320   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='Cr' !:
        fltyp$(ic+=1)='G' !:
        fltyp$(ic+=1)='PD'
00330   fltyp$(ic+=1)='Cr' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='N'
00340   fltyp$(ic+=1)='N'
00350 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00360   sln(ic+=1)=2 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=8 !:
        sln(ic+=1)=mmddyy !:
        sln(ic+=1)=10.2
00370   sln(ic+=1)=8 !:
        sln(ic+=1)=34 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=1 !:
        sln(ic+=1)=mmddyy
00380   sln(ic+=1)=1
00390 ! ** Field Masks ** !:
        ic=0 !:
        pointtwo=32 : number=30 !:
        ccyymmdd=3 : mmddyy=1
00400   mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=mmddyy !:
        mask(ic+=1)=pointtwo
00410   mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=0 !:
        mask(ic+=1)=number !:
        mask(ic+=1)=mmddyy
00420   mask(ic+=1)=number
00430 ! ** Storage Positions ** !:
        ! default to the same as order displayed !:
        ic=0
00440 ! ** Combo Boxes **                                                   !:
        ! CL=Field Number  : C$(CL,1)='ComboF'                                !:
        ! C$(CL,2)=Linked File Name                                           !:
        ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 !:
        ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         !:
        ! C$(CL,7)=Index File                                                 !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        limit_to_list$='1'
00450   cl=1 : c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\BankMstr.h"&env$('cno') !:
        c$(cl,3)='1' : c$(cl,4)='2' !:
        c$(cl,5)='3' : c$(cl,6)='30' !:
        c$(cl,7)=env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno') !:
        c$(cl,8)=limit_to_list$
00460   cl=2 : c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\TransactionType.dat" !:
        c$(cl,3)='1' : c$(cl,4)='1' !:
        c$(cl,5)='2' : c$(cl,6)='25' !:
        c$(cl,7)=env$('Q')&"\CLmstr\TransactionType.idx" !:
        c$(cl,8)=limit_to_list$
00470   cl=6 : c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\PayMstr.h"&env$('cno') !:
        c$(cl,3)='1' : c$(cl,4)='8' !:
        c$(cl,5)='9' : c$(cl,6)='30' !:
        c$(cl,7)=env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno') !:
        c$(cl,8)=limit_to_list$
00480   cl=9 : c$(cl,1)='ComboF' !:
        c$(cl,2)="S:\acsCL\PostingCode.dat" !:
        c$(cl,3)='1' : c$(cl,4)='1' !:
        c$(cl,5)='2' : c$(cl,6)='25' !:
        c$(cl,7)="S:\acsCL\PostingCode.idx" !:
        c$(cl,8)=limit_to_list$
00490   cl=11 : c$(cl,1)='ComboF' !:
        c$(cl,2)="S:\acsCL\SourceCode.dat" !:
        c$(cl,3)='1' : c$(cl,4)='1' !:
        c$(cl,5)='2' : c$(cl,6)='25' !:
        c$(cl,7)="S:\acsCL\SourceCode.idx" !:
        c$(cl,8)=limit_to_list$
00500   return 
00510 ! ______________________________________________________________________
00520 HAMSTER: ! 
00530   fnHamster("Transaction",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00540   return 
00550 ! ______________________________________________________________________
00560 XIT: fnxit
00570 ! ______________________________________________________________________
00580 ! <Updateable Region: ERTN>
00590 ERTN: fnerror(program$,err,line,act$,"xit")
00600   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00610   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00620   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00630 ERTN_EXEC_ACT: execute act$ : goto ERTN
00640 ! /region
00650 ! ______________________________________________________________________
