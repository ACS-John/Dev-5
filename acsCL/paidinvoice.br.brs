00010 ! Replace S:\acsCL\PaidInvoice
00020 ! Checkbook PaidInvoice File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! Dimension Ony the Arrays you need (this is important for Hamster) !:
        ! Additionally Never use exactaly 10 items in a file.
00080   dim cap$*128,lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),mask(4),sln(4)
00090   dim c$(4,8)*256
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Paid Invoice")
00120   fncno(cno)
00130   gosub BUILD_LAYOUT
00140   gosub OPEN_AND_CALL_HAMSTER
00150   goto XIT
00160 ! ______________________________________________________________________
00170 OPEN_FILE: ! (open_file_count)
00180   open_file_count=0
00190   open #open_file_count+=1: "Name="&env$('Q')&"\CLmstr\IvPaid.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\IVIndex.h"&env$('cno')&",Use,RecL=34,KPs=1,KLn=20,Shr",internal,outin,keyed  !:
        ! Version=0 or Version=1
00200   return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230 ! ** Field Labels **
00240   lbl$(1)="Vendor Key" !:
        lbl$(2)="Invoice Key" !:
        lbl$(3)="Date Paid" !:
        lbl$(4)="Check Number"
00250 ! ** Field Display Lengths ** !:
        mmddyy=8 : ccyymmdd=10 !:
        ! TC=0 ! Text Box Length Item Coutner
00260   tln(tc+=1)=8 !:
        tln(tc+=1)=12 !:
        tln(tc+=1)=mmddyy !:
        tln(tc+=1)=8
00270 ! ** Field Types ** !:
        ! Valid are C, G, N, PD, !:
        ! Default is 'G' !:
        fc=0 ! Field Type Item Counter
00280   fltyp$(fc+=1)='C' !:
        fltyp$(fc+=1)='C' !:
        fltyp$(fc+=1)='G' !:
        fltyp$(fc+=1)='G'
00290 ! ** Field Storage Lengths ** !:
        ! sc=0 ! Field Storage Length Item Counter
00300   sln(sc+=1)=8 !:
        sln(sc+=1)=12 !:
        sln(sc+=1)=6 !:
        sln(sc+=1)=8
00310 ! ** Field Mask ** !:
        number=30 !:
        pointtwo=32 ! number with 2 decimal places (no commas)!:
        mmddyy=1 : ccyymmdd=3 !:
        mc=0 ! mask item counter
00320   mask(mc+=1)=none !:
        mask(mc+=1)=none !:
        mask(mc+=1)=mmddyy !:
        mask(mc+=1)=number
00330 ! ** Storage Position ** !:
        sc=0 ! Storage Position Item Counter
00340 ! ** Let's Make Some Combo Boxes ** !:
        ! CL = Item you want a ComboBox on !:
        ! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len !:
        ! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File !:
        ! C$(cl,8)=limit to list option '1'=yes '0'=no !:
        limit_to_list$='1'
00350   cl=1 !:
        c$(cl,1)='ComboF' !:
        c$(cl,2)=env$('Q')&"\CLmstr\PayMstr.h"&env$('cno') !:
        c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) !:
        c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='30' !:
        c$(cl,7)=env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno') : c$(cl,8)=limit_to_list$
00360 ! cL=2 !:
        ! c$(CL,1)='ComboF' !:
        ! c$(CL,2)=env$('Q')&'\CLmstr\BankMstr.h'&env$('cno') !:
        ! c$(CL,3)='1' : c$(CL,4)=STR$(SLN(CL)) !:
        ! c$(CL,5)=STR$(SLN(CL)+1) : c$(CL,6)='30' !:
        ! c$(CL,7)=env$('Q')&'\CLmstr\BankIdx1.h'&env$('cno') : c$(CL,8)=LIMIT_TO_LIST$
00370 ! cL=4 !:
        ! c$(CL,1)='ComboF' !:
        ! c$(CL,2)=env$('Q')&'\CLmstr\TrMstr.h'&env$('cno') !:
        ! c$(CL,3)='1' : c$(CL,4)=STR$(SLN(CL)) !:
        ! c$(CL,5)=STR$(SLN(CL)+1) : c$(CL,6)='30' !:
        ! c$(CL,7)=env$('Q')&'\CLmstr\BankIdx1.h'&env$('cno') : c$(CL,8)=LIMIT_TO_LIST$
00380   return 
00390 ! ______________________________________________________________________
00400 OPEN_AND_CALL_HAMSTER: ! 
00410 ! if the file is created and you open it, indexs do not get updated correctly, until you close it and reopen it.
00420   gosub OPEN_FILE
00430   for j=1 to open_file_count !:
          close #j: !:
        next j
00440   gosub OPEN_FILE
00450   fnhamster("PaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00460   return 
00470 ! ______________________________________________________________________
00480 XIT: fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: fnerror(program$,err,line,act$,"xit")
00520   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
