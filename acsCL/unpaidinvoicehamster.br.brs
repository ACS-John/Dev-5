00010 ! Replace S:\acsCL\UnpaidInvoiceHamster
00020 ! Checkbook UnpaidInvoice File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070 ! Dimension Ony the Arrays you need (this is important for Hamster) !:
        ! Additionally Never use exactaly 10 items in a file.
00080   dim cap$*128,lbl$(15)*38,tln(15),p$(15)*160,fltyp$(15),mask(15),sln(15)
00090   dim c$(15,8)*40
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Unpaid Invoice (Hamster)")
00120   fncno(cno)
00130   gosub BUILD_LAYOUT
00140   gosub OPEN_AND_CALL_HAMSTER
00150   goto XIT
00160 ! ______________________________________________________________________
00170 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00180   open #first_file=open_file_count+=1: "Name=[Q]\CLmstr\PayTrans.h[cno],Version=2,KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Use,RecL=114,KPs=1,KLn=20,Shr",internal,outIn,keyed 
00190   open #open_file_count+=1: "Name=[Q]\CLmstr\PayTrans.h[cno],Version=2,KFName=[Q]\CLmstr\UnPdIdx2.h[cno],Use,RecL=114,KPs=31/27/1,KLn=2/4/26,Shr",internal,outIn,keyed 
00200   return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230 ! ** Field Labels **
00240   lbl$(1)="Vendor Key" !:
        lbl$(2)="Invoice Key" !:
        lbl$(3)="Invoice Date" !:
        lbl$(4)="Due Date" !:
        lbl$(5)="Purchase Order Number"
00250   lbl$(6)="Description" !:
        lbl$(7)="Amount" !:
        lbl$(8)="Payment Code" !:
        lbl$(9)="Bank" !:
        lbl$(10)="Check Number"
00260   lbl$(11)="Date Paid" !:
        lbl$(12)="Posting Code" !:
        lbl$(13)="Posting Date" !:
        lbl$(14)="Discount Amount" !:
        lbl$(15)="Discount Due Date"
00270 ! ** Field Display Lengths ** !:
        mmddyy=8 : ccyymmdd=10 !:
        ! TC=0 ! Text Box Length Item Coutner
00280   tln(tc+=1)=8 !:
        tln(tc+=1)=12 !:
        tln(tc+=1)=mmddyy !:
        tln(tc+=1)=mmddyy !:
        tln(tc+=1)=12
00290   tln(tc+=1)=18 !:
        tln(tc+=1)=10 !:
        tln(tc+=1)=1 !:
        tln(tc+=1)=2 !:
        tln(tc+=1)=8
00300   tln(tc+=1)=mmddyy !:
        tln(tc+=1)=1 !:
        tln(tc+=1)=6 !:
        tln(tc+=1)=10 !:
        tln(tc+=1)=ccyymmdd
00310 ! ** Field Types ** !:
        ! Valid are C, G, N, PD, !:
        ! Default is 'G' !:
        fc=0 ! Field Type Item Counter
00320   fltyp$(fc+=1)='Cr' !:
        fltyp$(fc+=1)='C' !:
        fltyp$(fc+=1)='G' !:
        fltyp$(fc+=1)='G' !:
        fltyp$(fc+=1)='C'
00330   fltyp$(fc+=1)='C' !:
        fltyp$(fc+=1)='N' !:
        fltyp$(fc+=1)='N' !:
        fltyp$(fc+=1)='N' !:
        fltyp$(fc+=1)='G'
00340   fltyp$(fc+=1)='G' !:
        fltyp$(fc+=1)='G' ! XXX !:
        fltyp$(fc+=1)='G' ! XXX !:
        fltyp$(fc+=1)='N' !:
        fltyp$(fc+=1)='N'
00350 ! ** Field Storage Lengths ** !:
        ! sc=0 ! Field Storage Length Item Counter
00360   sln(sc+=1)=8 !:
        sln(sc+=1)=12 !:
        sln(sc+=1)=6 !:
        sln(sc+=1)=6 !:
        sln(sc+=1)=12
00370   sln(sc+=1)=18 !:
        sln(sc+=1)=10.2 !:
        sln(sc+=1)=1 !:
        sln(sc+=1)=2 !:
        sln(sc+=1)=8
00380   sln(sc+=1)=6 !:
        sln(sc+=1)=1 !:
        sln(sc+=1)=6 !:
        sln(sc+=1)=10.2 !:
        sln(sc+=1)=8
00390 ! ** Field Mask ** !:
        number=30 !:
        pointtwo=32 ! number with 2 decimal places (no commas)!:
        mmddyy=1 : ccyymmdd=3 !:
        mc=0 ! mask item counter
00400   mask(mc+=1)=none !:
        mask(mc+=1)=none !:
        mask(mc+=1)=mmddyy !:
        mask(mc+=1)=mmddyy !:
        mask(mc+=1)=none
00410   mask(mc+=1)=none !:
        mask(mc+=1)=pointtwo !:
        mask(mc+=1)=number !:
        mask(mc+=1)=number !:
        mask(mc+=1)=number
00420   mask(mc+=1)=mmddyy !:
        mask(mc+=1)=number !:
        mask(mc+=1)=mmddyy !:
        mask(mc+=1)=pointtwo !:
        mask(mc+=1)=mmddyy
00430 ! ** Storage Position ** !:
        sc=0 ! Storage Position Item Counter
00440 ! ** Let's Make Some Combo Boxes ** !:
        ! CL = Item you want a ComboBox on !:
        ! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len !:
        ! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File !:
        ! C$(cl,8)=limit to list option '1'=yes '0'=no !:
        limit_to_list$='1'
00450   cl=8 !:
        c$(cl,1)='ComboF' !:
        c$(cl,2)="[Q]\CLmstr\PaymentCode.dat" !:
        c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) !:
        c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='25' !:
        c$(cl,7)="[Q]\CLmstr\PaymentCode.Idx" : c$(cl,8)=limit_to_list$
00460   cl=9 !:
        c$(cl,1)='ComboF' !:
        c$(cl,2)="[Q]\CLmstr\BankMstr.h[cno]" !:
        c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) !:
        c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='30' !:
        c$(cl,7)="[Q]\CLmstr\BankIdx1.h[cno]" : c$(cl,8)=limit_to_list$
00470   cl=12 !:
        c$(cl,1)='ComboF' !:
        c$(cl,2)="S:\acsCL\PostingCode.dat" !:
        c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) !:
        c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='25' !:
        c$(cl,7)="S:\acsCL\PostingCode.idx" : c$(cl,8)=limit_to_list$
00480   return 
00490 ! ______________________________________________________________________
00500 OPEN_AND_CALL_HAMSTER: ! 
00510 ! if the file is created and you open it, indexs do not get updated correctly, until you close it and reopen it.
00520   gosub OPEN_FILE
00530   for j=1 to open_file_count !:
          close #j: !:
        next j
00540   gosub OPEN_FILE
00550   fnHamster("UnpaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00560   return 
00570 ! ______________________________________________________________________
00580 XIT: fnxit
00590 ! ______________________________________________________________________
00600 ! <Updateable Region: ERTN>
00610 ERTN: fnerror(program$,err,line,act$,"xit")
00620   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00630   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00640   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00650 ERTN_EXEC_ACT: execute act$ : goto ERTN
00660 ! /region
00670 ! ______________________________________________________________________
