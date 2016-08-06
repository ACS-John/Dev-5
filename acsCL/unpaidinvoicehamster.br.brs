00010 ! Replace R:\acsCL\UnpaidInvoiceHamster
00020 ! Check Book UnpaidInvoice File
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! Dimension Ony the Arrays you need (this is important for Hamster) !:
        ! Additionally Never use exactaly 10 items in a file.
00080   dim cap$*128,lbl$(15)*38,tln(15),p$(15)*160,fltyp$(15),mask(15),sln(15)
00090   dim c$(15,8)*40
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Unpaid Invoice (Hamster)")
00120   let fncno(cno)
00130   gosub BUILD_LAYOUT
00140   gosub OPEN_AND_CALL_HAMSTER
00150   goto XIT
00160 ! ______________________________________________________________________
00170 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00180   open #first_file=open_file_count+=1: "Name=Q:\CLmstr\PayTrans.h"&str$(cno)&",Version=2,KFName=Q:\CLmstr\UnPdIdx1.h"&str$(cno)&",Use,RecL=114,KPs=1,KLn=20,Shr",internal,outin,keyed 
00190   open #open_file_count+=1: "Name=Q:\CLmstr\PayTrans.h"&str$(cno)&",Version=2,KFName=Q:\CLmstr\UnPdIdx2.h"&str$(cno)&",Use,RecL=114,KPs=31/27/1,KLn=2/4/26,Shr",internal,outin,keyed 
00200   return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230 ! ** Field Labels **
00240   let lbl$(1)="Vendor Key" !:
        let lbl$(2)="Invoice Key" !:
        let lbl$(3)="Invoice Date" !:
        let lbl$(4)="Due Date" !:
        let lbl$(5)="Purchase Order Number"
00250   let lbl$(6)="Description" !:
        let lbl$(7)="Amount" !:
        let lbl$(8)="Payment Code" !:
        let lbl$(9)="Bank" !:
        let lbl$(10)="Check Number"
00260   let lbl$(11)="Date Paid" !:
        let lbl$(12)="Posting Code" !:
        let lbl$(13)="Posting Date" !:
        let lbl$(14)="Discount Amount" !:
        let lbl$(15)="Discount Due Date"
00270 ! ** Field Display Lengths ** !:
        let mmddyy=8 : let ccyymmdd=10 !:
        ! TC=0 ! Text Box Length Item Coutner
00280   let tln(tc+=1)=8 !:
        let tln(tc+=1)=12 !:
        let tln(tc+=1)=mmddyy !:
        let tln(tc+=1)=mmddyy !:
        let tln(tc+=1)=12
00290   let tln(tc+=1)=18 !:
        let tln(tc+=1)=10 !:
        let tln(tc+=1)=1 !:
        let tln(tc+=1)=2 !:
        let tln(tc+=1)=8
00300   let tln(tc+=1)=mmddyy !:
        let tln(tc+=1)=1 !:
        let tln(tc+=1)=6 !:
        let tln(tc+=1)=10 !:
        let tln(tc+=1)=ccyymmdd
00310 ! ** Field Types ** !:
        ! Valid are C, G, N, PD, !:
        ! Default is 'G' !:
        let fc=0 ! Field Type Item Counter
00320   let fltyp$(fc+=1)='Cr' !:
        let fltyp$(fc+=1)='C' !:
        let fltyp$(fc+=1)='G' !:
        let fltyp$(fc+=1)='G' !:
        let fltyp$(fc+=1)='C'
00330   let fltyp$(fc+=1)='C' !:
        let fltyp$(fc+=1)='N' !:
        let fltyp$(fc+=1)='N' !:
        let fltyp$(fc+=1)='N' !:
        let fltyp$(fc+=1)='G'
00340   let fltyp$(fc+=1)='G' !:
        let fltyp$(fc+=1)='G' ! XXX !:
        let fltyp$(fc+=1)='G' ! XXX !:
        let fltyp$(fc+=1)='N' !:
        let fltyp$(fc+=1)='N'
00350 ! ** Field Storage Lengths ** !:
        ! sc=0 ! Field Storage Length Item Counter
00360   let sln(sc+=1)=8 !:
        let sln(sc+=1)=12 !:
        let sln(sc+=1)=6 !:
        let sln(sc+=1)=6 !:
        let sln(sc+=1)=12
00370   let sln(sc+=1)=18 !:
        let sln(sc+=1)=10.2 !:
        let sln(sc+=1)=1 !:
        let sln(sc+=1)=2 !:
        let sln(sc+=1)=8
00380   let sln(sc+=1)=6 !:
        let sln(sc+=1)=1 !:
        let sln(sc+=1)=6 !:
        let sln(sc+=1)=10.2 !:
        let sln(sc+=1)=8
00390 ! ** Field Mask ** !:
        let number=30 !:
        let pointtwo=32 ! number with 2 decimal places (no commas)!:
        let mmddyy=1 : let ccyymmdd=3 !:
        let mc=0 ! mask item counter
00400   let mask(mc+=1)=none !:
        let mask(mc+=1)=none !:
        let mask(mc+=1)=mmddyy !:
        let mask(mc+=1)=mmddyy !:
        let mask(mc+=1)=none
00410   let mask(mc+=1)=none !:
        let mask(mc+=1)=pointtwo !:
        let mask(mc+=1)=number !:
        let mask(mc+=1)=number !:
        let mask(mc+=1)=number
00420   let mask(mc+=1)=mmddyy !:
        let mask(mc+=1)=number !:
        let mask(mc+=1)=mmddyy !:
        let mask(mc+=1)=pointtwo !:
        let mask(mc+=1)=mmddyy
00430 ! ** Storage Position ** !:
        let sc=0 ! Storage Position Item Counter
00440 ! ** Let's Make Some Combo Boxes ** !:
        ! CL = Item you want a ComboBox on !:
        ! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len !:
        ! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File !:
        ! C$(cl,8)=limit to list option '1'=yes '0'=no !:
        let limit_to_list$='1'
00450   let cl=8 !:
        let c$(cl,1)='ComboF' !:
        let c$(cl,2)="Q:\CLmstr\PaymentCode.dat" !:
        let c$(cl,3)='1' : let c$(cl,4)=str$(sln(cl)) !:
        let c$(cl,5)=str$(sln(cl)+1) : let c$(cl,6)='25' !:
        let c$(cl,7)="Q:\CLmstr\PaymentCode.Idx" : let c$(cl,8)=limit_to_list$
00460   let cl=9 !:
        let c$(cl,1)='ComboF' !:
        let c$(cl,2)="Q:\CLmstr\BankMstr.h"&str$(cno) !:
        let c$(cl,3)='1' : let c$(cl,4)=str$(sln(cl)) !:
        let c$(cl,5)=str$(sln(cl)+1) : let c$(cl,6)='30' !:
        let c$(cl,7)="Q:\CLmstr\BankIdx1.h"&str$(cno) : let c$(cl,8)=limit_to_list$
00470   let cl=12 !:
        let c$(cl,1)='ComboF' !:
        let c$(cl,2)="R:\acsCL\PostingCode.dat" !:
        let c$(cl,3)='1' : let c$(cl,4)=str$(sln(cl)) !:
        let c$(cl,5)=str$(sln(cl)+1) : let c$(cl,6)='25' !:
        let c$(cl,7)="R:\acsCL\PostingCode.idx" : let c$(cl,8)=limit_to_list$
00480   return 
00490 ! ______________________________________________________________________
00500 OPEN_AND_CALL_HAMSTER: ! 
00510 ! if the file is created and you open it, indexs do not get updated correctly, until you close it and reopen it.
00520   gosub OPEN_FILE
00530   for j=1 to open_file_count !:
          close #j: !:
        next j
00540   gosub OPEN_FILE
00550   let fnhamster("UnpaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00560   return 
00570 ! ______________________________________________________________________
00580 XIT: let fnxit
00590 ! ______________________________________________________________________
00600 ! <Updateable Region: ERTN>
00610 ERTN: let fnerror(cap$,err,line,act$,"xit")
00620   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00630   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00640   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00650 ERTN_EXEC_ACT: execute act$ : goto ERTN
00660 ! /region
00670 ! ______________________________________________________________________
