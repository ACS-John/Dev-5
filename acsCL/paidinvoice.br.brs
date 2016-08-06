00010 ! Replace R:\acsCL\PaidInvoice
00020 ! Check Book PaidInvoice File
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! Dimension Ony the Arrays you need (this is important for Hamster) !:
        ! Additionally Never use exactaly 10 items in a file.
00080   dim cap$*128,lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),mask(4),sln(4)
00090   dim c$(4,8)*256
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Paid Invoice")
00120   let fncno(cno)
00130   gosub BUILD_LAYOUT
00140   gosub OPEN_AND_CALL_HAMSTER
00150   goto XIT
00160 ! ______________________________________________________________________
00170 OPEN_FILE: ! (open_file_count)
00180   let open_file_count=0
00190   open #open_file_count+=1: "Name=Q:\CLmstr\IvPaid.h"&str$(cno)&",KFName=Q:\CLmstr\IVIndex.h"&str$(cno)&",Use,RecL=34,KPs=1,KLn=20,Shr",internal,outin,keyed  !:
        ! Version=0 or Version=1
00200   return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00230 ! ** Field Labels **
00240   let lbl$(1)="Vendor Key" !:
        let lbl$(2)="Invoice Key" !:
        let lbl$(3)="Date Paid" !:
        let lbl$(4)="Check Number"
00250 ! ** Field Display Lengths ** !:
        let mmddyy=8 : let ccyymmdd=10 !:
        ! TC=0 ! Text Box Length Item Coutner
00260   let tln(tc+=1)=8 !:
        let tln(tc+=1)=12 !:
        let tln(tc+=1)=mmddyy !:
        let tln(tc+=1)=8
00270 ! ** Field Types ** !:
        ! Valid are C, G, N, PD, !:
        ! Default is 'G' !:
        let fc=0 ! Field Type Item Counter
00280   let fltyp$(fc+=1)='C' !:
        let fltyp$(fc+=1)='C' !:
        let fltyp$(fc+=1)='G' !:
        let fltyp$(fc+=1)='G'
00290 ! ** Field Storage Lengths ** !:
        ! sc=0 ! Field Storage Length Item Counter
00300   let sln(sc+=1)=8 !:
        let sln(sc+=1)=12 !:
        let sln(sc+=1)=6 !:
        let sln(sc+=1)=8
00310 ! ** Field Mask ** !:
        let number=30 !:
        let pointtwo=32 ! number with 2 decimal places (no commas)!:
        let mmddyy=1 : let ccyymmdd=3 !:
        let mc=0 ! mask item counter
00320   let mask(mc+=1)=none !:
        let mask(mc+=1)=none !:
        let mask(mc+=1)=mmddyy !:
        let mask(mc+=1)=number
00330 ! ** Storage Position ** !:
        let sc=0 ! Storage Position Item Counter
00340 ! ** Let's Make Some Combo Boxes ** !:
        ! CL = Item you want a ComboBox on !:
        ! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len !:
        ! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File !:
        ! C$(cl,8)=limit to list option '1'=yes '0'=no !:
        let limit_to_list$='1'
00350   let cl=1 !:
        let c$(cl,1)='ComboF' !:
        let c$(cl,2)="Q:\CLmstr\PayMstr.h"&str$(cno) !:
        let c$(cl,3)='1' : let c$(cl,4)=str$(sln(cl)) !:
        let c$(cl,5)=str$(sln(cl)+1) : let c$(cl,6)='30' !:
        let c$(cl,7)="Q:\CLmstr\PayIdx1.h"&str$(cno) : let c$(cl,8)=limit_to_list$
00360 ! Let CL=2 !:
        ! Let C$(CL,1)='ComboF' !:
        ! Let C$(CL,2)='Q:\CLmstr\BankMstr.h'&STR$(CNO) !:
        ! Let C$(CL,3)='1' : Let C$(CL,4)=STR$(SLN(CL)) !:
        ! Let C$(CL,5)=STR$(SLN(CL)+1) : Let C$(CL,6)='30' !:
        ! Let C$(CL,7)='Q:\CLmstr\BankIdx1.h'&STR$(CNO) : Let C$(CL,8)=LIMIT_TO_LIST$
00370 ! Let CL=4 !:
        ! Let C$(CL,1)='ComboF' !:
        ! Let C$(CL,2)='Q:\CLmstr\TrMstr.h'&STR$(CNO) !:
        ! Let C$(CL,3)='1' : Let C$(CL,4)=STR$(SLN(CL)) !:
        ! Let C$(CL,5)=STR$(SLN(CL)+1) : Let C$(CL,6)='30' !:
        ! Let C$(CL,7)='Q:\CLmstr\BankIdx1.h'&STR$(CNO) : Let C$(CL,8)=LIMIT_TO_LIST$
00380   return 
00390 ! ______________________________________________________________________
00400 OPEN_AND_CALL_HAMSTER: ! 
00410 ! if the file is created and you open it, indexs do not get updated correctly, until you close it and reopen it.
00420   gosub OPEN_FILE
00430   for j=1 to open_file_count !:
          close #j: !:
        next j
00440   gosub OPEN_FILE
00450   let fnhamster("PaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00460   return 
00470 ! ______________________________________________________________________
00480 XIT: let fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: let fnerror(cap$,err,line,act$,"xit")
00520   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
