00010 ! Replace S:\acsCL\PaymentCode
00020 ! Checkbook PaymentCode File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080   dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),mask(2),sln(2)
00090   dim c$(2,8)*40
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Payment Code")
00120   fncno(cno)
00130   let lbl$(1)="Payment Code" !:
        let lbl$(2)="Code Description"
00140   let tln(1)=1 : let tln(2)=25
00150   let fltyp$(1)="N" : let fltyp$(2)="C"
00160   let sln(1)=1 : let sln(2)=25
00170   let mask(1)=1030
00180   open #1: "Name="&env$('Q')&"\CLmstr\PaymentCode.dat,Version=1,KFName="&env$('Q')&"\CLmstr\PaymentCode.Idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outin,keyed 
00190   close #1: 
00200   open #1: "Name="&env$('Q')&"\CLmstr\PaymentCode.dat,Version=1,KFName="&env$('Q')&"\CLmstr\PaymentCode.Idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outin,keyed 
00210   fnhamster("PaymentCode",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00220 XIT: let fnxit
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: let fnerror(program$,err,line,act$,"xit")
00260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
