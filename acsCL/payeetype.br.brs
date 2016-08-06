00010 ! Replace R:\acsCL\PayeeType
00020 ! Check Book PayeeType File
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080   dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),mask(2),sln(2)
00090   dim c$(2,8)*40
00100 ! ______________________________________________________________________
00110   let fntop(program$,cap$="Payee Type")
00120   let fncno(cno)
00130   let lbl$(1)="Payee Type" !:
        let lbl$(2)="Type Description"
00140   let tln(1)=2 : let tln(2)=25
00150   let fltyp$(1)="N" : let fltyp$(2)="C"
00160   let sln(1)=2 : let sln(2)=25
00170   let mask(1)=1030
00180   open #1: "Name=R:\acsCL\PayeeType.dat,Version=1,KFName=R:\acsCL\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outin,keyed 
00190   close #1: 
00200   open #1: "Name=R:\acsCL\PayeeType.dat,Version=1,KFName=R:\acsCL\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outin,keyed 
00210   let fnhamster("PayeeType",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00220 XIT: let fnxit
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: let fnerror(cap$,err,line,act$,"xit")
00260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
