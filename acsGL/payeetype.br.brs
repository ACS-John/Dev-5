00010 ! Replace S:\acsGL\PayeeType
00020 ! Checkbook PayeeType File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128
00080   dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),mask(2),sln(2)
00090   dim c$(2,8)*40
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Payee Type")
00120   fncno(cno)
00130   lbl$(1)="Payee Type" !:
        lbl$(2)="Type Description"
00140   tln(1)=2 : tln(2)=25
00150   fltyp$(1)="N" : fltyp$(2)="C"
00160   sln(1)=2 : sln(2)=25
00170   mask(1)=1030
00180   open #1: "Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed 
00190   close #1: 
00200   open #1: "Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed 
00210   fnHamster("PayeeType",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00220 XIT: fnxit
00230 ! ______________________________________________________________________
00240 ! <Updateable Region: ERTN>
00250 ERTN: fnerror(program$,err,line,act$,"xit")
00260   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00270   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00280   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00290 ERTN_EXEC_ACT: execute act$ : goto ERTN
00300 ! /region
00310 ! ______________________________________________________________________
