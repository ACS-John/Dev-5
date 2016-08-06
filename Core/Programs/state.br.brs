00010 ! Replace R:\Core\Programs\State
00020 ! Attorney file !:
        ! with dynamic editor 1.0
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   let fntop(program$,cap$="State")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim p$(3)*25 ! should be (number of items in file) * longest length
00090   dim lbl$(3) ! should contain field descriptions (w/o :s)
00100   dim cap$*128,fltyp$(3),sln(3),mask(3),fln(3)
00110 ! ______________________________________________________________________
00120   let fncno(cno)
00130   let lbl$(1)="Abreviation" : let lbl$(2)="Name" !:
        let lbl$(3)="Code"
00140   let fln(1)=2 : let fln(2)=25 : let fln(3)=2
00150   let mask(1)=2000 !:
        let mask(3)=30
00160   open #1: "Name=R:\Core\Data\State.dat,KFName=R:\Core\Data\State.Idx,Use,RecL=29,KPs=1,KLn=2,Shr",internal,outin,keyed 
00170   let fnhamster("State",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
00180 XIT: let fnxit
00190 ! ______________________________________________________________________
00200 ! <Updateable Region: ERTN>
00210 ERTN: let fnerror(cap$,err,line,act$,"xit")
00220   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00230   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00240   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00250 ERTN_EXEC_ACT: execute act$ : goto ERTN
00260 ! /region
00270 ! ______________________________________________________________________
