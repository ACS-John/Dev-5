00010 ! Replace S:\Core\Programs\Race
00020 ! Attorney file !:
        ! with dynamic editor 1.0
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   fntop(program$,cap$="Race")
00060   on error goto Ertn
00070 !
00080   dim cap$*128,lbl$(1),fltyp$(1),sln(1),mask(1),fln(1),p$(1)*18
00090 !
00100   fncno(cno)
00110   lbl$(1)="Race"
00120   fln(1)=18
00130   mask(1)=2000
00140   open #1: "Name=S:\Core\Data\Race.dat,RecL=18,Use,Shr",internal,outIn,relative 
00150   fnHamster("Race",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
00160 XIT: fnxit
00170 !
00180 ! <Updateable Region: ERTN>
00190 ERTN: fnerror(program$,err,line,act$,"xit")
00200   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00210   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 !
