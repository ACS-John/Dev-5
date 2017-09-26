00010 ! Replace S:\Core\Programs\Eye
00020 ! Eye Color file
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   let fntop(program$,cap$="Eye Color")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cap$*128,lbl$(1),fltyp$(1),sln(1),mask(1),fln(1),p$(1)*10
00090 ! ______________________________________________________________________
00100   let fncno(cno)
00110   let lbl$(1)="Eye Color"
00120   let fln(1)=10
00130   let mask(1)=2000
00140   open #1: "Name=S:\Core\Data\Eye.dat,RecL=10,Use,Shr",internal,outin,relative 
00150   let fnhamster("Eye",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
00160 XIT: let fnxit
00170 ! ______________________________________________________________________
00180 ! <Updateable Region: ERTN>
00190 ERTN: let fnerror(program$,err,line,act$,"xit")
00200   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00210   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 ! ______________________________________________________________________
