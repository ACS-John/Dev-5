00010 ! Replace R:\Core\Programs\1099Type
00020 ! 1099Type file
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   let fntop(program$,cap$="1099Type")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cap$*128,fltyp$(2),p$(2)*25
00090 ! ______________________________________________________________________
00100   let fncno(cno)
00110   let lbl$(1)="Key" : let lbl$(2)="Desc"
00120   let fln(1)=1 : let fln(2)=25
00130   open #1: "Name=R:\Core\Data\1099Type.dat,KFName=R:\Core\Data\1099Type.idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outin,keyed 
00131   close #1: 
00132   open #1: "Name=R:\Core\Data\1099Type.dat,KFName=R:\Core\Data\1099Type.idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outin,keyed 
00140   let fnhamster("1099Type",mat lbl$,mat fln,1,mat p$)
00150 XIT: let fnxit
00160 ! ______________________________________________________________________
00170 ! <Updateable Region: ERTN>
00180 ERTN: let fnerror(cap$,err,line,act$,"xit")
00190   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00200   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00210   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00220 ERTN_EXEC_ACT: execute act$ : goto ERTN
00230 ! /region
00240 ! ______________________________________________________________________
