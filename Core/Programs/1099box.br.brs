00010 ! Replace S:\Core\Programs\1099Box
00020 ! 1099 Boxes File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fnhamster
00050   fntop(program$,cap$="1099 Boxes")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim cap$*128,fltyp$(2),sln(2),mask(2),fln(2),p$(2)*60,lbl$(2)
00090 ! ______________________________________________________________________
00100 ! the Third Item (C 40) is not used and is there for unintended use
00110   let lbl$(1)="Box Number" : let lbl$(2)="Description" ! Let LBL$(3)="NA"
00120   let fln(1)=2 : let fln(2)=60 ! Let FLN(3)=40
00130   let mask(1)=1030 ! Let MASK(3)=20000
00140   open #1: "Name="&env$('Q')&"\Data\1099Box.dat,KFName="&env$('Q')&"\Data\1099Box.Idx,Use,RecL=102,KPs=1,KLn=2,Shr",internal,outin,keyed 
00150   fnhamster("1099Box",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
00160 XIT: let fnxit
00170 ! ______________________________________________________________________
00180 ! <Updateable Region: ERTN>
00190 ERTN: let fnerror(program$,err,line,act$,"xit")
00200   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00210   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 ! ______________________________________________________________________
