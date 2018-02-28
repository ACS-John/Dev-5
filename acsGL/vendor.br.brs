20000 ! Replace S:\acsGL\Vendor
20020 ! Vendor file with dynamic editor - hamster
20040 ! ______________________________________________________________________
20060   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
20080   fntop(program$,cap$="Vendor")
20100   on error goto ERTN
20120 ! ______________________________________________________________________
20140   dim cap$*128,fltyp$(08),fln(08),mask(08),p$(08)*65,lbl$(08)*22,sln(08)
20160   dim c$(8,8)*40
20180 ! ______________________________________________________________________
20200   fncno(cno)
20220   lbl$(1)="Vendor"
20240   lbl$(2)="Name"
20260   lbl$(3)="Address (1)"
20280   lbl$(4)="Address (2)"
20300   lbl$(5)="City, State and Zip"
20320   lbl$(6)="YTD Purchases"
20340   lbl$(7)="1099 Box"
20360   lbl$(8)="Federal ID or SSN"
20380   fln(1)=8
20400   fln(2)=35
20420   fln(3)=20
20440   fln(4)=20
20460   fln(5)=20
20480   fln(6)=9
20500   fln(7)=2
20520   fln(8)=11
20540   sln(6)=5.2
20560   fltyp$(1)="CR"
20580   fltyp$(6)="PD"
20600   fltyp$(7)="N"
20620   mask(1)=2000
20640   mask(6)=30
20660   c$(07,1)='ComboF'
20680   c$(07,2)="[Q]\Data\1099Box.dat"
20700   c$(07,3)='1' : c$(07,4)='2'
20720   c$(07,5)='3' : c$(07,6)='60'
20740   c$(07,7)="[Q]\Data\1099Box.idx" : c$(07,8)='1'
20760   open #1: "Name=[Q]\GLmstr\gl1099.h[cno],KFName=[Q]\GLmstr\gl109idx.h[cno],Use,RecL=127,KPs=1,KLn=8,Shr",internal,outIn,keyed 
20780   fnHamster("gl1099",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask,mat startpos,mat c$)
20800 XIT: fnxit
20820 ! ______________________________________________________________________
20840 ! <Updateable Region: ERTN>
20860 ERTN: fnerror(program$,err,line,act$,"xit")
20880   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20900   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20920   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20940 ERTN_EXEC_ACT: execute act$ : goto ERTN
20960 ! /region
20980 ! ______________________________________________________________________
