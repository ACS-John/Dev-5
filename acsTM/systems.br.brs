20000 ! Replace S:\acsTM\Systems
20020 ! ______________________________________________________________________
20040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnHamster,fnindex_it
20060   on error goto ERTN
20080 ! ______________________________________________________________________
20100   dim cap$*128,lbl$(3)*38,tln(3),p$(3)*160,fltyp$(3),sln(3),mask(3)
20120 ! ______________________________________________________________________
20140   fntop(program$,cap$='Systems 420')
20160   gosub BUILD_LAYOUT
20180   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
20200   gosub HAMSTER: gosub CLOSE_FILE
20220   fnindex_it(env$('Q')&"\TMmstr\Systems.h420",env$('Q')&"\TMmstr\Systems-idx.h420","1,2")
20240   goto XIT
20260 ! ______________________________________________________________________
20280 OPEN_FILE: ! 
20300   open_file_count=0 ! this value is used in the close_file sub routine
20320   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\Systems.h420,Version=1,KFName="&env$('Q')&"\TMmstr\Systems-Idx.h420,Use,RecL=90,KPs=1,KLn=2,Shr",internal,outIn,keyed 
20340   return 
20360 ! ______________________________________________________________________
20380 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
20400 ! ______________________________________________________________________
20420 BUILD_LAYOUT: ! 
20440 ! ** Field Labels    **
20460   ic=0 ! temporary Item Counter
20480   lbl$(ic+=1)="ID"
20500   lbl$(ic+=1)="Name"
20520   lbl$(ic+=1)="Number"
20540 ! ** Text Box / Field Display   Lengths   **
20560   ic=0 ! temporary Item Counter
20580   mmddyy=8
20600   ccyymmdd=10
20620   tln(ic+=1)=2
20640   tln(ic+=1)=50
20660   tln(ic+=1)=2
20680 ! ** Field Types **
20700   ic=0
20720   fltyp$(ic+=1)='C'
20740   fltyp$(ic+=1)='C'
20760   fltyp$(ic+=1)='N'
20780 ! ** Field Storage Lengths **
20800   ic=0
20820   mmddyy=6 : ccyymmdd=8
20840   sln(ic+=1)=2
20860   sln(ic+=1)=50
20880   sln(ic+=1)=3
20900 ! ** Field Masks **
20920   ic=0
20940   pointtwo=32 : number=30
20960   ccyymmdd=3 : mmddyy=1 : glnumber=53
20980   mask(ic+=1)=0
21000   mask(ic+=1)=0
21020   mask(ic+=1)=number
21040   return 
21060 ! ______________________________________________________________________
21080 HAMSTER: ! 
21100   fnHamster("Systems",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp) ! ,mat c$)
21120   return 
21140 XIT: fnxit
21160 ! <Updateable Region: ERTN>
21180 ERTN: fnerror(program$,err,line,act$,"xit")
21200   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
21220   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
21240   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
21260 ERTN_EXEC_ACT: execute act$ : goto ERTN
21280 ! /region
21300 ! ______________________________________________________________________
