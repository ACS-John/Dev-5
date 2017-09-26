00010 ! Replace S:\acsGL\Year
00020 ! GL - Year File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2)
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='Year')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name="&env$('Q')&"\GLmstr\Year.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\GLmstr\Year-Idx.h"&str$(cno)&",Use,RecL=8,KPs=1,KLn=1,Shr",internal,outin,keyed 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   let fncno(cno)
00220 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00230   let lbl$(ic+=1)="Year Code" !:
        let lbl$(ic+=1)="Name"
00240 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00250   let tln(ic+=1)=1 !:
        let tln(ic+=1)=7
00260 ! ** Field Types ** !:
        let ic=0
00270   let fltyp$(ic+=1)='N' !:
        let fltyp$(ic+=1)='C'
00280 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00290   let sln(ic+=1)=1 !:
        let sln(ic+=1)=7
00300 ! ** Field Masks ** !:
        let ic=0 !:
        let pointtwo=32 : let number=30 !:
        let ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00310   let mask(ic+=1)=number !:
        let mask(ic+=1)=0
00320   return 
00330 ! ______________________________________________________________________
00340 HAMSTER: ! 
00350   let fnhamster("Year",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
00360   return 
00370 ! ______________________________________________________________________
00380 XIT: let fnxit
00390 ! ______________________________________________________________________
00400 ! <Updateable Region: ERTN>
00410 ERTN: let fnerror(program$,err,line,act$,"xit")
00420   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 ! ______________________________________________________________________
