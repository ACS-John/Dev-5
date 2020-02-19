00010 ! Replace S:\acsGL\Year
00020 ! GL - Year File
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   on error goto Ertn
00060 !
00070   dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2)
00080 !
00090   fntop(program$,cap$='Year')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 !
00140 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name=[Q]\GLmstr\Year.h[cno],Version=1,KFName=[Q]\GLmstr\Year-Idx.h[cno],Use,RecL=8,KPs=1,KLn=1,Shr",internal,outIn,keyed 
00160   return 
00170 !
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 !
00200 BUILD_LAYOUT: ! 
00210   fncno(cno)
00220 ! ** Field Labels    ** !:
        ic=0 ! temporary Item Counter
00230   lbl$(ic+=1)="Year Code" !:
        lbl$(ic+=1)="Name"
00240 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00250   tln(ic+=1)=1 !:
        tln(ic+=1)=7
00260 ! ** Field Types ** !:
        ic=0
00270   fltyp$(ic+=1)='N' !:
        fltyp$(ic+=1)='C'
00280 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00290   sln(ic+=1)=1 !:
        sln(ic+=1)=7
00300 ! ** Field Masks ** !:
        ic=0 !:
        pointtwo=32 : number=30 !:
        ccyymmdd=3 : mmddyy=1 : glnumber=53
00310   mask(ic+=1)=number !:
        mask(ic+=1)=0
00320   return 
00330 !
00340 HAMSTER: ! 
00350   fnHamster("Year",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
00360   return 
00370 !
00380 XIT: fnxit
00390 !
00400 ! <Updateable Region: ERTN>
00410 ERTN: fnerror(program$,err,line,act$,"xit")
00420   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 !
