00010 ! Replace S:\acsSU\TimeFrame
00020 ! Checkbook Transaction Allocation File - Hamster !:
        ! pretty useless to the end user - but quite usefull to the programmer
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2)
00080 ! ______________________________________________________________________
00090   fntop(program$,cap$='Time Frame')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name=S:\Core\Data\acsllc\TimeFrame.h[cno],Version=1,KFName=S:\Core\Data\acsllc\TimeFrame-Idx.h[cno],Use,RecL=52,KPs=1,KLn=2,Shr",internal,outIn,keyed 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   fncno(cno)
00220 ! ** Field Labels    ** !:
        ic=0 ! temporary Item Counter
00230   lbl$(ic+=1)="Time Frame ID" !:
        lbl$(ic+=1)="Description"
00250 ! ** Text Box / Field Display   Lengths   ** !:
        ic=0 ! temporary Item Counter !:
        mmddyy=8 !:
        ccyymmdd=10
00260   tln(ic+=1)=2 !:
        tln(ic+=1)=50
00280 ! ** Field Types ** !:
        ic=0
00290   fltyp$(ic+=1)='C' !:
        fltyp$(ic+=1)='C'
00310 ! ** Field Storage Lengths ** !:
        ic=0 !:
        mmddyy=6 : ccyymmdd=8
00320   sln(ic+=1)=2 !:
        sln(ic+=1)=50
00420   return 
00430 ! ______________________________________________________________________
00440 HAMSTER: ! 
00450   fnHamster("TimeFrame",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln)
00460   return 
00470 ! ______________________________________________________________________
00480 XIT: fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: fnerror(program$,err,line,act$,"xit")
00520   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
