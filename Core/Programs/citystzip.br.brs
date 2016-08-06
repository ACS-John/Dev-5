00010 ! Replace R:\Core\programs\citystzip
00020 ! City State Zip
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),mask(1),sln(1),c$(1,8)*40
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='City State Zip')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00115   gosub CLOSE_FILE : execute "INDEX Q:\Data\CityStZip.dat Q:\Data\CityStZip.idx 1 30 Replace DupKeys" ioerr L120
00120 L120: goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name=Q:\Data\CityStZip.dat,Version=1,KFName=Q:\Data\CityStZip.idx,Use,RecL=30,KPs=1,KLn=30,Shr",internal,outin,keyed 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   let fncno(cno)
00220   let lbl$(1)="City State Zip"
00230 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00240   let tln(ic+=1)=30
00250 ! ** Field Types ** !:
        let ic=0
00260   let fltyp$(ic+=1)='C'
00270 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00280   let sln(ic+=1)=30
00290 ! ** MASK section !:
        let ic=0
00300   let mask(ic+=1)=0
00310 ! ** Storage Positions ** !:
        ! default to the same as order displayed !:
        let ic=0
00320 ! ** Combo Boxes **                                                   !:
        ! CL=Field Number  : C$(CL,1)='ComboF'                                !:
        ! C$(CL,2)=Linked File Name                                           !:
        ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 !:
        ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         !:
        ! C$(CL,7)=Index File                                                 !:
        ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     !:
        let limit_to_list$='1'
00330   return 
00340 ! ______________________________________________________________________
00350 HAMSTER: ! 
00360   let fnhamster("UnpaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
00370   return 
00380 ! ______________________________________________________________________
00390 XIT: let fnxit
00400 ! ______________________________________________________________________
00410 ! <Updateable Region: ERTN>
00420 ERTN: let fnerror(cap$,err,line,act$,"xit")
00430   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00440   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00450   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00460 ERTN_EXEC_ACT: execute act$ : goto ERTN
00470 ! /region
00480 ! ______________________________________________________________________
