00010 ! Replace S:\acsSU\TimeFrame
00020 ! Checkbook Transaction Allocation File - Hamster !:
        ! pretty useless to the end user - but quite usefull to the programmer
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnhamster
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2)
00080 ! ______________________________________________________________________
00090   let fntop(program$,cap$='Time Frame')
00100   gosub BUILD_LAYOUT
00110   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE !:
        gosub HAMSTER
00120   goto XIT
00130 ! ______________________________________________________________________
00140 OPEN_FILE: ! !:
        let open_file_count=0 ! this value is used in the close_file sub routine
00150   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\TimeFrame.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\TMmstr\TimeFrame-Idx.h"&str$(cno)&",Use,RecL=52,KPs=1,KLn=2,Shr",internal,outin,keyed 
00160   return 
00170 ! ______________________________________________________________________
00180 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00190 ! ______________________________________________________________________
00200 BUILD_LAYOUT: ! 
00210   let fncno(cno)
00220 ! ** Field Labels    ** !:
        let ic=0 ! temporary Item Counter
00230   let lbl$(ic+=1)="Time Frame ID" !:
        let lbl$(ic+=1)="Description"
00250 ! ** Text Box / Field Display   Lengths   ** !:
        let ic=0 ! temporary Item Counter !:
        let mmddyy=8 !:
        let ccyymmdd=10
00260   let tln(ic+=1)=2 !:
        let tln(ic+=1)=50
00280 ! ** Field Types ** !:
        let ic=0
00290   let fltyp$(ic+=1)='C' !:
        let fltyp$(ic+=1)='C'
00310 ! ** Field Storage Lengths ** !:
        let ic=0 !:
        let mmddyy=6 : let ccyymmdd=8
00320   let sln(ic+=1)=2 !:
        let sln(ic+=1)=50
00420   return 
00430 ! ______________________________________________________________________
00440 HAMSTER: ! 
00450   let fnhamster("TimeFrame",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln)
00460   return 
00470 ! ______________________________________________________________________
00480 XIT: let fnxit
00490 ! ______________________________________________________________________
00500 ! <Updateable Region: ERTN>
00510 ERTN: let fnerror(program$,err,line,act$,"xit")
00520   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
