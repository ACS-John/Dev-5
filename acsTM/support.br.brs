00010 ! Replace S:\acsTM\Support
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim cap$*128,lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),sln(11),mask(11),c$(11,8)*256 ! SP(11) - not used
00070 ! ______________________________________________________________________
00080   let fntop(program$,cap$='Support 420')
00090   gosub BUILD_LAYOUT
00100   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
00110   gosub HAMSTER: gosub CLOSE_FILE
00120   execute "Index "&env$('Q')&"\TMmstr\support.h420  "&env$('Q')&"\TMmstr\support-idx.h420 1/7,6/2,replace,DupKeys"
00130   goto XIT
00140 ! ______________________________________________________________________
00150 OPEN_FILE: ! 
00160   let open_file_count=0 ! this value is used in the close_file sub routine
00170   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\Support.h420,Version=2,KFName="&env$('Q')&"\TMmstr\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outin,keyed 
00180   return 
00190 ! ______________________________________________________________________
00200 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00240 ! ** Field Labels    **
00250   let ic=0 ! temporary Item Counter
00260   let lbl$(ic+=1)="Client ID"
00270   let lbl$(ic+=1)="Sys#"
00280   let lbl$(ic+=1)="System ID"
00290   let lbl$(ic+=1)="Starting Date"
00300   let lbl$(ic+=1)="Time Frame"
00310   let lbl$(ic+=1)="Ending Date"
00320   let lbl$(ic+=1)="Cost to User"
00330   let lbl$(ic+=1)="Name"
00340   let lbl$(ic+=1)="Contact (1)"
00350   let lbl$(ic+=1)="Contact (2)"
00360   let lbl$(ic+=1)="Contact (3)"
00370 ! ** Text Box / Field Display   Lengths   **
00380   let ic=0 ! temporary Item Counter
00390   let mmddyy=8
00400   let ccyymmdd=10
00410   let tln(ic+=1)=6
00420   let tln(ic+=1)=2
00430   let tln(ic+=1)=2
00440   let tln(ic+=1)=ccyymmdd
00450   let tln(ic+=1)=2
00460   let tln(ic+=1)=ccyymmdd
00470   let tln(ic+=1)=10
00480   let tln(ic+=1)=50
00490   let tln(ic+=1)=50
00500   let tln(ic+=1)=50
00510   let tln(ic+=1)=50
00520 ! ** Field Types **
00530   let ic=0
00540   let fltyp$(ic+=1)='N'
00550   let fltyp$(ic+=1)='N'
00560   let fltyp$(ic+=1)='C'
00570   let fltyp$(ic+=1)='N'
00580   let fltyp$(ic+=1)='C'
00590   let fltyp$(ic+=1)='N'
00600   let fltyp$(ic+=1)='N'
00610   let fltyp$(ic+=1)='C'
00620   let fltyp$(ic+=1)='C'
00630   let fltyp$(ic+=1)='C'
00640   let fltyp$(ic+=1)='C'
00650 ! ** Field Storage Lengths **
00660   let ic=0
00670   let mmddyy=6 : let ccyymmdd=8
00680   let sln(ic+=1)=6
00690   let sln(ic+=1)=2
00700   let sln(ic+=1)=2
00710   let sln(ic+=1)=ccyymmdd
00720   let sln(ic+=1)=2
00730   let sln(ic+=1)=ccyymmdd
00740   let sln(ic+=1)=10.2
00750   let sln(ic+=1)=50
00760   let sln(ic+=1)=50
00770   let sln(ic+=1)=50
00780   let sln(ic+=1)=50
00790 ! ** Field Masks **
00800   let ic=0
00810   let pointtwo=32 : let number=30
00820   let ccyymmdd=3 : let mmddyy=1 : let glnumber=53
00830   let mask(ic+=1)=number
00840   let mask(ic+=1)=number
00850   let mask(ic+=1)=0
00860   let mask(ic+=1)=ccyymmdd
00870   let mask(ic+=1)=0
00880   let mask(ic+=1)=ccyymmdd
00890   let mask(ic+=1)=pointtwo
00900   let mask(ic+=1)=0
00910   let mask(ic+=1)=0
00920   let mask(ic+=1)=0
00930   let mask(ic+=1)=0
00940 ! ** Combo Boxes **
00950 ! CL=Field Number  : C$(CL,1)='ComboF'
00960 ! C$(CL,2)=Linked File Name
00970 ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
00980 ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
00990 ! C$(CL,7)=Index File
01000 ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
01010   let limit_to_list$='1'
01020   let cl=1 : let c$(cl,1)='ComboF'
01030   let c$(cl,2)=env$('Q')&"\TMmstr\Clmstr.h420"
01050   let c$(cl,3)='1' : let c$(cl,4)='5'
01060 ! let c$(cl,3)='1' : let c$(cl,4)='6'
01070   let c$(cl,5)='6' : let c$(cl,6)='30'
01080 ! let c$(cl,5)='7' : let c$(cl,6)='50'
01090   let c$(cl,7)=env$('Q')&"\TMmstr\CLIndex.h420"
01110   let c$(cl,8)=limit_to_list$
01120   let cl=3 : let c$(cl,1)='ComboF'
01130   let c$(cl,2)=env$('Q')&"\TMmstr\Systems.h420"
01140   let c$(cl,3)='1' : let c$(cl,4)='2'
01150   let c$(cl,5)='3' : let c$(cl,6)='50'
01160   let c$(cl,7)=env$('Q')&"\TMmstr\Systems-Idx.h420"
01170   let c$(cl,8)=limit_to_list$
01180   let cl=5 : let c$(cl,1)='ComboF'
01190   let c$(cl,2)=env$('Q')&"\TMmstr\TimeFrame.h420"
01200   let c$(cl,3)='1' : let c$(cl,4)='2'
01210   let c$(cl,5)='3' : let c$(cl,6)='50'
01220   let c$(cl,7)=env$('Q')&"\TMmstr\TimeFrame-Idx.h420"
01230   let c$(cl,8)=limit_to_list$
01240   return 
01250 ! ______________________________________________________________________
01260 HAMSTER: ! 
01270   let fnhamster("Support",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
01280   return 
01290 ! ______________________________________________________________________
01300 XIT: let fnxit
01310 ! ______________________________________________________________________
01320 ! <Updateable Region: ERTN>
01330 ERTN: let fnerror(program$,err,line,act$,"xit")
01340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01360   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01370 ERTN_EXEC_ACT: execute act$ : goto ERTN
01380 ! /region
01390 ! ______________________________________________________________________
