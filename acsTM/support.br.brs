00010 ! Replace S:\acsTM\Support
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fnerror,fnhamster
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim cap$*128,lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),sln(11),mask(11),c$(11,8)*256 ! SP(11) - not used
00070 ! ______________________________________________________________________
00080   fntop(program$,cap$='Support 420')
00090   gosub BUILD_LAYOUT
00100   gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
00110   gosub HAMSTER: gosub CLOSE_FILE
00120   execute "Index "&env$('Q')&"\TMmstr\support.h420  "&env$('Q')&"\TMmstr\support-idx.h420 1/7,6/2,replace,DupKeys"
00130   goto XIT
00140 ! ______________________________________________________________________
00150 OPEN_FILE: ! 
00160   open_file_count=0 ! this value is used in the close_file sub routine
00170   open #open_file_count+=1: "Name="&env$('Q')&"\TMmstr\Support.h420,Version=2,KFName="&env$('Q')&"\TMmstr\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,outin,keyed 
00180   return 
00190 ! ______________________________________________________________________
00200 CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
00210 ! ______________________________________________________________________
00220 BUILD_LAYOUT: ! 
00240 ! ** Field Labels    **
00250   ic=0 ! temporary Item Counter
00260   lbl$(ic+=1)="Client ID"
00270   lbl$(ic+=1)="Sys#"
00280   lbl$(ic+=1)="System ID"
00290   lbl$(ic+=1)="Starting Date"
00300   lbl$(ic+=1)="Time Frame"
00310   lbl$(ic+=1)="Ending Date"
00320   lbl$(ic+=1)="Cost to User"
00330   lbl$(ic+=1)="Name"
00340   lbl$(ic+=1)="Contact (1)"
00350   lbl$(ic+=1)="Contact (2)"
00360   lbl$(ic+=1)="Contact (3)"
00370 ! ** Text Box / Field Display   Lengths   **
00380   ic=0 ! temporary Item Counter
00390   mmddyy=8
00400   ccyymmdd=10
00410   tln(ic+=1)=6
00420   tln(ic+=1)=2
00430   tln(ic+=1)=2
00440   tln(ic+=1)=ccyymmdd
00450   tln(ic+=1)=2
00460   tln(ic+=1)=ccyymmdd
00470   tln(ic+=1)=10
00480   tln(ic+=1)=50
00490   tln(ic+=1)=50
00500   tln(ic+=1)=50
00510   tln(ic+=1)=50
00520 ! ** Field Types **
00530   ic=0
00540   fltyp$(ic+=1)='N'
00550   fltyp$(ic+=1)='N'
00560   fltyp$(ic+=1)='C'
00570   fltyp$(ic+=1)='N'
00580   fltyp$(ic+=1)='C'
00590   fltyp$(ic+=1)='N'
00600   fltyp$(ic+=1)='N'
00610   fltyp$(ic+=1)='C'
00620   fltyp$(ic+=1)='C'
00630   fltyp$(ic+=1)='C'
00640   fltyp$(ic+=1)='C'
00650 ! ** Field Storage Lengths **
00660   ic=0
00670   mmddyy=6 : ccyymmdd=8
00680   sln(ic+=1)=6
00690   sln(ic+=1)=2
00700   sln(ic+=1)=2
00710   sln(ic+=1)=ccyymmdd
00720   sln(ic+=1)=2
00730   sln(ic+=1)=ccyymmdd
00740   sln(ic+=1)=10.2
00750   sln(ic+=1)=50
00760   sln(ic+=1)=50
00770   sln(ic+=1)=50
00780   sln(ic+=1)=50
00790 ! ** Field Masks **
00800   ic=0
00810   pointtwo=32 : number=30
00820   ccyymmdd=3 : mmddyy=1 : glnumber=53
00830   mask(ic+=1)=number
00840   mask(ic+=1)=number
00850   mask(ic+=1)=0
00860   mask(ic+=1)=ccyymmdd
00870   mask(ic+=1)=0
00880   mask(ic+=1)=ccyymmdd
00890   mask(ic+=1)=pointtwo
00900   mask(ic+=1)=0
00910   mask(ic+=1)=0
00920   mask(ic+=1)=0
00930   mask(ic+=1)=0
00940 ! ** Combo Boxes **
00950 ! CL=Field Number  : C$(CL,1)='ComboF'
00960 ! C$(CL,2)=Linked File Name
00970 ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
00980 ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
00990 ! C$(CL,7)=Index File
01000 ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
01010   limit_to_list$='1'
01020   cl=1 : c$(cl,1)='ComboF'
01030   c$(cl,2)=env$('Q')&"\TMmstr\Clmstr.h420"
01050   c$(cl,3)='1' : c$(cl,4)='5'
01060 ! c$(cl,3)='1' : c$(cl,4)='6'
01070   c$(cl,5)='6' : c$(cl,6)='30'
01080 ! c$(cl,5)='7' : c$(cl,6)='50'
01090   c$(cl,7)=env$('Q')&"\TMmstr\CLIndex.h420"
01110   c$(cl,8)=limit_to_list$
01120   cl=3 : c$(cl,1)='ComboF'
01130   c$(cl,2)=env$('Q')&"\TMmstr\Systems.h420"
01140   c$(cl,3)='1' : c$(cl,4)='2'
01150   c$(cl,5)='3' : c$(cl,6)='50'
01160   c$(cl,7)=env$('Q')&"\TMmstr\Systems-Idx.h420"
01170   c$(cl,8)=limit_to_list$
01180   cl=5 : c$(cl,1)='ComboF'
01190   c$(cl,2)=env$('Q')&"\TMmstr\TimeFrame.h420"
01200   c$(cl,3)='1' : c$(cl,4)='2'
01210   c$(cl,5)='3' : c$(cl,6)='50'
01220   c$(cl,7)=env$('Q')&"\TMmstr\TimeFrame-Idx.h420"
01230   c$(cl,8)=limit_to_list$
01240   return 
01250 ! ______________________________________________________________________
01260 HAMSTER: ! 
01270   fnhamster("Support",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
01280   return 
01290 ! ______________________________________________________________________
01300 XIT: fnxit
01310 ! ______________________________________________________________________
01320 ! <Updateable Region: ERTN>
01330 ERTN: fnerror(program$,err,line,act$,"xit")
01340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01370 ERTN_EXEC_ACT: execute act$ : goto ERTN
01380 ! /region
01390 ! ______________________________________________________________________
