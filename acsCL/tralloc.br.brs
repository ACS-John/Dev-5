! Replace S:\acsCL\TrAlloc
! Checkbook Transaction Allocation File - Hamster 
! pretty useless to the end user - but quite usefull to the programmer
! ______________________________________________________________________
library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnHamster
on error goto ERTN

dim cap$*128,lbl$(9)*38,tln(9),p$(9)*160,fltyp$(9),sln(9),mask(9),sp(9),c$(9,8)*40

fntop(program$,cap$='Transaction Allocation (Hamster)')
gosub BUILD_LAYOUT
gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE 
fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
goto XIT

OPEN_FILE: ! r:
  open_file_count=0 ! this value is used in the close_file sub routine
  open #open_file_count+=1: "Name=[Q]\CLmstr\TrAlloc.h[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Use,RecL=80,KPs=1,KLn=11,Shr",internal,outIn,keyed 
return ! /r
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return 
BUILD_LAYOUT: ! r:
  fncno(cno)
! ** Field Labels    ** 
  ic=0 ! temporary Item Counter
  lbl$(ic+=1)="Bank Code" 
  lbl$(ic+=1)="Transaction Type" 
  lbl$(ic+=1)="Check/Reference" 
  lbl$(ic+=1)="General Ledger Number" 
  lbl$(ic+=1)="Amount"
  lbl$(ic+=1)="Description" 
  lbl$(ic+=1)="Invoice Date" 
  lbl$(ic+=1)="Purchase Order Number" 
  lbl$(ic+=1)="Posting Code"
! ** Text Box / Field Display   Lengths   ** 
  ic=0 ! temporary Item Counter 
  mmddyy=8 
  ccyymmdd=10
  tln(ic+=1)=2 
  tln(ic+=1)=1 
  tln(ic+=1)=8 
  tln(ic+=1)=12 
  tln(ic+=1)=9
  tln(ic+=1)=30 
  tln(ic+=1)=mmddyy 
  tln(ic+=1)=12 
  tln(ic+=1)=1
! ** Field Types ** 
  ic=0
  fltyp$(ic+=1)='N' 
  fltyp$(ic+=1)='N' 
  fltyp$(ic+=1)='Cr' 
  fltyp$(ic+=1)='C' 
  fltyp$(ic+=1)='PD'
  fltyp$(ic+=1)='C' 
  fltyp$(ic+=1)='G' 
  fltyp$(ic+=1)='C' 
  fltyp$(ic+=1)='N'
! ** Field Storage Lengths ** 
  ic=0 
  mmddyy=6 : ccyymmdd=8
  sln(ic+=1)=2 
  sln(ic+=1)=1 
  sln(ic+=1)=8 
  sln(ic+=1)=12 
  sln(ic+=1)=5.2
  sln(ic+=1)=30 
  sln(ic+=1)=6 
  sln(ic+=1)=12 
  sln(ic+=1)=1
! ** Field Masks ** 
  ic=0 
  pointtwo=32 : number=30 
  ccyymmdd=3 : mmddyy=1 : glnumber=53
  mask(ic+=1)=number 
  mask(ic+=1)=number 
  mask(ic+=1)=0 
  mask(ic+=1)=glnumber 
  mask(ic+=1)=pointtwo
  mask(ic+=1)=0 
  mask(ic+=1)=mmddyy 
  mask(ic+=1)=0 
  mask(ic+=1)=number
! ** Storage Positions ** 
  ! default to the same as order displayed 
  ic=0
  sp(ic+=1)=1 
  sp(ic+=1)=3 
  sp(ic+=1)=4 
  sp(ic+=1)=12 
  sp(ic+=1)=24
  sp(ic+=1)=29 
  sp(ic+=1)=59 
  sp(ic+=1)=68 
  sp(ic+=1)=80
! ** Combo Boxes **                                                   
  ! CL=Field Number  : C$(CL,1)='ComboF'                                
  ! C$(CL,2)=Linked File Name                                           
  ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length                 
  ! C$(CL,5)=Description Position : C$(CL,6)=Description Length         
  ! C$(CL,7)=Index File                                                 
  ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     
  limit_to_list$='1'
! cL=1 : c$(CL,1)='ComboF' 
  ! c$(CL,2)='[Q]\CLmstr\PayMstr.h[cno]' 
  ! c$(CL,3)='1' : c$(CL,4)='8' 
  ! c$(CL,5)='9' : c$(CL,6)='30' 
  ! c$(CL,7)='[Q]\CLmstr\PayIdx1.h[cno]' 
  ! c$(CL,8)=LIMIT_TO_LIST$
return ! /r
XIT: fnxit
include: ertn