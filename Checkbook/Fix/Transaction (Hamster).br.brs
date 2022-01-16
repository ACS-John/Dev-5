! Checkbook Transaction File
!
autoLibrary
  on error goto Ertn
!
  dim cap$*128,lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),mask(11),sln(11),c$(11,8)*256
!
  fnTop(program$,cap$='Transaction (Hamster)')
  gosub BUILD_LAYOUT
  gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
  fnHamster('Transaction',mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
  goto Xit
!
OPEN_FILE: !
  open_file_count=0 ! this value is used in the close_file sub routine
  open #open_file_count+=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],Version=2,KFName=[Q]\CLmstr\TrIdx1.h[cno],Use,RecL=78,KPs=1,KLn=11,Shr',internal,outIn,keyed
  open #open_file_count+=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],Version=2,KFName=[Q]\CLmstr\TrIdx2.h[cno],Use,RecL=78,KPs=28/1,KLn=8/11,Shr',internal,outIn,keyed
  open #open_file_count+=1: 'Name=[Q]\CLmstr\TrMstr.h[cno],Version=2,KFName=[Q]\CLmstr\TrIdx3.h[cno],Use,RecL=78,KPs=16/12/4,KLn=2/4/8,Shr',internal,outIn,keyed
  return
!
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
!
BUILD_LAYOUT: !
  lbl$(1)='Bank'
  lbl$(2)='Transaction Type'
  lbl$(3)='Check/Reference Number'
  lbl$(4)='Check Date'
  lbl$(5)='Amount'
  lbl$(6)='Payee'
  lbl$(7)='Name/Description (1 of 2)'
  lbl$(8)='Name/Description (2 of 2)'
  lbl$(9)='Posting Code'
  lbl$(10)='Statement Date Cleared'
  lbl$(11)='Source Code'
! ** Text Box / Field Display   Lengths   **
  ic=0 ! temporary Item Counter
  mmddyy=8
  ccyymmdd=10
  tln(ic+=1)=2
  tln(ic+=1)=1
  tln(ic+=1)=8
  tln(ic+=1)=mmddyy
  tln(ic+=1)=19
  tln(ic+=1)=8
  tln(ic+=1)=34
  tln(ic+=1)=1
  tln(ic+=1)=1
  tln(ic+=1)=mmddyy
  tln(ic+=1)=1
! ** Field Types **
  ic=0
  fltyp$(ic+=1)='N'
  fltyp$(ic+=1)='N'
  fltyp$(ic+=1)='Cr'
  fltyp$(ic+=1)='G'
  fltyp$(ic+=1)='PD'
  fltyp$(ic+=1)='Cr'
  fltyp$(ic+=1)='C'
  fltyp$(ic+=1)='C'
  fltyp$(ic+=1)='N'
  fltyp$(ic+=1)='N'
  fltyp$(ic+=1)='N'
! ** Field Storage Lengths **
  ic=0
  mmddyy=6 : ccyymmdd=8
  sln(ic+=1)=2
  sln(ic+=1)=1
  sln(ic+=1)=8
  sln(ic+=1)=mmddyy
  sln(ic+=1)=10.2
  sln(ic+=1)=8
  sln(ic+=1)=34
  sln(ic+=1)=1
  sln(ic+=1)=1
  sln(ic+=1)=mmddyy
  sln(ic+=1)=1
! ** Field Masks **
  ic=0
  pointtwo=32 : number=30
  ccyymmdd=3 : mmddyy=1
  mask(ic+=1)=0
  mask(ic+=1)=0
  mask(ic+=1)=0
  mask(ic+=1)=mmddyy
  mask(ic+=1)=pointtwo
  mask(ic+=1)=0
  mask(ic+=1)=0
  mask(ic+=1)=0
  mask(ic+=1)=number
  mask(ic+=1)=mmddyy
  mask(ic+=1)=number
! ** Storage Positions **
  ! default to the same as order displayed
  ic=0
! ** Combo Boxes **
  ! CL=Field Number  : C$(CL,1)='ComboF'
  ! C$(CL,2)=Linked File Name
  ! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
  ! C$(CL,5)=Description Position : C$(CL,6)=Description Length
  ! C$(CL,7)=Index File
  ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
  limit_to_list$='1'
  cl=1 : c$(cl,1)='ComboF'
  c$(cl,2)='[Q]\CLmstr\BankMstr.h[cno]'
  c$(cl,3)='1' : c$(cl,4)='2'
  c$(cl,5)='3' : c$(cl,6)='30'
  c$(cl,7)='[Q]\CLmstr\BankIdx1.h[cno]'
  c$(cl,8)=limit_to_list$
  cl=2 : c$(cl,1)='ComboF'
  c$(cl,2)='S:\Core\Data\TransactionType.dat'
  c$(cl,3)='1' : c$(cl,4)='1'
  c$(cl,5)='2' : c$(cl,6)='25'
  c$(cl,7)='S:\Core\Data\TransactionType.idx'
  c$(cl,8)=limit_to_list$
  cl=6 : c$(cl,1)='ComboF'
  c$(cl,2)='[Q]\CLmstr\PayMstr.h[cno]'
  c$(cl,3)='1' : c$(cl,4)='8'
  c$(cl,5)='9' : c$(cl,6)='30'
  c$(cl,7)='[Q]\CLmstr\PayIdx1.h[cno]'
  c$(cl,8)=limit_to_list$
  cl=9 : c$(cl,1)='ComboF'
  c$(cl,2)='S:\Core\Data\Checkbook\PostingCode.dat'
  c$(cl,3)='1' : c$(cl,4)='1'
  c$(cl,5)='2' : c$(cl,6)='25'
  c$(cl,7)='S:\Core\Data\Checkbook\PostingCode.idx'
  c$(cl,8)=limit_to_list$
  cl=11 : c$(cl,1)='ComboF'
  c$(cl,2)='S:\Core\Data\Checkbook\SourceCode.dat'
  c$(cl,3)='1' : c$(cl,4)='1'
  c$(cl,5)='2' : c$(cl,6)='25'
  c$(cl,7)='S:\Core\Data\Checkbook\SourceCode.idx'
  c$(cl,8)=limit_to_list$
  return

Xit: fnXit
include: ertn
