! Replace S:\acsTM\Support
 
	autoLibrary
	on error goto Ertn
 
	dim lbl$(11)*38,tln(11),p$(11)*160,fltyp$(11),sln(11),mask(11),c$(11,8)*256 ! SP(11) - not used
	
	fnTop(program$,'Support 420')
 
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
	gosub HAMSTER: gosub CLOSE_FILE
	execute 'Index S:\Core\Data\acsllc\support.h420  S:\Core\Data\acsllc\support-idx.h420 1/7,6/2,replace,DupKeys'
	goto Xit
 
OPEN_FILE: !
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: 'Name=S:\Core\Data\acsllc\Support.h420,Version=2,KFName=S:\Core\Data\acsllc\Support-Idx.h420,Use,RecL=246,KPs=1/7,KLn=6/2,Shr',internal,outIn,keyed
return
 
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
 
BUILD_LAYOUT: !
! ** Field Labels    **
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Client ID"
	lbl$(ic+=1)="Sys#"
	lbl$(ic+=1)="System ID"
	lbl$(ic+=1)="Starting Date"
	lbl$(ic+=1)="Time Frame"
	lbl$(ic+=1)="Ending Date"
	lbl$(ic+=1)="Cost to User"
	lbl$(ic+=1)="Name"
	lbl$(ic+=1)="Contact (1)"
	lbl$(ic+=1)="Contact (2)"
	lbl$(ic+=1)="Contact (3)"
! ** Text Box / Field Display   Lengths   **
	ic=0 ! temporary Item Counter
	mmddyy=8
	ccyymmdd=10
	tln(ic+=1)=6
	tln(ic+=1)=2
	tln(ic+=1)=2
	tln(ic+=1)=ccyymmdd
	tln(ic+=1)=2
	tln(ic+=1)=ccyymmdd
	tln(ic+=1)=10
	tln(ic+=1)=50
	tln(ic+=1)=50
	tln(ic+=1)=50
	tln(ic+=1)=50
! ** Field Types **
	ic=0
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths **
	ic=0
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=6
	sln(ic+=1)=2
	sln(ic+=1)=2
	sln(ic+=1)=ccyymmdd
	sln(ic+=1)=2
	sln(ic+=1)=ccyymmdd
	sln(ic+=1)=10.2
	sln(ic+=1)=50
	sln(ic+=1)=50
	sln(ic+=1)=50
	sln(ic+=1)=50
! ** Field Masks **
	ic=0
	pointtwo=32 : number=30
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=number
	mask(ic+=1)=number
	mask(ic+=1)=0
	mask(ic+=1)=ccyymmdd
	mask(ic+=1)=0
	mask(ic+=1)=ccyymmdd
	mask(ic+=1)=pointtwo
	mask(ic+=1)=0
	mask(ic+=1)=0
	mask(ic+=1)=0
	mask(ic+=1)=0
! ** Combo Boxes **
! CL=Field Number  : C$(CL,1)='ComboF'
! C$(CL,2)=Linked File Name
! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
! C$(CL,5)=Description Position : C$(CL,6)=Description Length
! C$(CL,7)=Index File
! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
	limit_to_list$='1'
	cl=1 : c$(cl,1)='ComboF'
	c$(cl,2)='S:\Core\Data\acsllc\Clmstr.h420'
	c$(cl,3)='1' : c$(cl,4)='5'
! c$(cl,3)='1' : c$(cl,4)='6'
	c$(cl,5)='6' : c$(cl,6)='30'
! c$(cl,5)='7' : c$(cl,6)='50'
	c$(cl,7)='S:\Core\Data\acsllc\CLIndex.h420'
	c$(cl,8)=limit_to_list$
	cl=3 : c$(cl,1)='ComboF'
	c$(cl,2)='S:\Core\Data\acsllc\Systems.h420'
	c$(cl,3)='1' : c$(cl,4)='2'
	c$(cl,5)='3' : c$(cl,6)='50'
	c$(cl,7)='S:\Core\Data\acsllc\Systems-Idx.h420'
	c$(cl,8)=limit_to_list$
	cl=5 : c$(cl,1)='ComboF'
	c$(cl,2)='S:\Core\Data\acsllc\TimeFrame.h420'
	c$(cl,3)='1' : c$(cl,4)='2'
	c$(cl,5)='3' : c$(cl,6)='50'
	c$(cl,7)='S:\Core\Data\acsllc\TimeFrame-Idx.h420'
	c$(cl,8)=limit_to_list$
return
 
HAMSTER: !
	fnHamster("Support",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
include: ertn
