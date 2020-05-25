! Replace S:\acsGL\Year
! GL - Year File
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2)
 
	fnTop(program$,cap$='Year')
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE : _
	gosub HAMSTER
	goto Xit
 
OPEN_FILE: ! : _
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\GLmstr\Year.h[cno],Version=1,KFName=[Q]\GLmstr\Year-Idx.h[cno],Use,RecL=8,KPs=1,KLn=1,Shr",internal,outIn,keyed
return
 
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
 
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    ** : _
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Year Code" : _
	lbl$(ic+=1)="Name"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=1 : _
	tln(ic+=1)=7
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=1 : _
	sln(ic+=1)=7
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=number : _
	mask(ic+=1)=0
return
 
HAMSTER: !
	fnHamster("Year",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
return
 
Xit: fnXit
 
include: Ertn
 
