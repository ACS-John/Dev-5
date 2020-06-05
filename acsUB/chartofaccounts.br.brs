! Replace S:\acsUB\ChartOfAccounts
! Temporary Chart of Accounts for Payroll when no GL or CB - Hamster : _
	! pretty useless to the end user - but quite usefull to the programmer
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4)
 
	fnTop(program$,cap$='Chart of Accounts')
	fncno(cno)
! Open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",Internal,Input,Relative  : _
	! Read #20,Using 'Form POS 150,2*N 1',Rec=1: D(1),D(2) : _
	! Close #20:
	d(1)=d(2)=1 ! default to fund number and sub number
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE : _
	gosub HAMSTER
	gosub CLOSE_FILE
	fnIndex("[Q]\UBmstr\GLmstr.h[cno]", "[Q]\UBmstr\glindex.h[cno]","1 12")
	goto Xit
 
OPEN_FILE: ! : _
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\UBmstr\GLmstr.h[cno],Version=0,KFName=[Q]\UBmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed
return
 
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
 
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    ** : _
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Department" : _
	lbl$(ic+=1)="Account" : _
	lbl$(ic+=1)="Sub Account" : _
	lbl$(ic+=1)="Description"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=3 : _
	tln(ic+=1)=6 : _
	tln(ic+=1)=3 : _
	tln(ic+=1)=50
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=3 : _
	sln(ic+=1)=6 : _
	sln(ic+=1)=3 : _
	sln(ic+=1)=50
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=0
	if d(1)=0 then mask(1)+=10000
	if d(2)=0 then mask(3)+=10000
return
 
HAMSTER: !
	fnHamster("ChartOfAccounts",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
return
 
Xit: chain "S:\acsUB\postgl"
 
include: Ertn
 
