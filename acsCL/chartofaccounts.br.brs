! Replace S:\acsCL\ChartOfAccounts
! Checkbook Transaction Allocation File - Hamster
	! pretty useless to the end user - but quite usefull to the programmer
!
	autoLibrary
	on error goto Ertn
!
	dim cap$*128,lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4)
!
	fnTop(program$,cap$='Chart of Accounts')
	fncno(cno)
	open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",internal,input,relative
	read #20,using 'Form POS 150,2*N 1',rec=1: d(1),d(2)
	close #20:
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
	gosub HAMSTER
	gosub CLOSE_FILE
	execute "Index [Q]\CLmstr\GLmstr.H[cno]"&' '&"[Q]\GLmstr\glindex.h[cno]1 12,replace,DupKeys" ioerr L140
L140: goto Xit
!
OPEN_FILE: !
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\CLmstr\GLmstr.H[cno],Version=0,KFName=[Q]\CLmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed
	return
!
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
!
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    **
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Department"
	lbl$(ic+=1)="Account"
	lbl$(ic+=1)="Sub Account"
	lbl$(ic+=1)="Description"
! ** Text Box / Field Display   Lengths   **
	ic=0 ! temporary Item Counter
	mmddyy=8
	ccyymmdd=10
	tln(ic+=1)=3
	tln(ic+=1)=6
	tln(ic+=1)=3
	tln(ic+=1)=50
! ** Field Types **
	ic=0
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths **
	ic=0
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=3
	sln(ic+=1)=6
	sln(ic+=1)=3
	sln(ic+=1)=50
! ** Field Masks **
	ic=0
	pointtwo=32 : number=30
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=number
	mask(ic+=1)=number
	mask(ic+=1)=number
	mask(ic+=1)=0
	if d(1)=0 then mask(1)+=10000
	if d(2)=0 then mask(3)+=10000
return
 
HAMSTER: !
	fnHamster("ChartOfAccounts",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
return
 
Xit: fnXit
include: ertn
