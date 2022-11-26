! Replace S:\acsPR\hourclassification
! Classification file for tracking comp time etc

autoLibrary
on error goto Ertn

dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2),sp(2),c$(2,8)*40

fnTop(program$,'Time Classifications')
gosub BUILD_LAYOUT
gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
gosub HAMSTER
goto Xit

OPEN_FILE: !
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: 'Name=[Q]\PRmstr\hourclass.h[cno],Version=1,KFName=[Q]\PRmstr\hourclass-Idx.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr',i,outIn,k
return

CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return

BUILD_LAYOUT: !
! ** Field Labels    **
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)='Classication Code'
	lbl$(ic+=1)='Classification Name'
! ** Text Box / Field Display   Lengths   **
	ic=0 ! temporary Item Counter
	mmddyy=8
	ccyymmdd=10
	tln(ic+=1)=5
	tln(ic+=1)=30
! ** Field Types **
	ic=0
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths **
	ic=0
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=5
	sln(ic+=1)=30
! ** Field Masks **
	ic=0
	pointtwo=32 : number=30
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0
	mask(ic+=1)=0
! ** Storage Positions **
	! default to the same as order displayed
	ic=0
	sp(ic+=1)=1
	sp(ic+=1)=6
return

HAMSTER: !
	fnHamster('TimeClass',mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return


include: ertn

Xit: fnXit
