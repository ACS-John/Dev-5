! Replace S:\acsGL\Year
! GL - Year File
 
autoLibrary
on error goto Ertn

dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2)

fnTop(program$,'Year')
! r: BUILD_LAYOUT
! ** Field Labels  **  Field Types ** Text Box Display Lengths ** Storage Lengths
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Year Code"	: fltyp$(ic)='N' : tln(ic)=1 : sln(ic)=1 : mask(ic)=30 ! number=30 
	lbl$(ic+=1)="Name"      	: fltyp$(ic)='C' : tln(ic)=7 : sln(ic)=7 : mask(ic)=0
! /r
gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE 
fnHamster("Year",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
goto Xit
 
OPEN_FILE: ! 
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\GLmstr\Year.h[cno],Version=1,KFName=[Q]\GLmstr\Year-Idx.h[cno],Use,RecL=8,KPs=1,KLn=1,Shr",internal,outIn,keyed
return
 
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return

Xit: fnXit
 
include: ertn
 
