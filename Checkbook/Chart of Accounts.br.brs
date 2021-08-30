! Checkbook Transaction Allocation File - Hamster
! pretty useless to the end user - but quite usefull to the programmer
autoLibrary
on error goto Ertn
fnTop(program$)

open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,i,r
read #20,using 'Form POS 150,2*N 1',rec=1: d(1),d(2)
close #20:
! r: BUILD_LAYOUT
dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4)
! ** Field Labels    ** Display   Lengths  ** Field Types ** Field Storage Lengths ** Field Masks **
ic=0 ! temporary Item Counter
lbl$(ic+=1)="Department"   	: tln(ic)= 3 	: fltyp$(ic)='N' 	: sln(ic)= 3 	: mask(ic)=30 : if d(1)=0 then mask(1)+=10000
lbl$(ic+=1)="Account"      	: tln(ic)= 6 	: fltyp$(ic)='N' 	: sln(ic)= 6 	: mask(ic)=30
lbl$(ic+=1)="Sub Account"  	: tln(ic)= 3 	: fltyp$(ic)='N' 	: sln(ic)= 3 	: mask(ic)=30 : if d(2)=0 then mask(3)+=10000
lbl$(ic+=1)="Description"  	: tln(ic)=50 	: fltyp$(ic)='C' 	: sln(ic)=50 	: mask(ic)=0
! /r
gosub OPEN_FILE ! gosub CLOSE_FILE : gosub OPEN_FILE
fnHamster("ChartOfAccounts",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
gosub CLOSE_FILE
fnIndex('[Q]\CLmstr\GLmstr.h[cno]','[Q]\GLmstr\glindex.h[cno]','1 12')
goto Xit
OPEN_FILE: ! r:
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\CLmstr\GLmstr.h[cno],Version=0,KFName=[Q]\CLmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed
return ! /r
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
Xit: fnXit
include: ertn
