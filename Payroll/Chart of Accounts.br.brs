! Replace S:\acsPR\ChartOfAccounts
! Temporary Chart of Accounts for Payroll when no GL or CB - Hamster
! pretty useless to the end user - but quite usefull to the programmer
autoLibrary
on error goto Ertn
fnTop(program$)
! r: Build Layout
	dim fltyp$(4),sln(4),tln(4),mask(4),lbl$(4)*38
	dim p$(4)*160
	! FieldType    : Storage Len : Display Len : mask       : Label
	fltyp$(1)='N' : sln(1)=3  : tln(1)=3  : mask(1)=30 : lbl$(1)="Department"
	fltyp$(2)='N' : sln(2)=6  : tln(2)=6  : mask(2)=30 : lbl$(2)="Account"
	fltyp$(3)='N' : sln(3)=3  : tln(3)=3  : mask(3)=30 : lbl$(3)="Sub Account"
	fltyp$(4)='C' : sln(4)=50 : tln(4)=50 : mask(4)=0  : lbl$(4)="Description"
	! Open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,i,r
	! Read #20,Using 'Form POS 150,2*N 1',Rec=1: D(1),D(2)
	! Close #20:
	d(1)=d(2)=1 ! default to fund number and sub number
	if d(1)=0 then mask(1)+=10000
	if d(2)=0 then mask(3)+=10000
! /r

! gosub OpenFile : gosub CloseFile : gosub OpenFile
open #1: "Name=[Q]\PRmstr\GLMstr.h[cno],Version=0,KFName=[Q]\PRmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed
fnHamster("ChartOfAccounts",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask)
! gosub CloseFile
close #1:
fnIndex('[Q]\PRmstr\glmstr.h[cno]','[Q]\PRmstr\glindex.h[cno]','1 12')
goto Xit

! OpenFile: ! r:
! 	OpenFile_count=0 ! this value is used in the CloseFile sub routine
! 	open #OpenFile_count+=1: "Name=[Q]\PRmstr\GLMstr.h[cno],Version=0,KFName=[Q]\PRmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed
! return ! /r
! CloseFile: for j=1 to OpenFile_count : close #j: : next j : return

Xit: fnXit
include: ertn
