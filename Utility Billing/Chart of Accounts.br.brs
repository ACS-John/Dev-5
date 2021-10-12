! Temporary Chart of Accounts for [cursystem] when no GL or CL - Hamster
! pretty useless to the end user - but quite usefull to the programmer

autoLibrary
on error goto Ertn

fnTop(program$)
! r: build_layout

	d(1)=d(2)=1 ! default to fund number and sub number

	dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4)
	ic=0
	! label                   	: Field Types     	: Storage Len 	: display len
	lbl$(ic+=1)="Department"  	: fltyp$(ic)='N' 	: sln(ic)= 3  	: tln(ic)= 3
	lbl$(ic+=1)="Account"     	: fltyp$(ic)='N' 	: sln(ic)= 6  	: tln(ic)= 6
	lbl$(ic+=1)="Sub Account"	: fltyp$(ic)='N' 	: sln(ic)= 3  	: tln(ic)= 3
	lbl$(ic+=1)="Description"	: fltyp$(ic)='C' 	: sln(ic)=50  	: tln(ic)=50
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

! /r
open #h=fnH: "Name=[Q]\UBmstr\GLmstr.h[cno],Version=0,KFName=[Q]\UBmstr\GLIndex.h[cno],Use,RecL=62,KPs=1,KLn=12,Shr",internal,outIn,keyed
fnHamster("ChartOfAccounts",mat lbl$,mat tln,h,mat p$,mat fltyp$,mat sln,mat mask)
close #h:
fnIndex("[Q]\UBmstr\GLmstr.h[cno]", "[Q]\UBmstr\glindex.h[cno]","1 12")
goto Xit
Xit: fnChain("S:\Utility Billing\Post to General Ledger")

include: ertn

