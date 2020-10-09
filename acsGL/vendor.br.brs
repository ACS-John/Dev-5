! Replace S:\acsGL\Vendor
! Vendor file with dynamic editor - hamster
!
	autoLibrary
	fnTop(program$)
	on error goto Ertn

	dim fltyp$(08),fln(08),mask(08),p$(08)*65,lbl$(08)*22,sln(08)
	dim c$(8,8)*40

	lbl$(1)="Vendor"
	lbl$(2)="Name"
	lbl$(3)="Address (1)"
	lbl$(4)="Address (2)"
	lbl$(5)="City, State and Zip"
	lbl$(6)="YTD Purchases"
	lbl$(7)="1099 Box"
	lbl$(8)="Federal ID or SSN"
	fln(1)=8
	fln(2)=35
	fln(3)=20
	fln(4)=20
	fln(5)=20
	fln(6)=9
	fln(7)=2
	fln(8)=11
	sln(6)=5.2
	fltyp$(1)="CR"
	fltyp$(6)="PD"
	fltyp$(7)="N"
	mask(1)=2000
	mask(6)=30
	c$(07,1)='ComboF'
	c$(07,2)="[Q]\Data\1099Box.dat"
	c$(07,3)='1' : c$(07,4)='2'
	c$(07,5)='3' : c$(07,6)='60'
	c$(07,7)="[Q]\Data\1099Box.idx" : c$(07,8)='1'
	open #1: "Name=[Q]\GLmstr\gl1099.h[cno],KFName=[Q]\GLmstr\gl109idx.h[cno],Use,RecL=127,KPs=1,KLn=8,Shr",internal,outIn,keyed 
	fnHamster("gl1099",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask,mat startpos,mat c$)
Xit: fnXit
include: ertn
