! Replace S:\Core\Programs\Race
! Attorney file : _
	! with dynamic editor 1.0
 
	autoLibrary
	fnTop(program$,cap$="Race")
	on error goto Ertn
 
	dim cap$*128,lbl$(1),fltyp$(1),sln(1),mask(1),fln(1),p$(1)*18
 
	fncno(cno)
	lbl$(1)="Race"
	fln(1)=18
	mask(1)=2000
	open #1: "Name=S:\Core\Data\Race.dat,RecL=18,Use,Shr",internal,outIn,relative
	fnHamster("Race",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
Xit: fnXit
 
include: Ertn
 
