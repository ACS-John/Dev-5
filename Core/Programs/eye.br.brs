! Replace S:\Core\Programs\Eye
! Eye Color file
 
	autoLibrary
	fnTop(program$,cap$="Eye Color")
	on error goto Ertn
 
	dim cap$*128,lbl$(1),fltyp$(1),sln(1),mask(1),fln(1),p$(1)*10
 
	fncno(cno)
	lbl$(1)="Eye Color"
	fln(1)=10
	mask(1)=2000
	open #1: "Name=S:\Core\Data\Eye.dat,RecL=10,Use,Shr",i,outi,r
	fnHamster("Eye",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
Xit: fnXit
 
include: ertn
 
