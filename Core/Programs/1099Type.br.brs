! Replace S:\Core\Programs\1099Type
! 1099Type file
 
	autoLibrary
	fnTop(program$,cap$="1099Type")
	on error goto Ertn
 
	dim cap$*128,fltyp$(2),p$(2)*25
 
	fncno(cno)
	lbl$(1)="Key" : lbl$(2)="Desc"
	fln(1)=1 : fln(2)=25
	open #1: "Name=S:\Core\Data\1099Type.dat,KFName=S:\Core\Data\1099Type.idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outIn,keyed
	close #1:
	open #1: "Name=S:\Core\Data\1099Type.dat,KFName=S:\Core\Data\1099Type.idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outIn,keyed
	fnHamster("1099Type",mat lbl$,mat fln,1,mat p$)
Xit: fnXit
 
include: Ertn
 
