! Replace S:\Core\Programs\1099Box
! 1099 Boxes File
 
	autoLibrary
	fnTop(program$,cap$="1099 Boxes")
	on error goto Ertn
 
	dim cap$*128,fltyp$(2),sln(2),mask(2),fln(2),p$(2)*60,lbl$(2)
 
! the Third Item (C 40) is not used and is there for unintended use
	lbl$(1)="Box Number" : lbl$(2)="Description" ! lBL$(3)="NA"
	fln(1)=2 : fln(2)=60 ! flN(3)=40
	mask(1)=1030 ! mASK(3)=20000
	open #1: "Name=[Q]\Data\1099Box.dat,KFName=[Q]\Data\1099Box.Idx,Use,RecL=102,KPs=1,KLn=2,Shr",internal,outIn,keyed
	fnHamster("1099Box",mat lbl$,mat fln,1,mat p$,mat fltyp$,mat sln,mat mask)
Xit: fnXit
 
include: Ertn
 
