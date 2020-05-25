! Replace S:\acsCL\PaymentCode
! Checkbook PaymentCode File
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128
	dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),mask(2),sln(2)
	dim c$(2,8)*40
 
	fnTop(program$,cap$="Payment Code")
	fncno(cno)
	lbl$(1)="Payment Code" : _
	lbl$(2)="Code Description"
	tln(1)=1 : tln(2)=25
	fltyp$(1)="N" : fltyp$(2)="C"
	sln(1)=1 : sln(2)=25
	mask(1)=1030
	open #1: "Name=[Q]\CLmstr\PaymentCode.dat,Version=1,KFName=[Q]\CLmstr\PaymentCode.Idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outIn,keyed
	close #1:
	open #1: "Name=[Q]\CLmstr\PaymentCode.dat,Version=1,KFName=[Q]\CLmstr\PaymentCode.Idx,Use,RecL=26,KPs=1,KLn=1,Shr",internal,outIn,keyed
	fnHamster("PaymentCode",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
Xit: fnXit
 
include: Ertn
 
