! Replace S:\acsGL\PayeeType
! Checkbook PayeeType File
autoLibrary
on error goto Ertn

fnTop(program$,"Payee Type")
dim lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),mask(2),sln(2)
lbl$(1)="Payee Type"       : fltyp$(1)="N" : tln(1)= 2 : sln(1)= 2 : mask(1)=1030
lbl$(2)="Type Description" : fltyp$(2)="C" : tln(2)=25 : sln(2)=25
dim c$(2,8)*40

open #1: "Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed
close #1:
open #1: "Name=[Q]\GLmstr\PayeeType.dat,Version=1,KFName=[Q]\GLmstr\PayeeType.Idx,Use,RecL=27,KPs=1,KLn=2,Shr",internal,outIn,keyed
fnHamster("PayeeType",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
Xit: fnXit
include: ertn

