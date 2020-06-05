! Replace S:\acsCL\Department
! __Departmental breakdown file for monticello and others for claims report
autoLibrary
on error goto Ertn
fnTop(program$,cap$="Department Breakdown")
 
dim lbl$(3)*24,tln(3),p$(3)*160,fltyp$(3),sln(3),mask(3)
dim c$(6,8)*40,cap$*128
 
lbl$(1)="Fund Number"
lbl$(2)="Department Number"
lbl$(3)="Description"
tln(1)=3 : tln(2)=2 : tln(3)=30
fltyp$(1)="N" : fltyp$(2)="n" : fltyp$(3)="C"
mask(1)=30 : mask(2)=30 : mask(3)=0
open #1: "Name=[Q]\CLmstr\dptmstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr",internal,outIn,keyed
close #1:
open #1: "Name=[Q]\CLmstr\dptmstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr",internal,outIn,keyed
fnHamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
close #1:
fnIndex('[Q]\CLmstr\DPTMSTR.h[cno]','[Q]\CLmstr\DPTIDX1.h[cno]','1 5')
Xit: fnXit
include: Ertn
