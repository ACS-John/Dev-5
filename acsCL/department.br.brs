! Replace S:\acsCL\Department
! Departmental breakdown file for monticello and others for claims report
autoLibrary
on error goto Ertn
fnTop(program$,"Department Breakdown")

open #1: "Name=[Q]\CLmstr\dptmstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr",internal,outIn,keyed
close #1:
open #1: "Name=[Q]\CLmstr\dptmstr.h[cno],KFName=[Q]\CLmstr\dptidx1.h[cno],Use,RecL=35,KPs=1,KLn=5,Shr",internal,outIn,keyed

dim lbl$(3)*24              , tln(3)    , fltyp$(3)     , mask(3)
lbl$(1)="Fund Number"       : tln(1)= 3 : fltyp$(1)="N" : mask(1)=3
lbl$(2)="Department Number" : tln(2)= 2 : fltyp$(2)="n" : mask(2)=3
lbl$(3)="Description"       : tln(3)=30 : fltyp$(3)="C" : mask(3)=0
dim p$(3)*160
dim sln(3)
dim c$(6,8)*40

fnHamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)

close #1:

fnIndex('[Q]\CLmstr\DPTMSTR.h[cno]','[Q]\CLmstr\DPTIDX1.h[cno]','1 5')

Xit: fnXit
include: ertn
