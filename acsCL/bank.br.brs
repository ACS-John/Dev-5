autoLibrary
on error goto Ertn
fnTop(program$)
 
dim lbl$(6)*24                    	, tln(6)    , fltyp$(6)
lbl$(1)="Bank Code"               	: tln(1)= 2 : fltyp$(1)="N"
lbl$(2)="Bank Name"               	: tln(2)=30 : fltyp$(2)="C"
lbl$(3)="General Ledger Number"  	: tln(3)=12 : fltyp$(3)="C"
lbl$(4)="Bank Balance"           	: tln(4)=11 : fltyp$(4)="PD"
lbl$(5)="Unpaid Invoices"        	: tln(5)=11 : fltyp$(5)="PD"
lbl$(6)="Last Check Number"      	: tln(6)= 8 : fltyp$(6)="C"

dim sln(6)
sln(3)=12
sln(4)=sln(5)=6.2

dim mask(6)
mask(1)=1030
mask(4)=32
mask(5)=30
mask(6)=30
dim c$(6,8)*256

cl=3
c$(cl,1)='ComboF'
c$(cl,2)="[Q]\CLmstr\GLmstr.h[cno]"
c$(cl,3)='1'
c$(cl,4)='12'
c$(cl,5)='13'
c$(cl,6)='50'
c$(cl,7)="[Q]\CLmstr\GLIndex.h[cno]"
c$(cl,8)='1'
open #1: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Use,RecL=64,KPs=1,KLn=2,Shr",internal,outIn,keyed
dim p$(6)*160
fnHamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
close #1:
fnIndex('[Q]\CLmstr\BankMstr.h[cno]','[Q]\CLmstr\BankIdx1.h[cno]','1 2') ! ioerr Xit
gosub FixGlNumbers
Xit: fnXit
 
FixGlNumbers: ! r:
	open #1: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed
	do
		read #1,using "form pos 33,c 12": gl$ eof L410
		gl$=lpad$(rtrm$(gl$),12)
		gl$(1:3)=lpad$(rtrm$(gl$(1:3)),3)
		rewrite #1,using "form pos 33,c 12": gl$
	loop
	L410: close #1:
return ! /r
include: ertn
