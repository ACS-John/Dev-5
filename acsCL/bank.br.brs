autoLibrary
on error goto Ertn
fntop(program$)

dim lbl$(6)*24,tln(6),p$(6)*160,fltyp$(6),sln(6),mask(6)
dim c$(6,8)*256
lbl$(1)="Bank Code" : lbl$(2)="Bank Name" 
lbl$(3)="General Ledger Number" : lbl$(4)="Bank Balance" 
lbl$(5)="Unpaid Invoices" : lbl$(6)="Last Check Number"
tln(1)=2 : tln(2)=30 : tln(3)=12 : tln(4)=11 
tln(5)=11 : tln(6)=8
fltyp$(1)="N" : fltyp$(2)="C" : fltyp$(3)="C" 
fltyp$(4)="PD" : fltyp$(5)="PD" : fltyp$(6)="C"
sln(3)=12 : sln(4)=sln(5)=6.2
mask(1)=1030 
mask(4)=32 : mask(5)=30 : mask(6)=30
cl=3: c$(cl,1)='ComboF' 
c$(cl,2)="[Q]\CLmstr\GLmstr.h[cno]" 
c$(cl,3)='1' : c$(cl,4)='12' 
c$(cl,5)='13' : c$(cl,6)='50' 
c$(cl,7)="[Q]\CLmstr\GLIndex.h[cno]" : c$(cl,8)='1'
open #1: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Use,RecL=64,KPs=1,KLn=2,Shr",internal,outIn,keyed 
close #1: 
open #1: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Use,RecL=64,KPs=1,KLn=2,Shr",internal,outIn,keyed 
fnHamster("Bank",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
close #1: 
execute "Index [Q]\CLmstr\BankMstr.h[cno]"&' '&"[Q]\CLmstr\BankIdx1.h[cno] 1 2 DupKeys Replace Shr -n" ioerr XIT
gosub FIX_GL_NUMBERS
XIT: fnxit

FIX_GL_NUMBERS: ! r:
	open #1: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",internal,outIn,keyed 
	L360: !
		read #1,using "form pos 33,c 12": gl$ eof L410
		gl$=lpad$(rtrm$(gl$),12)
		gl$(1:3)=lpad$(rtrm$(gl$(1:3)),3)
		rewrite #1,using "form pos 33,c 12": gl$
	goto L360
	L410: close #1: 
return ! /r
include: Ertn
