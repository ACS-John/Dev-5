! Replace S:\acsCL\UnpaidInvoiceHamster
! Checkbook UnpaidInvoice File
autoLibrary
on error goto Ertn
! Dimension Ony the Arrays you need (this is important for Hamster) 
! Additionally Never use exactaly 10 items in a file.
fnTop(program$,"Unpaid Invoice (Hamster)")
dim cap$*128,lbl$(15)*38,tln(15),p$(15)*160,fltyp$(15),mask(15),sln(15)
dim c$(15,8)*40
gosub BUILD_LAYOUT
! if the file is created and you open it, indexs do not get updated correctly, until you close it and reopen it.
gosub OPEN_FILE
for j=1 to open_file_count 
	close #j: 
next j
gosub OPEN_FILE
fnHamster("UnpaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
goto Xit
OPEN_FILE: ! r:
	open_file_count=0 ! this value is used in the close_file sub routine
	open #first_file=open_file_count+=1: "Name=[Q]\CLmstr\PayTrans.h[cno],Version=2,KFName=[Q]\CLmstr\UnPdIdx1.h[cno],Use,RecL=114,KPs=1,KLn=20,Shr",internal,outIn,keyed 
	open #open_file_count+=1: "Name=[Q]\CLmstr\PayTrans.h[cno],Version=2,KFName=[Q]\CLmstr\UnPdIdx2.h[cno],Use,RecL=114,KPs=31/27/1,KLn=2/4/26,Shr",internal,outIn,keyed 
return ! /r
BUILD_LAYOUT: ! r:
! ** Field Labels **
	lbl$(1)="Vendor Key" 
	lbl$(2)="Invoice Key" 
	lbl$(3)="Invoice Date" 
	lbl$(4)="Due Date" 
	lbl$(5)="Purchase Order Number"
	lbl$(6)="Description" 
	lbl$(7)="Amount" 
	lbl$(8)="Payment Code" 
	lbl$(9)="Bank" 
	lbl$(10)="Check Number"
	lbl$(11)="Date Paid" 
	lbl$(12)="Posting Code" 
	lbl$(13)="Posting Date" 
	lbl$(14)="Discount Amount" 
	lbl$(15)="Discount Due Date"
! ** Field Display Lengths ** 
	mmddyy=8 : ccyymmdd=10 
	! TC=0 ! Text Box Length Item Coutner
	tln(tc+=1)=8 
	tln(tc+=1)=12 
	tln(tc+=1)=mmddyy 
	tln(tc+=1)=mmddyy 
	tln(tc+=1)=12
	tln(tc+=1)=18 
	tln(tc+=1)=10 
	tln(tc+=1)=1 
	tln(tc+=1)=2 
	tln(tc+=1)=8
	tln(tc+=1)=mmddyy 
	tln(tc+=1)=1 
	tln(tc+=1)=6 
	tln(tc+=1)=10 
	tln(tc+=1)=ccyymmdd
! ** Field Types ** 
	! Valid are C, G, N, PD, 
	! Default is 'G' 
	fc=0 ! Field Type Item Counter
	fltyp$(fc+=1)='Cr' 
	fltyp$(fc+=1)='C' 
	fltyp$(fc+=1)='G' 
	fltyp$(fc+=1)='G' 
	fltyp$(fc+=1)='C'
	fltyp$(fc+=1)='C' 
	fltyp$(fc+=1)='N' 
	fltyp$(fc+=1)='N' 
	fltyp$(fc+=1)='N' 
	fltyp$(fc+=1)='G'
	fltyp$(fc+=1)='G' 
	fltyp$(fc+=1)='G' ! XXX 
	fltyp$(fc+=1)='G' ! XXX 
	fltyp$(fc+=1)='N' 
	fltyp$(fc+=1)='N'
! ** Field Storage Lengths ** 
	! sc=0 ! Field Storage Length Item Counter
	sln(sc+=1)=8 
	sln(sc+=1)=12 
	sln(sc+=1)=6 
	sln(sc+=1)=6 
	sln(sc+=1)=12
	sln(sc+=1)=18 
	sln(sc+=1)=10.2 
	sln(sc+=1)=1 
	sln(sc+=1)=2 
	sln(sc+=1)=8
	sln(sc+=1)=6 
	sln(sc+=1)=1 
	sln(sc+=1)=6 
	sln(sc+=1)=10.2 
	sln(sc+=1)=8
! ** Field Mask ** 
	number=30 
	pointtwo=32 ! number with 2 decimal places (no commas)
	mmddyy=1 : ccyymmdd=3 
	mc=0 ! mask item counter
	mask(mc+=1)=none 
	mask(mc+=1)=none 
	mask(mc+=1)=mmddyy 
	mask(mc+=1)=mmddyy 
	mask(mc+=1)=none
	mask(mc+=1)=none 
	mask(mc+=1)=pointtwo 
	mask(mc+=1)=number 
	mask(mc+=1)=number 
	mask(mc+=1)=number
	mask(mc+=1)=mmddyy 
	mask(mc+=1)=number 
	mask(mc+=1)=mmddyy 
	mask(mc+=1)=pointtwo 
	mask(mc+=1)=mmddyy
! ** Storage Position ** 
	sc=0 ! Storage Position Item Counter
! ** Let's Make Some Combo Boxes ** 
	! CL = Item you want a ComboBox on 
	! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len 
	! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File 
	! C$(cl,8)=limit to list option '1'=yes '0'=no 
	limit_to_list$='1'
	cl=8 
	c$(cl,1)='ComboF' 
	c$(cl,2)="[Q]\CLmstr\PaymentCode.dat" 
	c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) 
	c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='25' 
	c$(cl,7)="[Q]\CLmstr\PaymentCode.Idx" : c$(cl,8)=limit_to_list$
	cl=9 
	c$(cl,1)='ComboF' 
	c$(cl,2)="[Q]\CLmstr\BankMstr.h[cno]" 
	c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) 
	c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='30' 
	c$(cl,7)="[Q]\CLmstr\BankIdx1.h[cno]" : c$(cl,8)=limit_to_list$
	cl=12 
	c$(cl,1)='ComboF' 
	c$(cl,2)="S:\acsCL\PostingCode.dat" 
	c$(cl,3)='1' : c$(cl,4)=str$(sln(cl)) 
	c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='25' 
	c$(cl,7)="S:\acsCL\PostingCode.idx" : c$(cl,8)=limit_to_list$
return ! /r
Xit: fnXit
include: ertn
