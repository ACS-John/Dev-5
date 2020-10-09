! Replace S:\acsCL\PaidInvoice
! Checkbook PaidInvoice File
autoLibrary
on error goto Ertn
 
! Dimension Ony the Arrays you need (this is important for Hamster)
! Additionally Never use exactaly 10 items in a file.
dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),mask(4),sln(4)
dim c$(4,8)*256
fnTop(program$,"Paid Invoice")
 
gosub BUILD_LAYOUT
gosub OPEN_FILE
for j=1 to open_file_count
	close #j:
next j
gosub OPEN_FILE
fnHamster("PaidInvoice",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
goto Xit
 
OPEN_FILE: ! (open_file_count)
	open_file_count=0
	open #open_file_count+=1: "Name=[Q]\CLmstr\IvPaid.h[cno],KFName=[Q]\CLmstr\IVIndex.h[cno],Use,RecL=34,KPs=1,KLn=20,Shr",internal,outIn,keyed
	! Version=0 or Version=1
return
!
BUILD_LAYOUT: ! r:
! ** Field Labels **
	lbl$(1)="Vendor Key"
	lbl$(2)="Invoice Key"
	lbl$(3)="Date Paid"
	lbl$(4)="Check Number"
! ** Field Display Lengths **
	mmddyy=8 : ccyymmdd=10
	! TC=0 ! Text Box Length Item Coutner
	tln(tc+=1)=8
	tln(tc+=1)=12
	tln(tc+=1)=mmddyy
	tln(tc+=1)=8
! ** Field Types **
	! Valid are C, G, N, PD,
	! Default is 'G'
	fc=0 ! Field Type Item Counter
	fltyp$(fc+=1)='C'
	fltyp$(fc+=1)='C'
	fltyp$(fc+=1)='G'
	fltyp$(fc+=1)='G'
! ** Field Storage Lengths **
	! sc=0 ! Field Storage Length Item Counter
	sln(sc+=1)=8
	sln(sc+=1)=12
	sln(sc+=1)=6
	sln(sc+=1)=8
! ** Field Mask **
	number=30
	pointtwo=32 ! number with 2 decimal places (no commas)!:
	mmddyy=1 : ccyymmdd=3
	mc=0 ! mask item counter
	mask(mc+=1)=none
	mask(mc+=1)=none
	mask(mc+=1)=mmddyy
	mask(mc+=1)=number
! ** Storage Position **
	sc=0 ! Storage Position Item Counter
! ** Let's Make Some Combo Boxes **
	! CL = Item you want a ComboBox on
	! C$(cl,2)=linked file : c$(cl,3)=key pos c$(cl,4)=key len
	! c$(cl,5)=desc pos c$(cl,6)=desc len  C$(cl,7)=Index File
	! C$(cl,8)=limit to list option '1'=yes '0'=no
	limit_to_list$='1'
	cl=1
	c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\CLmstr\PayMstr.h[cno]"
	c$(cl,3)='1' : c$(cl,4)=str$(sln(cl))
	c$(cl,5)=str$(sln(cl)+1) : c$(cl,6)='30'
	c$(cl,7)="[Q]\CLmstr\PayIdx1.h[cno]" : c$(cl,8)=limit_to_list$
! cL=2
	! c$(CL,1)='ComboF'
	! c$(CL,2)='[Q]\CLmstr\BankMstr.h[cno]'
	! c$(CL,3)='1' : c$(CL,4)=STR$(SLN(CL))
	! c$(CL,5)=STR$(SLN(CL)+1) : c$(CL,6)='30'
	! c$(CL,7)='[Q]\CLmstr\BankIdx1.h[cno]' : c$(CL,8)=LIMIT_TO_LIST$
! cL=4
	! c$(CL,1)='ComboF'
	! c$(CL,2)='[Q]\CLmstr\TrMstr.h[cno]'
	! c$(CL,3)='1' : c$(CL,4)=STR$(SLN(CL))
	! c$(CL,5)=STR$(SLN(CL)+1) : c$(CL,6)='30'
	! c$(CL,7)='[Q]\CLmstr\BankIdx1.h[cno]' : c$(CL,8)=LIMIT_TO_LIST$
return ! /r
Xit: fnXit
include: ertn
