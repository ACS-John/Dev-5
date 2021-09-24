! Replace S:\Core\GLControl
! Checkbook Transaction Allocation File - Hamster
! pretty useless to the end user - but quite usefull to the programmer
autoLibrary
on error goto Ertn
fnTop(program$,'General Ledger Control')
dim lbl$(6)*38,tln(6),p$(6)*160,fltyp$(6),sln(6),mask(6),c$(6,8)*40
 
gosub BUILD_LAYOUT
gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
fnHamster("GLControl",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)

! r: Fix Gl Accounts  
	! left pad general ledger number and reference number
	restore #1:
	do
		read #1, using "form pos 43,c 12": gl$ eof FgaEoF
		gl$=lpad$(rtrm$(gl$),12)
		rewrite #1, using "form pos 43,c 12,": gl$
	loop
	FgaEoF: !
! /r

goto Xit
 
OPEN_FILE: !
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\CLmstr\FundMstr.h[cno],KFName=[Q]\CLmstr\FundIdx1.h[cno],Use,RecL=75,KPs=1,KLn=3,Shr",internal,outIn,keyed
return
 
CLOSE_FILE: close #1: : return
 
BUILD_LAYOUT: ! r:
	fncno(cno)
! ** Field Labels    **
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Fund Number"
	lbl$(ic+=1)="Description"
	lbl$(ic+=1)="General Ledger Number Due To"
	lbl$(ic+=1)="General Ledger Number Due From"
	lbl$(ic+=1)="General Ledger Number Accounts Payable"
	lbl$(ic+=1)="General Ledger Number for Discounts"
! ** Text Box / Field Display   Lengths   **
	ic=0 ! temporary Item Counter
	mmddyy=8
	ccyymmdd=10
	tln(ic+=1)=3
	tln(ic+=1)=30
	tln(ic+=1)=9
	tln(ic+=1)=9
	tln(ic+=1)=12
	tln(ic+=1)=12
! ** Field Types **
	ic=0
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths **
	ic=0
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=3
	sln(ic+=1)=30
	sln(ic+=1)=9
	sln(ic+=1)=9
	sln(ic+=1)=12
	sln(ic+=1)=12
! ** Field Masks **
	ic=0
	pointtwo=32 : number=30
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=1000+number
	mask(ic+=1)=0
	mask(ic+=1)=0
	mask(ic+=1)=0
	mask(ic+=1)=0
	mask(ic+=1)=0
! ** Combo Boxes **
	! CL=Field Number  : C$(CL,1)='ComboF'
	! C$(CL,2)=Linked File Name
	! C$(CL,3)=Key Position         : C$(CL,4)=Key Length
	! C$(CL,5)=Description Position : C$(CL,6)=Description Length
	! C$(CL,7)=Index File
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
	limit_to_list$='1'
	cl=3: c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\CLmstr\GLmstr.h[cno]"
	c$(cl,3)='4' : c$(cl,4)='9'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)="[Q]\CLmstr\GLIndex.h[cno]" : c$(cl,8)=limit_to_list$
	cl=4: c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\CLmstr\GLmstr.h[cno]"
	c$(cl,3)='4' : c$(cl,4)='9'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)="[Q]\CLmstr\GLIndex.h[cno]" : c$(cl,8)='1'
	cl=5: c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\CLmstr\GLmstr.h[cno]"
	c$(cl,3)='1' : c$(cl,4)='12'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)="[Q]\CLmstr\GLIndex.h[cno]" : c$(cl,8)='1'
	cl=6: c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\CLmstr\GLmstr.h[cno]"
	c$(cl,3)='1' : c$(cl,4)='12'
	c$(cl,5)='13' : c$(cl,6)='50'
	c$(cl,7)="[Q]\CLmstr\GLIndex.h[cno]" : c$(cl,8)='1'
return ! /r
 
Xit: fnXit
 
include: ertn
