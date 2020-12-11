! formerly S:\acsGL\bldstdaJ
! Standard Adjustments - Hamster (will need a conversion pgm. now only one adjustment per entry - was 10!)
 
	autoLibrary
	on error goto Ertn
 
	dim lbl$(4)*38,tln(4),p$(4)*160,fltyp$(4),sln(4),mask(4),sp(4),c$(4,8)*40
 
	fnTop(program$)
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE    : gosub OPEN_FILE
	gosub HAMSTER   : gosub FIXGLACCOUNTS : gosub CLOSE_FILE : gosub INDEX
	goto Xit
 
OPEN_FILE: !
	open_file_count=1 ! this value is used in the close_file sub routine
	if exists("[Q]\GLmstr\GLSTdad.h[cno]")=0 then goto L190
	if exists("[Q]\GLmstr\glstdidx.h[cno]")=0 then gosub INDEX
	open #open_file_count: "Name=[Q]\GLmstr\glstdad.h[cno],KFName=[Q]\GLmstr\glstdidx.h[cno],Version=1,Shr",internal,outIn,keyed
	goto L220
L190: open #open_file_count: "Name=[Q]\GLmstr\glstdad.h[cno],Version=1,Replace,RecL=59",internal,outIn
	gosub CLOSE_FILE
	gosub INDEX
L220: return
 
INDEX: !
	execute "Index [Q]\GLmstr\glstdad.h[cno]"&' '&"[Q]\GLmstr\glstdidx.h[cno]" &" 1 12 Replace,DupKeys"
return
 
FIXGLACCOUNTS: ! r: left pad general ledger number and reference number
	restore #open_file_count:
	do
		read #open_file_count, using "form pos 43,c 12": gl$ eof L340
		gl$=lpad$(rtrm$(gl$),12)
		rewrite #open_file_count, using "form pos 43,c 12,": gl$
	loop
L340: !
return ! /r
 
CLOSE_FILE: ! r:
	for j=1 to open_file_count
		close #j: ioerr ignore
	next j
return ! /r
 
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    ** : _
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Reference" : _
	lbl$(ic+=1)="Description" : _
	lbl$(ic+=1)="G/L Number" : _
	lbl$(ic+=1)="Amount"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=12 : _
	tln(ic+=1)=30 : _
	tln(ic+=1)=12 : _
	tln(ic+=1)=10.2
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='PD'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=12 : _
	sln(ic+=1)=30 : _
	sln(ic+=1)=12 : _
	sln(ic+=1)=5.2
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=pointtwo
! ** Storage Positions ** : _
	! starting field position - default to the same as order displayed : _
	ic=0
	sp(ic+=1)=1 : _
	sp(ic+=1)=13 : _
	sp(ic+=1)=43 : _
	sp(ic+=1)=55
! ** Combo Boxes **                                                   : _
	cl=3 : c$(cl,1)='ComboF' : _
	c$(cl,2)="[Q]\GLmstr\GLmstr.h[cno]" : _
	c$(cl,3)="1" : c$(cl,4)="12" : _
	c$(cl,5)="13" : c$(cl,6)="30" : _
	c$(cl,7)="[Q]\GLmstr\glindex.h[cno]" : _
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     : _
	limit_to_list$='1'
return
 
HAMSTER: !
	fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
 
include: ertn
 
	fltyp$(ic+=1)='N'
