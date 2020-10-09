! Replace S:\acsGL\bankreconciliation
! Bank Reconciliation File - Hamster
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,lbl$(10)*38,tln(10),p$(10)*160,fltyp$(10),sln(10),mask(10),sp(10),c$(10,8)*40
 
	fnTop(program$,cap$='Bank Reconciliation')
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE : _
	gosub HAMSTER : gosub FIXGLACCOUNTS: gosub CLOSE_FILE: gosub INDEX
	goto Xit
 
OPEN_FILE: ! : _
	open_file_count=1 ! this value is used in the close_file sub routine
	if exists("[Q]\GLmstr\bankrec.H[cno]")=0 then goto L190
	if exists("[Q]\GLmstr\glstdidx.h[cno]")=0 then gosub INDEX
	open #open_file_count: "Name=[Q]\GLmstr\bankrec.H[cno],KFName=[Q]\GLmstr\bankrec-idx.H[cno],Version=1,Shr",internal,outIn,keyed
	goto L220
L190: open #open_file_count: "Name=[Q]\GLmstr\bankrec.h[cno],Version=1,Replace,RecL=91",internal,outIn
	gosub CLOSE_FILE
	gosub INDEX
L220: return
 
INDEX: !
	execute "Index [Q]\GLmstr\bankrec.H[cno]"&' '&"[Q]\GLmstr\bankrec-idx.h[cno]" &" 79/3/4 12/1/8 Replace,DupKeys"
return
 
FIXGLACCOUNTS: ! left pad general ledger number and reference number
	restore #open_file_count:
L300: read #open_file_count, using "form pos 43,c 12,pos 4,c 8": gl$,tr$ eof L340
	gl$=lpad$(rtrm$(gl$),12)
	tr$=lpad$(rtrm$(tr$),8)
	rewrite #open_file_count, using "form pos 43,c 12,pos 4,c 8": gl$,tr$
	goto L300
L340: return
 
CLOSE_FILE: for j=1 to open_file_count
		close #j: ioerr L380
L380: next j
return
 
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    ** : _
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Bank G/L" : _
	lbl$(ic+=1)="T/C" : _
	lbl$(ic+=1)="Ref #" : _
	lbl$(ic+=1)="Date"
	lbl$(ic+=1)="Amount" : _
	lbl$(ic+=1)="Payee" : _
	lbl$(ic+=1)="Description" : _
	lbl$(ic+=1)="P/C"
	lbl$(ic+=1)="Cleared" : _
	lbl$(ic+=1)="S/C"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=12 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=8 : _
	tln(ic+=1)=8
	tln(ic+=1)=10.2 : _
	tln(ic+=1)=8 : _
	tln(ic+=1)=35 : _
	tln(ic+=1)=1
	tln(ic+=1)=8 : _
	tln(ic+=1)=1
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='CR' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='PD' : _
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=12 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=8 : _
	sln(ic+=1)=6
	sln(ic+=1)=10.2 : _
	sln(ic+=1)=8 : _
	sln(ic+=1)=35 : _
	sln(ic+=1)=1
	sln(ic+=1)=6 : _
	sln(ic+=1)=1
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0 : _
	mask(ic+=1)=30 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=1
	mask(ic+=1)=10 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=30
	mask(ic+=1)=1 : _
	mask(ic+=1)=30
! ** Storage Positions ** : _
	! starting field position - default to the same as order displayed : _
	ic=0
	sp(ic+=1)=79 : _
	sp(ic+=1)=3 : _
	sp(ic+=1)=4 : _
	sp(ic+=1)=12
	sp(ic+=1)=18 : _
	sp(ic+=1)=28 : _
	sp(ic+=1)=36 : _
	sp(ic+=1)=71
	sp(ic+=1)=72 : _
	sp(ic+=1)=78
! ** Combo Boxes **                                                   : _
	cl=1 : c$(cl,1)='ComboF' : _
	c$(cl,2)="[Q]\GLmstr\GLmstr.h[cno]" : _
	c$(cl,3)="1" : c$(cl,4)="12" : _
	c$(cl,5)="13" : c$(cl,6)="30" : _
	c$(cl,7)="[Q]\GLmstr\glindex.h[cno]" : _
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     : _
	limit_to_list$='1'
! ** Combo Boxes **                                                   : _
	cl=2 : c$(cl,1)='ComboF' : _
	c$(cl,2)="[Q]\GLmstr\transcode.h[cno]" : _
	c$(cl,3)="1" : c$(cl,4)="2" : _
	c$(cl,5)="3" : c$(cl,6)="30" : _
	c$(cl,7)="[Q]\GLmstr\transcode-idx.h[cno]" : _
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     : _
	limit_to_list$='1'
return
 
HAMSTER: !
	fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
 
include: ertn
 
	fltyp$(ic+=1)='N'
