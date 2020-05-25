! Replace S:\acsGL\schedule_hamster
! general ledger breakdowns for each schedule
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),sp(1),c$(1,8)*40
 
	fnTop(program$,cap$='Bank Reconciliation')
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE : _
	gosub HAMSTER : gosub FIXGLACCOUNTS: gosub CLOSE_FILE: gosub INDEX
	goto Xit
 
OPEN_FILE: ! : _
	schedule=1 : _
	open_file_count=1 ! this value is used in the close_file sub routine
	if exists("[Q]\GLmstr\Schedule"&str$(schedule)&".h[cno]")=0 then goto L190
	if exists("[Q]\GLmstr\schedule"&str$(schedule)&"-idx.h[cno]")=0 then gosub INDEX
	open #open_file_count: "Name=[Q]\GLmstr\schedule"&str$(schedule)&".H[cno],KFName=[Q]\GLmstr\schedule"&str$(schedule)&"-idx.H[cno],Version=1,Shr",internal,outIn,keyed
	goto L220
L190: open #open_file_count: "Name=[Q]\GLmstr\schedule"&str$(schedule)&".h[cno],Version=1,Replace,RecL=12",internal,outIn
	gosub CLOSE_FILE
	gosub INDEX
L220: return
 
INDEX: !
	execute "Index [Q]\GLmstr\schedule"&str$(schedule)&".H[cno]"&' '&"[Q]\GLmstr\schedule"&str$(schedule)&"-idx.h[cno]" &" 1 12 Replace,DupKeys"
return
 
FIXGLACCOUNTS: ! left pad general ledger number and reference number
	restore #open_file_count:
L300: read #open_file_count, using "form pos 1,c 12": gl$ eof L340
	gl$=lpad$(rtrm$(gl$),12)
	rewrite #open_file_count, using "form pos 1,c 12": gl$
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
	lbl$(ic+=1)="G/L Number"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=12
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=12
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0
! ** Storage Positions ** : _
	! starting field position - default to the same as order displayed : _
	ic=0
	sp(ic+=1)=1
! ** Combo Boxes **                                                   : _
	cl=1 : c$(cl,1)='ComboF' : _
	c$(cl,2)="[Q]\GLmstr\GLmstr.h[cno]" : _
	c$(cl,3)="1" : c$(cl,4)="12" : _
	c$(cl,5)="13": c$(cl,6)="40" : _
	c$(cl,7)="[Q]\GLmstr\glindex.h[cno]" : _
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     : _
	limit_to_list$='1'
! ** Combo Boxes **                                                   : _
	! cL=2 : c$(CL,1)='ComboF' : _
	! c$(CL,2)="[Q]\GLmstr\transcode.h[cno]" : _
	! c$(CL,3)="1" : c$(CL,4)="2" : _
	! c$(CL,5)="3" : c$(CL,6)="30" : _
	! c$(CL,7)="[Q]\GLmstr\transcode-idx.h[cno]" : _
	! ! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)                     : _
	! lIMIT_TO_LIST$='1'
return
 
HAMSTER: !
	fnHamster("schgl",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
 
include: Ertn
 
	fltyp$(ic+=1)='N'
