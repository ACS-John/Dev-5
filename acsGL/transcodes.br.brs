! Replace S:\acsGL\transcodes
! Special transactions type file - Hamster
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,lbl$(2)*38,tln(2),p$(2)*160,fltyp$(2),sln(2),mask(2),sp(2),c$(2,8)*40
 
	fnTop(program$,cap$='Transaction Codes')
	gosub BUILD_LAYOUT
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE : _
	gosub HAMSTER : gosub CLOSE_FILE: gosub INDEX
	goto Xit
 
OPEN_FILE: ! : _
	open_file_count=1 ! this value is used in the close_file sub routine
	if exists("[Q]\GLmstr\transcodes.h[cno]")=0 then goto L190
	if exists("[Q]\GLmstr\transcodes-idx.h[cno]")=0 then gosub INDEX
	open #open_file_count: "Name=[Q]\GLmstr\transcodes.H[cno],KFName=[Q]\GLmstr\transcodes-idx.H[cno],Version=1,Shr",internal,outIn,keyed
	goto L220
L190: open #open_file_count: "Name=[Q]\GLmstr\transcodes.h[cno],Version=1,Replace,RecL=90",internal,outIn
	gosub CLOSE_FILE
	gosub INDEX
L220: return
 
INDEX: !
	execute "Index [Q]\GLmstr\transcodes.h[cno]"&' '&"[Q]\GLmstr\transcodes-idx.h[cno]" &" 1 2 Replace,DupKeys"
return
 
CLOSE_FILE: for j=1 to open_file_count
		close #j: ioerr L300
L300: next j
return
 
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    ** : _
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="Trans Code" : _
	lbl$(ic+=1)="Description"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=2 : _
	tln(ic+=1)=20
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='n' : _
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=2 : _
	sln(ic+=1)=20
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=number : _
	mask(ic+=1)=0
! ** Storage Positions ** : _
	! starting field position - default to the same as order displayed : _
	ic=0
	sp(ic+=1)=1 : _
	sp(ic+=1)=3
! ** Combo Boxes **
! cL=1 : c$(CL,1)='ComboF' : _
	! c$(CL,2)='[Q]\CLmstr\PayMstr.h[cno]' : _
	! c$(CL,3)='1' : c$(CL,4)='8' : _
	! c$(CL,5)='9' : c$(CL,6)='30' : _
	! c$(CL,7)='[Q]\CLmstr\PayIdx1.h[cno]' : _
	! c$(CL,8)=LIMIT_TO_LIST$
return
 
HAMSTER: !
	fnHamster("TrAlloc",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
 
include: ertn
 
	fltyp$(ic+=1)='N'
