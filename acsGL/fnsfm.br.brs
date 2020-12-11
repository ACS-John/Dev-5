! Replace S:\acsGL\fnsfm
! General Ledger Financial Statement Layout - Hamster
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,lbl$(21)*38,tln(21),p$(21)*160,fltyp$(21),sln(21),mask(21),sp(21),c$(21,8)*40
	dim fil$(6)*18,idx$(6)*18,id$(6)*40
 
	fnTop(program$,cap$='Financial Statement Design')
	fncno(cno)
	id$(1)=" 1. Balance Sheet File" : _
	fil$(1)="ACGLFNSB.h[cno]": idx$(1)="agfsidx4.h[cno]"
	id$(2)=" 2. Income Statement File" : _
	fil$(2)="ACGLFNSI.h[cno]": idx$(2)="agfsidx3.h[cno]"
	id$(3)=" 3. Fund Statement / Cash Flow File" : _
	fil$(3)="ACGLFNSF.h[cno]": idx$(3)="agfsidx5.h[cno]"
	id$(4)=" 4. Secondary Balance Sheet File" : _
	fil$(4)="ACGLFNSC.h[cno]": idx$(4)="agfsidx1.h[cno]"
	id$(5)=" 5. Secondary Income Statement File" : _
	fil$(5)="ACGLFNSJ.h[cno]": idx$(5)="agfsidx2.h[cno]"
	id$(6)=" 6. Secondary Fund / Cash Flow File" : _
	fil$(6)="ACGLFNSG.h[cno]": idx$(6)="agfsidx6.h[cno]"
	gosub BUILD_LAYOUT
MAIN: !
	fnTos(sn$="FsDesign") : _
	mylen=20: mypos=mylen+3 : right=1
	fnFra(1,1,6,60,"Financial Statement Choices","Choose the financial statement to work with.")
	fnOpt(1,2,id$(1),0,1) : _
	resp$(1)="True"
	fnOpt(2,2,id$(2) ,0,1) : _
	resp$(2)="False"
	fnOpt(3,2,id$(3),0,1) : _
	resp$(3)="False"
	fnOpt(4,2,id$(4),0,1) : _
	resp$(4)="False"
	fnOpt(5,2,id$(5),0,1) : _
	resp$(5)="False"
	fnOpt(6,2,id$(6),0,1) : _
	resp$(6)="False"
	fnCmdKey("&Next",1,1,0,"Access the chosen financial statement design..")
	fnCmdKey("&Cancel",5,1,0,"Return to main menu.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)="True" then selection=1
	if resp$(2)="True" then selection=2
	if resp$(3)="True" then selection=3
	if resp$(4)="True" then selection=4
	if resp$(5)="True" then selection=5
	if resp$(6)="True" then selection=6
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE : _
	gosub HAMSTER : _
	gosub CLOSE_FILE : _
	gosub INDEX
	goto MAIN
 
OPEN_FILE: ! : _
	open_file_count=0 ! this value is used in the close_file sub routine
	open #open_file_count+=1: "Name=[Q]\GLmstr\"&fil$(selection)&",KFName=[Q]\GLmstr\"&idx$(selection)&",Use,RecL=83,KPs=1,KLn=5,Shr",internal,outIn,keyed
return
 
CLOSE_FILE: for j=1 to open_file_count : close #j: : next j : return
 
INDEX: !
	execute "Index [Q]\GLmstr\"&fil$(selection)&' '&"[Q]\GLmstr\"&idx$(selection)&" 1 5 Replace DupKeys"
return
 
BUILD_LAYOUT: !
	fncno(cno)
! ** Field Labels    ** : _
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="F/S #" : _
	lbl$(ic+=1)="Description" : _
	lbl$(ic+=1)="Type of Entry" : _
	lbl$(ic+=1)="Starting Print" : _
	lbl$(ic+=1)="Lines to Skip"
	lbl$(ic+=1)="Dollar Sign" : _
	lbl$(ic+=1)="Underlines" : _
	lbl$(ic+=1)="Reverse Sign" : _
	lbl$(ic+=1)="B/S Column"
	lbl$(ic+=1)="Print Accumulator" : _
	lbl$(ic+=1)="Clr 1" : _
	lbl$(ic+=1)="Clr 2" : _
	lbl$(ic+=1)="Clr 3"
	lbl$(ic+=1)="Clr 4" : _
	lbl$(ic+=1)="Clr 5" : _
	lbl$(ic+=1)="Clr 6" : _
	lbl$(ic+=1)="Clr 7"
	lbl$(ic+=1)="Clr 8" : _
	lbl$(ic+=1)="Clr 9"
	lbl$(ic+=1)="I/C % Base" : _
	lbl$(ic+=1)="Cost Center"
! ** Text Box / Field Display   Lengths   ** : _
	ic=0 ! temporary Item Counter : _
	mmddyy=8 : _
	ccyymmdd=10
	tln(ic+=1)=5 : _
	tln(ic+=1)=50 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=2 : _
	tln(ic+=1)=2
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=1
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=1
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=1
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=1 : _
	tln(ic+=1)=3
	tln(ic+=1)=5
! ** Field Types ** : _
	ic=0
	fltyp$(ic+=1)='c' : _
	fltyp$(ic+=1)='c' : _
	fltyp$(ic+=1)='C' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N' : _
	fltyp$(ic+=1)='N'
	fltyp$(ic+=1)='N'
! ** Field Storage Lengths ** : _
	ic=0 : _
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=5 : _
	sln(ic+=1)=50 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=2 : _
	sln(ic+=1)=2
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=1
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=1
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=1
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=1 : _
	sln(ic+=1)=3
	sln(ic+=1)=5
! ** Field Masks ** : _
	ic=0 : _
	pointtwo=32 : number=30 : _
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=0 : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number
	mask(ic+=1)=number : _
	mask(ic+=1)=number : _
	mask(ic+=1)=number
	mask(ic+=1)=number
! ** Storage Positions ** : _
	! default to the same as order displayed : _
	ic=0
	sp(ic+=1)=1 : _
	sp(ic+=1)=6 : _
	sp(ic+=1)=56 : _
	sp(ic+=1)=57 : _
	sp(ic+=1)=59
	sp(ic+=1)=61 : _
	sp(ic+=1)=62 : _
	sp(ic+=1)=63 : _
	sp(ic+=1)=64
	sp(ic+=1)=65 : _
	sp(ic+=1)=66 : _
	sp(ic+=1)=67 : _
	sp(ic+=1)=68
	sp(ic+=1)=69 : _
	sp(ic+=1)=70 : _
	sp(ic+=1)=71 : _
	sp(ic+=1)=72
	sp(ic+=1)=73 : _
	sp(ic+=1)=74: : _
	sp(ic+=1)=75
	sp(ic+=1)=78
return
 
HAMSTER: !
	fnHamster("Acglfnsb",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
return
 
Xit: fnXit
 
include: ertn
 
