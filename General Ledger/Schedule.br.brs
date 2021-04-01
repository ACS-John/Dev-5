! formerly S:\acsGL\glSchFM
! Schedule File  (Maintenance routines) was Form POS 1,N 2,2*C 78,3*N  1,80*c 12  now Form POS 1,N 3,2*C 78,3*N  1  Breakdowns in seperate file
! r: setup, open files, library, set constants, on err, etc
	autoLibrary
	on error goto Ertn
 
	dim gl$(80)*12
	dim sn$*78,ft$*78
	dim schnam$*78,ml$(3)*80
	dim option$(6)*60,item$(7)*80
	dim resp$(7)*80
 
	fnTop(program$)
	gosub BUILD_LAYOUT
	if exists("[Q]\GLmstr\acglschs.h[cno]")=0 then
		close #10: ioerr ignore
		open #10: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],KFName=[Q]\GLmstr\schindex.h[cno]",internal,outIn,keyed ioerr ignore
		close #10,free: ioerr ignore
		CreateAcGlSchs: open #10: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],SIZE=0,RecL=162",internal,output
		CloseAcGlSchs: close #10: ioerr ignore
		close #11: ioerr ignore
		gosub INDEX
	else if ~exists("[Q]\GLmstr\schindex.h[cno]")or ~exists("[Q]\GLmstr\schindx2.h[cno]")=0 then
		gosub INDEX
	end if
	L210: !
	open #schedule:=10: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],KFName=[Q]\GLmstr\schindex.h[cno],Shr",internal,outIn,keyed ioerr L1580
	open #11: "Name=[Q]\GLmstr\ACGLSCHS.h[cno],KFName=[Q]\GLmstr\SchIndX2.h[cno],Shr",internal,outIn,keyed ioerr CloseAcGlSchs
	goto SCHEDULEGRID
	close #10: ioerr ignore
	execute "Index [Q]\GLmstr\ACGLSCHS.h[cno]"&' '&"[Q]\GLmstr\SchIndX2.h[cno] 3 30 Replace DupKeys -n"
goto L210 ! /r
SCHEDULEGRID: ! r:
	fnTos(sn$="Schedule")
	respc=0
	mat chdr$(7) : mat cmask$(7) : mat flxitm$(7)
	chdr$(1)="Rec"
	chdr$(2)="Schedule #" : chdr$(3)="Schedule Name"
	chdr$(4)="Footnote" : chdr$(5)="Dollar"
	chdr$(6)="Reverse Sign" : chdr$(7)="Choice"
	cmask$(2)='30' : cmask$(3)=cmask$(4)=''
	cmask$(5)='30' : cmask$(6)='30'
	cmask$(6)='30'
	fnflexinit1('schedulegl',lc=1,1,10,70,mat chdr$,mat cmask$,1)
	restore #10:
READ_SCHEDULE: ! read schedule file
	read #schedule,using 'Form POS 1,N 3,2*C 78,3*N 1': sn,schnam$,ft$,dp,rs,cm eof EO_SCHEDULE_GRID noRec L350
	item$(1)=str$(rec(schedule))
	item$(2)=str$(sn): item$(3)=schnam$: item$(4)=ft$
	item$(5)=str$(dp) : item$(6)=str$(rs) : item$(7)=str$(cm)
	fnflexadd1(mat item$)
L350: goto READ_SCHEDULE
EO_SCHEDULE_GRID: !
	fnCmdKey("&Add",1,0,0,"Allows you to add new schedules.")
 
	fnCmdKey("&Edit",2,1,0,"Highlight any record and press Enter or click Edit to change any existing schedule.")
	fnCmdKey("&Delete",8,0,0,"Highlight any record and click Delete to remove the schedule.")
! fnCmdKey("&Print",3,0,0,"Takes you directly to the pr Schedules option")
	fnCmdKey("E&xit",5,0,1,"Exits to main menu")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	add=edit=0
	editrec=val(resp$(1))
	if ckey=2 then edit=1
	if ckey=3 then chain "S:\acsGL\acglschp" ! prints prints a schedule
	if ckey=1 then
		add=1
		sn=dp=rs=cm=0
		schnam$=ft$=""
		goto ADD_EDIT_SCHEDULES ! add
	else if ckey=2 then
		read #schedule,using 'Form POS 1,N 3,2*C 78,3*N 1',rec=editrec: sn,schnam$,ft$,dp,rs,cm noRec SCHEDULEGRID
		holdsn=sn
		goto ADD_EDIT_SCHEDULES
	else if ckey=8 then
		read #schedule,using 'Form POS 1,N 3,2*C 78,3*N 1',rec=editrec,release: sn,schnam$,ft$,dp,rs,cm noRec SCHEDULEGRID
		gosub DELETEIT
		goto SCHEDULEGRID
	end if
	pause
! /r
ADD_EDIT_SCHEDULES: ! r:
	fnTos(sn$="Schedule1")
	mylen=20: mypos=mylen+3 : right=1
	fnLbl(1,1,"Schedule Number:",mylen,right)
	fncombof('glschedule',1,mypos,0,"[Q]\GLmstr\acglschs.h[cno]",1,3,4,30,"[Q]\GLmstr\schindex.h[cno]",add_all)
	if edit=1 then resp$(1)=str$(sn)
	if add=1 then resp$(1)=""
	fnLbl(2,1,"Schedule Nane::",mylen,right)
	fnTxt(2,mypos,80,0,left,"",0,"",0 )
	resp$(2)=schnam$
	fnLbl(3,1,"Footnote:",mylen,right)
	fnTxt(3,mypos,80,0,left,"",0,"",0 )
	resp$(3)=ft$
	fnChk(4,mypos,"Print Dollar Signs:",1)
	if dp=1 then resp$(4)="True" else resp$(4)="False"
	fnChk(5,mypos,"Reverse Sign:",1)
	if rs=1 then resp$(5)="True" else resp$(5)="False"
	fnLbl(6,1,"Type of Schedule:",mylen,right)
	option$(1)="Print Year to Date Only"
	option$(2)="Print Current Month and Year to Date"
	option$(3)="Print Comparison (Income and Expense Accounts"
	option$(4)="Print Comparison (Balance Sheet Accounts"
	fncomboa("TypeOfPrint",6,mypos,mat option$,"You can choose any of the four types of schedules.",60)
	if cm=0 then cm=1
	resp$(6)=option$(cm)
	fnCmdKey("&Display G/L #'s",1,1,0,"Allows you to review, add, or change the G/L accounts that are contained in this schedule.")
	fnCmdKey("&Cancel",5,0,1,"Returns to list of schedules withouit saving any changes.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto SCHEDULEGRID
	sn=val(resp$(1)(1:3)) conv ADD_EDIT_SCHEDULES
	schnam$=resp$(2)
	ft$=resp$(3)
	if resp$(4)="True" then dp=1 else dp=0
	if resp$(5)="True" then rs=1 else rs=0
	for j=1 to j
		if resp$(6)=option$(j) then cm=j
	next j
	if edit=1 then goto REWRITE_EXISTING_SCHEDULE
	if add=1 then goto WRITE_NEW_SCHEDULE
	pause
! /r
REWRITE_EXISTING_SCHEDULE: ! r:
	if sn=0 then goto ADD_EDIT_SCHEDULES
	if holdsn<>sn and holdsn<>0 then
		goto MSGBOX1
	else
		goto L950
	end if
	MSGBOX1: !
	mat ml$(3)
	ml$(1)="You are changing schedule # "&str$(holdsn)&" to "
	ml$(2)="schedule # "&str$(sn)&".  Click OK to continue, "
	ml$(3)="else Cancel to prevent changing the #."
	fnmsgbox(mat ml$,resp$,'',49)
	if resp$="OK" then
		execute "Copy [Q]\GLmstr\schedule"&str$(holdsn)&".h[cno]"&' '&"[Q]\GLmstr\schedule"&str$(sn)&".h[cno] -n" ioerr ignore ! move breakdowns to new schedule #
		L950: !
		rewrite #10,using L1010,rec=editrec: sn,schnam$,ft$,dp,rs,cm
		goto SCHEDULE_BREAKDOWN
	end if
goto ADD_EDIT_SCHEDULES ! /r
WRITE_NEW_SCHEDULE: ! r:
	write #10,using L1010: sn,schnam$,ft$,dp,rs,cm
	L1010: form pos 1,n 3,2*c 78,3*n 1
	new1=1
goto SCHEDULE_BREAKDOWN ! /r
INDEX: ! r: (main schedule files)
	execute "Index [Q]\GLmstr\ACGLSCHS.h[cno]"&' '&"[Q]\GLmstr\schindex.h[cno] 1 3 Replace DupKeys -n"
	execute "Index [Q]\GLmstr\ACGLSCHS.h[cno]"&' '&"[Q]\GLmstr\SchIndX2.h[cno] 4 30 Replace DupKeys -n"
return ! /r
INDEX2: ! r: index to gl breakdowns
	execute "Index [Q]\GLmstr\schedule"&str$(sn)&".h[cno]"&' '&"[Q]\GLmstr\schedule"&str$(sn)&"-idx.h[cno]" &" 1 12 Replace,DupKeys" ioerr ignore
return ! /r
! PROOF: ! r:
!   restore #10,key>="  ": eof ignore ioerr ADD_EDIT_SCHEDULES
!   ! fnwait("Printing: Please wait...",1)
!   ! on fkey 5 goto L1530
!   fnopenprn
!   do
!     read #10,using L1010: sn,schnam$,ft$,dp,rs,cm eof L1530
!     pr #255,using L1250: date$('mm/dd/yy'),time$,"Print Schedules File Proof List"
!     L1250: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos 51,c 31,skip 1
!     pr #255,using L1270: env$('cnam'),dat$
!     L1270: form pos 1,cc 122,skip 1,pos 1,cc 122,skip 2
!     pr #255,using L1290: "Schedule Number",sn
!     L1290: form pos 1,c 15,pos 20,pic(zz),skip 1
!     pr #255,using L1310: "Schedule Name  ",schnam$
!     L1310: form pos 1,c 15,pos 20,c 80,skip 1
!     pr #255,using L1310: "FootNote       ",ft$
!     pr #255,using L1360: "Dollar Sign Print",dp
!     pr #255,using L1360: "Reverse Sign",rs
!     pr #255,using L1360: "Print Current Month Figures",cm
!     L1360: form pos 1,c 27,pos 30,pic(#),skip 1
!     pr #255: tab(29);"Dept  Account Sub"
!     for j=1 to 80
!       if gl$(j)="  0     0  0" then goto L1470
!       if j1><48 then goto L1450
!       pr #255: newpage
!       pr #255,using L1430: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
!       L1430: form skip 6,pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
!       goto L1470
!       L1450: !
!       pr #255,using L1460: "G/L Account Number",gl$(j)(1:3),gl$(j)(4:9),gl$(j)(10:12)
!       L1460: form pos 1,c 18,pos 30,c 3,x 2,c 6,x 2,c 3,skip 1
!       L1470: !
!       j1=j1+1
!     next j
!     j1=0
!     pr #255: newpage
!   loop
! !
!   L1530: !
!   fncloseprn
!   on fkey 5 ignore
!   if fnprocess=1 then goto Xit
! goto ADD_EDIT_SCHEDULES ! /r
Xit: fnXit
L1580: if err=4152 then goto CreateAcGlSchs else goto ERTN
include: ertn
SCHEDULE_BREAKDOWN: ! r:
	! general ledger breakdowns for each schedule
 
	dim lbl$(1)*38,tln(1),p$(1)*160,fltyp$(1),sln(1),mask(1),sp(1),c$(1,8)*40
 
	gosub OPEN_FILE : gosub CLOSE_FILE : gosub OPEN_FILE
	fnHamster("schgl",mat lbl$,mat tln,1,mat p$,mat fltyp$,mat sln,mat mask,mat sp,mat c$)
	gosub FIXGLACCOUNTS
	gosub CLOSE_FILE
	gosub INDEX2
goto SCHEDULEGRID ! /r
OPEN_FILE: ! r:
	open_file_count=1 ! this value is used in the close_file sub routine
	close #open_file_count: ioerr ignore
	if exists("[Q]\GLmstr\Schedule"&str$(sn)&".h[cno]")=0 then
		open #open_file_count: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],Version=1,Replace,RecL=12",internal,outIn
		gosub CLOSE_FILE
		gosub INDEX2
	else
		if exists("[Q]\GLmstr\schedule"&str$(sn)&"-idx.h[cno]")=0 then gosub INDEX2
		open #open_file_count: "Name=[Q]\GLmstr\schedule"&str$(sn)&".h[cno],KFName=[Q]\GLmstr\schedule"&str$(sn)&"-idx.h[cno],Shr",internal,outIn,keyed
	end if
return ! /r
FIXGLACCOUNTS: ! r: left pad general ledger number and reference number
	restore #open_file_count:
	do
		read #open_file_count, using "form pos 1,c 12": gl$ eof L1990
		gl$=lpad$(rtrm$(gl$),12)
		rewrite #open_file_count, using "form pos 1,c 12": gl$
	loop
	L1990: !
return ! /r
CLOSE_FILE: ! r:
	for j=1 to open_file_count
		close #j: ioerr ignore
	next j
return ! /r
BUILD_LAYOUT: ! r:
	! ** Field Labels    **
	ic=0 ! temporary Item Counter
	lbl$(ic+=1)="G/L Number"
! ** Text Box / Field Display   Lengths   **
	ic=0 ! temporary Item Counter
	mmddyy=8
	ccyymmdd=10
	tln(ic+=1)=12
! ** Field Types **
	ic=0
	fltyp$(ic+=1)='C'
! ** Field Storage Lengths **
	ic=0
	mmddyy=6 : ccyymmdd=8
	sln(ic+=1)=12
! ** Field Masks **
	ic=0
	pointtwo=32 : number=30
	ccyymmdd=3 : mmddyy=1 : glnumber=53
	mask(ic+=1)=0
! ** Storage Positions **
	! starting field position - default to the same as order displayed
	ic=0
	sp(ic+=1)=1
! ** Combo Boxes **
	cl=1 : c$(cl,1)='ComboF'
	c$(cl,2)="[Q]\GLmstr\GLmstr.h[cno]"
	c$(cl,3)="1" : c$(cl,4)="12"
	c$(cl,5)="13": c$(cl,6)="40"
	c$(cl,7)="[Q]\GLmstr\glindex.h[cno]"
	! C$(CL,8)=limit to list option ('1'=Yes; '0'=No)
	limit_to_list$='1'
! ** Combo Boxes **
	! cL=2 : c$(CL,1)='ComboF'
	! c$(CL,2)="[Q]\GLmstr\transcode.h[cno]"
	! c$(CL,3)="1" : c$(CL,4)="2"
	! c$(CL,5)="3" : c$(CL,6)="30"
	! c$(CL,7)="[Q]\GLmstr\transcode-idx.h[cno]"
	! c$(CL,8)="1"
	! lIMIT_TO_LIST$=('1'=yes' ; '0'=NO)
return ! /r
DELETEIT: !  r: delete a schedule
	mat ml$(3)
	ml$(1)="You are attempting to delete schedule # "&str$(sn)&"."
	ml$(2)="Click OK to continue, "
	ml$(3)="else Cancel to prevent deleting the schedule."
	fnmsgbox(mat ml$,resp$,'',49)
	if uprc$(resp$)="OK" then goto L2310 else goto ADD_EDIT_SCHEDULES
	L2310: delete #10,rec=editrec:
	fnFree("[Q]\GLmstr\schedule"&str$(sn)&".h[cno]")
return ! /r
