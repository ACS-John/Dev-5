! Replace S:\acsPR\hours.br
! enter and track houly breakdowns of time for comp time, etc
! fnTop("S:\acsPR\hourclassification2","Time Classification")
def library fnHours(eno)

	autoLibrary
	on error goto Ertn

	dim message$*40,resp$(10)*40
	dim oldclass$*5,class$*5,classification$*30
	dim impname$*25,empname$*30
	dim flxitm$(8)*30,key$*21,ml$(3)*80

	open #hBreakdown=fnH: "Name=[Q]\PRmstr\HourBreakdown.h[cno],KFName=[Q]\PRmstr\HourBreakdown-idx.h[cno],Shr",internal,outIn,keyed 
	open #hClassification=fnH: "Name=[Q]\PRmstr\HourClass.h[cno],KFName=[Q]\PRmstr\HourClass-idx.h[cno],Shr",internal,outIn,keyed ioerr MSGBOX3
	open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k 
	MAIN: ! 
		addhours=edithours=0
		fnTos
		respc=0 : lc=0 : mat resp$=('') 
		mylen=20 : mypos=mylen+2
		fnLbl(lc+=1,1,'Employee Number:',mylen,1,0,0)
		fncombof("PRmstr",lc,mypos,0,"[Q]\PRmstr\Employee.h[cno]",1,8,9,30,"[Q]\PRmstr\EmployeeIdx-no.h[cno]",2,pas, "Enter the employee number you wish to work with.",0)
		if hact$="[All]" then resp$(1)="[All]" else resp$(1)=str$(eno)
		fnButton(lc+=2,32,"&Refresh",46,"",0,0,0,0,1) 
		!   fnButton(lc,41,"&Add",43) 
		!   fnButton(lc,46,"&Edit",45) 
		!   fnButton(lc,52,"&Delete",44) 
		!   fnButton(lc,60,"&Cancel",5)
		mat chdr$(8) : mat cmask$(8) : mat flxitm$(8) 
		chdr$(1)="Ref #"
		chdr$(2)="Emp #"
		chdr$(3)="Name"
		chdr$(4)="Classification" 
		chdr$(5)="Date"
		chdr$(6)="Increase" 
		chdr$(7)="Decrease"
		chdr$(8)="Balance" 
		cmask$(5)='3'
		cmask$(6)='10'
		cmask$(7)='10' 
		cmask$(8)='10' 
		fnflexinit1('Hours',lc+2,1,15,66,mat chdr$,mat cmask$,1) 
		lc+=18
		if hact$="[All]" then 
			restore #hBreakdown: nokey EOBREAKDOWN 
			balance=0
			oldclass$=""
			goto READHOURBREAKDOWN
		end if
		key$=lpad$(str$(eno),8)&"             " 
		restore #hBreakdown,key>=key$: nokey EOBREAKDOWN 
		balance=0
		oldclass$=""
	READHOURBREAKDOWN: !
		holdempno=empno: oldclass$=class$: read #hBreakdown,using "form pos 1,n 8,c 5,n 8,2*n 9.2",release: empno,class$,tdate,increase,decrease eof EOBREAKDOWN
		if hact$="[All]" then empkey$=lpad$(str$(empno),8): goto L310
		if empno<>eno then goto EOBREAKDOWN
		empkey$=lpad$(str$(eno),8)
		L310: !
		empname$=""
		read #hEmployee,using "form pos 9,c 30",key=empkey$,release: empname$ nokey ignore
		if trim$(oldclass$)<>"" and oldclass$<>class$ then 
			mat flxitm$=("")
			balance=0
			fnflexadd1(mat flxitm$)
		end if
		if hact$="[All]" and holdempno<>empno then mat flxitm$=(""): balance=0: fnflexadd1(mat flxitm$)
		balance+=increase-decrease
		classification$=""
		read #hClassification,using "form pos 6,c 30",key=class$,release: classification$ nokey ignore
		flxitm$(1)=str$(rec(hBreakdown))
		flxitm$(2)=str$(empno): flxitm$(3)=empname$ 
		flxitm$(4)=classification$
		flxitm$(5)=str$(tdate) 
		flxitm$(6)=str$(increase): flxitm$(7)=str$(decrease) 
		flxitm$(8)=str$(balance)
		fnflexadd1(mat flxitm$)
	goto READHOURBREAKDOWN
	EOBREAKDOWN: ! 
		if hact$="[All]" then hact$=""
		!   fnLbl(lc,85,'',0,1)
		!   fnButton(lc,32,"&Refresh",46,"",0,0,0,0,1) 
		!   fnButton(lc,41,"&Add",43) 
		!   fnButton(lc,46,"&Edit",45) 
		!   fnButton(lc,52,"&Delete",44) 
		!   fnButton(lc,60,"&Cancel",5)
		!   fnCmdKey("&Refresh",46,1) 
		fnCmdKey("&Add",43) 
		fnCmdKey("&Edit",45) 
		fnCmdKey("&Delete",44) 
		fnCmdKey("&Cancel",5,0,1)
		ckey=fnAcs(mat resp$) 
		if ckey=5 then goto Xit
		hact$=trim$(resp$(1)(1:8))
		if hact$="[All]" then goto MAIN
		empno=eno=val(resp$(1)(1:8))
		editrec=val(resp$(2)) ! record # if edit
		if ckey=45 then edithours=1 else edithours=0
		if ckey=43 then addhours=1 else addhours=0
		if ckey=44 then goto MSGBOX1 ! delete a record
		if ckey=46 then goto MAIN ! refresh grid
	ADDFM: ! add hours
		holdeno=eno ! allow then to enter time on more than one employee while here, but warn them
		if empno=0 then empno=eno ! assign to default employee if adding
		empkey$=lpad$(str$(eno),8)
		empname$=""
		read #hEmployee,using "form pos 9,c 30",key=empkey$,release: empname$ nokey ignore
		if addhours=1 then class$="": increase=decrease=0
		if edithours=1 then 
			read #hBreakdown,using "form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease noRec ADD_FM_DONE
		end if
		fnTos
		respc=0 : lc=0 : mylen=21 : mypos=mylen+2: mat resp$=(""): right=1
		fnFra(1,9,8,70,"Hourly Information - "&empname$,"",0) : frame1=1
		fnLbl(lc+=1,1,'Employee Number:',mylen,right,0,frame1)
		fncombof("PRmstr",lc,mypos,0,"[Q]\PRmstr\Employee.h[cno]",1,8,9,30,"[Q]\PRmstr\EmployeeIdx-no.h[cno]",0,pas, "Enter the employee number to whom the time should be recorded",frame1) 
		resp$(1)=str$(empno)
		fnLbl(lc+=1,1,'Classification:',mylen,right,0,frame1)
		fncombof("Hours",lc,mypos,0,"[Q]\PRmstr\Hourclass.h[cno]",1,5,6,30,"[Q]\PRmstr\Hourclass-idx.h[cno]",0,pas, "Enter the proper classification of hours. If you need a new classification, you must add it under a different menu option",frame1) 
		resp$(2)=class$
		fnLbl(lc+=1,1,'Date:',mylen,right,0,frame1)
		if addhours=1 then tdate=0
		fnTxt(lc,mypos,10,0,right,'1003',0,"",frame1 ) 
		resp$(3)=str$(tdate)
		fnLbl(lc+=1,1,'Increase:',mylen,right,0,frame1)
		fnTxt(lc,mypos,10,0,right,'32',0,"",frame1 ) 
		resp$(4)=str$(increase)
		fnLbl(lc+=1,1,'Decrease:',mylen,right,0,frame1)
		fnTxt(lc,mypos,10,0,right,'32',0,"",frame1 ) 
		resp$(5)=str$(decrease)
		fnCmdSet(4)
		ckey=fnAcs(mat resp$) 
		if ckey=5 then goto MAIN
		empno=val(resp$(1)(1:8)) 
		class$=resp$(2)(1:5) 
		tdate=val(resp$(3)) 
		increase=val(resp$(4)) 
		decrease=val(resp$(5))
		if empno<>holdeno then goto MSGBOX2 ! attempting to enter time on different employee
		L740: !
		if increase=0 and decrease=0 and addhours=1 then goto ADDFM  ! do not add blank records
		if addhours=1 then 
			write #hBreakdown,using "form pos 1,n 8,c 5,n 8,2*n 9.2": empno,class$,tdate,increase,decrease 
			goto ADDFM
		else if edithours=1 then 
			rewrite #hBreakdown,using "form pos 1,n 8,c 5,n 8,2*n 9.2",rec=editrec: empno,class$,tdate,increase,decrease 
			edithours=0
			goto ADD_FM_DONE
		end if
	ADD_FM_DONE: goto MAIN
	MSGBOX1: ! delete this record?
		mat ml$(3) 
		ml$(1)="You have chosen to delete the "&classification$&" for " 
		ml$(2)="employee "&str$(empno)&". Click on Yes to delete the entry, else" 
		ml$(3)="No to return to the display screen" 
		fnmsgbox(mat ml$,resp$,'',52)
		if resp$="Yes" then goto L810 else goto MAIN
		L810: !
		delete #hBreakdown,rec=editrec: 
	goto MAIN
	MSGBOX2: ! changing employees
		mat ml$(3) 
		ml$(1)="You are attempting to enter hours on a different employee." 
		ml$(2)="You were assigned to employee "&str$(holdeno)&"." 
		ml$(3)="Do you wish to change to employee "&str$(empno)&"?" 
		fnmsgbox(mat ml$,resp$,'',52)
		if resp$="Yes" then eno=empno: goto L740 else empno=holdeno: goto ADDFM
	MSGBOX3: ! set up classifications of time
		mat ml$(3) 
		ml$(1)="You must set up the classification file before you can use" 
		ml$(2)="this feature.  Go to Files on the main menu and then " 
		ml$(3)="take Time Classifications." 
		fnmsgbox(mat ml$,resp$,'',65)
		goto Xit
	Xit: ! 
	close #hBreakdown: ioerr ignore
	close #hClassification: ioerr ignore
	close #hEmployee: ioerr ignore
fnend 
! initial setup
! open #hBreakdown=fnH: "Name=[Q]\PRmstr\HourBreakdown.h[cno],RecL=39,KFName=[Q]\PRmstr\HourBreakdown-idx.h[cno],kps=1/9/14,kln=8/5/8,replace",internal,outIn,keyed 
! close #hBreakdown: 
! fnIndex('[Q]\PRmstr\HourBreakdown.h[cno]','[Q]\PRmstr\HourBreakdown-idx.h[cno]','1/9/14 8/5/8')
include: ertn
