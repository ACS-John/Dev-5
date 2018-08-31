! Replace S:\acsPR\newprRpt3
! User Designed Reports

	library 'S:\Core\Library': fntop,fnxit, fnDedNames,fnerror,fnTos,fnLbl,fnCmdKey,fnAcs,fncombof,fnmsgbox,fnChk,fnTxt,fnFra,fncomboa,fnindex_it,fngethandle,fnStatusClose,fnprint_designed_report
	on error goto ERTN

	dim rt$*78,ch$(2)*132,psc(100)
	dim inp(20),pp(20),ti(20),rt40$*40
	dim rptn$*2,cap$*128,resp$(100)*150
	dim ml$(4)*128

	fntop(program$,cap$="Design Reports")
	fnindex_it("[Q]\PRmstr\PRReport.h[cno]","[Q]\PRmstr\PRRptIdx.h[cno]","1 2")
	fnStatusClose
	open #h_prreport:=1: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\PRRptIdx.h[cno],RecL=1049,KLn=2,KPs=1,Use,Shr",internal,outIn,keyed 
F_PRREPORT: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
! 

	gosub DATANAMES
SCR1: ! 
ASKREPORT: ! 
	fnTos(sn$="Report-ask")
	respc=0
	fnLbl(1,1,"Report:",11,1)
	fncombof("Report",1,14,43,"[Q]\PRmstr\prreport.h[cno]",1,2,3,30,"[Q]\PRmstr\prrptidx.h[cno]",1+addall,0,"Select from the list of reports. To add a report, click the Add button.",container)
	resp$(respc+=1)=""
	fnCmdKey("&Add",1,0,0,"Add a new employee" )
	fnCmdKey("E&dit",2,0,0,"Modify the selected report")
	fnCmdKey("Print",4,1,0,"Run the selected report")
	fnCmdKey("E&xit",5,0,1,"Return to menu")
	fnAcs(sn$,0,mat resp$,ckey) ! ask report #
	if ckey=5 then goto XIT
	editrec=addone=0
	rptn=val(resp$(1)(1:2))
	if ckey=1 then 
		addone=1
		rptn=0
		goto CONTINUE_FOR_ADD_AND_EDIT
	else if ckey=2 then 
		editrec=1
		goto CONTINUE_FOR_ADD_AND_EDIT
	else if ckey=4 then 
		fnprint_designed_report(rptn)
	end if 
	goto ASKREPORT
CONTINUE_FOR_ADD_AND_EDIT: ! 
	rptn$=lpad$(str$(rptn),2)
	if addone=1 then 
		read #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti nokey SCR2
	else if editrec=1 then 
		read #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti nokey EDIT_READ_NOKEY
	end if 
	goto L400
EDIT_READ_NOKEY: ! r:
	mat ml$(2)
	ml$(1)="A record with this number does not exist!"
	ml$(2)="Select a different numbe if you wish to add a new report."
	fnmsgbox(mat ml$,resp$,cap$,48)
	goto ASKREPORT ! /r
L400: ! 
	holdrn=rn
! 
SCR2: ! 
	if addone=1 then ! r: initialize variables to 0 or blank
		rn=0
		rt$=""
		mat ch$=("")
		ips=0
		sd$=""
		sd=cp=0
		mat psc=(0)
		mat pp=(0)
		mat ti=(0)
		holdrn=0
	end if  ! /r
	fnTos(sn$="Report-add")
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,"Report #:",mylen,1)
	fnTxt(1,mypos,2,2,0,"30",0,"")
	resp$(respc+=1)=str$(rn)
	fnLbl(2,1,"Report Title:",mylen,1)
	fnTxt(2,mypos,78,0,0,"",0,"")
	resp$(respc+=1)=rt$
	fnLbl(3,1,"Column Headings:",mylen,1)
	fnLbl(4,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
	fnTxt(5,1,132,0,0,"",0,"The heading can be two lines.  This will be the 1st line.")
	resp$(respc+=1)=ch$(1)
	fnLbl(6,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
	fnTxt(7,1,132,0,0,"",0,"This is the 2nd line of the heading line.")
	resp$(respc+=1)=ch$(2)
	mylen=50
	fnLbl(12,1,"Item for pr Selection (blank for all):",mylen,1)
	if ips>0 and ips=<udim(code$) then resp$(respc+=1)=code$(ips+1) else resp$(respc+=1)=""
	fncomboa("DataNames2",12,mylen+3,mat code$,"If you want limit the report to a value in a particular field in the employee record, Indicate which field it is by locatiing the ID number of the field using Help button.",25,0)
	fnChk(13,mylen+3,"Summarize Departmental Records:",1)
	if sd= 1 then resp$(respc+=1)="TRUE" else resp$(respc+=1)="FALSE"
	fnCmdKey("&Next",1,1,0,"Save changes and move to next questions" )
	fnCmdKey("&Delete",4,0,0,"Deletes this report from your system.")
	fnCmdKey("&Cancel",5,0,1,"Return to selection screen.")
	fnAcs(sn$,0,mat resp$,ckey) ! ask report #
	addone=0
	if ckey=5 then goto SCR1
	rn=val(resp$(1)(1:2))
	if holdrn>0 and rn<>holdrn then 
! r: confirm_key_change
		mat ml$(3)
		ml$(1)="You are attempting to change report number"
		ml$(2)="from "&str$(holdrn)& " to "&str$(rn)&"."
		ml$(3)="Take OK to continue, else Cancel."
		fnmsgbox(mat ml$,resp$,cap$,49)
		if resp$="OK" then 
			holdrn=rn
		else 
			goto SCR2
		end if  ! /r
	end if 
	rt40$=resp$(2)(1:40)
	ch$(1)=resp$(3)
	ch$(2)=resp$(4)
	ips=0
	for j=1 to udim(mat code$)
		if resp$(5)=code$(j) then ips=j-1 : goto L760
	next j
L760: ! 
	if resp$(6)(1:1)="T" then sd$="Y": sd=1 else sd$="N": sd=0
	if ips<0 or ips>126 or (ips>1 and ips<6) then 
		mat ml$(2)
		ml$(1)="You can not use "&code$(ips+1)&" as selection criteria!"
		ml$(2)=" Take OK to select a different item."
		fnmsgbox(mat ml$,resp$,cap$,48)
		goto SCR2
	end if 
	if sd$="Y" then sd=1 else sd=0
	rt$=rt40$
	if ckey=4 then 
		goto DELETEIT
		DELETEIT: ! 
		mat ml$(2)
		ml$(1)="You have chosen to delete report # "&rptn$
		ml$(2)="Take Ok to continue, else Cancel to keep the report."
		fnmsgbox(mat ml$,resp$,cap$,49)
		if resp$="OK" then 
			delete #h_prreport,key=rptn$: 
		else 
			goto SCR1
		end if 
	end if 
! 
! SCR3: ! r:
! If RN=0 Then Goto DONE  ! ain't this just redundant?
	if ips=0 then goto SCR4
	fnTos(sn$="Report-sel")
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,"Print Selection Criteria:",30,1)
	z=0
	for x=1 to 5
		for j=2 to 21
			fnTxt(j,x*16,12,0,0,"33",0,"If you chosen to limit the report to certain criteria, enter the values here that should match information in the employee's record.")
			resp$(respc+=1)=str$(psc(z+=1))
		next j
	next x
	fnCmdKey("&Next",1,1,0,"Save changes and move to next questions" )
	fnCmdKey("&Back",6,0,0,"Back up a screen.")
	fnCmdKey("&Cancel",5,0,1,"Return to selection screen.")
	fnAcs(sn$,0,mat resp$,ckey) ! ask matching criteria
	if ckey=5 then goto SCR1
	for j=1 to 100
		psc(j)=val(resp$(j))
	next j
	if ckey=6 then goto SCR2
goto SCR4 ! /r

SCR4: ! r:
	fnTos(sn$="Report-scr4") 
	respc=0: mylen=15: mypos=mylen+3
	fnFra(1,1,23,90,"Selection of Column Information","Select the informatin that should be printed in each column of your report.")
	fnLbl(1,1,"Print       Want ",50,1,0,1)
	fnLbl(2,1,"Item to pr                 Position    A Total",52,1,0,1)
	for j=1 to 20
		if inp(j)>0 and inp(j)=<udim(code$) then resp$(respc+=1)=code$(inp(j)+1) else resp$(respc+=1)=" "
	! if inp(j)>0 and inp(j)=<udim(code$) then resp$(respc+=1)=code$(inp(j)  ) else resp$(respc+=1)=" "
		fncomboa("DataNames",j+2,1,mat code$,"",25,1)
		fnTxt(j+2,37,3,0,0,"30",0,"The position is the starting position acress the page where this column should print.",1) 
		resp$(respc+=1)=str$(pp(j))
		if ti(j)=1 then resp$(respc+=1)="True" else resp$(respc+=1)="False"
		fnChk(j+2,48,"",1,1) ! total the column
	next j
	fnCmdKey("&Next",1,1,0,"Save changes on this report design." ) 
	fnCmdKey("&Back",6,0,0,"Back up a screen.") 
	fnCmdKey("&Cancel",5,0,1,"Return to report selection screen without saving any changes.")
	fnAcs(sn$,0,mat resp$,ckey) ! enter column information
	if ckey<>5 then
		x=0
		for j=3 to 60 step 3
			x+=1
			for j1=1 to udim(code$)
				if resp$(j-2)=code$(j1) then inp(x)=j1-1 : goto L1330
			next j1
			L1330: !
			pp(x)=val(resp$(j-1))
			if resp$(j)="True" then ti(x)=1 else ti(x)=0
		next j
		if rptn=rn then 
			rewrite #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
		else 
			read #h_prreport,using 'form pos 1,n 2',key=lpad$(str$(rn),2): rn nokey CHANGETHENUMBER
		end if
	end if
goto SCR1 ! /r


CHANGETHENUMBER: ! r:
	write #h_prreport,using F_PRREPORT: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
	delete #h_prreport,key=rptn$: nokey ignore
goto SCR1 ! /r

XIT: !
close #h_prreport: ioerr ignore
fnxit
IGNORE: continue 
! L1770: ! r: ADD NEW RECORD
!   rt$="": mat ch$=("")
!   ips=sd=cp=0
!   mat psc=(0): mat inp=(0): mat pp=(0): mat ti=(0)
!   write #h_prreport,using F_PRREPORT: rptn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
!   rptn$=lpad$(str$(rptn),2)
!   read #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti ! nokey L1530
!   goto L400 ! /r
def fn_add_dn(dn_x_1$*30,dn_x_2$*30,dn_x_3$*30)
	dn_counter+=1
	datanames$(dn_counter,1)=dn_x_1$
	datanames$(dn_counter,2)=dn_x_2$
	datanames$(dn_counter,3)=dn_x_3$
fnend 
DATANAMES: ! r:
	dim datanames$(104,3)*30
	dn_counter=0
	fn_add_dn('Employee Number'        ,'eno'         ,'N 8'   )
	fn_add_dn('Name'                   ,'EM$(1)'      ,'C 30'  )
	fn_add_dn('Address'                ,'EM$(2)'      ,'C 30'  )
	fn_add_dn('City State Zip Code'    ,'EM$(3)'      ,'C 30'  )
	fn_add_dn('Social Security #'      ,'SS$'         ,'C 11'  )
	fn_add_dn('Race'                   ,'RS(1)'       ,'N 1'   )
	fn_add_dn('Sex'                    ,'RS(2)'       ,'N 1'   )
	fn_add_dn('Marital Status'         ,'EM(1)'       ,'N 2'   )
	fn_add_dn('Federal Exemptions'     ,'EM(2)'       ,'N 2'   )
	fn_add_dn('State Exemptions'       ,'EM(3)'       ,'N 2'   )
	fn_add_dn('Employment Status'      ,'EM(4)'       ,'N 2'   )
	fn_add_dn('Pay Code'               ,'EM(5)'       ,'N 2'   )
	fn_add_dn('Fica Code'              ,'EM(6)'       ,'N 2'   )
	fn_add_dn('Eic Code'               ,'EM(7)'       ,'N 2'   )
	fn_add_dn('Sick Pay Code'          ,'EM(8)'       ,'PD 3.3')
	fn_add_dn('Vacation Pay Code'      ,'EM(9)'       ,'PD 3.3')
	fn_add_dn('Sick Hours Accrued'     ,'EM(10)'      ,'PD 4.2')
	fn_add_dn('Vacation Hours Accrued' ,'EM(11)'      ,'PD 4.2')
	fn_add_dn('Standard Federal W/H'   ,'EM(12)'      ,'PD 4.2')
	fn_add_dn('Federal Tax Add-Om'     ,'EM(13)'      ,'PD 4.2')
	fn_add_dn('Standard State W/H'     ,'EM(14)'      ,'PD 4.2')
	fn_add_dn('State Tax Add-On'       ,'EM(15)'      ,'PD 4.2')
	fn_add_dn('Date Hired'             ,'EM(16)'      ,'N 6'   )
	fn_add_dn('Last Payroll Date'      ,'EM(17)'      ,'N 6'   )
	fn_add_dn('Telephone Number'       ,'PH$'         ,'C 12'  )
	fn_add_dn('Birth Date'             ,'BD'          ,'N 6'   )
	fn_add_dn('Department Number'      ,'TDN'         ,'N 3'   )
	fn_add_dn('G/L Account #'          ,'GL$'         ,'C 12'  )
	fn_add_dn('Last Review Date'       ,'TDT(1)'      ,'N 6'   )
	fn_add_dn('Next Review Date'       ,'TDT(2)'      ,'N 6'   )
	fn_add_dn('Last Increase Date'     ,'TDT(3)'      ,'N 6'   )
	fn_add_dn('Last Payroll Date'      ,'TDT(4)'      ,'N 6'   )
	fn_add_dn('State Code'             ,'TCD(1)'      ,'N 2'   )
	fn_add_dn('Worknams Comp Code'     ,'TCD(2)'      ,'N 2'   )
	fn_add_dn('Union Code'             ,'TCD(3)'      ,'N 2'   )
	fn_add_dn('Amount of Last Increase','tli'         ,'pd 4.2')
	fn_add_dn('Salary'                 ,'TDET(1)'     ,'PD 4.2')
	fn_add_dn('Regular Hourly Rate'    ,'TDET(2)'     ,'PD 4.2')
	fn_add_dn('Overtime Hourly Rate'   ,'TDET(3)'     ,'PD 4.2')
	fn_add_dn('Misc - 1'               ,'TDET(4)'     ,'PD 4.2')
	fn_add_dn('Misc - 2'               ,'TDET(5)'     ,'PD 4.2')
	fn_add_dn('Misc - 3'               ,'TDET(6)'     ,'PD 4.2')
	fn_add_dn('Misc - 4'               ,'TDET(7)'     ,'PD 4.2')
	fn_add_dn('Misc - 5'               ,'TDET(8)'     ,'PD 4.2')
	fn_add_dn('Misc - 6'               ,'TDET(9)'     ,'PD 4.2')
	fn_add_dn('Misc - 7'               ,'TDET(10)'    ,'PD 4.2')
	fn_add_dn('Misc - 8'               ,'TDET(11)'    ,'PD 4.2')
	fn_add_dn('Misc - 9'               ,'TDET(12)'    ,'PD 4.2')
	fn_add_dn('Misc - 10'              ,'TDET(13)'    ,'PD 4.2')
	fn_add_dn('Misc - 11'              ,'TDET(14)'    ,'PD 4.2')
	fn_add_dn('Misc - 12'              ,'TDET(15)'    ,'PD 4.2')
	fn_add_dn('Misc - 13'              ,'tdet(16)'    ,'PD 4.2')
	fn_add_dn('Misc - 14'              ,'tdet(17)'    ,'PD 4.2')
	fn_add_dn('Misc - 15'              ,'tdet(18)'    ,'PD 4.2')
	fn_add_dn('Misc - 16'              ,'tdet(19)'    ,'PD 4.2')
	fn_add_dn('Misc - 17'              ,'tdet(20)'    ,'PD 4.2')
	fn_add_dn('Misc - 18'              ,'tdet(21)'    ,'PD 4.2')
	fn_add_dn('Misc - 19'              ,'tdet(22)'    ,'PD 4.2')
	fn_add_dn('Misc - 20'              ,'tdet(23)'    ,'PD 4.2')
	fn_add_dn('Department Number'      ,'TDN'         ,'N 3'   )
	fn_add_dn('Payroll Date'           ,'prd'         ,'pd 6'  )
	fn_add_dn('Check Number'           ,'ckno'        ,'N 7'   )
	fn_add_dn('Regular Hours'          ,'tdc(1)'      ,'PD 3.2')
	fn_add_dn('Overtime Hours'         ,'tdc(2)'      ,'Pd 3.2')
	fn_add_dn('Sick Hours'             ,'tdc(3)'      ,'PD 3.2')
	fn_add_dn('Vacation Hours'         ,'tdc(4)'      ,'PD 3.2')
	fn_add_dn('Holiday Hours'          ,'tdc(5)'      ,'PD 3.2')
	fn_add_dn('Workmans Comp Wage''s'  ,'tdc(6)'      ,'Pd 5.2')
	fn_add_dn('SS Wages'               ,'tdc(7)'      ,'PD 5.2')
	fn_add_dn('Medicare Wages'         ,'tdc(8)'      ,'PD 5.2')
	fn_add_dn('Federal U/C Wage'       ,'tdc(9)'      ,'PD 5.2')
	fn_add_dn('State U/c Wage'         ,'tdc(10)'     ,'PD 5.2')
	fn_add_dn('Federal Withholdings'   ,'tcp(1)'      ,'PD 5.2')
	fn_add_dn('SS Withholdings'        ,'tcp(2)'      ,'PD 5.2')
	fn_add_dn('Medicare Withholdings'  ,'tcp(3)'      ,'PD 5.2')
	fn_add_dn('State Withholdings'     ,'tcp(4)'      ,'PD 5.2')
	fn_add_dn('Misc - 1'               ,'tcp(5)'      ,'PD 5.2')
	fn_add_dn('Misc - 2'               ,'tcp(6)'      ,'pd 5.2')
	fn_add_dn('Misc - 3'               ,'tcp(7)'      ,'pd 5.2')
	fn_add_dn('Misc - 4'               ,'tcp(8)'      ,'pd 5.2')
	fn_add_dn('Misc - 5'               ,'tcp(9)'      ,'pd 5.2')
	fn_add_dn('Misc - 6'               ,'tcp(10)'     ,'pd 5.2')
	fn_add_dn('Misc - 7 '              ,'tcp(11)'     ,'pd 5.2')
	fn_add_dn('Misc - 8 '              ,'tcp(12)'     ,'pd 5.2')
	fn_add_dn('Misc - 9 '              ,'tcp(13)'     ,'pd 5.2')
	fn_add_dn('Misc - 10'              ,'tcp(14)'     ,'pd 5.2')
	fn_add_dn('Misc - 11'              ,'tcp(15)'     ,'pd 5.2')
	fn_add_dn('Misc - 12'              ,'tcp(16)'     ,'pd 5.2')
	fn_add_dn('Misc - 13'              ,'tcp(17)'     ,'pd 5.2')
	fn_add_dn('Misc - 14'              ,'tcp(18)'     ,'pd 5.2')
	fn_add_dn('Misc - 15'              ,'tcp(19)'     ,'pd 5.2')
	fn_add_dn('Misc - 16'              ,'tcp(20)'     ,'pd 5.2')
	fn_add_dn('Misc - 17'              ,'tcp(21)'     ,'pd 5.2')
	fn_add_dn('Misc - 18'              ,'tcp(22)'     ,'pd 5.2')
	fn_add_dn('Misc - 19'              ,'tcp(23)'     ,'pd 5.2')
	fn_add_dn('Misc - 20'              ,'tcp(24)'     ,'pd 5.2')
	fn_add_dn('Eic'                    ,'tcp(25)'     ,'pd 5.2')
	fn_add_dn('Regular Earnings'       ,'tcp(26)'     ,'pd 5.2')
	fn_add_dn('OT Earnings'            ,'tcp(27)'     ,'pd 5.2')
	fn_add_dn('Other Compensation'     ,'tcp(28)'     ,'pd 5.2')
	fn_add_dn('Meals'                  ,'tcp(29)'     ,'pd 5.2')
	fn_add_dn('Tips'                   ,'tcp(30)'     ,'pd 5.2')
	fn_add_dn('Total Wage'             ,'tcp(31)'     ,'pd 5.2')
	fn_add_dn('Net Pay'                ,'tcp(32)'     ,'pd 5.2')

	dim fullname$(20)*20
	fnDedNames(mat fullname$)

	dim code$(105)*30
	code$(1)=""
	for j=1 to udim(datanames$)
		code$(j+1)=datanames$(j,1)
		if j>39 and j<60 and trim$(fullname$(j-39))<>"" then 
			code$(j+1)=trim$(fullname$(j-39))(1:22)
		else if j>39 and j<60 and trim$(fullname$(j-39))="" then 
			code$(j+1)="Misc -"&str$(j-39)&"-Std Ded"
		else if j>76 and j<95 and trim$(fullname$(j-76))<>"" then ! miscellaneous withholding amounts
			code$(j+1)=trim$(fullname$(j-76))(1:27)&"-Wh"
		else if j>76 and j<94 and trim$(fullname$(j-76))="" then ! plug names if none
			code$(j+1)="Misc - "&str$(j-76)
		end if 
	next j
return  ! /r
include: ertn