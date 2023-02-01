autoLibrary
fnTop(program$)
on error goto Ertn
! r: setup

dim rt40$*40
dim rptn$*2
dim resp$(100)*150
dim ml$(4)*128

fnIndex('[Q]\PRmstr\PRReport.h[cno]','[Q]\PRmstr\PRRptIdx.h[cno]','1 2')
fnStatusClose
open #hReports:=fnH: 'Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\PRRptIdx.h[cno],RecL=1049,KLn=2,KPs=1,Use,Shr',i,outIn,k ! formerly #1
!         :             rn ,rt$ ,mat ch$,ips,sd,cp,mat psc   ,mat xInp,mat pp  ,mat ti
Freports: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,20*pd 2 ,20*pd 2,20*n 1
dim rt$*78
dim ch$(2)*132
dim psc(100)
dim xInp(20),pp(20),ti(20)

! r: set mat DataNames$
	dim DataNames$(104,3)*30
	dn_counter=0
	fn_add_dn('Employee Number'       	,'eno'         ,'n 8'   )
	fn_add_dn('Name'                   	,'em$(1)'      ,'c 30'  )
	fn_add_dn('Address'                	,'em$(2)'      ,'c 30'  )
	fn_add_dn('City State Zip Code'   	,'em$(3)'      ,'c 30'  )
	fn_add_dn('Social Security No'    	,'ss$'         ,'c 11'  )
	fn_add_dn('Race'                   	,'rs(1)'       ,'n 1'   )
	fn_add_dn('Sex'                    	,'rs(2)'       ,'n 1'   )
	fn_add_dn('Marital Status'        	,'em(1)'       ,'n 2'   )
	fn_add_dn('Federal Exemptions'    	,'em(2)'       ,'n 2'   )
	fn_add_dn('State Exemptions'      	,'em(3)'       ,'n 2'   )
	fn_add_dn('Employment Status'     	,'em(4)'       ,'n 2'   )
	fn_add_dn('Pay Code'               	,'em(5)'       ,'n 2'   )
	fn_add_dn('Fica Code'              	,'em(6)'       ,'n 2'   )
	fn_add_dn('Eic Code'               	,'em(7)'       ,'n 2'   )
	fn_add_dn('Sick Pay Code'         	,'em(8)'       ,'pd 3.3')
	fn_add_dn('Vacation Pay Code'     	,'em(9)'       ,'pd 3.3')
	fn_add_dn('Sick Hours Accrued'    	,'em(10)'      ,'pd 4.2')
	fn_add_dn('Vacation Hours Accrued'	,'em(11)'      ,'pd 4.2')
	fn_add_dn('Standard Federal W/H'  	,'em(12)'      ,'pd 4.2')
	fn_add_dn('Federal Tax Add-Om'    	,'em(13)'      ,'pd 4.2')
	fn_add_dn('Standard State W/H'    	,'em(14)'      ,'pd 4.2')
	fn_add_dn('State Tax Add-On'      	,'em(15)'      ,'pd 4.2')
	fn_add_dn('Date Hired'             	,'em(16)'      ,'n 6'   )
	fn_add_dn('Last Payroll Date'     	,'em(17)'      ,'n 6'   )
	fn_add_dn('Telephone Number'      	,'ph$'         ,'c 12'  )
	fn_add_dn('Birth Date'             	,'bd'          ,'n 6'   )
	fn_add_dn('Department Number'     	,'tdn'         ,'n 3'   )
	fn_add_dn('G/L Account'           	,'gl$'         ,'c 12'  )
	fn_add_dn('Last Review Date'      	,'tdt(1)'      ,'n 6'   )
	fn_add_dn('Next Review Date'      	,'tdt(2)'      ,'n 6'   )
	fn_add_dn('Last Increase Date'    	,'tdt(3)'      ,'n 6'   )
	fn_add_dn('Last Payroll Date'     	,'tdt(4)'      ,'n 6'   )
	fn_add_dn('State Code'             	,'tcd(1)'      ,'n 2'   )
	fn_add_dn('Worknams Comp Code'    	,'tcd(2)'      ,'n 2'   )
	fn_add_dn('Union Code'             	,'tcd(3)'      ,'n 2'   )
	fn_add_dn('Amount of Last Increase','tli'         ,'pd 4.2')
	fn_add_dn('Salary'                 	,'tdet(1)'     ,'pd 4.2')
	fn_add_dn('Regular Hourly Rate'   	,'tdet(2)'     ,'pd 4.2')
	fn_add_dn('Overtime Hourly Rate'  	,'tdet(3)'     ,'pd 4.2')
	fn_add_dn('Misc - 1'               	,'tdet(4)'     ,'pd 4.2')
	fn_add_dn('Misc - 2'               	,'tdet(5)'     ,'pd 4.2')
	fn_add_dn('Misc - 3'               	,'tdet(6)'     ,'pd 4.2')
	fn_add_dn('Misc - 4'               	,'tdet(7)'     ,'pd 4.2')
	fn_add_dn('Misc - 5'               	,'tdet(8)'     ,'pd 4.2')
	fn_add_dn('Misc - 6'               	,'tdet(9)'     ,'pd 4.2')
	fn_add_dn('Misc - 7'               	,'tdet(10)'    ,'pd 4.2')
	fn_add_dn('Misc - 8'               	,'tdet(11)'    ,'pd 4.2')
	fn_add_dn('Misc - 9'               	,'tdet(12)'    ,'pd 4.2')
	fn_add_dn('Misc - 10'              	,'tdet(13)'    ,'pd 4.2')
	fn_add_dn('Misc - 11'              	,'tdet(14)'    ,'pd 4.2')
	fn_add_dn('Misc - 12'              	,'tdet(15)'    ,'pd 4.2')
	fn_add_dn('Misc - 13'              	,'tdet(16)'    ,'pd 4.2')
	fn_add_dn('Misc - 14'              	,'tdet(17)'    ,'pd 4.2')
	fn_add_dn('Misc - 15'              	,'tdet(18)'    ,'pd 4.2')
	fn_add_dn('Misc - 16'              	,'tdet(19)'    ,'pd 4.2')
	fn_add_dn('Misc - 17'              	,'tdet(20)'    ,'pd 4.2')
	fn_add_dn('Misc - 18'              	,'tdet(21)'    ,'pd 4.2')
	fn_add_dn('Misc - 19'              	,'tdet(22)'    ,'pd 4.2')
	fn_add_dn('Misc - 20'              	,'tdet(23)'    ,'pd 4.2')
	fn_add_dn('Department Number'     	,'tdn'         ,'n 3'   )
	fn_add_dn('Payroll Date'           	,'prd'         ,'pd 6'  )
	fn_add_dn('Check Number'           	,'ckno'        ,'n 7'   )
	fn_add_dn('Regular Hours'          	,'tdc(1)'      ,'pd 3.2')
	fn_add_dn('Overtime Hours'         	,'tdc(2)'      ,'pd 3.2')
	fn_add_dn('Sick Hours'             	,'tdc(3)'      ,'pd 3.2')
	fn_add_dn('Vacation Hours'         	,'tdc(4)'      ,'pd 3.2')
	fn_add_dn('Holiday Hours'          	,'tdc(5)'      ,'pd 3.2')
	fn_add_dn('Workmans Comp Wage''s' 	,'tdc(6)'      ,'pd 5.2')
	fn_add_dn('SS Wages'               	,'tdc(7)'      ,'pd 5.2')
	fn_add_dn('Medicare Wages'        	,'tdc(8)'      ,'pd 5.2')
	fn_add_dn('Federal U/C Wage'      	,'tdc(9)'      ,'pd 5.2')
	fn_add_dn('State U/C Wage'        	,'tdc(10)'     ,'pd 5.2')
	fn_add_dn('Federal Withholdings'  	,'tcp(1)'      ,'pd 5.2')
	fn_add_dn('SS Withholdings'       	,'tcp(2)'      ,'pd 5.2')
	fn_add_dn('Medicare Withholdings' 	,'tcp(3)'      ,'pd 5.2')
	fn_add_dn('State Withholdings'    	,'tcp(4)'      ,'pd 5.2')
	fn_add_dn('Misc - 1'               	,'tcp(5)'      ,'pd 5.2')
	fn_add_dn('Misc - 2'               	,'tcp(6)'      ,'pd 5.2')
	fn_add_dn('Misc - 3'               	,'tcp(7)'      ,'pd 5.2')
	fn_add_dn('Misc - 4'               	,'tcp(8)'      ,'pd 5.2')
	fn_add_dn('Misc - 5'               	,'tcp(9)'      ,'pd 5.2')
	fn_add_dn('Misc - 6'               	,'tcp(10)'     ,'pd 5.2')
	fn_add_dn('Misc - 7 '              	,'tcp(11)'     ,'pd 5.2')
	fn_add_dn('Misc - 8 '              	,'tcp(12)'     ,'pd 5.2')
	fn_add_dn('Misc - 9 '              	,'tcp(13)'     ,'pd 5.2')
	fn_add_dn('Misc - 10'              	,'tcp(14)'     ,'pd 5.2')
	fn_add_dn('Misc - 11'              	,'tcp(15)'     ,'pd 5.2')
	fn_add_dn('Misc - 12'              	,'tcp(16)'     ,'pd 5.2')
	fn_add_dn('Misc - 13'              	,'tcp(17)'     ,'pd 5.2')
	fn_add_dn('Misc - 14'              	,'tcp(18)'     ,'pd 5.2')
	fn_add_dn('Misc - 15'              	,'tcp(19)'     ,'pd 5.2')
	fn_add_dn('Misc - 16'              	,'tcp(20)'     ,'pd 5.2')
	fn_add_dn('Misc - 17'              	,'tcp(21)'     ,'pd 5.2')
	fn_add_dn('Misc - 18'              	,'tcp(22)'     ,'pd 5.2')
	fn_add_dn('Misc - 19'              	,'tcp(23)'     ,'pd 5.2')
	fn_add_dn('Misc - 20'              	,'tcp(24)'     ,'pd 5.2')
	fn_add_dn('Eic'                    	,'tcp(25)'     ,'pd 5.2')
	fn_add_dn('Regular Earnings'      	,'tcp(26)'     ,'pd 5.2')
	fn_add_dn('OT Earnings'            	,'tcp(27)'     ,'pd 5.2')
	fn_add_dn('Other Compensation'    	,'tcp(28)'     ,'pd 5.2')
	fn_add_dn('Meals'                  	,'tcp(29)'     ,'pd 5.2')
	fn_add_dn('Tips'                   	,'tcp(30)'     ,'pd 5.2')
	fn_add_dn('Total Wage'             	,'tcp(31)'     ,'pd 5.2')
	fn_add_dn('Net Pay'                	,'tcp(32)'     ,'pd 5.2')

	dim fullname$(20)*20
	fnDedNames(mat fullname$)

	dim code$(105)*30
	code$(1)=''
	for j=1 to udim(DataNames$)
		code$(j+1)=DataNames$(j,1)
		if j>39 and j<60 and trim$(fullname$(j-39))<>'' then
			code$(j+1)=trim$(fullname$(j-39))(1:22)
		else if j>39 and j<60 and trim$(fullname$(j-39))='' then
			code$(j+1)='Misc -'&str$(j-39)&'-Std Ded'
		else if j>76 and j<95 and trim$(fullname$(j-76))<>'' then ! miscellaneous withholding amounts
			code$(j+1)=trim$(fullname$(j-76))(1:27)&'-Wh'
		else if j>76 and j<94 and trim$(fullname$(j-76))='' then ! plug names if none
			code$(j+1)='Misc - '&str$(j-76)
		end if
	next j
! /r
! /r

AskReport: ! r:
dim reportSelected$*128
reportSelected$=fnPcRegRead$('reportSelected')
do
	fnTos
	respc=isEditing=isAdding=0
	fnLbl(1,1,'Report:',11,1)
	fnComboF('Report',1,14,43,'[Q]\PRmstr\prreport.h[cno]',1,2,3,30,'[Q]\PRmstr\prrptidx.h[cno]',1+addall,0,'Select from the list of reports. To add a report, click the Add button.',container)
	resp$(rc_reportSelected=respc+=1)=reportSelected$
	fnCmdKey('&Add'      	,ck_add=1  ,0,0,'Add a new employee' )
	fnCmdKey('E&dit'     	,ck_edit=2 ,0,0,'Modify the selected report')
	fnCmdKey('Proof List'	,ck_proof=6,0,0,'Print a Proof List of Custom Reports')
	fnCmdKey('Print'     	,ck_print=4,1,0,'Run the selected report')
	fnCmdKey('E&xit'     	,ck_exit=5 ,0,1,'Return to menu')
	ckey=fnAcs(mat resp$) ! ask report #
	if ckey=ck_exit then goto Xit
	reportSelected$=resp$(rc_reportSelected)
	reportSelectedN=val(reportSelected$(1:2))
	fnPcReg_write('reportSelected',reportSelected$)
	if ckey=ck_add then
		isAdding=1
		reportSelectedN=0
		goto DoAddOrEdit
	else if ckey=ck_edit then
		isEditing=1
		goto DoAddOrEdit
	else if ckey=ck_print then
		fnPrintCustomReport(reportSelectedN)
	else if ckey=ck_proof then
		fnCustomReportProofList
	end if
loop
DoAddOrEdit: !
	rptn$=lpad$(str$(reportSelectedN),2)
	if isAdding=1 then
		read #hReports,using Freports,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat xInp,mat pp,mat ti nokey ScrFm1
	else if isEditing=1 then
		read #hReports,using Freports,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat xInp,mat pp,mat ti nokey EditReadNokey
	end if
	! L400: !
	holdrn=rn
goto ScrFm1 ! /r
	EditReadNokey: ! r:
		mat ml$(2)
		ml$(1)='A record with this number does not exist!'
		ml$(2)='Select a different numbe if you wish to add a new report.'
		fnMsgBox(mat ml$,resp$,'',48)
	goto AskReport ! /r

ScrFm1: ! r:
	if isAdding=1 then ! r: initialize variables to 0 or blank
		rn=0
		rt$=''
		mat ch$=('')
		ips=0
		sd$=''
		sd=cp=0
		mat psc=(0)
		mat pp=(0)
		mat ti=(0)
		holdrn=0
	end if  ! /r
	fnTos
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,'Report #:',mylen,1)
	fnTxt(1,mypos,2,2,0,'30',0,'')
	resp$(respc+=1)=str$(rn)
	fnLbl(2,1,'Report Title:',mylen,1)
	fnTxt(2,mypos,78,0,0,'',0,'')
	resp$(respc+=1)=rt$
	fnLbl(3,1,'Column Headings:',mylen,1)
	fnLbl(4,7,'1    2    3    4    5    6    7    8    9    0    1    2    3 ',132,0)
	fnTxt(5,1,132,0,0,'',0,'The heading can be two lines.  This will be the 1st line.')
	resp$(respc+=1)=ch$(1)
	fnLbl(6,7,'1    2    3    4    5    6    7    8    9    0    1    2    3 ',132,0)
	fnTxt(7,1,132,0,0,'',0,'This is the 2nd line of the heading line.')
	resp$(respc+=1)=ch$(2)
	mylen=50
	fnLbl(12,1,'Item for pr Selection (blank for all):',mylen,1)
	if ips>0 and ips=<udim(code$) then resp$(respc+=1)=code$(ips+1) else resp$(respc+=1)=''
	fnComboA('DataNames2',12,mylen+3,mat code$,'If you want limit the report to a value in a particular field in the employee record, Indicate which field it is by locatiing the ID number of the field using Help button.',25,0)
	fnChk(13,mylen+3,'Summarize Departmental Records:',1)
	if sd= 1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnCmdKey('&Next',1,1,0,'Save changes and move to next questions' )
	fnCmdKey('&Delete',4,0,0,'Deletes this report from your system.')
	fnCmdKey('&Cancel',5,0,1,'Return to selection screen.')
	ckey=fnAcs(mat resp$) ! ask report #
	isAdding=0
	if ckey=5 then goto AskReport
	rn=val(resp$(1)(1:2))
	if holdrn>0 and rn<>holdrn then
! r: confirm_key_change
		mat ml$(3)
		ml$(1)='You are attempting to change report number'
		ml$(2)='from '&str$(holdrn)& ' to '&str$(rn)&'.'
		ml$(3)='Take OK to continue, else Cancel.'
		fnMsgBox(mat ml$,resp$,'',49)
		if resp$='OK' then
			holdrn=rn
		else
			goto ScrFm1
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
	if resp$(6)(1:1)='T' then sd$='Y': sd=1 else sd$='N': sd=0
	if ips<0 or ips>126 or (ips>1 and ips<6) then
		mat ml$(2)
		ml$(1)='You can not use '&code$(ips+1)&' as selection criteria!'
		ml$(2)=' Take OK to select a different item.'
		fnMsgBox(mat ml$,resp$,'',48)
		goto ScrFm1
	end if
	if sd$='Y' then sd=1 else sd=0
	rt$=rt40$
	if ckey=4 then
		goto DELETEIT
		DELETEIT: !
		mat ml$(2)
		ml$(1)='You have chosen to delete report # '&rptn$
		ml$(2)='Take Ok to continue, else Cancel to keep the report.'
		fnMsgBox(mat ml$,resp$,'',49)
		if resp$='OK' then
			delete #hReports,key=rptn$:
		else
			goto AskReport
		end if
	end if
goto ScrFm2 ! /r
ScrFm2: ! r:
	! If RN=0 Then Goto DONE  ! ain't this just redundant?
	if ips=0 then goto ScrFm3
	fnTos
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,'Print Selection Criteria:',30,1)
	z=0
	for x=1 to 5
		for j=2 to 21
			fnTxt(j,x*16,12,0,0,'33',0,'If you chosen to limit the report to certain criteria, enter the values here that should match information in the employee''s record.')
			resp$(respc+=1)=str$(psc(z+=1))
		next j
	next x
	fnCmdKey('&Next',1,1,0,'Save changes and move to next questions' )
	fnCmdKey('&Back',6,0,0,'Back up a screen.')
	fnCmdKey('&Cancel',5,0,1,'Return to selection screen.')
	ckey=fnAcs(mat resp$) ! ask matching criteria
	if ckey=5 then goto AskReport
	for j=1 to 100
		psc(j)=val(resp$(j))
	next j
	if ckey=6 then goto ScrFm1
goto ScrFm3 ! /r
ScrFm3: ! r:
	fnTos
	respc=0: mylen=15: mypos=mylen+3
	fnFra(1,1,23,90,'Selection of Column Information','Select the informatin that should be printed in each column of your report.')
	fnLbl(1,1,'Print       Want ',50,1,0,1)
	fnLbl(2,1,'Item to pr                 Position    A Total',52,1,0,1)
	for j=1 to 20
		if xInp(j)>0 and xInp(j)=<udim(code$) then resp$(respc+=1)=code$(xInp(j)+1) else resp$(respc+=1)=' '
		! if xInp(j)>0 and xInp(j)=<udim(code$) then resp$(respc+=1)=code$(xInp(j)  ) else resp$(respc+=1)=' '
		fnComboA('DataNames',j+2,1,mat code$,'',25,1)
		fnTxt(j+2,37,3,0,0,'30',0,'The position is the starting position acress the page where this column should print.',1)
		resp$(respc+=1)=str$(pp(j))
		if ti(j)=1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
		fnChk(j+2,48,'',1,1) ! total the column
	next j
	fnCmdKey('&Next',1,1,0,'Save changes on this report design.' )
	fnCmdKey('&Back',6,0,0,'Back up a screen.')
	fnCmdKey('&Cancel',5,0,1,'Return to report selection screen without saving any changes.')
	ckey=fnAcs(mat resp$) ! enter column information
	if ckey<>5 then
		x=0
		for j=3 to 60 step 3
			x+=1
			for j1=1 to udim(code$)
				if resp$(j-2)=code$(j1) then xInp(x)=j1-1 : goto L1330
			next j1
			L1330: !
			pp(x)=val(resp$(j-1))
			if resp$(j)='True' then ti(x)=1 else ti(x)=0
		next j
		if reportSelectedN=rn then
			rewrite #hReports,using Freports,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat xInp,mat pp,mat ti
		else
			read #hReports,using 'form pos 1,n 2',key=lpad$(str$(rn),2): rn nokey ChangeTheNumber
		end if
	end if
goto AskReport ! /r
	ChangeTheNumber: ! r:
		write #hReports,using Freports: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat xInp,mat pp,mat ti
		delete #hReports,key=rptn$: nokey ignore
	goto AskReport ! /r

def fn_add_dn(dn_x_1$*30,dn_x_2$*30,dn_x_3$*30)
	dn_counter+=1
	DataNames$(dn_counter,1)=dn_x_1$
	DataNames$(dn_counter,2)=dn_x_2$
	DataNames$(dn_counter,3)=dn_x_3$
fnend
Xit: ! r:
close #hReports: ioerr ignore
fnXit ! /r

! L1770: ! r: ADD NEW RECORD
!   rt$='': mat ch$=('')
!   ips=sd=cp=0
!   mat psc=(0): mat xInp=(0): mat pp=(0): mat ti=(0)
!   write #hReports,using Freports: reportSelectedN,rt$,mat ch$,ips,sd,cp,mat psc,mat xInp,mat pp,mat ti
!   rptn$=lpad$(str$(reportSelectedN),2)
!   read #hReports,using Freports,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat xInp,mat pp,mat ti ! nokey L1530
! goto L400 ! /r
include: ertn
