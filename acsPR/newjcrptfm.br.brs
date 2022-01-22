! Replace S:\acsPR\newJCRptFM
! Job Cost User-Designed Report File
 
	autoLibrary
	on error goto Ertn
 
	dim cap$*128,message$*40,msgline$(2)*60,resp$(102)*150
	dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,iom$(3),scm$(3)*40
	dim io1$(10),io2$(7),fd$(20),rptemp(20),tempch$(4)*256,rptn$*6,rnew$*6
	dim rt$*51,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20),fc(20)
	dim tcj(20),tcs(20),rno$(50)*2,em$*40,wrd3$(2)*23,io3$(2),code1$(4)*30
	dim rt40$*51
	dim abbrevanme$(20)*8,fullname$(20)*20,ml$(3)*90,ty$(24)*50,ty2$(25)*30
 
	fnTop(program$,cap$="User Designed Reports - JC")
	pg=3
 
	open #1: "Name=S:\acsPR\JCREPORT.MST,KFName=S:\acsPR\JCREPORT.idx,Shr",i,outIn,k
	fnDedNames(mat fullname$,mat abbrevname$)
 
	gosub DATA_FOR_COLUMNS
SCR1: ! ask report #
ASKREPORT: !
	if rn=0 then rn=1
	fnTos(sn$="jcReport-ask") : _
	respc=0
	fnLbl(1,1,"Report #:",11,right)
	df$="S:\acsPR\Jcreport.mst" : if$="S:\acsPR\jcreport.idx" : _
	fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1,0,"Select from the list of reports. To add a report, click the Add button.")
	resp$(1)=str$(rn)
	fnCmdKey("&Add",1,0,0,"Add a new report" ) : _
	fnCmdKey("E&dit",2,1,0,"Access the highlited record") : _
	fnCmdKey("&Next Record",3,0,0,"Access next record in report # order") : _
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$) ! ask report #
	if ckey=5 then goto Xit
	editrec=addone=0
	hreport$=resp$(1)(1:2) : _
	rptn=val(resp$(1)(1:2)) : _
	rptn$=lpad$(str$(rptn),2)
	if ckey=1 then addone=1: rptn=0: goto ADD_EDIT
	if ckey=2 then editrec=1: goto ADD_EDIT
RECREATE_GRID: !
 
ADD_EDIT: !
	rptn$=lpad$(str$(rptn),2)
	if addone=1 then rn=0: rt$="": mat ch$=(""): mat tempch$=(""): ips=0: sd$="": sd=cp=0: mat psc=(0): mat pp=(0) : mat ti=(0): holdrn=0 : goto SCR2
	if editrec=1 then read #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L460
	goto L470
 
L460: mat ml$(2) : _
	ml$(1)="A record with this number does not exist!" : _
	ml$(2)="Select a different numbe if you wish to add a new report." : _
	fnmsgbox(mat ml$,resp$,cap$,48) : _
	goto ASKREPORT
L470: holdrn=rn
 
SCR2: ! add/edit first screen of report
	fnTos(sn$="Report-add") : _
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,"Report #:",mylen,1)
	fnTxt(1,mypos,2,2,0,'30',0,"") : _
	resp$(respc+=1)=str$(rn)
	fnLbl(2,1,"Report Title:",mylen,1)
	fnTxt(2,mypos,78,78,0,"",0,"") : _
	resp$(respc+=1)=rt$
 
	fnLbl(3,1,"Column Headings:",mylen,1)
	fnLbl(4,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
	fnTxt(5,1,132,132,0,"",0,"The heading can be two lines.  This will be the 1st line.") : _
	resp$(respc+=1)=ch$(1)
	fnLbl(6,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
	fnTxt(7,1,132,0,0,"",0,"This is the 2nd line of the heading line.") : _
	resp$(respc+=1)=ch$(2)
	mylen=50
	fnLbl(12,1,"Item for pr Selection (blank for all):",mylen,1)
	if ips>0 and ips=<udim(ty2$) then resp$(respc+=1)=ty2$(ips+1) else resp$(respc+=1)=""
	fncomboa("DataNames2",12,mylen+3,mat ty2$,"If you want limit the report to a value in a particular field in the report record, Indicate which field it is by locating the ID number.",25,0)
	resp$(respc+=1)=str$(psc)
	fnChk(13,mylen+3,"Summarize Category Records:",1)
	if sd= 1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnLbl(14,1,"Selection Codes:",mylen,1)
	code1$(1)="1 - Equal to" : _
	code1$(2)="2 - Equal to or greater than" : _
	code1$(3)="3 - Equal to or less than" : _
	code1$(4)="4 - Range of numbers" : _
	respc+=1: for j=1 to udim(code1$) : _
		if sc=val(code1$(j)(1:1)) then resp$(respc)=code1$(j)(1:1) : _
		next j
	fncomboa("selCode",14,mylen+3,mat code1$,"",30)
	fnCmdKey("&Next",1,1,0,"Save changes and move to next questions" ) : _
	fnCmdKey("&Delete",4,0,0,"Deletes this report from your system.") : _
	fnCmdKey("&Cancel",5,0,1,"Return to selection screen.")
	ckey=fnAcs(mat resp$) ! edit first screen for report format
	addone=0
	if ckey=5 then goto SCR1
	if ckey=4 then goto DELETEIT : goto SCR3
	rn=val(resp$(1)(1:2))
	if holdrn>0 and rn<>holdrn then goto L770 else goto L790
L770: mat ml$(3) : _
	ml$(1)="You are attempting to change report " : _
	ml$(2)="# "&str$(holdrn)& " to report # "&str$(rn)&"." : _
	ml$(3)="Take OK to continue, else Cancel." : _
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then holdrn=rn: goto L790 else goto SCR2
L790: rt40$=resp$(2)
	ch$(1)=resp$(3)
	ch$(2)=resp$(4)
	ips=0
	for j=1 to udim(ty2$)
		if resp$(5)=trim$(ty2$(j)) then ips=val(ty2$(j)(1:2)): goto L860
	next j
L860: if resp$(6)(1:1)="T" then sd$="Y": sd=1 else sd$="N": sd=0
	if ips>1 and (ps>1 and ps<5) then goto L880 else goto L890 ! can't use name fields fro selection criteria
L880: mat ml$(2) : _
	ml$(1)="You can not use "&trim$(ty2$(ips+1))&" as selection criteria!" : _
	ml$(2)=" Take OK to select a different item." : _
	fnmsgbox(mat ml$,resp$,cap$,48) : _
	goto SCR2
L890: if sd$="Y" then sd=1 else sd=0
	rt$=rt40$
	if ckey=1 then rewrite #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
	if ips>0 then goto SCR5 else : goto SCR3 ! ask criteris for pr selection
	goto SCR3
 
DELETEIT: !
	mat ml$(2) : _
	ml$(1)="You have chosen to delete report # "&rptn$ : _
	ml$(2)="Take Ok to continue, else Cancel to keep the report." : _
	fnmsgbox(mat ml$,resp$,cap$,49)
	if resp$="OK" then goto L980 else goto L990
L980: delete #1,key=rptn$:
L990: goto SCR1
SCR3: ! ask column # to edit
	if column=0 then column=1
	fnTos(sn$="ask-column") : _
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,"Column #:",mylen,1)
	fnTxt(1,mypos,2,2,0,'30',0,"Column numbers must be from 1 to 20,") : _
	resp$(respc+=1)=str$(column)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$) ! acs column
	if ckey=5 then goto SCR1
	column=val(resp$(1))
	if column=0 then goto SCR1
	if column<0 or column>20 then goto SCR2
	goto SCR4 ! allow to edit
SCR4: ! edit/columns
	fnTos(sn$="columns") : _
	mylen=25 : mypos=mylen+2: respc=0: left=1
	fnLbl(1,1,"Report #:  "&str$(rn),mylen,left)
	fnLbl(2,1,"Column #:  "&str$(column),mylen,left)
	fnLbl(3,1,"Formula for printing:",mylen,left)
	fnTxt(3,mypos,50,50,0,"",0,"See instructions for creating the formula for the information that is to pr in this column.") : _
	resp$(respc+=1)=f$(column)
	fnLbl(4,1,"Starting Position:",mylen,left)
	fnTxt(4,mypos,3,3,0,'30',0,"") : _
	resp$(respc+=1)=str$(pp(column))
	fnLbl(5,1,"Field Size:",mylen,left)
	fnTxt(5,mypos,3,3,0,'30',0,"") : _
	resp$(respc+=1)=str$(ppr(column))
	fnLbl(6,1,"Decimal Positions:",mylen,left)
	fnTxt(6,mypos,1,1,0,'30',0,"") : _
	resp$(respc+=1)=str$(dp(column))
	fnChk(7,mypos,"Detail Print:",left)
	if fc(column)=1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnChk(8,mypos,"Total by Job:",left)
	if tcj(column)=1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnChk(9,mypos,"Grand Totals:",left)
	if tcs(column)=1 then resp$(respc+=1)='True' else resp$(respc+=1)='False'
	fnCmdKey("&Next",1,1,0,"Save changes and move to next column" ) : _
	fnCmdKey("&Review Variables",2,0,0,"Get a list of variables that can be used in a formula.") : _
	fnCmdKey("&Delete",4,0,0,"Deletes this column from the report.") : _
	fnCmdKey("C&omplete",3,0,1,"Save changes and return to main screen.") : _
	fnCmdKey("&Cancel",5,0,1,"Return to main screen without saving any changes on this screen.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto SCR1
	psc(column)=column ! set array to column number
	f$(column)=resp$(1) ! formula
	pp(column)=val(resp$(2)) ! starting position
	ppr(column)=val(resp$(3)) ! field size
	dp(column)=val(resp$(4)) ! decimal position
	if resp$(5)='True' then fc(column)=1 else fc(column)=0
	if resp$(6)='True' then tcj(column)=1 else tcj(column)=0
	if resp$(7)='True' then tcs(column)=1 else tcs(column)=0
	rewrite #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
	if ckey=1 then column=min(column+1,20): goto SCR4
	if ckey=4 then f$(column)="": pp(column)=0: ppr(column)=0: dp(column)=0 : _
		fc(column)=0: tcj(column)=0: tcs(column)=0: rewrite #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs : _
		goto SCR4
	if ckey=3 then goto L1790
	if ckey=2 then goto REVIEW_VARIABLES
	goto SCR1
 
CHANGETHENUMBER: !
	write #1,using L1780: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
	delete #1,key=rptn$: nokey L1510
L1510: lst=0
	form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
	goto SCR1
MAIN_SCREEN: !
	if rno=0 then rno=1
	fnTos(sn$="user1") : _
	mylen=25 : mypos=mylen+2: respc=0: left=1
	df$="S:\acsPR\Jcreport.mst" : if$="S:\acsPR\jcreport.idx" : _
	fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1) : _
	fncombof("CRjcreportALL",1,1,80,df$,1,2,3,74,if$,2)
	resp$(1)=str$(rno)
	fnCmdKey("&Add",1,0,0,"Add a new customer" ) : _
	fnCmdKey("E&dit",2,1,0,"Review or change the record.") : _
	fnCmdKey("&Cancel",5,0,1,"Exit the program.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	rno=val(resp$(1)(1:2))
	if ckey=1 then addone=1: rt$="" : mat ch$=("") : ips=sd=cp=sc=0 : mat ps=(0) : _
		mat f$=("") : mat pp=(0) : mat ppr=(0) : mat dp=(0) : mat fc=(0) : _
		mat tcj=(0) : mat tcs=(0): goto EDIT_ADD_REPORT
	if ckey=2 then rptn$=lpad$(str$(rno),2) : _
		read #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey MAIN_SCREEN : goto EDIT_ADD_REPORT
 
EDIT_ADD_REPORT: !
	fnTos(sn$="namlst1") : _
	mylen=25 : mypos=mylen+2: respc=0: left=1
	fnLbl(1,1,"Report #:",mylen,left)
	fnTxt(1,mypos,2,0,0,'30',0,"Each report must be assigned a unique number between 1 and 100.") : _
	resp$(respc+=1)=str$(rn)
	fnLbl(2,1,"Report Name:",mylen,left)
	fnTxt(2,mypos,51,0,0,"",0,"Give each report a unique descriptive name.") : _
	resp$(respc+=1)=rt$
	fnLbl(4,1,"Colort #:",mylen,left)
	fnChk(2,mypos,"Print All Jobs:",left) : _
	resp$(respc+=1)='False'
	fnChk(3,mypos,"Print One Job Per Page:",left) : _
	resp$(respc+=1)='False'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
L1780: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
L1790: close #1:
	fnrx(rn)
	execute "INDEX S:\acsPR\JCREPORT.MST,S:\acsPR\JCREPORT.idx,1,2,Replace,DupKeys -n"
	fnchain('S:\acsPR\newjcRptS1')
 
	restore #1,key>="  ": nokey L2490
	fnopenwin(win=102,10,28,15,52,cap$)
	wrd3$(1)="Print All Report Files"
	wrd3$(2)="Select Reports to Print"
	io3$(1)="4,2,C 23,N"
	io3$(2)="5,2,C 23,N"
	pr f "16,34,C 11,B,5": "Cancel (F5)"
	rinput #win,select mat io3$,attr "H": mat wrd3$
	prtall=curfld-1
	close #win: ioerr L1950
L1950: if cmdkey=5 then goto SCR1
	if prtall=0 then goto L2060
	for j=1 to 20
		fnopenwin(win=103,10,20,15,59,cap$)
		if j>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Report Number Entered was "&rno$(j-1)
		pr #win,fields "4,2,C 23,N": "Report Number to Print:"
		pr f "16,35,C 09,B,5": "Done (F5)"
L2020: input #win,fields "4,26,N 2,UET,N": rno(j) conv L2020
		rno$(j)=lpad$(str$(rno(j)),2)
		if cmdkey=5 or rno(j)=0 then goto L2060
	next j
L2060: on fkey 5 goto L2490
	fnopenprn
	k=0
L2090: if prtall=0 then goto L2140
L2100: k=k+1
	if val(rno$(k))=0 then goto L2490
	read #1,using L1780,key=rno$(k): rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L2100
	goto L2160
L2140: read #1,using L1780: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs eof L2490
	form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
L2160: pr #255,using L2170: "Job Cost Report File Proof List"
L2170: form skip 2,pos 50,c 32
	pr #255,using L2190: "Report Number",rn
L2190: form pos 1,c 13,pos 20,pic(zz)
	pr #255,using L2210: "Report Title",rt$
L2210: form pos 1,c 12,pos 13,cc 66
	pr #255,using L2230: "Column Headings",ch$(1)
L2230: form pos 1,c 15,skip 2,c 132
	pr #255,using L2250: ch$(2)
L2250: form pos 1,c 132,skip 2
	pr #255,using L2270: "Item # for Selection",ips
L2270: form pos 1,c 20,pos 30,pic(zz#)
	pr #255,using L2290: "Summarize Categories",sd
L2290: form pos 1,c 26,pos 32,pic(#)
	pr #255,using L2290: "Condense Print",cp
	pr #255,using L2290: "Selection Code",sc
	pr #255,using L2330: "Print Selection Criteria"
L2330: form skip 1,pos 1,c 30,skip 2
	for j=1 to 20
		pr #255,using L2360: psc(j),psc(j+20),psc(j+40),psc(j+60),psc(j+80)
L2360: form pos 1,5*n 20.3
	next j
	pr #255,using L2390: "Formula for Value","Starting","# of pr Positions","# of Decimal","skip Detail Print","Total Column","Overall Totals"
L2390: form skip 1,pos 1,c 17,pos 39,c 8,pos 48,c 20,pos 71,c 12,pos 84,c 17,pos 103,c 12,pos 119,c 14
	pr #255,using L2410: "to be Printed","Print Position","Required","Positions","by Job","by System"
L2410: form pos 1,c 13,pos 38,c 14,pos 53,c 8,pos 72,c 9,pos 107,c 6,pos 123,c 9,skip 2
	for j=1 to 20
		pr #255,using L2440: f$(j),pp(j),ppr(j),dp(j),fc(j),tcj(j),tcs(j)
L2440: form pos 1,c 50,pos 52,n 3,pos 56,n 3,pos 76,n 1,pos 93,n 1,pos 110,n 1,pos 127,n 1
	next j
	pr #255: newpage
	goto L2090
 
L2490: fncloseprn
	on fkey 5 ignore
	goto SCR1
 
 
SRCH: !
	bk=0
 
Xit: fnXit
 
include: ertn
 
SCR5: ! selection criteria
	if ips=0 then goto SCR4
	fnTos(sn$="Report-sel") : _
	respc=0: mylen=15: mypos=mylen+3
	fnLbl(1,1,"Print Selection Criteria:",30,1)
	z=0
	for x=1 to 5
		for j=2 to 21
			fnTxt(j,x*16,12,0,0,"33",0,"If you chosen to limit the report to certain criteria, enter the values here that should match information in the employee's record.") : _
			resp$(respc+=1)=str$(psc(z+=1))
		next j
	next x
	fnCmdKey("&Next",1,1,0,"Save changes and move to next questions" ) : _
	: _
	: fnCmdKey("&Back",6,0,0,"Back up a screen.") : _
	fnCmdKey("&Cancel",5,0,1,"Return to selection screen.")
	ckey=fnAcs(mat resp$) ! ask matching criteria
	if ckey=5 then goto SCR1
	for j=1 to 100
		psc(j)=val(resp$(j))
	next j
	if ckey=6 then goto SCR2
	goto SCR3
DATA_FOR_COLUMNS: !
	ty$(1)= "X1  Job Number                 6"
	ty$(2)= "x2  Job Name                  40"
	ty$(3)= "x3  Job Address               30"
	ty$(4)= "x4  Job Address               30"
	ty$(5)= "x5  City, State Zip           30"
	ty$(6)= "X6  Estimated Completion Date  6"
	ty$(7)= "X7  Contract Amount           10"
	ty$(8)= "X8  Billing to Date           10"
	ty$(9)= "X9  Billing Status             2"
	ty$(10)= "x10 Category Number            5"
	ty$(11)= "x11 Category Name             25"
	ty$(12)= "X12 Labor Estimate            10"
	ty$(13)= "X13 Hours Estimate            10"
	ty$(14)= "X14 Other Estimate            10"
	ty$(15)= "X15 Labor Cost to Date        10"
	ty$(16)= "X16 Hours Worked to Date      10"
	ty$(17)= "X17 Other Cost to Date        10"
	ty$(18)= "X18 Labor Cost - Current      10"
	ty$(19)= "X19 Hours Worked - Current    10"
	ty$(20)= "X20 Other Cost - Current      10"
	ty$(21)= "X21 Units Completed           10"
	ty$(22)= "X22 Estimated Units            8"
	ty$(23)= "X23 Labor % Complete           3"
	ty$(24)= "X24 Other % Complete           3"
	ty1$(1)=""
	for j=1 to 24
		ty2$(j+1)=str$(j)&" = "&ty$(j)(5:24)
	next j
return
REVIEW_VARIABLES: !
	fnTos(sn$="ask-column") : _
	respc=0: mylen=15: mypos=mylen+3
	fncomboa("Variables",1,mylen+3,mat ty$,"Listing of variables that can be used in a formula.",60,0)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$) ! variables
	goto SCR4
