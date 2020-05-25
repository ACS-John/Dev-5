! formerly S:\acsPR\newJCInput
! Enter (Job Cost) Time
 
	autoLibrary
	on error goto Ertn
 
	dim sub$*30
	dim cn$*11
	dim n$*40
	dim empnam$*30
	dim ji1(6)
	dim jn$*6
	dim ji2(5)
	dim ch2$(14)
	dim cm2$(14)
	dim bk$(20)*28
	dim resp$(30)*60
	dim ml$(3)*80
	dim fullname$(20)*20
	dim comboname$(22)*23
	dim item2$(14)*20
	dim tdt(4)
	dim tcd(3)
	dim tdet(23)
 
	fnTop(program$)
 
	fnDedNames(mat fullname$)
	for j=1 to 20: comboname$(j+1)=cnvrt$("pic(zz)",j)&" "&fullname$(j): next j
	comboname$(1)=" 0 Not Applicable"
	comboname$(22)="21 Add Amount to Pay"
 
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
	open #5: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-name.h[cno],Shr",internal,input,keyed
	open #7: "Name=[Q]\PRmstr\Burden.H[cno],KFName=[Q]\PRmstr\BurdenIdx.H[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed
	if exists("jcWork."&session$) >0 then goto L280 else goto L300
L280: mat ml$(2)
	ml$(1)="An unposted file appears to exist! "
	ml$(2)="Enter Yes to work with this file, else No to create a new batch of entries."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then goto L320 else goto L300
L300: open #3: "Name=jcWork."&session$&",SIZE=0,RecL=84,Replace",internal,outIn,relative
	goto L330
L320: open #3: "Name=jcWork."&session$,internal,outIn,relative
L330: open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed
	open #14: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,input,keyed
	open #12: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed
	open #13: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed
 
	addone=1 ! set code as adding when first entering
 
TRANSACTION_ENTRY: !
	if addone=1 then ji1(5)=ji1(6)=ji2(3)=0: ji1(2)=2
L420: fnTos
	respc=0 : frac=0
	mylen=28 : mypos=mylen+3
	fnLbl(1,1,"Employee #:",mylen,1)
	fncmbemp(1,mypos)
	resp$(respc+=1)=str$(ji1(1))
	fnLbl(2,1,"Method of Payment:",mylen,1)
	opt$(1)="1 = Salary" : opt$(2)= "2 = Hourly"
	opt$(3)= "3 = Both"
	fncomboa("Methods",2,mypos,mat opt$,empty$,13)
	if ji1(2)=1 then resp$(respc+=1)=opt$(1)
	if ji1(2)=2 then resp$(respc+=1)=opt$(2)
	if ji1(2)=3 then resp$(respc+=1)=opt$(3)
	fnLbl(3,1,"Date:",mylen,1)
	fnTxt(3,mypos,8,8,0,"1",0,"Date of transaction")
	resp$(respc+=1)=str$(ji1(3))
	fnLbl(4,1,"Payroll Department:",mylen,1)
	fncombof("Deptname",4,mypos,25,"[Q]\PRmstr\DeptName.h[cno]",1,3,4,25,"[Q]\PRmstr\DeptNameIdx.h[cno]",0,0, " ",0,0)
	resp$(respc+=1)=str$(ji1(4))
	fnLbl(5,1,"Regular Hours:",mylen,1)
	fnTxt(5,mypos,8,8,0,"32",0,"")
	resp$(respc+=1)=str$(ji1(5))
	fnLbl(6,1,"Overtime Hours:",mylen,1)
	fnTxt(6,mypos,8,8,0,"32",0,"")
	resp$(respc+=1)=str$(ji1(6))
	fnLbl(7,1,"Job Number:",mylen,1)
	fncmbjob(7,mypos)
	resp$(respc+=1)=jn$
	fnLbl(8,1,"Category:",mylen,1)
	fncmbcategory(8,mypos)
	resp$(respc+=1)=str$(ji2(1))
	fnLbl(9,1,"Sub-Category:",mylen,1)
	fncmbsubcat(9,mypos)
	resp$(respc+=1)=str$(ji2(2))
	fnLbl(10,1,"Amount:",mylen,1)
	fnTxt(10,mypos,10,10,0,"10",0,"Amount to be charged to job. Payroll will be extended as it is posted.")
	resp$(respc+=1)=str$(ji2(3))
	fnLbl(11,1,"Deduction/Addition Code:",mylen,1)
	fncomboa("Deductions",11,mypos,mat comboname$,empty$,23)
	if ji2(4)=0 then resp$(respc+=1)=comboname$(1): goto L700
	if ji2(4)>0 and ji2(4)<=20 then resp$(respc+=1)=comboname$(ji2(4)) else resp$(respc+=1)=""
L700: fnLbl(12,1,"Units:",mylen,1)
	fnTxt(12,mypos,6,6,0,"30",0,"Enter units, if applicable.")
	resp$(respc+=1)=str$(ji2(5))
	fnLbl(13,1,"Personnel Burden:",mylen,1)
	fnTxt(13,mypos,10,10,0,"10",0,"Personnel burden will calculated automatically if you have the burden % set up on each employee.")
	resp$(respc+=1)=str$(pt)
	picture=0
	fnCmdKey("&Save",1,1,0,"Saves all changes.")
	fnCmdKey("Co&rrection",7,0,0,"Make a correction to any entry.")
	fnCmdKey("&LIsting",9,0,0,"Print a listing of all entries.")
	fnCmdKey("De&lete",4,0,0,"Deletes this job.")
	fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
	fnCmdKey("&Post",8,0,1,"Post these entries to the job files.")
	fnAcs2(mat resp$,ckey) ! detail job screen     editrec
	if ckey=5 then goto L830 else goto L850
L830: mat ml$(2)
	ml$(1)="You have chosen to cancel without postng these entries!  "
	ml$(2)="Take Yes to Exit, else take No to return to the entry screens."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then goto Xit else goto TRANSACTION_ENTRY
L850: if ckey=7 then goto CORRECTIONS
	if ckey=8 then goto POSTTOJOBS
	if ckey=9 then goto PRINTPROOFLIST
	ji1(1)=val(resp$(1)(1:8)) ! employee number
	ji1(2)=val(resp$(2)(1:1)) ! Method of pay
	ji1(3)=val(resp$(3)) ! date
	ji1(4)=val(resp$(4)(1:3)) ! dept #
	ji1(5)=val(resp$(5)) ! regular hours
	ji1(6)=val(resp$(6)) ! ot hours
	jn$=resp$(7)(1:6) ! job #
	if trim$(jn$)="" then goto L960 else goto L980
L960: mat ml$(2)
	ml$(1)="You failed to enter a job number. Take Yes to continue;"
	ml$(2)="else take No to return to previous screen and enter the job number."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then goto L980 else goto L420
L980: ji2(1)=val(resp$(8)(1:3)) ! category
	if ji2(1)=0 and dontwarnsubcat=0 then goto L1000 else goto L1020
L1000: mat ml$(2)
	ml$(1)="You failed to enter a category number. Take Yes to continue;"
	ml$(2)="else take No to return to previous screen and enter the category number."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then dontwarnsubcat=1: goto L1020 else goto L420
L1020: ji2(2)=val(resp$(9)(1:3)) ! sub-category
	if ji2(2)=0 and dontwarnsubcat=0 then goto L1040 else goto L1060
L1040: mat ml$(2)
	ml$(1)="You failed to enter a sub-category number. Take Yes to continue;"
	ml$(2)="else take No to return to previous screen and enter the sub-category number."
	fnmsgbox(mat ml$,resp$,'',52)
	if resp$="Yes" then dontwarnsubcat=1 : goto L1060 else goto L420
L1060: ji2(3)=val(resp$(10)) ! amount
	ji2(4)=val(resp$(11)(1:2)) ! deduction code
	ji2(5)=val(resp$(12)) ! units
	pt=val(resp$(13)) ! personnel burden
	gosub UPDATE_AMOUNT
	empnam$=""
	read #1,using "FORM POS 9,C 30",key=cnvrt$("pic(ZZZZZZZ#)",ji1(1)): empnam$ nokey L1110
L1110: if addone=1 then goto L1120 else goto L1150
L1120: write #3,using L1140: mat ji1, jn$, mat ji2, pt, empnam$, sal
	goto L1160
L1140: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
L1150: rewrite #3,using L1140,rec=editrec: mat ji1, jn$, mat ji2, pt, empnam$, sal noRec CORRECTIONS
L1160: if addone=1 then goto TRANSACTION_ENTRY else goto CORRECTIONS
 
PRINTPROOFLIST: !
	on fkey 5 goto PROOF_LIST_DONE
	fnopenprn
	goto L1480
 
PROOF_LIST_HDR: !
	pr #255,using L1440: env$('cnam')
	pr #255,using L1440: "Job Cost Input Proof List"
	pr #255,using L1440: "Date: "&date$&"      Time: "&time$
L1440: form pos 1,cc 113,skip 1
	pr #255: "Ref #   Emp #  Method-Pay  Date   Dept  Reg-Hrs   OT-Hrs  Job #   Category  Sub-Category   Amount  Ded-Add  Units"
	return
 
L1480: gosub PROOF_LIST_HDR
	for j=1 to lrec(3)
		read #3,using L1140,rec=j: mat ji1,jn$,mat ji2,pt
		if j=1 then goto L1550
		if ji1(1)=en then goto L1590
		pr #255,using L1540: " ________"," ________"," ____________",t5,t6,t10 pageoflow PROOF_LIST_NWPG
L1540: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Total",pos 38,2*n 9.2,x 29,n 13.2,skip 2
L1550: en=ji1(1)
		t5=0
		t6=0
		t10=0
L1590: pr #255,using L1600: j,mat ji1,jn$,mat ji2 pageoflow PROOF_LIST_NWPG
L1600: form pos 1,n 5,n 8,n 6,n 13,n 5,2*n 9.2,x 2,c 6,n 11,n 10,n 13.2,n 6,n 10.2,skip 1
		t5=t5+ji1(5)
		t6=t6+ji1(6)
		t10=t10+ji2(3)
		gt5=gt5+ji1(5)
		gt6=gt6+ji1(6)
		gt10=tg10+ji2(3)
	next j
	pr #255,using L1540: " ________"," ________"," ____________",t5,t6,t10
	pr #255,using L1700: " ________"," ________"," ____________",gt5,gt6,gt10
L1700: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Grand Totals",pos 38,2*n 9.2,x 29,n 13.2,skip 2
PROOF_LIST_DONE: !
	gt5=gt6=gt10=0
	fncloseprn
goto TRANSACTION_ENTRY
 
POSTTOJOBS: !
	close #1: ioerr ignore
	close #2:
	close #3:
	close #11:
	close #12:
	close #13:
fnchain("S:\acsPR\newJCMerge")
 
PROOF_LIST_NWPG: !
	pr #255: newpage
	gosub PROOF_LIST_HDR
	continue
 
Xit: fnXit
 
! INPUT FROM DISKETTE FILE    ! took this option out on new system
 
CORRECTIONS: !
	addone=0: editone=0
	fnTos
	ch2$( 1)="Rec #"
	ch2$( 2)="Employee #"
	ch2$( 3)="MOP"
	ch2$( 4)="Date"
	ch2$( 5)="Dept #"
	ch2$( 6)="RegHrs"
	ch2$( 7)="OTHrs"
	ch2$( 8)="Job #"
	ch2$( 9)="Cat #"
	ch2$(10)="Su-Cat"
	ch2$(11)="Amount"
	ch2$(12)="Ded #"
	ch2$(13)="Units"
	ch2$(14)="Burden"
	mat ch2$(14) ! : Mat CM2$(14) : Mat ITEM2$(14)
	cm2$(1)="30": cm2$(2)="30": cm2$(3)="30"
	cm2$(4)="1"
	cm2$(5)="30": cm2$(6)="32": cm2$(7)="32"
	cm2$(8)="": cm2$(9)="30": cm2$(10)="30"
	cm2$(11)="10": cm2$(12)="30": cm2$(13)="30": cm2$(14)="10"
	fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
	restore #3:
	do
		read #3,using L1140: mat ji1, jn$, mat ji2, pt, empnam$, sal eof L2160
		item2$(1)=str$(rec(3)): item2$(2)=str$(ji1(1))
		item2$(3)=str$(ji1(2)): item2$(4)=str$(ji1(3))
		item2$(5)=str$(ji1(4)): item2$(6)=str$(ji1(5))
		item2$(7)=str$(ji1(6)) : item2$(8)=jn$
		item2$(9)=str$(ji2(1)): item2$(10)=str$(ji2(2))
		item2$(11)=str$(ji2(3)) : item2$(12)=str$(ji2(4))
		item2$(13)=str$(ji2(5)): item2$(14)=str$(pt)
		fnflexadd1(mat item2$)
	loop
	L2160: !
	fnCmdKey("&Add",1,0,0,"Add a new transaction." )
	fnCmdKey("E&dit",2,1,0,"Edit the highlited record")
	fnCmdKey("&Delete",4,0,0,"Deletes the highlited record")
	fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new transaction information")
	fnCmdKey("E&Xit",5,0,1,"Returns to main screen.")
	fnAcs2(mat resp$,ckey) ! review_details  grid of transactions
	if ckey=5 then goto TRANSACTION_ENTRY
	editrec=val(resp$(1))
	if ckey=1 then addone=1: mat ji1=(0): mat ji2=(0): jn$="": goto TRANSACTION_ENTRY
	if ckey=2 then read #3,using L1140,rec=editrec: mat ji1, jn$, mat ji2, pt, empnam$, sal: editone=1 : goto TRANSACTION_ENTRY
	if ckey=4 then delete #3,rec=editrec: : goto CORRECTIONS
goto CORRECTIONS
UPDATE_AMOUNT: !
	read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=cnvrt$("pic(ZZZZZZZ#)",ji1(1))&cnvrt$("pic(ZZ#)",ji1(4)): teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet nokey L2270
goto L2280
L2270: !
	mat ml$(2)
	ml$(1)="There is no department number "&str$(ji1(4))&" on employee number "&str$(ji1(1))&"!"
	ml$(2)="Take OK to correct."
	fnmsgbox(mat ml$,resp$,'',0)
goto L420
L2280: !
	if ji2(3)=0 then ji2(3)=tdet(2)*ji1(5)+tdet(3)*ji1(6)
	if rtrm$(jn$)="" and ji2(1)=0 then goto L2390
	read #11,using L2310,key=lpad$(rtrm$(jn$),6): n$ nokey L2330
	L2310: form pos 7,c 40
goto L2340
L2330: !
	mat ml$(2)
	ml$(1)="The job # number appears to be an incorrect number!  "
	ml$(2)="Take OK to correct."
	fnmsgbox(mat ml$,resp$,'',0)
goto L420
L2340: !
	cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(ji2(1)),5)
	! Read #12,Using 2360,Key=CN$: KC$ Nokey 2380     ! dont verify this kj
	form pos 1,c 11
	goto L2390
	mat ml$(2)
	ml$(1)="Category # "&str$(ji2(1))&" has never been used on this job before!  "
	ml$(2)="Do you wish to use this number or select a different one?"
	fnmsgbox(mat ml$,resp$,'',36)
if resp$="Yes" then goto L2390 else goto L420
L2390: !
	if ji2(2)=0 then goto L2450
	read #13,using L2420,key=lpad$(str$(ji2(2)),3): sub$ nokey L2440
	L2420: form pos 4,c 30
goto L2450
L2440: !
	mat ml$(2)
	ml$(1)="The subcategory # number appears to be an incorrect number!  "
	ml$(2)="Take OK to correct."
	fnmsgbox(mat ml$,resp$,'',0)
goto L420
L2450: !
	if lrec(13)=0 then goto L2480 ! if they are not using subcategory, skip warning
	if ji2(2)=0 then goto L2470
goto L2480
L2470: !
	mat ml$(2)
	ml$(1)="The subcategory # number cannot be zero.!  "
	ml$(2)="Take OK to correct."
	fnmsgbox(mat ml$,resp$,'',0)
goto L420
L2480: !
if ji2(4)<1 or ji2(4)>20 then pt=ptp/100*ji2(3) else pt=0
	eno$=cnvrt$("n 8",ji1(1)) : ptp=0
	read #7,using "form pos 39,n 6.3",key=eno$: ptp nokey L2510
	L2510: !
	if ji2(4)<1 or ji2(4)>20 then pt=ptp/100*ji2(3) else pt=0
	if c1=2 then goto POSTTOJOBS
return
include: Ertn
