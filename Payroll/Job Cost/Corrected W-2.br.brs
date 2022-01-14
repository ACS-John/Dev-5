! used to file a corrected w2 using prace.exe
 
autoLibrary
fnTop(program$)
on error goto Ertn
 
dim ss$*11,d(14),ty(21),s(13),t(13),z$*8
dim desc$(6)*15,amt(6),io1$(12)
dim tcp(32),tdc(10),resp$(70)*50
dim w(13),a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12
dim oldw(13)
dim heading$*50
dim fa$(2),fb$(1),fc$(1),fd$(1),l$(10),newdedfed(20),newdedcode(20)
dim abrevname$(20)*8
dim in4$(30)
dim dedcode$(20)*2
dim dedyn$(20)*1
dim miscded(20),box12(20),txt$*80,totalbox12(20)
dim fm4$*255
fm4$="form  pos 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
read #1,using 'form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 618,10*n 1,pos 638,10*n 1': mat a$,b$,mat d$,loccode,mat e$,mat dedcode,mat dedfed
for j=1 to 3: a$(j)=a$(j)(1:30): next j
close #1:
dim fullname$(20)*20,newcalcode(20)
dim dedfica(20),dedst(20),deduc(20)
fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
DATE_SCREEN: !
L290: fnTos
	rc=cf=0: mylen=25 : mypos=mylen+3
	fnFra(1,1,3,50,"Date Range for W2's","Normally this would the first and last day of the calendar year",0)
	cf+=1 : fratype=cf
	fnLbl(1,1,"Starting Date:",mylen,1,0,1)
	fnTxt(1,mypos,10,0,1,"3",0,"First day of calendar year",1)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,1)
	fnTxt(2,mypos,10,0,1,"3",0,"Last day of calendar year",1)
	resp$(rc+=1)=str$(end_date)
	fnCmdKey("Next",1,1,0,"Prints the report")
	fnCmdKey("Cancel",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	beg_date=val(resp$(1))
	end_date=val(resp$(2))
	taxyear=val(resp$(2)(1:4))
	if beg_date=0 or end_date=0 then goto L290
	ssrate=.062
	ssmax=102000
	mcrate=.0145
	mcmax=999999
	w1=4
	namcde$="F"
	topmargin=13
	bottom=141
	posx=130
ASK_INFO: !
	fnTos
	rc=cf=0: mylen=46: mypos=mylen+3
	fnFra(1,1,5,60,"Print W-2s","This W-2 program prints to preprinted W2 forms coded with 22222.",0)
	cf+=1 : franum=cf
	fnLbl(1,1,"Social Security Withholding Rate:",mylen,1,0,franum)
	fnTxt(1,mypos,10,0,1,"34",0,"Use format such as .062.",franum)
	resp$(rc+=1)=str$(ssrate)
	fnLbl(2,1,"Maximum Wage Subject to SS Withholdings:",mylen,1,0,franum)
	fnTxt(2,mypos,10,0,1,'10',0,"Enter the maximum wage subject to social security withholdings for the current year just ended.",franum)
	resp$(rc+=1)=str$(ssmax)
	fnLbl(4,1,"Medicare Withholding Rate:",mylen,1,0,franum)
	fnTxt(4,mypos,10,0,1,"34",0,"Use format such as .0145 .",franum)
	resp$(rc+=1)=str$(mcrate)
	fnLbl(5,1,"Maximum Wage Subject to Medicare Withholdings:",mylen,1,0,franum)
	fnTxt(5,mypos,10,0,1,'10',0,"At the present time there is no maximum.  Enter a number larger than any one's wages can be. For example, 999999.00",franum)
	resp$(rc+=1)=str$(mcmax)
	fnFra(8,1,3,60,"Printing or Exporting","You have the option to either pr the W-2s or export them to another system for printing.")
	cf+=1 : franum=cf : mylen=26 : mypos=mylen+2
	fnOpt(1,3,"Print W-2",0,franum)
	resp$(rc+=1)='True'
	fnOpt(2,3,"Export to another system",0,franum)
	resp$(rc+=1)='False'
	fnFra(13,1,3,60,"Identify the Following Deductions","You have twenty miscellaneous deductions available to you. If you have Qualified Pension or Dependent Care, start with the first deduction and count down to identify the number of the deduction.")
	cf+=1 : franum=cf
	fnLbl(1,1,"Qualified Pension Plan:",mylen,1,0,franum)
	fnTxt(1,mypos,2,0,1,'30',0,"If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
	resp$(rc+=1)=str$(pn1)
	fnLbl(2,1,"Dependent Care Benefits:",mylen,1,0,franum)
	fnTxt(2,mypos,2,0,1,'30',0,"If you have dependent care benefits that should be identifies on the W-2, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum)
	resp$(rc+=1)=str$(dc1)
	fnLbl(18,1,"Employee Name Format-(F)irst name 1st; (L)ast name 1st:",57,1,0,0)
	fnTxt(18,60,1,0,1,"",0,"Is the first name shown first in the employee record or is the Last name shoun first. Indicate with an F or an L.",0)
	resp$(rc+=1)=namcde$
	fnFra(20,1,3,60,"W-2 Alignment","You can move the pr up or down on either W-2 by increasing or decreasing the millimeters on the top margin.")
	cf+=1 : franum=cf
	fnLbl(1,1,"Top Margin - Top W-2:",mylen,1,0,franum)
	fnTxt(1,mypos,3,0,1,'30',0,"Decrease the top margin to move the pr up. Increase the top margin to move the W-2 down.",franum)
	resp$(rc+=1)=str$(topmargin)
	fnLbl(2,1,"Top Margin - Bottom W-2:",mylen,1,0,franum)
	fnTxt(2,mypos,3,0,1,'30',0,"The spacing on the bottom W-2 is controlled seperate from the top W-2.",franum)
	resp$(rc+=1)=str$(bottom)
	fnLbl(3,1,"Position of Pension X:",mylen,1,0,franum)
	fnTxt(3,mypos,3,0,1,'30',0,"Increasing the position of the X will move it right.  Decreasing will move it left.",franum)
	resp$(rc+=1)=str$(posx)
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	ssrate=val(resp$(1))
	ssmax=val(resp$(2))
	mcrate=val(resp$(3))
	mcmax=val(resp$(4))
	if resp$(5)='True' then w1=4 else w1=3
	if resp$(6)='True' then w1=3
	pn1=val(resp$(7))
	dc1=val(resp$(8))
	namcde$=resp$(9)
	topmargin=val(resp$(10))
	bottom=val(resp$(11))
	posx=val(resp$(12))
	med$="Y"
	gosub ASK_DEDUCTIONS
	gosub VBOPENPRINT
	goproc=0
	if w1=2 then gosub L5930
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
	open #2: "Name=[Q]\PRmstr\department.h[cno],KFName=[Q]\PRmstr\deptidx.h[cno]",internal,outIn,keyed
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
	open #3: "Name=[Temp]\Addr."&session$,internal,input ioerr L960
	close #3,free:
L960: open #3: "Name=[Temp]\Addr.[Session],size=0,RecL=33,NoShr",internal,output
	if w1=3 then gosub L4430
	write #3,using L990: ssmax,w1,nw
L990: form pos 1,n 10.2,2*n 1
	open #14: "Name=[Q]\PRmstr\W2Box16.h[cno],KFName=[Q]\PRmstr\W2Index.h[cno],Shr",i,i,k ioerr L1030
	z$="NO" ! 1/12/90
	box16=1
L1030: if loccode=0 then goto ASK_STARTING else goto AskDefaultLocality
L1040: read #1,using F_employee: eno,mat k$,ss$,em6 eof L2090
	if endnum>0 and eno>endnum then goto L2090 ! ending employee number entered
	gosub EXTRACT_NAME
	kz$=lpad$(rtrm$(str$(eno)),8)
	px$=""
	mat desc$=(" ")
	mat amt=(0)
	mat miscded=(0)
	tdedret=0 ! REMOVE EXPLANATION  FROM LINE 905 TO LIST RETIREMENT IN BOX 13
F_employee: form pos 1,n 8,3*c 30,c 11,pos 122,n 2
	if numb=0 then goto L1160
	if eno<empno then goto L1040
L1160: first=1
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey L1040
L1190: read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L1650
	if heno<>eno then goto L1650
	if prd<beg_date or prd>end_date then goto L1190 ! not this year
	read #2,using "form pos 48,n 2", key=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zzz)",tdn): tcd nokey L1230 ! get state code
L1230: if tcd<1 or tcd>10 then tcd=1
	if w1=3 then stcode=tcd : goto L1270
	if first=1 then stcode=tcd
	if first=0 and stcode><tcd then goto L1290
L1270: state$=d$(tcd)(1:2)
	stcode$=e$(tcd)
L1290: dedfica=0
	dedret=0
	for j=1 to 20
		if newdedfed(j)>=1 and newdedcode(j)=1 then dedret=dedret+tcp(j+4)
		if dedfica(j)=1 and newdedcode(j)=1 then dedfica=dedfica+tcp(j+4)
		miscded(j)=miscded(j)+tcp(j+4)
	next j
! tDEDRET=TDEDRET+DEDRET ! ACCUMULATE BOX 13 RETIREMENT; THIS LINE WILL ONLY WORK IF NO CAFETERIA; REMOVE ! OFF 861 AND 882 FOR RETIREMENT ONLY ! can change 882 only if know specific ded to show in box 13
	w(1)=w(1)+tcp(1) ! FED W/H YTD
	w(2)=w(2)+ tcp(31)-dedret ! TOTAL TAXABLE WAGES
	w3=w3+tcp(2) ! FICA W/H YTD
	w(4)=w(4)+tcp(24) ! EIC TOTAL
	if em6=9 then goto L1460 else w(5)=w(5)+tcp(31)-tcp(30)-dedfica ! TOTAL SS WAGES
	w(11)=w(11)+tcp(31)-dedfica ! TOTAL MC WAGES & TIPS
	if em6=2 then w(5)=0 ! NO SS
	if em6=1 then w(11)=0 ! NO MC
L1460: !
	if uprc$(med$)="Y" then w(3)=w(3)+tcp(2) ! ss wh and medicare seperated in employee record
	if uprc$(med$)="Y" then w(12)=w(12)+tcp(3) ! medicare
	if tcd><stcode then goto L1580
	w(7)=w(7)+tcp(4) ! STATE WH
	w(9)=w(9)+tcp(31)-dedret ! STATE WAGES
	if loccode=0 or tcp(loccode+4)=0 then goto L1550
	w(8)=w(8)+tcp(loccode+4) ! LOCAL WITHHOLDING
	w(10)=w(10)+tcp(31)-dedret ! LOCAL WAGES
L1550: if pn1>0 and tcp(pn1+4)>0 then px$="X"
	if dc1>0 and dc1<11 then dcb=dcb+tcp(dc1+4)
	goto L1620
L1580: if loccode=0 then lowh=0 else lowh=tcp(loccode+4)
	write #3,using L1600: eno,tcd,tcp(31)-dedret,tcp(3),lowh,f$
L1600: form pos 1,n 8,n 2,3*pd 5.2,c 8
	goproc=1
L1620: first=0
	goto L1190 ! read next check record
L1650: if box16=1 then gosub L4120
	if tdedret=0 then goto L1720
	for j=3 to 6
		if ltrm$(rtrm$(desc$(j)))="" then goto L1690 else goto L1710
L1690: desc$(j)=lpad$("G "&cnvrt$("Nz 10.2",tdedret),15)
		goto L1720
L1710: next j
L1720: for j=1 to 20
		if uprc$(dedyn$(j))<>"Y" or miscded(j)=0 then goto L1790
		for j2=3 to 6
			if ltrm$(rtrm$(desc$(j2)))="" then goto L1760 else goto L1780
L1760: desc$(j2)=lpad$(dedcode$(j)&" "&cnvrt$("Nz 10.2",miscded(j)),15): totalbox12(j)=totalbox12(j)+miscded(j)
			goto L1790
L1780: next j2
L1790: next j
	w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
	w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
	if uprc$(med$)="N" then w(3)=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH ! change to seperate medicare
	if uprc$(med$)="N" then w(12)=w3-w(3) ! MEDICARE WITHHELD  ! change to seperate medicare
	if uprc$(med$)="N" and w(5)>w(11)-1 and w(5)<w(11)+1 then w(5)=w(11) ! set ss wages and medicare wages equal in within 1.00
	if uprc$(med$)="N" and em6=1 then w(12)=0 : w(3)=w3 ! NO MC ALL SS ! change to seperate medicare
	if uprc$(med$)="N" and em6=2 then w(3)=0 : w(12)=w3 ! NO SS ALL MC ! change to seperate medicare
	if em6=9 then w(3)=w(5)=w(11)=w(12)=0 ! NO SS OR MC
	if w(8)=0 then pf$="" : goto L1920
	if z$="YES" then gosub ASK_LOCALITY
	pf$=f$
L1920: g$=str$(eno)
	if w(2)=0 and w(5)=0 then goto L2060 ! skip w2 if no wages
	gosub PRINTW2
	mat s=s+w
	wctr=wctr+1
	! if w1=3 then goto L2060
goto L2060 ! IF WCTR<41 THEN GOTO 1310
! 	desc$(3)=lpad$("  "&cnvrt$("Nz 10.2",s(13)),15)
! 	mat w=s
! 	g$="SUB TOTAL"
! 	gosub TOT1
! 	nosub=1
! 	desc$(3)=""
! 	wctr=0
! 	mat t=t+s
! 	mat s=(0)
L2060: !
	mat w=(0)
	nqp=dcb=w3=0
goto L1040
L2090: !
	if w1=3 then goto L2230
goto ASK_STARTING
 
	if wctr=0 or nosub=0 then goto L2140
	desc$(3)=lpad$("  "&cnvrt$("Nz 10.2",s(13)),15)
L2130: mat w=s: g$="SUB TOTAL" : gosub TOT1
L2140: mat t=t+s
	misc=3
	for j=1 to 10
		if totalbox12(j)=0 then goto L2210
		desc$(misc)=lpad$("  "&cnvrt$("Nz 10.2", totalbox12(j)),15)
		misc=misc+1
		if misc>7 then goto L2220 ! only allow 4 different deductions
L2210: next j
L2220: !
	mat w=t
	g$="Final Total"
	first$=mid$=last$=""
	gosub TOT1
L2230: !
	close #1:
	close #2:
	close #3:
	if w1=3 then close #5: : goto Xit
	L2270: !
	fnpa_finis
	if goproc=1 then goto PRW2B
goto Xit
 
AskDefaultLocality: ! r:
	fnTos
	rc=cf=0: mylen=20: mypos=mylen+3
	fnLbl(1,1,"Locality Name:",mylen,1,0,0)
	fnTxt(1,mypos,12,0,1,"",0,"If you have answered that you have local withholdings in the company information file, you must enter the locality name")
	resp$(rc+=1)=z$
	fnLbl(3,5,"(Enter the locality name if the same on all employees.",60,0,0,0)
	fnLbl(4,5,"Leave blank if not applicable.  Enter YES if applicable",60,0,0,0)
	fnLbl(5,5,"but not he same on all employees.)",60,0,0,0)
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	z$=resp$(1)
	z$=uprc$(rtrm$(z$))
	if rtrm$(z$)="" then z$="NO"
	if z$="YES" or z$="NO" then goto ASK_STARTING
	f$=z$
	if z$="" then goto AskDefaultLocality
goto ASK_STARTING ! /r
ASK_STARTING: ! r:
	fnTos
	respc=cf=0: mylen=40: mypos=mylen+3 : mylen2=62: mypos2=20
	fnLbl(1,1,"Starting Employee Number:",mylen,1,0,0)
	fncmbemp(1,mypos)
	resp$(respc+=1)=""
	fnLbl(2,1,"Ending Employee Number (blank for all):",mylen,1,0,0)
	fncmbemp(2,mypos)
	resp$(respc+=1)=""
	fnLbl(4,mypos2,"To pr a single W2, use the same starting and ending number.",mylen2,0,0,0)
	fnLbl(9,mypos2," ",mylen2,0,0,0)
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Complete",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then totalcode=1 : goto L2130
	restore #1:
	numb=val(resp$(1)(1:8))
	endnum=val(resp$(2)(1:8))
	if numb=0 then goto L1040
	empno=numb
goto L1040 ! /r
 
ASK_LOCALITY: ! r:
	fnTos
	rc=cf=0: mylen=30: mypos=mylen+3
	fnLbl(1,1,k$(1),mylen,1,0,0)
	fnLbl(2,1,"Locality Name:",mylen,1,0,0)
	fnTxt(2,mypos,12,0,1,"",0,"Enter the Locality for this employee.",0)
	resp$(rc+=1)=f$
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	f$=resp$(1)
	g$=rtrm$(g$)
	if g$="1" then goto L2790
	f$=g$
	L2790: !
return ! /r
 
PRW2B: ! r:
	open #1: "Name=[Temp]\Control."&session$,internal,output
	restore #1:
	L2830: form pos 1,c 128
	write #1,using L2830: "FILE [Temp]\Addr.[Session],,,PRW2ADDR.h[cno],[Q]\PRmstr,,[Q]\PRmstr,,A,N"
	write #1,using L2830: "MASK 9,2,n,a,1,8,n,a"
	close #1:
		fnFree("[Q]\PRmstr\PRW2ADDR.h[cno]")
	execute "Sort [Temp]\Control.[Session] -n"
goto Xit ! /r
Xit: fnXit
 
TOT1: ! r:
	mat k$=(""): ss$=stcode$=state$=pf$="": eno=0: k$(1)="Total Sheet"
	x$=" ": p1=58: p2=126
goto PRINTW2 ! /r
PRINTW2: ! r: pr W2 form
	gosub ASK_OLD_INFO
	column1=15
	column2=60
	column3=113
	column4=160
	inc=0
	pr #20: 'Call Print.MyFontSize(10)'
	lyne+=4.5: pr #20: 'Call Print.AddText("'&ss$&'",'&str$(column4)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&str$(taxyear)&'",'&str$(column3)&','&str$(lyne)&')'
	lyne=lyne+4.5: pr #20: 'Call Print.AddText("'&a$(1)&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=a$(2)
	lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=a$(3)
	lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	if trim$(oldss$)<>trim$(ss$) then goto L3090 else goto L3110
	L3090: !
	txt$=oldss$
	lyne+=3: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	L3110: !
	txt$=b$
	lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$= trim$(first$)&" "&trim$(mid$)(1:1)
	lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=last$
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=k$(2)
	lyne=lyne+5.3: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=k$(3)
	lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(2))
	lyne=lyne+17: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(2))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(1))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(1))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(5))
	lyne+=8: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(5))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(3))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(3))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(11))
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(11))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(12))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(12))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(6))
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(6))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(4))
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(4))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",dcb)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",olddcb)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
	txt$=desc$(1)
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=box1a$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box1)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
	txt$=desc$(3)
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=box2b$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box2)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
	txt$=px$
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(posx)&','&str$(lyne)&')'
	if posx<51 then goto L3700
	txt$=px$
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(posx-50)&','&str$(lyne)&')'
	L3700: !
	txt$=desc$(4)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=box3c$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box3)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
	txt$=desc$(5)
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
	txt$=box4d$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box4)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
	! tXT$=DESC$(6)
	! lYNE=LYNE+8.5: pr #20: 'Call Print.AddText("'&TXT$&'",'&STR$(COLUMN4)&','&STR$(LYNE)&')'
	txt$=state$
	lyne=lyne+21: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=state$
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=stcode$
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=stcode$
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(9))
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(9))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(7))
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(7))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(10))
	lyne+=17: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(10))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",w(8))
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(8))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	txt$=pf$(1:6)
	lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
	txt$=pf$(1:6)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
	fnpa_newpage : lyne=topmargin: count=0
	goto L4110
L4100: x$=""
L4110: !
return ! /r
L4120: ! r:
	read #14,using fm4$,key=kz$: kz$,mat in4$ nokey L4290
	for j=1 to 6
		amt(j)=val(in4$(j*5-3))
		if in4$(j*5-2)="1" then w(2)+=amt(j)
		if in4$(j*5-1)="1" then w(5)+=amt(j)
		if in4$(j*5-1)="1" then w(11)+=amt(j)
		if in4$(j*5-0)="1" then w(9)+=amt(j)
		if in4$(j*5-2)="2" then w(2)-=amt(j)
		if in4$(j*5-1)="2" then w(5)-=amt(j)
		if in4$(j*5-1)="2" then w(11)-=amt(j)
		if in4$(j*5-0)="2" then w(9)-=amt(j)
		if j>1 then desc$(j)=lpad$(in4$(j*5-4)(1:1)&"  "&cnvrt$("Nz 10.2",amt(j)),15)
		if j=1 then desc$(j)=lpad$(in4$(j*5-4)(1:1)&"  "&ltrm$(cnvrt$("Nz 10.2",amt(j))),15)
	! If (J=3 OR J=4) AND (IN4$(J*5-4)(1:1)="D" OR IN4$(J*5-4)(1:1)="E" OR IN4$(J*5-4)(1:1)="F" OR IN4$(J*5-4)(1:1)="H") Then w(13)=W(13)+AMT(J) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
	next j
	L4290: !
return ! /r
L4430: ! r:
	dim fl$*40
	! pr NEWPAGE
	close #101: ioerr ignore
	open #101: "SROW=2,SCOL=2,EROW=07,ECOL=35,BORDER=DR,CAPTION=SELECT LASER W2 SOFTWARE",display,outIn
	pr f "3,5,C 28": "1 = ADVANCED MICRO SOLUTIONS"
	pr f "4,5,C 28": "2 = CENTER PIECE SOFTWARE"
	pr f "6,5,C 25,R,N": " ENTER YOUR SELECTION #: "
	pr f "8,8,C 16,R,N": "PRESS F5 TO STOP"
	L4510: !
	input fields "6,30,N 1,UET,N": sw1 conv L4510
	if cmdkey=5 then goto Xit
on sw1 goto L4540,L4550 none L4510 ! /r
L4540: fl$="\1099ETC.W04\W2DATA\W2DAT.PRN" : goto L4560
L4550: fl$="\CPS04\ASCIIW2.TXT" : goto L4560
L4560: ! r:
	pr #101: newpage
	pr f "3,5,C 30,R,N": "ENTER OUTPUT PATH & FILE NAME"
	L4580: !
	rinput fields "5,5,C 30,UT,N": fl$
	if cmdkey=5 then goto Xit
	on sw1 goto L4620,L4640
	fl$=rtrm$(fl$)
	L4620: !
	open #5: "Name="&fl$&",REPLACE",d,o ioerr L4580
	goto L4650
	L4640: !
	open #5: "Name="&fl$&",RecL=470,REPLACE",d,o ioerr L4580
	L4650: !
	close #101:
return ! /r
W2LASER: ! GENERATE FILE FOR LAZER W2
	on sw1 goto L4690,L5400
L4690: ! r: LASER W2 FOR ADVANCED MICRO SOLUTIONS
	p1=0
	for j=1 to 11
		p1=pos(ss$," ",1)
		if p1>0 then ss$(p1:p1)=""
	next j
	pr #5: "ROAN=";g$
	pr #5: "FEIN=";b$
	pr #5: "WAGES=";w(2)
	pr #5: "FITW=";w(1)
	pr #5: "PNAME1=";a$(1)
	pr #5: "PNAME2=";" "
	pr #5: "SSWAGES=";w(5)
	pr #5: "SSWH=";w(3)
	pr #5: "PADDR1=";a$(2)
	pr #5: "PADDR2=";a$(3)
	pr #5: "MCWAGES=";w(11)
	pr #5: "MCWH=";w(12)
	pr #5: "SSN=";ss$
	pr #5: "SSTIPS=";w(6)
	pr #5: "ALLOCATIP=";0
	pr #5: "RNAME1=";(rtrm$(last$)&","&first$)(1:24)
	pr #5: "RNAME2=";k$(2)(1:24)
	pr #5: "AEIC=";w(4)
	pr #5: "DEPDCARE=";dcb
	pr #5: "RADDR1=";" "
	pr #5: "RADDR2=";k$(3)(1:24)
	pr #5: "LAB14A=";" "
	pr #5: "BOX14A=";0
	pr #5: "LAB12A=";desc$(3)(1:4)
	pr #5: "BOX12A=";desc$(3)(5:15)
	pr #5: "CNTRYCODE=";" "
	pr #5: "RCOUNTRY=";" "
	pr #5: "LAB14B=";" "
	pr #5: "BOX14B=";0
	pr #5: "LAB12B=";desc$(4)(1:4)
	pr #5: "BOX12B=";desc$(4)(5:15)
	pr #5: "LAB14C=";" "
	pr #5: "BOX14C=";0
	pr #5: "LAB12C=";desc$(5)(1:4)
	pr #5: "BOX12C=";desc$(5)(5:15)
	pr #5: "EESTAT=";"0"
	pr #5: "EERETR=";px$
	pr #5: "LAB14D=";" "
	pr #5: "BOX14D=";0
	pr #5: "LAB12D=";desc$(6)(1:4)
	pr #5: "BOX12D=";desc$(6)(5:15)
	pr #5: "EESICK=";0
	pr #5: "BOX11Q=";nqp
	pr #5: "NQPLANS=";" "
	pr #5: "STATE1=";state$
	pr #5: "SEIN1=";stcode$
	pr #5: "SWAGES1=";w(9)
	pr #5: "SITW1=";w(7)
	pr #5: "LWAGES1=";w(10)
	pr #5: "LITW1=";w(8)
	pr #5: "LOCAL1=";pf$
	pr #5: "STATE2=";" "
	pr #5: "SEIN2=";" "
	pr #5: "SWAGES2=";0
	pr #5: "SITW2=";0
	pr #5: "LWAGES2=";0
	pr #5: "LITW2=";0
	pr #5: "LOCAL2=";" "
	pr #5: "FName=";first$(1:24)
	pr #5: "LName=";last$(1:24)
	pr #5: "TAG=";" "
	pr #5: "EBAT=";" "
	pr #5: "PHONE=";" "
	pr #5: "*"
goto L4100 ! /r
L5400: ! r: LAZER W2 FOR CENTER PIECE SOFTWARE
	p1=pos(k$(1)," ",1)
	if p1=0 then p1=len(k$(1))+1
	q1$='","'
	p3=pos(k$(3),",",1) : a3=2
	if p3=0 then p3=pos(k$(3)," ",1): a3=1
	if p3=0 then p3=len(k$(3))+1: a3=1
	n1$=k$(1)(1:p1-1)
	n2$=k$(1)(p1+1:p1+16)
	c1$=k$(3)(1:p3-1): c2$=k$(3)(p3+a3:p3+a3+1): c3$=k$(3)(p3+a3+3:p3+a3+12)
	pr #5,using L5510: n1$,n2$,k$(2),"",c1$,c2$,c3$,"",ss$,f$,state$,0,w(4),w(1),w(2),w(3),w(5),w(6),w(11),w(12),nqp,dcb,0,amt(1),"",amt(2),"",amt(3),"",0,"",0,0,0,0,0,w(7),w(9),w(8),w(10),0,0,0,0,"",""
	L5510: form pos 1,c 13,c 16,2*c 30,c 15,c 2,2*c 10,c 11,c 15,c 2,13*n 10.2,c 1,n 10.2,c 1,n 10.2,c 1,n 10.2,c 1,13*n 10.2,c 2,c 15
	c$=','
goto L4100 ! /r
 
ASK_DEDUCTIONS: ! r: ask if any misecllaneous deductions should pr in box 12
	fnTos
	rc=cf=0: mylen=20: mypos=mylen+3
	fnLbl(1,1,"Indicate if any of the 20 miscellaneous deductions",50,1,0,0)
	fnLbl(2,1,"should appear in any boxes on the W-2.",44,1,0,0)
	fnLbl(4,7," Deduction Name    Yes     Box #     Code",40,0,0,0)
	for j=1 to 20
		fnLbl(j+4,1,fullname$(j),mylen,1,0,0)
		fnChk(j+4,26,"",0,0)
		resp$(rc+=1)='False'
		fnTxt(j+4,35,2,0,1,'30',0,"Enter the box number on the W-2 where this deduction should print.",0)
		resp$(rc+=1)=str$(box12(j))
		fnTxt(j+4,45,2,0,1,"",0,"Enter the Code that should appear in the box.",0)
		resp$(rc+=1)=dedcode$(j)
	next j
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	x=0
	for j=1 to 20
		x+=1: if resp$(x)='True' then dedyn$(j)="Y"
		x+=1: box12(j)=val(resp$(x))
		x+=1: dedcode$(j)=resp$(x)
	next j
return ! /r
EXTRACT_NAME: ! r:
	dim first$*15,mid$*15,last$*20,k$(3)*30,k$(3)*30
	k$(1)=uprc$(rtrm$(k$(1))): ! nAMCDE$="s"
	x1=pos(k$(1)," ",1)
	x2=pos(k$(1)," ",x1+1)
	x3=pos(k$(1)," ",x2+1)
	if uprc$(namcde$)="L" then goto L5870
	first$=k$(1)(1:max(min(15,x1-1),1))
	if x2>0 then mid$=k$(1)(x1+1:x2-1): last$=k$(1)(x2+1:len(k$(1)))
	if x2=0 then last$=k$(1)(x1+1:len(k$(1))): mid$=""
goto L5910 ! /r
L5870: ! r: last name first
	if x1>0 and k$(1)(x1-1:x1-1)="," then last$=k$(1)(1:x1-2) else last$=k$(1)(1:max(x1-1,1))
	if x2>0 then first$=k$(1)(x1+1:x2-1): mid$=k$(1)(x2+1:len(k$(1)))
	if x2=0 then first$=k$(1)(x1+1:len(k$(1))): mid$=""
	L5910: ! pr FIRST$,MID$,LAST$
return ! /r
L5930: ! r: left or right stub
	pr newpage
	close #101: ioerr ignore
	open #101: "SROW=4,SCOL=14,EROW=6,ECOL=60,BORDER=DR,CAPTION=<"&env$('program_caption')&" - Right or Left Stub",display,outIn
	pr f "5,20,C 40,N": '5 1/2" stub on Left or Right (L/R):'
	pr f "7,28,C 9,B,1": "Next (F1)"
	pr f "7,39,C 11,B,5": "Cancel (F5)"
	input fields "5,56,Cu 1,UT,N": left$
	if cmdkey=5 then goto Xit
return ! /r
VBOPENPRINT: ! r:
	if file(20)=-1 then
		fnpa_open ! open #20: "Name=[Q]\PRmstr\W2"&wsid$&".txt,Replace,RecL=5000",d,o
		lyne=topmargin ! starting of 1st line
		character=1.5
	end if
return ! /r
ASK_OLD_INFO: ! r:
	fnTos
	lc=rc=0 : mylen=20 : mypos=mylen+3
	if totalcode=0 then heading$="Enter Information from Incorrect W-2" else heading$="Total Screen for Corrected W-2s"
	fnLbl(lc+=1,1,heading$,40,0)
	fnLbl(lc+=2,1,"Employee "&str$(eno),mylen,1)
	fnLbl(lc+=1,1,"SS #:",mylen,1)
	fnTxt(lc,mypos,11,0,0,"")
	resp$(rc+=1)=ss$
	fnLbl(lc+=1,1,"Total Wage:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(2))
	fnLbl(lc+=1,1,"SS Wages:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(5))
	fnLbl(lc+=1,1,"Medicare Wages:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(11))
	fnLbl(lc+=1,1,"State Wages:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(9))
	mypos+=37: fnLbl(lc=1,38,"Federal Wh:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(1))
	fnLbl(lc+=1,38,"SS Withholdings:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(3))
	fnLbl(lc+=1,38,"Medicare Wh:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(12))
	fnLbl(lc+=1,38,"State Wh:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(w(7))
	fnLbl(lc+=1,38,"Box 12a Code:",mylen,1)
	fnTxt(lc,mypos,2,0,0,"")
	resp$(rc+=1)=box1a$
	fnLbl(lc+=1,38,"Box 12a Amount:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(box1)
	fnLbl(lc+=1,38,"Box 12b Code:",mylen,1)
	fnTxt(lc,mypos,2,0,0,"")
	resp$(rc+=1)=box2a$
	fnLbl(lc+=1,38,"Box 12b Amount:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(box2)
	fnLbl(lc+=1,38,"Box 12c Code:",mylen,1)
	fnTxt(lc,mypos,2,0,0,"")
	resp$(rc+=1)=box3a$
	fnLbl(lc+=1,38,"Box 12c Amount:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(box3)
	fnLbl(lc+=1,38,"Box 12d Code:",mylen,1)
	fnTxt(lc,mypos,2,0,0,"")
	resp$(rc+=1)=box4a$
	fnLbl(lc+=1,38,"Box 12d Amount:",mylen,1)
	fnTxt(lc,mypos,10,0,0,'10')
	resp$(rc+=1)=str$(box4)
	fnCmdKey('&Next',1,1,0)
	fnCmdKey('&Cancel',5,0,1)
	ckey=fnAcs(mat resp$) ! old amounts
	if totalcode=1 then goto L2270
	if ckey=5 then goto ASK_STARTING
	oldss$=resp$(1) ! ss#
	oldw(2)=val(resp$(2))  ! total wage
	oldw(5)=val(resp$(3))  ! total ss wage
	oldw(11)=val(resp$(4)) ! total medicare wage
	oldw(9)=val(resp$(5))  ! total state wage
	oldw(1)=val(resp$(6))  ! federal wh
	oldw(3)=val(resp$(7))  ! ss wh
	oldw(12)=val(resp$(8)) ! medicare wh
	oldw(7)=val(resp$(9))  ! state  wh
	box1a$=resp$(10)       ! box 12a code
	box1=val(resp$(11))    ! box 12a amount
	box2b$=resp$(12)       ! box 12b code
	box2=val(resp$(13))    ! box 12b amount
	box3c$=resp$(14)       ! box 12c code
	box3=val(resp$(15))    ! box 12c amount
	box4d$=resp$(16)       ! box 12d code
	box4=val(resp$(17))    ! box 12d amount
return ! /r
include: ertn
