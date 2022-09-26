! Replace S:\acsPR\newQTRFedUC
! Quarterly Federal U/C Worksheet
 
	autoLibrary
	on error goto Ertn
 
	dim dedcode(20),calcode(20),dedfed(20),option1$(4)*20
	dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
	dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10)
	dim e$(10)*12,tpt(32),cap$*128,resp$(15)*30
	dim tcp(32),tdc(10)
	dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),qtr(32)
	dim ytdtotal(32),ss$*11,em$(3)*30,m$*20
 
	fnTop(program$,cap$="Quarterly Federal Unemployment Worksheet")
	fnopenprn
 
	fncreg_read('calculation date text',m$)
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i
	read #20,using L250: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
	ficamaxw=ficamaxw*10
	L250: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
	close #20:
 
! If FNPROCESS=1 Then Goto 230
MENU1: ! r:
	fnTos(sn$="prqtrfeduc")
	respc=0
	if val(date$(4:5))=1 then taxyear=val(date$(1:2))+2000-1 else taxyear =val(date$(1:2))+2000 ! current tax year (if processing in jan, assume last year)
	fnLbl(1,1,"Tax Year:",26,1)
	fnTxt(1,30,4,0,0,'30',0,"")
	resp$(respc+=1)=str$(taxyear)
	option1$(1)="March 31"
	option1$(2)="June 30"
	option1$(3)="September 30"
	option1$(4)="December 31"
	fnLbl(2,1,"Quarter Ending Date:",26,1)
	fnComboA("pr941-yr",2,30,mat option1$,"Enter the quarter ending date")
	if val(date$(4:5))=3 or val(date$(4:5))=4 or val(date$(4:5))=5 then resp$(respc+=1)=option1$(1) ! march filing
	if val(date$(4:5))=6 or val(date$(4:5))=7 or val(date$(4:5))=8 then resp$(respc+=1)=option1$(2) ! June  filing
	if val(date$(4:5))=9 or val(date$(4:5))=10 or val(date$(4:5))=11 then resp$(respc+=1)=option1$(3) ! September filing
	if val(date$(4:5))=12 or val(date$(4:5))=1 or val(date$(4:5))=2 then resp$(respc+=1)=option1$(4) ! December
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	taxyear=val(resp$(1)) ! tax year
	if taxyear<2000 then goto L510
	ending_date=taxyear*10000+1231 conv L510
 
	for j=1 to 4
		if resp$(2)=option1$(j) then qtr=j: m$=option1$(j): goto L550 ! quarter ending date
	next j
L550: !
if qtr=1 then begdate=taxyear*10000+0312: enddate=val(taxyear$)*10000+0318
if qtr=2 then begdate=taxyear*10000+0612: enddate=val(taxyear$)*10000+0618
if qtr=3 then begdate=taxyear*10000+0912: enddate=val(taxyear$)*10000+0918
if qtr=4 then begdate=taxyear*10000+1212: enddate=val(taxyear$)*10000+1218
 
on pageoflow goto PgOf
open #2: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
gosub HDR
open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",i,outIn,k
open #3: "Name=[Q]\PRmstr\Department.h[cno],Shr, KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",i,outIn,k
L650: !
	read #2,using L660: eno,mat em$,ss$,em5,em6 eof DONE
	L660: form pos 1,n 8,3*c 30,c 11,pos 120,2*n 2
	m1=m2=h2=h3=dedytdfeduc=dedqtrfeduc=0
	mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
	mat ytdtotal=(0)
	
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey ANALYZE_WAGES
	L730: !
	read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
	if heno<>eno then goto ANALYZE_WAGES
	if prd<beg_date or prd>end_date then goto L730 ! not this year
	if em5=1 then pedate=begdate+19: box1+=1 ! monthly pay period
	if em5=2 then pedate=begdate+15 : box1+=1 ! semi-monthly
	if em5=3 then pedate=begdate+14 : box1+=1 ! bi-weekly
	if em5=4 then pedate=begdate+7: box1+=1 ! weekly
	! deptkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)
	if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp: mat tpt=tpt+tcp ! 1st qtr earnings
	if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp : mat tpt=tpt+tcp
	if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp : mat tpt=tpt+tcp
	if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp : mat tpt=tpt+tcp
	if prd>=qtr1 and prd<ending_date then mat ytdtotal=ytdtotal+tcp ! only total year to date wages to end of current quarter
goto L730 ! /r
ANALYZE_WAGES: ! r: analyze wages on each person
	if qtr=1 then mat qtr=qtr1tcp
	if qtr=2 then mat qtr=qtr2tcp
	if qtr=3 then mat qtr=qtr3tcp
	if qtr=4 then mat qtr=qtr4tcp
	for j=1 to 20
		if deduc(j)=1 and dedcode(j)=1 then dedytdfeduc+=ytdtotal(j+4) : dedqtrfeduc+=qtr(j+4) ! TOTAL DEDUCTIONS FOR federal u/c FOR QUARTER
	next j
	m2=m2+ytdtotal(31)-dedytdfeduc ! TOTAL WAGES less deductions FOR THIS EMPLOYEE FOR YEAR
	m1=m1+qtr(31)-dedqtrfeduc ! TOTAL WAGES less deductions FOR QURATER
	if m2=0 then goto L650
	gosub L1360
goto L650 ! /r
 
HDR: ! r:
	p2=p2+1
	pr #255,using L1060: "Page ",p2
	L1060: form pos 70,c 5,pic(zzz),skip 1
	pr #255:
	pr #255,using L1090: cap$
	L1090: form pos 20,c 40,skip 1
	pr #255,using L1110: "For quarter ended "&m$
	L1110: form pos 20,cc 40,skip 1
	pr #255:
	pr #255,using L1140: "     Rate",a$(1),"Fed ID",b$(1)
	L1140: form pos 1,c 9,pos 17,c 40,pos 59,c 6,pos 69,c 40,skip 1
	pr #255,using L1160: feducrat,a$(2)
	L1160: form pos 3,pic(zzzz.##),pos 17,c 40,skip 1
	pr #255,using L1180: a$(3)
	L1180: form pos 17,c 40,skip 1
	pr #255:
	pr #255: tab(44);"Total Wages   Excess Wages    Taxable"
	pr #255: " SS Number             Name";
	pr #255,using L1230: "For Quarter   Over $",feducmax,"Wages"
	L1230: form pos 44,c 20,pic(zzzzz.##),pos 75,c 5,skip 1
	pr #255: "___________  __________________________";
	pr #255: tab(44);"___________   ____________    _______"
return ! /r
DONE: ! r:
	eofcode=1
	gosub PAGETOTALS
	close #2: ioerr ignore
	close #3: ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnXit
L1360: ! r:
	if m1=0 then goto L1510 ! skip if quarterly wage=0
	p3=p3+1
	if m2<feducmax then goto L1440
	if m2-m1>feducmax then goto L1420
	h2=feducmax-(m2-m1)
	goto L1450
	L1420: !
	h2=0
	goto L1450
	L1440: !
	h2=m1
	L1450: !
	h3=m1-h2
	pr #255,using L1470: ss$,em$(1)(1:28),m1,h3,h2
	L1470: form pos 1,c 11,pos 14,c 28,pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##),skip 1
	t1+=m1 : t2+=h3 : t3+=h2
	pr #255: pageoflow PgOf
	p1=p1+2
	L1510: !
return ! /r
PAGETOTALS: ! r:
	pr #255,using L1550: "___________    ___________  _________" pageoflow L1640
	L1550: form pos 44,c 37,skip 1
	pr #255: "Employees on this page:";p3;"    Page Totals";
	pr #255,using L1580: t1,t2,t3
	L1580: form pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##),skip 1
	if nw=1 and eofcode=1 then goto L1610
	pr #255: newpage
	L1610: !
	p3=t1=t2=t3=0
return ! /r
L1640: pr #255: newpage
PgOf: ! r:
	gosub PAGETOTALS
	pr #255: newpage
	gosub HDR
continue ! /r
L510: ! r:
	mat ml$(2)
	ml$(1)="You must enter a valid tax year such as 2007."
	ml$(2)="Take OK to enter the year."
	fnMsgBox(mat ml$,resp$,cap$,0)
goto MENU1 ! /r
include: ertn
