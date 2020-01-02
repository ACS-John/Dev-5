! Replace S:\acsPR\newJCCPR2

library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fnDedNames
on error goto Ertn

dim ss$*11
dim tr(9)
dim tded(6),tdc(5),dedcode(10)
dim tr1(9)
dim en1$*8,pl1$(6)*30,pl2$(6)*6,ded(6)
dim em$(3)*30,tdy(6),tdc(10),tcp(32),tdep(20,26),ttc(32)
dim tdet(17)
dim qtr1tcp(32)
dim qtr2tcp(32)
dim qtr3tcp(32)
dim qtr4tcp(32)
dim ytdtotal(32)
dim quartertotals(32)

fntop(program$,"Certified Payroll Register")

fnopenprn
if file$(255)(1:3)<>"PRN" then jbskip=1
dim dedcode(20)
open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input,relative  
read #1,using 'Form POS 618,10*N 1,POS 758,N 2',rec=1: mat dedcode,un 
close #1: 

dim fullname$(20)*20,abrevname$(20)*8,dedfed(20),calcode(20),dedfica(20),dedst(20),deduc(20),gl$(20)*12
fnDedNames(mat fullname$,mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
for j=1 to 20
	if trim$(fullname$)="Union" then un=j ! determine union deduction
next j

open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed 
open #2: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
open #3: "Name="&env$('temp')&"\Work."&session$,internal,input,relative 
open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input ioerr XIT
open #8: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed 
open #7: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
dim dr(7)
read #3,using L430,rec=1: df,dt,mat dr
L430: form pos 1,2*n 6,7*pd 3


Read4: !
	read #4,using L450: r4 eof Finis
	L450: form pos 1,pd 3
	if r4=1 then goto Read4
	dim jn1$*6,jn$*6,en$*8
	read #3,using L480,rec=r4: en1$,jn1$,mat tr1
	L480: form pos 5,c 8,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
	if jn1$><jn$ then goto L660
	if en1$><en$ then goto L780
	if tr1(3)><tr(3) then 
		gosub L1450
		goto L880
	end if
	L520: !
	for j=1 to 7
		if int(tr1(4)*.01)=dr(j) then goto L570
	next j
goto Read4

L570: ! r:
	dim hr2(8),hr1(8)
	hr1(j)=hr1(j)+tr1(5)
	hr1(8)=hr1(8)+tr1(5)
	hr2(j)=hr2(j)+tr1(6)
	hr2(8)=hr2(8)+tr1(6)
	gp1=gp1+tr1(5)*tdet2
	gp2=gp2+tr1(6)*tdet3
	tr(3)=tr1(3)
goto Read4 ! /r
L660: ! r:
	if rtrm$(jn$)="" then goto L700
	gosub L1610
	gosub TOTALS
	pr #255: newpage
	L700: !
	en$=en1$
	jn$=jn1$
	dim n$*40
	n$=""
	read #2,using L740,key=jn$: n$ nokey L750
	L740: form pos 7,c 40
	L750: !
	if end4=0 then gosub L1300
goto L860 ! /r
L780: ! r:
	if val(en$)=0 then goto MOVEINFO
	gosub L1610
	en$=en1$
goto L860 ! /r

L860: ! r:
	read #1,using L870,key=en$: mat em$,ss$,em2,lpd,tgp nokey L1120
	L870: form pos 9,3*c 30,c 11,x 4,n 2,pos 162,n 6,pd 5.2,pd 3
	L880: !
	mat ded=(0)
	tdet2=0
	tgp=tdet3=0
	gosub DETERMINE_EARNINGS
	tdet2=ttdc(1): tdet3=ttdc(2) ! regular and ot hours
	tgp=tgp+ttc(21)
	for j=1 to 5
		tcd1=tcd1+ttdc(j)
	next j
	ded(1)=ded(1)+ttc(2)+ttc(15)
	ded(2)=ded(2)+ttc(1)
	ded(3)=ded(3)+ttc(3)
	if un>0 and un<21 then ded(4)=ded(4)+ttc(un+4)
	for j=1 to 20
		if j=un then goto L1080
		if dedcode(j)=2 then ded(5)=ded(5)-ttc(j+3) else ded(5)=ded(5)+ttc(j+3)
		L1080: !
	next j
	tcp22=tcp22+ttc(22)
goto MOVEINFO ! /r
L1120: ! r: 
	mat em$=("")
	ss$=""
	em2=0
	ta1=0
goto MOVEINFO ! /r
MOVEINFO: ! r:
	pl1$(1)=em$(1)
	pl1$(2)=em$(2)
	pl1$(3)=em$(3)
	pl1$(4)=ss$
	pl1$(5)=lpad$(str$(em2),6) ! pL1$(5)=LPAD$(STR$(TDN),6)
goto L520 ! /r

PGOF: ! r:
	pr #255: newpage
	gosub L1300
continue ! /r
L1300: ! r:
	p1=59-int(len(rtrm$(env$('cnam'))))/2
	dim a2$*70
	a2$="Job # "&ltrm$(jn$)&"  Job Name "&rtrm$(n$)
	p2=59-int(len(rtrm$(a2$)))/2
	pr #255,using L1340: env$('cnam'),a2$
	L1340: form skip 2,pos p1,c 70,skip 1,pos p2,c 70,skip 1
	pr #255: tab(40);"****  Certified Payroll Register  ****"
	pr #255,using L1370: "Period Ending",dt
	L1370: form pos 48,c 14,pic(zz/zz/zz),skip 1
	pr #255: "Name  &  Address"
	pr #255: "City, State Zip                <-------- Hours Worked this Job --------> Total  Pay  Pay  <--------------- Summary ---------------->"
	pr #255,using L1410: "    Fed-Exempt",mat dr,"Hours  Rate Typ      Gross    FICA Fed W/H   Other      Net"
L1410: form pos 1,c 30,pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),x 1,c 59,skip jbskip
	pr #255: "______________________________ _____ _____ _____ _____ _____ _____ _____ _____  ____ ___    _______  ______  ______   _____  _______"
return ! /r
L1450: ! r:
	if tgp=0 then x3=0 else x3=(gp1+gp2)/tgp
	if hr1(8)=0 then goto L1510
	lnp=lnp+1
	if lnp>5 then lnp=6
	pr #255,using L1500: pl1$(lnp),mat hr1,tdet2," REG",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF
	L1500: form pos 1,c 30,9*n 6.2,c 6,n 9.2,3*n 8.2,n 9.2
	L1510: !
	if hr2(8)=0 then goto L1560
	lnp=lnp+1
	if lnp>5 then lnp=6
	if hr1(8)=0 then pr #255,using L1500: pl1$(lnp),mat hr2,tdet3," OVT",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF else pr #255,using L1550: pl1$(lnp),mat hr2,tdet3," OVT" pageoflow PGOF
	L1550: form pos 1,c 30,9*n 6.2,c 6
	L1560: !
	hr8=hr8+hr1(8)+hr2(8)
	mat hr1=(0)
	mat hr2=(0)
return ! /r
L1610: ! r:
	gosub L1450
	lnp=lnp+1
	if lnp>5 then goto L1680
	for j=lnp to 5
		pr #255,using L1660: pl1$(j) pageoflow PGOF
		L1660: form pos 1,c 30,skip 1
	next j
	L1680: !
	gded= gded+(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5)))
	jgp=jgp+gp1+gp2 ! TOTAL GROSS FOR JOB
	mat tded=tded+ded ! TOTAL DEDUCTION
	tcdt=tcdt+tcd1 ! TOTAL HOURS FOR ALL JOBS
	thr=thr+hr8 ! TOTAL HOURS FOR JOB
	tgp=0
	gp1=0
	gp2=0
	mat ded=(0)
	tcd1=0
	hr8=0
	tcp22=0
	lnp=0
return ! /r
TOTALS: ! r:
	pr #255,using L1850: "* * * *  Totals for Job # ",jn$,"* * * *" pageoflow PGOF
	L1850: form pos 10,c 26,c 7,c 8,skip 1
	pr #255: tab(6);"Total Hours   Gross Pay     Total        Total" pageoflow PGOF
	pr #255: tab(31);"Deductions     Net-Pay" pageoflow PGOF
	pr #255,using L1890: thr,jgp,gded,jgp-gded pageoflow PGOF
	L1890: form pos 5,4*n 12.2,skip 2
	thr=0
	jgp=0
	gded=0
return ! /r
Finis: ! r:
	gosub L1610
	gosub TOTALS
	fncloseprn
goto XIT ! /r
XIT: fnxit
DETERMINE_EARNINGS: ! r:
	tfd=tmd=td14=tdw=0: mat caf=(0): mat ttc=(0): mat ttdc=(0)
	mat tcp=(0): mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0) 
	mat qtr4tcp=(0): mat ytdtotal=(0): mat tdc=(0): mat tty=(0)
	fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0 
	medqtr=eicyr=eicqtr=wagesqtr=0
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #7,key>=checkkey$: nokey L2500
	L2180: !
	read #7,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,oldckno,mat tdc,mat tcp eof STORE_VARIABLES : lastrec=rec(3)
	if heno<>eno then goto STORE_VARIABLES
	if prd<beg_date or prd>end_date then goto L2180 ! not this year
	if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
	if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
	if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
	if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
	mat ytdtotal=ytdtotal+tcp
	mat tty=tty+tcp
	if prd=>df and prd<=dt then mat ttc=ttc+tcp: mat ttdc=ttdc+tdc ! total for this date range
	if prd=d1 then gosub ACCUMULATE_DEPT_TOTALS
goto L2180 ! /r
STORE_VARIABLES: ! r:
	wagesyr=ytdtotal(31) ! total wages
	fedyr=ytdtotal(1) ! ytdl fed
	ficayr=ytdtotal(2) ! fica year to date
	medyr=ytdtotal(3) ! medicare year to date
	stateyr=ytdtotal(4) ! total state  quarter
	eicyr=ytdtotal(25) ! eic
	if prd>=qtr1 and prd<qtr2 then mat quartertotals=qtr1tcp
	if prd>=qtr2 and prd<qtr3 then mat quartertotals=qtr2tcp
	if prd>=qtr3 and prd<qtr4 then mat quartertotals=qtr3tcp
	if prd>=qtr4 and prd<end_date then mat quartertotals=qtr4tcp
	wagesqtr=quartertotals(31) ! total wages quarter
	fedqtr=quartertotals(1) ! total fed  quarter
	ficaqtr=quartertotals(2) ! total fica quarter
	medqtr=quartertotals(3) ! total medicare quarter
	stateqtr=quartertotals(4) ! total state  quarter
	eicqtr=quartertotals(25) ! eic qtr
	for j=1 to 20
		if dedfed(j)=1 then dedfedyr+=ytdtotal(j+4) ! deduct for federal wh
	next j
L2500: !
return ! /r
ACCUMULATE_DEPT_TOTALS: ! r:
	! ACCUMULATE CURRENT INFO FROM EACH DEPARTMENT
	if tdep=0 then goto L2570
	for j2=1 to tdep
		if tdep(j2,5)=tdn then goto L2590
	next j2
	L2570: !
	tdep=tdep+1
	j2=tdep
	L2590: !
	tdep(j2,1)=tdep(j2,1)+tcp(31)-tcp(30) ! total wage less tips
	deptgl$="" 
	read #8,using "Form pos 12,c 12,pos 62,2*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",tdn): deptgl$,tdet(2),tdet(3) ! Nokey 1660
	tdep(j2,2)=val(deptgl$(1:3)) ! salary for this department
	tdep(j2,3)=val(deptgl$(4:9))
	tdep(j2,4)=val(deptgl$(10:12))
	tdep(j2,5)=tdn
	tdep(j2,6)=tdep(j2,6)+tcp(2)+tcp(3)
	for j3=1 to 20
		tdep(j2,j3+6)=tdep(j2,j3+6)+tcp(j3+4)
	next j3
	if s1><1 then goto L2720
	if rate=0 then rate=tdet(2)
	if rate>0 then rt$="PAY RATE"&cnvrt$("N 10.2",rate) else rt$=""
	L2720: !
	tpd3=tpd3+round(tdc(3)*tdet(2),2) ! sick pay
	tpd4=tpd4+round(tdc(4)*tdet(2),2) ! vacation pay
	tpd5=tpd5+round(tdc(5)*tdet(2),2)
	tdc1=ttdc(1) ! Regular Hours
	tdc2=ttdc(2) ! OverTime Hours
	tdc3=ttdc(3)
	tdc4=ttdc(4)
	tdc5=ttdc(5)
	ttdct=ttdc(1)+ttdc(2)+ttdc(3)+ttdc(4)+ttdc(5) ! Total Hours
return  ! /r
include: Ertn
