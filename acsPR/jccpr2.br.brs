! Replace S:\acsPR\JCCPR2

	library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn
	on error goto ERTN

	dim em$(3)*30,ss$*11,jn$*6,tr(9),en$*8,tdet(3),n$*40,tcp(22)
	dim tded(6),tdc(5),dedcode(10),a2$*70,cap$*128,message$*40
	dim jn1$*6,tr1(9),en1$*8,pl1$(6)*30,pl2$(6)*6,dr(7),hr1(8),hr2(8),ded(6)
	fntop(program$,"Certified Payroll Register")

	pr newpage
	message$="Printing: please wait..."
	fnwait(message$,1)
	fnopenprn(cp,58,220,process)
	if file$(255)(1:3)<>"PRN" then jbskip=1

	pl2$(1)="FICA"
	pl2$(2)="Fed"
	pl2$(3)="State"
	pl2$(4)="Union"
	pl2$(5)="Other"

	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input,relative  
	read #1,using 'Form POS 618,10*N 1,POS 758,N 2',rec=1: mat dedcode,un 
	close #1: 

	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed 
	open #2: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
	open #3: "Name=Work."&session$,internal,input,relative 
	open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input 
	open #5: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,input,relative 
	read #3,using L350,rec=1: df,dt,mat dr
L350: form pos 1,2*n 6,7*pd 3
L360: read #4,using L370: r4 eof L1890
L370: form pos 1,pd 3
	if r4=1 then goto L360
	read #3,using L400,rec=r4: en1$,jn1$,mat tr1
L400: form pos 5,c 8,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
	if jn1$><jn$ then goto L580
	if en1$><en$ then goto L700
	if tr1(3)><tr(3) then 
		gosub L1390
		goto L800
	end if
		
		
	end if
L440: for j=1 to 7
		if int(tr1(4)*.01)=dr(j) then goto L490
	next j
	goto L360

L490: hr1(j)=hr1(j)+tr1(5)
	hr1(8)=hr1(8)+tr1(5)
	hr2(j)=hr2(j)+tr1(6)
	hr2(8)=hr2(8)+tr1(6)
	gp1=gp1+tr1(5)*tdet2
	gp2=gp2+tr1(6)*tdet3
	tr(3)=tr1(3)
	goto L360

L580: if rtrm$(jn$)="" then goto L620
	gosub L1550
	gosub TOTALS
	pr #255: newpage
L620: en$=en1$
	jn$=jn1$
	n$=""
	read #2,using L660,key=jn$: n$ nokey L670
L660: form pos 7,c 40
L670: if end4=0 then gosub L1240
	goto L780

L700: if val(en$)=0 then goto MOVEINFO
	gosub L1550
	en$=en1$
	goto L780


L780: read #1,using L790,key=en$: mat em$,ss$,em2,lpd,tgp,ta1 nokey L1060
L790: form pos 9,3*c 30,c 11,x 4,n 2,pos 162,n 6,pd 5.2,pd 3
L800: adr=ta1
	mat ded=(0)
	tdet2=0
	tgp=tdet3=0
L840: if adr=0 then goto MOVEINFO
	read #5,using L860,rec=adr: tdn,tdt4,mat tdet,mat tdc,mat tcp,adr
L860: form pos 9,n 3,pos 42,n 6,pos 58,3*pd 4.2,pos 150,5*pd 3.2,pos 358,22*pd 5.2,pd 3
	if tdn><tr1(3) then goto L900
	tdet2=tdet(2)
	tdet3=tdet(3)
L900: if lpd><tdt4 then goto L1040
	tgp=tgp+tcp(21)
	for j=1 to 5
		tcd1=tcd1+tdc(j)
	next j
	ded(1)=ded(1)+tcp(2)+tcp(15)
	ded(2)=ded(2)+tcp(1)
	ded(3)=ded(3)+tcp(3)
	if un>0 and un<11 then ded(4)=ded(4)+tcp(un+3)
	for j=1 to 10
		if j=un then goto L1020
		if dedcode(j)=2 then ded(5)=ded(5)-tcp(j+3) else ded(5)=ded(5)+tcp(j+3)
L1020: next j
	tcp22=tcp22+tcp(22)
L1040: goto L840

L1060: mat em$=("")
	ss$=""
	em2=0
	ta1=0
MOVEINFO: ! 
	pl1$(1)=em$(1)
	pl1$(2)=em$(2)
	pl1$(3)=em$(3)
	pl1$(4)=ss$
! pL1$(5)=LPAD$(STR$(TDN),6)
	pl1$(5)=lpad$(str$(em2),6)
	goto L440

PGOF: ! 
	pr #255: newpage
	gosub L1240
continue 

L1240: ! r:
	p1=59-int(len(rtrm$(env$('program_capiton'))))/2
	a2$="Job # "&ltrm$(jn$)&"  Job Name "&rtrm$(n$)
	p2=59-int(len(rtrm$(a2$)))/2
	pr #255,using L1280: env$('program_capiton'),a2$
	L1280: form skip 2,pos p1,c 70,skip 1,pos p2,c 70,skip 1
	pr #255: tab(40);"****  Certified Payroll Register  ****"
	pr #255,using L1310: "Period Ending",dt
	L1310: form pos 48,c 14,pic(zz/zz/zz),skip 1
	pr #255: "Name  &  Address"
	pr #255: "City, State Zip                <-------- Hours Worked this Job --------> Total  Pay  Pay  <--------------- Summary ---------------->"
	pr #255,using L1350: "    Fed-Exempt",mat dr,"Hours  Rate Typ      Gross    FICA Fed W/H   Other      Net"
	L1350: form pos 1,c 30,pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),x 1,c 59,skip jbskip
	pr #255: "______________________________ _____ _____ _____ _____ _____ _____ _____ _____  ____ ___    _______  ______  ______   _____  _______"
return ! /r

L1390: ! r:
	if tgp=0 then x3=0 else x3=(gp1+gp2)/tgp
	if hr1(8)=0 then goto L1450
	lnp=lnp+1
	if lnp>5 then lnp=6
	pr #255,using L1440: pl1$(lnp),mat hr1,tdet2," REG",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF
	L1440: form pos 1,c 30,9*n 6.2,c 6,n 9.2,3*n 8.2,n 9.2
	L1450: !
	if hr2(8)=0 then goto L1500
	lnp=lnp+1
	if lnp>5 then lnp=6
	if hr1(8)=0 then pr #255,using L1440: pl1$(lnp),mat hr2,tdet3," OVT",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF else pr #255,using L1490: pl1$(lnp),mat hr2,tdet3," OVT" pageoflow PGOF
	L1490: form pos 1,c 30,9*n 6.2,c 6
	L1500: !
	hr8=hr8+hr1(8)+hr2(8)
	mat hr1=(0)
	mat hr2=(0)
return ! /r

L1550: ! r:
	gosub L1390
	lnp=lnp+1
	if lnp>5 then goto L1620
	for j=lnp to 5
		pr #255,using L1600: pl1$(j) pageoflow PGOF
		L1600: form pos 1,c 30,skip 1
	next j
	L1620: !
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
	pr #255,using L1790: "* * * *  Totals for Job # ",jn$,"* * * *" pageoflow PGOF
	L1790: form pos 10,c 26,c 7,c 8,skip 1
	pr #255: tab(6);"Total Hours   Gross Pay     Total        Total" pageoflow PGOF
	pr #255: tab(31);"Deductions     Net-Pay" pageoflow PGOF
	pr #255,using L1830: thr,jgp,gded,jgp-gded pageoflow PGOF
	L1830: form pos 5,4*n 12.2,skip 2
	thr=0
	jgp=0
	gded=0
return ! /r

L1890: gosub L1550
	gosub TOTALS
	close #1: 
	close #5: 
	fncloseprn
goto XIT
XIT: fnxit
include: Ertn

