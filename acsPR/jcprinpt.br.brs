! Replace S:\acsPR\jcprInpt     !    I *think* this program is obsoluete
! Transfer Job Cost to Payroll

	library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox,fnopenprn,fncloseprn,fnchain
	on error goto ERTN
	on fkey 5 goto L2440

	dim inp(19),iolabel1$(21),io1$(23),io1b$(19),em$*30,label1$(21)*21
	dim en$*8,ta(2),tdet(13),chg$(2)*1,tinp(19),std$*27,f1$*204,wrd2$(4)*29
	dim f2$*204,pr(9,25),hen$*8,cap$*128,message$*40,hr(2),h(7)
	dim msgline$(2)*60,response$(5)*1

	fntop(program$,cap$="Transfer Job Cost to Payroll")
! 

	open #1: "Name=[Q]\PRmstr\RPNAMES.H[cno],Shr",internal,input 
	read #1,using L190: label1$(7),label1$(8),label1$(9),label1$(10),label1$(11),label1$(12),label1$(13),label1$(14),label1$(15),label1$(16),label1$(17)
L190: form pos 741,11*c 20
	close #1: 

	gosub SORTIT
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,outIn,keyed 
	open #2: "Name=[Q]\PRmstr\RPTRAIL.h[cno],Shr",internal,outIn,relative 
	open #3: "Name=[Q]\PRmstr\rpwork"&wsid$&".h[cno],SIZE=0,RecL=117,Replace",internal,output 
	open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input 
	open #5: "Name=[Q]\PRmstr\JCPRH1.H[cno]",internal,input,relative 
	open #6: "Name=ADDR2."&wsid$,internal,input 
	f1$="FORM POS 1,C 20"
	f2$=f1$
	for j=1 to 9
		f1$=rtrm$(f1$)&",N 12.2"
		f2$=rtrm$(f2$)&",N 12.2"
	next j
	for j=1 to 14
		iolabel1$(j)=str$(j+6)&",2,Cr 21,N"
		io1$(j)=str$(j+6)&",24,N 10.2,UT,N"
		io1b$(j)=str$(j+6)&",24,N 10.2,N"
	next j
	for j=15 to 21
		iolabel1$(j)=str$(j-8)&",36,Cr 21,N"
		io1$(j)=str$(j-8)&",58,N 10.2,UT,N"
		if j>19 then goto L450
		io1b$(j)=str$(j-8)&",58,N 10.2,N"
L450: next j
	io1$(22)="15,67,Cu 1,UT,N"
	io1$(23)="16,67,Cu 1,UT,N"
	label1$(01)="Regular Hours"
	label1$(02)="Overtime Hours"
	label1$(03)="Sick Hours"
	label1$(04)="Vacation Hours"
	label1$(05)="Holiday Hours"
	label1$(06)="Salary"
	label1$(07)="Other Compensation"
	label1$(18)="Meals"
	label1$(19)="Tips"
	label1$(20)="Regular Hourly Rate"
	label1$(21)="Overtime Hourly Rate"
	for j=1 to 21: label1$(j)=rtrm$(label1$(j))&":" : next j
L600: read #4,using L610: jci eof L1740
L610: form pos 1,pd 3
	read #5,using L630,rec=jci: mat h,dt2,jn$ noRec L600
L630: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
	if h(1)><eno or h3><h(3) then goto L840
L650: h2=h(2)
	if h2=1 then goto L690
	inp(1)=inp(1)+h(4)
	inp(2)=inp(2)+h(5)
L690: eno=h(1)
	h3=h(3)
	if h(7)=11 then goto L740
	if h(7)=0 then goto L600 else inp(h(7)+7)=inp(h(7)+7)+h(6)
	goto L600
L740: inp(7)=inp(7)+h(6)
	goto L600

L770: pr newpage
	win=101
	fnopenwin(win,10,20,14,59,cap$)
	pr #win,fields "4,2,C 16,N": "Employee Number:"
	pr f "15,35,c 09,B,5": "Done (F5)"
L820: input #win,fields "4,21,N 8,UT,N": eno conv L820
L830: if cmdkey=5 or eno=0 then goto TOTALSCREEN
L840: if eno=0 then goto L650
L850: en$=lpad$(str$(eno),8)
	read #1,using L870,key=en$: em$,em8,em9,tgp,mat ta nokey L650
L870: form pos 9,c 30,pos 126,2*pd 3.3,pos 168,pd 5.2,2*pd 3
	if s9=1 then goto L920
	if r><0 then goto L910
	if eno=eno2 then goto L920
L910: tgp=0
L920: if s9=0 then adr=ta(1)
L930: read #2,using L940,rec=adr: dep,mat tdet,nta
L940: form pos 9,n 3,pos 58,13*pd 4.2,pos 468,pd 3
	if s9=1 or r=1 then goto L970
	if dep><h3 then goto L1630 ! CHANGE TO "if dep><h3 then mat inp=(0)" TO ASK ALL DEPARTMENTS ON INPUT
L970: if s9=1 then goto L1010
	std$="Skip this Department (Y/N):"
	goto L1070

L1010: std$="Delete this Entry (Y/N):"
	tdet(2)=hr(1)
	tdet(3)=hr(2)
	tgp=tgp-gpd
	goto L1110

L1070: if h2=1 or h2=3 then inp(6)=tdet(1)
	for j=1 to 10
		inp(j+7)=inp(j+7)+tdet(j+3)
	next j
L1110: pr newpage
	win=101
	fnopenwin(win,2,7,22,74,cap$)
	pr #win,fields "04,02,Cr 21,N": "Employee Number:" 
	pr #win,fields "04,24,C 08,N": ltrm$(en$)
	pr #win,fields "05,02,Cr 21,N": "Employee Name:" 
	pr #win,fields "05,24,C 30,N": rtrm$(em$)
	pr #win,fields "06,02,Cr 21,N": "Department Number:" 
	pr #win,fields "06,24,C 03,N": str$(dep)
	pr #win,fields mat iolabel1$: mat label1$
	pr #win,fields "15,36,Cr 29,N": std$
	pr #win,fields "16,36,Cr 29,N": "Make Changes Permanent (Y/N):"
	pr #win,fields mat io1$: mat inp,tdet(2),tdet(3)
	pr f "23,22,C 09,B,1": "Next (F1)"
	pr f "23,32,C 25,B,5": "Cancel (no transfer) (F5)"
L1230: input #win,fields mat io1$: mat inp,mat hr,mat chg$ conv CONV1
	if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
	if cmdkey>0 then goto L1320 else ce=curfld
L1260: ce=ce+1: if ce>udim(io1$) then ce=1
L1270: io1$(ce)=rtrm$(io1$(ce)) 
	ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L1260
	ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1230
CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
	ce=cnt+1
ERR1: pr f "24,78,C 1": bell : goto L1270
L1320: if cmdkey=5 then goto XIT
	if chg$(1)="Y" and s9=1 then goto L2590
	if chg$(1)="Y" then goto L1630
	if chg$(2)="N" then goto L1430
	tdet(1)=inp(6)
	for j=1 to 10
		tdet(j+3)=inp(j+7)
	next j
	tdet(2)=hr(1)
	tdet(3)=hr(2)
	rewrite #2,using L940,rec=adr: dep,mat tdet,nta
	L1430: !
	gpd=0
	if em8><-2 then goto L1490
	if inp(3)=0 then goto L1490
	pr f "5,40,c 38": "Not Eligable for Sick Leave"
goto L1230

L1490: !
	if em9><-2 then goto L1540
	if inp(4)=0 then goto L1540
	pr f "6,40,c 38": "Not Eligible for Vacation"
goto L1230

L1540: for j=1 to 5
		if j=2 then gpd=gpd+inp(j)*hr(2) else gpd=gpd+inp(j)*hr(1)
	next j
	gpd=gpd+inp(6)+inp(7)+inp(18)+inp(19)
	if s9=1 then goto L2630
	write #3,using L1600: eno,dep,mat inp,gpd,mat hr,adr
L1600: form pos 1,n 8,n 3,5*pd 4.2,15*pd 5.2,2*pd 4.2,pd 3
	mat tinp=tinp+inp
	tgp=tgp+gpd
L1630: adr=nta
	if adr>0 then goto L930
	rewrite #1,using L1660,key=en$: tgp nokey L1700
L1660: form pos 168,pd 5.2
	if hen$<>en$ then ent1=ent1+1
	if hen$<>en$ then teno=teno+eno
	hen$=en$
L1700: mat inp=(0)
	eno2=eno
	if end4=1 and r=0 then goto TOTALSCREEN
	if r=0 then goto L650 else goto L770
L1740: end4=1
	goto L830

TOTALSCREEN: ! 
	pr newpage
	win=101
	fnopenwin(win,2,7,22,74,cap$)
	close #3: 
	open #3: "Name=[Q]\PRmstr\rpwork"&wsid$&".h[cno]",internal,outIn,relative 
	label1$(20)=" "
	label1$(21)=" "
	pr #win,fields "04,02,Cr 34,N": "Total or Employee Numbers Entered:" 
	pr #win,fields "04,37,C 10,N": str$(teno)
	pr #win,fields "05,02,Cr 34,N": "Number of Employee Entered:" 
	pr #win,fields "05,37,C 10,N": str$(ent1)
	pr #win,fields mat iolabel1$: mat label1$
	pr #win,fields mat io1b$: mat tinp
	wrd2$(1)="1. Make Corrections"
	wrd2$(2)="2. pr Proof Listing"
	wrd2$(3)="3. Calculate Pay"
	wrd2$(4)="4. pr Daily Employee Hours"
	for j=1 to udim(wrd2$)
		io2$(j)=str$(j+14)&",38,C 29,N"
	next j
	pr f "23,27,C 25,B,5": "Cancel (no transfer) (F5)"
L1970: rinput #win,select mat io2$,attr "H": mat wrd2$
	cor=curfld
	if cmdkey=5 then goto XIT
	label1$(20)="Reg Hourly Rate"
	label1$(21)="O/T Hourly Rate"
	on cor goto L2450,L2040,L2810,L3110 none L1970

L2040: r=0
	pc=0
	pr newpage
	fnwait(message$,1)
	fnopenprn
L2090: r=r+1
	read #3,using L1600,rec=r: eno,dep,mat inp,gpd,mat hr,adr eof L2430,noRec L2430
	if pc=9 then gosub L2240
	pc=pc+1
	pr(pc,1)=eno
	pr(pc,2)=dep
	pr(pc,22)=gpd
	pr(pc,23)=r
	pr(pc,24)=hr(1)
	pr(pc,25)=hr(2)
	for j=1 to 19
		pr(pc,j+2)=inp(j)
	next j
	goto L2090

L2240: pc2=pc2+1
	if pc2<3 then goto L2270 else pr #255: newpage
	pc2=1
L2270: pr #255,using L2280: " "
L2280: form c 1,skip 4
	pr #255,using f1$: "Record #     ",pr(1,23),pr(2,23),pr(3,23),pr(4,23),pr(5,23),pr(6,23),pr(7,23),pr(8,23),pr(9,23)
	pr #255,using f1$: "Employee #",pr(1,1),pr(2,1),pr(3,1),pr(4,1),pr(5,1),pr(6,1),pr(7,1),pr(8,1),pr(9,1)
	pr #255,using f1$: "Department #",pr(1,2),pr(2,2),pr(3,2),pr(4,2),pr(5,2),pr(6,2),pr(7,2),pr(8,2),pr(9,2)
	for j=1 to 19
		pr #255,using f2$: label1$(j),pr(1,j+2),pr(2,j+2),pr(3,j+2),pr(4,j+2),pr(5,j+2),pr(6,j+2),pr(7,j+2),pr(8,j+2),pr(9,j+2)
	next j
	pr #255,using f2$: "Dept Gross Pay ",pr(1,22),pr(2,22),pr(3,22),pr(4,22),pr(5,22),pr(6,22),pr(7,22),pr(8,22),pr(9,22)
	pr #255,using f2$: "Reg Hourly Rate",pr(1,24),pr(2,24),pr(3,24),pr(4,24),pr(5,24),pr(6,24),pr(7,24),pr(8,24),pr(9,24)
	pr #255,using f2$: "O/T Hourly Rate",pr(1,25),pr(2,25),pr(3,25),pr(4,25),pr(5,25),pr(6,25),pr(7,25),pr(8,25),pr(9,25)
	pr #255,using L2280: " "
	mat pr=(0)
	pc=0
	return 

L2430: gosub L2240
L2440: fncloseprn
L2450: win=101
	fnopenwin(win,10,20,14,59,cap$)
	pr #win,fields "4,2,C 24,N": "Record Number to Change:"
	pr f "15,35,C 09,B,5": "Done (F5)"
L2490: input #win,fields "4,27,N 5,UT,N": r conv L2490
	close #win: ioerr L2510
L2510: if cmdkey=5 or r=0 then goto L2700
	read #3,using L1600,rec=r: eno,dep2,mat inp,gpd,mat hr,adr noRec L2450,eof L2450
	teno=teno-eno
	mat tinp=tinp-inp
	if eno=0 then goto L2450
	s9=1
	goto L850

L2590: eno=0
	dep=0
	mat inp=(0)
	gpd=0
L2630: rewrite #3,using L1600,rec=r: eno,dep,mat inp,gpd,mat hr,adr
	tgp=tgp+gpd
	teno=teno+eno
	mat tinp=tinp+inp
	if chg$(1)><"Y" then rewrite #1,using L1660,key=en$: tgp
	goto L2450

L2700: ! r:
	pr newpage
	msgline$(1)="Add another Employee? (Y/N)" 
	msgline$(2)=""
	fnoldmsgbox(mat response$,cap$,mat msgline$,2)
	if response$(1)="N" then goto TOTALSCREEN
	r=0
	close #3: 
	open #3: "Name=[Q]\PRmstr\rpwork"&wsid$&".h[cno]",internal,output 
	s9=0
	eno=dep=gpd=0: mat inp=(0): mat hr=(0)
goto L770 ! /r

L2810: ! r:
	pr newpage
	close #1: 
	close #2: 
	close #3: 
	close #4: 
	close #5: 
	open #5: "Name=[Q]\PRmstr\JCPRH1.H[cno]",internal,output ioerr L2890
	close #5,free: 
	L2890: !
	open #5: "Name=[Q]\PRmstr\JCPRH1.H[cno],SIZE=0,RecL=40",internal,output 
	close #5: 
fnchain("S:\acsPR\prCalk") ! /r

XIT: fnxit

SORTIT: ! r: Replace SORTIT
	open #1: "Name=Sort"&wsid$&".tmp,RecL=128,Replace",internal,output 
	open #2: "Name=[Q]\PRmstr\JCPRH1.H[cno]",internal,input 
	write #1,using L3030: "! SORT FOR TRANSFER JC TO PR IN PROCESS"
	write #1,using L3030: "FILE [Q]\PRmstr\JCPRH1.H[cno],,,"&env$('Temp')&"\Addr."&session$&",,,acsPR,,A,N"
	write #1,using L3030: "MASK 1,8,N,A,10,2,PD,A"
	write #1,using L3030: "FILE [Q]\PRmstr\JCPRH1.H[cno],,,ADDR2."&wsid$&",,,acsPR,,A,N"
	write #1,using L3030: "MASK 1,8,N,A,27,14,C,A"
	L3030: form pos 1,c 128
	close #1: 
	execute "FREE "&env$('Temp')&"\Addr."&session$&" -n" ioerr ignore
	execute "FREE ADDR2."&wsid$&" -n" ioerr ignore
	close #2: 
	execute "SORT Sort"&wsid$&".tmp -n"
return ! /r

L3110: ! r:
	pr newpage
	win=101
	fnopenwin(win,10,14,14,65,cap$)
	pr #win,fields "4,02,C 41,N": "Employee Number to pr (blank for all):"
	pr f "15,34,C 11,B,5": "Cancel (F5)"
L3160: input #win,fields "4,44,Nz 8,UT,N": en1 conv L3160
	if cmdkey=5 then goto TOTALSCREEN
	fnopenprn
	eno=en2=dt2=t1=t2=t3=t4=0
	restore #6: 
	gosub HDR
L3220: read #6,using L3230: jci eof END_OF_FILE
L3230: form pos 1,pd 3
	read #5,using L3250,rec=jci: mat h,dt1,jn$ noRec L3220
L3250: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
	if en1=0 then goto L3290
	if en2>0 and en1><h(1) then goto L3470
	if en1><h(1) then goto L3220
L3290: if h(1)><eno or dt1><dt2 then gosub T1
	if h(1)><eno then gosub T2
	if eno=0 or h(1)<>eno then goto L3320 else goto L3370
	L3320: !
	en$=lpad$(str$(h(1)),8)
	em$=""
	read #1,using L870,key=en$: em$ nokey L3370
	pr #255,using L3360: em$
	L3360: form pos 1,c 40,skip 1
	L3370: pr #255,using L3380: h(1),dt1,jn$,h(4),h(5) pageoflow PGOF
	L3380: form pos 1,n 8,pic(zz####/##/##bb),c 6,2*n 8.2,skip 1
	eno=en2=h(1)
	dt2=dt1
	t1=t1+h(4)
	t2=t2+h(5)
	t3=t3+h(4)
	t4=t4+h(5)
goto L3220

L3470: !
	gosub T1
	gosub T2
goto TOTALSCREEN

T1: ! 
	if eno=0 then goto L3570
	pr #255,using L3550: ""," _______" ," _______"
	pr #255,using L3550: "Daily Total",t1,t2
	L3550: form pos 9,c 20,2*g 8.2,skip 1
	pr #255: 
	L3570: !
	t1=t2=0
return 

T2: ! 
	if eno=0 then goto L3650
	pr #255,using L3550: ""," _______" ," _______"
	pr #255,using L3550: "Employee Total",t3,t4
	pr #255: 
	L3650: !
	t3=t4=0
return 

PGOF: ! 
	pr #255: newpage
	gosub HDR
continue 

HDR: ! 
	pr #255,using L3750: "Employee Time Breakdown"
	L3750: form pos 17,c 50,skip 1
return 

END_OF_FILE: ! 
	gosub T1
	gosub T2
	fncloseprn
	goto TOTALSCREEN
Include: Ertn
