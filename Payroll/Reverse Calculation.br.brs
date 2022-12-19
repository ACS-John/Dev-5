! formerly S:\acsPR\newprRevCal
! Reverse Calculation
 
autoLibrary
on error goto Ertn
 
dim x$*8,tdc(6)
dim bankgl$*12,bk$*24,glwk$*256
dim tr$(5)*35,tr(2),resp$(5)*40
dim tgl$*12,t(26),prgl(15,3),prgl$(15)*12 ! ,desc$*50
dim tr$*12,td$*30,dat$*20,cp(32),tdc(10)
dim a$*40,em$*30,tgl(3),tcp(32),eno$*8,ttgl(3)
dim ml$(3)*100,fullname$(20)*20,abrevname$(20)*8,newcalcode(20)
dim newdedfed(20),dedfica(20),dedst(20),deduc(20),dedcode(20),gl$(20)*12
 
fnTop(program$)
 
cd1=val(date$(4:5)&date$(7:8)&date$(1:2))
fnDedNames(mat fullname$,mat abrevname$,mat dedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i
read #1,using 'form pos 605,C 12,N 1': bankgl$,gli
close #1:
if gli=1 then
	gosub POSTGL0
	open #14: "Name=[Q]\GLmstr\GLBRec.h[cno],KFName=[Q]\GLmstr\GLRecIdx.h[cno],Shr",i,outIn,k ioerr L250
	goto L260
	L250: !
	gli=0
	L260: !
end if
open #6: "Name=[Q]\CLmstr\TRMSTR.h[cno],KFName=[Q]\CLmstr\TRIDX1.h[cno],Shr",i,outIn,k ioerr L330
open #7: "Name=[Q]\CLmstr\TRMSTR.h[cno],KFName=[Q]\CLmstr\TRIDX2.h[cno],Shr",i,outIn,k
if exists("[Q]\CLmstr\Tralloc-Idx.h[cno]") then
	open #tralloc:=8: "Name=[Q]\CLmstr\TrAlloc.h[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",i,outIn,k
else
	open #tralloc:=8: "Name=[Q]\CLmstr\TrAlloc.h[cno],Shr",i,outi,r
end if
open #9: "Name=[Q]\CLmstr\BankMstr.h[cno],KFName=[Q]\CLmstr\BankIdx1.h[cno],Shr",i,outIn,k
open #20: "Name=[Q]\CLmstr\Company.h[cno],Shr",i,i,r ioerr L330
read #20,using 'form pos 152,N 2',rec=1: bcde
close #20:
cli=1
L330: !
open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,outIn,k
open #hDepartment:=5: "Name=[Q]\PRmstr\Department.h[cno],Shr, KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",i,outIn,k
open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",i,outIn,k
 
d1=fnPayPeriodEndingDate
dat$=cnvrt$("pic(########)",d1)
dat=val(dat$(5:6))*10000 +val(dat$(7:8))*100 +val(dat$(3:4)) ! set payroll date back to mmddyy format for some files
 
ASK_EMPLOYEE_NO: !
	if reverse_all=1 then goto L730
	fnTos
	respc=0
	fnLbl(1,1,"Employee to Reverse:",25,1)
	fnComboF("Employee",1,28,0,"[Q]\PRmstr\Employee.h[cno]",1,8,9,20,"[Q]\PRmstr\EmployeeIdx-no.h[cno]",2,0, "Select the employee to reverse.")
	resp$(respc+=1)="[All]"
	fnLbl(2,1,"Payroll Date:",25,1)
	fnTxt(2,28,12,0,1,"3",0,"You can reverse a check from any pay period.  Be sure the payroll date is correct.")
	resp$(respc+=1)=str$(d1)
	fnLbl(3,1,"Check Number:",25,1)
	fnTxt(3,28,8,0,1,'30',0,"Only applicable if checks have been printed. ")
	resp$(respc+=1)= ""
	if exists('[Q]\CLmstr\BankMstr.h[cno]') then
		fnLbl(4,1,"Bank Code for Checkbook:",25,1)
		fnComboF('Bank',4,28,0,"[Q]\CLmstr\BankMstr.h[cno]",1,2,3,30,"[Q]\CLmstr\BankIdx1.h[cno]",limit_to_list,0,'',frame)
		resp$(resp_cl_bank_code=respc+=1)=str$(bcde)
	end if
	if success=1 then
		fnLbl(6,1,"Employee Number "&str$(eno)&" successfully reversed!",40,1)
	end if
	fnCmdKey("&Next",1,1,0,"Proceed with reversing of payroll." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")

	ckey=fnAcs(mat resp$) ! ask employee #
	if ckey=5 then goto Xit
	success=0
	if resp$(1)="[All]" then
		reverse_all=1
	else
		eno=val(resp$(1)(1:8))
	end if
	d1=val(resp$(2))
	if d1=0 then
		mat ml$(2)
		ml$(1)="You must enter the payroll date!"
		fnMsgBox(mat ml$,resp$,'',0)
		goto ASK_EMPLOYEE_NO
	end if
	w1=val(resp$(3))
	if resp_cl_bank_code then bcde=val(resp$(resp_cl_bank_code)(1:2))
	mat tcp=(0)
	if reverse_all=1 then gosub CREATE_LIST

	L730: ! r: main loop
	if reverse_all=1 then read #13,using "form pos 1,n 8,n 7": eno,w1 eof Finis
	gosub REVERSE_BANK_REC
	if cli=1 then gosub UPDATE_CHECKBOOK ! update checkbook
	x$=lpad$(str$(eno),8)
	read #1,using L780,key=x$,release: em10,em11 nokey L1190
L780: form pos 132,2*pd 4.2
	if pgl1=1 then gosub POSTGL2
	cxk=0
	! DELETE_OLD_PAYROLL_CHECK: !
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	restore #4,key>=checkkey$: nokey UPDATE_MASTER
do
	read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof UPDATE_MASTER
	if heno<>eno then goto UPDATE_MASTER
	if prd=d1 then mat tcp=tcp+cp : delete #4:
	cxk=1
	if prd=d1 then em10=em10+tdc(3) ! add sick hours back
	if prd=d1 then em11=em11+tdc(4) ! add vacation hours back
loop
UPDATE_MASTER: !
	if cxk=0 then goto L1220
	rewrite #1,using L950,key=x$: em10,em11,0 ! WRITE 0 IN LAST PAYROLL DATE IN MASTER RECORD
	L950: form pos 132,2*pd 4.2,pos 162,n 6
	success=1
	! UPDATE_DEPARTMENT: !
	restore #hDepartment,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey L1040
	do
		read #hDepartment,using 'form pos 1,N 8,n 3,pos 42,n 6': teno,tdn,lastpd eof L1040
		if teno=eno and fndate_mmddyy_to_ccyymmdd(lastpd)=d1 then
			rewrite #hDepartment,using "form pos 42,n 6": 0
		end if
	loop while teno=eno
	L1040: !
goto ASK_EMPLOYEE_NO
Finis: !
	if pgl1=1 then goto POSTGL4
	close #1: ioerr ignore
	close #2: ioerr ignore
goto Xit
! /r
REVERSE_BANK_REC: ! r:
	hw1=0
	if gli<>1 then goto L1170
	bk$=bankgl$&lpad$(str$(w1),12)
	rewrite #14,using L1150,key=bk$: 0 nokey L1170
	L1150: form pos 63,pd 5.2
	hw1=w1
	L1170: !
return  ! /r
L1190: !
	mat ml$(2)
	ml$(1)="Employee Number "&ltrm$(x$)&" does not exist!"
	ml$(2)="Please select a different Employee Number."
	fnMsgBox(mat ml$,resp$,'',0)
goto ASK_EMPLOYEE_NO
 
L1220: !
	mat ml$(2)
	ml$(1)="No information found for Employee Number "&ltrm$(x$)
	ml$(2)="Please select a different Employee Number."
	fnMsgBox(mat ml$,resp$,'',0)
goto ASK_EMPLOYEE_NO
 
UPDATE_CHECKBOOK: ! r:
	if bcde=0 and w1=0 then goto L1510 ! no bank info
	k$=cnvrt$("N 2",bcde)&"1"&lpad$(str$(w1),8)
	read #6,using L1320,key=k$: bcde,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd nokey L1300
	goto L1320
	L1300: !
	mat ml$(2)
	ml$(1)="Did not find check # "&str$(w1)&" in the Check "
	ml$(2)="Book system. Will proceed without voiding."
	fnMsgBox(mat ml$,resp$,'',0)
	goto L1510
	L1320: form pos 1,n 2,n 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1
	adr=tr(1)
	read #9,using L1350,key=cnvrt$("N 2",bcde): bal nokey L1390
	L1350: form pos 45,2*pd 6.2
	bal=bal+tr3
	rewrite #9,using L1350,key=cnvrt$("N 2",bcde): bal
	tr3=0
	L1390: !
	tr$(3)=tr$(4)=""
	tr$(5)="VOID"
	adr=tr(1)
	mat tr=(0)
	if clr=0 then clr=cd1
	rewrite #6,using L1320,key=k$: bcde,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd
	if exists("[Q]\CLmstr\Tralloc-Idx.h[cno]") then goto DELETE4_02ALLOC
	L1460: !
	if adr=0 then goto L1510
	read #8,using 'form pos 65,pd 3',rec=adr: nta noRec L1510
	delete #8,rec=adr:
	adr=nta: goto L1460
L1510: return ! /r
DELETE4_02ALLOC: ! r: delete allocations in 4.02 checkbook system
	restore #tralloc,key=k$: nokey L1570
	L1540: read #8,using "form pos 1,c 11": trallockey$
	if trallockey$<>k$ then goto L1510
	delete #tralloc,key=k$:
	L1570: !
goto L1540 ! /r
POSTGL0: ! r:
	mat ml$(2)
	ml$(1)="Do you wish to create reversing"
	ml$(2)="General Ledger entries? (Y/N)?"
	fnMsgBox(mat ml$,resp$,'',292)
	if resp$(1:1)="Y" then pgl1=1 else pgl1=0
	if pgl1=1 then gosub POSTGL1
return  ! /r
 
POSTGL1: ! r:
	glinstal=1
	fli2$(1)="11,64,n 3,u"
	fli2$(2)="11,68,n 6,u"
	fli2$(3)="11,75,n 3,u"
	open #1: "Name=[Q]\GLmstr\GLBUCKET.h[cno],Shr",i,i,r ioerr L1740
	read #1,using 'form pos 1,n 1',rec=1: glb noRec ignore
	close #1:
	if glb=2 then gosub L3690
	L1740: !
	fnTos
	respc=0
	fnLbl(1,1,"General Ledger Posting Date:",25,1)
	fnTxt(1,28,12,0,1,"3",0,"If this revesing entry should be posted to the general ledger, what date should be used?")
	fnCmdKey("&Next",1,1,0,"Proceed with reversing entry." )
	fnCmdKey("E&xit",5,0,1,"Don't Post")
	ckey=fnAcs(mat resp$) ! posting date
	if ckey<>5 then
		dat1=val(resp$(1))
		if glb=2 then 
			glwk$="[Q]\GLmstr\GL"&cnvrt$("PIC(######)",dat1)&".h[cno]"
		else
			glwk$="[Q]\GLmstr\GL_Work_[acsUserId].h[cno]"
		end if
		if glb=2 and uprc$(rtrm$(accrue$))="Y" then
			open #11: "Name=[Q]\GLmstr\GL"&cnvrt$("PIC(######)",d2)&".h[cno],RecL=104,USE",internal,output
		end if
		open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i
		read #1,using 'form pos 1,c 40,pos 437,15*c 12': a$,mat prgl$
		close #1:
		for j=1 to 15
			prgl(j,1)=val(prgl$(j)(1:3))
			prgl(j,2)=val(prgl$(j)(4:9))
			prgl(j,3)=val(prgl$(j)(10:12))
		next j
		nametab=36-len(rtrm$(a$))/2
		fnOpenPrn
		gosub GlDistHeaders
	end if
return  ! /r
POSTGL2: ! r:
	oldteno=teno
	rec1=ta(1)
	L2050: if rec1=0 then goto L2340
	read #2,using L2070,rec=rec1: teno,mat tgl,dat,mat tcp,nta
	L2070: form pos 1,n 8,x 3,n 3,n 6,n 3,pos 42,n 6,pos 358,22*pd 5.2,pos 468,pd 3
	if dat><d1 then goto L2340
	if tgl(2)=0 then goto L2340
	if mastercd=0 then gosub OPNWORK_DUESTUFF
	tgl$=lpad$(str$(tgl(1)),3)&lpad$(str$(tgl(2)),6)&lpad$(str$(tgl(3)),3)
	if tgl(1)=0 or tgl(1)=oldtgl then goto L2130 else gosub OPNWORK_DUESTUFF
	L2130: if oldteno=teno then goto L2160
	eno$=lpad$(str$(teno),8)
	read #1,using 'form pos 9,c 30',key=eno$,release: em$ nokey L2340
	L2160: !
	pr #255,using L2180: teno,em$,mat tgl,-tcp(31)+tcp(29)+tcp(30) pageoflow PgOf
	L2180: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),n 12.2,skip 1
	for j=1 to 24 ! ACCUMULATE 24 WITHHOLDINGS
		if j<=4 then goto L2240
		if dedcode(j-4)=1 then goto L2240
		t(j)=t(j)+tcp(j)
		goto L2250
	L2240: t(j)=t(j)-tcp(j)
	L2250: next j
	t(25)=t(25)+tcp(25) ! EIC
	t(26)=t(26)-tcp(32) ! ACCUMULATE NET
	subtotal=subtotal+tcp(31)-tcp(29)-tcp(30) ! ACCUMULATE TOTAL BY ACCT TO BE POSTED TO GL
	totaldue=totaldue-tcp(31)+tcp(29)+tcp(30) ! DUE TO PAYROLL CLEARING
	totaldr=totaldr+tcp(31)-tcp(29)-tcp(30)
	totalrec=totalrec+tcp(31)-tcp(29)-tcp(30) ! TOTAL DUE FRO OTHER FUNDS
	mat ttgl=tgl : gosub POSTGL3
	rec1=nta: goto L2050
	L2340: !
return  ! /r
POSTGL3: ! r: SUBTOTAL ROUTINE AND WRITE GL TRANS
	if glinstal=0 then goto L2460
	if diskin=0 then gosub L3340
	td$="Payroll summary"
	if uprc$(accrue$)<>"Y" then goto L2440
	accrued=round(subtotal/day*dayslm,2)
	write #11,using L2450: mat ttgl,d2,accrued,5,0,tr$,"Payroll accrual",prgl$(15)
	totacc=totacc+accrued
	L2440: !
	write #3,using L2450: mat ttgl,dat,-subtotal+accrued,5,0,tr$,"Reversing "&em$,prgl$(15)
	L2450: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 52,c 12
	L2460: !
	pr #255,using L2470: "-----------",-subtotal
	L2470: form pos 65,c 11,skip 1,pos 64,pic(---------.##),skip 1
	if accrued<>0 then pr #255,using L2490: "ACCRUED PORTION",accrued else pr #255:
	L2490: form pos 45,c 16,pos 64,pic(---------.##),skip 2
	subtotal=0
return  ! /r
GlDistHeaders: ! r: headers for General Ledger Distribution for Payroll
	p1=p1+1
	pr #255,using L2550: date$,a$,"Page",p1
	L2550: form skip 1,pos 1,c 8,pos nametab,c 40,pos 77,c 5,pic(zzz),skip 1
	pr #255,using L2570: time$,"General Ledger Distribution for Payroll",dat1
	L2570: form pos 1,c 8,pos 17,c 40,skip 1,pos 29,pic(zz/zz/zz),skip 2
	pr #255: "Employee                                               G/L                 Amount"
	pr #255: " Number        Name                                  Account         Debits     Credits"
	pr #255: ""
return  ! /r
POSTGL4: ! r:
	mat ttgl=tgl
	! GOSUB 1790 ! WRITE LAST ENTRY
	if multigl=1 then  ! ONLY ONE FUND OR COMPANY
		gosub OPNWORK_DUESTUFF
		gosub L3520
	end if
	gosub POST_WH_AND_NET
	gosub PRINT_TOTALS
	fnClosePrn
	if glinstal=0 then goto Xit
	if glb=2 then goto Xit
fnChain("S:\General Ledger\Merge") ! /r
Xit: fnXit
PgOf: ! r:
	pr #255: newpage
	gosub GlDistHeaders
continue  ! /r
POST_WH_AND_NET: ! r: ASSIGN G/L NUMBERS AND POST TO GL WORK FILE
	for j=1 to 26
		if t(j)=0 then goto L3030
		if t(j)<0 then goto L2950
		if j<=4 then goto L2850 else goto L2870
		L2850: !
		pr #255,using L2180: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),-t(j) pageoflow PgOf
		goto L2930
		L2870: !
		pr #255,using L2920: 0," ",gl$(j-4),-t(j) pageoflow PgOf
		if j<25 then goto L2890 else goto L2910
		L2890: !
		pr #255,using L2920: 0," ",gl$(j-4),-t(j) pageoflow PgOf
		goto L2920
		L2910: !
		pr #255,using L2920: 0," ",gl$(j-11),-t(j) pageoflow PgOf
		L2920: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,c 12,n 12.2,skip 1
		L2930: !
		totaldr=totaldr+t(j)
		goto L2980
		L2950: !
		pr #255,using L2960: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),-t(j) pageoflow PgOf
		L2960: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),x 12,n 12.2,skip 1
		totalcr=totalcr+t(j)
		L2980: !
		if glinstal=0 then goto L3030
		if j<=4 then write #3,using L2450: prgl(j,1),prgl(j,2),prgl(j,3),dat,-t(j),5,0,tr$,td$,prgl$(15)
		if j>4 and j<25 then write #3,using L3010: gl$(j-4),dat,-t(j),5,0,tr$,td$,prgl$(15)
		L3010: form pos 1,n 3,c 12,pd 6.2,n 2,n 2,c 12,c 52,c 12
		if j>24 then write #3,using L2450: prgl(j-11,1),prgl(j-11,2),prgl(j-11,3),dat,-t(j),5,0,tr$,td$,prgl$(15)
		L3030: !
	next j
	if uprc$(accrue$)<>"Y" then goto L3070
	write #11,using L2450: g1,g2,g3,d2,-totacc,5,0,tr$,"Payroll accrual",prgl$(15)
	write #3,using L2450: g1,g2,g3,dat,-totacc,5,0,tr$,"Payroll accrual",prgl$(15)
	L3070: !
return  ! /r
OPNWORK_DUESTUFF: ! r: OPEN G/L WORK FILES AND CREATE DUE TO AND DUE FROM ENTRIES
	if tgl(1)=0 then goto L3330
	if mastercd=1 then goto L3160
	mat ml$(2)
	ml$(1)="The G/L accounts you are using indicate you have seperate funds or"
	ml$(2)="cost centers on the system.  Enter yes if you have more than one fund."
	fnMsgBox(mat ml$,resp$,'',4)
	if resp$(1:1)="T" then multigl=1
	if multigl><1 then goto L3160
	L3160: if multigl=2 then goto L3330
	! CREATE DUE TO PAYROLL FUND ENTRIES
	if mastercd=0 then goto L3290 ! FIRST TIME THRU ROUTINE
	pr f "10,2,c 78": "ENTER THE G/L ACCOUNT # FOR YOUR 'DUE TO PAYROLL CLEARING ACCOUNT '"
	pr f "11,2,c 60": "ON FUND # "&tgl$(1:3)
	L3210: input fields mat fli2$: mat ttgl conv L3210
	pr #255,using L2960: 0," ",mat ttgl,-totaldue
	totalcr=totalcr+totaldue
	gosub PRINT_TOTALS
	gosub GlDistHeaders
	if glinstal=0 then goto L3290
	write #3,using L2450: mat ttgl,dat,-totaldue,5,0," ","Reversing Payroll summary",prgl$(15)
	close #3:
	L3290: totaldue=0
	totalcr=0
	totaldr=0
	L3330: mastercd=1
	L3340: if glinstal=0 then goto L3490
	diskin=1
	open #3: "Name="&glwk$,internal,outIn ioerr L3450
	read #3,using L3410: dat2,trcode eof L3410
	L3410: form pos 13,n 6,pos 25,n 2
	if dat2=dat and trcode=5 then goto L3490
	if glb=2 then goto L3490
	close #3,free:
	L3450: open #3: "Name="&glwk$&",size=0,RecL=104",internal,output ioerr L3480
	goto L3490
	close #3:
	L3480: open #3: "Name="&glwk$,internal,output ioerr L3490
	L3490: oldtgl=tgl(1)
return  ! /r
L3520: ! r: FINAL PAGE FOR CONTROL SET OF BOOKS  (MULTI-FUNDS ONLY)
	pr f "10,2,c 78": "ENTER THE G/L ACCOUNT # FOR YOUR 'DUE FROM OTHER FUNDS '"
	pr f "11,2,c 60": "ON YOUR CONTROL SET OF BOOKS"
	input fields mat fli2$: mat ttgl conv L3210
	pr #255,using L2180: 0," ",mat ttgl,-totalrec
	totaldr=totaldr+totalrec
	if glinstal=0 then goto L3600
	write #3,using L2450: mat ttgl,dat,-totalrec,5,0," ","Reversing Payroll summary",prgl$(15)
	L3600: !
return  ! /r
PRINT_TOTALS: ! r: AND UNDERLINES
	if totacc<>0 then pr #255,using L2180: 0," ",g1,g2,g3,-totacc
	pr #255,using L3650: "___________","___________",-totaldr,-totalcr
	L3650: form pos 65,c 11,x 1,c 11,skip 1,pos 64,pic(---------.##),pic(---------.##),skip 1
	pr #255,using L3650: "===========","==========="
return  ! /r
L3690: ! r:
	close #101: ioerr ignore
	open #12: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLINDEX.h[cno],Shr",i,i,k ioerr ignore
	mat ml$(2)
	ml$(1)="Did you accrue part of this payroll"
	ml$(2)="in the previous month? (Y/N)"
	fnMsgBox(mat ml$,resp$,'',36)
	accrue$=resp$(1)(1:1)
return  ! /r
! ACCRUAL: ! r:
!   fnTos
!   respc=0: mypos=50
!   fnLbl(1,1,"Number of Days in this Pay Period:",mypos,1)
!   fnTxt(1,mypos+3,10,0,1,'30',0,"In order to know how much to accure, the system needs to know the days to accure.")
!   resp$(1)=str$(day)
!   fnLbl(2,1,"Number of Days to Expense in Last Month:",mypos,1)
!   fnTxt(2,mypos+3,10,0,1,'30',0,"In order to know how much to accure, the system needs to know the days to accure.")
!   resp$(2)=str$(dayslm)
!   fnLbl(3,1,"G/L # for Due From Other Funds on Fund # "&oldtgl$(1:3)&":",mypos,1)
!   fnQgl(3,mypos+3,0,2,pas)
!   resp$(3)=fnrgl$(bankgl$)
!   fnLbl(4,1,"Last Day of Previous Month:",mypos,1)
!   fnTxt(4,mypos+3,10,0,1,"1",0,"Enter the month end date.")
!   resp$(4)=str$(d2)
!   fnCmdKey("&Next",1,1,0,"Continue posting." )
!   fnCmdKey("E&xit",5,0,1,"Returns to menu")
!   ckey=fnAcs(mat resp$) ! ask accrual info
!   if ckey=5 then goto Xit
!   day=val(resp$(1)) ! days in pay period
!   dayslm=val(resp$(2)) ! days last month
!   key$=fnagl$(resp$(3))
!   g1=val(key$(1:3)): g2=val(key$(4:9)) : g3=val(key$(10:12))
!   d2=val(resp$(4)) ! last day previous month
!   acgl$=cnvrt$("N 3",g1)&cnvrt$("N 6",g2)&cnvrt$("N 3",g3)
!   if trim$(acgl$)<>"" then read #12,using L3960,key=acgl$: desc$ nokey L3970
! L3960: form pos 13,c 30
! L3970: !
! return ! /r
CREATE_LIST: ! r:
	open #13: "Name=[Temp]\prreverse[Session],size=0,RecL=128,replace",internal,outIn
	L4000: !
	read #1,using "form pos 1,n 8,pos 132,2*PD 4.2",release: eno,em10,em11 eof L4100
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	restore #4,key>=checkkey$: nokey L4000
	L4040: !
	read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L4000
	if heno<>eno then goto L4000
	if prd=d1 then goto L4080
	goto L4040
	L4080: !
	write #13,using 'form pos 1,n 8,n 7': eno,ckno
	goto L4000
	L4100: !
	restore #13:
return  ! /r
include: ertn
