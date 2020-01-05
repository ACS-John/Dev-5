! formerly S:\acsPR\newprYTDMis
! Miscellaneous Deductions Register - YTD
! ______________________________________________________________________
library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fnerror,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fnGetPayrollDates,fnDedNames
on error goto Ertn

dim em$*30,em(6),message$*40
dim dat$*20,t1(20),t2(20)
dim dedcode(20),calcode(20),dedfed(20)
dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10)
dim e$(10)*12,tpt(32),resp$(15)*30
dim tcp(32),tdc(10),ytdtotal(32),ss$*11
! ______________________________________________________________________
	fntop(program$)

	fnGetPayrollDates(beg_date,end_date)

MENU1: ! 
	fnTos(sn$="prytdmis")
	respc=0
	fnLbl(1,43," ",1,1)
	fnLbl(1,1,"Beginning Date of Tax Year:",26,1)
	fnTxt(1,30,12,0,0,"3",0,"") 
	resp$(respc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date of Tax Year:",26,1)
	fnTxt(2,30,12,0,0,"3",0,"") 
	resp$(respc+=1)=str$(end_date)
	fnCmdSet(2): fnAcs(sn$,0,mat resp$,ck)
	if ck=5 then goto Xit
	beg_date=val(resp$(1)) ! beginning of year
	end_date=val(resp$(2)) ! ending day of year

	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
	for j=1 to 20: abbrevname$(j)=lpad$(rtrm$(abbrevname$(j)),6) : next j
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #20,using L360: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
	ficamaxw=ficamaxw*10
	L360: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
	close #20: 
	open #2: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed 
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 

	open #3: "Name=[Q]\PRmstr\Department.h[cno],Shr, KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed 

	fnopenprn
	gosub HDR
	do
	ReadEmp: !
	read #2,using Femp1: eno,em$,mat em eof EOJ
	Femp1: form pos 1,n 8,c 30,pos 112,6*n 2,pos 173,2*pd 3
	mat ytdtotal=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey ReadEmp
	do
		L510: !
		read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof PRINT_INFO
		if heno<>eno then goto PRINT_INFO
		if prd<beg_date or prd>end_date then goto L510 ! not this year
		mat ytdtotal=ytdtotal+tcp
	loop

	PRINT_INFO: ! r:
	for j=1 to 20
		t1(j)=ytdtotal(j+4)
	next j
	pr #255,using L660: eno,em$(1:18),t1(1),t1(2),t1(3),t1(4),t1(5),t1(6),t1(7),t1(8),t1(9),t1(10) pageoflow NEWPGE
	L660: form pos 1,g 8,x 2,c 18,10*n 10.2,skip 1
	for j=1 to 10
		if trim$(abbrevname$(j+10))<>"" then goto L670 ! have more than 10 deductions
	next j
	goto L690
	L670: !
	pr #255,using L680: t1(11),t1(12),t1(13),t1(14),t1(15),t1(16),t1(17),t1(18),t1(19),t1(20) pageoflow NEWPGE
	L680: form pos 34,10*n 10.2,skip 1
	L690: !
	mat t2=t2+t1
loop ! /r
! ______________________________________________________________________
EOJ: ! r:
	pr #255,using L740: rpt$("  ________",10)
	L740: form pos 29,c 100,skip 1
	pr #255,using L660: "","Totals",t2(1),t2(2),t2(3),t2(4),t2(5),t2(6),t2(7),t2(8),t2(9),t2(10)
	for j=1 to 10
		if trim$(abbrevname$(j+10))<>"" then goto L800 ! have more than 10 deductions
	next j
	goto L810
L800: !
pr #255,using L680: t2(11),t2(12),t2(13),t2(14),t2(15),t2(16),t2(17),t2(18),t2(19),t2(20)
L810: ! 
fncloseprn
close #1: ioerr ignore
close #2: ioerr ignore
goto Xit ! /r
! ______________________________________________________________________
NEWPGE: pr #255: newpage : gosub HDR : continue 
! ______________________________________________________________________
HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	pr #255: "\ql   "
	pr #255,using L980: "Emp-Numb  Employee Name",abbrevname$(1),abbrevname$(2),abbrevname$(3),abbrevname$(4),abbrevname$(5),abbrevname$(6),abbrevname$(7),abbrevname$(8),abbrevname$(9),abbrevname$(10)(1:6)
	L980: form pos 1,c 32,9*c 10,c 6
	pr #255: "________  __________________";rpt$("  ________",10)
	for j=1 to 10
		if trim$(abbrevname$(j+10))<>"" then goto L1040 ! have more than 10 deductions
	next j
	goto L1070
	L1040: !
	pr #255,using L1050: abbrevname$(11),abbrevname$(12),abbrevname$(13),abbrevname$(14),abbrevname$(15),abbrevname$(16),abbrevname$(17),abbrevname$(18),abbrevname$(19),abbrevname$(20)
	L1050: form pos 38,10*c 10
	pr #255: "                                 ";rpt$("  ________",10)
	L1070: !
return ! /r
Xit: fnxit
include: ertn