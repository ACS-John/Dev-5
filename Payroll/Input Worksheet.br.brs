! Replace S:\acsPR\newprInpWk
library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn
on error goto Ertn
dim em$*30,em(3),tdt(4),tdy(6),ta(2)
dim message$*40
dim gl$*12,tdt(4),tcd(3),tdet(23)
fntop(program$)
open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
open #2: "Name=[Q]\PRmstr\Department.h[cno], KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed
fnopenprn
gosub HDR
READ_EMPLOYEE: ! r: main loop
	read #1,using L220: eno,em$,em4,mat em eof DONE
	L220: form pos 1,n 8,c 30,pos 118,n 2,pos 132,2*pd 4.2,pos 156,n 6
	if em4=9 then goto READ_EMPLOYEE
	fsttrl=1
	restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey READ_EMPLOYEE
	L260: !
	read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof READ_EMPLOYEE
	if teno<>eno then pr #255: pageoflow NEWPGE: goto READ_EMPLOYEE
	if fsttrl=1 then goto L320
	pr #255,using L300: tdn,tdet(2),rpt$("  __________",3) pageoflow NEWPGE
	L300: form pos 44,n 3,n 10.2,c 36,skip 1
	goto L350
	L320: !
	pr #255,using L330: eno,em$,tdn,tdet(2),rpt$("  __________",3) pageoflow NEWPGE
	L330: form pos 1,pic(zzzzzzzz),x 2,c 33,n 3,n 10.2,c 36,skip 1
	fsttrl=0
	L350: !
goto L260 ! /r

NEWPGE: pr #255: newpage : gosub HDR : continue

DONE: ! r:
	close #1: ioerr ignore
	close #2: ioerr ignore
	pr #255,using L420: "Totals",rpt$("  ==========",3)
	L420: form pos 11,c 33,skip 1,pos 57,c 36,skip 1
	fncloseprn
goto Xit ! /r
Xit: fnxit

HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&date$("Month DD, CCYY")&"}"
	pr #255: "\ql   "
	pr #255: "Employee                                  Dept    Hourly  <----------- Hours -------------->"
	pr #255: " Number   Employee Name                   Numb      Rate   Regluar     Overtime      Other"
	pr #255: "________  ______________________________  ____    ______  __________  __________  __________"
return ! /r

include: Ertn
