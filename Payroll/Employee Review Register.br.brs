! Replace S:\acsPR\newprEmpRev
! Employee Review Register
 
	autoLibrary
	on error goto Ertn
! gosub CHECK_PASSWORD
 
	dim em$*30,em(3),tdt(4),tdy(6),tcd(3)
	dim ytdtdc(10),tdc(10),tcp(32),ytdtotal(32),tdet(23)
	dim message$*40
 
	fnTop(program$)
 
	fnGetPayrollDates(beg_date,end_date)
 
MENU1: !
	fnTos
	respc=0
	fnLbl(1,47," ",1,1)
	fnLbl(1,1,"Beginning Date of Tax Year:",30,1)
	fnTxt(1,34,12,0,0,"3",0,"")
	resp$(respc+=1)=str$(beg_date)
	fnLbl(2,1,"Last Payroll Date to Analyze:",30,1)
	fnTxt(2,34,12,0,0,"3",0,"")
	resp$(respc+=1)=str$(end_date)
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	beg_date=val(resp$(1)) ! beginning of year
	end_date=val(resp$(2)) ! ending day of year
 
	on fkey 5 goto DONE
	fnopenprn
 
	gosub HDR
	open #1: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
	F_employee: form pos 1,n 8,c 30,pos 118,n 2,pos 132,2*pd 4.2,pos 156,n 6,pos 173
	open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",i,outIn,k
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",i,outIn,k
	L390: !
	read #1,using F_employee: eno,em$,em4,mat em eof DONE
	a=pos (rtrm$(em$)," ",1)
	b=pos (rtrm$(em$)," ",a+1)
	em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
	fsttrl=1
	restore #2,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey L390
	L480: !
	read #2,using 'form pos 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L390
	if teno<>eno then goto L390
	
	if tdet(1)> 0 then payrate=tdet(1) else payrate=tdet(2) ! set payrate as salary or hourly
	mat ytdtotal=(0) : mat ytdtdc=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey L480
	L550: !
	read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,ctdn,prd,ckno,mat tdc,mat tcp eof L610
	if heno<>teno then goto L610 ! read next department
	if prd<beg_date or prd>end_date then goto L550 ! not this year
	if ctdn<>tdn then goto L550 ! not same department
	mat ytdtdc=ytdtdc+tdc !  hours etc
	mat ytdtotal=ytdtotal+tcp ! earnings, etc
	goto L550
L610: if fsttrl=1 then goto L640
	gosub L730 ! pr TRAILER ONLY
	goto L660
L640: gosub L670 ! pr MASTER AND FIRST TRAILER
	fsttrl=0
L660: goto L480
L670: pr #255,using L710: eno,em$(1:23),tdn,em(3),tdt(1),tli,tdt(3),tdt(2),em(2),ytdtdc(4),em(1),ytdtdc(3),ytdtdc(5),payrate pageoflow L690
	goto L710
L690: pr #255: newpage
	gosub HDR
L710: form pos 1,pic(zzzzzzzz),pos 10,c 23,pos 34,pic(zzz),pos 38,pic(zz/zz/zz),pos 47,pic(zz/zz/zz),pos 55,pic(------.##),pos 66,pic(zz/zz/zz),pos 75,pic(zz/zz/zz),pos 83,6*n 10.2,skip 1
	return
L730: pr #255,using L740: tdn,tdt(1),tli,tdt(3),tdt(2),tdc(4),tdc(3),tdc(5),payrate pageoflow L760
L740: form pos 34,pic(zzz),pos 47,pic(zz/zz/zz),pos 55,pic(------.##),pos 66,pic(zz/zz/zz),pos 75,pic(zz/zz/zz),pos 93,pic(---,---.##),pos 113,3*n 10.2,skip 1
	goto L780
L760: pr #255: newpage
	gosub HDR
L780: return
 
DONE: !
	close #1: ioerr ignore
	close #2: ioerr ignore
	fncloseprn
goto Xit
 
HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b As of "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
	pr #255: "\ql   "
	pr #255,using L940: "Employee    Employee Name","Dept   Date   Last Rev", "Last Increase   Next Rev     Vacation Hours","   Sick Hours   Hol Hours"
	L940: form pos 1,c 25,pos 33,c 22,pos 59,c 43,pos 108,c 25
	pr #255,using L960: "Number","Hired     Date     Amount      Date","Date     Accrued     Taken   Accrued     Taken     Taken  Pay Rate"
	L960: form pos 2,c 6,pos 39,c 35,pos 77,c 66
	pr #255: ""
return ! /r
 
Xit: fnXit
include: ertn
