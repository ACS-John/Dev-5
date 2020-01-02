! formerly S:\acsPR\newprRegTot
! beginning with 4.0 the tax deposit reads from the checkhistory file

library 'S:\Core\Library': fntop,fnxit, fnerror,fnDedNames,fnopenprn,fncloseprn,fngethandle,fnTos,fnFra,fnTxt,fnLbl,fnCmdKey,fnAcs,fnss_employee,fnss_employer,fnGetPayrollDates
on error goto ERTN

dim deptot(999,2),t(36)
dim fullname$(20)*20,ab$(20)*8,cp(32),tdc(10)
dim em$*30

fntop(program$)

fnGetPayrollDates(beg_date,end_date)
ssr1=fnss_employee
ssr2=fnss_employer
! If FNPROCESS=1 Then Goto 410
fnTos(sn$="TaxDeposit") 
rc=0: mylen=22: mypos=mylen+3: frameno=1
fnFra(1,1,3,40,"Date Range of Deposit","Enter the date range for the payrolls to be included.")
fnLbl(1,1,"Beginning Date:",mylen,1,0,frameno)
fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this deposit. ",frameno)
resp$(rc+=1)=str$(beg_date)
fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this deposit. ",frameno)
resp$(rc+=1)=str$(end_date)
fnCmdKey("Next",1,1,0,"Calculate tax deposit.")
fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
fnAcs(sn$,0,mat resp$,ckey)
if ckey=5 then goto XIT
beg_date=val(resp$(1)) 
end_date=val(resp$(2))
! 
fnopenprn
! 
fnDedNames(mat fullname$,mat ab$)
for j=1 to 20
	ab$(j)=lpad$(rtrm$(ab$(j)),8)
next j
gosub PrHeader
open #h_employee:=fngethandle: "Name=[Q]\PRmstr\Employee.h[cno],Shr",internal,input,relative 
open #h_checks:=fngethandle: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
do ! 
	ReadEmployee: ! 
	read #h_employee,using "Form POS 1,N 8,C 30": eno,em$ eof PrFinalTotals
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	foundone=1
	restore #h_checks,key>=checkkey$: nokey ReadEmployee
	do
		read #h_checks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,dep,prd,ckno,mat tdc,mat cp eof ReadEmployee
		if eno=heno and prd=>beg_date and prd<=end_date then
			if dep>0 then ! department 0 not totaled
				deptot(dep,1)=deptot(dep,1)+cp(31)
				deptot(dep,2)=deptot(dep,2)+cp(2)+cp(3)
			end if
			! pr #255: 'employee number: '&str$(heno)&' employee record: '&str$(rec(h_employee))&' check number: '&str$(ckno)&' check history record number: '&str$(rec(h_checks))
			pr #255,using 'form pos 1,pic(zzzz/zz/zz),pos 11,19*n 10.2,n 4': prd,cp(31),cp(2)+cp(3),cp(1),cp(4),cp(5),cp(6),cp(7),cp(8),cp(9),cp(10),cp(11),cp(12),cp(13),cp(14),cp(32),cp(2)+cp(3) pageoflow PGOF
			cp15throughCp24=cp(15)+cp(16)+cp(17)+cp(18)+cp(19)+cp(20)+cp(21)+cp(22)+cp(23)+cp(24)
			if cp15throughCp24<>0 then 
				pr #255,using 'form pos 61,10*c 10': ab$(11),ab$(12),ab$(13),ab$(14),ab$(15),ab$(16),ab$(17),ab$(18),ab$(19),ab$(20)
				pr #255,using 'form pos 59,10*n 10.2': cp(15),cp(16),cp(17),cp(18),cp(19),cp(20),cp(21),cp(22),cp(23),cp(24) pageoflow PGOF
			end if
			for j=1 to 32 : t(j)=t(j)+cp(j) : next j
			t(34)=t(34)+tdc(10)
			t(35)=t(35)+tdc(9)
			if foundone=1 then foundone=0: t(36)=t(36)+1
		end if
	loop while eno=heno
loop

PGOF: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
PrFinalTotals: ! r: EoF target
	pr #255:''
	pr #255,using 'form pos 4,c 5,pos 11,16*n 10.2,n 5': "Total",t(31),t(2)+t(3),t(1),t(4),t(5),t(6),t(7),t(8), t(9),t(10),t(11),t(12),t(13),t(14),t(32),t(2)+t(3) pageoflow PGOF
	if t(15)+t(16)+t(17)+t(18)+t(19)+t(20)+t(21)+t(22)+t(23)+t(24)<>0 then 
		pr #255,using 'form pos 59,10*n 10.2': t(15),t(16),t(17),t(18),t(19),t(20),t(21),t(22),t(23),t(24) pageoflow PGOF
	end if
	pr #255: ''
	pr #255: ''
	pr #255,using L820: "Calculated Tax Deposit:" ,"Medicare W/H",t(3),"SS Withholding",t(2),"Federal Withholding",t(1),"Employer's FICA Match",round(t(2)/ssr1*ssr2,2)+t(3),"Less EIC",-t(25),"Total Deposit",t(3)+t(2)+t(1)+round(t(2)/ssr1*ssr2,2)+t(3)-t(25) ! 2013
	L820: form pos 8,c 30,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 42,"----------",skip 1,pos 20,c 20,n 12.2,skip 1,pos 42,"=========="
	pr #255: ''
	pr #255,using 'form pos 8,c 40': "Summary of FICA Match by Department:"
	pr #255,using 'form pos 10,c 4,pos 25,c 15': "Dept", "FICA Match"
	pr #255: ''
	for j=1 to 999
		if deptot(j,2)<>0 then
			pr #255,using 'form pos 10,n 4,pos 25,n 10.2': j, round(deptot(j,2),2) ! 2013
			gtotal=gtotal+round(deptot(j,2),2) ! 2013
		end if
	next j
	pr #255,using L940: "Total",gtotal
	L940: form pos 25,"__________",skip 1,pos 10,c 6,pos 23,n 12.2,skip 1,pos 25,"=========="
	pr #255,using "form skip 2,pos 1,c 40": "Total Employees: "&str$(t(36))
	fncloseprn
	close #1: ioerr ignore
goto XIT ! /r
PrHeader: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&"  To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
	pr #255: "\ql   "
	! pr #255,Using 1060: TIME$,"From ",BEGD," To ",ENDD
	pr #255,using L1080: "Date","     Gross","   FICA/ME","   Federal","     State",ab$(1),ab$(2),ab$(3),ab$(4),ab$(5),ab$(6),ab$(7),ab$(8),ab$(9),ab$(10),"   Net","  Emp FICA"
	L1080: form pos 3,c 4,pos 11,4*c 10,x 2,10*c 10,x 2,c 6,c 10,c 5,skip 2
return ! /r
XIT: fnxit
include: ertn
