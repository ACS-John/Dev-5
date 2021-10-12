! formerly S:\acsPR\newprRegTot
! beginning with 4.0 the tax deposit reads from the checkhistory file

autoLibrary
on error goto Ertn

dim deptot(999,2)

dim tx(36)

dim cp(32)

dim em$*30

fnTop(program$)

fnGetPayrollDates(beg_date,end_date)
ssr1=fnss_employee
ssr2=fnss_employer
! If FNPROCESS=1 Then Goto 410
fnTos
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
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
beg_date=val(resp$(1))
end_date=val(resp$(2))

fnopenprn

dim fullname$(20)*20
dim ab$(20)*8
fnDedNames(mat fullname$,mat ab$)
for j=1 to 20
	ab$(j)=lpad$(rtrm$(ab$(j)),8)
next j
gosub PrHeader
open #h_employee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],Shr",i,i,r
open #h_checks=fnH: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
do
	ReadEmployee: !
	read #h_employee,using "Form POS 1,N 8,C 30": eno,em$ eof PrFinalTotals
	checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
	foundone=1
	restore #h_checks,key>=checkkey$: nokey ReadEmployee
	do
		dim tdc(10)
		read #h_checks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,dep,prd,ckno,mat tdc,mat cp eof ReadEmployee
		if eno=heno and prd=>beg_date and prd<=end_date then
			if dep>0 then ! department 0 not totaled
				deptot(dep,1)+=cp(31)
				deptot(dep,2)+=cp(2)+cp(3)
			end if
			! pr #255: 'employee number: '&str$(heno)&' employee record: '&str$(rec(h_employee))&' check number: '&str$(ckno)&' check history record number: '&str$(rec(h_checks))
			pr #255,using 'form pos 1,pic(zzzz/zz/zz),pos 11,19*n 10.2,n 4': prd,cp(31),cp(2)+cp(3),cp(1),cp(4),cp(5),cp(6),cp(7),cp(8),cp(9),cp(10),cp(11),cp(12),cp(13),cp(14),cp(32),cp(2)+cp(3) pageoflow PgOf
			cp15throughCp24=cp(15)+cp(16)+cp(17)+cp(18)+cp(19)+cp(20)+cp(21)+cp(22)+cp(23)+cp(24)
			if cp15throughCp24<>0 then
				pr #255,using 'form pos 61,10*c 10': ab$(11),ab$(12),ab$(13),ab$(14),ab$(15),ab$(16),ab$(17),ab$(18),ab$(19),ab$(20)
				pr #255,using 'form pos 59,10*n 10.2': cp(15),cp(16),cp(17),cp(18),cp(19),cp(20),cp(21),cp(22),cp(23),cp(24) pageoflow PgOf
			end if
			for j=1 to 32 : tx(j)+=cp(j) : next j
			tx(34)+=tdc(10)
			tx(35)+=tdc(9)
			if foundone=1 then foundone=0: tx(36)+=1
		end if
	loop while eno=heno
loop

PgOf: ! r:
	pr #255: newpage
	gosub PrHeader
continue ! /r
PrFinalTotals: ! r: EoF target
	pr #255:''
	pr #255,using 'form pos 4,c 5,pos 11,16*n 10.2,n 5': "Total",tx(31),tx(2)+tx(3),tx(1),tx(4),tx(5),tx(6),tx(7),tx(8), tx(9),tx(10),tx(11),tx(12),tx(13),tx(14),tx(32),tx(2)+tx(3) pageoflow PgOf
	if tx(15)+tx(16)+tx(17)+tx(18)+tx(19)+tx(20)+tx(21)+tx(22)+tx(23)+tx(24)<>0 then
		pr #255,using 'form pos 59,10*n 10.2': tx(15),tx(16),tx(17),tx(18),tx(19),tx(20),tx(21),tx(22),tx(23),tx(24) pageoflow PgOf
	end if
	pr #255: ''
	pr #255: ''
	pr #255,using L820: "Calculated Tax Deposit:" ,"Medicare W/H",tx(3),"SS Withholding",tx(2),"Federal Withholding",tx(1),"Employer's FICA Match",round(tx(2)/ssr1*ssr2,2)+tx(3),"Less EIC",-tx(25),"Total Deposit",tx(3)+tx(2)+tx(1)+round(tx(2)/ssr1*ssr2,2)+tx(3)-tx(25) ! 2013
	L820: form pos 8,c 30,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 10,c 30,n 12.2,skip 1,pos 42,"----------",skip 1,pos 20,c 20,n 12.2,skip 1,pos 42,"=========="
	pr #255: ''
	pr #255,using 'form pos 8,c 40': "Summary of FICA Match by Department:"
	pr #255,using 'form pos 10,c 4,pos 25,c 15': "Dept", "FICA Match"
	pr #255: ''
	for j=1 to 999
		if deptot(j,2)<>0 then
			pr #255,using 'form pos 10,n 4,pos 25,n 10.2': j, round(deptot(j,2),2)
			gtotal+=round(deptot(j,2),2)
		end if
	next j
	pr #255,using L940: "Total",gtotal
	L940: form pos 25,"__________",skip 1,pos 10,c 6,pos 23,n 12.2,skip 1,pos 25,"=========="
	pr #255,using "form skip 2,pos 1,c 40": "Total Employees: "&str$(tx(36))
	fncloseprn
	close #1: ioerr ignore
goto Xit ! /r
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
Xit: fnXit
include: Ertn
