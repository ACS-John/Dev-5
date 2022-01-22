autoLibrary
on error goto Ertn

fnTop(program$)
open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,i,k
open #hDd=fnH: 'Name=[Q]\PRmstr\DD.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],Shr,kps=1,kln=10,Use',i,outIn,k
 
fnTos
fnLbl(1,35,'',1,1) ! bigger screen
fnLbl(2,1,'As of Date:',20,1)
fnTxt(2,20+3,10,0,1,'3',0,'This report will list any employees who direct deposit on the date the report is printed.')
resp$(1)=date$('mmddyy')
fnCmdKey('&Print',1,1,0,'Print the '&env$('program_caption')&'.' )
fnCmdKey('E&xit',5,0,1,'Returns to menu')
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
asOfDate=val(resp$(1))
fnopenprn
gosub PrHdr ! pr header
do
	ReadDD: !
	read #hDd,using 'form pos 1,C 10,C 1,N 9,N 2,N 17': key$,dd$,rtn,acc,acn eof Finis
	if uprc$(dd$)='Y' then   ! Y means Yes Direct Deposit is active for this person
		key$=lpad$(rtrm$(ltrm$(key$)),8)
		dim em$(3)*30 ! (1)=Emp Name, (2)=Emp Addr, (3)=Emp CSZ
		! em17  = Last Payroll Date (from first screen of employee record, not departmental record)
		read #hEmployee,using 'form pos 9,3*C 30,pos 162,N 6,pos 173',key=key$: mat em$,em17 nokey ReadDD
		pr #255,using 'form pos 1,c 40,n 14,n 4,n 17': key$&' '&em$(1),rtn,acc,acn pageoflow PrNewPg
	end if
loop
Finis: ! r:
	close #hDd: ioerr ignore
	close #hEmployee: ioerr ignore
	fncloseprn
goto Xit ! /r
PrNewPg: ! r:
	pr #255: newpage
	gosub PrHdr
continue ! /r
PrHdr: ! r:
	pr #255,using 'form pos 1,c 25': 'Page '&str$(pgNo+=1)&' '&date$
	pr #255: '\qc  {\f221 \fs22 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f201 \fs20 \b '&env$('program_caption')&'}'
	pr #255: '\qc  {\f181 \fs16 \b As of '&cnvrt$('pic(zzzz/zz/zz)',asOfDate)&'}'
	pr #255: '\ql   '
	pr #255: '   Emp   Name                                  Routing  C/S      Account'
return ! /r
Xit: fnXit
include: ertn
