! formerly S:\acsPR\newDDLIST
! Direct Deposit listing
	autoLibrary
	on error goto Ertn
 
 
	! em17  = Last Payroll Date (from first screen of employee record, not departmental record)
	dim em$(3)*30 ! (1)=Emp Name, (2)=Emp Addr, (3)=Emp CSZ
 
	fnTop(program$)
	open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
	open #dd=fnH: "Name=[Q]\PRmstr\DD.h[cno],RecL=72,KFName=[Q]\PRmstr\DDidx1.h[cno],Shr,kps=1,kln=10,Use",internal,outIn,keyed
 
	fnTos
	fnLbl(1,35,"",1,1) ! bigger screen
	fnLbl(2,1,"As of Date:",20,1)
	fnTxt(2,20+3,10,0,1,"3",0,"This report will list any employees who direct deposit on the date the report is printed.")
	resp$(1)=str$(d1)
	fnCmdKey("&Print",1,1,0,"Print the "&env$('program_caption')&"." )
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	ppd=val(resp$(1))
	fnopenprn
	gosub PrHdr ! pr header
	do
		ReadDD: !
		read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17": key$,dd$,rtn,acc,acn eof Finis
		if uprc$(dd$)="Y" then   ! Y means Yes Direct Deposit is active for this person
			key$=lpad$(rtrm$(ltrm$(key$)),8)
			read #hEmployee,using 'Form pos 9,3*C 30,Pos 162,N 6,Pos 173',key=key$: mat em$,em17 nokey ReadDD
			pr #255,using "form pos 1,c 40,n 14,n 4,n 17": key$&" "&em$(1),rtn,acc,acn pageoflow PrNewPg
		end if
	loop
Finis: ! r:
	close #dd: ioerr ignore
	close #hEmployee: ioerr ignore
	fncloseprn
goto Xit ! /r
PrNewPg: ! r:
	pr #255: newpage
	gosub PrHdr
continue ! /r
PrHdr: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b As of "&cnvrt$("pic(zzzz/zz/zz)",ppd)&"}"
	pr #255: "\ql   "
	pr #255: "   Emp   Name                                  Routing  C/S      Account"
return ! /r
Xit: fnXit
include: ertn
