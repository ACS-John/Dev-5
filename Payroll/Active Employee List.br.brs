	autoLibrary
	on error goto Ertn
 
	dim a$*40,em$(1)*30,ta(2),cp(22),tcp(22),hc(5),thc(5),whc(10)
	dim dedcode(10),calcode(10),dedfed(10),message$*40
 
	fnTop(program$)
 
	on fkey 5 goto DONE
 
	fnGetPayrollDates(beg_date,end_date)
	gosub ASKFORMAT
 
	open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],Shr",internal,input,relative
	F_employee: form pos 1,n 8,c 30,pos 162,n 6,pos 118,n 2
	open #5: "Name=[Temp]\Temp1."&session$&",RecL=66,Replace",internal,output
	fnopenprn
 
TOPOFLOOP: !
	read #hEmployee,using F_employee: eno,em$(1),lpd,em4 eof L340
	gosub L770
	if status=0 then goto L300 ! allow all to print
	if em4=status then goto TOPOFLOOP ! skip terminated employees
	L300: ! If fndate_mmddyy_to_ccyymmdd(LPD)><fnPayPeriodEndingDate Then Goto 260
	write #5,using L320: eno,last$,first$,mid$,lpd,em4
	L320: form pos 1,n 8,c 20,c 15,c 15,n 6,n 2
	goto TOPOFLOOP
	L340: !
	gosub HDR
	close #5:
	execute "INDEX [Temp]\Temp1."&session$&" [Temp]\TempIdx."&session$&" 9 50 Replace DupKeys"
	open #5: "Name=[Temp]\Temp1."&session$&",KFName=[Temp]\TempIdx."&session$,internal,outIn,keyed
	L390: !
	read #5,using L320: eno,last$,first$,mid$,lpd,em4 eof DONE
	pr #255,using L410: eno,trim$(first$)&" "&trim$(mid$)&" "&trim$(last$),lpd,em4 pageoflow PGOF
	L410: form pos 1,n 8,x 3,c 30,x 3,pic(zz/zz/zz),x 9,n 2,skip 1
goto L390
 
PGOF: ! r:
	pr #255: newpage
	gosub HDR
continue ! /r
 
HDR: ! r:
	pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
	pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs16 \b "&trim$(date$("Month DD, CCYY"))&"}"
	pr #255: "\ql   "
	pr #255,using "form pos 1,c 80": "Employee #  Name                      Last Payroll Date   Status"
return ! /r
DONE: !
	close #5: ioerr ignore
	fncloseprn
goto Xit
 
Xit: fnXit
 
L770: ! r:
	dim first$*15,mid$*15,last$*20,item1$(2)*30
	em$(1)=uprc$(rtrm$(em$(1)))
	x1=pos(em$(1)," ",1)
	x2=pos(em$(1)," ",x1+1)
	x3=pos(em$(1)," ",x2+1)
	if uprc$(namcde$)="L" then goto L870
	first$=trim$(em$(1)(1:max(x1-1,1)))
	if x2>0 then mid$=trim$(em$(1)(x1+1:x2-1)): last$=trim$(em$(1)(x2+1:len(em$(1))))
	if x2=0 then last$=trim$(em$(1)(x1+1:len(em$(1)))): mid$=""
	goto L910
	L870: ! last name first
	if x1>0 and em$(1)(x1-1:x1-1)="," then last$=trim$(em$(1)(1:x1-2)) else last$=trim$(em$(1)(1:max(x1-1,1)))
	if x2>0 then first$=trim$(em$(1)(x1+1:x2-1)): mid$=trim$(em$(1)(x2+1:len(em$(1))))
	if x2=0 then first$=trim$(em$(1)(x1+1:len(em$(1)))): mid$=""
	L910: ! pr FIRST$,MID$,LAST$
return ! /r
ASKFORMAT: ! r:
	fnTos(sn$="Emplist")
	respc=0
	fnLbl(1,1,"Order for Printing Name:",28,1)
	fi$="Emplist1"
	item1$(1)="First Name First"
	item1$(2)="Last Name First": fncomboa(fi$,1,31,mat item1$,"How is the employee name entered in the employee record?).")
	resp$(respc+=1)=item1$(1)
	fnLbl(2,1,"Status Code:",28,1)
	fncombof("EmpStatus",2,30,25,"[Q]\PRmstr\EmpStatus.dat",1,2,3,25,"[Q]\PRmstr\EmpStatus.idx",0,0, "Indicate the code used for terminated employees",fracustinfo,0)
	resp$(respc+=1)=str$(status)
	fnCmdKey("&Next",1,1,0,"Proceed with printing." )
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey) ! ask employee #
	if ckey=5 then goto Xit
	namcde$=resp$(1)(1:1)
	status=val(resp$(2)(1:2))
return ! /r
include: ertn
