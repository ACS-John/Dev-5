! r: setup
autoLibrary
on error goto Ertn
fnTop(program$)
! /r
! r: body
	if ~fn1099NecAsk(seltp,type,min1,beg_date,end_date) then goto Xit
	open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,i,k
	open #hTrans=fnH: 'Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]',i,outIn,k
	do
		START: !
		dim empAddr$(3)*30
		mat empAddr$=('')
		dim vn$*8,nam$*30,ss$*11
		read #hEmployee,using 'form pos 1,c 8,3*c 30,c 11': vn$,nam$,empAddr$(1),empAddr$(2),ss$ eof EoEmployee
		eno=val(vn$) ioerr START
		dim box(11)
		mat box=(0)
		checkkey$=cnvrt$('pic(zzzzzzz#)',eno)&cnvrt$('pic(zz#)',0)&cnvrt$('pd 6',0) ! index employee#,department# and payroll date
		restore #hTrans,key>=checkkey$: nokey START
		do
			dim tcp(32),tdc(10)
			read #hTrans,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': heno,tdn,prd,ckno,mat tdc,mat tcp eof CALL_1099_LIBRARY
			if heno<>eno then goto CALL_1099_LIBRARY
			if prd=>beg_date and prd<=end_date then
				amt1=tcp(seltp+4)
				box(1)+=amt1 ! replaced type here with a 1 because box 1 is NonEmpCompensation
			end if
		loop
		CALL_1099_LIBRARY: !
		if box(type)=>min1 then
			fn1099NecPrint(cnvrt$('n 8',eno),nam$(1:18),mat empAddr$,trim$(ss$),mat box)
		end if
	loop
	EoEmployee: ! 
	close #hEmployee: ioerr ignore
	close #hTrans: ioerr ignore
	seltp=type=min1=0
	vn$=nam$=ss$=''
	mat empAddr$=('')
	mat box=(0)
	fn1099NecPrintClose
goto Xit ! /r
Xit: fnXit
include: ertn
