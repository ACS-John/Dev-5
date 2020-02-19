! Replace S:\acsPR\newpr1099 (formerly)
! r: setup, fntop, open files, on error, etc
	library 'S:\Core\Library': fntop,fnxit, fnerror,fn1099print,fngethandle,fn1099print_close,fnask_1099_info
	on error goto Ertn
!
	dim vn$*8,nam$*30,empAddr$(3)*30,ss$*11,box(11)
	dim tcp(32),tdc(10)
	fntop(program$)
	open #hEmployee:=fngethandle: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed
	open #hChecks:=fngethandle: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
! /r
	if ~fnask_1099_info (seltp,type,min1,beg_date,end_date) then goto XIT
START: ! r: main loop
	mat empAddr$=("")
	read #hEmployee,using 'form pos 1,c 8,3*c 30,c 11': vn$,nam$,empAddr$(1),empAddr$(2),ss$ eof FINIS
	eno=val(vn$) ioerr START
	mat box=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #hChecks,key>=checkkey$: nokey START
	do
		read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof CALL_1099_LIBRARY
		if heno<>eno then goto CALL_1099_LIBRARY
		if prd=>beg_date and prd<=end_date then
			amt1=tcp(seltp+4)
			box(type)=box(type)+amt1
		end if
	loop
	CALL_1099_LIBRARY: !
	if box(type)=>min1 then
		fn1099print(cnvrt$("n 8",eno),nam$(1:18),mat empAddr$,trim$(ss$),mat box)
	end if
	goto START
! /r
FINIS: ! r:
	close #hEmployee: ioerr ignore
	close #hChecks: ioerr ignore
	seltp=type=min1=0
	vn$=nam$=""
	mat empAddr$=("")
	mat box=(0)
	fn1099print_close
	goto XIT ! /r
XIT: fnxit
include: ertn
