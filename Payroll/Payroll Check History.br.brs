! formerly S:\acsPR\checkhistory
! Payroll Check History
fn_setup
fntop(program$)

open #hEmployee:=fngethandle: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,outIn,keyed
if ~exists("[Q]\PRmstr\PayrollChecks.h[cno]") then
	open #hTmp:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],RecL=224,use",internal,outIn,keyed
	close #hTmp:
	gosub ReIndexPayrollChecks
else if ~exists("[Q]\PRmstr\checkidx.h[cno],Shr") then
	gosub ReIndexPayrollChecks
end if
open #hCheckIdx1:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno],Shr",internal,outIn,keyed
open #hCheckIdx3:=fngethandle: "Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr",internal,outIn,keyed
hact$=""
fn_checkfile(hact$,hCheckIdx3,hCheckIdx1,hEmployee)
close #hEmployee: ioerr ignore
close #hCheckIdx1: ioerr ignore
close #hCheckIdx3: ioerr ignore
gosub ReIndexPayrollChecks
Xit: fnxit
ReIndexPayrollChecks: ! r:
	fnindex_it('[Q]\PRmstr\PayrollChecks.h[cno]','[Q]\PRmstr\checkidx3.h[cno]','1/12/9 8/6/3')
	fnindex_it('[Q]\PRmstr\PayrollChecks.h[cno]','[Q]\PRmstr\checkidx.h[cno]','1 17')
	fnStatusClose
return ! /r


def library fncheckfile(hact$*8,hCheckIdx3,hCheckIdx1,hEmployee)
	if ~setup then let fn_setup
	fncheckfile=fn_checkfile(hact$,hCheckIdx3,hCheckIdx1,hEmployee)
fnend
def fn_checkfile(hact$*8,hCheckIdx3,hCheckIdx1,hEmployee)
	dim resp$(60)*128
	dim hf(46)
	dim hf$(46)
	dim gridname$*30
	dim oldgridname$*30

	! fntop(program$,cap$="Payroll Check History")
	cancel=5 : back=2 : ckEdit=1 : save=1 : disable=1
	add=4
	transtype$(1)="Check Only"
	transtype$(2)="Departmental Breakdowns"
	transtype$(3)="Grand Totals"
	transtype$(4)="Quarterly Totals"
	transtype$(5)="Annual Totals"
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	fnreg_read('Check History - enable long names when printing',pr_ckHstEnableLongNames$,'False')
	if pr_ckHstEnableLongNames$='True' then enableLongName=1 else enableLongName=0

	open #hGridName:=9: "Name=[Q]\PRmstr\GridNames.H[cno],USE,RecL=30",internal,outIn,relative
	if lrec(hGridName)=0 then
		oldgridname$= gridname$="[All]                         "
		write #hGridName,using "form pos 1,c 30",rec=1: gridname$
	end if
	dim dednames$(20)*20
	dim abrevname$(20)*8
	dim newdedcode(20)
	fnDedNames(mat dednames$,mat abrevname$,mat newdedcode)
	mat hf=(1)
	for j=1 to 20
		if trim$(dednames$(j))="" then hf(j+25)=0 ! default the (All) to only those deductions that have names
		dednames$(j)=trim$(dednames$(j))
	next j
	L320: !
	if exists("[Q]\PRmstr\payrollreports.H[cno]") =0 then 
		open #hPrReport:=fngethandle: "Name=[Q]\PRmstr\payrollreports.H[cno],KFName=[Q]\PRmstr\reportidx.H[cno],RecL=85,kps=1,kln=30,replace",internal,outIn,keyed
		! 1 - 30 Name c 30
		! 31 - 76 selections 46*n 1
		close #hPrReport:
	else if exists("[Q]\PRmstr\reportidx.H[cno]") =0 then
		fnindex_it('[Q]\PRmstr\payrollreports.H[cno]','[Q]\PRmstr\reportidx.H[cno]','1 30')
		fnStatusClose
	end if
	
	
	open #hPrReport:=fngethandle: "Name=[Q]\PRmstr\payrollreports.H[cno],KFName=[Q]\PRmstr\reportidx.H[cno],Shr",internal,outIn,keyed
	if kln(hPrReport)<>30 then
		close #hPrReport:
		fnFree('[Q]\PRmstr\payrollreports.H[cno]')
		fnFree('[Q]\PRmstr\reportidx.H[cno]')
		goto L320
	end if
	justopen=1 : gosub SelectColumns : justopen=0
	SCREEN1: ! r:
		qtr1printed=qtr2printed=qtr3printed=qtr4printed=0
		mat annualtdc=(0): mat annualtcp=(0): mat employeetdc=(0): mat employeetcp=(0)
		mat qtr1tcp=(0): mat qtr2tcp= (0): mat qtr3tcp=(0): mat qtr4tcp=(0)
		mat tcp=(0): mat tdc=(0)
		mat qtr1tdc=(0): mat qtr2tdc=(0): mat qtr3tdc=(0): mat qtr4tdc=(0)
		holdeno=holdckno=holdtdn=holdprd=eno=prd=ckno=tdn=0
		mat totaltdc=(0): mat totaltcp=(0) : mat grand2tcp=(0) : : mat grand2tdc=(0)
		goto ScrFilters ! fnScrFilters(CKEY,SEL_CODE,BEG_DATE,END_DATE,Z$,HACT$)
		Screen1B: !
		printit=0 
		if ckey=1 then 
			goto SCREEN2
		else if ckey=2 then 
			printit=1
			! if trim$(z$)="" or trim$(z$)="[All]" then goto SCREEN1 ! don't allow pr to work if no customer selected
			fnopenprn
			goto PastFlexInit ! read headings for reports then start reading thru the checks same as a grid
		else if ckey=cancel then 
			goto CheckFileXit
		end if
	goto SCREEN2 ! /r
	SCREEN2: ! r:
		fnTos(sn$="CheckHistory-2")
		gosub FlexGrid
		fnCmdKey('&Edit',ckEdit,1,0)
		fnCmdKey('&Back',back,0,0)
		fnCmdKey('&Add',add,0,0)
		fnCmdKey('&Close',cancel,0,1)
		fnAcs(sn$,0,mat resp$,ckey) ! check history building grid
		addcode=0 : holdeno=0
		if ckey=back then goto SCREEN1
		if ckey=cancel then goto SCREEN1
		if ckey=ckEdit and checkonly=1 then
			mat mg$(3)
			mg$(1)="You cannot edit a record when you have selected to display "
			mg$(2)="by Departmental Details.  You must select Check only"
			mg$(3)="before you can make changes."
			fnmsgbox(mat mg$,resp$,cap$,0)
			goto SCREEN1
		else if ckey=ckEdit then
			editrec=val(resp$(1)) conv SCREEN2
			mat employeetdc=(0)
			mat employeetcp=(0)
			goto SCREEN3
		else if ckey=add then
			if trim$(hact$)<>'[All]' then eno=val(hact$) else eno=0
			tdn=prd=ckno=0 : mat tdc=(0) : mat tcp=(0) : addcode=1
			goto SCREEN3_ADD
		end if
	goto SCREEN2 ! /r
	SCREEN3: ! r:
		read #hCheckIdx3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",rec=editrec: heno,tdn,prd,ckno,mat tdc,mat tcp noRec SCREEN2
		mat holdtdc=tdc: mat holdtcp=tcp ! hold arrays to see if dollar changes
	SCREEN3_ADD: !
		fnTos(sn$="CheckHistory-3")
		lc=rc=0 : mylen=20 : mypos=mylen+3
		if addcode=1 then disablecode=0 else disablecode=1
		fnLbl(lc+=1,1,"Employee #:",mylen,1)
		if disablecode then
		fnTxt(lc,mypos,8,0,0,"30",disablecode)
		else
			fncmbemp(lc,mypos,1)
		end if
		resp$(rc+=1)=str$(heno)
		fnLbl(lc+=1,1,"Department:",mylen,1)
		fnTxt(lc,mypos,3,0,0,"",0)
		resp$(rc+=1)=str$(tdn)
		fnLbl(lc+=1,1,"Date:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"3")
		resp$(rc+=1)=str$(prd)
		fnLbl(lc+=1,1,"Check #:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"30")
		resp$(rc+=1)=str$(ckno)
		fnLbl(lc+=1,1,"Regular Hours:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"32")
		resp$(rc+=1)=str$(tdc(1))
		fnLbl(lc+=1,1,"Overtime Hours:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"32")
		resp$(rc+=1)=str$(tdc(2))
		fnLbl(lc+=1,1,"Sick Hours:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"32")
		resp$(rc+=1)=str$(tdc(3))
		fnLbl(lc+=1,1,"Vacation Hours:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"32")
		resp$(rc+=1)=str$(tdc(4))
		fnLbl(lc+=1,1,"Holiday Hours:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"32")
		resp$(rc+=1)=str$(tdc(5))
		fnLbl(lc+=1,1,"Regular Earnings:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(26))
		fnLbl(lc+=1,1,"Overtime Earnings:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(27))
		fnLbl(lc+=1,1,"Other Compensation:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(28))
		fnLbl(lc+=1,1,"Meals:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(29))
		fnLbl(lc+=1,1,"Tips:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(30))
		fnLbl(lc+=1,1,"Total Wage:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(31))
		fnLbl(lc+=1,1,"Net Pay:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(32))
		fnLbl(lc+=2,1,"Workmans Comp Wages:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tdc(6))
		fnLbl(lc+=1,1,"SS Wages:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tdc(7))
		fnLbl(lc+=1,1,"Medicare Wages:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tdc(8))
		fnLbl(lc+=1,1,"Federal U/C Wages:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tdc(9))
		fnLbl(lc+=1,1,"State U/C Wages:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tdc(10))

		mypos+=37 : lc=2
		fnLbl(lc+=1,38,"Federal Wh:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(1))
		fnLbl(lc+=1,38,"SS Withholdings:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(2))
		fnLbl(lc+=1,38,"Medicare Wh:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(3))
		fnLbl(lc+=1,38,"State Wh:",mylen,1)
		fnTxt(lc,mypos,10,0,0,"10")
		resp$(rc+=1)=str$(tcp(4))
		for j=1 to 20
			if trim$(dednames$(j))="" then
				fnLbl(lc+=1,38,dednames$(j)&" ",mylen,1)
			else
				fnLbl(lc+=1,38,dednames$(j)&":",mylen,1)
			end if
			fnTxt(lc,mypos,10, 0,0,"10",fn_isBlank(dednames$(j)))
			resp$(rc+=1)=str$(tcp(j+4))
		next j
		fnCmdKey('&Save',save,1,0)
		fnCmdKey('&Delete',4,0,0)
		fnCmdKey('&Cancel',cancel,0,1)
		fnAcs(sn$,0,mat resp$,ckey) ! correcting check
		if ckey=cancel then
			goto SCREEN2
		else if ckey=4 then
			mat mg$(3)
			mg$(1)="You are deleting a check.  This will change the "
			mg$(2)="earnings. It will change the quarterly and annual reports."
			mg$(3)="Click OK to delete; else Cancel to retain the record."
			fnmsgbox(mat mg$,resp$,cap$,49)
			if resp$="OK" then delete #hCheckIdx3,rec=editrec: : eno=holdeno
			goto SCREEN2
		end if
		! r: get local values from mat resp$
		heno   =val(resp$(1) (1:8) ) ! employee #
		tdn    =val(resp$(2)       ) ! dept #
		prd    =val(resp$(3)       ) ! date
		ckno   =val(resp$(4)       ) ! check #
		tdc(1) =val(resp$(5)       ) ! reg hrs
		tdc(2) =val(resp$(6)       ) ! ot hrs
		tdc(3) =val(resp$(7)       ) ! sick hrs
		tdc(4) =val(resp$(8)       ) ! vac hrs
		tdc(5) =val(resp$(9)       ) ! hol hrs
		tcp(26)=val(resp$(10)      ) ! reg pay
		tcp(27)=val(resp$(11)      ) ! ot pay
		tcp(28)=val(resp$(12)      ) ! other comp
		tcp(29)=val(resp$(13)      ) ! Meals
		tcp(30)=val(resp$(14)      ) ! Tips
		tcp(31)=val(resp$(15)      ) ! Total Pay
		tcp(32)=val(resp$(16)      ) ! Net pay
		tdc(6) =val(resp$(17)      ) ! Workman comp wages
		tdc(7) =val(resp$(18)      ) ! ss wages
		tdc(8) =val(resp$(19)      ) ! Medicare wages
		tdc(9) =val(resp$(20)      ) ! Federal uc
		tdc(10)=val(resp$(21)      ) ! state uc
		tcp(1) =val(resp$(22)      ) ! fed wh
		tcp(2) =val(resp$(23)      ) ! ss  wh
		tcp(3) =val(resp$(24)      ) ! med wh
		tcp(4) =val(resp$(25)      ) ! state wh
		for j=1 to 20
			tcp(j+4)=val(resp$(j+25)) ! std deductions
		next j
		! /r
		if addcode=1 then
			testNet=tcp(32)
			testGross=tcp(31)
			testNetCalculated=testGross
			for testDeductionItem=1 to udim(mat dednames$)
				if newdedcode(testDeductionItem)=1 then ! deduction
					testNetCalculated-=tcp(testDeductionItem)
				else if newdedcode(testDeductionItem)=2 then ! addition
					testNetCalculated+=tcp(testDeductionItem)
				else if newdedcode(testDeductionItem)=3 then ! benefit
					! no change
				end if
			next testDeductionItem
			if testNetCalculated<>testNet then
				mat mg$(0)
				fnAddOneC(mat mg$,'The calculated Net Amount of the check is '&str$(testNetCalculated)&',')
				fnAddOneC(mat mg$,'but the Net Amount entered is '&str$(testNet)&'.')
				fnAddOneC(mat mg$,'Please correct to continue.')
				fnmsgbox(mat mg$,resp$,cap$,0+48) ! ok + excl
				goto SCREEN3_ADD
			else
				write #hCheckIdx3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp
				addcode=0
				goto SCREEN2
			end if
		end if
		if sum(holdtdc)<>sum(tdc) or sum(holdtcp)<>sum(tcp) then
			mat mg$(3)
			mg$(1)="You have changed dollar amounts on a real check! "
			mg$(2)="This will change the quarterly and annual reports.."
			mg$(3)="Click OK to continue; else Cancel to exit without saving the changes."
			fnmsgbox(mat mg$,resp$,cap$,49)
			if resp$="OK" then
				gosub Screen3Save
			end if
		else if ckey=save then
			gosub Screen3Save
		end if
	goto SCREEN2 ! /r
	Screen3Save: ! r:
		rewrite #hCheckIdx3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",rec=editrec: heno,tdn,prd,ckno,mat tdc,mat tcp: eno=heno
	return ! /r
	ScrFilters: ! r:
		fnTos(sn$="CHECKhISTORY")
		rc=cf=0
		fnFra(1,1,3,26,"Information to be Shown","You can choose to have the checks listed as one total or have the department breakdowns shown.  You cannot select both!",0)
		cf+=1 : fratype=cf
		fnOpt(1,3,"Departmental Details",0,fratype)
		if checkonly=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnOpt(2,3,"Check only",0,fratype)
		if details=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		if details=0 and checkonly=0 then resp$(rc)="True"
		fnFra(6,1,6,26,"Print options","You can get totals by any combination of the following options.",0)
		cf+=1 : fratype=cf
		fnChk(1,3,"Grand Totals",0,fratype)
		if grand=1 then resp$(rc+=1)="True" else             resp$(rc+=1)="False"
		fnChk(2,3,"Quarterly Totals",0,fratype)
		if quarterly=1 then resp$(rc+=1)="True" else         resp$(rc+=1)="False"
		fnChk(3,3,"Annual Totals",0,fratype)
		if annual=1 then resp$(rc+=1)="True" else            resp$(rc+=1)="False"
		fnChk(4,3,"Employee Totals",0,fratype)
		if employee=1 then resp$(rc+=1)="True" else          resp$(rc+=1)="False"
		fnFra(1,30,6,42,"Date Range","You can transactions for any date range or leave these blank to see all transactions.")
		cf+=1 : fradate=cf : mylen=26 : mypos=mylen+2
		fnLbl(1,1,"Starting Date:",mylen,1,0,fradate)
		fnTxt(1,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(beg_date)
		fnLbl(2,1,"Ending Date:",mylen,1,0,fradate)
		fnTxt(2,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(end_date)
		fnLbl(3,1,"1st Day of 1st quarter:",mylen,1,0,fradate)
		fnTxt(3,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(qtr1)
		fnLbl(4,1,"1st Day of 2nd quarter:",mylen,1,0,fradate)
		fnTxt(4,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(qtr2)
		fnLbl(5,1,"1st Day of 3rd quarter:",mylen,1,0,fradate)
		fnTxt(5,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(qtr3)
		fnLbl(6,1,"1st Day of 4th quarter:",mylen,1,0,fradate)
		fnTxt(6,mypos,10,0,1,"3",0,empty$,fradate)
		resp$(rc+=1)=str$(qtr4)
		fnFra(10,30,2,60,"Employee","You can review check information for all employees or for an individual.")
		cf+=1 : fraaccount=cf
		fnLbl(1,1,"Employee:",8,1,0,fraaccount)
		fncmbemp(1,10,1,fraaccount)
		rc_employee=rc+=1
		if trim$(hact$)<>"" then
			resp$(rc)=hact$
		else if resp$(rc)="" or trim$(resp$(rc))="True" then
			resp$(rc)="[All]"
		end if
		fnLbl(15,20,"Column format to use:",40,1)
		fncombof("payrollrpt",15,62,30,"[Q]\PRmstr\payrollreports.h[cno]",1,30,0,0,"[Q]\PRmstr\reportidx.h[cno]",0,pas, "",frame)
		resp$(rc+=1)=gridname$
		fnCmdKey("&Display Grid",1,1,0,"Displays a list of checks on the scree using the format you have selected.")
		fnCmdKey("&Print Report",2,0,0,"Prints a check listing using the columns selected.")
		fnCmdKey("&Maintain column selections",3,0,0,"Allows you to add or change columns that should be displayed.")
		fnCmdKey("&Back",5,0,1,"Returns to employee record")
		fnAcs(sn$,0,mat resp$,ckey) ! dates and options
		printit=0: f1=0
		if ckey=cancel then goto CheckFileXit
		checkonly=details=grand=quarterly=annual=employee=0 : holdnam$=""
		eno=holdeno=printeno=holdckno=printckno=0 : mat cp1=(0)
		if resp$(1)="True" then checkonly=1
		if resp$(2)="True" then details=1
		if resp$(3)="True" then grand=1
		if resp$(4)="True" then quarterly=1
		if resp$(5)="True" then annual=1
		if resp$(6)="True" then employee=1
		beg_date=val(resp$(7))
		end_date=val(resp$(8))
		qtr1=val(resp$(9))
		qtr2=val(resp$(10))
		qtr3=val(resp$(11))
		qtr4=val(resp$(12))
		z$=holdz$=hact$=resp$(rc_employee)(1:8) : z$=holdz$=hact$=lpad$(trim$(z$),8)
		qtr5=val(resp$(12)(1:4))*10000+1231
		begin_year=val(resp$(12)(1:4))*10000+0101
		end_year=val(resp$(12)(1:4))*10000+1231
		gridname$=rpad$(trim$(resp$(14)),30)
		rewrite #hGridName,using "form pos 1,c 30",rec=1: gridname$
		! if ckey<>3 and checkonly+details+grand+quarterly+annual+employee=0 then
		! 	mg$(1)="You must select at least one type of information to be shown. "
		! 	mg$(2)="                Click OK to correct."
		! 	fnmsgbox(mat mg$,resp$,cap$,0)
		! 	goto SCREEN1
		! end if

		if ckey=3 then gosub SelectColumns : goto ScrFilters
		justopen=1 : gosub SelectColumns : justopen=0
	goto Screen1B ! /r

	SelectColumns: ! r:
		dim dat$*20,scr1$(10)*30,alloc(10),nam$*30,cap$*128
		dim holdnam$*30
		dim name$(46)*11
		dim r(20,4),hd1$*255,serviceName$(10)*20,tg(11),end_date$*60,metraddr$*30
		dim cp0(45),cp1(45),cp2(45),hs1(45)
		name$(1)="Emp #"
		name$(2)="Dept"
		name$(3)="Date"
		name$(4)="Check #"
		name$(5)="Reg Hrs"
		name$(6)="OT Hrs"
		name$(7)="Sick Hrs"
		name$(8)="Vac Hrs"
		name$(9)="Hol Hrs"
		name$(10)="Reg Pay"
		name$(11)="OT Pay"
		name$(12)="Other Pay"
		name$(13)="Meals"
		name$(14)="Tips"
		name$(15)="Total Pay"
		name$(16)="Net Pay"
		name$(17)="W/C Wage"
		name$(18)="SS Wage"
		name$(19)="Med Wage"
		name$(20)="Fed UC Wage"
		name$(21)="St U/C Wage"
		name$(22)="Fed Wh"
		name$(23)="SS Wh"
		name$(24)="Med Wh"
		name$(25)="St Wh"
		for j=1 to 20
			name$(j+25)=dednames$(j)(1:11)
		next j
		name$(46)="EIC"
		OPEN_SELECTIONS: !
		if lrec(hGridName) then
			read #hGridName,using "form pos 1,c 30",rec=1: gridname$
			read #hPrReport,using "form pos 1,c 30,46*n 1",key=gridname$: gridname$,mat hf nokey L2600
			goto L2620
		end if
		write #hGridName,using "form pos 1,c 30",rec=1: gridname$
		L2600: !
		oldgridname$= gridname$="[All]                         "
		write #hPrReport,using "form pos 1,c 30,46*n 1": gridname$,mat hf
		L2620: !
		oldgridname$=gridname$
		resp$(1)=gridname$
		for j=1 to udim(hf)
			if hf(j)=1 then resp$(j+1)="True"
			hf$(j)=rtrm$(name$(j))&r$
		next j
		if justopen=1 then goto DoJustOpen
		SelectColumnsTos: !
		fnTos(sn$="Checkprint")
		rc=cf=0 : linecnt=2
		fnLbl(1,1,"Grid or Report Name:",20,1)
		fncombof("payrollrpt",1,22,30,"[Q]\PRmstr\payrollreports.h[cno]",1,30,0,0,"[Q]\PRmstr\reportidx.h[cno]",0,pas, "",frame)
		resp$(rc+=1)=resp$(1)
		for j=1 to 23
			fnChk(linecnt+=1,16,name$(j),1,rratype)
			if hf(j)=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		next j
		linecnt=2
		for j=1 to 22
			fnChk(linecnt+=1,35,name$(j+23),1,rratype)
			if hf(j+23)=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		next j
		fnChk(linecnt+=1,35,name$(46),1,rratype)
		if hf(46)=1 then resp$(rc+=1)="True" else resp$(rc+=1)="False"
		fnCmdKey("&Next",1,1,0,"Begins printing your report.")
		if addone=0 then let fnCmdKey("&Add",2,0,0,"Allows you to add another report or grid format..")
		if addone=1 then let fnCmdKey("&Save This Format",4,0,0,"Save this format for later use.")
		fnCmdKey("&Use This Format",3,0,0,"Use the format as now displayed.")
		fnCmdKey("&Cancel",5,0,1,"Cancel without saving the format selections.")
		fnAcs(sn$,0,mat resp$,ckey)
		if ckey<>5 then
			addone=0
			if ckey=2 then
				addone=1
				gosub AddGrid
				goto SelectColumnsTos
			else
				gridname$=rpad$(trim$(resp$(1)),30)
				if oldgridname$<>gridname$ then
					read #hPrReport,using "form pos 1,c 30,46*n 1",key=gridname$: gridname$,mat hf nokey SelectColumnsXit
					goto L2620
				end if
				for j=1 to 46
					if resp$(j+1)="True" then hf(j)=1 else hf(j)=0
				next j
				DoJustOpen: !
				dim hfm$*500
				dim hd$*500
				dim ul$*500
				if enableLongName then 
					hfm$="FORM POS 1,c 25"
					ul$=hd$="                         "
				else
						hfm$="FORM POS 1,c 12"
						ul$=hd$="            "
				end if 
				hs1=0: hs2=0
				for j=1 to udim(hf$)
					if hf(j)<>0 then
						hs2=hs2+1
						if j=1 then ! employee #
							hfm$=hfm$&",NZ 8"             : hs1=hs1+8  : hz1=hs2 : ul$=ul$&" -------"    : hs3=8
						else if j=2 then ! dept #
							hfm$=hfm$&",NZ 5"             : hs1=hs1+5  : hz1=hs2 : ul$=ul$&" ----"       : hs3=5
						else if j=3 then ! date
							hfm$=hfm$&",pic(bzzzz/zz/zz)" : hs1=hs1+11 : hz1=hs2 : ul$=ul$&" ----------" : hs3=11
						else if j=4 then ! check number
							hfm$=hfm$&",NZ 7"             : hs1=hs1+7  : hz1=hs2 : ul$=ul$&" ------"     : hs3=7
						else if j>4 and j<10 then ! hours
							hfm$=hfm$&",G 8.2"            : hs1=hs1+8            : ul$=ul$&" -------"    : hs3=8
						else if j>9 and j<17 then ! wages
							hfm$=hfm$&",G 10.2"           : hs1=hs1+10           : ul$=ul$&" ---------"  : hs3=10
						else if j>16 and j<47 then ! deductions
							hfm$=hfm$&",G 10.2"           : hs1=hs1+10           : ul$=ul$&" ---------"  : hs3=10
						end if
						hd$=hd$&lpad$(trim$(name$(j)(1:hs3-1)),hs3)
					end if
				next j
				mat cp0(hs2)
				mat cp1(hs2)
				mat cp2(hs2)
				rewrite #hGridName,using "form pos 1,c 30",rec=1: gridname$
				rewrite #hPrReport,using "form pos 1,c 30,46*n 1",key=oldgridname$: gridname$,mat hf
				f1=0
				if ckey=10 then goto SelectColumns
			end if
		end if
		SelectColumnsXit: !
	return ! /r

	PrintDetails: ! r:
	dim desc$*25,ds$*25
		if f1=0 then gosub Hdr
		if printit=0 and employee=1 and holdeno>0 and checkonly=0 and holdeno><eno then gosub EmployeeTotal
		mat cp0=(0)
		ds$=item$(2)
		for j=1 to 45
			hs1(j)=val(item$(j+2))
		next j
		form pos 1,n 8,n 3,pd 6,n 7,5*pd 3.2,37*pd 5.2
		hs3=0
		for j=1 to udim(mat hs1)
			if j=1 and eno<>holdeno then
				empz$=lpad$(str$(hs1(1)),8)
				nam$=''
				read #hEmployee,using "form pos 9,c 25",key=empz$: nam$ nokey ignore
			end if
			if hf(j)<>0 then
				hs3+=1
				cp0(hs3)=hs1(j)
				if eofcode=1 and grand=1 and desc$="Grand Total" then goto L3330 ! skip accumulating if finished
				if trim$(desc$)="1st Qtr" or trim$(desc$)="2nd Qtr" or trim$(desc$)="3rd Qtr" or trim$(desc$)="4th Qtr" or trim$(desc$)="YTD" or trim$(desc$)="Grand Total" or trim$(desc$)="Employee Total" then goto L3330
				cp2(hs3)+=hs1(j) ! accumulate totals
				L3330: !
			end if
		next j
		if enableLongName then
			let namelen=25
		else
			let namelen=12
		end if 
		if trim$(nam$)<>"" and holdnam$<>nam$ then desc$=nam$(1:namelen) : holdnam$=nam$ : nam$=""
		! if trim$(desc$)="Total Ck" then goto L3360 ! don't pr total check on printout
		! L3360: !
		pr #255,using hfm$: desc$(1:namelen),mat cp0
		if desc$(1:12)="Employee Tot" then pr #255:
		desc$=""
		mat cp1=cp1+cp0
		! Mat CP2=CP2+CP0  kj 1/30/08
		mat cp0=(0)
	return ! /r
	PgOf: ! r:
		pr #255: newpage
		gosub Hdr
	continue ! /r

	Hdr: ! r:
		! need date$,time$
		pr #255: "\qc  {\f181 \fs20 \b "&trim$(env$('cnam'))&" }"
		pr #255: "\qc  {\f181 \fs28 \b "&trim$(gridname$)&" }"
		if beg_date<>0 and end_date<>0 then
			pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)& "  To "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
		end if
		pr #255: ""
		pr #255: "\ql "
		pr #255: hd$
		pr #255: ul$ ! pause
		f1=1
	return ! /r

	GrandTotal: ! r:
		if grand then
			if printit=1 then pr #255: ul$
			if printit=1 then pr #255: "     <Grand Totals>"
			if hf(1)=1 then cp2(1)=0 ! no totals on employee numbers
			if hf(1)=1 and hf(2)=1 then cp2(2)=0 ! no totals on departments
			if hf(1)=0 and hf(2)=1 then cp2(1)=0 ! no totals on departments
			if hf(1)=1 and hf(2)=1 and hf(3)=1 then cp2(3)=0 ! no totals on date
			if hf(1)=0 and hf(2)=1 and hf(3)=1 then cp2(2)=0 ! no totals on date
			if hf(1)=1 and hf(2)=0 and hf(3)=1 then cp2(2)=0 ! no totals on date
			if hf(1)=0 and hf(2)=0 and hf(3)=1 then cp2(1)=0 ! no totals on date
			if hf(1)=1 and hf(2)=1 and hf(3)=1 and hf(4)=1 then cp2(4)=0 ! no totals on ck num
			if hf(1)=0 and hf(2)=0 and hf(3)=1 and hf(4)=1 then cp2(2)=0 ! no totals on ck num ! new 11/9/15 could this be right?
			if hf(1)=0 and hf(2)=0 and hf(3)=0 and hf(4)=1 then cp2(1)=0 ! no totals on cknum
			if hf(1)=1 and hf(2)=0 and hf(3)=1 and hf(4)=1 then cp2(3)=0 ! no totals on cknum
			if printit=1 then pr #255,using hfm$: "",mat cp2
			if printit=0 then desc$="Grand Total": mat totaltcp=grand2tcp: mat totaltdc=grand2tdc: gosub PrintGrid
			mat grand2tcp=(0) : mat grand2tdc=(0)
			mat cp2=(0)
			form pos 1,c 29,18*g 10.2,skip 1
			if printit=1 then pr #255: ul$
		end if
	return ! /r

	EmployeeTotal: ! r:
		if hf(1)=1 then cp1(1)=0 ! no totals on employee numbers
		if hf(1)=1 and hf(2)=1 then cp1(2)=0 ! no totals on departments
		if hf(1)=0 and hf(2)=1 then cp1(1)=0 ! no totals on departments
		if hf(1)=1 and hf(2)=1 and hf(3)=1 then cp1(3)=0 ! no totals on date
		if hf(1)=1 and hf(2)=0 and hf(3)=1 then cp1(2)=0 ! no totals on date
		if hf(1)=0 and hf(2)=1 and hf(3)=1 then cp1(2)=0 ! no totals on date
		if hf(1)=0 and hf(2)=0 and hf(3)=1 then cp1(1)=0 ! no totals on date
		if hf(1)=1 and hf(2)=1 and hf(3)=1 and hf(4)=1 then cp1(4)=0 ! no totals on ck num
		if hf(1)=0 and hf(2)=1 and hf(3)=1 and hf(4)=1 then cp1(3)=0 ! no totals on cknum
		if hf(1)=0 and hf(2)=0 and hf(3)=1 and hf(4)=1 then cp1(2)=0 ! no totals on cknum

		pr #255,using hfm$: "   Emp Total",mat cp1
		mat cp1=(0)
		pr #255: "" pageoflow PgOf
	return ! /r

	FlexGrid: ! r:
		dim colmask$(48),colhdr$(48)*20
		
		dim transtype$(5)*40,tcp(32)
		dim totaltcp(32),totaltdc(10)
		dim holdtotaltcp(32)
		dim holdtotaltdc(10)
		dim tdc(10)
		dim qtr1tdc(10),qtr2tdc(10),qtr3tdc(10),qtr4tdc(10)
		dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
		dim annualtdc(10),annualtcp(32)
		dim mg$(2)*80
		
		
		dim employeetdc(10),employeetcp(32),holdtdc(10),holdtcp(32),grand2tcp(32)
		dim grand2tdc(10)

		if trim$(z$)="[All]" then z$=""
		if trim$(z$)<>"" then z$=lpad$(trim$(z$),8)
		mat colhdr$(48) : mat colmask$(48)
		x=2
		colhdr$(1)="Rec" : colhdr$(2)="Desc"
		colmask$(1)="30": colmask$(2)=""
		if hf(1)=1 then colhdr$(x+=1)=name$(1) : colmask$(x)="30" ! employee #
		if hf(2)=1 then colhdr$(x+=1)=name$(2) : colmask$(x)="30" ! dept #
		if hf(3)=1 then colhdr$(x+=1)=name$(3) : colmask$(x)="3" ! Payroll Date
		if hf(4)=1 then colhdr$(x+=1)=name$(4) : colmask$(x)="30" ! check stop
		if hf(5)=1 then colhdr$(x+=1)=name$(5) : colmask$(x)="32"
		if hf(6)=1 then colhdr$(x+=1)=name$(6) : colmask$(x)="32" ! ot hours
		if hf(7)=1 then colhdr$(x+=1)=name$(7) : colmask$(x)="32"
		if hf(8)=1 then colhdr$(x+=1)=name$(8) : colmask$(x)="32"
		if hf(9)=1 then colhdr$(x+=1)=name$(9) : colmask$(x)="32"
		if hf(10)=1 then colhdr$(x+=1)=name$(10) : colmask$(x)="10"
		if hf(11)=1 then colhdr$(x+=1)=name$(11) : colmask$(x)="10"
		if hf(12)=1 then colhdr$(x+=1)=name$(12) : colmask$(x)="10"
		if hf(13)=1 then colhdr$(x+=1)=name$(13) : colmask$(x)="10"
		for j=14 to 46
			if hf(j)=1 then colhdr$(x+=1)=name$(j) : colmask$(x)="10"
		next j
		mat colhdr$(x) : mat colmask$(x) : mat printitem$(x)
		fnflexinit1("prchecks",1,1,20,100,mat colhdr$,mat colmask$,1)
	PastFlexInit: !
		if trim$(hact$)="[All]" then restore #hCheckIdx3: : goto READ_BREAKDOWNS
		restore #hCheckIdx3,key>=lpad$(hact$,8)&cnvrt$("pd 6",beg_date)&"   ": nokey FlexGridXit ioerr FlexGridXit
	READ_BREAKDOWNS: !
		holdeno=eno
		holdckno=ckno
		holdtdn=tdn
		holdprd=prd
		L4310: !
		read #hCheckIdx3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",release: eno,tdn,prd,ckno,mat tdc,mat tcp eof EoChecks
		if trim$(hact$)<>"[All]" and hact$<>cnvrt$("pic(zzzzzzzz)",eno) then goto ConsiderAnnual
		if beg_date<>0 and prd<beg_date then goto L4310
		if end_date><0 and prd>end_date then goto L4310
		dim printitem$(48)*70
		if holdeno>0 and eno<>holdeno and trim$(hact$)<>"[All]" then goto EoChecks ! not same account and should treated as end of file
		if holdeno>0 and eno<>holdeno then goto ConsiderAnnual             ! not same account
		L4380: !
		if annual=1 and prd>=begin_year and prd<=end_year then mat annualtdc=annualtdc+tdc: mat annualtcp=annualtcp+tcp
		if prd>=beg_date and prd<=end_date then mat employeetdc=employeetdc+tdc: mat employeetcp=employeetcp+tcp : mat grand2tcp=grand2tcp+tcp: mat grand2tdc=grand2tdc+tdc
		! need to start analyzing quarters, etc here
		if quarterly<>0 then
			mat holdtotaltcp=totaltcp: mat holdtotaltdc=totaltdc ! hold these subtotals in case the quarterly destroys them
			if prd=>qtr2 and qtr1printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PrintGrid : holdckno=0 ! last check not printed yet
			if prd=>qtr2 and qtr1printed=0 and sum(qtr1tdc)+sum(qtr1tcp)>0 then mat totaltdc=qtr1tdc: mat totaltcp=qtr1tcp: qtr1printed=1: desc$="1st Qtr" : gosub PrintGrid : holdckno=0
			if prd=>qtr3 and qtr2printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PrintGrid ! last check not printed yet
			if prd=>qtr3 and qtr2printed=0 and sum(qtr2tdc)+sum(qtr2tcp)>0 then mat totaltdc=qtr2tdc: mat totaltcp=qtr2tcp: qtr2printed=1: desc$="2nd Qtr" : gosub PrintGrid : holdckno=0
			if prd=>qtr4 and qtr3printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PrintGrid ! last check not printed yet
			if prd=>qtr4 and qtr3printed=0 and sum(qtr3tdc)+sum(qtr3tcp)>0 then mat totaltdc=qtr3tdc: mat totaltcp=qtr3tcp: qtr3printed=1: desc$="3rd Qtr": gosub PrintGrid: holdckno=0
			if prd=>qtr5 and qtr4printed=0 and sum(totaltdc)+sum(totaltcp)<>0 then gosub PrintGrid ! last check not printed yet
			if prd>qtr5 and qtr4printed=0 and sum(qtr4tdc)+sum(qtr4tcp)>0 then mat totaltdc=qtr4tdc: mat totaltcp=qtr4tcp: qtr4printed=1: desc$="4th Qtr": gosub PrintGrid : goto ConsiderAnnual
			if prd>=qtr1 and prd<qtr2 then mat qtr1tdc=qtr1tdc+tdc : : mat qtr1tcp=qtr1tcp+tcp
			if prd>=qtr2 and prd<qtr3 then mat qtr2tdc=qtr2tdc+tdc : mat qtr2tcp=qtr2tcp+tcp
			if prd>=qtr3 and prd<qtr4 then mat qtr3tdc=qtr3tdc+tdc : mat qtr3tcp=qtr3tcp+tcp
			if prd>=qtr4 and prd=<qtr5 then mat qtr4tdc=qtr4tdc+tdc : mat qtr4tcp=qtr4tcp+tcp
		end if
		if checkonly=1 and holdckno=0 then
			mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp
			goto READ_BREAKDOWNS ! same check, no details
		else if checkonly=1 and holdckno<>0 and holdckno=ckno then
			mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp
			goto READ_BREAKDOWNS ! same check, no details
		else if checkonly=1 and holdckno<>0 and holdckno<>ckno and (sum(tdc)+sum(tcp))>0 then
			desc$="Total Ck"
			holdrecnum=0
			gosub PrintGrid
			mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp
			desc$="Total Ck"
		end if
		if details=1 then
			enoprint=eno
			tdnprint=tdn
			prdprint=prd
			cknoprint=ckno
			mat totaltdc=totaltdc+tdc
			mat totaltcp=totaltcp+tcp
		end if
		if details=1 then gosub PrintGrid
	goto READ_BREAKDOWNS ! /r
	PrintGrid: ! r:
		dim item$(48)*70
		recnum=rec(hCheckIdx3): if trim$(desc$)<>"" then recnum=0
		if trim$(desc$)="1st Qtr" or trim$(desc$)="2nd Qtr" or trim$(desc$)="3rd Qtr" or trim$(desc$)="4th Qtr" or trim$(desc$)="YTD" or trim$(desc$)="Grand Total" or trim$(desc$)="Employee Total" then
			enoprint=tdnprint=prdprint=cknoprint=0
			item$(1)=item$(3)=item$(4)=item$(5)=item$(6)="" : item$(2)=desc$
			desc$=""
		else
			if details=0 then enoprint=holdeno : tdnprint=holdtdn : prdprint=holdprd : cknoprint=holdckno
			if printit=1 then ! use different key for pr instead of grid
				employeekey$=cnvrt$("pic(zzzzzzzz)",eno)
			else if printit=1 or details=1 then  ! use different key for pr instead of grid
				employeekey$=cnvrt$("pic(zzzzzzzz)",eno)
			else ! key for grids
				employeekey$=cnvrt$("pic(zzzzzzzz)",holdeno)
			end if
			L4660: !
			if sum(totaltcp)=(0) and sum(totaltdc)=(0) then goto PrintGridXit
			read #hEmployee,using "form pos 9,c 25",key=employeekey$: desc$ nokey ignore
			item$( 1)=cnvrt$("pic(zzzzzzz)",recnum)
			item$( 2)=desc$
			item$( 3)=cnvrt$("pic(zzzzzzzz)",enoprint)
			item$( 4)=cnvrt$("pic(zzz)",tdnprint)
			item$( 5)=str$(prdprint)
			item$( 6)=cnvrt$("pic(zzzzzzz)",cknoprint)
		end if
		item$( 7)=str$(totaltdc( 1))
		item$( 8)=str$(totaltdc(2))
		item$( 9)=str$(totaltdc( 3))
		item$(10)=str$(totaltdc( 4))
		item$(11)=str$(totaltdc( 5))
		item$(12)=str$(totaltcp(26))
		item$(13)=str$(totaltcp(27))
		item$(14)=str$(totaltcp(28))
		item$(15)=str$(totaltcp(29))
		item$(16)=str$(totaltcp(30))
		item$(17)=str$(totaltcp(31))
		item$(18)=str$(totaltcp(32))
		item$(19)=str$(totaltdc( 6))
		item$(20)=str$(totaltdc( 7))
		item$(21)=str$(totaltdc( 8))
		item$(22)=str$(totaltdc( 9))
		item$(23)=str$(totaltdc(10))
		item$(24)=str$(totaltcp( 1))
		item$(25)=str$(totaltcp( 2))
		item$(26)=str$(totaltcp( 3))
		item$(27)=str$(totaltcp( 4))
		items=27
		for j=1 to 20
			item$(items+=1)=cnvrt$("pic(-------.zz)",totaltcp(j+4))
		next j
		item$(items+=1)=str$(totaltcp(25)) ! eic
		if printit=1 then desc$=item$(2): gosub PrintDetails: goto L4840
		x=2
		printitem$(1)=item$(1): printitem$(2)=item$(2)
		for j=1 to 46
			if hf(j)=1 then printitem$(x+=1)=item$(j+2)
		next j
		L4820: !
		fnflexadd1(mat printitem$)
		holdeno=eno
		holdckno=ckno
		holdtdn=tdn
		holdprd=prd
		L4840: !
		if repeatit=1 then 
			repeatit=0
		else
			if trim$(desc$)="1st Qtr" or trim$(desc$)="2nd Qtr" or trim$(desc$)="3rd Qtr" or trim$(desc$)="4th Qtr" then
				mat item$=("")
				repeatit=1
				goto L4820
			end if
		end if
		if trim$(printitem$(2))="Employee Total" then
			mat printitem$=(""): fnflexadd1(mat printitem$)
		end if
		mat totaltdc=(0): mat totaltcp=(0)
		desc$=""
		if qtr1printed=1 then
			qtr1printed=2
		else if qtr2printed=1 then
			qtr2printed=1
		else if qtr3printed=1 then
			qtr3printed=2
		else if qtr4printed=1 then
			qtr4printed=2
		end if
		PrintGridXit: !
	return ! /r
	EoChecks: ! r:
		eofcode=1
	goto ConsiderAnnual ! /r
	ConsiderAnnual: ! r:
		! If EOFCODE=1 AND EMPLOYEE=1 AND PRD>=BEG_DATE AND PRD<=END_DATE Then Mat EMPLOYEETDC=EMPLOYEETDC+TDC: Mat EMPLOYEETCP=EMPLOYEETCP+TCP : Mat GRAND2TCP=GRAND2TCP+TCP: Mat GRAND2TDC=GRAND2TDC+TDC
		! If EOFCODE=1 AND ANNUAL=1 AND PRD>=BEGIN_YEAR AND PRD<=END_YEAR Then Mat ANNUALTDC=ANNUALTDC+TDC: Mat ANNUALTCP=ANNUALTCP+TCP
		if sum(totaltdc)+sum(totaltcp)<>0 then gosub PrintGrid ! last check not printed yet
		if sum(totaltdc)+sum(totaltcp)<>0 and checkonly=1 then desc$="Total Ck"
		if (sum(qtr1tdc)>0 or sum(qtr1tcp)>0) and qtr1printed=0 then
			mat totaltdc=qtr1tdc: mat totaltcp=qtr1tcp
			qtr1printed=1: desc$="1st Qtr": holdnam$="": gosub PrintGrid
		end if
		if (sum(qtr2tdc)>0 or sum(qtr2tcp)>0) and qtr2printed=0 then
			mat totaltdc=qtr2tdc: mat totaltcp=qtr2tcp: qtr2printed=1
			desc$="2nd Qtr": holdnam$="": gosub PrintGrid
		end if
		if (sum(qtr3tdc)>0 or sum(qtr3tcp)>0) and qtr3printed=0 then
			mat totaltdc=qtr3tdc: mat totaltcp=qtr3tcp: qtr3printed=1
			desc$="3rd Qtr": holdnam$="" : gosub PrintGrid
		end if
		if (sum(qtr4tdc)>0 or sum(qtr4tcp)>0) and qtr4printed=0 then
			mat totaltdc=qtr4tdc: mat totaltcp=qtr4tcp: qtr4printed=1
			desc$="4th Qtr": holdnam$="": gosub PrintGrid
		end if
		if annual=1 then
			enoprint=tdnprint=prdprint=cknoprint=0
			mat totaltdc=annualtdc: mat totaltcp=annualtcp
			annual_printed=1 : desc$="YTD": gosub PrintGrid
			mat annualtcp=(0) : mat annualtdc=(0)
		end if
		! If PRINTIT=1 Then Goto 4810 ! don't use the totals if pr report
		! If PRINTIT=1 Then Gosub EmployeeTotal
		if employee=1 and holdeno<>0 then
			mat totaltdc=employeetdc
			mat totaltcp=employeetcp
			employee_printed=1
			desc$="Employee Total"
			gosub PrintGrid
			mat employeetcp=(0)
			mat employeetdc=(0)
		end if
		if eofcode=1 and grand=1 then gosub GrandTotal
		if eofcode=1 then eofcode=0: holdeno=0: goto L5120
		if trim$(hact$)="[All]" and quarterly=1 and holdeno>0 then
			qtr1printed=qtr2printed=qtr3printed=qtr4printed=0
			mat qtr1tcp  =(0) : mat qtr2tcp=(0) : mat qtr3tcp=(0) : mat qtr4tcp=(0)
			mat qtr1tdc  =(0) : mat qtr2tdc=(0) : mat qtr3tdc=(0) : mat qtr4tdc=(0)
			mat totaltdc =(0) : mat totaltcp=(0)
			mat annualtcp=(0)
			goto L4380
		end if
		if trim$(hact$)="[All]" then goto L4380
		L5120: !
		if printit=1 then let fncloseprn : goto SCREEN1
		FlexGridXit: !
	return ! /r
	AddGrid: ! r:
		fnTos(sn$="Addgrid")
		lc=rc=0 : mylen=20 : mypos=mylen+3
		fnLbl(1,1,"Grid or Report Name:",20,1)
		fnTxt(1,mypos,30,0,0,"")
		resp$(1)=""
		fnCmdKey('&Save',1,1,0,"Adds this new grid or report to your selections.")
		fnCmdKey('&Cancel',5,0,1,"Returns to selection screen without adding this report.")
		fnAcs(sn$,0,mat resp$,ckey) ! add grid name
		if ckey<>5 then
			oldgridname$=gridname$=rpad$(trim$(resp$(1)),30)
			rewrite #hGridName,using "form pos 1,c 30",rec=1: gridname$
			mat hf=(0)
			write #hPrReport,using "form pos 1,c 30,46*n 1": gridname$,mat hf
			mat resp$=(""): resp$(1)=gridname$
		end if
	return ! /r
	CheckFileXit: !
	close #hGridName: ioerr ignore
	close #hPrReport: ioerr ignore
fnend
def fn_setup
	if ~setup then
		setup=1
		on error goto Ertn

		library 'S:\Core\Library': fntop
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnindex_it,fnStatusClose
		library 'S:\Core\Library': fnTos,fnAcs,fnCmdKey
		library 'S:\Core\Library': fnxit
		library 'S:\Core\Library': fnFra,fnOpt,fnLbl,fnTxt
		library 'S:\Core\Library': fnCmdSet,fncmbact,fnChk
		library 'S:\Core\Library': fncmbemp,fncombof,fnButton
		library 'S:\Core\Library': fnopenprn,fncloseprn
		library 'S:\Core\Library': fnflexinit1,fnflexadd1
		library 'S:\Core\Library': fnmsgbox
		library 'S:\Core\Library': fnDedNames
		library 'S:\Core\Library': fnGetPayrollDates
		library 'S:\Core\Library': fnAddOneC
		library 'S:\Core\Library': fnFree
		library 'S:\Core\Library': fnreg_read

	end if
fnend
def fn_isBlank(text$*128; ___,returnN)
	if trim$(text$)='' then
		returnN=1
	end if
	fn_isBlank=returnN
fnend
include: ertn