! based on a stripped down version of Payroll Check History
fn_setup
fnTop(program$)
eno$=''
checkRec=0
pr 'fn returns ';fn_selectCheck(eno$,checkRec)
pr 'eno$='&eno$
pr 'checkRec=';checkRec
end
Xit: fnXit

def library fnSelectCheck(&eno$,&checkRec; hEmployee)
	if ~setup then fn_setup
	fnSelectCheck=fn_selectCheck(eno$,checkRec, hEmployee)
fnend
def fn_selectCheck(&eno$,&checkRec; hEmployee,___, _
	hEmployeePassed,returnN,gridname$*30,hCheckIdx3,hGridName)
	! r: setup dim, open(s), constants, dednames$, etc
	dim resp$(5)*128
	dim mg$(0)*128
	beg_date=date(days(date$)-int(365/2),'ccyymmdd') ! val(str$(date('ccyy')-1)&'0101')
	end_date=20391231
	if ~hEmployee then
		open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,outIn,k
	else
		hEmployeePassed=1
	end if
	open #hCheckIdx3=fnH: 'Name=[Q]\PRmstr\PayrollChecks.h[cno],KFName=[Q]\PRmstr\checkidx3.h[cno],Shr',i,i,k
	gridname$='[All]                         '
	dim dednames$(20)*20
	dim abrevname$(20)*8
	dim deductionCode(20)
	fnDedNames(mat dednames$,mat abrevname$,mat deductionCode)
	dim hf(46)
	dim hf$(46)
	mat hf=(1)
	for j=1 to 20
		if trim$(dednames$(j))='' then hf(j+25)=0 ! default the (All) to only those deductions that have names
		dednames$(j)=trim$(dednames$(j))
	next j

	hPrReport=fnOpenPrReport

	gosub SetGridHeaders
	! /r

	ScrFilters: ! r:
		mat tcp=(0): mat tdc=(0)
		holdeno=holdckno=holdtdn=holdprd=eno=prd=ckno=tdn=0
		fnTos
		rc=0
		fnLbl(1,1,'Select Employee who received the check you''d like to test the calculation of.',77,2)
		fnLbl(3,1,'Employee:',8,1)
		fnCmbEmp(3,12,1)
		resp$(rc_employee=rc+=1)=fnPcRegRead$('employee')
		! fnLbl(3,1,'Column format to use:',40,1)
		! fnComboF('payrollrpt',3,41,30,'[Q]\PRmstr\payrollreports.h[cno]',1,30,0,0,'[Q]\PRmstr\reportidx.h[cno]',0,pas, '',frame)
		! resp$(rc+=1)=gridname$
		fnCmdKey('&Next',1,1,0,'Continue to select a check from the choosen employee')
		! fnCmdKey('&Maintain column selections',3,0,0,'Allows you to add or change columns that should be displayed.')
		fnCmdKey('&Cancel',ckCancel=5,0,1,'Cancel check selection')
		ckey=fnAcs(mat resp$) ! dates and options
		f1=0
		if ckey=ckCancel then goto Finis
		fnPcReg_Write('employee',resp$(rc_employee))


		holdnam$=''
		eno=holdeno=holdckno=0 : mat cp1=(0)
		! always details=1
		z$=holdz$=eno$=lpad$(trim$(resp$(rc_employee)(1:8)),8)
		gosub SetGridHeaders
		if ckey=1 then
			goto ScreenSelectCheck
		else if ckey=ckCancel then
			goto Finis
		end if
	goto ScreenSelectCheck ! /r

	ScreenSelectCheck: ! r:
		fnTos
			mat resp$=('')
		gosub FlexGrid

		fnCmdKey('Select',ckSelect	=1,1,0)
		fnCmdKey('Back'  ,ckBack  	=2,0,0)
		fnCmdKey('Close' ,ckCancel	=5,0,1)
		ckey=fnAcs(mat resp$) ! check history building grid
		if ckey=ckBack then
			goto ScrFilters
		else if ckey=ckCancel then
			eno$=''
			checkRec=0
			returnN=0
			goto Finis
		else if ckey=ckSelect then
			checkRec=val(resp$(1)) ! conv ScreenSelectCheck
			! pr eno$,checkRec : pause
			returnN=checkRec
			goto Finis
		end if
	goto ScreenSelectCheck ! /r

	SetGridHeaders: ! r:
		dim cp0(45),cp1(45),cp2(45),hs1(45)
		dim name$(46)*11
		name$(1)='Emp #'
		name$(2)='Dept'
		name$(3)='Date'
		name$(4)='Check #'
		name$(5)='Reg Hrs'
		name$(6)='OT Hrs'
		name$(7)='Sick Hrs'
		name$(8)='Vac Hrs'
		name$(9)='Hol Hrs'
		name$(10)='Reg Pay'
		name$(11)='OT Pay'
		name$(12)='Other Pay'
		name$(13)='Meals'
		name$(14)='Tips'
		name$(15)='Total Pay'
		name$(16)='Net Pay'
		name$(17)='W/C Wage'
		name$(18)='SS Wage'
		name$(19)='Med Wage'
		name$(20)='Fed UC Wage'
		name$(21)='St U/C Wage'
		name$(22)='Fed Wh'
		name$(23)='SS Wh'
		name$(24)='Med Wh'
		name$(25)='St Wh'
		for j=1 to 20
			name$(j+25)=dednames$(j)(1:11)
		next j
		name$(46)='EIC'
		gridname$=resp$(1)='[All]                         '
		mat hf=(1)
		dim hfm$*500
		hfm$='form pos 1,c 25'
		dim hd$*500
		dim ul$*500
		ul$=hd$='                         '
		hs1=0: hs2=0
		for j=1 to udim(hf$)
			if hf(j) then
				hs2+=1
				if j=1 then ! employee #
					hfm$=hfm$&',NZ 8'              	: hs1+= 8   	: hz1=hs2 : ul$=ul$&' -------'    : hs3=8
				else if j=2 then ! dept #
					hfm$=hfm$&',NZ 5'              	: hs1+= 5   	: hz1=hs2 : ul$=ul$&' ----'       : hs3=5
				else if j=3 then ! date
					hfm$=hfm$&',pic(bzzzz/zz/zz)'	: hs1+=11   	: hz1=hs2 : ul$=ul$&' ----------' : hs3=11
				else if j=4 then ! check number
					hfm$=hfm$&',NZ 7'              	: hs1+= 7   	: hz1=hs2 : ul$=ul$&' ------'     : hs3=7
				else if j>4 and j<10 then ! hours
					hfm$=hfm$&',G 8.2'             	: hs1+= 8    	           : ul$=ul$&' -------'    : hs3=8
				else if j>9 and j<17 then ! wages
					hfm$=hfm$&',G 10.2'            	: hs1+=10    	          : ul$=ul$&' ---------'  : hs3=10
				else if j>16 and j<47 then ! deductions
					hfm$=hfm$&',G 10.2'            	: hs1+=10      	         : ul$=ul$&' ---------'  : hs3=10
				end if
				hd$=hd$&lpad$(trim$(name$(j)(1:hs3-1)),hs3)
			end if
		next j
		mat cp0(hs2)
		mat cp1(hs2)
		mat cp2(hs2)
	return ! /r
	FlexGrid: ! r:
		dim colmask$(48),colhdr$(48)*20
		dim tcp(32)
		dim totaltcp(32),totaltdc(10)
		dim holdtotaltcp(32)
		dim holdtotaltdc(10)
		dim tdc(10)
		dim holdtdc(10),holdtcp(32)

		if trim$(z$)='[All]' then z$=''
		if trim$(z$)<>'' then z$=lpad$(trim$(z$),8)
		mat colhdr$(48) : mat colmask$(48)
		x=2
		colhdr$(1)='Rec' : colhdr$(2)='Desc'
		colmask$(1)='30': colmask$(2)=''
		if hf( 1) then colhdr$(x+=1)=name$( 1) : colmask$(x)='30' ! employee #
		if hf( 2) then colhdr$(x+=1)=name$( 2) : colmask$(x)='30' ! dept #
		if hf( 3) then colhdr$(x+=1)=name$( 3) : colmask$(x)= '3' ! Payroll Date
		if hf( 4) then colhdr$(x+=1)=name$( 4) : colmask$(x)='30' ! check stop
		if hf( 5) then colhdr$(x+=1)=name$( 5) : colmask$(x)='32'
		if hf( 6) then colhdr$(x+=1)=name$( 6) : colmask$(x)='32' ! ot hours
		if hf( 7) then colhdr$(x+=1)=name$( 7) : colmask$(x)='32'
		if hf( 8) then colhdr$(x+=1)=name$( 8) : colmask$(x)='32'
		if hf( 9) then colhdr$(x+=1)=name$( 9) : colmask$(x)='32'
		if hf(10) then colhdr$(x+=1)=name$(10) : colmask$(x)='10'
		if hf(11) then colhdr$(x+=1)=name$(11) : colmask$(x)='10'
		if hf(12) then colhdr$(x+=1)=name$(12) : colmask$(x)='10'
		if hf(13) then colhdr$(x+=1)=name$(13) : colmask$(x)='10'
		for j=14 to 46
			if hf(j) then colhdr$(x+=1)=name$(j) : colmask$(x)='10'
		next j
		mat colhdr$(x) : mat colmask$(x) : mat printitem$(x)
		fnFlexInit1('prchecks',1,1,20,100,mat colhdr$,mat colmask$,1)
		restore #hCheckIdx3,key>=lpad$(eno$,8)&cnvrt$('pd 6',beg_date)&'   ': nokey FlexGridXit ioerr FlexGridXit
	ReadBreakdowns: !
		holdeno=eno
		holdckno=ckno
		holdtdn=tdn
		holdprd=prd
		L4310: !
		read #hCheckIdx3,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2',release: eno,tdn,prd,ckno,mat tdc,mat tcp eof EoChecks
		if trim$(eno$)<>'[All]' and eno$<>cnvrt$('pic(zzzzzzzz)',eno) then goto FlexGridFinis
		if beg_date<>0 and prd<beg_date then goto L4310
		if end_date><0 and prd>end_date then goto L4310
		dim printitem$(48)*70
		if holdeno>0 and eno<>holdeno and trim$(eno$)<>'[All]' then goto EoChecks ! not same account and should treated as end of file
		if holdeno>0 and eno<>holdeno then goto FlexGridFinis             ! not same account

		! need to start analyzing quarters, etc here

		if ~holdckno then
			mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp
			goto ReadBreakdowns ! same check, no details
		else if holdckno and holdckno=ckno then
			mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp
			goto ReadBreakdowns ! same check, no details
		else if holdckno and holdckno<>ckno and (sum(tdc)+sum(tcp))>0 then
			desc$='TotalChk'
			holdrecnum=0
			gosub FlexGridAddLine
			mat totaltdc=totaltdc+tdc: mat totaltcp=totaltcp+tcp
			desc$='TotalChk'
		end if
		! if details=1 then
			enoprint=eno
			tdnprint=tdn
			prdprint=prd
			cknoprint=ckno
			mat totaltdc=totaltdc+tdc
			mat totaltcp=totaltcp+tcp
		! end if
		gosub FlexGridAddLine ! if details=1 then gosub FlexGridAddLine
	goto ReadBreakdowns !
	EoChecks: !
		! eofcode=1
	goto FlexGridFinis !
	FlexGridFinis: !
		FlexGridXit: !
	return ! /r
		FlexGridAddLine: ! r:
			dim item$(48)*70
			recnum=rec(hCheckIdx3)
				! if ~details then enoprint=holdeno : tdnprint=holdtdn : prdprint=holdprd : cknoprint=holdckno
				employeekey$=cnvrt$('pic(zzzzzzzz)',eno)
				! L4660: !
				! if sum(totaltcp)=(0) and sum(totaltdc)=(0) then goto FlexGridAddLineXit
				dim desc$*25
				read #hEmployee,using 'form pos 9,c 25',key=employeekey$: desc$ nokey ignore
				item$( 1)=cnvrt$('pic(zzzzzzz)',recnum)
				item$( 2)=desc$
				item$( 3)=cnvrt$('pic(zzzzzzzz)',enoprint)
				item$( 4)=cnvrt$('pic(zzz)',tdnprint)
				item$( 5)=str$(prdprint)
				item$( 6)=cnvrt$('pic(zzzzzzz)',cknoprint)

			item$( 7)=str$(totaltdc( 1))
			item$( 8)=str$(totaltdc( 2))
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
				item$(items+=1)=cnvrt$('pic(-------.zz)',totaltcp(j+4))
			next j
			item$(items+=1)=str$(totaltcp(25)) ! eic
			x=2
			printitem$(1)=item$(1): printitem$(2)=item$(2)
			for j=1 to 46
				if hf(j)=1 then printitem$(x+=1)=item$(j+2)
			next j

			L4820: !
			fnFlexAdd1(mat printitem$)
			holdeno=eno
			holdckno=ckno
			holdtdn=tdn
			holdprd=prd

			L4840: !
			if repeatit=1 then repeatit=0

			if trim$(printitem$(2))='Employee Total' then
				mat printitem$=(''): fnFlexAdd1(mat printitem$)
			end if

			mat totaltdc=(0): mat totaltcp=(0)
			desc$=''
			FlexGridAddLineXit: !
		return ! /r

	Finis: ! r:
	close #hGridName: ioerr ignore
	close #hPrReport: ioerr ignore
	if ~hEmployeePassed then
		close #hEmployee: ioerr ignore
	end if
	close #hCheckIdx3: ioerr ignore
	fn_selectCheck=returnN ! /r
fnend
	def fn_isBlank(text$*128; ___,returnN)
		if trim$(text$)='' then
			returnN=1
		end if
		fn_isBlank=returnN
	fnend

include: fn_setup
