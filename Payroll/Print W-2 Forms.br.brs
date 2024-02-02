!  (formerly) S:\acsPR\newPRW2A
! r: setup
	autoLibrary
	on error goto Ertn
	fnTop(program$)

	dim fw2box16$*255
	fw2box16$='form  pos 1,C 8'&rpt$(',C 12,G 10.2,3*G 1',6)
 
	open #hCompany=fnH: 'Name=[Q]\PRmstr\Company.h[cno],Shr',i,i
	dim empId$*12
	dim d$(10)*8
	dim e$(10)*12
	read #hCompany,using Fcompany: mat d$,loccode,ssmax,mat e$
	ssmax=ssmax*10
	Fcompany: form pos 150,10*c 8,n 2,pos 239,pd 4.2,pos 317,10*c 12
	close #hCompany:

	dim fullname$(20)*20
	dim abrevname$(20)*8
	dim deductionCode(20)
	dim newcalcode(20)
	dim newdedfed(20)
	dim dedfica(20)
	dim dedst(20)
	dim deduc(20)

	fnDedNames(mat fullname$,mat abrevname$,mat deductionCode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)

	dim w2box12Opt$(0)*3
	mat w2box12Opt$(0)
	box_12a=fnAddOneC(mat w2box12Opt$,'12a')
	box_12b=fnAddOneC(mat w2box12Opt$,'12b')
	box_12c=fnAddOneC(mat w2box12Opt$,'12c')
	box_12d=fnAddOneC(mat w2box12Opt$,'12d')
	box_14 =fnAddOneC(mat w2box12Opt$,'14')

! /r
AskInfo: !
dim cLocality$*8


if ~fnask_w2_info(taxYear$,ssrate,ssmax,mcrate,mcmax,pn1,dc1,	unused_state$,loccode,cLocality$) then goto Xit
			beg_date=val(taxYear$&'0101')
			end_date=val(taxYear$&'1231')
! AskDeductions: ! r: ! ask if any misecllaneous deductions should pr in box 12
	fullnameBlankCount=0
	for fullnameItem=1 to udim(mat fullname$)
		if trim$(fullname$(fullnameItem))='' then fullnameBlankCount+=1
	nex fullnameItem
	if fullnameBlankCount<udim(mat fullname$) then ! if they're not all blank
		fnTos
		rc=cf=0 : mylen=20 : mypos=mylen+3
		fnLbl(1,1,'Indicate if any of the miscellaneous deductions',50,1,0,0)
		fnLbl(2,1,'should appear in box 12 or 14 on the W-2.',44,1,0,0)
		fnLbl(4,7,'Deduction Name')
		fnLbl(4,26,'Yes' )
		fnLbl(4,35,'Box' )
		fnLbl(4,45,'Code')
		dim dedyn$(20)*5
		for dedItem=1 to 20
			if trim$(fullname$(dedItem))<>'' then
				fncreg_read('w2 deduction '&str$(dedItem)&' box 12 enable',dedyn$(dedItem),'False')
				fncreg_read('w2 deduction '&str$(dedItem)&' box 12 which',tmpBox12x$) : box12which(dedItem)=val(tmpBox12x$)
				fncreg_read('w2 deduction '&str$(dedItem)&' box 12 code',dedcode$(dedItem))
			end if
		nex dedItem
		dedItem=0
		dim respc_box12opt(20)
		dim box12which(20)
		dim resp$(128)*256
		tmpLine=5
		for dedItem=1 to 20
			if trim$(fullname$(dedItem))<>'' then
				fnLbl(tmpLine+=1,1,fullname$(dedItem),mylen,1,0,0)
				fnChk(tmpLine,26,'',0,0,0,0)
				resp$(rc+=1)=dedyn$(dedItem)
				fnComboA('w2Copy',tmpLine,35,mat w2box12Opt$, '',3)
				if box12which(dedItem)=0 then
					resp$(respc_box12opt(dedItem)=rc+=1)=''
				else
					resp$(respc_box12opt(dedItem)=rc+=1)=w2box12Opt$(box12which(dedItem))
				end if
				fnTxt(tmpLine,45,2,0,1,'',0,'Enter the Code that should appear in the box.')
				resp$(rc+=1)=dedcode$(dedItem)
			end if
		next dedItem
		fnCmdSet(2)
		ckey=fnAcs(mat resp$)
		if ckey=5 then
			if exportFormatID then
				close #hExport:
			end if
			goto AskInfo
		else
			x=0
			dim dedcode$(20)*2
			for dedItem=1 to 20
				if trim$(fullname$(dedItem))<>'' then
					dedyn$(dedItem)=resp$(x+=1)
					box12which(dedItem)=srch(mat w2box12Opt$,resp$(respc_box12opt(dedItem)))
					x+=1 ! box12(dedItem)=val(resp$(x+=1))
					dedcode$(dedItem)=resp$(x+=1)
					fncreg_write('w2 deduction '&str$(dedItem)&' box 12 enable',dedyn$(dedItem))
					fncreg_write('w2 deduction '&str$(dedItem)&' box 12 which',str$(box12which(dedItem)))
					fncreg_write('w2 deduction '&str$(dedItem)&' box 12 code',dedcode$(dedItem))
				end if
			nex dedItem
		end if
	end if ! /r
! r: open files, initialize output, etc

	! lyne=topmargin ! starting of 1st line
	goproc=0
	open #hEmployee=fnH: 'Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr',i,i,k ! #1
	open #hDepartment=fnH: 'Name=[Q]\PRmstr\department.h[cno],KFName=[Q]\PRmstr\deptidx.h[cno]',i,outIn,k ! #2
	open #hChecks=fnH: 'Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]',i,outIn,k
	! if env$('acsDeveloper')<>'' then pr 'hChecks=';hChecks : pause
	open #hAddr=fnH: 'Name=[Temp]\Addr.[Session],Replace,RecL=33,NoShr',internal,output
	write #hAddr,using 'form pos 1,n 10.2,n 1': ssmax,w1
	open #hW2Box16=fnH: 'Name=[Q]\PRmstr\W2Box16.h[cno],KFName=[Q]\PRmstr\W2Index.h[cno],Shr',i,i,k ioerr ignore
	! if loccode=0 or cLocality$='YES' or cLocality$='NO' then
	!   goto ReadEmployee
	! else
	!   empLocality$=cLocality$
	!   gosub AskEmpLocality
	! end if
! /r
do ! r: main loop
	ReadEmployee: ! 
	dim k$(3)*30
	dim ss$*11
	read #hEmployee,using 'form pos 1,n 8,3*c 30,c 11,pos 122,n 2': eno,mat k$,ss$,em6 eof EoEmployee
	if endnum>0 and eno>endnum then goto EoEmployee ! ending employee number entered
	dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
	fnNameParse(k$(1),nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
	if numb<>0 and eno<empno then goto ReadEmployee

	kz$=lpad$(rtrm$(str$(eno)),8)
	retirementPlanX$=''
	box12aCode$=box12aAmt$=box12bCode$=box12bAmt$=box12cCode$=box12cAmt$=box12dCode$=box12dAmt$=''
	box14Amt=0
	dim amt(6)
	mat amt=(0)
	dim miscded(20)
	mat miscded=(0)
	xFirst=1
	! Read #hDepartment,Using 1190,Rec=TA: TENO,TCD,MAT TY,TA
	checkkey$=cnvrt$('pic(zzzzzzz#)',eno)&cnvrt$('pic(zz#)',0)&cnvrt$('pd 6',0) ! index employee#,department# and payroll date
	restore #hChecks,key>=checkkey$: nokey ReadEmployee
	do ! r:
		ReadCheck: !
		dim tdc(10)
		dim tcp(32)
		read #hChecks,using 'form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2': heno,tdn,prd,ckno,mat tdc,mat tcp eof EoChecksForEmp
		if heno<>eno then goto EoChecksForEmp
		if prd<beg_date or prd>end_date then goto ReadCheck ! not this year
		read #hDepartment,using 'form pos 48,n 2', key=cnvrt$('pic(zzzzzzz#)',eno)&cnvrt$('pic(zzz)',tdn): tcd nokey ignore ! get state code
		if tcd<1 or tcd>10 then tcd=1
		if exportFormatID then
			stcode=tcd
		else if xFirst then 
			stcode=tcd
		else if ~xFirst and stcode<>tcd then
			goto L1300
		end if
		state$=d$(tcd)(1:2)
		stcode$=e$(tcd)
		L1300: !
		dedfica=0
		dedret=0
		for dedItem=1 to 20
			if newdedfed(dedItem)>=1 and deductionCode(dedItem)=1 then dedret=dedret+tcp(dedItem+4)
			if dedfica(dedItem)=1 and deductionCode(dedItem)=1 then dedfica=dedfica+tcp(dedItem+4)
			miscded(dedItem)=miscded(dedItem)+tcp(dedItem+4)
		next dedItem
		dim w(13)
		w(1)+=tcp(1) ! FED W/H YTD
		w(2)+=tcp(31)-dedret ! TOTAL TAXABLE WAGES
		ytdFica+=tcp(2) ! FICA W/H YTD
		w(4)+=tcp(24) ! EIC TOTAL
		if em6<>9 then
			w(5)+=tcp(31)-tcp(30)-dedfica ! TOTAL SS WAGES
			w(11)+=tcp(31)-dedfica ! TOTAL MC WAGES & TIPS
			! if env$('client')='Washington Parrish' then w(11)+=tcp(6) ! add deferred comp match into medicare wages
			if em6=2 then w(5)=0 ! NO SS
			if em6=1 then w(11)=0 ! NO MC
		end if
		! if env$('client')<>'Washington Parrish' then
			w(6)=w(6)+tcp(30) ! FICA TIPS YTD
		! end if
		w(3)+=tcp(2)
		w(12)+=tcp(3)
		if tcd=stcode then
			w(7)+=tcp(4) ! STATE WH
			w(9)+=tcp(31)-dedret ! STATE WAGES
			if loccode=0 or tcp(loccode+4)=0 then goto L1560
			w(8)+=tcp(loccode+4) ! LOCAL WITHHOLDING
			w(10)+=tcp(31)-dedret ! LOCAL WAGES
			L1560: !
			if pn1>0 and tcp(pn1+4)>0 then retirementPlanX$='X'
			if dc1>0 and dc1<11 then dcb+=tcp(dc1+4)
		else
			if loccode=0 then lowh=0 else lowh=tcp(loccode+4)
			write #hAddr,using 'form pos 1,n 8,n 2,3*pd 5.2,c 8': eno,tcd,tcp(31)-dedret,tcp(3),lowh,empLocality$
			goproc=1
		end if
		xFirst=0
	loop ! /r read next check record
	EoChecksForEmp: !
	gosub Box16process
	dim totalbox12(20)
	for dedItem=1 to 20 ! r:
		if trim$(fullname$(dedItem))<>'' then
			if dedyn$(dedItem)='True' and miscded(dedItem)<>0 then
				if box12which(dedItem)=box_12a then
					if box12aCode$='' and box12aAmt$='' then
						box12aCode$=lpad$(dedcode$(dedItem),4)
						box12aAmt$=cnvrt$('Nz 10.2',miscded(dedItem))
					else if env$('client')='Billings' then  ! specific catch for adding 2 items into one box 
						if trim$(box12aCode$)='G' then 
							let box12aAmt$=cnvrt$('Nz 10.2',val(box12aAmt$)+miscded(dedItem))
						end if 
					end if
					! descWhich=3
				else if box12which(dedItem)=box_12b then
					if box12bCode$='' and box12bAmt$='' then
						box12bCode$=lpad$(dedcode$(dedItem),4)
						box12bAmt$=cnvrt$('Nz 10.2',miscded(dedItem))
						totalbox12(dedItem)+=miscded(dedItem)
					end if
					! descWhich=4
				else if box12which(dedItem)=box_12c then
					if box12cCode$='' and box12cAmt$='' then
						box12cCode$=lpad$(dedcode$(dedItem),4)
						box12cAmt$=cnvrt$('Nz 10.2',miscded(dedItem))
						totalbox12(dedItem)+=miscded(dedItem)
					end if
					! descWhich=5
				else if box12which(dedItem)=box_12d then
					if box12dCode$='' and box12dAmt$='' then
						box12dCode$=lpad$(dedcode$(dedItem),4)
						box12dAmt$=cnvrt$('Nz 10.2',miscded(dedItem))
						totalbox12(dedItem)+=miscded(dedItem)
					end if
				else if box12which(dedItem)=box_14 then  !   box 14 stuff
					if ~box14Amt then
						box14Amt=miscded(dedItem)
						totalbox14(dedItem)+=box14Amt
						! if box14Amt then pause
					end if
					! descWhich=6
				end if
				! if trim$(desc$(descWhich))='' then
				!   desc$(descWhich)=lpad$(dedcode$(dedItem)&' '&cnvrt$('Nz 10.2',miscded(dedItem)),15)
				!   ! totalbox12(dedItem)+=miscded(dedItem)
				! end if
			end if
		end if
	next dedItem ! /r
	w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
	w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
	if em6=9 then w(3)=w(5)=w(11)=w(12)=0 ! NO SS OR MC
	if w(8)=0 then
		printLocality$=''
	else
		if cLocality$='YES' then gosub AskEmpLocality
		printLocality$=empLocality$
	end if
	dim controlNumber$*12
	controlNumber$=str$(eno)
	if w(2)<>0 or w(5)<>0 then ! only pr w2 if wages
		if exportFormatID=1 then
			gosub PrintW2
	! else if exportFormatID=2 then
	!   gosub EXPORT_CPS ! removed access 01/03/2017
		else
			gosub PrintW2
		end if
		mat sx=sx+w
		wctr=wctr+1
	end if
	mat w=(0)
	nqp=dcb=ytdFica=0
loop ! /r
EoEmployee: ! r:
	dim sx(13)
	dim tx(13)
	mat tx=tx+sx
	misc=3
	dim desc$(6)*15
	for dedItem=1 to 20 ! changed from 10 to 20 on 1/4/17
		if totalbox12(dedItem)<>0 then
			desc$(misc)=lpad$('  '&cnvrt$('Nz 10.2', totalbox12(dedItem)),15)
			misc+=1
			if misc>7 then goto Finis ! only allow 4 different deductions
		end if
	next dedItem
goto Finis: ! /r
Finis: ! r:
	close #hEmployee:
	close #hDepartment:
	close #hAddr:
	close #hW2Box16:
	! if ~exportFormatID then
	! 	mat w=tx
	! 	controlNumber$='Final Total'
	! 	nameFirst$=nameMiddle$=nameLast$=''
	! 	mat k$=('')
	! 	! state$=''
	! 	ss$=printLocality$=''
	! 	gosub PrintW2
	! end if
	fnW2PrintClose

	if exportFormatID then
		goto Xit
	else if goproc then
		goto PrintSecondStateW2
	end if
goto Xit ! /r
Xit: fnXit
PrintSecondStateW2: ! r:
	open #1: 'Name=[Temp]\Control.[session],RecL=128,Replace',internal,output
	restore #1:
	write #1,using 'form pos 1,c 128': 'FILE [Temp]\Addr.[Session],,,PRW2ADDR.h[cno],[Q]\PRmstr,,[Q]\PRmstr,,A,N'
	write #1,using 'form pos 1,c 128': 'MASK 9,2,n,a,1,8,n,a'
	close #1:
	fnFree('[Q]\PRmstr\PRW2ADDR.h[cno]')
	execute 'Sort [Temp]\Control.[session] -n'
fnChain('S:\acsPR\PrintSecondStateW2') ! /r
AskEmpLocality: ! r:
	fnTos
	rc=cf=0
	mylen=30
	mypos=mylen+3
	fnLbl(1,1,k$(1),mylen,1,0,0)
	fnLbl(2,1,'Locality Name:',mylen,1,0,0)
	fnTxt(2,mypos,12,0,1,'',0,'Enter the Locality for this employee.',0)
	resp$(rc+=1)=empLocality$
	fnCmdKey('&Next',1,1,0,'Proceed to next screen.')
	fnCmdKey('E&xit',5,0,1,'Returns to menu')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	empLocality$=resp$(1)
	controlNumber$=rtrm$(controlNumber$)
	if controlNumber$<>'1' then 
		empLocality$=controlNumber$
	end if
return  ! /r
Box16process: ! r: Box 16
	! passed hW2Box16,kz$, mat w, etc
	dim in4$(30)
	read #hW2Box16,using fw2box16$,key=kz$: kz$,mat in4$ nokey B16Finis
	for j=1 to 6
		amt(j)=val(in4$(j*5-3))
		if in4$(j*5-2)='1' then w(2)+=amt(j)
		if in4$(j*5-1)='1' then w(5)+=amt(j)
		!   if env$('client')='Washington Parrish' then goto L3760
		if in4$(j*5-1)='1' then w(11)+=amt(j)
		! L3760: !
		if in4$(j*5-0)='1' then w(9)+=amt(j)
		if in4$(j*5-2)='2' then w(2)=w(2)-amt(j)
		if in4$(j*5-1)='2' then w(5)=w(5)-amt(j)
		!   if env$('client')='Washington Parrish' then goto L3810
		if in4$(j*5-1)='2' then w(11)=w(11)-amt(j)
		! L3810: !
		if in4$(j*5-0)='2' then w(9)=w(9)-amt(j)
		if j=1 then
			desc$(j)=lpad$(in4$(j*5-4)(1:2)&'  '&ltrm$(cnvrt$('Nz 10.2',amt(j))),15)
		else if j=2 then
			desc$(j)=lpad$(in4$(j*5-4)(1:2)&'  '&cnvrt$('Nz 10.2',amt(j)),15)
		else if j=3 then
			box12aCode$=in4$(j*5-4)(1:2)
			box12aAmt$=cnvrt$('Nz 10.2',amt(j))
		else if j=4 then
			box12bCode$=in4$(j*5-4)(1:2)
			box12bAmt$=cnvrt$('Nz 10.2',amt(j))
		else if j=5 then
			box12cCode$=in4$(j*5-4)(1:2)
			box12cAmt$=cnvrt$('Nz 10.2',amt(j))
		else if j=6 then
			box12dCode$=in4$(j*5-4)(1:2)
			box12dAmt$=cnvrt$('Nz 10.2',amt(j))
		end if
		! if (j=3 or j=4) and (in4$(j*5-4)(1:1)='D' or in4$(j*5-4)(1:1)='E' or in4$(j*5-4)(1:1)='F' or in4$(j*5-4)(1:1)='H') then w(13)=w(13)+amt(j) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
	next j
	B16Finis: !
return  ! /r
PrintW2: ! r:
	fnW2Text(ss$,controlNumber$,mat w,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6),box14Amt)
return  ! /r
def fn_QAC(mat qac$,qacText$*256)
	qacCount+=1
	mat qac$(qacCount)
	qac$(qacCount)=qacText$
	fn_QAC=qacCount
fnend
include: ertn