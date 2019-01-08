!  (formerly) S:\acsPR\newPRW2A 
! r: setup
	library 'S:\Core\Library': fntop
	library 'S:\Core\Library': fnxit
	library 'S:\Core\Library': fnchain
	library 'S:\Core\Library': fnTos
	library 'S:\Core\Library': fnLbl
	library 'S:\Core\Library': fnTxt
	library 'S:\Core\Library': fnCmdKey
	library 'S:\Core\Library': fnAcs
	library 'S:\Core\Library': fnmsgbox
	library 'S:\Core\Library': fnChk
	library 'S:\Core\Library': fnpa_finis
	library 'S:\Core\Library': fnCmdSet
	library 'S:\Core\Library': fnpa_open
	library 'S:\Core\Library': fnpa_newpage
	library 'S:\Core\Library': fnCmdSet
	library 'S:\Core\Library': fnpa_open
	library 'S:\Core\Library': fnpa_newpage
	library 'S:\Core\Library': fncreg_read
	library 'S:\Core\Library': fncreg_write
	library 'S:\Core\Library': fnpa_background
	library 'S:\Core\Library': fngethandle
	library 'S:\Core\Library': fnDedNames
	library 'S:\Core\Library': fncomboa
	library 'S:\Core\Library': fnw3
	library 'S:\Core\Library': fnNameParse
	library 'S:\Core\Library': fnAddOneC
	library 'S:\Core\Library': fnask_w2_info
	library 'S:\Core\Library': fnw2_text
	library 'S:\Core\Library': fnFree
	on error goto ERTN

	dim fw2box16$*255
	dim ss$*11
	dim sx(13),tx(13)
	dim cLocality$*8
	dim desc$(6)*15,amt(6)
	dim tcp(32),tdc(10)
	dim resp$(128)*256
	dim w(13)
	dim a$(3)*40
	dim empId$*12,controlNumber$*12
	dim d$(10)*8,e$(10)*12
	dim newdedfed(20),newdedcode(20)
	dim newcalcode(20),dedfica(20),dedst(20),deduc(20),fullname$(20)*20
	dim abrevname$(20)*8
	dim dedcode$(20)*2,dedyn$(20)*5
	dim miscded(20),totalbox12(20)
	dim tmpMsgLine$(0)*256
	!
	dim in4$(30)
	dim nameFirst$*64,nameMiddle$*64,nameLast$*64,nameSuffix$*64
	dim k$(3)*30

	fntop(program$)
	fw2box16$="FORM  POS 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)

	open #hCompany:=fngethandle: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input 
	read #hCompany,using fCompany: mat a$,empId$,mat d$,loccode,mat e$
	fCompany: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12 
	for j=1 to 3: a$(j)=a$(j)(1:30): next j
	close #hCompany: 
	!
	fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
	!
	dim w2box12Opt$(0)*3
	mat w2box12Opt$(0)
	box_12a=fnAddOneC(mat w2box12Opt$,'12a')
	box_12b=fnAddOneC(mat w2box12Opt$,'12b')
	box_12c=fnAddOneC(mat w2box12Opt$,'12c')
	box_12d=fnAddOneC(mat w2box12Opt$,'12d')
	box_14 =fnAddOneC(mat w2box12Opt$,'14')
	!
	dim w2laser_output_filename$*256
	!
	dim w2Copy$*68
	!
	! /r
	ASK_INFO: !
	if ~fnask_w2_info(taxYear$,beg_date,end_date,empStart$,empEnd$,ssrate,ssmax,mcrate,mcmax,mat w2destinationOpt$,enableW3$,enableBackground$,w2Copy,w2Copy$,exportFormatID,w2laser_output_filename$,pn1,dc1,topmargin,bottom,unused_state$,loccode,cLocality$) then goto XIT
	! pause  !
	dim w2ssnMask(6) ! W2CopyFile2pp$(6)*128,W2CopyFile$(6)*128,
	w2ssnMask(1)=0 ! W2CopyFile$(1)='S:\Core\pdf\'&taxYear$&'\W-2\Copy A.pdf' :  W2CopyFile2pp$(1)='S:\Core\pdf\'&taxYear$&'\W-2\Copy A - 2pp.pdf'
	w2ssnMask(2)=0 ! W2CopyFile$(2)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 1.pdf' :  W2CopyFile2pp$(2)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 1 - 2pp.pdf'
	w2ssnMask(3)=0 ! W2CopyFile$(3)='S:\Core\pdf\'&taxYear$&'\W-2\Copy B.pdf' :  W2CopyFile2pp$(3)='S:\Core\pdf\'&taxYear$&'\W-2\Copy B - 2pp.pdf'
	w2ssnMask(4)=1 ! W2CopyFile$(4)='S:\Core\pdf\'&taxYear$&'\W-2\Copy C.pdf' :  W2CopyFile2pp$(4)='S:\Core\pdf\'&taxYear$&'\W-2\Copy C - 2pp.pdf'
	w2ssnMask(5)=1 ! W2CopyFile$(5)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 2.pdf' :  W2CopyFile2pp$(5)='S:\Core\pdf\'&taxYear$&'\W-2\Copy 2 - 2pp.pdf'
	w2ssnMask(6)=1 ! W2CopyFile$(6)='S:\Core\pdf\'&taxYear$&'\W-2\Copy D.pdf' :  W2CopyFile2pp$(6)='S:\Core\pdf\'&taxYear$&'\W-2\Copy D - 2pp.pdf'
	! r: open export files (if appropriate, return to ASK_INFO screen if failure
	if exportFormatID then 
		w2laser_output_filename$=srep$(w2laser_output_filename$,'[CompanyNumber]',env$('cno'))
		w2laser_output_filename$=srep$(w2laser_output_filename$,'[companynumber]',env$('cno'))
		w2laser_output_filename$=srep$(w2laser_output_filename$,'[COMPANYNUMBER]',env$('cno'))
		w2laser_output_filename$=srep$(w2laser_output_filename$,'[TaxYear]',taxYear$)
		w2laser_output_filename$=srep$(w2laser_output_filename$,'[taxyear]',taxYear$)
		w2laser_output_filename$=srep$(w2laser_output_filename$,'[TAXYEAR]',taxYear$)
		open #hExport:=fngethandle: "Name="&br_filename$(w2laser_output_filename$)&",REPLACE",display,output ioerr ASK_INFO
	end if 
	! /r
! ASK_DEDUCTIONS: ! r: ! ask if any misecllaneous deductions should pr in box 12
	fnTos(sn$="Prw2-box12")
	rc=cf=0 : mylen=20 : mypos=mylen+3
	fnLbl(1,1,"Indicate if any of the miscellaneous deductions",50,1,0,0)
	fnLbl(2,1,"should appear in box 12 or 14 on the W-2.",44,1,0,0)
	fnLbl(4,7,"Deduction Name")
	fnLbl(4,26,"Yes" )
	fnLbl(4,35,"Box" )
	fnLbl(4,45,"Code")
	for dedItem=1 to 20
		if trim$(fullname$(dedItem))<>'' then
			fncreg_read('w2 deduction '&str$(dedItem)&' box 12 enable',dedyn$(dedItem),'False')
			fncreg_read('w2 deduction '&str$(dedItem)&' box 12 which',tmpBox12x$) : box12which(dedItem)=val(tmpBox12x$)
			fncreg_read('w2 deduction '&str$(dedItem)&' box 12 code',dedcode$(dedItem))
		end if
	nex dedItem
	dedItem=0
	dim respc_box12opt(20),box12which(20)
	tmpLine=5
	for dedItem=1 to 20
		if trim$(fullname$(dedItem))<>'' then 
			fnLbl(tmpLine+=1,1,fullname$(dedItem),mylen,1,0,0)
			fnChk(tmpLine,26,"",0,0,0,0)
			resp$(rc+=1)=dedyn$(dedItem)
			fncomboa('w2Copy',tmpLine,35,mat w2box12Opt$, '',3)
			if box12which(dedItem)=0 then
				resp$(respc_box12opt(dedItem)=rc+=1)=''
			else
				resp$(respc_box12opt(dedItem)=rc+=1)=w2box12Opt$(box12which(dedItem))
			end if
			fnTxt(tmpLine,45,2,0,1,"",0,"Enter the Code that should appear in the box.")
			resp$(rc+=1)=dedcode$(dedItem)
		end if
	next dedItem
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then 
		if exportFormatID then
			close #hExport:
		end if
		goto ASK_INFO
	else
		x=0
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
	end if ! /r
	! r: open files, initialize output, etc
	if exportFormatID=0 then 
		fnpa_open('',w2Copy$,'PDF') 
	end if
! lyne=topmargin ! starting of 1st line
	goproc=0
	open #hEmployee:=1: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
	open #hDepartment:=2: "Name=[Q]\PRmstr\department.h[cno],KFName=[Q]\PRmstr\deptidx.h[cno]",internal,outIn,keyed 
	open #hChecks:=fngethandle: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed 
	open #hAddr:=fngethandle: "Name="&env$('Temp')&"\Addr."&session$&",Replace,RecL=33,NoShr",internal,output 
	write #hAddr,using 'form pos 1,n 10.2,n 1': ssmax,w1
	open #hW2Box16:=fngethandle: "Name=[Q]\PRmstr\W2Box16.h[cno],KFName=[Q]\PRmstr\W2Index.h[cno],Shr",internal,input,keyed ioerr ignore
	w2printCount=0
	! if loccode=0 or cLocality$="YES" or cLocality$="NO" then 
	!   goto READ_EMPLOYEE 
	! else 
	!   empLocality$=cLocality$
	!   gosub ASK_EMP_LOCALITY
	! end if
! /r
READ_EMPLOYEE: ! r:
	do
		read #hEmployee,using 'form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3': eno,mat k$,ss$,em6,ta eof EO_EMPLOYEE
		if endnum>0 and eno>endnum then goto EO_EMPLOYEE ! ending employee number entered
		fnNameParse(k$(1),nameFirst$,nameMiddle$,nameLast$,nameSuffix$)
		if numb<>0 and eno<empno then goto READ_EMPLOYEE
		kz$=lpad$(rtrm$(str$(eno)),8)
		retirementPlanX$=""
		box12aCode$=box12aAmt$=box12bCode$=box12bAmt$=box12cCode$=box12cAmt$=box12dCode$=box12dAmt$=''
		box14Amt=0
		mat amt=(0)
		mat miscded=(0)
!   tdedret=0 ! REMOVE EXPLANATION  FROM LINE 905 TO LIST RETIREMENT IN BOX 13
		first=1
		! Read #hDepartment,Using 1190,Rec=TA: TENO,TCD,MAT TY,TA
		checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
		restore #hChecks,key>=checkkey$: nokey READ_EMPLOYEE
		do
			READ_CHECK: !
			read #hChecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof EO_CHECKS_FOR_EMP
			if heno<>eno then goto EO_CHECKS_FOR_EMP
			if prd<beg_date or prd>end_date then goto READ_CHECK ! not this year
			read #hDepartment,using "form pos 48,n 2", key=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zzz)",tdn): tcd nokey ignore ! get state code
			if tcd<1 or tcd>10 then tcd=1
			if exportFormatID then 
				stcode=tcd 
			else
				if first=1 then stcode=tcd
				if first=0 and stcode><tcd then 
					goto L1300
				end if
			end if
			state$=d$(tcd)(1:2)
			stcode$=e$(tcd)
			L1300: ! 
			dedfica=0
			dedret=0
			for dedItem=1 to 20
				if newdedfed(dedItem)>=1 and newdedcode(dedItem)=1 then dedret=dedret+tcp(dedItem+4)
				if dedfica(dedItem)=1 and newdedcode(dedItem)=1 then dedfica=dedfica+tcp(dedItem+4)
				miscded(dedItem)=miscded(dedItem)+tcp(dedItem+4)
			next dedItem
			! tDEDRET=TDEDRET+DEDRET ! ACCUMULATE BOX 13 RETIREMENT; THIS LINE WILL ONLY WORK IF NO CAFETERIA; REMOVE ! OFF 861 AND 882 FOR RETIREMENT ONLY ! can change 882 only if know specific ded to show in box 13
			w(1)+=tcp(1) ! FED W/H YTD
			w(2)+=tcp(31)-dedret ! TOTAL TAXABLE WAGES
			ytdFica+=tcp(2) ! FICA W/H YTD
			w(4)+=tcp(24) ! EIC TOTAL
			if em6<>9 then 
				w(5)+=tcp(31)-tcp(30)-dedfica ! TOTAL SS WAGES
				w(11)+=tcp(31)-dedfica ! TOTAL MC WAGES & TIPS
				! if env$('client')="Washington Parrish" then w(11)+=tcp(6) ! add deferred comp match into medicare wages
				if em6=2 then w(5)=0 ! NO SS
				if em6=1 then w(11)=0 ! NO MC
			end if
			! if env$('client')<>"Washington Parrish" then 
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
				if pn1>0 and tcp(pn1+4)>0 then retirementPlanX$="X"
				if dc1>0 and dc1<11 then dcb+=tcp(dc1+4)
			else
				if loccode=0 then lowh=0 else lowh=tcp(loccode+4)
				write #hAddr,using 'form pos 1,n 8,n 2,3*pd 5.2,c 8': eno,tcd,tcp(31)-dedret,tcp(3),lowh,empLocality$
				goproc=1
			end if
			first=0
		loop ! read next check record
		EO_CHECKS_FOR_EMP: ! 
		gosub BOX16_process
		for dedItem=1 to 20
			if trim$(fullname$(dedItem))<>'' then 
				if dedyn$(dedItem)="True" and miscded(dedItem)<>0 then 
					if box12which(dedItem)=box_12a then
						if box12aCode$='' and box12aAmt$='' then 
							box12aCode$=lpad$(dedcode$(dedItem),4) 
							box12aAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
						end if
						! descWhich=3
					else if box12which(dedItem)=box_12b then
						if box12bCode$='' and box12bAmt$='' then 
							box12bCode$=lpad$(dedcode$(dedItem),4)
							box12bAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
							totalbox12(dedItem)+=miscded(dedItem)
						end if
						! descWhich=4
					else if box12which(dedItem)=box_12c then
						if box12cCode$='' and box12cAmt$='' then 
							box12cCode$=lpad$(dedcode$(dedItem),4)
							box12cAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
							totalbox12(dedItem)+=miscded(dedItem)
						end if
						! descWhich=5
					else if box12which(dedItem)=box_12d then
						if box12dCode$='' and box12dAmt$='' then 
							box12dCode$=lpad$(dedcode$(dedItem),4)
							box12dAmt$=cnvrt$("Nz 10.2",miscded(dedItem))
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
					! if trim$(desc$(descWhich))="" then 
					!   desc$(descWhich)=lpad$(dedcode$(dedItem)&" "&cnvrt$("Nz 10.2",miscded(dedItem)),15)
					!   ! totalbox12(dedItem)+=miscded(dedItem)
					! end if
				end if 
			end if
		next dedItem
		w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
		w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
		if em6=9 then w(3)=w(5)=w(11)=w(12)=0 ! NO SS OR MC
		if w(8)=0 then 
			printLocality$=""
		else
			if cLocality$="YES" then gosub ASK_EMP_LOCALITY
			printLocality$=empLocality$
		end if
		controlNumber$=str$(eno)
		if w(2)<>0 or w(5)<>0 then ! only pr w2 if wages
			if exportFormatID=1 then 
				gosub EXPORT_AMS
		! else if exportFormatID=2 then 
		!   gosub EXPORT_CPS !  ! removed access 01/03/2017
			else 
				gosub PrintW2
			end if
			mat sx=sx+w
			wctr=wctr+1
		end if
		mat w=(0)
		nqp=dcb=ytdFica=0
	loop ! /r
EO_EMPLOYEE: ! r:
	mat tx=tx+sx
	misc=3
	for dedItem=1 to 20 ! changed from 10 to 20 on 1/4/17
		if totalbox12(dedItem)<>0 then 
			desc$(misc)=lpad$("  "&cnvrt$("Nz 10.2", totalbox12(dedItem)),15)
			misc+=1
			if misc>7 then goto FINIS ! only allow 4 different deductions
		end if
	next dedItem
	goto FINIS: ! /r
FINIS: ! r:
	close #hEmployee: 
	close #hDepartment: 
	close #hAddr: 
	close #hW2Box16: 
	if ~exportFormatID then 
		mat w=tx 
		controlNumber$="Final Total" 
		nameFirst$=nameMiddle$=nameLast$=""
		mat k$=("")
		! state$=''
		ss$=printLocality$="" 
		gosub PrintW2
		fnpa_finis
	end if
	if enableW3$="True" then let fnw3(taxYear$,empId$,mat a$,mat w,dcb,state$,stcode$)
	if exportFormatID then 
			mat tmpMsgLine$(2)
			tmpMsgLine$(1)='Export file created:'
			tmpMsgLine$(2)=os_filename$(file$(hExport))
			close #hExport:
			fnmsgbox(mat tmpMsgLine$,resp$) ! ,16+4)
		goto XIT
	else
		if goproc=1 then 
			goto PRW2B
		end if
	end if
goto XIT ! /r
XIT: fnxit
PRW2B: ! r:
	open #1: "Name="&env$('Temp')&"\Control."&session$,internal,output 
	restore #1: 
	write #1,using 'form pos 1,c 128': "FILE "&env$('Temp')&"\Addr."&session$&",,,PRW2ADDR.H[cno],[Q]\PRmstr,,[Q]\PRmstr,,A,N"
	write #1,using 'form pos 1,c 128': "MASK 9,2,n,a,1,8,n,a"
	close #1: 
	fnFree("[Q]\PRmstr\PRW2ADDR.H[cno]")
	execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
fnchain("S:\acsPR\prw2b") ! /r
ASK_EMP_LOCALITY: ! r:
	fnTos(sn$="Prw2-5")
	rc=cf=0
	mylen=30
	mypos=mylen+3
	fnLbl(1,1,k$(1),mylen,1,0,0)
	fnLbl(2,1,"Locality Name:",mylen,1,0,0)
	fnTxt(2,mypos,12,0,1,"",0,"Enter the Locality for this employee.",0)
	resp$(rc+=1)=empLocality$
	fnCmdKey("&Next",1,1,0,"Proceed to next screen.")
	fnCmdKey("E&xit",5,0,1,"Returns to menu")
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	empLocality$=resp$(1)
	controlNumber$=rtrm$(controlNumber$)
	if controlNumber$="1" then goto L2770
	empLocality$=controlNumber$
	L2770: ! 
return  ! /r
BOX16_process: ! r: Box 16
	! passed hW2Box16,kz$, mat w, etc
	read #hW2Box16,using fw2box16$,key=kz$: kz$,mat in4$ nokey B16Finis
	for j=1 to 6
		amt(j)=val(in4$(j*5-3))
		if in4$(j*5-2)="1" then w(2)+=amt(j)
		if in4$(j*5-1)="1" then w(5)+=amt(j)
		!   if env$('client')="Washington Parrish" then goto L3760
		if in4$(j*5-1)="1" then w(11)+=amt(j)
		! L3760: !
		if in4$(j*5-0)="1" then w(9)+=amt(j)
		if in4$(j*5-2)="2" then w(2)=w(2)-amt(j)
		if in4$(j*5-1)="2" then w(5)=w(5)-amt(j)
		!   if env$('client')="Washington Parrish" then goto L3810
		if in4$(j*5-1)="2" then w(11)=w(11)-amt(j)
		! L3810: ! 
		if in4$(j*5-0)="2" then w(9)=w(9)-amt(j)
		if j=1 then 
			desc$(j)=lpad$(in4$(j*5-4)(1:2)&"  "&ltrm$(cnvrt$("Nz 10.2",amt(j))),15)
		else if j=2 then
			desc$(j)=lpad$(in4$(j*5-4)(1:2)&"  "&cnvrt$("Nz 10.2",amt(j)),15)
		else if j=3 then
			box12aCode$=in4$(j*5-4)(1:2)
			box12aAmt$=cnvrt$("Nz 10.2",amt(j))
		else if j=4 then
			box12bCode$=in4$(j*5-4)(1:2)
			box12bAmt$=cnvrt$("Nz 10.2",amt(j))
		else if j=5 then
			box12cCode$=in4$(j*5-4)(1:2)
			box12cAmt$=cnvrt$("Nz 10.2",amt(j))
		else if j=6 then
			box12dCode$=in4$(j*5-4)(1:2)
			box12dAmt$=cnvrt$("Nz 10.2",amt(j))
		end if
		! if (j=3 or j=4) and (in4$(j*5-4)(1:1)="D" or in4$(j*5-4)(1:1)="E" or in4$(j*5-4)(1:1)="F" or in4$(j*5-4)(1:1)="H") then w(13)=w(13)+amt(j) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
	next j
	B16Finis: ! 
return  ! /r
EXPORT_AMS: ! r: LASER W2 FOR ADVANCED MICRO SOLUTIONS
	pr #hExport: "ROAN="&controlNumber$
	pr #hExport: "FEIN="&empId$
	pr #hExport: "WAGES="&str$(w(2))
	pr #hExport: "FITW="&str$(w(1))
	pr #hExport: "PNAME1="&a$(1)
	pr #hExport: "PNAME2="
	pr #hExport: "SSWAGES="&str$(w(5))
	pr #hExport: "SSWH="&str$(w(3))
	pr #hExport: "PADDR1="&a$(2)
	pr #hExport: "PADDR2="&a$(3)
	pr #hExport: "MCWAGES="&str$(w(11))
	pr #hExport: "MCWH="&str$(w(12))
	pr #hExport: "SSN="&srep$(ss$,' ','')
	pr #hExport: "SSTIPS="&str$(w(6))
	pr #hExport: "ALLOCATIP="  ! "ALLOCATIP=";0
	pr #hExport: "RNAME1="&(rtrm$(nameLast$)&","&nameFirst$)(1:24)
	pr #hExport: "RNAME2="&(k$(2)(1:24))
! pr #hExport: "AEIC=";w(4)    ! this field is no longer supported 1/4/2017
	pr #hExport: "DEPDCARE="&str$(dcb)
	pr #hExport: "RADDR1="
	pr #hExport: "RADDR2="&(k$(3)(1:24))
	if box14Amt<>0 then 
		pr #hExport: "LAB14A=TRANS"
	else
		pr #hExport: "LAB14A="
	end if
	pr #hExport: "BOX14A="&str$(box14Amt) ! pr #hExport: "BOX14A=0"
	pr #hExport: "LAB12A="&box12aCode$
	pr #hExport: "BOX12A="&box12aAmt$
	pr #hExport: "CNTRYCODE="
	pr #hExport: "RCOUNTRY="
	pr #hExport: "EESTAT=" ! 0"
	pr #hExport: "EERETR="&retirementPlanX$
	pr #hExport: "EESICK=" ! 0"
	pr #hExport: "LAB14B="
	pr #hExport: "BOX14B=0"
	pr #hExport: "LAB12B="&box12bCode$
	pr #hExport: "BOX12B="&box12bAmt$
	pr #hExport: "LAB14C="
	pr #hExport: "BOX14C=0"
	pr #hExport: "LAB12C="&box12cCode$
	pr #hExport: "BOX12C="&box12cAmt$
	pr #hExport: "LAB14D="
	pr #hExport: "BOX14D=0"
	pr #hExport: "LAB12D="&box12dCode$
	pr #hExport: "BOX12D="&box12dAmt$
	pr #hExport: "BOX11Q="&str$(nqp)
	pr #hExport: "NQPLANS="
	pr #hExport: "STATE1="&state$
	pr #hExport: "SEIN1="&stcode$
	pr #hExport: "SWAGES1="&str$(w(9))
	pr #hExport: "SITW1="&str$(w(7))
	pr #hExport: "LWAGES1="&str$(w(10))
	pr #hExport: "LITW1="&str$(w(8))
	pr #hExport: "LOCAL1="&printLocality$
	pr #hExport: "STATE2="
	pr #hExport: "SEIN2="
	pr #hExport: "SWAGES2=0"
	pr #hExport: "SITW2=0"
	pr #hExport: "LWAGES2=0"
	pr #hExport: "LITW2=0"
	pr #hExport: "LOCAL2="
	pr #hExport: "FName="&nameFirst$(1:24)
	pr #hExport: "LName="&nameLast$(1:24)
	pr #hExport: "TAG="
	pr #hExport: "EBAT="
	pr #hExport: "PHONE="
	pr #hExport: "*"
return ! /r
PrintW2: ! r:
	w2printCount+=1
	if w2printCount/2=int(w2printCount/2) then ! it's the second one on a page 
		!   if enableBackground$='True' then let fnpa_pic('S:\Core\pdf\W-2 Copy 1.png',1,bottom,200,200)
		!   if enableBackground$='True' then let fnpa_pic(W2CopyFile$(w2Copy),1,bottom,200,200)
		fnw2_text(bottom,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6),box14Amt)
		fnpa_newpage
	else
		if enableBackground$='True' then 
			fnpa_background('S:\Core\pdf\'&taxYear$&'\W-2\Copy '&w2Copy$(1:1)&'.pdf')
		end if
		fnw2_text(topmargin,w2ssnMask(w2Copy),mat a$,empId$,ss$,controlNumber$,mat w,dcb$,nameFirst$,nameMiddle$,nameLast$,nameSuffix$,retirementPlanX$,mat k$,box12aCode$,box12aAmt$,box12bCode$,box12bAmt$,box12cCode$,box12cAmt$,box12dCode$,box12dAmt$,state$,stcode$,printLocality$(1:6),box14Amt)
	end if
return  ! /r
def fnQAC(mat qac$,qacText$*256)
	qacCount+=1
	mat qac$(qacCount)
	qac$(qacCount)=qacText$
	fnQAC=qacCount
fnend
include: ertn
