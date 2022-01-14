! Replace S:\acsPR\newPR941  ! fix the count (box 1; needs logic to look thru history and count the active employees on a certain date)
! 941 Summary  ( Prints a detail of employees and the complete 941 using priint ace
 
	autoLibrary
	fnTop(program$,cap$="941 Summary")
	on error goto Ertn
 
	dim dedcode(20),calcode(20),dedfed(20),option1$(4)*20
	dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
	dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10),em$(3)*30
	dim e$(10)*12,tpt(32),cap$*128,resp$(15)*30 ! option$(4)*15,message$*40,
	dim tcp(32),tdc(10) ! qtr1ytd(32),qtr1ytd(32),qtr3ytd(32),qtr4ytd(32)
	dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),qtr(32)
	dim ytdtotal(32),ss$*11,m$*20
	dim city$*15,state$*2,zip$*9,csz$*40,ml$(2)*80
 
	fncreg_read('calculation date text',m$)
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
	read #20,using L280: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
	ficamaxw=ficamaxw*10
	L280: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
	ficarate=ficarate/100
	mcr=mcr*.01
	close #20:
 
MENU1: ! r:
	fnTos(sn$="pr941")
	respc=0
	if val(date$(4:5))=1 then taxyear=val(date$(1:2))+2000-1 else taxyear =val(date$(1:2))+2000 ! current tax year (if processing in jan, assume last year)
	fnLbl(1,1,"Tax Year:",26,1)
	fnTxt(1,30,4,0,0,'30',0,"")
	resp$(respc+=1)=str$(taxyear)
	option1$(1)="March 31"
	option1$(2)="June 30"
	option1$(3)="September 30"
	option1$(4)="December 31"
	fnLbl(2,1,"Quarter Ending Date:",26,1)
	fncomboa("pr941-yr",2,30,mat option1$,"Enter the quarter ending date")
	if val(date$(4:5))=3 or val(date$(4:5))=4 or val(date$(4:5))=5 then resp$(respc+=1)=option1$(1) ! march filing
	if val(date$(4:5))=6 or val(date$(4:5))=7 or val(date$(4:5))=8 then resp$(respc+=1)=option1$(2) ! June  filing
	if val(date$(4:5))=9 or val(date$(4:5))=10 or val(date$(4:5))=11 then resp$(respc+=1)=option1$(3) ! September filing
	if val(date$(4:5))=12 or val(date$(4:5))=1 or val(date$(4:5))=2 then resp$(respc+=1)=option1$(4) ! December
	fnChk(3,30,"Print Worksheet:",1)
	resp$(respc+=1)='True'
	fnFra(5,1,4,30,"Tax Liability","Enter the total tax liability by month")
	fnLbl(1,1,"Month 1:",10,1,0,1)
	fnTxt(1,13,12,0,1,'10',0,"",1)
	resp$(respc+=1)=""
	fnLbl(2,1,"Month 2:",10,1,0,1)
	fnTxt(2,13,12,0,1,'10',0,"",1)
	resp$(respc+=1)=""
	fnLbl(3,1,"Month 3:",10,1,0,1)
	fnTxt(3,13,12,0,1,'10',0,"",1)
	resp$(respc+=1)=""
	fnFra(11,1,7,72,"Adjustments","Enter any applicable adjustments")
	mylen=52
	fnLbl(1,1,"Current quarter's fraction of cents:",mylen,1,0,2)
	fnTxt(1,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(2,1,"Current quarter's sick pay:",mylen,1,0,2)
	fnTxt(2,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(3,1,"Current quarter's adjustments for tips and ins:",mylen,1,0,2)
	fnTxt(3,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(4,1,"Current year's income tax withholding:",mylen,1,0,2)
	fnTxt(4,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(5,1,"Prior quarters' ss and medicare taxes:",mylen,1,0,2)
	fnTxt(5,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(6,1,"Special Additions to Federal income taxes:",mylen,1,0,2)
	fnTxt(6,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(7,1,"Special Additions to ss and medicare:",mylen,1,0,2)
	fnTxt(7,mylen+3,12,0,1,'10',0,"",2)
	resp$(respc+=1)=""
	fnLbl(20,1,"Total deposits for quarter including overpayments:",mylen+1,1,0,0)
	fnTxt(20,mylen+4,12,0,1,'10',0,"",0)
	resp$(respc+=1)=""
	fnCmdSet(2): ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	taxyear=val(resp$(1)) ! tax year
	if taxyear<2000 then goto L810
	ending_date=taxyear*10000+1231 conv L810
	goto L820
	L810: ! r:
		mat ml$(2)
		ml$(1)="You must enter a valid tax year such as 2018."
		ml$(2)="Take OK to enter the year."
		fnmsgbox(mat ml$,resp$,cap$,0)
	goto MENU1 ! /r
	L820: !
	for j=1 to 4
		if resp$(2)=option1$(j) then qtr=j: m$=option1$(j): goto L850 ! quarter ending date
	next j
	L850: !
	if qtr=1 then begdate=taxyear*10000+0312: enddate=val(taxyear$)*10000+0318
	if qtr=2 then begdate=taxyear*10000+0612: enddate=val(taxyear$)*10000+0618
	if qtr=3 then begdate=taxyear*10000+0912: enddate=val(taxyear$)*10000+0918
	if qtr=4 then begdate=taxyear*10000+1212: enddate=val(taxyear$)*10000+1218
	if resp$(3)='True' then frm=2 else frm=1 ! need a worksheet
	box15a=val(resp$(4)) ! first month liability
	box15b=val(resp$(5))
	box15c=val(resp$(6))
	box7a=val(resp$(7)) ! fractions
	box7b=val(resp$(8)) ! sick pay
	box7c=val(resp$(9)) ! tips
	box7d=val(resp$(10)) ! tax wh
	box7e=val(resp$(11)) ! prior qtr
	box7f=val(resp$(12)) ! special add
	box7g=val(resp$(13)) ! special add - ss
	box11=val(resp$(14))
	fnopenprn
	fn_start_print
	fn_build_941
	fn_print941_info
! gosub LASER_941
goto FINIS ! /r
 
def fn_start_print
	on pageoflow goto PgOf
	open #2: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
	open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
	open #3: "Name=[Q]\PRmstr\Department.h[cno],Shr, KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed
	if frm=2 then gosub WK_HEADER
	L1140: read #2,using L1150: eno,mat em$,ss$,em5,em6 eof WK_END
	L1150: form pos 1,n 8,3*c 30,c 11,pos 120,2*n 2
	m1=m2=h2=h3=dedytdfica=dedqtrfica=dedytdfederal=dedqtrfederal=m4=0
	mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
	mat ytdtotal=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey ANALYZE_WAGES
	L1210: read #4,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
	if heno<>eno then goto ANALYZE_WAGES
	if prd<beg_date or prd>end_date then goto L1210 ! not this year
	if em5=1 then pedate=begdate+19: box1+=1 ! monthly pay period
	if em5=2 then pedate=begdate+15 : box1+=1 ! semi-monthly
	if em5=3 then pedate=begdate+14 : box1+=1 ! bi-weekly
	if em5=4 then pedate=begdate+7: box1+=1 ! weekly
	!   deptkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)
	! form pos 48,N 2
	if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp: mat tpt=tpt+tcp ! 1st qtr earnings
	if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp : mat tpt=tpt+tcp
	if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp : mat tpt=tpt+tcp
	if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp : mat tpt=tpt+tcp
	if prd>=qtr1 and prd<ending_date then mat ytdtotal=ytdtotal+tcp ! only total year to date wages to end of current quarter
	goto L1210
	ANALYZE_WAGES: ! analyze wages on each person
	if qtr=1 then mat qtr=qtr1tcp
	if qtr=2 then mat qtr=qtr2tcp
	if qtr=3 then mat qtr=qtr3tcp
	if qtr=4 then mat qtr=qtr4tcp
	!   dcq=0 ! total wage for quarter
	!   tcy=0 ! total wages to end of quarter
	for j=1 to 20
		if dedfed(j)=1 and dedcode(j)=1 then dedytdfederal+=ytdtotal(j+4): dedqtrfederal+=qtr(j+4) ! TOTAL DEDUCTION FROM WAGES  TO TAXABEL FEDERAL WAGE
		if dedfica(j)=1 and dedcode(j)=1 then dedytdfica+=ytdtotal(j+4) : dedqtrfica+=qtr(j+4) ! TOTAL DEDUCTIONS FOR FICA FOR QUARTER
	next j
	m2=m2+ytdtotal(31)-dedytdfica ! TOTAL WAGES less deductions FOR THIS EMPLOYEE FOR YEAR
	m1=m1+qtr(31)-dedqtrfica ! TOTAL WAGES less deductions FOR QURATER
	m4=m4+qtr(31)-dedqtrfederal ! TOTAL Taxable WAGES less deductions FOR QURATER
	fedwh=fedwh+qtr(1) ! FEDERAL WH FOR QUARTER
	eicqtr+=qtr(25) ! EIC FOR QUARTER
	if m2=0 then goto L1140
	fn_print_details
	goto L1140
 
	WK_HEADER: !
	p2=p2+1
	pr #255,using L1600: "Page ",p2
	L1600: form pos 70,c 5,pic(zzz)
	pr #255: '                                 941 Summary'
	pr #255: tab(15);"Employer's Quarterly Federal Tax Return Worksheet"
	pr #255,using L1630: "For quarter ended "&m$&", "&str$(taxyear)
	L1630: form pos 20,cc 40
	pr #255: ""
	pr #255: ""
	if eof=1 then goto L1790
	pr #255,using L1680: a$(1),"Fed ID",b$(1)
	L1680: form pos 17,c 40,pos 59,c 6,pos 69,c 40
	pr #255,using L1700: a$(2),"State ID",e$(1)
	L1700: form pos 17,c 40,pos 59,c 8,pos 69,c 12,skip 1
	pr #255,using L1720: a$(3),"State",d$(1)
	L1720: form pos 17,c 40,pos 59,c 5,pos 69,c 8,skip 1
	pr #255: ""
	pr #255: tab(41);"Total Wages    Social-Sec.      Medicare"
	pr #255: " SS Number             Name";
	pr #255: tab(41);"For Quarter        Wages";tab(75);"Wages"
	pr #255: "___________  __________________________";
	pr #255: tab(41);"___________   ____________  ____________"
	L1790: return
 
	WK_END: !
	fn_totals
	pr #255: newpage
	on pageoflow ignore
fnend
FINIS: !
	fncloseprn
Xit: fnXit
def fn_print_details ! detailed listing
	if m1=0 then goto L2130
	p3=p3+1
	h2=h3=0
	if em6=2 or em6=9 then goto L2010
	if m2<ficamaxw then goto L2000
	if m2-m1>ficamaxw then goto L1980
	h2=ficamaxw-(m2-m1)
	goto L2010
	L1980: !
	h2=0
	goto L2010
	L2000: !
	h2=m1
	L2010: !
	if em6=1 or em6=9 then goto L2050
	if m2<mcm then h3=m1 : goto L2050 ! MCM = MEDICARE MAXIMUM WAGE
	if m2-m1>mcm then h3=0 : goto L2050
	h3=mcm-(m2-m1)
	L2050: !
	if frm=1 then goto L2090
	pr #255,using L2080: ss$,em$(1)(1:27),m4,h2,h3
	pr #255: ""
	L2080: form pos 1,c 11,pos 14,c 27,pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
	L2090: !
	t1=t1+m4
	t3=t3+h3
	t2=t2+h2
	p1=p1+2
	L2130: !
fnend
def fn_totals
	if frm=1 then goto L2230
	pr #255,using L2180: "___________    ___________  ____________"
	L2180: form pos 41,c 41,skip 1
	pr #255: "       Total Employees:";p3;"     Totals";
	pr #255,using L2210: t1,t2,t3
	L2210: form pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
	L2230: !
	p3=0
	gt1=gt1+t1
	gt2=gt2+t2
	gt3=gt3+t3
	gt4=gt4+t4
	t1=0
	t2=0
	t3=0
	t4=0
fnend
PgOf: ! r:
	pr #255: newpage
	gosub WK_HEADER
continue ! /r
def fn_build_941
	!   if env$('client')="Washington Parrish" then tpt(30)=0 ! used tips for something else
	wagefica=round((gt2-tpt(30))*(ficarate*2),2) ! 2013
	taxfica=round(tpt(30)*(ficarate*2),2) ! 2013 requirements that employee fica be 4.2 and employer be 6.2  (was ficarate*2)
	!   tipfica= round((tpt(30))*(ficarate*2),2) ! 2013    FICARATE)
	mcwh=round((gt3)*mcr*2,2)
	!   BOX1   ! count on 12th of each qtr (analyze history for this in another section)
	box2=gt1
	box3=fedwh
	box4=0 ! ( unused )
	box5a1=gt2-tpt(30) ! FICA WAGES LESS TIPS
	box5a2=wagefica
	box5b1=tpt(30) ! TIPS
	box5b2=taxfica
	box5c1=gt3
	box5c2=mcwh
	box5d=wagefica+taxfica+mcwh
	box6=box3+box5d ! total taxes before adj
	box7h=box7a+box7b+box7d+box7e+box7f+box7g ! total adjustments
	box8=box6-box7h ! total due after adjustments
	box9=eicqtr ! eic
	box10=box8-box9 ! total taxes after eic
	box11=box11 ! total deposits
	box12=box10-box11 ! blance due
	if box10-box11<0 then box13=abs(box10-box11) : box12=0 else box13=0 ! overpayment if any
	box15d=box15a+box15b+box15c ! total deposits for quarter
fnend
! LASER_941: ! r:
	fnpa_open
	fn_print941_origional
	close #2: ioerr ignore
	close #3: ioerr ignore
	!
	fnpa_finis
return  ! /r
include: ertn
def fn_print941_origional
	fnpa_fontsize(12)
	fnpa_pic("S:\acsPR\941.bmp",1,1)
	! fnpa_pic("S:\acsPR\941_2012_A.bmp",1,1)
	pr #20: 'Call Print.MyFont("Courier New")'
	for j=1 to 10
		x=val(b$(1)(j:j)) conv L2960 ! pull any spaces or non-numeric characters out of federal id#
		goto L2970
		L2960: !
		b$(1)(j:j)=""
		L2970: !
		if b$(1)(j:j)=" " then b$(1)(j:j)=""
		if b$(1)(j:j)="-" then b$(1)(j:j)=""
	next j
	fnpa_fontsize(16)
	lyne=15 ! starting line of fed id
	fnpa_txt(b$(1)(1:1),47,lyne)
	fnpa_txt(b$(1)(2:2),56,lyne)
	fnpa_txt(b$(1)(3:3),70,lyne)
	fnpa_txt(b$(1)(4:4),79,lyne)
	fnpa_txt(b$(1)(5:5),88,lyne)
	fnpa_txt(b$(1)(6:6),97,lyne)
	fnpa_txt(b$(1)(7:7),106,lyne)
	fnpa_txt(b$(1)(8:8),115,lyne)
	fnpa_txt(b$(1)(9:9),124,lyne)
	fnpa_fontsize(12)
	fnpa_txt(trim$(a$(1)),35,32)
	fnpa_txt(trim$(a$(2)),35,39)
	csz$=a$(3): fn_csz
	fnpa_txt(trim$(city$),35,48)
	fnpa_txt(trim$(state$),93,48)
	fnpa_txt(trim$(zip$),108,48)
	if qtr=1 then quarter=29 ! 1st quarter
	if qtr=2 then quarter=35 ! 2nd quarter
	if qtr=3 then quarter=41 ! 3rd quarter
	if qtr=4 then quarter=47 ! 4rd quarter
	fnpa_txt("X",143,quarter)
	tab1=63: tab2=113: tab3=160 ! WAS 62,112,159  ! 76,126,173 WORKED WHEN TO FAR RIGHT
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzzzzz)",box1),tab3,72)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box2),tab3,80)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box3),tab3,87)
	! fnpa_txt(CNVRT$("pic(-,---,---.##)",BOX4),TAB3,90)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5a1),tab1,108)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5a2),tab2,108)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5b1),tab1,116)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5b2),tab2,116)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5c1),tab1,123)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5c2),tab2,123)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box5d),tab3,131)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box6),tab3,138)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7a),tab2,150)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7b),tab2,158)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7c),tab2,165)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7d),tab2,172)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7e),tab2,180)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7f),tab2,187)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7g),tab2,194)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box7h),tab3,202)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box8),tab3,209)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box9),tab3,216)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box10),tab3,224)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box11),tab3,232)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box12),tab3,238)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box13),tab2,247)
	fnpa_newpage
	fnpa_pic("S:\acsPR\941-back.bmp",1,1)
	fnpa_txt(trim$(state$(1:1)),15,33)
	fnpa_txt(trim$(state$(2:2)),21,33)
	tab4=75
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box15a),tab4,61)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box15b),tab4,68)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box15c),tab4,74)
	fnpa_txt(cnvrt$("pic(-,---,---.##)",box15a+box15b+box15c),tab4,82)
fnend
def fn_csz ! EXTRACT  CITY$,STATE$,ZIP$ form CSZ$
	L3680: !
	p1=pos(csz$,".",1)
	if p1>0 then csz$(p1:p1)=" ": p2=p1: goto L3680
	! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
	p2=0
	L3720: !
	p1=pos(csz$,",",1)
	if p1>0 then csz$(p1:p1)=" ": p2=p1: goto L3720
	! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
	L3750: !
	p1=pos(rtrm$(csz$),"  ",1)
	if p1>0 then csz$(p1+1:p1+1)="" : goto L3750
	csz$=ltrm$(rtrm$(csz$)): p1=pos(csz$," ",-1)
	zip$=csz$(p1+1:len(csz$)): zip$=ltrm$(rtrm$(zip$))
	p2=pos(csz$(1:p1-1)," ",-1) : state$=csz$(p2+1:p1-1)(1:2) : state$=ltrm$(rtrm$(state$))
	city$=csz$(1:p2-1)(1:15): city$=ltrm$(rtrm$(city$))
fnend
def fn_print941_info ! pr 941 information
	pr #255: "Employer ID # (EIN): ";b$(1)
	pr #255: "Name (not trade name): "
	pr #255: "Trade Name: ";a$(1)
	pr #255: "Address: ";a$(2)
	pr #255: "City, State Zip: ";a$(3)
	pr #255:
	pr #255,using "form pos 5, cr 50,pic(zzzzzzzzzzz#)": "Number of Employees receiving pay for Quarter: ",box1
	pr #255,using "form pos 5, cr 50,pic(-,---,---.##)": "Wages, Tips & Other Compensation: ",box2
	pr #255,using "form pos 5, cr 50,pic(-,---,---.##)": "Income Tax Withheld: ",box3
	pr #255:
	pr #255,using "form pos 5,cr 18,x 1,2*pic(---,---,---.##)": "5a",box5a1,box5a2
	pr #255,using "form pos 5,cr 18,x 1,2*pic(---,---,---.##)": "5b",box5b1,box5b2
	pr #255,using "form pos 5,cr 18,x 1,2*pic(---,---,---.##)": "5c",box5c1,box5c2
	pr #255:
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "5d",box5d
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "6",box6
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7a",box7a
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7b",box7b
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7c",box7c
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7d",box7d
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7e",box7e
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7f",box7f
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7g",box7g
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7h",box7h
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "8",box8
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "9",box9
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": '10',box10
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "11",box11
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "12",box12
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "13",box13
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15a",box15a
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15b",box15b
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15c",box15c
	pr #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15a+15b+15c",box15a+box15b+box15c
fnend  ! fn_Print941_info
