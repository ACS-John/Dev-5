! Replace S:\acsGL\AcPrReg
! -- PAYROLL REGISTER
 
	autoLibrary
	on error goto Ertn
 
	dim miscname$(10)*20,dedcode(10),cap$*128,empd(22)
	dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
	dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
 
	fnTop(program$,cap$="Print Payroll Registers")
	fndat(dat$)
 
	fnTos(sn$="PayrollReg")
	rc=cf=0: mylen=22: mypos=mylen+3: frameno=1
	fnFra(1,1,3,40,"Date Range for Report","Enter the date range for the payrolls to be included.")
	fnLbl(1,1,"Beginning Date:",mylen,1,0,frameno)
	fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno)
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
	fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this report. ",frameno)
	resp$(rc+=1)=str$(end_date)
	fnCmdKey("Next",1,1,0,"Calculate tax deposit.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
 
	beg_date=val(resp$(1))
	end_date=val(resp$(2))
 
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r: read #1,using 'form pos 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,pos 407,PD 5.3,PD 5.2,pos 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode : close #1:
	ficarate=ficarate/100 : feducrat=feducrat/100 : mcr=mcr/100
	open #h_prmstr=fnH: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",i,outIn,k
	fPrmstr: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
	open #h_acprcks=fnH: "Name=[Q]\GLmstr\AcPrCks.h[cno],Shr",i,outi,r
	report$="Payroll Check Register"
	fnOpenPrn
	fn_hdr1
L350: if d(1)>0 then goto L360 else goto L390
L360: if sum(empd)=0 then goto L390
	pr #255,using L870: eno,"Total",empd(4),empd(5),empd(6),empd(7),empd9,empd(8),empd(22),empd(19),empd(21) pageoflow PGOF1
	pr #255:
	mat empd=(0)
L390: read #h_prmstr,using 'form pos 1,N 4,3*C 25,pos 271,2*N 5': eno,mat k$,mat adr eof L650
! if eno=19 then pr eno : pause
	if adr(1)=0 then goto L390
	ca=adr(1)
	do
		read #h_acprcks,using 'form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca conv L350
		if fndate_mmddyy_to_ccyymmdd(d(2))<beg_date or fndate_mmddyy_to_ccyymmdd(d(2))>end_date then goto L470
		fn_941_breakdown
		fn_accumulate_totals
		fn_print_1
L470: if nca=0 then goto L350
		ca=nca
	loop
L650: ! pr TOTALS
	fn_totals_1
	pr #255: newpage
	fn_941_summary
	fn_report3
	goto Finis
 
def fn_header
	nametab=66-int(len(env$('cnam'))/2)
	pr #255,using L530: date$('mm/dd/yy'),time$,env$('cnam')
	L530: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
	p1=66-int(len(rtrm$(report$))/2)
	pr #255,using L560: rtrm$(report$)
	L560: form pos p1,c 50
	if report$="Payroll Check Register" then pr #255,using "form pos 40,cc 50": "From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To "&cnvrt$("pic(zzzz/zz/zz)",end_date): goto L590
	p1=66-int(len(rtrm$(dat$))/2)
	pr #255,using L560: rtrm$(fnpedat$)
	L590: !
fnend
def fn_hdr1
	fn_header
	pr #255:''
	pr #255,using 'form pos 1,c 132': "EMP #    EMPLOYEE NAME         GROSS   FED W/H  FICA W/H    ST W/H  MISC W/H   LOC W/H       NET     TIPS    EIC   DATE    CHK#"
	pr #255:''
fnend
def fn_accumulate_totals
	t1=t1+d(4) ! ACCUMULATE TOTALS
	t2=t2+d(5)
	t3=t3+d(6)
	t4=t4+d(7)
	d9=0
	for j=9 to 18
		if dedcode(j-8)=2 then d9=d9-d(j) else d9=d9+d(j)
	next j
	t5=t5+d9
	t6=t6+d(8)
	t7=t7+d(22)
	t8=t8+d(19)
	t9=t9+d(21)
	at8=at8+d(20)
fnend
def fn_print_1
	pr #255,using L870: eno,k$(1)(1:19),d(4),d(5),d(6),d(7),d9,d(8),d(22),d(19),d(21),d(2),d(3) pageoflow PGOF1
	mat empd=empd+d
	L870: form pos 1,pic(zzzz),pos 8,c 19,7*n 10.2,pic(------.##),pic(----.##),pic(zzz/zz/zz),pic(zzzzzzz),skip 1
fnend
PGOF1: ! r:
	pr #255: newpage
	fn_hdr1
continue ! /r
def fn_totals_1
	pr #255,using L930: "TOTALS",t1,t2,t3,t4,t5,t6,t7,t8,t9
	L930: form skip 1,pos 15,c 6,pos 27,7*n 10.2,pic(------.##),pic(----.##),skip 3
	u1=t2+round(t3*2,2)-t9 ! 2013
	pr #255,using L960: "TOTAL TAX DEPOSIT DUE", u1
	L960: form pos 1,c 21,pos 48,pic(---,---.##)
fnend
def fn_report3
	t1=0
	t2=0
	t3=0
	t4=0
	t5=0
	t6=0
	t7=0
	t8=0
	t9=0
	fn_hdr3
	restore #h_prmstr:
	! restore #h_PRmstr,key>="    ": nokey L1140 eof L1140 ! nokey Finis eof L1090
	do
		! L1090: !
		read #h_prmstr,using fPrmstr: eno,mat k$,mat l$,mat m,mat adr eof L1140
		fn_print_3
		fn_calk_3b
	loop
	L1140: !
fnend
def fn_hdr3
		report$="PAYROLL REGISTER - EARNINGS TO DATE"
		fn_header
		pr #255,using L1180: "*********************** Y.T.D. ***************************    **************** Q.T.D *******************"
L1180: form skip 1,pos 23,c 105,skip 1
		pr #255: "EMP #  EMPLOYEE NAME    GROSS   FED W/H FICA W/H   ST W/H  LOC W/H    TIPS     EIC      GROSS  FED W/H FICA W/H  ST W/H  LOC W/H"
fnend
Finis: !
	close #h_acprcks: ioerr L1230
L1230: !
	fn_totals_3
! pr #255,USING 40: HEX$("2B0205000A1042")
	fnClosePrn
	goto Xit
def fn_calk_3b
		t1=t1+m(1)
		t2=t2+m(3)
		t3=t3+m(5)
		t4=t4+m(7)
		l1=l1+m(9)
		l3=l3+m(31)
		l5=l5+m(35)
		t5=t5+m(2)
		t6=t6+m(4)
		t7=t7+m(6)
		t8=t8+m(8)
		l2=l2+m(10)
		l4=l4+m(32)
		l6=l6+m(36)
fnend
def fn_print_3
		pr #255,using L1450: eno,k$(1)(1:12),m(1),m(3),m(5),m(7),m(9),m(31),m(35),m(2),m(4),m(6),m(8),m(10) pageoflow PGOF3
L1450: form pos 1,pic(zzzz),pos 7,c 12,pic(----,---.##),pic(---,---.##),pic(--,---.##),pic(--,---.##),pic(--,---.##),pic(-----.##),pic(-----.##),pic(----,---.##),pic(------.##),pic(--,---.##),pic(-----.##),pic(-----.##),skip 1 ! 11/10/86 add 1 z to first pic
		goto PAST_PGOF3
PGOF3: !
		pr #255: newpage
		fn_hdr3
PAST_PGOF3: !
		fn_calc_3
		at6=at6+at5
		mcw2=mcw2+mcw1
		if m(1)-m(2)>=feducwag then goto L1550
		if m(1)<=feducwag then fuc1=m(2) else fuc1=feducwag-(m(1)-m(2))
		fuc2=fuc2+fuc1
L1550: !
fnend
def fn_totals_3
		pr #255,using L1570: "TOTALS",t1,t2,t3,t4,l1,l3,l5,t5,t6,t7,t8,l2
L1570: form skip 1,pos 10,c 6,pic(zzz,zzz,zzz.##),n 10.2,3*n 9.2,2*n 8.2,pic(zzzz,zzz.##),2*n 9.2,2*n 8.2,skip 3
		pr #255,using L1600: "TAXABLE SOC-SEC. WAGES FOR QUARTER",at6
		pr #255,using L1600: "TAXABLE MEDICARE WAGES FOR QUARTER",mcw2
L1600: form pos 41,c 35,pic(----,---,---.##),skip 1
		pr #255,using L1620: "TOTAL TIPS FOR QUARTER",l4
L1620: form pos 50,c 30,pos 80,pic(----,---.##),skip 1
		pr #255,using L1620: "TOTAL EIC FOR QUARTER",l6
		pr #255,using L1620: "TOTAL FED U/C WAGES QTD",fuc2
		pr #255,using L1620: "TOTAL FED U/C TAX QTD",fuc2*feducrat
fnend
def fn_941_breakdown
		tmp$=(lpad$(str$(d(2)),6))
		ckdat=val(tmp$(3:4))
		if ckdat>0 and ckdat<32 then x=ckdat else x=31
		deposit(x,1)=deposit(x,1)+d(4)
		deposit(x,2)=deposit(x,2)+d(5)+d(6)+d(6)-d(21) ! FEDERAL + DOUBLE SS - EIC
fnend
def fn_941_summary
		report$="941 BREAKDOWN"
		fn_header
		pr #255:
		pr #255: tab(43);"GROSS";tab(63);"TAXES"
L1790: form pos 5,c 30,pos 38,n 10.2,pos 58,n 10.2,skip 1
		for j=1 to 31
			pr #255,using L1790: "DAY "&str$(j),deposit(j,1),deposit(j,2)
			gross=gross + deposit(j,1)
			taxes=taxes+deposit(j,2)
		next j
		pr #255,using L1860: "----------","----------","       TOTALS",gross,taxes,"==========","=========="
L1860: form pos 38,c 10,pos 58,c 10,skip 1,pos 5,c 25,pos 38,n 10.2,pos 58,n 10.2,skip 1,pos 38,c 10,pos 58,c 10,skip 1
		pr #255: newpage
fnend
def fn_calc_3
		twy=m(1)
		twq=m(2)
		if twy<ficawage then at5=twq : goto L1940
		if twy-twq>ficawage then at5=0 : goto L1940
		at5=ficawage-(twy-twq)
L1940: !
		if twy<mcm then mcw1=twq : goto L1970
		if twy-twq>mcm then mcw1=0 : goto L1970
		mcw1=mcm-(twy-twq)
L1970: !
fnend
Xit: fnXit
 
include: ertn
