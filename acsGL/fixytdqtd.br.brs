! Replace S:\acsGL\fixytdqtd
! -- PAYROLL REGISTER
 
	autoLibrary
	on error goto Ertn
 
	dim cnam$*40,miscname$(10)*20,dedcode(10),cap$*128,empd(22)
	dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
	dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
 
	fnTop(program$,cap$="Fix YTD - QTD Earnings")
	fncno(cno,cnam$) : _
	fndat(dat$)
 
	fnTos(sn$="FixYTDQTD") : _
	rc=cf=0: mylen=28: mypos=mylen+3: frameno=1
	fnFra(1,1,3,45,"Date Range to Fix Quarter To Date Earnings","Enter the date range for the payrolls to be included in this quarter. Leave blank to skip quarter.")
	fnLbl(1,1,"Beginning Date of Quarter:",mylen,1,0,frameno)
	fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno) : _
	resp$(rc+=1)=str$(beg_date)
	fnLbl(2,1,"Ending Date of Quarter:",mylen,1,0,frameno)
	fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this quarter. Blank if not fixing quarter.",frameno) : _
	resp$(rc+=1)=str$(end_date)
	frameno=2
	fnFra(6,1,3,45,"Date Range to Fix YTD Earnings.","Enter the date range for the payrolls to be included in year to date earnings. Leave blank to skip fixing the year to date earnings.")
	fnLbl(1,1,"Beginning Date of the Year:",mylen,1,0,frameno)
	fnTxt(1,mypos,12,0,1,"3",0,"Enter the first day of the year. Leave blank in only fixing the quarter.",frameno) : _
	resp$(rc+=1)=str$(begytd_date)
	fnLbl(2,1,"Ending Date of Year:",mylen,1,0,frameno)
	fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in then annual figures. Blank if not fixing year to date.",frameno) : _
	resp$(rc+=1)=str$(endytd_date)
	fnCmdKey("Next",1,1,0,"Fix earnings records.")
	fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
 
	beg_date=val(resp$(1)) : _
	end_date=val(resp$(2))
	begytd_date=val(resp$(3)) : _
	endytd_date=val(resp$(4))
	open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,outi,r: read #1,using 'Form POS 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,POS 407,PD 5.3,PD 5.2,POS 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode : _
	close #1:
	ficarate=ficarate/100 : feducrat=feducrat/100 : mcr=mcr/100
	nametab=66-int(len(rtrm$(cnam$))/2)
	open #1: "Name=[Q]\GLmstr\PRmstr.h[cno],KFName=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\GLmstr\ACPRCKS.h[cno],Shr",i,outi,r
L380: read #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m,mat adr eof Xit
	fixqtr=fixytd=0
	if beg_date>0 and end_date>0 then fixqtr=1: goto L410 else goto L440
L410: for j=2 to 36 step 2 ! set qtd to zero
		m(j)=0
	next j
L440: if begytd_date>0 and endytd_date>0 then fixytd=1: goto L450 else goto L480
L450: for j=1 to 35 step 2 ! set year to date to zero
		m(j)=0
	next j
L480: if adr(1)=0 then goto REWRITE_MASTER
	ca=adr(1)
L500: read #2,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca noRec REWRITE_MASTER
	if fixqtr=0 or (fndate_mmddyy_to_ccyymmdd(d(2))<beg_date or fndate_mmddyy_to_ccyymmdd(d(2))>end_date) then goto L560
	x=3
	for j=2 to 36 step 2
		m(j)+=d(x+=1) ! add quarterly info
	next j
L560: if fixytd=0 or (fndate_mmddyy_to_ccyymmdd(d(2))<begytd_date or fndate_mmddyy_to_ccyymmdd(d(2))>endytd_date) then goto L610
	x=3
	for j=1 to 35 step 2
		m(j)+=d(x+=1)
	next j
L610: if nca=0 then goto REWRITE_MASTER
	ca=nca
	goto L500
REWRITE_MASTER: !
	rewrite #1,using 'Form POS 1,N 4,3*C 25,C 11,36*PD 5.2,2*N 5': eno,mat k$,ss$,mat m
	goto L380
Xit: fnXit
 
include: ertn
