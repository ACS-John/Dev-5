! State U/C Report
	autoLibrary
	on error goto Ertn
! r: setup
	dim ss$*11,em$(3)*30,department$*128
	dim a$(3)*40,b$(2)*12,d$(10)*8
	dim m(10),r(10),cap$*128
	dim e$(10)*12,dedcode(20)
	dim calcode(20)
	dim dedfed(20)
	dim fullname$(20)*20
	dim abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
	dim option1$(10)
	dim qtr(32)
	dim ytdtotal(32)
	dim tcp(32),tdc(10)
	dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
	dim qtr_option$(4)*3
	qtr_option$(1)="1"
	qtr_option$(2)="2"
	qtr_option$(3)="3"
	qtr_option$(4)="4"
	dim quarter_ending_date$*20

	dim column$(4) ! columns to include on the report (True or False)
	dim ptotal(4)
	dim grand_total(4)
	dim col_amt_form$(4)*80
	col_amt_form$(1)='pic(--,---,---.zz)  '
	col_amt_form$(2)='pic(---,---,---.zz) '
	col_amt_form$(3)='pic(-,---,---.zz) '
	col_amt_form$(4)='pic(--,---,---.zz)'
	total_underline$(1)="___________   "
	total_underline$(2)=" ___________ "
	total_underline$(3)="__________   "
	total_underline$(4)="__________   "
	dim heading_1$(4)*20
	dim heading_2$(4)*20
	dim heading_3$(4)*20
	heading_1$(1)="Total Wages"
	heading_2$(1)="           "
	heading_3$(1)="___________"
	heading_1$(2)="   Excess Wages"
	heading_2$(2)="   Over $"&cnvrt$("pic(zzzzz)",0)&' '
	heading_3$(2)="   ____________"
	heading_1$(3)="    Taxable"
	heading_2$(3)="     Wages "
	heading_3$(3)="   ________"
	heading_1$(4)="     State   "
	heading_2$(4)="      W/H    "
	heading_3$(4)="  ___________"
 
	fnTop(program$,cap$="State UnEmp Report")
	fncreg_read('calculation date text',quarter_ending_date$)
	fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
	open #1: "Name=[Q]\PRmstr\Company.h[cno],Shr",i,i
	read #1,using 'form pos 1,3*C 40,2*C 12,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,PD 4.2,PD 4.2,10*PD 4.2,10*PD 3.3,10*C 12,pos 618,30*N 1': mat a$,mat b$,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
	close #1:
	ficamaxw=ficamaxw*10
	fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
	for j=1 to 10
		if trim$(e$(j))<>"" then
			x+=1
			option1$(x)=str$(j)&" = "&d$(j)
		end if
	next j
	mat option1$(max(1,x))
	fncreg_read('uc1 - quarter'                ,quarter$  ) : if quarter$  ='' then quarter$  ='1'
	fncreg_read('uc1 - show total wage'        ,column$(1)) : if column$(1)='' then column$(1)='True'
	fncreg_read('uc1 - show excess wage'       ,column$(2)) : if column$(2)='' then column$(2)='True'
	fncreg_read('uc1 - show taxable wage'      ,column$(3)) : if column$(3)='' then column$(3)='True'
	fncreg_read('uc1 - show state tax withheld',column$(4)) : if column$(4)='' then column$(1)='True'
! /r
! r: main screen (falls through to next section)
	fnTos(sn$="pruc1b")
	respc=0: x=0
	fnLbl(1,1,"Quarter Ending Date:",26,1)
	fnTxt(1,30,20,0,0,"",0,"Use alpha format (eg. March 31, 20xx)")
	if trim$(quarter_ending_date$)='' then resp$(respc+=1)=date$("Month DD, CCYY") else resp$(respc+=1)=quarter_ending_date$
	fnLbl(2,1,"State Code:",26,1)
	fnComboA("pruc1-2",2,30,mat option1$,"Enter the state code from the company information file for the state being processed.")
	resp$(respc+=1)=option1$(1)
	fnLbl(3,1,"Quarter Code:",26,1)
	fnComboA("pruc1-3",3,30,mat qtr_option$,"Enter the quarter you are processing.")
	resp$(respc+=1)=quarter$
	fnChk(5,40,"Round to Whole Dollars:",1)
	resp$(respc+=1)='False'
	fnChk(6,40,"Show Total Column:",1)
	resp$(respc+=1)=column$(1)
	fnChk(7,40,"Show Excess Wage Column:",1)
	resp$(respc+=1)=column$(2)
	fnChk(8,40,"Show Taxable Column:",1)
	resp$(respc+=1)=column$(3)
	fnChk(8,40,"Show State Tax Withheld:",1)
	resp$(respc+=1)=column$(4)
	fnLbl(10,1,"Payroll Department:",26,1)
	fnComboF("DeptName",10,30,29,"[Q]\PRmstr\DeptName.h[cno]",1,3,4,25,"[Q]\PRmstr\DeptNameIdx.h[cno]",2,0, " ",0,0)
	resp$(respc+=1)='[All]'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	quarter_ending_date$=resp$(1) ! quarter ending date
	stcode=val(resp$(2)(1:2)) ! state code
	heading_2$(2)="   Over $"&cnvrt$("pic(zzzzz)",m(stcode))&' '
	quarter_code=val(resp$(3)) ! quarter to analyze earnings
	fncreg_write('uc1 - quarter',resp$(3))
	if resp$(4)(1:1)="T" then round$="Y"
	column$(1)=resp$(5) ! want total wage column
	column$(2)=resp$(6) ! want excess wage column
	column$(3)=resp$(7) ! want taxable wage column
	column$(4)=resp$(8) ! True=want state tax withheld column
	fncreg_write('uc1 - show total wage',column$(1))
	fncreg_write('uc1 - show excess wage',column$(2))
	fncreg_write('uc1 - show taxable wage',column$(3))
	fncreg_write('uc1 - show state tax withheld',column$(4))
	department$=resp$(9)
	if round$="Y" then m(stcode)=round(m(stcode),0)
	if quarter_code=1 then ending_date=qtr2
	if quarter_code=2 then ending_date=qtr3
	if quarter_code=3 then ending_date=qtr4
	if quarter_code=4 then ending_date=end_date
! /r
	on fkey 5 goto Finis
	fnOpenPrn
	if file$(255)(1:3)="PRN" then redir=0 else redir=1
 
	open #2: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",i,i,k
	open #h_department:=3: "Name=[Q]\PRmstr\Department.h[cno],Shr, KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",i,outIn,k
	open #h_payrollchecks:=4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",i,outIn,k
	gosub HDR
TOP: !
	read #2,using "form pos 1,N 8,3*C 30,C 11": eno,mat em$,ss$ eof DONE
	if eno=149 then pause 
	m1=m2=h2=h3=dcq=dcy=0 : mat ytdtotal=(0)
	mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
	mat ytdtota(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #h_payrollchecks,key>=checkkey$: nokey ANALYZE_WAGES
MAIN_LOOP: ! r:
	read #h_payrollchecks,using "form pos 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
	if heno<>eno then goto ANALYZE_WAGES
	if prd<beg_date or prd>end_date then goto MAIN_LOOP ! not this year
	if department$<>'[All]' and tdn<>val(department$(1:3)) then goto MAIN_LOOP
	deptkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)
	read #h_department,using L810,key=deptkey$: sc1 nokey L830 ! no state code (assume this state)
L810: form pos 48,n 2
	if sc1<>stcode then goto MAIN_LOOP ! must be right state code
L830: if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
	if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
	if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
	if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
	if prd>=qtr1 and prd<=ending_date then mat ytdtotal=ytdtotal+tcp ! only total year to date wages to end of current quarter
	goto MAIN_LOOP ! /r
ANALYZE_WAGES: ! r: analyze wages on each person
	if quarter_code=1 then mat qtr=qtr1tcp
	if quarter_code=2 then mat qtr=qtr2tcp
	if quarter_code=3 then mat qtr=qtr3tcp
	if quarter_code=4 then mat qtr=qtr4tcp
	dcq=0 ! total wage for quarter
	for j=1 to 20
		if dedcode(j)=1 and deduc(j)=1 then
			dcy=dcy+ytdtotal(j+4)
			dcq=dcq+qtr(j+4)
		end if
	next j
! if env$('client')="Washington Parrish" then
!   m2=m2+ytdtotal(31)-dcy+ytdtotal(6)
!   goto L1010 ! add deferred comp match to uc wages on Wash Par
! end if
	m2=m2+ytdtotal(31)-dcy
! L1010: !
! if env$('client')="Washington Parrish" then
!   m1=m1+qtr(31)-dcq+qtr(6)
!   goto L1030 ! add deferred comp to qtr uc wages
! end if
	m1=m1+qtr(31)-dcq
! L1030: !
	if m2=0 then goto TOP
	state_wh=qtr(4)
	gosub SUBTOTALS
	goto TOP
! /r
DONE: ! r:
	eofcode=1
	gosub PAGE_TOTALS
	gosub HDR
	pr #255,using L1460: "Grand Totals:",grand_total(1),grand_total(2),grand_total(3)
L1460: form pos 5,c 15,pos 42,pic(--,---,---.zz),pos 57,pic(--,---,---.zz),pos 70,pic(----,---.zz),skip redir
	pr #255,using L1480: "U/C Tax Due:",round(grand_total(3)*r(stcode)*.01,2)
L1480: form pos 5,c 15,pic(--,---,---.zz)
Finis: close #2: ioerr ignore
	close #h_department: ioerr ignore
	fnClosePrn
	fnXit ! /r
SUBTOTALS: ! r:
	if m1=0 then goto L1780 ! skip IF QUARTERLY WAGE=0
	p3=p3+1
	if round$="Y" then m1=round(m1,0)
	if m2<m(stcode) then goto L1640
	if m2-m1>m(stcode) then goto L1620
	h2=m(stcode)-(m2-m1)
	if round$="Y" then h2=round(h2,0)
	goto L1650
L1620: !
	h2=0
	goto L1650
L1640: !
	h2=m1
L1650: !
	h3=m1-h2
	if round$="Y" then h3=round(h3,0)
	if column$(1)<>'True' then m1=0 ! no total column
	if column$(2)<>'True' then h3=0 ! no excess column
	if column$(3)<>'True' then h2=0 ! no taxable column
	if column$(4)<>'True' then state_wh=0 ! no state withholding column
! pr #255,using L1710: ss$,em$(1)(1:28),m1,h3,h2,state_wh
! L1710: form pos 1,c 11,pos 14,c 28,pos 42,pic(--,---,---.zz),pos 57,pic(--,---,---.zz),pos 70,pic(----,---.zz),pos 83,pic(----,---.zz),skip 1
	pr #255: rpad$(ss$,11)&'   '&em$(1)(1:28);
	col_amt(1)=m1
	col_amt(2)=h3
	col_amt(3)=h2
	col_amt(4)=state_wh
	fn_print_line_amt(mat col_amt,mat col_amt_form$)
	for col_item=1 to udim(mat column$)
		ptotal(col_item)+=col_amt(col_item)
		grand_total(col_item)+=col_amt(col_item) ! grand totals
	next col_item
	pr #255: pageoflow PgOf
	p1=p1+2
L1780: !
	return  ! /r
PAGE_TOTALS: ! r:
	fn_print_line_str(mat total_underline$,44)
	pr #255: "Employees on this page:";p3;"    Page Totals";
! pr #255,using L1880: ptotal(1),ptotal(2),ptotal(3),ptotal(4)
	fn_print_line_amt(mat ptotal,mat col_amt_form$)
	if nw=1 and eofcode=1 then goto L1910
	pr #255: newpage
	L1910: !
	p3=0
	mat ptotal=(0)
return  ! /r
PgOf: ! r:
	gosub PAGE_TOTALS
	gosub HDR
continue  ! /r
Xit: fnXit
HDR: ! r:
! r: page heading
	pr #255,using L1090: "Page ",p2+=1
L1090: form pos 70,c 5,pic(zzz)
	pr #255: ''
	pr #255: "\qc  {\f201 \fs24 \b Schedule A - Employer's Report of Wages Paid to Each Employee}"
	pr #255: "\ql   "
	pr #255,using L1150: "For quarter ended "&quarter_ending_date$
	if department$<>'[All]' then pr #255,using L1150: "Department "&department$
L1150: form pos 20,cc 40
	pr #255:
	pr #255,using L1180: "     Rate",a$(1),"Fed ID",b$(1)
L1180: form pos 1,c 9,pos 17,c 40,pos 59,c 6,pos 69,c 40,skip 1
	if stcode=0 then stcode=1
	pr #255,using L1210: r(stcode),a$(2),"State ID",e$(stcode)
L1210: form pos 3,pic(zzzz.##),pos 17,c 40,pos 59,c 8,pos 69,c 12,skip 1
	pr #255,using L1230: a$(3),"STATE",d$(stcode)
L1230: form pos 17,c 40,pos 59,c 5,pos 69,c 8,skip 1
	pr #255:
! /r
! r: column headings
	fn_print_line_str(mat heading_1$, 44)
	pr #255: " SS Number             Name              ";
	fn_print_line_str(mat heading_2$, 44)
	pr #255: "___________  __________________________";
	fn_print_line_str(mat heading_3$, 44) ! underlines
! /r
return  ! /r
def fn_print_line_str(mat pl_str$; pl_pos)
	dim pl_line$*256
	pl_line$=''
	if pl_pos=0 then pl_pos=42
	for pl_item=1 to udim(mat column$)
		if column$(pl_item)='True' then
			pl_line$=pl_line$&pl_str$(pl_item)
		end if
	next pl_item
	pr #255,using 'form pos pl_pos,c': pl_line$
fnend
def fn_print_line_amt(mat pl_amt,mat pl_form$; pl_pos)
	pl_line$=''
	if pl_pos=0 then pl_pos=42
	for pl_item=1 to udim(mat column$)
		if column$(pl_item)='True' then
			pl_line$=pl_line$&cnvrt$(pl_form$(pl_item),pl_amt(pl_item))
		end if
	next pl_item
	pr #255,using 'form pos pl_pos,c': pl_line$
fnend
include: ertn
