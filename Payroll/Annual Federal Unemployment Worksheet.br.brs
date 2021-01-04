! formerly S:\acsPR\newprFedUC
! Annual Federal U/C Worksheet
 
autoLibrary
on error goto Ertn
 
dim a$(3)*40
dim b$(2)*12
dim dedcode(20),calcode(20),dedfed(20)
dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
dim resp$(15)*30
dim tcp(32),tdc(10)
dim ytdtotal(32),ss$*11,em$(3)*30
 
 
fnTop(program$)
fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
fnGetPayrollDates(beg_date,end_date)
open #20: "Name=[Q]\PRmstr\Company.h[cno],Shr",internal,input
read #20,using L230: mat a$,b$(1),mcr,mcm,feducrat,feducmax
L230: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,x 80,x 2,pd 4.2
close #20:
 
! If FNPROCESS=1 Then Goto 230
! MENU1: !
fnTos
respc=0 : lc=0
col1len=29 : col2pos=31
fnLbl(lc+=1,43," ",1,1)
fnLbl(lc,1,"Beginning Date of Tax Year:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"3",0,"If filing annually, this would be the first day of the year.")
resp$(respc+=1)=str$(beg_date)
fnLbl(lc+=1,1,"Ending Date of Tax Year:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"3",0,"If filing annually, this would be the last day of the year.")
resp$(respc+=1)=str$(end_date)
fnLbl(lc+=1,1,"Deposits Made:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"10",0,"Total deposits made for this time frame.")
resp$(respc+=1)=str$(deposits)
lc+=1
fnLbl(lc+=1,1,"FUTA Tax Liability 1st Qtr:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"10",0,"Total FUTA Tax Libality for the first quarter.")
resp$(resp_qtr1:=respc+=1)=str$(futaqtr1)
fnLbl(lc+=1,1,"FUTA Tax Liability 2nd Qtr:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"10",0,"Total FUTA Tax Libality for the second quarter.")
resp$(resp_qtr2:=respc+=1)=str$(futaqtr2)
fnLbl(lc+=1,1,"FUTA Tax Liability 3rd Qtr:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"10",0,"Total FUTA Tax Libality for the third quarter.")
resp$(resp_qtr3:=respc+=1)=str$(futaqtr3)
fnLbl(lc+=1,1,"FUTA Tax Liability 4th Qtr:",col1len,1)
fnTxt(lc,col2pos,12,0,0,"10",0,"Total FUTA Tax Libality for the fourth quarter.")
resp$(resp_qtr4:=respc+=1)=str$(futaqtr4)
lc+=1
fnFra(lc+=1,1,5,40,"Option for printing","The system can print the form or just fill in the blanks on a pre-printed form.",0)
	lc=0
	frameId=1
	fnOpt(lc+=1,2,"Print complete form",0,frameId)
	resp$(resp_OptComplete:=respc+=1)="True"
	fnOpt(lc+=1,2,"Fill in the blanks",0,frameId)
	resp$(resp_OptFillIn:=respc+=1)="False"
	lc+=1 : col1len=12 : col2pos=14
	fnLbl(lc+=1,1,"Top Margin:",col1len,1,0,frameId)
	fnTxt(lc,col2pos,3,0,0,"30",0,"Reduce the top margin to move the pr up. Increse to move down.",frameId)
	resp$(resp_top:=respc+=1)=str$(8)
	fnLbl(lc+=1,1,"Left Margin:",col1len,1,0,frameId)
	fnTxt(lc,col2pos,3,0,0,"30",0,"Reduce the left margin to move the pr left. Increse to move right.",frameId)
	resp$(resp_left:=respc+=1)=str$(5)
 
fnCmdSet(2)
fnAcs(mat resp$,ckey)
if ckey=5 then goto Xit
beg_date=val(resp$(1)) ! beginning of year
end_date=val(resp$(2)) ! ending day of year
deposits=val(resp$(3))
topmargin=val(resp$(resp_top))
leftmargin=val(resp$(resp_left))
futaqtr1=val(resp$(resp_qtr1))
futaqtr2=val(resp$(resp_qtr2))
futaqtr3=val(resp$(resp_qtr3))
futaqtr4=val(resp$(resp_qtr4))
if resp$(resp_OptComplete)="True" then fullform=1 ! pr full form
if resp$(resp_OptFillIn)="True" then fullform=2 ! fill in blanks
 
fnopenprn
on pageoflow goto PGOF
open #hEmployee=fnH: "Name=[Q]\PRmstr\Employee.h[cno],KFName=[Q]\PRmstr\EmployeeIdx-no.h[cno],Shr",internal,input,keyed ! was #2
gosub HDR
open #4: "Name=[Q]\PRmstr\payrollchecks.h[cno],KFName=[Q]\PRmstr\checkidx.h[cno]",internal,outIn,keyed
open #3: "Name=[Q]\PRmstr\Department.h[cno],Shr, KFName=[Q]\PRmstr\DeptIdx.h[cno],Shr",internal,outIn,keyed
do
	read #hEmployee,using L730: eno,mat em$,ss$,em5,em6 eof EOF2
	L730: form pos 1,n 8,3*c 30,c 11,pos 120,2*n 2
	m2=dedytdfeduc=0
	mat ytdtotal=(0)
	checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
	restore #4,key>=checkkey$: nokey ANALYZE_WAGES
	L780: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
	if heno<>eno then goto ANALYZE_WAGES
	if prd<beg_date or prd>end_date then goto L780 ! not this year
	mat ytdtotal=ytdtotal+tcp
	goto L780
	ANALYZE_WAGES: ! analyze wages on each person
	for j=1 to 20
		if deduc(j)=1 and dedcode(j)=1 then dedytdfeduc+=ytdtotal(j+4) ! TOTAL DEDUCTIONS FOR federal u/c FOR QUARTER
	next j
	m2=m2+ytdtotal(31)-dedytdfeduc ! TOTAL WAGES less deductions FOR THIS EMPLOYEE FOR YEAR
	if m2 then
		gosub PRINT_LINE
	end if
loop
HDR: ! r:
	pr #255,using 'Form POS 20,Cc 40,Cr 20': env$('program_caption'),"Page "&str$(p2+=1)
	pr #255,using 'Form POS 20,CC 40': "For year ending "&cnvrt$("pic(zzzz/zz/zz)",end_date)
	pr #255: ""
	pr #255,using L940: "     Rate",a$(1),"Fed ID",b$(1)
	L940: form pos 1,c 9,pos 17,c 40,pos 59,c 6,pos 69,c 40
	pr #255,using 'Form POS 3,PIC(ZZZZ.##),POS 17,C 40': feducrat,a$(2)
	pr #255,using 'Form POS 17,C 40': a$(3)
	pr #255: ""
	pr #255: tab(44);"Total Wages   Excess Wages    Taxable"
	pr #255: " SS Number             Name";
	pr #255,using L1010: "   For Year   Over $",feducmax,"Wages"
	L1010: form pos 44,c 20,pic(zzzzz.##),pos 75,c 5
	pr #255: "___________  __________________________";tab(44);"___________   ____________    _______"
return ! /r
 
EOF2: !
	gosub TOTALS
	fncloseprn
	close #hEmployee: ioerr ignore
	close #3: ioerr ignore
	gosub PRINT_940
	
Xit: fnXit
 
PRINT_LINE: !
	tw=0
	if m2>=feducmax then tw=feducmax else tw=min(feducmax,m2)
	pr #255,using L1200: ss$,em$(1)(1:28),m2,max(m2-feducmax,0),tw
	L1200: form pos 1,c 11,pos 14,c 28,pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##)
	t1+=m2 : t2+=max(m2-feducmax,0) : t3+=tw
	pr #255: "" pageoflow PGOF
return
 
TOTALS: ! r:
	pr #255: tab(44);"___________    ___________  _________"
	pr #255,using L1280: "Grand Totals",t1,t2,t3
	L1280: form pos 28,c 12,pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##)
	pr #255: ""
	pr #255,using L1310: "Total Futa Tax",t3*feducrat/100
	L1310: form pos 26,c 14,pos 42,pic(--,---,---.##)
	fncloseprn
return ! /r
 
PGOF: pr #255: newpage : gosub HDR : continue
 
PRINT_940: ! r: only fills in the blanks at this time
	! r: VBOPENPRINT
	if file(20)=-1 then
		fnpa_open ('','940-PR','PDF')! open #20: "Name=[Q]\PRmstr\940"&wsid$&".txt,Replace,RecL=5000",display,output
		lyne=margin ! starting of 1st line
	end if
	column2=103
	column3=153
	column4=119.5
	! /r
	fnpa_font
	fnpa_fontsize(20)
	if fullform=1 then
		fnpa_background('S:\Core\pdf\2018\940-PR\Page 1.pdf')
		! fnpa_pic("S:\acsPR\Form 940 Front.bmp",1,1)
		! fnpa_pic("S:\acsPR\2009.bmp",34+leftmargin,5)
		! fnpa_pic("S:\acsPR\2010.bmp",34+leftmargin,5)
		! fnpa_pic("S:\acsPR\2011.bmp",34+leftmargin,5)
		! fnpa_pic("S:\acsPR\2012.bmp",34+leftmargin,5)
		! fnpa_pic("S:\acsPR\2013.bmp",34+leftmargin,5)
		! fnpa_pic("S:\acsPR\2014.bmp",34+leftmargin,5)
	end if
	fnpa_fontsize(12)
	for j=1 to 10
		x=val(b$(1)(j:j)) conv L1610 ! pull any spaces or non-numeric characters out of federal id#
		goto L1620
		L1610: !
		b$(1)(j:j)=""
		L1620: !
		if b$(1)(j:j)=" " then b$(1)(j:j)=""
		if b$(1)(j:j)="-" then b$(1)(j:j)=""
	next j
	fnpa_fontsize(16)
	lyne=13.5+topmargin ! starting line of fed id
	fnpa_txt(b$(1)(1:1),45+leftmargin,lyne)
	fnpa_txt(b$(1)(2:2),55+leftmargin,lyne)
	fnpa_txt(b$(1)(3:3),68+leftmargin,lyne)
	fnpa_txt(b$(1)(4:4),77+leftmargin,lyne)
	fnpa_txt(b$(1)(5:5),86+leftmargin,lyne)
	fnpa_txt(b$(1)(6:6),95+leftmargin,lyne)
	fnpa_txt(b$(1)(7:7),104+leftmargin,lyne)
	fnpa_txt(b$(1)(8:8),113+leftmargin,lyne)
	fnpa_txt(b$(1)(9:9),122+leftmargin,lyne)
	fnpa_fontsize(12)
	fnpa_txt(trim$(a$(1)),32+leftmargin,30+topmargin)
	fnpa_txt(trim$(a$(2)),32+leftmargin,39+topmargin)
	fncsz(a$(3),city$,state$,zip$)
	fnpa_txt(trim$(city$),32+leftmargin,47+topmargin)
	fnpa_txt(trim$(state$),88+leftmargin,47+topmargin)
	fnpa_txt(trim$(zip$),102+leftmargin,47+topmargin)
	fnpa_txt(trim$(state$(1:1)),100+leftmargin,72+topmargin)
	fnpa_txt(trim$(state$(2:2)),111+leftmargin,72+topmargin)
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t1),column3+leftmargin,103+topmargin) ! total wages
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",0),column2+leftmargin,111+topmargin) ! exempt payments
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t2),column2+leftmargin,130+topmargin) ! excess of maximum
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t2),column3+leftmargin,139+topmargin) ! subtotal
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t3),column3+leftmargin,147+topmargin) ! taxable wages
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t3*feducrat/100),column3+leftmargin,155+topmargin) ! tax
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t3*feducrat/100),column3+leftmargin,199+topmargin) ! tax
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",deposits),column3+leftmargin,208+topmargin) ! deposits
	if (t3*feducrat/100)-deposits>0 then let fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",(t3*feducrat/100)-deposits),column3+leftmargin,222.5+topmargin) ! due
	if (t3*feducrat/100)-deposits<=0 then let fnpa_txt(cnvrt$("pic(-----------.##)",abs((t3*feducrat/100)-deposits)),column3+leftmargin,31+topmargin) ! overpaid
	fnpa_newpage
	fnpa_background('S:\Core\pdf\2018\940-PR\Page 2.pdf')
	! fnpa_pic("S:\acsPR\Form 940 pg2.bmp",1,1)
	
	if fullform=1 then x=2.5: y=1 else x=0: y=0 ! adjust for bad alignment
	lyne=26.4 +topmargin-x : leftmargin=leftmargin-y
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr1),column4+leftmargin,lyne+=8.2) ! tax liability for 1st qtr
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr2),column4+leftmargin,lyne+=8.2) ! tax liability for 2nd qtr
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr3),column4+leftmargin,lyne+=8.2) ! tax liability for 3rd  qtr
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr4),column4+leftmargin,lyne+=8.2) ! tax liability for 4th  qtr
	fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr1+futaqtr2+futaqtr3+futaqtr4),column4+leftmargin,lyne+=8.2+margin) ! total liability
	fnpa_finis
return
include: ertn
