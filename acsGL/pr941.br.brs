! Replace S:\acsGL\PR941  ! fix the count (box 1; needs logic to look thru history and count the active employees on a certain date)
! 941 Summary  ( Prints a detail of employees and the complete 941 using priint ace

autoLibrary
fnTop(program$,"Print 941 Report")
on error goto Ertn
dim io1$(2),ss$*11,em$(3)*30,ty(21),tqm(17),frm_wrd$(2)*11
dim dedcode(10),calcode(10),dedfed(10),option1$(4)*20
dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10),msgline$(2)*60
dim e$(10)*12,tpt(26),pt(26)
dim message$*40,option$(4)*15,resp$(15)*30
dim d(2),e$(2)*12,prgl(5,3),miscname$(10)*20,dedfica(10),dedst(10),deduc(10),miscgl$(10)*12
dim tb$*30,m(36),k(1),l$(1)*11,k$(3)*30
dim city$*15,state$*2,zip$*9,csz$*40,x$*40




open #1: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative
read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
close #1:
ficarate=ficarate/100
mcr=mcr*.01
MENU1: !
	fnTos
	respc=0
	if val(date$(4:5))=1 then taxyear=val(date$(1:2))+2000-1 else taxyear =val(date$(1:2))+2000 ! current tax year (if processing in jan, assume last year)
	fnLbl(1,1,"Tax Year:",26,1)
	fnTxt(1,30,4,0,0,"30",0,"")
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
	resp$(respc+=1)="True"
	fnFra(5,1,4,30,"Tax Liability","Enter the total tax liability by month")
	fnLbl(1,1,"Month 1:",10,1,0,1)
	fnTxt(1,13,12,0,1,"10",0,"",1)
	resp$(respc+=1)=""
	fnLbl(2,1,"Month 2:",10,1,0,1)
	fnTxt(2,13,12,0,1,"10",0,"",1)
	resp$(respc+=1)=""
	fnLbl(3,1,"Month 3:",10,1,0,1)
	fnTxt(3,13,12,0,1,"10",0,"",1)
	resp$(respc+=1)=""
	fnFra(11,1,7,72,"Adjustments","Enter any applicable adjustments")
	mylen=52
	fnLbl(1,1,"Current quarter's fraction of cents:",mylen,1,0,2)
	fnTxt(1,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(2,1,"Current quarter's sick pay:",mylen,1,0,2)
	fnTxt(2,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(3,1,"Current quarter's adjustments for tips and ins:",mylen,1,0,2)
	fnTxt(3,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(4,1,"Current year's income tax withholding:",mylen,1,0,2)
	fnTxt(4,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(5,1,"Prior quarters' ss and medicare taxes:",mylen,1,0,2)
	fnTxt(5,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(6,1,"Special Additions to Federal income taxes:",mylen,1,0,2)
	fnTxt(6,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(7,1,"Special Additions to ss and medicare:",mylen,1,0,2)
	fnTxt(7,mylen+3,12,0,1,"10",0,"",2)
	resp$(respc+=1)=""
	fnLbl(20,1,"Total deposits for quarter including overpayments:",mylen+1,1,0,0)
	fnTxt(20,mylen+4,12,0,1,"10",0,"",0)
	resp$(respc+=1)=""
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	taxyear$=resp$(1) ! tax year
	for j=1 to 4
		if resp$(2)=option1$(j) then qtr=j: m$=option1$(j): goto L790 ! quarter ending date
	next j
L790: if qtr=1 then begdate=val(taxyear$)*10000+0312: enddate=val(taxyear$)*10000+0318
	if qtr=2 then begdate=val(taxyear$)*10000+0612: enddate=val(taxyear$)*10000+0618
	if qtr=3 then begdate=val(taxyear$)*10000+0912: enddate=val(taxyear$)*10000+0918
	if qtr=4 then begdate=val(taxyear$)*10000+1212: enddate=val(taxyear$)*10000+1218
	if resp$(3)="True" then frm=2 else frm=1 ! need a worksheet
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
	box11=val(resp$(14)) ! total deposits
! Gosub GET_MAT_TPT
	gosub START_PRINT
	gosub BUILD_941
	gosub LASER_941
	goto DONE

START_PRINT: !
	if frm=1 then goto L1070
	message$="Printing: please wait..."
	fnwait(message$,1)
	on fkey 5 goto DONE
	fnopenprn
	on pageoflow goto PGOF
L1070: open #2: "Name=[Q]\GLmstr\PRmstr.h[cno],Kfn_ame=[Q]\GLmstr\PRIndex.h[cno],Shr",internal,input,keyed
	if frm=2 then gosub WK_HEADER
L1090: m1=0
	m2=0
	h2=0
	h3=0
	extram1=0
	dfy=dfq=0
! Read #2,Using 1120: ENO,MAT EM$,SS$,EM5,EM6,TA Eof WK_END
	read #2,using L1170: mat k,mat k$,mat l$,mat m eof WK_END
L1170: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
!  Form POS 1,N 8,3*C 30,C 11,POS 120,2*N 2,POS 173,PD 3
	for j=1 to 10
		if dedfed(j)=1 and dedcode(j)=1 then dwy+=m(j+10): dwq+=m(j+11)
		if dedfica(j)=1 and dedcode(j)=1 then dfy=dfy+m(j+10): dfq=dfq+m(j+11)
	next j
	form pos 168,21*pd 5.2,pos 273,17*pd 5.2,pos 468,pd 3
	m2=m2+m(1)
	m1=m1+m(2)
	tqm1=tqm1+m(4)
	tqm14=tqm14+m(36)
	if m2=0 then goto L1090
! will need a routine here to read payroll dates, but think the checks are cleared each month
! kEY$=CNVRT$("pic(ZZZZZZZZ)",ENO)&RPT$(CHR$(0),6) : _
	! Restore #4,Key>=KEY$:
! Read #4,Using 1270: DEPENO,PRD Nokey 1360 Eof 1360
! Form POS 1,N 8,PD 6
! If EM5=1 Then pEDATE=BEGDATE+19 ! monthly pay period
! If EM5=2 Then pEDATE=BEGDATE+15 ! semi-monthly
! If EM5=3 Then pEDATE=BEGDATE+14 ! bi-weekly
! If EM5=4 Then pEDATE=BEGDATE+7 ! weekly
! If DEPENO<>ENO Then Goto 1360
! If DEPENO=ENO AND PRD<BEGDATE Then Goto 1260 ! search for next check
! If DEPENO=ENO AND PRD>PEDATE Then Goto 1360 ! do't count
! If DEPENO=ENO Then bOX1+=1 ! count employees who worked the twelfth day of the first month of the quarter
	gosub PRINT_DETAILS
	goto L1090

WK_HEADER: !
	p2=p2+1
	pr #255,using L1480: "Page ",p2
	pr #255: ""
L1480: form pos 70,c 5,pic(zzz)
	pr #255: tab(15);"Employer's Quarterly Federal Tax Return Worksheet"
	pr #255,using L1510: "For quarter ended "&m$&", "&taxyear$
L1510: form pos 20,cc 40
	pr #255: ""
	pr #255: ""
	if eof=1 then goto L1670
	pr #255,using L1560: a$(1),"Fed ID",b$(1)
L1560: form pos 17,c 40,pos 59,c 6,pos 69,c 40
	pr #255,using L1580: a$(2),"State ID",b$(2)
L1580: form pos 17,c 40,pos 59,c 8,pos 69,c 12,skip 1
	pr #255,using L1600: a$(3),"State",d$(1)
L1600: form pos 17,c 40,pos 59,c 5,pos 69,c 8,skip 1
	pr #255: ""
	pr #255: tab(41);"Total Wages    Social-Sec.      Medicare"
	pr #255: " SS Number             Name";
	pr #255: tab(41);"For Quarter        Wages";tab(75);"Wages"
	pr #255: "___________  __________________________";
	pr #255: tab(41);"___________   ____________  ____________"
L1670: return

WK_END: !
	gosub TOTALS
	if frm=1 then goto L1730
	fncloseprn
L1730: return
DONE: !
Xit: fnXit

PRINT_DETAILS: ! detailed listing
	if m1=0 then goto L2030
	p3=p3+1
	h2=h3=0
! If EM6=2 OR EM6=9 Then Goto 1840
	if m2-dfy<ficawage then goto L1880
	if (m2-dfy)-(m1-dfq)>ficawage then goto L1860
	h2=ficawage-(m2-dfy-m1-dfq)
	goto L1890
L1860: h2=0
	goto L1890
L1880: h2=m1-dfq
L1890: ! If EM6=1 OR EM6=9 Then Goto 1880
	if m2-dfy<mcm then h3=m1-dfq : goto L1930
	if m2-dfy-m1-dfq>mcm then h3=0 : goto L1930
	h3=mcm-(m2-dfy-m1-dfq)
L1930: !
	if frm=1 then goto L1980
	pr #255,using L1970: ss$,k$(1)(1:27),m1-dfq,h2,h3
	pr #255: ""
L1970: form pos 1,c 11,pos 14,c 27,pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
L1980: t1=t1+m1-dfq
	t5=t5+m1
	t3=t3+h3
	t2=t2+h2
	p1=p1+2
L2030: return

TOTALS: !
	if frm=1 then goto L2130
	pr #255,using L2080: "___________    ___________  ____________"
L2080: form pos 41,c 41,skip 1
	pr #255: "       Total Employees:";p3;"     Totals";
	pr #255,using L2110: t1,t2,t3
L2110: form pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
	fncloseprn
L2130: p3=0
	gt1=gt1+t5-dwq
	gt2=gt2+t2
	gt3=gt3+t3
	gt4=gt4+t4
	t1=t2=t3=t4=t5=0
return

PGOF: !
	pr #255: newpage
	gosub WK_HEADER
	continue

SUMMARY: !
	fnopenprn
	eof=1: gosub WK_HEADER
	wagefica=fn_a((gt2-m(32))*(ficarate+ficarate+.02)) ! FICARATE*2) ! pull tips out  2011
	taxfica=fn_a(m(32)*(ficarate+ficarate+.02)) ! FICARATE*2)  2011
	tipfica= fn_a((m(2)-m(32))*ficarate)
	mcwh=fn_a((gt3)*mcr*2)
	pr #255: ""
	pr #255,using L2360: " 2.  Total wages and tips subject to withholding",gt1
L2360: form pos 1,c 60,n 10.2
	pr #255,using L2360: " 3.  Total income tax withheld",tqm1
	pr #255: "" : _
	pr #255: ""
	pr #255,using L2400: " 6ab. Taxable social security wages paid",gt2-m(32),wagefica ! 6a. and 6b.
L2400: form pos 1,c 40,n 10.2,x 10,n 10.2
	pr #255,using L2400: " 6cd. Taxable tips reported",tpt(20),taxfica ! 6c. and 6d.
	pr #255,using L2400: " 7ab. Taxable medicare insurance wages",gt3,mcwh ! 7a. and 7b.
	pr #255,using L2360: " 8.  Total FICA taxes",wagefica+taxfica+mcwh
	for j=1 to 5 : _
		pr #255: "" : _
	next j
	pr #255,using L2360: "11.  Total taxes excluding any adjustments you have made",tqm1+wagefica+taxfica+mcwh
	pr #255,using L2360: "12.  Advanced earned income credit",tqm14
	pr #255,using L2360: "13.  Net Taxes        ",(tqm1+wagefica+taxfica+mcwh)-tqm14
	for j=1 to 6 : _
		pr #255: "" : _
	next j
return

GET_MAT_TPT: !
	open #19: "Name=[Q]\GLmstr\PRTOT.h[cno],Kfn_ame=[Q]\GLmstr\PRTOTIDX.h[cno],Shr",internal,input,keyed
L2530: read #19,using L2540: mo,mat pt eof L2600
L2540: form pos 1,n 2,pos 10,25*pd 5.2,n 4
	if qtr=1 and mo>0 and mo<4 then mat tpt=tpt+pt
	if qtr=2 and mo>3 and mo<7 then mat tpt=tpt+pt
	if qtr=3 and mo>6 and mo<10 then mat tpt=tpt+pt
	if qtr=4 and mo>10 then mat tpt=tpt+pt
	goto L2530
L2600: close #19:
return

BUILD_941: !
	wagefica=fn_a((gt2-tpt(20))*(ficarate+ficarate+.02)) ! FICARATE*2)  2011
	taxfica=fn_a(tpt(20)*(ficarate+ficarate+.02)) ! FICARATE*2) 2011
	tipfica= fn_a((tpt(20)-tpt(15))*ficarate)
	mcwh=fn_a((gt3)*mcr*2)
!   BOX1   ! count on 12th of each qtr (analyze history for this in another section)
	box2=gt1
	box3=tqm1
	box4=0 ! ( unused )
	box5a1=gt2-tpt(20)
	box5a2=wagefica
	box5b1=tpt(20)
	box5b2=taxfica
	box5c1=gt3
	box5c2=mcwh
	box5d=wagefica+taxfica+mcwh
	box6=box3+box5d ! total taxes before adj
	box7h=box7a+box7b+box7d+box7e+box7f+box7g ! total adjustments
	box8=box6-box7h ! total due after adjustments
	box9=tqm14 ! eic
	box10=box8-box9 ! total taxes after eic
	box12=box10-box11 ! blance due
	if box10-box11<0 then box13=abs(box10-box11) : box12=0 else box13=0 ! overpayment if any
	box15d=box15a+box15b+box15c ! total deposits for quarter
return

LASER_941: !
	gosub OPEN_PRINTER
	gosub PRINT941
	gosub RELEASE_PRINT
return

OPEN_PRINTER: !
	if file(20)=-1 then
		open #20: "Name=[Q]\GLmstr\Pr941"&wsid$&".txt,Replace,RecL=5000",display,output
		pr #20: 'Call Print.MyOrientation("Portrait")'
	end if
return

PRINT941: !
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.addPicture("S:\acsGL\941.bmp",1,1)'
	pr #20: 'Call Print.MyFont("Courier New")'
	for j=1 to 10
		x=val(b$(1)(j:j)) conv L3180 ! pull any spaces or non-numeric characters out of federal id#
		goto L3190
L3180: b$(1)(j:j)=""
L3190: if b$(1)(j:j)=" " then b$(1)(j:j)=""
	next j
	pr #20: 'Call Print.MyFontSize(16)'
	lyne=15 ! starting line of fed id
	pr #20: 'Call Print.AddText("'&b$(1)(1:1)&'",'&str$(47)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(2:2)&'",'&str$(56)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(3:3)&'",'&str$(70)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(4:4)&'",'&str$(79)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(5:5)&'",'&str$(88)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(6:6)&'",'&str$(97)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(7:7)&'",'&str$(106)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(8:8)&'",'&str$(115)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$(1)(9:9)&'",'&str$(124)&','&str$(lyne)&')'
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.AddText("'&trim$(a$(1))&'",'&str$(35)&','&str$(32)&')'
	pr #20: 'Call Print.AddText("'&trim$(a$(2))&'",'&str$(35)&','&str$(39)&')'
	csz$=a$(3): gosub CSZ
	pr #20: 'Call Print.AddText("'&trim$(city$)&'",'&str$(35)&','&str$(48)&')'
	pr #20: 'Call Print.AddText("'&trim$(state$)&'",'&str$(93)&','&str$(48)&')'
	pr #20: 'Call Print.AddText("'&trim$(zip$)&'",'&str$(108)&','&str$(48)&')'
	if qtr=1 then quarter=29 ! 1st quarter
	if qtr=2 then quarter=35 ! 2nd quarter
	if qtr=3 then quarter=41 ! 3rd quarter
	if qtr=4 then quarter=47 ! 4rd quarter
	pr #20: 'Call Print.AddText("X",'&str$(143)&','&str$(quarter)&')'
	tab1=63: tab2=113: tab3=160 ! WAS 62,112,159  ! 76,126,173 WORKED WHEN TO FAR RIGHT
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(zzzzzzzzzzzzzz)",box1)&'",'&str$(tab3)&','&str$(72)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box2)&'",'&str$(tab3)&','&str$(80)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box3)&'",'&str$(tab3)&','&str$(87)&')'
! pr #20: 'Call Print.AddText("'&CNVRT$("pic(-,---,---.##)",BOX4)&'",'&STR$(TAB3)&','&STR$(90)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5a1)&'",'&str$(tab1)&','&str$(108)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5a2)&'",'&str$(tab2)&','&str$(108)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5b1)&'",'&str$(tab1)&','&str$(116)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5b2)&'",'&str$(tab2)&','&str$(116)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5c1)&'",'&str$(tab1)&','&str$(123)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5c2)&'",'&str$(tab2)&','&str$(123)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5d)&'",'&str$(tab3)&','&str$(131)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box6)&'",'&str$(tab3)&','&str$(138)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7a)&'",'&str$(tab2)&','&str$(150)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7b)&'",'&str$(tab2)&','&str$(158)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7c)&'",'&str$(tab2)&','&str$(165)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7d)&'",'&str$(tab2)&','&str$(172)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7e)&'",'&str$(tab2)&','&str$(180)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7f)&'",'&str$(tab2)&','&str$(187)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7g)&'",'&str$(tab2)&','&str$(194)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7h)&'",'&str$(tab3)&','&str$(202)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box8)&'",'&str$(tab3)&','&str$(209)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box9)&'",'&str$(tab3)&','&str$(216)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box10)&'",'&str$(tab3)&','&str$(224)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box11)&'",'&str$(tab3)&','&str$(232)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box12)&'",'&str$(tab3)&','&str$(238)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box13)&'",'&str$(tab2)&','&str$(247)&')'
	fnpa_newpage
	pr #20: 'Call Print.addPicture("S:\acsGL\941-back.bmp",1,1)'
	pr #20: 'Call Print.AddText("'&trim$(state$(1:1))&'",'&str$(15)&','&str$(33)&')'
	pr #20: 'Call Print.AddText("'&trim$(state$(2:2))&'",'&str$(21)&','&str$(33)&')'
	tab4=75
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15a)&'",'&str$(tab4)&','&str$(61)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15b)&'",'&str$(tab4)&','&str$(68)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15c)&'",'&str$(tab4)&','&str$(74)&')'
	pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15a+box15b+box15c)&'",'&str$(tab4)&','&str$(82)&')'
return
RELEASE_PRINT: !
	close #2: ioerr L3830
L3830: close #3: ioerr L3840
L3840: !
	fnpa_finis
return
CSZ: ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
L3890: p1=pos(csz$,".",1)
	if p1>0 then csz$(p1:p1)=" ": p2=p1: goto L3890
! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
	p2=0
L3930: p1=pos(csz$,",",1)
	if p1>0 then csz$(p1:p1)=" ": p2=p1: goto L3930
! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
L3960: p1=pos(rtrm$(csz$),"  ",1)
	if p1>0 then csz$(p1+1:p1+1)="" : goto L3960
	csz$=ltrm$(rtrm$(csz$)): p1=pos(csz$," ",-1)
	x$=csz$(p1+1:len(csz$)): zip$=ltrm$(rtrm$(zip$))
	zip$=x$(1:5)
	p2=pos(csz$(1:p1-1)," ",-1) : state$=csz$(p2+1:p1-1)(1:2) : state$=ltrm$(rtrm$(state$))
	city$=csz$(1:p2-1)(1:15): city$=ltrm$(rtrm$(city$))
return
def fn_a(r)=int(r*100+.5)/100 ! /r
include: ertn