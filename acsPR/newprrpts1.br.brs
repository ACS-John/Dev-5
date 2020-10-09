! Replace S:\acsPR\newprRptS1
! pr User Designed Reports
	fn_setup
	fnTop(program$,"Print Designed Reports")
OPEN3: !
	close #3: ioerr ignore
	open #3: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\PRRptIdx.h[cno],Shr",internal,input,keyed ioerr NO_PRREPORT
	if lrec(3)=0 then goto NO_PRREPORT
	close #3:
	goto MENU1
 
MENU1: !
	fnTos
	respc=0
	fnLbl(1,1,"Report:",11,1)
	fncombof("Report",1,14,43,"[Q]\PRmstr\prreport.h[cno]",1,2,3,30,"[Q]\PRmstr\prrptidx.h[cno]",1+addall,1,"Select from the list of reports. You can only select one report at a time.",0)
	resp$(respc+=1)=""
	fnCmdKey("&Next",1,1,0,"Prints the highlighted report." )
	fnCmdKey("&Complete",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey) ! ask report #
	if ckey=5 then goto Xit
	rn=val(resp$(1)(1:2))
	fn_print_designed_report(rn,3)
	goto OPEN3
 
def library fnprint_designed_report(rn)
		fn_setup
		fnprint_designed_report=fn_print_designed_report(rn)
fnend
def fn_print_designed_report(rn)
	rn$=lpad$(str$(rn),2)
 
	open #h_prreport:=fnH: "Name=[Q]\PRmstr\PRReport.h[cno],KFName=[Q]\PRmstr\PRRptIdx.h[cno],Shr",internal,input,keyed ioerr PDR_XIT
	read #h_prreport,using 'form pos 3,c 78',key=rn$: rt$ nokey NORECORDSONFILE
	read #h_prreport,using 'form pos 3,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1',key=rn$: rt$,mat ch$,ips,tdep,cp,mat psc,mat a,mat pp,mat ti ioerr NORECORDSONFILE ! nokey NORECORDSONFILE
	close #h_prreport:
	if sum(a)=0 then
		mat ml$(2)
		ml$(1)="The report you have selected has no fields selected to print."
		ml$(2)="Click OK to contine."
		fnmsgbox(mat ml$,resp$,'',48)
		goto PDR_XIT
	end if
 
	fnCopy('S:\acsPR\newPRRpt_s1.brs',"[Q]\PRmstr\Tmp_Designed_Report-"&session$&"-brs.h[cno]")
 
	open #h_tmp_dr:=fnH: "Name=[Q]\PRmstr\Tmp_Designed_Report-"&session$&"-brs.h[cno],RecL=255",display,output
	pr #h_tmp_dr,using F_C255: "00081 RN$="""&rn$&""""
	pf$="19900 pr #255, USING F_PR_OUT: "
	pfd$="20010 pr #255, USING F_PR_OUT: "
	af$="19910 F_PR_OUT: form"
	gpf$="21000 pr #255, using 21010: "
	gaf$="21010 form skip 2,""Totals"""
	for j=1 to 20
		if a(j)=0 then goto L1770
		
		if a(j)<30 then goto L1620
		
		! pF$=RTRM$(PF$)&TY$(A(J),1)&"," : pFD$=RTRM$(PFD$)&TY$(A(J),4)&","
		pf$=rtrm$(pf$)&ty$(a(j),1) : pfd$=rtrm$(pfd$)&ty$(a(j),4)&","
		goto L1630
		
		L1620: !
		pf$=rtrm$(pf$)&ty$(a(j),1) : pfd$=rtrm$(pfd$)&ty$(a(j),1)
		L1630: !
		af$=rtrm$(af$)&",POS "&str$(pp(j))&ty$(a(j),3)
		if tdep=1 and a(j)>29 then goto L1660
		
		if ti(j)><1 then goto L1770
		
		L1660: !
		lf1=len(rtrm$(ty$(a(j),1)))-1
		lf2=len(rtrm$(ty$(a(j),4)))
		if ti(j)><1 then 
			goto L1700
		end if
		if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then 
			goto L1720
		end if
		pr #h_tmp_dr,using F_C255: str$(19910+j)&" "&ty$(a(j),4)&"="&ty$(a(j),4)&"+"&ty$(a(j),1)(1:lf1)
		L1700: !
		if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then 
			goto L1720
		end if
		pr #h_tmp_dr,using F_C255: str$(19930+j)&" "&ty$(a(j),5)&"="&ty$(a(j),5)&"+"&ty$(a(j),1)(1:lf1)
		L1720: !
		if ti(j)><1 then goto L1770
		if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then 
			goto L1760
		end if
		
		gaf$=rtrm$(gaf$)&",pos "&str$(pp(j))&ty$(a(j),3)
		gpf$=rtrm$(gpf$)&ty$(a(j),5)&","
		L1760: !
		ti1=1
		L1770: !
	next j
	af$=srep$(af$,'form,','form ') ! af$(11:11)=" "
	lf1=len(rtrm$(pf$))
	lf2=len(rtrm$(pfd$))
	pf$(lf1:lf1)=" "
	pfd$(lf2:lf2)=" "
	pf$=rtrm$(pf$)&" pageoflow pgof"
	pfd$=rtrm$(pfd$)&" pageoflow pgof"
	lf1=len(rtrm$(gpf$))
	gpf$(lf1:lf1)=" "
	gpf$=rtrm$(gpf$)&" pageoflow pgof"
	pr #h_tmp_dr,using F_C255: rf1$
	F_C255: form pos 1,c 255
	! pr #h_tmp_dr,Using 2050: AF1$
	! pr #h_tmp_dr,Using 2050: AF2$
	! pr #h_tmp_dr,Using 2240: RF2$
	pr #h_tmp_dr,using F_C255: pf$
	pr #h_tmp_dr,using F_C255: af$
	pr #h_tmp_dr,using F_C255: "20099 L20099: GOTO PRTRPT"
	pr #h_tmp_dr,using F_C255: "20000 L20000: IF TDEP=0 THEN L20099"
	pr #h_tmp_dr,using F_C255: "20010 L20010: "&pfd$(7:255)
	pr #h_tmp_dr,using F_C255: "20020 L20020: MAT dt=(0)"
	pr #h_tmp_dr,using F_C255: "19990 goto L19804"
	if ti1 then
		pr #h_tmp_dr,using F_C255: gpf$
		pr #h_tmp_dr,using F_C255: gaf$
	end if
	pr #h_tmp_dr,using F_C255: "21200 GOTO EOF1"
	pr #h_tmp_dr,using F_C255: "20100 ! Check for Totals to pr ______________"
	pr #h_tmp_dr,using F_C255: "19899 L19899: if tdep=1 then goto F_PR_OUT"
	if ips=0 then goto L2210
	lf1=len(rtrm$(ty$(ips,1)))-1 : lf2=len(rtrm$(ty$(ips,4)))-1
	! pr #h_tmp_dr,using F_C255: "19801 ipsw=0"
	if ips>24 and ips<104 then goto L2140
	pr #h_tmp_dr,using F_C255: "19811 For j=1 to 100"
	pr #h_tmp_dr,using F_C255: "19812 if psc(1)=-1 and TY$(IPS,1)(1:LF1)<>'' then L19817"
	pr #h_tmp_dr,using F_C255: "19814   if "&ty$(ips,1)(1:lf1)&"=psc(j) then L19817"
	pr #h_tmp_dr,using F_C255: "19813   if psc(j)=0 then L19804"
	pr #h_tmp_dr,using F_C255: "19815 next j"
	pr #h_tmp_dr,using F_C255: "19816 goto L19804"
	pr #h_tmp_dr,using F_C255: "19817 L19817: ipsw=1"
	goto L2210
	L2140: !
		pr #h_tmp_dr,using F_C255: "19832 for j=1 to 100"
		pr #h_tmp_dr,using F_C255: "19833   if psc(j)=0  or (psc(1)=-1 and ty$(ips,1)(1:lf1)<>'') then 19836"
		pr #h_tmp_dr,using F_C255: "19834   if "&ty$(ips,1)(1:lf1)&"= psc(j) then 19838"
		pr #h_tmp_dr,using F_C255: "19835 next j"
		pr #h_tmp_dr,using F_C255: "19836 if ipsw=0 then ipsw=9"
		pr #h_tmp_dr,using F_C255: "19837 goto L19804"
		pr #h_tmp_dr,using F_C255: "19838 ipsw=1"
	L2210: !
		pr #h_tmp_dr,using F_C255: "20001 if ips=0 or ipsw=1 then L20010 else L20020"
		close #h_tmp_dr:
		open #h_tmp_proc:=fnH: "Name=[Temp]\PROC."&session$&",Replace",display,output
		pr #h_tmp_proc,using F_C255: "load [Q]\PRmstr\Tmp_Designed_Report-"&session$&"-brs.h[cno],Source"
		
		
		

		pr #h_tmp_proc,using F_C255: "RUN"
		close #h_tmp_proc:
	chain "PROC=[Temp]\PROC."&session$
	PDR_XIT: !
fnend
 
NORECORDSONFILE: ! r: no records on file
! if rn=0 then goto Xit_PRAUTO
	mat ml$(2)
	ml$(1)="It appears this report is not in the report file!"
	ml$(2)="Click OK to contine."
	fnmsgbox(mat ml$,resp$,'',48)
	goto PDR_XIT ! /r
 
! XIT_PRAUTO: fnXit
 
Xit: fnXit

include: ertn
 
NO_PRREPORT: ! r:
	mat ml$(2)
	ml$(1)="No reports have been designed in the User Designed"
	ml$(2)="Reports file.  Click OK to go there now."
	fnmsgbox(mat ml$,resp$,'',48)
fnchain("S:\Payroll\User Designed Reports") ! /r
def fn_setup
		autoLibrary
		on error goto Ertn
 
		dim rn$*2,rt$*78,ch$(2)*132,psc(100),ti(20),ty$(104,5)*20,a$*255
		dim a$(20)*32,a(20),pp(20)
		dim rf1$*255,pf$*255,af$*255,gpf$*255,pfd$*255
		dim gaf$*255,ml$(3)*70
		dim resp$(10)*132 ! iom$(20),af1$*255,af2$*255,af1$*255,af2$*255,msgline$(2)*60,t(104),tt(104),e$(7)*30,e(17),ansm(20),wrdm$(20)*65,rf2$*255,message$*40,
 
		execute "clear proc only"
		fncno(cno)
! r: set mat ty$
		data "ENO,"       ,"Pos 1,n 8,"     ,",pic(zzzzzzzz)","DT(2)","GT(2)"
		data "EM$(1),"    ,"Pos 9,C 30,"    ,",c 30","DT(3)","GT(3)"
		data "EM$(2),"    ,"Pos 39,C 30,"   ,",c 30","DT(4)","GT(4)"
		data "EM$(3),"    ,"Pos 69,C 30,"   ,",c 30","DT(5)","GT(5)"
		data "SS$,"       ,"Pos 99,C 11,"   ,",c 11","DT(6)","GT(6)"
		data "RS(1),"     ,"POS 110,n 1,"   ,",N 1","DT(7)","GT(7)"
		data "RS(2),"     ,"POS 111,n 1,"   ,",N 1","DT(8)","GT(8)"
		data "EM(1),"     ,"POS 112,N 2,"   ,",pic(zz)","DT(9)","GT(9)"
		data "EM(2),"     ,"POS 114,N 2,"   ,",pic(zz)","DT(10)","GT(10)"
		data "EM(3),"     ,"POS 116,N 2,"   ,",pic(zz)","DT(11)","GT(11)"
		data "EM(4),"     ,"POS 118,N 2,"   ,",pic(zz)","DT(12)","GT(12)"
		data "EM(5),"     ,"POS 120,N 2,"   ,",pic(zz)","DT(13)","GT(13)"
		data "EM(6),"     ,"POS 122,N 2,"   ,",pic(zz)","DT(14)","GT(14)"
		data "EM(7),"     ,"POS 124,N 2,"   ,",pic(zz)","DT(15)","GT(15)"
		data "EM(8),"     ,"POS 126,PD 3.3,",",pic(---.###)","DT(16)","GT(16)"
		data "EM(9),"     ,"POS 129,PD 3.3,",",pic(---.###)","DT(17)","GT(17)"
		data "EM(10),"    ,"POS 132,PD 4.2,",",pic(--,---.##)","DT(18)","GT(18)"
		data "EM(11),"    ,"POS 136,PD 4.2,",",pic(--,---.##)","DT(19)","GT(19)"
		data "EM(12),"    ,"POS 140,PD 4.2,",",pic(--,---.##)","DT(20)","GT(20)"
		data "EM(13),"    ,"POS 144,PD 4.2,",",pic(--,---.##)","DT(21)","GT(21)"
		data "EM(14),"    ,"POS 148,PD 4.2,",",PIC(--,---.##)","DT(22)","GT(22)"
		data "EM(15),"    ,"POS 152,PD 4.2,",",PIC(--,---.##)","DT(23)","GT(23)"
		data "EM(16),"    ,"POS 156,N 6,"   ,",pic(zz/zz/zz)","DT(24)","GT(24)"
		data "LPD,"       ,"POS 162,N 6,"   ,",pic(zz/zz/zz)","DT(25)","GT(25)"
		data "PH$,"       ,"POS 168,C 12,"  ,",C 12","DT(26)","GT(26)"
		data "BD,"        ,"POS 180,N 6,"   ,",pic(zz/zz/zz)","dt(27)","GT(27)"
		data "TDN,"       ,"POS 9,N 3,"     ,",n 3","DT(28)","GT(28)"
		data "TGL2,"      ,"POS 15,n 6,"    ,",n 6","DT(29)","GT(29)"
		data "TDT(1),"    ,"POS 24,N 6,"    ,",pic(zz/zz/zz)","DT(30)","GT(30)"
		data "TDT(2),"    ,"POS 30,N 6,"    ,",pic(zz/zz/zz)","DT(31)","GT(31)"
		data "TDT(3),"    ,"POS 36,N 6,"    ,",pic(zz/zz/zz)","DT(32)","GT(32)"
		data "TDT(4),"    ,"POS 42,N 6,"    ,",pic(zz/zz/zz)","DT(33)","GT(33)"
		data "TCD(1),"    ,"POS 48,N 2,"    ,",pic(zz)","DT(34)","GT(34)"
		data "TCD(2),"    ,"POS 50,N 2,"    ,",pic(zz)","DT(35)","GT(35)"
		data "TCD(3),"    ,"POS 52,N 2,"    ,",pic(zz)","DT(36)","GT(36)"
		data "TLI,"       ,"POS 54,PD 4.2," ,",pic(---,---.##)","DT(37)","GT(37)"
		data "TDET(1),"   ,"POS 58,PD 4.2," ,",pic(zz,zzz.##)","DT(38)","GT(38)"
		data "TDET(2),"   ,"POS 62,PD 4.2," ,",pic(zz,zzz.##)","DT(39)","GT(39)"
		data "TDET(3),"   ,"POS 66,PD 4.2," ,",pic(zz,zzz.##)","DT(40)","GT(40)"
		data "TDET(4),"   ,"POS 70,PD 4.2," ,",pic(---,---.##)","DT(41)","GT(41)"
		data "TDET(5),"   ,"POS 74,PD 4.2," ,",pic(---,---.##)","DT(42)","GT(42)"
		data "TDET(6),"   ,"POS 78,PD 4.2," ,",pic(---,---.##)","DT(43)","GT(43)"
		data "TDET(7),"   ,"POS 82,PD 4.2," ,",pic(---,---.##)","DT(44)","GT(44)"
		data "TDET(8),"   ,"POS 86,PD 4.2," ,",pic(---,---.##)","DT(45)","GT(45)"
		data "TDET(9),"   ,"POS 90,PD 4.2," ,",pic(---,---.##)","DT(46)","GT(46)"
		data "TDET(10),"  ,"POS 94,PD 4.2," ,",pic(---,---.##)","DT(47)","GT(47)"
		data "TDET(11),"  ,"POS 98,PD 4.2," ,",pic(---,---.##)","DT(48)","GT(48)"
		data "TDET(12),"  ,"POS 102,PD 4.2,",",pic(---,---.##)","DT(49)","GT(49)"
		data "TDET(13),"  ,"POS 106,PD 4.2,",",pic(---,---.##)","DT(50)","GT(50)"
		data "TDET(14),"  ,"POS 110,PD 4.2,",",pic(---,---.##)","DT(51)","GT(51)"
		data "TDET(15),"  ,"POS 114,PD 4.2,",",pic(---,---.##)","DT(52)","GT(52)"
		data "TDET(16),"  ,"POS 118,PD 4.2,",",pic(---,---.##)","DT(53)","GT(53)"
		data "TDET(17),"  ,"POS 122,PD 4.2,",",pic(---,---.##)","DT(54)","GT(54)"
		data "TDET(18),"  ,"POS 126,PD 4.2,",",pic(---,---.##)","DT(55)","GT(55)"
		data "TDET(19),"  ,"POS 130,PD 4.2,",",pic(---,---.##)","DT(56)","GT(56)"
		data "TDET(20),"  ,"POS 134,PD 4.2,",",pic(---,---.##)","DT(57)","GT(57)"
		data "TDET(21),"  ,"POS 138,PD 4.2,",",pic(---,---.##)","DT(58)","GT(58)"
		data "TDET(22),"  ,"POS 142,PD 4.2,",",pic(---,---.##)","DT(59)","GT(59)"
		data "TDET(23),"  ,"POS 146,PD 4.2,",",pic(---,---.##)","DT(60)","GT(60)"
		data "tdn,"       ,"POS 9,n 3,"     ,",pic(-###)","DT(61)","GT(61)"
		data "prd,"       ,"POS 12,PD 6,"   ,",pic(ZZZZ/zz/zz)","DT(62)","GT(62)"
		data "ckno,"      ,"POS 18,n 7,"    ,",pic(ZZZZzzz)","DT(63)","GT(63)"
		data "TDc(1),"    ,"POS 25,PD 3.2," ,",pic(---,---.##)","DT(64)","GT(64)"
		data "TDc(2),"    ,"POS 28,PD 3.2," ,",pic(---,---.##)","DT(65)","GT(65)"
		data "TDc(3),"    ,"POS 31,PD 3.2," ,",pic(---,---.##)","DT(66)","GT(66)"
		data "TDc(4),"    ,"POS 34,PD 3.2," ,",pic(---,---.##)","DT(67)","GT(67)"
		data "TDc(5),"    ,"POS 37,PD 3.2," ,",pic(---,---.##)","DT(68)","GT(68)"
		data "TDC(6),"    ,"POS 40,PD 3.2," ,",pic(-,---.##)","DT(69)","GT(69)"
		data "TDC(7),"    ,"POS 45,PD 5.2," ,",pic(----,---.##)","DT(70)","GT(70)"
		data "TDC(8),"    ,"POS 50,PD 5.2," ,",pic(----,---.##)","DT(71)","GT(71)"
		data "TDC(9),"    ,"POS 55,PD 5.2," ,",pic(----,---.##)","DT(72)","GT(72)"
		data "TDC(10),"   ,"POS 60,PD 5.2," ,",pic(----,---.##)","DT(73)","GT(73)"
		data "Tcp(1),"    ,"POS 65,PD 5.2," ,",pic(--,---,---.##)","DT(74)","GT(74)"
		data "Tcp(2),"    ,"POS 70,PD 5.2," ,",pic(--,---,---.##)","DT(75)","GT(75)"
		data "Tcp(3),"    ,"POS 75,PD 5.2," ,",pic(--,---,---.##)","DT(76)","GT(76)"
		data "Tcp(4),"    ,"POS 80,PD 5.2," ,",pic(--,---,---.##)","DT(77)","GT(77)"
		data "Tcp(5),"    ,"POS 85,PD 5.2," ,",pic(--,---,---.##)","DT(78)","GT(78)"
		data "Tcp(6),"    ,"POS 90,PD 5.2," ,",pic(--,---,---.##)","DT(79)","GT(79)"
		data "Tcp(7),"    ,"POS 95,PD 5.2," ,",pic(--,---,---.##)","DT(80)","GT(80)"
		data "Tcp(8),"    ,"POS 100,PD 5.2,",",pic(--,---,---.##)","DT(81)","GT(81)"
		data "tcp(9),"    ,"POS 105,PD 5.2,",",pic(--,---,---.##)","DT(82)","GT(82)"
		data "tcp(10),"   ,"POS 110,PD 5.2,",",pic(--,---,---.##)","DT(83)","GT(83)"
		data "tcp(11),"   ,"POS 115,PD 5.2,",",pic(--,---,---.##)","DT(84)","GT(84)"
		data "tcp(12),"   ,"POS 120,PD 5.2,",",pic(--,---,---.##)","DT(85)","GT(85)"
		data "tcp(13),"   ,"POS 125,PD 5.2,",",pic(--,---,---.##)","DT(86)","GT(86)"
		data "tcp(14),"   ,"POS 130,PD 5.2,",",pic(--,---,---.##)","DT(87)","GT(87)"
		data "tcp(15),"   ,"POS 135,PD 5.2,",",pic(--,---,---.##)","DT(88)","GT(88)"
		data "tcp(16),"   ,"POS 140,PD 5.2,",",pic(--,---,---.##)","DT(89)","GT(89)"
		data "tcp(17),"   ,"POS 145,PD 5.2,",",pic(--,---,---.##)","DT(90)","GT(90)"
		data "tcp(18),"   ,"POS 150,PD 5.2,",",pic(--,---,---.##)","DT(91)","GT(91)"
		data "tcp(19),"   ,"POS 155,PD 5.2,",",pic(--,---,---.##)","DT(92)","GT(92)"
		data "tcp(20),"   ,"POS 160,PD 5.2,",",pic(--,---,---.##)","DT(93)","GT(93)"
		data "tcp(21),"   ,"POS 165,PD 5.2,",",pic(--,---,---.##)","DT(94)","GT(94)"
		data "Tcp(22),"   ,"POS 170,PD 5.2,",",pic(--,---,---.##)","DT(95)","GT(95)"
		data "Tcp(23),"   ,"POS 175,PD 5.2,",",pic(--,---,---.##)","DT(96)","GT(96)"
		data "tcp(24),"   ,"POS 175,PD 5.2,",",pic(--,---,---.##)","DT(97)","GT(97)"
		data "Tcp(25),"   ,"POS 180,PD 5.2,",",pic(--,---,---.##)","DT(98)","GT(98)"
		data "tcp(26),"   ,"POS 185,PD 5.2,",",pic(--,---,---.##)","DT(99)","GT(99)"
		data "Tcp(27),"   ,"POS 190,PD 5.2,",",pic(--,---,---.##)","DT(100)","GT(100)"
		data "tcp(28),"   ,"POS 195,PD 5.2,",",pic(--,---,---.##)","DT(101)","GT(101)"
		data "Tcp(29),"   ,"POS 200,PD 5.2,",",pic(--,---,---.##)","DT(102)","GT(102)"
		data "tcp(30),"   ,"POS 205,PD 5.2,",",pic(--,---,---.##)","DT(103)","GT(103)"
		data "Tcp(31),"   ,"POS 210,PD 5.2,",",pic(--,---,---.##)","DT(104)","GT(104)"
		data "tcp(32),"   ,"POS 215,PD 5.2,",",pic(--,---,---.##)","DT(105)","GT(105)"
		read mat ty$
! /r
fnend
