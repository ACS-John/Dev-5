00010 ! Replace S:\acsPR\newprRptS1
00020 ! pr User Designed Reports
00022   fn_setup
00130   fntop(program$,cap$="Print Designed Reports")
01220 OPEN3: ! 
01230   close #3: ioerr ignore
01240   open #3: "Name="&env$('Q')&"\PRmstr\PRReport.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\PRRptIdx.h"&str$(cno)&",Shr",internal,input,keyed ioerr NO_PRREPORT
01250   if lrec(3)=0 then goto NO_PRREPORT
01252   close #3: 
01260   goto MENU1
01270 ! ______________________________________________________________________
01280 MENU1: ! 
01290   fntos(sn$="PrintReport-ask")
01292   respc=0
01300   fnlbl(1,1,"Report:",11,1)
01310   fncombof("Report",1,14,43,env$('Q')&"\PRmstr\prreport.h"&str$(cno),1,2,3,30,env$('Q')&"\PRmstr\prrptidx.h"&str$(cno),1+addall,1,"Select from the list of reports. You can only select one report at a time.",0)
01312   resp$(respc+=1)=""
01320   fncmdkey("&Next",1,1,0,"Prints the highlighted report." )
01322   fncmdkey("&Complete",5,0,1,"Returns to menu")
01330   fnacs(sn$,0,mat resp$,ckey) ! ask report #
01332   if ckey=5 then goto XIT
01334   rn=val(resp$(1)(1:2))
01336   fn_print_designed_report(rn,3)
01338   goto OPEN3
01340 ! 
01342   def library fnprint_designed_report(rn)
01343     fn_setup
01344     fnprint_designed_report=fn_print_designed_report(rn)
01346   fnend 
01354   def fn_print_designed_report(rn)
01358     rn$=lpad$(str$(rn),2)
01360 ! 
01361     open #h_prreport:=fngethandle: "Name="&env$('Q')&"\PRmstr\PRReport.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\PRRptIdx.h"&env$('cno')&",Shr",internal,input,keyed ioerr PDR_XIT
01362     read #h_prreport,using 'form pos 3,c 78',key=rn$: rt$ nokey NORECORDSONFILE
01364     read #h_prreport,using 'form pos 3,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1',key=rn$: rt$,mat ch$,ips,tdep,cp,mat psc,mat a,mat pp,mat ti ioerr NORECORDSONFILE ! nokey NORECORDSONFILE
01366     close #h_prreport: 
01368     if sum(a)=0 then 
01370       mat ml$(2)
01372       ml$(1)="The report you have selected has no fields selected to print."
01374       ml$(2)="Click OK to contine."
01376       fnmsgbox(mat ml$,resp$,cap$,48)
01378       goto PDR_XIT
01380     end if 
01400 ! 
01410     fnCopy('S:\acsPR\newPRRpt_s1.brs',env$('Q')&"\PRmstr\Tmp_Designed_Report-"&session$&"-brs.h"&env$('cno'))
01464 ! 
01472     open #h_tmp_dr:=fngethandle: "Name="&env$('Q')&"\PRmstr\Tmp_Designed_Report-"&session$&"-brs.h"&env$('cno')&",RecL=255",display,output 
01474     pr #h_tmp_dr,using F_C255: "00081 RN$="""&rn$&""""
01520     pf$="19900 pr #255, USING F_PR_OUT: "
01530     pfd$="20010 pr #255, USING F_PR_OUT: "
01540     af$="19910 F_PR_OUT: form"
01550     gpf$="21000 pr #255, using 21010: "
01560     gaf$="21010 form skip 2,""Totals"""
01570     for j=1 to 20
01580       if a(j)=0 then goto L1770
01590       if a(j)<30 then goto L1620
01600 ! pF$=RTRM$(PF$)&TY$(A(J),1)&"," : pFD$=RTRM$(PFD$)&TY$(A(J),4)&","
01601       pf$=rtrm$(pf$)&ty$(a(j),1) : pfd$=rtrm$(pfd$)&ty$(a(j),4)&","
01610       goto L1630
01620 L1620: pf$=rtrm$(pf$)&ty$(a(j),1) : pfd$=rtrm$(pfd$)&ty$(a(j),1)
01630 L1630: ! 
01632       af$=rtrm$(af$)&",POS "&str$(pp(j))&ty$(a(j),3)
01640       if tdep=1 and a(j)>29 then goto L1660
01650       if ti(j)><1 then goto L1770
01660 L1660: ! 
01662       lf1=len(rtrm$(ty$(a(j),1)))-1 : lf2=len(rtrm$(ty$(a(j),4)))
01670       if ti(j)><1 then goto L1700
01680       if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then goto L1720
01690       pr #h_tmp_dr,using F_C255: str$(19910+j)&" "&ty$(a(j),4)&"="&ty$(a(j),4)&"+"&ty$(a(j),1)(1:lf1)
01700 L1700: ! 
01702       if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then goto L1720
01710       pr #h_tmp_dr,using F_C255: str$(19930+j)&" "&ty$(a(j),5)&"="&ty$(a(j),5)&"+"&ty$(a(j),1)(1:lf1)
01720 L1720: ! 
01722       if ti(j)><1 then goto L1770
01730       if a(j)=2 or a(j)=3 or a(j)=4 or a(j)=5 or a(j)=6 or a(j)=26 or a(j)=27 or a(j)=62 or a(j)=63 then goto L1760
01740       gaf$=rtrm$(gaf$)&",pos "&str$(pp(j))&ty$(a(j),3)
01750       gpf$=rtrm$(gpf$)&ty$(a(j),5)&","
01760 L1760: ! 
01762       ti1=1
01770 L1770: ! 
01772     next j
01780     af$=srep$(af$,'form,','form ') ! af$(11:11)=" "
01790     lf1=len(rtrm$(pf$))
01792     lf2=len(rtrm$(pfd$))
01800     pf$(lf1:lf1)=" "
01802     pfd$(lf2:lf2)=" "
01810     pf$=rtrm$(pf$)&" pageoflow pgof"
01812     pfd$=rtrm$(pfd$)&" pageoflow pgof"
01820     lf1=len(rtrm$(gpf$))
01830     gpf$(lf1:lf1)=" "
01840     gpf$=rtrm$(gpf$)&" pageoflow pgof"
01850     pr #h_tmp_dr,using F_C255: rf1$
01860 F_C255: form pos 1,c 255
01870 ! pr #h_tmp_dr,Using 2050: AF1$
01880 ! pr #h_tmp_dr,Using 2050: AF2$
01890 ! pr #h_tmp_dr,Using 2240: RF2$
01900     pr #h_tmp_dr,using F_C255: pf$
01910     pr #h_tmp_dr,using F_C255: af$
01920     pr #h_tmp_dr,using F_C255: "20099 L20099: GOTO PRTRPT"
01930     pr #h_tmp_dr,using F_C255: "20000 L20000: IF TDEP=0 THEN L20099"
01940     pr #h_tmp_dr,using F_C255: "20010 L20010: "&pfd$(7:255)
01950     pr #h_tmp_dr,using F_C255: "20020 L20020: MAT dt=(0)"
01960     pr #h_tmp_dr,using F_C255: "19990 goto L19804"
01970     if ti1 then 
01980       pr #h_tmp_dr,using F_C255: gpf$
01990       pr #h_tmp_dr,using F_C255: gaf$
02000     end if 
02002     pr #h_tmp_dr,using F_C255: "21200 GOTO EOF1"
02010     pr #h_tmp_dr,using F_C255: "20100 ! Check for Totals to pr ______________"
02020     pr #h_tmp_dr,using F_C255: "19899 L19899: if tdep=1 then goto F_PR_OUT"
02030     if ips=0 then goto L2210
02040     lf1=len(rtrm$(ty$(ips,1)))-1 : lf2=len(rtrm$(ty$(ips,4)))-1
02050 ! pr #h_tmp_dr,using F_C255: "19801 ipsw=0"
02060     if ips>24 and ips<104 then goto L2140
02070     pr #h_tmp_dr,using F_C255: "19811 For j=1 to 100"
02075     pr #h_tmp_dr,using F_C255: "19812 if psc(1)=-1 and TY$(IPS,1)(1:LF1)<>'' then L19817"
02080     pr #h_tmp_dr,using F_C255: "19814   if "&ty$(ips,1)(1:lf1)&"=psc(j) then L19817"
02090     pr #h_tmp_dr,using F_C255: "19813   if psc(j)=0 then L19804"
02100     pr #h_tmp_dr,using F_C255: "19815 next j"
02110     pr #h_tmp_dr,using F_C255: "19816 goto L19804"
02120     pr #h_tmp_dr,using F_C255: "19817 L19817: ipsw=1"
02130     goto L2210
02140 L2140: ! 
02142     pr #h_tmp_dr,using F_C255: "19832 for j=1 to 100"
02150     pr #h_tmp_dr,using F_C255: "19833   if psc(j)=0  or (psc(1)=-1 and ty$(ips,1)(1:lf1)<>'') then 19836"
02160     pr #h_tmp_dr,using F_C255: "19834   if "&ty$(ips,1)(1:lf1)&"= psc(j) then 19838"
02170     pr #h_tmp_dr,using F_C255: "19835 next j"
02180     pr #h_tmp_dr,using F_C255: "19836 if ipsw=0 then ipsw=9"
02190     pr #h_tmp_dr,using F_C255: "19837 goto L19804"
02200     pr #h_tmp_dr,using F_C255: "19838 ipsw=1"
02210 L2210: ! 
02212     pr #h_tmp_dr,using F_C255: "20001 if ips=0 or ipsw=1 then L20010 else L20020"
02220     close #h_tmp_dr: 
02230     open #h_tmp_proc:=fngethandle: "Name="&env$('temp')&"\PROC."&session$&",Replace",display,output 
02240     pr #h_tmp_proc,using F_C255: "load "&env$('Q')&"\PRmstr\Tmp_Designed_Report-"&session$&"-brs.h"&env$('cno')&",Source"
02250     pr #h_tmp_proc,using F_C255: "RUN"
02260     close #h_tmp_proc: 
02270     chain "PROC="&env$('temp')&"\PROC."&session$
02272 PDR_XIT: ! 
02274   fnend 
02280 ! ______________________________________________________________________
02290 NORECORDSONFILE: ! r: no records on file
02300 ! if rn=0 then goto XIT_PRAUTO
02310   mat ml$(2)
02312   ml$(1)="It appears this report is not in the report file!"
02314   ml$(2)="Click OK to contine."
02318   fnmsgbox(mat ml$,resp$,cap$,48)
02320   goto PDR_XIT ! /r
02330 ! ______________________________________________________________________
02340 ! XIT_PRAUTO: fnxit
02350 ! ______________________________________________________________________
02360 XIT: fnxit
02370 IGNORE: continue 
02380 ! <Updateable Region: ERTN>
02390 ERTN: fnerror(program$,err,line,act$,"xit")
02400   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02410   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02420   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02430 ERTN_EXEC_ACT: execute act$ : goto ERTN
02440 ! /region
02450 ! ______________________________________________________________________
02500 NO_PRREPORT: ! r:
02510   mat ml$(2)
02512   ml$(1)="No reports have been designed in the User Designed"
02514   ml$(2)="Reports file.  Click OK to go there now."
02516   fnmsgbox(mat ml$,resp$,cap$,48)
02520   fnchain("S:\acsPR\newprRpt3") ! /r
22000   def fn_setup
22020     library 'S:\Core\Library': fntop,fnxit, fnerror,fnchain,fncno,fntos,fnlbl,fncombof,fncmdkey,fnacs,fnmsgbox,fngethandle,fnCopy
22040     on error goto ERTN
22060 ! ______________________________________________________________________
22080     dim rn$*2,rt$*78,ch$(2)*132,psc(100),ti(20),ty$(104,5)*20,a$*255
22100     dim a$(20)*32,a(20),pp(20)
22120     dim rf1$*255,pf$*255,af$*255,gpf$*255,pfd$*255
22140     dim gaf$*255,cap$*128,ml$(3)*70
22160     dim resp$(10)*132 ! iom$(20),af1$*255,af2$*255,af1$*255,af2$*255,msgline$(2)*60,t(104),tt(104),e$(7)*30,e(17),ansm(20),wrdm$(20)*65,rf2$*255,message$*40,
22180 ! ______________________________________________________________________
22200     execute "clear proc only"
22220     fncno(cno)
22240 ! r: set mat ty$
22260     data "ENO,","Pos 1,n 8,",",pic(zzzzzzzz)","DT(2)","GT(2)"
22280     data "EM$(1),","Pos 9,C 30,",",c 30","DT(3)","GT(3)"
22300     data "EM$(2),","Pos 39,C 30,",",c 30","DT(4)","GT(4)"
22320     data "EM$(3),","Pos 69,C 30,",",c 30","DT(5)","GT(5)"
22340     data "SS$,","Pos 99,C 11,",",c 11","DT(6)","GT(6)"
22360     data "RS(1),","POS 110,n 1,",",N 1","DT(7)","GT(7)"
22380     data "RS(2),","POS 111,n 1,",",N 1","DT(8)","GT(8)"
22400     data "EM(1),","POS 112,N 2,",",pic(zz)","DT(9)","GT(9)"
22420     data "EM(2),","POS 114,N 2,",",pic(zz)","DT(10)","GT(10)"
22440     data "EM(3),","POS 116,N 2,",",pic(zz)","DT(11)","GT(11)"
22460     data "EM(4),","POS 118,N 2,",",pic(zz)","DT(12)","GT(12)"
22480     data "EM(5),","POS 120,N 2,",",pic(zz)","DT(13)","GT(13)"
22500     data "EM(6),","POS 122,N 2,",",pic(zz)","DT(14)","GT(14)"
22520     data "EM(7),","POS 124,N 2,",",pic(zz)","DT(15)","GT(15)"
22540     data "EM(8),","POS 126,PD 3.3,",",pic(---.###)","DT(16)","GT(16)"
22560     data "EM(9),","POS 129,PD 3.3,",",pic(---.###)","DT(17)","GT(17)"
22580     data "EM(10),","POS 132,PD 4.2,",",pic(--,---.##)","DT(18)","GT(18)"
22600     data "EM(11),","POS 136,PD 4.2,",",pic(--,---.##)","DT(19)","GT(19)"
22620     data "EM(12),","POS 140,PD 4.2,",",pic(--,---.##)","DT(20)","GT(20)"
22640     data "EM(13),","POS 144,PD 4.2,",",pic(--,---.##)","DT(21)","GT(21)"
22660     data "EM(14),","POS 148,PD 4.2,",",PIC(--,---.##)","DT(22)","GT(22)"
22680     data "EM(15),","POS 152,PD 4.2,",",PIC(--,---.##)","DT(23)","GT(23)"
22700     data "EM(16),","POS 156,N 6,",",pic(zz/zz/zz)","DT(24)","GT(24)"
22720     data "LPD,","POS 162,N 6,",",pic(zz/zz/zz)","DT(25)","GT(25)"
22740     data "PH$,","POS 168,C 12,",",C 12","DT(26)","GT(26)"
22760     data "BD,","POS 180,N 6,",",pic(zz/zz/zz)","dt(27)","GT(27)"
22780     data "TDN,","POS 9,N 3,",",n 3","DT(28)","GT(28)"
22800     data "TGL2,","POS 15,n 6,",",n 6","DT(29)","GT(29)"
22820     data "TDT(1),","POS 24,N 6,",",pic(zz/zz/zz)","DT(30)","GT(30)"
22840     data "TDT(2),","POS 30,N 6,",",pic(zz/zz/zz)","DT(31)","GT(31)"
22860     data "TDT(3),","POS 36,N 6,",",pic(zz/zz/zz)","DT(32)","GT(32)"
22880     data "TDT(4),","POS 42,N 6,",",pic(zz/zz/zz)","DT(33)","GT(33)"
22900     data "TCD(1),","POS 48,N 2,",",pic(zz)","DT(34)","GT(34)"
22920     data "TCD(2),","POS 50,N 2,",",pic(zz)","DT(35)","GT(35)"
22940     data "TCD(3),","POS 52,N 2,",",pic(zz)","DT(36)","GT(36)"
22960     data "TLI,","POS 54,PD 4.2,",",pic(---,---.##)","DT(37)","GT(37)"
22980     data "TDET(1),","POS 58,PD 4.2,",",pic(zz,zzz.##)","DT(38)","GT(38)"
23000     data "TDET(2),","POS 62,PD 4.2,",",pic(zz,zzz.##)","DT(39)","GT(39)"
23020     data "TDET(3),","POS 66,PD 4.2,",",pic(zz,zzz.##)","DT(40)","GT(40)"
23040     data "TDET(4),","POS 70,PD 4.2,",",pic(---,---.##)","DT(41)","GT(41)"
23060     data "TDET(5),","POS 74,PD 4.2,",",pic(---,---.##)","DT(42)","GT(42)"
23080     data "TDET(6),","POS 78,PD 4.2,",",pic(---,---.##)","DT(43)","GT(43)"
23100     data "TDET(7),","POS 82,PD 4.2,",",pic(---,---.##)","DT(44)","GT(44)"
23120     data "TDET(8),","POS 86,PD 4.2,",",pic(---,---.##)","DT(45)","GT(45)"
23140     data "TDET(9),","POS 90,PD 4.2,",",pic(---,---.##)","DT(46)","GT(46)"
23160     data "TDET(10),","POS 94,PD 4.2,",",pic(---,---.##)","DT(47)","GT(47)"
23180     data "TDET(11),","POS 98,PD 4.2,",",pic(---,---.##)","DT(48)","GT(48)"
23200     data "TDET(12),","POS 102,PD 4.2,",",pic(---,---.##)","DT(49)","GT(49)"
23220     data "TDET(13),","POS 106,PD 4.2,",",pic(---,---.##)","DT(50)","GT(50)"
23240     data "TDET(14),","POS 110,PD 4.2,",",pic(---,---.##)","DT(51)","GT(51)"
23260     data "TDET(15),","POS 114,PD 4.2,",",pic(---,---.##)","DT(52)","GT(52)"
23280     data "TDET(16),","POS 118,PD 4.2,",",pic(---,---.##)","DT(53)","GT(53)"
23300     data "TDET(17),","POS 122,PD 4.2,",",pic(---,---.##)","DT(54)","GT(54)"
23320     data "TDET(18),","POS 126,PD 4.2,",",pic(---,---.##)","DT(55)","GT(55)"
23340     data "TDET(19),","POS 130,PD 4.2,",",pic(---,---.##)","DT(56)","GT(56)"
23360     data "TDET(20),","POS 134,PD 4.2,",",pic(---,---.##)","DT(57)","GT(57)"
23380     data "TDET(21),","POS 138,PD 4.2,",",pic(---,---.##)","DT(58)","GT(58)"
23400     data "TDET(22),","POS 142,PD 4.2,",",pic(---,---.##)","DT(59)","GT(59)"
23420     data "TDET(23),","POS 146,PD 4.2,",",pic(---,---.##)","DT(60)","GT(60)"
23440     data "tdn,","POS 9,n 3,",",pic(-###)","DT(61)","GT(61)"
23460     data "prd,","POS 12,PD 6,",",pic(ZZZZ/zz/zz)","DT(62)","GT(62)"
23480     data "ckno,","POS 18,n 7,",",pic(ZZZZzzz)","DT(63)","GT(63)"
23500     data "TDc(1),","POS 25,PD 3.2,",",pic(---,---.##)","DT(64)","GT(64)"
23520     data "TDc(2),","POS 28,PD 3.2,",",pic(---,---.##)","DT(65)","GT(65)"
23540     data "TDc(3),","POS 31,PD 3.2,",",pic(---,---.##)","DT(66)","GT(66)"
23560     data "TDc(4),","POS 34,PD 3.2,",",pic(---,---.##)","DT(67)","GT(67)"
23580     data "TDc(5),","POS 37,PD 3.2,",",pic(---,---.##)","DT(68)","GT(68)"
23600     data "TDC(6),","POS 40,PD 3.2,",",pic(-,---.##)","DT(69)","GT(69)"
23620     data "TDC(7),","POS 45,PD 5.2,",",pic(----,---.##)","DT(70)","GT(70)"
23640     data "TDC(8),","POS 50,PD 5.2,",",pic(----,---.##)","DT(71)","GT(71)"
23660     data "TDC(9),","POS 55,PD 5.2,",",pic(----,---.##)","DT(72)","GT(72)"
23680     data "TDC(10),","POS 60,PD 5.2,",",pic(----,---.##)","DT(73)","GT(73)"
23700     data "Tcp(1),","POS 65,PD 5.2,",",pic(--,---,---.##)","DT(74)","GT(74)"
23720     data "Tcp(2),","POS 70,PD 5.2,",",pic(--,---,---.##)","DT(75)","GT(75)"
23740     data "Tcp(3),","POS 75,PD 5.2,",",pic(--,---,---.##)","DT(76)","GT(76)"
23760     data "Tcp(4),","POS 80,PD 5.2,",",pic(--,---,---.##)","DT(77)","GT(77)"
23780     data "Tcp(5),","POS 85,PD 5.2,",",pic(--,---,---.##)","DT(78)","GT(78)"
23800     data "Tcp(6),","POS 90,PD 5.2,",",pic(--,---,---.##)","DT(79)","GT(79)"
23820     data "Tcp(7),","POS 95,PD 5.2,",",pic(--,---,---.##)","DT(80)","GT(80)"
23840     data "Tcp(8),","POS 100,PD 5.2,",",pic(--,---,---.##)","DT(81)","GT(81)"
23860     data "tcp(9),","POS 105,PD 5.2,",",pic(--,---,---.##)","DT(82)","GT(82)"
23880     data "tcp(10),","POS 110,PD 5.2,",",pic(--,---,---.##)","DT(83)","GT(83)"
23900     data "tcp(11),","POS 115,PD 5.2,",",pic(--,---,---.##)","DT(84)","GT(84)"
23920     data "tcp(12),","POS 120,PD 5.2,",",pic(--,---,---.##)","DT(85)","GT(85)"
23940     data "tcp(13),","POS 125,PD 5.2,",",pic(--,---,---.##)","DT(86)","GT(86)"
23960     data "tcp(14),","POS 130,PD 5.2,",",pic(--,---,---.##)","DT(87)","GT(87)"
23980     data "tcp(15),","POS 135,PD 5.2,",",pic(--,---,---.##)","DT(88)","GT(88)"
24000     data "tcp(16),","POS 140,PD 5.2,",",pic(--,---,---.##)","DT(89)","GT(89)"
24020     data "tcp(17),","POS 145,PD 5.2,",",pic(--,---,---.##)","DT(90)","GT(90)"
24040     data "tcp(18),","POS 150,PD 5.2,",",pic(--,---,---.##)","DT(91)","GT(91)"
24060     data "tcp(19),","POS 155,PD 5.2,",",pic(--,---,---.##)","DT(92)","GT(92)"
24080     data "tcp(20),","POS 160,PD 5.2,",",pic(--,---,---.##)","DT(93)","GT(93)"
24100     data "tcp(21),","POS 165,PD 5.2,",",pic(--,---,---.##)","DT(94)","GT(94)"
24120     data "Tcp(22),","POS 170,PD 5.2,",",pic(--,---,---.##)","DT(95)","GT(95)"
24140     data "Tcp(23),","POS 175,PD 5.2,",",pic(--,---,---.##)","DT(96)","GT(96)"
24160     data "tcp(24),","POS 175,PD 5.2,",",pic(--,---,---.##)","DT(97)","GT(97)"
24180     data "Tcp(25),","POS 180,PD 5.2,",",pic(--,---,---.##)","DT(98)","GT(98)"
24200     data "tcp(26),","POS 185,PD 5.2,",",pic(--,---,---.##)","DT(99)","GT(99)"
24220     data "Tcp(27),","POS 190,PD 5.2,",",pic(--,---,---.##)","DT(100)","GT(100)"
24240     data "tcp(28),","POS 195,PD 5.2,",",pic(--,---,---.##)","DT(101)","GT(101)"
24260     data "Tcp(29),","POS 200,PD 5.2,",",pic(--,---,---.##)","DT(102)","GT(102)"
24280     data "tcp(30),","POS 205,PD 5.2,",",pic(--,---,---.##)","DT(103)","GT(103)"
24300     data "Tcp(31),","POS 210,PD 5.2,",",pic(--,---,---.##)","DT(104)","GT(104)"
24320     data "tcp(32),","POS 215,PD 5.2,",",pic(--,---,---.##)","DT(105)","GT(105)"
24340     read mat ty$
24360 ! /r
24380   fnend 
