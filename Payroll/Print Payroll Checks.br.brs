06000 ! formerly S:\acsPR\newprCkPrt
06020 ! pr Payroll Checks ! Nebs 9039t: Standard Check Format (Laser Stub-Check-Stub)
06040 ! ______________________________________________________________________
06060   library 'S:\Core\Library': fntop,fnxit, fnerror,fnGetPayrollDates, fnopenprn,fncloseprn,fnchain,fntos,fnlbl,fncomboa,fntxt,fncombof,fncmdset,fnacs,fnmsgbox,fndate_mmddyy_to_ccyymmdd,fnopt,fnqgl,fnrgl$,fncmdkey,fnagl$,fnbutton,fnss_employee,fnss_employer,fncd,fnclient_has,fnreg_read,fnreg_write,fngethandle,fncreg_read,fncreg_write,fnDedNames
06080   on error goto ERTN
06100   fntop(program$)
06160   if env$('client')="Divernon" then let print_netzero_checks=1
06180   if env$('client')="West Accounting" then let print_netzero_checks=1
08100 ! r: dims and constants
08120   dim em$(3)*30 ! payee name and address
08122   dim tdet(17)
08124   dim tdc(10)
08126   dim tcp(32)
08128   dim tdep(20,26)
08130   dim ttc(32)
08142   dim d$(10)*8
08148   dim gln$(15)*12
08160   dim tty(32)
08162   dim mgl$(11)
08164   dim ded$(29)
08166   dim de$*30 ! description (from CL (for transaction allocations, i think)
08168   dim lcn$*8
08170   dim tr(2)
08172   dim tr$(5)*35
08200   dim resp$(25)*128,d1$*20,ttc(32),ttdc(10)
08220   dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),ytdtotal(32)
08240   dim quartertotals(32)
08242   dim dedfed(20)
08244   dim calcode(20)
08246   dim dedcode(20)
08260   dim fullname$(20)*20
08262   dim abrevname$(20)*8
08264   dim ml$(2)*128 ! temp variable for messagebox message lines
08280   dim dedfica(20),dedst(20),deduc(20),gl$(20)*12
08300   dim dtr$(5)*35,key$*21
08320   dim dept(6),bankgl$*12,gl$*12,bn$*30,text$*90,v(7,8),deptsum(6)
08340   dim hnames$(20)*8
08360   dim eng$*128,wording$(27)*9,amount(11)
08380 ! 
08400   dim opt_check_format$(6)*20, scc$(6)
08410   let opt_check_format$(1)="Check, Stub"        : let scc$(1)="CS"
08420   let opt_check_format$(2)="Stub, Check"        : let scc$(2)="SC"
08430   let opt_check_format$(3)="Stub, Check, Stub"  : let scc$(3)="SCS"
08440   let opt_check_format$(4)="Check, Stub, Stub"  : let scc$(4)="CSS"
08450   let opt_check_format$(5)="Stub, Stub, Check"  : let scc$(5)="SSC"
08460   let opt_check_format$(6)="Stub, Check, Check" : let scc$(6)="SCC"
08500 ! 
08520   dim opt_check_type$(3)*14
08540   let opt_check_type$(1)="Regular Check"
08560   let opt_check_type$(2)="Direct Deposit"
08580   let opt_check_type$(3)="All"
08600 ! r: set mat wording$
08620   let wc=0 ! Wording$ counter
08640   let wording$(wc+=1)='One'
08660   let wording$(wc+=1)='Two'
08680   let wording$(wc+=1)='Three'
08700   let wording$(wc+=1)='Four'
08720   let wording$(wc+=1)='Five'
08740   let wording$(wc+=1)='Six'
08760   let wording$(wc+=1)='Seven'
08780   let wording$(wc+=1)='Eight'
08800   let wording$(wc+=1)='Nine'
08820   let wording$(wc+=1)='Ten'
08840   let wording$(wc+=1)='Eleven'
08860   let wording$(wc+=1)='Twelve'
08880   let wording$(wc+=1)='Thirteen'
08900   let wording$(wc+=1)='Fourteen'
08920   let wording$(wc+=1)='Fifteen'
08940   let wording$(wc+=1)='Sixteen'
08960   let wording$(wc+=1)='Seventeen'
08980   let wording$(wc+=1)='Eighteen'
09000   let wording$(wc+=1)='Nineteen'
09020   let wording$(wc+=1)='Twenty'
09040   let wording$(wc+=1)='Thirty'
09060   let wording$(wc+=1)='Forty'
09080   let wording$(wc+=1)='Fifty'
09100   let wording$(wc+=1)='Sixty'
09120   let wording$(wc+=1)='Seventy'
09140   let wording$(wc+=1)='Eighty'
09160   let wording$(wc+=1)='Ninety'
09180 ! /r
09181   dim opt_yn$(2)*4
09182   let opt_yn$(1)="Yes"
09184   let opt_yn$(2)="No"
09185 ! 
09200 ! /r
10240 ! r: set default answers and semi-consants and open some files
10260   let ssr1=fnss_employee*.01
10270   let ssr2=fnss_employer*.01
10280   open #20: "Name="&env$('Q')&"\PRmstr\prCode.h"&env$('cno')&",Shr",internal,input 
10290   read #20,using 'Form POS 2,POS 5,N 5': ckno
10300   close #20: 
10310   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
10342   fncreg_read('Prenumbered Checks',pre$)
10344   fncreg_read('Post to CL',acsclcv$)
10346   fncreg_read('Post Employer Portion of FiCA',ficam1$)
10348   fncreg_read('Check Format',sc1$)
10350   fncreg_read('Print Vacation and Sick Leave on Check',accr$)
10352   fncreg_read('CL Bank Code',bankcode$) : bankcode=val(bankcode$) : if bankcode=0 then bankcode=1
10354   fncreg_read('Comp Time Code',compcode$)
10420   fnDedNames(mat fullname$,mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
10450   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
10460   read #20,using 'Form POS 1,x 120,POS 150,10*C 8,POS 437,15*C 12,N 1': mat d$,mat gln$,gl_installed
10470   close #20: 
10480 ! ___________________________
10490   mat hnames$=abrevname$ : bankgl$=gln$(15)
10650   if fnclient_has('CL') then let fn_open_acscl
10660 ! ___________________________
10680   if bankcode=0 then bankcode=1
10690   check_number=ckno
10710 ! if env$('client')="West Rest Haven" then let sc1$="C"
10720   if env$('client')="Billings" then let sc1$="CSS"
10730   if env$('client')="Divernon" or env$('client')="Thomasboro" or env$('client')="Edinburg" or env$('client')="Philo" or env$('client')="Hope Welty" or env$('client')="Monticello" then let ficam1$="Y"
10740   let ddcode$="R"
10742   fnreg_read('PR.Check print.skip alignment',skip_alignment$) : if skip_alignment$='' then let skip_alignment$='No'
10745   goto MAIN_QUESTIONS ! /r
10750 MAIN_QUESTIONS: ! r:
10760   fntos(sn$="prckprt")
10770   let respc=0
10780   fnlbl(1,1,"Payroll Date:",38,1)
10790   fntxt(1,41,10,0,1,"3",0,"")
10800   let resp$(resp_payroll_date:=1)=str$(d1)
10810   fnlbl(2,1,"Are Checks Prenumbered?",38,1)
10840   fncomboa("prckprt-2",2,41,mat opt_yn$,"The system needs to know if the checks are already numbered.",3)
10850   if pre$="Y" then let resp$(2)=opt_yn$(1) else let resp$(2)=opt_yn$(2)
10860   fnlbl(3,1,"Beginning Check Number:",38,1)
10870   fntxt(3,41,7,0,1,"30",0,"")
10880   let resp$(3)=str$(check_number)
10890   fnlbl(4,1,"Date of Checks:",38,1)
10900   fntxt(4,41,10,0,1,"3",0,"")
10910   let resp$(resp_date_of_checks:=4)=date$("ccYYMMDD")
10920   fnlbl(5,1,"Beginning Employee Number:",38,1)
10930   fntxt(5,41,8,0,1,"30",0,"")
10940   let resp$(5)=str$(empno)
10950   fnlbl(6,1,"Post to ACS Checkbook",38,1)
10952   if fnclient_has('CL') then 
10954     fncomboa("prckprt-3",6,41,mat opt_yn$)
10956     if acsclcv$="Y" then let resp$(6)=opt_yn$(1) else let resp$(6)=opt_yn$(2)
10958   else 
10960     fntxt(6,41,3, 0,0,'',1,'ACS Checkbook license not detected.')
10962     let resp$(6)=opt_yn$(2) : acsclcv$='N'
10964   end if 
10980   fnlbl(7,1,"Post Employer's Portion of FiCA?",38,1)
10990   fncomboa("prckprt-4",7,41,mat opt_yn$,"The system can generate and post the employer's portion of FICA at the time the check is being written.",3)
11000   if ficam1$="Y" then let resp$(7)=opt_yn$(1) else let resp$(7)=opt_yn$(2)
11010   fnlbl(8,1,"Check Format:",38,1)
11060   fncomboa("ckprt-2",8,41,mat opt_check_format$)
11070   whichScc=srch(mat scc$,sc1$)
11080   if whichScc>0 then let resp$(8)=opt_check_format$(whichScc) else let resp$(8)=opt_check_format$(4)
11140   fnlbl(9,1,"Check Type (Regular or Direct Deposit):",38,1)
11150   fncomboa("ckprt-5",9,41,mat opt_check_type$,"If you have direct deposits, you can use this option to pr check on plain paper to give the employees.",15)
11160   if ddcode$="R" then let resp$(9)=opt_check_type$(1)
11170   if ddcode$="D" then let resp$(9)=opt_check_type$(2)
11180   if ddcode$="A" then let resp$(9)=opt_check_type$(3)
11190   fnlbl(10,1,"Print Vacation and Sick Leave?",38,1)
11200   fncomboa("prckprt-6",10,41,mat opt_yn$)
11210   if accr$="Y" then let resp$(10)=opt_yn$(1) else let resp$(10)=opt_yn$(2)
11212 ! 
11213   let respc=10
11214 ! 
11220   if cl_installed=0 and exists(env$('Q')&"\CLmstr\bankmstr.h"&env$('cno')) then 
11230     fnlbl(11,1,"Bank Account:",38,1)
11240     fncombof("Bankmstr",11,41,20,env$('Q')&"\CLmstr\bankmstr.h"&env$('cno'),1,2,3,15,env$('Q')&"\CLmstr\Bankidx1.h"&env$('cno'),1,0, "Select bank account for printing")
11250     let resp$(resp_cl_bankcode:=respc+=1)=str$(bankcode)
11252   end if 
11260   if exists(env$('Q')&"\PRmstr\hourclass.h"&env$('cno')) then 
11280     fnlbl(12,1,"Comp Time Code:",38,1)
11290     fncombof("timeclass",12,41,20,env$('Q')&"\PRmstr\hourclass.h"&env$('cno'),1,5,6,25,env$('Q')&"\PRmstr\hourclass-idx.h"&env$('cno'),1,0, "Select time classification code for comp time, if applicable.")
11300     let resp$(resp_combcode:=respc+=1)=compcode$
11310   end if 
11314   fnlbl(14,1,"Print All Checks (or ask after first):",38,1)
11315   fncomboa("prckprt-prall",14,41,mat opt_yn$)
11316   let resp$(resp_skip_align=respc+=1)=skip_alignment$
11318   if gl_installed then 
11320     fnlbl(16,1,"General Ledger detected.",38,1)
11322   end if 
11323   if cl_installed then 
11324     fnlbl(17,1,"Checkbook detected.",38,1)
11325   end if 
11327   fncmdset(2) ! need button to show totals
11328   fnacs(sn$,0,mat resp$,ck)
11330   if ck=5 then goto XIT ! /r
11332 ! r: validate answers (and move to local variables from mat resp$)
11340   let d1=val(resp$(resp_payroll_date)) ! payroll date
11350   let pre$=uprc$(resp$(2)(1:1)) ! pre-numbered checks Y or N
11360   check_number=val(resp$(3)) ! check #
11370   ckdat$=resp$(resp_date_of_checks) ! check date
11380   let dat=val(ckdat$(5:6)&ckdat$(7:8)&ckdat$(3:4))
11390   let empno=val(resp$(5)) ! beginning employee #
11400   acsclcv$=uprc$(resp$(6)(1:1)) ! post Checkbook system
11410   let ficam1$=uprc$(resp$(7)(1:1)) ! post fica match
11430   let sc1$=scc$(srch(mat opt_check_format$,resp$(8)))
11460   let ddcode$=uprc$(resp$(9)(1:1)) ! regular check or direct deposit
11470   accr$=uprc$(resp$(10)(1:1)) ! pr vac and sick
11472   if resp_cl_bankcode then 
11480     bankcode=val(resp$(resp_cl_bankcode)(1:3)) ! bank code
11481   end if 
11482   if resp_combcode then 
11490     compcode$=resp$(resp_combcode)(1:5) ! comp time code
11492   end if 
11496 ! date_of_checks=val(ckdat$)
11500   let prdmmddyy=val(ckdat$(5:6))*10000+val(ckdat$(7:8))*100+val(ckdat$(3:4)) ! convert date back to mmddyy format
11506   let skip_alignment$=resp$(resp_skip_align)
11507   if skip_alignment$='Yes' then allign=3
11508 ! 
11510   if acsclcv$="Y" then cl_installed=1 else cl_installed=0
11520   if ficam1$="Y" then let ficam1=1 else let ficam1=0
11530   if pre$="Y" then let pre=1 else let pre=0
11532 ! if env$('client')="Washington Parrish" and (prdate<10100 or prdate>123199) then goto MAIN_QUESTIONS
11540   if d1<beg_date or d1>end_date then ! not this year
11545     mat ml$(4)
11550     let ml$(1)='The Payroll Date you have choosen ('&cnvrt$('pic(zzzz/zz/zz)',d1)&') is is outside your years'
11555     let ml$(2)='beginning  and ending date range ('&cnvrt$('pic(zzzz/zz/zz)',beg_date)&' - '&cnvrt$('pic(zzzz/zz/zz)',end_date)&').'
11560     let ml$(3)='Checks with a payroll date outside this date range can not be processed.'
11565     let ml$(4)='Would you like to "Change Payroll Dates" now?'
11570     fnmsgbox(mat ml$,resp$,'',16+4)
11575     if resp$='Yes' then 
11580       fnchain('S:\Payroll\Change Payroll Dates')
11585     else ! if resp$='No' then
11590       goto MAIN_QUESTIONS
11595     end if 
11600   end if 
11605 ! pause
11610 ! 
11615   if check_number<0 then 
11620     mat ml$(2)
11625     let ml$(1)="You must enter a valid check number!"
11630     let ml$(2)="Click OK to return to previous screen. "
11635     fnmsgbox(mat ml$,resp$)
11640     goto MAIN_QUESTIONS
11645   end if 
11646 ! /r
11648 ! r: save answers for next time
11650   fncreg_write('Prenumbered Checks',pre$)
11652   fncreg_write('Post to CL',acsclcv$)
11654   fncreg_write('Post Employer Portion of FiCA',ficam1$)
11656   fncreg_write('Check Format',sc1$)
11658   fncreg_write('Print Vacation and Sick Leave on Check',accr$)
11660   fncreg_write('CL Bank Code',str$(bankcode))
11662   fncreg_write('Comp Time Code',compcode$)
11664   fnreg_write('PR.Check print.skip alignment',skip_alignment$)
11670 ! /r
11675 ! r: get and validate bank code if ACSCL is in play
11680   if cl_installed=1 then 
11685     read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi nokey L1280 ioerr L1290
11690   end if 
11695   goto L1300
11700 L1280: ! 
11705   mat ml$(2)
11710   let ml$(1)="You must enter a valid bank code!"
11715   let ml$(2)="Click OK to return to previous screen. "
11720   fnmsgbox(mat ml$,resp$)
11725   goto MAIN_QUESTIONS
11730 L1290: ! 
11735   mat ml$(3)
11740   let ml$(1)="You have indicated that you want to post checkbook, "
11745   let ml$(2)="but no checkbook files can be found! "
11750   let ml$(3)="Click OK to return to previous screen. "
11755   fnmsgbox(mat ml$,resp$)
11760   goto MAIN_QUESTIONS
11765 L1300: ! 
11770 ! /r
11810 ! if env$('client')<>"Washington Parrish" then let prdate=d1
11820   if cl_installed then 
11830     open #7: "Name="&env$('Q')&"\PRmstr\MGLMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\MGLIDX1.h"&env$('cno')&",Shr",internal,input,keyed 
11840   end if 
11850   open #praddr:=1: "Name="&env$('Q')&"\PRmstr\prAddr1.h"&env$('cno')&",Shr",internal,input 
11860   open #rpmstr:=2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpindex.h"&env$('cno')&",Shr",internal,input,keyed 
11870   open #6: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno'),internal,outin,keyed 
11880   open #3: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
11890   open #breakdown=31: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&env$('cno'),internal,outin,keyed 
11900   open #dd=30: "Name="&env$('Q')&"\PRmstr\DD.h"&env$('cno')&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&env$('cno')&",Shr,kps=1,kln=10,Use",internal,outin,keyed 
11910   if fnclient_has('GL') and gl_installed=1 then 
11920     let gl_installed=0
11930     open #h_gl_glbrec:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLRECIDX.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L1440
11940     let gl_installed=1
11950 L1440: ! 
11960   end if 
11980 MAIN_LOOP_TOP: ! 
11990   let s1=1
12000 L1480: ! 
12010   read #rpmstr,using L1570: eno,mat em$,ss$,em10,em11,lpd eof EO_RPMSTR
12020   mat v=(0): let v1=1
12030 ! If env$('client')="WashingtonParrish" Then Goto 1110
12040   let dirdep$=rpad$(str$(eno),10)
12050   let dd$="": read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17",key=dirdep$: key$,dd$,rtn,acc,acn nokey L1530
12060 L1530: ! 
12070   if uprc$(dd$)="Y" and ddcode$="D" then goto L1570
12080   if uprc$(dd$)<>"Y" and ddcode$="R" then goto L1570
12090   if ddcode$="A" then goto L1570 ! all
12100   goto L1480
12110 L1570: form pos 1,n 8,3*c 30,c 11,pos 132,2*pd 4.2,pos 162,n 6
12120   if empno>eno then goto L1480 ! start with certain employee
12130   let tdepXcount=0
12140   mat tdep=(0)
12150   let tdc1=0 : let tdc2=0
12160   let tdc3=tdc4=tdc5=0
12170   let tpd3=tpd4=tpd5=0
12180   let tdct=0
12190   let rate=0
12200   let s1=1
12210 ! If fndate_mmddyy_to_ccyymmdd(LPD)><D1 Then Goto 1360  ! with comment can reprint any payroll
12220   fn_determine_earnings
12230   if print_netzero_checks and ttc(32)=0 and fndate_mmddyy_to_ccyymmdd(lpd)=d1 then goto L1670 ! pr zero checks
12240   if ttc(32)=0 then goto L1480 ! no earnings
12250 L1670: ! 
12260 ! Mat TCP=(0)
12270 ! Mat TDC=(0)
12280   goto PRE_CHECK
12290 ! ______________________________________________________________________
12300 PRE_CHECK: ! 
12310   let ttc(26)=ttc(26)-tpd3-tpd4-tpd5
12320   let ttc(28)=ttc(28)+ttc(29)+ttc(30) ! OTHER COMP-CURRENT
12330   let ttc(1)=ttc(1)-ttc(25) : let tty(1)=tty(1)-tty(25)
12340 L2070: ! 
12350   if cl_installed=1 then let fn_cknum
12360   fnopenprn
12380   if env$('client')="Eldorado" then let fn_eldorado_check_and_stub : goto L2150
12400   if sc1$="SCS" then let fn_print_stub : let fn_print_check : let fn_print_stub
12410   if sc1$="CSS" then let fn_print_check : let fn_print_stub : let fn_print_stub
12420   if sc1$="SSC" then let fn_print_stub : let fn_print_stub : let fn_print_check
12430   if sc1$="SCC" then let fn_print_stub : let fn_print_check : let fn_print_check
12432   if sc1$="CS" then let fn_print_check : let fn_print_stub
12434   if sc1$="SC" then let fn_print_stub : let fn_print_check
12440 L2150: ! 
12450   if fp(d1*.01)>.9 then 
12460     let hd1=19000000+fncd(d1)
12470   else 
12480     let hd1=20000000+fncd(d1)
12490   end if 
12500 ! let hsk$=lpad$(str$(eno),8)&cnvrt$("PD 6",hd1)
12520   if allign=3 or skip_alignment$='Yes' then 
12530     pr #255: chr$(12) ! NEWPAGE
12540     goto CHECK_PRINT_TOP
12550   end if 
12560   fncloseprn
12570   goto ALLIGNMENT
12580 ! ______________________________________________________________________
12590 ALLIGNMENT: ! r:
12600   fntos(sn$="prckprt2")
12610   let respc=0 : let rc=0
12620   fnopt(1,3,"Reprint same check",0,franum)
12630   let resp$(rc+=1)="False"
12640   fnopt(2,3,"Print next",0,franum)
12650   let resp$(rc+=1)="False"
12660   fnopt(3,3,"Print all remaining",0,franum)
12670   let resp$(rc+=1)="True"
12672   if env$('client')='Billings' then let resp$(2)='True' : let resp$(3)='False'
12680   fncmdset(2): let fnacs(sn$,0,mat resp$,ck) ! allignment
12690   if resp$(1)="True" then allign=1
12700   if resp$(2)="True" then allign=2
12710   if resp$(3)="True" then allign=3
12720   if ck=5 then let getout=1: allign=2: goto CHECK_PRINT_TOP ! write history on last check and quit
12730   on allign goto REPRINT_SAME_CHECK,CHECK_PRINT_TOP,CHECK_PRINT_TOP none ALLIGNMENT
12740 ! /r
12750 REPRINT_SAME_CHECK: ! 
12760   if pre=0 then goto L3130
12770   if cl_installed=1 then let fn_build_check_record
12780   check_number=check_number+1
12790 L3130: ! 
12800   goto L2070
12810 ! ______________________________________________________________________
12820 CHECK_PRINT_TOP: ! 
12830   if cl_installed=1 then let fn_build_check_record
12850   let tdc1=0
12860   let tdc2=0
12870   let tdc3=tdc4=tdc5=0
12880   let tpd3=tpd4=tpd5=0
12890   let tdct=0
12900   let rate=0
12910   let ttc(32)
12920   mat dept=(0)
12950   if gl_installed=1 then ! r: update GL's GLBREC
12952     read #h_gl_glbrec,using 'form pos 1,c 12',key=bankgl$&lpad$(rtrm$(str$(check_number)),12): gl$ nokey L3300
12954   else 
12956     goto L3320
12958   end if 
12960   rewrite #h_gl_glbrec,using F_GL_GLBREC: bankgl$,lpad$(rtrm$(str$(check_number)),12),em$(1),"PR",dat,ttc22,0
12970   goto L3320
12980 ! 
12990 L3300: ! 
13000   write #h_gl_glbrec,using F_GL_GLBREC: bankgl$,lpad$(rtrm$(str$(check_number)),12),em$(1),"PR",dat,ttc22,0
13010 F_GL_GLBREC: form pos 1,2*c 12,c 30,c 2,n 6,pd 5.2,n 1
13020 L3320: ! /r
13030   if getout=1 then goto FINIS
13040   let ttc(32)=0
13050   check_number=check_number+1
13060   goto MAIN_LOOP_TOP
13070 ENG_DOL_IDK: ! r: ENTER AMOUNT IN DOL, EXIT IN ENG$
13072   if env$('client')="Merriam Woods" then 
13074     let n=65
13076   else 
13078     let n=58
13080   end if 
13082   let dol=ttc(32)
13084   if dol<=0 then 
13086     let eng$="*** VOID ***"
13088     goto L3810
13090   else if dol=>10**8 then 
13092     let eng$="Value too big for editing"
13094     goto L3810
13096   end if 
13150   let eng$="***"
13160   amount(1)=int(dol*100+.500000001)
13170   for a0=2 to 10
13180     amount(a0)=int(amount(a0-1)/10+.000000001)
13190   next a0
13200   for a0=1 to 10
13210     amount(a0)=amount(a0)-amount(a0+1)*10
13220   next a0
13230   if amount(11)+amount(10)+amount(9)=0 then goto L3530
13240   a0=9
13250   fn_l3830
13260   let eng$=rtrm$(eng$)&" Million"
13270   L3530: ! 
13280   if amount(8)+amount(7)+amount(6)=0 then goto L3570
13290   a0=6
13300   fn_l3830
13310   let eng$=rtrm$(eng$)&" Thousand"
13320   L3570: if amount(5)+amount(4)+amount(3)=0 then goto L3600
13330   a0=3
13340   fn_l3830
13350   L3600: ! 
13360   if dol>=1 then goto L3620
13370   let eng$=rtrm$(eng$)&" Zero"
13380   L3620: ! 
13390   let eng$=rtrm$(eng$)&" Dollar"
13400   if dol<2 and dol>=1 then goto L3660
13410   let eng$=rtrm$(eng$)&"s"
13420   if len(rtrm$(eng$))>58 then goto L3660
13430   L3660: ! 
13440   let eng$=rtrm$(eng$)&" and"
13450   if amount(2)+amount(1)=0 then goto L3720
13460   amount(3)=0
13470   a0=1
13480   fn_l3830
13490   goto L3730
13500   L3720: ! 
13510   let eng$=rtrm$(eng$)&" Zero"
13520   L3730: ! 
13530   let eng$=rtrm$(eng$)&" Cent"
13540   if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3760
13550   let eng$=rtrm$(eng$)&"s"
13560   L3760: ! 
13570   if len(rtrm$(eng$))<58 then goto L3810
13580   for j=1 to 9
13590     let n=59-j
13600     if eng$(n:n)=" " then goto L3810
13610   next j
13620   L3810: ! 
13630 return  ! /r
13650 def fn_l3830
13660   if amount(a0+2)=0 then goto L3860
13670   let eng$=rtrm$(eng$)&" "&wording$(amount(a0+2))
13680   let eng$=rtrm$(eng$)&" Hundred"
13690   L3860: ! 
13700   if amount(a0+1)=0 and amount(a0)=0 then goto L3920
13710   if amount(a0+1)<2 then goto L3910
13720   let eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)+18)
13730   if amount(a0)=0 then goto L3920
13740   amount(a0+1)=0
13750   L3910: ! 
13760   let eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)*10+amount(a0))
13770   L3920: ! 
13780 fnend 
13790 EO_RPMSTR: ! r:
13792   close #rpmstr: 
13800   close #3: 
13810   if gl_installed=1 then close #h_gl_glbrec: 
13820   fncloseprn
13830   goto FINIS ! /r
13840 FINIS: ! 
13850   if cl_installed=1 and allign=4 then let fn_build_check_record
13870 XIT: let fnxit
13890 def fn_open_acscl
13900   open #h_cl_bank:=12: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L4220
13910   cl_installed=1
13920   open #15: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
13930   open #h_cl_payee:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
13940   open #14: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
13950   open #h_cl_trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
13960   open #22: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
13970   !   if exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then
13980   open #h_cl_trans_alloc:=23: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",Version=2,KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&env$('cno')&",Shr",internal,outin,keyed 
13990   !   else 
14000   !     open #h_cl_trans_alloc:=23: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",Shr",internal,outin,relative
14010   !   end if
14020   open #h_cl_glmstr:=fngethandle: "Name="&env$('Q')&"\CLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\GLINDEX.h"&env$('cno')&",Shr",internal,outin,keyed 
14050   read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,ckno nokey L4200
14060   L4200: ckno=ckno+1
14070   ! Let DAT=VAL(DATE$(4:5)&DATE$(7:8)&DATE$(1:2))
14080   L4220: ! 
14090 fnend 
14100 def fn_build_check_record
14110   let tr$(1)=cnvrt$("N 8",check_number)
14120   let tr$(2)=cnvrt$("N 6",dat)
14130   let tr$(3)=cnvrt$("N 10.2",ttc(32))
14140   let tr$(4)=cnvrt$("N 8",eno)
14150   let tr$(5)=em$(1)
14160   if allign>1 then goto L4350
14170   let tr$(3)=tr$(4)=""
14180   let tr$(5)="VOID"
14190   mat ded$=("")
14200   goto WRITE_PAYROLL_TRANS
14210   L4350: let ded$(1)=str$(ttc(31))
14220   for j=2 to 25
14230     if j=3 then let ded$(j)=str$(ttc(2)) : goto L4390
14240     ! ADD MEDICARE TO FICA  no kj
14250     let ded$(j)=str$(ttc(j-1))
14260   L4390: next j
14270   ! Let DED$(25)=STR$(TTC(29)) ! meals
14280   ! Let DED$(26)=STR$(TTC(30)) ! tips
14290   ! Let DED$(27)=STR$(TDC1) ! reg hourly rate
14300   ! Let DED$(28)=STR$(TDC2) ! ot rate
14310   if tdc1=0 then goto L4480
14320   ! Let DED$(29)=STR$(INT(TDC1/40+.99))
14330   goto WRITE_PAYROLL_TRANS
14340   ! ______________________________________________________________________
14350   L4480: ! If EM(5)=1 Then Let DED$(29)="4" ! weeks worked
14360   if em(5)=2 then let ded$(29)="2"
14370   if em(5)=3 then let ded$(29)="2"
14380   if em(5)=4 then let ded$(29)="1"
14390   ! ______________________________________________________________________
14400   WRITE_PAYROLL_TRANS: ! 
14402   ! pr tr$(1),tr$(5) :  pause
14410   mat tr=(0)
14412   ! r: removed existing CL Check (and it's allocations) first
14420   clk$=lpad$(str$(bankcode),2)&"1"&tr$(1)
14430   read #h_cl_trans,using 'form pos 79,2*pd 3',key=clk$: nt1 nokey L4610
14440   delete #h_cl_trans,key=clk$: 
14450   let key$=lpad$(str$(bankcode),2)&str$(tcde)&rpad$(tr$(1),8)
14460   restore #h_cl_trans_alloc,key>=key$: nokey L4610
14470   L4590: ! 
14480   read #h_cl_trans_alloc,using 'Form Pos 1,C 11': newkey$ eof L4610
14490   if newkey$=key$ then 
14500     delete #h_cl_trans_alloc: 
14510     goto L4590
14520   end if 
14530   L4610: ! 
14532   ! /r
14540   !   if exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then
14550   let tx3=val(tr$(3))
14560   let tr2=val(tr$(2))
14570   write #h_cl_trans,using F_CL_TRANS_V1: bankcode,1,tr$(1),tr2,tx3,tr$(4),tr$(5),0,clr,4
14580   !     goto L4630
14590   !   end if
14600   !     if version(h_cl_trans)>0 then
14610   !       let tx3=val(tr$(3))
14620   !       let tr2=val(tr$(2))
14630   !       write #h_cl_trans,using F_CL_TRANS_v1: bankcode,1,tr$(1),tr2,tx3,tr$(4),tr$(5),0,clr,4,mat tr
14640   !     else 
14650   !       write #h_cl_trans,using F_CL_TRANS_v0: bankcode,1,mat tr$,0,clr,4,mat tr
14652   ! F_CL_TRANS_v0: form pos 1,n 2,n 1,c 8,g 6,g 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
14660   !     end if
14670   ! L4630: !
14680   !   close #h_cl_trans: ioerr ignore
14690   !   open #h_cl_trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TRIDX1.h"&env$('cno')&",Shr",internal,outin,keyed
14700   read #h_cl_payee,using 'form pos 129,pd 5.2',key=lpad$(rtrm$(tr$(4)),8): ytdp nokey L4690 ! UPDATE PAYEE FILE
14710   let ytdp=ytdp+val(tr$(3)) conv ignore
14720   ! REWRITE #h_cl_payee,USING 3720,KEY=LPAD$(RTRM$(TR$(4)),8): YTDP NOKEY 3730
14740   L4690: ! 
14750   read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey L4740
14760   bn$=rtrm$(bn$)
14770   bal=bal-val(tr$(3)) conv ignore
14790   rewrite #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2): bn$,bal,upi,tr$(1) nokey L4740
14800   F_CLFILE_12: form pos 3,c 30,pos 45,pd 6.2,pd 6.2,g 8
14810   L4740: ! 
14820   ! k$=lpad$(rtrm$(str$(bankcode)),2)&lpad$(str$(1),1)&lpad$(tr$(1),8)
14840   F_CL_TRANS_V1: form pos 1,n 2,n 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
14850   ! WRITE ALLOCATIONS
14860   if allign=1 then goto L5250
14870   for j=1 to 29
14880    if val(ded$(j))=0 then goto L5230
14890    let gl$=""
14900    on j goto L4840,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L5220,L5220,BCR_GLN_VALIDATED none L5230
14910    ! ______________________________________________________________________
14920    L4840: ! 
14930    for j1=1 to tdepXcount
14940      if j<6 or j>25 then goto L4870 ! kj 91707
14950      if dedcode(j-5)=3 then goto L5230 ! kj 91707  don't write entries for benefits
14960      L4870: ! 
14970      alloc=tdep(j1,1)
14980      let gl$=cnvrt$("N 3",tdep(j1,2))&cnvrt$("N 6",tdep(j1,3))&cnvrt$("N 3",tdep(j1,4))
14990      let sd5$="Gross Pay"
15000      goto L4990
15010      L4910: ! 
15020      if j=2 then let sd5$="Federal WH" : let gl$=gln$(1)
15030      if j=3 then let sd5$="FICA WH" : let gl$=gln$(2) : let fica0=val(ded$(j))
15040      if j=4 then let sd5$="Medicare" : let gl$=gln$(2) : let medi0=val(ded$(j)): goto L4990
15050      if j=5 then let sd5$="State WH" : let gl$=gln$(3)
15060      if j>5 and j<26 then let sd5$=abrevname$(j-5) : let gl$=gl$(j-5)
15070      if j=26 then let gl$=gln$(1): let sd5$="eic" : goto L4990 ! use federal
15080      if j=27 then goto L4990 ! skip tips i think
15090      ! If J=28 Then Let GL$=GLN$(1): Let SD5$="Meals" : Goto 4890 ! use wages
15100      L4990: ! 
15110      cd1=1
15120      read #h_cl_glmstr,using F_CL_GLMSTR,key=rpad$(gl$,kln(h_cl_glmstr)),release: de$ nokey INVALIDGLNUMBER
15130      F_CL_GLMSTR: form pos 13,c 30
15140      BCR_GLN_VALIDATED: ! 
15150      if j>1 then alloc=val(ded$(j))
15160      if j=29 then let miscode=(alloc*100)+29 else let miscode=j
15170      ! store # of deduction in the invoice date field;
15180      ! if weeks worked store weeks worked and code both
15190      if j>1 then alloc=-alloc
15200      if j<6 or j>25 then goto L5070 ! all tax w/h = negative
15210      if dedcode(j-5)=2 then alloc=-alloc ! reverse on additions to net
15220      L5070: ! 
15230      if j=30 then alloc=0 ! meals
15240      !       if env$('client')="Washington Parrish" and j=16 then alloc=0 ! don't allow tips to post on washington parrish
15250      if j=4 and ficam1=1 then alloc=alloc-medic3 ! prb 2012
15260      if j=3 and ficam1=1 then alloc=alloc-ficam3 ! prb 2012
15280      if alloc=0 then goto L5220 ! dont write zero allocation
15290      let lr3=lrec(23)+1
15300      if j=3 then let fica1=alloc : let fica_rec=lr3
15310      if j=4 then let medi1=alloc
15320      write #h_cl_trans_alloc,using L5140,rec=lr3: bankcode,1,val(tr$(1)),gl$,alloc,de$(1:30),miscode,0
15330      L5140:  form pos 1,n 2,n 1,g 8,c 12,pd 5.2,c 30,n 6,pd 3
15340       !       if ~exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then
15350       !         if tr(2)>0 then
15360       !           rewrite #h_cl_trans_alloc,using 'form pos 65,pd 3',rec=tr(2): lr3
15370       !         end if
15390       !         if tr(1)=0 then let tr(1)=lr3
15400       !         let tr(2)=lr3
15422       !         rewrite #h_cl_trans,using 'form pos 79,2*pd 3',key=k$: mat tr
15424       !       end if
15440      L5220: ! 
15450      if j=1 then next j1
15460      L5230: ! 
15470   next j
15480   fn_mgl
15490   L5250: ! 
15500 fnend 
15510 ! ______________________________________________________________________
15680 def fn_cknum ! check for duplicate check numbers
15690   L5410: ! 
15700   let dk$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(check_number),8)
15710   read #h_cl_trans,using L5440,key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey L5720
15720   let dtr$(3)=str$(dtr3)
15730   L5440: form pos 4,c 8,g 6,pd 10.2,c 8,c 35,pos 79,2*pd 3
15740   DUPLICATE_CHECK: ! 
15750   fntos(sn$="Prckprt4")
15760   let respc=0: let mypos=50
15770   fnlbl(1,1,"Check Number "&str$(check_number)&" has been previously used!",50,1)
15780   fnlbl(3,1,"Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",val(dtr$(2))),50,0)
15790   fnlbl(4,1,"Amount: "&dtr$(3),50,0)
15800   fnlbl(5,1,"To: "&rtrm$(dtr$(5)),50,0)
15810   fnlbl(7,1,"Click          to Delete the previous entry else" ,50,1)
15820   fnbutton(7,10,"Delete",3,"Press Delete to delete the old check from history and replace it with the new check, else",1,6)
15830   let text$="enter the correct check number for "&trim$(dtr$(5))&":"
15840   let textlenght=len(trim$(text$))
15850   fnlbl(8,4,text$,textlenght,0)
15860   fntxt(8,textlenght+7,7,0,1,"30",0,"")
15870   let resp$(respc+=1)=str$(check_number)
15880   fncmdkey("&Next",1,1,0,"Continue with checkprinting." )
15890   fncmdkey("E&xit",5,0,1,"Returns to menu")
15900   fnacs(sn$,0,mat resp$,ckey) ! dupllicate check number
15910   if ckey=5 then goto XIT
15920   if ckey=3 then goto L5670 ! if delete
15930   ckn2=val(resp$(1))
15940   if ckn2=0 then goto DUPLICATE_CHECK
15950   check_number=ckn2
15960   let tr$(1)=lpad$(str$(ckn2),8)
15970   goto L5410
15980   ! ______________________________________________________________________
15990   L5670: ! 
16000   bal=bal+val(dtr$(3))
16010   delete #h_cl_trans,key=dk$: 
16020   let key$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(check_number),8)
16030   restore #h_cl_trans_alloc,key>=key$: nokey L5720
16040   do
16050     read #h_cl_trans_alloc,using 'Form Pos 1,C 11': newkey$ eof L5720
16060     if newkey$=key$ then 
16070       delete #h_cl_trans_alloc: 
16080     end if 
16090   loop while newkey$=key$
16100   L5720: ! 
16110 fnend 
16120 def fn_mgl ! WRITE BENEFITS & FICA MATCH
16130   cd1=2
16140   for j=1 to tdepXcount
16150     mat mgl$=("            ")
16160     read #7,using L5790,key=lpad$(str$(tdep(j,5)),3): mat mgl$ nokey L5800
16170     L5790: form pos 4,11*c 12
16180     L5800: ! 
16190     for j2=6 to 16
16200       if tdep(j,j2)=0 then goto L5980
16210       if j2>6 then goto L5870
16220       if ficam1=0 then goto L5980
16230       let sd5$=de$="FICA Match"
16240       let fica2=fica2+tdep(j,j2)
16250       let j4=3
16260       goto L5900
16270       L5870: ! 
16280       if dedcode(j2-6)><3 then goto L5980
16290       let j4=j2-2
16300       let sd5$=de$=rtrm$(abrevname$(j4-4))&" Match"
16310       L5900: ! 
16320       let gl$=mgl$(j2-5)
16330       read #h_cl_glmstr,using F_CL_GLMSTR,key=gl$,release: de$ nokey INVALIDGLNUMBER
16340       ! 
16350       ! 
16360       EXLNKD_L5920: ! 
16370       let lr3=lrec(23)+1
16380       write #h_cl_trans_alloc,using L5140,rec=lr3: bankcode,1,val(tr$(1)),gl$,tdep(j,j2),de$(1:30),j4,0
16390       !       if ~exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then
16400       !         if tr(2)>0 and version(h_cl_trans_alloc)=2 then
16410       !           rewrite #h_cl_trans_alloc,using 'form pos 65,pd 3',rec=tr(2): lr3
16420       !         end if
16430       !         if tr(1)=0 then let tr(1)=lr3
16440       !         let tr(2)=lr3
16442       !         rewrite #h_cl_trans,using 'form pos 79,2*pd 3',key=k$: mat tr
16444       !       end if
16460       L5980: ! 
16470     next j2
16480   next j
16490   ! let fn_FICA_FIX
16500 fnend 
16510 IGNORE: continue 
16520 ! <Updateable Region: ERTN>
16530 ERTN: let fnerror(program$,err,line,act$,"xit")
16540   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
16550   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
16560   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
16570 ERTN_EXEC_ACT: execute act$ : goto ERTN
16580 ! /region
16590 ! r: Check pr routines
17000 def fn_print_check
17010   if ttc(32)<=0 then 
17020     ca$="***VOID***"
17030   else 
17040     ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",ttc(32)))
17050   end if 
17060   gosub ENG_DOL_IDK
17070   if uprc$(ddcode$)="D" then let eng$="Direct Deposit" : ca$="V O I D"
17080   if env$('client')="ACS" then 
17090     fn_check_acs
17100   else if env$('client')="Ash Grove" then 
17110     fn_check_legacy(3,3)
17140   else if env$('client')="Bethany" then 
17150     fn_check_bethany
17160   else if env$('client')="Billings" then 
17170     fn_check_billings
17180   else if env$('client')="Campbell" then 
17190     fn_check_dynamic(26,13,13,9,15, 58,69)
17200   else if env$('client')="Carr Plumbing" then 
17210     length                = 26
17220     line_date             =  4
17230     line_amount           =  7
17240     line_amount_english   = 10
17250     line_name_and_address = 14
17260     pos_date              = 78
17270     pos_amt               = 72
17280     line_nameOnly         =  7
17290     pos_nameOnly=0 ! default to 12
17300     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly)
17310   else if env$('client')="Cerro Gordo" then 
17320     fn_check_cerrogordo
17330   else if env$('client')="Cerro Gordo T" then 
17340     length                = 27
17350     line_date             = 14
17360     line_amount           = 14
17370     line_amount_english   =  9
17380     line_name_and_address = 15
17390     pos_date              = 55
17400     pos_amt               = 71
17410     line_nameOnly         =  0
17420     pos_nameOnly          =  0
17430     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly)
17440   else if env$('client')="Divernon" then 
17442     fn_check_divernon
17450   else if env$('client')="Edinburg" then 
17452     fn_check_edinburg
17460   else if env$('client')="Edison" then ! r: 6/29/17  (most recent)
17461     length                = 25
17462     line_date             =  6
17463     line_amount           = 11
17464     line_amount_english   = 9
17465     line_name_and_address = 13  ! 0/not printing is the default
17466     pos_date              = 74  ! 65 is default
17467     pos_amt               = 79 ! pos_date+18 is default
17468     line_nameOnly         =  0
17469     pos_nameOnly          =  0
17470     line_checkNumber      = line_date
17471     pos_checkNumber       = 83
17473     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number)
17474     ! /r
17480   else if env$('client')="Energy Exchanger" then ! r: 5/9/2017 
17490     length                = 23
17500     line_date             =  4
17510     line_amount           = 12
17520     line_amount_english   = 9
17530     line_name_and_address = 13
17540     pos_date              = 75
17550     pos_amt               = 80
17560     line_nameOnly         =  0
17570     pos_nameOnly          =  0
17572     line_checkNumber      = line_date
17574     pos_checkNumber       = 86
17580     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number)
17582     ! /r
17590   else if env$('client')="Hope Welty" then 
17600     fn_check_hope_welty
17610   else if env$('client')="Kathys Bookkeeping" then 
17620     fn_check_dynamic(22,9,9,6,10, 58,72)
17630   else if env$('client')="Lamar" then 
17640     fn_check_lamar
17650   else if env$('client')="Lovington" then 
17660     fn_check_lovington
17670   else if env$('client')="Merriam Woods" then 
17680     fn_check_merriam_woods
17690   else if env$('client')="Monticello" then 
17700     fn_check_monticello
17710   else if env$('client')="Philo" then 
17720     fn_check_philo
17750   else if env$('client')="Thomasboro" then 
17760     fn_check_thomasboro
17770   else if env$('client')="Thomas Richardson" then 
17780     fn_check_tom_richardson
17790   else if env$('client')="Unity" then 
17800     fn_check_unity
17830   else if env$('client')="West Accounting" then 
17840     fn_check_west_accounting
17850   else 
17860     fn_check_dynamic(21,6,6,7,13) ! let fn_check_legacy ! default for everyone without a special routine...
17870   end if 
17880 fnend 
18000 def fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address; pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,checkNumber)
18020   ! 
18040   if pos_date=0 then let pos_date=65
18060   if pos_amt=0 then let pos_amt=pos_date+18
18080       if pos_nameOnly=0 then let pos_nameOnly=12
18100   ! 
18120   for line_item=1 to length
18140     if line_item=line_date and line_item=line_amount then 
18150       pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),pos pos_amt,c 18': dat,ca$
18160     else if line_item=line_date and line_item=line_checkNumber and line_checkNumber<>0 then 
18170       pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),pos pos_checkNumber,N 8': dat,checkNumber
18180     else if line_item=line_date then 
18200       pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ)': dat
18220     else if line_item=line_nameOnly and line_item=line_amount then
18240       pr #255,using "form Pos pos_nameOnly,C 30,pos pos_amt,c 18": em$(1),ca$
18260     else if line_item=line_nameOnly then
18280       pr #255,using "form Pos pos_nameOnly,C 30": em$(1)
18300     else if line_item=line_amount and line_item=line_name_and_address then
18320       pr #255,using "form Pos 12,C 30,pos pos_amt,c 18": em$(1),ca$
18340       pr #255,using "form Pos 12,C 30": em$(2) : let line_item+=1
18360       pr #255,using "form Pos 12,C 30": em$(3) : let line_item+=1
18380     else if line_item=line_amount_english then 
18400       pr #255,using 'form Pos 9,C 62': eng$(1:n)
18420       pr #255,using 'form Pos 9,C 62': eng$(n+1:128) : let line_item+=1
18440     else if line_item=line_name_and_address then 
18460       pr #255,using "form Pos 12,C 30": em$(1)
18480       pr #255,using "form Pos 12,C 30": em$(2) : let line_item+=1
18500       pr #255,using "form Pos 12,C 30": em$(3) : let line_item+=1
18520     else if line_item=line_amount then
18540       pr #255,using "form pos pos_amt,c 18": ca$
18560     else 
18580       pr #255: ""
18600     end if 
18620   next line_item
18640 fnend 
19000 def fn_check_legacy(; extra_lines_at_top,extra_lines_at_bottom)
19020   for j=1 to extra_lines_at_top
19040     pr #255: ""
19060   next j
19070   fn_check_dynamic(21+extra_lines_at_bottom,6,6,7,13)
19460 fnend 
20000 def fn_check_acs
20020   pr #255,using 'Form skip 3,POS 80,PIC(ZZ/ZZ/ZZ)': dat
20040   pr #255,using 'Form skip 2,POS 10,c 30,pos 74,c 18': em$(1),ca$
20060   pr #255,using 'Form skip 2,pos 9,C 62': eng$(1:n)
20080   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
20100   for j=1 to 3
20120     pr #255: ""
20140   next j
20160   for j=1 to 3
20180     pr #255,using "Form Pos 12,C 60": em$(j)
20200   next j
20220   pr #255,using "form pos 1,c 1,skip 7": ""
20240 fnend 
22000 def fn_check_bethany
22020   for j=1 to 5
22040     pr #255: ""
22060   next j
22080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
22120   let datepos=65
22140   pr #255: 
22160   pr #255,using 'Form POS 9,C 62': eng$(1:n)
22180   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
22200   for j=1 to 3
22220     pr #255: ""
22240   next j
22260   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
22280   pr #255: 
22300   for j=1 to 3
22320     pr #255,using "Form Pos 12,C 60": em$(j)
22340   next j
22360   pr #255,using "form pos 1,c 1,skip 9": "" ! changed skip 6 to skip 9 on 10/17/2016
22380 fnend 
24000 def fn_check_billings
24040   pr #255: ""
24060   pr #255: ""
24080   pr #255,using 'form pos 40,c 38,skip 1': "Void After 60 Days"
24100   pr #255: ""
24110   pr #255: ""
24140   pr #255,using 'Form POS 9,C 62': eng$(1:n)
24160   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
24180   pr #255,using 'Form POS 63,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
24200   pr #255: ""
24220   pr #255: ""
24240   pr #255: ""
24260   pr #255: ""
24280   pr #255,using "Form Pos 12,C 60": em$(1)
24300   pr #255,using "Form Pos 12,C 60": em$(2)
24320   pr #255,using "Form Pos 12,C 60": em$(3)
24340   pr #255: ""
24360   pr #255: ""
24380   pr #255: ""
24400   pr #255: ""
24420   pr #255: ""
24440   pr #255: ""
24460   pr #255: ""
24480   pr #255: ""
24490   pr #255: ""
24540 fnend 
28000 def fn_check_cerrogordo
28020   pr #255: "" : pr #255: "" : pr #255: ""
28040   pr #255: "" : pr #255: "" : pr #255: ""
28080   pr #255,using 'Form POS 9,C 62': eng$(1:n)
28100   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
28120   pr #255: ""
28140   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
28160   pr #255: ""
28180   pr #255: ""
28200   for j=1 to 3
28220     pr #255,using "Form Pos 12,C 60": em$(j)
28240   next j
28260   pr #255: ""
28280   pr #255: ""
28300   pr #255: ""
28320   pr #255: ""
28340   pr #255: ""
28360   pr #255: ""
28380   pr #255: ""
28400 fnend 
32000 def fn_check_divernon
32020   pr #255: ""
32040   pr #255: ""
32060   pr #255: ""
32080   pr #255: ""
32100   pr #255: ""
32120   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
32140   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
32160   pr #255,using 'Form POS 9,C 62': eng$(1:n)
32180   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
32200   pr #255: ""
32220   pr #255: ""
32240   pr #255: ""
32260   pr #255,using "Form Pos 12,C 60": em$(1)
32280   pr #255,using "Form Pos 12,C 60": em$(2)
32300   pr #255,using "Form Pos 12,C 60": em$(3)
32320   pr #255: ""
32340   pr #255: ""
32360   pr #255: ""
32380   pr #255: ""
32400   pr #255: ""
32420   pr #255: ""
32440 fnend 
34000 def fn_check_edinburg
34020   for j=1 to 5
34040     pr #255: ""
34060   next j
34080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
34120   let datepos=65
34140   pr #255: 
34160   pr #255,using 'Form POS 9,C 62': eng$(1:n)
34180   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
34200   for j=1 to 3
34220     pr #255: ""
34240   next j
34260   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
34280   pr #255: 
34300   for j=1 to 3
34320     pr #255,using "Form Pos 12,C 60": em$(j)
34340   next j
34360   pr #255,using "form pos 1,c 1,skip 6": ""
34380   pr #255: : pr #255: 
34400 fnend 
36000 def fn_check_hope_welty
36020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
36040   pr #255: : pr #255: 
36080   pr #255,using 'Form POS 9,C 62': eng$(1:n)
36100   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
36120   pr #255: 
36140   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
36160   let x=3
36180   for j=1 to x
36200     pr #255: ""
36220   next j
36240   for j=1 to 3
36260     pr #255,using "Form Pos 12,C 60": em$(j)
36280   next j
36300   pr #255: ""
36320   pr #255: ""
36340   pr #255: ""
36360   pr #255: ""
36380   pr #255: ""
36400   pr #255: ""
36420 fnend 
38000 def fn_check_lamar
38020   for j=1 to 9
38040     pr #255: ""
38060   next j
38080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
38120   let datepos=65
38140   pr #255,using 'Form POS datepos,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
38160   pr #255,using 'Form POS 9,C 62': eng$(1:n)
38180   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
38200   for j=1 to 3
38220     pr #255: ""
38240   next j
38260   for j=1 to 3
38280     pr #255,using "Form Pos 12,C 60": em$(j)
38300   next j
38320   pr #255,using "form pos 1,c 1,skip 6": ""
38340 fnend 
40000 def fn_check_lovington
40020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
40040   pr #255,using "form pos 76,n 8,skip 1": check_number
40060   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
40070   pr #255: ''
40080   pr #255,using 'Form POS 63,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
40100   pr #255,using 'Form POS 9,C 62': eng$(1:n)
40120   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
40180   pr #255: ""
40190   pr #255,using "Form Pos 12,C 60": em$(1)
40200   pr #255,using "Form Pos 12,C 60": em$(2)
40220   pr #255,using "Form Pos 12,C 60": em$(3)
40280   pr #255: ""
40300   pr #255: ""
40320   pr #255: ""
40340   pr #255: ""
40360   pr #255: ""
40380   pr #255: ""
40400   pr #255: ""
40420   pr #255: ""
40440   pr #255: ""
40450   pr #255: ""
40460 fnend 
42000 def fn_check_monticello
42020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
42060   pr #255,using 'Form POS 9,C 62': eng$(1:n)
42080   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
42100   pr #255: : pr #255: 
42120   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 2,C 18': dat,ca$
42140   pr #255: ""
42160   for j=1 to 3
42180     pr #255,using "Form Pos 12,C 60": em$(j)
42200   next j
42220   pr #255: ""
42240   pr #255: ""
42260   pr #255: ""
42280   pr #255: ""
42300   pr #255: ""
42320   pr #255: ""
42340   pr #255: ""
42360 fnend 
44000 def fn_check_merriam_woods
44020   pr #255: ""
44040   pr #255: ""
44060   pr #255: ""
44080   pr #255: ""
44100   pr #255: ""
44120   pr #255: ""
44140   pr #255: ""
44160   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
44200   pr #255,using 'Form POS 9,C 62': eng$(1:n)
44220   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
44240   pr #255: ""
44260   pr #255,using 'Form POS 65,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
44280   pr #255: ""
44300   pr #255: ""
44320   pr #255,using "Form Pos 12,C 60": em$(1)
44340   pr #255,using "Form Pos 12,C 60": em$(2)
44360   pr #255,using "Form Pos 12,C 60": em$(3)
44380   pr #255: ""
44400   pr #255: ""
44420   pr #255: ""
44440   pr #255: ""
44460   pr #255: ""
44480   pr #255: ""
44500 fnend 
48000 def fn_check_philo
48020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
48060   pr #255,using 'Form POS 9,C 62': eng$(1:n)
48080   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
48100   pr #255: : pr #255: 
48120   pr #255,using 'Form POS 57,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
48140   let x=3
48160   for j=1 to x
48180     pr #255: ""
48200   next j
48220   for j=1 to 3
48240     pr #255,using "Form Pos 12,C 60": em$(j)
48260   next j
48280   pr #255: ""
48300   pr #255: ""
48320   pr #255: ""
48340   pr #255: ""
48360   pr #255: ""
48380 fnend 
52000 def fn_check_tom_richardson
52020   !   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255:
52040   pr #255: ""
52060   pr #255: ""
52080   pr #255: ""
52100   pr #255: ""
52120   pr #255: ""
52140   pr #255: ""
52160   pr #255: ""
52180   pr #255: ""
52200   pr #255: ""
52210   pr #255: ""
52220   pr #255: ""
52240   pr #255,using 'Form POS 67,PIC(ZZ/ZZ/ZZ)': dat
52260   pr #255,using 'Form POS 9,C 62,X 4,C 18': eng$(1:n),ca$
52280   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
52300   pr #255: ""
52320   pr #255,using "Form Pos 12,C 60": em$(1)
52340   pr #255,using "Form Pos 12,C 60": em$(2)
52360   pr #255,using "Form Pos 12,C 60": em$(3)
52380   pr #255: ""
52400   pr #255: ""
52420   pr #255: ""
52440   pr #255: ""
52460   pr #255: ""
52480   pr #255: ""
52500   pr #255: ""
52520 fnend 
54000 def fn_check_thomasboro
54020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
54060   pr #255,using 'Form POS 9,C 62': eng$(1:n)
54080   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
54100   pr #255: : pr #255: : pr #255: 
54120   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
54140   let x=3
54160   for j=1 to x
54180     pr #255: ""
54200   next j
54220   for j=1 to 3
54240     pr #255,using "Form Pos 12,C 60": em$(j)
54260   next j
54280   pr #255: ""
54300   pr #255: ""
54320   pr #255: ""
54340   pr #255: ""
54360   pr #255: ""
54380   pr #255: ""
54400 fnend 
56000 def fn_check_unity
56020   for j=1 to 5
56040     pr #255: ""
56060   next j
56080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
56120   let datepos=65
56140   pr #255: 
56160   pr #255,using 'Form POS 9,C 62': eng$(1:n)
56180   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
56200   for j=1 to 3
56220     pr #255: ""
56240   next j
56260   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
56280   pr #255: 
56300   for j=1 to 3
56320     pr #255,using "Form Pos 12,C 60": em$(j)
56340   next j
56360   pr #255,using "form pos 1,c 1,skip 6": ""
56380 fnend 
57000 def fn_check_west_accounting
57020   for j=1 to 8
57040     pr #255: ""
57060   next j
57080   let datepos=65
57100   pr #255,using 'Form POS datepos,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
57120   pr #255,using 'Form POS 9,C 62': eng$(1:n)
57140   pr #255,using 'Form POS 9,C 62': eng$(n+1:128)
57160   pr #255: ""
57180   pr #255: ""
57200   pr #255: ""
57220   for j=1 to 3
57240     pr #255,using "Form Pos 12,C 60": em$(j)
57260   next j
57280   for j=1 to 8
57300     pr #255: ""
57320   next j
57340 fnend 
59000 ! /r
60000 ! r: Stub pr routines
60010 def fn_print_stub
60020   let tdedcp=tdedytd=0
60030   for j=1 to 23
60040     if j<5 then 
60050       goto L2230
60060     else if dedcode(j-4)=2 then 
60070       let tdedcp=tdedcp-ttc(j)
60080       let tdedytd=tdedytd-tty(j)
60090       goto L2240
60100     else if dedcode(j-4)=3 then 
60110       goto L2240
60120     end if 
60130     L2230: ! 
60140     let tdedcp=tdedcp+ttc(j): let tdedytd=tdedytd+tty(j)
60150     L2240: ! 
60160   next j
60170   if env$('client')='Billings' then 
60180     fn_stub_billings
60222   else if env$('client')="Edison" then 
60224     fn_stub_standard( 0,2)
60230   else if env$('client')="Kathys Bookkeeping" then 
60240     fn_stub_standard( 0,4)
60250   else if env$('client')='Kincaid' then 
60260     fn_stub_kincaid
60270   else if env$('client')='West Accounting' then 
60280     fn_stub_standard( 1) ! standard, but show tips
60290   else if env$('client')='Energy Exchanger' then 
60300     ! r: setup mat ltext$ and mat lPos for fn_stub_hitBoxes
60310     ! Page Settings:  Orientation: Landscapt
60320     !                      Height:  7"
60330     !                       Width:  8.5"
60340     !                       Lines Per Page:  54 (default)
60350     !                       Font Size:  10
60360     !                      Top Margin: .5
60370     !                   Bottom Margin: .5
60380     !                     Left Margin: .2
60390     !                    Right Margin: .2
60400     dim ltext$(0,0)*256,lPos(0,0)
60410     mat ltext$(15,10) : mat lPos(15,10)
60420     mat ltext$=('')
60430     ! lyne=12 : litem=0
60440     !   lpos(lyne,litem+=1)= 1   : ltext$(lyne,litem)=rpt$('----+----|',12)
60450     lyne=5 : litem=0
60460       lpos(lyne,litem+=1)=  1   : ltext$(lyne,litem)=str$(eno)
60470       lpos(lyne,litem+=1)= 13   : ltext$(lyne,litem)=date$(days(d1,'ccyymmdd'),'mm/dd/yy')
60480       lpos(lyne,litem+=1)= 22   : ltext$(lyne,litem)=cnvrt$('N 8.2',rate) ! Hourly Rate
60490       lpos(lyne,litem+=1)= 32   : ltext$(lyne,litem)=cnvrt$('N 6.2',tdc1) ! Reg Hours
60500       lpos(lyne,litem+=1)= 46   : ltext$(lyne,litem)=cnvrt$('N 5.2',tdc2) ! OT Hours
60510       lpos(lyne,litem+=1)= 60   : ltext$(lyne,litem)='' ! daily Rate
60520       lpos(lyne,litem+=1)= 67   : ltext$(lyne,litem)='' ! No Days
60530       lpos(lyne,litem+=1)= 80   : ltext$(lyne,litem)='' ! Monthly Rate
60540       lpos(lyne,litem+=1)= 90  : ltext$(lyne,litem)='' ! Salary Adjustment
60550       lpos(lyne,litem+=1)=112  : ltext$(lyne,litem)=cnvrt$('pic(## ## ##)',prdmmddyy) ! Check Date MM DD YY
60560     lyne=11 : litem=0
60570       lpos(lyne,litem+=1)=  1   : ltext$(lyne,litem)=cnvrt$('N  9.2',ttc(31))! gross salary
60580       lpos(lyne,litem+=1)= 12   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(1)) ! Fed w/h
60590       lpos(lyne,litem+=1)= 24   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(4)) ! State w/h
60600       lpos(lyne,litem+=1)= 36   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(2)) ! FICA
60610       lpos(lyne,litem+=1)= 50   : ltext$(lyne,litem)=cnvrt$('N  9.2',ttc(5)) ! pre insurance tax
60620       lpos(lyne,litem+=1)= 60   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(6))
60630       lpos(lyne,litem+=1)= 72   : ltext$(lyne,litem)=cnvrt$('N 11.2',ttc(7))
60640       lpos(lyne,litem+=1)= 84   : ltext$(lyne,litem)=cnvrt$('N 10.2',ttc(8))
60650       lpos(lyne,litem+=1)= 96   : ltext$(lyne,litem)=cnvrt$('N  8.2',ttc(9))
60660       lpos(lyne,litem+=1)=106   : ltext$(lyne,litem)=cnvrt$('N 13.2',ttc(32)) ! net pay 
60670     lyne=14 : litem=0                                   ! YTD of line above
60680       lpos(lyne,litem+=1)=  1   : ltext$(lyne,litem)=cnvrt$('N  9.2',tty(31)) ! gross ytd
60690       lpos(lyne,litem+=1)= 12   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(1)) ! fed w/h ytd
60700       lpos(lyne,litem+=1)= 24   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(4)) ! state w/h ytd
60710       lpos(lyne,litem+=1)= 36   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(2)) ! FICA YTD
60720       lpos(lyne,litem+=1)= 50   : ltext$(lyne,litem)=cnvrt$('N  9.2',tty(5))
60730       lpos(lyne,litem+=1)= 60   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(6))
60740       lpos(lyne,litem+=1)= 72   : ltext$(lyne,litem)=cnvrt$('N 11.2',tty(7))
60750       lpos(lyne,litem+=1)= 84   : ltext$(lyne,litem)=cnvrt$('N 10.2',tty(8))
60760       lpos(lyne,litem+=1)= 96   : ltext$(lyne,litem)=cnvrt$('N  8.2',tty(9))
60770       lpos(lyne,litem+=1)=106   : ltext$(lyne,litem)=''
60880     ! /r
60890     pr #255: '{\fs16 '; !   <-- set to font size 8 
60900     fn_stub_hitBoxes(mat ltext$,mat lPos)
60910     pr #255: "}" ! <-- end the font size of 8
60920     fn_stub_energyExcnahger_extra(mat v,mat abrevname$,mat deptsum)
60930   else 
60940     fn_stub_standard
60950   end if 
60960 fnend 
61900 F_STUB_01: form pos 3,c 18,2*pic(-------.--),x 4,c 12,pic(-------.--),pic(-----------.--)
61920 F_STUB_02: form pos 3,c 9,pic(------.--),2*pic(-------.--),x 4,c 12,pic(-------.--),pic(-----------.--)
62000 def fn_stub_standard(; stst_show_tips,ststaddlength)
62020   pr #255: ""
62040   pr #255,using 'form pos 3,c 30,n 8,x 2,c 13,pic(zz/zz/zz),n 10': em$(1),eno,"",prdmmddyy,check_number
62060   for j=1 to 20
62080     if tty(j+4)=0 then abrevname$(j)="" else abrevname$(j)=hnames$(j)
62100   next j
62120   pr #255,using 'form pos 60,c 7,pos 77,c 3': "Current","YTD"
62140   if uprc$(accr$)="N" then 
62160     pr #255,using 'form pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Hours","Pay","Med W/H",ttc(3),tty(3)
62180   else 
62200     pr #255,using 'form pos 14,c 7,pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Accrued","Hours","Pay","Med W/H",ttc(3),tty(3)
62220   end if 
62240   pr #255,using F_STUB_01: "Regular",tdc1,ttc(26),"Fed W/H",ttc(1),tty(1)
62260   pr #255,using F_STUB_01: "Over Time",tdc2,ttc(27),"FICA W/H",ttc(2),tty(2)
62280   if uprc$(accr$)="N" then 
62300     pr #255,using F_STUB_01: "Sick",tdc3,tpd3,"State W/H",ttc(4),tty(4)
62320     pr #255,using F_STUB_01: "Vacation",tdc4,tpd4,abrevname$(1),ttc(5),tty(5)
62340   else 
62360     pr #255,using F_STUB_02: "Sick",em10,tdc3,tpd3,"State W/H",ttc(4),tty(4)
62380     pr #255,using F_STUB_02: "Vacation",em11,tdc4,tpd4,abrevname$(1),ttc(5),tty(5)
62400   end if 
62420   pr #255,using F_STUB_01: "Holiday",tdc5,tpd5,abrevname$(2),ttc(6),tty(6)
62440   pr #255,using F_STUB_01: "Other",0,ttc(28),abrevname$(3),ttc(7),tty(7)
62460   pr #255,using F_STUB_01: rt$,0,0,abrevname$(4),ttc(8),tty(8)
62480   if trim$(compcode$)="" then 
62500     pr #255,using F_STUB_01: "",0,0,abrevname$(5),ttc(9),tty(9)
62520   else 
62540     fn_extract_comp_time
62560     pr #255,using F_STUB_02: "Comp Time",balance,0,0,abrevname$(5),ttc(9),tty(9)
62580   end if 
62600   pr #255,using F_STUB_01: "YTD Pay",0,tty(31),abrevname$(6),ttc(10),tty(10)
62620   pr #255,using F_STUB_01: "YTD Deductions",0,tdedytd,abrevname$(7),ttc(11),tty(11)
62640   pr #255,using F_STUB_01: "Current Pay",0,ttc(31),abrevname$(8),ttc(12),tty(12)
62660   pr #255,using F_STUB_01: "Cur Deductions",0,tdedcp,abrevname$(9),ttc(13),tty(13)
62680   pr #255,using F_STUB_01: "Net Pay",0,ttc(32),"Other",ttc(14)+ttc(15)+ttc(16)+ttc(17)+ttc(18)+ttc(19)+ttc(20)+ttc(21)+ttc(22)+ttc(23)+ttc(24),tty(14)+tty(15)+tty(16)+tty(17)+tty(18)+tty(19)+tty(20)+tty(21)+tty(22)+tty(23)+tty(24)
62700   pr #255: ""
62720   if stst_show_tips and ttc(28)<>0 then 
62740     pr #255,using F_STUB_02: "Tips",ttc(28)
62760   else 
62780     pr #255: ""
62800   end if 
62820   pr #255: ""
62840   for j=1 to ststaddlength
62860     pr #255: ""
62880   next j
62900 fnend 
63000 def fn_stub_kincaid
63020   pr #255: ""
63040   pr #255,using 'form pos 3,c 30,n 8,x 2,c 13,pic(zz/zz/zz),n 10': em$(1),eno,"",prdmmddyy,check_number
63060   for j=1 to 20
63080     if tty(j+4)=0 then abrevname$(j)="" else abrevname$(j)=hnames$(j)
63100   next j
63120   pr #255,using 'form pos 60,c 7,pos 77,c 3': "Current","YTD"
63140   if uprc$(accr$)="N" then 
63160     pr #255,using 'form pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Hours","Pay","Med W/H",ttc(3),tty(3)
63180   else 
63200     pr #255,using 'form pos 14,c 7,pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Accrued","Hours","Pay","Med W/H",ttc(3),tty(3)
63220   end if 
63240   pr #255,using F_STUB_01: "Regular",tdc1,ttc(26),"Fed W/H",ttc(1),tty(1)
63260   pr #255,using F_STUB_01: "Over Time",tdc2,ttc(27),"FICA W/H",ttc(2),tty(2)
63280   if uprc$(accr$)="N" then 
63300     pr #255,using F_STUB_01: "Sick",tdc3,tpd3,"State W/H",ttc(4),tty(4)
63320     pr #255,using F_STUB_01: "Vacation",tdc4,tpd4,abrevname$(1),ttc(5),tty(5)
63340   else 
63360     pr #255,using F_STUB_02: "Sick",em10,tdc3,tpd3,"State W/H",ttc(4),tty(4)
63380     pr #255,using F_STUB_02: "Vacation",em11,tdc4,tpd4,abrevname$(1),ttc(5),tty(5)
63400   end if 
63420   pr #255,using F_STUB_01: "Holiday",tdc5,tpd5,abrevname$(2),ttc(6),tty(6)
63440   pr #255,using F_STUB_01: "Other",0,ttc(28),abrevname$(3),ttc(7),tty(7)
63460   pr #255,using F_STUB_01: rt$,0,0,abrevname$(4),ttc(8),tty(8)
63480   if trim$(compcode$)="" then 
63500     pr #255,using F_STUB_01: "",0,0,abrevname$(5),ttc(9),tty(9)
63520   else 
63540     fn_extract_comp_time
63560     pr #255,using F_STUB_02: "Comp Time",balance,0,0,abrevname$(5),ttc(9),tty(9)
63580   end if 
63600   pr #255,using F_STUB_01: "YTD Pay",0,tty(31),abrevname$(6),ttc(10),tty(10)
63620   pr #255,using F_STUB_01: "YTD Deductions",0,tdedytd,abrevname$(7),ttc(11),tty(11)
63640   pr #255,using F_STUB_01: "Current Pay",0,ttc(31),abrevname$(8),ttc(12),tty(12)
63660   pr #255,using F_STUB_01: "Cur Deductions",0,tdedcp,abrevname$(9),ttc(13),tty(13)
63680   pr #255,using F_STUB_01: "Net Pay",0,ttc(32),"Other",ttc(14)+ttc(15)+ttc(16)+ttc(17)+ttc(18)+ttc(19)+ttc(20)+ttc(21)+ttc(22)+ttc(23)+ttc(24),tty(14)+tty(15)+tty(16)+tty(17)+tty(18)+tty(19)+tty(20)+tty(21)+tty(22)+tty(23)+tty(24)
63700   pr #255: ""
63720   pr #255: ""
63740   pr #255: ""
63760   pr #255: ""
63780 fnend 
64000 def fn_stub_billings
64020   let stub_one_or_two+=1
64040   if stub_one_or_two=3 then let stub_one_or_two=1
64060   if stub_one_or_two=2 then 
64070     pr #255: ""
64072     pr #255: ""
64080     pr #255: ""
64100     pr #255: ""
64120   end if 
64140   pr #255: ""
64160   pr #255,using 'form pos 3,c 30,n 8,x 2,c 13,pic(zz/zz/zz),n 10': em$(1),eno,"",prdmmddyy,check_number
64180   for j=1 to 20
64200     if tty(j+4)=0 then abrevname$(j)="" else abrevname$(j)=hnames$(j)
64220   next j
64240   pr #255,using 'form pos 60,c 7,pos 77,c 3': "Current","YTD"
64260   if uprc$(accr$)="N" then 
64280     pr #255,using 'form pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Hours","Pay","Med W/H",ttc(3),tty(3)
64300   else 
64320     pr #255,using 'form pos 14,c 7,pos 26,c 5,pos 37,c 3,pos 45,c 12,pic(-------.--),pic(-----------.--)': "Accrued","Hours","Pay","Med W/H",ttc(3),tty(3)
64340   end if 
64360   pr #255,using F_STUB_01: "Regular",tdc1,ttc(26),"Fed W/H",ttc(1),tty(1)
64380   pr #255,using F_STUB_01: "Over Time",tdc2,ttc(27),"FICA W/H",ttc(2),tty(2)
64400   if uprc$(accr$)="N" then 
64420     pr #255,using F_STUB_01: "Sick",tdc3,tpd3,"State W/H",ttc(4),tty(4)
64440     pr #255,using F_STUB_01: "Vacation",tdc4,tpd4,abrevname$(1),ttc(5),tty(5)
64460   else 
64480     pr #255,using F_STUB_02: "Sick",em10,tdc3,tpd3,"State W/H",ttc(4),tty(4)
64500     pr #255,using F_STUB_02: "Vacation",em11,tdc4,tpd4,abrevname$(1),ttc(5),tty(5)
64520   end if 
64540   pr #255,using F_STUB_01: "Holiday",tdc5,tpd5,abrevname$(2),ttc(6),tty(6)
64560   pr #255,using F_STUB_01: "",0,ttc(28),abrevname$(3),ttc(7),tty(7)
64580   pr #255,using F_STUB_01: rt$,0,0,abrevname$(4),ttc(8),tty(8)
64600   if trim$(compcode$)="" then 
64620     pr #255,using F_STUB_01: "",0,0,abrevname$(5),ttc(9),tty(9)
64640   else 
64660     fn_extract_comp_time
64680     pr #255,using F_STUB_02: "Comp Time",balance,0,0,abrevname$(5),ttc(9),tty(9)
64700   end if 
64720   pr #255,using F_STUB_01: "YTD Pay",0,tty(31),abrevname$(6),ttc(10),tty(10)
64740   pr #255,using F_STUB_01: "YTD Deductions",0,tdedytd,abrevname$(7),ttc(11),tty(11)
64760   pr #255,using F_STUB_01: "Current Pay",0,ttc(31),abrevname$(8),ttc(12),tty(12)
64780   pr #255,using F_STUB_01: "Cur Deductions",0,tdedcp,abrevname$(9),ttc(13),tty(13)
64800   pr #255,using F_STUB_01: "Net Pay",0,ttc(32),"",ttc(14)+ttc(15)+ttc(16)+ttc(17)+ttc(18)+ttc(19)+ttc(20)+ttc(21)+ttc(22)+ttc(23)+ttc(24),tty(14)+tty(15)+tty(16)+tty(17)+tty(18)+tty(19)+tty(20)+tty(21)+tty(22)+tty(23)+tty(24)
64820   pr #255: ""
64840   pr #255: ""
64850   if stub_one_or_two=1 then 
64860     pr #255: ""
64880     pr #255: ""
64890   end if
64900 fnend 
66000 def fn_stub_hitBoxes(mat ltext$,mat lPos)
66020   ! mat ltext$(lineCount,boxNumber)=textForBox$ (must be formatted)
66040   ! mat lPos(luneCount,BoxNumber)=Position of Box
66060   ! udim(mat ltext$,1) defines lineCount/length of stub
66080   dim hbLine$*256
66120   for hbLine=1 to udim(mat ltext$,1)
66130     hbLine$=rpt$(' ',256)
66140     for lposItem=1 to udim(mat lPos,2)
66160       hbLine$(lPos(hbLine,lposItem):(lPos(hbLine,lposItem)+len(lText$(hbLine,lposItem))-1))=ltext$(hbLine,lposItem)
66180     nex lposItem
66200     pr #255: rtrm$(hbLine$)
66220   nex hbLine
66240 fnend
67000 def fn_stub_energyExcnahger_extra(mat s,mat rpnames2$,mat dept)
67002   pr #255: ''
67010   for x=1 to 6
67020     ! if dept(x)=0 then 
67030       pr #255,using ees_L1510: rpnames2$(x+4),s(x,7),s(x,8)
67040       ees_L1510: form pos 54,c 10,pos 64,pic(-------.##),pos 74,pic(-------.##)
67050     ! else
67060     !   pr #255,using ees_L1480: dept(x),s(x,1),s(x,2),s(x,3),s(x,4),s(x,5),s(x,6),rpnames2$(x+4),s(x,7),s(x,8)
67070     !   ees_L1480: form pos 2,n 3,pos 6,5*n 7.2,n 11.2,x 2,c 10,2*n 10.2
67080     ! end if
67090   next x
67100   ! if s(7,6)<>0 then 
67110   !   pr #255,using ees_L1550: "OTHER",s(7,1),s(7,2),s(7,3),s(7,4),s(7,5),s(7,6),"TOTAL",s(7,7),s(7,8)
67120   !   ees_L1550: form pos 1,c 5,pos 6,pic(----.##),pos 14,pic(---.##),pos 21,pic(---.##),pos 28,pic(---.##),pos 35,pic(---.##),pos 42,pic(---,---.##),pos 54,c 5,pos 64,pic(-------.##),pos 74,pic(-------.##),skip 1
67130   ! end if
67140 fnend
70000 ! /r
72000 def fn_extract_comp_time
72020   balance=0
72040   let key$=lpad$(str$(eno),8)&"             "
72060   restore #breakdown,key>=key$: nokey EOBREAKDOWN
72080   READHOURBREAKDOWN: ! 
72100   read #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2",release: empno2,class$,tdate,increase,decrease eof EOBREAKDOWN ! kj 4/18/07
72120   if empno2<>eno then goto EOBREAKDOWN
72140   if trim$(class$)<>trim$(compcode$) then goto READHOURBREAKDOWN
72160   balance+=increase-decrease
72180   goto READHOURBREAKDOWN
72200   EOBREAKDOWN: ! 
72220 fnend 
74000 def fn_determine_earnings
74020   mat caf=(0): mat ttc=(0): mat ttdc=(0)
74040   mat tcp=(0): mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0)
74060   mat qtr4tcp=(0): mat ytdtotal=(0): mat tdc=(0): mat tty=(0)
74080   let fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0
74100   let medqtr=eicyr=eicqtr=wagesqtr=0
74120   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! indexed by employee#,department# and payroll date
74140   restore #3,key>=checkkey$: nokey L6920
74160   L6580: ! 
74180   read #3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,oldckno,mat tdc,mat tcp eof STORE_VARIABLES : let lastrec=rec(3)
74200   if heno<>eno then goto STORE_VARIABLES
74220   if prd<beg_date or prd>end_date then ! not this year
74230     goto L6580
74232   end if 
74240   if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
74260   if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
74280   if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
74300   if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
74320   mat ytdtotal=ytdtotal+tcp
74340   mat tty=tty+tcp
74360   if prd=d1 then mat ttc=ttc+tcp: mat ttdc=ttdc+tdc ! total for this check
74380   if prd=d1 then let fn_accumulate_dept_totals1(tdepXcount,mat tdep,tdn,rate)
74420   if env$('client')="Eldorado" then let fn_accumulate_dept_totals2
74430   if env$('client')="Energy Exchanger" then let fn_accumulate_dept_totals2
74440   if prd=d1 then rewrite #3,using "form pos 18,n 7",rec=lastrec: check_number
74460   goto L6580
74480   STORE_VARIABLES: ! 
74500   !   let wagesyr=ytdtotal(31) ! total wages
74520   let fedyr=ytdtotal(1) ! ytdl fed
74540   let ficayr=ytdtotal(2) ! fica year to date
74560   let medyr=ytdtotal(3) ! medicare year to date
74580   let stateyr=ytdtotal(4) ! total state  quarter
74600   let eicyr=ytdtotal(25) ! eic
74620   if prd>=qtr1 and prd<qtr2 then mat quartertotals=qtr1tcp
74640   if prd>=qtr2 and prd<qtr3 then mat quartertotals=qtr2tcp
74660   if prd>=qtr3 and prd<qtr4 then mat quartertotals=qtr3tcp
74680   if prd>=qtr4 and prd<end_date then mat quartertotals=qtr4tcp
74700   let wagesqtr=quartertotals(31) ! total wages quarter
74720   let fedqtr=quartertotals(1) ! total fed  quarter
74740   let ficaqtr=quartertotals(2) ! total fica quarter
74760   let medqtr=quartertotals(3) ! total medicare quarter
74780   let stateqtr=quartertotals(4) ! total state  quarter
74800   let eicqtr=quartertotals(25) ! EIC qtr
74820   !   for j=1 to 20
74840   !     if dedfed(j)=1 then let dedfedyr+=ytdtotal(j+4) ! deduct for federal wh
74860   !   next j
74880   L6920: ! 
74900 fnend 
76000 def fn_accumulate_dept_totals1(&tdepXcount,mat tdep,tdn,&rate) ! probably others too
76020   ! ACCUMULATE CURRENT INFO FROM EACH DEPARTMENT
76040   if tdepXcount<>0 then 
76060     for j2=1 to tdepXcount
76080       if tdep(j2,5)=tdn then goto adt_L1790
76100     next j2
76120   end if
76140   let tdepXcount=tdepXcount+1
76160   let j2=tdepXcount
76180   adt_L1790: ! 
76200   let tdep(j2,1)=tdep(j2,1)+tcp(31)-tcp(30) ! total wage less tips
76220   let deptgl$=""
76240   read #6,using "Form pos 12,c 12,pos 62,2*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",tdn): deptgl$,tdet(2),tdet(3) ! Nokey 1660
76260   let tdep(j2,2)=val(deptgl$(1:3)) ! salary for this department
76280   let tdep(j2,3)=val(deptgl$(4:9))
76300   let tdep(j2,4)=val(deptgl$(10:12))
76320   let tdep(j2,5)=tdn
76340   fn_fica_matching
76360   let tdep(j2,6)=ficam2+medic2 ! fica+match
76380   for j3=1 to 20
76400     let tdep(j2,j3+6)=tdep(j2,j3+6)+tcp(j3+4)
76420   next j3
76440   if s1=1 then 
76460     if rate=0 then let rate=tdet(2)
76480     if rate>0 then let rt$="PAY RATE"&cnvrt$("N 10.2",rate) else let rt$=""
76500   end if
76510   let tpd3=tpd3+round(tdc(3)*tdet(2),2) ! sick pay
76520   let tpd4=tpd4+round(tdc(4)*tdet(2),2) ! vacation pay
76540   let tpd5=tpd5+round(tdc(5)*tdet(2),2) ! if env$('client')="West Rest Haven" then let tpd5=tpd5+round(tdc(5)*(tdet(2)*1.5),2) else let tpd5=tpd5+round(tdc(5)*tdet(2),2)
76560   let tdc1=ttdc(1) ! Regular Hours
76580   let tdc2=ttdc(2) ! OverTime Hours
76600   let tdc3=ttdc(3)
76620   let tdc4=ttdc(4)
76640   let tdc5=ttdc(5)
76660   !   let ttdct=ttdc(1)+ttdc(2)+ttdc(3)+ttdc(4)+ttdc(5) ! Total Hours
76680 fnend 
78000 def fn_accumulate_dept_totals2
78020   for v1=1 to 6
78040     if tdn=deptsum(v1) then goto L8090 ! determine if dept # used on this employee already
78060     if deptsum(v1)=0 then let deptsum(v1)=tdn: goto L8080
78080   next v1
78100   if v1>6 then let v1=6 ! summarize any departments over 6 and the seventh row
78120   L8080: ! 
78140   if prd<>d1 then goto L8150
78160   L8090: ! 
78180   for r=1 to 5
78200     let v(v1,r)+=tdc(r)
78220     let v(7,r)+=tdc(r) ! total line
78240   next r
78260   let v(v1,6)+=tcp(31) ! pay
78280   let v(7,6)+=tcp(31) ! total pay line
78300   L8150: ! 
78320   for r=1 to 6
78340     if prd=d1 then 
78360       let v(r,7)+=tcp(r+8) ! last five misc deductions
78362     end if
78380     ! FILL TABLE S WITH YEAR TO DATE DEDUCTIONS
78400     let v(r,8)=v(r,8)+tcp(r+8)
78420   next r
78440   ! FILL TABLE S WITH CURRENT DEDUCTIONS
78460   for j=9 to 14
78480     if dedcode(j-3)=1 then goto L8270
78500     let v(7,8)=v(7,8)-tcp(j)
78520     if prd=d1 then let v(7,7)=+v(7,7)-tcp(j)
78540     goto L8290
78560     L8270: ! 
78580     let v(7,8)=v(7,8)+tcp(j)
78600     if prd=d1 then let v(7,7)=v(7,7)+tcp(j)
78620     L8290: !
78622   next j
78640   ! ROUTINE TO ACCUMULATE HOURS ETC. FOR SUMMARY
78660 fnend 
80000 def fn_fica_fix ! fix rounding problem on fica
80020   let fica3=fica0+medi0+fica1+medi1+fica2
80040   if fica3=0 then goto FICA_END
80060   read #h_cl_trans_alloc,using L5140,rec=fica_rec: bankcode,a1,tr1,gl$,alloc
80080   alloc=alloc-fica3
80100   rewrite #h_cl_trans_alloc,using L5140,rec=fica_rec: bankcode,a1,tr1,gl$,alloc
80120   FICA_END: let fica3=fica0=medi0=fica1=medi1=fica2=0
80140 fnend 
82000 def fn_fica_matching ! CALCULATE MATCHING FICA
82020   let ficawg=round(tcp(2)/ssr1,2) ! employee's fica rate
82040   let ficam2=round(ficawg*ssr2,2) ! employers fica rate
82060   let mediwg=tcp(3)/.0145 ! employee medicare rate
82080   let medic2=round(mediwg*.0145,2) ! employers medicare rate
82100   if fmeno=eno then goto SENO
82120   let fmeno=eno
82140   let ficam3=ficam2
82160   let medic3=medic2
82180   goto MFEND
82200   SENO: ! same employee
82220   let ficam3=ficam3+ficam2
82240   let medic3=medic3+medic2
82260   MFEND: ! 
82280 fnend 
84000 def fn_eldorado_check_and_stub
84020   ! meant for use at top and bottom margin at .5, left and right at .3, font size 9
84040   !  fn_accumulate_dept_totals2
84100   pr #255: ""
84120   pr #255: ""
84140   pr #255,using 'form pos 82,pic(zz/zz/zz)': dat
84160   pr #255: ""
84180   pr #255,using 'form pos 82,n 10': check_number
84200   pr #255: ""
84220   pr #255: ""
84240   pr #255: ""
84280   if ttc(32)<0 then let ttc(32)=0
84300   pr #255,using 'form pos 7,c 62,pos 85,pic($$$$$,$$$.##)': eng$(1:n),ttc(32)
84340   pr #255,using X7660: eng$(n+1:128)
84360   X7660: form pos 9,c 70,skip 3
84380   pr #255,using X7700: em$(1)
84400   pr #255,using X7700: em$(2)
84420   pr #255,using X7701: em$(3) : goto PRINTSTUB_ELDORADO
84440   X7700: form pos 10,c 30
84460   X7701: form pos 10,c 30,skip 11
84480   pr #255,using X7720: eno,d1,rate,tdc1,tdc(2),tcp(26),tcp(27),tcp(28),dat
84500   ! pr #255,using X7720: eno,d1,rate,tdc1,tdc2,ttc(16),ttc(17),ttc(18),dat
84520   X7720: form skip 11,pos 1,n 6,x 4,pic(zzzz/zz/zz),2*n 9.2,n 9.2,n 12.2,n 12.2,n 12.2,pos 95,pic(zz/zz/zz)
84540   pr #255: ''
84560   pr #255: ''
84580   pr #255,using X7740: abrevname$(1),abrevname$(2),abrevname$(3),abrevname$(4)
84600   X7740: form pos 44,4*c 9
84620   pr #255: ""
84640   pr #255,using X7760: ttc(31),ttc(1),ttc(3),ttc(2)+ttc(4),ttc(5),ttc(6),ttc(7),ttc(8),0,ttc(32)
84660   X7760: form pos 1,2*n 9.2,5*n 9.2,n 8.2,n 8.2,pos 93,pic($$$,$$$.##)
84680   pr #255: ""
84700   pr #255,using X7780: ytdtotal(31),ytdtotal(1),ytdtotal(3),ytdtotal(2)+ytdtotal(4),ytdtotal(5),ytdtotal(6),ytdtotal(7),ytdtotal(8),0 ! ,YTDTOTAL(9)
84720   X7780: form pos 1,7*n 10.2,n 8.2,n 8.2
84740   pr #255: ""
84760   pr #255: ""
84780   pr #255: ""
84800   pr #255: ""
84820   for x=1 to 6
84840     if deptsum(x)=0 then 
84860       pr #255,using X7850: abrevname$(x+4),v(x,7),v(x,8)
84880       X7850:  form pos 60,c 8,x 2,pic(-----------.##),x 5,pic(-----------.##)
84900     else 
84920       pr #255,using X7820: deptsum(x),v(x,1),v(x,2),v(x,3),v(x,4),v(x,5),v(x,6),abrevname$(x+4),v(x,7),v(x,8)
84940       X7820:  form pos 1,g 3,x 1,4*n 8.2,n 9.2,n 12.2,x 2,c 8,x 7,n 9.2,x 10,n 9.2
84960     end if 
84980   next x
85000   if v(7,6)=0 then 
85020     pr #255,using X7850: "Total",v(7,7),v(7,8)
85040   else 
85060     pr #255,using X7820: "",v(7,1),v(7,2),v(7,3),v(7,4),v(7,5),v(7,6),"Total",v(7,7),v(7,8)
85080   end if 
85100   ! Let TDC1=TDC2=0
85120   let rate=0
85140   mat v=(0)
85160   for k=1 to 6
85180     let deptsum(k)=0
85200   next k
85220   if check_number>0 then check_number=check_number+1
85240   PRINTSTUB_ELDORADO: pr #255: ""
85260   fn_print_stub
85280 fnend 
88000 INVALIDGLNUMBER: ! r:
88020   fntos(sn$="Prckprt3")
88040   let mylen=30 : let mypos=mylen+2
88060   fnlbl(1,1,"Employee Number:",mylen,1)
88080   fntxt(1,mypos,10, 0,0,'',1)
88100   let resp$(1)=str$(eno)
88120 ! 
88140   fnlbl(2,1,"Department Number:",mylen,1)
88160   fntxt(2,mypos,10, 0,0,'',1)
88180   let resp$(2)=str$(tdn)
88200 ! 
88220   fnlbl(4,1,"Invalid General Ledger Number:",mylen,1)
88240   fntxt(4,mypos,12, 0,0,'',1)
88260   let resp$(3)=gl$
88280 ! 
88300   fnlbl(5,1,"Purpose for GL Number:",mylen,1)
88320   fntxt(5,mypos,40, 0,0,'',1)
88340   let resp$(4)=sd5$
88360 ! 
88380   fnlbl(7,1,"The General Ledger Number is invalid.",40,0)
88400   fnlbl(8,1,"Please select the correct one.",40,0)
88420   fnlbl(3,1,"Correct General Ledger Number:",mylen,1)
88440   fnqgl(3,mypos,0,2,pas)
88460   let resp$(5)=fnrgl$(goodgl$)
88480   fncmdkey("&Next",1,1,0,"Continue with checkprinting." )
88500   fncmdkey("E&xit",5,0,1,"Returns to menu")
88520   fnacs(sn$,0,mat resp$,ckey) ! bad general ledger numbers
88540   if ckey=5 then goto XIT
88560   let gl$=fnagl$(resp$(5))
88600   read #h_cl_glmstr,using F_CL_GLMSTR,key=gl$,release: de$ nokey INVALIDGLNUMBER
88620   on cd1 goto BCR_GLN_VALIDATED,EXLNKD_L5920 none BCR_GLN_VALIDATED
88640 ! /r
