06000 ! formerly S:\acsPR\newprCkPrt
06020 ! pr Payroll Checks ! Nebs 9039t: Standard Check Format (Laser Stub-Check-Stub)
06040 ! r: library and on error
06060   library 'S:\Core\Library': fntop,fnxit, fnerror,fnGetPayrollDates
06061   library 'S:\Core\Library': fnopenprn,fncloseprn,fnchain,fnTos,fnLbl
06062   library 'S:\Core\Library': fncomboa,fnTxt,fncombof,fnCmdSet,fnAcs,fnmsgbox
06063   library 'S:\Core\Library': fndate_mmddyy_to_ccyymmdd,fnOpt,fnqgl,fnrgl$
06064   library 'S:\Core\Library': fnCmdKey,fnagl$,fnButton,fnss_employee,fnss_employer
06065   library 'S:\Core\Library': fncd,fnclient_has,fnreg_read,fnreg_write
06066   library 'S:\Core\Library': fngethandle,fncreg_read,fncreg_write,fnDedNames
06080   on error goto ERTN
06090 ! /r
06100   fntop(program$)
06160   if env$('client')="Divernon" then print_netzero_checks=1
06180   if env$('client')="West Accounting" or env$('client')="Payroll Done Right" then print_netzero_checks=1
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
08280   dim dedfica(20),dedSt(20),dedUc(20),gl$(20)*12
08300   dim dtr$(5)*35,key$*21
08320   dim dept(6),bankgl$*12,gl$*12,bn$*30,text$*90,v(7,8),deptsum(6)
08340   dim hnames$(20)*8
08360   dim eng$*128,wording$(27)*9,amount(11)
08380 ! 
08400   dim opt_check_format$(6)*20, scc$(6)
08410   opt_check_format$(1)="Check, Stub"        : scc$(1)="CS"
08420   opt_check_format$(2)="Stub, Check"        : scc$(2)="SC"
08430   opt_check_format$(3)="Stub, Check, Stub"  : scc$(3)="SCS"
08440   opt_check_format$(4)="Check, Stub, Stub"  : scc$(4)="CSS"
08450   opt_check_format$(5)="Stub, Stub, Check"  : scc$(5)="SSC"
08460   opt_check_format$(6)="Stub, Check, Check" : scc$(6)="SCC"
08500 ! 
08520   dim opt_check_type$(3)*14
08540   opt_check_type$(1)="Regular Check"
08560   opt_check_type$(2)="Direct Deposit"
08580   opt_check_type$(3)="All"
08600 ! r: set mat wording$
08620   wc=0 ! Wording$ counter
08640   wording$(wc+=1)='One'
08660   wording$(wc+=1)='Two'
08680   wording$(wc+=1)='Three'
08700   wording$(wc+=1)='Four'
08720   wording$(wc+=1)='Five'
08740   wording$(wc+=1)='Six'
08760   wording$(wc+=1)='Seven'
08780   wording$(wc+=1)='Eight'
08800   wording$(wc+=1)='Nine'
08820   wording$(wc+=1)='Ten'
08840   wording$(wc+=1)='Eleven'
08860   wording$(wc+=1)='Twelve'
08880   wording$(wc+=1)='Thirteen'
08900   wording$(wc+=1)='Fourteen'
08920   wording$(wc+=1)='Fifteen'
08940   wording$(wc+=1)='Sixteen'
08960   wording$(wc+=1)='Seventeen'
08980   wording$(wc+=1)='Eighteen'
09000   wording$(wc+=1)='Nineteen'
09020   wording$(wc+=1)='Twenty'
09040   wording$(wc+=1)='Thirty'
09060   wording$(wc+=1)='Forty'
09080   wording$(wc+=1)='Fifty'
09100   wording$(wc+=1)='Sixty'
09120   wording$(wc+=1)='Seventy'
09140   wording$(wc+=1)='Eighty'
09160   wording$(wc+=1)='Ninety'
09180 ! /r
09181   dim opt_yn$(2)*4
09182   opt_yn$(1)="Yes"
09184   opt_yn$(2)="No"
09185 ! 
09200 ! /r
10000 ! r: set default answers and semi-consants and open some files
10020   ssr1=fnss_employee*.01
10040   ssr2=fnss_employer*.01
10060   open #20: "Name="&env$('Q')&"\PRmstr\prCode.h"&env$('cno')&",Shr",internal,input 
10080   read #20,using 'Form POS 2,POS 5,N 5': ckno
10100   close #20: 
10120   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
10140   fncreg_read('Prenumbered Checks',pre$)
10160   fncreg_read('Post to CL',acsclcv$)
10180   fncreg_read('Post Employer Portion of FiCA',ficam1$)
10200   fncreg_read('Check Format',sc1$)
10220   fncreg_read('Print Vacation and Sick Leave on Check',accr$)
10240   fncreg_read('CL Bank Code',bankcode$) : bankcode=val(bankcode$) : if bankcode=0 then bankcode=1
10260   fncreg_read('Comp Time Code',compcode$)
10280   fnDedNames(mat fullname$,mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedSt,mat dedUc,mat gl$)
10300   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
10320   read #20,using 'Form POS 1,x 120,POS 150,10*C 8,POS 437,15*C 12,N 1': mat d$,mat gln$,gl_installed
10340   close #20: 
10360 ! ___________________________
10380   mat hnames$=abrevname$ : bankgl$=gln$(15)
10400   if fnclient_has('CL') then let fn_open_acscl
10420 ! ___________________________
10440   if bankcode=0 then bankcode=1
10460   check_number=ckno
10480 ! if env$('client')="West Rest Haven" then sc1$="C"
10500   if env$('client')="Billings" then sc1$="CSS"
10520   if env$('client')="Divernon" or env$('client')="Thomasboro" or env$('client')="Edinburg" or env$('client')="Philo" or env$('client')="Hope Welty" or env$('client')="Monticello" then ficam1$="Y"
10540   ddcode$="R"
10560   fnreg_read('PR.Check print.skip alignment',skip_alignment$) : if skip_alignment$='' then skip_alignment$='No'
10580   goto MAIN_QUESTIONS ! /r
11000 MAIN_QUESTIONS: ! r:
11004   fnTos(sn$="prckprt")
11008   respc=0
11012   fnLbl(1,1,"Payroll Date:",38,1)
11016   fnTxt(1,41,10,0,1,"3",0,"")
11020   resp$(resp_payroll_date:=1)=str$(d1)
11024   fnLbl(2,1,"Are Checks Prenumbered?",38,1)
11028   fncomboa("prckprt-2",2,41,mat opt_yn$,"The system needs to know if the checks are already numbered.",3)
11032   if pre$="Y" then resp$(2)=opt_yn$(1) else resp$(2)=opt_yn$(2)
11036   fnLbl(3,1,"Beginning Check Number:",38,1)
11040   fnTxt(3,41,7,0,1,"30",0,"")
11044   resp$(3)=str$(check_number)
11048   fnLbl(4,1,"Date of Checks:",38,1)
11052   fnTxt(4,41,10,0,1,"3",0,"")
11056   resp$(resp_date_of_checks:=4)=date$("ccYYMMDD")
11060   fnLbl(5,1,"Beginning Employee Number:",38,1)
11064   fnTxt(5,41,8,0,1,"30",0,"")
11068   resp$(5)=str$(beginningEmployeeNumber)
11072   fnLbl(6,1,"Post to ACS Checkbook",38,1)
11076   if fnclient_has('CL') then 
11080     fncomboa("prckprt-3",6,41,mat opt_yn$)
11084     if acsclcv$="Y" then resp$(6)=opt_yn$(1) else resp$(6)=opt_yn$(2)
11088   else 
11092     fnTxt(6,41,3, 0,0,'',1,'ACS Checkbook license not detected.')
11096     resp$(6)=opt_yn$(2) : acsclcv$='N'
11100   end if 
11104   fnLbl(7,1,"Post Employer's Portion of FiCA?",38,1)
11108   fncomboa("prckprt-4",7,41,mat opt_yn$,"The system can generate and post the employer's portion of FICA at the time the check is being written.",3)
11112   if ficam1$="Y" then resp$(7)=opt_yn$(1) else resp$(7)=opt_yn$(2)
11116   fnLbl(8,1,"Check Format:",38,1)
11120   fncomboa("ckprt-2",8,41,mat opt_check_format$)
11124   whichScc=srch(mat scc$,sc1$)
11128   if whichScc>0 then resp$(8)=opt_check_format$(whichScc) else resp$(8)=opt_check_format$(4)
11132   fnLbl(9,1,"Check Type (Regular or Direct Deposit):",38,1)
11136   fncomboa("ckprt-5",9,41,mat opt_check_type$,"If you have direct deposits, you can use this option to pr check on plain paper to give the employees.",15)
11140   if ddcode$="R" then resp$(9)=opt_check_type$(1)
11144   if ddcode$="D" then resp$(9)=opt_check_type$(2)
11148   if ddcode$="A" then resp$(9)=opt_check_type$(3)
11152   fnLbl(10,1,"Print Vacation and Sick Leave?",38,1)
11156   fncomboa("prckprt-6",10,41,mat opt_yn$)
11160   if accr$="Y" then resp$(10)=opt_yn$(1) else resp$(10)=opt_yn$(2)
11164 ! 
11168   respc=10
11172 ! 
11176   if cl_installed=0 and exists(env$('Q')&"\CLmstr\bankmstr.h"&env$('cno')) then 
11180     fnLbl(11,1,"Bank Account:",38,1)
11184     fncombof("Bankmstr",11,41,20,env$('Q')&"\CLmstr\bankmstr.h"&env$('cno'),1,2,3,15,env$('Q')&"\CLmstr\Bankidx1.h"&env$('cno'),1,0, "Select bank account for printing")
11188     resp$(resp_cl_bankcode:=respc+=1)=str$(bankcode)
11192   end if 
11196   if exists(env$('Q')&"\PRmstr\hourclass.h"&env$('cno')) then 
11200     fnLbl(12,1,"Comp Time Code:",38,1)
11204     fncombof("timeclass",12,41,20,env$('Q')&"\PRmstr\hourclass.h"&env$('cno'),1,5,6,25,env$('Q')&"\PRmstr\hourclass-idx.h"&env$('cno'),1,0, "Select time classification code for comp time, if applicable.")
11208     resp$(resp_combcode:=respc+=1)=compcode$
11212   end if 
11216   fnLbl(14,1,"Print All Checks (or ask after first):",38,1)
11220   fncomboa("prckprt-prall",14,41,mat opt_yn$)
11224   resp$(resp_skip_align=respc+=1)=skip_alignment$
11228   if gl_installed then 
11232     fnLbl(16,1,"General Ledger detected.",38,1)
11236   end if 
11240   if cl_installed then 
11244     fnLbl(17,1,"Checkbook detected.",38,1)
11248   end if 
11260   fncmdkey('Test Check Format',ck_TestCheck:=21)
11265   fnCmdSet(2) ! need button to show totals
11268   fnAcs(sn$,0,mat resp$,ck)
11272   if ck=5 then goto XIT ! /r
11276   if ck=ck_TestCheck then testCheckFormat=1 else testCheckFormat=0
11332 ! r: validate answers (and move to local variables from mat resp$)
11340   d1                      =val(resp$(resp_payroll_date))             ! payroll date
11350   pre$                    =uprc$(resp$(2)(1:1))                     ! pre-numbered checks Y or N
11360   check_number            =val(resp$(3))                             ! check #
11370   ckdat$                  =resp$(resp_date_of_checks)                ! check date
11380   dat                     =val(ckdat$(5:6)&ckdat$(7:8)&ckdat$(3:4))
11390   beginningEmployeeNumber =val(resp$(5))                             ! beginning employee #
11400   acsclcv$                =uprc$(resp$(6)(1:1))                     ! post Checkbook system
11410   ficam1$                 =uprc$(resp$(7)(1:1))                     ! post fica match
11430   sc1$                    =scc$(srch(mat opt_check_format$,resp$(8)))
11460   ddcode$                 =uprc$(resp$(9)(1:1))                     ! regular check or direct deposit
11470   accr$                   =uprc$(resp$(10)(1:1))                    ! pr vac and sick
11472   if resp_cl_bankcode then 
11480     bankcode              =val(resp$(resp_cl_bankcode)(1:3))        ! bank code
11481   end if 
11482   if resp_combcode then 
11490     compcode$             =resp$(resp_combcode)(1:5)                 ! comp time code
11492   end if 
11496 ! date_of_checks=val(ckdat$)
11500   prdmmddyy=val(ckdat$(5:6))*10000+val(ckdat$(7:8))*100+val(ckdat$(3:4)) ! convert date back to mmddyy format
11506   skip_alignment$         =resp$(resp_skip_align)
11507   if skip_alignment$='Yes' then allign=3
11508 ! 
11510   if acsclcv$="Y" then cl_installed=1 else cl_installed=0
11520   if ficam1$="Y" then ficam1=1 else ficam1=0
11530   if pre$="Y" then pre=1 else pre=0
11532 ! if env$('client')="Washington Parrish" and (prdate<10100 or prdate>123199) then goto MAIN_QUESTIONS
11540   if d1<beg_date or d1>end_date then ! not this year
11545     mat ml$(4)
11550     ml$(1)='The Payroll Date you have choosen ('&cnvrt$('pic(zzzz/zz/zz)',d1)&') is is outside your years'
11555     ml$(2)='beginning  and ending date range ('&cnvrt$('pic(zzzz/zz/zz)',beg_date)&' - '&cnvrt$('pic(zzzz/zz/zz)',end_date)&').'
11560     ml$(3)='Checks with a payroll date outside this date range can not be processed.'
11565     ml$(4)='Would you like to "Change Payroll Dates" now?'
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
11625     ml$(1)="You must enter a valid check number!"
11630     ml$(2)="Click OK to return to previous screen. "
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
11678   if testCheckFormat then goto L1300
11680   if cl_installed=1 then 
11685     read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi nokey L1280 ioerr L1290
11690   end if 
11695   goto L1300
11700   L1280: ! 
11705   mat ml$(2)
11710   ml$(1)="You must enter a valid bank code!"
11715   ml$(2)="Click OK to return to previous screen. "
11720   fnmsgbox(mat ml$,resp$)
11725   goto MAIN_QUESTIONS
11730   L1290: ! 
11735   mat ml$(3)
11740   ml$(1)="You have indicated that you want to post checkbook, "
11745   ml$(2)="but no checkbook files can be found! "
11750   ml$(3)="Click OK to return to previous screen. "
11755   fnmsgbox(mat ml$,resp$)
11760   goto MAIN_QUESTIONS
17000 L1300: ! /r
17010 ! if env$('client')<>"Washington Parrish" then prdate=d1
17020   if ~testCheckFormat then
17030     if cl_installed then 
17040       open #7: "Name="&env$('Q')&"\PRmstr\MGLMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\MGLIDX1.h"&env$('cno')&",Shr",internal,input,keyed 
17050     end if 
17060     open #praddr:=1: "Name="&env$('Q')&"\PRmstr\prAddr1.h"&env$('cno')&",Shr",internal,input 
17070     open #hEmployee:=fngethandle: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpindex.h"&env$('cno')&",Shr",internal,input,keyed 
17080     open #hDepartment:=fngethandle: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno'),internal,outIn,keyed 
17090     open #3: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outIn,keyed 
17100     open #breakdown=31: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&env$('cno'),internal,outIn,keyed 
17110     open #dd=30: "Name="&env$('Q')&"\PRmstr\DD.h"&env$('cno')&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&env$('cno')&",Shr,kps=1,kln=10,Use",internal,outIn,keyed 
17120     if fnclient_has('GL') and gl_installed=1 then 
17130       gl_installed=0
17140       open #h_gl_glbrec:=fngethandle: "Name="&env$('Q')&"\GLmstr\GLBREC.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLRECIDX.h"&env$('cno')&",Shr",internal,outIn,keyed ioerr L1440
17150       gl_installed=1
17160       L1440: ! 
17170     end if 
17180   end if
17190   MAIN_LOOP_TOP: ! 
17200   s1=1
17210   ReadNextEmployee: ! 
17220     if testCheckFormat then
17290       fn_getTestValues
17292       goto L1570
17340     else
17350       read #hEmployee,using 'form pos 1,n 8,3*c 30,pos 132,2*pd 4.2,pos 162,n 6': eno,mat em$,em10,em11,lpd eof EO_RPMSTR
17360     end if
17370     mat v=(0) : v1=1
17380     ! If env$('client')="WashingtonParrish" Then Goto 1110
17390     dd$=""
17400     read #dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17",key=rpad$(str$(eno),10): key$,dd$,rtn,acc,acn nokey ignore
17410     if uprc$(dd$)="Y" and ddcode$="D" then goto L1570
17420     if uprc$(dd$)<>"Y" and ddcode$="R" then goto L1570
17430     if ddcode$="A" then goto L1570 ! all
17440   goto ReadNextEmployee
17450   L1570: !
17460   if beginningEmployeeNumber>eno then goto ReadNextEmployee ! start with certain employee
17470   tdepXcount=0
17480   mat tdep=(0)
17490   tdc1=tdc2=tdc3=tdc4=tdc5=0
17500   tpd3=tpd4=tpd5=0
17510   tdct=0
17520   rate=0
17530   s1=1
17540   ! If fndate_mmddyy_to_ccyymmdd(LPD)><D1 Then Goto 1360  ! with comment can reprint any payroll
17550   if ~testCheckFormat then 
17552     fn_determine_earnings
17554   end if
17560   if print_netzero_checks and ttc(32)=0 and fndate_mmddyy_to_ccyymmdd(lpd)=d1 then goto ProduceThatCheck ! pr zero checks
17570   if ttc(32)=0 then goto ReadNextEmployee ! no earnings
17580   ProduceThatCheck: ! 
17590   ! Mat TCP=(0)
17600   ! Mat TDC=(0)
17610   goto PRE_CHECK
17620 ! ______________________________________________________________________
18000 PRE_CHECK: ! 
18020   ttc(26)=ttc(26)-tpd3-tpd4-tpd5
18040   ttc(28)=ttc(28)+ttc(29)+ttc(30) ! OTHER COMP-CURRENT
18060   ttc(1)=ttc(1)-ttc(25) : tty(1)=tty(1)-tty(25)
18080 L2070: ! 
18100   if cl_installed=1 then let fn_cknum
18120   fnopenprn
18140   if env$('client')="Eldorado" then let fn_eldorado_check_and_stub : goto L2150
18160   if sc1$="SCS" then let fn_print_stub : fn_print_check : fn_print_stub
18180   if sc1$="CSS" then let fn_print_check : fn_print_stub : fn_print_stub
18200   if sc1$="SSC" then let fn_print_stub : fn_print_stub : fn_print_check
18220   if sc1$="SCC" then let fn_print_stub : fn_print_check : fn_print_check
18240   if sc1$="CS" then let fn_print_check : fn_print_stub
18260   if sc1$="SC" then let fn_print_stub : fn_print_check
18280 L2150: ! 
18300   if fp(d1*.01)>.9 then 
18320     hd1=19000000+fncd(d1)
18340   else 
18360     hd1=20000000+fncd(d1)
18380   end if 
18400 ! hsk$=lpad$(str$(eno),8)&cnvrt$("PD 6",hd1)
18420   if ~testCheckFormat and (allign=3 or skip_alignment$='Yes') then 
18440     pr #255: chr$(12) ! NEWPAGE
18460     goto CHECK_PRINT_TOP
18480   end if 
18500   fncloseprn
18510   if testCheckFormat then let fnChain(program$)
18520   goto ALLIGNMENT
18540 ! ______________________________________________________________________
19000 ALLIGNMENT: ! r:
19020   fnTos(sn$="prckprt2")
19040   respc=0 : rc=0
19060   fnOpt(1,3,"Reprint same check",0,franum)
19080   resp$(rc+=1)="False"
19100   fnOpt(2,3,"Print next",0,franum)
19120   resp$(rc+=1)="False"
19140   fnOpt(3,3,"Print all remaining",0,franum)
19160   resp$(rc+=1)="True"
19180   if env$('client')='Billings' then resp$(2)='True' : resp$(3)='False'
19200   fnCmdSet(2)
19220   fnAcs(sn$,0,mat resp$,ck) ! allignment
19240   if resp$(1)="True" then allign=1
19260   if resp$(2)="True" then allign=2
19280   if resp$(3)="True" then allign=3
19300   if ck=5 then getout=1: allign=2: goto CHECK_PRINT_TOP ! write history on last check and quit
19320   on allign goto REPRINT_SAME_CHECK,CHECK_PRINT_TOP,CHECK_PRINT_TOP none ALLIGNMENT
19340 ! /r
19360 REPRINT_SAME_CHECK: ! 
19380   if pre=0 then goto L3130
19400   if cl_installed=1 then let fn_build_check_record
19420   check_number=check_number+1
19440 L3130: ! 
19460   goto L2070
19480 ! ______________________________________________________________________
20000 CHECK_PRINT_TOP: ! 
20010   if cl_installed=1 then let fn_build_check_record
20020   tdc1=0
20030   tdc2=0
20040   tdc3=tdc4=tdc5=0
20050   tpd3=tpd4=tpd5=0
20060   tdct=0
20070   rate=0
20080   ttc(32)
20090   mat dept=(0)
20100   if gl_installed=1 then ! r: update GL's GLBREC
20110     read #h_gl_glbrec,using 'form pos 1,c 12',key=bankgl$&lpad$(rtrm$(str$(check_number)),12): gl$ nokey L3300
20120   else 
20130     goto L3320
20140   end if 
20150   rewrite #h_gl_glbrec,using F_GL_GLBREC: bankgl$,lpad$(rtrm$(str$(check_number)),12),em$(1),"PR",dat,ttc22,0
20160   goto L3320
20170 ! 
20180 L3300: ! 
20190   write #h_gl_glbrec,using F_GL_GLBREC: bankgl$,lpad$(rtrm$(str$(check_number)),12),em$(1),"PR",dat,ttc22,0
20200   F_GL_GLBREC: form pos 1,2*c 12,c 30,c 2,n 6,pd 5.2,n 1
20210 L3320: ! /r
20220   if getout=1 then goto FINIS
20230   ttc(32)=0
20240   check_number=check_number+1
20250 goto MAIN_LOOP_TOP
22000 def fn_EnglishAmount(dol,mat eng$; n1)
22010  ! pass amount in dol, returned in mat eng$
22020  ! n1 = break point (58 is default)
22030  ! n2  used to hardcoded to 58, let's try setting it to n1 instead 12/29/2017
22040  !
22050   if n1=0 then let n1=58
22060   n2=n1
22070   dol=ttc(32)
22080   if dol<=0 then 
22090     eng$="*** VOID ***"
22100     goto L3810
22110   else if dol=>10**8 then 
22120     eng$="Value too big for editing"
22130     goto L3810
22140   end if 
22150   eng$="***"
22160   amount(1)=int(dol*100+.500000001)
22170   for a0=2 to 10
22180     amount(a0)=int(amount(a0-1)/10+.000000001)
22190   next a0
22200   for a0=1 to 10
22210     amount(a0)=amount(a0)-amount(a0+1)*10
22220   next a0
22230   if amount(11)+amount(10)+amount(9)=0 then goto L3530
22240   a0=9
22250   fn_engDol_hundred
22260   eng$=rtrm$(eng$)&" Million"
22270   L3530: ! 
22280   if amount(8)+amount(7)+amount(6)=0 then goto L3570
22290   a0=6
22300   fn_engDol_hundred
22310   eng$=rtrm$(eng$)&" Thousand"
22320   L3570: if amount(5)+amount(4)+amount(3)=0 then goto L3600
22330   a0=3
22340   fn_engDol_hundred
22350   L3600: ! 
22360   if dol>=1 then goto L3620
22370   eng$=rtrm$(eng$)&" Zero"
22380   L3620: ! 
22390   eng$=rtrm$(eng$)&" Dollar"
22400   if dol<2 and dol>=1 then goto L3660
22410   eng$=rtrm$(eng$)&"s"
22420   if len(rtrm$(eng$))>n2 then goto L3660
22430   L3660: ! 
22440   eng$=rtrm$(eng$)&" and"
22450   if amount(2)+amount(1)=0 then 
22460     eng$=rtrm$(eng$)&" Zero" 
22470   else
22480     amount(3)=0
22490     a0=1
22500     fn_engDol_hundred
22510   end if
22520   eng$=rtrm$(eng$)&" Cent"
22530   if abs(dol-int(dol+.000000001)-.01)<.001 then goto L3760
22540   eng$=rtrm$(eng$)&"s"
22550   L3760: ! 
22560   if len(rtrm$(eng$))<n2 then goto L3810
22570   for j=1 to 9
22580     n1=(n2+1)-j
22590     if eng$(n1:n1)=" " then goto L3810
22600   next j
22610   L3810: ! 
22612   fn_EnglishAmount=n1
22620 fnend
23000 def fn_engDol_hundred
23020   if amount(a0+2)<>0 then 
23040     eng$=rtrm$(eng$)&" "&wording$(amount(a0+2))
23060     eng$=rtrm$(eng$)&" Hundred"
23080   end if
23100   if amount(a0+1)=0 and amount(a0)=0 then goto L3920
23120   if amount(a0+1)<2 then goto L3910
23140   eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)+18)
23160   if amount(a0)=0 then goto L3920
23180   amount(a0+1)=0
23200   L3910: ! 
23220   eng$=rtrm$(eng$)&" "&wording$(amount(a0+1)*10+amount(a0))
23240   L3920: ! 
23260 fnend 
24000 EO_RPMSTR: ! r:
24020   close #hEmployee: 
24040   close #3: 
24060   if gl_installed=1 then close #h_gl_glbrec: 
24080   fncloseprn
24100   goto FINIS ! /r
24120 FINIS: ! 
24140   if cl_installed=1 and allign=4 then let fn_build_check_record
24160 XIT: fnxit
25000 def fn_open_acscl
25020   open #h_cl_bank:=12: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno')&",Shr",internal,outIn,keyed ioerr L4220
25040   cl_installed=1
25060   open #15: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,outIn,relative 
25080   open #h_cl_payee:=fngethandle: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&env$('cno')&",Shr",internal,outIn,keyed 
25100   open #14: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&env$('cno')&",Shr",internal,outIn,keyed 
25120   open #h_cl_trans:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outIn,keyed 
25140   open #22: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outIn,keyed 
25160   !   if exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then
25180   open #h_cl_trans_alloc:=fngethandle: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",Version=2,KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&env$('cno')&",Shr",internal,outIn,keyed 
25200   !   else 
25220   !     open #h_cl_trans_alloc:=f: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",Shr",internal,outIn,relative
25240   !   end if
25260   open #h_cl_glmstr:=fngethandle: "Name="&env$('Q')&"\CLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\GLINDEX.h"&env$('cno')&",Shr",internal,outIn,keyed 
25280   read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,ckno nokey L4200
25300   L4200: ckno=ckno+1
25320   ! dAT=VAL(DATE$(4:5)&DATE$(7:8)&DATE$(1:2))
25340   L4220: ! 
25360 fnend 
27000 def fn_build_check_record
27010   tr$(1)=cnvrt$("N 8",check_number)
27020   tr$(2)=cnvrt$("N 6",dat)
27030   tr$(3)=cnvrt$("N 10.2",ttc(32))
27040   tr$(4)=cnvrt$("N 8",eno)
27050   tr$(5)=em$(1)
27060   if allign>1 then 
27070     ded$(1)=str$(ttc(31))
27080     for j=2 to 25
27090       if j=3 then ded$(j)=str$(ttc(2)) : goto L4390
27100       ! ADD MEDICARE TO FICA  no kj
27110       ded$(j)=str$(ttc(j-1))
27120     L4390: !
27130     next j
27140     ! dED$(25)=STR$(TTC(29)) ! meals
27150     ! dED$(26)=STR$(TTC(30)) ! tips
27160     ! dED$(27)=STR$(TDC1) ! reg hourly rate
27170     ! dED$(28)=STR$(TDC2) ! ot rate
27180     if tdc1=0 then goto L4480
27190     ! dED$(29)=STR$(INT(TDC1/40+.99))
27200   else
27210     tr$(3)=tr$(4)=""
27220     tr$(5)="VOID"
27230     mat ded$=("")
27240   end if
27250   goto WRITE_PAYROLL_TRANS
27260   ! ______________________________________________________________________
27270   L4480: ! If EM(5)=1 Then dED$(29)="4" ! weeks worked
27280   if em(5)=2 then ded$(29)="2"
27290   if em(5)=3 then ded$(29)="2"
27300   if em(5)=4 then ded$(29)="1"
27310   ! ______________________________________________________________________
27320   WRITE_PAYROLL_TRANS: ! 
27330   if testCheckFormat then goto L5250
27340   ! pr tr$(1),tr$(5) :  pause
27350   mat tr=(0)
27360   ! r: removed existing CL Check (and it's allocations) first
27370   clk$=lpad$(str$(bankcode),2)&"1"&tr$(1)
27380   read #h_cl_trans,using 'form pos 79,2*pd 3',key=clk$: nt1 nokey L4610
27390   delete #h_cl_trans,key=clk$: 
27400   key$=lpad$(str$(bankcode),2)&str$(tcde)&rpad$(tr$(1),8)
27410   restore #h_cl_trans_alloc,key>=key$: nokey L4610
27420   L4590: ! 
27430   read #h_cl_trans_alloc,using 'Form Pos 1,C 11': newkey$ eof L4610
27440   if newkey$=key$ then 
27450     delete #h_cl_trans_alloc: 
27460     goto L4590
27470   end if 
27480   L4610: ! 
27490   ! /r
27500   !   if exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then
27510   tx3=val(tr$(3))
27520   tr2=val(tr$(2))
27530   write #h_cl_trans,using F_CL_TRANS_V1: bankcode,1,tr$(1),tr2,tx3,tr$(4),tr$(5),0,clr,4
27540   read #h_cl_payee,using 'form pos 129,pd 5.2',key=lpad$(rtrm$(tr$(4)),8): ytdp nokey L4690 ! UPDATE PAYEE FILE
27550   ytdp=ytdp+val(tr$(3)) conv ignore
27560   L4690: ! 
27570   read #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,lcn$ nokey L4740
27580   bn$=rtrm$(bn$)
27590   bal=bal-val(tr$(3)) conv ignore
27600   rewrite #h_cl_bank,using F_CLFILE_12,key=lpad$(str$(bankcode),2): bn$,bal,upi,tr$(1) nokey L4740
27610   F_CLFILE_12: form pos 3,c 30,pos 45,pd 6.2,pd 6.2,g 8
27620   L4740: ! 
27630   F_CL_TRANS_V1: form pos 1,n 2,n 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1,2*pd 3
27640   ! WRITE ALLOCATIONS
27650   if allign=1 then goto L5250
27660   for j=1 to 29
27670    if val(ded$(j))=0 then goto L5230
27680    gl$=""
27690    on j goto L4840,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L4910,L5220_Next_J1,L5220_Next_J1,BCR_GLN_VALIDATED none L5230
27700    ! ______________________________________________________________________
27710    L4840: ! 
27720    for j1=1 to tdepXcount
27730      if j<6 or j>25 then goto L4870 ! kj 91707
27740      if dedcode(j-5)=3 then goto L5230 ! kj 91707  don't write entries for benefits
27750      L4870: ! 
27760      alloc=tdep(j1,1)
27770      gl$=cnvrt$("N 3",tdep(j1,2))&cnvrt$("N 6",tdep(j1,3))&cnvrt$("N 3",tdep(j1,4))
27780      sd5$="Gross Pay"
27790      goto L4990
27800      L4910: ! 
27810      if j=2 then sd5$="Federal WH" : gl$=gln$(1)
27820      if j=3 then sd5$="FICA WH" : gl$=gln$(2) : fica0=val(ded$(j))
27830      if j=4 then sd5$="Medicare" : gl$=gln$(2) : medi0=val(ded$(j)): goto L4990
27840      if j=5 then sd5$="State WH" : gl$=gln$(3)
27850      if j>5 and j<26 then sd5$=abrevname$(j-5) : gl$=gl$(j-5)
27860      if j=26 then gl$=gln$(1): sd5$="eic" : goto L4990 ! use federal
27870      if j=27 then goto L4990 ! skip tips i think
27880      ! If J=28 Then gL$=GLN$(1): sD5$="Meals" : Goto 4890 ! use wages
27890      L4990: ! 
27900      cd1=1
27910      read #h_cl_glmstr,using F_CL_GLMSTR,key=rpad$(gl$,kln(h_cl_glmstr)),release: de$ nokey INVALIDGLNUMBER
27920      F_CL_GLMSTR: form pos 13,c 30
27930      BCR_GLN_VALIDATED: ! 
27940      if j>1 then alloc=val(ded$(j))
27950      if j=29 then miscode=(alloc*100)+29 else miscode=j
27960      ! store # of deduction in the invoice date field;
27970      ! if weeks worked store weeks worked and code both
27980      if j>1 then alloc=-alloc
27990      if j<6 or j>25 then goto L5070 ! all tax w/h = negative
28000      if dedcode(j-5)=2 then alloc=-alloc ! reverse on additions to net
28010      L5070: ! 
28020      if j=30 then alloc=0 ! meals
28030      !       if env$('client')="Washington Parrish" and j=16 then alloc=0 ! don't allow tips to post on washington parrish
28040      if j=4 and ficam1=1 then alloc=alloc-medic3 ! prb 2012
28050      if j=3 and ficam1=1 then alloc=alloc-ficam3 ! prb 2012
28060      if alloc<>0 then  ! write non-zero allocations
28070        lr3=lrec(h_cl_trans_alloc)+1
28080        if j=3 then fica1=alloc : fica_rec=lr3
28090        if j=4 then medi1=alloc
28100        write #h_cl_trans_alloc,using F_ClTransAlloc,rec=lr3: bankcode,1,val(tr$(1)),gl$,alloc,de$(1:30),miscode,0
28110        F_ClTransAlloc:  form pos 1,n 2,n 1,g 8,c 12,pd 5.2,c 30,n 6,pd 3
28120      end if
28130      L5220_Next_J1: ! 
28140    if j=1 then next j1
28150    L5230: ! 
28160   next j
28170   fn_mgl
28180   L5250: ! 
28190 fnend 
29000 def fn_cknum ! check for duplicate check numbers
29020   if testCheckFormat then goto L5720
29040   L5410: ! 
29060   dk$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(check_number),8)
29080   read #h_cl_trans,using L5440,key=dk$: dtr$(1),dtr$(2),dtr3,dtr$(4),dtr$(5) nokey L5720
29100   dtr$(3)=str$(dtr3)
29120   L5440: form pos 4,c 8,g 6,pd 10.2,c 8,c 35,pos 79,2*pd 3
29140   DUPLICATE_CHECK: ! 
29160   fnTos(sn$="Prckprt4")
29180   respc=0: mypos=50
29200   fnLbl(1,1,"Check Number "&str$(check_number)&" has been previously used!",50,1)
29220   fnLbl(3,1,"Date: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",val(dtr$(2))),50,0)
29240   fnLbl(4,1,"Amount: "&dtr$(3),50,0)
29260   fnLbl(5,1,"To: "&rtrm$(dtr$(5)),50,0)
29280   fnLbl(7,1,"Click          to Delete the previous entry else" ,50,1)
29300   fnButton(7,10,"Delete",3,"Press Delete to delete the old check from history and replace it with the new check, else",1,6)
29320   text$="enter the correct check number for "&trim$(dtr$(5))&":"
29340   textlenght=len(trim$(text$))
29360   fnLbl(8,4,text$,textlenght,0)
29380   fnTxt(8,textlenght+7,7,0,1,"30",0,"")
29400   resp$(respc+=1)=str$(check_number)
29420   fnCmdKey("&Next",1,1,0,"Continue with checkprinting." )
29440   fnCmdKey("E&xit",5,0,1,"Returns to menu")
29460   fnAcs(sn$,0,mat resp$,ckey) ! dupllicate check number
29480   if ckey=5 then goto XIT
29500   if ckey=3 then goto L5670 ! if delete
29520   ckn2=val(resp$(1))
29540   if ckn2=0 then goto DUPLICATE_CHECK
29560   check_number=ckn2
29580   tr$(1)=lpad$(str$(ckn2),8)
29600   goto L5410
29620   ! ______________________________________________________________________
29640   L5670: ! 
29660   bal=bal+val(dtr$(3))
29680   delete #h_cl_trans,key=dk$: 
29700   key$=lpad$(str$(bankcode),2)&"1"&lpad$(str$(check_number),8)
29720   restore #h_cl_trans_alloc,key>=key$: nokey L5720
29740   do
29760     read #h_cl_trans_alloc,using 'Form Pos 1,C 11': newkey$ eof L5720
29780     if newkey$=key$ then 
29800       delete #h_cl_trans_alloc: 
29820     end if 
29840   loop while newkey$=key$
29860   L5720: ! 
29880 fnend 
30000 def fn_mgl ! WRITE BENEFITS & FICA MATCH
30020   cd1=2
30040   for j=1 to tdepXcount
30050     mat mgl$=("            ")
30060     read #7,using L5790,key=lpad$(str$(tdep(j,5)),3): mat mgl$ nokey L5800
30070     L5790: form pos 4,11*c 12
30080     L5800: ! 
30090     for j2=6 to 16
30200       if tdep(j,j2)=0 then goto L5980
30210       if j2>6 then goto L5870
30220       if ficam1=0 then goto L5980
30230       sd5$=de$="FICA Match"
30240       fica2=fica2+tdep(j,j2)
30250       j4=3
30260       goto L5900
30270       L5870: ! 
30280       if dedcode(j2-6)><3 then goto L5980
30290       j4=j2-2
30300       sd5$=de$=rtrm$(abrevname$(j4-4))&" Match"
30310       L5900: ! 
30320       gl$=mgl$(j2-5)
30330       read #h_cl_glmstr,using F_CL_GLMSTR,key=gl$,release: de$ nokey INVALIDGLNUMBER
30340       ! 
30350       ! 
30360       EXLNKD_L5920: ! 
30362       if ~testCheckFormat then
30370         lr3=lrec(h_cl_trans_alloc)+1
30380         write #h_cl_trans_alloc,using F_ClTransAlloc,rec=lr3: bankcode,1,val(tr$(1)),gl$,tdep(j,j2),de$(1:30),j4,0
30382       end if
30460       L5980: ! 
30470     next j2
30480   next j
30490   ! fn_FICA_FIX
30500 fnend 
31000 IGNORE: continue 
32000 ! <Updateable Region: ERTN>
32020 ERTN: fnerror(program$,err,line,act$,"xit")
32040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
32060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
32080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
32100 ERTN_EXEC_ACT: execute act$ : goto ERTN
32120 ! /region
33000 ! r: Check pr routines
34000 def fn_print_check
34005   if ttc(32)<=0 then 
34010     ca$="***VOID***"
34015   else 
34020     ca$=rtrm$(cnvrt$("PIC($$$,$$$,$$$.##)",ttc(32)))
34025   end if 
34030   !
34035   if env$('client')="Merriam Woods" then 
34040     englishAmountBreakPoint=65
34045   else if env$('client')="Payroll Done Right" and env$('cno')='18' then
34050     englishAmountBreakPoint=82
34055   else
34060     englishAmountBreakPoint=0   !   0=default=58
34065   end if
34070   n1=fn_EnglishAmount(ttc(32),mat eng$, englishAmountBreakPoint)
34075   !
34080   if uprc$(ddcode$)="D" then eng$="Direct Deposit" : ca$="V O I D"
34085   !
34090   if env$('client')="ACS" then 
34095     fn_check_acs
34100   else if env$('client')="Ash Grove" then 
34105     fn_check_legacy(3,3)
34110   else if env$('client')="Bethany" then 
34115     fn_check_bethany
34120   else if env$('client')="Billings" then 
34125     fn_check_billings
34128   else if env$('client')="Campbell" then ! r: updated 1/17/2018 - uses very few options
34129     length                 =26
34130     line_date              =13
34131     line_amount            =13
34132     line_amount_english    =9
34134     line_name_and_address  =15
34135     pos_date               =57
34136     pos_amt                =69
34137     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt) ! /r
34140   else if env$('client')="Carr Plumbing" then ! r:
34145     length                = 26
34150     line_date             =  4
34155     line_amount           =  7
34160     line_amount_english   = 10
34165     line_name_and_address = 14
34170     pos_date              = 78
34175     pos_amt               = 72
34180     line_nameOnly         =  7
34185     pos_nameOnly=0 ! default to 12
34190     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly) ! /r
34195   else if env$('client')="Cerro Gordo" then 
34200     fn_check_cerrogordo
34205   else if env$('client')="Cerro Gordo T" then ! r:
34210     length                = 27
34215     line_date             = 14
34220     line_amount           = 14
34225     line_amount_english   =  9
34230     line_name_and_address = 15
34235     pos_date              = 55
34240     pos_amt               = 71
34245     line_nameOnly         =  0
34250     pos_nameOnly          =  0
34255     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly) ! /r
34260   else if env$('client')="Divernon" then 
34265     fn_check_divernon
34270   else if env$('client')="Edinburg" then 
34275     fn_check_edinburg
34280   else if env$('client')="Edison" then ! r: 6/29/2017
34285     length                = 25
34290     line_date             =  6
34295     line_amount           = 11
34300     line_amount_english   = 9
34305     line_name_and_address = 13  ! 0/not printing is the default
34310     pos_date              = 74  ! 65 is default
34315     pos_amt               = 79 ! pos_date+18 is default
34320     line_nameOnly         =  0
34325     pos_nameOnly          =  0
34330     line_checkNumber      = line_date
34335     pos_checkNumber       = 83
34340     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number)
34345     ! /r
34350   else if env$('client')="Energy Exchanger" then ! r: 5/9/2017 
34355     length                = 23
34360     line_date             =  4
34365     line_amount           = 12
34370     line_amount_english   = 9
34375     line_name_and_address = 13
34380     pos_date              = 75
34385     pos_amt               = 80
34390     line_nameOnly         =  0
34395     pos_nameOnly          =  0
34400     line_checkNumber      = line_date
34405     pos_checkNumber       = 86
34410     fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number)
34415     ! /r
34420   else if env$('client')="Hope Welty" then 
34425     fn_check_hope_welty
34430   else if env$('client')="Kathys Bookkeeping" then 
34435     fn_check_dynamic(22,9,9,6,10, 58,72)
34440   else if env$('client')="Lamar" then 
34445     fn_check_lamar
34450   else if env$('client')="Lovington" then 
34455     fn_check_lovington
34460   else if env$('client')="Merriam Woods" then 
34465     fn_check_merriam_woods
34470   else if env$('client')="Monticello" then 
34475     fn_check_monticello
34480   else if env$('client')="Philo" then 
34485     fn_check_philo
34490   else if env$('client')="Thomasboro" then 
34495     fn_check_thomasboro
34500   else if env$('client')="Thomas Richardson" then 
34505     fn_check_tom_richardson
34510   else if env$('client')="Unity" then 
34515     fn_check_unity
34520   else if env$('client')="West Accounting" or env$('client')="Payroll Done Right" then 
34525     if env$('client')="Payroll Done Right" and env$('cno')='18' then ! r: 12/29/2017  (most recent)
34530       length                = 25
34535       line_date             =  3
34540       line_amount           =  6
34545       line_amount_english   =  8
34550       line_name_and_address =  0  ! 0/not printing is the default
34555       pos_date              = 80  ! 65 is default
34560       pos_amt               = 75 ! pos_date+18 is default
34565       line_nameOnly         =  6
34570       pos_nameOnly          =  8
34575       line_checkNumber      =  0
34580       pos_checkNumber       =  0
34585       pos_amount_english    =  1   ! default is 9
34590       fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address, pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,check_number,pos_amount_english)
34595       ! /r
34600     else
34605       fn_check_west_accounting
34610     end if
34615   else 
34620     fn_check_dynamic(21,6,6,7,13) ! fn_check_legacy ! default for everyone without a special routine...
34625   end if 
34630 fnend 
35000 def fn_check_dynamic(length,line_date,line_amount,line_amount_english,line_name_and_address; pos_date,pos_amt,line_nameOnly,pos_nameOnly,line_checkNumber,pos_checkNumber,checkNumber,pos_amount_english)
35020   ! 
35040   if pos_date=0 then pos_date=65
35060   if pos_amt=0 then pos_amt=pos_date+18
35080   if pos_nameOnly=0 then pos_nameOnly=12
35082   if pos_amount_english=0 then pos_amount_english=9
35100   ! 
35120   for line_item=1 to length
35122 ! if line_item=4 the pr line_item : pause
35140     if line_item=line_date and line_item=line_amount then 
35150       pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),pos pos_amt,c 18': dat,ca$
35160     else if line_item=line_date and line_item=line_checkNumber and line_checkNumber<>0 then 
35170       pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),pos pos_checkNumber,N 8': dat,checkNumber
35210     else if line_item=line_nameOnly and line_item=line_amount then
35220       pr #255,using "form Pos pos_nameOnly,C 30,pos pos_amt,c 18": em$(1),ca$
35230     else if line_item=line_nameOnly and line_item=line_date then
35232       ! pause
35240       if pos_date<pos_nameOnly then
35250         pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ),Pos pos_nameOnly,C 30': dat,em$(1)
35260       else
35270         pr #255,using 'form Pos pos_nameOnly,C '&str$(min(30,pos_date-pos_nameOnly))&',Pos pos_date,pic(ZZ/ZZ/ZZ)': em$(1)(1:pos_date-pos_nameOnly),dat
35280       end if
35282     else if line_item=line_date then 
35284       pr #255,using 'form Pos pos_date,pic(ZZ/ZZ/ZZ)': dat
35290     else if line_item=line_nameOnly then
35292       pr #255,using "form Pos pos_nameOnly,C 30": em$(1)
35300     else if line_item=line_amount and line_item=line_name_and_address then
35320       pr #255,using "form Pos 12,C 30,pos pos_amt,c 18": em$(1),ca$
35340       pr #255,using "form Pos 12,C 30": em$(2) : line_item+=1
35360       pr #255,using "form Pos 12,C 30": em$(3) : line_item+=1
35380     else if line_item=line_amount_english then 
35400       pr #255,using 'form Pos pos_amount_english,C n1': eng$(1:n1)
35420       pr #255,using 'form Pos pos_amount_english,C 62': eng$(n1+1:inf) : line_item+=1
35440     else if line_item=line_name_and_address then 
35460       pr #255,using "form Pos 12,C 30": em$(1)
35480       pr #255,using "form Pos 12,C 30": em$(2) : line_item+=1
35500       pr #255,using "form Pos 12,C 30": em$(3) : line_item+=1
35520     else if line_item=line_amount then
35540       pr #255,using "form pos pos_amt,c 18": ca$
35560     else 
35580       pr #255: ""
35600     end if 
35620   next line_item
35640 fnend 
36000 def fn_check_legacy(; extra_lines_at_top,extra_lines_at_bottom)
36020   for j=1 to extra_lines_at_top
36040     pr #255: ""
36060   next j
36070   fn_check_dynamic(21+extra_lines_at_bottom,6,6,7,13)
36460 fnend 
37000 def fn_check_acs
37020   pr #255,using 'Form skip 3,POS 80,PIC(ZZ/ZZ/ZZ)': dat
37040   pr #255,using 'Form skip 2,POS 10,c 30,pos 74,c 18': em$(1),ca$
37060   pr #255,using 'Form skip 2,pos 9,C 62': eng$(1:n1)
37080   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
37100   for j=1 to 3
37120     pr #255: ""
37140   next j
37160   for j=1 to 3
37180     pr #255,using "Form Pos 12,C 60": em$(j)
37200   next j
37220   pr #255,using "form pos 1,c 1,skip 7": ""
37240 fnend 
38000 def fn_check_bethany
38020   for j=1 to 5
38040     pr #255: ""
38060   next j
38080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
38120   datepos=65
38140   pr #255: 
38160   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
38180   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
38200   for j=1 to 3
38220     pr #255: ""
38240   next j
38260   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
38280   pr #255: 
38300   for j=1 to 3
38320     pr #255,using "Form Pos 12,C 60": em$(j)
38340   next j
38360   pr #255,using "form pos 1,c 1,skip 9": "" ! changed skip 6 to skip 9 on 10/17/2016
38380 fnend 
39000 def fn_check_billings
39040   pr #255: ""
39060   pr #255: ""
39080   pr #255,using 'form pos 40,c 38,skip 1': "Void After 60 Days"
39100   pr #255: ""
39110   pr #255: ""
39140   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
39160   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
39180   pr #255,using 'Form POS 63,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
39200   pr #255: ""
39220   pr #255: ""
39240   pr #255: ""
39260   pr #255: ""
39280   pr #255,using "Form Pos 12,C 60": em$(1)
39300   pr #255,using "Form Pos 12,C 60": em$(2)
39320   pr #255,using "Form Pos 12,C 60": em$(3)
39340   pr #255: ""
39360   pr #255: ""
39380   pr #255: ""
39400   pr #255: ""
39420   pr #255: ""
39440   pr #255: ""
39460   pr #255: ""
39480   pr #255: ""
39490   pr #255: ""
39540 fnend 
40000 def fn_check_cerrogordo
40020   pr #255: "" : pr #255: "" : pr #255: ""
40040   pr #255: "" : pr #255: "" : pr #255: ""
40080   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
40100   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
40120   pr #255: ""
40140   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
40160   pr #255: ""
40180   pr #255: ""
40200   for j=1 to 3
40220     pr #255,using "Form Pos 12,C 60": em$(j)
40240   next j
40260   pr #255: ""
40280   pr #255: ""
40300   pr #255: ""
40320   pr #255: ""
40340   pr #255: ""
40360   pr #255: ""
40380   pr #255: ""
40400 fnend 
41000 def fn_check_divernon
41020   pr #255: ""
41040   pr #255: ""
41060   pr #255: ""
41080   pr #255: ""
41100   pr #255: ""
41120   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
41140   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
41160   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
41180   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
41200   pr #255: ""
41220   pr #255: ""
41240   pr #255: ""
41260   pr #255,using "Form Pos 12,C 60": em$(1)
41280   pr #255,using "Form Pos 12,C 60": em$(2)
41300   pr #255,using "Form Pos 12,C 60": em$(3)
41320   pr #255: ""
41340   pr #255: ""
41360   pr #255: ""
41380   pr #255: ""
41400   pr #255: ""
41420   pr #255: ""
41440 fnend 
42000 def fn_check_edinburg
42020   for j=1 to 5
42040     pr #255: ""
42060   next j
42080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
42120   datepos=65
42140   pr #255: 
42160   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
42180   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
42200   for j=1 to 3
42220     pr #255: ""
42240   next j
42260   pr #255,using 'Form POS 55,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
42280   pr #255: 
42300   for j=1 to 3
42320     pr #255,using "Form Pos 12,C 60": em$(j)
42340   next j
42360   pr #255,using "form pos 1,c 1,skip 6": ""
42380   pr #255: : pr #255: 
42400 fnend 
43000 def fn_check_hope_welty
43020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
43040   pr #255: : pr #255: 
43080   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
43100   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
43120   pr #255: 
43140   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
43160   x=3
43180   for j=1 to x
43200     pr #255: ""
43220   next j
43240   for j=1 to 3
43260     pr #255,using "Form Pos 12,C 60": em$(j)
43280   next j
43300   pr #255: ""
43320   pr #255: ""
43340   pr #255: ""
43360   pr #255: ""
43380   pr #255: ""
43400   pr #255: ""
43402   pr #255: ""
43404   pr #255: ""
43406   pr #255: ""
43420 fnend 
44000 def fn_check_lamar
44020   for j=1 to 9
44040     pr #255: ""
44060   next j
44080   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
44120   datepos=65
44140   pr #255,using 'Form POS datepos,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
44160   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
44180   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
44200   for j=1 to 3
44220     pr #255: ""
44240   next j
44260   for j=1 to 3
44280     pr #255,using "Form Pos 12,C 60": em$(j)
44300   next j
44320   pr #255,using "form pos 1,c 1,skip 6": ""
44340 fnend 
45000 def fn_check_lovington
45020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
45040   pr #255,using "form pos 76,n 8,skip 1": check_number
45060   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
45070   pr #255: ''
45080   pr #255,using 'Form POS 63,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
45100   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
45120   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
45180   pr #255: ""
45190   pr #255,using "Form Pos 12,C 60": em$(1)
45200   pr #255,using "Form Pos 12,C 60": em$(2)
45220   pr #255,using "Form Pos 12,C 60": em$(3)
45280   pr #255: ""
45300   pr #255: ""
45320   pr #255: ""
45340   pr #255: ""
45360   pr #255: ""
45380   pr #255: ""
45400   pr #255: ""
45420   pr #255: ""
45440   pr #255: ""
45450   pr #255: ""
45460 fnend 
46000 def fn_check_monticello
46020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
46060   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
46080   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
46100   pr #255: : pr #255: 
46120   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 2,C 18': dat,ca$
46140   pr #255: ""
46160   for j=1 to 3
46180     pr #255,using "Form Pos 12,C 60": em$(j)
46200   next j
46220   pr #255: ""
46240   pr #255: ""
46260   pr #255: ""
46280   pr #255: ""
46300   pr #255: ""
46320   pr #255: ""
46340   pr #255: ""
46360 fnend 
47000 def fn_check_merriam_woods
47020   pr #255: ""
47040   pr #255: ""
47060   pr #255: ""
47080   pr #255: ""
47100   pr #255: ""
47120   pr #255: ""
47140   pr #255: ""
47160   if sc1$="C" then pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
47200   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
47220   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
47240   pr #255: ""
47260   pr #255,using 'Form POS 65,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
47280   pr #255: ""
47300   pr #255: ""
47320   pr #255,using "Form Pos 12,C 60": em$(1)
47340   pr #255,using "Form Pos 12,C 60": em$(2)
47360   pr #255,using "Form Pos 12,C 60": em$(3)
47380   pr #255: ""
47400   pr #255: ""
47420   pr #255: ""
47440   pr #255: ""
47460   pr #255: ""
47480   pr #255: ""
47500 fnend 
48000 def fn_check_philo
48020   pr #255: : pr #255: : pr #255: : pr #255: : pr #255: 
48060   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
48080   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
48100   pr #255: : pr #255: 
48120   pr #255,using 'Form POS 57,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$
48140   x=3
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
52260   pr #255,using 'Form POS 9,C 62,X 4,C 18': eng$(1:n1),ca$
52280   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
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
54060   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
54080   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
54100   pr #255: : pr #255: : pr #255: 
54120   pr #255,using 'Form POS 53,PIC(ZZ/ZZ/ZZ),X 6,C 18': dat,ca$
54140   x=3
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
56120   datepos=65
56140   pr #255: 
56160   pr #255,using 'Form POS 9,C 62': eng$(1:n1)
56180   pr #255,using 'Form POS 9,C 62': eng$(n1+1:128)
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
57040     pr #255: ""                                                ! line 1-8
57060   next j
57100   pr #255,using 'Form Pos 65,PIC(ZZ/ZZ/ZZ),X 4,C 18': dat,ca$  ! line 9
57120   pr #255,using 'Form Pos 9,C 62': eng$(1:n1)                   ! line 10
57140   pr #255,using 'Form Pos 9,C 62': eng$(n1+1:128)               ! line 11
57160   pr #255: ""                                                   ! line 12
57180   pr #255: ""                                                   ! line 13
57200   pr #255: ""                                                   ! line 14
57210   pr #255,using "Form Pos 12,C 60": em$(1)                     ! line 15
57220   pr #255,using "Form Pos 12,C 60": em$(2)                     ! line 16
57230   pr #255,using "Form Pos 12,C 60": em$(3)                     ! line 17
57280   for j=1 to 8
57300     pr #255: ""                                                ! line 18-25
57320   next j
57340 fnend 
59000 ! /r
60000 ! r: Stub pr routines
60010 def fn_print_stub
60020   tdedcp=tdedytd=0
60030   for j=1 to 23
60040     if j<5 then 
60050       goto L2230
60060     else if dedcode(j-4)=2 then 
60070       tdedcp=tdedcp-ttc(j)
60080       tdedytd=tdedytd-tty(j)
60090       goto L2240
60100     else if dedcode(j-4)=3 then 
60110       goto L2240
60120     end if 
60130     L2230: ! 
60140     tdedcp=tdedcp+ttc(j): tdedytd=tdedytd+tty(j)
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
60270   else if env$('client')='West Accounting' or env$('client')='Payroll Done Right' then 
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
64020   stub_one_or_two+=1
64040   if stub_one_or_two=3 then stub_one_or_two=1
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
72040   key$=lpad$(str$(eno),8)&"             "
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
74080   fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0
74100   medqtr=eicyr=eicqtr=wagesqtr=0
74120   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! indexed by employee#,department# and payroll date
74140   restore #3,key>=checkkey$: nokey L6920
74160   L6580: ! 
74180   read #3,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,oldckno,mat tdc,mat tcp eof STORE_VARIABLES : lastrec=rec(3)
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
74500   !   wagesyr=ytdtotal(31) ! total wages
74520   fedyr=ytdtotal(1) ! ytdl fed
74540   ficayr=ytdtotal(2) ! fica year to date
74560   medyr=ytdtotal(3) ! medicare year to date
74580   stateyr=ytdtotal(4) ! total state  quarter
74600   eicyr=ytdtotal(25) ! eic
74620   if prd>=qtr1 and prd<qtr2     then mat quartertotals=qtr1tcp
74640   if prd>=qtr2 and prd<qtr3     then mat quartertotals=qtr2tcp
74660   if prd>=qtr3 and prd<qtr4     then mat quartertotals=qtr3tcp
74680   if prd>=qtr4 and prd<end_date then mat quartertotals=qtr4tcp
74700   wagesqtr=quartertotals(31) ! total wages quarter
74720   fedqtr=quartertotals(1) ! total fed  quarter
74740   ficaqtr=quartertotals(2) ! total fica quarter
74760   medqtr=quartertotals(3) ! total medicare quarter
74780   stateqtr=quartertotals(4) ! total state  quarter
74800   eicqtr=quartertotals(25) ! EIC qtr
74820   !   for j=1 to 20
74840   !     if dedfed(j)=1 then dedfedyr+=ytdtotal(j+4) ! deduct for federal wh
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
76140   tdepXcount=tdepXcount+1
76160   j2=tdepXcount
76180   adt_L1790: ! 
76200   tdep(j2,1)=tdep(j2,1)+tcp(31)-tcp(30) ! total wage less tips
76220   deptgl$=""
76240   read #hDepartment,using "Form pos 12,c 12,pos 62,2*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",tdn): deptgl$,tdet(2),tdet(3) ! Nokey 1660
76260   tdep(j2,2)=val(deptgl$(1:3)) ! salary for this department
76280   tdep(j2,3)=val(deptgl$(4:9))
76300   tdep(j2,4)=val(deptgl$(10:12))
76320   tdep(j2,5)=tdn
76340   fn_fica_matching
76360   tdep(j2,6)=ficam2+medic2 ! fica+match
76380   for j3=1 to 20
76400     tdep(j2,j3+6)=tdep(j2,j3+6)+tcp(j3+4)
76420   next j3
76440   if s1=1 then 
76460     if rate=0 then rate=tdet(2)
76480     if rate>0 then rt$="PAY RATE"&cnvrt$("N 10.2",rate) else rt$=""
76500   end if
76510   tpd3=tpd3+round(tdc(3)*tdet(2),2) ! sick pay
76520   tpd4=tpd4+round(tdc(4)*tdet(2),2) ! vacation pay
76540   tpd5=tpd5+round(tdc(5)*tdet(2),2) ! if env$('client')="West Rest Haven" then tpd5=tpd5+round(tdc(5)*(tdet(2)*1.5),2) else tpd5=tpd5+round(tdc(5)*tdet(2),2)
76560   tdc1=ttdc(1) ! Regular Hours
76580   tdc2=ttdc(2) ! OverTime Hours
76600   tdc3=ttdc(3)
76620   tdc4=ttdc(4)
76640   tdc5=ttdc(5)
76660   !   ttdct=ttdc(1)+ttdc(2)+ttdc(3)+ttdc(4)+ttdc(5) ! Total Hours
76680 fnend 
78000 def fn_accumulate_dept_totals2
78020   for v1=1 to 6
78040     if tdn=deptsum(v1) then goto L8090 ! determine if dept # used on this employee already
78060     if deptsum(v1)=0 then deptsum(v1)=tdn: goto L8080
78080   next v1
78100   if v1>6 then v1=6 ! summarize any departments over 6 and the seventh row
78120   L8080: ! 
78140   if prd<>d1 then goto L8150
78160   L8090: ! 
78180   for r=1 to 5
78200     v(v1,r)+=tdc(r)
78220     v(7,r)+=tdc(r) ! total line
78240   next r
78260   v(v1,6)+=tcp(31) ! pay
78280   v(7,6)+=tcp(31) ! total pay line
78300   L8150: ! 
78320   for r=1 to 6
78340     if prd=d1 then 
78360       v(r,7)+=tcp(r+8) ! last five misc deductions
78362     end if
78380     ! FILL TABLE S WITH YEAR TO DATE DEDUCTIONS
78400     v(r,8)=v(r,8)+tcp(r+8)
78420   next r
78440   ! FILL TABLE S WITH CURRENT DEDUCTIONS
78460   for j=9 to 14
78480     if dedcode(j-3)=1 then goto L8270
78500     v(7,8)=v(7,8)-tcp(j)
78520     if prd=d1 then v(7,7)=+v(7,7)-tcp(j)
78540     goto L8290
78560     L8270: ! 
78580     v(7,8)=v(7,8)+tcp(j)
78600     if prd=d1 then v(7,7)=v(7,7)+tcp(j)
78620     L8290: !
78622   next j
78640   ! ROUTINE TO ACCUMULATE HOURS ETC. FOR SUMMARY
78660 fnend 
80000 def fn_fica_fix ! fix rounding problem on fica
80020   fica3=fica0+medi0+fica1+medi1+fica2
80040   if fica3=0 then goto FICA_END
80060   read #h_cl_trans_alloc,using F_ClTransAlloc,rec=fica_rec: bankcode,a1,tr1,gl$,alloc
80080   alloc=alloc-fica3
80100   rewrite #h_cl_trans_alloc,using F_ClTransAlloc,rec=fica_rec: bankcode,a1,tr1,gl$,alloc
80120   FICA_END: fica3=fica0=medi0=fica1=medi1=fica2=0
80140 fnend 
82000 def fn_fica_matching ! CALCULATE MATCHING FICA
82020   ficawg=round(tcp(2)/ssr1,2) ! employee's fica rate
82040   ficam2=round(ficawg*ssr2,2) ! employers fica rate
82060   mediwg=tcp(3)/.0145 ! employee medicare rate
82080   medic2=round(mediwg*.0145,2) ! employers medicare rate
82100   if fmeno=eno then goto SENO
82120   fmeno=eno
82140   ficam3=ficam2
82160   medic3=medic2
82180   goto MFEND
82200   SENO: ! same employee
82220   ficam3=ficam3+ficam2
82240   medic3=medic3+medic2
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
84280   if ttc(32)<0 then ttc(32)=0
84300   pr #255,using 'form pos 7,c 62,pos 85,pic($$$$$,$$$.##)': eng$(1:n1),ttc(32)
84340   pr #255,using X7660: eng$(n1+1:128)
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
85100   ! tDC1=TDC2=0
85120   rate=0
85140   mat v=(0)
85160   for k=1 to 6
85180     deptsum(k)=0
85200   next k
85220   if check_number>0 then check_number=check_number+1
85240   PRINTSTUB_ELDORADO: pr #255: ""
85260   fn_print_stub
85280 fnend 
88000 INVALIDGLNUMBER: ! r:
88020   fnTos(sn$="Prckprt3")
88040   mylen=30 : mypos=mylen+2
88060   fnLbl(1,1,"Employee Number:",mylen,1)
88080   fnTxt(1,mypos,10, 0,0,'',1)
88100   resp$(1)=str$(eno)
88120 ! 
88140   fnLbl(2,1,"Department Number:",mylen,1)
88160   fnTxt(2,mypos,10, 0,0,'',1)
88180   resp$(2)=str$(tdn)
88200 ! 
88220   fnLbl(4,1,"Invalid General Ledger Number:",mylen,1)
88240   fnTxt(4,mypos,12, 0,0,'',1)
88260   resp$(3)=gl$
88280 ! 
88300   fnLbl(5,1,"Purpose for GL Number:",mylen,1)
88320   fnTxt(5,mypos,40, 0,0,'',1)
88340   resp$(4)=sd5$
88360 ! 
88380   fnLbl(7,1,"The General Ledger Number is invalid.",40,0)
88400   fnLbl(8,1,"Please select the correct one.",40,0)
88420   fnLbl(3,1,"Correct General Ledger Number:",mylen,1)
88440   fnqgl(3,mypos,0,2,pas)
88460   resp$(5)=fnrgl$(goodgl$)
88480   fnCmdKey("&Next",1,1,0,"Continue with checkprinting." )
88500   fnCmdKey("E&xit",5,0,1,"Returns to menu")
88520   fnAcs(sn$,0,mat resp$,ckey) ! bad general ledger numbers
88540   if ckey=5 then goto XIT
88560   gl$=fnagl$(resp$(5))
88600   read #h_cl_glmstr,using F_CL_GLMSTR,key=gl$,release: de$ nokey INVALIDGLNUMBER
88620   on cd1 goto BCR_GLN_VALIDATED,EXLNKD_L5920 none BCR_GLN_VALIDATED
88640 ! /r


89000 def fn_getTestValues
89010   eno=999
89020   eno=99999999
89030   em$(1)='|Name ------VOID-VOID-VOID---|'
89040   em$(2)='|Address --------------------|'
89050   em$(3)='|City, State and Zip---------|'
89060   em10=        12                                  ! sick hours accrued
89070   em11=        14                                  ! vacation hours accrued
89080   lpd=   date(days(d1,'ccyymmdd'),'mmddyy')       ! last payroll date
89090   rate=        10                                  ! Hourly Rate
89100   rt$=         "PAY RATE"&cnvrt$("N 10.2",rate)   ! text used in some stub routines
89110   tpd3=         9                                  ! sick pay
89120   tpd4=         8                                  ! vacation pay
89130   tpd5=         7                                  
89140   tdc1=        40                                  ! Regular Hours
89150   tdc2=        10                                  ! OverTime Hours
89160   tdc3=         6                                  
89170   tdc4=         5                                  
89180   tdc5=         4                                  
89190   ttc(31)=  34500                                  ! gross salary
89200   ttc(1) =    250                                  ! Fed w/h
89210   ttc(4) =     75                                  ! State w/h
89220   ttc(2) =     15                                  ! FICA
89230   ttc(5) =      3                                  ! pre insurance tax
89240   ttc(6) =      2                                  
89250   ttc(7) =      1                                  
89260   ttc(8) =      0.5                                
89270   ttc(9) =      0.4                                
89280   ttc(32)= 13895.27                                  ! net pay 
89290    ! YTD of line above
89300   tty(31)=  30000                                  ! gross ytd
89310   tty(1) =    100                                  ! fed w/h ytd
89320   tty(4) =     50                                  ! state w/h ytd
89330   tty(2) =     25                                  ! FICA YTD
89340   tty(5) =      5
89350   tty(6) =      4
89360   tty(7) =      3
89370   tty(8) =      2
89380   tty(9) =      1
89390 fnend

