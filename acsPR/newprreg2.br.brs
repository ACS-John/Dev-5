08000 ! Replace S:\acsPR\newprReg2
08020 ! Payroll Register (Part 2 of 2)
08040   let fn_setup
08060   let det=0
08080   let fn_payroll_register_2(det)
08100 XIT: let fnxit
10000   def fn_setup
10020     if ~setup then 
10040       let setup=1
10060       library 'S:\Core\Library': fntop,fnxit,fnGetPayrollDates,fnDedNames,fnerror,fnopenprn,fncloseprn,fnprocess,fndate_mmddyy_to_ccyymmdd,fnss_employee,fnss_employer,fnindex_it,fnstatus_close,fngethandle
10080       on error goto ERTN
10100     end if 
10120   fnend 
12000   def library fnpayroll_register_2(; det,include_tips_in_other_wh,append_reg1)
12020     let fn_setup
12040     let fnpayroll_register_2=fn_payroll_register_2( det,include_tips_in_other_wh,append_reg1)
12060   fnend 
14000   def fn_payroll_register_2(; det,include_tips_in_other_wh,append_reg1)
14020 ! DET=1 if you dont want details printed but you want department totals.
14040 ! DET=2 if you dont want details or department totals printed.
14080 ! ______________________________________________________________________
14100     dim a$*40,em$*30,cp(32),tcp(32),hc(5),thc(5),d$*20,whc(10),gcp(32)
14120     dim tdc(10),fullname$(20)*20,abbrevname$(20)*20,client$*30
14140     dim dedcode(10),calcode(10),dedfed(10),statname$(10)*8
14160     dim newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20)
14180     dim statewh(10),totaltcp(32),totalthc(5),deptname$*20
14200     dim sucrat(10),stuc1(10),stuc2(10),err$(3)*65,cap$*128
14220 ! ______________________________________________________________________
16000     let fntop(program$,cap$="Payroll Registers")
16060     open #20: "Name="&env$('Q')&"\PRmstr\prCode.h"&env$('cno')&",Shr",internal,input ioerr L180
16080     read #20,using 'Form POS 5,N 5': ckno
16100     close #20: 
16120     L180: ! 
16140     if ~append_reg1 then let fnopenprn( 0,0,0,fnprocess,' (Departmental Register)')
16160     ! ______________________________________________________________________
18000     open #9: "Name="&env$('Q')&"\PRmstr\DeptName.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptNameIdx.h"&env$('cno')&",Shr",internal,input,keyed ioerr L220X
18020     let founddept=1
18040     L220X: !
18050     fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d$)
18100     let d1$=cnvrt$("pic(zzzzzzzz)",d1)
18120     let ppd=val(d1$(5:6))*10000+val(d1$(7:8))*100+val(d1$(3:4))
18140     fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
18200     ! ______________________________________________________________________
18220     let ssr1=fnss_employee*.01
18240     let ssr2=fnss_employer*.01
18260     open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
18280     read #1,using F_COMPANY: a$,ficar2,feducrat,mat statname$,ficar1,mat sucrat
18300     F_COMPANY: form pos 1,c 40,pos 133,pd 6.3,pos 145,pd 5.2,pos 150,10*c 8,pos 236,pd 3.3,pos 287,10*pd 3.3,pos 618,30*n 1,pos 648,10*c 6
18320     close #1: ioerr ignore
18340     ! 
18380     let ficar1=ficar1*.01
18400     let ficar2=ficar2*.01
18420     let ficarate=ficar1+ficar2
18440     let fnindex_it(env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno'),env$('Q')&"\PRmstr\CheckIdx2.h"&env$('cno'),"9/12/1 3/6/8")
18460     ! execute "Index "&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&","&env$('Q')&"\PRmstr\CheckIdx2.h"&env$('cno')&" 9/12/1 3/6/8,replace,DupKeys -n"
18480     open #h_payrollchecks:=fngethandle: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CheckIdx2.h"&env$('cno'),internal,input,keyed 
18500     open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
18520     open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno'),internal,outin,keyed 
18540     ! Read #2,Using 370,Rec=ADR: ENO, DEP1,LPD,TCD(1),MAT TDET,MAT HC,MCWH,MAT CP
22000     READ_CHECKS: ! 
22010     read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,dep1,prdate,ckno,mat tdc,mat cp eof L1990
22012     !  if eno=307 then pr 'eno '&str$(eno) : exe 'break other_wh' : break_is_on=1 else if break_is_on then exe 'break other_wh off' : break_is_on=0
22020     ! mcwh now in cp(3)
22040     if eno=0 and dep1=0 then goto READ_CHECKS
22060     if prdate><fndate_mmddyy_to_ccyymmdd(ppd) then goto READ_CHECKS
22080     read #2,using 'form pos 48,n 2',key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",dep1): statecode
22140     read #1,using 'form pos 9,c 30',key=lpad$(str$(eno),8): em$
22160     if det=2 then goto L580
22200     a=pos (rtrm$(em$)," ",1)
22220     b=pos (rtrm$(em$)," ",a+1)
22240     let em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a)  error ignore
22260     if dep2=0 then goto L550
22280     if dep1=dep2 then goto L580
22300     gosub PRINTDEPARTMENTTOTALS
22320     pr #255: newpage
22340     L550: ! 
22360     let deptname$=""
22380     if founddept=1 then 
22400       read #9,using "form pos 4,c 20",key=rpad$(ltrm$(str$(dep1)),3): deptname$ nokey ignore
22420     end if 
22440     gosub HDR
22460     let dep2=dep1
22480     L580: ! 
22500     let oi=cp(27)+cp(28)+cp(29)+cp(30)
22520     let t3=t4=0
22540     for j=5 to 24
22560       if newdedcode(j-4)=3 then goto L680
22580       if newdedcode(j-4)=2 then 
22582         let other_wh=other_wh-cp(j) ! if break_is_on and cp(j)<>0 then pr 'cp('&str$(j)&') deducts '&str$(cp(j))
22584       else 
22586         let other_wh=other_wh+cp(j) ! if break_is_on and cp(j)<>0 then pr 'cp('&str$(j)&')    adds '&str$(cp(j))
22588       end if 
22600       if newdedfed(j-4)=2 and newdedcode(j-4)=1 then goto L630 else goto L660
22620 L630: let t3=t3+cp(j) : let tt3=tt3+cp(j): let gtt3=gtt3+cp(j) ! cafiteria
22640 !     if client$="Washington Parrish" and j=5 then let totaldef=totaldef+cp(5) ! add deferred comp match and to later add to medicare wages
22660       goto L680
22680 L660: if newdedfed(j-4)=1 and newdedcode(j-4)=1 then goto L670 else goto L680
22700 L670: let t4=t4+cp(j) ! retirement only
22720 L680: next j
22740     let other_wh=other_wh-cp(24)
22750 !   if include_tips_in_other_wh then let other_wh+=tcp(30) ! include tips in Other Withholdings added for West Accounting on 1/18/2016
22760     let taxwg1=taxwg1+cp(31)-t3-t4
22780     let taxwg2=taxwg2+cp(31)-t3
22800     let tothc=0
22820     for j=1 to 5
22840       let hc(j)=tdc(j)
22860       let tothc=tothc+hc(j)
22880     next j
22900     for j=1 to 5
22920       let tothrs=tothrs+tdc(j)
22940       let grandtothrs=grandtothrs+tdc(j)
22960     next j
22980     let ntc=ntc+1
23000     if det=1 or det=2 then goto L760
23020     pr #255,using F_PR_LINE: dep1,eno,em$(1:11),mat hc,tothc,cp(31),cp(3),cp(2),cp(1),cp(4),other_wh,cp(32) pageoflow NWPG
23040 F_PR_LINE: form pos 1,n 4,n 8,x 2,c 12,6*n 7.2,7*n 9.2,skip 1
23060 L760: ! 
23070     let sswg=ficawag=tdet(1)
23080     let ficawage=ficawage+ficawag
23100     let totalfi=totalfi+ficawag
23120     let sswh1=sswh1+cp(2)
23140     let sswh2=sswh2+cp(2)
23160     let mcwh1=mcwh1+cp(3)
23180     let mcwh2=mcwh2+cp(3)
23200     let mcwg=tdc(8)
23220     let sswg=tdc(7)
23240 !   if client$="Washington Parrish" and mcwh>0 then let mcwg=cp(31)-t3+cp(15) ! add deferred comp match to medicare wages  (always must be misc 2 deduction)
23260     let sswg1=sswg1+sswg
23280     let sswg2=sswg2+sswg
23300     let mcwg1=mcwg1+mcwg
23320     let mcwg2=mcwg2+mcwg
23340     let stateuc+=round(tdc(10)*sucrat(statecode)*.01,2)
23360     let feduc+=round(tdc(9)*feducrat*.01,2)
23380 !   if client$="Washington Parrish" then let feducwg=tdc(9)+tcp(5): goto L870
23400     let feducwg=tdc(9)
23420 ! L870: ! 
23430     let fedwages=fedwages+feducwg
23440     let totalfuc=totalfuc+feducwg
23460 !   if client$="Washington Parrish" then let stucwg=tdc(10)+tcp(5): goto L900
23480     let stucwg=tdc(10)
23500 ! L900: ! 
23510     let stuc1(statecode)=stuc1(statecode)+stucwg
23520     let stuc2(statecode)=stuc2(statecode)+stucwg
23540     mat tcp=tcp+cp
23560     mat thc=thc+hc
23580     let statewh(statecode)=statewh(statecode)+cp(4) ! accululate state w/h by dept
23600     mat totaltcp=totaltcp+cp
23620     mat totalthc=totalthc+hc
23640     let other_wh=0
23660     goto READ_CHECKS
23680 ! ______________________________________________________________________
26000 PRINTDEPARTMENTTOTALS: ! r:
26020     let lp=lp+1
26040     let oi=tcp(17)+tcp(18)+tcp(20)+tcp(19)
26060     for j=5 to 24
26080       if newdedcode(j-4)=3 then goto L1030
26100       if newdedcode(j-4)=2 then let other_wh=other_wh-tcp(j) else let other_wh=other_wh+tcp(j)
26120 L1030: ! 
26140     next j
26160     let other_wh=other_wh-tcp(25)
26180     pr #255: "                   ________________________________________________________________________________________________________________" pageoflow NWPG
26200     pr #255,using F_PR_DTOTALS_1: " Department Totals:",thc(2),thc(4),tothrs,tcp(3),tcp(1),other_wh pageoflow NWPG
26220     let tothrs=0
26240     pr #255,using F_PR_DTOTALS_2: thc(1),thc(3),thc(5),tcp(31),tcp(2),tcp(4),tcp(32) pageoflow NWPG
26260     pr #255: "                   ================================================================================================================" pageoflow NWPG
26280 F_PR_DTOTALS_1: form pos 1,c 26,n 14.2,n 14.2,n 14.2,n 18.2,n 18.2,n 18.2,n 10.2,skip 1
26300 F_PR_DTOTALS_2: form pos 1,n 33.2,n 14.2,n 14.2,n 16.2,n 18.2,n 18.2,n 18.2,skip 1
26320     if 66-lp<26 then pr #255: newpage
26330     pr #255: ""
26332     pr #255: ""
26334     pr #255,using 'form pos 10,c 50': "Department Totals"
26340     pr #255: ""
26350     pr #255,using 'form pos 12,c 30': "Tax Expense"
26420     pr #255,using F_PR_DTOTALS_3: "Medicare ",tcp(3)
26440     if env$('client')="Thomas Richardson" or env$('client')="Kincaid" then 
26460       pr #255,using F_PR_DTOTALS_3: "SS  ",round(tcp(2)/ssr1*ssr2,2) ! show only employeer expense here
26480     else 
26500       pr #255,using F_PR_DTOTALS_3: "SS  ",tcp(2)+round(tcp(2)/ssr1*ssr2,2) ! show total AND employeer matching
26520     end if 
26540 F_PR_DTOTALS_3: form pos 20,c 10,pic(-------.##),skip 1
26560     pr #255,using F_PR_DTOTALS_3: "Fed U/C",feduc
26580     pr #255,using F_PR_DTOTALS_3: "State U/C",stateuc
26600     pr #255,using F_PR_DTOTALS_3: "     Total",round(tcp(2)/ssr1*ssr2,2)+feduc+stateuc+tcp(3) ! 2013
26620     pr #255: ""
26640     pr #255,using F_PR_DTOTALS_4: "Net Pay",tcp(32)
26660     pr #255: ""
26680     pr #255,using F_PR_DTOTALS_4: "Taxable Wages",taxwg1
26700 F_PR_DTOTALS_4: form pos 10,c 20,n 10.2,skip 1
26720     let tfw=ficawage
26740     if tfw>=tcp(31)-tt3-.1 and tfw<=tcp(31)-tt3+.1 then goto L1310
26760     goto L1320
26780 L1310: ! 
26800     let tfw=tcp(31)
26820 L1320: ! 
26840 ! pr #255,using 1100: "FICA Wages",tfw ! use same form as "Taxable wages"
26860     let tucw=fedwages
26880     if tucw>=tcp(31)-tt3-.1 and tucw<=tcp(31)-tt3+.1 then goto L1350 else goto L1360
26900 L1350: ! 
26920     let tucw=tcp(31)-tt3
26940 L1360: ! 
26960     pr #255,using F_PR_DTOTALS_4: "SS Wages",sswg1
26980     pr #255,using F_PR_DTOTALS_4: "MC Wages",mcwg1
27000     pr #255,using F_PR_DTOTALS_4: "Fed U/C Wages",tucw
27020     for j=1 to 10
27040       if stuc1(j)=0 then goto L1420
27060       pr #255,using F_PR_DTOTALS_4: statname$(j)&" UC Wages",stuc1(j)
27080 L1420: ! 
27100     next j
27120     pr #255,using F_PR_DTOTALS_4: "Payroll Tax Deposit",tcp(1)+tcp(2)+round(tcp(2)/ssr1*ssr2,2)-tcp(25)+tcp(3)*2 ! 2013
27140     pr #255,using 'form skip 1,pos 12,c 30': "Payroll Deductions"
27160     pr #255,using F_PR_DTOTALS_3: "SS-Wh",sswh1
27180     pr #255,using F_PR_DTOTALS_3: "MC-Wh",mcwh1
27200     pr #255,using F_PR_DTOTALS_3: "Fed",tcp(1)
27220     pr #255,using F_PR_DTOTALS_3: "State",tcp(4)
27240     for j=1 to 20
27260       if tcp(j+4)=0 then goto L1530
27280       if newdedcode(j)<>1 then goto L1530
27300       pr #255,using F_PR_DTOTALS_3: abbrevname$(j),tcp(j+4)
27320 L1530: ! 
27340     next j
27360     pr #255,using 'form skip 1,pos 12,c 30': "Additions to Net"
27380     for j=1 to 20
27400       if tcp(j+4)=0 or newdedcode(j)<>2 then goto L1580
27420       pr #255,using F_PR_DTOTALS_3: abbrevname$(j),tcp(j+4)
27440 L1580: ! 
27460     next j
27480     if tcp(25)=0 then goto L1610
27500     pr #255,using F_PR_DTOTALS_3: "EIC",tcp(25)
27520 L1610: ! 
27540     if final=1 then goto PRINTDEPARTMENTTOTALS_XIT
27560     let other_wh=0
27580     let o1=0
27600     mat gcp=gcp+tcp
27620 ! IF NTC=0 THEN GOTO asdf
27640 ! Let EMPMATCH=EMPMATCH+TCP(3) ! add medicare into employer match
27660 ! form pos 1,n 6
27680 ! form pos 1,n 6,n 3,35*pd 5.2,n 4
27700 ! ASDF: !
27720 !  goto L1700
27740 ! L1700: !
27760     let ntc=0 : mat tcp=(0) : let tt3=0
27780     mat thc=(0) : let ficawage=0 : let fedwages=0
27800     mat stuc1=(0) : let taxwg1=sswh1=mcwh1=sswg1=mcwg1=stateuc=feduc=0
27820     let dep=dep+1 ! count # of departments used
27840 PRINTDEPARTMENTTOTALS_XIT: ! 
27860     return  ! /r
27880 ! ______________________________________________________________________
30000 NWPG: ! r:
30020     pr #255: newpage
30040     ct1=ct1+1: pr fields "12,45,CL 5,N": str$(ct1)
30060     gosub HDR
30080     continue  ! /r
30100 ! ______________________________________________________________________
32000 HDR: ! r:
32020     pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
32040     pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
32060     pr #255: "\qc  {\f201 \fs20 \b Payroll Departmental Register}"
32080     pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zz/zz/zz)",ppd)&"}"
32100     pr #255: "\qc  {\f181 \fs16 \b "&trim$(deptname$)&"}"
32120     pr #255: "\ql   "
32140 ! pr #255,Using 1860: A$,"Payroll Register",D$,TRIM$(DEPTNAME$)
32160 ! Form SKIP 2,POS 1,CC 132,SKIP 1,POS 58,C 18,SKIP 1,POS 1,CC 132,SKIP 1,POS 1,CC 132,SKIP 2
32180 L1860: pr #255: tab(29);"<----------------Hours----------------->";
32200     pr #255: tab(71);"<-Pay->";
32220     pr #255: tab(79);"<-----------------Deductions---------------->";
32240     pr #255: tab(129);"Net"
32260     if eofcode=1 then goto L1940
32280     pr #255: "Dept";
32300     if det=1 or det=2 then goto L1940
32320     pr #255: tab(8);"Emp #  Name";
32340 L1940: pr #255: tab(29);" Reg    O/T   Sick    Vac    Hol   Total";
32360     pr #255: tab(71);"  Total   Med WH    SS WH  Federal    State    Other      Pay"
32380     let lp=6
32400     return  ! /r
32420 ! ______________________________________________________________________
34000 L1990: ! 
34020     let eofcode=1
34040     if det=2 then goto L2040
34060     gosub PRINTDEPARTMENTTOTALS
34080     if dep=1 then goto L2200 ! only 1 dept printed-no summary
34100     pr #255: newpage ! pr final departmental summary
34120 L2040: ! pr #255,Using 1840: A$,"Payroll Register",D$
34140     pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
34160     pr #255: "\qc  {\f201 \fs20 \b Payroll Register - Departmental Totals}"
34180     pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zz/zz/zz)",ppd)&"}"
34200     pr #255: "\ql   "
34220     mat tcp=totaltcp : mat thc=totalthc : let tt3=gtt3
34240     let ficawage=totalfi : let fedwages=totalfuc : mat stuc1=stuc2
34260     let sswh1=sswh2 : let mcwh1=mcwh2 : let sswg1=sswg2
34280     let mcwg1=mcwg2 : let taxwg1=taxwg2 : let tothrs=grandtothrs
34300     pr #255: ""
34320     pr #255,using 'form pos 52,c 40': "Summary for all Departments"
34340     gosub L1860
34360     let final=1
34380     gosub PRINTDEPARTMENTTOTALS
34400     pr #255,using 'form skip 1,pos 12,c 30': "State W/H Breakdown"
34420     for j=1 to 10
34440       if statewh(j)=0 then goto L2190
34460       pr #255,using F_PR_DTOTALS_4: statname$(j),statewh(j)
34480 L2190: ! 
34500     next j
36000 L2200: ! 
36020     close #1: ioerr ignore
36040     close #2: ioerr ignore
36060     close #h_payrollchecks: ioerr ignore
36070 ! fnstatus_close
36080     let fncloseprn
36100     let fnindex_it(env$('Q')&"\PRmstr\prTot.h"&env$('cno'),env$('Q')&"\PRmstr\PRTotIdx.h"&env$('cno'),"1 9")
36110 ! fnstatus_close
36120   fnend 
38000 IGNORE: continue 
40000 ! <Updateable Region: ERTN>
40020 ERTN: let fnerror(program$,err,line,act$,"xit")
40040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
40060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
40080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
40100 ERTN_EXEC_ACT: execute act$ : goto ERTN
40120 ! /region
