00080 ! r: Test The Tables!
00100    !  if env$('ACSdeveloper')<>'' then 
00120    !    fn_setup 
00130    !    fn_setupOpenFiles
00140    !    fn_test_state_calk
00160    !    end
00180    !  end if
00190 ! /r
00210 ! S:\Payroll\Calc (formerly) S:\acsPR\newprCalk
00220 ! Payroll Calculation
00230 ! if you accidentally renumber it, be sure and list 'every year' and make sure the line numbers still match up
00232 ! sswh - social security withholding
00234 ! mcwh - medicare withholding
00236 ! MinHourlyWage -  minumum hourly wage
00238 ! g_pay_periods_per_year used to be t6
00240 ! twy - total wages yearToDate
00242 ! gdp - gross pay for department
00244   fn_setup
00250   fntop(program$,cap$="Payroll Calculation")
00525   gosub ASKDATES
00527   if ckey=5 then goto XIT
00530   dat$=lpad$(str$(d1),6)
00535   mo1=val(dat$(5:6)) : da=val(dat$(7:8)) : yr=val(dat$(3:4))
00540   ppd=round(yr*365+int(yr/4)+motab(mo1)+da,2)
00545   d1=mo1*10000+da*100+yr
00550   gosub ASKSKIPWH
00560   if ckey=5 then goto XIT
00562   fnAutomatedSavePoint('before')
00564   fn_setupOpenFiles
00567   ReadRpWork: ! 
00568   read #h_rpwork,using F_RPWORK: x$,dep,mat inp,gpd,mat hr eof EO_RPWORK
00569   if env$('client')='West Accounting' or env$('client')='Payroll Done Right' then gosub WEST_ACC_WORKMANSCOMP
00571   ! pr 'FIRST READ OF RPWORK right after read rpwork inp(6)=';inp(6) : pause
00572   newdeptkey$=cnvrt$("pic(zzzzzzz#)",val(x$))&cnvrt$("pic(zz#)",dep)
00573   ! totaldef=0
00575   ! Form POS 1,C 8,N 3,5*PD 4.2,15*PD 5.2,2*PD 4.2,PD 3
00577   eno=val(x$)
00579   if eno=0 then goto ReadRpWork
00581   if n$=x$ then goto L1540
00583   twc=twy=tfy=cafy=eicytd=deducy=0
00585   if rtrm$(n$)<>"" then gosub SUBROUTINE2
00589 goto L1010
00591 ! ______________________________________________________________________
00593 SUBROUTINE2: ! r: (reallocate state taxes based on earnings by dept and state
00595   s3=0 : tcp(4)=0 : tcp4=0
00597   oldeno=val(n$)
00599   restore #h_department,key>=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",0): 
00601   if em(14)=-1 then goto L960
00603   ! Read #h_department,Using 610,Rec=TRA: tdt(4),TCD(1),ty4,tqm4,tcp4,tcp31,TCP22,NTA,MAT DST
00670   L670: ! 
00672   read #h_department,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet eof L960
00674   if debug then let fnStatus('department read employee '&str$(eno)&' department '&str$(tdn))
00680   if teno<>oldeno then goto L960
00690   if d1><tdt(4) then goto L670
00700   holdtdn=tdn
00710   olddeptkey$=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",holdtdn)
00720   read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2",key=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",tdn)&cnvrt$("pd 6",prd): heno,tdn,prdate,ckno,mat tdc,mat tcp nokey L670
00722   if debug then let fnStatus('read check history: heno='&str$(heno)&',tdn='&str$(tdn)&',prdate='&str$(prdate)&',ckno='&str$(ckno)&'...')
00740   dst3=0
00750   for j=1 to 20
00760     if dedst(j)>0 then dst3=dst3+tcp(j+4)
00770   next j
00780   ! sTWH(tcd(1),1)=STWH(tcd(1),1)-DST3
00790   if stwh(tcd(1),1)=0 then goto L670
00800   if stwh(tcd(1),2)><0 then goto L870
00810   if em(14)=0 then goto L840
00820   if in2$(4)<>"Y" then stwh(tcd(1),2)=em(14)
00830   goto L870
00840   L840: ! 
00842   if in2$(2)="Y" then s3=0: goto L860
00850   on tcd(1) gosub ST01,ST02,ST03,ST04,ST05,ST06,ST07,ST08,ST09,ST10
00860   L860: ! 
00862   stwh(tcd(1),2)=s3
00870   L870: ! 
00872   if env$('client')="Lamar" then 
00873     tcp4=(stwh(tcd(1),2))*((tcp(31)-dst3)/stwh(tcd(1),1))
00874   else 
00875     tcp4=(stwh(tcd(1),2)+em(15))*((tcp(31)-dst3)/stwh(tcd(1),1))
00876   end if 
00880   tcp4=round(tcp4,2)
00890   if in2$(2)="Y" then tcp4=0
00900   tcp(32)-=tcp4: tcp(4)=tcp4
00920   rewritekey$=cnvrt$("pic(zzzzzzz#)",oldeno)&cnvrt$("pic(zz#)",holdtdn)&cnvrt$("pd 6",prd) ! index employee#,department# and payroll date
00930   rewrite #h_payrollchecks,using 'Form pos 80,pd 5.2,poS 220,pd 5.2',key=rewritekey$: tcp(4),tcp(32)
00936   fn_report_stuff
00940   rewrite #h_department,using 'Form pos 42,n 6',key=olddeptkey$: tdt(4)
00952   goto L670
00960   L960: ! 
00962   rewrite #hEmployee,using F_RPMSTR,key=n$: mat em,d1,tgp
00970   if fp(d1*.01)>.9 then hd1=19000000+fncd(d1) else hd1=20000000+fncd(d1)
00980   mat stwh=(0)
00990 return ! /r
01010 L1010: ! r: read employee, call calc deduction etc  basically beginning of main loop i think
01012   read #hEmployee,using F_RPMSTR,key=x$: mat em,lpd,tgp nokey EMPLOYEE_NOT_FOUND
01014   F_RPMSTR: form pos 112,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2
01020   gosub CALK_ALL_DEDUCTIONS_ALL_DEPT
01040   n$=x$
01050   if d3$><"Y" then goto L1170 ! Accrue Sick and Vacation
01060   if em(8)><-1 then goto L1110 ! Check for elgibility
01070   if em(16)<10100 or em(16)>123199 then goto L1110
01080   dat$=lpad$(str$(em(16)),6) : mo=val(dat$(1:2))
01090   da=val(dat$(3:4)) : yr=val(dat$(5:6))
01100   dh=round(yr*365+int(yr/4)+motab(mo)+da,2)
01110   if ppd-dh<sck(1) then goto L1110
01120   em(8)=sck(3) : em(10)=sck(2)
01125 L1110: ! 
01126   if em(8)>0 then em(10)+=em(8) ! Accrue Sick
01130   ! if env$('client')='Battlefield' then goto L1140
01135   if em(8)>0 then write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Sick",prd,em(8),0 ioerr ignore
01140   ! L1140: ! 
01142   if em(9)>0 then em(11)+=em(9) ! Accrue Vacation
01150   ! if env$('client')='Battlefield' then goto L1170
01160   if em(9)>0 then write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Vac",prd,em(9),0 ioerr ignore
01170   L1170: ! 
01172   twy=0
01180   tf4_a=0 ! Calculate Total Federal WithHoldings
01190   fed_wh_annual_estimate=0
01200 ! IF in2$(1)="Y" THEN GOTO 1420
01202   ! if em(1)=2 then j2=4 else j2=round(1+em(1)*3,2) ! 2=HEAD OF HOUSEHOLD
01204   if em(1)=0 then ! 0 - Single
01210     j2=1
01212   else if em(1)=1 then ! 1 - Married
01214     j2=4
01216   else if em(1)=2 then ! 2 - Single - Head of Household
01218     j2=4
01220   else if em(1)=3 then ! 3 - Married - filing joint return - only one working
01222     j2=4
01224   else if em(1)=4 then ! 4 - Married - filing joint - both working
01225     j2=4
01226   else if em(1)=5 then ! 5 - Married - filing seperate - both working
01227     j2=4
01228   end if
01240   on em(5) goto PAYCODE_1,PAYCODE_2,PAYCODE_3,PAYCODE_4 none BAD_PAY_CODE
01248 ! /r
01250 BAD_PAY_CODE: ! r:
01252   mat ml$(1)
01254   ml$(1)="Incorrect Pay Code "&str$(em(5))&" on Employee Number "&trim$(x$)&". Did not calculate pay on this Employee"
01256   fnmsgbox(mat ml$,resp$,cap$,0)
01270 goto ReadRpWork ! /r
01280 ! ______________________________________________________________________
01285 PAYCODE_1: t6=12 : g_pay_periods_per_year=12 : goto PAST_PAYCODE
01287 PAYCODE_2: t6=24 : g_pay_periods_per_year=24 : goto PAST_PAYCODE
01290 PAYCODE_3: t6=26 : g_pay_periods_per_year=26 : goto PAST_PAYCODE
01300 PAYCODE_4: t6=52 : g_pay_periods_per_year=52
01310 PAST_PAYCODE: ! r: continues here
01311 ! pr '@ PAST_PAYCODE' : pause
01312   if in2$(1)="Y" then goto L1470
01320   if fedpct>0 then 
01330     tf4_a=round((tgp-ded)*fedpct,2) : fed_wh_annual_estimate=tf4_a
01340     goto L1470
01350   end if 
01360   if em(12)=0 then goto L1370
01370   tf4_a=0
01380   if em(12)=-1 then goto L1470
01390   tf4_a=em(12) : goto L1470
01395   L1370: ! 
01400   t2=round(em(2)*(fed_annual_wh_allowance/g_pay_periods_per_year),2) ! this is one of the lines that change every year (line 1240)
01405   g2=tgp-t2-ded
01410   if g2>0 then 
01425     g2=round(g2*g_pay_periods_per_year,2) ! g2 - becomes estimated annual net pay
01430     j1=fn_table_line(mat ft,g2, j2)
01460     fed_wh_annual_estimate=tf4_a=round(ft(j1,j2+1)+(g2-ft(j1,j2))*ft(j1,j2+2),2)
01465     ! table total federal w/h used in some state routines
01470     tf4_a=round(tf4_a/g_pay_periods_per_year,2)
01475   else 
01480     g2=0
01482   end if 
01485   if in2$(1)><"Y" then tf4_a=tf4_a+em(13)
01487   L1470: ! 
01488   mat stuc=(0)
01489   read #h_department,using "form pos 48,n 2",key=newdeptkey$: tcd(1) ! get state code
01491   gosub DETERMINE_EARNINGS
01493   for j=1 to 20
01495     if newdedfed(j)=2 and newdedcode(j)=1 then 
01497       cafy+=caf(j)
01499       cafd+=caf(j)
01501     end if 
01503   next j
01505   twy+=twd : tfy+=(ytdFICA+tmd) : ficatfy=tfy
01507   oldsswg=twy-cafy : eicytd+=td14 : stuc(tcd(1))+=twd-cafd
01509   cafd=0
01540   L1540: ! 
01542   read #h_department,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=newdeptkey$: teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet ! Nokey X
01550   if tgp=0 then pog=1: goto L1620 ! Allow checks to calculate with no gross pay
01560   if tgp=gpd then pog=1 : goto L1620
01570   if tgp<>0 then goto L1610
01572   mat ml$(1)
01574   ml$(1)="Employee Number "&trim$(x$)&" skipped Total Gross Pay = 0, Must be Re-entered"
01576   fnmsgbox(mat ml$,resp$,cap$,0)
01600   goto ReadRpWork
01610   L1610: !
01612   pog=gpd/tgp
01620   L1620: ! 
01622   for j=1 to 20
01630     if env$('client')="Franklinton" then 
01632       if j=1 and em(4)=3 then ! retirement of firemen  ! franklinton
01634         inp(j+7)=round(inp(j+7)*gpd/100,2)
01650         goto L1710 ! franklinton
01652       else 
01654         if j=2 then ! retirement of police !franklinton
01656           inp(j+7)=round(inp(j+7)*((hr(1)*(inp(1)+inp(3)+inp(4))+inp(6)+inp(17))/100),2)
01660           goto L1710 ! franklinton
01662         end if 
01664       end if 
01666       !   else if env$('client')="Washington Parrish" and j=3 and newcalcode(j)=2 then
01668       !     inp(j+7)=round(inp(j+9)*(gpd+defcompmatch)/100,2)
01670       !     goto L1700
01672       !   else if env$('client')="West Accounting" and j=10 and inp(17)<>0 then
01673       !     gosub WEST_ACC_WORKMANSCOMP
01678     end if 
01690     if newcalcode(j)=2 then inp(j+9)=round(inp(j+9)*gpd/100,2)
01700   ! L1700: ! 
01702     if in2$(4)="Y" then inp(j+9)=0
01710   L1710: ! 
01712   next j
01720   em(10)-=inp(3) : em(11)-=inp(4)
01730   ! if env$('client')='Battlefield' then goto L1760
01740   if inp(3)>0 then ! write sick hours taken to breakdown file
01742     write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Sick",prd,0,inp(3) ioerr ignore
01744   end if 
01750   if inp(4)>0 then ! write vacation hours taken to breakdown file
01752     write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Vac",prd,0,inp(4) ioerr ignore
01754   end if 
01760   ! L1760: ! 
01762   if inp(5)>0 then ! write holiday hours taken to breakdown file
01764     write #breakdown,using "Form pos 1,n 8,c 5,n 8,2*n 9.2": eno,"Hol",prd,0,inp(5) ioerr ignore
01766   end if 
01772   if sck(4)=999 then sck(4)=1000 ! system will only hold 999 maximum accrued sick hours.  If maximum is set at 999, assume no maximum
01780   if sck(4)<>0 and em(10)>sck(4) then em(10)=sck(4)
01790   if vacm<>0 and em(11)>vacm then em(11)=vacm
01800   ext=0 ! Excess Tips
01810   goto NO_EXCESS_TIPS
01812 ! ______________________________________________________________________
01814   if inp(9)=0 then 
01816     goto NO_EXCESS_TIPS
01818   else 
01820     tr=round(inp(1)*MinHourlyWage+inp(2)*MinHourlyWage*1.5,2)
01821     g1=gpd-inp(9)
01822     ext=0
01823     if g1>=tr then 
01824       g2=inp(9)
01826     else 
01828       g2=gpd-tr
01830     end if 
01832   end if 
01840 NO_EXCESS_TIPS: ! 
01842   deduc=ficat3=f3=0 ! FICA
01850   for j=1 to 20
01860     if dedfica(j)=1 and newdedcode(j)=1 then ficat3+=inp(j+9)
01870     if deduc(j)=1 then deduc+=inp(j+9): deducy+=caf(j) ! total deductions for unemployment for current period and year to date
01880   next j
01890   sswg=sswh=mcwh=0
01900   if tgp=0 then f3=0: goto CALC_NO_GROSS ! calculate checks w/ no gross pay
01910   if in2$(3)="Y" then goto FEDWH_DEPT
01920   on em(6)+1 goto L1930,SS_TAX_ONLY,L3240 none FEDWH_DEPT
01930 ! ______________________________________________________________________
01935 L1930: ! 
01940 ! if env$('client')="Washington Parrish" then
01941 !   tf0=tgp-t3+totaldef ! add deferred in taxable wages for washington parrish
01942 !   goto L1950
01944 ! end if
01945   tf0=tgp-t3 ! if over both max else if over both max this time else if over max-1
01950 ! L1950: !
01951   if ficatfy>=ficamxr+ficamx2 then 
01952     goto FICAEND
01953   else if (ficatfy-ficamxr)+(tf0*ficar2)>=ficamx2 then 
01954     mcwh=ficamxr+ficamx2-ficatfy
01955     goto FICAEND
01956   else if ficatfy>=ficamxr then 
01957     mcwh=tf0*ficar2 : sswg=0
01958     goto FICAEND
01959   end if 
01960 ! if went over first max this time else Under 1st Max 
01961   if ficatfy+(tf0*ficarate)>=ficamxr then 
01962     tf1=ficamax-ficatfy/ficarate : tf2=tgp-t3 
01963     sswh=(tf1*ficar1) 
01964     mcwh=(tf2*ficar2) 
01965     sswg=tf1 
01966   else 
01967     sswh=tf0*ficar1 
01968     mcwh=tf0*ficar2
01969     sswg=tf0
01970   end if
01971 FICAEND: ! 
01980   if sswg>ficamax-oldsswg-.10 and sswg<ficamax-oldsswg+.10 then 
01982     sswg=ficamax-oldsswg
01984   end if
01990   if tgp-t3>0 then 
01992     ficapog=((gpd-ficat3)/(tgp-t3)) 
01994   else 
01996     ficapog=1
01998   end if
02000   sswh=round(sswh*ficapog,2) 
02002   mcwh=round(mcwh*ficapog,2) 
02004   f3=sswh+mcwh : oldsswg+=sswg
02010 CALC_NO_GROSS: tfy+=f3
02020 FEDWH_DEPT: ! Fed WH for Dept ! Federal Withholding for Department
02021   if debug then let fnStatus('federal  withholding for department calculating')
02022   f4=round(tf4_a*pog,2)
02030   stwh(tcd(1),1)+=gpd : eic4=0 ! Calculate EIC
02040   if em(7)=0 then goto CURRENT_PERIOD else g2=tgp
02050   eic1=round(8970/em(7)/g_pay_periods_per_year,2)                ! this is one of the lines that change every year (formerly line 1800)
02060   eic2=round(16450/em(7)/g_pay_periods_per_year,2)               ! this is one of the lines that change every year (formerly line 1810)
02070   eic3=round(1830/em(7)/g_pay_periods_per_year,2)                ! this is one of the lines that change every year (formerly line 1820)
02080   if g2<=eic1 then eic4=round(tgp*.2040,2)
02090   if g2>eic1 and g2<=eic2 then eic4=eic3
02100   if g2>eic2 then eic4=eic3-(tgp-eic2)*.09588
02110   if ytdTotal(25)+eic4<0 then eic4=-ytdTotal(25)
02120   eic4=round(eic4*pog,2)
02130 CURRENT_PERIOD: ! 
02132   tcp(1)=f4 : tcp(2)=sswh : tcp(3)=mcwh: tcp(4)=tcp4
02140   for j=5 to 24
02144     tcp(j)=inp(j+5)
02148   next j
02150   tcp(25)=min(eic4,tcp(1))
02152   tcp(27)=round(inp(2)*hr(2),2)
02160   tcp(28)=inp(7)
02161   tcp(29)=inp(8)
02162   tcp(30)=inp(9)
02163   tcp(26)=gpd-tcp(27)-tcp(28)-tcp(29)-tcp(30)
02164   tcp(31)=gpd
02166   tcp(32)=gpd-tcp(1)-tcp(2)-tcp(3) ! -TCP(4)
02170   for j=5 to 24
02180     if newdedcode(j-4)=3 then goto L2200
02190     if newdedcode(j-4)=2 then tcp(32)+=tcp(j) else tcp(32)-=tcp(j)
02200 L2200: ! 
02202   next j
02210   for j=1 to 31 : tcp(j)=round(tcp(j),2): next j
02220   tcp(32)+=tcp(25)-tcp(29)-tcp(30)
02230 ! if env$('client')="Washington Parrish" then tcp(32)=tcp(32)+tcp(30) ! add tips which is really an other compensation back to net
02240 ! the following commented lines may have to be put back in and the tdet array extended to hold them  ???  kj
02250 ! SS_WAGE: !
02260   if em(6)=9 then 
02262     tdc(7)=0
02264   else 
02266     tdc(7)=round(sswg*ficapog,2)
02268   end if 
02270 ! MEDICARE_WAGE: !
02280   tdc(8)=round((tgp-t3)*ficapog,2)
02290   tdc(10)=0 ! State U/C Wage
02300 ! if stuc(tcd(1))>=sucw(tcd(1)) then goto L2300
02305 ! L2300: !
02306   if stuc(tcd(1))+(gpd-ext-deduc)>sucw(tcd(1)) then 
02307     tdc(10)=sucw(tcd(1))-stuc(tcd(1))
02308   else 
02309     tdc(10)=gpd-ext-deduc
02310   end if 
02311   if tdc(10)<0 then tdc(10)=0 ! if don't have maximum uc wage in company it will come up with negatives
02315   tdc(9)=0 ! Fed U/C Wage
02320   if feducmax=0 then goto FEDERAL_UC_WAGE
02325   if twy-deducy>=feducmax then goto L2370
02330   if twy-deducy+(gpd-ext-deduc)>feducmax then goto FEDERAL_UC_WAGE
02335 FEDERAL_UC_WAGE: ! 
02336   tdc(9)=gpd-ext-deduc
02338   goto L2370
02370 L2370: ! 
02372   tdc(9)=min(max(feducmax-(twy-deducy),0),gpd-ext-deduc)
02380   for j=1 to 5 : tdc(j)=inp(j) : next j ! Hours
02382 ! pause ! WORKMANS_COMP: !
02384 ! em(5) is paycode
02386 ! mat wcm is workman's comp maximum
02388 ! trp is (temp variable only used here)
02390 ! tcp(26) is Regular Earnings
02392 ! tcp(27) is OT Earnings
02394 ! wc is (temp variable only used here)
02396 ! tdc(6) is Workman's Comp Wages
02397 ! if env$('client')="West Accounting" then ! perhaps everyone should be doing it this way -prd 01/06/2016
02398 !   tcp(14)=tdc(1)*inp(19)*.01 ! base on regular hours times w/c rate
02400 !   fnStatus('tcp(14) was set to '&str$(tcp(14))&' by tcp(14) = tdc(1)('&str$(tdc(1))&' * inp(19)('&str$(inp(19))&') * .01')
02401 !   inp(19)=0   ! <-- nice idea but it does not make a difference
02402 !   fnStatusPause
02404 ! end if  ! else 
02406   trp=tcp(26)+tcp(27) ! base on wages
02408 ! end if
02410   wc=0
02420   if wcm(em(5))=0 or twc+trp<wcm(em(5)) then 
02421     wc=trp
02422   else 
02423     wc=wcm(em(5))-twc
02424   end if 
02430   twc=twc+wc : tdc(6)=wc
02440   rewrite #h_department,using "form pos 42,n 6,pos 58,23*pd 4.2",key=newdeptkey$: d1,mat tdet
02460   tcp(4)=0
02462   write #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,tdn,prd,0,mat tdc,mat tcp
02463 ! fnStatus('WRITING payroll check with tcp(4)='&str$(tcp(4))&' and tcp(32)='&str$(tcp(32)))
02464 ! fnStatusPause
02470   twy+=gpd : cafy+=ficat3 : eicytd+=ytdTotal(25)
02480   if tdet(16)<>0 then stuc(tcd(1))+=tdet(16) ! ??? kj
02490   goto ReadRpWork
02500 ! /r
02502 EMPLOYEE_NOT_FOUND: ! r:
02504   n$=" "
02506   mat ml$(1)
02508   ml$(1)="Employee Number "&x$&" is not on file. No check calculated."
02510   fnmsgbox(mat ml$,resp$,cap$,0)
02514   goto ReadRpWork
02518 ! /r
02520 EO_RPWORK: ! r:
02522   if rtrm$(n$)<>"" then gosub SUBROUTINE2
02530   close #hEmployee: 
02540   close #h_department: 
02550   close #h_rpwork: ! ,Free:
02570   fnFree(env$('Q')&"\PRmstr\jcprh1.h"&env$('cno')) ! get rid of jobcost time entry file if exists
02580   goto XIT ! /r
02590 XIT: fnxit
02600 IGNORE: continue 
02840 CALK_ALL_DEDUCTIONS_ALL_DEPT: ! r:
02842 ! Calculate all deduct for federal for all departments
02850   tgp=t3=ded=0
03020 L3020: ! 
03022   for j=1 to 20
03024     if (j+9)=17 and (env$('client')='West Accounting' or env$('client')='Payroll Done Right') then goto L3090 ! if processing inp(17) SKIP IT do not process it.
03030     if newdedfed(j)>=1 and newdedcode(j)=1 then 
03032       gosub SUBROUTINE6
03034     else 
03036       goto L3060
03038     end if 
03050     if newcalcode(j)=1 then 
03052       ded=ded+inp(j+9)
03054     else 
03056       ded=ded+inp(j+9)*gpd/100
03058     end if 
03060 L3060: ! 
03062     if newdedfed(j)><2 then goto L3090
03078     if newcalcode(j)=1 then 
03080       t3=t3+inp(j+9)
03082     else 
03084       t3=t3+inp(j+9)*gpd/100
03086     end if 
03090 L3090: ! 
03092   next j
03110   tgp=tgp+gpd
03130   read #h_rpwork,using F_RPWORK: newx$,newdep,mat inp,gpd,mat hr eof L3150
03132   if env$('client')='West Accounting' or env$('client')='Payroll Done Right' then gosub WEST_ACC_WORKMANSCOMP
03133 ! pr 'A right after read rpwork inp(6)=';inp(6) : pause
03140   if newx$=x$ then goto L3020
03150 L3150: ! 
03152   workkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",dep)
03160   restore #h_rpwork,key>=workkey$: 
03170   read #h_rpwork,using F_RPWORK: x$,dep,mat inp,gpd,mat hr eof EO_RPWORK
03172   if env$('client')='West Accounting' or env$('client')='Payroll Done Right' then gosub WEST_ACC_WORKMANSCOMP !  11/14/2017 - env$('client')='Payroll Done Right'  Does not want any special processing for deduction 8
03174 ! pr 'B right after read rpwork  inp(6)=';inp(6) : pause
03180   return  ! /r
03200 SS_TAX_ONLY: ! r: SOC-SEC-TAX ONLY
03202   tf0=tgp-t3
03210   if ficatfy>=ssmax then 
03212     sswg=0
03214     goto FICAEND ! OVER MAX
03216   end if 
03220   if ficatfy+tf0>=ssmax then ! WENT OVER MAX THIS TIME
03222     sswh=(ssmax-ficatfy)*ficar1
03224     sswg=ssmax-ficatfy
03226     goto FICAEND
03228   end if 
03230   sswh=tf0*ficar1
03234   sswg=tf0
03238   goto FICAEND ! UNDER MAX /r
03240 L3240: ! r: MEDICARE-TAX ONLY??
03242   ! if env$('client')="Washington Parrish" then ! MEDICARE-TAX ONLY  (add deferred comp match to medicare wages)
03244   !   tf0=tgp-t3+totaldef
03246   !   goto L3260
03248   ! end if
03250   tf0=tgp-t3 ! MEDICARE-TAX ONLY
03260   ! L3260: ! 
03262   if ficatfy>=mcmax then goto FICAEND ! OVER MAX
03270   if ficatfy+tf0>=mcmax then ! Went over max this time
03272     mcwh=(mcmax-ficatfy)*ficar2
03274     goto FICAEND
03276   end if 
03280   mcwh=tf0*ficar2
03282 goto FICAEND ! UNDER MAX  /r
03290 SUBROUTINE6: ! r:
03292   sc1=1
03294   read #h_department,using 'form pos 48,n 2',key=newdeptkey$: sc1 nokey ignore
03300   if sc1=0 then sc1=1
03320   ! If env$('client')="Washington Parrish" AND J=3 Then sD3=INP(J+9)*(GPD+DEFCOMPMATCH)/100 : Goto 3150 ! add deferred comp to gross for calculating pension deduction
03330   if newcalcode(j)=1 then sd3=inp(j+9) else sd3=inp(j+9)*gpd/100
03340   stwh(sc1,1)=stwh(sc1,1)-sd3
03350 return  ! /r
03360 ! ______________________________________________________________________
03370 ASKSKIPWH: ! r:
03380   fnTos(sn$="Skipdeductions")
03382   rc=cf=0: mylen=42: mypos=45
03390   fnChk(1,46,"Skip Federal Withholdings:",1)
03392   resp$(rc+=1)="False"
03400   fnChk(2,46,"Skip State Withholdings:",1)
03402   resp$(rc+=1)="False"
03410   fnChk(3,46,"Skip Fica Withholdings:",1)
03412   resp$(rc+=1)="False"
03420   fnChk(4,46,"Skip Standard Withholdings:",1)
03422   resp$(rc+=1)="False"
03430   fnLbl(6,1,"Standard Federal % Override:",mylen,1,0)
03440   fnTxt(6,mypos,4,0,1,"32",0,"Normally zero. The government allows you to use a standard percent on bonuses, etc. See Circular E for allowable %.")
03442   resp$(rc+=1)=""
03450   fnCmdKey("Next",1,1,0,"Proceed with calculations.")
03460   fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
03470   fnAcs(sn$,0,mat resp$,ckey) ! skip deductions & std %
03480   if ckey<>5 then 
03490     if resp$(1)(1:1)="T" then in2$(1)="Y" else in2$(1)="N"
03500     if resp$(2)(1:1)="T" then in2$(2)="Y" else in2$(2)="N"
03510     if resp$(3)(1:1)="T" then in2$(3)="Y" else in2$(3)="N"
03520     if resp$(4)(1:1)="T" then in2$(4)="Y" else in2$(4)="N"
03530     fedpct=val(resp$(5)) ! federal wh percent
03532   end if
03540 return  ! /r
03580 ERTN: fnerror(program$,err,line,act$,"NO") ! r:
03590   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03610   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03620 ERTN_EXEC_ACT: execute act$ : goto ERTN ! /r
08000 ILWH: ! r: REPLACE ACSWRK\ILLINOIS.WH,SOURCE ! ILLINOIS   NO TABLE
08020   ! line 1 allowances = +1 for claiming self, +1 for claiming spouse
08040   ! line 2 allowances = +1 for each other (not you nor spouse) dependent
08060   ! em(3) - number of allowances
08080   ! g_pay_periods_per_year = number of pay periods (formerly b8)
08100   g2=round((stwh(tcd(1),1))*g_pay_periods_per_year,2)
08120   !  new way needs awesome function !    allowances_line_1=fn_allowances_spouse_and_self
08140   !  new way needs awesome function !    allowances_line_2=em(3)-allowances_line_1
08160   !  new way needs awesome function !    g2=g2-(allowances_line_1*2175+allowances_line_2*1000)
08180   g2=g2-1000*em(3)
08200   s3=g2*.0495 ! changed from .0375 on 7/10/17  ! changed from .03 to .05 1/1/11, changed from .05 to .0375 1/1/15, ok as of 1/6/16
08220   s3=round(s3/g_pay_periods_per_year,2)
08240   if s3<.1 then s3=0 ! do not withhold less than 10 cents.
08260 return  ! /r
09000 MOWH: ! r: REPLACE ACSWRK\MISSOURI.WH,SOURCE ! MISSOURI MO(10,3) REC # 28  REVISED 1/1/2002
09020   if ~setup_mowh then  ! r: MO Missouri
09040     setup_mowh=1
09060     dim mo(10,3)
09080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=28: mat mo ! Missouri
09100     mo( 1,1)=   0 : mo( 1,2)=  0  : mo( 1,3)=0.015
09120     mo( 2,1)=1000 : mo( 2,2)= 15  : mo( 2,3)=0.02
09140     mo( 3,1)=2000 : mo( 3,2)= 35  : mo( 3,3)=0.025
09160     mo( 4,1)=3000 : mo( 4,2)= 60  : mo( 4,3)=0.03
09180     mo( 5,1)=4000 : mo( 5,2)= 90  : mo( 5,3)=0.035
09200     mo( 6,1)=5000 : mo( 6,2)=125  : mo( 6,3)=0.04
09220     mo( 7,1)=6000 : mo( 7,2)=165  : mo( 7,3)=0.045
09240     mo( 8,1)=7000 : mo( 8,2)=210  : mo( 8,3)=0.05
09260     mo( 9,1)=8000 : mo( 9,2)=260  : mo( 9,3)=0.055
09280     mo(10,1)=9000 : mo(10,2)=315  : mo(10,3)=0.06
09300   end if ! /r
09320   ! MARITAL STATUS =2 IF HEAD OF HOUSEHOLD
09340   numb4=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
09360   if em(1)=0 or em(1)=2 then numb6=min(5000,fed_wh_annual_estimate) ! FEDERAL DED LIMITED TO 5000 FOR SINGLE
09380   if em(1)<>0 then numb6=min(10000,fed_wh_annual_estimate) ! FEDERAL DED LIMITED TO 10000 FOR MARRIED OR HEAD OF HOUSEHOLD
09400   if em(1)=1 or em(1)=3 or em(1)=4 or em(1)=5 then h1=3925 : goto L4110
09420   if em(1)=2 then h1=7850 : goto L4110
09440   h1=4700
09460   goto L4110
09480   L4110: ! 
09500   h2=0
09510   ! on em(1)+1 goto L4160,L4140,L4180 none L4190
09520   if em(3)<>0 then 
09530     !
09540     if em(1)=0 then 
09550       h2=1200+(em(3)-1)*1200 ! SINGLE
09560     else if em(1)=1 or em(1)=3 or em(1)=4 or em(1)=5 then 
09570       h2=min(em(3),2)*1200+max(em(3)-2,0)*1200 ! MARRIED
09580     else if em(1)=2 then 
09590       h2=3500+max(em(3)-4,0)*1200 ! HEAD OF HOUSE HOLD
09600     end if
09610   end if
09700   h3=numb4-h1-h2-numb6
09720   if h3<0 then h3=0
09740   j1=fn_table_line(mat mo,h3)
09860   s3=(mo(j1,2)+(h3-mo(j1,1))*mo(j1,3))/g_pay_periods_per_year
09880   s3=round(s3,0)
09900   if s3<.1 then s3=0
09920 return  ! /r
10000 ARWH: ! r: REPLACE ACSWRK\ARKANSAS.WH,SOURCE ! Arkansas #5 ar(7,3)  REVISED 7/01/91
10020   if ~setup_arwh then ! r: setup AR Arkansas
10040     dim ar(6,3) ! ar(7,3)
10060     setup_arwh=1
10080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=5: mat ar ! Arkansas
10100     ! Page 1 of http://www.dfa.arkansas.gov/offices/incomeTax/withholding/Documents/whformula.pdf
10120     ! over                              Percentage
10140     ar(1,1)=    0 : ar(1,2)=   0    :  ar(1,3)=0.009
10160     ar(2,1)= 4300 : ar(2,2)=  38.7  :  ar(2,3)=0.024
10180     ar(3,1)= 8400 : ar(3,2)= 137.1  :  ar(3,3)=0.034
10200     ar(4,1)=12600 : ar(4,2)= 279.9  :  ar(4,3)=0.044
10220     ar(5,1)=21000 : ar(5,2)= 649.5  :  ar(5,3)=0.059
10240     ar(6,1)=35100 : ar(6,2)=1481.4  :  ar(6,3)=0.069
10260   end if ! /r
10280   t1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
10300   t2=2000
10320   t3=t1-t2
10340   j1=fn_table_line(mat ar,t3)
10360   s1=round(ar(j1,2)+(t3-ar(j1,1))*ar(j1,3),2)
10380   s2=em(3)*20
10400   s3=round((s1-s2)/g_pay_periods_per_year,2)
10420   if s3<.1 then s3=0
10440 return  ! /r
11000 AZWH: ! r: REPLACE ACSWRK\ARIZONA.WH,SOURCE ! ARIZONA:  NO TABLE  REVISED 1/01/10
11020   ! effective june 30, 2010 the rates changed and also the base change from a percent of federal wh to a percent of total taxable wages
11040   stp=0
11060   if em(3)=1 then stp=.013
11080   if em(3)=2 then stp=.018
11100   if em(3)=3 then stp=.027
11120   if em(3)=4 then stp=.036
11140   if em(3)=5 then stp=.042
11160   if em(3)=6 then stp=.0510
11180   s3=round(stwh(tcd(1),1)*stp,2)
11200   h3=min(h3,1200)
11220 return  ! /r
12000 MSWH: ! r: REPLACE ACSWRK\MISISIPI.WH,SOURCE ! MISSISSIPPI  NO TABLE
12020   ! **********  REMOVE THE EM(15) FROM LINE 740 **********
12040   ! SUBSTITUTE THE EXEMPTIONS INTO THE FIELD NOW CALLED STATE TAX ADD-ON
12060   ! THE EXEMPTIONS MUST BE ENTERED IN DOLLARS AND THE STANDARD DEDUCTION
12080   ! MUST BE ADDED TO THE EXEMPTIONS.
12100   ! SINGLE =2300, MARRIED=3400, MARRIED BOTH WORKING=1700
12120   h1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
12140   h3=h1-em(15)
12160   if h3<=0 then s3=0 : goto L4481
12180   if h3<10000 then goto L4474
12200   s3=350+.05*(h3-10000)
12220   goto L4481
12240   L4474: if h3>0 and h3<=5000 then goto L4477
12260   s3=150+.04*(h3-5000)
12280   goto L4481
12300   L4477: s3=.03*h3
12320   if s3<.1 then s3=0
12340   goto L4481
12360   L4481: s3=s3/g_pay_periods_per_year
12380   s3=round(s3,2)
12400   if s3<.1 then s3=0
12420 return  ! /r
13000 OKWH: ! r:  ACSWRK\OKLAHOMA.WH,SOURCE ! rec=39 ok(8,6) REV. 1/01/07 (table change also!)
13020   if ~setup_okwh then ! r: OK Oklahoma
13040     setup_okwh=1
13060     dim ok(8,6)
13080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=39: mat ok ! Oklahoma
13100     ! r: single 
13120     ok(1,1)=    0 : ok(1,2)=   0   : ok(1,3)=0
13140     ok(2,1)= 6300 : ok(2,2)=   0   : ok(2,3)=0.005
13160     ok(3,1)= 7300 : ok(3,2)=   5   : ok(3,3)=0.01
13180     ok(4,1)= 8800 : ok(4,2)=  20   : ok(4,3)=0.02
13200     ok(5,1)=10050 : ok(5,2)=  45   : ok(5,3)=0.03
13220     ok(6,1)=11200 : ok(6,2)=  79.5 : ok(6,3)=0.04
13240     ok(7,1)=13500 : ok(7,2)= 171.5 : ok(7,3)=0.05
13260     ok(8,1)=15000 : ok(8,2)= 246.5 : ok(8,3)=0.0525
13280     ! /r
13300     ! r: married 
13320     ok(1,4)=    0  : ok(1,5)=  0 : ok(1,6)=0
13340     ok(2,4)=12600  : ok(2,5)=  0 : ok(2,6)=0.005
13360     ok(3,4)=14600  : ok(3,5)= 10 : ok(3,6)=0.01
13380     ok(4,4)=17600  : ok(4,5)= 40 : ok(4,6)=0.02
13400     ok(5,4)=20100  : ok(5,5)= 90 : ok(5,6)=0.03
13420     ok(6,4)=22400  : ok(6,5)=159 : ok(6,6)=0.04
13440     ok(7,4)=24800  : ok(7,5)=255 : ok(7,6)=0.05
13460     ok(8,4)=27600  : ok(8,5)=395 : ok(8,6)=0.0525
13480     ! /r
13500   end if ! /r
13520   g2=stwh(tcd(1),1)*g_pay_periods_per_year
13540   g2=g2-em(3)*1000
13560   if em(1)=0 or em(1)=2 then j2=1 else j2=4 ! single of married
13580   j1=fn_table_line(mat ok,g2)
13600   s3=ok(j1,j2+1)+(g2-ok(j1,j2))*ok(j1,j2+2)
13620   s3=s3/g_pay_periods_per_year
13640   s3=round(s3,2)
13660   s3=round(s3,0)
13680   if s3<.1 then s3=0
13700 return  ! /r
14000 ASKDATES: ! r:
14020   open #h_dates:=11: "Name="&env$('Q')&"\PRmstr\Dates.h"&env$('cno')&",USE,RecL=76,shr",internal,outIn,relative 
14040   read #h_dates,using "form pos 1,2*n 8,x 32,n 8,c 20",rec=1,release: beg_date,end_date,d1,d1$ noRec ASKDATES_WRITE_DATE
14060   goto ASKDATES_SCREEN
14080   ASKDATES_WRITE_DATE: ! 
14100   write #h_dates,using "form pos 1,2*n 8,x 32,n 8,c 20",rec=1: beg_date,end_date,d1,d1$
14120   ASKDATES_SCREEN: ! 
14140   fnTos(sn$="Calculation-1")
14160   rc=cf=0: mylen=42: mypos=45: frameno=1
14180   gosub GET_ALPHA_DATE ! get alpha date
14200   fnFra(1,1,4,66,"Payroll Date","Enter the payroll date.")
14220   fnLbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
14240   fnTxt(1,mypos,10,0,1,"3",0,"Enter the date which you want used for your earnings records. ",frameno)
14260   resp$(rc+=1)=str$(d1)
14280   fnLbl(2,1,"Report Heading Date:",mylen,1,0,frameno)
14300   fnTxt(2,mypos,20,0,0," ",0,"Enter the date in alpha format for use in report headings, etc." ,frameno)
14320   resp$(rc+=1)= d1$
14340   fnChk(3,46,"Accrue Vacation and Sick Leave this period:",1,frameno)
14360   resp$(rc+=1)="False"
14380   fnFra(7,25,2,42,"Date Range","In order to Identify earnings and deductions, these answers must be correct.")
14400   frameno=2 : mylen=26 : mypos=mylen+2
14420   fnLbl(1,1,"Starting Date:",mylen,1,0,frameno)
14440   fnTxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno)
14460   resp$(rc+=1)=str$(beg_date)
14480   fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
14500   fnTxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno)
14520   resp$(rc+=1)=str$(end_date)
14540   fnCmdKey("Next",1,1,0,"Proceed with calculations.")
14560   fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
14580   fnAcs(sn$,0,mat resp$,ckey)
14600   if ckey<>5 then 
14620     prd=d1=val(resp$(1))
14640     d1$=resp$(2)
14660     if resp$(3)(1:1)="T" then d3$="Y" else d3$="N"
14680     beg_date=val(resp$(4))
14700     end_date=val(resp$(5))
14720     rewrite #h_dates,using "form pos 1,2*n 8,x 32,n 8,c 20",rec=1: beg_date,end_date,d1,d1$
14740     close #h_dates: 
14760   end if
14780 return  ! /r
15000 DETERMINE_EARNINGS: ! r: passed eno, dep,beg_date, end_date, returns mat ytdTotal,ytdFICA,tmd,td14,twd,mat caf
15020   ytdFICA=tmd=td14=0: mat caf=(0)
15040   mat tcp=(0)
15060   mat ytdTotal=(0) : mat tdc=(0)
15080   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",dep)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
15100   restore #h_payrollchecks,key>=checkkey$: nokey dePrCkNokey
15120   do
15140     read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prdate,ckno,mat tdc,mat tcp eof dePrCkEof
15160     if heno=eno and prdate=>beg_date and prdate<=end_date then 
15200       mat ytdTotal=ytdTotal+tcp
15220     end if
15240   loop while heno=eno
15260   dePrCkEof: ! 
15280   ytdFICA=ytdTotal(2) ! fica year to date
15300   tmd=ytdTotal(3) ! medicare year to date
15320   td14=ytdTotal(25) ! eic
15340   twd=ytdTotal(31) ! total wages
15360   for j=1 to 20
15380     caf(j)=ytdTotal(j+4) ! total miscellaneous deductions for year
15400   next j
15420   dePrCkNokey:!
15440 return  ! /r
16000 def fn_setup
16020   library 'S:\Core\Library': fntop, fnerror, fnxit,fnTos,fnFra,fnChk,fnLbl,fnTxt,fnCmdKey,fnAcs,fncd,fnpayroll_client_state$,fnmsgbox,fnStatus,fngethandle,fnStatusPause,fnDedNames,fnAutomatedSavePoint
16022   library 'S:\Core\Library': fnFree
16040   on error goto ERTN
16060   debug=0 ! if env$('ACSDeveloper')<>'' then debug=1 else debug=0
16080   ! ______________________________________________________________________
16100   dim sck(4),motab(12),stwh(10,2),sucw(10),sucr(10)
16140   dim inp(29),dat$*20,cap$*128,caf(20)
16160   dim fullname$(20)*20,abrevname$(20)*8,resp$(10)*40
16180   dim tdt(4),tcd(3),tdet(17),tdc(10),tcp(32)
16200   dim ytdTotal(32)
16240   dim x$*8,em(16),hr(2),n$*8,in2$(4),stuc(10)
16260   dim dedcode(10),calcode(10),dedfed(10),d1$*20,wcm(4) ,newx$*8
16280   dim newdedcode(20),newcalcode(20),newdedfed(20),newdedcode(20)
16300   dim dedfica(20),dedst(20),deduc(20)
16320   dim ml$(1)*256
16380   fn_setupFederalTables(mat ft,fed_annual_wh_allowance)
16460   mtc=0 ! motab counter
16480   motab(mtc+=1)=0   : motab(mtc+=1)=31  : motab(mtc+=1)=59
16500   motab(mtc+=1)=90  : motab(mtc+=1)=120 : motab(mtc+=1)=151
16520   motab(mtc+=1)=181 : motab(mtc+=1)=212 : motab(mtc+=1)=243
16540   motab(mtc+=1)=273 : motab(mtc+=1)=304 : motab(mtc+=1)=334
17000   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
17020   read #20,using 'Form POS 145,PD 5.2,POS 230,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,POS 618,30*N 1,POS 708,3*PD 4.3,3*PD 3.2,4*PD 4.2,POS 133,PD 6.3,PD 6.2': fucr,loccode,feducmax,ficarate,ficamax,ficamxr,mat sucw,mat sucr,mat dedcode,mat calcode,mat dedfed,mat sck,vacm,MinHourlyWage,mat wcm,ficar2,ficamx2
17040   close #20: 
17060   ficamax=ficamax*10
17080   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
17100   ssmax=ficamax : mcmax=ficamx2 : ficar1=ficarate*.01
17120   ficar2=ficar2*.01 : ficarate=ficar1+ficar2
17140   ficamxr=ficamax*ficarate : ficamx2=(ficamx2-ficamax)*ficar2
17160   ! 
17180   ! if env$('client')="West Accounting" then 
17200   !   saif(1)=173.33
17220   !   saif(2)=86.66
17240   !   saif(3)=80
17260   !   saif(4)=40
17280   ! end if 
17300 fnend 
18000 def fn_setupFederalTables(mat ft,&fed_annual_wh_allowance)
18020   fed_annual_wh_allowance=4050 ! (was 4000)   Withholding allowance. The 2016 amount for one withholding allowance on an annual basis is $4,050
18040   dim ft(8,6)
18060   ! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
18080   ! r: Federal - SINGLE person (including head of household)
18100    ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
18120    ft(2,1)=  2300 : ft(2,2)=     0    : ft(2,3)=0.1  
18140    ft(3,1)= 11625 : ft(3,2)=   932.5  : ft(3,3)=0.15 
18160    ft(4,1)= 40250 : ft(4,2)=  5226.25 : ft(4,3)=0.25 
18180    ft(5,1)= 94200 : ft(5,2)= 18713.75 : ft(5,3)=0.28 
18200    ft(6,1)=193950 : ft(6,2)= 46643.75 : ft(6,3)=0.33 
18220    ft(7,1)=419000 : ft(7,2)=120910.25 : ft(7,3)=0.35 
18240    ft(8,1)=420700 : ft(8,2)=121505.25 : ft(8,3)=0.396
18260   ! /r
18280   ! r: Federal - MARRIED person
18300    ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
18320    ft(2,4)=  8650  : ft(2,5)=     0    : ft(2,6)=0.1
18340    ft(3,4)= 27300  : ft(3,5)=  1865    : ft(3,6)=0.15
18360    ft(4,4)= 84550  : ft(4,5)= 10452.5  : ft(4,6)=0.25
18380    ft(5,4)=161750  : ft(5,5)= 29752.5  : ft(5,6)=0.28
18400    ft(6,4)=242000  : ft(6,5)= 52222.5  : ft(6,6)=0.33
18420    ft(7,4)=425350  : ft(7,5)=112728    : ft(7,6)=0.35
18440    ft(8,4)=479350  : ft(8,5)=131628    : ft(8,6)=0.396
18460   ! /r
18480   ! close #h_tables: 
18500 fnend
30000 def fn_setupOpenFiles
30020   open #breakdown=fngethandle: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&env$('cno')&",Shr",internal,outIn,keyed ioerr ignore ! formerly file #31
30040   open #hEmployee:=fngethandle: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno'),internal,outIn,keyed  ! formerly file #1
30060   open #h_department:=2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outIn,keyed 
30080   open #h_payrollchecks:=4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&",Shr,Use,RecL=224,KPs=1,KLn=17",internal,outIn,keyed 
30100   open #44: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx3.h"&env$('cno')&",Shr",internal,outIn,keyed 
30120   open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno'),internal,outIn,keyed 
30140   F_RPWORK: form pos 1,c 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
30160 fnend
32000 GET_ALPHA_DATE: ! r:
32010   dim month$(12),payrolldate$*20
32020   payrolldate$=cnvrt$("pic(########)",d1)
32040   year=val(payrolldate$(1:4))
32060   month=val(payrolldate$(5:6))
32080   day=val(payrolldate$(7:8))
32100   month$(1)="January"
32120   month$(2)="February"
32140   month$(3)="March"
32160   month$(4)="April"
32180   month$(5)="May"
32200   month$(6)="June"
32220   month$(7)="July"
32240   month$(8)="August"
32260   month$(9)="September"
32280   month$(10)="October"
32300   month$(11)="November"
32320   month$(12)="December"
32340   d1$=month$(month)&" "&str$(day)&", "&str$(year)
32360 return  ! /r
36000 def fn_table_line(mat tl_table,tl_seek_amount; tl_second_dimension)
36002   ! this function finds where [tl_seek_amount] falls within a range in a singe row (1st element) of a 2 dimensional array)
36004   ! this function identifies which column (2nd element) of the 2d array to search with [tl_second_dimension] which defaults to the first 
36010   if tl_second_dimension=0 then tl_second_dimension=1
36020   for tl_item=1 to udim(mat tl_table,1)-1
36040     if tl_seek_amount>tl_table(tl_item,tl_second_dimension) and tl_seek_amount<=tl_table(tl_item+1,tl_second_dimension) then 
36060       goto TL_XIT
36080     end if 
36100   next tl_item
36120   tl_item=udim(mat tl_table,1)
36140   TL_XIT: ! 
36160   fn_table_line=tl_item
36180 fnend 
37000 ST01: ! r:
37020 ! tcd(1) = state code
37040 ! g_pay_periods_per_year     = number of pay periods per year (formerly b8)
37060 ! em(3)  = allowances
37080 ! em(1)  = married (1=yes and more )
37100   s3=0
37120   if fnpayroll_client_state$='AR' then 
37140     gosub ARWH
37160   else if fnpayroll_client_state$='AZ' then 
37180     gosub AZWH
37200   else if fnpayroll_client_state$='GA' then 
37220     ! if env$('acsDeveloper')<>'' then 
37240     s3=fn_wh_georgia(stwh(tcd(1),1),g_pay_periods_per_year,em(3),em(1),em(7))
37260     ! else
37280     !   s3=0 ! fn_wh_georgia(stwh(tcd(1),1),g_pay_periods_per_year,em(3),em(1),em(7))
37300     ! end if
37320   else if fnpayroll_client_state$='IL' then 
37340     gosub ILWH
37360   else if fnpayroll_client_state$='IN' then 
37380     gosub INWH
37400   else if fnpayroll_client_state$='KY' then ! added 10/03/2016 for R R Crawford Engineering
37420     s3=fn_wh_kentuky(stwh(tcd(1),1),g_pay_periods_per_year,em(3))
37440   else if fnpayroll_client_state$='LA' then 
37460     gosub LAWH
37480   else if fnpayroll_client_state$='MO' then 
37500     gosub MOWH
37520   else if fnpayroll_client_state$='MS' then 
37540     gosub MSWH
37560   else if fnpayroll_client_state$='OK' then 
37580     gosub OKWH
37600   else if fnpayroll_client_state$='OR' then 
37620     s3=fn_wh_oregon(stwh(tcd(1),1),fed_wh_annual_estimate,g_pay_periods_per_year,em(3),em(1))
37640   else if fnpayroll_client_state$='TN' then 
37660     goto ST1_XIT ! no Tenn wh
37680   else if fnpayroll_client_state$='TX' then 
37700     goto ST1_XIT ! no Texas wh
37720   end if 
37740   ST1_XIT: ! 
37760 return  ! /r
37780 ST02: s3=0 : return
37800 ST03: s3=0 : return
37820 ST04: s3=0 : return
37840 ST05: s3=0 : return
37860 ST06: s3=0 : return
37880 ST07: s3=0 : return
37900 ST08: s3=0 : return
37920 ST09: s3=0 : return
37940 ST10: s3=0 : return
38000 LAWH: ! r: REPLACE ACSWRK\LOUSIANA.WH,SOURCE ! LOUISANA: NO TABLE: LA(5): revised 1/01/03
38020   h1=0
38040   h2=0
38060   h3=0
38080   mat la=(0)
38100   s=round(stwh(tcd(1),1),2)
38120   if em(1)=0 or em(1)=2 then 
38140     y=em(3)-1
38160     x=1
38180     if y>=0 then goto L3800
38200     x=0
38220     y=0
38240     goto L3800
38260   end if
38280   if em(3)=0 then y=0 : x=0
38300   if em(3)=1 then y=0 : x=1
38320   if em(3)>=2 then y=em(3)-2 : x=2
38340   L3800: ! 
38360   if x<2 then m1=12500 : m2=25000
38380   if x>=2 then m1=25000 : m2=50000
38400   n=g_pay_periods_per_year
38420   if s>0 then a=(s*.021) else a=0
38440   if s>(m1/n) then b=.0135*(s-(m1/n)) else b=0
38460   if s>(m2/n) then c=.0135*(s-(m2/n)) else c=0
38480   d=.021*(((x*4500)+(y*1000))/n)
38500   if ((x*4500)+(y*1000))>m1 then 
38520     e=.0135*(((x*4500)+(y*1000)-m1)/n)
38540   else 
38560     e=0
38580   end if 
38600   if (a+b+c)-(d+e)>0 then 
38620     s3=(a+b+c)-(d+e)
38640   else 
38660     s3=0
38680   end if 
38700   s3=round(s3,2)
38720   if s3<.1 then s3=0
38740 return  ! /r
39000 INWH: ! r: INDIANA    NO TABLE   07/01/2000  ! still in effect 71508, changed on 1/1/2016, but I didn't bother to update it because no one is using it.
39020   ! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
39040   h1=h2=h3=0
39060   h1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
39080   h2=em(3)*1000
39100   h3=h1-h2
39120   if h3>0 then 
39140     s3=h3*.034 ! +H3*.003  SOME COUNTIES HAVE WH
39160     s3=round(s3/g_pay_periods_per_year,2)
39180     if s3<.1 then s3=0
39200   end if 
39220 return  ! /r
40000 def fn_wh_oregon(wor_wages_taxable_current,wor_fed_wh_annual_estimate,wor_pay_periods_per_year,wor_allowances,wor_is_married)
40020   if ~wor_setup then 
40040     wor_setup=1
40080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=40: mat or1,mat or2
40120     dim or1(4,3) !  r: Withholding Table for Single with fewer than 3 allowances
40140     or1(1,1)=    0 : or1(1,2)= 197   : or1(1,3)=0.05
40160     or1(2,1)= 3350 : or1(2,2)= 367   : or1(2,3)=0.07
40180     or1(3,1)= 8450 : or1(3,2)= 724   : or1(3,3)=0.09
40200     or1(4,1)=50000 : or1(4,2)=4459.5 : or1(4,3)=0.09
40210     ! /r
40220     dim or2(4,3) ! r: Single with 3 or more allowances, or married
40240     or2(1,1)=    0 : or2(1,2)= 197 : or2(1,3)=0.05
40260     or2(2,1)= 6700 : or2(2,2)= 537 : or2(2,3)=0.07
40280     or2(3,1)=16900 : or2(3,2)=1251 : or2(3,3)=0.09
40300     or2(4,1)=50000 : or2(4,2)=4223 : or2(4,3)=0.09
40340     ! /r
40360   end if 
41000     ! requires locally populated variables Mat OR1 and Mat OR2
41020     ! returns Oregon State Withholding
41040     ! Oregon  !  rec=40
41060     ! 
41120     ! RECALK: ! used only for debugging purposes
41140   wor_allowances_effective=wor_allowances
41160     ! 
41180   wor_they_are_single=wor_they_are_married=0
41200   if wor_is_married=0 or wor_is_married=2 then wor_they_are_single=1
41220   if wor_is_married=1 or wor_is_married=3 or wor_is_married=4 or wor_is_married=5 then wor_they_are_married=1
41240     ! 
41260   if wor_wages_taxable_current>100000 and wor_they_are_single then wor_allowances_effective=0
41280   if wor_wages_taxable_current>200000 and wor_they_are_married then wor_allowances_effective=0
41300     ! 
41320   if wor_they_are_married or (wor_they_are_single and wor_allowances_effective>=3) then ! (married or more than 3 allowances)
41340     wor_table=2
41360   else ! (single and less than 3 allowances)
41380     wor_table=1
41400   end if 
41420     ! 
41440   if wor_table=2 then ! wor_they_are_married then
41460     wor_standard_deduction=4350
41480   else ! if wor_table=1 then ! if wor_they_are_single then
41500     wor_standard_deduction=2175
41520   end if 
41540     ! 
41560   wor_wages_annual_estimate=wor_wages_taxable_current*wor_pay_periods_per_year
41580     ! 
41600   wor_phase_out=fn_oregonPhaseOut(wor_wages_annual_estimate,wor_fed_wh_annual_estimate,wor_table,wor_they_are_single,wor_they_are_married)
41620     ! 
41640     ! wor_base=wor_wages_taxable_current*wor_pay_periods_per_year-min(wor_fed_wh_annual_estimate,8550)-(wor_allowances_effective*2250)
41660   wor_base=wor_wages_annual_estimate-wor_phase_out-wor_standard_deduction
41680     ! 
41700   if wor_table=2 then 
41720     wor_table_line=fn_table_line(mat or2,wor_base)
41740     wor_pre_base=or2(wor_table_line,2)
41760     wor_tax_rate=or2(wor_table_line,3)
41780     wor_remove_prev=or2(wor_table_line,1)
41800   else ! wor_table=1
41820     wor_table_line=fn_table_line(mat or1,wor_base)
41840     wor_pre_base=or1(wor_table_line,2)
41860     wor_tax_rate=or1(wor_table_line,3)
41880     wor_remove_prev=or1(wor_table_line,1)
41900   end if 
42000   if debug then let fnStatus('-------------------------------------------')
42020   if wor_they_are_single then 
42040     if debug then let fnStatus('  Single with '&str$(wor_allowances_effective)&' allowances')
42060   else if wor_they_are_married then 
42080     if debug then let fnStatus('  Married with '&str$(wor_allowances_effective)&' allowances')
42100   else 
42120     if debug then let fnStatus('  Maridal Status is undetermined!!!  ')
42140   end if 
42160   if debug then let fnStatus('    Current Wage (Gross)    = '&str$(wor_wages_taxable_current))
42180   if debug then let fnStatus('    Pay Periods Per Year    = '&str$(wor_pay_periods_per_year))
42200   if debug then let fnStatus('    Annual wage (estimate)  = '&str$(wor_wages_annual_estimate))
42220   if debug then let fnStatus('    standard deduction     = '&str$(wor_standard_deduction))
42240   if debug then let fnStatus('    table '&str$(wor_table)&' line '&str$(wor_table_line))
42260   if debug then let fnStatus('    phase out              = '&str$(wor_phase_out))
42280   if debug then let fnStatus('    fed_wh_annual_estimate = '&str$(wor_fed_wh_annual_estimate))
42300   if debug then let fnStatus('.')
42320   if debug then let fnStatus('    BASE = '&str$(wor_wages_annual_estimate)&' (an..wages) - '&str$(wor_phase_out)&' (phase out/fed wh) - '&str$(wor_standard_deduction)&' (std ded)')
42340   if debug then let fnStatus('    base                   = '&str$(wor_base))
42360     ! fn  status('    pre_base               = '&str$(wor_pre_base))
42380     ! fn  status('    tax rate               = '&str$(wor_tax_rate))
42400     ! fn  status('    remove_prev            = '&str$(wor_remove_prev))
42420   if debug then let fnStatus('.')
42440   if debug then let fnStatus('                                   WH = '&str$(wor_pre_base)&' + [('&str$(wor_base)&' - '&str$(wor_remove_prev)&')] x '&str$(wor_tax_rate)&'] - (195 x '&str$(wor_allowances_effective)&')')
42460     ! 
42480     ! WH = 1,244 + [(BASE – 16,900        ) * 0.09] – (195 * allowances)
42500     ! wor_return=or2(wor_table_line,2)+(wor_base-or2(wor_table_line,1))*or2(wor_table_line,3)
42520   wor_return = wor_pre_base +(( wor_base - wor_remove_prev) * wor_tax_rate) - (195 * wor_allowances_effective)
42540   fnStatus('withholding before dividing by pay periods = '&str$(wor_return))
42560   wor_return=wor_return/wor_pay_periods_per_year
42580   wor_return=round(wor_return,2)
42600   if wor_return<.1 then wor_return=0
42620   fnStatus('calculated withholding ='&str$(wor_return))
42640   if debug then let fnStatusPause ! pause
42660   fn_wh_oregon=wor_return
42680 fnend 
44000 def fn_oregonPhaseOut(opo_wages,opo_fed_wh,opo_table,opo_is_single,opo_is_married)
44020   if opo_wages<50000 then 
44040     opo_return=min(opo_fed_wh,6500)
44060   else if opo_table=1 then 
44080     if opo_wages => 50000 and opo_wages<125000 then opo_return= 6550 : goto OPO_XIT
44100     if opo_wages =>125000 and opo_wages<130000 then opo_return= 5200 : goto OPO_XIT
44120     if opo_wages =>130000 and opo_wages<135000 then opo_return= 3900 : goto OPO_XIT
44140     if opo_wages =>135000 and opo_wages<140000 then opo_return= 2600 : goto OPO_XIT
44160     if opo_wages =>140000 and opo_wages<145000 then opo_return= 1300 : goto OPO_XIT
44180     if opo_wages =>145000 then opo_return=0
44200   else ! if opo_table=2 then
44220     if opo_is_married then 
44240       if opo_wages => 50000 and opo_wages<250000 then opo_return= 6550 : goto OPO_XIT
44260       if opo_wages =>250000 and opo_wages<260000 then opo_return= 5200 : goto OPO_XIT
44280       if opo_wages =>260000 and opo_wages<270000 then opo_return= 3900 : goto OPO_XIT
44300       if opo_wages =>270000 and opo_wages<280000 then opo_return= 2600 : goto OPO_XIT
44320       if opo_wages =>280000 and opo_wages<290000 then opo_return= 1300 : goto OPO_XIT
44340       if opo_wages =>290000 then opo_return=0
44360     else ! if opo_is_single then
44380       if opo_wages => 50000 and opo_wages<125000 then opo_return= 6550 : goto OPO_XIT
44400       if opo_wages =>125000 and opo_wages<130000 then opo_return= 5200 : goto OPO_XIT
44420       if opo_wages =>130000 and opo_wages<135000 then opo_return= 3900 : goto OPO_XIT
44440       if opo_wages =>135000 and opo_wages<140000 then opo_return= 2600 : goto OPO_XIT
44460       if opo_wages =>140000 and opo_wages<145000 then opo_return= 1300 : goto OPO_XIT
44480       if opo_wages =>145000 then opo_return=0
44500     end if 
44520   end if 
44540   OPO_XIT: ! 
44560   fn_oregonPhaseOut=opo_return
44580 fnend 
46000 def fn_wh_kentuky(wky_wages_taxable_current,g_pay_periods_per_year,wky_allowances)
46020   ! KYWH: ! REPLACE kentucky.wh/acswrk,source ! kentucky:  rec=20  ky(6,3) ! revised 12/31/2005
46040   ! wky_wages_taxable_current - formerly b8
46060   ! g_pay_periods_per_year - formerly stwh(tcd1,1)
46080   if ~wky_setup then 
46100     wky_setup=1
46120     ! r: Pull the withholding routines from new\acswrk
46160     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=20: mat ky ! Kentucky
46180     dim ky(6,3)
46200     ky(1,1)=0     : ky(1,2)=0    : ky(1,3)=0.02
46220     ky(2,1)=3000  : ky(2,2)=60   : ky(2,3)=0.03
46240     ky(3,1)=4000  : ky(3,2)=90   : ky(3,3)=0.04
46260     ky(4,1)=5000  : ky(4,2)=130  : ky(4,3)=0.05
46280     ky(5,1)=8000  : ky(5,2)=280  : ky(5,3)=0.058
46300     ky(6,1)=75000 : ky(6,2)=4166 : ky(6,3)=0.06
46340     ! /r
46360   end if 
46380   h1=(wky_wages_taxable_current)*g_pay_periods_per_year
46400   h2=h1-1970
46420   j1=fn_table_line(mat ky,h2)
46440   s3=ky(j1,2)+(h2-ky(j1,1))*ky(j1,3)
46460   s3=s3-20*wky_allowances
46480   s3=s3/g_pay_periods_per_year
46500   s3=round(s3,2)
46520   if s3<.1 then s3=0 ! do not withhold less than 10 cents.
46540   fn_wh_kentuky=s3
46560 fnend 
47000 def fn_wh_georgia(wga_wages_taxable_current,g_pay_periods_per_year,wga_allowances,wga_is_married,wga_eicCode)
47010   ! created 06/29/2017
47020   ! wga_wages_taxable_current - formerly b8
47030   ! g_pay_periods_per_year - formerly stwh(tcd1,1)
47040   if ~wga_setup then 
47050     wga_setup=1
47052     gaAnnualDependantAllowance=3000
47060     ! r: single Table F Page 43 of Employeer's Tax Guide dated 1/16/2017
47070     dim gawhTableF(6,3)
47080     gawhTableF(1,1)=    0 : gawhTableF(1,2)=   0.00 : gawhTableF(1,3)=0.01
47090     gawhTableF(2,1)= 1000 : gawhTableF(2,2)=  10.00 : gawhTableF(2,3)=0.02
47100     gawhTableF(3,1)= 3000 : gawhTableF(3,2)=  50.00 : gawhTableF(3,3)=0.03
47110     gawhTableF(4,1)= 5000 : gawhTableF(4,2)= 110.00 : gawhTableF(4,3)=0.04
47120     gawhTableF(5,1)= 7000 : gawhTableF(5,2)= 190.00 : gawhTableF(5,3)=0.05
47130     gawhTableF(6,1)=10000 : gawhTableF(6,2)= 340.00 : gawhTableF(6,3)=0.06
47140     ! /r
47150     ! r: single Table G Page 44 of Employeer's Tax Guide dated 1/16/2017
47160     dim gawhTableG(6,3)
47170     gawhTableG(1,1)=    0 : gawhTableG(1,2)=   0.00 : gawhTableG(1,3)=0.01
47180     gawhTableG(2,1)=  500 : gawhTableG(2,2)=   5.00 : gawhTableG(2,3)=0.02
47190     gawhTableG(3,1)= 1500 : gawhTableG(3,2)=  25.00 : gawhTableG(3,3)=0.03
47200     gawhTableG(4,1)= 2500 : gawhTableG(4,2)=  55.00 : gawhTableG(4,3)=0.04
47210     gawhTableG(5,1)= 3500 : gawhTableG(5,2)=  95.00 : gawhTableG(5,3)=0.05
47220     gawhTableG(6,1)= 5000 : gawhTableG(6,2)= 170.00 : gawhTableG(6,3)=0.06
47230     ! /r
47240     ! r: single Table H Page 45 of Employeer's Tax Guide dated 1/16/2017
47250     dim gawhTableH(6,3)
47260     gawhTableH(1,1)=    0 : gawhTableH(1,2)=   0.00 : gawhTableH(1,3)=0.01
47270     gawhTableH(2,1)=  750 : gawhTableH(2,2)=   7.50 : gawhTableH(2,3)=0.02
47280     gawhTableH(3,1)= 2250 : gawhTableH(3,2)=  37.50 : gawhTableH(3,3)=0.03
47290     gawhTableH(4,1)= 3750 : gawhTableH(4,2)=  82.50 : gawhTableH(4,3)=0.04
47300     gawhTableH(5,1)= 5250 : gawhTableH(5,2)= 142.50 : gawhTableH(5,3)=0.05
47310     gawhTableH(6,1)= 7000 : gawhTableH(6,2)= 230.00 : gawhTableH(6,3)=0.06
47320     ! /r
47330   end if 
47340   Ga_StateDeduction=fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
47350   Ga_StatePersonalAllowance=fn_statePersonalAllowance('GA',wga_is_married,wga_eicCode)
47360   if env$('acsDeveloper')<>'' then dev=1 else dev=0
47370   ! if dev then pr 'wga_wages_taxable_current=';wga_wages_taxable_current
47380   ! if dev then pr '   g_pay_periods_per_year=';g_pay_periods_per_year
47382   ! if dev then pr '   Ga_StatePersonalAllowance=';Ga_StatePersonalAllowance
47390   ! if dev then pr '   fn_standardStateDeduction=';Ga_StateDeduction
47410   ! if dev then pr '   wga_allowances=';wga_allowances
47420   ga_WagesAnnual=(wga_wages_taxable_current)*g_pay_periods_per_year
47430   ! if dev then pr '   Ga_WagesAnnual=';Ga_WagesAnnual
47440   ga_WagesAnnualTaxable=Ga_WagesAnnual-Ga_StateDeduction ! fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
47450   ga_WagesAnnualTaxable-=Ga_StatePersonalAllowance ! fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
47460   ! if dev then pr '   Annual Wages (less state deduction and personal allowance)=';Ga_WagesAnnualTaxable
47480   if wga_is_married=0 then ! SINGLE INDIVIDUAL 
47490     mat gawh(udim(mat gawhTableH,1),udim(mat gawhTableH,2))
47500     mat gawh=gawhTableH
47502     ! if dev then pr '   using Table H'
47510   else if wga_is_married=4 then ! MARRIED FILING JOINT RETURN (both spouses having income) OR MARRIED FILING SEPARATE RETURN
47520     mat gawh(udim(mat gawhTableG,1),udim(mat gawhTableG,2))
47530     mat gawh=gawhTableG
47532     ! if dev then pr '   using Table G'
47540   else if wga_is_married=3 or wga_is_married=2 or wga_is_married=1 then ! MARRIED FILING JOINT RETURN (one spouse having income) OR HEAD OF HOUSEHOLD or 1-Married (nothing else known)
47550     mat gawh(udim(mat gawhTableF,1),udim(mat gawhTableF,2))
47560     mat gawh=gawhTableF
47562     ! if dev then pr '   using Table F'
47570   end if
47580   j1=fn_table_line(mat gawh,Ga_WagesAnnualTaxable)
47582   ! if dev then pr '   table line ';j1
47610   ! if dev then pause
47612   ga_AnnualWagesSubjToWithhold=Ga_WagesAnnualTaxable-gaAnnualDependantAllowance*wga_allowances
47620   s3=gawh(j1,2)+(ga_AnnualWagesSubjToWithhold-gawh(j1,1))*gawh(j1,3)
47630   s3=s3
47640   s3=s3/g_pay_periods_per_year
47650   s3=round(s3,2) ! round to the nearest whole dollar
47660   if s3<.1 then s3=0 ! do not withhold less than 10 cents.
47670   fn_wh_georgia=s3
47680 fnend 
64000 def fn_standardStateDeduction(state$,wga_is_married,wga_eicCode)
64020   if state$='GA' then
64040     if wga_is_married=0 or wga_is_married=2 then ! Single (or Single - Head of Household)
64060       standardStateDeduction=2300
64080     else if wga_is_married=5 then ! Married - filing seperate - both working
64100       standardStateDeduction=1500
64120     else if wga_is_married=4 then ! Married - filing joint - both working
64140       standardStateDeduction=3000
64160     else if wga_is_married=3 then ! Married - filing joint return - only one working
64180       standardStateDeduction=3000
64200     else  ! 1 - Married - (filing status unknown) - just use lowest deduction
64220       standardStateDeduction=1500
64240     end if
64260   else
64280     pr 'fn_standardStateDeduction not yet configured for state: "'&state$&'"'
64300     pause
64320   end if
64340   fn_standardStateDeduction=standardStateDeduction
64900 fnend
65000 def fn_statePersonalAllowance(state$,wga_is_married,wga_eicCode)
65020   if state$='GA' then
65040     if wga_is_married=0 or wga_is_married=2 then ! Single (or Single - Head of Household)
65060       statePersonalAllowance=2700
65080     else if wga_is_married=3 then ! Married - filing joint - only one working
65100       statePersonalAllowance=3700
65120     else if wga_is_married=4 then ! Married - filing joint - both working
65140       statePersonalAllowance=7400
65160     else if wga_is_married=5 then ! Married - filing seperate - both working
65180       statePersonalAllowance=3700
65200     else  ! 1 - Married - (filing status unknown) - just use lowest married deduction
65220       statePersonalAllowance=3700
65240     end if
65260   else
65280     pr 'fn_statePersonalAllowance not yet configured for state: "'&state$&'"'
65300     pause
65320   end if
65340   fn_statePersonalAllowance=statePersonalAllowance
65900 fnend
66000 def fn_test_state_calk
66020   fn_setup
66040   ! show the state you are assined  to and you change it if you like.
66060   pay_periods_per_year=52
66080   wages_taxable_current=399.60
66100   fed_wh=7.75
66120   allowances=2
66140   is_married=4  ! is_married = 0 - Single
66180                     ! is_married = 1 - Married
66200                     ! is_married = 2 - Single - Head of Household
66220                     ! is_married = 3 - Married - filing joint return - only one working
66240                     ! is_married = 4 - Married - filing seperate or joint return both working
66260   eicCode=0    ! eicCode = 0 - Not qualified for EIC
66262                    ! eicCode = 1 - Single or Spouse not file
66264                    ! eicCode = 2 - Married both filing
66280   pr 'wages_taxable_current: ';wages_taxable_current
66300   pr ' pay_periods_per_year: ';pay_periods_per_year
66320   pr '               fed_wh: ';fed_wh
66340   pr '           allowances: ';allowances
66360   pr '           is_married: ';is_married
66380   pr '              eicCode: ';eicCode
66400   pr 'Kentuky Function returns ';fn_wh_kentuky(wages_taxable_current,pay_periods_per_year,allowances)
66420   pr 'Georgia Function returns ';fn_wh_georgia(wages_taxable_current,pay_periods_per_year,allowances,is_married,eicCode)
66440   pr 'Oregon Function returns ';fn_wh_oregon(wages_taxable_current,fed_wh,pay_periods_per_year,allowances,is_married)
66460   if env$('ACSdeveloper')<>'' then pause 
66480 fnend 
68000 def fn_report_stuff
68020   ! fnStatus('check completely calcualated for eno '&str$(eno))
68040   ! fnStatus('tcp(1)='&str$(tcp(1)))
68060   ! fnStatus('tcp(2)='&str$(tcp(2)))
68080   ! fnStatus('tcp(3)='&str$(tcp(3)))
68100   ! fnStatus('tcp(4)='&str$(tcp(4)))
68120   ! fnStatus('tcp(13)='&str$(tcp(13)))
68140   ! fnStatus('tcp(14)='&str$(tcp(14)))
68160   ! fnStatus('tcp(32)='&str$(tcp(32)))
68180   ! fnStatusPause
68200 fnend 
69000 WEST_ACC_WORKMANSCOMP: ! r:
69001   ! inp(6) Other Compensation
69002   ! em(5)  Pay Code
69003   ! if other compensation > 0 then
69004   !   if pay code is 1 hours = 173.33
69005   !   if pay code is 2 hours = 86.66
69006   !   if pay code is 3 hours = 80
69007   !   if pay code is 4 hours = 40
69008   ! else 
69009   !   hours = regular hours + overtime hours
69010   ! end if
69020   tmphrs=inp(1)+inp(2) ! if inp(6)>0 then tmphrs=saif(em(5)) else tmphrs=inp(1)+inp(2)
69040   !     fnStatus('inp(17) changed to '&str$(round(tmphrs*inp(17)*.01,2))&' round('&str$(tmphrs)&' * inp(17)('&str$(inp(17))&' * .01)',2)
69060   !     fnStatusPause
69080   inp(17)=round(tmphrs*inp(17)*.01,2) ! inp(17)=round(tmphrs*inp(17)*.01,2)
69100 return  ! /r
