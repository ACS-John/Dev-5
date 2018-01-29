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
01125   L1110: ! 
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
14530   fnchk(13,65,'Enable 2018 Federal Withholdings (FOR TESTING ONLY)', 1,0) 
14532   rc_taxYear=rc+=1 
14534   if env$('taxYear')='2018' then resp$(rc_taxYear)='True' else resp$(rc_taxYear)='False'
14540   fnCmdKey("Next",1,1,0,"Proceed with calculations.")
14560   fnCmdKey("Cancel",5,0,1,"Returns to menu without calculating")
14580   fnAcs(sn$,0,mat resp$,ckey)
14600   if ckey<>5 then 
14620     prd=d1=val(resp$(1))
14640     d1$=resp$(2)
14660     if resp$(3)(1:1)="T" then d3$="Y" else d3$="N"
14680     beg_date=val(resp$(4))
14700     end_date=val(resp$(5))
14710 if resp$(rc_taxYear)='True' then let setenv('taxYear','2018') else let setenv('taxYear','2017') 
14712 ! pr env$('taxYear') : pause
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
20000 def fn_setupOpenFiles
20020   open #breakdown=fngethandle: "Name="&env$('Q')&"\PRmstr\HourBreakdown.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\HourBreakdown-idx.H"&env$('cno')&",Shr",internal,outIn,keyed ioerr ignore ! formerly file #31
20040   open #hEmployee:=fngethandle: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno'),internal,outIn,keyed  ! formerly file #1
20060   open #h_department:=2: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outIn,keyed 
20080   open #h_payrollchecks:=4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&",Shr,Use,RecL=224,KPs=1,KLn=17",internal,outIn,keyed 
20100   open #44: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx3.h"&env$('cno')&",Shr",internal,outIn,keyed 
20120   open #h_rpwork:=3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\rpwork"&wsid$&"Idx.h"&env$('cno'),internal,outIn,keyed 
20140   F_RPWORK: form pos 1,c 8,n 3,5*pd 4.2,25*pd 5.2,2*pd 4.2
20160 fnend
22000 GET_ALPHA_DATE: ! r:
22010   dim month$(12),payrolldate$*20
22020   payrolldate$=cnvrt$("pic(########)",d1)
22040   year=val(payrolldate$(1:4))
22060   month=val(payrolldate$(5:6))
22080   day=val(payrolldate$(7:8))
22100   month$(1)="January"
22120   month$(2)="February"
22140   month$(3)="March"
22160   month$(4)="April"
22180   month$(5)="May"
22200   month$(6)="June"
22220   month$(7)="July"
22240   month$(8)="August"
22260   month$(9)="September"
22280   month$(10)="October"
22300   month$(11)="November"
22320   month$(12)="December"
22340   d1$=month$(month)&" "&str$(day)&", "&str$(year)
22360 return  ! /r
26000 def fn_table_line(mat tl_table,tl_seek_amount; tl_second_dimension)
26002   ! this function finds where [tl_seek_amount] falls within a range in a singe row (1st element) of a 2 dimensional array)
26004   ! this function identifies which column (2nd element) of the 2d array to search with [tl_second_dimension] which defaults to the first 
26010   if tl_second_dimension=0 then tl_second_dimension=1
26020   for tl_item=1 to udim(mat tl_table,1)-1
26040     if tl_seek_amount>tl_table(tl_item,tl_second_dimension) and tl_seek_amount<=tl_table(tl_item+1,tl_second_dimension) then 
26060       goto TL_XIT
26080     end if 
26100   next tl_item
26120   tl_item=udim(mat tl_table,1)
26140   TL_XIT: ! 
26160   fn_table_line=tl_item
26180 fnend 
27000 ST01: ! r:
27020 ! tcd(1) = state code
27040 ! g_pay_periods_per_year     = number of pay periods per year (formerly b8)
27060 ! em(3)  = allowances
27080 ! em(1)  = married (1=yes and more )
27100   s3=0
27120   if fnpayroll_client_state$='AR' then 
27140     gosub ARWH
27160   else if fnpayroll_client_state$='AZ' then 
27180     gosub AZWH
27200   else if fnpayroll_client_state$='GA' then 
27220     ! if env$('acsDeveloper')<>'' then 
27240     s3=fn_wh_georgia(stwh(tcd(1),1),g_pay_periods_per_year,em(3),em(1),em(7))
27260     ! else
27280     !   s3=0 ! fn_wh_georgia(stwh(tcd(1),1),g_pay_periods_per_year,em(3),em(1),em(7))
27300     ! end if
27320   else if fnpayroll_client_state$='IL' then 
27340     gosub ILWH
27360   else if fnpayroll_client_state$='IN' then 
27380     gosub INWH
27400   else if fnpayroll_client_state$='KY' then ! added 10/03/2016 for R R Crawford Engineering
27420     s3=fn_wh_kentuky(stwh(tcd(1),1),g_pay_periods_per_year,em(3))
27440   else if fnpayroll_client_state$='LA' then 
27460     gosub LAWH
27480   else if fnpayroll_client_state$='MO' then 
27500     gosub MOWH
27520   else if fnpayroll_client_state$='MS' then 
27540     gosub MSWH
27560   else if fnpayroll_client_state$='OK' then 
27580     gosub OKWH
27600   else if fnpayroll_client_state$='OR' then 
27620     s3=fn_wh_oregon(stwh(tcd(1),1),fed_wh_annual_estimate,g_pay_periods_per_year,em(3),em(1))
27640   else if fnpayroll_client_state$='TN' then 
27660     goto ST1_XIT ! no Tenn wh
27680   else if fnpayroll_client_state$='TX' then 
27700     goto ST1_XIT ! no Texas wh
27720   end if 
27740   ST1_XIT: ! 
27760 return  ! /r
27780 ST02: s3=0 : return
27800 ST03: s3=0 : return
27820 ST04: s3=0 : return
27840 ST05: s3=0 : return
27860 ST06: s3=0 : return
27880 ST07: s3=0 : return
27900 ST08: s3=0 : return
27920 ST09: s3=0 : return
27940 ST10: s3=0 : return
28000 def fn_setupFederalTables(mat ft,&fed_annual_wh_allowance)
28040   dim ft(8,6)
28060   if env$('taxYear')<>'' then taxYear=2017 else taxYear=val(env$('taxYear'))
28070   fed_annual_wh_allowance=4050 ! (was 4000)   Withholding allowance. The 2016 amount for one withholding allowance on an annual basis is $4,050
28080   ! r: Federal - SINGLE person (including head of household)
28100   if taxYear<=2017 then
28120     ! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
28140     ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
28160     ft(2,1)=  2300 : ft(2,2)=     0    : ft(2,3)=0.1  
28180     ft(3,1)= 11625 : ft(3,2)=   932.5  : ft(3,3)=0.15 
28200     ft(4,1)= 40250 : ft(4,2)=  5226.25 : ft(4,3)=0.25 
28220     ft(5,1)= 94200 : ft(5,2)= 18713.75 : ft(5,3)=0.28 
28240     ft(6,1)=193950 : ft(6,2)= 46643.75 : ft(6,3)=0.33 
28260     ft(7,1)=419000 : ft(7,2)=120910.25 : ft(7,3)=0.35 
28280     ft(8,1)=420700 : ft(8,2)=121505.25 : ft(8,3)=0.396
28300   else if taxYear=2018 then
28310     fed_annual_wh_allowance=4150
28320     ft(1,1)=     0 : ft(1,2)=     0    : ft(1,3)=0    
28340     ft(2,1)=  3700 : ft(2,2)=     0    : ft(2,3)=0.1  
28360     ft(3,1)= 13225 : ft(3,2)=   952.5  : ft(3,3)=0.12 
28380     ft(4,1)= 42400 : ft(4,2)=  4453.5  : ft(4,3)=0.22 
28400     ft(5,1)= 86200 : ft(5,2)= 14089.5  : ft(5,3)=0.24 
28420     ft(6,1)=161200 : ft(6,2)= 32089.5  : ft(6,3)=0.32 
28440     ft(7,1)=203700 : ft(7,2)= 45689.5  : ft(7,3)=0.35 
28460     ft(8,1)=503700 : ft(8,2)=150689.5  : ft(8,3)=0.37 
28480   end if
28500   ! /r
28520   ! r: Federal - MARRIED person
28540   if taxYear<=2017 then
28560     ! Page 46 from   https://www.irs.gov/pub/irs-pdf/p15.pdf
28580     ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
28600     ft(2,4)=  8650  : ft(2,5)=     0    : ft(2,6)=0.1
28620     ft(3,4)= 27300  : ft(3,5)=  1865    : ft(3,6)=0.15
28640     ft(4,4)= 84550  : ft(4,5)= 10452.5  : ft(4,6)=0.25
28660     ft(5,4)=161750  : ft(5,5)= 29752.5  : ft(5,6)=0.28
28680     ft(6,4)=242000  : ft(6,5)= 52222.5  : ft(6,6)=0.33
28700     ft(7,4)=425350  : ft(7,5)=112728    : ft(7,6)=0.35
28720     ft(8,4)=479350  : ft(8,5)=131628    : ft(8,6)=0.396
28740   else if taxYear=2018 then
28760     ft(1,4)=     0  : ft(1,5)=     0    : ft(1,6)=0
28780     ft(2,4)= 11550  : ft(2,5)=     0    : ft(2,6)=0.1
28800     ft(3,4)= 30600  : ft(3,5)=  1905    : ft(3,6)=0.12
28820     ft(4,4)= 88950  : ft(4,5)=  8907    : ft(4,6)=0.22
28840     ft(5,4)=176550  : ft(5,5)= 28179    : ft(5,6)=0.24
28860     ft(6,4)=326550  : ft(6,5)= 64179    : ft(6,6)=0.32
28880     ft(7,4)=411550  : ft(7,5)= 91379    : ft(7,6)=0.35
28900     ft(8,4)=611550  : ft(8,5)=161379    : ft(8,6)=0.37
28920   end if
28940   ! /r
28980 fnend
30000 ARWH: ! r: REPLACE ACSWRK\ARKANSAS.WH,SOURCE ! Arkansas #5 ar(7,3)  REVISED 7/01/91
30020   if ~setup_arwh then ! r: setup AR Arkansas
30040     dim ar(6,3) ! ar(7,3)
30060     setup_arwh=1
30080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=5: mat ar ! Arkansas
30100     ! Page 1 of http://www.dfa.arkansas.gov/offices/incomeTax/withholding/Documents/whformula.pdf
30120     ! over                              Percentage
30140     ar(1,1)=    0 : ar(1,2)=   0    :  ar(1,3)=0.009
30160     ar(2,1)= 4300 : ar(2,2)=  38.7  :  ar(2,3)=0.024
30180     ar(3,1)= 8400 : ar(3,2)= 137.1  :  ar(3,3)=0.034
30200     ar(4,1)=12600 : ar(4,2)= 279.9  :  ar(4,3)=0.044
30220     ar(5,1)=21000 : ar(5,2)= 649.5  :  ar(5,3)=0.059
30240     ar(6,1)=35100 : ar(6,2)=1481.4  :  ar(6,3)=0.069
30260   end if ! /r
30280   t1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
30300   t2=2000
30320   t3=t1-t2
30340   j1=fn_table_line(mat ar,t3)
30360   s1=round(ar(j1,2)+(t3-ar(j1,1))*ar(j1,3),2)
30380   s2=em(3)*20
30400   s3=round((s1-s2)/g_pay_periods_per_year,2)
30420   if s3<.1 then s3=0
30440 return  ! /r
32000 AZWH: ! r: REPLACE ACSWRK\ARIZONA.WH,SOURCE ! ARIZONA:  NO TABLE  REVISED 1/01/10
32020   ! effective june 30, 2010 the rates changed and also the base change from a percent of federal wh to a percent of total taxable wages
32040   stp=0
32060   if em(3)=1 then stp=.013
32080   if em(3)=2 then stp=.018
32100   if em(3)=3 then stp=.027
32120   if em(3)=4 then stp=.036
32140   if em(3)=5 then stp=.042
32160   if em(3)=6 then stp=.0510
32180   s3=round(stwh(tcd(1),1)*stp,2)
32200   h3=min(h3,1200)
32220 return  ! /r
34000 def fn_wh_georgia(wga_wages_taxable_current,g_pay_periods_per_year,wga_allowances,wga_is_married,wga_eicCode)
34010   ! created 06/29/2017
34020   ! wga_wages_taxable_current - formerly b8
34030   ! g_pay_periods_per_year - formerly stwh(tcd1,1)
34040   if ~wga_setup then 
34050     wga_setup=1
34052     gaAnnualDependantAllowance=3000
34060     ! r: single Table F Page 43 of Employeer's Tax Guide dated 1/16/2017
34070     dim gawhTableF(6,3)
34080     gawhTableF(1,1)=    0 : gawhTableF(1,2)=   0.00 : gawhTableF(1,3)=0.01
34090     gawhTableF(2,1)= 1000 : gawhTableF(2,2)=  10.00 : gawhTableF(2,3)=0.02
34100     gawhTableF(3,1)= 3000 : gawhTableF(3,2)=  50.00 : gawhTableF(3,3)=0.03
34110     gawhTableF(4,1)= 5000 : gawhTableF(4,2)= 110.00 : gawhTableF(4,3)=0.04
34120     gawhTableF(5,1)= 7000 : gawhTableF(5,2)= 190.00 : gawhTableF(5,3)=0.05
34130     gawhTableF(6,1)=10000 : gawhTableF(6,2)= 340.00 : gawhTableF(6,3)=0.06
34140     ! /r
34150     ! r: single Table G Page 44 of Employeer's Tax Guide dated 1/16/2017
34160     dim gawhTableG(6,3)
34170     gawhTableG(1,1)=    0 : gawhTableG(1,2)=   0.00 : gawhTableG(1,3)=0.01
34180     gawhTableG(2,1)=  500 : gawhTableG(2,2)=   5.00 : gawhTableG(2,3)=0.02
34190     gawhTableG(3,1)= 1500 : gawhTableG(3,2)=  25.00 : gawhTableG(3,3)=0.03
34200     gawhTableG(4,1)= 2500 : gawhTableG(4,2)=  55.00 : gawhTableG(4,3)=0.04
34210     gawhTableG(5,1)= 3500 : gawhTableG(5,2)=  95.00 : gawhTableG(5,3)=0.05
34220     gawhTableG(6,1)= 5000 : gawhTableG(6,2)= 170.00 : gawhTableG(6,3)=0.06
34230     ! /r
34240     ! r: single Table H Page 45 of Employeer's Tax Guide dated 1/16/2017
34250     dim gawhTableH(6,3)
34260     gawhTableH(1,1)=    0 : gawhTableH(1,2)=   0.00 : gawhTableH(1,3)=0.01
34270     gawhTableH(2,1)=  750 : gawhTableH(2,2)=   7.50 : gawhTableH(2,3)=0.02
34280     gawhTableH(3,1)= 2250 : gawhTableH(3,2)=  37.50 : gawhTableH(3,3)=0.03
34290     gawhTableH(4,1)= 3750 : gawhTableH(4,2)=  82.50 : gawhTableH(4,3)=0.04
34300     gawhTableH(5,1)= 5250 : gawhTableH(5,2)= 142.50 : gawhTableH(5,3)=0.05
34310     gawhTableH(6,1)= 7000 : gawhTableH(6,2)= 230.00 : gawhTableH(6,3)=0.06
34320     ! /r
34330   end if 
34340   Ga_StateDeduction=fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
34350   Ga_StatePersonalAllowance=fn_statePersonalAllowance('GA',wga_is_married,wga_eicCode)
34360   if env$('acsDeveloper')<>'' then dev=1 else dev=0
34370   ! if dev then pr 'wga_wages_taxable_current=';wga_wages_taxable_current
34380   ! if dev then pr '   g_pay_periods_per_year=';g_pay_periods_per_year
34382   ! if dev then pr '   Ga_StatePersonalAllowance=';Ga_StatePersonalAllowance
34390   ! if dev then pr '   fn_standardStateDeduction=';Ga_StateDeduction
34410   ! if dev then pr '   wga_allowances=';wga_allowances
34420   ga_WagesAnnual=(wga_wages_taxable_current)*g_pay_periods_per_year
34430   ! if dev then pr '   Ga_WagesAnnual=';Ga_WagesAnnual
34440   ga_WagesAnnualTaxable=Ga_WagesAnnual-Ga_StateDeduction ! fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
34450   ga_WagesAnnualTaxable-=Ga_StatePersonalAllowance ! fn_standardStateDeduction('GA',wga_is_married,wga_eicCode)
34460   ! if dev then pr '   Annual Wages (less state deduction and personal allowance)=';Ga_WagesAnnualTaxable
34480   if wga_is_married=0 then ! SINGLE INDIVIDUAL 
34490     mat gawh(udim(mat gawhTableH,1),udim(mat gawhTableH,2))
34500     mat gawh=gawhTableH
34502     ! if dev then pr '   using Table H'
34510   else if wga_is_married=4 then ! MARRIED FILING JOINT RETURN (both spouses having income) OR MARRIED FILING SEPARATE RETURN
34520     mat gawh(udim(mat gawhTableG,1),udim(mat gawhTableG,2))
34530     mat gawh=gawhTableG
34532     ! if dev then pr '   using Table G'
34540   else if wga_is_married=3 or wga_is_married=2 or wga_is_married=1 then ! MARRIED FILING JOINT RETURN (one spouse having income) OR HEAD OF HOUSEHOLD or 1-Married (nothing else known)
34550     mat gawh(udim(mat gawhTableF,1),udim(mat gawhTableF,2))
34560     mat gawh=gawhTableF
34562     ! if dev then pr '   using Table F'
34570   end if
34580   j1=fn_table_line(mat gawh,Ga_WagesAnnualTaxable)
34582   ! if dev then pr '   table line ';j1
34610   ! if dev then pause
34612   ga_AnnualWagesSubjToWithhold=Ga_WagesAnnualTaxable-gaAnnualDependantAllowance*wga_allowances
34620   s3=gawh(j1,2)+(ga_AnnualWagesSubjToWithhold-gawh(j1,1))*gawh(j1,3)
34630   s3=s3
34640   s3=s3/g_pay_periods_per_year
34650   s3=round(s3,2) ! round to the nearest whole dollar
34660   if s3<.1 then s3=0 ! do not withhold less than 10 cents.
34670   fn_wh_georgia=s3
34680 fnend 
36000 ILWH: ! r: REPLACE ACSWRK\ILLINOIS.WH,SOURCE ! ILLINOIS   NO TABLE
36020   ! line 1 allowances = +1 for claiming self, +1 for claiming spouse
36040   ! line 2 allowances = +1 for each other (not you nor spouse) dependent
36060   ! em(3) - number of allowances
36080   ! g_pay_periods_per_year = number of pay periods (formerly b8)
36100   g2=round((stwh(tcd(1),1))*g_pay_periods_per_year,2)
36120   !  new way needs awesome function !    allowances_line_1=fn_allowances_spouse_and_self
36140   !  new way needs awesome function !    allowances_line_2=em(3)-allowances_line_1
36160   !  new way needs awesome function !    g2=g2-(allowances_line_1*2175+allowances_line_2*1000)
36180   g2=g2-1000*em(3)
36200   s3=g2*.0495 ! changed from .0375 on 7/10/17  ! changed from .03 to .05 1/1/11, changed from .05 to .0375 1/1/15, ok as of 1/6/16
36220   s3=round(s3/g_pay_periods_per_year,2)
36240   if s3<.1 then s3=0 ! do not withhold less than 10 cents.
36260 return  ! /r
38000 INWH: ! r: INDIANA    NO TABLE   07/01/2000  ! still in effect 71508, changed on 1/1/2016, but I didn't bother to update it because no one is using it.
38020   ! Indiana tax table is out of date...  and looks pretty complicated:  http://www.in.gov/dor/reference/files/dn01.pdf
38040   h1=h2=h3=0
38060   h1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
38080   h2=em(3)*1000
38100   h3=h1-h2
38120   if h3>0 then 
38140     s3=h3*.034 ! +H3*.003  SOME COUNTIES HAVE WH
38160     s3=round(s3/g_pay_periods_per_year,2)
38180     if s3<.1 then s3=0
38200   end if 
38220 return  ! /r
42000 def fn_wh_kentuky(wky_wages_taxable_current,g_pay_periods_per_year,wky_allowances)
42020   ! KYWH: ! REPLACE kentucky.wh/acswrk,source ! kentucky:  rec=20  ky(6,3) ! revised 12/31/2005
42040   ! wky_wages_taxable_current - formerly b8
42060   ! g_pay_periods_per_year - formerly stwh(tcd1,1)
42080   if ~wky_setup then 
42100     wky_setup=1
42120     ! r: Pull the withholding routines from new\acswrk
42160     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=20: mat ky ! Kentucky
42180     dim ky(6,3)
42200     ky(1,1)=0     : ky(1,2)=0    : ky(1,3)=0.02
42220     ky(2,1)=3000  : ky(2,2)=60   : ky(2,3)=0.03
42240     ky(3,1)=4000  : ky(3,2)=90   : ky(3,3)=0.04
42260     ky(4,1)=5000  : ky(4,2)=130  : ky(4,3)=0.05
42280     ky(5,1)=8000  : ky(5,2)=280  : ky(5,3)=0.058
42300     ky(6,1)=75000 : ky(6,2)=4166 : ky(6,3)=0.06
42340     ! /r
42360   end if 
42380   h1=(wky_wages_taxable_current)*g_pay_periods_per_year
42400   h2=h1-1970
42420   j1=fn_table_line(mat ky,h2)
42440   s3=ky(j1,2)+(h2-ky(j1,1))*ky(j1,3)
42460   s3=s3-20*wky_allowances
42480   s3=s3/g_pay_periods_per_year
42500   s3=round(s3,2)
42520   if s3<.1 then s3=0 ! do not withhold less than 10 cents.
42540   fn_wh_kentuky=s3
42560 fnend 
44000 LAWH: ! r: REPLACE ACSWRK\LOUSIANA.WH,SOURCE ! LOUISANA: NO TABLE: LA(5): revised 1/01/03
44020   h1=0
44040   h2=0
44060   h3=0
44080   mat la=(0)
44100   s=round(stwh(tcd(1),1),2)
44120   if em(1)=0 or em(1)=2 then 
44140     y=em(3)-1
44160     x=1
44180     if y>=0 then goto L3800
44200     x=0
44220     y=0
44240     goto L3800
44260   end if
44280   if em(3)=0 then y=0 : x=0
44300   if em(3)=1 then y=0 : x=1
44320   if em(3)>=2 then y=em(3)-2 : x=2
44340   L3800: ! 
44360   if x<2 then m1=12500 : m2=25000
44380   if x>=2 then m1=25000 : m2=50000
44400   n=g_pay_periods_per_year
44420   if s>0 then a=(s*.021) else a=0
44440   if s>(m1/n) then b=.0135*(s-(m1/n)) else b=0
44460   if s>(m2/n) then c=.0135*(s-(m2/n)) else c=0
44480   d=.021*(((x*4500)+(y*1000))/n)
44500   if ((x*4500)+(y*1000))>m1 then 
44520     e=.0135*(((x*4500)+(y*1000)-m1)/n)
44540   else 
44560     e=0
44580   end if 
44600   if (a+b+c)-(d+e)>0 then 
44620     s3=(a+b+c)-(d+e)
44640   else 
44660     s3=0
44680   end if 
44700   s3=round(s3,2)
44720   if s3<.1 then s3=0
44740 return  ! /r
46000 MOWH: ! r: REPLACE ACSWRK\MISSOURI.WH,SOURCE ! MISSOURI MO(10,3) REC # 28  REVISED 1/1/2002
46020   if ~setup_mowh then  ! r: MO Missouri
46040     setup_mowh=1
46060     dim mo(10,3)
46080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=28: mat mo ! Missouri
46100     mo( 1,1)=   0 : mo( 1,2)=  0  : mo( 1,3)=0.015
46120     mo( 2,1)=1000 : mo( 2,2)= 15  : mo( 2,3)=0.02
46140     mo( 3,1)=2000 : mo( 3,2)= 35  : mo( 3,3)=0.025
46160     mo( 4,1)=3000 : mo( 4,2)= 60  : mo( 4,3)=0.03
46180     mo( 5,1)=4000 : mo( 5,2)= 90  : mo( 5,3)=0.035
46200     mo( 6,1)=5000 : mo( 6,2)=125  : mo( 6,3)=0.04
46220     mo( 7,1)=6000 : mo( 7,2)=165  : mo( 7,3)=0.045
46240     mo( 8,1)=7000 : mo( 8,2)=210  : mo( 8,3)=0.05
46260     mo( 9,1)=8000 : mo( 9,2)=260  : mo( 9,3)=0.055
46280     mo(10,1)=9000 : mo(10,2)=315  : mo(10,3)=0.06
46300   end if ! /r
46320   ! MARITAL STATUS =2 IF HEAD OF HOUSEHOLD
46340   numb4=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
46360   if em(1)=0 or em(1)=2 then numb6=min(5000,fed_wh_annual_estimate) ! FEDERAL DED LIMITED TO 5000 FOR SINGLE
46380   if em(1)<>0 then numb6=min(10000,fed_wh_annual_estimate) ! FEDERAL DED LIMITED TO 10000 FOR MARRIED OR HEAD OF HOUSEHOLD
46400   if em(1)=1 or em(1)=3 or em(1)=4 or em(1)=5 then h1=3925 : goto L4110
46420   if em(1)=2 then h1=7850 : goto L4110
46440   h1=4700
46460   goto L4110
46480   L4110: ! 
46500   h2=0
46510   ! on em(1)+1 goto L4160,L4140,L4180 none L4190
46520   if em(3)<>0 then 
46530     !
46540     if em(1)=0 then 
46550       h2=1200+(em(3)-1)*1200 ! SINGLE
46560     else if em(1)=1 or em(1)=3 or em(1)=4 or em(1)=5 then 
46570       h2=min(em(3),2)*1200+max(em(3)-2,0)*1200 ! MARRIED
46580     else if em(1)=2 then 
46590       h2=3500+max(em(3)-4,0)*1200 ! HEAD OF HOUSE HOLD
46600     end if
46610   end if
46700   h3=numb4-h1-h2-numb6
46720   if h3<0 then h3=0
46740   j1=fn_table_line(mat mo,h3)
46860   s3=(mo(j1,2)+(h3-mo(j1,1))*mo(j1,3))/g_pay_periods_per_year
46880   s3=round(s3,0)
46900   if s3<.1 then s3=0
46920 return  ! /r
48000 MSWH: ! r: REPLACE ACSWRK\MISISIPI.WH,SOURCE ! MISSISSIPPI  NO TABLE
48020   ! **********  REMOVE THE EM(15) FROM LINE 740 **********
48040   ! SUBSTITUTE THE EXEMPTIONS INTO THE FIELD NOW CALLED STATE TAX ADD-ON
48060   ! THE EXEMPTIONS MUST BE ENTERED IN DOLLARS AND THE STANDARD DEDUCTION
48080   ! MUST BE ADDED TO THE EXEMPTIONS.
48100   ! SINGLE =2300, MARRIED=3400, MARRIED BOTH WORKING=1700
48120   h1=round(stwh(tcd(1),1)*g_pay_periods_per_year,2)
48140   h3=h1-em(15)
48160   if h3<=0 then s3=0 : goto L4481
48180   if h3<10000 then goto L4474
48200   s3=350+.05*(h3-10000)
48220   goto L4481
48240   L4474: if h3>0 and h3<=5000 then goto L4477
48260   s3=150+.04*(h3-5000)
48280   goto L4481
48300   L4477: s3=.03*h3
48320   if s3<.1 then s3=0
48340   goto L4481
48360   L4481: s3=s3/g_pay_periods_per_year
48380   s3=round(s3,2)
48400   if s3<.1 then s3=0
48420 return  ! /r
52000 OKWH: ! r:  ACSWRK\OKLAHOMA.WH,SOURCE ! rec=39 ok(8,6) REV. 1/01/07 (table change also!)
52020   if ~setup_okwh then ! r: OK Oklahoma
52040     setup_okwh=1
52060     dim ok(8,6)
52080     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=39: mat ok ! Oklahoma
52100     ! r: single 
52120     ok(1,1)=    0 : ok(1,2)=   0   : ok(1,3)=0
52140     ok(2,1)= 6300 : ok(2,2)=   0   : ok(2,3)=0.005
52160     ok(3,1)= 7300 : ok(3,2)=   5   : ok(3,3)=0.01
52180     ok(4,1)= 8800 : ok(4,2)=  20   : ok(4,3)=0.02
52200     ok(5,1)=10050 : ok(5,2)=  45   : ok(5,3)=0.03
52220     ok(6,1)=11200 : ok(6,2)=  79.5 : ok(6,3)=0.04
52240     ok(7,1)=13500 : ok(7,2)= 171.5 : ok(7,3)=0.05
52260     ok(8,1)=15000 : ok(8,2)= 246.5 : ok(8,3)=0.0525
52280     ! /r
52300     ! r: married 
52320     ok(1,4)=    0  : ok(1,5)=  0 : ok(1,6)=0
52340     ok(2,4)=12600  : ok(2,5)=  0 : ok(2,6)=0.005
52360     ok(3,4)=14600  : ok(3,5)= 10 : ok(3,6)=0.01
52380     ok(4,4)=17600  : ok(4,5)= 40 : ok(4,6)=0.02
52400     ok(5,4)=20100  : ok(5,5)= 90 : ok(5,6)=0.03
52420     ok(6,4)=22400  : ok(6,5)=159 : ok(6,6)=0.04
52440     ok(7,4)=24800  : ok(7,5)=255 : ok(7,6)=0.05
52460     ok(8,4)=27600  : ok(8,5)=395 : ok(8,6)=0.0525
52480     ! /r
52500   end if ! /r
52520   g2=stwh(tcd(1),1)*g_pay_periods_per_year
52540   g2=g2-em(3)*1000
52560   if em(1)=0 or em(1)=2 then j2=1 else j2=4 ! single of married
52580   j1=fn_table_line(mat ok,g2)
52600   s3=ok(j1,j2+1)+(g2-ok(j1,j2))*ok(j1,j2+2)
52620   s3=s3/g_pay_periods_per_year
52640   s3=round(s3,2)
52660   s3=round(s3,0)
52680   if s3<.1 then s3=0
52700 return  ! /r
54000 def fn_wh_oregon(wor_wages_taxable_current,wor_fed_wh_annual_estimate,wor_pay_periods_per_year,wor_allowances,wor_is_married)
54020   if ~wor_setup then 
54040     wor_setup=1
54060     ! read #h_tables,using 'Form POS 31,102*PD 6.4',rec=40: mat or1,mat or2
54080     dim or1(4,3) !  r: Withholding Table for Single with fewer than 3 allowances
54100     or1(1,1)=    0 : or1(1,2)= 197   : or1(1,3)=0.05
54120     or1(2,1)= 3350 : or1(2,2)= 367   : or1(2,3)=0.07
54140     or1(3,1)= 8450 : or1(3,2)= 724   : or1(3,3)=0.09
54160     or1(4,1)=50000 : or1(4,2)=4459.5 : or1(4,3)=0.09
54180     ! /r
54200     dim or2(4,3) ! r: Single with 3 or more allowances, or married
54220     or2(1,1)=    0 : or2(1,2)= 197 : or2(1,3)=0.05
54240     or2(2,1)= 6700 : or2(2,2)= 537 : or2(2,3)=0.07
54260     or2(3,1)=16900 : or2(3,2)=1251 : or2(3,3)=0.09
54280     or2(4,1)=50000 : or2(4,2)=4223 : or2(4,3)=0.09
54300     ! /r
54320   end if 
54340     ! requires locally populated variables Mat OR1 and Mat OR2
54360     ! returns Oregon State Withholding
54380     ! Oregon  !  rec=40
54400     ! 
54420     ! RECALK: ! used only for debugging purposes
54440   wor_allowances_effective=wor_allowances
54460     ! 
54480   wor_they_are_single=wor_they_are_married=0
54500   if wor_is_married=0 or wor_is_married=2 then wor_they_are_single=1
54520   if wor_is_married=1 or wor_is_married=3 or wor_is_married=4 or wor_is_married=5 then wor_they_are_married=1
54540     ! 
54560   if wor_wages_taxable_current>100000 and wor_they_are_single then wor_allowances_effective=0
54580   if wor_wages_taxable_current>200000 and wor_they_are_married then wor_allowances_effective=0
54600     ! 
54620   if wor_they_are_married or (wor_they_are_single and wor_allowances_effective>=3) then ! (married or more than 3 allowances)
54640     wor_table=2
54660   else ! (single and less than 3 allowances)
54680     wor_table=1
54700   end if 
54720     ! 
54740   if wor_table=2 then ! wor_they_are_married then
54760     wor_standard_deduction=4350
54780   else ! if wor_table=1 then ! if wor_they_are_single then
54800     wor_standard_deduction=2175
54820   end if 
54840     ! 
54860   wor_wages_annual_estimate=wor_wages_taxable_current*wor_pay_periods_per_year
54880     ! 
54900   wor_phase_out=fn_oregonPhaseOut(wor_wages_annual_estimate,wor_fed_wh_annual_estimate,wor_table,wor_they_are_single,wor_they_are_married)
54920     ! 
54940     ! wor_base=wor_wages_taxable_current*wor_pay_periods_per_year-min(wor_fed_wh_annual_estimate,8550)-(wor_allowances_effective*2250)
54960   wor_base=wor_wages_annual_estimate-wor_phase_out-wor_standard_deduction
54980     ! 
55000   if wor_table=2 then 
55020     wor_table_line=fn_table_line(mat or2,wor_base)
55040     wor_pre_base=or2(wor_table_line,2)
55060     wor_tax_rate=or2(wor_table_line,3)
55080     wor_remove_prev=or2(wor_table_line,1)
55100   else ! wor_table=1
55120     wor_table_line=fn_table_line(mat or1,wor_base)
55140     wor_pre_base=or1(wor_table_line,2)
55160     wor_tax_rate=or1(wor_table_line,3)
55180     wor_remove_prev=or1(wor_table_line,1)
55200   end if 
55220   if debug then let fnStatus('-------------------------------------------')
55240   if wor_they_are_single then 
55260     if debug then let fnStatus('  Single with '&str$(wor_allowances_effective)&' allowances')
55280   else if wor_they_are_married then 
55300     if debug then let fnStatus('  Married with '&str$(wor_allowances_effective)&' allowances')
55320   else 
55340     if debug then let fnStatus('  Maridal Status is undetermined!!!  ')
55360   end if 
55380   if debug then let fnStatus('    Current Wage (Gross)    = '&str$(wor_wages_taxable_current))
55400   if debug then let fnStatus('    Pay Periods Per Year    = '&str$(wor_pay_periods_per_year))
55420   if debug then let fnStatus('    Annual wage (estimate)  = '&str$(wor_wages_annual_estimate))
55440   if debug then let fnStatus('    standard deduction     = '&str$(wor_standard_deduction))
55460   if debug then let fnStatus('    table '&str$(wor_table)&' line '&str$(wor_table_line))
55480   if debug then let fnStatus('    phase out              = '&str$(wor_phase_out))
55500   if debug then let fnStatus('    fed_wh_annual_estimate = '&str$(wor_fed_wh_annual_estimate))
55520   if debug then let fnStatus('.')
55540   if debug then let fnStatus('    BASE = '&str$(wor_wages_annual_estimate)&' (an..wages) - '&str$(wor_phase_out)&' (phase out/fed wh) - '&str$(wor_standard_deduction)&' (std ded)')
55560   if debug then let fnStatus('    base                   = '&str$(wor_base))
55580     ! fn  status('    pre_base               = '&str$(wor_pre_base))
55600     ! fn  status('    tax rate               = '&str$(wor_tax_rate))
55620     ! fn  status('    remove_prev            = '&str$(wor_remove_prev))
55640   if debug then let fnStatus('.')
55660   if debug then let fnStatus('                                   WH = '&str$(wor_pre_base)&' + [('&str$(wor_base)&' - '&str$(wor_remove_prev)&')] x '&str$(wor_tax_rate)&'] - (195 x '&str$(wor_allowances_effective)&')')
55680     ! 
55700     ! WH = 1,244 + [(BASE – 16,900        ) * 0.09] – (195 * allowances)
55720     ! wor_return=or2(wor_table_line,2)+(wor_base-or2(wor_table_line,1))*or2(wor_table_line,3)
55740   wor_return = wor_pre_base +(( wor_base - wor_remove_prev) * wor_tax_rate) - (195 * wor_allowances_effective)
55760   fnStatus('withholding before dividing by pay periods = '&str$(wor_return))
55780   wor_return=wor_return/wor_pay_periods_per_year
55800   wor_return=round(wor_return,2)
55820   if wor_return<.1 then wor_return=0
55840   fnStatus('calculated withholding ='&str$(wor_return))
55860   if debug then let fnStatusPause ! pause
55880   fn_wh_oregon=wor_return
55900 fnend 
58000 def fn_oregonPhaseOut(opo_wages,opo_fed_wh,opo_table,opo_is_single,opo_is_married)
58020   if opo_wages<50000 then 
58040     opo_return=min(opo_fed_wh,6500)
58060   else if opo_table=1 then 
58080     if opo_wages => 50000 and opo_wages<125000 then opo_return= 6550 : goto OPO_XIT
58100     if opo_wages =>125000 and opo_wages<130000 then opo_return= 5200 : goto OPO_XIT
58120     if opo_wages =>130000 and opo_wages<135000 then opo_return= 3900 : goto OPO_XIT
58140     if opo_wages =>135000 and opo_wages<140000 then opo_return= 2600 : goto OPO_XIT
58160     if opo_wages =>140000 and opo_wages<145000 then opo_return= 1300 : goto OPO_XIT
58180     if opo_wages =>145000 then opo_return=0
58200   else ! if opo_table=2 then
58220     if opo_is_married then 
58240       if opo_wages => 50000 and opo_wages<250000 then opo_return= 6550 : goto OPO_XIT
58260       if opo_wages =>250000 and opo_wages<260000 then opo_return= 5200 : goto OPO_XIT
58280       if opo_wages =>260000 and opo_wages<270000 then opo_return= 3900 : goto OPO_XIT
58300       if opo_wages =>270000 and opo_wages<280000 then opo_return= 2600 : goto OPO_XIT
58320       if opo_wages =>280000 and opo_wages<290000 then opo_return= 1300 : goto OPO_XIT
58340       if opo_wages =>290000 then opo_return=0
58360     else ! if opo_is_single then
58380       if opo_wages => 50000 and opo_wages<125000 then opo_return= 6550 : goto OPO_XIT
58400       if opo_wages =>125000 and opo_wages<130000 then opo_return= 5200 : goto OPO_XIT
58420       if opo_wages =>130000 and opo_wages<135000 then opo_return= 3900 : goto OPO_XIT
58440       if opo_wages =>135000 and opo_wages<140000 then opo_return= 2600 : goto OPO_XIT
58460       if opo_wages =>140000 and opo_wages<145000 then opo_return= 1300 : goto OPO_XIT
58480       if opo_wages =>145000 then opo_return=0
58500     end if 
58520   end if 
58540   OPO_XIT: ! 
58560   fn_oregonPhaseOut=opo_return
58580 fnend 

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
