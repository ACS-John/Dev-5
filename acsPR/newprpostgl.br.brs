00010 ! Replace S:\acsPR\newprPostGL
00020 ! Payroll Post to General Ledger
12000 ! r: setup library, on err, dims, fntop, etc
12020   library 'S:\Core\Library': fntop,fnxit, fnwait,fnoldmsgbox,fnwin3,fnerror,fnopenprn,fncloseprn,fnchain,fntos,fnlbl,fntxt,fncmdkey,fnacs,fndate_mmddyy_to_ccyymmdd,fnmsgbox,fnqgl,fnrgl$,fnagl$,fnchk,fnGetPayrollDates,fnDedNames,fnclient_has,fnIndex_it,fnstatus_close
12040   on error goto ERTN
12060   ! ______________________________________________________________________
12080   dim a$*40,em$*30,tgl(3),tcp(32),eno$*8,ttgl(3),oldtgl(3)
12100   dim tr(7),tr$*12,td$*30,dat$*20,a(100),i$*21,glwk$*30,desc$*50
12120   dim tgl$*12,oldtgl$*12,t(26),prgl(26,3),prgl$(15)*12,dedcode(20),cap$*128
12140   dim message$*40,msgline$(2)*60,ml$(4)*80,resp$(10)*60
12160   dim fullname$(20)*20,abbrevname$(20)*8,newcalcode(20),newdedfed(20),dedfica(20)
12180   dim dedst(20),deduc(20),gl$(20)*12,d1$*20
12200   ! ______________________________________________________________________
16000   let fntop(program$,cap$="Post to General Ledger")
16040   fnIndex_it(env$('Q')&'\PRmstr\Department.h'&env$('cno'),env$('Q')&'\PRmstr\DeptId4.h'&env$('cno'),'12/1/9 12/8/3') ! sort department file in general ledger sequence
16050   fnstatus_close
16060   let fnopenprn
16080   ! ______________________________________________________________________
16100   open #20: "Name="&env$('Q')&"\GLmstr\GLBucket.h"&env$('cno')&",Shr",internal,input,relative ioerr L260
16120   read #20,using 'Form POS 1,N 1',rec=1: glb norec ignore
16140   close #20: 
16160   if glb=2 then let fn_askaccrue
16200   L260: ! 
16180   ! ______________________________________________________________________
18000   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
18020   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,input,keyed 
18040   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
18060   if fnclient_has('CL') then ! exists(env$('Q')&"\CLmstr") then 
18080     mat ml$(3)
18100     let ml$(1)="Normally you would not take this menu option to post"
18120     let ml$(2)="General Ledger if you have the Checkbook system."
18140     let ml$(3)="Click OK to continue or Cancel to stop."
18160     let fnmsgbox(mat ml$,resp$,cap$,1)
18180     if resp$="OK" then goto ASK_DATE else goto XIT
18200   end if
18220 goto ASK_DATE ! /r
22000 ASK_DATE: ! 
22020   let fntos(sn$="PostGl")
22040   let respc=0
22060   let fnlbl(1,40,"",1,1) ! bigger screen
22080   let fnlbl(1,1,"Beginning Payroll Date:",25,1)
22100   let fntxt(1,28,10,0,1,"3",0,"For current payroll, always use the calculation date.  You can post or re-post older payrolls by using the older payroll date.")
22120   let resp$(respc+=1)=str$(d1)
22140   let fnlbl(2,1,"Ending Payroll Date:",25,1)
22160   let fntxt(2,28,10,0,1,"3",0,"You can post a ranges of payrolls by entering a beginning and ending date.  Use the same date for a single payroll.")
22180   let resp$(respc+=1)=str$(d1)
22200   let fnchk(3,28,"Print Report Only:",1)
22220   let resp$(respc+=1)="False"
22240   let fncmdkey("&Next",1,1,0,"Proceed with posting." )
22260   let fncmdkey("E&xit",5,0,1,"Returns to menu")
22280   let fnacs(sn$,0,mat resp$,ckey) ! ask payroll date
24000   if ckey=5 then goto XIT
24020   let dat1=d1=val(resp$(1))
24040   let dat2=d2=val(resp$(2))
24060   if resp$(3)="True" then let skipposting=1
24080   if glb=2 then let glwk$=env$('Q')&"\GLmstr\GL"&date$(days(dat1,'ccyymmdd'),'mmddyy')&".h"&env$('cno')
24100   if glb><2 then let glwk$=env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno')
24120   if glb=2 and accrue$="Yes" then 
24140     open #11: "Name="&env$('Q')&"\GLmstr\GL"&date$(days(d2,'ccyymmdd'),'mmddyy')&".h"&env$('cno')&",RecL=104,Use",internal,output 
24160   end if 
24180 ! 
26000   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
26020   read #1,using 'Form POS 1,C 40,POS 437,15*C 12,N 1,POS 618,10*N 1': a$,mat prgl$,glinstal ! need to get from other file
26040   for j=1 to 4 ! 1=fed 2=fica/med 3=med 4=state
26060     if j=3 then 
26080       let prgl(j,1)=val(prgl$(2)(1:3))
26100       let prgl(j,2)=val(prgl$(2)(4:9))
26120       let prgl(j,3)=val(prgl$(2)(10:12))
26140     else 
26160       let prgl(j,1)=val(prgl$(j)(1:3))
26180       let prgl(j,2)=val(prgl$(j)(4:9))
26200       let prgl(j,3)=val(prgl$(j)(10:12))
26220     end if 
26240   next j
26260   for j=5 to 24
26280     let prgl(j,1)=val(gl$(j-4)(1:3))
26300     let prgl(j,2)=val(gl$(j-4)(4:9))
26320     let prgl(j,3)=val(gl$(j-4)(10:12))
26340   next j
26360   let j=25: let prgl(j,1)=val(prgl$(14)(1:3))
26380   let prgl(j,2)=val(prgl$(14)(4:9))
26400   let prgl(j,3)=val(prgl$(14)(10:12)) ! eic
26420   let j=26: let prgl(j,1)=val(prgl$(15)(1:3))
26440   let prgl(j,2)=val(prgl$(15)(4:9))
26460   let prgl(j,3)=val(prgl$(15)(10:12)) ! cash
26480   let nametab=36-len(rtrm$(a$))/2
26500   close #1: 
26520   open #2: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,input,keyed 
26540   open #6: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptId4.h"&env$('cno')&",Shr",internal,outin,keyed 
26560   let fn_pr_hdr
28000   do
28020     DEPT_READ: ! 
28040     mat oldtgl=tgl
28060     read #6,using "Form POS 1,N 8,POS 12,n 3,n 6,n 3,pos 42,n 6": teno,mat tgl,paydat eof L1400
28080     ! If fndate_mmddyy_to_ccyymmdd(PAYDAT)<DAT1 OR fndate_mmddyy_to_ccyymmdd(PAYDAT)>DAT2 Then Goto 770 ! payroll date in department record must match
28100     let oldtgl$=tgl$
28120     mat ttgl=oldtgl
28140     let oldteno=teno
28160     checkkey$=cnvrt$("pic(zzzzzzzz)",teno)&"         "
28180     restore #4,key>=checkkey$: nokey DEPT_READ
28200     L840: ! 
28220     read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prdate,ckno,mat tdc,mat tcp eof DEPT_READ
28240     if heno<>teno then goto DEPT_READ
28260     if prdate<dat1 or prdate>dat2 then goto L840
28280     ! If SUM(TGL)=0 Then Goto 500
28300     if mastercd=0 then let fn_l1800
28320     let tgl$=lpad$(str$(tgl(1)),3)&lpad$(str$(tgl(2)),6)&lpad$(str$(tgl(3)),3)
28340     if oldtgl$=tgl$ or oldtgl$="" then goto L920
28360     let fn_write_gl_trans
28380     L920: ! 
28400     if tgl(1)=0 or tgl(1)=oldtgl then goto L930 else let fn_l1800 ! No Cost Center or same Cost Center
28420     L930: ! If OLDTENO=TENO Then Goto 830
28440     let eno$=lpad$(str$(teno),8)
28460     read #2,using L960,key=eno$: em$ nokey DEPT_READ
28480     L960: form pos 9,c 30
28500     pr #255,using L980: teno,em$,mat tgl,tcp(31) ! -TCP(29)-TCP(30) Pageoflow PR_HDR kj
28520     L980: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),n 12.2,skip 1
28540     for j=1 to 24 ! ACCUMULATE 20 WITHHOLDINGS plus fed,fica,med,state
28560       if j<=4 then goto L1040
28580       if dedcode(j-4)=1 then goto L1040
28600       let t(j)=t(j)+tcp(j)
28620       goto L1050
28640       L1040: ! 
28660       let t(j)=t(j)-tcp(j)
28680       L1050: ! 
28700     next j
28720     let t(25)=t(25)+tcp(25) ! EIC
28740     let t(26)=t(26)-tcp(32) ! ACCUMULATE NET
28760     let subtotal=subtotal+tcp(31) ! -TCP(19)-TCP(20)
28780     ! accumulate total by acct to be posted to gl kj
28800     let totaldue=totaldue-tcp(31) ! +TCP(29)+TCP(30) ! DUE TO PAYROLL CLEARING kj
28820     let totaldr=totaldr+tcp(31) ! -TCP(29)-TCP(30)  ! kj
28840     let totalrec=totalrec+tcp(31) ! -TCP(29)-TCP(30) ! TOTAL DUE FROM OTHER FUNDS  kj
28860   loop
28880 ! ______________________________________________________________________
32000 L1400: ! r:
32020   let eofcode=1
32040   mat ttgl=tgl
32060   let oldtgl$=tgl$
32080   let fn_write_gl_trans ! WRITE LAST ENTRY
32100   if multigl=1 then ! ONLY ONE FUND OR COMPANY
32120     let fn_l1800
32140     let fn_finalscrctrlbookmulitfunds
32160   end if
32180   let fn_assglnum ! POST WH AND NET
32200   let fn_printtotalsandunderlines
32220   close #1: ioerr ignore
32240   close #2: ioerr ignore
32260   close #4: ioerr ignore
32280   ! 
32300   let fncloseprn
32320   if ~skipposting=1 and glinstal and glb<>2 then 
32340     fnchain("S:\acsGL\ACGLMRGE")
32360   end if
32380 goto XIT ! /r
34000 PgOf: !  r:
34020   pr #255: newpage
34040   let fn_pr_hdr
34060 continue ! /r
36000 def fn_write_gl_trans ! SUBTOTAL ROUTINE AND WRITE GL TRANS
36020   if glinstal then
36040   if diskin=0 then let fn_L2090
36060   let td$="Payroll summary"
36080   if accrue$="Yes" then 
36100     accrued=round(subtotal/day*dayslm,2)
36120     write #11,using L1220: mat ttgl,date(days(d2,'ccyymmdd'),'mmddyy'),accrued,5,0,tr$,"Payroll Accrual",prgl$(15)
36140     let totacc=totacc+accrued
36160   end if
36180   write #14,using L1220: mat ttgl,dat,subtotal-accrued,5,0,tr$,"Payroll Summary",prgl$(15) ! gross wages for this department
36200   L1220: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 52,c 12
36220   end if
36240   pr #255,using L1240: "-----------",subtotal
36260   L1240: form pos 65,c 11,skip 1,pos 64,pic(---------.##),skip 1
36280   if accrued<>0 then pr #255,using L1260: "Accrued Portion",-accrued else pr #255: 
36300   L1260: form pos 45,c 16,pos 64,pic(---------.##),skip 2
36320   let subtotal=0
36340 fnend 
38000 def fn_pr_hdr
38020   let p1=p1+1
38040   pr #255,using L1320: date$,a$,"PAGE",p1
38060   L1320: form skip 1,pos 1,c 8,pos nametab,c 40,pos 77,c 5,pic(zzz),skip 1
38080   pr #255,using L1340: time$, "General Ledger Distribution for Payroll","From "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dat1)&"  To "&cnvrt$("PIC(ZZZZ/ZZ/ZZ)",dat2)
38100   L1340: form pos 1,c 8,pos 17,c 40,skip 1,pos 17,cc 40,skip 2
38120   pr #255: "Employee                                               G/L                 Amount"
38140   pr #255: " Number        Name                                  Account         Debits     Credits"
38160   pr #255: 
38180 fnend 
42000 def fn_assglnum ! assign g/l numbers and post to gl work file
42020   for j=1 to 26
42040     if t(j)=0 then goto L1740
42060     if t(j)<0 then goto L1690
42080     pr #255,using L980: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),t(j) pageoflow PgOf
42100     let totaldr=totaldr+t(j)
42120     goto L1720
42140     L1690: !
42160     pr #255,using L1700: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),t(j) pageoflow PgOf
42180     L1700: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),x 12,n 12.2,skip 1
42200     let totalcr=totalcr+t(j)
42220     L1720: !
42240     if glinstal=0 then goto L1740
42260     write #14,using L1220: prgl(j,1),prgl(j,2),prgl(j,3),dat,t(j),5,0,tr$,td$,prgl$(15) ! write summary entries for deductions and net pay
42280     L1740: !
42300   next j
42320   if accrue$<>"Yes" then goto L1780
42340   write #11,using L1220: g1,g2,g3,date(days(d2,'ccyymmdd'),'mmddyy'),-totacc,5,0,tr$,"Payroll Accrual",prgl$(15)
42360   write #14,using L1220: g1,g2,g3,dat,totacc,5,0,tr$,"Payroll Accrual",prgl$(15)
42380   L1780: ! 
42400 fnend 
44000 def fn_l1800 ! OPEN G/L WORK FILES AND CREATE DUE TO AND DUE FROM ENTRIES
44020   if tgl(1)=0 then goto L2080
44040   if mastercd=1 then goto L1870
44060   mat ml$(4)
44080   let ml$(1)="The G/L accounts you are using indicate you have seperate funds or"
44100   let ml$(2)="cost centers on the system.  Click Yes if you do have more than."
44120   let ml$(3)="one set of books that are self balancing in your system."
44140   let ml$(4)="Click NO if you are using the cost center code for other purposes."
44160   let fnmsgbox(mat ml$,resp$,cap$,52)
44180   if resp$="Yes" then let multigl=1 else let multigl=2
44200   if multigl><1 then goto L1870
44220   let mulitidsk=1
44240   L1870: ! 
44260   if multigl=2 then goto L2080
44280   ! CREATE DUE TO PAYROLL FUND ENTRIES
44300   if mastercd=0 then goto L2050 ! FIRST TIME THRU ROUTINE
44320   let fntos(sn$="PostGl3")
44340   let respc=0: let mypos=45
44360   let fnlbl(1,1,"Due to Payroll Clearing Account on Fund # "&oldtgl$(1:3)&":",mypos,1)
44380   let fnqgl(1,mypos+3,0,2,pas)
44400   let resp$(1)=fnrgl$(bankgl$)
44420   let fncmdkey("&Next",1,1,0,"Continue posting." )
44440   let fncmdkey("E&xit",5,0,1,"Returns to menu")
44460   let fnacs(sn$,0,mat resp$,ckey) ! ask clearing
44480   if ckey=5 then goto XIT
44500   let key$=k$=bankgl$=fnagl$(resp$(1))
44520   let ttgl(1)=val(key$(1:3)): let ttgl(2)=val(key$(4:9)): let ttgl(3)=val(key$(10:12))
44540   pr #255,using L1700: 0," ",mat ttgl,totaldue
44560   let totalcr=totalcr+totaldue
44580   let fn_printtotalsandunderlines
44600   let fn_pr_hdr
44620   if glinstal=0 then goto L2050
44640   write #14,using L1220: mat ttgl,dat,totaldue,5,0," ","Payroll Summary",prgl$(15)
44660   close #14: 
44680   L2050: !
44700   let totaldue=0
44720   let totalcr=0 : let totaldr=0
44740   let i$="for Cost Center # "&str$(tgl(1))
44760   L2080: !
44780   let mastercd=1
44800   fn_L2090
44820 fnend
46000 def fn_L2090
46020   if glinstal=0 then goto L2210
46040   let diskin=1
46060   open #14: "Name="&glwk$,internal,outin ioerr L2170
46080   read #14,using L2130: dat3,trcode eof L2130
46100   L2130: form pos 13,n 6,pos 25,n 2
46120   if dat3=dat and trcode=5 then goto L2210
46140   if glb=2 then goto L2210
46160   close #14,free: 
46180   L2170: !
46200   open #14: "Name="&glwk$&",Replace,RecL=104",internal,output 
46220   goto L2210
46240   close #14: 
46260   open #14: "Name="&glwk$,internal,output ioerr L2210
46280   L2210: ! 
46300   let oldtgl=tgl(1)
46320 fnend 
48000 def fn_finalscrctrlbookmulitfunds
48020   ! FINAL PAGE FOR CONTROL SET OF BOOKS  (MULTI-FUNDS ONLY)
48040   let fntos(sn$="PostGl4")
48060   let respc=0: let mypos=45
48080   let fnlbl(1,1,"G/L # for Due From Other Funds on Fund # "&oldtgl$(1:3)&":",mypos,1)
48100   let fnqgl(1,mypos+3,0,2,pas)
48120   let resp$(1)=fnrgl$(bankgl$)
48140   let fncmdkey("&Next",1,1,0,"Continue posting." )
48160   let fncmdkey("E&xit",5,0,1,"Returns to menu")
48180   let fnacs(sn$,0,mat resp$,ckey) ! ask clearing
48200   if ckey=5 then goto XIT
48220   let key$=fnagl$(resp$(1))
48240   let ttgl(1)=val(key$(1:3)): let ttgl(2)=val(key$(4:9)): let ttgl(3)=val(key$(10:12))
48260   pr #255,using L980: 0," ",mat ttgl,totalrec
48280   let totaldr+=totalrec
48300   if glinstal then 
48320     write #14,using L1220: mat ttgl,dat,totalrec,5,0," ","Payroll Summary",prgl$(15)
48340   end if
48360 fnend 
52000 def fn_printtotalsandunderlines: ! pr TOTALS AND UNDERLINES
52020   if totacc<>0 then pr #255,using L980: 0," ",g1,g2,g3,totacc
52040   pr #255,using L2420: "___________","___________",totaldr,totalcr
52060   L2420: form pos 65,c 11,x 1,c 11,skip 1,pos 64,pic(---------.##),pic(---------.##)
52080   pr #255,using L2420: "===========","==========="
52100   pr #255: newpage
52120 fnend 
56000 def fn_askaccrue
56020   open #12: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLindex.h"&env$('cno')&",Shr",internal,input,keyed ioerr L2500
56040   let glthere=1
56060   L2500: ! 
56080   let msgline$(1)="Do you wish to accrue part of this Payroll"
56100   let msgline$(2)="in the previous month?"
56120   let fnmsgbox(mat msgline$,resp$,cap$,4)
56140   accrue$=resp$
56160   if accrue$<>"Yes" then goto ASKACCRUE_XIT
56180   ! ______________________________________________________________________
56200   ACCRUAL: ! r:
56220   let fntos(sn$="PostGl5")
56240   let respc=0: let mypos=50
56260   let fnlbl(1,1,"Number of Days in this Pay Period:",mypos,1)
56280   let fntxt(1,mypos+3,10,0,1,"30",0,"In order to know how much to accure, the system needs to know the days to accure.")
56300   let resp$(1)=str$(day)
56320   let fnlbl(2,1,"Number of Days to Expense in Last Month:",mypos,1)
56340   let fntxt(2,mypos+3,10,0,1,"30",0,"In order to know how much to accure, the system needs to know the days to accure.")
56360   let resp$(2)=str$(dayslm)
56380   let fnlbl(3,1,"G/L # for Due From Other Funds on Fund # "&oldtgl$(1:3)&":",mypos,1)
56400   let fnqgl(3,mypos+3,0,2,pas)
56420   let resp$(3)=fnrgl$(bankgl$)
56440   let fnlbl(4,1,"Last Day of Previous Month:",mypos,1)
56460   let fntxt(4,mypos+3,10,0,1,"1",0,"Enter the month end date.")
56480   let resp$(4)=str$(d2)
56500   let fncmdkey("&Next",1,1,0,"Continue posting." )
56520   let fncmdkey("E&xit",5,0,1,"Returns to menu")
56540   let fnacs(sn$,0,mat resp$,ckey) ! ask accrual info
56560   if ckey=5 then goto XIT
56580   let day=val(resp$(1)) ! days in pay period
56600   let dayslm=val(resp$(2)) ! days last month
56620   let key$=fnagl$(resp$(3))
56640   let g1=val(key$(1:3)): let g2=val(key$(4:9)) : let g3=val(key$(10:12))
56660   let d2=val(resp$(4)) ! last day previous month
56680   acgl$=cnvrt$("N 3",g1)&cnvrt$("N 6",g2)&cnvrt$("N 3",g3)
56700   if glthere=1 then 
56720     read #12,using L2860,key=acgl$: desc$ nokey ASKACCRUE_XIT
56740   end if 
56760   if day<1 or day>31 then
56780     mat ml$(2)
56800     let ml$(1)="Invalid number of days in pay period!"
56820     let ml$(2)="Click OK to fix."
56840     let fnmsgbox(mat ml$,resp$,cap$,0)
56860     goto ACCRUAL
56880   end if
56900   if d2<10100 or d2>123199 then 
56920     mat ml$(2)
56940     let ml$(1)="Invalid date for last day of month!"
56960     let ml$(2)="Click OK to fix."
56980     let fnmsgbox(mat ml$,resp$,cap$,0)
57000     goto ACCRUAL
57020   end if
57040   if dayslm<1 or dayslm>31 then 
57060     mat ml$(2)
57080     let ml$(1)="Invalid number of days for last month!"
57100     let ml$(2)="Click OK to fix."
57120     let fnmsgbox(mat ml$,resp$,cap$,0)
57140     goto ACCRUAL
57160   end if
57180   if dayslm>day then goto ACCRUAL
57200   acgl$=cnvrt$("N 3",g1)&cnvrt$("N 6",g2)&cnvrt$("N 3",g3)
57220   if glthere=1 then read #12,using L2860,key=acgl$: desc$ nokey ASKACCRUE_XIT
57240   L2860: form pos 13,c 50
57260   ASKACCRUE_XIT: ! ! /r
57280 fnend 
58060 XIT: let fnxit
58080 ! ______________________________________________________________________
62000 ERTN: let fnerror(program$,err,line,act$,"NO")
62020   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
62040   if uprc$(act$)="PAUSE" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT ! if env$("ACSDeveloper")<>"" then execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
62060   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
62080 ERTN_EXEC_ACT: execute act$ : goto ERTN
62100 ! ______________________________________________________________________
