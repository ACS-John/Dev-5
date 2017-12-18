00010 ! Replace S:\acsPR\newJCCPR2
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fncno,fnerror,fnDedNames
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   dim ss$*11,jn$*6,tr(9),en$*8,n$*40
00070   dim tded(6),tdc(5),cnam$*40,dedcode(10),a2$*70,cap$*128,message$*40
00080   dim jn1$*6,tr1(9),en1$*8,pl1$(6)*30,pl2$(6)*6,dr(7),hr1(8),hr2(8),ded(6)
00090   dim em$(3)*30,tdet(17),tdy(6),tdc(10),tcp(32),tdep(20,26),ttc(32)
00100   dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),ytdtotal(32)
00110   dim quartertotals(32),dedfed(20),calcode(20),dedcode(20)
00120   dim fullname$(20)*20,abrevname$(20)*8,ml$(2)*80
00130   dim dedfica(20),dedst(20),deduc(20),gl$(20)*12,client$*30
00140 ! ______________________________________________________________________
00150   fncno(cno,cnam$)
00160 ! 
00170   fntop(program$,cap$="Certified Payroll Register")
00180 ! ______________________________________________________________________
00190   on fkey 5 goto L1990
00200   fnopenprn
00210   if file$(255)(1:3)<>"PRN" then jbskip=1
00280 ! ______________________________________________________________________
00290   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input,relative  !:
        read #1,using 'Form POS 618,10*N 1,POS 758,N 2',rec=1: mat dedcode,un !:
        close #1: 
00300 ! ______________________________________________________________________
00306 ! ______________________________________________________________________
00310   fnDedNames(mat fullname$,mat abrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
00320   for j=1 to 20
00330     if trim$(fullname$)="Union" then un=j ! determine union deduction
00340   next j
00350 ! ______________________________________________________________________
00360   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00370   open #2: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00380   open #3: "Name="&env$('temp')&"\Work."&session$,internal,input,relative 
00390   open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input ioerr XIT
00400   open #8: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno'),internal,outin,keyed 
00410   open #7: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00420   read #3,using L430,rec=1: df,dt,mat dr
00430 L430: form pos 1,2*n 6,7*pd 3
00440 L440: read #4,using L450: r4 eof L1950
00450 L450: form pos 1,pd 3
00460   if r4=1 then goto L440
00470   read #3,using L480,rec=r4: en1$,jn1$,mat tr1
00480 L480: form pos 5,c 8,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2
00490   if jn1$><jn$ then goto L660
00500   if en1$><en$ then goto L780
00510   if tr1(3)><tr(3) then goto L830
00520 L520: for j=1 to 7
00530     if int(tr1(4)*.01)=dr(j) then goto L570
00540   next j
00550   goto L440
00560 ! ______________________________________________________________________
00570 L570: hr1(j)=hr1(j)+tr1(5)
00580   hr1(8)=hr1(8)+tr1(5)
00590   hr2(j)=hr2(j)+tr1(6)
00600   hr2(8)=hr2(8)+tr1(6)
00610   gp1=gp1+tr1(5)*tdet2
00620   gp2=gp2+tr1(6)*tdet3
00630   tr(3)=tr1(3)
00640   goto L440
00650 ! ______________________________________________________________________
00660 L660: if rtrm$(jn$)="" then goto L700
00670   gosub L1610
00680   gosub TOTALS
00690   pr #255: newpage
00700 L700: en$=en1$
00710   jn$=jn1$
00720   n$=""
00730   read #2,using L740,key=jn$: n$ nokey L750
00740 L740: form pos 7,c 40
00750 L750: if end4=0 then gosub L1300
00760   goto L860
00770 ! ______________________________________________________________________
00780 L780: if val(en$)=0 then goto MOVEINFO
00790   gosub L1610
00800   en$=en1$
00810   goto L860
00820 ! ______________________________________________________________________
00830 L830: gosub L1450
00840   goto L880
00850 ! ______________________________________________________________________
00860 L860: read #1,using L870,key=en$: mat em$,ss$,em2,lpd,tgp nokey L1120
00870 L870: form pos 9,3*c 30,c 11,x 4,n 2,pos 162,n 6,pd 5.2,pd 3
00880 L880: mat ded=(0)
00890   tdet2=0
00900   tgp=tdet3=0
00930   gosub DETERMINE_EARNINGS
00940   tdet2=ttdc(1): tdet3=ttdc(2) ! regular and ot hours
00970   tgp=tgp+ttc(21)
00980   for j=1 to 5
00990     tcd1=tcd1+ttdc(j)
01000   next j
01010   ded(1)=ded(1)+ttc(2)+ttc(15)
01020   ded(2)=ded(2)+ttc(1)
01030   ded(3)=ded(3)+ttc(3)
01040   if un>0 and un<21 then ded(4)=ded(4)+ttc(un+4)
01050   for j=1 to 20
01060     if j=un then goto L1080
01070     if dedcode(j)=2 then ded(5)=ded(5)-ttc(j+3) else ded(5)=ded(5)+ttc(j+3)
01080 L1080: next j
01090   tcp22=tcp22+ttc(22)
01100   goto MOVEINFO
01110 ! ______________________________________________________________________
01120 L1120: mat em$=("")
01130   ss$=""
01140   em2=0
01150   ta1=0
01160 MOVEINFO: ! 
01170   pl1$(1)=em$(1)
01180   pl1$(2)=em$(2)
01190   pl1$(3)=em$(3)
01200   pl1$(4)=ss$
01210 ! pL1$(5)=LPAD$(STR$(TDN),6)
01220   pl1$(5)=lpad$(str$(em2),6)
01230   goto L520
01240 ! ______________________________________________________________________
01250 PGOF: ! 
01260   pr #255: newpage
01270   gosub L1300
01280   continue 
01290 ! ______________________________________________________________________
01300 L1300: p1=59-int(len(rtrm$(cnam$)))/2
01310   a2$="Job # "&ltrm$(jn$)&"  Job Name "&rtrm$(n$)
01320   p2=59-int(len(rtrm$(a2$)))/2
01330   pr #255,using L1340: cnam$,a2$
01340 L1340: form skip 2,pos p1,c 70,skip 1,pos p2,c 70,skip 1
01350   pr #255: tab(40);"****  Certified Payroll Register  ****"
01360   pr #255,using L1370: "Period Ending",dt
01370 L1370: form pos 48,c 14,pic(zz/zz/zz),skip 1
01380   pr #255: "Name  &  Address"
01390   pr #255: "City, State Zip                <-------- Hours Worked this Job --------> Total  Pay  Pay  <--------------- Summary ---------------->"
01400   pr #255,using L1410: "    Fed-Exempt",mat dr,"Hours  Rate Typ      Gross    FICA Fed W/H   Other      Net"
01410 L1410: form pos 1,c 30,pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),pic(zzz/zz),x 1,c 59,skip jbskip
01420   pr #255: "______________________________ _____ _____ _____ _____ _____ _____ _____ _____  ____ ___    _______  ______  ______   _____  _______"
01430   return 
01440 ! ______________________________________________________________________
01450 L1450: if tgp=0 then x3=0 else x3=(gp1+gp2)/tgp
01460   if hr1(8)=0 then goto L1510
01470   lnp=lnp+1
01480   if lnp>5 then lnp=6
01490   pr #255,using L1500: pl1$(lnp),mat hr1,tdet2," REG",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF
01500 L1500: form pos 1,c 30,9*n 6.2,c 6,n 9.2,3*n 8.2,n 9.2
01510 L1510: if hr2(8)=0 then goto L1560
01520   lnp=lnp+1
01530   if lnp>5 then lnp=6
01540   if hr1(8)=0 then pr #255,using L1500: pl1$(lnp),mat hr2,tdet3," OVT",gp1+gp2,x3*ded(1),x3*ded(2),x3*(ded(3)+ded(4)+ded(5)),gp1+gp2-(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5))) pageoflow PGOF else pr #255,using L1550: pl1$(lnp),mat hr2,tdet3," OVT" pageoflow PGOF
01550 L1550: form pos 1,c 30,9*n 6.2,c 6
01560 L1560: hr8=hr8+hr1(8)+hr2(8)
01570   mat hr1=(0)
01580   mat hr2=(0)
01590   return 
01600 ! ______________________________________________________________________
01610 L1610: gosub L1450
01620   lnp=lnp+1
01630   if lnp>5 then goto L1680
01640   for j=lnp to 5
01650     pr #255,using L1660: pl1$(j) pageoflow PGOF
01660 L1660: form pos 1,c 30,skip 1
01670   next j
01680 L1680: gded= gded+(x3*ded(1)+x3*ded(2)+x3*(ded(3)+ded(4)+ded(5)))
01690   jgp=jgp+gp1+gp2 ! TOTAL GROSS FOR JOB
01700   mat tded=tded+ded ! TOTAL DEDUCTION
01710   tcdt=tcdt+tcd1 ! TOTAL HOURS FOR ALL JOBS
01720   thr=thr+hr8 ! TOTAL HOURS FOR JOB
01730   tgp=0
01740   gp1=0
01750   gp2=0
01760   mat ded=(0)
01770   tcd1=0
01780   hr8=0
01790   tcp22=0
01800   lnp=0
01810   return 
01820 ! ______________________________________________________________________
01830 TOTALS: ! 
01840   pr #255,using L1850: "* * * *  Totals for Job # ",jn$,"* * * *" pageoflow PGOF
01850 L1850: form pos 10,c 26,c 7,c 8,skip 1
01860   pr #255: tab(6);"Total Hours   Gross Pay     Total        Total" pageoflow PGOF
01870   pr #255: tab(31);"Deductions     Net-Pay" pageoflow PGOF
01880   pr #255,using L1890: thr,jgp,gded,jgp-gded pageoflow PGOF
01890 L1890: form pos 5,4*n 12.2,skip 2
01900   thr=0
01910   jgp=0
01920   gded=0
01930   return 
01940 ! ______________________________________________________________________
01950 L1950: gosub L1610
01960   gosub TOTALS
01990 L1990: fncloseprn
02000   goto XIT
02010 ! ______________________________________________________________________
02020 ! <Updateable Region: ERTN>
02030 ERTN: fnerror(program$,err,line,act$,"xit")
02040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02050   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02060   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02070 ERTN_EXEC_ACT: execute act$ : goto ERTN
02080 ! /region
02090 ! ______________________________________________________________________
02100 XIT: fnxit
02110 ! ______________________________________________________________________
02120 DETERMINE_EARNINGS: ! 
02130   tfd=tmd=td14=tdw=0: mat caf=(0): mat ttc=(0): mat ttdc=(0)
02140   mat tcp=(0): mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0) !:
        mat qtr4tcp=(0): mat ytdtotal=(0): mat tdc=(0): mat tty=(0)
02150   fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0 !:
        medqtr=eicyr=eicqtr=wagesqtr=0
02160   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
02170   restore #7,key>=checkkey$: nokey L2500
02180 L2180: read #7,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,oldckno,mat tdc,mat tcp eof STORE_VARIABLES : lastrec=rec(3)
02190   if heno<>eno then goto STORE_VARIABLES
02200   if prd<beg_date or prd>end_date then goto L2180 ! not this year
02210   if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
02220   if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
02230   if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
02240   if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
02250   mat ytdtotal=ytdtotal+tcp
02260   mat tty=tty+tcp
02270   if prd=>df and prd<=dt then mat ttc=ttc+tcp: mat ttdc=ttdc+tdc ! total for this date range
02280   if prd=d1 then gosub ACCUMULATE_DEPT_TOTALS
02290   goto L2180
02300 STORE_VARIABLES: ! 
02310   wagesyr=ytdtotal(31) ! total wages
02320   fedyr=ytdtotal(1) ! ytdl fed
02330   ficayr=ytdtotal(2) ! fica year to date
02340   medyr=ytdtotal(3) ! medicare year to date
02350   stateyr=ytdtotal(4) ! total state  quarter
02360   eicyr=ytdtotal(25) ! eic
02370   if prd>=qtr1 and prd<qtr2 then mat quartertotals=qtr1tcp
02380   if prd>=qtr2 and prd<qtr3 then mat quartertotals=qtr2tcp
02390   if prd>=qtr3 and prd<qtr4 then mat quartertotals=qtr3tcp
02400   if prd>=qtr4 and prd<end_date then mat quartertotals=qtr4tcp
02410   wagesqtr=quartertotals(31) ! total wages quarter
02420   fedqtr=quartertotals(1) ! total fed  quarter
02430   ficaqtr=quartertotals(2) ! total fica quarter
02440   medqtr=quartertotals(3) ! total medicare quarter
02450   stateqtr=quartertotals(4) ! total state  quarter
02460   eicqtr=quartertotals(25) ! eic qtr
02470   for j=1 to 20
02480     if dedfed(j)=1 then dedfedyr+=ytdtotal(j+4) ! deduct for federal wh
02490   next j
02500 L2500: return 
02510 ACCUMULATE_DEPT_TOTALS: ! 
02520   ! ACCUMULATE CURRENT INFO FROM EACH DEPARTMENT
02530   if tdep=0 then goto L2570
02540   for j2=1 to tdep
02550     if tdep(j2,5)=tdn then goto L2590
02560   next j2
02570 L2570: tdep=tdep+1
02580   j2=tdep
02590 L2590: tdep(j2,1)=tdep(j2,1)+tcp(31)-tcp(30) ! total wage less tips
02600   deptgl$="" !:
        read #8,using "Form pos 12,c 12,pos 62,2*pd 4.2",key=cnvrt$("pic(ZZZZZZZ#)",eno)&cnvrt$("pic(ZZ#)",tdn): deptgl$,tdet(2),tdet(3) ! Nokey 1660
02610   tdep(j2,2)=val(deptgl$(1:3)) ! salary for this department
02620   tdep(j2,3)=val(deptgl$(4:9))
02630   tdep(j2,4)=val(deptgl$(10:12))
02640   tdep(j2,5)=tdn
02650   tdep(j2,6)=tdep(j2,6)+tcp(2)+tcp(3)
02660   for j3=1 to 20
02670     tdep(j2,j3+6)=tdep(j2,j3+6)+tcp(j3+4)
02680   next j3
02690   if s1><1 then goto L2720
02700   if rate=0 then rate=tdet(2)
02710   if rate>0 then rt$="PAY RATE"&cnvrt$("N 10.2",rate) else rt$=""
02720 L2720: tpd3=tpd3+round(tdc(3)*tdet(2),2) ! sick pay
02730   tpd4=tpd4+round(tdc(4)*tdet(2),2) ! vacation pay
02740   tpd5=tpd5+round(tdc(5)*tdet(2),2)
02750   tdc1=ttdc(1) ! Regular Hours
02760   tdc2=ttdc(2) ! OverTime Hours
02770   tdc3=ttdc(3)
02780   tdc4=ttdc(4)
02790   tdc5=ttdc(5)
02800   ttdct=ttdc(1)+ttdc(2)+ttdc(3)+ttdc(4)+ttdc(5) ! Total Hours
02810   return  ! If TA>0 Then Goto 1460
