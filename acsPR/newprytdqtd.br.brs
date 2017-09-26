00010 ! Replace S:\acsPR\newprYTDQTD
00020 ! Year-To-Date Quarter-To-Date Register
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn, fncloseprn,fnerror,fntos,fnlbl,fntxt,fnacs,fncmdkey,fnfra,fnprocess,fnGetPayrollDates,fnDedNames
00050   on error goto ERTN
00060 ! gosub CHECK_PASSWORD
00070 ! ______________________________________________________________________
00080   dim tdc(10),fullname$(20)*20,abbrevname$(20)*20
00090   dim dat$*20,rptemp(20),rptot(21,2)
00100   dim em$*30,em(6),tot(20),cap$*128,message$*40
00110   dim newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20)
00120   dim ytdtotal(32),qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
00130   dim quartertotals(32),tcp(32),tdc(10),resp$(10)*40
00140 ! ______________________________________________________________________
00150   let fntop(program$,cap$="YTD and Qtd Register")
00170   open #1: "Name="&env$('Q')&"\PRmstr\prCode.h"&env$('cno')&",Shr",internal,input ioerr L180 
00172   read #1,using 'Form POS 5,N 5': ckno 
00174   close #1: 
00180 L180: open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #1,using 'Form POS 133,PD 6.3,PD 6.2,POS 239,PD 4.2,POS 618,30*N 1,10*C 6': mcr,mcm,ficamaxw !:
        close #1: 
00190   let ficamaxw=ficamaxw*10
00200   fnDedNames(mat fullname$,mat abbrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
00210   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,input,keyed 
00220   if fnprocess=1 then goto L240
00230   gosub ASK_DATES
00240 L240: let fnopenprn
00250 ! ______________________________________________________________________
00260   gosub HDR
00270   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00280 L280: read #1,using L290: eno,em$,mat em eof L930
00290 L290: form pos 1,n 8,c 30,pos 112,6*n 2
00300   let a=pos (rtrm$(em$)," ",1)
00310   let b=pos (rtrm$(em$)," ",a+1)
00320   ! if env$('client')="West Rest Haven" then goto L370 ! don't turn name around
00330   let em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
00340   mat qtr1tcp=(0) : mat qt2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
00350   let fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=medqtr=0
00360   mat quartertotals=(0): mat ytdtotal=(0): let dedfedyr=dedficayr=0 !:
        let dedstateyr=deducyr=wagesyr=0
00370   mat rptemp=(0) ! L370:
00380   let twy=tty=twq=ttq=tficawy=tficawq=0
00390   gosub DETERMINE_EARNINGS
00400   let rptemp(1)=rptemp(1)+wagesyr ! total wages ytd
00410   let tx1=tx1+wagesyr-dedfedyr ! taxable ytd
00420   let rptemp(2)=rptemp(2)+fedyr ! federal wh ytd
00430   let rptemp(3)=rptemp(3)+ficayr ! fica wh ytd
00440   let rptemp(4)=rptemp(4)+stateyr ! state wh ytd
00450   let rptemp(5)=rptemp(5)+wagesqtr ! total wages quarter
00460   let rptemp(6)=rptemp(6)+fedqtr ! federal wh quarter
00470   let rptemp(7)=rptemp(7)+ficaqtr ! fica quarter
00480   let rptemp(8)=rptemp(8)+stateqtr ! state wh quarter
00490   let rptemp(9)=rptemp(9)+medyr ! medicare  wh year
00500   let rptemp(10)=rptemp(10)+medqtr ! medicare qtr
00510   for k2=1 to 20
00520     let rptot(k2,1)=rptot(k2,1)+ytdtotal(k2+4) ! deductions
00530     let rptot(k2,2)=rptot(k2,2)+quartertotals(k2+4) ! deductions qtr
00540   next k2
00550   let rptot(21,1)=rptot(21,1)+eicytd ! eic  ytd
00560   let rptot(21,2)=rptot(21,2)+eicqtr ! eic qtr
00570   let twy=twy+wagesyr-dedfedyr: let twq=twq+wagesqtr-dedfedqtr ! taxable wages year and quarter
00580 ! If EM(6)=9 Then Let TFICAWY=TFICAWY+wagesyr: Let TFICAWQ=TFICAWQ+WAGESQTR : Goto 610
00590   let ttips=ttips+ytdtotal(30) ! tips
00600 ! Goto 280
00610 ! Let MCW1=TFW=0 ! mcw1=total medicare wages for this employee  tfw = totoal ss wage for this employee
00620   let ficawagesyr=wagesyr- dedficayr !:
        let ficawagesqtr=quartertotals(31)- dedficaqtr
00640   on em(6)+1 goto L650,L690,L650 none L770
00645 ! determine medicare maximum wages
00650 L650: let mcw1+=ficawagesqtr ! mo maximum mc wage
00655   let mcw2+=ficawagesyr ! total year to date medicare wages
00680   if em(6)=2 then goto L770
00685 ! determine fica taxable wages
00690 L690: if ficawagesyr<ficamaxw then let tfwy+=ficawagesyr : let tfq+=ficawagesqtr: goto L770
00710   let tfwy+=ficawagesyr-(ficawagesyr-ficamaxw)
00715   let tfq+=ficawagesqtr-(min(ficawagesyr-ficamaxw,ficawagesqtr))
00770 L770: gosub L790
00780   goto L280
00790 L790: mat tot=tot+rptemp
00800   let tx2=tx2+tx1
00810 L810: print #255,using L890: eno,em$(1:19),rptemp(1),tx1,rptemp(3),rptemp(2),rptemp(4),rptemp(5),rptemp(7),rptemp(6),rptemp(8) pageoflow PGOF
00820   print #255,using L900: rptemp(9),rptemp(10) pageoflow PGOF
00830   let tx1=0
00840   return 
00850 ! ______________________________________________________________________
00860 PGOF: ! 
00870   print #255: newpage
00880   gosub HDR
00890 L890: form pos 1,pic(zzzzzzzz),pos 10,c 19,pos 29,n 13.2,n 12.2,n 11.2,n 11.2,n 11.2,n 13.2,n 11.2,n 11.2,n 11.2,skip 1
00900 L900: form pos 54,n 11.2,x 35,n 11.2,skip 1
00910   continue 
00920 ! ______________________________________________________________________
00930 L930: print #255: 
00940   let eno=0
00950   let em$="Final Totals"
00960   mat rptemp=tot
00970   let tx1=tx2
00980   gosub L810
00990   print #255,using L1000: "YTD","QTD"
01000 L1000: form skip 2,pos 29,c 3,pos 39,c 3,skip 2
01010   for j=1 to 20
01020     if trim$(abbrevname$)="" then goto L1050
01030     print #255,using L1040: abbrevname$(j),rptot(j,1),rptot(j,2)
01040 L1040: form pos 1,c 20,pos 22,2*n 12.2,skip 1
01050 L1050: next j
01060   print #255,using L1040: "EIC",rptot(j,1),rptot(j,2)
01070   print #255,using L1040: "Total Soc-Sec. Wages",tfwy,tfq
01080   print #255,using L1040: "Total Medicare Wages",mcw2,mcw1
01090   print #255,using L1040: "Total Tips-YTD",ttips
01100   form pos 1,c 33,2* n 10.2,skip 1
01110   goto DONE
01120 ! ______________________________________________________________________
01130 DONE: ! 
01140   let fncloseprn
01150   close #1: ioerr L1160
01160 L1160: close #2: ioerr L1170
01170 L1170: let fnxit
01180 ! ______________________________________________________________________
01190 HDR: ! 
01200   print #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
01210   print #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
01220   print #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
01230 ! Print #255: "\qc  {\f181 \fs16 \b Payroll Date: "&CNVRT$("pic(zz/zz/zz)",PPD)&"}"
01240   print #255: "\qc  {\f181 \fs16 \b "&trim$(dat$)&"}"
01250   print #255: "\ql   "
01260   print #255: tab(32); "<--------------------Year To Date--------------------->  <-------------Quarter To Date-------------->"
01270   print #255: "  Number  Name                   Total      Taxable       SS/Med   Fed W/H    St W/H       Total        SS/Med   Fed W/H    St W/H"
01280   print #255: "________  ___________________  __________ ___________ __________ __________ __________  ___________ __________ __________ __________"
01290   return 
01300 ! ______________________________________________________________________
01310 ! <Updateable Region: ERTN>
01320 ERTN: let fnerror(program$,err,line,act$,"xit")
01330   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01340   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01350   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
01360 ERTN_EXEC_ACT: execute act$ : goto ERTN
01370 ! /region
01380 ! ______________________________________________________________________
01390 XIT: let fnxit
01400 ! ______________________________________________________________________
01410 ! CHECK_PASSWORD: ! 
01420 !   return ! if env$('client')="Washington Parrish" then goto L1430 else return 
01430 ! L1430: if wsid$="09" or wsid$="99" then return 
01440 !   let fntos(sn$="WpTrap") !:
      !   let respc=0 : let mylen=25 : let mypos=mylen+2
01450 !   let fnlbl(1,1,"         Quit!      ",mylen,2)
01460 !   let fnlbl(2,1,"Stay out of Payroll!",mylen,2)
01470 !   let fnlbl(3,1,"Call Brenda for Password",mylen,2)
01480 !   let fntxt(3,mylen+3,8,8,1,"",0,"You must have a password to get out.") !:
      !   let resp$(respc+=1)=""
01490 !   let fncmdkey("E&xit",5,1,1,"Returns to menu")
01500 !   let fnacs(sn$,0,mat resp$,ckey)
01510 !   if trim$(uprc$(resp$(1)))="GETMEOUT" then goto XIT else goto L1430
01520 ASK_DATES: ! 
01530   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,dat$)
01580   let fntos(sn$="YtdQtdReg-1") !:
        let rc=cf=0: let mylen=42: let mypos=45: let frameno=1
01590   let fnfra(1,1,4,66,"Payroll Date","Enter the payroll date.")
01600   let fnlbl(1,1,"Payroll Period Ending Date:",mylen,1,0,frameno)
01610   let fntxt(1,mypos,10,0,1,"3",0,"Normally the last payroll date, but can beny point in time. ",frameno) !:
        let resp$(rc+=1)=str$(d1)
01620   let fnlbl(2,1,"Report Heading Date:",mylen,1,0,frameno)
01630   let fntxt(2,mypos,20,0,0," ",0,"Enter the date in alpha format for use in report heading." ,frameno) !:
        let resp$(rc+=1)= dat$
01640   let fnfra(7,25,6,42,"Date Range","In order to Identify earnings and deductions, these answers must be correct.") !:
        let frameno=2 : let mylen=26 : let mypos=mylen+2
01650   let fnlbl(1,1,"Starting Date:",mylen,1,0,frameno)
01660   let fntxt(1,mypos,10,0,1,"3",0,"Enter the beginning date of your payrll year.",frameno) !:
        let resp$(rc+=1)=str$(beg_date)
01670   let fnlbl(2,1,"Ending Date:",mylen,1,0,frameno)
01680   let fntxt(2,mypos,10,0,1,"3",0,"Enter the last payroll date of the year",frameno) !:
        let resp$(rc+=1)=str$(end_date)
01690   let fnlbl(3,1,"1st Day of 1st quarter:",mylen,1,0,frameno)
01700   let fntxt(3,mypos,10,0,1,"3",0,"Enter the first day of the first quarter. Could be something other than January 1st if your last payroll of the previous year should be included in this year",frameno) !:
        let resp$(rc+=1)=str$(qtr1)
01710   let fnlbl(4,1,"1st Day of 2nd quarter:",mylen,1,0,frameno)
01720   let fntxt(4,mypos,10,0,1,"3",0,"Normally would be April 1st, but could be different if your payroll dates and check dates are not the same.",frameno) !:
        let resp$(rc+=1)=str$(qtr2)
01730   let fnlbl(5,1,"1st Day of 3rd quarter:",mylen,1,0,frameno)
01740   let fntxt(5,mypos,10,0,1,"3",0,"Normally would be July 1st",frameno) !:
        let resp$(rc+=1)=str$(qtr3)
01750   let fnlbl(6,1,"1st Day of 4th quarter:",mylen,1,0,frameno)
01760   let fntxt(6,mypos,10,0,1,"3",0,"Normally would be October 1st.",frameno) !:
        let resp$(rc+=1)=str$(qtr4)
01770   let fncmdkey("Next",1,1,0,"Proceed with calculations.")
01780   let fncmdkey("Cancel",5,0,1,"Returns to menu without calculating")
01790   let fnacs(sn$,0,mat resp$,ckey)
01800   if ckey=5 then goto XIT
01810   let dat=prdate=d1=val(resp$(1))
01820   let dat$=resp$(2)
01830   let beg_date=val(resp$(3)) !:
        let end_date=val(resp$(4)) !:
        let qtr1=val(resp$(5)) !:
        let qtr2=val(resp$(6)) !:
        let qtr3=val(resp$(7)) !:
        let qtr4=val(resp$(8))
01840   let qtr5=val(resp$(9)(1:4))*10000+1231
01850   let begin_year=val(resp$(9)(1:4))*10000+0101
01860   let end_year=val(resp$(9)(1:4))*10000+1231
01870   open #11: "Name="&env$('Q')&"\PRmstr\Dates.h"&env$('cno'),internal,outin,relative 
01880 ! Rewrite #11,Using "form pos 1,6*n 8,n 8,c 20",Rec=1: BEG_DATE,END_DATE,QTR1,QTR2,QTR3,QTR4,D1,DAT$
01890   close #11: 
01900   return 
01910 DETERMINE_EARNINGS: ! 
01920   let tfd=tmd=td14=tdw=0: mat caf=(0)
01930   mat tcp=(0): mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0) !:
        mat qtr4tcp=(0): mat ytdtotal=(0): mat tdc=(0)
01940   let fedyr=ficayr=stateyr=wagesqtr=fedqtr=ficaqtr=stateqtr=medyr=0 !:
        let medqtr=eicyr=eicqtr=wagesqtr=0
01950   let checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
01960   restore #4,key>=checkkey$: nokey L2290
01970 L1970: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof STORE_VARIABLES
01980   if heno<>eno then goto STORE_VARIABLES
01990   if prd<beg_date or prd>end_date then goto L1970 ! not this year
02000   if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
02010   if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
02020   if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
02030   if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
02040   mat ytdtotal=ytdtotal+tcp
02050   goto L1970
02060 STORE_VARIABLES: ! 
02070   let wagesyr=ytdtotal(31) ! total wages
02080   let fedyr=ytdtotal(1) ! ytdl fed
02090   let ficayr=ytdtotal(2) ! fica year to date
02100   let medyr=ytdtotal(3) ! medicare year to date
02110   let stateyr=ytdtotal(4) ! total state  quarter
02120   let eicyr=ytdtotal(25) ! eic
02130   if prdate>=qtr1 and prdate<qtr2 then mat quartertotals=qtr1tcp
02140   if prdate>=qtr2 and prdate<qtr3 then mat quartertotals=qtr2tcp
02150   if prdate>=qtr3 and prdate<qtr4 then mat quartertotals=qtr3tcp
02160   if prdate>=qtr4 and prdate=<end_date then mat quartertotals=qtr4tcp
02170   let wagesqtr=quartertotals(31) ! total wages quarter
02180   let fedqtr=quartertotals(1) ! total fed  quarter
02190   let ficaqtr=quartertotals(2) ! total fica quarter
02200   let medqtr=quartertotals(3) ! total medicare quarter
02210   let stateqtr=quartertotals(4) ! total state  quarter
02220   let eicqtr=quartertotals(25) ! eic qtr
02230   for j=1 to 20
02240     if newdedfed(j)=1 then let dedfedyr+=ytdtotal(j+4) ! deduct for federal wh
02250     if dedfica(j)=1 then let dedficayr+=ytdtotal(j+4) !:
            let dedficaqtr+=quartertotals(j+4) ! deduct for fica
02260     if dedst(j)=1 then let dedstateyr+=ytdtotal(j+4) ! deduct for state
02270     if deduc(j)=1 then let deducyr+=ytdtotal(j+4) ! deduct for unemployment
02280   next j
02290 L2290: return 
