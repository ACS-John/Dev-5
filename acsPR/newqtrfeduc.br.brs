00010 ! Replace S:\acsPR\newQTRFedUC
00020 ! Quarterly Federal U/C Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fntos,fnlbl,fntxt,fncomboa,fncmdset,fnacs,fnmsgbox,fncreg_read,fnDedNames,fnGetPayrollDates
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dedcode(20),calcode(20),dedfed(20),option1$(4)*20
00080   dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
00090   dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10)
00100   dim e$(10)*12,tpt(32),cap$*128,resp$(15)*30
00110   dim tcp(32),tdc(10)
00120   dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),qtr(32)
00130   dim ytdtotal(32),ss$*11,em$(3)*30,m$*20
00140 ! ______________________________________________________________________
00150   fntop(program$,cap$="Quarterly Federal Unemployment Worksheet")
00170   fnopenprn
00180 ! ______________________________________________________________________
00190   fncreg_read('calculation date text',m$)
00200   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
00210   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4)
00220   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00230   read #20,using L250: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
00240   let ficamaxw=ficamaxw*10
00250 L250: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
00260   close #20: 
00270 ! ______________________________________________________________________
00280 ! If FNPROCESS=1 Then Goto 230
00290 MENU1: ! 
00300   fntos(sn$="prqtrfeduc") !:
        let respc=0
00310   if val(date$(4:5))=1 then let taxyear=val(date$(1:2))+2000-1 else let taxyear =val(date$(1:2))+2000 ! current tax year (if processing in jan, assume last year)
00320   fnlbl(1,1,"Tax Year:",26,1)
00330   fntxt(1,30,4,0,0,"30",0,"") !:
        let resp$(respc+=1)=str$(taxyear)
00340   let option1$(1)="March 31"
00350   let option1$(2)="June 30"
00360   let option1$(3)="September 30"
00370   let option1$(4)="December 31"
00380   fnlbl(2,1,"Quarter Ending Date:",26,1)
00390   fncomboa("pr941-yr",2,30,mat option1$,"Enter the quarter ending date")
00400   if val(date$(4:5))=3 or val(date$(4:5))=4 or val(date$(4:5))=5 then let resp$(respc+=1)=option1$(1) ! march filing
00410   if val(date$(4:5))=6 or val(date$(4:5))=7 or val(date$(4:5))=8 then let resp$(respc+=1)=option1$(2) ! June  filing
00420   if val(date$(4:5))=9 or val(date$(4:5))=10 or val(date$(4:5))=11 then let resp$(respc+=1)=option1$(3) ! September filing
00430   if val(date$(4:5))=12 or val(date$(4:5))=1 or val(date$(4:5))=2 then let resp$(respc+=1)=option1$(4) ! December
00440   fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
00450   if ck=5 then goto XIT
00460   let taxyear=val(resp$(1)) ! tax year
00470   if taxyear<2000 then goto L510
00480   let ending_date=taxyear*10000+1231 conv L510
00490   goto L520
00500 ! 
00510 L510: mat ml$(2) !:
        let ml$(1)="You must enter a valid tax year such as 2007." !:
        let ml$(2)="Take OK to enter the year." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto MENU1
00520 L520: for j=1 to 4
00530     if resp$(2)=option1$(j) then let qtr=j: let m$=option1$(j): goto L550 ! quarter ending date
00540   next j
00550 L550: if qtr=1 then begdate=taxyear*10000+0312: let enddate=val(taxyear$)*10000+0318
00560   if qtr=2 then begdate=taxyear*10000+0612: let enddate=val(taxyear$)*10000+0618
00570   if qtr=3 then begdate=taxyear*10000+0912: let enddate=val(taxyear$)*10000+0918
00580   if qtr=4 then begdate=taxyear*10000+1212: let enddate=val(taxyear$)*10000+1218
00590 ! ______________________________________________________________________
00600   on pageoflow goto PGOF
00610   open #2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00620   gosub HDR
00630   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00640   open #3: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",Shr, KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
00650 L650: read #2,using L660: eno,mat em$,ss$,em5,em6 eof DONE
00660 L660: form pos 1,n 8,3*c 30,c 11,pos 120,2*n 2
00670   let m1=m2=h2=h3=dedytdfeduc=dedqtrfeduc=0
00680   mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
00690   mat ytdtotal=(0)
00700 ! 
00710   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
00720   restore #4,key>=checkkey$: nokey ANALYZE_WAGES
00730 L730: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
00740   if heno<>eno then goto ANALYZE_WAGES
00750   if prd<beg_date or prd>end_date then goto L730 ! not this year
00760   if em5=1 then let pedate=begdate+19: box1+=1 ! monthly pay period
00770   if em5=2 then let pedate=begdate+15 : box1+=1 ! semi-monthly
00780   if em5=3 then let pedate=begdate+14 : box1+=1 ! bi-weekly
00790   if em5=4 then let pedate=begdate+7: box1+=1 ! weekly
00800 ! let deptkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)
00810   if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp: mat tpt=tpt+tcp ! 1st qtr earnings
00820   if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp : mat tpt=tpt+tcp
00830   if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp : mat tpt=tpt+tcp
00840   if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp : mat tpt=tpt+tcp
00850   if prd>=qtr1 and prd<ending_date then mat ytdtotal=ytdtotal+tcp ! only total year to date wages to end of current quarter
00860   goto L730
00870 ANALYZE_WAGES: ! analyze wages on each person
00880   if qtr=1 then mat qtr=qtr1tcp
00890   if qtr=2 then mat qtr=qtr2tcp
00900   if qtr=3 then mat qtr=qtr3tcp
00910   if qtr=4 then mat qtr=qtr4tcp
00940   for j=1 to 20
00950     if deduc(j)=1 and dedcode(j)=1 then let dedytdfeduc+=ytdtotal(j+4) : let dedqtrfeduc+=qtr(j+4) ! TOTAL DEDUCTIONS FOR federal u/c FOR QUARTER
00960   next j
00970   let m2=m2+ytdtotal(31)-dedytdfeduc ! TOTAL WAGES less deductions FOR THIS EMPLOYEE FOR YEAR
00980   let m1=m1+qtr(31)-dedqtrfeduc ! TOTAL WAGES less deductions FOR QURATER
00990   if m2=0 then goto L650
01000   gosub L1360
01010   goto L650
01020 ! ______________________________________________________________________
01030 HDR: ! 
01040   let p2=p2+1
01050   pr #255,using L1060: "Page ",p2
01060 L1060: form pos 70,c 5,pic(zzz),skip 1
01070   pr #255: 
01080   pr #255,using L1090: cap$
01090 L1090: form pos 20,c 40,skip 1
01100   pr #255,using L1110: "For quarter ended "&m$
01110 L1110: form pos 20,cc 40,skip 1
01120   pr #255: 
01130   pr #255,using L1140: "     Rate",a$(1),"Fed ID",b$(1)
01140 L1140: form pos 1,c 9,pos 17,c 40,pos 59,c 6,pos 69,c 40,skip 1
01150   pr #255,using L1160: feducrat,a$(2)
01160 L1160: form pos 3,pic(zzzz.##),pos 17,c 40,skip 1
01170   pr #255,using L1180: a$(3)
01180 L1180: form pos 17,c 40,skip 1
01190   pr #255: 
01200   pr #255: tab(44);"Total Wages   Excess Wages    Taxable"
01210   pr #255: " SS Number             Name";
01220   pr #255,using L1230: "For Quarter   Over $",feducmax,"Wages"
01230 L1230: form pos 44,c 20,pic(zzzzz.##),pos 75,c 5,skip 1
01240   pr #255: "___________  __________________________";
01250   pr #255: tab(44);"___________   ____________    _______"
01260   return 
01270 ! ______________________________________________________________________
01280 DONE: ! 
01290   let eofcode=1
01300   gosub PAGETOTALS
01310   close #2: ioerr L1320
01320 L1320: close #3: ioerr L1330
01330 L1330: let fncloseprn
01340 XIT: let fnxit
01350 ! ______________________________________________________________________
01360 L1360: if m1=0 then goto L1510 ! skip if quarterly wage=0
01370   let p3=p3+1
01380   if m2<feducmax then goto L1440
01390   if m2-m1>feducmax then goto L1420
01400   let h2=feducmax-(m2-m1)
01410   goto L1450
01420 L1420: let h2=0
01430   goto L1450
01440 L1440: let h2=m1
01450 L1450: let h3=m1-h2
01460   pr #255,using L1470: ss$,em$(1)(1:28),m1,h3,h2
01470 L1470: form pos 1,c 11,pos 14,c 28,pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##),skip 1
01480   let t1+=m1 : let t2+=h3 : let t3+=h2
01490   pr #255: pageoflow PGOF
01500   let p1=p1+2
01510 L1510: return 
01520 ! ______________________________________________________________________
01530 PAGETOTALS: ! 
01540   pr #255,using L1550: "___________    ___________  _________" pageoflow L1640
01550 L1550: form pos 44,c 37,skip 1
01560   pr #255: "Employees on this page:";p3;"    Page Totals";
01570   pr #255,using L1580: t1,t2,t3
01580 L1580: form pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##),skip 1
01590   if nw=1 and eofcode=1 then goto L1610
01600   pr #255: newpage
01610 L1610: let p3=t1=t2=t3=0
01620   return 
01630 ! ______________________________________________________________________
01640 L1640: pr #255: newpage
01650 PGOF: ! 
01660   gosub PAGETOTALS
01670   pr #255: newpage
01680   gosub HDR
01690   continue 
01700 ! ______________________________________________________________________
01710 ! <Updateable Region: ERTN>
01720 ERTN: pause : let fnerror(program$,err,line,act$,"xit")
01730   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01740   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01750   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01760 ERTN_EXEC_ACT: execute act$ : goto ERTN
01770 ! /region
01780 ! ______________________________________________________________________
