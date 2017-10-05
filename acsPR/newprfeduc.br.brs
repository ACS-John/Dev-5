00010 ! Replace S:\acsPR\newprFedUC
00020 ! Annual Federal U/C Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnGetPayrollDates,fnerror,fntos,fnlbl,fntxt,fncmdset,fnacs,fnfra,fnopt,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_font,fnpa_fontsize,fnpa_pic,fnpa_txt,fnDedNames
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$(3)*40,b$(2)*12
00080   dim dedcode(20),calcode(20),dedfed(20)
00090   dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
00100   dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10)
00110   dim e$(10)*12,cap$*128,resp$(15)*30
00120   dim tcp(32),tdc(10)
00140   dim ytdtotal(32),ss$*11,em$(3)*30,csz$*40
00150 ! ______________________________________________________________________
00170   fntop(program$,cap$="Annual Federal Unemployment Worksheet")
00190   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
00200   fnGetPayrollDates(beg_date,end_date)
00210   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00220   read #20,using L230: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
00230 L230: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
00240   close #20: 
00250 ! ______________________________________________________________________
00260 ! If FNPROCESS=1 Then Goto 230
00270 ! MENU1: !
00280   fntos(sn$="prfeduc") !:
        respc=0
00290   fnlbl(1,43," ",1,1)
00300   fnlbl(1,1,"Beginning Date of Tax Year:",29,1)
00310   fntxt(1,30,12,0,0,"3",0,"If filing annually, this would be the first day of the year.") !:
        resp$(respc+=1)=str$(beg_date)
00320   fnlbl(2,1,"Ending Date of Tax Year:",29,1)
00330   fntxt(2,30,12,0,0,"3",0,"If filing annually, this would be the last day of the year.") !:
        resp$(respc+=1)=str$(end_date)
00340   fnlbl(3,1,"Deposits Made:",29,1)
00350   fntxt(3,30,12,0,0,"10",0,"Total deposits made for this time frame.") !:
        resp$(respc+=1)=str$(deposits)
00360   fnlbl(4,1,"Top Margin:",29,1)
00370   fntxt(4,30,3,0,0,"30",0,"Reduce the top margin to move the pr up. Increse to move down.") !:
        resp$(respc+=1)=str$(5)
00380   fnlbl(5,1,"Left Margin:",29,1)
00390   fntxt(5,30,3,0,0,"30",0,"Reduce the left margin to move the pr left. Increse to move right.") !:
        resp$(respc+=1)=str$(5)
00400   fnlbl(7,1,"FUTA Tax Liability 1st Qtr:",29,1)
00410   fntxt(7,30,12,0,0,"10",0,"Total FUTA Tax Libality for the first quarter.") !:
        resp$(respc+=1)=str$(futaqtr1)
00420   fnlbl(8,1,"FUTA Tax Liability 2nd Qtr:",29,1)
00430   fntxt(8,30,12,0,0,"10",0,"Total FUTA Tax Libality for the second quarter.") !:
        resp$(respc+=1)=str$(futaqtr2)
00440   fnlbl(9,1,"FUTA Tax Liability 3rd Qtr:",29,1)
00450   fntxt(9,30,12,0,0,"10",0,"Total FUTA Tax Libality for the third quarter.") !:
        resp$(respc+=1)=str$(futaqtr3)
00460   fnlbl(10,1,"FUTA Tax Liability 4th Qtr:",29,1)
00470   fntxt(10,30,12,0,0,"10",0,"Total FUTA Tax Libality for the fourth quarter.") !:
        resp$(respc+=1)=str$(futaqtr4)
00480   fnfra(12,1,2,45,"Option for printing","The system can pr the actual form or just fill in the blanks on a pre-printed form.",0)
00490   fnopt(1,2,"Print complete form",0,1) !:
        resp$(respc+=1)="True"
00500   fnopt(2,2,"Fill in the blanks",0,1) !:
        resp$(respc+=1)="False"
00510   fncmdset(2): fnacs(sn$,0,mat resp$,ck)
00520   if ck=5 then goto XIT
00530   beg_date=val(resp$(1)) ! beginning of year
00540   end_date=val(resp$(2)) ! ending day of year
00560   deposits=val(resp$(3))
00570   topmargin=val(resp$(4))
00580   leftmargin=val(resp$(5))
00590   futaqtr1=val(resp$(6))
00600   futaqtr2=val(resp$(7))
00610   futaqtr3=val(resp$(8))
00620   futaqtr4=val(resp$(9))
00630   if resp$(10)="True" then fullform=1 ! pr full form
00640   if resp$(11)="True" then fullform=2 ! fill in blanks
00650   fnopenprn
00660 ! ______________________________________________________________________
00670   on pageoflow goto PGOF
00680   open #2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00690   gosub HDR
00700   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00710   open #3: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",Shr, KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
00720 L720: read #2,using L730: eno,mat em$,ss$,em5,em6 eof EOF2
00730 L730: form pos 1,n 8,3*c 30,c 11,pos 120,2*n 2
00740   m2=dedytdfeduc=0
00750   mat ytdtotal=(0)
00760   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
00770   restore #4,key>=checkkey$: nokey ANALYZE_WAGES
00780 L780: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
00790   if heno<>eno then goto ANALYZE_WAGES
00800   if prd<beg_date or prd>end_date then goto L780 ! not this year
00810   mat ytdtotal=ytdtotal+tcp
00820   goto L780
00830 ANALYZE_WAGES: ! analyze wages on each person
00840   for j=1 to 20
00850     if deduc(j)=1 and dedcode(j)=1 then dedytdfeduc+=ytdtotal(j+4) ! TOTAL DEDUCTIONS FOR federal u/c FOR QUARTER
00860   next j
00870   m2=m2+ytdtotal(31)-dedytdfeduc ! TOTAL WAGES less deductions FOR THIS EMPLOYEE FOR YEAR
00880   if m2=0 then goto L720
00890   gosub PRINT_LINE
00900   goto L720
00910 HDR: ! 
00920   pr #255,using 'Form POS 20,Cc 40,Cr 20': cap$,"Page "&str$(p2+=1) !:
        pr #255,using 'Form POS 20,CC 40': "For year ending "&cnvrt$("pic(zzzz/zz/zz)",end_date) !:
        pr #255: ""
00930   pr #255,using L940: "     Rate",a$(1),"Fed ID",b$(1)
00940 L940: form pos 1,c 9,pos 17,c 40,pos 59,c 6,pos 69,c 40
00950   pr #255,using 'Form POS 3,PIC(ZZZZ.##),POS 17,C 40': feducrat,a$(2)
00960   pr #255,using 'Form POS 17,C 40': a$(3)
00970   pr #255: ""
00980   pr #255: tab(44);"Total Wages   Excess Wages    Taxable"
00990   pr #255: " SS Number             Name";
01000   pr #255,using L1010: "   For Year   Over $",feducmax,"Wages"
01010 L1010: form pos 44,c 20,pic(zzzzz.##),pos 75,c 5
01020   pr #255: "___________  __________________________";tab(44);"___________   ____________    _______"
01030   return 
01040 ! ______________________________________________________________________
01050 EOF2: ! 
01060   gosub TOTALS
01070 ! DONE: !
01080   fncloseprn
01090 ! CANCEL_OUT: !
01092 close #2: ioerr L1100
01100 L1100: close #3: ioerr L1110
01110 L1110: gosub PRINT_940
01140 XIT: fnxit
01150 ! ______________________________________________________________________
01160 PRINT_LINE: ! 
01170   tw=0
01180   if m2>=feducmax then tw=feducmax else tw=min(feducmax,m2)
01190   pr #255,using L1200: ss$,em$(1)(1:28),m2,max(m2-feducmax,0),tw
01200 L1200: form pos 1,c 11,pos 14,c 28,pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##)
01210   t1+=m2 : t2+=max(m2-feducmax,0) : t3+=tw
01220   pr #255: "" pageoflow PGOF
01230   return 
01240 ! ______________________________________________________________________
01250 TOTALS: ! 
01260   pr #255: tab(44);"___________    ___________  _________"
01270   pr #255,using L1280: "Grand Totals",t1,t2,t3
01280 L1280: form pos 28,c 12,pos 42,pic(--,---,---.##),pos 57,pic(--,---,---.##),pos 70,pic(----,---.##)
01290   pr #255: ""
01300   pr #255,using L1310: "Total Futa Tax",t3*feducrat/100
01310 L1310: form pos 26,c 14,pos 42,pic(--,---,---.##)
01320   fncloseprn
01330   return 
01340 ! ______________________________________________________________________
01350 PGOF: pr #255: newpage : gosub HDR : continue 
01360 ! ______________________________________________________________________
01370 ! <Updateable Region: ERTN>
01380 ERTN: fnerror(program$,err,line,act$,"xit")
01390   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01400   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01410   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01420 ERTN_EXEC_ACT: execute act$ : goto ERTN
01430 ! /region
01440 ! ______________________________________________________________________
01450 PRINT_940: !  only fills in the blanks at this time
01460   gosub VBOPENPRINT
01470   fnpa_font
01480   fnpa_fontsize(20)
01490   if fullform=2 then goto L1570
01500   fnpa_pic("S:\acsPR\Form 940 Front.bmp",1,1)
01510 ! fnpa_pic("S:\acsPR\2009.bmp",34+leftmargin,5)
01520 ! fnpa_pic("S:\acsPR\2010.bmp",34+leftmargin,5)
01530 ! fnpa_pic("S:\acsPR\2011.bmp",34+leftmargin,5)
01540 ! fnpa_pic("S:\acsPR\2012.bmp",34+leftmargin,5)
01550 ! fnpa_pic("S:\acsPR\2013.bmp",34+leftmargin,5)
01560 ! fnpa_pic("S:\acsPR\2014.bmp",34+leftmargin,5)
01570 L1570: fnpa_fontsize(12)
01580   for j=1 to 10
01590     x=val(b$(1)(j:j)) conv L1610 ! pull any spaces or non-numeric characters out of federal id#
01600     goto L1620
01610 L1610: b$(1)(j:j)=""
01620 L1620: if b$(1)(j:j)=" " then b$(1)(j:j)=""
01630     if b$(1)(j:j)="-" then b$(1)(j:j)=""
01640   next j
01650   fnpa_fontsize(16)
01660   lyne=13.5+topmargin ! starting line of fed id
01670   fnpa_txt(b$(1)(1:1),45+leftmargin,lyne)
01680   fnpa_txt(b$(1)(2:2),55+leftmargin,lyne)
01690   fnpa_txt(b$(1)(3:3),68+leftmargin,lyne)
01700   fnpa_txt(b$(1)(4:4),77+leftmargin,lyne)
01710   fnpa_txt(b$(1)(5:5),86+leftmargin,lyne)
01720   fnpa_txt(b$(1)(6:6),95+leftmargin,lyne)
01730   fnpa_txt(b$(1)(7:7),104+leftmargin,lyne)
01740   fnpa_txt(b$(1)(8:8),113+leftmargin,lyne)
01750   fnpa_txt(b$(1)(9:9),122+leftmargin,lyne)
01760   fnpa_fontsize(12)
01770   fnpa_txt(trim$(a$(1)),32+leftmargin,30+topmargin)
01780   fnpa_txt(trim$(a$(2)),32+leftmargin,39+topmargin)
01790   csz$=a$(3): gosub CSZ
01800   fnpa_txt(trim$(city$),32+leftmargin,47+topmargin)
01810   fnpa_txt(trim$(state$),88+leftmargin,47+topmargin)
01820   fnpa_txt(trim$(zip$),102+leftmargin,47+topmargin)
01830   fnpa_txt(trim$(state$(1:1)),100+leftmargin,72+topmargin)
01840   fnpa_txt(trim$(state$(2:2)),111+leftmargin,72+topmargin)
01850   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t1),column3+leftmargin,103+topmargin) ! total wages
01860   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",0),column2+leftmargin,111+topmargin) ! exempt payments
01870   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t2),column2+leftmargin,130+topmargin) ! excess of maximum
01880   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t2),column3+leftmargin,139+topmargin) ! subtotal
01890   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t3),column3+leftmargin,147+topmargin) ! taxable wages
01900   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t3*feducrat/100),column3+leftmargin,155+topmargin) ! tax
01910   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",t3*feducrat/100),column3+leftmargin,199+topmargin) ! tax
01920   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",deposits),column3+leftmargin,208+topmargin) ! deposits
01930   if (t3*feducrat/100)-deposits>0 then let fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",(t3*feducrat/100)-deposits),column3+leftmargin,222.5+topmargin) ! due
01940   if (t3*feducrat/100)-deposits<=0 then let fnpa_txt(cnvrt$("pic(-----------.##)",abs((t3*feducrat/100)-deposits)),column3+leftmargin,31+topmargin) ! overpaid
01950   fnpa_newpage
01955   fnpa_pic("S:\acsPR\Form 940 pg2.bmp",1,1)
01960   column4=119.5
01965   if fullform=1 then x=2.5: y=1 else x=0: y=0 ! adjust for bad alignment
01970   lyne=26.4 +topmargin-x : leftmargin=leftmargin-y
01980   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr1),column4+leftmargin,lyne+=8.2) ! tax liability for 1st qtr
01990   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr2),column4+leftmargin,lyne+=8.2) ! tax liability for 2nd qtr
02000   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr3),column4+leftmargin,lyne+=8.2) ! tax liability for 3rd  qtr
02010   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr4),column4+leftmargin,lyne+=8.2) ! tax liability for 4th  qtr
02020   fnpa_txt(cnvrt$("pic(zzzzzzzzzzz.##)",futaqtr1+futaqtr2+futaqtr3+futaqtr4),column4+leftmargin,lyne+=8.2+margin) ! total liability
02030   gosub RELEASE_PRINT
02040   return 
02050 VBOPENPRINT: ! 
02060   if file(20)=-1 then 
02070     fnpa_open ! open #20: "Name="&env$('Q')&"\PRmstr\940"&wsid$&".txt,Replace,RecL=5000",display,output
02090     lyne=margin ! starting of 1st line
02100     column2=103 !:
          column3=153
02110   end if 
02120   return 
02130 RELEASE_PRINT: ! 
02140   fnpa_finis
02170   return 
02180 CSZ: ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
02190 L2190: p1=pos(csz$,".",1)
02200   if p1>0 then csz$(p1:p1)=" ": p2=p1: goto L2190
02210 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
02220   p2=0
02230 L2230: p1=pos(csz$,",",1)
02240   if p1>0 then csz$(p1:p1)=" ": p2=p1: goto L2230
02250 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
02260 L2260: p1=pos(rtrm$(csz$),"  ",1)
02270   if p1>0 then csz$(p1+1:p1+1)="" : goto L2260
02280   csz$=ltrm$(rtrm$(csz$)): p1=pos(csz$," ",-1)
02290   zip$=csz$(p1+1:len(csz$)): zip$=ltrm$(rtrm$(zip$))
02300   p2=pos(csz$(1:p1-1)," ",-1) : state$=csz$(p2+1:p1-1)(1:2) : state$=ltrm$(rtrm$(state$))
02310   city$=csz$(1:p2-1)(1:15): city$=ltrm$(rtrm$(city$))
02320   return 
