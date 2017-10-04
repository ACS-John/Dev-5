00010 !  REPLACE S:\acsPR\newcorrectedPRW2A
00020 ! used to file a corrected w2 using prace.exe
00030 ! ___________________
00040   library 'S:\Core\Library': fncno,fntop,fnxit,fncloseprn,fnopenprn,fnconsole,fntos,fnfra,fnlbl,fntxt,fncmdkey,fnacs,fnopt,fncombof,fnmsgbox,fnchk,fncmbemp,fnpa_finis,fnpa_open,fnpa_newpage,fnDedNames
00050   fntop(program$,cap$="Corrected W2 Forms")
00060   on error goto L4300
00070 ! ___________________
00080   on fkey 5 goto XIT
00120   dim ss$*11,d(14),ty(21),s(13),t(13),z$*8,desc$(6)*15,amt(6),io1$(12)
00130   dim tcp(32),tdc(10),resp$(70)*50
00140   dim w(13),a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12,cap$*128
00150   dim oldw(13),heading$*50
00160   dim fa$(2),fb$(1),fc$(1),fd$(1),l$(10),newdedfed(20),newdedcode(20)
00170   dim newcalcode(20),dedfica(20),dedst(20),deduc(20),fullname$(20)*20
00180   dim abrevname$(20)*8
00190   dim fm4$*255,in4$(30),sel$(4)*40,io6$(4),dedcode$(20)*2,dedyn$(20)*1
00200   dim miscded(20),box12(20),txt$*80,totalbox12(20)
00210   let fm4$="FORM  POS 1,C 8"&rpt$(",C 12,G 10.2,3*G 1",6)
00220   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00230   read #1,using L240: mat a$,b$,mat d$,loccode,mat e$,mat dedcode,mat dedfed
00240 L240: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 618,10*n 1,pos 638,10*n 1
00250   for j=1 to 3: a$(j)=a$(j)(1:30): next j
00260   close #1: 
00270   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
00280 DATE_SCREEN: ! 
00290 L290: let fntos(sn$="W2-1") !:
        let rc=cf=0: let mylen=25 : let mypos=mylen+3
00300   fnfra(1,1,3,50,"Date Range for W2's","Normally this would the first and last day of the calendar year",0) !:
        cf+=1 : let fratype=cf
00310   fnlbl(1,1,"Starting Date:",mylen,1,0,1)
00320   fntxt(1,mypos,10,0,1,"3",0,"First day of calendar year",1) !:
        let resp$(rc+=1)=str$(beg_date)
00330   fnlbl(2,1,"Ending Date:",mylen,1,0,1)
00340   fntxt(2,mypos,10,0,1,"3",0,"Last day of calendar year",1) !:
        let resp$(rc+=1)=str$(end_date)
00350   fncmdkey("Next",1,1,0,"Prints the report")
00360   fncmdkey("Cancel",5,0,1,"Returns to menu")
00370   fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
00380   beg_date=val(resp$(1))
00390   let end_date=val(resp$(2))
00400   let taxyear=val(resp$(2)(1:4))
00410   if beg_date=0 or end_date=0 then goto L290
00420   let ssrate=.062 !:
        let ssmax=102000 !:
        let mcrate=.0145 !:
        let mcmax=999999 !:
        let w1=4 !:
        let namcde$="F" !:
        let topmargin=13 !:
        bottom=141 !:
        let posx=130
00430 ASK_INFO: ! 
00440   fntos(sn$="Prw2-2") !:
        let rc=cf=0: let mylen=46: let mypos=mylen+3
00450   fnfra(1,1,5,60,"Print W-2s","This W-2 program prints to preprinted W2 forms coded with 22222.",0) !:
        cf+=1 : let franum=cf
00460   fnlbl(1,1,"Social Security Withholding Rate:",mylen,1,0,franum)
00470   fntxt(1,mypos,10,0,1,"34",0,"Use format such as .062.",franum) !:
        let resp$(rc+=1)=str$(ssrate)
00480   fnlbl(2,1,"Maximum Wage Subject to SS Withholdings:",mylen,1,0,franum)
00490   fntxt(2,mypos,10,0,1,"10",0,"Enter the maximum wage subject to social security withholdings for the current year just ended.",franum) !:
        let resp$(rc+=1)=str$(ssmax)
00500   fnlbl(4,1,"Medicare Withholding Rate:",mylen,1,0,franum)
00510   fntxt(4,mypos,10,0,1,"34",0,"Use format such as .0145 .",franum) !:
        let resp$(rc+=1)=str$(mcrate)
00520   fnlbl(5,1,"Maximum Wage Subject to Medicare Withholdings:",mylen,1,0,franum)
00530   fntxt(5,mypos,10,0,1,"10",0,"At the present time there is no maximum.  Enter a number larger than any one's wages can be. For example, 999999.00",franum) !:
        let resp$(rc+=1)=str$(mcmax)
00540   fnfra(8,1,3,60,"Printing or Exporting","You have the option to either pr the W-2s or export them to another system for printing.") !:
        cf+=1 : let franum=cf : let mylen=26 : let mypos=mylen+2
00550   fnopt(1,3,"Print W-2",0,franum) !:
        let resp$(rc+=1)="True"
00560   fnopt(2,3,"Export to another system",0,franum) !:
        let resp$(rc+=1)="False"
00570   fnfra(13,1,3,60,"Identify the Following Deductions","You have twenty miscellaneous deductions available to you. If you have Qualified Pension or Dependent Care, start with the first deduction and count down to identify the number of the deduction.") !:
        cf+=1 : let franum=cf
00580   fnlbl(1,1,"Qualified Pension Plan:",mylen,1,0,franum)
00590   fntxt(1,mypos,2,0,1,"30",0,"If you have a qualified pension plan that requires the pension plan box to be checked, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum) !:
        let resp$(rc+=1)=str$(pn1)
00600   fnlbl(2,1,"Dependent Care Benefits:",mylen,1,0,franum)
00610   fntxt(2,mypos,2,0,1,"30",0,"If you have dependent care benefits that should be identifies on the W-2, count down from your 1st miscellaneous deduction to determine the number to enter here.",franum) !:
        let resp$(rc+=1)=str$(dc1)
00620   fnlbl(18,1,"Employee Name Format-(F)irst name 1st; (L)ast name 1st:",57,1,0,0)
00630   fntxt(18,60,1,0,1,"",0,"Is the first name shown first in the employee record or is the Last name shoun first. Indicate with an F or an L.",0) !:
        let resp$(rc+=1)=namcde$
00640   fnfra(20,1,3,60,"W-2 Alignment","You can move the pr up or down on either W-2 by increasing or decreasing the millimeters on the top margin.") !:
        cf+=1 : let franum=cf
00650   fnlbl(1,1,"Top Margin - Top W-2:",mylen,1,0,franum)
00660   fntxt(1,mypos,3,0,1,"30",0,"Decrease the top margin to move the pr up. Increase the top margin to move the W-2 down.",franum) !:
        let resp$(rc+=1)=str$(topmargin)
00670   fnlbl(2,1,"Top Margin - Bottom W-2:",mylen,1,0,franum)
00680   fntxt(2,mypos,3,0,1,"30",0,"The spacing on the bottom W-2 is controlled seperate from the top W-2.",franum) !:
        let resp$(rc+=1)=str$(bottom)
00690   fnlbl(3,1,"Position of Pension X:",mylen,1,0,franum)
00700   fntxt(3,mypos,3,0,1,"30",0,"Increasing the position of the X will move it right.  Decreasing will move it left.",franum) !:
        let resp$(rc+=1)=str$(posx)
00710   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
00720   fncmdkey("&Cancel",5,0,1,"Returns to menu")
00730   fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
00740   let ssrate=val(resp$(1))
00750   let ssmax=val(resp$(2))
00760   let mcrate=val(resp$(3))
00770   let mcmax=val(resp$(4))
00780   if resp$(5)="True" then let w1=4 else let w1=3
00790   if resp$(6)="True" then let w1=3
00800   let pn1=val(resp$(7))
00810   let dc1=val(resp$(8))
00820   let namcde$=resp$(9)
00830   let topmargin=val(resp$(10))
00840   bottom=val(resp$(11))
00850   let posx=val(resp$(12))
00860   let med$="Y"
00870   gosub L5540
00880   gosub VBOPENPRINT
00890   let goproc=0
00900   if w1=2 then gosub L5930
00910   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00920   open #2: "Name="&env$('Q')&"\PRmstr\department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\deptidx.h"&env$('cno'),internal,outin,keyed 
00930   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00940   open #3: "Name="&env$('Temp')&"\Addr."&session$,internal,input ioerr L960
00950   close #3,free: 
00960 L960: open #3: "Name="&env$('Temp')&"\Addr."&session$&",size=0,RecL=33,NoShr",internal,output 
00970   if w1=3 then gosub L4430
00980   write #3,using L990: ssmax,w1,nw
00990 L990: form pos 1,n 10.2,2*n 1
01000   open #14: "Name="&env$('Q')&"\PRmstr\W2Box16.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\W2Index.h"&env$('cno')&",Shr",internal,input,keyed ioerr L1030
01010   let z$="NO" ! 1/12/90
01020   box16=1
01030 L1030: if loccode=0 then goto ASK_STARTING else goto L2310
01040 L1040: read #1,using L1130: eno,mat k$,ss$,em6,ta eof L2090
01050   if endnum>0 and eno>endnum then goto L2090 ! ending employee number entered
01060   gosub L5770
01070   let kz$=lpad$(rtrm$(str$(eno)),8)
01080   let px$=""
01090   mat desc$=(" ")
01100   mat amt=(0)
01110   mat miscded=(0)
01120   let tdedret=0 ! REMOVE EXPLANATION  FROM LINE 905 TO LIST RETIREMENT IN BOX 13
01130 L1130: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
01140   if numb=0 then goto L1160
01150   if eno<empno then goto L1040
01160 L1160: let first=1
01170   checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
01180   restore #4,key>=checkkey$: nokey L1040
01190 L1190: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L1650
01200   if heno<>eno then goto L1650
01210   if prd<beg_date or prd>end_date then goto L1190 ! not this year
01220   read #2,using "form pos 48,n 2", key=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zzz)",tdn): tcd nokey L1230 ! get state code
01230 L1230: if tcd<1 or tcd>10 then let tcd=1
01240   if w1=3 then let stcode=tcd : goto L1270
01250   if first=1 then let stcode=tcd
01260   if first=0 and stcode><tcd then goto L1290
01270 L1270: let state$=d$(tcd)(1:2)
01280   let stcode$=e$(tcd)
01290 L1290: let dedfica=0
01300   let dedret=0
01310   for j=1 to 20
01320     if newdedfed(j)>=1 and newdedcode(j)=1 then let dedret=dedret+tcp(j+4)
01330     if dedfica(j)=1 and newdedcode(j)=1 then let dedfica=dedfica+tcp(j+4)
01340     let miscded(j)=miscded(j)+tcp(j+4)
01350   next j
01360 ! LET TDEDRET=TDEDRET+DEDRET ! ACCUMULATE BOX 13 RETIREMENT; THIS LINE WILL ONLY WORK IF NO CAFETERIA; REMOVE ! OFF 861 AND 882 FOR RETIREMENT ONLY ! can change 882 only if know specific ded to show in box 13
01370   let w(1)=w(1)+tcp(1) ! FED W/H YTD
01380   let w(2)=w(2)+ tcp(31)-dedret ! TOTAL TAXABLE WAGES
01390   let w3=w3+tcp(2) ! FICA W/H YTD
01400   let w(4)=w(4)+tcp(24) ! EIC TOTAL
01410   if em6=9 then goto L1460 else let w(5)=w(5)+tcp(31)-tcp(30)-dedfica ! TOTAL SS WAGES
01420   let w(11)=w(11)+tcp(31)-dedfica ! TOTAL MC WAGES & TIPS
01440   if em6=2 then let w(5)=0 ! NO SS
01450   if em6=1 then let w(11)=0 ! NO MC
01460 L1460: ! 
01470   if uprc$(med$)="Y" then let w(3)=w(3)+tcp(2) ! ss wh and medicare seperated in employee record
01480   if uprc$(med$)="Y" then let w(12)=w(12)+tcp(3) ! medicare
01490   if tcd><stcode then goto L1580
01500   let w(7)=w(7)+tcp(4) ! STATE WH
01510   let w(9)=w(9)+tcp(31)-dedret ! STATE WAGES
01520   if loccode=0 or tcp(loccode+4)=0 then goto L1550
01530   let w(8)=w(8)+tcp(loccode+4) ! LOCAL WITHHOLDING
01540   let w(10)=w(10)+tcp(31)-dedret ! LOCAL WAGES
01550 L1550: if pn1>0 and tcp(pn1+4)>0 then let px$="X"
01560   if dc1>0 and dc1<11 then let dcb=dcb+tcp(dc1+4)
01570   goto L1620
01580 L1580: if loccode=0 then let lowh=0 else let lowh=tcp(loccode+4)
01590   write #3,using L1600: eno,tcd,tcp(31)-dedret,tcp(3),lowh,f$
01600 L1600: form pos 1,n 8,n 2,3*pd 5.2,c 8
01610   let goproc=1
01620 L1620: let first=0
01630 ! If TA>0 Then Goto 1120
01640   goto L1190 ! read next check record
01650 L1650: if box16=1 then gosub L4120
01660   if tdedret=0 then goto L1720
01670   for j=3 to 6
01680     if ltrm$(rtrm$(desc$(j)))="" then goto L1690 else goto L1710
01690 L1690: let desc$(j)=lpad$("G "&cnvrt$("Nz 10.2",tdedret),15)
01700     goto L1720
01710 L1710: next j
01720 L1720: for j=1 to 20
01730     if uprc$(dedyn$(j))<>"Y" or miscded(j)=0 then goto L1790
01740     for j2=3 to 6
01750       if ltrm$(rtrm$(desc$(j2)))="" then goto L1760 else goto L1780
01760 L1760: let desc$(j2)=lpad$(dedcode$(j)&" "&cnvrt$("Nz 10.2",miscded(j)),15): let totalbox12(j)=totalbox12(j)+miscded(j)
01770       goto L1790
01780 L1780: next j2
01790 L1790: next j
01800   let w(5)=min(ssmax-w(6),w(5)) ! SS WAGES CANNOT EXCEED MAXIMUM
01810   let w(11)=min(mcmax,w(11)) ! MC WAGES CANNOT EXCEED MAXIMUM
01830   if uprc$(med$)="N" then let w(3)=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH ! change to seperate medicare
01840   if uprc$(med$)="N" then let w(12)=w3-w(3) ! MEDICARE WITHHELD  ! change to seperate medicare
01850   if uprc$(med$)="N" and w(5)>w(11)-1 and w(5)<w(11)+1 then let w(5)=w(11) ! set ss wages and medicare wages equal in within 1.00
01860   if uprc$(med$)="N" and em6=1 then let w(12)=0 : let w(3)=w3 ! NO MC ALL SS ! change to seperate medicare
01870   if uprc$(med$)="N" and em6=2 then let w(3)=0 : let w(12)=w3 ! NO SS ALL MC ! change to seperate medicare
01880   if em6=9 then let w(3)=w(5)=w(11)=w(12)=0 ! NO SS OR MC
01890   if w(8)=0 then let pf$="" : goto L1920
01900   if z$="YES" then gosub L2690
01910   let pf$=f$
01920 L1920: let g$=str$(eno)
01930   if w(2)=0 and w(5)=0 then goto L2060 ! skip w2 if no wages
01940   gosub PRINTW2
01950   mat s=s+w
01960   let wctr=wctr+1
01970   if w1=3 then goto L2060
01980   goto L2060 ! IF WCTR<41 THEN GOTO 1310
01990   let desc$(3)=lpad$("  "&cnvrt$("Nz 10.2",s(13)),15)
02000   mat w=s: let g$="SUB TOTAL": gosub TOT1
02010   let nosub=1
02020   let desc$(3)=""
02030   let wctr=0
02040   mat t=t+s
02050   mat s=(0)
02060 L2060: mat w=(0)
02070   let nqp=dcb=w3=0
02080   goto L1040
02090 L2090: if w1=3 then goto L2230
02100   goto ASK_STARTING
02110   if wctr=0 or nosub=0 then goto L2140
02120   let desc$(3)=lpad$("  "&cnvrt$("Nz 10.2",s(13)),15)
02130 L2130: mat w=s: let g$="SUB TOTAL" : gosub TOT1
02140 L2140: mat t=t+s
02150   let misc=3
02160   for j=1 to 10
02170     if totalbox12(j)=0 then goto L2210
02180     let desc$(misc)=lpad$("  "&cnvrt$("Nz 10.2", totalbox12(j)),15)
02190     let misc=misc+1
02200     if misc>7 then goto L2220 ! only allow 4 different deductions
02210 L2210: next j
02220 L2220: mat w=t: let g$="Final Total" : let first$=mid$=last$="" : gosub TOT1
02230 L2230: close #1: 
02240   close #2: 
02250   close #3: 
02260   if w1=3 then close #5: : goto XIT
02270 L2270: gosub RELEASE_PRINT
02280   if goproc=1 then goto PRW2B
02290 XIT: let fnxit
02300 ! _________________________
02310 L2310: let fntos(sn$="Prw2-6") !:
        let rc=cf=0: let mylen=20: let mypos=mylen+3
02320   fnlbl(1,1,"Locality Name:",mylen,1,0,0)
02330   fntxt(1,mypos,12,0,1,"",0,"If you have answered that you have local withholdings in the company information file, you must enter the locality name") !:
        let resp$(rc+=1)=z$
02340   fnlbl(3,5,"(Enter the locality name if the same on all employees.",60,0,0,0)
02350   fnlbl(4,5,"Leave blank if not applicable.  Enter YES if applicable",60,0,0,0)
02360   fnlbl(5,5,"but not he same on all employees.)",60,0,0,0)
02370   fncmdkey("&Next",1,1,0,"Proceed to next screen.") !:
        fncmdkey("&Cancel",5,0,1,"Returns to menu")
02380   fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
02390   let z$=resp$(1)
02400   let z$=uprc$(rtrm$(z$))
02410   if rtrm$(z$)="" then let z$="NO"
02420   if z$="YES" or z$="NO" then goto ASK_STARTING
02430   let f$=z$
02440   if z$="" then goto L2310
02450 ASK_STARTING: ! 
02460   fntos(sn$="Prw2-3") !:
        let respc=cf=0: let mylen=40: let mypos=mylen+3 : let mylen2=62: let mypos2=20
02470   fnlbl(1,1,"Starting Employee Number:",mylen,1,0,0)
02480   fncmbemp(1,mypos) !:
        let resp$(respc+=1)=""
02490   fnlbl(2,1,"Ending Employee Number (blank for all):",mylen,1,0,0)
02500   fncmbemp(2,mypos) !:
        let resp$(respc+=1)=""
02510   fnlbl(4,mypos2,"To pr a single W2, use the same starting and ending number.",mylen2,0,0,0)
02560   fnlbl(9,mypos2," ",mylen2,0,0,0)
02570   fncmdkey("&Next",1,1,0,"Proceed to next screen.") !:
        fncmdkey("&Complete",5,0,1,"Returns to menu")
02580   fnacs(sn$,0,mat resp$,ckey)
02600   if ckey=5 then let totalcode=1 : goto L2130
02610   restore #1: 
02620   let numb=val(resp$(1)(1:8))
02630   let endnum=val(resp$(2)(1:8))
02640   if numb=0 then goto L1040
02650   let empno=numb
02660   goto L1040
02670 ! ___________________________________________
02680 ASK_LOCALITY: ! 
02690 L2690: let fntos(sn$="Prw2-5") !:
        let rc=cf=0: let mylen=30: let mypos=mylen+3
02700   fnlbl(1,1,k$(1),mylen,1,0,0)
02710   fnlbl(2,1,"Locality Name:",mylen,1,0,0)
02720   fntxt(2,mypos,12,0,1,"",0,"Enter the Locality for this employee.",0) !:
        let resp$(rc+=1)=f$
02730   fncmdkey("&Next",1,1,0,"Proceed to next screen.") !:
        fncmdkey("&Cancel",5,0,1,"Returns to menu")
02740   fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
02750   let f$=resp$(1)
02760   let g$=rtrm$(g$)
02770   if g$="1" then goto L2790
02780   let f$=g$
02790 L2790: return 
02800 ! ___________________________________________
02810 PRW2B: open #1: "Name="&env$('Temp')&"\Control."&session$,internal,output 
02820   restore #1: 
02830 L2830: form pos 1,c 128
02840   write #1,using L2830: "FILE "&env$('Temp')&"\Addr."&session$&",,,PRW2ADDR.H"&env$('cno')&","&env$('Q')&"\PRmstr,,"&env$('Q')&"\PRmstr,,A,N"
02850   write #1,using L2830: "MASK 9,2,n,a,1,8,n,a"
02860   close #1: 
02870   execute "Free "&env$('Q')&"\PRmstr\PRW2ADDR.H"&env$('cno')&" -n" ioerr L2880
02880 L2880: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
02890   fnxit ! stop  ! Let FNCHAIN("S:\acsPR\prw2b")
02900 ! ___________________________________________
02910 TOT1: mat k$=(""): let ss$=stcode$=state$=pf$="": let eno=0: let k$(1)="Total Sheet"
02920   let x$=" ": let p1=58: let p2=126
02930 PRINTW2: ! pr W2 FORM
02940   gosub ASK_OLD_INFO
02950   column1=15 !:
        column2=60 !:
        column3=113 !:
        column4=160
02960   let inc=0
02970   pr #20: 'Call Print.MyFontSize(10)'
02980   let txt$=ss$
02990   let lyne+=4.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03000   let txt$=str$(taxyear)
03010   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03020   let txt$=a$(1)
03030   let lyne=lyne+4.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03040   let txt$=a$(2)
03050   let lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03060   let txt$=a$(3)
03070   let lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03080   if trim$(oldss$)<>trim$(ss$) then goto L3090 else goto L3110
03090 L3090: let txt$=oldss$
03100   let lyne+=3: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03110 L3110: let txt$=b$
03120   let lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03130   let txt$= trim$(first$)&" "&trim$(mid$)(1:1)
03140   let lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03150   let txt$=last$
03160   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03170   let txt$=k$(2)
03180   let lyne=lyne+5.3: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03190   let txt$=k$(3)
03200   let lyne=lyne+8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03210   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(2))
03220   let lyne=lyne+17: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03230   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(2))
03240   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03250   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(1))
03260   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03270   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(1))
03280   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03290   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(5))
03300   let lyne+=8: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03310   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(5))
03320   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03330   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(3))
03340   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03350   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(3))
03360   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03370   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(11))
03380   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03390   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(11))
03400   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03410   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(12))
03420   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03430   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(12))
03440   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03450   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(6))
03460   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03470   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(6))
03480   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03490   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(4))
03500   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03510   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(4))
03520   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03530   let txt$=cnvrt$("pic(zz,zzz,zzz.##",dcb)
03540   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03550   let txt$=cnvrt$("pic(zz,zzz,zzz.##",olddcb)
03560   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3)&','&str$(lyne)&')'
03570   let txt$=desc$(1)
03580   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03590   let txt$=box1a$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box1)
03600   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
03610   let txt$=desc$(3)
03620   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03630   let txt$=box2b$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box2)
03640   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
03650   let txt$=px$
03660   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(posx)&','&str$(lyne)&')'
03670   if posx<51 then goto L3700
03680   let txt$=px$
03690   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(posx-50)&','&str$(lyne)&')'
03700 L3700: let txt$=desc$(4)
03710   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03720   let txt$=box3c$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box3)
03730   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
03740   let txt$=desc$(5)
03750   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column4)&','&str$(lyne)&')'
03760   let txt$=box4d$&"  "&cnvrt$("pic(zz,zzz,zzz.##",box4)
03770   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column3-3)&','&str$(lyne)&')'
03780 ! Let TXT$=DESC$(6)
03790 ! Let LYNE=LYNE+8.5: pr #20: 'Call Print.AddText("'&TXT$&'",'&STR$(COLUMN4)&','&STR$(LYNE)&')'
03800   let txt$=state$
03810   let lyne=lyne+21: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03820   let txt$=state$
03830   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03840   let txt$=stcode$
03850   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03860   let txt$=stcode$
03870   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03880   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(9))
03890   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03900   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(9))
03910   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03920   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(7))
03930   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03940   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(7))
03950   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
03960   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(10))
03970   let lyne+=17: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
03980   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(10))
03990   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
04000   let txt$=cnvrt$("pic(zz,zzz,zzz.##",w(8))
04010   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
04020   let txt$=cnvrt$("pic(zz,zzz,zzz.##",oldw(8))
04030   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
04040   let txt$=pf$(1:6)
04050   let lyne+=8.5: pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column2)&','&str$(lyne)&')'
04060   let txt$=pf$(1:6)
04070   pr #20: 'Call Print.AddText("'&txt$&'",'&str$(column1)&','&str$(lyne)&')'
04080   fnpa_newpage : let lyne=topmargin: count=0
04090   goto L4110
04100 L4100: let x$=""
04110 L4110: return 
04120 L4120: read #14,using fm4$,key=kz$: kz$,mat in4$ nokey L4290
04130   for j=1 to 6
04140     amt(j)=val(in4$(j*5-3))
04150     if in4$(j*5-2)="1" then let w(2)=w(2)+amt(j)
04160     if in4$(j*5-1)="1" then let w(5)=w(5)+amt(j)
04180     if in4$(j*5-1)="1" then let w(11)=w(11)+amt(j)
04190 L4190: if in4$(j*5-0)="1" then let w(9)=w(9)+amt(j)
04200     if in4$(j*5-2)="2" then let w(2)=w(2)-amt(j)
04210     if in4$(j*5-1)="2" then let w(5)=w(5)-amt(j)
04230     if in4$(j*5-1)="2" then let w(11)=w(11)-amt(j)
04240 L4240: if in4$(j*5-0)="2" then let w(9)=w(9)-amt(j)
04250     if j>1 then let desc$(j)=lpad$(in4$(j*5-4)(1:1)&"  "&cnvrt$("Nz 10.2",amt(j)),15)
04260     if j=1 then let desc$(j)=lpad$(in4$(j*5-4)(1:1)&"  "&ltrm$(cnvrt$("Nz 10.2",amt(j))),15)
04270 ! If (J=3 OR J=4) AND (IN4$(J*5-4)(1:1)="D" OR IN4$(J*5-4)(1:1)="E" OR IN4$(J*5-4)(1:1)="F" OR IN4$(J*5-4)(1:1)="H") Then Let W(13)=W(13)+AMT(J) ! SUBTOTAL BOX 17 IF D,E,F,OR H CODES
04280   next j
04290 L4290: return 
04300 L4300: if err=61 then pr fields "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L4320
04310   goto L4360
04320 L4320: pr newpage
04330   if err=4148 then pr fields "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L4350
04340   goto L4360
04350 L4350: pr fields "23,1,C 80,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
04360 L4360: pr fields "24,1,C 80,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
04370   input fields "24,60,C 1,N": quitcode$
04380   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
04390   pr fields "23,1,C 80,N": ""
04400   pr fields "24,1,C 80,N": ""
04410   retry 
04420   goto XIT
04430 L4430: dim fl$*40
04440 ! pr NEWPAGE
04450   close #101: ioerr L4460
04460 L4460: open #101: "SROW=2,SCOL=2,EROW=07,ECOL=35,BORDER=DR,CAPTION=SELECT LASER W2 SOFTWARE",display,outin 
04470   pr fields "3,5,C 28": "1 = ADVANCED MICRO SOLUTIONS"
04480   pr fields "4,5,C 28": "2 = CENTER PIECE SOFTWARE"
04490   pr fields "6,5,C 25,R,N": " ENTER YOUR SELECTION #: "
04500   pr fields "8,8,C 16,R,N": "PRESS F5 TO STOP"
04510 L4510: input fields "6,30,N 1,UET,N": sw1 conv L4510
04520   if cmdkey=5 then goto XIT
04530   on sw1 goto L4540,L4550 none L4510
04540 L4540: let fl$="\1099ETC.W04\W2DATA\W2DAT.PRN" : goto L4560
04550 L4550: let fl$="\CPS04\ASCIIW2.TXT" : goto L4560
04560 L4560: pr #101: newpage
04570   pr fields "3,5,C 30,R,N": "ENTER OUTPUT PATH & FILE NAME"
04580 L4580: rinput fields "5,5,C 30,UT,N": fl$
04590   if cmdkey=5 then goto XIT
04600   on sw1 goto L4620,L4640
04610   let fl$=rtrm$(fl$)
04620 L4620: open #5: "Name="&fl$&",REPLACE",display,output ioerr L4580
04630   goto L4650
04640 L4640: open #5: "Name="&fl$&",RecL=470,REPLACE",display,output ioerr L4580
04650 L4650: close #101: 
04660   return 
04670 W2LASER: ! GENERATE FILE FOR LAZER W2
04680   on sw1 goto L4690,L5400
04690 L4690: ! LASER W2 FOR ADVANCED MICRO SOLUTIONS
04700   let p1=0
04710   for j=1 to 11
04720     let p1=pos(ss$," ",1)
04730     if p1>0 then let ss$(p1:p1)=""
04740   next j
04750   pr #5: "ROAN=";g$
04760   pr #5: "FEIN=";b$
04770   pr #5: "WAGES=";w(2)
04780   pr #5: "FITW=";w(1)
04790   pr #5: "PNAME1=";a$(1)
04800   pr #5: "PNAME2=";" "
04810   pr #5: "SSWAGES=";w(5)
04820   pr #5: "SSWH=";w(3)
04830   pr #5: "PADDR1=";a$(2)
04840   pr #5: "PADDR2=";a$(3)
04850   pr #5: "MCWAGES=";w(11)
04860   pr #5: "MCWH=";w(12)
04870   pr #5: "SSN=";ss$
04880   pr #5: "SSTIPS=";w(6)
04890   pr #5: "ALLOCATIP=";0
04900   pr #5: "RNAME1=";(rtrm$(last$)&","&first$)(1:24)
04910   pr #5: "RNAME2=";k$(2)(1:24)
04920   pr #5: "AEIC=";w(4)
04930   pr #5: "DEPDCARE=";dcb
04940   pr #5: "RADDR1=";" "
04950   pr #5: "RADDR2=";k$(3)(1:24)
04960   pr #5: "LAB14A=";" "
04970   pr #5: "BOX14A=";0
04980   pr #5: "LAB12A=";desc$(3)(1:4)
04990   pr #5: "BOX12A=";desc$(3)(5:15)
05000   pr #5: "CNTRYCODE=";" "
05010   pr #5: "RCOUNTRY=";" "
05020   pr #5: "LAB14B=";" "
05030   pr #5: "BOX14B=";0
05040   pr #5: "LAB12B=";desc$(4)(1:4)
05050   pr #5: "BOX12B=";desc$(4)(5:15)
05060   pr #5: "LAB14C=";" "
05070   pr #5: "BOX14C=";0
05080   pr #5: "LAB12C=";desc$(5)(1:4)
05090   pr #5: "BOX12C=";desc$(5)(5:15)
05100   pr #5: "EESTAT=";"0"
05110   pr #5: "EERETR=";px$
05120   pr #5: "LAB14D=";" "
05130   pr #5: "BOX14D=";0
05140   pr #5: "LAB12D=";desc$(6)(1:4)
05150   pr #5: "BOX12D=";desc$(6)(5:15)
05160   pr #5: "EESICK=";0
05170   pr #5: "BOX11Q=";nqp
05180   pr #5: "NQPLANS=";" "
05190   pr #5: "STATE1=";state$
05200   pr #5: "SEIN1=";stcode$
05210   pr #5: "SWAGES1=";w(9)
05220   pr #5: "SITW1=";w(7)
05230   pr #5: "LWAGES1=";w(10)
05240   pr #5: "LITW1=";w(8)
05250   pr #5: "LOCAL1=";pf$
05260   pr #5: "STATE2=";" "
05270   pr #5: "SEIN2=";" "
05280   pr #5: "SWAGES2=";0
05290   pr #5: "SITW2=";0
05300   pr #5: "LWAGES2=";0
05310   pr #5: "LITW2=";0
05320   pr #5: "LOCAL2=";" "
05330   pr #5: "FName=";first$(1:24)
05340   pr #5: "LName=";last$(1:24)
05350   pr #5: "TAG=";" "
05360   pr #5: "EBAT=";" "
05370   pr #5: "PHONE=";" "
05380   pr #5: "*"
05390   goto L4100
05400 L5400: ! LAZER W2 FOR CENTER PIECE SOFTWARE
05410   let p1=pos(k$(1)," ",1)
05420   if p1=0 then let p1=len(k$(1))+1
05430   let q1$='","'
05440   let p3=pos(k$(3),",",1) : a3=2
05450   if p3=0 then let p3=pos(k$(3)," ",1): a3=1
05460   if p3=0 then let p3=len(k$(3))+1: a3=1
05470   let n1$=k$(1)(1:p1-1)
05480   let n2$=k$(1)(p1+1:p1+16)
05490   c1$=k$(3)(1:p3-1): c2$=k$(3)(p3+a3:p3+a3+1): c3$=k$(3)(p3+a3+3:p3+a3+12)
05500   pr #5,using L5510: n1$,n2$,k$(2),"",c1$,c2$,c3$,"",ss$,f$,state$,0,w(4),w(1),w(2),w(3),w(5),w(6),w(11),w(12),nqp,dcb,0,amt(1),"",amt(2),"",amt(3),"",0,"",0,0,0,0,0,w(7),w(9),w(8),w(10),0,0,0,0,"",""
05510 L5510: form pos 1,c 13,c 16,2*c 30,c 15,c 2,2*c 10,c 11,c 15,c 2,13*n 10.2,c 1,n 10.2,c 1,n 10.2,c 1,n 10.2,c 1,13*n 10.2,c 2,c 15
05520   c$=','
05530   goto L4100
05540 L5540: ! ask if any misecllaneous deductions should pr in box 12
05550 ASK_DEDUCTIONS: ! 
05560   fntos(sn$="Prw2-4") !:
        let rc=cf=0: let mylen=20: let mypos=mylen+3
05570   fnlbl(1,1,"Indicate if any of the 20 miscellaneous deductions",50,1,0,0)
05580   fnlbl(2,1,"should appear in any boxes on the W-2.",44,1,0,0)
05590   fnlbl(4,7," Deduction Name    Yes     Box #     Code",40,0,0,0)
05600   for j=1 to 20
05610     fnlbl(j+4,1,fullname$(j),mylen,1,0,0)
05620     fnchk(j+4,26,"",0,0) !:
          let resp$(rc+=1)="False"
05630     fntxt(j+4,35,2,0,1,"30",0,"Enter the box number on the W-2 where this deduction should print.",0) !:
          let resp$(rc+=1)=str$(box12(j))
05640     fntxt(j+4,45,2,0,1,"",0,"Enter the Code that should appear in the box.",0) !:
          let resp$(rc+=1)=dedcode$(j)
05650   next j
05660   fncmdkey("&Next",1,1,0,"Proceed to next screen.")
05670   fncmdkey("&Cancel",5,0,1,"Returns to menu")
05680   fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
05690   let x=0
05700   for j=1 to 20
05710     let x+=1: if resp$(x)="True" then let dedyn$(j)="Y"
05720     let x+=1: box12(j)=val(resp$(x))
05730     let x+=1: let dedcode$(j)=resp$(x)
05740   next j
05750   return 
05760 EXTRACT_NAME: ! 
05770 L5770: dim first$*15,mid$*15,last$*20,k$(3)*30,k$(3)*30
05780   let k$(1)=uprc$(rtrm$(k$(1))): ! Let NAMCDE$="s"
05790   let x1=pos(k$(1)," ",1)
05800   let x2=pos(k$(1)," ",x1+1)
05810   let x3=pos(k$(1)," ",x2+1)
05820   if uprc$(namcde$)="L" then goto L5870
05830   let first$=k$(1)(1:max(min(15,x1-1),1))
05840   if x2>0 then let mid$=k$(1)(x1+1:x2-1): let last$=k$(1)(x2+1:len(k$(1)))
05850   if x2=0 then let last$=k$(1)(x1+1:len(k$(1))): let mid$=""
05860   goto L5910
05870 L5870: ! last name first
05880   if x1>0 and k$(1)(x1-1:x1-1)="," then let last$=k$(1)(1:x1-2) else let last$=k$(1)(1:max(x1-1,1))
05890   if x2>0 then let first$=k$(1)(x1+1:x2-1): let mid$=k$(1)(x2+1:len(k$(1)))
05900   if x2=0 then let first$=k$(1)(x1+1:len(k$(1))): let mid$=""
05910 L5910: ! pr FIRST$,MID$,LAST$
05920   return 
05930 L5930: pr newpage ! left or right stub
05940   close #101: ioerr L5950
05950 L5950: open #101: "SROW=4,SCOL=14,EROW=6,ECOL=60,BORDER=DR,CAPTION=<"&cap$&" - Right or Left Stub",display,outin 
05960   pr fields "5,20,C 40,N": '5 1/2" stub on Left or Right (L/R):'
05970   pr fields "7,28,C 9,B,1": "Next (F1)"
05980   pr fields "7,39,C 11,B,5": "Cancel (F5)"
05990   input fields "5,56,Cu 1,UT,N": left$
06000   if cmdkey=5 then goto XIT
06010   return 
06020 VBOPENPRINT: ! 
06030   if file(20)=-1 then 
06040     fnpa_open ! open #20: "Name="&env$('Q')&"\PRmstr\W2"&wsid$&".txt,Replace,RecL=5000",display,output
06070     let lyne=topmargin ! starting of 1st line
06080     character=1.5
06090   end if 
06100   return 
06110 RELEASE_PRINT: ! 
06120 ! 
06130   fnpa_finis
06150   return 
06160 ASK_OLD_INFO: ! 
06170   fntos(sn$="Ask_old") !:
        let lc=rc=0 : let mylen=20 : let mypos=mylen+3
06180   if totalcode=0 then let heading$="Enter Information from Incorrect W-2" else let heading$="Total Screen for Corrected W-2s"
06190   fnlbl(lc+=1,1,heading$,40,0)
06200   fnlbl(lc+=2,1,"Employee "&str$(eno),mylen,1)
06210   fnlbl(lc+=1,1,"SS #:",mylen,1)
06220   fntxt(lc,mypos,11,0,0,"") !:
        let resp$(rc+=1)=ss$
06230   fnlbl(lc+=1,1,"Total Wage:",mylen,1)
06240   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(2))
06250   fnlbl(lc+=1,1,"SS Wages:",mylen,1)
06260   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(5))
06270   fnlbl(lc+=1,1,"Medicare Wages:",mylen,1)
06280   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(11))
06290   fnlbl(lc+=1,1,"State Wages:",mylen,1)
06300   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(9))
06310   let mypos+=37: let fnlbl(lc=1,38,"Federal Wh:",mylen,1)
06320   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(1))
06330   fnlbl(lc+=1,38,"SS Withholdings:",mylen,1)
06340   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(3))
06350   fnlbl(lc+=1,38,"Medicare Wh:",mylen,1)
06360   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(12))
06370   fnlbl(lc+=1,38,"State Wh:",mylen,1)
06380   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(w(7))
06390   fnlbl(lc+=1,38,"Box 12a Code:",mylen,1)
06400   fntxt(lc,mypos,2,0,0,"") !:
        let resp$(rc+=1)=box1a$
06410   fnlbl(lc+=1,38,"Box 12a Amount:",mylen,1)
06420   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(box1)
06430   fnlbl(lc+=1,38,"Box 12b Code:",mylen,1)
06440   fntxt(lc,mypos,2,0,0,"") !:
        let resp$(rc+=1)=box2a$
06450   fnlbl(lc+=1,38,"Box 12b Amount:",mylen,1)
06460   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(box2)
06470   fnlbl(lc+=1,38,"Box 12c Code:",mylen,1)
06480   fntxt(lc,mypos,2,0,0,"") !:
        let resp$(rc+=1)=box3a$
06490   fnlbl(lc+=1,38,"Box 12c Amount:",mylen,1)
06500   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(box3)
06510   fnlbl(lc+=1,38,"Box 12d Code:",mylen,1)
06520   fntxt(lc,mypos,2,0,0,"") !:
        let resp$(rc+=1)=box4a$
06530   fnlbl(lc+=1,38,"Box 12d Amount:",mylen,1)
06540   fntxt(lc,mypos,10,0,0,"10") !:
        let resp$(rc+=1)=str$(box4)
06550   fncmdkey('&Next',1,1,0) !:
        fncmdkey('&Cancel',5,0,1)
06560   fnacs(sn$,0,mat resp$,ckey) ! old amounts
06561   if totalcode=1 then goto L2270
06570   if ckey=5 then goto ASK_STARTING
06580   let oldss$=resp$(1) ! ss#
06590   let oldw(2)=val(resp$(2)) ! total wage
06600   let oldw(5)=val(resp$(3)) ! total ss wage
06610   let oldw(11)=val(resp$(4)) ! total medicare wage
06620   let oldw(9)=val(resp$(5)) ! total state wage
06630   let oldw(1)=val(resp$(6)) ! federal wh
06640   let oldw(3)=val(resp$(7)) ! ss wh
06650   let oldw(12)=val(resp$(8)) ! medicare wh
06660   let oldw(7)=val(resp$(9)) ! state  wh
06670   box1a$=resp$(10) ! box 12a code
06680   box1=val(resp$(11)) ! box 12a amount
06690   box2b$=resp$(12) ! box 12b code
06700   box2=val(resp$(13)) ! box 12b amount
06710   box3c$=resp$(14) ! box 12c code
06720   box3=val(resp$(15)) ! box 12c amount
06730   box4d$=resp$(16) ! box 12d code
06740   box4=val(resp$(17)) ! box 12d amount
06750   return 
