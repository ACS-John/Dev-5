00010 ! Replace S:\acsPR\SelAuto
00020 ! Select Automatic Processing Programs
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3b,fncno,fnfkey,fnoldmsgbox,fnxit,fnconsole
00050   fntop("S:\acsPR\SelAuto",cap$="Select Automatic Processing Programs")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim m$(100)*80,pgm$(100)*22,pgm(100)
00090   dim nxtpgm$(20)*20,nxtdesc$(20)*35,wk(20),mo(20),qt(20)
00100   dim io1$(20),l1(20),desc$*35,sf$(20),sd$(20)*35
00110   dim cnam$*40,temp_desc$*35,msgline$(2)*60
00120   dim cap$*128,fkey$(12)*20,temp$(20)*80
00130 ! ______________________________________________________________________
00140   cap$="Select Automatic Processing Programs"
00150   fncno(cno,cnam$)
00155   fnconsole(1)
00160 ! ______________________________________________________________________
00170   data "    pr Payroll Worksheet","S:\acsPR\PRWKSHT"
00180   data "    Employee Proof List","S:\acsPR\PRPROOF"
00190   data "    Reassign Department Addresses","S:\acsPR\PRFIXADR"
00200   data "    Enter Time Sheets","S:\acsPR\PRINPUT"
00210   data "    Payroll Register","S:\acsPR\PRREG1"
00220   data "    Other Pay and Deductions Register","S:\acsPR\PROPDREG"
00230   data "    Payroll Checks","S:\acsPR\PRCKPRT"
00240   data "    YTD/QTD Register","S:\acsPR\PRYTDQTD"
00250   data "    Post To General Ledger","S:\acsPR\PRPOSTGL"
00260   data "      Payroll Tax Deposit Summary","S:\acsPR\PRREGTOT"
00270   data "      Zero Monthly","S:\acsPR\PRZMONTH"
00280   data "      State UnEmp.-One State","S:\acsPR\PRUC1"
00290   data "      941 Summary","S:\acsPR\PR941"
00300   data "      Quarterly Fed U/C Worksheet","S:\acsPR\QTRFEDUC"
00310   data "      State UnEmp.-Multi States","S:\acsPR\PRUC2"
00320   data "      Zero Quarterly","S:\acsPR\PRZQTD"
00330   data "      Annual Federal U/C Worksheet","S:\acsPR\PRFEDUC"
00340   data "      Deduction Register YTD","S:\acsPR\PRYTDMIS"
00350   data "      Enter W-2 Box 14 Info","S:\acsPR\W2BOX16"
00360   data "      W-2 Forms","S:\acsPR\PRW2A"
00370   data "      Electronic W2 Program","S:\acsPR\PRELECW2"
00380   data "      pr 1099","S:\acsPR\PR1099"
00390   data "      Electronic 401K","S:\acsPR\PR401K"
00400   data "      Zero Yearly","S:\acsPR\PRZYTD"
00410   data "      pr Reports In Report File","S:\acsPR\PRRPTs1"
00420   data "      Employee Review Register","S:\acsPR\PREMPREV"
00430   data "      Workmans Comp. Report","S:\acsPR\PRWKMCMP"
00440   data "      Labels","S:\acsPR\PRLABEL"
00450   data "      Input Manually Written Checks","S:\acsPR\PRINMAN"
00460   data "      Change Pay Period Ending Date","S:\acsPR\CHGDATE"
00470   data "      Maintain Tax Deposit File","S:\acsPR\PRREGFM"
00480   data "      Extend Time Card ","S:\acsPR\TIMECARD"
00490   data "      pr Input Worksheet","S:\acsPR\PRINPWK"
00500   data "    Job File Menu","S:\acsPR\JCMAINT"
00510   data "    Sub-Category File Menu","S:\acsPR\JCSCMINT"
00520   data "    User-Designed Reports File Menu","S:\acsPR\JCRPTFM"
00530   data "    Enter Time","S:\acsPR\JCINPUT"
00540   data "    Enter Charges","S:\acsPR\JCINPTC"
00550   data "    Enter Billings","S:\acsPR\JCIPBIL"
00560   data "    Transfer J/C To P/R","S:\acsPR\JCPRINPT"
00570   data "    Certified Payroll Register","S:\acsPR\JCCPR1"
00580   data "    pr Job Status Worksheet","S:\acsPR\JCSTWKSH"
00590   data "    Enter Percent Complete  ","S:\acsPR\JCPCTCMP"
00600   data "    pr Job Cost Report   ","S:\acsPR\JCPRTDET"
00610   data "    pr User-Designed Reports",""
00620   data "    Zero Current Period Info","S:\acsPR\JCZCUR"
00630   data "    Remove Completed Jobs   ","S:\acsPR\JCREMOVE"
00640   data "    pr Name and Number List","S:\acsPR\JCNAMLST"
00650 L650: read m$(i+=1),pgm$(i) eof OPEN_FILES : goto L650
00660 ! ______________________________________________________________________
00670 OPEN_FILES: ! 
00680   execute "Copy "&env$('Q')&"\PRmstr\PRPgmn.h"&str$(cno)&" sa"&wsid$&".tmp -n" ioerr L690
00690 L690: open #prpgmn=1: "Name=sa"&wsid$&".tmp,Use,RecL=58",internal,outin,relative 
00700 ! ______________________________________________________________________
00710 MENU1: ! 
00720   fnwin3b(win=101,cap$,21,75,0,4)
00730   pr #win,fields "1,1,C 75,R,N": "Line   Program                              Weekly    Monthly  Quarterly"
00740   for l1=1 to 20
00750     let io1$(l1)=str$(l1+1)&",3,C 71,N"
00760     read #prpgmn,using 'Form POS 1,C 20,C 35 ,3*N 1',rec=l1: nxtpgm$(l1),nxtdesc$(l1),wk(l1),mo(l1),qt(l1) norec L780
00770     goto L790
00780 L780: write #prpgmn,using 'Form POS 1,C 20,C 35 ,3*N 1',rec=l1: "","",0,0,0 !:
          let nxtpgm$(l1)=nxtdesc$(l1)="" : let wk(l1)=mo(l1)=qt(l1)=0
00790 L790: gosub PRINT_LINE_L1
00800   next l1
00810   input #win,select mat io1$,attr "H": mat temp$ !:
        let l1=curfld
00820   if cmdkey=1 then goto MENU1_SAVE
00830   if cmdkey=5 then goto XIT
00840   goto ASK_LINE_DATA
00850 ! ______________________________________________________________________
00860 XIT: ! 
00870   close #prpgmn: ioerr L880
00880 L880: if exists('sa'&wsid$&'.tmp') then execute 'Free sa'&wsid$&'.tmp'
00890   fnxit
00900 ! ______________________________________________________________________
00910 PRINT_LINE_L1: ! 
00920   pr #win,fields str$(l1+1)&",3,N 2,N": l1 !:
        pr #win,fields str$(l1+1)&",8,C 35,N": nxtdesc$(l1)
00930   if wk(l1)=1 then let tmpyn$="Y" else let tmpyn$="N"
00940   pr #win,fields str$(l1+1)&",50,C 1,N": tmpyn$
00950   if mo(l1)=1 then let tmpyn$="Y" else let tmpyn$="N"
00960   pr #win,fields str$(l1+1)&",61,C 1,N": tmpyn$
00970   if qt(l1)=1 then let tmpyn$="Y" else let tmpyn$="N"
00980   pr #win,fields str$(l1+1)&",72,C 1,N": tmpyn$
00990   return 
01000 ! ______________________________________________________________________
01010 CHANGE_PROGRAM: ! 
01020   let j1=j3=0 : mat sd$(20)
01030   fnwin3b(win=102,cap$,22,38,0,0) !:
        pr #win: newpage
01040   mat fkey$=("") : let fkey$(1)="Next" : let fkey$(2)="Top" !:
        fnfkey(24,mat fkey$,mat disfk,empty$,es=0)
01050   for j=1 to udim(m$)
01060     if pos(pgm$(j),"\",1)=0 then goto L1210
01070     let pgm(j1+=1)=j : let sf$(j1)=str$(j1+1)&",2,C 35,N"
01080     pr #win,fields sf$(j1): ltrm$(m$(j))(1:35)
01090     if j1<20 then goto L1210
01100 L1100: input #win,select mat sf$,attr "H": mat sd$
01110     if cmdkey=5 then goto L1280
01120     if cmdkey><2 then goto L1150
01130     let nxtpgm$(l1)=nxtdesc$(l1)="" : let wk(l1)=mo(l1)=qt(l1)=0
01140     goto L1260
01150 L1150: if cmdkey=1 then goto L1180
01160     let nxtpgm$(l1)=pgm$(pgm(curfld)) !:
          let nxtdesc$(l1)=ltrm$(m$(pgm(curfld)))(1:35) !:
          let wk(l1)=mo(l1)=qt(l1)=1
01170     goto L1260
01180 L1180: if j3=1 then goto CHANGE_PROGRAM
01190     let j1=0
01200     pr #win: newpage
01210 L1210: next j
01220   if j1<1 then goto CHANGE_PROGRAM
01230   mat sd$(j1)
01240   let j3=1
01250   goto L1100
01260 L1260: close #win: 
01270   ce+=1
01280 L1280: goto ASK_LINE_DATA
01290 ! ______________________________________________________________________
01300 ASK_LINE_DATA: ! 
01310   fnwin3b(win=103,cap$,7,55,0,0)
01320   pr #win,fields "2,2,Cr 12,N": "Line Number:" !:
        pr #win,fields "2,15,N 2,N": l1
01330   pr #win,fields "3,2,Cr 12,N": "Program:" !:
        pr #win,fields "4,2,Cr 12,N": "Weekly:" !:
        pr #win,fields "5,2,Cr 12,N": "Monthly:" !:
        pr #win,fields "6,2,Cr 12,N": "Quarterly:"
01340   let io3$(1)="3,15,C 35,PU,N" : let io3$(2)="4,15,Cu 1,CUT,N" !:
        let io3$(3)="5,15,Cu 1,UT,N" : let io3$(4)="6,15,Cu 1,UT,N"
01350   mat fkey$=("") !:
        let fkey$(1)="Save" : let fkey$(4)="Delete" : let fkey$(5)="Cancel" !:
        let fkey$(7)="Change Program" !:
        fnfkey(16,mat fkey$,mat disfk,em$="",es=0)
01360   if wk(l1)=1 then let temp_wk$="Y" else let temp_wk$="N"
01370   if mo(l1)=1 then let temp_mo$="Y" else let temp_mo$="N"
01380   if qt(l1)=1 then let temp_qt$="Y" else let temp_qt$="N"
01390   let temp_desc$=nxtdesc$(l1)
01400   if rtrm$(temp_desc$)="" then goto CHANGE_PROGRAM
01410 L1410: rinput #win,fields mat io3$: temp_desc$,temp_wk$,temp_mo$,temp_qt$
01420   if ce>0 then let io3$(ce)(ce1:ce2)="U": ce=0
01430   if cmdkey>0 then goto L1500 else ce=curfld
01440 L1440: ce=ce+1: if ce>udim(io3$) then ce=1
01450 L1450: let io3$(ce)=rtrm$(io3$(ce)) : ce1=pos(io3$(ce),"U",9) !:
        if ce1=0 then goto L1440
01460   ce2=ce1+1 : let io3$(ce)(ce1:ce1)="UC" : goto L1410
01470 CONV3: if ce>0 then let io3$(ce)(ce1:ce2)="U"
01480   ce=cnt+1
01490 ERR3: pr fields "24,78,C 1": bell : goto L1450
01500 L1500: if cmdkey=5 then goto DONE_ASK_LINE_DATA
01510   if cmdkey=7 then goto CHANGE_PROGRAM
01520   if cmdkey=4 then let nxtdesc$(l1)="" : let nxtpgm$(l1)="" !:
          let wk(l1)=0 : let mo(l1)=0 : let qt(l1)=0 !:
          gosub SAVE_LINE !:
          goto DONE_ASK_LINE_DATA
01530   if cmdkey<>1 then goto L1410
01540   let nxtdesc$(l1)=temp_desc$
01550   if temp_wk$="Y" then let wk(l1)=1 else let wk(l1)=0
01560   if temp_mo$="Y" then let mo(l1)=1 else let mo(l1)=0
01570   if temp_qt$="Y" then let qt(l1)=1 else let qt(l1)=0
01580   gosub SAVE_LINE
01590 DONE_ASK_LINE_DATA: goto MENU1
01600 ! ______________________________________________________________________
01610 SAVE_LINE: ! 
01620   rewrite #prpgmn,using 'Form POS 1,C 20,C 35 ,3*N 1',rec=l1: nxtpgm$(l1),nxtdesc$(l1),wk(l1),mo(l1),qt(l1)
01630   return 
01640 ! ______________________________________________________________________
01650 MENU1_SAVE: ! 
01660   for j1=1 to 20 !:
          rewrite #prpgmn,using 'Form POS 1,C 20,C 35 ,3*N 1',rec=j1: nxtpgm$(j1),nxtdesc$(j1),wk(j1),mo(j1),qt(j1) !:
        next j1
01670   close #prpgmn: 
01680   execute "copy sa"&wsid$&".tmp "&env$('Q')&"\PRmstr\PRPgmn.h"&str$(cno)&" -n"
01690   execute "Free sa"&wsid$&".tmp -n"
01700   goto XIT
01710 ! ______________________________________________________________________
01720 ! <Updateable Region: ERTN>
01730 ERTN: let fnerror(program$,err,line,act$,"xit")
01740   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01750   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01760   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01770 ERTN_EXEC_ACT: execute act$ : goto ERTN
01780 ! /region
01790 ! ______________________________________________________________________
