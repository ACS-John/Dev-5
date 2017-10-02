00010 ! Replace S:\acsGL\SelAuto
00020 ! Select Automatic Processing Programs
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fntos,fnflexinit1,fnflexadd1,fncmdkey,fnacs,fntxt,fnlbl,fnclient_has
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim m$(200)*80,pgm$(200)*22,hlpg$(200)*40,status$(200),cap$*128,xec$*80
00080   dim mx$(200)*80,pgmx$(200)*22,statusx$(200),pgm(200),mo$(12)*10
00090   dim cnam$*40,dat$*20,s(3),s(11),pedat$*20,autoproc$(200)*80,temp$(20)*80
00100   dim cch$*20,actpd$*6,iom$(2),scm$(2),sendto$*40,xec$*80,rpmsg$*46
00110   dim sf1$(20),l1(20),desc$*35,sf$(20),sd$(20)*35,prog$*8,srq(20)
00120   dim nxtpgm$(20)*35,nxtdesc$(20)*35,ff$(3)*18,prim(20),pn(20),cp(20)
00130   dim ln$*128,ln2$*128,item$(1)*35,resp$(60)*35,prg$(100)*40,nam$(100)*35
00140 ! ______________________________________________________________________
00150   let fntop(program$,cap$="Select Programs")
00160   let fncno(cno)
00170   open #1: "Name="&env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno)&",Use,RecL=76",internal,outin,relative ioerr L190
00180   goto L210
00190 L190: if exists(env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno)) >0 then execute "Free "&env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno)
00200   open #1: "Name="&env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno)&",Use,RecL=76",internal,outin,relative ioerr MAIN
00210 L210: if lrec(1)=0 then write #1,using L1690: nxtpgm$(1),nxtdesc$(1),pn(1),cp(1),prim(1),srq(1)
00220   for j=1 to lrec(1)
00230     read #1,using L1690: nxtpgm$(j),nxtdesc$(j),pn(j),cp(j),prim(j),srq(j) norec L240
00240 L240: next j
00250   form pos 1,c 20,c 35,n 3,3*n 1
00260   close #1: 
00270 MAIN: ! 
00280 L280: let fntos(sn$="GLInput") !:
        let mylen=20: let mypos=mylen+3 : let right=1
00290   let item=0: let resp=0
00300   let fnlbl(1,1,"Selected Items          Primary=1 Secondary=2        Menu Options to Select From",80,0)
00310   for j=1 to 20
00320     let fntxt(j+1,1,35,0,left,"",0,"Items select for automatic processing.",0 ) !:
          let resp$(resp+=1)=nxtdesc$(j)
00330     let fntxt(j+1,39,1,0,right,"",0,"Should the primary or secondary format be used?",0 )
00340     if prim(j)=0 then let prim(j)=1
00350     let resp$(resp+=1)=str$(prim(j))
00360   next j
00370   mat chdr$(2) : mat cmask$(2) : mat item$(2) !:
        chdr$(1)='Item #' !:
        chdr$(2)='Menu option'
00380   cmask$(1)='30' !:
        cmask$(2)='' !:
        let fnflexinit1('selauto',2,50,20,35,mat chdr$,mat cmask$,1,0,frame) !:
        let editrec=0
00390   close #1: ioerr ignore
00400   open #1: "Name=gL.mnu",display,input 
00410 L410: linput #1: ln$ eof L540
00420   if ln$(1:1)<>">" then goto L410 ! skip headings
00430   if ln$(1:1)=">" then let ln$(1:1)=""
00440   if ln$(1:1)=">" then let ln$(1:1)="" ! delete up to two >>
00450   let x=pos(srep$(ln$,'^','~'),'~',1) ! pos(ln$,"^",1)
00460   if x=0 then goto L410 ! skip headings
00470   let desc$=ln$(1:x-1)(1:35)
00480   let item+=1
00490   let y=pos(srep$(ln$,'^','~'),'~',x) ! pos(ln$,"^",x)
00500   let prg$(item)=ln$(x+1:len(ln$))
00510   let nam$(item)=ln$(1:x-1)(1:35)
00520   let item$(1)=str$(item) !:
        let item$(2)=desc$ !:
        let fnflexadd1(mat item$)
00530   goto L410
00540 L540: close #1: ioerr ignore
00550   if fnclient_has('G2') then open #1: "Name=S:\General Ledger\Accountants\Menu.mnu",display,input else goto L700
00560 L560: linput #1: ln$ eof L690
00570   if ln$(1:1)<>">" then goto L560 ! skip headings
00580   if ln$(1:1)=">" then let ln$(1:1)=""
00590   if ln$(1:1)=">" then let ln$(1:1)="" ! delete up to two >>
00600   let x=pos(srep$(ln$,'^','~'),'~',1) ! pos(ln$,"^",1)
00610   if x=0 then goto L560 ! skip headings
00620   let desc$=ln$(1:x-1)(1:35)
00630   let item+=1
00640   let y=pos(srep$(ln$,'^','~'),'~',x) ! pos(ln$,"^",x)
00650   let prg$(item)=ln$(x+1:len(ln$))
00660   let nam$(item)=ln$(1:x-1)(1:35)
00670   let item$(1)=str$(item) !:
        let item$(2)=desc$ !:
        let fnflexadd1(mat item$)
00680   goto L560
00690 L690: let fnlbl(22,1," ")
00700 L700: let fncmdkey("&Next",1,1,0,"Selects the highlited option for automatic processing.")
00710   let fncmdkey("&Save",2,0,0,"Saves the selections and returns to menu.")
00720   let fncmdkey("&Delete All",4,0,0,"Deletes all selections.")
00730   let fncmdkey("&Cancel",5,0,1,"Returns to main menu without saving the selections.")
00740   let fnacs(sn$,0,mat resp$,ckey)
00750   if ckey=5 then goto XIT
00760   if ckey=2 then goto L860
00770   if ckey=4 then mat nxtdesc$=(""): !:
          mat nxtpgm$=("") !:
          mat prim=(0) !:
          execute "drop "&env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno) !:
          goto MAIN
00780   let sel=val(resp$(41))
00790   for j=1 to 20
00800     let prim(j)=val(resp$(j*2))
00810   next j
00820   for j=1 to 20
00830     if trim$(nxtdesc$(j))="" then let nxtdesc$(j)=nam$(sel): let nxtpgm$(j)=prg$(sel): goto L850
00840   next j
00850 L850: if ckey=1 then goto L280
00860 L860: goto L1640
00870   data "  W-2 Special Information File","S:\acsGL\W2Box16","W2Box16","E"
00880   data "  Budget","S:\acsGL\BGMaint2","BGMaint2","E"
00890   data "  Enter Period Ending Dates","S:\General Ledger\Period Ending Dates","EntDat","E"
00900   data "  Enter Transactions","S:\General Ledger\Enter Transactions","GLInput","E"
00910   data "  Post Transactions","S:\acsGL\GLJCMRGE","GLJCMRGE","E"
00920   data "  Reverse Posted Transactions","S:\acsGL\UNPost","","E"
00930   data "  Enter Standard Adjustments","S:\General Ledger\Enter Standard Adjustments","BldStdAj","E"
00940   data "  Post Standard Adjustments","S:\General Ledger\Post Standard Adjustments","StdAdj","E"
00950   data "  pr Bank Reconciliation","S:\acsGL\GLCkRec","GLCkRec","E"
00960   data "  pr Trial Balance Worksheet","S:\acsGL\ACGLWKSH","acglwksh","E"
00970   data "  pr Budget Worksheet","S:\acsGL\BudWkSh","BudWkSh","E"
00980   data "  pr Transactions Journal","S:\acsGL\AcGLTrJr","acgltrjr","ES24"
00990   data "  pr Trial Balance","S:\General Ledger\Trial Balance","acgltb","ES25"
01000 ! Financial Stmts.
01010   data "  Cover Letter","S:\acsGL\ACGLCOVL","acglcovl","E"
01020 ! Balance Sheets
01030   data "    Standard - B/S","S:\acsGL\ACGLBAL","acglbal","ES54"
01040   data "    Comparison - B/S","S:\General Ledger\Comparative Balance Sheet","acglbalc","ES55"
01050   data "    Period Comparison - B/S","S:\General Ledger\Period Comparison Balance Sheet","acglbaly","ES56"
01060   data "    Fund Comparison - B/S","S:\acsGL\ACGLBALF","","ES56"
01070 ! Income Statements
01080   data "    Standard - I/C","S:\General Ledger\Income Statement","acglinc","ES57"
01090   data "    Quarterly - I/C","S:\acsGL\ACGLINCQ","acglincq","ES59"
01100   data "    Percent - I/C","S:\acsGL\ACGLINCP","acglincp","ES58"
01110   data "    Comparison - I/C","S:\acsGL\ACGLINCC","acglincc","ES59"
01120   data "    Budget - I/C","S:\acsGL\ACGLINCB","acglincb","ES60"
01130   data "    Period Comparison - I/C","S:\acsGL\ACGLINCY","acglincy","ES61"
01140   data "    Fund Comparison - I/C","S:\acsGL\ACGLINCF","acglincf","ES89"
01150   data "    Variance - I/C","S:\acsGL\ACGLINCV","acglincv","ES84"
01160   data "    YTD Column Only - I/C","S:\acsGL\ACGLINC1","acglinc1","ES83"
01170   data "    4 Column Budget - I/C","S:\acsGL\ACGLINC4","acglinc4","ES86"
01180   data "    GASB Budgetary Schedule - I/C","S:\acsGL\ACGLINCo","acglinco","ES86"
01190   data "    Year Comparison -I/C","S:\acsGL\ACGLINYY","acglinyy","ES87"
01200 ! Stmt of Change in Financial Position
01210   data "    Standard (FP)","S:\acsGL\ACGLCHGR","acglchgr","ES62"
01220   data "    Change Amount (FP)","S:\acsGL\ACGLCHGS","acglchgs","ES63"
01230   data "    Comparative (FP)","S:\acsGL\acGLChg","acGLchg","ES64"
01240   data "    Comparative Change Amount (FP)","S:\acsGL\acGLChgC","acGLchgc","ES65"
01250 ! Cash Flow Statement
01260   data "    Standard (CF)","S:\acsGL\ACGLCASH","ACGLCash","ES81"
01270   data "    Budget (CF)","S:\acsGL\acGLCASB","acGLCasB","ES82"
01280   data "    Fund Comparison (CF)","S:\acsGL\acGLCasF","acGLCASF","ES88"
01290   data "    YTD Budget - Over Under (CF)","S:\acsGL\acGLCasO","acGLcaso","ES85"
01300   data "  Statement of Retained Earnings","S:\acsGL\acGLRest","acGLrest","E"
01310   data "  pr Schedules","S:\General Ledger\Print Schedules","GLSchPrt","E"
01320   data "  Calculate or pr Ratios","S:\acsGL\RatioFM","RatioFM","E"
01330   data "  Notes to Financial Statements","S:\acsGL\acGLNote","acGLNote","E"
01340 ! Monthly
01350   data "  pr Payroll Registers","S:\acsGL\ACPRReg","ACPRReg","E"
01360   data "  Post Payroll Checks","S:\acsGL\PRMerge","PRMerge","E"
01370   data "  Post Entries from Holding Files","S:\acsGL\acGLPost","acGLPost","E"
01390 ! Quarterly
01400   data "  pr State Unemployment Compensation Report","S:\acsGL\PRStatUC","prstatuc","E"
01410   data "  pr 941 Summary","S:\acsGL\PR941","PR941","E"
01420   data "  Zero QTD Payroll Info","S:\acsGL\PRZQTD","przqtd","E"
01430 ! Annually
01440   data "  pr Accumulated Trial Balance","S:\General Ledger\Print Accumulated Trial Balance","acGLAcTb","ES33"
01450   data "  Clear Accumulated Transactions","S:\acsGL\acGLClr","acGLClr","E"
01460   data "  Close Books at Year End","S:\General Ledger\Close Books at Year End","acGLClos","E"
01470   data "  Enter Budget Amounts","S:\acsGL\BudInpt","BudInpt","E"
01480   data "  Prior Period Adjustments","S:\acsGL\PriorPeriodAdj","PriorPeriodAdj","E"
01490   data "  pr W-2's","S:\acsGL\PRW2","PRW2","E"
01500   data "  Create Electronic W-2s","S:\acsGL\GLELECW2","GLElecW2","E"
01510   data "  pr 1099s","S:\acsGL\GLPRT109","GLPrt109","E"
01520   data "  Create Electronic 1099s","S:\acsGL\ELEC1099","Elec1099","E"
01530   data "  Zero YTD Payroll Information","S:\acsGL\PRZYTD","PRZYTD","E"
01540   data "  Zero YTD Vendor Purchases","S:\acsGL\GLZER109","GLZer109","E"
01550 ! Utilities
01560   data "  pr Chart of Accounts","S:\acsGL\acGLCHRT","acGLChrt","E"
01570   data "  Generate Reversing Entries","S:\acsGL\Reverse","","E"
01580   data "  Remove Entries Posted by Mistake","S:\acsGL\Unpost","","E"
01590   data "  Consolidate Master Files","S:\acsGL\COMBGL","CombGL","E"
01600   data "  Create ASCII Files","S:\acsGL\ASCIIGLM","ASCIIGLM","E"
01610   data "  Create Checkbook System Files","S:\acsGL\CLBLD","CLBld","E"
01620   read m$(i+=1),pgm$(i),hlpg$(i),status$(i)
01630   form pos 1,c 20,c 35,n 3,3*n 1
01640 L1640: execute "drop "&env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno)
01650   close #1: ioerr ignore
01660   open #1: "Name="&env$('Q')&"\GLmstr\acGLPGMN.h"&str$(cno)&",Use,RecL=76",internal,outin,relative 
01670   for j1=1 to 20 !:
          if j1>lrec(3) then write #1,using L1690: nxtpgm$(j1),nxtdesc$(j1),pn(j1),cp(j1),prim(j1),srq(j1)
01680   next j1
01690 L1690: form pos 1,c 35,c 35,n 3,3*n 1
01700   close #1: ioerr ignore
01710   close #3: ioerr XIT
01720 XIT: let fnxit
01730 ! ______________________________________________________________________
01740 ! <Updateable Region: ERTN>
01750 ERTN: let fnerror(program$,err,line,act$,"xit")
01760   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01770   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01780   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01790 ERTN_EXEC_ACT: execute act$ : goto ERTN
01800 ! /region
01810 ! ______________________________________________________________________
