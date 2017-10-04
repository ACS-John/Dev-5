00010 ! Replace S:\acsUB\RmBudget
00020 ! -- Remove Old Budget Transactions
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnxit,fnerror,fntos,fnacs,fntxt,fnlbl,fndate_mmddyy_to_ccyymmdd,fnwait,fncmdset,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim ba(13),bt1(14,2)
00080   dim tr(2),cap$*128,txt$*80,udf$*256
00090 ! ______________________________________________________________________
00100   fncno(cno)
00102   let udf$=env$('temp')&'\'
00110   fntop("S:\acsUB\RmBudget",cap$="Remove Old Budget Transactions")
00120 ! ______________________________________________________________________
00130   gosub BUD1
00140   if bud1=0 then goto XIT
00150 ! ______________________________________________________________________
00160   sn$="RmBudget" !:
        fntos(sn$)
00170   let txt$="All paid budget records with a date prior" !:
        fnlbl(1,1,txt$,44,2)
00180   let txt$="to this date will be removed." !:
        fnlbl(2,1,txt$,44,2)
00190   let txt$="Oldest Date to Retain (MMDDYY):" !:
        fnlbl(4,1,txt$,33,1)
00200   fntxt(4,35,8,0,0,"1") !:
        let resp$(1)=""
00210   fncmdset(2)
00220   fnacs(sn$,0,mat resp$,ckey)
00230   let rd1=val(resp$(1))
00240   if ckey=5 then goto XIT
00250   open #2: "Name="&udf$&"Work1.dat,Size=0,RecL=149,Replace",internal,outin,relative 
00260 L260: read #81,using L270: z$,mat ba,mat tr eof END1
00270 L270: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
00280   adr=tr(1)
00290   mat tr=(0)
00300 L300: if adr=0 then goto L450
00310   read #82,using L370,rec=adr,release: z$,mat bt1,nba norec L450
00320   let d1=bt1(1,1) ! transaction date
00330   let d2=rd1 ! cutoff date
00340   if sum(bt1)=0 then goto L440
00350   if bt1(14,1)=0 then goto L370
00360   let d1=fndate_mmddyy_to_ccyymmdd(d1) !:
        let d2=fndate_mmddyy_to_ccyymmdd(d2) !:
        if d1<d2 then goto L440
00370 L370: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
00380   lr2=lrec(2)+1
00390   write #2,using L370,rec=lr2: z$,mat bt1,0
00400   if tr(2)>0 then rewrite #2,using L410,rec=tr(2): lr2
00410 L410: form pos 147,pd 3
00420   if tr(1)=0 then let tr(1)=lr2
00430   let tr(2)=lr2
00440 L440: adr=nba: goto L300
00450 L450: rewrite #81,using L460: mat tr
00460 L460: form pos 75,2*pd 3
00470   goto L260
00480 ! ______________________________________________________________________
00490 END1: close #2: 
00500   close #81: 
00510   close #82: 
00520   execute 'Copy "'&udf$&'Work1.dat" "'&env$('Q')&"\UBmstr\BudTrans.h"&str$(cno)&'" -n'
00530   execute "Index "&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&' '&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&" 1 10 Replace DupKeys -n"
00540   execute "Free "&udf$&"Work1.dat -n"
00550   goto XIT
00560 ! ______________________________________________________________________
00570 XIT: let fnxit
00580 ! ______________________________________________________________________
00590 BUD1: bud1=0
00600   open #81: "Name="&env$('Q')&"\UBmstr\BudMstr.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\BudIdx1.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L630
00610   open #82: "Name="&env$('Q')&"\UBmstr\BudTrans.h"&str$(cno)&",Shr",internal,outin,relative 
00620   bud1=1
00630 L630: return 
00640 ! ______________________________________________________________________
00650 ! <Updateable Region: ERTN>
00660 ERTN: let fnerror(program$,err,line,act$,"xit")
00670   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00680   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00690   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00700 ERTN_EXEC_ACT: execute act$ : goto ERTN
00710 ! /region
