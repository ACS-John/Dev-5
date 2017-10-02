00010 ! Replace S:\acsPR\jcSCMint
00020 ! Sub-Category Description File
00025   chain "S:\acsPR\newjcscmint"
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim iom$(4),scm$(4)*27
00080   dim cde$*3,des$*30,cnam$*40,sc$*3,cnt$*25,cap$*128,message$*40
00090 ! ______________________________________________________________________
00100   let fntop("S:\acsPR\jcSCMaint",cap$="Sub-Category Description")
00110   let fncno(cno,cnam$)
00115   let fnconsole(1)
00120 ! 
00130   open #1: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\SCIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L1220
00140 MENU1: ! 
00150   pr newpage
00160   let fnopenwin(win=101,08,19,15,60,cap$)
00170   let scm$(1)="1. Initial File Preparation"
00180   let scm$(2)="2. Add or Edit"
00190   let scm$(3)="3. pr Proof List"
00200   let scm$(4)="4. Search"
00210   for j=1 to 4: let iom$(j)=str$(j+3)&",2,C 38,N": next j !:
        let iom$(2)="5,2,C 38,C,N"
00220   pr fields "16,34,C 09,B,5": "Exit (F5)"
00230 L230: rinput #win,select mat iom$,attr "H": mat scm$
00240   alp=0
00250   let ti=curfld
00260   if cmdkey=5 then goto L470
00270   if ti=0 then goto L470
00280   on ti goto L300,L600,L950,SRCH3 none L230
00290 ! ______________________________________________________________________
00300 L300: pr newpage
00310   let win=102
00320   let fnopenwin(win,08,20,16,59,cap$)
00330   pr #win,fields "4,1,Cc 40,R,N": "* * * Warning * * *"
00340   pr #win,fields "5,1,Cc 40,N": "This selection will erase all records"
00350   pr #win,fields "6,1,Cc 40,N": "in the Sub-Category Description File."
00360   pr #win,fields "8,2,C 24,N": "Enter ERASE to continue:"
00370   pr fields "17,34,C 11,B,5": "Cancel (F5)"
00380 L380: input #win,fields "8,27,Cu 5,UT,N": cont$ conv L380
00390   if cont$="ERASE" then let new1=1 else let new1=2
00400   if new1=2 then goto MENU1
00410   if new1<>1 then goto L380
00420   close #1: ioerr L430
00430 L430: open #1: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno),internal,output ioerr ERTN2
00440   close #1,free: ioerr L450
00450 L450: open #1: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&",SIZE=0,RecL=33",internal,output 
00460   cont=new1=1
00470 L470: close #1: 
00480   if new1=0 then goto XIT
00490   open #1: "Name=PROC."&wsid$,display,output ioerr L520
00500   restore #1: 
00510   goto L530
00520 L520: open #1: "Name=PROC."&wsid$&",SIZE=0",display,output 
00530 L530: pr #1: "CLEAR"
00540   pr #1: "PROCERR RETURN"
00550   pr #1: "Index "&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&","&env$('Q')&"\PRmstr\SCIndex.h"&str$(cno)&",1,3,Replace,DupKeys"
00560   if cont=1 then !:
          pr #1: "CHAIN 'S:\acsPR\JCSCMINT'" else pr #1: "Chain 'menu'"
00570   close #1: 
00580   chain "PROC=PROC."&wsid$
00590 ! ______________________________________________________________________
00600 L600: if ti=4 then goto SRCH3
00610 L610: pr newpage
00620   let win=102
00630   let fnopenwin(win,10,20,14,59,cap$)
00640   pr #win,fields "4,2,C 17,N": "Description Code:"
00650   pr fields "15,34,C 11,B,5": "Cancel (F5)"
00660   if ti=4 then cde$=lpad$(rtrm$(k$),3) : goto L720
00670 L670: input #win,fields "4,20,Nz 2,UT,N": cde conv L670
00680   if cmdkey=5 or cde=0 then goto MENU1
00690   cde$=uprc$(lpad$(rtrm$(str$(cde)),3))
00700   let des$=""
00710   let da=0
00720 L720: close #win: ioerr L740
00730   pr newpage
00740 L740: let win=103
00750   let fnopenwin(win,08,18,14,62,cap$)
00760   read #1,using L770,key=cde$: cde$,des$ nokey L800
00770 L770: form pos 1,c 3,c 30
00780   pr #win,fields "3,1,Cc 40,R,N": "Modifying"
00790   goto L810
00800 L800: pr #win,fields "3,1,Cc 40,R,N": "Adding"
00810 L810: pr fields "15,22,C 12,B,1": "Save (Enter)"
00820   pr fields "15,35,C 11,B,4": "Delete (F4)"
00830   pr fields "15,47,C 11,B,5": "Cancel (F5)"
00840   pr #win,fields "5,2,Cr 12,N": "Code:"
00850   pr #win,fields "6,2,Cr 12,N": "Description:"
00860   pr #win,fields "5,15,C 3,N": ltrm$(cde$)
00870 L870: rinput #win,fields "6,15,C 30,UT,N": des$ conv L870
00880   if cmdkey=5 then goto MENU1
00890   if cmdkey=4 then delete #1,key=cde$: nokey L600: let new1=1 : goto L600
00900   rewrite #1,using L770,key=cde$: cde$,des$ nokey L920
00910   if ti=4 then goto SRCH3 else goto L600
00920 L920: write #1,using L770: cde$,des$: let new1=1
00930   goto L600
00940 ! ______________________________________________________________________
00950 L950: pr newpage
00960   let fnwait(102,cap$,message$="Printing:  please wait...",1)
00970   on fkey 5 goto L1090
00980   let fnopenprn(cp,58,220,process)
00990   restore #1,key>="   ": nokey MENU1
01000   gosub L1130
01010 L1010: read #1,using L770: cde$,des$ eof L1090
01020   pr #255,using L1030: cde$,des$ pageoflow L1050
01030 L1030: form pos 16,c 5,c 30,skip 1
01040   goto L1010
01050 L1050: pr #255: newpage
01060   gosub L1130
01070   continue 
01080 ! ______________________________________________________________________
01090 L1090: on fkey 5 ignore 
01100   let fncloseprn
01110   goto MENU1
01120 ! ______________________________________________________________________
01130 L1130: pr #255,using L1140: date$,cnam$
01140 L1140: form pos 1,c 8,cc 52
01150   pr #255,using L1160: time$,"Sub-Category File Proof Listing"
01160 L1160: form pos 1,c 8,pos 11,cc 50,skip 2
01170   pr #255: tab(15);"Code  Description"
01180   pr #255: tab(15);"____  ______________________________"
01190   return 
01200 ! ______________________________________________________________________
01210 ERTN2: ! 
01220 L1220: if err=4152 then goto L450 else goto ERTN
01230 ! ______________________________________________________________________
01240 XIT: let fnxit
01250 ! ______________________________________________________________________
01260 SRCHEND: ! 
01270   close #win: ioerr L1280
01280 L1280: goto MENU1
01290 ! ______________________________________________________________________
01300 SRCH3: ! 
01310   bk=0
01320   let win=102
01330   let fnopenwin(win,02,47,23,79,cap$)
01340 ! LET JN$=LPAD$(RTRM$(JCI$(HCE-2)),6)
01350 ! READ #41,USING 12500,KEY=JN$: JN$,N$,MAT A$,MAT B NOKEY (list 'asdf')
01360   restore #1,search>="": nokey L1370
01370 L1370: pr #win: newpage
01380   pr fields "2,47,C 33,R,N": " "&n$(1:31)
01390   let ln=0
01400 L1400: read #1,using L1410: cde$,des$ eof L1460
01410 L1410: form pos 1,c 3,c 25
01420   let ln=ln+1
01430   pr fields str$(ln+2)&",49,C 3,N": cde$
01440   pr fields str$(ln+2)&",54,C 25,N": des$
01450   if ln<20 then goto L1400
01460 L1460: pr fields "23,47,C 33,R,N": " Press Enter to continue" ! asdf
01470   pr fields "24,47,C 33,R,N": " or enter category Number:"
01480   input fields "24,74,C 2,UT,N": k$
01490   if rtrm$(k$)><"" then close #win: : goto L610
01500   if ln<20 then goto SRCHEND
01510   goto L1370
01520 ! ______________________________________________________________________
01530 ! <Updateable Region: ERTN>
01540 ERTN: let fnerror(program$,err,line,act$,"xit")
01550   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01560   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01570   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01580 ERTN_EXEC_ACT: execute act$ : goto ERTN
01590 ! /region
01600 ! ______________________________________________________________________
