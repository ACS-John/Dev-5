00010 ! Replace S:\acsCL\DptMstr
00020 ! ??? department
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fndat
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim hd$(2)*60,cnam$*40,dat$*20,ots$(5),ins$(22),ink$(22)*42
00080   dim pdate$(2)*20,de$*50,sm1$(6)*37,sf1$(6),d(2)
00090   dim d$*30,key$*9,nkey$*9,flo$(4),fli$(3),scr$(3)
00100 ! ______________________________________________________________________
00110   let fncno(cno,cnam$)
00120   let fndat(dat$)
00130 ! 
00140   let hp1=26-int(len(ltrm$(rtrm$(cnam$)))/2) !:
        let hp2=26-int(len(ltrm$(rtrm$(dat$)))/2)
00150   let flo$(1)="8,07,C 64,R,N" : let flo$(2)="10,17,C 20" !:
        let flo$(3)="10,32,C 20" : let flo$(4)="11,08,C 20"
00160   let fli$(1)="10,25,N 3,UT,N"
00170   let fli$(2)="10,40,N 2,UE,N"
00180   let fli$(3)="11,25,C 30,UT,N"
00190   let scr$(1)="Fund #:"
00200   let scr$(2)="Dept #:"
00210   let scr$(3)="Department Name:"
00220 ! 
00230   for j=1 to 22 : let ins$(j)=str$(j+1)&",2,C 42,H,N" : next j
00240 L240: open #1: "Name="&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\DPTIDX1.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L1800
00250   let hd$(1)="     **ENTER FUND & DEPT # AS 0 OR BLANK TO STOP**"
00260   let hd$(2)="     **ENTER FUND & DEPT  # AS 0 OR BLANK TO DELETE**"
00270   for j=1 to 6
00280     let sf1$(j)=str$(j+5)&",11,C 37"
00290   next j
00300   let sf1$(3)="8,11,C 37,C,N"
00310   cap$="Department"
00320 MENU1: pr newpage
00330   if process=1 then let ti=4 : goto L450
00340   cnam$=rtrm$(cnam$)
00350   close #101: ioerr L360
00360 L360: open #101: "SROW=5,SCOL=10,EROW=12,ECOL=48,BORDER=DR,CAPTION=Department File",display,outin 
00370   pr fields "2,9,Cc 41,R,N": cnam$
00380   pr fields "3,9,Cc 41,R,N": "Company Number "&str$(cno)
00390   let sm1$(1)="1.  Initial File Preparation" !:
        let sm1$(2)="2.  Add New Records" !:
        let sm1$(3)="3.  Modify or Inquire Records" !:
        let sm1$(4)="4.  pr Proof List" !:
        let sm1$(5)="5.  Search" !:
        let sm1$(6)="6.  Reorganize"
00400   pr fields "13,25,C 09,B,5": "Exit (F5)"
00410 L410: rinput select mat sf1$,attr "H": mat sm1$
00420   if cmdkey=5 then goto L1270
00430   let ti0=ti=curfld
00440   mat ta=(0)
00450 L450: on ti goto L460,L790,L800,L1530,L1810,L2070 none L410
00460 L460: pr newpage
00470   close #101: ioerr L480
00480 L480: open #101: "SROW=5,SCOL=13,EROW=16,ECOL=64,BORDER=DR,CAPTION=INITIAL FILE PREPARATION",display,outin 
00490   pr fields "5,18,Cc 41,R,N": cnam$
00500   pr fields "6,18,C 41,R,N": "            COMPANY NUMBER "&str$(cno)
00510   pr fields "8,13,C 52,R,N": " ******************   WARNING   ******************"
00520   pr fields "10,13,C 52,H,N": "  THIS SELECTION WILL DESTROY ALL EXISTING RECORDS"
00530   pr fields "11,13,C 52,H,N": "  ON THE DEPARTMENT NAME FILE."
00540   pr fields "17,28,C 16,R,N": "PRESS F5 TO STOP"
00550   pr fields "13,15,C 47,R,N": " Enter ERASE to continue"
00560 L560: input fields "15,57,CU 5,UE,N",attr "R": pas$
00570   if cmdkey=5 then goto MENU1
00580   if pas$><"COPY " then goto L620
00590   close #1: ioerr L600
00600 L600: execute "COPY A:DPTMSTR.H"&str$(cno)&","&env$('Q')&"\CLmstr\*.*"
00610   goto L770
00620 L620: if ltrm$(rtrm$(pas$))><"BUILD" then goto L730
00630   close #1: ioerr ignore
00640   open #2: "Name=DPTMSTR.h"&str$(cno)&"/DPTMSTR,KFName=DPTIDX1.h"&str$(cno)&"/DPTMSTR,Shr",internal,input,keyed 
00650   open #1: "Name="&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&",SIZE=0,RecL=35,Replace",internal,output 
00660 L660: read #2,using L670: gl$,de$ eof END1
00670 L670: form pos 1,c 12,c 50
00680   write #1,using L670: gl$,de$
00690   goto L660
00700 END1: close #1: 
00710   close #2: 
00720   goto L770
00730 L730: if pas$><"ERASE" then goto L560
00740 L740: close #1: ioerr ignore
00750   open #1: "Name="&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&",SIZE=0,RecL=35,Replace",internal,output 
00760   close #1: 
00770 L770: execute "Index "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&' '&env$('Q')&"\CLmstr\DPTIDX1.h"&str$(cno)&" 1 5 Replace DupKeys"
00780   goto L240
00790 L790: let new1=1
00800 L800: pr newpage
00810   pr fields mat flo$: hd$(1),mat scr$
00820   pr fields "13,20,C 25,R,N": " F1=CONTINUE, F5=STOP"
00830 L830: input fields mat fli$,attr "R": hfun,hdpt conv L830
00840   if ce>0 then let fli$(ce)(ce1:ce2)="U": ce=0
00850   if cmdkey>0 or curfld=2 then goto L920 else ce=curfld
00860 L860: ce=ce+1: if ce>udim(fli$) then ce=1
00870 L870: let fli$(ce)=rtrm$(uprc$(fli$(ce))) : ce1=pos(fli$(ce),"U",1) : if ce1=0 then goto L860
00880   ce2=ce1+1 : let fli$(ce)(ce1:ce1)="UC" : goto L830
00890 CONV0: if ce>0 then let fli$(ce)(ce1:ce2)="U"
00900   ce=cnt+1
00910 ERR0: pr fields "24,78,C 1": bell : goto L870
00920 L920: ! 
00930   if hfun<0 or hdpt<0 then goto L830
00940   if hfun+hdpt=0 then goto MENU1
00950   let key$=cnvrt$("N 3",hfun)&cnvrt$("N 2",hdpt)
00960   read #1,using L1250,key=key$: fun,dpt,d$ nokey L980
00970   let ti=3: goto L990
00980 L980: if ti=2 then goto L1050 else goto L830
00990 L990: pr newpage
01000   close #101: ioerr L1010
01010 L1010: open #101: "SROW=7,SCOL=07,EROW=12,ECOL=70,BORDER=DR,CAPTION=GL MASTER FILE",display,outin 
01020   pr fields mat flo$: hd$(1),mat scr$
01030   pr fields flo$(1): hd$(2)
01040   pr fields mat fli$: fun,dpt,d$
01050 L1050: if ti><3 then pr fields "2,20,C 25,N": ""
01060   pr fields "13,20,C 25,R,N": " F1=CONTINUE, F5=STOP"
01070   ce=3: goto L1120
01080 L1080: input fields mat fli$,attr "R": fun,dpt,d$ conv CONV1
01090   if ce>0 then let fli$(ce)(ce1:ce2)="U": ce=0
01100   if cmdkey>0 or curfld=3 then goto L1170 else ce=curfld+1
01110 L1110: if ce>udim(fli$) then ce=1
01120 L1120: let fli$(ce)=rtrm$(uprc$(fli$(ce))) : ce1=pos(fli$(ce),"U",1)
01130   if ce1=0 then ce=ce+1 : goto L1110 else ce2=ce1+1 : let fli$(ce)(ce1:ce1)="UC" : goto L1080
01140 CONV1: if ce>0 then let fli$(ce)(ce1:ce2)="U"
01150   ce=cnt+1
01160 ERR1: pr fields "24,78,C 1": bell : goto L1120
01170 L1170: if cmdkey=5 then goto MENU1
01180 ! IF TI=2 AND DPT=0 THEN GOTO 470
01190   if fun<0 then convc=1: goto ERR1
01200   if dpt<0 then convc=2: goto ERR1
01210   let nkey$=cnvrt$("N 3",fun)&cnvrt$("N 2",dpt)
01220   if ti=2 then goto L1450
01230   if nkey$><key$ then goto L1310
01240 L1240: rewrite #1,using L1250,key=nkey$: fun,dpt,d$
01250 L1250: form pos 1,n 3,n 2,c 30
01260   let ti=ti0: goto L450
01270 L1270: close #1: 
01280   if new1=1 then goto L1760
01290   goto XIT
01300 IGNORE: continue 
01310 L1310: pr newpage
01320   if dpt><0 then goto L1350
01330   pr fields "8,10,c 60,n": "YOU CHOSE TO DELETE ACCOUNT # "&str$(hfun)&" "&str$(hdpt)
01340   goto L1390
01350 L1350: read #1,using L1360,key=nkey$: fun nokey L1380
01360 L1360: form pos 1,n 3
01370   goto L800
01380 L1380: pr fields "8,4,c 76,n": "YOU CHOSE TO CHANGE ACCOUNT # "&str$(hfun)&" "&str$(hdpt)&" TO "&str$(fun)&" "&str$(dpt)
01390 L1390: pr fields "12,10,c 35,n": "ENTER 1 TO CONTINUE; ELSE ENTER 0"
01400 L1400: input fields "12,50,n 1,ue,n": j conv L1400
01410   if j<0 or j>1 then goto L1400
01420   if j=0 then goto L450
01430   delete #1,key=key$: 
01440   if dpt=0 then goto L1510
01450 L1450: if bb=cb then goto L1480
01460   if bb=0 then bb=cb
01470   if cb=0 then cb=bb
01480 L1480: read #1,using L1250,key=nkey$: fun nokey L1500
01490   goto L1240
01500 L1500: write #1,using L1250: fun,dpt,d$
01510 L1510: let new1=1
01520   goto L450
01530 L1530: restore #1,search>="": eof L1540
01540 L1540: pr newpage
01550   close #101: ioerr ignore
01560   open #101: "SROW=08,SCOL=18,EROW=12,ECOL=58,BORDER=DR,CAPTION=GL PROOF LIST",display,outin 
01570   pr fields "08,18,Cc 41,H,N": cnam$
01580   pr fields "09,18,C 41,H,N": "            COMPANY NUMBER "&str$(cno)
01590   pr fields "11,18,C 41,R,N": "              IN PROCESS"
01600   pr fields "13,30,C 16,R,N": "PRESS F5 TO STOP"
01610   on fkey 5 goto L1720
01620   let fnopenprn(cp,58,220,process)
01630   gosub L1690
01640 L1640: read #1,using L1250: fun,dpt,d$ eof L1720
01650   pr #255,using L1660: fun,dpt,d$ pageoflow L1680
01660 L1660: form pos 1,n 3,n 7,x 2,c 30,skip 1
01670   goto L1640
01680 L1680: pr #255: newpage : gosub L1690 : continue 
01690 L1690: pr #255,using L1700: date$('mm/dd/yy'),time$,cnam$,"Department File Listing",dat$
01700 L1700: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos hp1,c 40,skip 1,pos 18,c 50,skip 1,pos hp2,c 20,skip 2
01710   return 
01720 L1720: let fncloseprn
01730   on fkey 5 ignore 
01740   if process=1 then goto XIT else goto MENU1
01750   goto XIT
01760 L1760: close #1: ioerr L1770
01770 L1770: close #2: ioerr L1780
01780 L1780: execute "Index "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&' '&env$('Q')&"\CLmstr\DPTIDX1.h"&str$(cno)&" 1 5 Replace DupKeys -n"
01790   goto XIT
01800 L1800: if err=4152 then goto L740
01810 L1810: restore #1,search>="": nokey MENU1
01820 L1820: let ln=eof1=0
01830   pr newpage
01840   pr fields "1,2,C 4,R,N": "FUND"
01850   pr fields "1,7,C 4,R,N": "DEPT"
01860   pr fields "1,13,C 30,R,N": "DESCRIPTION"
01870 L1870: read #1,using L1250: fun,dpt,d$ eof L1950
01880   let ots$(1)=str$(ln+2)&",2,N 3,H,N"
01890   let ots$(2)=str$(ln+2)&",8,N 2,H,N"
01900   let ots$(3)=str$(ln+2)&",13,C 30,H,N"
01910   pr fields mat ots$: fun,dpt,d$
01920   let ln=ln+1
01930   if ln>21 then goto L1970
01940   goto L1870
01950 L1950: let eof1=1
01960   if ln=0 then goto MENU1
01970 L1970: pr fields "24,2,C 78,R,N": "HIGHLIGHT ACCOUNT # & PRESS ENTER TO SELECT; PRESS F1 TO CONTINUE; F5 TO STOP"
01980   mat ink$(ln)
01990 L1990: input fields mat ins$,attr "R": mat ink$
02000   if cmdkey>0 then goto L2040
02010   let key$=ink$(curfld)(1:3)&ink$(curfld)(7:8)
02020   read #1,using L1250,key=key$: fun,dpt,d$ nokey L1990
02030   let ti=3: goto L990
02040 L2040: if cmdkey=5 or eof1=1 then goto MENU1
02050   goto L1820
02060 ! ______________________________________________________________________
02070 L2070: close #1: ioerr L2080
02080 L2080: execute "Copy "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&" "&env$('Temp')&"\WORK -D -n"
02090   execute "Free "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&" -n"
02100   execute "RENAME "&env$('Temp')&"\WORK "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&" -n"
02110   execute "Index "&env$('Q')&"\CLmstr\DPTMSTR.h"&str$(cno)&' '&env$('Q')&"\CLmstr\DPTIDX1.h"&str$(cno)&" 1 5 Replace DupKeys -n"
02120   goto L240
02130 ! ______________________________________________________________________
02140 ! <Updateable Region: ERTN>
02150 ERTN: let fnerror(program$,err,line,act$,"xit")
02160   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
02170   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02180   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02190 ERTN_EXEC_ACT: execute act$ : goto ERTN
02200 ! /region
02210 ! ______________________________________________________________________
02220 XIT: let fnxit
02230 ! ______________________________________________________________________
