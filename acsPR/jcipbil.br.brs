00010 ! Replace S:\acsPR\jcIpBil
00020 ! Enter Billings
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim msgline$(2)*60,response$(5)*1,wrd2$(4)*38,wrds$(2)*20,n$*40
00080   dim io1$(4),jn$*6,inp(3),n$*40,a$(3)*30,b(4),cap$*128,message$*40
00090 ! ______________________________________________________________________
00100   let fntop("S:\acsPR\jcIpBil",cap$="Enter Billings")
00110   let fncno(cno)
00120 ! 
00130   c1=3
00135   let fnconsole(1)
00140 ! ______________________________________________________________________
00150   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
00160   open #2: "Name="&env$('temp')&"\Work."&session$,internal,output ioerr L180
00170   close #2,free: 
00180 L180: open #2: "Name="&env$('temp')&"\Work."&session$&",SIZE=0,RecL=17,Replace",internal,outin,relative 
00190 L190: let shoption=1
00200 L200: pr newpage
00210   let win=101
00220   let fnopenwin(win,09,08,16,71,cap$)
00230   pr #win,fields "4,2,Cr 14,N": "Job Number:"
00240   pr #win,fields "5,2,Cr 14,N": "Amount:"
00250   pr #win,fields "6,2,Cr 14,N": "Date (mmddyy):"
00260   pr #win,fields "7,2,Cr 14,N": "Status:"
00270   let io1$(1)="4,17,C 6,UT,N"
00280   let io1$(2)="5,17,N 10.2,UT,N"
00290   let io1$(3)="6,17,Nz 6,UT,N"
00300   let io1$(4)="7,17,N 2,UT,N"
00310   if c1=2 then pr #win,fields mat io1$: jn$,mat inp
00320   if shoption=1 then pr fields "17,24,C 09,B,1": "Next (F1)"
00330   if shoption=1 then pr fields "17,34,C 09,B,5": "Done (F5)"
00340   if shoption=1 then pr fields "17,44,C 11,B,6": "Search (F6)"
00350   if shoption=2 then pr fields "17,17,C 09,B,1": "Next (F1)"
00360   if shoption=2 then pr fields "17,27,C 11,B,4": "Delete (F4)"
00370   if shoption=2 then pr fields "17,39,C 11,B,5": "Cancel (F5)"
00380   if shoption=2 then pr fields "17,51,C 11,B,6": "Search (F6)"
00390 L390: input #win,fields mat io1$: jn$,mat inp conv L390
00400   if ce>0 then let io1$(ce)(ce1:ce2)="U": ce=0
00410   if cmdkey>0 then goto L510 else ce=curfld
00420   if ce<>1 then goto L450
00430   let jn$=lpad$(rtrm$(jn$),6) !:
        read #1,using L440,key=jn$: n$ nokey L600 !:
        pr #win,fields "4,24,C 40,H,N": n$
00440 L440: form pos 7,c 40
00450 L450: ce=ce+1 : if ce>udim(io1$) then ce=1
00460 L460: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L450
00470   ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L390
00480 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00490   ce=cnt+1
00500 ERR1: pr fields "24,78,C 1": bell : goto L460
00510 L510: if cmdkey=5 then goto L730
00520   if cmdkey=6 then gosub SRCH : let win=101 !:
          pr #win,fields io1$(1): k$ : c1=3: goto L390
00530   if c1=2 and cmdkey=4 then goto L1150
00540   if inp(2)<10100 or inp(2)>123199 then ce=3: goto ERR1
00550   if inp(1)=0 then ce=2: goto ERR1
00560   let jn$=lpad$(rtrm$(jn$),6)
00570   read #1,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3 nokey L600
00580   goto L660
00590 ! ______________________________________________________________________
00600 L600: let msgline$(1)="Job Number not found."
00610   let msgline$(2)="Please reselect."
00620   let fnoldmsgbox(mat response$,cap$,mat msgline$,1)
00630   ce=1
00640   goto ERR1
00650 ! ______________________________________________________________________
00660 L660: let ta=ta+inp(1)
00670   if c1=2 then goto L1120
00680 L680: let rw=lrec(2)+1
00690   write #2,using L700,rec=rw: jn$,mat inp duprec L680
00700 L700: form pos 1,c 6,pd 5.2,pd 4,n 2
00710   goto L200
00720 ! ______________________________________________________________________
00730 L730: pr newpage
00740   let win=102
00750   let fnopenwin(win,07,20,15,59,cap$)
00760   pr #win,fields "9,1,C 40,R,N": " Total of Amounts Entered:"
00770   pr #win,fields "9,28,N 10.2,R,N": ta
00780   let wrd2$(1)="1. pr Billing Proof List"
00790   let wrd2$(2)="2. Corrections"
00800   let wrd2$(3)="3. Additional Entries"
00810   let wrd2$(4)="4. Post to Job Cost File"
00820   for j=1 to udim(wrd2$)
00830     let io2$(j)=str$(j+3)&",2,C 38,N"
00840   next j
00850   pr fields "16,27,C 25,B,5": "Exit without posting (F5)"
00860 L860: rinput #win,select mat io2$,attr "H": mat wrd2$
00870   c1=curfld
00880   if cmdkey=5 then goto XIT
00890   on c1 goto PROOFLIST,L1160,L190,L1300 none L860
00900 ! ______________________________________________________________________
00910 PROOFLIST: ! 
00920   pr newpage
00930   let message$="Printing Proof List..."
00940   let fnwait(103,cap$,message$,1)
00950   on fkey 5 goto L1080
00960   let fnopenprn(cp,58,220,process)
00970   pr #255,using L980: "Job Cost Input Billings Proof List"
00980 L980: form skip 2,pos 10,c 60,skip 1
00990   pr #255: tab(10);"Date:  ";date$;"   Time:  ";time$
01000   pr #255: 
01010   pr #255: "Ref #   Job #      Amount     Date     Status"
01020   for j=1 to lrec(2)
01030     read #2,using L700,rec=j: jn$,mat inp norec L1100
01040     if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L1070
01050     pr #255,using L1060: j,jn$,mat inp
01060 L1060: form pos 1,n 5,x 2,c 8,n 10.2,2*n 10,skip 1
01070 L1070: next j
01080 L1080: let fncloseprn
01090   on fkey 5 ignore 
01100 L1100: goto L730
01110 ! ______________________________________________________________________
01120 L1120: rewrite #2,using L700,rec=rr: jn$,mat inp
01130   goto L1160
01140 ! ______________________________________________________________________
01150 L1150: rewrite #2,using L700,rec=rr: "",0,0,0
01160 L1160: pr newpage
01170   let win=103
01180   let fnopenwin(win,10,20,14,59,cap$)
01190   pr #win,fields "4,2,C 28,N": "Reference Number to correct:"
01200   pr fields "15,35,C 09,B,5": "Done (F5)"
01210 L1210: input #win,fields "4,31,Nz 5,UT,N": rr conv L1210
01220   if cmdkey=5 then goto L730
01230   if rr=0 then goto L1210
01240   if rr<1 or rr>rw then goto L1210
01250   read #2,using L700,rec=rr: jn$,mat inp
01260   let ta=ta-inp(1)
01270   let shoption=2
01280   goto L200
01290 ! ______________________________________________________________________
01300 L1300: for j=1 to rw
01310     read #2,using L700,rec=j: jn$,mat inp norec XIT
01320     if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L1370
01330     read #1,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4 nokey L1370
01340     b3=b3+inp(1)
01350     if inp(3)><0 then b4=inp(3)
01360     rewrite #1,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4
01370 L1370: next j
01380   goto XIT
01390 ! ______________________________________________________________________
01400 XIT: let fnxit
01410 ! ______________________________________________________________________
01420 SRCH: ! 
01430   bk=0
01440   let hce=ce
01450   close #103: ioerr L1460
01460 L1460: open #103: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,output 
01470   pr #103: newpage
01480 L1480: pr newpage
01490   let win=102
01500   let fnopenwin(win,06,10,10,69,wrds$(1))
01510   let prtall=0
01520   pr #win,fields "4,2,C 38,N": "Beginning Search Data (blank for all):"
01530   pr fields "11,34,C 11,B,5": "Cancel (F5)"
01540 L1540: input #win,fields "4,41,C 6,UT,N": nam$
01550   if cmdkey=5 then goto SEARCHEND
01560   let nam$=lpad$(rtrm$(nam$),6)
01570   restore #1,search>=nam$: nokey L1540
01580 ! Close #WIN: Ioerr 1790
01590 L1590: pr newpage
01600   pr fields "1,02,Cc 06,R,N": "Job No"
01610   pr fields "1,09,Cc 36,R,N": "Job Name"
01620   pr fields "1,46,Cc 08,R,N": "Est.Date"
01630   pr fields "1,55,Cc 12,R,N": "Ct.Amount"
01640   pr fields "1,68,Cc 12,R,N": "Bld.Amount"
01650   cde=0
01660   for j=1 to 20
01670     read #1,using L1680: jn$,n$,mat a$,mat b eof SREND
01680 L1680: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2 ! pd 7.2 was pd 5.2
01690     cde=1
01700     pr fields str$(j+1)&",2,C 6,N": jn$
01710     pr fields str$(j+1)&",9,C 36,N": n$(1:36)
01720     pr fields str$(j+1)&",46,PIC(ZZ/ZZ/ZZ),N": b(1)
01730     pr fields str$(j+1)&",55,N 12.2,N": b(2)
01740     pr fields str$(j+1)&",68,N 12.2,N": b(3)
01750     if j>1 then goto L1790
01760     bk=bk+1
01770     if bk>20 then bk=1
01780     bk$(bk)=bl$(2)(1:28)
01790 L1790: next j
01800 SREND: if j>1 then let j=j-1
01810   mat in2$(j)
01820   pr fields "24,09,C 09,B,1": "Next (F1)"
01830   pr fields "24,19,C 09,B,2": "Back (F2)"
01840   pr fields "24,29,C 09,B,5": "Stop (F5)"
01850   pr fields "24,39,C 25,R,N": "or Select Account Number:"
01860   let k$=""
01870   rinput fields "24,65,C 6,UT,N": k$
01880   alp=0
01890   if cmdkey=5 then goto SEARCHEND
01900   if cmdkey=1 then let k$=""
01910   if rtrm$(k$)><"" then let jn$=k$ : goto SEARCHEND
01920   if cmdkey><2 then goto L1970
01930   bk=bk-1
01940   if bk<1 then goto L2000
01950   restore #1,key>=bk$(bk): nokey L2000
01960   bk=bk-1
01970 L1970: let selclp=1
01980   goto L1590
01990 ! ______________________________________________________________________
02000 L2000: let selclp=0
02010   goto L1480
02020 ! ______________________________________________________________________
02030 SEARCHEND: close #103: ioerr L2040
02040 L2040: return 
02050 ! ______________________________________________________________________
02060 ! <Updateable Region: ERTN>
02070 ERTN: let fnerror(program$,err,line,act$,"xit")
02080   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02090   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02100   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02110 ERTN_EXEC_ACT: execute act$ : goto ERTN
02120 ! /region
02130 ! ______________________________________________________________________
