00010 ! Replace S:\acsPR\JCInput
00020 ! Enter (Job Cost) Time
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox, fnopenprn,fncloseprn,fncno,fnerror,fnxit,fnchain,fntop,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
00080   dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,cnam$*40,io2b$(2)*20
00090   dim ji1(6),jn$*6,ji2(5),label2$(12)*26,iolabel2$(12),io2$(12),iosc$(20)
00100   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
00110   dim message$*40
00120 ! ______________________________________________________________________
00130   fntop("S:\acsPR\JCInput",cap$="Enter Time")
00140   fncno(cno,cnam$)
00150 ! 
00155   fnconsole(1)
00160 ! ___________________________
00170   for j=1 to 20 : let iosc$(j)=str$(j+1)&",10,C 40,UT,N" : next j
00180   let label2$(1)="Employee Number:"
00190   let label2$(2)="Method of Payment:"
00200   let label2$(3)="Date (mmddyy):"
00210   let label2$(4)="Payroll Department Number:"
00220   let label2$(5)="Regular Hours:"
00230   let label2$(6)="O.T. Hours:"
00240   let label2$(7)="Job Number:"
00250   let label2$(8)="Category:"
00260   let label2$(9)="Sub-Category Code:"
00270   let label2$(10)="Amount:"
00280   let label2$(11)="Deduction/Addition Code:"
00290   let label2$(12)="Units:"
00300 ! ___________________________
00310   let io2$(1)="6,28,G 8,UT,N"
00320   let io2$(2)="7,28,n 1,UT,N"
00330   let io2$(3)="8,28,n 6,UT,N"
00340   let io2$(4)="9,28,n 3,UT,N"
00350   let io2$(5)="10,28,n 8.2,UT,N"
00360   let io2$(6)="11,28,n 8.2,UT,N"
00370   let io2$(7)="12,28,c 6,UT,N"
00380   let io2$(8)="13,28,n 5,UT,N"
00390   let io2$(9)="14,28,n 2,UT,N"
00400   let io2$(10)="15,28,n 10.2,UT,N"
00410   let io2$(11)="16,28,n 2,UT,N"
00420   let io2$(12)="17,28,n 8.2,UT,N"
00430 ! ___________________________
00440   for j=1 to 12 : let iolabel2$(j)=str$(j+3)&",2,Cr 26,N" : next j
00450 ! ______________________________________________________________________
00460   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00470   open #5: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDX2.h"&str$(cno)&",Shr",internal,input,keyed 
00480   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,input,relative 
00490   open #3: "Name="&env$('temp')&"\Work."&session$&",SIZE=0,RecL=84,Replace",internal,outin,relative 
00500   open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00510   open #14: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCINDX2.H"&str$(cno)&",Shr",internal,input,keyed 
00520   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00530   open #13: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\SCIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00540 ! ______________________________________________________________________
00550 SCR1: ! 
00560   pr newpage
00570   fnopenwin(win=101,09,20,14,59,cap$)
00580   let wrd1$(1)="1. Regular input"
00590   let wrd1$(2)="2. Input from Diskette"
00600   let io1$(1)="4,2,C 38,N"
00610   let io1$(2)="5,2,C 38,N"
00620   pr fields "15,35,C 09,B,5": "Exit (F5)"
00630 L630: rinput #win,select mat io1$,attr "H": mat wrd1$
00640   if cmdkey=5 then goto XIT
00650   on curfld goto SCREENFORINPUT1,L3220 none L630
00660 ! ______________________________________________________________________
00670 SCREENFORINPUT1: ! 
00680   let shoption=1 ! john did this to remove some other stuff
00690 SCREENFORINPUT2: ! 
00700   pr newpage
00710   fnopenwin(win=102,5,4,20,75,cap$)
00720   let io2$(01)="04,29,G 8,UT,N"
00730   let io2$(02)="05,29,N 1,UT,N"
00740   let io2$(03)="06,29,N 6,UT,N"
00750   let io2$(04)="07,29,N 3,UT,N"
00760   let io2$(05)="08,29,N 8.2,UT,N"
00770   let io2$(06)="09,29,N 8.2,UT,N"
00780   let io2$(07)="10,29,C 6,UT,N"
00790   let io2$(08)="11,29,N 5,UT,N"
00800   let io2$(09)="12,29,N 2,UT,N"
00810   let io2$(10)="13,29,N 10.2,UT,N"
00820   let io2$(11)="14,29,N 2,UT,N"
00830   let io2$(12)="15,29,N 8.2,UT,N"
00840   pr #win,fields mat iolabel2$: mat label2$
00850   pr #win,fields "5,31,C 28,N": "(1.Salary, 2.Hourly, 3.Both)"
00860   pr #win,fields "14,32,C 40,N": "(0.Regular, 1-10.Deduct #, 11.Extra Pay)"
00870   if shoption=1 then pr fields "21,25,C 09,B,1": "Next (F1)"
00880   if shoption=1 then pr fields "21,35,C 09,B,5": "Stop (F5)"
00890   if shoption=1 then pr fields "21,45,C 09,B,6": "Help (F6)"
00900   if shoption=2 then pr fields "21,19,C 09,B,1": "Next (F1)"
00910   if shoption=2 then pr fields "21,29,C 11,B,2": "Delete (F2)"
00920   if shoption=2 then pr fields "21,41,C 09,B,5": "Stop (F5)"
00930   if shoption=2 then pr fields "21,51,C 09,B,6": "Help (F6)"
00940   if c1=2 or jcsrch=1 then pr #win,fields mat io2$: mat ji1,jn$,mat ji2
00950   let jcsrch=0
00960   if ji1(1)=0 then let io2$(4)="7,29,N 3,UT,N" : goto L1000
00970 L970: pr #win,fields mat io2$: ji1(1),ji1(2),ji1(3)
00980 ! LET io2$(4)="7,29,N 3,CUT,N"
00990   if ck1=6 and ji1(2)=2 then goto L1110
01000 L1000: input #win,fields mat io2$: en$,ji1(2),ji1(3),ji1(4),ji1(5),ji1(6),jn$,mat ji2 conv CONV1
01010   ck1=cmdkey
01020   let ji1(1)=val(en$) conv L1040
01030   goto L1070
01040 L1040: ck1=6
01050   ce4=1 : let nam$=uprc$(rtrm$(en$))
01060   goto L2870
01070 L1070: if ce>0 then let io2$(ce)(ce1:ce2)="U": ce=0
01080   ce4=curfld
01090   if ck1>0 then goto L1230 else ce=curfld
01100   if ce<>1 then goto L1130
01110 L1110: let en$=lpad$(str$(ji1(1)),8)
01120   read #1,using L1390,key=en$: empnam$,ta1 nokey ERR1 !:
        pr #win,fields "4,38,C 30,N": empnam$
01130 L1130: if ce<>7 then goto L1150
01140   read #11,using L1500,key=lpad$(rtrm$(jn$),6): n$ nokey ERR1 !:
        pr #win,fields "10,36,C 30,N": n$(1:30)
01150 L1150: if ce<>9 then goto L1170
01160   read #13,using L1590,key=lpad$(str$(ji2(2)),3): sub$ nokey ERR1 !:
        pr #win,fields "12,32,C 30,N": sub$
01170 L1170: ce=ce+1: if ce>udim(io2$) then ce=1
01180 L1180: let io2$(ce)=rtrm$(uprc$(io2$(ce))) !:
        ce1=pos(io2$(ce),"U",1) !:
        if ce1=0 then goto L1130
01190   ce2=ce1+1 !:
        let io2$(ce)(ce1:ce1)="UC" !:
        goto L1000
01200 CONV1: if ce>0 then let io2$(ce)(ce1:ce2)="U"
01210   ce=cnt+1
01220 ERR1: pr fields "24,78,C 1": bell : goto L1180
01230 L1230: if ck1=5 then goto WHATNOWSCREEN
01240   let io2$(8)="11,29,N 5,UT,N"
01250   if ck1=6 and ce4=1 then goto L2790
01260   if ck1=6 and ce4=7 then goto L3880
01270   if ck1=2 and c1=2 then goto L1760
01280   if ck1><1 then ce=curfld : goto L1180
01290   let ptp=0
01300   mat hr=(0)
01310   let er=0
01320   if ji1(2)<1 or ji1(2)>3 then ce=2: goto ERR1
01330   if ji1(3)<10100 or ji1(3)>123199 then ce=3: goto ERR1
01340   if ji1(1)=0 then goto L1460
01350   let en$=lpad$(str$(ji1(1)),8)
01360   ce=1
01370   read #1,using L1390,key=en$: empnam$,ta1 nokey ERR1
01380   ce=0
01390 L1390: form pos 9,c 30,pos 173,pd 3
01400 L1400: read #2,using L1410,rec=ta1: tdn,ptp,sal,mat hr,nta
01410 L1410: form pos 9,n 3,pos 30,n 6.3,pos 58,3*pd 4.2,pos 468,pd 3
01420   if tdn=ji1(4) then goto L1460
01430   if nta=0 then ce=4: goto ERR1
01440   let ta1=nta
01450   goto L1400
01460 L1460: if ji2(3)=0 then let ji2(3)=hr(1)*ji1(5)+hr(2)*ji1(6)
01470   if rtrm$(jn$)="" and ji2(1)=0 then goto L1560
01480   ce=7
01490   read #11,using L1500,key=lpad$(rtrm$(jn$),6): n$ nokey ERR1
01500 L1500: form pos 7,c 40
01510   ce=8
01520   cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(ji2(1)),5)
01530   read #12,using L1540,key=cn$: kc$ nokey ERR1
01540 L1540: form pos 1,c 11
01550   ce=0
01560 L1560: if ji2(2)=0 then goto L1610
01570   ce=9
01580   read #13,using L1590,key=lpad$(str$(ji2(2)),3): sub$ nokey ERR1
01590 L1590: form pos 4,c 30
01600   ce=0
01610 L1610: if ji2(3)=0 then ce=10: goto ERR1
01620   if ji2(4)<1 or ji2(4)>10 then let pt=ptp/100*ji2(3) else let pt=0
01630   if c1=2 then goto L1740
01640 L1640: let rw=lrec(3)+1
01650   write #3,using L1660,rec=rw: mat ji1, jn$, mat ji2, pt, empnam$, sal duprec L1640
01660 L1660: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
01670   for j=1 to 5
01680     let ji2(j)=0
01690   next j
01700   let ji1(5)=0
01710   let ji1(6)=0
01720   let jn$=""
01730   goto SCREENFORINPUT2
01740 L1740: rewrite #3,using L1660,rec=rr: mat ji1,jn$,mat ji2,pt,empnam$,sal
01750   goto L1790
01760 L1760: rewrite #3,using L1660,rec=rr: -1,0,0,0,0,0,"",0,0,0,0,0,0,"",0
01770   goto L1790
01780 L1780: let shoption=2 ! john did this to remove some other stuff
01790 L1790: pr newpage
01800   let win=104
01810   fnopenwin(win,10,20,14,59,cap$)
01820   pr #win,fields "04,2,C 28,N": "Reference Number to correct:"
01830   pr fields "15,35,C 09,B,5": "Done (F5)"
01840 L1840: input #win,fields "04,31,N 5,UT,N": rr conv L1840
01850   if cmdkey=5 then goto WHATNOWSCREEN
01860   if rr<1 or rr>rw then goto L1840
01870   close #win: ioerr L1880
01880 L1880: read #3,using L1660,rec=rr: mat ji1,jn$,mat ji2,pt,empnam$,sal
01890   goto SCREENFORINPUT2
01900 ! ______________________________________________________________________
01910 WHATNOWSCREEN: ! 
01920   pr newpage
01930   let win=103
01940   fnopenwin(win,8,20,15,59,cap$)
01950   let wrd3$(1)="1. pr Input Proof List"
01960   let wrd3$(2)="2. Corrections"
01970   let wrd3$(3)="3. Additional Entries"
01980   let wrd3$(4)="4. Post to Job Cost File"
01990   for j=1 to udim(wrd3$)
02000     let io3$(j)=str$(j+3)&",2,C 38,N"
02010   next j
02020   pr fields "16,27,C 25,B,5": "Exit Without Posting (F5)"
02030   rinput #win,select mat io3$, attr "H": mat wrd3$
02040   close #win: ioerr L970
02050   if cmdkey=5 then goto XIT
02060   c1=curfld
02070   on c1 goto PRINTPROOFLIST,L1780,SCREENFORINPUT1,POSTTOJOBS none WHATNOWSCREEN
02080 ! ______________________________________________________________________
02090 PRINTPROOFLIST: ! 
02100   pr newpage
02110   let message$="Printing Input Proof List..."
02120   fnwait(105,cap$,message$,1)
02130   on fkey 5 goto PROOF_LIST_DONE
02140   fnopenprn(cp,58,220,process)
02150   goto L2250
02160 ! ______________________________________________________________________
02170 PROOF_LIST_HDR: ! 
02180   pr #255,using L2210: cnam$
02190   pr #255,using L2210: "Job Cost Input Proof List"
02200   pr #255,using L2210: "Date: "&date$&"      Time: "&time$
02210 L2210: form pos 1,cc 113,skip 1
02220   pr #255: "Ref #   Emp #  Method-Pay  Date   Dept  Reg-Hrs   OT-Hrs  Job #   Category  Sub-Category   Amount  Ded-Add  Units"
02230   return 
02240 ! ______________________________________________________________________
02250 L2250: gosub PROOF_LIST_HDR
02260   for j=1 to lrec(3)
02270     read #3,using L1660,rec=j: mat ji1,jn$,mat ji2,pt
02280     if j=1 then goto L2320
02290     if ji1(1)=en then goto L2360
02300     pr #255,using L2310: " ________"," ________"," ____________",t5,t6,t10 pageoflow PROOF_LIST_NWPG
02310 L2310: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Total",pos 38,2*n 9.2,x 29,n 13.2,skip 2
02320 L2320: let en=ji1(1)
02330     let t5=0
02340     let t6=0
02350     let t10=0
02360 L2360: pr #255,using L2370: j,mat ji1,jn$,mat ji2 pageoflow PROOF_LIST_NWPG
02370 L2370: form pos 1,n 5,n 8,n 6,n 13,n 5,2*n 9.2,x 2,c 6,n 11,n 10,n 13.2,n 6,n 10.2,skip 1
02380     let t5=t5+ji1(5)
02390     let t6=t6+ji1(6)
02400     let t10=t10+ji2(3)
02410     let gt5=gt5+ji1(5)
02420     let gt6=gt6+ji1(6)
02430     let gt10=tg10+ji2(3)
02440   next j
02450   pr #255,using L2310: " ________"," ________"," ____________",t5,t6,t10
02460   pr #255,using L2470: " ________"," ________"," ____________",gt5,gt6,gt10
02470 L2470: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Grand Totals",pos 38,2*n 9.2,x 29,n 13.2,skip 2
02480 PROOF_LIST_DONE: ! 
02490   let gt5=gt6=gt10=0
02500   fncloseprn
02510   goto WHATNOWSCREEN
02520 ! ______________________________________________________________________
02530 POSTTOJOBS: ! 
02540   close #1: 
02550   close #2: 
02560   close #3: 
02570   close #11: 
02580   close #12: 
02590   close #13: 
02600   fnchain("S:\acsPR\JCMerge")
02610 ! ______________________________________________________________________
02620 PROOF_LIST_NWPG: ! 
02630   pr #255: newpage
02640   gosub PROOF_LIST_HDR
02650   continue 
02660 ! ______________________________________________________________________
02670 ! <Updateable Region: ERTN>
02680 ERTN: let fnerror(program$,err,line,act$,"xit")
02690   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02710   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02720 ERTN_EXEC_ACT: execute act$ : goto ERTN
02730 ! /region
02740 ! ______________________________________________________________________
02750   if err=61 then goto SCREENFORINPUT2
02760 ! ______________________________________________________________________
02770 XIT: let fnxit
02780 ! ______________________________________________________________________
02790 L2790: pr newpage
02800   close #101: ioerr L2810
02810 L2810: open #101: "SROW=6,SCOL=8,EROW=08,ECOL=75,BORDER=DR,CAPTION=ALPHA NAME SEARCH",display,outin 
02820   let prtall=0
02830   pr fields "7,9,C 45,H,N": "Employee Name (blank for all):"
02840   pr fields "9,34,C 11,B,5": "Cancel (F5)"
02850 L2850: input fields "7,47,C 28,UET,N": nam$
02860   if cmdkey=5 then goto SCREENFORINPUT1
02870 L2870: let l1=max(1,len(rtrm$(nam$)))
02880   if rtrm$(nam$)="" then let prtall=1
02890   restore #5,search>=nam$(1:l1): nokey L2850
02900 L2900: form pos 1,c 8,c 30
02910 L2910: pr newpage
02920   close #101: ioerr L2930
02930 L2930: open #101: "SROW=2,SCOL=8,EROW=23,ECOL=60,BORDER=DR",display,outin 
02940   pr fields "1,10,C 40,R,N": "  Number  Name                       "
02950   cde=0
02960   mat sc$(20)
02970   for j=1 to 20
02980     read #5,using L2900,release: a$,n$ eof L3030
02990     if nam$(1:l1)=n$(1:l1) or prtall=1 then goto L3000 else goto L3030
03000 L3000: cde=1
03010     let sc$(j)=a$&"  "&n$
03020   next j
03030 L3030: if cde=0 then goto L3190
03040   if j<1 then let j=1 else let j=j-1
03050   mat sc$(j)
03060   pr fields mat iosc$: mat sc$
03070   pr fields "24,08,C 45,R,N": " Enter to SELECT; F1 TO CONTINUE; F5 to stop"
03080 L3080: input select mat iosc$: mat sc$
03090   if cmdkey=5 then goto L2790
03100   if cmdkey=1 then goto L3160
03110   let ent$=sc$(curfld)(1:8)
03120   read #1,using L3130,key=ent$: eno,mat em$ nokey L3080
03130 L3130: form pos 1,n 8,3*c 30
03140   let ent=eno
03150   let ji1(1)=eno: let ji1(2)=2: goto SCREENFORINPUT2
03160 L3160: if cmdkey=5 then goto L2790
03170   let selclp=1
03180   goto L2910
03190 L3190: let selclp=0
03200   goto L2790
03210 ! ______________________________________________________________________
03220 L3220: pr newpage ! INPUT FROM DISKETTE FILE
03230   let win=102
03240   fnopenwin(win,10,20,15,59,cap$)
03250   let io2b$(1)="8,42,CU 1,UET,N"
03260   let io2b$(2)="10,43,Nz 6,UET,N"
03270   pr #win,fields "4,2,Cr 26,N": "Diskette Drive:"
03280   pr #win,fields "5,2,Cr 26,N": "Transaction Date (mmddyy):"
03290   let io2b$(1)="4,29,Cu 1,UT,N"
03300   let io2b$(2)="5,29,Nz 6,UT,N"
03310   pr fields "16,30,C 09,B,1": "Next (F1)"
03320   pr fields "16,41,C 11,B,5": "Cancel (F5)"
03330   let dv$="A"
03340 L3340: rinput #win,fields mat io2b$: dv$,td1 conv CONV2
03350   if ce>0 then let io2b$(ce)(ce1:ce2)="U": ce=0
03360   if cmdkey>0 then goto L3430 else ce=curfld
03370 L3370: ce=ce+1: if ce>udim(io2b$) then ce=1
03380 L3380: let io2b$(ce)=rtrm$(io2b$(ce)) !:
        ce1=pos(io2b$(ce),"U",9) !:
        if ce1=0 then goto L3370
03390   ce2=ce1+1 : let io2b$(ce)(ce1:ce1)="UC" : goto L3340
03400 CONV2: if ce>0 then let io2b$(ce)(ce1:ce2)="U"
03410   ce=cnt+1
03420 ERR2: pr fields "24,78,C 1": bell : goto L3380
03430 L3430: if cmdkey=5 then goto SCR1
03440   if dv$="A" or dv$="B" then goto L3450 else ce=1: goto ERR2
03450 L3450: if td1<10100 or td1>123199 then ce=2: goto ERR2
03460   open #9: "Name="&dv$&":TIMEDAT.EXP",display,input 
03470 L3470: linput #9: ln$ eof L3850
03480   if ln$(1:1)=>"0" and ln$(1:1)<="9" then goto R1
03490   if rtrm$(ln$(1:5))="" and ln$(6:7)=>"01" and ln$(6:7)<="99" then goto R2
03500   if rtrm$(ln$(1:10))="" and ln$(11:11)=>"0" and ln$(11:11)<="9" then goto R3
03510 R1: let j1=val(ln$(1:8)) conv L3470
03520   if j1>0 then goto L3540
03530   goto L3470
03540 L3540: if j1=ji1(1) then goto L3470 ! TOTAL RECORD
03550   mat ji1=(0)
03560   mat ji2=(0)
03570   let ji1(1)=j1 ! EMPLOYEE #
03580   let ji1(2)=2 ! HOURLY
03590   let ji2(1)=1 ! Category - LABOR
03600   goto L3470
03610 R2: let ji2(2)=ji1(4)=val(ln$(1:10))
03620   let ji1(3)=td1
03630   goto L3470
03640 R3: let jn$=rtrm$(ltrm$(ln$(11:18)))
03650   for j=1 to len(jn$)
03660     if jn$(j:j)<"0" or jn$(j:j)>"9" then let jn$(j:j)=""
03670   next j
03680   let jn$=cnvrt$("N 6",val(jn$)) conv L3470
03690   let ji1(5)=val(ln$(72:82)) ! REG HRS
03700   let ji1(6)=val(ln$(83:90)) ! OVT HRS
03710   let empnam$=""
03720   let en$=cnvrt$("N 8",ji1(1))
03730   read #1,using L3740,key=en$: empnam$,ta1 nokey L3830
03740 L3740: form pos 9,c 30,pos 173,pd 3
03750 L3750: read #2,using L3760,rec=ta1: tdn,ptp,sal,mat hr,nta
03760 L3760: form pos 9,n 3,pos 30,n 6.3,pos 58,3*pd 4.2,pos 468,pd 3
03770   if tdn=ji1(4) then goto L3800
03780   if nta=0 then goto L3830
03790   let ta1=nta : goto L3750
03800 L3800: let ji2(3)=hr(1)*ji1(5)+hr(2)*ji1(6)
03810   if ji2(4)<1 or ji2(4)>10 then let pt=ptp/100*ji2(3) else let pt=0
03820 L3820: let rw=lrec(3)+1
03830 L3830: write #3,using L1660,rec=rw: mat ji1,jn$,mat ji2, pt, empnam$, sal duprec L3820
03840   goto L3470
03850 L3850: close #9: 
03860   let sh$(1)="   JOB COST INPUT TIME FROM DISKETTE"
03870   goto WHATNOWSCREEN
03880 L3880: ! ______________________________________________________________________
03890 SRCH: bk=0 : pr newpage
03900   let wrds$(1)="1. JOB # SEARCH" : let skl(1)=6
03910   let wrds$(2)="2. JOB NAME SEARCH" : let skl(2)=25
03920   let seq=2 : let fil=14: goto L4020
03930   for j=1 to udim(ios$): let ios$(j)=str$(j+12)&",25,C 30,N": next j
03940   close #101: ioerr L3950
03950 L3950: open #101: "SROW=12,SCOL=24,EROW="&str$(udim(ios$)+13)&",ECOL=55,BORDER=SR,CAPTION=SELECT TYPE OF SEARCH",display,outin 
03960   pr #101: newpage
03970   pr fields mat ios$: mat wrds$
03980   pr fields str$(udim(ios$)+14)&",32,C 16,R,N": "PRESS F5 TO STOP"
03990   input select mat ios$: mat wrds$
04000   let seq=curfld
04010   if cmdkey=5 then goto SRCHEND
04020 L4020: pr newpage
04030   close #101: ioerr L4040
04040 L4040: open #101: "SROW=6,SCOL=10,EROW=10,ECOL=69,BORDER=DR,CAPTION="&wrds$(seq),display,outin 
04050   let prtall=0
04060   pr fields "7,11,C 22,N": "Beginning Search Data:"
04070   pr fields "9,11,C 58,H,N": "NOTE: Enter Beginning Search Data as blank for all records"
04080   pr fields "11,32,C 16,R,N": "PRESS F5 TO STOP"
04090 L4090: input fields "7,34,C "&str$(skl(seq))&",UT,N": nam$
04100   if cmdkey=5 then goto SRCHEND
04110   if seq=1 then let nam$=lpad$(rtrm$(nam$),skl(1))
04120   if seq=2 then let nam$=rpad$(ltrm$(nam$),skl(2))
04130   restore #14,search>=nam$: nokey L4090
04140   close #101: ioerr L4150
04150 L4150: pr newpage
04160   pr fields "1,2,C 6,R,N": "JOB #:"
04170   pr fields "1,9,C 40,R,N": "JOB NAME"
04180   pr fields "1,50,C 8,R,N": "EST.Date"
04190   pr fields "1,59,C 10,R,N": "EST.Date"
04200   pr fields "1,59,C 10,R,N": " CT.AMOUNT"
04210   pr fields "1,70,C 10,R,N": "BLD.AMOUNT"
04220   cde=0
04230   for j=1 to 20
04240     read #14,using L4250: jn$,n$,mat a$,mat b eof SREND
04250 L4250: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
04260     cde=1
04270     pr fields str$(j+1)&",2,C 6,UT,N": jn$
04280     pr fields str$(j+1)&",9,C 40,UT,N": n$
04290     pr fields str$(j+1)&",50,PIC(ZZ/ZZ/ZZ),UT,N": b(1)
04300     pr fields str$(j+1)&",59,N 10.2,UT,N": b(2)
04310     pr fields str$(j+1)&",70,N 10.2,UT,N": b(3)
04320     if j>1 then goto L4360
04330     bk=bk+1
04340     if bk>20 then bk=1
04350     bk$(bk)=bl$(2)(1:28)
04360 L4360: next j
04370 SREND: if j>1 then let j=j-1
04380   mat in2$(j)
04390   pr fields "24,02,C 67,R,N": "ENTER TO CONTINUE; F2 TO BACKUP; F5 TO STOP; Select ACCOUNT Number:"
04400 L4400: input fields "24,70,C 6,RE,N": k$
04410   alp=0
04420   if cmdkey=5 then goto SRCHEND
04430   if rtrm$(k$)><"" then bl$=k$ : goto SRCHEND
04440   if cmdkey><2 then goto L4490
04450   bk=bk-1
04460   if bk<1 then goto L4510
04470   restore #14,key>=bk$(bk): nokey L4510
04480   bk=bk-1
04490 L4490: let selclp=1
04500   goto L4150
04510 L4510: let selclp=0
04520   goto L4020
04530 SRCHEND: if rtrm$(k$)="" or len(rtrm$(k$))>6 then alp=0: goto L4580
04540   let jn$=lpad$(rtrm$(k$),6)
04550   read #11,using L4250,key=jn$: hjn$,n$,mat a$,mat b nokey L4400
04560   let ti=alp=3 : let jcsrch=1: let io2$(8)="11,29,N 5,UC,N": restore #11: : goto SCREENFORINPUT2
04570   close #101: ioerr L4580
04580 L4580: restore #11: : goto SCREENFORINPUT2
