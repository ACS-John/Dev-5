00010 ! Replace S:\acsPR\jcInptC
00020 ! Input (Job Cost) Charges
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnoldmsgbox,fnwait,fnwin3b,fnopenprn,fncloseprn,fncno,fnerror,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cn$*11,k$*11,n$*40,en$*8,hr(2),empnam$*30,ds$*30,a$(3)*30,b(4)
00080   dim rn$*12,jn$*6,ji2(3),d$*30,label1$(7)*24,iolabel1$(7),io1$(7)
00090   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,cnt$*25,cap$*128
00100   dim msgline$(2)*60,response$(5)*1,wrd2$(4)*38
00110 ! ______________________________________________________________________
00120   let fncno(cno)
00130 ! 
00140   let cap$="Input (Job Cost) Charges"
00145   let fntop("S:\acsPR\jcInptC", "Enter Charges")
00146   let fnconsole(1)
00150 ! ______________________________________________________________________
00160   data "Reference Number:"
00170   data "Date (mmddyy):"
00180   data "Job Number:"
00190   data "Category:"
00200   data "Sub-Category:"
00210   data "Amount:"
00220   data "Description:"
00230   read mat label1$
00240 ! ______________________________________________________________________
00250   let io1$(1)="4,20,C 12,UT,N"
00260   let io1$(2)="5,20,Nz 6,UT,N"
00270   let io1$(3)="6,20,c 6,UT,N"
00280   let io1$(4)="7,20,n 5,UT,N"
00290   let io1$(5)="08,20,N 2,UT,N"
00300   let io1$(6)="09,20,n 10.2,UT,N"
00310   let io1$(7)="10,20,C 30,UT,N"
00320 ! ______________________________________________________________________
00330   for j=1 to 7
00340     let iolabel1$(j)=str$(j+3)&",2,Cr 17,N"
00350   next j
00360 ! ______________________________________________________________________
00370   open #3: "Name="&env$('temp')&"\Work."&session$&",SIZE=0,RecL=63,Replace",internal,outin,relative 
00380   open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00390   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00400   open #13: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\SCIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00410 ! ______________________________________________________________________
00420 INPUTSCREEN1: ! 
00430   let shoption=1
00440 INPUTSCREEN2: ! 
00450   print newpage
00460   let win=102
00470   let fnopenwin(win,08,08,18,72,cap$)
00480   print #win,fields mat iolabel1$: mat label1$
00490   print #win,fields io1$(2): dat
00500   if shoption=1 then print fields "19,24,C 09,B,1": "Next (F1)"
00510   if shoption=1 then print fields "19,34,C 09,B,5": "Stop (F5)"
00520   if shoption=1 then print fields "19,44,C 11,B,5": "Search (F6)"
00530   if shoption=2 then print fields "19,18,C 09,B,1": "Next (F1)"
00540   if shoption=2 then print fields "19,28,C 11,B,4": "Delete (F4)"
00550   if shoption=2 then print fields "19,40,C 09,B,5": "Stop (F5)"
00560   if shoption=2 then print fields "19,50,C 11,B,5": "Search (F6)"
00570   if c1=2 then print #win,fields mat io1$: rn$,dat,jn$,mat ji2,d$
00580 L580: input #win,fields mat io1$: rn$,dat,jn$,mat ji2,d$ conv L1770
00590   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
00600   if cmdkey>0 then goto L780 else let ce=curfld
00610   if ce<>3 then goto L640
00620 L620: read #11,using L900,key=lpad$(rtrm$(jn$),6): n$ nokey L630 !:
        print #win,fields "6,27,C 38,H,N": n$(1:38) !:
        goto L640
00630 L630: let msgline$(1)="Job Number not on file." !:
        let msgline$(2)="Please select a different Job Number" !:
        let fnoldmsgbox(mat response$,cap$,mat msgline$,1) !:
        let ce=3 !:
        goto ERR1
00640 L640: if ce<>4 then goto L690
00650   let jn$=lpad$(rtrm$(jn$),6)
00660   let cat$=jn$&lpad$(rtrm$(str$(ji2(1))),5)
00670   read #12,using L2700,key=cat$: jn2$,cn$,cnt$ nokey L680 !:
        print #win,fields "7,27,C 38,H,N": cnt$(1:38) !:
        goto L690
00680 L680: let msgline$(1)="Invalid Category Number" !:
        let msgline$(2)="Please select a different Category Number" !:
        let fnoldmsgbox(mat response$,cap$,mat msgline$,1) !:
        let ce=4 !:
        goto ERR1
00690 L690: if ce<>5 then goto L720
00700   read #13,using L1020,key=lpad$(str$(ji2(2)),3): ds$ nokey L710 !:
        print #win,fields "8,27,C 38,H,N": ds$(1:38) !:
        goto L720
00710 L710: let msgline$(1)="Invalid Sub-Category Number" !:
        let msgline$(2)="Please select a different Sub-Category Number" !:
        let fnoldmsgbox(mat response$,cap$,mat msgline$,1) !:
        let ce=5 !:
        goto ERR1
00720 L720: let ce=ce+1: if ce>udim(io1$) then let ce=1
00730 L730: let io1$(ce)=rtrm$(io1$(ce)) !:
        let ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L640
00740   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L580
00750 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00760   let ce=cnt+1
00770 ERR1: print fields "24,78,C 1": bell : goto L730
00780 L780: if cmdkey=5 then goto WHATNEXT
00790   if cmdkey<>6 then goto L850
00800   let ce=curfld
00810   on ce-2 gosub SRCH,CATEGORY_SEARCH,SRCH3 none L620
00820   if ce=3 then print #win,fields io1$(3): jn$
00830   goto L620
00840 ! ______________________________________________________________________
00850 L850: if cmdkey=4 and c1=2 then goto DEL
00860   if ltrm$(rtrm$(rn$))="-1" then let ce=1 : goto ERR1
00870   if dat<10100 or dat>123199 then let ce=2: goto ERR1
00880   if rtrm$(jn$)="" and ji2(1)=0 then goto L1000
00890   read #11,using L900,key=lpad$(rtrm$(jn$),6): n$ nokey L920
00900 L900: form pos 7,c 40
00910   goto L930
00920 L920: let ce=3: goto ERR1
00930 L930: let cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(ji2(1)),5)
00940   read #12,using L950,key>=cn$: k$ nokey L990
00950 L950: form pos 1,c 11
00960   if cn$(1:6)<>k$(1:6) then goto L990
00970   if cn$(7:11)<>k$(7:11) then let ce=4: goto L1790
00980   goto L1000
00990 L990: let ce=3: goto L1790
01000 L1000: if ji2(2)=0 then goto L1030
01010   read #13,using L1020,key=lpad$(str$(ji2(2)),3): ds$ nokey L1040
01020 L1020: form pos 4,c 30
01030 L1030: goto L1050
01040 L1040: let ce=5: goto L1790
01050 L1050: if ji2(3)=0 then let ce=6: goto L1790
01060   let pt=ptp*ji2(3)
01070   if c1=2 then goto L1120
01080 L1080: let rw=lrec(3)+1
01090   write #3,using L1100,rec=rw: rn$,dat,jn$,mat ji2,d$ duprec L1080
01100 L1100: form pos 1,c 12,pd 4,c 6,2*pd 3,pd 5.2,c 30
01110   goto INPUTSCREEN2
01120 L1120: rewrite #3,using L1100,rec=rr: rn$,dat,jn$,mat ji2,d$
01130   goto L1210
01140 ! ______________________________________________________________________
01150 DEL: ! 
01160   rewrite #3,using L1100,rec=rr: "-1",0,"",0,0,0,""
01170   goto L1210
01180 ! ______________________________________________________________________
01190 ADDITIONALENTRIES: ! 
01200   let shoption=2
01210 L1210: print newpage
01220   let fnopenwin(win=103,10,20,14,59,cap$)
01230   print #win,fields "4,2,C 28,N": "Reference Number to correct:"
01240   print fields "15,35,C 09,B,5": "Done (F5)"
01250 L1250: input #win,fields "4,31,N 5,UT,N": rr conv L1250
01260   if cmdkey=5 then goto WHATNEXT
01270   if rr<1 or rr>rw then goto L1250
01280   close #win: ioerr L1290
01290 L1290: read #3,using L1100,rec=rr: rn$,dat,jn$,mat ji2,d$
01300   goto INPUTSCREEN2
01310 ! ______________________________________________________________________
01320 WHATNEXT: ! 
01330   print newpage
01340   let fnopenwin(win=102,09,20,16,59,cap$)
01350   let wrd2$(1)="1. Print Input Proof List"
01360   let wrd2$(2)="2. Corrections"
01370   let wrd2$(3)="3. Additional Entries"
01380   let wrd2$(4)="4. Post to Job Cost File"
01390   for j=1 to udim(wrd2$)
01400     let io2$(j)=str$(j+3)&",2,C 38,N"
01410   next j
01420   print fields "17,27,c 25,B,5": "Exit without posting (F5)"
01430 L1430: rinput #win,select mat io2$,attr "H": mat wrd2$
01440   let c1=curfld
01450   if cmdkey=5 then goto XIT
01460   on c1 goto PRINTPROOFLIST,ADDITIONALENTRIES,INPUTSCREEN1,POSTTOJOB none L1430
01470 ! ______________________________________________________________________
01480 PRINTPROOFLIST: ! 
01490   print newpage
01500   let fnwait(104,cap$,message$,1)
01510   let fnopenprn
01520   print #255,using L1530: "Job Cost Input Proof List"
01530 L1530: form pos 1,cc 80,skip 1
01540   print #255,using L1530: "Date: "&date$&"     Time: "&time$
01550   print #255: "Ref #   Reference #    Date    Job #     Category   Sub-Category     Amount   Description"
01560   for j=1 to rw
01570     read #3,using L1100,rec=j: rn$,dat,jn$,mat ji2,d$
01580     print #255,using L1590: j,rn$,dat,jn$,mat ji2,d$
01590 L1590: form pos 1,n 5,x 3,c 12,n 8,x 3,c 8,n 7,n 14,n 15.2,x 3,c 30,skip 1
01600     let ta=ta+ji2(3)
01610   next j
01620   print #255,using L1630: " ____________",ta
01630 L1630: form pos 63,c 13,skip 1,pos 63,n 13.2
01640   let ta=0
01650   let fncloseprn
01660   goto WHATNEXT
01670 ! ______________________________________________________________________
01680 POSTTOJOB: ! 
01690   close #3: 
01700   close #11: 
01710   close #12: 
01720   close #13: 
01730   chain "S:\acsPR\JCMRGC"
01740 ! ______________________________________________________________________
01750 XIT: let fnxit
01760 ! ______________________________________________________________________
01770 L1770: if ce>0 then let io1$(ce)(ce1:ce2)="U"
01780   let ce=cnt+1
01790 L1790: print fields "24,1,C 7,N": bell
01800   let io1$(ce)=rtrm$(io1$(ce))
01810   let ce1=pos(uprc$(io1$(ce)),"U",1)
01820   let ce2=ce1+1
01830   let io1$(ce)(ce1:ce1)="UC"
01840   goto L580
01850 ! ______________________________________________________________________
01860 SRCH: let bk=0 : let hce=ce
01870   close #103: ioerr L1880
01880 L1880: open #103: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,output 
01890   print #103: newpage
01900   let wrds$(1)="1. JOB # SEARCH" : let skl(1)=6
01910   let wrds$(2)="2. JOB NAME SEARCH" : let skl(2)=25
01920   let seq=11 : goto L2020
01930   for j=1 to udim(ios$): let ios$(j)=str$(j+12)&",25,C 30,N": next j
01940 L1940: close #101: ioerr L1950
01950 L1950: open #101: "SROW=12,SCOL=24,EROW="&str$(udim(ios$)+13)&",ECOL=55,BORDER=SR,CAPTION=SELECT TYPE OF SEARCH",display,outin 
01960   print #101: newpage
01970   print fields mat ios$: mat wrds$
01980   print fields str$(udim(ios$)+14)&",35,C 09,B,5": "Stop (F5)"
01990   input select mat ios$,attr "H": mat wrds$
02000   let seq=curfld
02010   if cmdkey=5 then goto SRCHEND
02020 L2020: print newpage
02030   let win2=101
02040   let fnwin3b(win2,wrds$(seq-10),5,41+skl(seq-10),1,1)
02050   let prtall=0
02060   print #win2,fields "4,2,C 38,N": "Beginning Search Data (blank for all):"
02070 L2070: input #win2,fields "4,41,C "&str$(skl(seq-10))&",UT,N": nam$
02080   if cmdkey=5 then goto L1940
02090   if seq=11 then let nam$=lpad$(rtrm$(nam$),skl(1))
02100   restore #seq,search>=nam$: nokey L2070
02110   close #win2: ioerr L2120
02120 L2120: print newpage
02130   print fields "1,2,C 6,R,N": "Job #:"
02140   print fields "1,9,C 40,R,N": "Job Name"
02150   print fields "1,50,C 8,R,N": "Est.Date"
02160   print fields "1,59,C 10,R,N": "Est.Date"
02170   print fields "1,59,C 14,R,N": "   Ct.Amount"
02180   let cde=0
02190   for j=1 to 20
02200     read #seq,using L2210: jn$,n$,mat a$,mat b eof SREND
02210 L2210: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
02220     let cde=1
02230     print fields str$(j+1)&",2,C 6,UT,N": jn$
02240     print fields str$(j+1)&",9,C 40,UT,N": n$
02250     print fields str$(j+1)&",50,PIC(ZZ/ZZ/ZZ),UT,N": b(1)
02260     print fields str$(j+1)&",59,N 14.2,UT,N": b(2)
02270     if j>1 then goto L2310
02280     let bk=bk+1
02290     if bk>20 then let bk=1
02300     let bk$(bk)=bl$(2)(1:28)
02310 L2310: next j
02320 SREND: if j>1 then let j=j-1
02330   mat in2$(j)
02340   print fields "24,02,C 16,B,1": "Continue (Enter)"
02350   print fields "24,19,C 09,B,2": "Back (F2)"
02360   print fields "24,29,C 09,B,5": "Stop (F5)"
02370   print fields "24,39,C 25,R,N": "or Select Account Number:"
02380   input fields "24,65,C 6,UT,N": k$
02390   let alp=0
02400   if cmdkey=5 then goto SRCHEND
02410   if rtrm$(k$)><"" then let jn$=k$ !:
          print #win,fields io1$(3): k$ : goto SRCHEND
02420   if cmdkey><2 then goto L2470
02430   let bk=bk-1
02440   if bk<1 then goto L2500
02450   restore #seq,key>=bk$(bk): nokey L2500
02460   let bk=bk-1
02470 L2470: let selclp=1
02480   goto L2120
02490 ! ______________________________________________________________________
02500 L2500: let selclp=0
02510   goto L2020
02520 ! ______________________________________________________________________
02530 SRCHEND: close #103: ioerr L2540
02540 L2540: return 
02550 ! ______________________________________________________________________
02560 CATEGORY_SEARCH: ! 
02570   let bk=0
02580   close #103: ioerr L2590
02590 L2590: open #103: "SROW=2,SCOL=47,EROW=23,ECOL=79,BORDER=DR",display,output 
02600   print #103: newpage
02610   let jn$=lpad$(rtrm$(jn$),6)
02620   read #11,using L2630,key=jn$: jn$,n$,mat a$,mat b nokey L2750
02630 L2630: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2
02640   restore #12,search>=jn$: nokey L2750
02650 L2650: let ln=0
02660   print #103: newpage
02670   print fields "2,47,C 33,R,N": " "&n$(1:31)
02680 L2680: read #12,using L2700: jn2$,cn$,cnt$ eof L2750
02690   if jn2$<>jn$ then goto L2750
02700 L2700: form pos 1,c 6,c 5,c 25
02710   let ln=ln+1
02720   print fields str$(ln+2)&",48,C 5,UT,N": cn$
02730   print fields str$(ln+2)&",54,C 25,UT,N": cnt$
02740   if ln<20 then goto L2680
02750 L2750: print fields "23,47,Cc 33,B,1": "Next (Enter)"
02760   print fields "24,48,C 25,R,N": "or enter Category Number:"
02770   print fields "24,74,C 5,UT,N": ""
02780   input fields "24,74,C 5,UT,N": k$
02790   let ji2(1)=val(k$) conv L2810
02800   if rtrm$(k$)<>"" then print #win,fields io1$(4): ji2(1)
02810 L2810: if rtrm$(k$)><"" then goto SRCHEND
02820   if ln<20 then goto SRCHEND
02830   goto L2650
02840 ! ______________________________________________________________________
02850 SRCH3: let bk=0
02860   close #103: ioerr L2870
02870 L2870: open #103: "SROW=2,SCOL=47,EROW=23,ECOL=79,BORDER=DR",display,output 
02880 L2880: print #103: newpage
02890   print fields "2,47,C 33,R,N": " "&n$(1:31)
02900   restore #13: 
02910   let ln=0
02920 L2920: read #13,using L2930: sc$,cnt$ eof L2980
02930 L2930: form pos 1,c 3,c 25
02940   let ln=ln+1
02950   print fields str$(ln+2)&",49,C 3,UT,N": sc$
02960   print fields str$(ln+2)&",54,C 25,UT,N": cnt$
02970   if ln<20 then goto L2920
02980 L2980: print fields "23,47,C 33,R,N": " PRESS ENTER TO CONTINUE"
02990   print fields "24,47,C 33,R,N": " OR ENTER SUB-CAT #:"
03000   input fields "24,69,C 2,RE,N": k$
03010   let ji2(2)=val(k$) conv L3030
03020   if rtrm$(k$)<>"" then print #win,fields io1$(5): ji2(2)
03030 L3030: if rtrm$(k$)><"" then goto SRCHEND
03040   if ln<20 then goto SRCHEND
03050   goto L2880
03060 ! ______________________________________________________________________
03070 ! <Updateable Region: ERTN>
03080 ERTN: let fnerror(program$,err,line,act$,"xit")
03090   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03100   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03110   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
03120 ERTN_EXEC_ACT: execute act$ : goto ERTN
03130 ! /region
03140 ! ______________________________________________________________________
