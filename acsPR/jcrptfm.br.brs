00010 ! Replace S:\acsPR\JCRptFM
00020 ! Job Cost User-Designed Report File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox,fnfkey, fnopenprn,fncloseprn, fncno,fnerror,fnchain,fnrx,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,message$*40,msgline$(2)*60,response$(5)*1
00080   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,iom$(3),scm$(3)*40
00090   dim io1$(10),io2$(7),fd$(20),rptemp(20),tempch$(4)*256,rptn$*6,rnew$*6
00100   dim rt$*51,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20),fc(20)
00110   dim tcj(20),tcs(20),rno$(50)*2,em$*40,wrd3$(2)*23,io3$(2)
00120 ! ______________________________________________________________________
00130   let fntop("S:\acsPR\JCRptFM",cap$="User Designed Reports")
00140   let fncno(cno)
00145   let fnconsole(1)
00150   let pg=3
00160 ! ______________________________________________________________________
00170   open #1: "Name=S:\acsPR\JCREPORT.MST,KFName=S:\acsPR\JCREPORT.idx,Shr",internal,outin,keyed 
00180 ! ______________________________________________________________________
00190 MENU1: print newpage
00200   let fnopenwin(win=101,09,20,15,59,cap$)
00210   let scm$(1)="1. Add or Edit" !:
        let scm$(2)="2. Print Proof List" !:
        let scm$(3)="3. Search"
00220   for j=1 to udim(scm$) : let iom$(j)=str$(j+3)&",2,C 38,N" : next j
00230   print fields "16,35,C 09,B,5": "Exit (F5)"
00240 L240: rinput #win,select mat iom$,attr "H": mat scm$ !:
        let ti=curfld
00250   if cmdkey=5 then goto XIT
00260   on ti+1 goto XIT,L290,L2180,SRCH none L240
00270   goto XIT
00280 ! ______________________________________________________________________
00290 L290: print newpage
00300   let win=102
00310   let fnopenwin(win,10,20,14,59,cap$)
00320   print #win,fields "4,02,C 23,N": "Job Cost Report Number:"
00330   print fields "15,35,C 09,B,5": "Done (F5)"
00340 L340: input #win,fields "4,26,Nz 2,UT,N": rptn conv L340
00350   let rx=rptn
00360   if cmdkey=5 then goto XIT
00370   if rptn=0 then goto L340
00380   let rn=rptn
00390   let rptn$=lpad$(str$(rptn),2)
00400   read #1,using L410,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L440
00410 L410: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
00420   goto L520
00430 ! ______________________________________________________________________
00440 L440: let msgline$(1)="Report Number "&ltrm$(rptn$)&" was not found."
00450   let msgline$(2)="Do you wish to add it now? (Y/N)"
00460   let fnoldmsgbox(mat response$,cap$,mat msgline$,2)
00470   if response$(1)="Y" then goto L500
00480   if response$(1)="N" then goto L290
00490 ! ______________________________________________________________________
00500 L500: let rt$="" : mat ch$=("") : let ips=sd=cp=sc=0 : mat ps=(0) !:
        mat f$=("") : mat pp=(0) : mat ppr=(0) : mat dp=(0) : mat fc=(0) !:
        mat tcj=(0) : mat tcs=(0)
00510   write #1,using L410: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
00520 L520: let tempch$(1)=ch$(1)(1:66)
00530   let tempch$(2)=ch$(1)(67:132)
00540   let tempch$(3)=ch$(2)(1:66)
00550   let tempch$(4)=ch$(2)(67:132)
00560 L560: print newpage
00570   let fnopenwin(win=103,2,6,22,73,cap$)
00580   print #win: newpage
00590   print #win,fields "02,02,Cr 14,N": "Report Number:"
00600   print #win,fields "03,02,Cr 14,N": "Report Title:"
00610   print #win,fields "05,01,Cc 68,R,N": "Column Headings (line 1)"
00620   print #win,fields "06,02,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
00630   print #win,fields "08,02,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
00640   print #win,fields "11,01,Cc 68,R,N": "Column Headings (line 2)"
00650   print #win,fields "13,02,C 66,N": "    5   10   15   20   25   30   35   40   45   50   55   60    66"
00660   print #win,fields "15,02,C 66,N": "  70   75   80   85   90   95  100  105  110  115  120  125    132"
00670   print #win,fields "17,2,Cr 51,N": "Item Number for Print Selection (blank for all):"
00680   print #win,fields "18,2,Cr 51,N": "Summarize Category Records (1=Y):"
00690   print #win,fields "19,2,Cr 51,N": "Use Condensed Print (1=Y):"
00700   print #win,fields "20,2,Cr 51,N": "Selection Codes: (1.=  2.>=  3.<=  4.number range):"
00710   let io1$(01)="02,17,Nz 2,UT,N"
00720   let io1$(02)="03,17,C 51,CUT,N"
00730   let io1$(03)="07,02,C 66,UT,N"
00740   let io1$(04)="09,02,C 66,UT,N"
00750   let io1$(05)="12,02,C 66,UT,N"
00760   let io1$(06)="14,02,C 66,UT,N"
00770   let io1$(07)="17,54,Nz 3,UT,N"
00780   let io1$(08)="18,54,Cu 1,UT,N"
00790   let io1$(09)="19,54,Cu 1,UT,N"
00800   let io1$(10)="20,54,Nz 1,UT,N"
00810 L810: mat fkey$=("") !:
        let fkey$(1)="Next" !:
        let fkey$(4)="Delete" !:
        let fkey$(5)="Done" !:
        let em$="" !:
        let fnfkey(23,mat fkey$,mat disfk,em$,0)
00820   if sd=1 then let sd$="Y" else let sd$="N"
00830   if cp=1 then let cp$="Y" else let cp$="N"
00840 L840: rinput #win,fields mat io1$: rn,rt$,mat tempch$,ips,sd$,cp$,sc conv CONV1
00850   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
00860   if cmdkey>0 then goto L930 else let ce=curfld
00870 L870: let ce=ce+1: if ce>udim(io1$) then let ce=1
00880 L880: let io1$(ce)=rtrm$(io1$(ce)) : let ce1=pos(io1$(ce),"U",1) !:
        if ce1=0 then goto L870
00890   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L840
00900 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
00910   let ce=cnt+1
00920 ERR1: print fields "24,78,C 1": bell : goto L880
00930 L930: if cmdkey=5 then goto MENU1
00940   if sd$<>"Y" and sd$<>"N" then let ce=8 : goto ERR1
00950   if sd$="Y" then let sd=1 else let sd=0
00960   if cp$<>"Y" and cp$<>"N" then let ce=9 : goto ERR1
00970   if cp$="Y" then let cp=1 else let sd=0
00980   if rn<1 then let ce=1 : goto ERR1
00990   if cmdkey=4 then goto L1080
01000   if rn=rptn then goto L1120
01010   let rnew$=lpad$(str$(rn),2)
01020   read #1,using L1030,key=rnew$: rnew nokey L1120
01030 L1030: form pos 1,n 2
01040   print fields "2,40,C 38,N": "DUPLICATE REPORT NUMBER.  PRESS ENTER."
01050 L1050: input fields "2,79,C 1,N": cnt$ conv L1050
01060   goto L810
01070 ! ______________________________________________________________________
01080 L1080: let rptn$=lpad$(str$(rptn),2)
01090   delete #1,key=rptn$: 
01100   goto L290
01110 ! ______________________________________________________________________
01120 L1120: if ips<0 or ips>124 then goto L1130 else goto L1160
01130 L1130: let ce=7
01140   goto L2910
01150 ! _______________________________________________
01160 L1160: if sd<0 or sd>1 then goto L1170 else goto L1200
01170 L1170: let ce=8
01180   goto L2910
01190 ! _______________________________________________
01200 L1200: if cp<0 or cp>1 then goto L1210 else goto L1240
01210 L1210: let ce=9
01220   goto L2910
01230 ! _______________________________________________
01240 L1240: if sc<0 or sc>4 then goto L1250 else goto L1280
01250 L1250: let ce=10
01260   goto L2910
01270 ! _______________________________________________
01280 L1280: let ch$(1)=tempch$(1)&tempch$(2)
01290   let ch$(2)=tempch$(3)&tempch$(4)
01300   if ips=0 then goto L1570
01310   print newpage
01320   for j=1 to 20
01330     let fd$(j)=str$(j+1)&",40,N 12.3,UT,N"
01340   next j
01350   for w=1 to 5
01360     for j=2 to 21
01370       print fields "1,25,C 50": "Job Cost Report Number "&str$(rptn)
01380       print fields str$(j)&",5,C 30,N": "Print Selection Criteria"
01390     next j
01400     if w=1 then goto L1410 else goto L1430
01410 L1410: let k=0
01420     goto L1440
01430 L1430: let k=k+20
01440 L1440: for q=1 to 20
01450       let rptemp(q)=psc(q+k)
01460     next q
01470 L1470: rinput fields mat fd$: mat rptemp conv L1470
01480     for q=1 to 20
01490       let psc(q+k)=rptemp(q)
01500     next q
01510     if rptemp(20)><0 then goto L1530
01520     let lst=1
01530 L1530: mat rptemp=(0)
01540     print newpage
01550     if lst=1 then goto L1570
01560   next w
01570 L1570: for j=1 to 20
01580 L1580: print newpage
01590     let win=105
01600     let fnopenwin(win,07,04,19,77,cap$)
01610     print #win,fields "4,2,Cr 21,N": "Report Number:" !:
          print #win,fields "4,24,C 2,N": str$(rptn)
01620     print #win,fields "5,2,Cr 21,N": "Column Number:" !:
          print #win,fields "5,24,C 10,N": str$(j)
01630     print #win,fields "06,2,Cr 21,N": "Formula for Printing:"
01640     print #win,fields "07,2,Cr 21,N": "Starting Position:"
01650     print #win,fields "08,2,Cr 21,N": "Field Size:"
01660     print #win,fields "09,2,Cr 21,N": "Decimal Positions:"
01670     print #win,fields "10,2,Cr 21,N": "Detail Print (Y/N):"
01680     print #win,fields "11,2,Cr 21,N": "Total by Job (Y/N):"
01690     print #win,fields "12,2,Cr 21,N": "Grand Totals (Y/N):"
01700     mat fkey$=("") !:
          let fkey$(1)="Next" !:
          let fkey$(2)="Back" !:
          let fkey$(3)="Screen 1" !:
          let fkey$(4)="Completed" !:
          let em$="" !:
          let fnfkey(20,mat fkey$,mat disfk,em$,0)
01710     let io2$(1)="06,24,C 50,UT,N"
01720     let io2$(2)="07,24,Nz 3,UT,N"
01730     let io2$(3)="08,24,Nz 3,UT,N"
01740     let io2$(4)="09,24,N 01,UT,N"
01750     let io2$(5)="10,24,Cu 1,UT,N"
01760     let io2$(6)="11,24,Cu 1,UT,N"
01770     let io2$(7)="12,24,Cu 1,UT,N"
01780     if fc(j)=1 then let detailprint$="N" else let detailprint$="Y"
01790     if tcj(j)=1 then let totalbyjob$="Y" else let totalbyjob$="N"
01800     if tcs(j)=1 then let grandtotal$="Y" else let grandtotal$="N"
01810 L1810: rinput #win,fields mat io2$: f$(j),pp(j),ppr(j),dp(j),detailprint$,totalbyjob$,grandtotal$ conv CONV2
01820     if ce>0 then let io2$(ce)(ce1:ce2)="U": let ce=0
01830     if cmdkey>0 then goto L1900 else let ce=curfld
01840 L1840: let ce=ce+1: if ce>udim(io2$) then let ce=1
01850 L1850: let io2$(ce)=rtrm$(io2$(ce)) !:
          let ce1=pos(io2$(ce),"U",1) !:
          if ce1=0 then goto L1840
01860     let ce2=ce1+1 : let io2$(ce)(ce1:ce1)="UC" : goto L1810
01870 CONV2: if ce>0 then let io2$(ce)(ce1:ce2)="U"
01880     let ce=cnt+1
01890 ERR2: print fields "24,78,C 1": bell : goto L1850
01900 L1900: if rtrm$(f$(j))="" then goto L2020
01910     if detailprint$="Y" then let fc(j)=0 else let fc(j)=1
01920     if totalbyjob$="Y" then let tcj(j)=1 else let tcj(j)=0
01930     if grandtotal$="Y" then let tcs(j)=1 else let tcs(j)=0
01940     if detailprint$<>"Y" and detailprint$<>"N" then let ce=5 : goto ERR2
01950     if totalbyjob$<>"Y" and totalbyjob$<>"N" then let ce=6 : goto ERR2
01960     if grandtotal$<>"Y" and grandtotal$<>"N" then let ce=7 : goto ERR2
01970     if pp(j)<1 or pp(j)>198 then let ce=2: goto L3000
01980     if ppr(j)<1 or ppr(j)>198 then let ce=3: goto L3000
01990     if fc(j)<0 or fc(j)>1 then let ce=5: goto L3000
02000     if tcj(j)<0 or tcj(j)>1 then let ce=6: goto L3000
02010     if tcs(j)<0 or tcs(j)>1 then let ce=7: goto L3000
02020 L2020: if cmdkey=4 then goto L2080
02030     if cmdkey=3 then goto L560
02040     if cmdkey=2 then let j=j-1 else goto L2060
02050     if j>0 then goto L1580
02060 L2060: if cmdkey=1 then goto L2070
02070 L2070: next j
02080 L2080: if rptn=rn then goto L2120
02090   delete #1,key=rptn$: 
02100   write #1,using L410: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
02110   goto L2130
02120 L2120: rewrite #1,using L410,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
02130 L2130: close #1: 
02140   let fnrx(rn)
02150   execute "INDEX S:\acsPR\JCREPORT.MST,S:\acsPR\JCREPORT.idx,1,2,Replace,DupKeys -n"
02160   let fnchain('S:\acsPR\jcRptS1')
02170 ! ______________________________________________________________________
02180 L2180: print newpage
02190   restore #1,key>="  ": nokey L2850
02200   let fnopenwin(win=102,10,28,15,52,cap$)
02210   let wrd3$(1)="Print All Report Files"
02220   let wrd3$(2)="Select Reports to Print"
02230   let io3$(1)="4,2,C 23,N"
02240   let io3$(2)="5,2,C 23,N"
02250   print fields "16,34,C 11,B,5": "Cancel (F5)"
02260   rinput #win,select mat io3$,attr "H": mat wrd3$
02270   let prtall=curfld-1
02280   close #win: ioerr L2290
02290 L2290: if cmdkey=5 then goto MENU1
02300   if prtall=0 then goto L2400
02310   for j=1 to 20
02320     let fnopenwin(win=103,10,20,15,59,cap$)
02330     if j>1 then print #win,fields "6,1,Cc 40,R,N": "Last Report Number Entered was "&rno$(j-1)
02340     print #win,fields "4,2,C 23,N": "Report Number to Print:"
02350     print fields "16,35,C 09,B,5": "Done (F5)"
02360 L2360: input #win,fields "4,26,N 2,UET,N": rno(j) conv L2360
02370     let rno$(j)=lpad$(str$(rno(j)),2)
02380     if cmdkey=5 or rno(j)=0 then goto L2400
02390   next j
02400 L2400: print newpage
02410   let fnwait(104,cap$,message$="Printing Proof List...",1)
02420   on fkey 5 goto L2850
02430   let fnopenprn(cp,58,220,process)
02440   let k=0
02450 L2450: if prtall=0 then goto L2500
02460 L2460: let k=k+1
02470   if val(rno$(k))=0 then goto L2850
02480   read #1,using L2510,key=rno$(k): rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L2460
02490   goto L2520
02500 L2500: read #1,using L2510: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs eof L2850
02510 L2510: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
02520 L2520: print #255,using L2530: "Job Cost Report File Proof List"
02530 L2530: form skip 2,pos 50,c 32
02540   print #255,using L2550: "Report Number",rn
02550 L2550: form pos 1,c 13,pos 20,pic(zz)
02560   print #255,using L2570: "Report Title",rt$
02570 L2570: form pos 1,c 12,pos 13,cc 66
02580   print #255,using L2590: "Column Headings",ch$(1)
02590 L2590: form pos 1,c 15,skip 2,c 132
02600   print #255,using L2610: ch$(2)
02610 L2610: form pos 1,c 132,skip 2
02620   print #255,using L2630: "Item # for Selection",ips
02630 L2630: form pos 1,c 20,pos 30,pic(zz#)
02640   print #255,using L2650: "Summarize Categories",sd
02650 L2650: form pos 1,c 26,pos 32,pic(#)
02660   print #255,using L2650: "Condense Print",cp
02670   print #255,using L2650: "Selection Code",sc
02680   print #255,using L2690: "Print Selection Criteria"
02690 L2690: form skip 1,pos 1,c 30,skip 2
02700   for j=1 to 20
02710     print #255,using L2720: psc(j),psc(j+20),psc(j+40),psc(j+60),psc(j+80)
02720 L2720: form pos 1,5*n 20.3
02730   next j
02740   print #255,using L2750: "Formula for Value","Starting","# of Print Positions","# of Decimal","Skip Detail Print","Total Column","Overall Totals"
02750 L2750: form skip 1,pos 1,c 17,pos 39,c 8,pos 48,c 20,pos 71,c 12,pos 84,c 17,pos 103,c 12,pos 119,c 14
02760   print #255,using L2770: "to be Printed","Print Position","Required","Positions","by Job","by System"
02770 L2770: form pos 1,c 13,pos 38,c 14,pos 53,c 8,pos 72,c 9,pos 107,c 6,pos 123,c 9,skip 2
02780   for j=1 to 20
02790     print #255,using L2800: f$(j),pp(j),ppr(j),dp(j),fc(j),tcj(j),tcs(j)
02800 L2800: form pos 1,c 50,pos 52,n 3,pos 56,n 3,pos 76,n 1,pos 93,n 1,pos 110,n 1,pos 127,n 1
02810   next j
02820   print #255: newpage
02830   goto L2450
02840 ! ______________________________________________________________________
02850 L2850: let fncloseprn
02860   on fkey 5 ignore 
02870   goto MENU1
02880 ! ______________________________________________________________________
02890   if ce>0 then let io1$(ce)(ce1:ce2)="U"
02900   let ce=cnt+1
02910 L2910: print fields "24,80,C 1,N": bell
02920   let io1$(ce)=rtrm$(io1$(ce))
02930   let ce1=pos(uprc$(io1$(ce)),"U",1)
02940   let ce2=ce1+1
02950   let io1$(ce)(ce1:ce1)="RC"
02960   goto L840
02970 ! ______________________________________________________________________
02980   if ce>0 then let io2$(ce)(ce1:ce2)="U"
02990   let ce=cnt+1
03000 L3000: print fields "24,80,C 1,N": bell
03010   let io2$(ce)=rtrm$(io2$(ce))
03020   let ce1=pos(io2$(ce),"U",1)
03030   let ce2=ce1+1
03040   let io2$(ce)(ce1:ce1)="RC"
03050   goto L1810
03060 ! ______________________________________________________________________
03070 SRCH: ! 
03080   let bk=0
03090 L3090: print newpage
03100   let fnopenwin(win=102,10,15,14,65,cap$)
03110   let prtall=0
03120   print #win,fields "4,2,C 39,N": "Starting Report Number (blank for all):"
03130   print fields "15,34,C 11,B,5": "Cancel (F5)"
03140 L3140: input #win,fields "4,42,C 2,UT,N": nam$
03150   if cmdkey=5 then goto SRCHEND
03160   let nam$=lpad$(rtrm$(nam$),2)
03170   restore #1,search>=nam$: nokey L3140
03180   close #win: ioerr L3190
03190 L3190: print newpage
03200   print fields "1,2,C 6,R,N": "Rep #:"
03210   print fields "1,9,C 40,R,N": "Report Name"
03220   let cde=0
03230   for j=1 to 20
03240     read #1,using L410: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs eof SREND
03250     let cde=1
03260     print fields str$(j+1)&",2,N 2,N": rn
03270     print fields str$(j+1)&",9,C 40,N": rt$(1:40)
03280     if j>1 then goto L3320
03290     let bk=bk+1
03300     if bk>20 then let bk=1
03310     let bk$(bk)=bl$(2)(1:28)
03320 L3320: next j
03330 SREND: if j>1 then let j=j-1
03340   mat in2$(j)
03350   mat fkey$=("") !:
        let fkey$(1)="Next" !:
        let fkey$(2)="Back" !:
        let fkey$(5)="Stop" !:
        let em$="or Select Report Number:" !:
        let es=2 !:
        let fnfkey(24,mat fkey$,mat disfk,em$,es)
03360 L3360: input fields "24,67,C 2,UT,N": k$
03370   if cmdkey=5 then goto SRCHEND
03380   if rtrm$(k$)><"" then let bl$=k$ : goto SRCHEND
03390   if cmdkey><2 then goto L3440
03400   let bk=bk-1
03410   if bk<1 then goto L3460
03420   restore #1,key>=bk$(bk): nokey L3460
03430   let bk=bk-1
03440 L3440: let selclp=1
03450   goto L3190
03460 L3460: let selclp=0
03470   goto L3090
03480 ! ______________________________________________________________________
03490 SRCHEND: if rtrm$(k$)="" then goto L3550
03500   let rn=rx=rptn=val(k$) conv L3550
03510   let rptn$=lpad$(rtrm$(k$),2)
03520   read #1,using L410,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L3360
03530   let ti=1 : goto L520
03540   close #101: ioerr L3550
03550 L3550: goto MENU1
03560 ! ______________________________________________________________________
03570 XIT: let fnxit
03580 ! ______________________________________________________________________
03590 ! <Updateable Region: ERTN>
03600 ERTN: let fnerror(program$,err,line,act$,"xit")
03610   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03620   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03630   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
03640 ERTN_EXEC_ACT: execute act$ : goto ERTN
03650 ! /region
03660 ! ______________________________________________________________________
