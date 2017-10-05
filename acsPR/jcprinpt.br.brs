00010 ! Replace S:\acsPR\jcprInpt     !    I *think* this program is obsoluete
00020 ! Transfer Job Cost to Payroll
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnoldmsgbox,fnopenprn,fncloseprn,fncno,fnerror,fnchain
00050   on error goto ERTN
00060   on fkey 5 goto L2440
00070 ! ______________________________________________________________________
00080   dim inp(19),iolabel1$(21),io1$(23),io1b$(19),em$*30,label1$(21)*21
00090   dim en$*8,ta(2),tdet(13),chg$(2)*1,tinp(19),std$*27,f1$*204,wrd2$(4)*29
00100   dim f2$*204,pr(9,25),hen$*8,cap$*128,message$*40,hr(2),h(7)
00110   dim msgline$(2)*60,response$(5)*1
00120 ! ______________________________________________________________________
00130   fntop(program$,cap$="Transfer Job Cost to Payroll")
00140   fncno(cno)
00150 ! 
00160 ! ______________________________________________________________________
00170   open #1: "Name="&env$('Q')&"\PRmstr\RPNAMES.H"&str$(cno)&",Shr",internal,input 
00180   read #1,using L190: label1$(7),label1$(8),label1$(9),label1$(10),label1$(11),label1$(12),label1$(13),label1$(14),label1$(15),label1$(16),label1$(17)
00190 L190: form pos 741,11*c 20
00200   close #1: 
00210 ! ______________________________________________________________________
00220   gosub SORTIT
00230   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,outin,keyed 
00240   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",Shr",internal,outin,relative 
00250   open #3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&str$(cno)&",SIZE=0,RecL=117,Replace",internal,output 
00260   open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input 
00270   open #5: "Name="&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno),internal,input,relative 
00280   open #6: "Name=ADDR2."&wsid$,internal,input 
00290   f1$="FORM POS 1,C 20"
00300   let f2$=f1$
00310   for j=1 to 9
00320     f1$=rtrm$(f1$)&",N 12.2"
00330     let f2$=rtrm$(f2$)&",N 12.2"
00340   next j
00350   for j=1 to 14
00360     iolabel1$(j)=str$(j+6)&",2,Cr 21,N"
00370     io1$(j)=str$(j+6)&",24,N 10.2,UT,N"
00380     io1b$(j)=str$(j+6)&",24,N 10.2,N"
00390   next j
00400   for j=15 to 21
00410     iolabel1$(j)=str$(j-8)&",36,Cr 21,N"
00420     io1$(j)=str$(j-8)&",58,N 10.2,UT,N"
00430     if j>19 then goto L450
00440     io1b$(j)=str$(j-8)&",58,N 10.2,N"
00450 L450: next j
00460   io1$(22)="15,67,Cu 1,UT,N"
00470   io1$(23)="16,67,Cu 1,UT,N"
00480   label1$(01)="Regular Hours"
00490   label1$(02)="Overtime Hours"
00500   label1$(03)="Sick Hours"
00510   label1$(04)="Vacation Hours"
00520   label1$(05)="Holiday Hours"
00530   label1$(06)="Salary"
00540   label1$(07)="Other Compensation"
00550   label1$(18)="Meals"
00560   label1$(19)="Tips"
00570   label1$(20)="Regular Hourly Rate"
00580   label1$(21)="Overtime Hourly Rate"
00590   for j=1 to 21: label1$(j)=rtrm$(label1$(j))&":" : next j
00600 L600: read #4,using L610: jci eof L1740
00610 L610: form pos 1,pd 3
00620   read #5,using L630,rec=jci: mat h,dt2,jn$ norec L600
00630 L630: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
00640   if h(1)><eno or h3><h(3) then goto L840
00650 L650: h2=h(2)
00660   if h2=1 then goto L690
00670   inp(1)=inp(1)+h(4)
00680   inp(2)=inp(2)+h(5)
00690 L690: eno=h(1)
00700   h3=h(3)
00710   if h(7)=11 then goto L740
00720   if h(7)=0 then goto L600 else inp(h(7)+7)=inp(h(7)+7)+h(6)
00730   goto L600
00740 L740: inp(7)=inp(7)+h(6)
00750   goto L600
00760 ! ______________________________________________________________________
00770 L770: pr newpage
00780   let win=101
00790   fnopenwin(win,10,20,14,59,cap$)
00800   pr #win,fields "4,2,C 16,N": "Employee Number:"
00810   pr f "15,35,c 09,B,5": "Done (F5)"
00820 L820: input #win,fields "4,21,N 8,UT,N": eno conv L820
00830 L830: if cmdkey=5 or eno=0 then goto TOTALSCREEN
00840 L840: if eno=0 then goto L650
00850 L850: en$=lpad$(str$(eno),8)
00860   read #1,using L870,key=en$: em$,em8,em9,tgp,mat ta nokey L650
00870 L870: form pos 9,c 30,pos 126,2*pd 3.3,pos 168,pd 5.2,2*pd 3
00880   if s9=1 then goto L920
00890   if r><0 then goto L910
00900   if eno=eno2 then goto L920
00910 L910: tgp=0
00920 L920: if s9=0 then adr=ta(1)
00930 L930: read #2,using L940,rec=adr: dep,mat tdet,nta
00940 L940: form pos 9,n 3,pos 58,13*pd 4.2,pos 468,pd 3
00950   if s9=1 or r=1 then goto L970
00960   if dep><h3 then goto L1630 ! CHANGE TO "if dep><h3 then mat inp=(0)" TO ASK ALL DEPARTMENTS ON INPUT
00970 L970: if s9=1 then goto L1010
00980   std$="Skip this Department (Y/N):"
00990   goto L1070
01000 ! ______________________________________________________________________
01010 L1010: std$="Delete this Entry (Y/N):"
01020   tdet(2)=hr(1)
01030   tdet(3)=hr(2)
01040   tgp=tgp-gpd
01050   goto L1110
01060 ! ______________________________________________________________________
01070 L1070: if h2=1 or h2=3 then inp(6)=tdet(1)
01080   for j=1 to 10
01090     inp(j+7)=inp(j+7)+tdet(j+3)
01100   next j
01110 L1110: pr newpage
01120   let win=101
01130   fnopenwin(win,2,7,22,74,cap$)
01140   pr #win,fields "04,02,Cr 21,N": "Employee Number:" !:
        pr #win,fields "04,24,C 08,N": ltrm$(en$)
01150   pr #win,fields "05,02,Cr 21,N": "Employee Name:" !:
        pr #win,fields "05,24,C 30,N": rtrm$(em$)
01160   pr #win,fields "06,02,Cr 21,N": "Department Number:" !:
        pr #win,fields "06,24,C 03,N": str$(dep)
01170   pr #win,fields mat iolabel1$: mat label1$
01180   pr #win,fields "15,36,Cr 29,N": std$
01190   pr #win,fields "16,36,Cr 29,N": "Make Changes Permanent (Y/N):"
01200   pr #win,fields mat io1$: mat inp,tdet(2),tdet(3)
01210   pr f "23,22,C 09,B,1": "Next (F1)"
01220   pr f "23,32,C 25,B,5": "Cancel (no transfer) (F5)"
01230 L1230: input #win,fields mat io1$: mat inp,mat hr,mat chg$ conv CONV1
01240   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
01250   if cmdkey>0 then goto L1320 else ce=curfld
01260 L1260: ce=ce+1: if ce>udim(io1$) then ce=1
01270 L1270: io1$(ce)=rtrm$(io1$(ce)) !:
        ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L1260
01280   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1230
01290 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
01300   ce=cnt+1
01310 ERR1: pr f "24,78,C 1": bell : goto L1270
01320 L1320: if cmdkey=5 then goto XIT
01330   if chg$(1)="Y" and s9=1 then goto L2590
01340   if chg$(1)="Y" then goto L1630
01350   if chg$(2)="N" then goto L1430
01360   tdet(1)=inp(6)
01370   for j=1 to 10
01380     tdet(j+3)=inp(j+7)
01390   next j
01400   tdet(2)=hr(1)
01410   tdet(3)=hr(2)
01420   rewrite #2,using L940,rec=adr: dep,mat tdet,nta
01430 L1430: let gpd=0
01440   if em8><-2 then goto L1490
01450   if inp(3)=0 then goto L1490
01460   pr f "5,40,c 38": "Not Eligable for Sick Leave"
01470   goto L1230
01480 ! ______________________________________________________________________
01490 L1490: if em9><-2 then goto L1540
01500   if inp(4)=0 then goto L1540
01510   pr f "6,40,c 38": "Not Eligible for Vacation"
01520   goto L1230
01530 ! ______________________________________________________________________
01540 L1540: for j=1 to 5
01550     if j=2 then let gpd=gpd+inp(j)*hr(2) else let gpd=gpd+inp(j)*hr(1)
01560   next j
01570   let gpd=gpd+inp(6)+inp(7)+inp(18)+inp(19)
01580   if s9=1 then goto L2630
01590   write #3,using L1600: eno,dep,mat inp,gpd,mat hr,adr
01600 L1600: form pos 1,n 8,n 3,5*pd 4.2,15*pd 5.2,2*pd 4.2,pd 3
01610   mat tinp=tinp+inp
01620   tgp=tgp+gpd
01630 L1630: adr=nta
01640   if adr>0 then goto L930
01650   rewrite #1,using L1660,key=en$: tgp nokey L1700
01660 L1660: form pos 168,pd 5.2
01670   if hen$<>en$ then ent1=ent1+1
01680   if hen$<>en$ then teno=teno+eno
01690   hen$=en$
01700 L1700: mat inp=(0)
01710   eno2=eno
01720   if end4=1 and r=0 then goto TOTALSCREEN
01730   if r=0 then goto L650 else goto L770
01740 L1740: end4=1
01750   goto L830
01760 ! ______________________________________________________________________
01770 TOTALSCREEN: ! 
01780   pr newpage
01790   let win=101
01800   fnopenwin(win,2,7,22,74,cap$)
01810   close #3: 
01820   open #3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&str$(cno),internal,outin,relative 
01830   label1$(20)=" "
01840   label1$(21)=" "
01850   pr #win,fields "04,02,Cr 34,N": "Total or Employee Numbers Entered:" !:
        pr #win,fields "04,37,C 10,N": str$(teno)
01860   pr #win,fields "05,02,Cr 34,N": "Number of Employee Entered:" !:
        pr #win,fields "05,37,C 10,N": str$(ent1)
01870   pr #win,fields mat iolabel1$: mat label1$
01880   pr #win,fields mat io1b$: mat tinp
01890   let wrd2$(1)="1. Make Corrections"
01900   let wrd2$(2)="2. pr Proof Listing"
01910   let wrd2$(3)="3. Calculate Pay"
01920   let wrd2$(4)="4. pr Daily Employee Hours"
01930   for j=1 to udim(wrd2$)
01940     io2$(j)=str$(j+14)&",38,C 29,N"
01950   next j
01960   pr f "23,27,C 25,B,5": "Cancel (no transfer) (F5)"
01970 L1970: rinput #win,select mat io2$,attr "H": mat wrd2$
01980   cor=curfld
01990   if cmdkey=5 then goto XIT
02000   label1$(20)="Reg Hourly Rate"
02010   label1$(21)="O/T Hourly Rate"
02020   on cor goto L2450,L2040,L2810,L3110 none L1970
02030 ! ______________________________________________________________________
02040 L2040: r=0
02050   pc=0
02060   pr newpage
02070   fnwait(101,cap$,message$,1)
02080   fnopenprn(cp,58,220,process)
02090 L2090: r=r+1
02100   read #3,using L1600,rec=r: eno,dep,mat inp,gpd,mat hr,adr eof L2430,norec L2430
02110   if pc=9 then gosub L2240
02120   pc=pc+1
02130   pr(pc,1)=eno
02140   pr(pc,2)=dep
02150   pr(pc,22)=gpd
02160   pr(pc,23)=r
02170   pr(pc,24)=hr(1)
02180   pr(pc,25)=hr(2)
02190   for j=1 to 19
02200     pr(pc,j+2)=inp(j)
02210   next j
02220   goto L2090
02230 ! ______________________________________________________________________
02240 L2240: pc2=pc2+1
02250   if pc2<3 then goto L2270 else pr #255: newpage
02260   pc2=1
02270 L2270: pr #255,using L2280: " "
02280 L2280: form c 1,skip 4
02290   pr #255,using f1$: "Record #     ",pr(1,23),pr(2,23),pr(3,23),pr(4,23),pr(5,23),pr(6,23),pr(7,23),pr(8,23),pr(9,23)
02300   pr #255,using f1$: "Employee #",pr(1,1),pr(2,1),pr(3,1),pr(4,1),pr(5,1),pr(6,1),pr(7,1),pr(8,1),pr(9,1)
02310   pr #255,using f1$: "Department #",pr(1,2),pr(2,2),pr(3,2),pr(4,2),pr(5,2),pr(6,2),pr(7,2),pr(8,2),pr(9,2)
02320   for j=1 to 19
02330     pr #255,using f2$: label1$(j),pr(1,j+2),pr(2,j+2),pr(3,j+2),pr(4,j+2),pr(5,j+2),pr(6,j+2),pr(7,j+2),pr(8,j+2),pr(9,j+2)
02340   next j
02350   pr #255,using f2$: "Dept Gross Pay ",pr(1,22),pr(2,22),pr(3,22),pr(4,22),pr(5,22),pr(6,22),pr(7,22),pr(8,22),pr(9,22)
02360   pr #255,using f2$: "Reg Hourly Rate",pr(1,24),pr(2,24),pr(3,24),pr(4,24),pr(5,24),pr(6,24),pr(7,24),pr(8,24),pr(9,24)
02370   pr #255,using f2$: "O/T Hourly Rate",pr(1,25),pr(2,25),pr(3,25),pr(4,25),pr(5,25),pr(6,25),pr(7,25),pr(8,25),pr(9,25)
02380   pr #255,using L2280: " "
02390   mat pr=(0)
02400   pc=0
02410   return 
02420 ! ______________________________________________________________________
02430 L2430: gosub L2240
02440 L2440: fncloseprn
02450 L2450: let win=101
02460   fnopenwin(win,10,20,14,59,cap$)
02470   pr #win,fields "4,2,C 24,N": "Record Number to Change:"
02480   pr f "15,35,C 09,B,5": "Done (F5)"
02490 L2490: input #win,fields "4,27,N 5,UT,N": r conv L2490
02500   close #win: ioerr L2510
02510 L2510: if cmdkey=5 or r=0 then goto L2700
02520   read #3,using L1600,rec=r: eno,dep2,mat inp,gpd,mat hr,adr norec L2450,eof L2450
02530   teno=teno-eno
02540   mat tinp=tinp-inp
02550   if eno=0 then goto L2450
02560   s9=1
02570   goto L850
02580 ! ______________________________________________________________________
02590 L2590: eno=0
02600   dep=0
02610   mat inp=(0)
02620   let gpd=0
02630 L2630: rewrite #3,using L1600,rec=r: eno,dep,mat inp,gpd,mat hr,adr
02640   tgp=tgp+gpd
02650   teno=teno+eno
02660   mat tinp=tinp+inp
02670   if chg$(1)><"Y" then rewrite #1,using L1660,key=en$: tgp
02680   goto L2450
02690 ! ______________________________________________________________________
02700 L2700: pr newpage
02710   msgline$(1)="Add another Employee? (Y/N)" !:
        msgline$(2)=""
02720   fnoldmsgbox(mat response$,cap$,mat msgline$,2)
02730   if response$(1)="N" then goto TOTALSCREEN
02740   r=0
02750   close #3: 
02760   open #3: "Name="&env$('Q')&"\PRmstr\rpwork"&wsid$&".h"&str$(cno),internal,output 
02770   s9=0
02780   eno=dep=gpd=0: mat inp=(0): mat hr=(0)
02790   goto L770
02800 ! ______________________________________________________________________
02810 L2810: pr newpage
02820   close #1: 
02830   close #2: 
02840   close #3: 
02850   close #4: 
02860   close #5: 
02870   open #5: "Name="&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno),internal,output ioerr L2890
02880   close #5,free: 
02890 L2890: open #5: "Name="&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno)&",SIZE=0,RecL=40",internal,output 
02900   close #5: 
02910   fnchain("S:\acsPR\prCalk")
02920 ! ______________________________________________________________________
02930 XIT: fnxit
02940 ! ______________________________________________________________________
02950 SORTIT: ! Replace SORTIT
02960   open #1: "Name=Sort"&wsid$&".tmp,RecL=128,Replace",internal,output 
02970   open #2: "Name="&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno),internal,input 
02980   write #1,using L3030: "! SORT FOR TRANSFER JC TO PR IN PROCESS"
02990   write #1,using L3030: "FILE "&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno)&",,,"&env$('Temp')&"\Addr."&session$&",,,acsPR,,A,N"
03000   write #1,using L3030: "MASK 1,8,N,A,10,2,PD,A"
03010   write #1,using L3030: "FILE "&env$('Q')&"\PRmstr\JCPRH1.H"&str$(cno)&",,,ADDR2."&wsid$&",,,acsPR,,A,N"
03020   write #1,using L3030: "MASK 1,8,N,A,27,14,C,A"
03030 L3030: form pos 1,c 128
03040   close #1: 
03050   execute "FREE "&env$('Temp')&"\Addr."&session$&" -n" ioerr L3060
03060 L3060: execute "FREE ADDR2."&wsid$&" -n" ioerr L3070
03070 L3070: close #2: 
03080   execute "SORT Sort"&wsid$&".tmp -n"
03090   return 
03100 ! ______________________________________________________________________
03110 L3110: pr newpage
03120   let win=101
03130   fnopenwin(win,10,14,14,65,cap$)
03140   pr #win,fields "4,02,C 41,N": "Employee Number to pr (blank for all):"
03150   pr f "15,34,C 11,B,5": "Cancel (F5)"
03160 L3160: input #win,fields "4,44,Nz 8,UT,N": en1 conv L3160
03170   if cmdkey=5 then goto TOTALSCREEN
03180   fnopenprn(cp,58,220,process)
03190   eno=en2=dt2=t1=t2=t3=t4=0
03200   restore #6: 
03210   gosub HDR
03220 L3220: read #6,using L3230: jci eof END_OF_FILE
03230 L3230: form pos 1,pd 3
03240   read #5,using L3250,rec=jci: mat h,dt1,jn$ norec L3220
03250 L3250: form pos 1,n 8,n 1,pd 2,2*pd 4.2,pd 5.2,n 2,n 8,c 6
03260   if en1=0 then goto L3290
03270   if en2>0 and en1><h(1) then goto L3470
03280   if en1><h(1) then goto L3220
03290 L3290: if h(1)><eno or dt1><dt2 then gosub T1
03300   if h(1)><eno then gosub T2
03310   if eno=0 or h(1)<>eno then goto L3320 else goto L3370
03320 L3320: en$=lpad$(str$(h(1)),8)
03330   em$=""
03340   read #1,using L870,key=en$: em$ nokey L3370
03350   pr #255,using L3360: em$
03360 L3360: form pos 1,c 40,skip 1
03370 L3370: pr #255,using L3380: h(1),dt1,jn$,h(4),h(5) pageoflow PGOF
03380 L3380: form pos 1,n 8,pic(zz####/##/##bb),c 6,2*n 8.2,skip 1
03390   eno=en2=h(1)
03400   dt2=dt1
03410   t1=t1+h(4)
03420   t2=t2+h(5)
03430   t3=t3+h(4)
03440   t4=t4+h(5)
03450   goto L3220
03460 ! ______________________________________________________________________
03470 L3470: gosub T1
03480   gosub T2
03490   goto TOTALSCREEN
03500 ! ______________________________________________________________________
03510 T1: ! 
03520   if eno=0 then goto L3570
03530   pr #255,using L3550: ""," _______" ," _______"
03540   pr #255,using L3550: "Daily Total",t1,t2
03550 L3550: form pos 9,c 20,2*g 8.2,skip 1
03560   pr #255: 
03570 L3570: t1=t2=0
03580   return 
03590 ! ______________________________________________________________________
03600 T2: ! 
03610   if eno=0 then goto L3650
03620   pr #255,using L3550: ""," _______" ," _______"
03630   pr #255,using L3550: "Employee Total",t3,t4
03640   pr #255: 
03650 L3650: t3=t4=0
03660   return 
03670 ! ______________________________________________________________________
03680 PGOF: ! 
03690   pr #255: newpage
03700   gosub HDR
03710   continue 
03720 ! ______________________________________________________________________
03730 HDR: ! 
03740   pr #255,using L3750: "Employee Time Breakdown"
03750 L3750: form pos 17,c 50,skip 1
03760   return 
03770 ! ______________________________________________________________________
03780 END_OF_FILE: ! 
03790   gosub T1
03800   gosub T2
03810   fncloseprn
03820   goto TOTALSCREEN
03830 ! ______________________________________________________________________
03840 ! <Updateable Region: ERTN>
03850 ERTN: fnerror(program$,err,line,act$,"xit")
03860   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03870   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03880   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03890 ERTN_EXEC_ACT: execute act$ : goto ERTN
03900 ! /region
03910 ! ______________________________________________________________________
