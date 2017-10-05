00020 ! 
00030   on error goto L2650
00040   library 'S:\Core\Library': fncno,fnxit,fntop,fncloseprn,fnopenprn,fnconsole,fnsearch
00050   fntop(program$,cap$="Enter Time")
00060   fncno(cno,cnam$)
00065   fnconsole(1)
00070   dim scr1$(10),fl1$(12),io1$(10),scrid$(2)*60,inp(7),iv$*12,a1$*30,app(20)
00080   dim fl2$(10),scr2$(7)*36,e$*9,e1$*25,he$*9,r(11),des$*30,cnam$*40,de$*30
00090   dim a$(5)*30,ph$*12,ss$*11,dd(10),sc(10),ca(10),ph2$*12,ss2$*11,ar(5),arta(2),cm$*70,app(40),ma(40),cap$*128
00100   ar(5)=1 ! set any new customers up as a balance forward
00110   ntab=60-int(len(ltrm$(rtrm$(cnam$)))/2)
00120   fl1$(11)="2,10,C 60,H,N"
00130   fl1$(12)="3,6,C 60,H,N"
00140   fl2$(5)="2,10,C 60,H,N"
00150   fl2$(6)="14,10,C 60,H,N"
00160   fl2$(7)="15,10,C 60,H,N"
00170   io1$(1)="5,25,N 5,EUT,N"
00180   io1$(2)="6,25,N 9,EuT,N"
00190   io1$(3)="7,25,N 7.2,XeuT,N"
00200   io1$(4)="8,25,N 7.2,peu,N"
00210   io1$(5)="9,25,N 10.2,EuT,N"
00220   io1$(6)="10,25,N 6,EuT,N"
00230   io1$(7)="11,25,N 2,EuT,N"
00240   io1$(8)="12,25,N 2,EuT,N"
00250   io1$(9)="13,25,N 4,EuT,N"
00260   io1$(10)="14,25,C 30,EuT,N"
00270   for j=1 to 10
00280     fl1$(j)=str$(j+4)&",10,C 20"
00290     if j<8 then fl2$(j)=str$(j+4)&",2,C 40"
00300   next j
00310   fl2$(8)="5,25,N 10.2,ut,N"
00320   fl2$(9)=fl1$(11)
00330   fl2$(10)="15,2,C 60"
00340   data "CLIENT #"
00350   data "EMPLOYEE #"
00360   data "HOURS"
00370   data "RATE"
00380   data "AMOUNT"
00390   data "DATE"
00400   data "CATEGORY"
00410   data "MONTH CODE"
00420   data "SERVICE CODE"
00430   data "DESCRIPTION"
00440   read mat scr1$ ioerr L2650
00450   data "TOTAL HOURS ENTERED IS "
00460   data "ENTER 1 TO INPUT TIME SHEETS"
00470   data "      2 TO INPUT OTHER CHARGES"
00480   data "      3 TO INPUT ADJUSTMENTS"
00490   data "      4 FOR A LISTING OF ENTRIES"
00500   data "      5 TO MAKE CORRECTIONS"
00510   data "      6 TO MERGE TRANSACTIONS"
00520   read mat scr2$ ioerr L2650
00530   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2650
00540   open #32: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00550   open #2: "Name="&env$('Q')&"\TMmstr\TMWK"&wsid$&".H"&str$(cno),internal,outin,relative ioerr L570
00560   close #2,free: 
00570 L570: open #2: "Name="&env$('Q')&"\TMmstr\TMWK"&wsid$&".H"&str$(cno)&",RecL=86,REPLACE",internal,outin,relative ioerr L2650
00580   open #7: "Name="&env$('Q')&"\TMmstr\SCMSTR.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\SCIndex.H"&str$(cno)&",Shr",internal,input,keyed ioerr L2650
00590   open #8: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2650
00600 L600: pr newpage
00610   scrid$(1)="TIME MANAGEMENT INPUT SUB-MENU"
00620   scrid$(2)="ENTER THE SELECTION NUMBER OF YOUR CHOICE"
00630   pr f mat fl2$: mat scr2$,thrs,mat scrid$
00640   pr f "22,2,Cc 60,B,1": "Continue (F1)"
00650   pr f "24,2,Cc 60,B,5": "Abort and drop all inputed transactions (F5)"
00660 L660: input fields "15,45,N 1,EU,N": b7 conv L660
00670   if cmdkey=5 then goto XIT
00680   if cmdkey>1 then goto L660
00690   chg=0
00700   mat inp=(0)
00710   on b7 goto L720,L720,L720,L2230,L2110,L2620 none L660
00720 L720: scrid$(1)="TIME MANAGEMENT "&scr2$(b7+1)(12:36)
00730   scrid$(2)="Enter CLIENT # as -1 to return to sub menu."
00740 L740: inp(1)=0
00750   inp(3)=0
00760   inp(4)=0
00770   inp(7)=0
00780   inp(5)=0
00790   expcode=0
00800   des$=""
00810   sc=0
00820   b8=0
00830 L830: pr newpage
00840 L840: pr f mat fl1$: mat scr1$,mat scrid$
00850   if chg=2 and sum(inp)=0 then inp(1)=-1
00860   io1$(2)="6,25,N 9,UET,N"
00870   io1$(3)="7,25,N 7.2,XeuT,N"
00880   io1$(5)="9,25,N 10.2,uT,N"
00890   e1$=a1$=" "
00900   pr f mat io1$: mat inp,b8,sc,des$
00910   pr f "22,13,C 45,R,N": "F1 Continue; F4 Search; F5 Complete"
00920   if chg=2 then goto L1280
00930 L930: inp(4)=75
00931   sno=1: rinput fields mat io1$,attr "R": mat inp,b8,sc,des$ conv CONV1
00933   if cmdkey=4 then goto TMSRCH
00934   if cmdkey=5 then inp(1)=-1
00940   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
00950   if cmdkey>0 or curfld=2 then goto L1020 else ce=curfld
00960 L960: ce=ce+1: if ce>udim(io1$) then ce=1
00970 L970: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L960
00980   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L930
00990 CONV1: if ce>0 then io1$(ce)(ce1:ce2)="U"
01000   ce=cnt+1
01010 ERR1: pr f "24,78,C 1": bell : goto L970
01020 L1020: if cmdkey=6 then hce=curfld : goto SRCH1
01030   if inp(1)<0 then goto L1390
01040   if inp(1)<=0 then goto L1180
01050   k$=lpad$(str$(inp(1)),5)
01060   if cmdkey=2 and err=4272 then goto L1070 else goto L1140
01070 L1070: pr f "4,45,c 30,n": "Enter customer name:"
01080   input fields "5,50,c 30,ue,n": a$(1)
01090   write #1,using L1100: k$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma
01100 L1100: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,x 60,20*n 1,pos 474,20*pd 3.2,x 20,20*pd 3.2
01110   pr f "4,45,c 30,n": " "
01120   pr f "5,50,c 30,n": " "
01130   pr f "23,45,C 35,N": " "
01140 L1140: read #1,using L1560,key=k$: a1$ nokey L1160 ioerr L2650
01150   goto L1180
01160 L1160: ce=1
01170   goto L1320
01180 L1180: if inp(2)=0 then goto L1240
01190   e$=lpad$(str$(inp(2)),9)
01200   read #8,using L1670,key=e$,release: e1$,mat r nokey L1220 ioerr L2650
01210   goto L1240
01220 L1220: ce=2
01230   goto L1320
01240 L1240: pr f "5,40,c 30,n": a1$
01250   pr f "6,40,c 25,n": e1$
01260   io1$(2)="6,25,N 9,u,N"
01270   ce=3: goto L1320
01280 L1280: inp(4)=75 ! current rate
01282 L1282: sno=2: rinput fields mat io1$,attr "R": mat inp,b8,sc,des$ conv CONV2
01290   if ce>0 then io1$(ce)(ce1:ce2)="U": ce=0
01300   if cmdkey>0 then goto L1370 else ce=curfld
01302   if ce=3 then inp(5)=round(inp(3)*inp(4),2)
01303   if ce=7 and inp(7)=6 then sc=601
01304   if ce=7 and inp(7)=2 then sc=201
01310 L1310: ce=ce+1: if ce>udim(io1$) then ce=1
01320 L1320: io1$(ce)=rtrm$(uprc$(io1$(ce))) : ce1=pos(io1$(ce),"U",1) : if ce1=0 then goto L1310
01330   ce2=ce1+1 : io1$(ce)(ce1:ce1)="UC" : goto L1282
01340 CONV2: if ce>0 then io1$(ce)(ce1:ce2)="U"
01350   ce=cnt+1
01360 ERR2: pr f "24,78,C 1": bell : goto L1320
01370 L1370: if cmdkey=6 then hce=curfld : goto SRCH1
01380   if env$('client')="ACS" and (inp(7)=6 or inp(7)=2) and b8=0 then ce=8: goto ERR2
01390 L1390: if inp(1)=-1 and chg><2 then goto L600
01400   if inp(1)=-1 then mat inp=(0) else goto L1480
01410   b6=0
01420   b7=0
01430   b8=0
01440   sc=0
01450   iv$=" "
01460   nta=0
01470   goto L2090
01480 L1480: if rtrm$(des$)<>"" then goto L1490
01490 L1490: if b8<0 or b8>29 then ce=8: goto ERR2
01500   if inp(7)<1 or inp(7)>30 then ce=7: goto ERR2
01510   if inp(6)<10182 or inp(6)>123199 then ce=6: goto ERR2
01520   if inp(1)=0 then goto L1620
01530   ce=1
01540   k$=lpad$(str$(inp(1)),5)
01550   read #1,using L1560,key=k$: a1$,mat app nokey ERR2 ioerr L2650
01560 L1560: form pos 6,c 30,pos 375,20*n 1,x 60,20*n 1
01570   if env$('client')="ACS" and (b8=0 or app(b8)=1) then goto L1610
01572   if b8=0 then goto L1610
01580   if b8>0 and b8<31 then goto L1610
01590   ce=8
01600   goto ERR2
01610 L1610: ce=0
01620 L1620: if b7=2 then goto L1790
01630   ce=2
01640   e$=lpad$(str$(inp(2)),9)
01650   if e$=he$ then goto L1680
01660   read #8,using L1670,key=e$,release: e1$,mat r nokey ERR2 ioerr L2650
01670 L1670: form pos 10,c 25,pos 578,11*pd 3.2
01680 L1680: ce=0
01690   he$=e$
01700   if inp(7)>10 then r1=11 else r1=inp(7)
01710   if r1=0 then goto L1760
01720   if inp(3)=0 then goto L1780
01730   if inp(4)=0 then inp(4)=r(r1) else goto L1750
01740   goto L1760
01750 L1750: if r(r1)=0 then r(r1)=inp(4): rewrite #8,using L1670,key=e$: e1$,mat r
01760 L1760: if inp(4)=0 then ce=4: goto ERR2
01770   if inp(5)<>0 then goto L1790
01780 L1780: if inp(3)=0 and inp(4)=0 then goto L1790 else inp(5)=inp(3)*inp(4)
01790 L1790: ! 
01800   de$=""
01810   if sc=0 then goto L1870
01820   ce=9
01830   read #7,using L1840,key=lpad$(str$(sc),4): de$ nokey ERR2 ioerr L2650
01840 L1840: form pos 5,c 30
01850   if ltrm$(des$)="" then des$=de$
01860   ce=0
01870 L1870: if chg=2 then goto L2090
01880   rw=rw+1
01882   pause 
01890   if expcode=0 then write #2,using L1900,rec=rw: mat inp,b6,b7,b8,sc,iv$,0,des$ else write #2,using L1900,rec=rw: mat inp,b6,2,b8,sc,iv$,0,des$
01900 L1900: form pos 1,n 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,c 12,pd 3,c 30
01910   thrs=thrs+inp(3)
01915   if env$('client')<>"ACS" then goto L2080 ! only ask phone on acs
01920   expcode=1
01930   if pe$="Y" then goto L2080
01940   close #101: ioerr L1950
01950 L1950: open #101: "SROW=20,SCOL=14,EROW=20,ECOL=60,BORDER=DR",display,outin 
01960   pr #101: newpage
01970   pr f "20,15,C 44": "Do you wish to record PHONE EXPENSE (Y/N):"
01980 L1980: pe$="N" ! input fields "20,58,CU 1,UEA,N": pe$
01990   if pe$="Y" then close #101: : goto L2020
02000   if pe$="N" then close #101: : goto L2080
02010   goto L1980
02020 L2020: des$="TELEPHONE EXPENSE"
02030   inp(5)=inp(3)*10.
02040   inp(3)=inp(4)=0
02050   ce=5
02060   pr f mat io1$: mat inp,b8,sc,des$
02070   goto L1320
02080 L2080: pe$=" ": goto L740
02090 L2090: rewrite #2,using L1900,rec=rr: mat inp,b6,b7,b8,sc,iv$,nta,des$
02100   thrs=thrs+inp(3)
02110 L2110: pr newpage
02120   pr f "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
02130   pr f "12,12,C 40": "LAST REF # ON FILE IS: "&str$(lrec(2))
02140 L2140: input fields "10,60,n 5,eu,n": rr conv L2140
02150   chg=2
02160   if rr=0 then goto L600
02170   if rr>rw or rr<1 then goto L2140
02180   scrid$(1)="TIME MANAGEMENT CORRECTION SCREEN"
02190   scrid$(2)="Enter CLIENT # as -1 to DELETE THIS ENTRY."
02200   read #2,using L1900,rec=rr: mat inp,b6,b7,b8,sc,iv$,nta,des$ ioerr L2650
02210   thrs=thrs-inp(3)
02220   goto L830
02230 L2230: pr newpage
02240   pr f "10,10,c 60,h,n": "TIME MANAGEMENT CORRECTION LISTING IN PROCESS"
02250   fnopenprn(cp,58,220,process)
02260   if rtrm$(file$(255))(1:4)<>"PRN:" then goto L2280
02270   if cp=1 then pr #255,using L2280: hex$("2B0205000F1042") else pr #255,using L2280: hex$("2B0205000F1042")
02280 L2280: form pos 1,c 9,skip 0
02290   form c 9,skip 0
02300   pr #255,using L2310: date$,"TIME MANAGEMENT INPUT LISTING ",time$,cnam$
02310 L2310: form pos 1,c 8,pos 40,c 40,skip 1,pos 1,c 8,pos ntab,c 40,skip 2
02320   pr #255: "REF #  CLIENT #  EMPLOYEE #    HOURS       RATE     AMOUNT    DATE   CATEGORY   MONTH CODE     SERVICE-CODE   TYPE  DESCRIPTION"
02330   inp2=0
02340   tinp3=0
02350   totexp=0
02360   tottime=0
02370   thrs=0
02380   for j=1 to rw
02390     read #2,using L1900,rec=j: mat inp,b6,b7,b8,sc,iv$,nta,des$ ioerr L2650
02400     if inp(7)=0 then goto L2570
02410     if inp2=0 then goto L2470
02420     if inp2><inp(2) then pr #255,using L2430: tinp3,tottime,totexp else goto L2470
02430 L2430: form pos 21,"      ----------",skip 1,pos 10,"TOTAL HOURS",pos 27,n 10.2,skip 1,pos 10,"TOTAL LABOR",pos 27,n 10.2,skip 1,pos 10,"TOTAL EXPENSE",pos 27,n 10.2,skip 2
02440     tinp3=0
02450     totexp=0
02460     tottime=0
02470 L2470: tinp3=tinp3+inp(3)
02480     if b7=2 then totexp=totexp+inp(5) else tottime=tottime+inp(5)
02490     k$=lpad$(str$(inp(1)),5)
02500     a1$=""
02510     read #1,using L1560,key=k$: a1$ nokey L2530 ioerr L2530
02520     if des$(1:5)="TELEP" then a1$=des$
02530 L2530: pr #255,using L2540: j,mat inp,b8,sc,b7,a1$
02540 L2540: form pos 1,n 5,n 8,n 12,3*n 11.2,n 9,2*n 9,x 12,n 4,x 4,n 8,x 3,c 30,skip 1
02550     thrs=thrs+inp(3)
02560     inp2=inp(2)
02570 L2570: next j
02580   pr #255,using L2430: tinp3,tottime,totexp
02590   dim sendto$*80
02600   fncloseprn ! sENDTO$=FILE$(255): pr #255: NEWPAGE : Close #255: : If SENDTO$(1:4)<>"PRN:" Then Execute "SY START /W "&SENDTO$ : Execute "DROP "&SENDTO$&" -N"
02610   goto L600
02620 L2620: close #1: 
02630   close #2: 
02640   chain "S:\acsTM\TMMRGINP"
02650 L2650: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2670
02660   goto L2710
02670 L2670: pr newpage
02680   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2700
02690   goto L2710
02700 L2700: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02710 L2710: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02720   input fields "24,60,C 1,N": quitcode$
02730   if err=61 and rtrm$(uprc$(quitcode$))="Q" then goto L600
02740   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
02750   pr f "23,3,C 78,N": ""
02760   pr f "24,3,C 78,N": ""
02770   retry 
02780 XIT: pr newpage : fnxit
02790   dim bk$(20)*30,nam$*25,a1$*30
02800 SRCH1: s1=1 ! NAME SEARCH
02810   open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outin  ! SAVE SCREEN
02820 L2820: pr #127: newpage
02830   close #101: ioerr L2840
02840 L2840: open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=BUSINESS NAME SEARCH",display,outin 
02850   prtall=0
02860   pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
02870   pr f "9,32,C 16,R,N": "Press F5 to stop"
02880 L2880: input fields "7,50,C 25,UE,N": nam$
02890   if cmdkey=5 then goto SRCHEND
02900   nam$=rtrm$(nam$)
02910   l1=len(nam$)
02920   restore #32,search>=nam$: nokey L2880
02930   close #101: ioerr L2940
02940 L2940: pr newpage
02950   pr f "1,10,C 5,R,N": "ACCT#"
02960   pr f "1,17,C 30,R,N": "COMPANY NAME"
02970   cde=0
02980   for j=1 to 20
02990     read #32,using L3000,release: k$,a1$ eof L3100
03000 L3000: form pos 1,c 5,c 30
03010     if a1$(1:l1)=nam$ or prtall=1 then goto L3020 else goto L3100
03020 L3020: cde=1
03030     pr f str$(j+1)&",10,C 5,ut,N": k$
03040     pr f str$(j+1)&",17,C 30,ut,N": a1$
03050     if j>1 then goto L3090
03060     bk=bk+1
03070     if bk>20 then bk=1
03080     bk$(bk)=a1$
03090 L3090: next j
03100 L3100: if j>1 then j=j-1
03110   mat in2$(j)
03120   pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
03130 L3130: input fields "24,58,N 5,RE,N": k1 conv L3130
03140   alp=0
03150   if cmdkey=5 then goto SRCHEND
03160   if rtrm$(k$)="" then goto L3200
03170   k$=lpad$(str$(k1),5)
03180   read #1,using L1560,key=k$: a1$,mat app nokey L3130
03190   goto SRCHEND
03200 L3200: if cmdkey><2 then goto L3250
03210   bk=bk-1
03220   if bk<1 then goto L3270
03230   restore #32,key>=bk$(bk): nokey L3270
03240   bk=bk-1
03250 L3250: selclp=1
03260   goto L2940
03270 L3270: selclp=0
03280   goto L2820
03290 SRCHEND: close #101: ioerr L3300
03300 L3300: close #127: ioerr L3310
03310 L3310: ce=hce
03320   if k1=0 then goto L3350
03330   pr f io1$(1): k1
03340   pr f "5,40,c 30,n": a1$
03350 L3350: on sno goto L970,L1320
04800 TMSRCH: ! search for customer #
04810   dim heading$*70,form$*80,numeric_format$*20,selection$*70
04820   file_num=32 ! alpha index on clients
04830   form$="form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2"
04840   numeric_format$='pic($$$,$$$.##)'
04850   key_length=5
04860   heading$="Acct #횼ame컴컴컴컴컴컴컴컴컴컴Address컴컴컴컴Balance"
04870   fnsearch(cap$,file_num,heading$,form$,numeric_format$,selection$,key_length)
04880   k$=z$=selection$ ! pull key from first field in search line
04890   inp(1)=0
04900   inp(1)=val(selection$) conv L4910
04910 L4910: goto L840
