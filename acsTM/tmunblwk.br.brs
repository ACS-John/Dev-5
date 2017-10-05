00020   library "S:\Core\Library.br": fnopenprn,fncloseprn,fncno,fnxit,fntop
00040   on error goto L2780
00070   fntop(program$,cap$="Unbilled Work In Process")
00080   fncno(cno,cnam$)
00090   fnopenprn
00100   dim cnam$*40,cat$(30)*30,cliprt$*5,cap$*128
00110   dim l$(20,2)*25,l(20,2),s(13)
00120   dim z$*5,cliname$*30,ca(10),ta(25,2),fb(25),empname$*25,scdesc$*30
00130   dim cno$*5,cna$*30,en$*9,d(8)
00140   dim k$*5,e$*9,b(8),sc$*4,sc2$*6,iv$*12
00150   namtab=66-int(len(rtrm$(cnam$))/2)
00160   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,input,relative ioerr L2780
00170   read #1,using L180,rec=1: mat cat$ ioerr L2780
00180 L180: form pos 1,30*c 30
00190   close #1: 
00200   open #8: "Name="&env$('Q')&"\TMmstr\pedate.h"&str$(cno)&",RecL=20,use,Shr",internal,outin,relative 
00210   if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: dat
00220   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L2780
00230   open #2: "Name="&env$('Q')&"\TMmstr\TMTRAddr.h"&str$(cno)&",Shr",internal,input,relative ioerr L2780
00240   open #3: "Name="&env$('Q')&"\TMmstr\TMTRANS.H"&str$(cno)&",Shr",internal,input,relative ioerr L2780
00250   open #4: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L2780
00260   goto L460
00270 L270: read #4,using L280,key=e$: empname$ nokey L300 ioerr L2780
00280 L280: form pos 10,c 25
00290   return 
00300 L300: empname$="EMPLOYEE NOT ON FILE"
00310   if b(7)><2 then goto L340
00320   empname$="OTHER CHARGES"
00330   goto L450
00340 L340: if b(7)><3 then goto L370
00350   empname$="ADJUSTMENT"
00360   goto L450
00370 L370: if fb(j2)=1 or fb(j2)=2 then goto L410 else goto L380
00380 L380: if b(7)><-3 then goto L450
00390   empname$="*** WRITE OFF"
00400   goto L450
00410 L410: if b(7)><-1 then goto L440
00420   empname$="PARTIAL BILLING"
00430   goto L450
00440 L440: empname$="*** FINAL BILLED"
00450 L450: return 
00460 L460: open #5: "Name="&env$('Q')&"\TMmstr\SCMSTR.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\SCIndex.H"&str$(cno)&",Shr",internal,input,keyed ioerr L2780
00470   goto L530
00480   read #5,using L490,key=sc$: scdesc$ nokey L510 ioerr L2780
00490 L490: form pos 5,c 30
00500   goto L520
00510 L510: scdesc$=" "
00520 L520: return 
00530 L530: open #6: "Name="&env$('Q')&"\TMmstr\Work2.H"&wsid$,internal,input ioerr L550
00540   close #6,free: 
00550 L550: open #6: "Name="&env$('Q')&"\TMmstr\Work2.H"&wsid$&",SIZE=0,RecL=76",internal,output ioerr L2780
00560 L560: pr newpage
00570   in1$(1)="10,46,n 6,ute,n"
00580   in1$(2)="11,46,n 6,ute,n"
00590   pr f "10,10,cR 34,n": "ENTER AGING DATE IN MMDDYY FORMAT:"
00600   pr f "11,10,cR 34,n": "ENTER OLDEST DATE TO PRINT:"
00610   pr f mat in1$: dat,olddat
00620   pr f "12,20,c 40": "Press F1 to Continue; F5 to Cancel"
00630 L630: input fields mat in1$: dat,olddat conv L630
00640   if cmdkey=5 then goto XIT
00650   if cmdkey<>1 then goto L560
00660   if dat<10100 or dat>123199 then goto L560
00670   if olddat<10100 or olddat>123199 then goto L560
00680   rewrite #8,using "form pos 1,n 6",rec=1: dat
00690   close #8: 
00700   d7=int(dat/10000)
00710   d5=int((dat-d7*10000)/100)
00720   d8=dat-(d5*100+d7*10000)
00730   if d7>=1 and d7<=12 then goto L750
00740   goto L560
00750 L750: if d5>=1 and d5<=31 then goto L770
00760   goto L560
00770 L770: pr newpage
00780   pr f "10,10,c 57,n": "ENTER 1 IF ALL TRANSACTIONS SHOULD PRINT, ELSE ENTER 2 IF"
00790   pr f "11,10,c 56,n": "ONLY THOSE THAT HAVE NOT BEEN FINAL BILLED SHOULD PRINT."
00800 L800: input fields "11,68,n 1,ue,n": prtall conv L800
00810   if prtall<1 or prtall>2 then goto L770
00820 L820: pr newpage
00830   pr f "10,10,c 43,n": "ENTER 1 TO pr ALL CLIENTS, ELSE ENTER 2"
00840 L840: input fields "10,55,n 1,ue,n": prtcli conv L840
00850   if prtcli=1 then goto L990
00860   if prtcli><2 then goto L820
00870   open #7: "Name="&env$('Q')&"\TMmstr\Work1.h"&wsid$,internal,input ioerr L890
00880   close #7,free: 
00890 L890: open #7: "Name="&env$('Q')&"\TMmstr\Work1.h"&wsid$&",SIZE=0,RecL=5",internal,output ioerr L2780
00900 L900: pr newpage
00910   pr f "10,10,c 52,n": "ENTER CLIENT NUMBER TO PRINT, ENTER 0 WHEN COMPLETE"
00920 L920: input fields "10,65,n 5,ue,n": cliprt conv L920
00930   if cliprt=0 then goto L980
00940   cliprt$=lpad$(rtrm$(str$(cliprt)),5)
00950   write #7,using L960: cliprt$
00960 L960: form pos 1,c 5
00970   goto L900
00980 L980: close #7: 
00990 L990: pr newpage
01000   pr f "10,25,c 50,n": "NOW PRINTING UNBILLED WORK IN PROCESS"
01010   pr f "23,2,c 30,n": "Press F5 to stop"
01020   gosub L2600
01030   if prtcli=1 then goto L1090
01040   open #7: "Name="&env$('Q')&"\TMmstr\Work1.h"&wsid$&",NoShr",internal,input ioerr L2780
01050 L1050: if prtcli=1 then goto L1090
01060 L1060: read #7,using L960: cliprt$ eof L1480 ioerr L2780
01070   read #1,using L1100,key=cliprt$: z$,cliname$,pno,mat ca nokey L1060 ioerr L2780 ! READ CLIENT RECORDS
01080   goto L1110
01090 L1090: read #1,using L1100: z$,cliname$,pno,mat ca eof L1480 ioerr L2780 ! READ CLIENT RECORDS
01100 L1100: form pos 1,c 5,c 30,pos 179,n 9,pos 230,10*pd 3
01110 L1110: for j1=1 to 10
01120     if ca(j1)=0 then goto L1140
01130     gosub L1160
01140 L1140: next j1
01150   goto L1050
01160 L1160: read #2,using L1170,rec=ca(j1): mat ta,mat fb ioerr L2780 ! READ TRANSACTION ADDRESSES
01170 L1170: form pos 1,50*pd 3,25*n 1
01180   for j2=1 to 25
01190     if fb(j2)=2 and prtall=1 then goto L1210
01200     if fb(j2)=2 or fb(j2)=3 then goto L1460
01210 L1210: if ta(j2,1)=0 and ta(j2,2)=0 then goto L1460
01220     if p1=0 then goto L1250
01230     gosub L1740 ! pr SECTION HEADING
01240     gosub L2100 ! CATEGORY HEADING
01250 L1250: nta=ta(j2,1)
01260 L1260: read #3,using L1280,rec=nta: k$,e$,mat b,sc$,iv$,nta,scdesc$ ioerr L2780 ! READ DETAIL TRANS.
01270     if fndate_mmddyy_to_ccyymmdd(b(4))<fndate_mmddyy_to_ccyymmdd(olddat) then goto L1410
01280 L1280: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3,c 30
01290     if b(7)>-1 then goto L1310
01300     b(3)=-b(3)
01310 L1310: if p1><0 then goto L1360
01320     gosub L1640 ! PAGE HEADING
01330     gosub L2100 ! CATEGORY HEADING
01340     goto L1360
01350     gosub L1740 ! pr SECTION HEADING
01360 L1360: gosub L270 ! READ EMPLOYEE NAME
01370     gosub L2270 ! ACCUMULATE EMPLOYEE SUMMARY
01380     gosub L1940 ! DETERMINE OLDEST DATE
01390     gosub L2380 ! ACCUMULATE WORK TRANSACTION INFORMATION
01400     gosub L1790 ! pr DETAIL LINE
01410 L1410: if nta=0 then goto L1430
01420     goto L1260
01430 L1430: gosub L2010 ! pr TOTAL CATEGORY
01440     gosub L2130 ! pr EMPLOYEE SUMMARY
01450     gosub L1890 ! WRITE WORK TRANSACTION
01460 L1460: next j2
01470   return 
01480 L1480: close #1: 
01490   close #2: 
01500   close #3: 
01510   close #4: 
01520   if prtcli=1 then goto L1540
01530   close #7,free: 
01540 L1540: fncloseprn
01550   chain "S:\acsTM\TMUNAGPT",dat
01560   close #1: ioerr L1570
01570 L1570: close #2: ioerr L1580
01580 L1580: close #3: ioerr L1590
01590 L1590: close #4: ioerr L1600
01600 L1600: close #5: ioerr L1610
01610 L1610: close #6: ioerr L1620
01620 L1620: close #7: ioerr L1630
01630 L1630: goto XIT
01640 L1640: p1=p1+1 ! pr PAGE HEADING
01650   pr #255,using L1660: date$,cnam$
01660 L1660: form pos 1,c 8,pos namtab,c 40,skip 1
01670   pr #255,using L1680: time$,"UNBILLED WORK IN PROCESS REGISTER",p1
01680 L1680: form pos 1,c 8,pos 49,c 33,pos 120,pic(zzzz),skip 1
01690   pr #255,using L1700: dat
01700 L1700: form pos 62,pic(zz/zz/zz),skip 1
01710   pr #255: 
01720   pr #255,using L1730: "       EMPLOYEE","DATE     RATE","HOURS     CHARGE"," CATEGORY  SERVICE CODE / DESCRIPTION"
01730 L1730: form pos 12,c 15,pos 40,c 13,pos 56,c 16,pos 73,c 40,skip 1
01740 L1740: numb1=len(rtrm$(cliname$))+at+11
01750   clen=len(rtrm$(cliname$))
01760   pr #255,using L1770: z$,rtrm$(cliname$) pageoflow L1830
01770 L1770: form skip 1,pos 1,"*** ",c 5,"-",c clen,pos numb1," ***",skip 1
01780   return 
01790 L1790: if ltrm$(sc$)="0" or ltrm$(sc$)="" then sc2$=" " else sc2$=sc$&" -"
01800   pr #255,using L1810: e$,empname$,b(4),b(2),b(1),b(3),b(5),"-",b(8),sc2$," ",scdesc$ pageoflow L1830 ! pr DETAIL LINE
01810 L1810: form pos 2,c 9,pos 12,c 25,pos 38,pic(zz/zz/zz),n 8.2,n 7.2,n 11.2,x 2,n 2,x 1,c 1,n 2,x 2,c 6,c 1,c 30,skip 1
01820   goto L1860
01830 L1830: pr #255: newpage
01840   gosub L1640
01850   continue 
01860 L1860: let g1=g1+b(1)
01870   let g2=g2+b(3)
01880   return 
01890 L1890: if fb(j2)>1 then goto L1920 ! WRITE WORK TRANSACTION
01900   write #6,using L1910: cno$,cna$,en$,mat d
01910 L1910: form pos 1,c 5,c 30,c 9,n 2,n 6,pd 4.2,5*pd 4.2
01920 L1920: mat d=(0)
01930   return 
01940 L1940: d3=int(b(4)/100)+(b(4)-int(b(4)/100)*100)*10000 ! DETERMINE OLDEST DATE
01950   if d(2)><0 then goto L1980
01960   d(2)=d3
01970   goto L2000
01980 L1980: if d(2)<=d3 then goto L2000
01990   d(2)=d3
02000 L2000: return 
02010 L2010: pr #255,using L2020: "TOTAL ",cat$(j1),g1,g2 pageoflow L2040 ! pr TOTAL CATEGORY
02020 L2020: form skip 1,pos 12,c 6,c 30,pos 53,n 8.2,pos 61,n 11.2,skip 1
02030   goto L2070
02040 L2040: pr #255: newpage
02050   gosub L1640
02060   continue 
02070 L2070: let g1=0
02080   let g2=0
02090   return 
02100 L2100: pr #255,using L2110: cat$(j1) ! pr CATEGORY HEADING
02110 L2110: form pos 3,c 30,skip 1
02120   return 
02130 L2130: if l(1,1)=0 and l(1,2)=0 then goto L2240 ! pr EMPLOYEE SUMMARY
02140   pr #255,using L2150: "SUMMARY BY EMPLOYEE:"
02150 L2150: form pos 12,c 20,skip 1
02160   for j=1 to 20
02170     if l(j,1)=0 and l(j,2)=0 then goto L2240
02180     pr #255,using L2190: l$(j,1)(1:9),l$(j,2),l(j,1),l(j,2) pageoflow L2210
02190 L2190: form pos 17,c 9,x 1,c 25,pos 53,n 8.2,n 11.2,skip 1
02200     goto L2230
02210 L2210: pr #255: newpage
02220     gosub L1640
02230 L2230: next j
02240 L2240: mat l$=(" ")
02250   mat l=(0)
02260   return 
02270 L2270: for j=1 to 20 ! ACCUMULATE EMPLOYEE SUMMARY INFORMATION
02280     if b(7)<0 then goto L2370
02290     if l$(j,2)=empname$ then goto L2350
02300     if l(j,1)=0 and l(j,2)=0 then goto L2330
02310   next j
02320   goto L2370
02330 L2330: l$(j,1)=e$
02340   l$(j,2)=empname$
02350 L2350: l(j,1)=l(j,1)+b(1)
02360   l(j,2)=l(j,2)+b(3)
02370 L2370: return 
02380 L2380: e7=int(b(4)/10000) ! ACCUMULATE INFORMATION FOR WORK TRANSACTIONS
02390   e5=int((b(4)-e7*10000)/100)
02400   e8=b(4)-(e5*100+e7*10000)
02410   let f6=(d8-e8)*s(13)+(s(d7)-s(e7))+(d5-e5)
02420   if f6<30 then goto L2460
02430   if f6>30 and f6<=60 then goto L2480
02440   if f6>60 and f6<=90 then goto L2500
02450   if f6>90 then goto L2520
02460 L2460: d(5)=d(5)+b(3)
02470   goto L2530
02480 L2480: d(6)=d(6)+b(3)
02490   goto L2530
02500 L2500: d(7)=d(7)+b(3)
02510   goto L2530
02520 L2520: d(8)=d(8)+b(3)
02530 L2530: d(4)=d(4)+b(3)
02540   d(3)=d(3)+b(1)
02550   cno$=z$
02560   cna$=cliname$
02570   en$=lpad$(rtrm$(str$(pno)),5)
02580   d(1)=j1
02590   return 
02600 L2600: if int(d8/4)*100=int(d8/4*100) then goto L2630 ! INITIAL AGING PERIOD SETUP
02610   at3=0
02620   goto L2640
02630 L2630: at3=1
02640 L2640: s(1)=0
02650   s(2)=31
02660   s(3)=59+at3
02670   s(4)=90+at3
02680   s(5)=120+at3
02690   s(6)=151+at3
02700   s(7)=181+at3
02710   s(8)=212+at3
02720   s(9)=243+at3
02730   s(10)=273+at3
02740   s(11)=304+at3
02750   s(12)=334+at3
02760   s(13)=365+at3
02770   return 
02780 L2780: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2800
02790   goto L2840
02800 L2800: pr newpage
02810   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2830
02820   goto L2840
02830 L2830: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02840 L2840: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02850   input fields "24,60,C 1,N": quitcode$
02860   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
02870   pr f "23,3,C 78,N": ""
02880   pr f "24,3,C 78,N": ""
02890   retry 
02900 XIT: fnxit
02910   def fndate_mmddyy_to_ccyymmdd(x)
02920     let x2=(x-int(x*.01)*100)*10000+int(x*.01)
02930     if int(x2*.0001)<90 then let x2=x2+20000000 else let x2=x2+19000000
02940     fndate_mmddyy_to_ccyymmdd=x2
02950   fnend 
