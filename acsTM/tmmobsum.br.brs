00020   on error goto L2080
00030   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fnd1,fndat
00040   let fntop(program$,cap$="Monthly Billing Summary")
00050   let fncno(cno,cnam$)
00060   let fnconsole(1)
00070   let fndat(dat$)
00080   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01)
00090   dim cnam$*40,cat$(30)*30,dat$*20,cliname$*30,enam$*25
00100   dim z$*5,ca(10),ta(25,2),fb(25),dat$*20,cap$*128
00110   dim k$*5,e$*9,b(8),sc$*4,iv$*12,tempiv$*12
00120   dim x$(10)*30,c$(50)*25,d(50),e(50),f(50),g(10),h(10),i(10)
00130   let io1$(1)="10,44,C 20,U,N"
00140   let io1$(2)="12,58,N 6,U,N"
00150   print newpage
00160   print fields "10,5,c 35,n": "ENTER MONTHLY BILLING SUMMARY DATE:"
00170   print fields "12,5,C 60": "ENTER THE OLDEST BILLING DATE TO PRINT OR 0 FOR ALL:"
00180   print fields io1$(1): dat$
00190   print fields "23,2,c 30,n": "Press F5 to stop"
00200 L200: input fields mat io1$: dat$,od conv L200
00210   if cmdkey=5 then goto XIT
00220   let fndat(dat$,put=2)
00230   if od=0 or (od>10111 and od<123200) then goto L240 else goto L200
00240 L240: let fnopenprn
00250   print newpage
00260   let dattab=66-int(len(rtrm$(dat$))/2)
00270   print fields "10,20,c 60,n": "MONTHLY BILLING SUMMARY IN PROCESS"
00280   print fields "23,2,c 30,n": "Press F5 to stop"
00290   let dattab=66-int(len(rtrm$(dat$))/2)
00300   let dattab2=43-int(len(rtrm$(dat$))/2)
00310   let namtab=66-int(len(rtrm$(cnam$))/2)
00320   let namtab2=43-int(len(rtrm$(cnam$))/2)
00330   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,input,relative ioerr L2080
00340   read #1,using L350,rec=1: mat cat$ ioerr L2080
00350 L350: form pos 1,30*c 30
00360   close #1: 
00370   gosub L1020
00380   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L2080
00390   open #2: "Name="&env$('Q')&"\TMmstr\TMTRAddr.h"&str$(cno)&",Shr",internal,input,relative ioerr L2080
00400   open #3: "Name="&env$('Q')&"\TMmstr\TMTRANS.H"&str$(cno)&",Shr",internal,input,relative ioerr L2080
00410   open #4: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L2080
00420   goto L480
00430 L430: read #4,using L440,key=lpad$(str$(pno),9): enam$ nokey L460 ioerr L2080
00440 L440: form pos 10,c 25
00450   goto L470
00460 L460: let enam$=" "
00470 L470: return 
00480 L480: read #1,using L490: z$,cliname$,pno,mat ca eof L870 ioerr L2080
00490 L490: form pos 1,c 5,c 30,pos 179,n 9,pos 230,10*pd 3
00500   for j1=1 to 10
00510     if ca(j1)=0 then goto L530
00520     gosub L550
00530 L530: next j1
00540   goto L480
00550 L550: read #2,using L560,rec=ca(j1): mat ta,mat fb ioerr L2080
00560 L560: form pos 1,50*pd 3,25*n 1
00570   for j2=1 to 25
00580     if fb(j2)>=1 and fb(j2)<=3 then goto L600
00590     goto L800
00600 L600: let nta=ta(j2,1)
00610     let iv$=" "
00620     let hrs=0
00630     let std=0
00640     let bil=0
00650 L650: read #3,using L680,rec=nta: k$,e$,mat b,sc$,tempiv$,nta ioerr L2080
00660     if b(7)=-1 and fncd(b(4))<fncd(od) then goto L800
00670     if b(7)<0 then let iv$=tempiv$
00680 L680: form pos 1,c 5,c 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,c 4,c 12,pd 3
00690     if fb(j2)=2 or fb(j2)=3 then goto L720
00700     let ast$="*"
00710     goto L730
00720 L720: let ast$=" "
00730 L730: if b(7)>=0 then goto L760
00740     let bil=bil+b(3)
00750     if b(7)<0 then goto L780
00760 L760: let hrs=hrs+b(1)
00770     let std=std+b(3)
00780 L780: if nta><0 then goto L650
00790     gosub L820
00800 L800: next j2
00810   return 
00820 L820: gosub L430
00830   gosub L1110
00840   gosub L1590
00850   gosub L1710
00860   return 
00870 L870: close #1: 
00880   close #2: 
00890   close #3: 
00900   close #4: 
00910   gosub L1430
00920   gosub L1490
00930   gosub L1230
00940   gosub L1830
00950   goto L1000
00960   close #1: ioerr L970
00970 L970: close #2: ioerr L980
00980 L980: close #3: ioerr L990
00990 L990: close #4: ioerr L1000
01000 L1000: let fncloseprn
01010 XIT: let fnxit
01020 L1020: print #255,using L1030: date$,cnam$,time$,"MONTHLY BILLING SUMMARY"
01030 L1030: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 55,c 23,skip 1
01040   print #255,using L1050: dat$
01050 L1050: form pos dattab,c 20,skip 2
01060   print #255,using L1070: "CLIENT NAME","CATEGORY  HOURS","AT","BILLING","GAIN OR     % PARTIAL TYPE OF SERVICE","PARTNER IN CHARGE      INVOICE"
01070 L1070: form pos 6,c 11,pos 25,c 15,pos 46,c 2,pos 53,c 7,pos 62,c 37,pos 102,c 30,skip 1
01080   print #255,using L1090: "BILLED   STANDARD","LOSS","BILL ","NUMBER"
01090 L1090: form pos 34,c 17,pos 64,c 4,pos 77,c 5,pos 126,c 6,skip 2
01100   return 
01110 L1110: if std=0 then goto L1180
01120   print #255,using L1130: cliname$(1:26),b(5)," -",b(8),hrs,std,bil,bil-std,(bil-std)/std*100,"%",ast$,cat$(j1)(1:17),enam$(1:17),iv$ pageoflow L1150
01130 L1130: form pos 1,c 26,pos 27,n 2,c 2,n 2,n 7.2,n 11.2,pos 51,n 9.2,n 9.2,n 6,pos 75,c 1,x 2,c 1,pos 84,c 17,pos 102,c 17,pos 120,c 12,skip 1
01140   goto L1190
01150 L1150: print #255: newpage
01160   gosub L1020
01170   goto L1190
01180 L1180: print #255,using L1130: cliname$(1:26),b(5),"-",b(8),hrs,std,bil,bil-std,0," ",ast$,cat$(j1)(1:17),enam$(1:17),iv$ pageoflow L1150
01190 L1190: let l1=l1+hrs
01200   let m1=m1+std
01210   let n1=n1+bil
01220   return 
01230 L1230: for y=1 to 50
01240     if rtrm$(c$(y))="" then goto L1370
01250     if c$(y)="-1" then goto L1270
01260     goto L1280
01270 L1270: let c$(y)="UNASSIGNED"
01280 L1280: if e(y)=0 then goto L1320
01290     print #255,using L1300: c$(y),d(y),e(y),f(y),f(y)-e(y),(f(y)-e(y))/e(y)*100,"%"
01300 L1300: form pos 1,c 25,pos 26,n 10.2,pos 36,n 12.2,pos 49,n 11.2,pos 60,n 10.2,pos 70,n 6,pos 77,c 1,skip 1 ! 2/17/88
01310     goto L1330
01320 L1320: print #255,using L1300: c$(y),d(y),e(y),f(y),f(y),0,"%"
01330 L1330: let w=w+d(y)
01340     let x=x+e(y)
01350     let z=z+f(y)
01360   next y
01370 L1370: if x=0 then goto L1410
01380   print #255,using L1390: "  FINAL TOTALS",w,x,z,z-x,(z-x)/x*100,"%"
01390 L1390: form skip 1,pos 1,c 15,pos 27,n 9.2,pos 37,n 11.2,pos 49,n 11.2,pos 61,n 10.2,pos 71,pic(------),pos 77,c 1,skip 1 ! 2/17/88
01400   goto L1420
01410 L1410: print #255,using L1390: "  FINAL TOTALS",w,x,z,z-x,0," "
01420 L1420: return 
01430 L1430: if m1=0 then goto L1470
01440   print #255,using L1450: "  FINAL TOTALS",l1,m1,n1,n1-m1,(n1-m1)/m1*100,"%"
01450 L1450: form skip 2,pos 1,c 14,pos 31,n 9.2,pos 41,n 10.2,pos 51,n 9.2,n 9.2,n 6,c 1,skip 1 ! 2/17/88
01460   goto L1480
01470 L1470: print #255,using L1450: "  FINAL TOTALS",l1,m1,n1,n1,0," "
01480 L1480: return 
01490 L1490: print #255: newpage
01500   print #255,using L1510: cnam$,"BILLING ANALYSIS BY PARTNER"
01510 L1510: form skip 3,pos namtab2,c 40,skip 1,pos 29,c 28,skip 1
01520   print #255,using L1530: dat$
01530 L1530: form pos dattab2,c 20,skip 2
01540   print #255,using L1550: "PARTNER NAME","HOURS","AT","BILLING","GAIN OR     %"
01550 L1550: form pos 7,c 12,pos 31,c 5,pos 43,c 2,pos 53,c 7,pos 64,c 13,skip 1
01560   print #255,using L1570: "BILLED    STANDARD","LOSS"
01570 L1570: form pos 30,c 18,pos 66,c 4,skip 2
01580   return 
01590 L1590: if rtrm$(enam$)><"" then goto L1610
01600   let enam$="-1"
01610 L1610: for y=1 to 50
01620     if c$(y)=enam$ then goto L1670
01630     if rtrm$(c$(y))="" then goto L1660
01640   next y
01650   goto L1700
01660 L1660: let c$(y)=enam$
01670 L1670: let f(y)=f(y)+bil
01680   let d(y)=d(y)+hrs
01690   let e(y)=e(y)+std
01700 L1700: return 
01710 L1710: if rtrm$(cat$(j1))><"" then goto L1730
01720   let cat$(j1)="-1"
01730 L1730: for x7=1 to 10
01740     if x$(x7)=cat$(j1) then goto L1790
01750     if rtrm$(x$(x7))="" then goto L1780
01760   next x7
01770   goto L1820
01780 L1780: let x$(x7)=cat$(j1)
01790 L1790: let g(x7)=g(x7)+bil
01800   let h(x7)=h(x7)+hrs
01810   let i(x7)=i(x7)+std
01820 L1820: return 
01830 L1830: print #255: newpage
01840   print #255,using L1510: cnam$,"BILLING ANALYSIS BY CATEGORY"
01850   print #255,using L1530: dat$
01860   print #255,using L1870: "CATEGORY NAME","HOURS","AT","BILLING","GAIN OR     %"
01870 L1870: form pos 6,c 13,pos 31,c 5,pos 43,c 2,pos 53,c 7,pos 64,c 13,skip 1
01880   print #255,using L1890: "BILLED    STANDARD","LOSS"
01890 L1890: form pos 30,c 18,pos 66,c 4,skip 2
01900   for x7=1 to 10
01910     if rtrm$(x$(x7))="" then goto L2030
01920     if x$(x7)="-1" then goto L1940
01930     goto L1950
01940 L1940: let x$(x7)="UNASSIGNED"
01950 L1950: if i(x7)><0 then goto L1980
01960     print #255,using L1300: x$(x7)(1:25),h(x7),i(x7),g(x7),g(x7),0,"%"
01970     goto L1990
01980 L1980: print #255,using L1300: x$(x7)(1:25),h(x7),i(x7),g(x7),g(x7)-i(x7),(g(x7)-i(x7))/i(x7)*100,"%"
01990 L1990: let w1=w1+g(x7)
02000     let x1=x1+h(x7)
02010     let y1=y1+i(x7)
02020   next x7
02030 L2030: if y1=0 then goto L2060
02040   print #255,using L1390: "   FINAL TOTALS",x1,y1,w1,w1-y1,(w1-y1)/y1*100,"%"
02050   goto L2070
02060 L2060: print #255,using L1390: "   FINAL TOTALS",x1,y1,w1,w1-y1,0," "
02070 L2070: return 
02080 L2080: if err=61 then print fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2100
02090   goto L2140
02100 L2100: print newpage
02110   if err=4148 then print fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2130
02120   goto L2140
02130 L2130: print fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02140 L2140: print fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02150   input fields "24,60,C 1,N": quitcode$
02160   if rtrm$(uprc$(quitcode$))="Q" then goto L2200
02170   print fields "23,3,C 78,N": ""
02180   print fields "24,3,C 78,N": ""
02190   retry 
02200 L2200: goto XIT
