00020   on fkey 5 goto L770
00030   on error goto L1950
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnd1,fndat
00050   fntop(program$,cap$="YTD Billing Summary")
00060   fncno(cno,cnam$)
00070   fndat(dat$)
00080   dim cnam$*40,cat$(30)*30,dat$*20,cliname$*30,enam$*25
00090   dim cn$*5,en$*9,in$*12,sc$*4,y(7),hcn$*5,cap$*128,dat$*20
00100   dim x$(10)*30,c$(50)*25,d(50),e(50),f(50),g(10),h(10),i(10)
00110   pr newpage
00120   pr f "10,10,c 35,n": "ENTER YTD BILLING SUMMARY DATE"
00130   pr f "10,48,c 20,n": dat$
00140   pr f "14,30,c 20,n": "Press F5 to stop"
00150 L150: input fields "10,48,c 20,ue,n": dat$ conv L150
00160   if cmdkey=5 then goto XIT
00170   fndat(dat$,put=2)
00180   pr newpage
00190   dattab=66-int(len(rtrm$(dat$))/2)
00200   pr f "10,25,c 40,n": "YTD BILLING SUMMARY IN PROCESS"
00210   pr f "23,2,c 30,n": "Press F5 to stop"
00220   fnopenprn
00230   gosub L2080 ! sort
00240   dattab=66-int(len(rtrm$(dat$))/2)
00250   dattab2=43-int(len(rtrm$(dat$))/2)
00260   namtab=66-int(len(rtrm$(cnam$))/2)
00270   namtab2=43-int(len(rtrm$(cnam$))/2)
00280   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,input,relative ioerr L1950
00290   read #1,using L300,rec=1: mat cat$ ioerr L1950
00300 L300: form pos 1,30*c 30
00310   close #1: 
00320   gosub L830
00330   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1950
00340   goto L400
00350 L350: read #1,using L360,key=cn$: cliname$ nokey L380 ioerr L1950
00360 L360: form pos 6,c 30
00370   goto L390
00380 L380: cliname$=" "
00390 L390: return 
00400 L400: open #2: "Name="&env$('Q')&"\TMmstr\TMYTDTrn.h"&str$(cno)&",Shr",internal,input,relative ioerr L1950
00410   open #3: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L1950
00420   open #4: "Name="&env$('Temp')&"\Addr."&session$,internal,input ioerr L1950
00430   goto L490
00440 L440: read #3,using L450,key=lpad$(rtrm$(en$),9): enam$ nokey L470 ioerr L1950
00450 L450: form pos 10,c 25
00460   goto L480
00470 L470: enam$=" "
00480 L480: return 
00490 L490: read #4,using L500: addr eof L670 ioerr L1950
00500 L500: form pos 1,pd 3
00510   read #2,using L520,rec=addr: cn$,en$,in$,sc$,mat y ioerr L1950
00520 L520: form pos 1,c 5,c 9,c 12,c 4,n 6,2*n 2,pd 2,pd 4.2,2*pd 5.2
00530   if fst=1 then goto L560
00540   let fst=1
00550   hcn$=cn$
00560 L560: if hcn$=cn$ then goto L590
00570   gosub L1900
00580   hcn$=cn$
00590 L590: tbil=tbil+y(7)
00600   tgy=tgy+(y(7)-y(6))
00610   gosub L350
00620   gosub L440
00630   gosub L920
00640   gosub L1410
00650   gosub L1530
00660   goto L490
00670 L670: gosub L1900
00680   close #1: 
00690   close #2: 
00700   close #3: 
00710   close #4: 
00720   gosub L1250
00730   gosub L1310
00740   gosub L1050
00750   gosub L1650
00760   goto L810
00770 L770: close #1: ioerr L780
00780 L780: close #2: ioerr L790
00790 L790: close #3: ioerr L800
00800 L800: close #4: ioerr L810
00810 L810: fncloseprn
00820 XIT: fnxit
00830 L830: pr #255,using L840: date$,cnam$,time$,"YTD BILLING SUMMARY"
00840 L840: form skip 3,pos 1,c 8,pos namtab,c 40,skip 1,pos 1,c 8,pos 57,c 23,skip 1
00850   pr #255,using L860: dat$
00860 L860: form pos dattab,c 20,skip 2
00870   pr #255,using L880: "CLIENT NAME","CATEGORY   HOURS","AT","BILLING","GAIN OR     %   DATE   TYPE OF SERVICE","PARTNER IN CHARGE"
00880 L880: form pos 6,c 11,pos 25,c 16,pos 46,c 2,pos 54,c 7,pos 64,c 38,pos 112,c 20,skip 1
00890   pr #255,using L900: "BILLED  STANDARD","LOSS","BILLED"
00900 L900: form pos 35,c 17,pos 66,c 4,pos 79,c 6,skip 2
00910   return 
00920 L920: if y(6)=0 then goto L990
00930   pr #255,using L940: cliname$(1:26),y(3),"-",y(2),y(5),y(6),y(7),y(7)-y(6),(y(7)-y(6))/y(6)*100,"%",y(1),cat$(y(3))(1:22),enam$(1:22) pageoflow L960
00940 L940: form pos 1,c 26,pos 27,n 2,x 1,c 1,n 2,n 8.2,n 10.2,n 10.2,n 10.2,n 6,c 1,x 1,pic(zz/zz/zz),pos 88,c 22,pos 111,c 22,skip 1
00950   goto L1010
00960 L960: pr #255: newpage
00970   gosub L830
00980   goto L1010
00990 L990: if y(3)= 0 then let y(3)=1
01000   pr #255,using L940: cliname$(1:26),y(3),"-",y(2),y(5),y(6),y(7),y(7),0," ",y(1),cat$(y(3))(1:22),enam$(1:22) pageoflow L960
01010 L1010: l1=l1+y(5)
01020   m1=m1+y(6)
01030   n1=n1+y(7)
01040   return 
01050 L1050: for y3=1 to 50
01060     if rtrm$(c$(y3))="" then goto L1190
01070     if c$(y3)="-1" then goto L1090
01080     goto L1100
01090 L1090: c$(y3)="UNASSIGNED"
01100 L1100: if e(y3)=0 then goto L1140
01110     pr #255,using L1120: c$(y3),d(y3),e(y3),f(y3),f(y3)-e(y3),(f(y3)-e(y3))/e(y3)*100,"%"
01120 L1120: form pos 1,c 25,pos 28,n 8.2,pos 38,n 10.2,pos 50,n 10.2,pos 61,n 10.2,pos 72,n 5,pos 77,c 1,skip 1 ! 2/5/88
01130     goto L1150
01140 L1140: pr #255,using L1120: c$(y3),d(y3),e(y3),f(y3),f(y3),0,"%"
01150 L1150: let w=w+d(y3)
01160     let x=x+e(y3)
01170     let z=z+f(y3)
01180   next y3
01190 L1190: if x=0 then goto L1230
01200   pr #255,using L1210: "  FINAL TOTALS",w,x,z,z-x,(z-x)/x*100,"%"
01210 L1210: form skip 1,pos 1,c 15,pos 27,n 9.2,pos 37,n 11.2,pos 49,n 11.2,pos 61,n 10.2,pos 71,pic(------),pos 77,c 1,skip 1 ! 2/5/88
01220   goto L1240
01230 L1230: pr #255,using L1210: "  FINAL TOTALS",w,x,z,z-x,0," "
01240 L1240: return 
01250 L1250: if m1=0 then goto L1290
01260   pr #255,using L1270: "  FINAL TOTALS",l1,m1,n1,n1-m1,(n1-m1)/m1*100,"%"
01270 L1270: form skip 2,pos 1,c 14,pos 33,n 8.2,n 10.2,n 10.2,n 10.2,n 6,c 1,skip 1 ! 2/5/88
01280   goto L1300
01290 L1290: pr #255,using L1270: "  FINAL TOTALS",l1,m1,n1,n1,0," "
01300 L1300: return 
01310 L1310: pr #255: newpage
01320   pr #255,using L1330: cnam$,"BILLING ANALYSIS BY PARTNER"
01330 L1330: form skip 3,pos namtab2,c 40,skip 1,pos 29,c 28,skip 1
01340   pr #255,using L1350: dat$
01350 L1350: form pos dattab2,c 20,skip 2
01360   pr #255,using L1370: "PARTNER NAME","HOURS","AT","BILLING","GAIN OR     %"
01370 L1370: form pos 7,c 12,pos 31,c 5,pos 43,c 2,pos 53,c 7,pos 64,c 13,skip 1
01380   pr #255,using L1390: "BILLED    STANDARD","LOSS"
01390 L1390: form pos 30,c 18,pos 66,c 4,skip 2
01400   return 
01410 L1410: if rtrm$(enam$)><"" then goto L1430
01420   enam$="-1"
01430 L1430: for y2=1 to 50
01440     if c$(y2)=enam$ then goto L1490
01450     if rtrm$(c$(y2))="" then goto L1480
01460   next y2
01470   goto L1520
01480 L1480: c$(y2)=enam$
01490 L1490: let f(y2)=f(y2)+y(7)
01500   d(y2)=d(y2)+y(5)
01510   e(y2)=e(y2)+y(6)
01520 L1520: return 
01530 L1530: if rtrm$(cat$(y(3)))><"" then goto L1550
01540   cat$(y(3))="-1"
01550 L1550: for x7=1 to 10
01560     if x$(x7)=cat$(y(3)) then goto L1610
01570     if rtrm$(x$(x7))="" then goto L1600
01580   next x7
01590   goto L1640
01600 L1600: let x$(x7)=cat$(y(3))
01610 L1610: let g(x7)=g(x7)+y(7)
01620   h(x7)=h(x7)+y(5)
01630   i(x7)=i(x7)+y(6)
01640 L1640: return 
01650 L1650: pr #255: newpage
01660   pr #255,using L1330: cnam$,"BILLING ANALYSIS BY CATEGORY"
01670   pr #255,using L1350: dat$
01680   pr #255,using L1690: "CATEGORY NAME","HOURS","AT","BILLING","GAIN OR     %"
01690 L1690: form pos 6,c 13,pos 31,c 5,pos 43,c 2,pos 53,c 7,pos 64,c 13,skip 1
01700   pr #255,using L1710: "BILLED    STANDARD","LOSS"
01710 L1710: form pos 30,c 18,pos 66,c 4,skip 2
01720   for x7=1 to 10
01730     if rtrm$(x$(x7))="" then goto L1850
01740     if x$(x7)="-1" then goto L1760
01750     goto L1770
01760 L1760: let x$(x7)="UNASSIGNED"
01770 L1770: if i(x7)><0 then goto L1800
01780     pr #255,using L1120: x$(x7)(1:25),h(x7),i(x7),g(x7),g(x7),0,"%"
01790     goto L1810
01800 L1800: pr #255,using L1120: x$(x7)(1:25),h(x7),i(x7),g(x7),g(x7)-i(x7),(g(x7)-i(x7))/i(x7)*100,"%"
01810 L1810: let w1=w1+g(x7)
01820     let x1=x1+h(x7)
01830     let y1=y1+i(x7)
01840   next x7
01850 L1850: if y1=0 then goto L1880
01860   pr #255,using L1210: "   FINAL TOTALS",x1,y1,w1,w1-y1,(w1-y1)/y1*100,"%"
01870   goto L1890
01880 L1880: pr #255,using L1210: "   FINAL TOTALS",x1,y1,w1,w1-y1,0," "
01890 L1890: return 
01900 L1900: pr #255,using L1910: "---------","---------",tbil,tgy
01910 L1910: form pos 52,c 9,pos 62,c 9,skip 1,pos 51,2*n 10.2,skip 2
01920   tbil=0
01930   tgy=0
01940   return 
01950 L1950: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1970
01960   goto L2010
01970 L1970: pr newpage
01980   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2000
01990   goto L2010
02000 L2000: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02010 L2010: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02020   input fields "24,60,C 1,N": quitcode$
02030   if rtrm$(uprc$(quitcode$))="Q" then goto L2070
02040   pr f "23,3,C 78,N": ""
02050   pr f "24,3,C 78,N": ""
02060   retry 
02070 L2070: goto XIT
02080 L2080: open #9: "Name="&env$('Temp')&"\Control."&session$&",SIZE=0,RecL=128,REPLACE",internal,output 
02090 L2090: form pos 1,c 128
02100   write #9,using L2090: "FILE tmytdtrn.H"&str$(cno)&","&env$('Q')&"\TMmstr,,"&env$('Temp')&"\Addr."&session$&",,,ACSTM,,A,N"
02110   write #9,using L2090: "MASK 1,5,C,A,39,2,N,A,37,2,N,A,35,2,N,A,31,4,N,A"
02120   close #9: 
02130   execute "FREE "&env$('Temp')&"\Addr."&session$ ioerr L2140
02140 L2140: execute "Sort "&env$('Temp')&"\Control."&session$
02150   return 
