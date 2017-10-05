00020   on fkey 5 goto L970 ! 9/5/86
00030   on error goto L1570
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fnd1,fndat
00050   fntop(program$,cap$="Trial Balance")
00060   fncno(cno,cnam$)
00070   fndat(dat$)
00080   fnconsole(1)
00090   open #8: "Name="&env$('Q')&"\TMmstr\pedate.h"&str$(cno)&",RecL=20,use,Shr",internal,outin,relative 
00100   if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: d1
00110   dim z$*5,e$(4)*30,e(5),s(5),c(5),cnam$*40,u$*20,flo$(3),fli$(2),dat$*20
00120   dim cap$*128
00130   dim scr$(3)*50,ta(2),o(2),cnv$*6,q$*30,age(4),mo(12),iv$*12
00140   data 0,31,59,90,120,151,181,212,243,273,304,334
00150   read mat mo
00160   open #1: "Name="&env$('Q')&"\TMmstr\Company.h"&str$(cno)&",Shr",internal,outin ioerr L1570
00170   read #1,using L180: mat age ioerr L1570
00180 L180: form pos 170,4*pd 2,pos 226,c 20
00185   close #1: 
00190   at=int(66-len(rtrm$(cnam$))/2)
00200   fli$(1)="10,56,C 20,U,N"
00210   fli$(2)="12,56,N 6,U,N"
00220   pr newpage
00230 L230: pr f "5,30,C 18,H,N": "AGED TRIAL BALANCE"
00240   pr f "10,5,cr 50,n": "ENTER AS OF DATE  (EXAMPLE = "&dat$&":"
00250   pr f "12,5,Cr 50,N": "ENTER THE AGING DATE IN MMDDYY FORMAT:"
00260   pr f "10,56,c 20,n": dat$
00270   pr f "14,28,C 30,N": "F1 Continue   F5 Stop"
00280 L280: rinput fields mat fli$: dat$,d1 conv L280
00285   if cmdkey=5 then goto XIT
00286   if cmdkey<>1 then goto L230
00290   fndat(dat$,put=2)
00300   rewrite #8,using "form pos 1,n 6",rec=1: d1
00310   close #8: 
00320   gosub L1510
00330   pr newpage
00340   fnopenprn
00350   pr f "10,15,c 50,h": "A/R AGED TRIAL BALANCE PROGRAM IN PROCESS"
00360   pr f "23,2,C 30,N": "Press F5 to stop"
00370   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L1570
00380   open #2: "Name="&env$('Q')&"\TMmstr\ARTrans.h"&str$(cno)&",Shr",internal,input,relative ioerr L1570
00390   gosub L550
00400 L400: read #1,using L420: z$,e$(1),am6,am16,mat ta eof L700 ioerr L1570
00410   if am6=0 then goto L400
00420 L420: form pos 1,c 5,c 30,pos 283,pd 5.2,pos 298,n 1,pos 299,2*pd 3
00430   if am6<=0 then goto L450
00440   tam6=tam6+am6
00450 L450: if am6<0 then cb1=cb1+(-am6) else db1=db1+am6
00460   if am6<=0 then goto L400
00470   gosub L1000
00480   mat s=s+e
00490   pr #255,using L500: z$,e$(1),am6,mat e pageoflow L520
00500 L500: form pos 1,c 12,c 30,x 10,6*n 12.2,skip 1
00510   goto L400
00520 L520: pr #255: newpage
00530   gosub L550
00540   goto L400
00550 L550: p2=p2+1
00560   pr #255,using L570: date$,cnam$,"PAGE ",p2
00570 L570: form skip 1,pos 1,c 8,pos at,c 40,pos 120,c 5,n 4,skip 1
00580   pr #255,using L590: time$,"A/R AGED TRIAL BALANCE"
00590 L590: form pos 1,c 8,pos 55,c 22,skip 1
00600   q$=rtrm$("As of "&dat$)
00610   tabq=int(66-len(q$)/2)
00620   pr #255,using L630: q$
00630 L630: form pos tabq,c 26,skip 0
00640   if v9=9 then goto L690
00650   pr #255: 
00660   pr #255: tab(71);"-----------------------AGING--------------------------"
00670   pr #255,using L680: "CLIENT #","CLIENT NAME"," ","BALANCE","CURRENT",age(1),"-",age(2),age(2)+1,"-",age(3),age(3)+1,"-",age(4),"OVER ",age(4)
00680 L680: form pos 1,c 9,pos 12,c 13,pos 42,c 12,pos 58,c 7,pos 70,c 7,pos 83,n 3,c 1,n 2,pos 95,n 3,c 1,n 3,pos 106,n 3,c 1,n 3,pos 117,c 5,n 3,skip 1
00690 L690: return 
00700 L700: c6=s(1)+s(2)+s(3)+s(4)+s(5)
00710   if c6=0 then goto L790
00720   mat c=(0)
00730   for j=1 to 5
00740     if s(j)=0 then goto L760
00750     c(j)=s(j)/c6*100
00760 L760: next j
00770   pr #255: 
00780   pr #255,using L500: "","   COMPANY TOTALS",tam6,mat s
00790 L790: pr #255: newpage
00800   v9=9
00810   gosub L550
00820   pr #255,using L830: "A/R AGING SUMMARY"
00830 L830: form skip 2,pos 58,c 40,skip 2
00840   pr #255: tab(68);"AMOUNT   PERCENT"
00850   pr #255: 
00860   pr #255,using L870: "CURRENT",s(1),c(1)
00870 L870: form pos 43,c 10,pos 61,n 13.2,n 10.2,skip
00880   pr #255,using L870: str$(age(1)+1)&"-"&str$(age(2)),s(2),c(2)
00890   pr #255,using L870: str$(age(2)+1)&"-"&str$(age(3)),s(3),c(3)
00900   pr #255,using L870: str$(age(3)+1)&"-"&str$(age(4)),s(4),c(4)
00910   pr #255,using L870: "OVER "&str$(age(4)),s(5),c(5)
00920   pr #255: 
00930   pr #255,using L940: "                       TOTAL",db1
00940 L940: form pos 30,c 30,n 14.2,skip 2
00950   pr #255,using L940: "LESS CREDIT BALANCE ACCOUNTS",cb1
00960   pr #255,using L940: "NET TOTAL ACCOUNTS RECEIVABLE",db1-cb1
00970 L970: fncloseprn
00980   close #1: ioerr XIT
00990 XIT: fnxit
01000 L1000: ! AGING ROUTINE
01010   mat e=(0)
01020   if ta(1)=0 then goto L1500
01030   ta1=ta(1)
01040 L1040: read #2,using L1050,rec=ta1: iv$,mm,dd,yy,tr3,tr5,nta ioerr L1570
01050 L1050: form pos 6,c 12,3*n 2,x 5,pd 5.2,x 2,n 1,x 21,pd 3
01060   if tr5=4 or tr5=6 then goto L1210
01070 L1070: if mm=0 then goto L1190
01080   ag1=mo(mm)+dd+yy*365+int(yy/4)
01090   if yy-int(yy/4)*4=0 and mm>2 then ag1=ag1+1
01100   ag2=ag0-ag1
01110   if ag2>=age(4) then e(5)=e(5)+tr3 else goto L1130
01120   goto L1340
01130 L1130: if ag2>=age(3) then e(4)=e(4)+tr3 else goto L1150
01140   goto L1340
01150 L1150: if ag2>=age(2) then e(3)=e(3)+tr3 else goto L1170
01160   goto L1340
01170 L1170: if ag2>=age(1) then e(2)=e(2)+tr3 else goto L1190
01180   goto L1340
01190 L1190: e(1)=e(1)+tr3
01200   goto L1340
01210 L1210: if ar(5)=2 then goto L1240
01220   e(5)=e(5)-tr3
01230   goto L1340
01240 L1240: tr3=-tr3
01250   ta1=ta(1)
01260 L1260: read #2,using L1270,rec=ta1: hv$,mm,dd,yy,tr5,cta ioerr L1570
01270 L1270: form pos 6,c 12,pos 18,3*n 2,pos 36,n 1,pos 58,pd 3
01280   if tr5=4 or tr5=6 then goto L1300
01290   if iv$=hv$ then goto L1070
01300 L1300: if cta=0 then goto L1330
01310   ta1=cta
01320   goto L1260
01330 L1330: e(5)=e(5)+tr3
01340 L1340: if nta=0 then goto L1370
01350   ta1=nta
01360   goto L1040
01370 L1370: if e(5)>=0 then goto L1400
01380   e(4)=e(4)+e(5)
01390   e(5)=0
01400 L1400: if e(4)>=0 then goto L1430
01410   e(3)=e(3)+e(4)
01420   e(4)=0
01430 L1430: if e(3)>=0 then goto L1460
01440   e(2)=e(2)+e(3)
01450   e(3)=0
01460 L1460: if e(2)>=0 then goto L1500
01470   e(1)=e(1)+e(2)
01480   e(2)=0
01490   if e(1)<0 then v6=v6+(-e(1))
01500 L1500: return 
01510 L1510: mm=int(d1/10000)
01520   dd=int((d1-mm*10000)/100)
01530   yy=d1-(mm*10000+dd*100)
01540   ag0=mo(mm)+dd+yy*365+int(yy/4)
01550   if yy-int(yy/4)*4=0 and mm>2 then ag0=ag0+1
01560   return 
01570 L1570: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1590
01580   goto L1630
01590 L1590: pr newpage
01600   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L1620
01610   goto L1630
01620 L1620: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
01630 L1630: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
01640   input fields "24,60,C 1,N": quitcode$
01650   if rtrm$(uprc$(quitcode$))="Q" then goto L1690
01660   pr f "23,3,C 78,N": ""
01670   pr f "24,3,C 78,N": ""
01680   retry 
01690 L1690: goto XIT
