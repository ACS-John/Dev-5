00010 ! Replace S:\acsTM\ARPrtBil
00020   on fkey 5 goto L1090
00030   on error goto L2020
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnconsole
00050   dim p$*5,iv$*12,tr(6),id$*20,o(2),h$*10,ar(5),ta(2),e(5)
00060   fntop(program$,cap$="Statements")
00070   fncno(cno,cnam$)
00080   fnconsole(1)
00110   dim z$*5,e$(4)*30,name$(4)*40,d$*20,flo$(2),scr1$(2)*55
00120   dim age(4),st1$*5,mo(12)
00130   data 0,31,59,90,120,151,181,212,243,273,304,334
00140   read mat mo
00150   open #8: "Name="&env$('Q')&"\TMmstr\pedate.h"&str$(cno)&",RecL=20,use,Shr",internal,outin,relative 
00160   if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: d1
00170   close #8: 
00180   form c 9,skip 0
00190   open #1: "Name="&env$('Q')&"\TMmstr\Company.h"&str$(cno)&",Shr",internal,input ioerr L2020
00200   read #1,using L210: mat name$,mat age,all ioerr L2020
00210 L210: form pos 1,4*c 40,pos 170,4*pd 2,pos 164,n 1
00220   close #1: 
00230   at=int(65-len(rtrm$(name$(1)))/2)
00240   def fnc(x)=int(100*(x+sgn(x)*.0001))
00250   form pos 1,c 5,pos 6,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
00260   pr newpage
00270   pr fields "7,14,c 39,h,n": "POSITION A/R STATEMENT FORMS IN PRINTER"
00280   pr fields "10,10,c 50": "ENTER STATEMENT DATE IN FORMAT: MMDDYY"
00290   pr fields "10,51,N 6,N": d1
00300   pr fields "12,28,c 20": "Press F5 to Stop"
00310 L310: input fields "10,51,N 6,UE,N": d1 conv L310
00320   if cmdkey=5 then goto XIT
00330   if d1<10100 or d1>123199 then goto L310
00340   gosub L1960
00350   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L2020
00360   open #2: "Name="&env$('Q')&"\TMmstr\ARTrans.h"&str$(cno)&",Shr",internal,input,relative ioerr L2020
00370 L370: pr newpage
00380   pr fields "10,5,c 53": "ENTER CLIENT NUMBER TO START PRINTING, ELSE ENTER 0"
00390   input fields "10,60,C 5,UE,N": st1$ conv L370
00400   pr newpage
00410   fnopenprn
00420   pr fields "10,20,c 40,h,n": "STATEMENT pr IN PROCESS"
00430   pr fields "23,2,C 30,N": "Press F5 to stop"
00440   if rtrm$(st1$)="0" or rtrm$(st1$)="" then goto L450 else goto L1410
00450 L450: read #1,using L460: z$,mat e$,mat ar,mat ta eof L1090 ioerr L2020
00460 L460: form pos 1,c 5,4*c 30,pos 283,2*pd 5.2,pd 4.3,2*n 1,2*pd 3
00470 L470: if ar(4)=9 then goto L450
00480   if ar(1)<=0 then goto L450
00490   gosub L1430 ! AGING
00500 L500: gosub L890
00510   let prebal=0
00520   bal=0
00530   let pbf=0
00540   let ta=ta(1)
00550 L550: if ta=0 then goto L1130
00560 L560: read #2,using L580,rec=ta: p$,iv$,mat tr,id$,ta ioerr L2020
00570   if tr(3)=0 then goto L550
00580 L580: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
00590   if tr(6)><5 and pbf=0 then gosub L1010
00600   if tr(6)><5 then goto L690
00610   if tr(5)=4 or tr(5)=6 then goto L650
00620   let prebal=prebal+tr(3)
00630   if all=0 then bal=bal+tr(3) else goto L690
00640   goto L670
00650 L650: let prebal=prebal-tr(3)
00660   if all=0 then bal=bal-tr(3) else goto L690
00670 L670: if ta=0 then goto L1010 ! 10/19/87
00680   if all=0 then goto L560
00690 L690: let iv$=ltrm$(iv$)
00700   if tr(5)=4 or tr(5)=6 then bal=bal-tr(3) else bal=bal+tr(3)
00710   if tr(5)=4 or tr(5)=6 then let pap=63 else let pap=48
00720   if tr(5)=4 or tr(5)=6 then let tr3=-tr(3) else let tr3=tr(3)
00730   if rtrm$(id$)><"" then goto L800
00740   if tr(5)=1 then let id$="CHARGE"
00750   if tr(5)=2 then let id$="FINANCE CHARGE"
00760   if tr(5)=3 then let id$="STANDARD CHARGE"
00770   if tr(5)=4 then let id$="COLLECTION"
00780   if tr(5)=5 then let id$="DEBIT MEMO"
00790   if tr(5)=6 then let id$="CREDIT MEMO"
00800 L800: pr #255,using L810: tr(1),iv$(1:8),id$,tr(3),bal
00810 L810: form pos 1,pic(zz/zz/zz),x 4,c 10,c 20,pos pap,n 10.2,pos 77,n 12.2
00820   let p=p+1
00830   if ta=0 then goto L1130
00840   if p<38 then goto L560
00850   pr #255: newpage
00860   gosub L890
00870   form pos 124,n 6
00880   goto L560
00890 L890: let fnopenprn(cp,58,220,process)
00900   form pos 9,c 40,pos 71,c 24,skip 1
00905   pr #255: !                                                                  pr #255:
00910   pr #255,using L920: z$,d1,ar(1)
00920 L920: form skip 4,pos 60,c 5,x 6,pic(zz/zz/zz),skip 5,pos 62,pic($$$,$$$.##),skip 2
00930   for j=1 to 3
00940     pr #255,using L960: e$(j)
00950   next j
00960 L960: form pos 10,c 30,skip 1
00970   pr #255,using L980: " "
00980 L980: form pos 1,c 1,skip 9
00990   let p=23
01000   return 
01010 L1010: if all=1 or prebal=0 then goto L1050
01020   pr #255,using L1030: " BALANCE  FORWARD",prebal
01030 L1030: form pos 25,c 17,pos 77,n 12.2,skip 1
01040   let p=p+1
01050 L1050: let pbf=1
01060   if tr(6)><5 then goto L1080
01070   if ta=0 then goto L1130
01080 L1080: return 
01090 L1090: close #1: ioerr L1100
01100 L1100: close #2: ioerr L1110
01110 L1110: let fncloseprn
01120 XIT: let fnxit
01130 L1130: let sk=37-p
01140   pr #255,using L1150: "CURRENT","PAST",age(1),"PAST",age(2),"PAST",age(3)
01150 L1150: form skip sk,pos 15,c 7,x 10,c 4,pic(zzzz),x 11,c 4,pic(zzzz),x 10,c 4,pic(zzzz),skip 2
01160   pr #255,using L1170: e(1),e(2),e(3),e(4)+e(5),ar(1)
01170 L1170: form pos 10,n 10.2,x 8,n 10.2,x 9,n 10.2,x 8,n 10.2,pos 77,n 12.2
01180 ! pr #255,using 1226:"Please be sure to remit to  P O Box 758"
01190 ! form skip 5,pos 15,c 60,skip 1
01200   if align=3 then pr #255: newpage: goto L1330
01210   fncloseprn
01220   pr newpage
01230   pr fields "10,5,C 60": "CHECK FORM ALIGNMENT"
01240   pr fields "12,5,C 60": "ENTER 1 TO REPRINT SAME STATEMENT"
01250   pr fields "13,5,C 60": "      2 TO pr NEXT STATEMENT AND STOP"
01260   pr fields "14,5,C 60": "      3 TO pr ALL REMAINING STATEMENTS"
01270   pr fields "23,30,c 20": "Press F5 to Cancel"
01280 L1280: input fields "15,11,N 1,UE,N": align conv L1280
01290   if cmdkey=5 then goto XIT
01300   fnopenprn(cp,58,220,process)
01310   on align goto L500,L1330,L1330 none L1280
01320 ! ______________________________________________________________________
01330 L1330: let p=0
01340   pr newpage
01350   pr fields "10,20,c 40,h,n": "STATEMENT pr IN PROCESS"
01360   pr fields "23,2,C 30,N": "Press F5 to stop"
01370   bal=0
01380   if ta=0 then goto L450
01390   return 
01400 ! ______________________________________________________________________
01410 L1410: read #1,using L460,key=lpad$(rtrm$(st1$),5): z$,mat e$,mat ar,mat ta nokey L370 ioerr L2020
01420   goto L470
01430 L1430: let tam1=tam1+am1
01440   let tam6=tam6+ar(1)
01450   if ar(1)<0 then cb1=cb1+(-ar(1)) else let db1=db1+ar(1)
01460   mat e=(0)
01470   if ta(1)=0 then goto L1950
01480   let ta1=ta(1)
01490 L1490: read #2,using L1500,rec=ta1: iv$,mm,dd,yy,tr3,tr5,nta ioerr L2020
01500 L1500: form pos 6,c 12,3*n 2,x 5,pd 5.2,x 2,n 1,x 21,pd 3
01510   if tr5=4 or tr5=6 then goto L1660
01520 L1520: if mm=0 then goto L1640
01530   ag1=mo(mm)+dd+yy*365+int(yy/4)
01540   if yy-int(yy/4)*4=0 and mm>2 then ag1=ag1+1
01550   ag2=ag0-ag1
01560   if ag2>=age(4) then let e(5)=e(5)+tr3 else goto L1580
01570   goto L1790
01580 L1580: if ag2>=age(3) then let e(4)=e(4)+tr3 else goto L1600
01590   goto L1790
01600 L1600: if ag2>=age(2) then let e(3)=e(3)+tr3 else goto L1620
01610   goto L1790
01620 L1620: if ag2>=age(1) then let e(2)=e(2)+tr3 else goto L1640
01630   goto L1790
01640 L1640: let e(1)=e(1)+tr3
01650   goto L1790
01660 L1660: if ar(5)=2 then goto L1690
01670   let e(5)=e(5)-tr3
01680   goto L1790
01690 L1690: let tr3=-tr3
01700   let ta1=ta(1)
01710 L1710: read #2,using L1720,rec=ta1: hv$,mm,dd,yy,tr5,cta ioerr L2020
01720 L1720: form pos 6,c 12,pos 18,3*n 2,pos 36,n 1,pos 58,pd 3
01730   if tr5=4 or tr5=6 then goto L1750
01740   if iv$=hv$ then goto L1520
01750 L1750: if cta=0 then goto L1780
01760   let ta1=cta
01770   goto L1710
01780 L1780: let e(5)=e(5)+tr3
01790 L1790: if nta=0 then goto L1820
01800   let ta1=nta
01810   goto L1490
01820 L1820: if e(5)>=0 then goto L1850
01830   let e(4)=e(4)+e(5)
01840   let e(5)=0
01850 L1850: if e(4)>=0 then goto L1880
01860   let e(3)=e(3)+e(4)
01870   let e(4)=0
01880 L1880: if e(3)>=0 then goto L1910
01890   let e(2)=e(2)+e(3)
01900   let e(3)=0
01910 L1910: if e(2)>=0 then goto L1950
01920   let e(1)=e(1)+e(2)
01930   let e(2)=0
01940   if e(1)<0 then let v6=v6+(-e(1))
01950 L1950: return 
01960 L1960: let mm=int(d1/10000)
01970   let dd=int((d1-mm*10000)/100)
01980   let yy=d1-(mm*10000+dd*100)
01990   ag0=mo(mm)+dd+yy*365+int(yy/4)
02000   if yy-int(yy/4)*4=0 and mm>2 then ag0=ag0+1
02010   return 
02020 L2020: if err=61 then pr fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L2040
02030   goto L2080
02040 L2040: pr newpage
02050   if err=4148 then pr fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L2070
02060   goto L2080
02070 L2070: pr fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
02080 L2080: pr fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
02090   input fields "24,60,C 1,N": quitcode$
02100   if rtrm$(uprc$(quitcode$))="Q" then goto L2140
02110   pr fields "23,3,C 78,N": ""
02120   pr fields "24,3,C 78,N": ""
02130   retry 
02140 L2140: goto XIT
