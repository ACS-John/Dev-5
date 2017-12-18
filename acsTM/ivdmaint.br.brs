00020 ! 
00030   on error goto L990
00040   library 'S:\Core\Library': fncno,fnxit,fntop,fncloseprn,fnopenprn,fnconsole
00050   dim cde$*6,des$*55,gl(3),cnam$*40,a$*5,cap$*128
00060   fntop(program$,cap$="Invoice Description")
00070   fncno(cno,cnam$)
00075   fnconsole(1)
00080   hp1=43-int(len(rtrm$(cnam$))/2)
00090 ! 
00100   io1$(1)="14,5,C 55,U,N": io1$(2)="16,40,N 10.2,U,N"
00110   io1$(3)="18,33,N 3,U,N": io1$(4)="18,38,N 6,U,N": io1$(5)="18,46,N 3,U,N"
00120   open #1: "Name="&env$('Q')&"\TMmstr\IVDesc.h"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\IVDIndex.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L980
00130 L130: pr newpage
00140   pr f "3,9,C 50,N": "INVOICE DESCRIPTION FILE MENU"
00150   pr f "4,6,C 72,N": "COMPANY NUMBER "&env$('cno')&"  "&ltrm$(cnam$)
00160   pr f "6,9,C 60": "1 = INITIAL FILE PREPARATION"
00170   pr f "7,9,C 60": "2 = ADD OR FILE MAINTENANCE / INQUIRY"
00180   pr f "8,9,C 60": "3 = pr PROOF LIST"
00190   pr f "10,9,C 60": "0 = COMPLETED (RETURN TO T/M MENU)"
00200   pr f "12,9,C 20": "ENTER SELECTION #:"
00210 L210: input fields "12,28,N 1,UE,N": ti conv L210
00220   if ti=0 then goto L390
00230   on ti goto L240,L510,L740 none L210
00240 L240: pr newpage
00250   ctab=40-int(len(ltrm$(rtrm$(cnam$)))/2)
00260   pr f "7,"&str$(ctab)&",C 40,N": cnam$
00270   pr f "10,31,C 19,RB,N": "  *** WARNING ***"
00280   pr f "12,4,C 70": "YOU HAVE CHOSEN TO INITIALLY PREPARE THE INVOICE DESCRIPTION FILE."
00290   pr f "13,1,C 78": "IF YOU CONTINUE ALL EXISTING INVOICE DESCRIPTION RECORDS WILL BE ERASED."
00300   pr f "15,6,C 70": "ENTER PASSWORD TO CONTINUE; ELSE PRESS ENTER TO RETURN TO MENU"
00310 L310: input fields "15,75,C 5,IE,N": a$ conv L310
00320   if uprc$(a$)="THINK" then goto L330 else goto L130
00330 L330: close #1: ioerr L340
00340 L340: open #1: "Name="&env$('Q')&"\TMmstr\IVDesc.h"&env$('cno')&",NoShr",internal,outin ioerr L360
00350   close #1,free: 
00360 L360: open #1: "Name="&env$('Q')&"\TMmstr\IVDesc.h"&env$('cno')&",Replace,RecL=84",internal,output ioerr L990
00370   ti=1
00380   new1=1
00390 L390: close #1: 
00400   if new1=0 then goto XIT
00470   execute "Index "&env$('Q')&"\TMmstr\IVDesc.h"&env$('cno')&","&env$('Q')&"\TMmstr\IVDIndex.h"&env$('cno')&",1,6,REPLACE,DupKeys" ioerr L480
00480 L480: if ti=1 then chain 'S:\acsTM\IVDMAINT'
00490   close #1: ioerr ignore
00500   goto XIT
00510 L510: pr newpage
00520   pr f "10,5,C 60": "ENTER DESCRIPTION CODE OR BLANK TO STOP:"
00530   input fields "10,50,C 6,UE,N": cde$
00540   if rtrm$(cde$)="" then goto L130
00550   cde$=uprc$(lpad$(rtrm$(cde$),6))
00560   des$=""
00570   da=0
00580   mat gl=(0)
00590   read #1,using L600,key=cde$: cde$,des$,da,mat gl nokey L630 ioerr L990
00600 L600: form pos 1,c 6,c 55,pd 5.2,n 3,n 6,n 3
00610   pr f "7,15,C 26,N": "***  FILE MAINTENANCE  ***"
00620   goto L640
00630 L630: pr f "7,15,C 26,N": "********   ADD   *********"
00640 L640: pr f "12,5,C 60": "ENTER INVOICE DESCRIPTION OR BLANK TO DELETE THIS ENTRY"
00650   pr f "16,5,C 60": "STANDARD CHARGE AMOUNT OR ZERO:"
00660   pr f "18,9,C 60": "GENERAL LEDGER NUMBER:"
00670   pr f mat io1$: des$,da,mat gl
00680 L680: input fields mat io1$: des$,da,mat gl conv L680
00690   if rtrm$(des$)="" then delete #1,key=cde$: nokey L510: new1=1: goto L510
00700   rewrite #1,using L600,key=cde$: cde$,des$,da,mat gl nokey L720
00710   goto L510
00720 L720: write #1,using L600: cde$,des$,da,mat gl: new1=1
00730   goto L510
00740 L740: pr newpage
00750   fnopenprn
00760   pr f "10,20,C 45": "INVOICE DESCRIPTION PROOF LISTING IN PROCESS"
00770   pr f "24,3,C 30,N": "Press F5 to stop!"
00780   on fkey 5 goto L880
00790   restore #1,key>="      ": nokey L130
00800   gosub L910
00810 L810: read #1,using L600: cde$,des$,da,mat gl eof L880 ioerr L990
00820   pr #255,using L830: cde$,des$,da,mat gl pageoflow L850
00830 L830: form pos 1,c 8,c 57,n 10.2,n 5,n 7,n 4,skip 1
00840   goto L810
00850 L850: pr #255: newpage
00860   gosub L910
00870   continue 
00880 L880: fncloseprn
00890   on fkey 5 ignore 
00900   goto L130
00910 L910: pr #255,using L920: date$,cnam$
00920 L920: form skip 2,pos 1,c 8,pos hp1,c 40,skip 1
00930   pr #255,using L940: time$,"INVOICE DESCRIPTION PROOF LISTING"
00940 L940: form pos 1,c 8,pos 26,c 33,skip 2
00950   pr #255: "  CODE  DESCRIPTION                                              STD AMOUNT   GL  NUMBER"
00960   pr #255: "------  -------------------------------------------------------  ----------  --- ------ ---"
00970   return 
00980 L980: if err=4152 then goto L360
00990 L990: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1010
01000   goto L1050
01010 L1010: pr newpage
01020   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L1040
01030   goto L1050
01040 L1040: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
01050 L1050: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
01060   input fields "24,60,C 1,N": quitcode$
01070   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
01080   pr f "23,3,C 78,N": ""
01090   pr f "24,3,C 78,N": ""
01100   retry 
01110 XIT: fnxit
01120 IGNORE: continue 
