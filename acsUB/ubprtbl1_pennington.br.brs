00010 ! REPLACE S:\acsUB\ubprtbl1_Pennington
00020   on error goto L990
00030   on fkey 5 goto L450
00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fncno,fnLastBillingDate,fnxit,fnCmdSet,fntop,fnformnumb$,fnpause,fnopenprn,fncloseprn,fnCmdKey,fnconsole
00050   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),fli$(5),cap$*128
00051   dim csz$*30
00060   fncno(cno,cnam$) !:
        fnLastBillingDate(d1)
00075   fnconsole(on=1)
00080   def fnc(x)=int(100*(x+sgn(x)*.0001))
00090   pr newpage
00100   pr f "06,20,Cc 40,R,n": "POSITION BILLING FORMS IN THE PRINTER"
00110   pr f "08,23,c 26,N": " DATE OF BILLING (MMDDYY):"
00120   pr f "10,23,c 26,n": "DUE DATE OF BILL (MMDDYY):"
00130   pr f "12,23,c 26,n": " SERVICE FROM DATE (MMDD):"
00140   pr f "14,23,c 26,n": "   SERVICE TO DATE (MMDD):"
00150   pr f "16,20,Cc 40,R,N": "PRESS F5 TO STOP"
00160   fli$(1)="08,50,Nz 6,U,N"
00170   fli$(2)="10,50,Nz 6,CU,N"
00180   fli$(3)="12,50,Nz 4,U,N"
00190   fli$(4)="14,50,nz 4,U,n"
00200 L200: rinput fields mat fli$,attr "R": d1,d2,d3,d4 conv L200
00210   if cmdkey=5 then goto DONE
00220   if d1=0 then goto L240
00230   if d1<10100 or d1>123199 then goto L200
00240 L240: if d2=0 then goto L260
00250   if d2<10100 or d2>123199 then goto L200
00260 L260: if d3=0 then goto L280
00270   if d3<101 or d3>1231 then goto L200
00280 L280: if d4=0 then goto L300
00290   if d4<101 or d4>1231 then goto L200
00300 L300: open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00305 ! open #255: "Name=prn:/select,size=0,RecL=128,replace",display,output
00310   open #3: "Name="&env$('Q')&"\UBmstr\UBAdrBil.H"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\adrIndex.H"&env$('cno')&",Shr",internal,input,keyed 
00316   form pos 1,c 9,skip 0
00320   goto L880
00330 L330: gosub L720
00340   if numb><0 then goto L360
00350 L350: read #1,using L360: z$,mat e$,f$,a4,mat b,mat d,bal,f,mat g,bkno eof L450
00360 L360: form pos 1,c 10,4*c 30,c 12,pos 149,pd 2,pos 157,11*pd 4.2,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 1741,n 2
00370   if startcd=1 then goto L380 else goto L390
00380 L380: if val(prtbkno$(1:2))=bkno then goto L390 else goto L450
00390 L390: if f><d1 then goto L350
00400   csz$=""
00410   read #3,using L420,key=z$: e$(2),e$(3),e$(4),csz$ nokey L430
00420 L420: form pos 11,4*c 30
00430 L430: gosub L500
00440   goto L350
00450 L450: close #1: ioerr L460
00460 L460: close #3: ioerr L470
00470 L470: if nw<>1 then pr #255: newpage
00480   close #255: 
00490 L490: goto DONE
00500 L500: pb=bal-g(11)
00505   if g(2)=0 then x$="" else x$="Sewer" ! only show sewer if have charge
00510   pr #255,using L520: 'Water',g(1)+g(9),x$,g(2),d3,d4,d2,bal,bal+g(10)+g(7)
00520 L520: form skip 3,pos 45,c 9,n 10.2,skip 1,pos 45,c 9,pic(zzz,zzz.zz),skip 1,pos 11,pic(zzbzz),pos 20,pic(zzbzz),pos 30,pic(zzbzzbzz),skip 2,pos 48,n 9.2,n 10.2,skip 1
00530   pr #255,using L540: d(2),d(1),int(d(3)/100),g(1),z$
00540 L540: form pos 9,n 8,n 8,n 7,n 9.2,pos 45,c 30,skip 1
00550   if g(2)=0 then pr #255,using L560: e$(2) else pr #255,using L570: "Sewer",g(2),e$(2)
00560 L560: form pos 45,c 30,skip 1
00570 L570: form pos 26,c 5,pos 32,n 9.2,pos 45,c 30,skip 1
00580   if g(5)=0 then pr #255,using L560: e$(3) else pr #255,using L570: "Sanit",g(5),e$(3)
00590   if g(8)=0 then pr #255,using L560: e$(4) else pr #255,using L570: "Other",g(8),e$(4)
00600   if pb=0 then pr #255,using L610: csz$ else pr #255,using L620: "Prew",pb,csz$
00610 L610: form pos 45,c 30,skip 1
00620 L620: form pos 26,c 5,pos 32,n 9.2,pos 45,c 30,skip 1
00630   pr #255,using L640: g(9)
00640 L640: form skip 1,pos 32,n 9.2,skip 2
00650   pr #255,using L660: bal+g(10)+g(7),z$,bal
00660 L660: form pos 8,n 10.2,x 3,c 10,pos 32,n 9.2,skip 5
00665   count=count+1: if int(count/3)=count/3 then pr #255: newpage
00670   return 
00680   goto L490
00690   ! NOKEY GO pr WITHOUT NAME
00700   mat e$=(" ")
00710   goto L430
00720 L720: pr newpage
00730   pr f "12,14,c 40,N": "Starting Account (Blank for All):"
00740 L740: input fields "12,55,C 10,U,N": a$
00750   numb=val(a$) conv L740
00760   if numb=0 then goto L840
00770   a$=lpad$(str$(numb),10)
00780   ppos=pos (a$,".",1)
00790   if ppos<1 or ppos>7 then goto L800 else goto L740
00800 L800: if ppos=8 then goto L830
00810   if ppos=9 then a$=a$(2:10)&"0"
00820   if ppos=0 then a$=a$(4:10)&".00"
00830 L830: read #1,using L360,key=a$: z$,mat e$,f$,a4,mat b,mat d,bal,f,mat g,bkno nokey L720
00840 L840: pr newpage
00850   pr f "10,20,Cc 40,R,N": "Printing Utility Bills: Please wait..."
00860   pr f "12,20,Cc 40,R,N": "Press F5 to stop"
00870   return 
00880 L880: pr newpage
00890   pr f "10,20,C 37,N": "Book Number to pr (Blank for All):"
00900 L900: input fields "10,58,Nz 2,U,N": prtbkno conv L900
00910   if prtbkno=0 then goto L330
00920   pr newpage
00930   pr f "10,20,Cc 40,R,N": "Printing Utility Bills: Please wait..."
00940   pr f "12,20,Cc 40,R,N": "Press F5 to stop"
00950   prtbkno$=lpad$(str$(prtbkno),2)&"00000.00"
00960   startcd=1
00970   read #1,using L360,key>=prtbkno$: z$,mat e$,f$,a4,mat b,mat d,bal,f,mat g,bkno nokey L450
00980   goto L390
00990 L990: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L1010
01000   goto L1050
01010 L1010: pr newpage
01020   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L1040
01030   goto L1050
01040 L1040: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
01050 L1050: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
01060   input fields "24,60,C 1,N": quitcode$
01070   if rtrm$(uprc$(quitcode$))="Q" then goto DONE
01080   pr f "23,3,C 78,N": ""
01090   pr f "24,3,C 78,N": ""
01100   retry 
01110 DONE: fnxit
