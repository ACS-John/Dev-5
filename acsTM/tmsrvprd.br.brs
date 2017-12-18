00020   on fkey 5 goto L470
00030   on error goto L920
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole
00050   fntop(program$,cap$="Service Production Report")
00060   fncno(cno,cnam$)
00070   fnconsole(1)
00080   fnopenprn
00090   dim cat$(30)*30
00100   dim sc$*4,ds$*30,catno$*2,cnam$*40,cap$*128
00110   namtab=41-int(len(rtrm$(cnam$))/2)
00120   open #1: "Name="&env$('Q')&"\TMmstr\SCMSTR.H"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\SCIndex.H"&env$('cno')&",Shr",internal,input,keyed ioerr L920
00130   open #2: "Name="&env$('Q')&"\TMmstr\TMCat.h"&env$('cno')&",Shr",internal,input,relative ioerr L920
00140   read #2,using L150: mat cat$ ioerr L920
00150 L150: form pos 1,30*c 30
00160   close #2: 
00170   open #8: "Name="&env$('Q')&"\TMmstr\pedate.h"&env$('cno')&",RecL=20,use,Shr",internal,outin,relative 
00180   if lrec(8)=0 then write #8,using "form pos 1,n 6": d1 else read #8,using "form pos 1,n 6",rec=1,release: dat
00190   pr newpage
00200   pr f "10,5,c 57,n": "ENTER DATE FOR SERVICE PRODUCTION REPORT IN MMDDYY FORMAT"
00210   pr f "10,65,n 6,n": dat
00220   pr f "13,30,c 20": "Press F5 to Stop"
00230 L230: input fields "10,65,n 6,eu,n": dat conv L230
00240   if cmdkey=5 then goto XIT
00250   if dat<10100 or dat>123199 then goto L230
00260   rewrite #8,using "form pos 1,n 6",rec=1: dat
00270   close #8: 
00280   pr newpage
00290   pr f "10,15,c 57,n": "PRINT SERVICE PRODUCTION REPORT IN PROCESS"
00300   pr f "23,2,C 30,N": "Press F5 to stop"
00310   gosub L800
00320 L320: read #1,using L330: sc$,ds$,th,sf eof L460 ioerr L920
00330 L330: form pos 1,c 4,c 30,pd 4.2,pd 5.2
00340   catno$=sc$(1:2)
00350   catno=val(catno$)
00360   if fst=1 then goto L390
00370   fst=1
00380   gosub L500
00390 L390: if hcatno><catno then goto L420
00400   gosub L540
00410   goto L320
00420 L420: gosub L700
00430   gosub L500
00440   gosub L540
00450   goto L320
00460 L460: gosub L700
00470 L470: close #1: ioerr L480
00480 L480: fncloseprn
00490 XIT: fnxit
00500 L500: pr #255,using L510: catno*100,cat$(catno)
00510 L510: form pos 2,pic(zzzz),pos 8,c 30,skip 1
00520   hcatno=catno
00530   return 
00540 L540: if th><0 then goto L600
00550   pr #255,using L640: sc$,ds$,th,sf,0 pageoflow L570
00560   goto L650
00570 L570: pr #255: newpage
00580   gosub L800
00590   goto L650
00600 L600: pr #255,using L640: sc$,ds$,th,sf,sf/th pageoflow L620
00610   goto L650
00620 L620: pr #255: newpage
00630   gosub L800
00640 L640: form pos 5,c 4,pos 14,c 30,pos 53,n 13.2,n 13.2,n 13.2,skip 1
00650 L650: b1=b1+th
00660   b2=b2+sf
00670   m$=r$
00680   hcatno=catno
00690   return 
00700 L700: if b1=0 then goto L730
00710   b0=b2/b1
00720   if b0<10000 then goto L740
00730 L730: b0=0
00740 L740: pr #255,using L750: "TOTAL",cat$(hcatno),b1,b2,b0
00750 L750: form pos 17,c 5,pos 23,c 30,pos 53,n 13.2,n 13.2,n 13.2,skip 1
00760   b1=0
00770   b2=0
00780   pr #255: 
00790   return 
00800 L800: p1=p1+1
00810   pr #255,using L820: date$,cnam$,"PAGE",p1
00820 L820: form skip 3,pos 1,c 8,pos namtab,c 40,pos 75,c 5,pic(zzz),skip 1
00830   pr #255,using L840: time$,"SERVICE PRODUCTION REPORT"
00840 L840: form pos 1,c 8,pos 29,c 25,skip 1
00850   pr #255,using L860: "FOR YEAR-TO-DATE",dat
00860 L860: form pos 29,c 16,pos 46,pic(zz/zz/zz),skip 4
00870   pr #255,using L880: "SERVICE","CHARGED","AVERAGE"
00880 L880: form pos 4,c 7,pos 70,c 7,pos 83,c 7,skip 1
00890   pr #255,using L900: "CODE","DESCRIPTION","HOURS","AT STANDARD","HOURLY RATE"
00900 L900: form pos 5,c 4,pos 18,c 11,pos 60,c 5,pos 68,c 11,pos 81,c 11,skip 3
00910   return 
00920 L920: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L940
00930   goto L980
00940 L940: pr newpage
00950   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L970
00960   goto L980
00970 L970: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00980 L980: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00990   input fields "24,60,C 1,N": quitcode$
01000   if rtrm$(uprc$(quitcode$))="Q" then goto L1040
01010   pr f "23,3,C 78,N": ""
01020   pr f "24,3,C 78,N": ""
01030   retry 
01040 L1040: goto XIT
