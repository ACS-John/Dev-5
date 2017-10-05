00020   library 'S:\Core\Library': fnopenprn,fncloseprn
00030   on fkey 5 goto L830
00040   on error goto L860
00050 ! 
00060   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$
00070   fntop(program$,cap$="Client Directory")
00080   fncno(cno,cnam$)
00090   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,input,relative ioerr L860
00100   read #1,using L110: mat cat$ ioerr L860
00110 L110: form pos 1,10*c 30
00120   close #1: 
00130   for j=1 to 10
00140     flo$(j)=str$(j+10)&",30,C 30,N"
00150     fli$(j)=str$(j+10)&",62,N 1,U,N"
00160   next j
00170   dim z$*5,a$(5)*30,ph$*12,cnam$*40,prg$*20,cm$*70
00180   dim cat$(10)*30,flo$(10),fli$(10),catcode(10),dd(10),ph2$*12
00190   namtab=44-len(rtrm$(cnam$))/2
00200   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.h"&str$(cno)&",Shr",internal,input,keyed ioerr L860
00210 L210: pr newpage
00230   pr f "10,10,c 48,n": "ENTER DATE FOR CLIENT DIRECTORY IN MMDDYY FORMAT"
00240   pr f "10,60,n 6,n": dat
00250 L250: input fields "10,60,n 6,eu,n": dat conv L250
00260   if dat<10100 or dat>123199 then goto L210
00280   form pos 83,n 6
00290   pr newpage
00300   pr f "3,15,C 65,N": "You have the option to get the client directory sorted by "
00310   pr f "4,10,c 70,n": "category.  Place a 1 by any category you want printed.  Leave all"
00320   pr f "5,10,c 70,n": "items blank for numeric order."
00330   pr f mat flo$: mat cat$
00340 L340: input fields mat fli$: mat catcode conv L340
00350   if sum(catcode)=0 then numprint=1
00360   pr newpage
00370   pr f "10,25,c 48,n": "CLIENT DIRECTORY IN PROCESS"
00380   pr f "23,2,c 30,n": "Press F5 to stop"
00390   fnopenprn(cp,0,0,process)
00400   form c 9,skip 0
00410   if numprint=1 then goto L440
00420   for j=1 to 10
00430     if catcode(j)=0 then goto L790
00440 L440: gosub L610
00450 L450: read #1,using L460: z$,mat a$,ph$,pno,mye,mat dd,ph2$,cm$ eof L760 ioerr L860
00460 L460: form pos 1,c 5,5*c 30,c 12,pos 179,n 9,n 2,pos 190,10*pd 3,pos 260,c 12,pos 305,c 70
00470     if numprint =1 then goto L490
00480     if dd(j)>0 then goto L490 else goto L450
00490 L490: pr #255,using L500: z$,a$(1),"BUS PHONE:",ph$,mye,pno
00500 L500: form pos 2,c 5,pos 8,c 30,pos 39,c 10,pos 50,c 12,pos 70,pic(zzz),pos 76,pic(zzzzzzzzz),skip 1
00510     pr #255,using L520: a$(2),"HOME PHONE:",ph2$
00520 L520: form pos 8,c 30,pos 39,c 11,pos 51,c 30,skip 1
00530     pr #255,using L540: a$(3),"CONTACT:",a$(4) pageoflow L580
00540 L540: form pos 8,c 30,pos 39,c 8,pos 48,c 30,skip 1
00550     pr #255,using L560: cm$
00560 L560: form pos 8,c 70,skip 2
00570     goto L450
00580 L580: pr #255: newpage
00590     gosub L610
00600     goto L450
00610 L610: p1=p1+1
00620     pr #255,using L630: date$,cnam$,"PAGE",p1
00630 L630: form skip 3,pos 1,c 8,pos namtab,c 40,pos 76,c 5,n 4,skip 1
00640     pr #255,using L650: time$,"CLIENT DIRECTORY"
00650 L650: form pos 1,c 8,pos 36,c 16,skip 1
00660     if numprint = 1 then cattab=37 else cattab=44-len(rtrm$(cat$(j)))/2
00670     if numprint = 1 then pr #255,using L680: "NUMERIC ORDER" else pr #255,using L680: cat$(j)
00680 L680: form pos cattab,c 30,skip 1
00690     pr #255,using L700: dat
00700 L700: form pos 40,pic(zz/zz/zz),skip 3
00710     pr #255,using L720: "CLIENT","YEAR","EMPLOYEE"
00720 L720: form pos 2,c 6,pos 70,c 4,pos 77,c 8,skip 1
00730     pr #255,using L740: "NUMBER","NAME AND ADDRESS","CLIENT INFORMATION","END","IN CHARGE"
00740 L740: form pos 2,c 6,pos 12,c 16,pos 42,c 19,pos 71,c 3,pos 76,c 9,skip 2
00750     return 
00760 L760: if numprint=1 then goto L800
00770     pr #255: newpage
00780     restore #1,key>="     ": nokey L800
00790 L790: next j
00800 L800: close #1: ioerr L830
00810   if numprint =1 then goto L830 else let fncloseprn
00820   goto L840
00830 L830: fncloseprn
00840 L840: if uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" then chain prg$
00850   goto XIT
00860 L860: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L880
00870   goto L920
00880 L880: pr newpage
00890   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L910
00900   goto L920
00910 L910: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00920 L920: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY OR ENTER  Q  TO QUIT"
00930   input fields "24,60,C 1,N": quitcode$
00940   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00950   pr f "23,3,C 78,N": ""
00960   pr f "24,3,C 78,N": ""
00970   retry 
00980 XIT: fnxit
