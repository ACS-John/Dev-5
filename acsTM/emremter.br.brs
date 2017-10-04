00020 ! 
00030   on error goto L320
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole
00050   fntop(program$,cap$="Employee")
00060   fncno(cno,cnam$)
00070 ! 
00080 ! 
00090   fnconsole(1)
00100   dim eno$*9,prg$*20
00110 L110: pr newpage
00120   pr f "8,25,c 30,r,n": "********  WARNING  ********"
00130   pr f "11,10,c 70": "THIS PROGRAM REMOVES ALL EMPLOYESS THAT ARE CODED"
00140   pr f "12,10,c 70": "AS TERMINATED.  BE SURE THAT THE FIRM PRODUCTIVITY"
00150   pr f "13,10,c 70": "REPORTS HAVE BEEN RUN BEFORE CONTINUING."
00160   pr f "15,10,c 62": "ENTER 1 TO CONTINUE; ELSE ENTER 2 TO RETURN TO THE SYSTEM MENU"
00170 L170: input fields "15,75,N 1,UE,N": a conv L170
00180   on a goto L190,L310 none L110
00190 L190: pr newpage
00200   pr f "10,20,c 50,n": "REMOVE TERMINATED EMPLOYEES IN PROCESS"
00210   open #1: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L320
00220 L220: read #1,using L230: eno$,emp eof L270 ioerr L320
00230 L230: form pos 1,c 9,pos 37,n 1
00240   if emp><9 then goto L220
00250   delete #1: 
00260   goto L220
00270 L270: close #1: 
00280   execute "Index "&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&","&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",1,9,REPLACE,DupKeys"
00290   if uprc$(rtrm$(prg$))="S:\acsTM\EMAINT" then chain 'S:\acsTM\EMAINT'
00300   goto XIT
00310 L310: goto XIT
00320 L320: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L340
00330   goto L380
00340 L340: pr newpage
00350   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L370
00360   goto L380
00370 L370: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00380 L380: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00390   input fields "24,60,C 1,N": quitcode$
00400   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
00410   pr f "23,3,C 78,N": ""
00420   pr f "24,3,C 78,N": ""
00430   retry 
00440 XIT: let fnxit
