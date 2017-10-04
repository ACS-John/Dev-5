00020 ! 
00030   on error goto L410
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$
00050   fntop(program$,cap$="Category")
00060   fncno(cno,cnam$)
00070   dim a$(30)*30,fli$(30),flo$(32),hd$(2)*40,b$(30)*2
00080   pr newpage
00090   pr f "10,20,c 30,h,n": "CHANGE CATEGORIES IN PROCESS"
00100   open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Shr",internal,outin,relative ioerr L120
00110   goto L140
00120 L120: open #1: "Name="&env$('Q')&"\TMmstr\TMCat.h"&str$(cno)&",Replace,RecL=900",internal,outin,relative ioerr L410
00130   write #1,using L370,rec=1: mat a$
00140 L140: read #1,using L370,rec=1: mat a$ ioerr L410
00150   for j=1 to 10
00160     b$(j)=lpad$(str$(j),2)
00170     fli$(j)=str$(j+1)&",26,c 30,u,n"
00180     flo$(j)=str$(j+1)&",23,c 2,u,n"
00190   next j
00200   for j=11 to 20
00210     b$(j)=str$(j)
00220     b$(j+10)=str$(j+10)
00230     fli$(j)=str$(j+3)&",6,c 30,u,n"
00240     flo$(j)=str$(j+3)&",3,c 2,u,n"
00250     fli$(j+10)=str$(j+3)&",46,c 30,u,n"
00260     flo$(j+10)=str$(j+3)&",43,c 2,u,n"
00270   next j
00280   flo$(31)="1,28,c 40,h,n"
00290   flo$(32)="13,30,c 40,h,n"
00300   let hd$(1)="CHARGEABLE CATEGORIES"
00310   let hd$(2)="NON CHARGEABLE CATEGORIES"
00320   pr newpage
00330   pr f mat flo$: mat b$,mat hd$
00340   pr f mat fli$: mat a$
00342   pr f "24,30,c 40": "Enter to Save   F5 to Cancel"
00350   input fields mat fli$: mat a$
00355   if cmdkey=5 then pr newpage: goto XIT
00360   rewrite #1,using L370,rec=1: mat a$
00370 L370: form pos 1,30*c 30
00380   pr newpage
00390   close #1: 
00400 XIT: let fnxit
00410 L410: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L430
00420   goto L470
00430 L430: pr newpage
00440   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L460
00450   goto L470
00460 L460: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00470 L470: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00480   input fields "24,60,C 1,N": quitcode$
00490   if rtrm$(uprc$(quitcode$))="Q" then goto L530
00500   pr f "23,3,C 78,N": ""
00510   pr f "24,3,C 78,N": ""
00520   retry 
00530 L530: goto XIT
