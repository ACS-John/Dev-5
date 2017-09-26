00030   on error goto L280
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$,fnconsole,fnd1
00050   let fntop(program$,cap$="Client")
00060   let fncno(cno,cnam$)
00070   let fnconsole(1)
00080   dim eno$*9,cchrs(10),ncchrs(20),ccamt(10),nccamt(20),prg$*20
00090 L90: print newpage
00100   print fields "8,25,c 30,r,n": "********  Warning  ********"
00110   print fields "11,10,c 70": "This program zeros the current period totals in"
00120   print fields "12,10,c 70": "the employee master file.  Be sure you have run"
00130   print fields "13,10,c 70": "all of the current period reports before continuing."
00140   print fields "15,10,c 62": "[1] Continue  [2] Exit"
00150 L150: input fields "15,75,n 1,eu,n": a conv L150
00160   on a goto L170,XIT none L90
00170 L170: print newpage
00180   print fields "10,10,c 60,n": "ZERO CURRENT FIELDS IN EMPLOYEE MASTER FILE IN PROCESS"
00190   open #1: "Name="&env$('Q')&"\TMmstr\EMmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\EMIndex.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L280
00200 L200: read #1,using L210: eno$ eof L250 ioerr L280
00210 L210: form pos 1,c 9
00220   rewrite #1,using L230: mat cchrs,mat ncchrs,mat ccamt,mat nccamt
00230 L230: form pos 38,30*pd 4.2,pos 278,30*pd 5.2
00240   goto L200
00250 L250: close #1: 
00260   if uprc$(rtrm$(prg$))="S:\acsTM\EMAINT" then chain prg$
00270 XIT: let fnxit
00280 L280: if err=61 then print fields "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L300
00290   goto L340
00300 L300: print newpage
00310   if err=4148 then print fields "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L330
00320   goto L340
00330 L330: print fields "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00340 L340: print fields "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00350   input fields "24,60,C 1,N": quitcode$
00360   if rtrm$(uprc$(quitcode$))="Q" then goto L400
00370   print fields "23,3,C 78,N": ""
00380   print fields "24,3,C 78,N": ""
00390   retry 
00400 L400: goto XIT
