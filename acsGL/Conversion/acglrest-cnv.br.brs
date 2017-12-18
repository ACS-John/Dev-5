00020   on error goto L200
00030   library 'S:\Core\Library': fnxit,fntop
00040   dim actpd$*6,pedat$*20,tb$*32
00050   fntop(program$,"CHANGE_ME")
00060   open #20: "Name=CNO.H"&wsid$,internal,input,relative 
00070   read #20,using L80,rec=1: cno,cnam$,dat$,cp,nw,process,actpd$,pedat$
00080 L80: form pos 1,n 2,c 40,pos 63,c 20,pos 89,2*n 1,pos 141,n 1,pos 153,c 6,pos 195,c 20
00090   dim ln1$*78,ln$(10)*78,shd$*60,fli$(20),cnam$*40,dat$*20
00100   execute "Copy "&env$('Q')&"\GLmstr\AcGLStmt.h"&env$('cno')&" x -D"
00110   execute "copy x "&env$('Q')&"\GLmstr\AcGLStmt.h"&env$('cno')
00120   open #1: "Name="&env$('Q')&"\GLmstr\AcGLStmt.h"&env$('cno')&",Shr",internal,outin,relative 
00130   open #2: "Name=test,size=0,RecL=128,replace",display,output 
00140 L140: read #1,using L150: mat ln$ eof L180 norec L180
00150 L150: form pos 1,10*c 78
00160   pr #2,using L150: mat ln$
00170   goto L140
00180 L180: close #1: : close #2: 
00190   execute "copy test "&env$('Q')&"\GLmstr\AcGLStmt.h"&env$('cno')
00200 L200: fnxit
00210   goto L260
00220   pr newpage
00230   if err=4148 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L250
00240   goto L260
00250 L250: pr f "23,1,C 80,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00260 L260: pr f "24,1,C 80,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00270   input fields "24,60,C 1,N": quitcode$
00280   if rtrm$(uprc$(quitcode$))="Q" then goto L320
00290   pr f "23,1,C 80,N": ""
00300   pr f "24,1,C 80,N": ""
00310   retry 
00320 L320: fnxit
