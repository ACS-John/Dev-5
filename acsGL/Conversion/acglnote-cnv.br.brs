00020   on error goto L180
00030   library 'S:\Core\Library': fnxit,fntop
00040   dim actpd$*6,pedat$*20,tb$*32
00050   fntop(program$,"CHANGE_ME")
00060   open #20: "Name=CNO.H"&wsid$,internal,input,relative 
00070   read #20,using L80,rec=1: cno,cnam$,dat$,cp,nw,process,actpd$,pedat$
00080 L80: form pos 1,n 2,c 40,pos 63,c 20,pos 89,2*n 1,pos 141,n 1,pos 153,c 6,pos 195,c 20
00090   dim ln1$*78,ln$(10)*78,shd$*60,fli$(20),cnam$*40,dat$*20
00100   open #1: "Name="&env$('Q')&"\GLmstr\AcGLNote.h"&env$('cno')&",Shr",internal,outin,relative 
00110   open #2: "Name=test,size=0,RecL=128,replace",display,output 
00120 L120: read #1,using L130: mat ln$ eof L160 norec L160
00130 L130: form pos 1,10*c 78
00140   pr #2,using L130: mat ln$
00150   goto L120
00160 L160: close #1: : close #2: 
00170   execute "copy test "&env$('Q')&"\GLmstr\AcGLNote.h"&env$('cno')
00180 L180: fnxit
00190   goto L240
00200   pr newpage
00210   if err=4148 then pr f "23,1,C 80,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L230
00220   goto L240
00230 L230: pr f "23,1,C 80,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
00240 L240: pr f "24,1,C 80,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
00250   input fields "24,60,C 1,N": quitcode$
00260   if rtrm$(uprc$(quitcode$))="Q" then goto L300
00270   pr f "23,1,C 80,N": ""
00280   pr f "24,1,C 80,N": ""
00290   retry 
00300 L300: fnxit
