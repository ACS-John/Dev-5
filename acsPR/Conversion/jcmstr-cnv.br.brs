00010 ! Replace S:\acsPR\Conversion\JCmstr-Cnv
00020 ! Convert Job Cost and Category Files
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim jn$*6,n$*40,a$(3)*30,b(4),cn$*11,k$*25,l(13),ta(2)
00075   dim contact$*30,ph$*12,email$*60
00080 L80: pr newpage
00090   close #101: ioerr L100
00100 L100: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=CONVERT JOB COST AND CATEGORY FILES",display,outin 
00110   pr f "10,10,C 60": "COMPANY NUMBER (0 to stop):"
00120   pr f "12,20,C 32,R,N": "PRESS F1 TO CONTINUE; F5 TO STOP"
00130   input fields "10,56,Nz 5,U,N": cno
00140   if cmdkey=5 then goto XIT
00150 ! 
00160   execute "Copy "&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",X -D -n"
00170   open #2: "Name=X",internal,outin,relative 
00180   open #1: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",RecL=300,Replace",internal,outin,relative 
00190   for j=1 to lrec(2)
00200     read #2,using L210,rec=j: jn$,n$,mat a$,mat b
00210 L210: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 5.2,n 2
00220     if pcde=0 then pdte=0 else pdte=lpd
00230     write #1,using L231: jn$,n$,mat a$,mat b,contact$,ph$,email$
00231 L231: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2,c 30,c 12,c 60
00250   next j
00260   close #1: 
00270   close #2: 
00280   execute "Index "&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&","&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",1,6,Replace,DupKeys -n"
00290   execute "Copy "&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",X -D -n"
00300   open #2: "Name=X",internal,outin,relative 
00310   open #1: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",RecL=123,Replace",internal,outin,relative 
00320   for j=1 to lrec(2)
00330     read #2,using L340: cn$,k$,mat l,mat ta
00340 L340: form pos 1,c 11,c 25,11*pd 5.2,2*pd 2,2*pd 3
00350     write #1,using L360: cn$,k$,mat l,mat ta
00360 L360: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
00370   next j
00380   close #1: 
00390   close #2: 
00400   execute "Index "&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&","&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",1,11,Replace,DupKeys"
00410   pr f "12,5,C 60": "COMPLETED CONVERTING JOB FILE FOR COMPANY #: "&str$(cno)
00420   pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00430   input fields "13,40,C 1,IAE,N": pause$
00440   goto L80
00450 ! ______________________________________________________________________
00460 ! <updateable region: ertn>
00470 ERTN: fnerror(program$,err,line,act$,"xit")
00480   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00490   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00500   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00510 ERTN_EXEC_ACT: execute act$ : goto ERTN
00520 ! /region
00530 ! ______________________________________________________________________
00540 XIT: fnxit
00550 ! ______________________________________________________________________
