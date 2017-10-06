00010 ! Replace S:\acsPR\Conversion\CnvAll-Cnv
00020 ! CONVERT MASTER FILES
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnFree
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim gl(3),ta(2),k$*20,de$*30,fgl$(3)
00080 ! ______________________________________________________________________
00090 L90: pr newpage
00100   close #101: ioerr L110
00110 L110: open #101: "SROW=9,SCOL=9,EROW=11,ECOL=59,BORDER=DR,CAPTION=CONVERT MASTER FILES",display,outin 
00120   pr f "10,10,C 45": "COMPANY NUMBER TO CONVERT (0 TO STOP):"
00130   input fields "10,56,N 5,UE,N": cno
00140   if cno=0 then goto XIT
00160   open #1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno),internal,outin,keyed 
00170   open #2: "Name="&env$('Q')&"\CLmstr\PayAlloc.h"&str$(cno)&",SIZE=0,RecL=56,Replace",internal,outin,relative 
00180 L180: read #1,using 'Form POS 1,C 8,POS 147,N 3,N 6,N 3': p$,mat gl eof L280
00190   mat ta=(0)
00200 ! IF SUM(GL)=0 THEN GOTO 190
00210   lr2=lrec(2)+1
00220   write #2,using L230,rec=lr2: p$,mat gl,100,"",0
00230 L230: form pos 1,c 8,n 3,n 6,n 3,pd 3.2,c 30,pd 3
00240   mat ta=(lr2)
00250   rewrite #1,using L260: mat ta
00260 L260: form pos 147,2*pd 3
00270   goto L180
00280 L280: close #1: 
00290   close #2: 
00300   execute "Copy "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",X -D -n"
00310   execute "Copy X "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&"  -95 -n"
00320   execute "Index "&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&","&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno)&",1,20,Replace,DupKeys -n"
00330   open #1: "Name="&env$('Q')&"\CLmstr\PayTrans.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\UnPdIdx1.h"&str$(cno),internal,outin,keyed 
00340   open #2: "Name="&env$('Q')&"\CLmstr\UnPdAloc.h"&str$(cno)&",SIZE=0,RecL=70,Replace",internal,outin,relative 
00350 L350: read #1,using L360: k$,mat gl,de$,amt eof L460
00360 L360: form pos 1,c 20,pos 33,n 3,n 6,n 3,c 18,n 10.2
00370   mat ta=(0)
00380   lr2=lrec(2)+1
00390   write #2,using L400,rec=lr2: k$,mat gl,amt,de$,0
00400 L400: form pos 1,c 20,n 3,n 6,n 3,pd 5.2,c 30,pd 3
00410   mat ta=(lr2)
00420   rewrite #1,using L430: "",mat ta
00430 L430: form pos 33,c 12,pos 90,2*pd 3
00440   goto L350
00450 ! ______________________________________________________________________
00460 L460: close #1: 
00470   close #2: 
00480   open #1: "Name="&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\FundIdx1.h"&str$(cno),internal,outin,keyed ioerr NF1
00490   open #2: "Name=X,RecL=63,Replace",internal,output 
00500 L500: read #1,using L510: dn$,de$,mat fgl$ eof L560
00510 L510: form pos 1,c 3,c 30,3*c 9
00520   write #2,using L530: dn$,de$,fgl$(1),fgl$(2),0,fgl$(3)
00530 L530: form pos 1,c 3,c 30,2*c 9,n 3,c 9
00540   goto L500
00550 ! ______________________________________________________________________
00560 L560: close #1: 
00570 L570: close #2: 
00580   execute "COPY X,"&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&" -n"
00590   execute "Index "&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&","&env$('Q')&"\CLmstr\FundIdx1.h"&str$(cno)&",1,3,Replace,DupKeys -n"
00600   execute "Index "&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&","&env$('Q')&"\CLmstr\FundIdx2.h"&str$(cno)&",4,28,Replace,DupKeys -n"
00610   goto L730
00620 ! ______________________________________________________________________
00630 L630: pr newpage
00640   pr f "12,5,C 60": "COMPLETED CONVERTING FILES FOR COMPANY #: "&str$(cno)
00650   pr f "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00660   input fields "13,40,C 1,IAE,N": pause$
00670   goto L90
00680 ! ______________________________________________________________________
00690 NF1: open #2: "Name=X,RecL=63,Replace",internal,output 
00700   write #2,using L530: "  0","GENERAL FUND","","",0,""
00710   goto L570
00720 ! ______________________________________________________________________
00730 L730: ! Replace S:\acsCL\TRALLOC.CNV
00740   execute "Copy "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&" X -79 -n"
00750   fnFree(env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno))
00760   execute "Rename X "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&" -n"
00770   goto L630
00780 ! ______________________________________________________________________
00790 ! <Updateable Region: ERTN>
00800 ERTN: fnerror(program$,err,line,act$,"xit")
00810   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00820   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00830   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00840 ERTN_EXEC_ACT: execute act$ : goto ERTN
00850 ! /region
00860 ! ______________________________________________________________________
00870 XIT: stop 
00880 ! ______________________________________________________________________
