00010 ! Replace S:\acsPR\Conversion\pr-Fix
00020 ! beats me...
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim ta(2)
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 ! 
00110   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno),internal,outin,keyed 
00120   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno),internal,outin,relative 
00130 L130: read #1,using L140: lpd,tgp,mat ta eof XIT
00140 L140: form pos 162,n 6,pd 5.2,2*pd 3
00150   if lpd><0 then goto L130
00160   tgp=0
00170   r2=ta(1)
00180 L180: if r2=0 then goto L250
00190   read #2,using L200,rec=r2: td4,gpd,nta
00200 L200: form pos 42,n 6,pos 458,pd 5.2,pos 468,pd 3
00210   if td4><42394 then goto L230
00220   tgp+=gpd
00230 L230: r2=nta
00240   goto L180
00250 L250: if tgp=0 then goto L130
00260   rewrite #1,using L140: 32494,tgp
00270   goto L130
00280 ! ______________________________________________________________________
00290 XIT: stop 
00300 ! ______________________________________________________________________
00310 ! <updateable region: ertn>
00320 ERTN: fnerror(program$,err,line,act$,"xit")
00330   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00340   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00350   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00360 ERTN_EXEC_ACT: execute act$ : goto ERTN
00370 ! /region
00380 ! ______________________________________________________________________
