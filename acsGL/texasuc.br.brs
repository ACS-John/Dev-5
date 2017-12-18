00010 ! Replace S:\acsGL\TexasUC
00020 ! Texas Unemployment Compensation Report (Not on a menu, but can leave in)
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnwait,fnwin3b,fnopenprn,fncloseprn,fnprocess,fnchain,fncno,fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim k(1),k$(3)*25,l$(1)*11,d(14),m(20),n(2),m$*5
00080   dim cap$*128,message$*40,cnam$*40
00090   dim a$(3)*40,b$(2)*12,c$*5,e(2),e$(2)*11
00100 ! ______________________________________________________________________
00110   fntop(program$,cap$="Texas Unemployment Compensation Report")
00120   fncno(cno,cnam$)
00130   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input  !:
        read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,POS 188,PD 7.2': mat a$,mat b$,c$,ucm !:
        close #1: 
00140   if fnprocess=1 then goto L220
00150 SCR1: ! 
00160   fnwin3b(win=101,cap$,5,44,1,3,5)
00170   pr #win,fields "4,2,C 36,N": "Quarterly Period Ending Date (q-yy):"
00180 L180: input #win,fields "4,39,C 5,UT,N": m$ conv L180
00190   close #win: 
00200   if cmdkey=5 or cmdkey=99 then goto XIT
00210 ! ______________________________________________________________________
00220 L220: open #2: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00230   message$="Printing: please wait..." !:
        fnwait(message$,1)
00240   on fkey 5 goto XIT
00250   fnopenprn
00260   gosub L400
00270 L270: read #2,using L280: mat k,mat k$,mat l$,mat m eof L480
00280 L280: form pos 1,n 4,3*c 25,c 11,18*pd 5.2,2*n 5
00290   if m(2)=0 or k(1)=0 then goto L380
00300   if p1<61 then goto L340
00310   gosub L620
00320   pr #255: newpage
00330   gosub L400
00340 L340: gosub L530
00350   t1=t1+m(2)
00360   t2=t2+h3
00370   t3=t3+h2
00380 L380: goto L270
00390 ! ______________________________________________________________________
00400 L400: p2=p2+1
00410   pr #255,using L420: b$(2)(1:11),b$(1)(1:11),m$
00420 L420: form skip 6,pos 5,c 11,pos 49,c 11,pos 60,c 5,skip 4
00430   pr #255,using L440: a$(1),p2
00440 L440: form pos 5,c 40,pos 51,n 3,skip 6
00450   p1=16
00460   return 
00470 ! ______________________________________________________________________
00480 L480: gosub L620
00490   close #2: 
00500   fncloseprn
00510   fnchain("S:\acsGL\PRSTATUC")
00520 ! ______________________________________________________________________
00530 L530: p3=p3+1
00540   for ln=len(rtrm$(k$(1))) to 1 step -1
00550     if k$(1)(ln:ln)=" " then goto L570
00560   next ln
00570 L570: pr #255,using L580: l$(1),k$(1)(1:1),k$(1)(ln+1:ln+17),m(2)
00580 L580: form pos 6,c 11,pos 20,c 1,pos 27,c 17,pos 46,n 10.2,skip 2
00590   p1=p1+2
00600   return 
00610 ! ______________________________________________________________________
00620 L620: p1=p1+1
00630   for j1=1 to 63-p1
00640     pr #255: 
00650     p1=p1-1
00660   next j1
00670   pr #255,using L680: t1
00680 L680: form pos 46,n 10.2
00690   pr #255: newpage
00700   t1=0
00710   return 
00720 ! ______________________________________________________________________
00730 XIT: fnxit
00740 ! ______________________________________________________________________
00750 ! <Updateable Region: ERTN>
00760 ERTN: fnerror(program$,err,line,act$,"xit")
00770   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00780   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00790   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00800 ERTN_EXEC_ACT: execute act$ : goto ERTN
00810 ! /region
00820 ! ______________________________________________________________________
