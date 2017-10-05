00010 ! Replace S:\acsGL\RestPrint
00020 ! -- pr Retained Earnings Statement
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnprocess,fnpedat$,fnactpd$
20000   on error goto ERTN
20020 ! ______________________________________________________________________
20040   dim cap$*128,ln1$*8800,ln$*8800,dat$*20
20060 ! ______________________________________________________________________
20080   fntop(program$,cap$="Print Retained Earnings Statement")
20100   fndat(dat$)
20120   tempx=val(fnactpd$) conv L180
20140   if tempx=1 then 
20160     actpd$="one" 
20180   else if tempx=2 then 
20200     actpd$="two" 
20220   else if tempx=3 then 
20240     actpd$="three" 
20260   else if tempx=4 then 
20280     actpd$="four" 
20300   else if tempx=5 then 
20320     actpd$="five"
20340   else if tempx=6 then 
20360     actpd$="six" 
20380   else if tempx=7 then 
20400     actpd$="seven"
20420   else if tempx=8 then 
20440     actpd$="eight" 
20460   else if tempx=9 then 
20480     actpd$="nine"
20500   else if tempx=10 then 
20520     actpd$="ten"
20540   else if tempx=11 then 
20560     actpd$="eleven" 
20580   else if tempx=12 then 
20600     actpd$="twelve" 
20620   else if tempx=13 then 
20640     actpd$="thirteen" 
20660   else if tempx=14 then 
20680     actpd$="fourteen"
20700   end if
20720 L180: open #1: "Name="&env$('Q')&"\GLmstr\acglstmt.h"&env$('cno')&",Shr",display,input ioerr XIT
20740   fnopenprn
20760 READ_ACGLREST: ! 
20780   linput #1: ln$ eof DONE ioerr DONE
20800   for j2=1 to len(rtrm$(ln$))
20820     if ln$(j2:j2)><"@" then goto L320
20840     if ln$(j2+1:j2+1)="1" then 
20860       ln$(j2:j2+1)=fnpedat$&ln$(j2+2:132-len(fnpedat$)) 
20880     else 
20900       goto L280
20920     end if
20940     goto L310
20960 L280: !
20980     if ln$(j2+1:j2+1)="2" then 
21000       ln$(j2:j2+1)=rtrm$(dat$)&ln$(j2+2:132-len(rtrm$(dat$))) 
21020     else 
21040       goto L300
21060     end if
21080     goto L310
21100 L300: !
21120     if ln$(j2+1:j2+1)="3" then 
21140       ln$(j2:j2+1)=rtrm$(actpd$)&ln$(j2+2:132-len(rtrm$(actpd$)))
21160     else 
21180       goto L320
21200     end if
21220 L310: ! lN$=LN1$
21240 L320: !
21260   next j2
21280   pr #255: tab(10);ln$
21300   goto READ_ACGLREST
21320 ! ______________________________________________________________________
21340 DONE: !
21360   close #1: 
21380   fncloseprn
21400   goto XIT
21420 ! ______________________________________________________________________
21440 XIT: fnxit
21460 ! ______________________________________________________________________
21480 ! <Updateable Region: ERTN>
21500 ERTN: fnerror(program$,err,line,act$,"xit")
21520   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
21540   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
21560   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
21580 ERTN_EXEC_ACT: execute act$ : goto ERTN
21600 ! /region
21620 ! ______________________________________________________________________
