20000 ! Replace S:\acsUB\UBZEROYT
20020 ! ______________________________________________________________________
20040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fntos,fncno,fnxit,fnerror,fncmdset,fntop
20060   on error goto ERTN
20080 ! ______________________________________________________________________
20100   dim z$*10,cap$*128
20120 ! ______________________________________________________________________
20140   fncno(cno)
20160   fntop(program$,cap$="Zero Year to Date Usage")
20180 ! ______________________________________________________________________
20200 SCREEN1: ! 
20220   fntos(sn$:="ubZeroYt")
20240   fnlbl(1,1,'Type "ZERO" to Zero all Year To Date Usages:',48,1)
20260   fntxt(1,50,5)
20280   resp$(1)=""
20300   fncmdset(2)
20320   fnacs(sn$,0,mat resp$,ck)
20340   if ck=5 then goto XIT
20360   if rtrm$(uprc$(resp$(1)))<>"ZERO" then goto SCREEN1
20380   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
20400 READ_CUSTOMER: ! 
20420   read #1,using "Form POS 1,C 10": z$ eof DONE
20440   rewrite #1,using "Form POS 232,PD 5,POS 252,PD 5,POS 272,PD 5": 0,0,0
20460   goto READ_CUSTOMER
20480 DONE: close #1: 
20500   goto XIT
20520 ! <Updateable Region: ERTN>
20540 ERTN: fnerror(program$,err,line,act$,"xit")
20560   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20580   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20600   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20620 ERTN_EXEC_ACT: execute act$ : goto ERTN
20640 ! /region
20660 XIT: fnxit
