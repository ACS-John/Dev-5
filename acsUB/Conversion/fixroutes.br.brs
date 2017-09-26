00010 ! Replace S:\acsUB\conversion\fixroutes
00020 ! -- Custom written for Monticello to change route and sequence numbers from a text file
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnacs,fnlbl,fntxt,fnwait,fntos,fncno,fnxit,fnerror,fncmdset,fntop,fnopenprn,fncloseprn
00050   on errror goto ERTN
00060 ! ______________________________________________________________________
00070   dim text$*40,cap$*128,ln$*128
00080 ! ______________________________________________________________________
00090   let fncno(cno)
00100 ! 
00110   let fntop("S:\acsUB\TotalBal",cap$="Change Route and Sequence Numbers")
00120   let fnopenprn
00130 ! ______________________________________________________________________
00140   open #1: "Name="&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00150   open #2: "Name=newroute2.txt",display,input 
00160 READ_CUSTOMER: ! 
00170 L170: linput #2: ln$ eof XIT
00180 ! Let Z$=LPAD$(RTRM$(LN$(17:26)),10)
00181   let z$=lpad$(rtrm$(ln$(1:9)),10)
00190 ! Let ROUTE=VAL(LN$(1:7))
00191   let route=val(ln$(81:81))
00200 ! Let SEQUENCE=VAL(LN$(9:15))
00201   let sequence=val(ln$(72:79))
00205   print z$,route,sequence: pause 
00210   read #1,using "Form POS 1,c 10,pos 1741,n 2,pos 1743,n 7",key=z$: oldz$,oldroute,oldsequence nokey L250
00220   rewrite #1,using "Form pos 1741,n 2,pos 1743,n 7": route,sequence
00230   goto READ_CUSTOMER
00240 ! ______________________________________________________________________
00250 L250: print #255,using "form pos 1,c 50": "Account "&z$&" not found"
00260   goto L170
00270 XIT: let fncloseprn
00280   close #1: 
00290   execute "Index "&env$('Q')&"\UBmstr\Customer.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBIndx5.h"&str$(cno)&" 1741/1743 2/7 Replace DupKeys -n"
00300   let fnxit
00310 ! ______________________________________________________________________
00320 ! <Updateable Region: ERTN>
00330 ERTN: let fnerror(program$,err,line,act$,"xit")
00340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00360   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00370 ERTN_EXEC_ACT: execute act$ : goto ERTN
00380 ! /region
