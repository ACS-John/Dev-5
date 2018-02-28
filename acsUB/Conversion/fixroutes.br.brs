00010 ! Replace S:\acsUB\conversion\fixroutes
00020 ! -- Custom written for Monticello to change route and sequence numbers from a text file
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnAcs,fnLbl,fnTxt,fnwait,fnTos,fncno,fnxit,fnerror,fnCmdSet,fntop,fnopenprn,fncloseprn
00050   on errror goto ERTN
00060 ! ______________________________________________________________________
00070   dim text$*40,cap$*128,ln$*128
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 ! 
00110   fntop("S:\acsUB\TotalBal",cap$="Change Route and Sequence Numbers")
00120   fnopenprn
00130 ! ______________________________________________________________________
00140   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
00150   open #2: "Name=newroute2.txt",display,input 
00160 READ_CUSTOMER: ! 
00170 L170: linput #2: ln$ eof XIT
00180 ! z$=LPAD$(RTRM$(LN$(17:26)),10)
00181   z$=lpad$(rtrm$(ln$(1:9)),10)
00190 ! rOUTE=VAL(LN$(1:7))
00191   route=val(ln$(81:81))
00200 ! sEQUENCE=VAL(LN$(9:15))
00201   sequence=val(ln$(72:79))
00205   pr z$,route,sequence: pause 
00210   read #1,using "Form POS 1,c 10,pos 1741,n 2,pos 1743,n 7",key=z$: oldz$,oldroute,oldsequence nokey L250
00220   rewrite #1,using "Form pos 1741,n 2,pos 1743,n 7": route,sequence
00230   goto READ_CUSTOMER
00240 ! ______________________________________________________________________
00250 L250: pr #255,using "form pos 1,c 50": "Account "&z$&" not found"
00260   goto L170
00270 XIT: fncloseprn
00280   close #1: 
00290   execute "Index [Q]\UBmstr\Customer.h[cno]"&' '&"[Q]\UBmstr\UBIndx5.h[cno] 1741/1743 2/7 Replace DupKeys -n"
00300   fnxit
00310 ! ______________________________________________________________________
00320 ! <Updateable Region: ERTN>
00330 ERTN: fnerror(program$,err,line,act$,"xit")
00340   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00370 ERTN_EXEC_ACT: execute act$ : goto ERTN
00380 ! /region
