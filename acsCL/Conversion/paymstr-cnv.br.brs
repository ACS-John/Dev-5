00010 ! Replace S:\acsCL\conversion\paymstr-cnv
00020 ! it's old
00030 ! ________
00040   library 'S:\Core\Library': fncno,fnerror,fnxit
00050   on error goto ERTN
00060 ! _____________________________________
00070 ! fntop
00080   fncno(cno)
00090   pr newpage
00100   close #101: ioerr L110
00110 L110: open #101: "SROW=11,SCOL=20,EROW=13,ECOL=63,BORDER=DR,CAPTION=CHANGE PAYEE NUMBERS",display,outin 
00120   pr f "12,22,C 40": "ENTER COMPANY NUMBER TO BE CHANGE:"
00130   pr f "14,32,C 16,B,5": "PRESS F5 TO STOP"
00140 L140: rinput fields "12,57,N 2,UE,N": cno conv L140
00150   if cmdkey=5 then goto XIT
00160 ! 
00170   open #1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",Version=1,KFName="&env$('Q')&"\CLmstr\PayIdx2.h"&str$(cno)&",Shr",internal,outin,keyed 
00180   open #2: "Name="&env$('Q')&"\CLmstr\PayAlloc.h"&str$(cno),internal,outin,relative 
00190   open #3: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno),internal,outin,keyed 
00200 ! OPEN #4: "Name="&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno),INTERNAL,OUTIN,RELATIVE
00210   open #6: "Name="&env$('Q')&"\CLmstr\IvPaid.H"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\IVINDEX.H"&str$(cno)&"",internal,outin,keyed 
00220   pr f "14,32,C 16,RB,N": "  IN PROCESS"
00230   let nk=10
00240 L240: read #1,using L250: k1$,ad1 eof END1
00250 L250: form pos 1,c 8,pos 147,pd 3
00260   let k2$=lpad$(str$(nk),8)
00270   rewrite #1,using L250: k2$
00280   let nk=nk+5
00290   ad2=ad1
00300 L300: if ad2=0 then goto END2
00310   read #2,using L320,rec=ad2: p$,nta
00320 L320: form pos 1,c 8,pos 54,pd 3
00330   rewrite #2,using L320,rec=ad2: k2$
00340   ad2=nta : goto L300
00350 END2: restore #3,search>=k1$: nokey END3
00360 L360: read #3,using L370: p$ eof END3
00370 L370: form pos 28,c 8
00380   if k1$><p$ then goto END3
00390   rewrite #3,using L370: k2$
00400   goto L360
00410 END3: restore #6,search>=k1$: nokey END6
00420 L420: read #6,using L430: p$ eof END6
00430 L430: form pos 1,c 8
00440   if p$><k1$ then goto END6
00450   rewrite #6,using L430: k2$
00460   goto L420
00470 END6: goto L240
00480 END1: ! 
00490   close #1: 
00500   close #2: 
00510   close #3: 
00520   close #6: 
00530   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&" 1 8 Replace DupKeys"
00540   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\PayIdx2.H"&str$(cno)&" 9 30 Replace DupKeys"
00550   execute "Index "&env$('Q')&"\CLmstr\IvPaid.H"&str$(cno)&","&env$('Q')&"\CLmstr\IVINDEX.H"&str$(cno)&",1,20,Replace,DupKeys"
00560   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno)&" 1 11 Replace DupKeys"
00570   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno)&" 28/1 8/11 Replace DupKeys"
00580 XIT: let fnxit
00590 ! <Updateable Region: ERTN>
00600 ERTN: let fnerror(program$,err,line,act$,"xit")
00610   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00620   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00630   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00640 ERTN_EXEC_ACT: execute act$ : goto ERTN
00650 ! /region
