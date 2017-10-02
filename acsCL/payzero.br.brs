00020 ! ???
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,holdvn$*8,vcode$*8
00080   dim gl(3),sf1$*28,sn$*30,de$*30,rn$*12,de$*30,ta(2),tvn$*8
00090   dim flit$(4)*16,scrt$(4)*20,scid$*79,desc$(6)*14
00100 ! ______________________________________________________________________
00110   let fncno(cno)
00120 ! 
00130   open #1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00140   restore #1,key>="        ": eof XIT
00150 L150: read #1,using 'Form POS 129,PD 5.2': ytdp eof XIT
00160   rewrite #1,using 'Form POS 129,PD 5.2': 0
00170   goto L150
00180 XIT: stop 
00190 ! ______________________________________________________________________
00200 ! <Updateable Region: ERTN>
00210 ERTN: let fnerror(program$,err,line,act$,"xit")
00220   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00230   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00240   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00250 ERTN_EXEC_ACT: execute act$ : goto ERTN
00260 ! /region
