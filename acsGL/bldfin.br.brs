00010 ! Replace S:\acsGL\BldFin
00020 ! ???
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00050   fntop(program$,"CHANGE_ME")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim d$*50,bc(13),bp(13),bm(13),rf(6),dn$*3,an$*6,sn$*3,glk$*12,fsk$*5
00090   dim gln(3,3),fin(3),ta(2),ac(18),te$*1
00100 ! ______________________________________________________________________
00110   fncno(cno)
00120 ! 
00130   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&str$(cno)&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
00140   open #2: "Name="&env$('Q')&"\GLmstr\ACGLFNSI.h"&str$(cno),internal,output 
00150 READ_GLMSTR: ! 
00160   read #1,using L180: dno,ano,sno,d$,mat rf eof END1
00170   if ano<1000 then goto READ_GLMSTR
00180 L180: form pos 1,n 3,n 6,n 3,c 50,6*pd 3,42*pd 6.2,2*pd 3
00190   let rno=rno+10
00200   ac(1)=3
00210   if ano<2000 then ac(5)=1 else ac(5)=0
00220   write #2,using L230: rno,d$,"D",mat ac
00230 L230: form pos 1,n 5,c 50,c 1,2*n 2,15*n 1,n 3
00240   let rf(2)=rno
00250   rewrite #1,using L180: dno,ano,sno,d$,mat rf
00260   goto READ_GLMSTR
00270 END1: ! 
00280   close #1: 
00290   close #2: 
00300 XIT: stop 
00310 ! ______________________________________________________________________
00320 ! <Updateable Region: ERTN>
00330 ERTN: let fnerror(program$,err,line,act$,"xit")
00340   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00350   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00360   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00370 ERTN_EXEC_ACT: execute act$ : goto ERTN
00380 ! /region
00390 ! ______________________________________________________________________
