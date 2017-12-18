00010 ! Replace S:\acsPR\Conversion\PRmstr-Fix
00020 ! not sure
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$(3)*30,ss$*11,rs(2),em(16),ta(2),tr$*467
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 ! 
00110   open #10: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno'),internal,input,relative 
00120   open #11: "Name=RPMSTR.X,RecL=196,Replace",internal,output 
00130   open #12: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno'),internal,input,relative 
00140   open #13: "Name=RPTRAIL.X,SIZE=0,RecL=474,Replace",internal,outin,relative 
00150   ot13=1
00160   write #13,using L170,rec=1: mat tdz,ot13,0
00170 L170: form pos 1,n 8,n 3,n 3,n 6,n 3,4*n 6,3*n 2,24*pd 4.2,6*pd 3.2,60*pd 5.2,pd 3,pd 4.2
00180   form pos 1,n 8,n 3,n 3,n 6,n 3,4*n 6,3*n 2,24*pd 4.2,5*pd 3.2,pos 471,pd 4.2,pos 165,pd 3.2,60*pd 5.2,pd 3
00190 L190: r10=r10+1
00200   if r10>lrec(10) then goto END1
00210   read #10,using L220,rec=r10: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd norec L190,conv L190,eof END1
00220 L220: form pos 1,n 8,3*c 30,c 11,2*n 1,7*n 2,2*pd 3.3,6*pd 4.2,2*n 6,pd 5.2,2*pd 3,c 12,n 6
00230   adr=ta(1)
00240   mat ta=(0)
00250 L250: if adr=0 then goto L370
00260   read #12,using L270,rec=adr: tr$,adr,wkm
00270 L270: form pos 1,c 467,pd 3,pd 4.2
00280   ot13=lrec(13)+1
00290   if adr=0 then ota=0 else ota=ot13+1
00300   write #13,using L270,rec=ot13: tr$,ota,wkm
00310   if ta(1)=0 then ta(1)=ot13
00320   ta(2)=ot13
00330   rewrite #13,using L340,rec=1: ot13
00340 L340: form pos 468,pd 3
00350   goto L250
00360 ! ______________________________________________________________________
00370 L370: write #11,using L220: eno,mat em$,ss$,mat rs,mat em,lpd,tgp,mat ta,ph$,bd
00380   goto L190
00390 ! ______________________________________________________________________
00400 END1: close #10,free: 
00410   close #11: 
00420   close #12,free: 
00430   close #13: 
00440   execute "RENAME RPMSTR.X "&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&" -n"
00450   execute "RENAME RPTRAIL.X "&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno')&" -n"
00460   execute "Index "&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&' '&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&" 1 8 Replace DupKeys -n"
00470   execute "Index "&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&' '&env$('Q')&"\PRmstr\RPINDX2.h"&env$('cno')&" 9 30 Replace DupKeys -n"
00480 XIT: stop 
00490 ! ______________________________________________________________________
00500 ! <updateable region: ertn>
00510 ERTN: fnerror(program$,err,line,act$,"xit")
00520   if uprc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00530   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00540   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00550 ERTN_EXEC_ACT: execute act$ : goto ERTN
00560 ! /region
00570 ! ______________________________________________________________________
