00010 ! Replace S:\acsCL\Conversion\PayAlloc-Cnv
00020 ! convert PayAlloc file - version 0 to version 0 !:
        ! not sure exactally what it does yet
00030 ! def ___________________________________________________________________
00040   library 'S:\Core\Library': fncno,fnerror,fnxit
00050   dim k$*20,gl(3),ta(2),de$*30
00060   dim vn$*8,nam$*30,ad1$*30,ad2$*30,csz$*30,ss$*11,holdvn$*8,vcode$*8
00070   dim cnam$*40,dat$*20,gl(3),sf1$*28,sn$*30,de$*30
00080   dim rn$*12,de$*30,ta(2),tvn$*8,tr$(5)*35,tr(2),de$*30
00090   io1$(1)="10,51,N 2,U,N"
00100   fncno(cno)
00110   open #1: "Name="&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno),internal,outin,keyed 
00120   open #2: "Name="&env$('Q')&"\CLmstr\X.H"&str$(cno)&",RecL=164,Replace",internal,outin,relative 
00130   open #3: "Name="&env$('Q')&"\CLmstr\PayAlloc.h"&str$(cno)&",SIZE=0,RecL=56,Replace",internal,outin,relative 
00140 L140: read #1,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,N 3,N 6,N 3': vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat gl eof L190
00150   mat ta=(lrec(3)+1)
00160   write #3,using 'Form POS 1,C 8,N 3,N 6,N 3,PD 3.2,C 30,PD 3': vn$,mat gl,100,de$,0
00170   write #2,using 'Form POS 1,C 8,4*C 30,PD 5.2,N 2,C 11,2*PD 3,C 12': vn$,nam$,ad1$,ad2$,csz$,ytdp,typ,ss$,mat ta,ph$
00180   goto L140
00190 L190: close #1: : close #2: : close #3: 
00200   execute "Copy "&env$('Q')&"\CLmstr\X.H"&str$(cno)&' '&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)
00210   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\PayIdx1.h"&str$(cno)&" 1 8 Replace DupKeys"
00220   execute "Index "&env$('Q')&"\CLmstr\PayMstr.h"&str$(cno)&' '&env$('Q')&"\CLmstr\PayIdx2.H"&str$(cno)&" 9 30 Replace DupKeys"
00230   pr "COMPLETED CONVERTING PAYEE FILE FOR COMPANY #: "&str$(cno)
00240 XIT: fnxit
