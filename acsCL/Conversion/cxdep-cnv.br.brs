00010 ! Replace S:\acsCL\Conversion\cxdep-cnv
00020   let cno=3
00030 ! 
00040   def fncd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
00050   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),internal,outin 
00060   open #2: "Name="&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno),internal,outin 
00070   let fln=trmstr
00080 L80: read #fln,using 'Form POS 4,C 2,C 6': ty$,d$ eof END1
00090   if ty$="CA" then goto L140
00100   if ty$="C1" then goto L140
00110   if ty$="C2" then goto L140
00120   if ty$="C3" then goto L140
00130   goto L170
00140 L140: let d1=val(d$) conv L170
00150   let d2=fncd(d1)
00160   rewrite #fln,using 'Form POS 6,N 6': d2
00170 L170: goto L80
00180 END1: if fln=2 then goto L200
00190   let fln=2 : goto L80
00200 L200: close #trmstr: 
00210   close #2: 
00220   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno)&" 1 11 Replace DupKeys"
00230   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno)&" 1/28/4 3/8/8 Replace DupKeys"
00240   execute "Index "&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&' '&env$('Q')&"\CLmstr\TRGLIDX.H"&str$(cno)&" 12 12 Replace DupKeys"
