00010 ! Replace S:\acsCL\Conversion\Bank_Code-Fix
00020   cno=1
00030 ! 
00040   open #trmstr=1: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno),internal,outin 
00050   open #3: "Name="&env$('Q')&"\CLmstr\TRALLOC.h"&str$(cno)&",Shr",internal,outin 
00060 L60: read #trmstr,using 'Form POS 1,N 2,N 1,C 8': bc,tc,ck$ eof END1
00070   ck1=val(ck$) conv L60
00080   if bc><1 then goto L60
00090   if ck1<33642 or ck1>33761 then goto L60
00100   rewrite #trmstr,using 'Form POS 1,N 2': 2
00110   goto L60
00120 END1: ! 
00130 L130: read #3,using 'Form POS 1,N 2,N 1,C 8': bc,tc,ck$ eof END3
00140   ck1=val(ck$) conv L130
00150   if bc><1 then goto L130
00160   if ck1<33642 or ck1>33761 then goto L130
00170   rewrite #3,using 'Form POS 1,N 2': 2
00180   goto L130
00190 END3: ! 
00200   close #trmstr: 
00210   close #3: 
00220   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX1.H"&str$(cno)&" 1 11 REPLACE DupKeys -n"
00230   execute "Index "&env$('Q')&"\CLmstr\TRMSTR.H"&str$(cno)&' '&env$('Q')&"\CLmstr\TRIDX2.H"&str$(cno)&" 28/1 8/11 REPLACE DupKeys -n"
