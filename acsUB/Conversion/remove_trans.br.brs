00010 ! replace S:\acsUB\conversion\remove_trans
00020 ! programmers tool to remove transactions (not for use on real data)
00030 ! _______________
00040 ! library
00050 ! on error goto ertn
00060 ! ____________
00070   cno=2
00080   open #trans=11: "Name="&env$('Q')&"\UBmstr\ubTransVB.h"&str$(cno),internal,outin 
00090 L90: read #trans,using "Form pos 1,C 10": p$ eof L120
00100   if p$(1:2)=" 3" then delete #trans: : pr "deleted one"
00110   goto L90
00120 L120: pr 'done'
