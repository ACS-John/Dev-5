00010   let cno=1
00020   open #trmstr1=1: "Name=Q:\CLmstr\TrMstr.h"&str$(cno)&",KFName=Q:\CLmstr\TrIdx1.h"&str$(cno)&",Shr",internal,outin,keyed 
00030 L30: read #1,using 'form pos 12,n 6': d1 eof L60
00032   print d1 : pause 
00040   rewrite #1,using "form pos 12,n 6": d1
00050   goto L30
00060 L60: stop 
