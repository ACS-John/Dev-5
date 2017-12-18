00010   cno=1
00020   open #trmstr1=1: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00030 L30: read #1,using 'form pos 12,n 6': d1 eof L60
00032   pr d1 : pause 
00040   rewrite #1,using "form pos 12,n 6": d1
00050   goto L30
00060 L60: stop 
