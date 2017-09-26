00010   open #1: "Name="&env$('Q')&"\UBmstr\ubacctrn.h3610",internal,outin,relative 
00011   let trrec=1
00015   on error goto L60
00020 L20: read #1,using "form pos 1,c 10,pd 4.2,x 2,n 6,2*n 1",rec=trrec: p$,tamt,tdate,transcode,postcode
00030   pause 
00040   close #1: 
00050   end 
00060 L60: pause : let trrec+=1 : goto L20
