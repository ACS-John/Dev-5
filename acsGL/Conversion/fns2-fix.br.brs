00010 ! REPLACE FNS2.FIX/acsGL
00020   library 'Core\Library': fntop
00030   dim r$*5,d$*50,te$*1,ac(9),fil$(3),idx$(3)
00040 L40: pr newpage
00050   fntop(program$,"FNS2.FIX/acsGL")
00060   pr f "10,15,C 40": "ENTER COMPANY NUMBER TO FIX:"
00070 L70: input fields "10,45,N 2,UE,N": cno conv L70
00080   if cno=0 then stop 
00090 !:
        ! 00100  data ACGLFNSB,ACGLFNSI,ACGLFNSJ
00110   read mat fil$
00120   data FNSBINDX,FNSIINDX,FNSJINDX
00130   read mat idx$
00140   for j=1 to 3
00150     open #1: "Name="&env$('Q')&"\GLmstr\"&fil$(j)&".h"&env$('cno'),internal,input,relative 
00160     open #2: "Name="&env$('Temp')&"\WORK."&wsid$&",RecL=273,REPLACE",internal,output 
00170     for j1=1 to lrec(1)
00180       read #1,using L190,rec=j1: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L230,conv NJ1,norec NJ1
00190 L190: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
00200       if ltrm$(r$)="" or ltrm$(r$)="0" then goto NJ1
00210       write #2,using L190: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc
00220 NJ1: next j1
00230 L230: close #1: 
00240     close #2: 
00250     execute "COPY "&env$('Temp')&"\WORK."&wsid$&' '&env$('Q')&"\GLmstr\"&fil$(j)&".h"&env$('cno')
00260     execute "Index "&env$('Q')&"\GLmstr\"&fil$(j)&".h"&env$('cno')&' '&env$('Q')&"\GLmstr\"&idx$(j)&".h"&env$('cno')&" 1 5 REPLACE DupKeys"
00270   next j
00280   goto L40
