00010   library 'S:\Core\Library': fnxit,fntop
00020 ! set all base reference # to 0 in income statement file
00030   dim r$*5,d$*50,te$*1,ac(9),fil$(3),idx$(3)
00040 L40: pr newpage
00050   fntop(program$,"CHANGE_ME")
00060   pr f "10,15,C 40": "ENTER COMPANY NUMBER TO FIX:"
00070 L70: input fields "10,45,N 2,UE,N": cno conv L70
00080   if cno=0 then stop 
00090 !:
        ! 00100  data ACGLFNSB,ACGLFNSI,ACGLFNSJ
00110   read mat fil$
00120   data FNSBINDX,FNSIINDX,FNSJINDX
00130   read mat idx$
00140   for j=2 to 2
00150     open #1: "Name="&env$('Q')&"\GLmstr\"&fil$(j)&".h"&env$('cno'),internal,outIn,relative 
00160     for j1=1 to lrec(1)
00170       read #1,using L180,rec=j1: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,gp eof L210,conv NJ1,noRec NJ1
00180 L180: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
00190       rewrite #1,using L180,rec=j1: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,0
00200 NJ1: next j1
00210 L210: close #1: 
00220   next j
00230   goto L40
