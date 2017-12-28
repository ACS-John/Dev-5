00010 ! Replace S:\acsGL\Conversion\FNS-Fix
00020 ! financial statement file
00030   library 'S:\Core\Library': fntop,fnerror
00040   pr newpage
00050   fntop(program$,"CHANGE_ME")
00060   pr f "10,10,C 60": "ENTER THE COMPANY NUMBER OR 0 TO STOP:"
00070 L70: input fields "10,50,N 2,UE,N": cno conv L70
00080   if cno=0 or cmdkey=5 or cmdkey=99 then goto XIT
00090   dim rno$*5,recno$*5,d$*50,te$*1,fil$(6)*30,idx$(6)*30,ac(9)
00100   fil$(1)=env$('Q')&"\GLmstr\ACGLFNSB.h"&env$('cno'): idx$(1)=env$('Q')&"\GLmstr\FnSBIndx.h"&env$('cno')
00110   fil$(2)=env$('Q')&"\GLmstr\ACGLFNSI.h"&env$('cno'): idx$(2)=env$('Q')&"\GLmstr\FNSIINDX.h"&env$('cno')
00120   fil$(3)=env$('Q')&"\GLmstr\ACGLFNSF.h"&env$('cno'): idx$(3)=env$('Q')&"\GLmstr\FNSFIndx.h"&env$('cno')
00130   fil$(4)=env$('Q')&"\GLmstr\AcGLFnSc.h"&env$('cno'): idx$(4)=env$('Q')&"\GLmstr\FnScIndx.h"&env$('cno')
00140   fil$(5)=env$('Q')&"\GLmstr\ACGLFNSJ.h"&env$('cno'): idx$(5)=env$('Q')&"\GLmstr\FNSJINDX.h"&env$('cno')
00150   fil$(6)=env$('Q')&"\GLmstr\ACGLFNSG.h"&env$('cno'): idx$(6)=env$('Q')&"\GLmstr\FNSGIndx.h"&env$('cno')
00160   open #3: "Name=PROC."&wsid$,display,output ioerr L180
00170   close #3,free: 
00180 L180: open #3: "Name=PROC."&wsid$&",SIZE=0",display,output 
00190   for f1=1 to 6
00200     open #1: "Name="&fil$(f1),internal,input,relative ioerr L330
00210     open #2: "Name=X",internal,output ioerr L230
00220     close #2,free: 
00230 L230: open #2: "Name=X,SIZE=0,RecL=83",internal,output ioerr L230
00240     for j=1 to lrec(1)
00250       read #1,using L260,rec=j: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp noRec L280,conv L280
00260 L260: form pos 1,c 5,c 50,c 1,2*n 2,15*n 1,n 3,n 5
00270       write #2,using L260: rno$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp
00280 L280: next j
00290     close #1: 
00300     close #2: 
00310     execute "COPY X,"&fil$(f1)
00320     pr #3: "INDEX "&fil$(f1)&" "&idx$(f1)&" 1 5 REPLACE DupKeys"
00330 L330: next f1
00340   close #3: 
00350   chain "PROC=PROC."&wsid$
00360 XIT: stop 
