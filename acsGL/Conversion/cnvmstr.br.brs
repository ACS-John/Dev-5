00010 ! Replace S:\acsGL\Conversion\CnvMSTR
00020 ! some sort of conversion program for "&env$('Q')&"\GLmstr
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   fntop(program$,"CHANGE_ME")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim k$*12,d$*50,rf(6),bc(12),bm(12),bp(12),ta(2)
00090 ! ______________________________________________________________________
00100   pr newpage
00110   pr f "10,5,C 60": "COMPANY NUMBER TO CONVERT:"
00120 L120: input fields "10,55,N 2,UE,N": cno conv L120
00130   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno'),internal,outIn,keyed ioerr L120
00140   open #2: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno'),internal,output ioerr L160
00150   close #2,free: 
00160 L160: open #2: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",SIZE=0,RecL=416",internal,output 
00170 L170: read #1,using L180: k$,d$,rf(1),rf(3),rf(5),bb,cb,mat bc,mat bp,mat bm,pbp,fr(2),rf(4),rf(6) eof END1
00180 L180: form pos 1,c 12,c 50,3*pd 3,39*pd 6.2,3*pd 3
00190   write #2,using L200: k$,d$,mat rf,bb,cb,mat bc,0,mat bp,0,mat bm,0,pbp,mat ta
00200 L200: form pos 1,c 12,c 50,6*pd 3,42*pd 6.2,2*pd 3
00210   goto L170
00220 ! ______________________________________________________________________
00230 END1: close #1: 
00240   close #2: 
00250   execute "Index "&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&' '&env$('Q')&"\GLmstr\GLIndex.h"&env$('cno')&" 1 12 Replace DupKeys -n"
00260   goto XIT
00270 ! ______________________________________________________________________
00280 ! <Updateable Region: ERTN>
00290 ERTN: fnerror(program$,err,line,act$,"xit")
00300   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00310   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00320   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00330 ERTN_EXEC_ACT: execute act$ : goto ERTN
00340 ! /region
00350 ! ______________________________________________________________________
00360 XIT: fnxit
