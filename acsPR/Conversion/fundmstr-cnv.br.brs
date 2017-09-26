00010 ! Replace S:\acsPR\Conversion\FundMstr-Cnv
00020 ! Convert Fund Master File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim de$*30,fgl$(3)
00080 ! ______________________________________________________________________
00090 L90: print newpage
00100   close #101: ioerr L110
00110 L110: open #101: "SROW=9,SCOL=4,EROW=11,ECOL=65,BORDER=DR,CAPTION=Convert Fund Master File",display,outin 
00120   print fields "10,5,C 60": "COMPANY NUMBER (0 TO STOP):"
00130   input fields "10,51,N 5,UE,N": cno
00140   if cno=0 then goto XIT
00160   open #1: "Name="&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\CLmstr\FundIdx1.h"&str$(cno),internal,outin,keyed ioerr NF1
00170   open #2: "Name=X,RecL=63,Replace",internal,output 
00180 L180: read #1,using L190: dn$,de$,mat fgl$ eof L240
00190 L190: form pos 1,c 3,c 30,3*c 9
00200   write #2,using L210: dn$,de$,fgl$(1),fgl$(2),0,fgl$(3)
00210 L210: form pos 1,c 3,c 30,2*c 9,n 3,c 9
00220   goto L180
00230 ! ______________________________________________________________________
00240 L240: close #1: 
00250 L250: close #2: 
00260   execute "COPY X,"&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&" -n"
00270   execute "Index "&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&","&env$('Q')&"\CLmstr\FundIdx1.h"&str$(cno)&",1,3,Replace,DupKeys -n"
00280   execute "Index "&env$('Q')&"\CLmstr\FUNDMSTR.h"&str$(cno)&","&env$('Q')&"\CLmstr\FundIdx2.h"&str$(cno)&",4,28,Replace,DupKeys -n"
00290   print fields "12,5,C 60": "Completed Converting FundMstr File for Company: "&str$(cno)
00300   print fields "13,5,C 60": "PRESS ANY KEY TO CONTINUE"
00310   input fields "13,40,C 1,IAE,N": pause$
00320   goto L90
00330 ! ______________________________________________________________________
00340 NF1: open #2: "Name=X,RecL=63,Replace",internal,output 
00350   write #2,using L210: "  0","General Fund","","",0,""
00360   goto L250
00370 ! ______________________________________________________________________
00380 ! <Updateable Region: ERTN>
00390 ERTN: let fnerror(program$,err,line,act$,"xit")
00400   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00410   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00420   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00430 ERTN_EXEC_ACT: execute act$ : goto ERTN
00440 ! /region
00450 ! ______________________________________________________________________
00460 XIT: stop 
00470 ! ______________________________________________________________________
