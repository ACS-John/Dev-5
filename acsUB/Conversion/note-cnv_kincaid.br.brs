00010 ! Replace S:\acsUB\conversion\Note-cnv
00020 ! this program converts a field from ALL CAPITAL LETTERS !:
        ! to Book Title Capitalization
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnxit,fnerror,fncno
00050 ! On Error Goto ERTN
00060 ! ______________________________________________________________________
00070   dim nam$*30,rm$*60,ra(2),z$*10
00080 ! ______________________________________________________________________
00090   fncno(cno)
00100 ! 
00110   pr newpage
00120   goto L190
00130   pr fields "8,20,Cc 30,R,N": "Convert Notes "
00140   pr fields "10,1,Cr 38": "Company Number to Convert (0 to Stop):"
00150 ! 
00160   let io1$(1)="10,40,N 2,UT,N"
00170 L170: rinput fields mat io1$: cno conv L170
00180   if cno=0 or cmdkey=5 or cmdkey=99 then goto XIT
00190 L190: execute "Index "&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&' '&env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno)&" 1 10 Replace DupKeys -n"
00200   open #31: "Name="&env$('Q')&"\UBmstr\Note1.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\NoteIdx1.h"&str$(cno)&",Shr",internal,input,keyed 
00210   open #32: "Name="&env$('Q')&"\UBmstr\Note2.h"&str$(cno)&",Shr,Use,RecL=73",internal,outin,relative 
00220 L220: read #31,using L230: z$,mat ra eof L320
00230 L230: form pos 1,c 10,2*pd 3
00240   close #33: ioerr L245
00245 L245: if exists(env$('Q')&"\UBmstr\notes.h"&str$(cno)) = 0 then !:
          execute "mkdir "&env$('Q')&"\UBmstr\notes.h"&str$(cno)
00250   open #33: "Name="&env$('Q')&"\UBmstr\notes.h"&str$(cno)&"\"&trim$(z$)&".txt,RecL=128,use",display,output 
00260   adr=ra(1)
00270 L270: if adr=0 then goto L220
00280   read #32,using L300,rec=adr: k32$,rm$,adr
00290   pr #33: rm$
00300 L300: form pos 1,c 10,c 60,pd 3
00310   goto L270
00320 L320: goto DONE
00330 ! ______________________________________________________________________
00340 DONE: ! 
00350   pr "company number "&str$(cno)&" completed successfully"
00360 ! Goto 110
00370 XIT: chain "S:\acsUB\conversion\ubadrbil-cnv"
00380 ! ______________________________________________________________________
00390 ! <Updateable Region: ERTN>
00400 ERTN: let fnerror(program$,err,line,act$,"xit")
00410   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00420   goto L270
00430   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00440   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00450 ERTN_EXEC_ACT: execute act$ : goto ERTN
00460 ! /region
00470 ! ______________________________________________________________________
