00010 ! Replace Test\Error.br
00020 ! test the error routine
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnerror ! fnmsgbox,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! dim message$(4)*40,cap$*128
00080 ! note message$(4) is not dimmed long enough
00090 ! ______________________________________________________________________
00100   pr #0,using 'form pos 1,7*N 3,C 2,C 2': 1,2,3,4,5,6,7,"s",5,5
00110 ! let fntop(prg$="Test\Error",cap$="Test Error")
00120 ! let message$(1)="This is my message"
00130 ! let message$(2)="It can be asdfjfdskljfsdalkjfdsalkjsfdalkjfsdmany lines long"
00140 ! let message$(3)="This is "
00150 ! let message$(4)="This is j"
00160 ! let mt=4
00170 ! let fnmsgbox(mat message$, response$, cap$, mt)
00180 ! pr "The answer is "&response$
00190 XIT: ! 
00200   pr "Exit Successful"
00210   stop 
00220 ! ______________________________________________________________________
00230 ! <Updateable Region: ERTN>
00240 ERTN: ! 
00270   let fnerror(program$,err,line,act$,"QUIT_WO_CALC")
00280   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00290   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00310 ERTN_EXEC_ACT: execute act$ : goto ERTN
00320 ! /region
00330 ! ______________________________________________________________________
