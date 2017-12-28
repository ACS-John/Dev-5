00010 ! Replace S:\Core\Programs\PrintTest
00020 ! ___________
00030   library 'S:\Core\Library': fntop,fnopenprn,fncloseprn,fnTos,fnLbl,fnTxt,fnerror,fnAcs,fnCmdSet,fnxit,fnwait
00040   on error goto ERTN
00050 ! __________________
00051   dim prg$*256,cap$*128,ln$*900
00052 ! ___________________
00060   cancel=5
00070   fntop(prg$='S:\Core\Programs\PrintTest',cap$='Print Test')
00080 ! ___________________
00090   fnTos(sn$='PrintTest') !:
        rc=0
00100   fnLbl(1,1,'Characters Per Line:',40,2)
00110   fnTxt(1,42,3,0,0,'30') !:
        resp$(rc+=1)='80'
00120   fnLbl(2,1,'Lines to print:',40,2)
00130   fnTxt(2,42,3,0,0,'30') !:
        resp$(rc+=1)='54'
00140   fnCmdSet(2)
00150   fnAcs(sn$,0,mat resp$,ck)
00160   if ck=cancel then goto XIT
00162 ! _
00165 ! add fnWAIT here when it been rewritten
00170   fnopenprn
00180   ln$=rpt$("X",val(resp$(1)))
00190   for j=1 to val(resp$(2)) : pr #255: ln$ : next j
00200   fncloseprn
00210   goto XIT
00220 ! ______________________________________________________________________
00230 XIT: ! 
00240   fnxit
00250 ! ______________________________________________________________________
00260 ! <Updateable Region: ERTN>
00270 ERTN: fnerror(program$,err,line,act$,"xit")
00280   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00290   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00310 ERTN_EXEC_ACT: execute act$ : goto ERTN
00320 ! /region
00330 ! ______________________________________________________________________
