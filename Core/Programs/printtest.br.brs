00010 ! Replace R:\Core\Programs\PrintTest
00020 ! ___________
00030   library 'R:\Core\Library': fntop,fnopenprn,fncloseprn,fntos,fnlbl,fntxt,fnerror,fnacs,fncmdset,fnxit,fnwait
00040   on error goto ERTN
00050 ! __________________
00051   dim prg$*256,cap$*128,ln$*900
00052 ! ___________________
00060   let cancel=5
00070   let fntop(prg$='R:\Core\Programs\PrintTest',cap$='Print Test')
00080 ! ___________________
00090   let fntos(sn$='PrintTest') !:
        let rc=0
00100   let fnlbl(1,1,'Characters Per Line:',40,2)
00110   let fntxt(1,42,3,0,0,'30') !:
        let resp$(rc+=1)='80'
00120   let fnlbl(2,1,'Lines to print:',40,2)
00130   let fntxt(2,42,3,0,0,'30') !:
        let resp$(rc+=1)='54'
00140   let fncmdset(2)
00150   let fnacs(sn$,0,mat resp$,ck)
00160   if ck=cancel then goto XIT
00162 ! _
00165 ! add fnWAIT here when it been rewritten
00170   let fnopenprn
00180   let ln$=rpt$("X",val(resp$(1)))
00190   for j=1 to val(resp$(2)) : print #255: ln$ : next j
00200   let fncloseprn
00210   goto XIT
00220 ! ______________________________________________________________________
00230 XIT: ! 
00240   let fnxit
00250 ! ______________________________________________________________________
00260 ! <Updateable Region: ERTN>
00270 ERTN: let fnerror(cap$,err,line,act$,"xit")
00280   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00290   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00300   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00310 ERTN_EXEC_ACT: execute act$ : goto ERTN
00320 ! /region
00330 ! ______________________________________________________________________
