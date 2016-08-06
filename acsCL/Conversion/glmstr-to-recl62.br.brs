00010 ! Replace R:\acsCL\Conversion\Q:\GLmstr-to-recL62
00020 ! convert CL Q:\GLmstr from any record length to 62 !:
        ! for use from version 0 to version 0 !:
        ! okay to use on RecL of 72 or 80
00030   def library fnglmstrtorecl62
00040     library 'R:\Core\Library': fnerror,fncno
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070 ! dim
00080 ! ______________________________________________________________________
00090 ! fntop
00100     let fncno(cno)
00110 ! 
00120     execute "COPY Q:\CLmstr\GLmstr.H"&str$(cno)&" X."&session$&" -62"
00130     execute "COPY X."&session$&" Q:\CLmstr\GLmstr.H"&str$(cno)&" -D"
00140     execute "Free X."&session$
00150     execute "Index Q:\CLmstr\GLmstr.H"&str$(cno)&",Q:\CLmstr\GLIndex.h"&str$(cno)&",1,12,Replace,DupKeys"
00160     goto XIT
00170 ! ______________________________________________________________________
00180 ! <Updateable Region: ERTN>
00190 ERTN: let fnerror(cap$,err,line,act$,"xit")
00200     if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00210     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 ! ______________________________________________________________________
00260 XIT: fnend 
00270 ! ______________________________________________________________________
