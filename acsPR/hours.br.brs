00010 ! Replace S:\acsPR\hours.br
00020 ! access breakdown of hours by diffenent classifications
00030 !
00040   library 'S:\Core\Library': fnhours,fnxit,fnerror,fntop
00050   on error goto Ertn
00060 !
00070   fntop("S:\acsPR\hours","Breakdown of Hours")
00080   fnhours(eno)
00090   goto XIT
00100 !
00110 ! <Updateable Region: ERTN>
00120 ERTN: fnerror(program$,err,line,act$,"NO")
00130   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 !
00190 XIT: fnxit
00200 !
