00010 ! Replace R:\Core\Programs\csBackup
00020 ! Backup for Current System
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnbackup,fnxit,fncursys$,fnerror,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   let fntop("R:\Core\Programs\csBackup",uprc$(fncursys$)&" Backup")
00080   let backup=1 !:
        let restore=2
00090   let fnbackup(backup)
00100 XIT: let fnxit
00110 ! ______________________________________________________________________
00120 ! <Updateable Region: ERTN>
00130 ERTN: let fnerror(cap$,err,line,act$,"xit")
00140   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00150   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00160   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00170 ERTN_EXEC_ACT: execute act$ : goto ERTN
00180 ! /region
00190 ! ______________________________________________________________________
