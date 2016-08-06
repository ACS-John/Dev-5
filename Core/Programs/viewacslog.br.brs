00010 ! Replace R:\Core\Programs\ViewacsLog
00020 ! shows the acs-log.txt file
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fnerror,fnxit
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   if exists("acs-log.txt")<>0 then execute "sy start acs-log.txt"
00080 XIT: let fnxit
00090 ! ______________________________________________________________________
00100 ! <Updateable Region: ERTN>
00110 ERTN: let fnerror(cap$,err,line,act$,"xit")
00120   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00130   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00140   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00150 ERTN_EXEC_ACT: execute act$ : goto ERTN
00160 ! /region
00170 ! ______________________________________________________________________
