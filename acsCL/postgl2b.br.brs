00010 ! Replace S:\acsCL\PostGL2b
00020 ! pr GL Distribution Listing
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnpostgl2,fnxit,fnerror,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   fntop(program$,"GL Distribution Report")
00080   fnpostgl2(2)
00090   goto XIT
00100 ! ______________________________________________________________________
00110 ! <Updateable Region: ERTN>
00120 ERTN: let fnerror(program$,err,line,act$,"NO")
00130   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 ! ______________________________________________________________________
00190 XIT: let fnxit
