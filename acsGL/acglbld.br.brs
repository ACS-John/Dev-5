00010 ! Replace S:\acsGL\acglBld
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnacglbld
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   let fntop(program$,"Build Screens")
00070   let fnacglbld
00080 ! ______________________________________________________________________
00090 XIT: let fnxit
00100 ! ______________________________________________________________________
00110 ! <Updateable Region: ERTN>
00120 ERTN: let fnerror(program$,err,line,act$,"xit")
00130   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 ! ______________________________________________________________________
