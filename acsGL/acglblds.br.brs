00010 ! Replace S:\acsGL\acglBldS
00020 ! this program calls fnacglblds to builds the file    [Q]\GLmstr\ACGLScr.h
00030 !
00040   library 'S:\Core\Library': fnacglblds,fntop,fnerror,fnxit
00050   on error goto Ertn
00060 !
00070   dim flo$(31),fli$(65),scr$(30)*20,otd$(65)*30,d(2)
00080 !
00090   fntop(program$,"Build Screens")
00100   fnacglblds
00110   goto XIT
00120 !
00130 ERTN: ! <Updateable Region: ERTN>
00140   fnerror(program$,err,line,act$,"xit")
00150   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00170   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00180 ERTN_EXEC_ACT: execute act$ : goto ERTN
00190 ! /region
00200 !
00210 XIT: fnxit
00220 !
