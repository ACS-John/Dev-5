00010 ! Replace S:\Core\Programs\Web
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnmsgbox,fnerror,fnxit
00040 ! ______________________________________________________________________
00050   dim msgline$(3)*60,response$(5)*1,cap$*128
00060 ! ______________________________________________________________________
00070   cap$="ACS User's Website"
00080   let msgline$(1)="Do you wish open the web site:"
00090   let msgline$(2)="http://planetacs.net/user"
00100   let msgline$(3)="in your default browser?"
00110   let fnmsgbox(mat msgline$,resp$,cap$,3)
00120   if resp$="No" or resp$="Cancel" then goto XIT
00130   execute "sy start http://planetacs.net/user/index.htm"
00140 XIT: let fnxit("")
00150 ! ______________________________________________________________________
00160 ! <Updateable Region: ERTN>
00170 ERTN: let fnerror(program$,err,line,act$,"xit")
00180   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00190   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00200   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00210 ERTN_EXEC_ACT: execute act$ : goto ERTN
00220 ! /region
