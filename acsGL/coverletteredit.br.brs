00010 ! Replace S:\acsGL\CoverLetterEdit
00020 ! -- Edit Cover Letter
00040   library 'S:\Core\Library': fntop,fnxit,fnerror,fnget_atlantis
00050   on error goto ERTN
00070   dim cap$*128,atlantis$*256
00090   let fntop(program$,cap$="Edit Cover Leter")
00115   let fnget_atlantis(atlantis$)
00121   execute 'SY -w '&atlantis$&' "'&os_filename$(env$('Q')&"\GLmstr\ACGLCovF.h"&env$('cno'))&'" -n'
00130   goto XIT
00140 ! ______________________________________________________________________
00150 XIT: let fnxit
00170 ! <Updateable Region: ERTN>
00180 ERTN: let fnerror(program$,err,line,act$,"xit")
00190   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00200   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00210   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00220 ERTN_EXEC_ACT: execute act$ : goto ERTN
00230 ! /region
