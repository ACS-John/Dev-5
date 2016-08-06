00010 ! Replace R:\acsGL\CoverLetterEdit
00020 ! -- Edit Cover Letter
00030 ! ______________________________________________________________________
00040   library 'R:\Core\Library': fntop,fnxit,fnerror,fncno,fnconsole,fnget_wordprocessor_exe
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,atlantis$*80
00080 ! ______________________________________________________________________
00090   let fntop("R:\acsGL\CoverLetterEdit",cap$="Edit Cover Leter")
00100   let fnconsole(off=0)
00110   let fncno(cno)
00115   let fnget_wordprocessor_exe(atlantis$)
00120 ! Execute "SY -m NotePad Q:\GLmstr\ACGLCovF.h"&STR$(CNO)
00121   execute 'SY -w '&atlantis$&' "'&os_filename$("Q:\GLmstr\ACGLCovF.h"&str$(cno))&'" -n'
00130   goto XIT
00140 ! ______________________________________________________________________
00150 XIT: let fnxit
00160 ! ______________________________________________________________________
00170 ! <Updateable Region: ERTN>
00180 ERTN: let fnerror(cap$,err,line,act$,"xit")
00190   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00200   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00210   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00220 ERTN_EXEC_ACT: execute act$ : goto ERTN
00230 ! /region
00240 ! ______________________________________________________________________
