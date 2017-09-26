00010 ! Replace S:\acsGL\Conversion\add-h.wb
00020 ! this program renames "&env$('Q')&"\GLmstr\*.[cno] to "&env$('Q')&"\GLmstr\*.H[cno]
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror
00050   let fntop(program$,"CHANGE_ME")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   let fncno(cno)
00090   print newpage
00100 MENU1: ! 
00110   close #101: ioerr L120
00120 L120: open #101: "SROW=9,SCOL=23,EROW=11,ECOL=60,BORDER=DR,CAPTION="&cap$,display,outin: print #101: newpage
00130   print fields "10,24,C 32": "Company Number:"
00140   print fields "12,32,C 16,B,5": "Cancel (F5)"
00150 L150: rinput fields "10,57,N 5,UT,N": cno conv L150
00160   if cmdkey=5 then goto XIT
00170   goto START
00180 ! ______________________________________________________________________
00190 XIT: stop 
00200 ! ______________________________________________________________________
00210 START: ! 
00220   execute "Rename "&env$('Q')&"\GLmstr\*."&str$(cno)&' '&env$('Q')&"\GLmstr\*.h"&str$(cno)&" -n" ioerr RER
00230   print 'company '&str$(cno)&' completed.'
00240   goto MENU1
00250 ! ______________________________________________________________________
00260 RER: ! 
00270   print 'company '&str$(cno)&' had problems.'
00280   goto MENU1
00290 ! ______________________________________________________________________
00300 ! <Updateable Region: ERTN>
00310 ERTN: let fnerror(program$,err,line,act$,"xit")
00320   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00330   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00340   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00350 ERTN_EXEC_ACT: execute act$ : goto ERTN
00360 ! /region
