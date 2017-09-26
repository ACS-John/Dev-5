00012 ! ______________________________________________________________________
00020   library 'S:\Core\Library': fnmsgbox,fntop,fnerror
00025   on error goto ERTN
00029 ! ______________________________________________________________________
00030   dim message$(4)*400,cap$*128
00031 ! note message$(4) is not dimmed long enough
00035 ! ______________________________________________________________________
00040   let fntop(prg$="Test\msgbox",cap$="Test fnMsgBox")
00042   for mt=0 to 5
00050     let message$(1)="This is my message" !:
          let message$(2)="It can be many lines long" !:
          let message$(3)="This is " !:
          let message$(4)="This is j  - this is a very long line - much longer than previous lines it's a real whopper - i mean it man - big"
00060 ! let mt=0+256 !:
          let fnmsgbox(mat message$, response$, cap$, mt)
00070     print "The answer is "&response$
00072   next mt
00080 XIT: stop 
00090 ! ______________________________________________________________________
00100 ! <Updateable Region: ERTN>
00101 ERTN: let fnerror(program$,err,line,act$,"xit")
00102   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00103   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00104   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00105 ERTN_EXEC_ACT: execute act$ : goto ERTN
00106 ! /region
00110 ! ______________________________________________________________________
