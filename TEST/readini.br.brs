00010 ! Replace test\ReadINI
00020 ! test read ini sets it's program to ubfm...   and uses that ini file
00030 ! -------------------------------------------------------------------
00040   library 'S:\Core\Library': fntop,fnread_program_print_property,fnerror
00050 ! -------------------------------------------------------------------
00060   fntop(prg$='S:\Utility Billing\Customer',cap$='Test Read INI')
00070   fnread_program_print_property('SaveToAsStart',tf1$)
00080   pr "The value read was:"&tf1$
00081   pr "The length of the value is:"&str$(len(tf1$))
00090 XIT: stop 
00100 ! -------------------------------------------------------------------
00110 ! <Updateable Region: ERTN>
00120 ERTN: let fnerror(program$,err,line,act$,"xit")
00130   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00160 ERTN_EXEC_ACT: execute act$ : goto ERTN
00170 ! /region
00180 ! -------------------------------------------------------------------
