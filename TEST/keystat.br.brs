00010 ! Replace S:\Core\KeyStat
00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fnerror
00040   on error goto ERTN
00050 ! ______________________________________________________________________
00060   close #101: ioerr L70
00070 L70: open #101: "SRow=2,SCol=5,ERow=22,ECol=37,Border=ss,Caption=KeyStat",display,outIn 
00080   pr #101: newpage
00090   pr #101: "Press any key to see it's keystat"
00100   pr #101: "      unhex and hex values."
00110   pr #101: "    Or press CTRL+A for ATTN"
00120   pr #101: "      ------------------------"
00130   pr #101: " KeyStat is a workstation Basic"
00140   pr #101: "            function."
00150   pr #101: "      ------------------------"
00160   for j=1 to 13
00170     pr #101: "                                 "
00180   next j
00190 L190: k$=kstat$
00200   if k$="" then goto L190
00210   pr #101: " UnHex: "&unhex$(k$)
00220   pr #101: "   Hex: "&k$
00221   pr #101: "  FKey: "&str$(fkey)
00230   pr #101: " "
00240   goto L190
00250 ! ______________________________________________________________________
00260 ! <Updateable Region: ERTN>
00270 ERTN: fnerror(program$,err,line,act$,"xit")
00280   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00290   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00300   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00310 ERTN_EXEC_ACT: execute act$ : goto ERTN
00320 ! /region
