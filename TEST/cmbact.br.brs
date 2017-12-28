00010 ! Replace test\CmbAct
00011 ! -----------------------------------------------------------------------
00020   library 'S:\Core\Library': fncmbact,fnAcs,fnTos,fnCmdSet,fntop
00030   dim response$(2)*80
00032 ! -----------------------------------------------------------------------
00033   fntop(program$,"Test UB's ComboAccount")
00040   fnTos(sn$="CmbAct")
00075   fncmbact(1,1)
00100   fnCmdSet(2) !:
        fnAcs(sn$,0,mat response$,ckey)
00110   pr response$(1)
00111 ! 
00120   key$=response$(1)(32:41) !:
        pr "the key is "&key$
