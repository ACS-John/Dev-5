00010 ! Replace test\CmbAct
00011 ! -----------------------------------------------------------------------
00020   library 'S:\Core\Library': fncmbact,fnacs,fntos,fncmdset,fntop
00030   dim response$(2)*80
00032 ! -----------------------------------------------------------------------
00033   let fntop(program$,"Test UB's ComboAccount")
00040   let fntos(sn$="CmbAct")
00075   let fncmbact(1,1)
00100   let fncmdset(2) !:
        let fnacs(sn$,0,mat response$,ckey)
00110   pr response$(1)
00111 ! 
00120   let key$=response$(1)(32:41) !:
        pr "the key is "&key$
