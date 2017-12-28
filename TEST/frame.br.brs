00010 ! Replace test\Frame.br
00015 ! -------------------------------------------------------------------
00020   library 'S:\Core\Library': fnprg,fnTxt,fnTos,fnAcs,fnpic,fnFra,fnCmdKey
00025   dim ttt$*300
00034   execute "Config Console Off"
00045 ! -------------------------------------------------------------------
00050   prg$="test\Text3.br" : fnprg(prg$,2)
00060   sn$="Text3" !:
        fnTos(sn$,2)
00064   ttt$ = "Hiya. Look at me. I'm a duplicate!"
00065 ! -------------------------------------------------------------------
00066   fnFra(1,1,3,12,ttt$,sn$)
00067   fnFra(5,5,3,12,ttt$,sn$)
00068   fnFra(1,1,1,6,ttt$,sn$,2)
00069   fnFra(10,5,3,12,ttt$,sn$)
00070   mymask$ = "1001"
00071   ttt$ = "data\R.jpg"
00072   fnTxt(1,1,8,0,0,mymask$,0,ttt$,1,0)
00073 ! fnpic(2,1,3,8,TTT$)
00074 ! fnTxt(5,10,20,0,0,MYMASK$,0,TTT$)
00083   response$(1) = "121283" !:
        button_option = 2
00084   fnCmdKey('asdfasdf',1)
00085   fnAcs(sn$,win,mat response$,ckey)
00090 ! pr RESPONSE$(1)
