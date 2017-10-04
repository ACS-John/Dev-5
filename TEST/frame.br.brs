00010 ! Replace test\Frame.br
00015 ! -------------------------------------------------------------------
00020   library 'S:\Core\Library': fnprg,fntxt,fntos,fnacs,fnpic,fnfra,fncmdkey
00025   dim ttt$*300
00034   execute "Config Console Off"
00045 ! -------------------------------------------------------------------
00050   let prg$="test\Text3.br" : let fnprg(prg$,2)
00060   let sn$="Text3" !:
        fntos(sn$,2)
00064   let ttt$ = "Hiya. Look at me. I'm a duplicate!"
00065 ! -------------------------------------------------------------------
00066   fnfra(1,1,3,12,ttt$,sn$)
00067   fnfra(5,5,3,12,ttt$,sn$)
00068   fnfra(1,1,1,6,ttt$,sn$,2)
00069   fnfra(10,5,3,12,ttt$,sn$)
00070   let mymask$ = "1001"
00071   let ttt$ = "data\R.jpg"
00072   fntxt(1,1,8,0,0,mymask$,0,ttt$,1,0)
00073 ! Let fnpic(2,1,3,8,TTT$)
00074 ! Let fntxt(5,10,20,0,0,MYMASK$,0,TTT$)
00083   let response$(1) = "121283" !:
        button_option = 2
00084   fncmdkey('asdfasdf',1)
00085   fnacs(sn$,win,mat response$,ckey)
00090 ! pr RESPONSE$(1)
