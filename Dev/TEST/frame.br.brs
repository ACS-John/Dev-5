! Replace test\Frame.br

autoLibrary
fnTop(program$)

fnTos
dim ttt$*300
ttt$ = "Hiya. Look at me. I'm a duplicate!"

fnFra(1,1,3,12,ttt$,sn$)
fnFra(5,5,3,12,ttt$,sn$)
fnFra(1,1,1,6,ttt$,sn$,2)
fnFra(10,5,3,12,ttt$,sn$)

ttt$ = "data\R.jpg"
fnTxt(1,1,8,0,0,"1001",0,ttt$,1,0)
! fnpic(2,1,3,8,TTT$)
! fnTxt(5,10,20,0,0,MYMASK$,0,TTT$)
response$(1) = "121283" : _
button_option = 2
fnCmdKey('k',1)
fnAcs(mat response$,ckey)
! pr RESPONSE$(1)
